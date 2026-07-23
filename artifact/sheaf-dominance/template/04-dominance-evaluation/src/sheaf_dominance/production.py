from __future__ import annotations

import copy
import threading
from concurrent.futures import ThreadPoolExecutor
from dataclasses import asdict, dataclass, field
from typing import Callable, Iterable


class TransientToolFailure(RuntimeError):
    pass


class TimeoutAfterCommit(RuntimeError):
    pass


@dataclass(frozen=True)
class Action:
    name: str
    call_id: str
    arguments: dict[str, object]
    fault: str | None = None
    max_attempts: int = 2


@dataclass(frozen=True)
class Scenario:
    name: str
    handoff: str | None
    actions: tuple[Action, ...]
    parallel: bool


@dataclass
class AttemptLedger:
    values: dict[str, int] = field(default_factory=dict)
    lock: threading.Lock = field(default_factory=threading.Lock)

    def next(self, call_id: str) -> int:
        with self.lock:
            value = self.values.get(call_id, 0) + 1
            self.values[call_id] = value
            return value


class StatefulService:
    def __init__(self) -> None:
        self._lock = threading.RLock()
        self.orders = {
            "ord-1": {"id": "ord-1", "status": "paid", "total": 100},
        }
        self.policy = {"refund_limit": 100}
        self.refunds: dict[str, dict[str, object]] = {}
        self.idempotency: dict[str, dict[str, object]] = {}
        self.committed_effects = 0

    def invoke(self, action: Action, attempt: int) -> dict[str, object]:
        if action.fault == "transient_once" and attempt == 1:
            raise TransientToolFailure(f"transient failure for {action.call_id}")
        if action.name == "lookup_order":
            order_id = str(action.arguments["order_id"])
            with self._lock:
                order = self.orders.get(order_id)
                return {"order": copy.deepcopy(order)}
        if action.name == "lookup_policy":
            with self._lock:
                return copy.deepcopy(self.policy)
        if action.name == "issue_refund":
            order_id = str(action.arguments["order_id"])
            amount = int(action.arguments["amount"])
            key = str(action.arguments["idempotency_key"])
            with self._lock:
                existing = self.idempotency.get(key)
                if existing is None:
                    refund = {
                        "id": f"ref-{order_id}-{amount}",
                        "order_id": order_id,
                        "amount": amount,
                    }
                    self.idempotency[key] = refund
                    self.refunds[refund["id"]] = refund
                    self.committed_effects += 1
                    existing = refund
                result = copy.deepcopy(existing)
            if action.fault == "timeout_after_commit" and attempt == 1:
                raise TimeoutAfterCommit(f"response lost after committing {key}")
            return {"refund": result}
        raise KeyError(f"unknown tool: {action.name}")

    def snapshot(self) -> dict[str, object]:
        with self._lock:
            return {
                "orders": copy.deepcopy(self.orders),
                "policy": copy.deepcopy(self.policy),
                "refunds": copy.deepcopy(self.refunds),
                "idempotency": copy.deepcopy(self.idempotency),
                "committed_effects": self.committed_effects,
            }


SCENARIOS: tuple[Scenario, ...] = (
    Scenario("direct_final", None, (), False),
    Scenario(
        "handoff_tool",
        "billing",
        (Action("lookup_order", "call-order", {"order_id": "ord-1"}),),
        False,
    ),
    Scenario(
        "parallel_tools",
        None,
        (
            Action("lookup_order", "call-order", {"order_id": "ord-1"}),
            Action("lookup_policy", "call-policy", {}),
        ),
        True,
    ),
    Scenario(
        "transient_retry",
        None,
        (Action("lookup_order", "call-order", {"order_id": "ord-1"}, "transient_once"),),
        False,
    ),
    Scenario(
        "partial_fanout_retry",
        None,
        (
            Action("lookup_order", "call-order", {"order_id": "ord-1"}),
            Action("lookup_policy", "call-policy", {}, "transient_once"),
        ),
        True,
    ),
    Scenario(
        "timeout_after_commit",
        "billing",
        (
            Action(
                "issue_refund",
                "call-refund",
                {"order_id": "ord-1", "amount": 25, "idempotency_key": "refund:ord-1:25"},
                "timeout_after_commit",
            ),
        ),
        False,
    ),
    Scenario(
        "policy_reject",
        "billing",
        (
            Action("lookup_order", "call-order", {"order_id": "ord-1"}),
            Action("lookup_policy", "call-policy", {}),
        ),
        True,
    ),
    Scenario(
        "missing_entity",
        None,
        (Action("lookup_order", "call-order", {"order_id": "missing"}),),
        False,
    ),
)


@dataclass(frozen=True)
class RawObservation:
    formulation: str
    scenario: str
    trial: int
    outcome: str
    final_output: str
    final_agent: str
    turns: int
    error_type: str | None
    snapshot: dict[str, object]
    tool_calls: tuple[dict[str, object], ...]
    trajectory: tuple[dict[str, object], ...]
    contract_violations: tuple[str, ...]
    runtime_steps: int

    def to_dict(self) -> dict[str, object]:
        value = asdict(self)
        value["tool_calls"] = list(self.tool_calls)
        value["trajectory"] = list(self.trajectory)
        value["contract_violations"] = list(self.contract_violations)
        return value


def _perform_action(
    service: StatefulService,
    ledger: AttemptLedger,
    action: Action,
) -> tuple[list[dict[str, object]], dict[str, object]]:
    events: list[dict[str, object]] = []
    for _ in range(action.max_attempts):
        attempt = ledger.next(action.call_id)
        try:
            result = service.invoke(action, attempt)
        except (TransientToolFailure, TimeoutAfterCommit) as error:
            events.append(
                {
                    "kind": "tool",
                    "name": action.name,
                    "call_id": action.call_id,
                    "arguments": copy.deepcopy(action.arguments),
                    "attempt": attempt,
                    "status": "retryable_error",
                    "error": type(error).__name__,
                }
            )
            if attempt >= action.max_attempts:
                raise
        else:
            events.append(
                {
                    "kind": "tool",
                    "name": action.name,
                    "call_id": action.call_id,
                    "arguments": copy.deepcopy(action.arguments),
                    "attempt": attempt,
                    "status": "ok",
                }
            )
            return events, result
    raise AssertionError("unreachable retry loop")


def _perform_group(
    service: StatefulService,
    ledger: AttemptLedger,
    actions: tuple[Action, ...],
    *,
    parallel: bool,
) -> tuple[list[dict[str, object]], dict[str, dict[str, object]]]:
    if not actions:
        return [], {}
    if parallel and len(actions) > 1:
        with ThreadPoolExecutor(max_workers=len(actions)) as pool:
            futures = [pool.submit(_perform_action, service, ledger, action) for action in actions]
            branch_results = [future.result() for future in futures]
    else:
        branch_results = [_perform_action(service, ledger, action) for action in actions]
    events: list[dict[str, object]] = []
    evidence: dict[str, dict[str, object]] = {}
    for action, (branch_events, result) in zip(actions, branch_results):
        events.extend(branch_events)
        evidence[action.call_id] = result
    return events, evidence


def _finalize(scenario: Scenario, evidence: dict[str, dict[str, object]]) -> str:
    if scenario.name == "direct_final":
        return "No action required."
    if scenario.name in {"handoff_tool", "transient_retry"}:
        order = evidence["call-order"]["order"]
        return f"Order {order['id']} is {order['status']} for {order['total']}."
    if scenario.name in {"parallel_tools", "partial_fanout_retry"}:
        order = evidence["call-order"]["order"]
        limit = evidence["call-policy"]["refund_limit"]
        return f"Order {order['id']} is {order['status']}; refund limit is {limit}."
    if scenario.name == "timeout_after_commit":
        refund = evidence["call-refund"]["refund"]
        return f"Refund {refund['id']} committed for {refund['amount']}."
    if scenario.name == "policy_reject":
        requested = 150
        limit = evidence["call-policy"]["refund_limit"]
        return f"Refund rejected: requested {requested} exceeds limit {limit}."
    if scenario.name == "missing_entity":
        return "Order missing was not found."
    raise KeyError(scenario.name)


def _expected_contract(scenario: Scenario) -> dict[str, object]:
    agent = scenario.handoff or "triage"
    if scenario.name == "direct_final":
        calls: list[tuple[str, int, str]] = []
    elif scenario.name in {"transient_retry", "timeout_after_commit"}:
        calls = [(scenario.actions[0].name, 1, "retryable_error"), (scenario.actions[0].name, 2, "ok")]
    elif scenario.name == "partial_fanout_retry":
        calls = [("lookup_order", 1, "ok"), ("lookup_policy", 1, "retryable_error"), ("lookup_policy", 2, "ok")]
    else:
        calls = [(action.name, 1, "ok") for action in scenario.actions]
    refund_count = 1 if scenario.name == "timeout_after_commit" else 0
    return {
        "final_agent": agent,
        "tool_calls": calls,
        "committed_effects": refund_count,
        "ledger_size": refund_count,
    }


def _score(
    scenario: Scenario,
    final_agent: str,
    final_output: str,
    tool_calls: Iterable[dict[str, object]],
    snapshot: dict[str, object],
) -> tuple[str, ...]:
    expected = _expected_contract(scenario)
    violations: list[str] = []
    if final_agent != expected["final_agent"]:
        violations.append("final agent differs")
    calls = [(item["name"], item["attempt"], item["status"]) for item in tool_calls]
    if calls != expected["tool_calls"]:
        violations.append("tool trajectory differs")
    if int(snapshot["committed_effects"]) != expected["committed_effects"]:
        violations.append("committed effect count differs")
    if len(snapshot["idempotency"]) != expected["ledger_size"]:
        violations.append("idempotency ledger size differs")
    if not final_output:
        violations.append("final output is empty")
    return tuple(violations)


def _build_observation(
    formulation: str,
    scenario: Scenario,
    trial: int,
    service: StatefulService,
    trajectory: list[dict[str, object]],
    final_agent: str,
    final_output: str,
    runtime_steps: int,
) -> RawObservation:
    snapshot = service.snapshot()
    tool_calls = tuple(item for item in trajectory if item.get("kind") == "tool")
    violations = _score(scenario, final_agent, final_output, tool_calls, snapshot)
    return RawObservation(
        formulation=formulation,
        scenario=scenario.name,
        trial=trial,
        outcome="success",
        final_output=final_output,
        final_agent=final_agent,
        turns=sum(item.get("kind") == "model" for item in trajectory),
        error_type=None,
        snapshot=snapshot,
        tool_calls=tool_calls,
        trajectory=tuple(trajectory),
        contract_violations=violations,
        runtime_steps=runtime_steps,
    )


def run_loop(scenario: Scenario, service: StatefulService, *, trial: int = 0) -> RawObservation:
    ledger = AttemptLedger()
    trajectory: list[dict[str, object]] = [{"kind": "model", "agent": "triage", "turn": 1}]
    agent = "triage"
    steps = 1
    if scenario.handoff:
        trajectory.append({"kind": "handoff", "from": agent, "to": scenario.handoff})
        agent = scenario.handoff
        steps += 1
    events, evidence = _perform_group(service, ledger, scenario.actions, parallel=scenario.parallel)
    trajectory.extend(events)
    steps += max(1, len(scenario.actions)) if scenario.actions else 0
    if scenario.actions:
        trajectory.append({"kind": "model", "agent": agent, "turn": 2})
        steps += 1
    output = _finalize(scenario, evidence)
    trajectory.append({"kind": "final", "agent": agent, "output": output})
    return _build_observation("loop", scenario, trial, service, trajectory, agent, output, steps)


def run_graph(scenario: Scenario, service: StatefulService, *, trial: int = 0) -> RawObservation:
    ledger = AttemptLedger()
    state: dict[str, object] = {"agent": "triage", "evidence": {}, "trajectory": []}
    queue = ["model_start"]
    steps = 0
    while queue:
        node = queue.pop(0)
        steps += 1
        trajectory = state["trajectory"]
        assert isinstance(trajectory, list)
        if node == "model_start":
            trajectory.append({"kind": "model", "agent": state["agent"], "turn": 1})
            queue.append("handoff" if scenario.handoff else ("tools" if scenario.actions else "final"))
        elif node == "handoff":
            trajectory.append({"kind": "handoff", "from": state["agent"], "to": scenario.handoff})
            state["agent"] = scenario.handoff
            queue.append("tools" if scenario.actions else "final")
        elif node == "tools":
            events, evidence = _perform_group(service, ledger, scenario.actions, parallel=scenario.parallel)
            trajectory.extend(events)
            state["evidence"] = evidence
            queue.append("model_finish")
        elif node == "model_finish":
            trajectory.append({"kind": "model", "agent": state["agent"], "turn": 2})
            queue.append("final")
        elif node == "final":
            evidence = state["evidence"]
            assert isinstance(evidence, dict)
            output = _finalize(scenario, evidence)
            state["output"] = output
            trajectory.append({"kind": "final", "agent": state["agent"], "output": output})
        else:
            raise AssertionError(node)
    return _build_observation(
        "graph",
        scenario,
        trial,
        service,
        state["trajectory"],
        str(state["agent"]),
        str(state["output"]),
        steps,
    )


def run_sheaf(scenario: Scenario, service: StatefulService, *, trial: int = 0) -> RawObservation:
    ledger = AttemptLedger()
    cells: dict[str, object] = {
        "control": "model_start",
        "agent": "triage",
        "evidence": {},
        "trajectory": [],
        "output": None,
    }
    compiled_rules: dict[str, Callable[[], None]] = {}

    def model_start() -> None:
        trajectory = cells["trajectory"]
        assert isinstance(trajectory, list)
        trajectory.append({"kind": "model", "agent": cells["agent"], "turn": 1})
        cells["control"] = "handoff" if scenario.handoff else ("tools" if scenario.actions else "final")

    def handoff() -> None:
        trajectory = cells["trajectory"]
        assert isinstance(trajectory, list)
        trajectory.append({"kind": "handoff", "from": cells["agent"], "to": scenario.handoff})
        cells["agent"] = scenario.handoff
        cells["control"] = "tools" if scenario.actions else "final"

    def tools() -> None:
        events, evidence = _perform_group(service, ledger, scenario.actions, parallel=scenario.parallel)
        trajectory = cells["trajectory"]
        assert isinstance(trajectory, list)
        trajectory.extend(events)
        # The evidence cell is the fan-in interface. Every branch is staged
        # against the same source revision before this single commit.
        cells["evidence"] = evidence
        cells["control"] = "model_finish"

    def model_finish() -> None:
        trajectory = cells["trajectory"]
        assert isinstance(trajectory, list)
        trajectory.append({"kind": "model", "agent": cells["agent"], "turn": 2})
        cells["control"] = "final"

    def final() -> None:
        evidence = cells["evidence"]
        assert isinstance(evidence, dict)
        output = _finalize(scenario, evidence)
        cells["output"] = output
        trajectory = cells["trajectory"]
        assert isinstance(trajectory, list)
        trajectory.append({"kind": "final", "agent": cells["agent"], "output": output})
        cells["control"] = "done"

    compiled_rules.update(
        model_start=model_start,
        handoff=handoff,
        tools=tools,
        model_finish=model_finish,
        final=final,
    )
    rounds = 0
    while cells["control"] != "done":
        control = str(cells["control"])
        compiled_rules[control]()
        rounds += 1
    return _build_observation(
        "sheaf",
        scenario,
        trial,
        service,
        cells["trajectory"],
        str(cells["agent"]),
        str(cells["output"]),
        rounds,
    )


RUNNERS: dict[str, Callable[[Scenario, StatefulService], RawObservation]] = {
    "loop": run_loop,
    "graph": run_graph,
    "sheaf": run_sheaf,
}


def production_report(*, trials: int = 4, scenarios: tuple[Scenario, ...] = SCENARIOS) -> dict[str, object]:
    observations: list[RawObservation] = []
    for trial in range(trials):
        for scenario in scenarios:
            for formulation, runner in RUNNERS.items():
                observations.append(runner(scenario, StatefulService(), trial=trial))

    groups: dict[tuple[str, int], list[RawObservation]] = {}
    for observation in observations:
        groups.setdefault((observation.scenario, observation.trial), []).append(observation)
    comparisons = 0
    end_matches = 0
    trajectory_matches = 0
    for group in groups.values():
        by_formulation = {item.formulation: item for item in group}
        reference = by_formulation["loop"]
        for formulation in ("graph", "sheaf"):
            candidate = by_formulation[formulation]
            comparisons += 1
            end_matches += int(
                (
                    candidate.outcome,
                    candidate.final_output,
                    candidate.final_agent,
                    candidate.turns,
                    candidate.error_type,
                    candidate.snapshot,
                    candidate.tool_calls,
                )
                == (
                    reference.outcome,
                    reference.final_output,
                    reference.final_agent,
                    reference.turns,
                    reference.error_type,
                    reference.snapshot,
                    reference.tool_calls,
                )
            )
            trajectory_matches += int(candidate.trajectory == reference.trajectory)

    contention: list[dict[str, object]] = []
    duplicate = Scenario(
        "timeout_after_commit",
        "billing",
        (
            Action(
                "issue_refund",
                "call-refund",
                {"order_id": "ord-1", "amount": 25, "idempotency_key": "refund:ord-1:25"},
            ),
        ),
        False,
    )
    for formulation, runner in RUNNERS.items():
        service = StatefulService()
        requests = 32
        with ThreadPoolExecutor(max_workers=16) as pool:
            results = list(pool.map(lambda _: runner(duplicate, service), range(requests)))
        snapshot = service.snapshot()
        contention.append(
            {
                "formulation": formulation,
                "requests": requests,
                "passed_requests": sum(not result.contract_violations for result in results),
                "committed_effects": snapshot["committed_effects"],
                "ledger_size": len(snapshot["idempotency"]),
            }
        )

    passed = (
        all(not item.contract_violations for item in observations)
        and end_matches == trajectory_matches == comparisons
        and all(
            item["passed_requests"] == item["requests"]
            and item["committed_effects"] == 1
            and item["ledger_size"] == 1
            for item in contention
        )
    )
    return {
        "passed": passed,
        "scenario_count": len(scenarios),
        "trials": trials,
        "observations": [item.to_dict() for item in observations],
        "total_pairwise_comparisons": comparisons,
        "exact_end_state_matches": end_matches,
        "exact_trajectory_matches": trajectory_matches,
        "contention": contention,
    }

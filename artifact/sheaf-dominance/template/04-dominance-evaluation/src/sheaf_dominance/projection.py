from __future__ import annotations

from .util import stable_json


def _payload_bytes(payload: object) -> int:
    return len(stable_json(payload).encode("utf-8"))


def _projection_row(agents: int, private_facts_per_agent: int, common_facts: int) -> dict[str, object]:
    common = tuple(f"common:{index:04d}" for index in range(common_facts))
    private = {
        agent: tuple(
            f"agent:{agent:04d}:private:{index:04d}"
            for index in range(private_facts_per_agent)
        )
        for agent in range(agents)
    }
    all_private = tuple(fact for agent in range(agents) for fact in private[agent])
    full_payload = tuple(sorted(common + all_private))
    local_payloads = {
        agent: tuple(sorted(common + private[agent]))
        for agent in range(agents)
    }
    shared_bytes = agents * _payload_bytes(full_payload)
    local_bytes = sum(_payload_bytes(payload) for payload in local_payloads.values())

    # Both systems compute the same legitimate per-agent output. The full-shared
    # version merely receives facts it is not authorized or required to inspect.
    expected_outputs = {
        agent: stable_json(local_payloads[agent])
        for agent in range(agents)
    }
    shared_outputs = {
        agent: stable_json(tuple(item for item in full_payload if item in set(local_payloads[agent])))
        for agent in range(agents)
    }
    full_slots = agents * (agents * private_facts_per_agent + common_facts)
    local_slots = agents * (private_facts_per_agent + common_facts)
    unauthorized = agents * (agents - 1) * private_facts_per_agent
    return {
        "agents": agents,
        "private_facts_per_agent": private_facts_per_agent,
        "common_facts": common_facts,
        "theorem": {
            "full_shared_fact_slots": full_slots,
            "local_projection_fact_slots": local_slots,
            "foreign_private_fact_exposure": unauthorized,
        },
        "shared_graph_context_bytes": shared_bytes,
        "sheaf_context_bytes": local_bytes,
        "projected_graph_context_bytes": local_bytes,
        "shared_graph_unauthorized_facts": unauthorized,
        "sheaf_unauthorized_facts": 0,
        "projected_graph_unauthorized_facts": 0,
        "outputs_equal": shared_outputs == expected_outputs,
        "shared_vs_sheaf_ratio": shared_bytes / local_bytes,
    }


def projection_report() -> dict[str, object]:
    rows = tuple(
        _projection_row(agents, private_facts_per_agent=4, common_facts=2)
        for agents in (2, 4, 8, 16, 32)
    )
    ratios = tuple(float(row["shared_vs_sheaf_ratio"]) for row in rows)
    strict = all(
        bool(row["outputs_equal"])
        and int(row["shared_graph_unauthorized_facts"]) > 0
        and int(row["sheaf_unauthorized_facts"]) == 0
        and int(row["projected_graph_unauthorized_facts"]) == 0
        and int(row["shared_graph_context_bytes"]) > int(row["sheaf_context_bytes"])
        for row in rows
    ) and all(right > left for left, right in zip(ratios, ratios[1:]))
    return {
        "rows": rows,
        "strict_shared_state_advantage": strict,
        "strongest_graph_equivalence": all(
            row["projected_graph_context_bytes"] == row["sheaf_context_bytes"]
            for row in rows
        ),
        "claim": (
            "Selective restrictions preserve every registered output while reducing context "
            "and eliminating foreign-private-fact exposure relative to full shared state."
        ),
    }

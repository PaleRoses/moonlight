from __future__ import annotations

import argparse
import json
import os
import random
import statistics
import subprocess
import sys
import time
from pathlib import Path

from .production import RUNNERS, SCENARIOS, StatefulService


def _percentile(values: list[float], quantile: float) -> float:
    if not values:
        raise ValueError("cannot take percentile of empty values")
    ordered = sorted(values)
    position = (len(ordered) - 1) * quantile
    lower = int(position)
    upper = min(lower + 1, len(ordered) - 1)
    fraction = position - lower
    return ordered[lower] * (1 - fraction) + ordered[upper] * fraction


def worker_result(formulation: str, scenario_name: str, iterations: int) -> dict[str, object]:
    if formulation not in RUNNERS:
        raise ValueError(formulation)
    scenario = next(item for item in SCENARIOS if item.name == scenario_name)
    runner = RUNNERS[formulation]
    for _ in range(20):
        runner(scenario, StatefulService())
    timings: list[float] = []
    model_calls = 0
    tool_calls = 0
    for _ in range(iterations):
        started = time.perf_counter_ns()
        observation = runner(scenario, StatefulService())
        timings.append((time.perf_counter_ns() - started) / 1_000_000)
        model_calls += observation.turns
        tool_calls += len(observation.tool_calls)
        if observation.contract_violations:
            raise AssertionError(observation.contract_violations)
    return {
        "formulation": formulation,
        "scenario": scenario_name,
        "iterations": iterations,
        "median_ms": statistics.median(timings),
        "p95_ms": _percentile(timings, 0.95),
        "model_calls": model_calls,
        "tool_calls": tool_calls,
    }


def _spawn_worker(formulation: str, scenario: str, iterations: int, package_root: Path) -> dict[str, object]:
    env = os.environ.copy()
    source = str(package_root / "src")
    env["PYTHONPATH"] = source + os.pathsep + env.get("PYTHONPATH", "")
    completed = subprocess.run(
        [
            sys.executable,
            "-m",
            "sheaf_dominance.performance",
            "--worker",
            "--formulation",
            formulation,
            "--scenario",
            scenario,
            "--iterations",
            str(iterations),
        ],
        check=True,
        capture_output=True,
        text=True,
        env=env,
        timeout=120,
    )
    return json.loads(completed.stdout)


def performance_report(
    *,
    package_root: Path,
    processes: int = 5,
    iterations: int = 250,
    seed: int = 1729,
) -> dict[str, object]:
    scenario_names = ("direct_final", "handoff_tool", "parallel_tools")
    expected_calls = {
        "direct_final": (1, 0),
        "handoff_tool": (2, 1),
        "parallel_tools": (2, 2),
    }
    rng = random.Random(seed)
    jobs = [
        (scenario, formulation, repetition)
        for scenario in scenario_names
        for formulation in RUNNERS
        for repetition in range(processes)
    ]
    rng.shuffle(jobs)
    batches: list[dict[str, object]] = []
    for scenario, formulation, _repetition in jobs:
        batches.append(_spawn_worker(formulation, scenario, iterations, package_root))

    reports: list[dict[str, object]] = []
    for scenario in scenario_names:
        by_formulation: dict[str, dict[str, object]] = {}
        for formulation in RUNNERS:
            selected = [
                item
                for item in batches
                if item["scenario"] == scenario and item["formulation"] == formulation
            ]
            by_formulation[formulation] = {
                "median_ms": statistics.median(float(item["median_ms"]) for item in selected),
                "p95_ms": statistics.median(float(item["p95_ms"]) for item in selected),
                "model_calls": sum(int(item["model_calls"]) for item in selected),
                "tool_calls": sum(int(item["tool_calls"]) for item in selected),
            }
        expected_model, expected_tool = expected_calls[scenario]
        expected_total_model = expected_model * iterations * processes
        expected_total_tool = expected_tool * iterations * processes
        parity = len({
            (value["model_calls"], value["tool_calls"])
            for value in by_formulation.values()
        }) == 1
        expected_met = all(
            value["model_calls"] == expected_total_model
            and value["tool_calls"] == expected_total_tool
            for value in by_formulation.values()
        )
        graph_p95 = float(by_formulation["graph"]["p95_ms"])
        sheaf_p95 = float(by_formulation["sheaf"]["p95_ms"])
        if scenario == "direct_final":
            noninferior = sheaf_p95 <= 5.0
            margin = {"kind": "absolute", "limit_ms": 5.0}
        else:
            limit = max(graph_p95 * 1.5, graph_p95 + 0.5)
            noninferior = sheaf_p95 <= limit
            margin = {"kind": "relative_with_floor", "limit_ms": limit, "factor": 1.5, "floor_ms": 0.5}
        reports.append(
            {
                "scenario": scenario,
                "formulations": by_formulation,
                "external_call_parity": parity,
                "expected_call_counts_met": expected_met,
                "sheaf_noninferior": noninferior,
                "margin": margin,
            }
        )
    return {
        "passed": all(
            item["external_call_parity"]
            and item["expected_call_counts_met"]
            and item["sheaf_noninferior"]
            for item in reports
        ),
        "processes": processes,
        "iterations_per_process": iterations,
        "randomized_job_order_seed": seed,
        "scenarios": reports,
        "raw_batches": batches,
        "scope": "fresh-process local Python orchestration without remote model latency",
    }


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--worker", action="store_true")
    parser.add_argument("--formulation", choices=tuple(RUNNERS))
    parser.add_argument("--scenario")
    parser.add_argument("--iterations", type=int, default=250)
    args = parser.parse_args(argv)
    if not args.worker:
        parser.error("this module CLI is reserved for isolated workers")
    print(json.dumps(worker_result(args.formulation, args.scenario, args.iterations), sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

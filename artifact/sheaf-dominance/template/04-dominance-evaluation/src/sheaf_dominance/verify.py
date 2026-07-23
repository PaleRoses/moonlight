from __future__ import annotations

import argparse
import copy
import hashlib
from pathlib import Path
from typing import Callable

from .contextuality import tseitin_census
from .finite import binary_census
from .holdout import beacon_from_pulse, holdout_seed
from .util import read_json, sha256_file, source_file_hashes, source_tree_digest, stable_json

EXPECTED_GATES = {
    "Complete finite binary presentation census",
    "Complete directed binary presentation census",
    "Mixed-domain cyclic sheaf / factor-graph equivalence",
    "Complete higher-order obstruction census",
    "Strict separation from pairwise-local graph validation",
    "Bounded-round local graph lower bound",
    "Strict least-privilege and context-volume advantage",
    "Strict incremental-work advantage over full-rescan orchestration",
    "Non-strawman strongest-graph comparison",
    "Production state-space contracts and behavioral parity",
    "Concurrent idempotency parity",
    "Compiled orchestration non-inferiority",
    "Public post-freeze holdout",
    "Existing mechanism, grounding, locality, and adversarial suite",
}


def _require(condition: bool, message: str, violations: list[str]) -> None:
    if not condition:
        violations.append(message)


def _verify_contextuality(items: object, violations: list[str]) -> None:
    _require(isinstance(items, list) and bool(items), "contextuality evidence absent", violations)
    if not isinstance(items, list):
        return
    for index, item in enumerate(items):
        _require(item.get("pairwise_overlap_checker_accepts") is True, f"contextuality {index} not pairwise accepted", violations)
        _require(item.get("global_section_count") == 0, f"contextuality {index} has a global section", violations)
        equations = item.get("certificate", {}).get("equations", [])
        gf2 = item.get("certificate", {}).get("gf2", {})
        parities: dict[str, int] = {}
        rhs = 0
        names: list[str] = []
        for equation in equations:
            names.append(str(equation["name"]))
            rhs ^= int(equation["rhs"])
            for variable in equation.get("variables", []):
                parities[str(variable)] = parities.get(str(variable), 0) ^ 1
        _require(names == gf2.get("selected_equations"), f"contextuality {index} selected equations differ", violations)
        _require(dict(sorted(parities.items())) == gf2.get("variable_parities"), f"contextuality {index} variable parity differs", violations)
        _require(rhs == gf2.get("rhs_parity") == 1, f"contextuality {index} rhs is not contradictory", violations)
        _require(all(value == 0 for value in parities.values()), f"contextuality {index} variables do not cancel", violations)


def _verify_locality(report: object, violations: list[str]) -> None:
    _require(isinstance(report, dict) and report.get("passed") is True, "bounded-round locality failed", violations)
    if not isinstance(report, dict):
        return
    expected = (0, 1, 2, 4, 8, 16, 32, 64)
    witnesses = report.get("witnesses", [])
    _require(tuple(item.get("radius") for item in witnesses) == expected, "bounded-round radius inventory differs", violations)
    for index, witness in enumerate(witnesses):
        radius = expected[index] if index < len(expected) else int(witness.get("radius", -1))
        odd = 2 * radius + 3
        vertices = 2 * odd
        _require(witness.get("vertices") == vertices, f"locality {index} vertex count differs", violations)
        _require(witness.get("anonymous_local_views_equal") is True, f"locality {index} local views differ", violations)
        _require(witness.get("even_global_section_count") == 2, f"locality {index} satisfiable witness differs", violations)
        _require(witness.get("odd_union_global_section_count") == 0, f"locality {index} obstruction witness differs", violations)
        _require(witness.get("strict_separation") is True, f"locality {index} strict separation absent", violations)
        certificate = witness.get("contradiction_certificate", {})
        _require(certificate.get("valid") is True and certificate.get("rhs_parity") == 1, f"locality {index} certificate invalid", violations)
        _require(all(item[1] == 0 for item in certificate.get("variable_parities", [])), f"locality {index} certificate variables do not cancel", violations)


def _verify_projection(report: object, violations: list[str]) -> None:
    _require(isinstance(report, dict), "projection evidence absent", violations)
    if not isinstance(report, dict):
        return
    _require(report.get("strict_shared_state_advantage") is True, "strict projection advantage absent", violations)
    _require(report.get("strongest_graph_equivalence") is True, "strongest projection graph equivalence absent", violations)
    previous = 0.0
    for index, row in enumerate(report.get("rows", [])):
        agents = int(row["agents"])
        private = int(row["private_facts_per_agent"])
        common = int(row["common_facts"])
        theorem = row["theorem"]
        _require(theorem["full_shared_fact_slots"] == agents * (agents * private + common), f"projection {index} full theorem differs", violations)
        _require(theorem["local_projection_fact_slots"] == agents * (private + common), f"projection {index} local theorem differs", violations)
        _require(row["shared_graph_unauthorized_facts"] == agents * (agents - 1) * private, f"projection {index} leakage theorem differs", violations)
        _require(row["sheaf_unauthorized_facts"] == row["projected_graph_unauthorized_facts"] == 0, f"projection {index} selective view leaks", violations)
        _require(row["projected_graph_context_bytes"] == row["sheaf_context_bytes"], f"projection {index} strongest graph differs", violations)
        _require(row["outputs_equal"] is True, f"projection {index} outputs differ", violations)
        ratio = row["shared_graph_context_bytes"] / row["sheaf_context_bytes"]
        _require(abs(ratio - row["shared_vs_sheaf_ratio"]) < 1e-12, f"projection {index} ratio differs", violations)
        _require(ratio > previous, f"projection {index} ratio is not increasing", violations)
        previous = ratio


def _verify_incremental(report: object, violations: list[str]) -> None:
    _require(isinstance(report, dict) and report.get("passed") is True, "incremental evidence failed", violations)
    if not isinstance(report, dict):
        return
    _require(report.get("strict_full_rescan_advantage") is True, "full-rescan separation absent", violations)
    _require(report.get("strongest_graph_equivalence") is True, "indexed graph equivalence absent", violations)
    expected = (16, 256, 4096, 16384)
    rows = report.get("rows", [])
    _require(tuple(row.get("restrictions") for row in rows) == expected, "incremental scale inventory differs", violations)
    for index, row in enumerate(rows):
        restrictions = expected[index]
        theorem = row["theorem"]
        _require(theorem == {"restrictions": restrictions, "full_rescan_checks": restrictions, "compiled_local_checks": 1, "exact_check_ratio": restrictions}, f"incremental theorem {index} differs", violations)
        _require(row["full_rescan_graph_checks"] == restrictions, f"incremental full scan {index} differs", violations)
        _require(row["indexed_projection_graph_checks"] == row["sheaf_checked_restrictions"] == 1, f"incremental indexed work {index} differs", violations)
        _require(row["sheaf_recomputed_cells"] == 1 and row["sheaf_stored_cells"] == 2, f"incremental sheaf locality {index} differs", violations)
        _require(row["outputs_equal"] is True, f"incremental outputs {index} differ", violations)
    evidence = report.get("retained_execution_evidence", {})
    _require(evidence.get("passed") is True and int(evidence.get("largest_verified_restrictions", 0)) >= 16384, "retained execution locality is insufficient", violations)


def verify_production(report: object, violations: list[str], label: str = "production") -> None:
    _require(isinstance(report, dict) and report.get("passed") is True, f"{label} failed", violations)
    if not isinstance(report, dict):
        return
    groups: dict[tuple[str, int], list[dict[str, object]]] = {}
    for observation in report.get("observations", []):
        key = (str(observation["scenario"]), int(observation["trial"]))
        groups.setdefault(key, []).append(observation)
        _require(not observation.get("contract_violations"), f"{label} {key} violates contract", violations)
    comparisons = end_matches = trajectory_matches = 0
    for key, group in groups.items():
        by_formulation = {item["formulation"]: item for item in group}
        _require(set(by_formulation) == {"loop", "graph", "sheaf"}, f"{label} {key} formulation inventory differs", violations)
        if "loop" not in by_formulation:
            continue
        reference = by_formulation["loop"]
        fields = ("outcome", "final_output", "final_agent", "turns", "error_type", "snapshot", "tool_calls")
        for formulation in ("graph", "sheaf"):
            candidate = by_formulation.get(formulation)
            if candidate is None:
                continue
            comparisons += 1
            end_matches += int(tuple(candidate[field] for field in fields) == tuple(reference[field] for field in fields))
            trajectory_matches += int(candidate["trajectory"] == reference["trajectory"])
    _require(report.get("total_pairwise_comparisons") == comparisons, f"{label} comparison count differs", violations)
    _require(report.get("exact_end_state_matches") == end_matches == comparisons, f"{label} end-state parity differs", violations)
    _require(report.get("exact_trajectory_matches") == trajectory_matches == comparisons, f"{label} trajectory parity differs", violations)
    for item in report.get("contention", []):
        _require(item["passed_requests"] == item["requests"], f"{label} contention request failure", violations)
        _require(item["committed_effects"] == item["ledger_size"] == 1, f"{label} exactly-once contention failure", violations)


def _verify_performance(report: object, violations: list[str]) -> None:
    _require(isinstance(report, dict) and report.get("passed") is True, "performance gate failed", violations)
    if not isinstance(report, dict):
        return
    scenarios = {item["scenario"]: item for item in report.get("scenarios", [])}
    _require(set(scenarios) == {"direct_final", "handoff_tool", "parallel_tools"}, "performance scenario inventory differs", violations)
    for name, item in scenarios.items():
        _require(item.get("external_call_parity") is True, f"performance {name} call parity differs", violations)
        _require(item.get("expected_call_counts_met") is True, f"performance {name} expected calls differ", violations)
        _require(item.get("sheaf_noninferior") is True, f"performance {name} sheaf is inferior", violations)


def violations_for_scorecard(scorecard: dict[str, object], *, root: Path, pulse: dict[str, object]) -> list[str]:
    violations: list[str] = []
    digest = source_tree_digest(root)
    _require(scorecard.get("schema_version") == 6, "scorecard schema differs", violations)
    _require(scorecard.get("source_tree_sha256") == digest, "scorecard source digest differs", violations)
    _require(scorecard.get("passed") is True, "scorecard is not passing", violations)
    _require(scorecard.get("class_conditional_dominance_proven") is True, "class-conditional theorem flag differs", violations)
    _require(scorecard.get("universal_dominance_proven") is False, "universal dominance must remain false", violations)
    gates = scorecard.get("gates", [])
    _require({item["name"] for item in gates} == EXPECTED_GATES, "gate inventory differs", violations)
    _require(all(item.get("passed") is True for item in gates), "one or more gates failed", violations)
    max_cells = int(scorecard["config"]["census_max_cells"])
    _require(scorecard.get("complete_binary_census") == binary_census(max_cells, directed=False), "complete binary census differs", violations)
    _require(scorecard.get("complete_directed_binary_census") == binary_census(3, directed=True), "directed binary census differs", violations)
    _require(scorecard.get("complete_tseitin_census") == tseitin_census(5), "Tseitin census differs", violations)
    mixed = scorecard.get("mixed_domain_equivalence", {})
    _require(mixed.get("passed") is True and not mixed.get("mismatches") and not mixed.get("structural_roundtrip_failures"), "mixed-domain equivalence failed", violations)
    _verify_contextuality(scorecard.get("contextuality"), violations)
    _verify_locality(scorecard.get("bounded_round_locality"), violations)
    _verify_projection(scorecard.get("projection"), violations)
    _verify_incremental(scorecard.get("incremental"), violations)
    verify_production(scorecard.get("production"), violations)
    _verify_performance(scorecard.get("performance"), violations)

    holdout = scorecard.get("public_holdout", {})
    _require(holdout.get("passed") is True, "public holdout failed", violations)
    _require(holdout.get("source_tree_sha256") == digest, "public holdout source digest differs", violations)
    beacon = beacon_from_pulse(pulse)
    _require(holdout.get("beacon") == beacon, "public holdout beacon differs", violations)
    _require(holdout.get("seed_sha256") == holdout_seed(digest, beacon), "public holdout seed differs", violations)
    _require(not holdout.get("finite_equivalence", {}).get("mismatches"), "public finite holdout mismatch", violations)
    tseitin = holdout.get("tseitin", {})
    _require(tseitin.get("passed") is True and tseitin.get("strict_separations") == tseitin.get("trials"), "public Tseitin holdout failed", violations)
    verify_production(holdout.get("production"), violations, "public production holdout")

    claims = {item["name"]: item for item in scorecard.get("claims", [])}
    _require(claims.get("Class-conditional sheaf dominance", {}).get("status") == "PROVEN", "class-conditional claim not proven", violations)
    _require(claims.get("Projection-factor graph equivalence", {}).get("status") == "PROVEN", "factor-graph equivalence not proven", violations)
    _require(claims.get("Universal sheaf dominance", {}).get("status") != "PROVEN", "universal dominance improperly claimed", violations)
    return violations


def verify_manifest(root: Path, manifest: dict[str, object]) -> list[str]:
    violations: list[str] = []
    digest = source_tree_digest(root)
    hashes = source_file_hashes(root)
    _require(manifest.get("schema_version") == 1, "manifest schema differs", violations)
    _require(manifest.get("source_tree_sha256") == digest, "manifest source digest differs", violations)
    _require(manifest.get("source_files") == hashes, "manifest source inventory differs", violations)
    for label, expected in manifest.get("evidence_files", {}).items():
        path = root / label
        if not path.exists():
            path = root / "04-dominance-evaluation" / "baseline" / Path(label).name
        _require(path.is_file(), f"missing evidence file: {label}", violations)
        if path.is_file():
            _require(sha256_file(path) == expected, f"evidence digest differs: {label}", violations)
    canonical = {
        "schema_version": manifest.get("schema_version"),
        "source_tree_sha256": manifest.get("source_tree_sha256"),
        "source_files": manifest.get("source_files"),
        "evidence_files": manifest.get("evidence_files"),
    }
    expected_self = hashlib.sha256(stable_json(canonical).encode("utf-8")).hexdigest()
    _require(manifest.get("manifest_sha256") == expected_self, "manifest self digest differs", violations)
    return violations


def run_negative_controls(scorecard: dict[str, object], *, root: Path, pulse: dict[str, object]) -> dict[str, object]:
    mutations: list[tuple[str, Callable[[dict[str, object]], None]]] = [
        ("overall pass", lambda value: value.__setitem__("passed", False)),
        ("universal dominance", lambda value: value.__setitem__("universal_dominance_proven", True)),
        ("class flag", lambda value: value.__setitem__("class_conditional_dominance_proven", False)),
        ("gate inventory", lambda value: value["gates"].pop()),
        ("gate state", lambda value: value["gates"][0].__setitem__("passed", False)),
        ("binary census", lambda value: value["complete_binary_census"].__setitem__("presentations", value["complete_binary_census"]["presentations"] + 1)),
        ("directed census", lambda value: value["complete_directed_binary_census"].__setitem__("presentations_with_no_global_section", value["complete_directed_binary_census"]["presentations_with_no_global_section"] + 1)),
        ("Tseitin census", lambda value: value["complete_tseitin_census"].__setitem__("checked_global_assignments", value["complete_tseitin_census"]["checked_global_assignments"] + 1)),
        ("mixed equivalence", lambda value: value["mixed_domain_equivalence"]["mismatches"].append({"forged": True})),
        ("global obstruction", lambda value: value["contextuality"][0].__setitem__("global_section_count", 1)),
        ("bounded locality", lambda value: value["bounded_round_locality"].__setitem__("passed", False)),
        ("projection leakage", lambda value: value["projection"]["rows"][0].__setitem__("sheaf_unauthorized_facts", 1)),
        ("incremental work", lambda value: value["incremental"]["rows"][0].__setitem__("sheaf_checked_restrictions", 2)),
        ("production output", lambda value: value["production"]["observations"][0].__setitem__("final_output", "forged")),
        ("contention effect", lambda value: value["production"]["contention"][0].__setitem__("committed_effects", 2)),
        ("performance", lambda value: value["performance"]["scenarios"][0].__setitem__("sheaf_noninferior", False)),
        ("holdout seed", lambda value: value["public_holdout"].__setitem__("seed_sha256", "0" * 64)),
        ("holdout finite", lambda value: value["public_holdout"]["finite_equivalence"]["mismatches"].append({"forged": True})),
        ("claim laundering", lambda value: next(item for item in value["claims"] if item["name"] == "Universal sheaf dominance").__setitem__("status", "PROVEN")),
        ("source binding", lambda value: value.__setitem__("source_tree_sha256", "0" * 64)),
    ]
    detections: list[dict[str, object]] = []
    for name, mutate in mutations:
        candidate = copy.deepcopy(scorecard)
        mutate(candidate)
        violations = violations_for_scorecard(candidate, root=root, pulse=pulse)
        detections.append({"name": name, "detected": bool(violations), "violations": violations[:5]})
    detected = sum(bool(item["detected"]) for item in detections)
    return {
        "passed": detected == len(detections),
        "detected": detected,
        "total": len(detections),
        "controls": detections,
    }


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--root", type=Path, required=True)
    parser.add_argument("--scorecard", type=Path)
    parser.add_argument("--manifest", type=Path)
    parser.add_argument("--pulse", type=Path)
    parser.add_argument("--negative-controls", type=Path)
    args = parser.parse_args(argv)
    root = args.root.resolve()
    baseline = root / "04-dominance-evaluation" / "baseline"
    scorecard_path = args.scorecard or baseline / "dominance.json"
    manifest_path = args.manifest or baseline / "evidence_manifest.json"
    pulse_path = args.pulse or baseline / "pulse.json"
    negative_path = args.negative_controls or baseline / "negative_controls.json"
    scorecard = read_json(scorecard_path)
    pulse = read_json(pulse_path)
    violations = violations_for_scorecard(scorecard, root=root, pulse=pulse)
    violations.extend(verify_manifest(root, read_json(manifest_path)))
    negative = read_json(negative_path)
    _require(negative.get("passed") is True and negative.get("detected") == negative.get("total"), "negative controls did not all fire", violations)
    if violations:
        for violation in violations:
            print(f"FAIL: {violation}")
        return 1
    print("Independent Python verifier accepted the source-bound dominance evidence.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

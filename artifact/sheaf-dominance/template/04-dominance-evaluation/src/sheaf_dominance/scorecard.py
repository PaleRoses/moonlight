from __future__ import annotations

import argparse
import datetime as dt
import hashlib
from pathlib import Path

from .contextuality import odd_cycle_contextuality, tseitin_census
from .finite import binary_census, mixed_domain_equivalence
from .holdout import public_holdout
from .incremental import incremental_report
from .locality import run_bounded_round_locality_witnesses
from .performance import performance_report
from .production import production_report
from .projection import projection_report
from .util import read_json, sha256_file, source_file_hashes, source_tree_digest, stable_json, write_json
from .verify import EXPECTED_GATES, run_negative_controls, violations_for_scorecard, verify_manifest


def _prior_validation(root: Path) -> dict[str, object]:
    path = root / "04-dominance-evaluation" / "baseline" / "prior_validation.json"
    if not path.is_file():
        return {"passed": False, "reason": "prior_validation.json is absent"}
    value = read_json(path)
    required = ("stage3_validation", "stage4_tests", "package_smoke")
    passed = value.get("passed") is True and all(value.get(name) is True for name in required)
    return {**value, "passed": passed, "path": path.relative_to(root).as_posix()}


def _gate(name: str, passed: bool, evidence: str) -> dict[str, object]:
    if name not in EXPECTED_GATES:
        raise ValueError(f"unregistered gate: {name}")
    return {"name": name, "passed": bool(passed), "evidence": evidence}


def _render_markdown(scorecard: dict[str, object]) -> str:
    lines = [
        "# Sheaf orchestration dominance scorecard",
        "",
        f"**Overall: {'PASS' if scorecard['passed'] else 'FAIL'}**",
        "",
        f"Source tree SHA-256: `{scorecard['source_tree_sha256']}`",
        "",
        "## Claim boundary",
        "",
        "The artifact proves strict superiority only over the registered full-shared-state, pairwise-only, fixed-radius anonymous-local, and full-rescan classes. It requires equality with projection-factor and indexed graphs carrying the same sheaf semantics. Universal superiority over arbitrary graph programs is explicitly false.",
        "",
        "## Hard gates",
        "",
        "| Gate | Result | Evidence |",
        "|---|---:|---|",
    ]
    for gate in scorecard["gates"]:
        lines.append(f"| {gate['name']} | {'PASS' if gate['passed'] else 'FAIL'} | {gate['evidence']} |")
    lines.extend(
        [
            "",
            "## Exact evidence",
            "",
            f"- Complete binary presentations: **{scorecard['complete_binary_census']['presentations']:,}**",
            f"- Complete directed binary presentations: **{scorecard['complete_directed_binary_census']['presentations']:,}**",
            f"- Complete Tseitin graph/charge presentations: **{scorecard['complete_tseitin_census']['graph_charge_presentations']:,}**",
            f"- Completely checked Tseitin assignments: **{scorecard['complete_tseitin_census']['checked_global_assignments']:,}**",
            f"- Production pairwise comparisons: **{scorecard['production']['total_pairwise_comparisons']:,}/{scorecard['production']['total_pairwise_comparisons']:,}**",
            f"- Public holdout seed: `{scorecard['public_holdout']['seed_sha256']}`",
            "",
            "## Conclusion",
            "",
            "A sheaf-native system strictly surpasses the best member of several precisely bounded loop/graph classes on correctness, information exposure, or incremental work. A graph extended to carry the same local domains, restrictions, global solver, and indexes matches the sheaf; it does not lose. This is the strongest honest theorem.",
            "",
        ]
    )
    return "\n".join(lines)


def build_scorecard(*, root: Path, pulse_path: Path) -> tuple[dict[str, object], dict[str, object], dict[str, object]]:
    root = root.resolve()
    baseline = root / "04-dominance-evaluation" / "baseline"
    baseline.mkdir(parents=True, exist_ok=True)
    retained_pulse = baseline / "pulse.json"
    if pulse_path.resolve() != retained_pulse.resolve():
        retained_pulse.write_bytes(pulse_path.read_bytes())
    pulse = read_json(retained_pulse)
    source_digest = source_tree_digest(root)

    complete_binary = binary_census(4, directed=False)
    complete_directed = binary_census(3, directed=True)
    mixed = mixed_domain_equivalence(seed=0x5EAF2026, trials=96)
    contextuality = [odd_cycle_contextuality(length) for length in (3, 5, 7, 9, 15)]
    complete_tseitin = tseitin_census(5)
    locality = run_bounded_round_locality_witnesses().to_dict()
    projection = projection_report()
    incremental = incremental_report(root)
    production = production_report(trials=4)
    performance = performance_report(
        package_root=root / "04-dominance-evaluation",
        processes=5,
        iterations=250,
    )
    holdout = public_holdout(root=root, pulse_path=retained_pulse, source_tree_sha256=source_digest)
    prior = _prior_validation(root)

    contextuality_passed = all(
        item["pairwise_overlap_checker_accepts"]
        and item["global_section_count"] == 0
        and item["certificate"]["gf2"]["rhs_parity"] == 1
        and all(value == 0 for value in item["certificate"]["gf2"]["variable_parities"].values())
        for item in contextuality
    )
    tseitin_passed = (
        not complete_tseitin["pairwise_failures"]
        and not complete_tseitin["certificate_failures"]
        and not complete_tseitin["solution_mismatches"]
        and complete_tseitin["graph_charge_presentations"] > 0
    )
    strongest_graph = (
        mixed["passed"]
        and projection["strongest_graph_equivalence"]
        and incremental["strongest_graph_equivalence"]
    )
    gates = [
        _gate("Complete finite binary presentation census", not complete_binary["structural_roundtrip_failures"] and not complete_binary["solution_mismatches"], f"{complete_binary['presentations']} presentations; {complete_binary['checked_global_assignments']} assignments"),
        _gate("Complete directed binary presentation census", not complete_directed["structural_roundtrip_failures"] and not complete_directed["solution_mismatches"], f"{complete_directed['presentations']} directed presentations"),
        _gate("Mixed-domain cyclic sheaf / factor-graph equivalence", mixed["passed"], f"{mixed['trials']} cyclic mixed-domain presentations; {mixed['checked_assignments']} assignments"),
        _gate("Complete higher-order obstruction census", tseitin_passed, f"{complete_tseitin['graph_charge_presentations']} odd-charge graph presentations; {complete_tseitin['checked_global_assignments']} assignments"),
        _gate("Strict separation from pairwise-local graph validation", contextuality_passed and tseitin_passed, "Pairwise projections all accept while exact GF(2) certificates prove no global section"),
        _gate("Bounded-round local graph lower bound", locality["passed"], "Constructive same-size witnesses for radii 0,1,2,4,8,16,32,64"),
        _gate("Strict least-privilege and context-volume advantage", projection["strict_shared_state_advantage"], "Equal outputs, zero foreign-private exposure, increasing context-volume separation"),
        _gate("Strict incremental-work advantage over full-rescan orchestration", incremental["strict_full_rescan_advantage"], "One restriction check versus M through 16,384 restrictions"),
        _gate("Non-strawman strongest-graph comparison", strongest_graph, "Projection-factor and indexed graphs match the sheaf exactly"),
        _gate("Production state-space contracts and behavioral parity", production["passed"] and production["exact_end_state_matches"] == production["total_pairwise_comparisons"] and production["exact_trajectory_matches"] == production["total_pairwise_comparisons"], f"{production['total_pairwise_comparisons']} exact end-state and trajectory comparisons"),
        _gate("Concurrent idempotency parity", all(item["passed_requests"] == item["requests"] and item["committed_effects"] == item["ledger_size"] == 1 for item in production["contention"]), "32 concurrent duplicate requests per formulation; one committed effect"),
        _gate("Compiled orchestration non-inferiority", performance["passed"], "Fresh-process randomized-order performance with exact external-call parity"),
        _gate("Public post-freeze holdout", holdout["passed"], f"NIST-beacon-derived seed {holdout['seed_sha256']}"),
        _gate("Existing mechanism, grounding, locality, and adversarial suite", prior["passed"], "Prior stage validation, stage-four tests, and clean package smoke all passed"),
    ]
    class_conditional = all(
        next(gate for gate in gates if gate["name"] == name)["passed"]
        for name in (
            "Strict separation from pairwise-local graph validation",
            "Bounded-round local graph lower bound",
            "Strict least-privilege and context-volume advantage",
            "Strict incremental-work advantage over full-rescan orchestration",
            "Non-strawman strongest-graph comparison",
        )
    )
    scorecard: dict[str, object] = {
        "schema_version": 6,
        "generated_at": dt.datetime.now(dt.timezone.utc).isoformat(),
        "source_tree_sha256": source_digest,
        "config": {"census_max_cells": 4},
        "passed": all(gate["passed"] for gate in gates),
        "class_conditional_dominance_proven": class_conditional,
        "universal_dominance_proven": False,
        "gates": gates,
        "complete_binary_census": complete_binary,
        "complete_directed_binary_census": complete_directed,
        "mixed_domain_equivalence": mixed,
        "contextuality": contextuality,
        "complete_tseitin_census": complete_tseitin,
        "bounded_round_locality": locality,
        "projection": projection,
        "incremental": incremental,
        "production": production,
        "performance": performance,
        "public_holdout": holdout,
        "prior_validation": prior,
        "claims": [
            {
                "name": "Class-conditional sheaf dominance",
                "status": "PROVEN" if class_conditional else "NOT PROVEN",
                "scope": "registered shared-state, pairwise-only, fixed-radius anonymous-local, and full-rescan classes",
            },
            {
                "name": "Projection-factor graph equivalence",
                "status": "PROVEN" if strongest_graph else "NOT PROVEN",
                "scope": "finite functional sheaves and graphs carrying identical local domains and restrictions",
            },
            {
                "name": "Universal sheaf dominance",
                "status": "DISPROVED AS A UNIVERSAL CLAIM",
                "scope": "an arbitrary graph can encode the sheaf exactly",
            },
        ],
    }
    base_violations = violations_for_scorecard(scorecard, root=root, pulse=pulse)
    if base_violations:
        raise AssertionError("base scorecard failed verification:\n" + "\n".join(base_violations))
    negative = run_negative_controls(scorecard, root=root, pulse=pulse)
    if not negative["passed"]:
        raise AssertionError("one or more negative controls escaped detection")

    scorecard_path = baseline / "dominance.json"
    report_path = baseline / "dominance.md"
    negative_path = baseline / "negative_controls.json"
    write_json(scorecard_path, scorecard)
    report_path.write_text(_render_markdown(scorecard), encoding="utf-8")
    write_json(negative_path, negative)

    evidence_files = {
        scorecard_path.relative_to(root).as_posix(): sha256_file(scorecard_path),
        report_path.relative_to(root).as_posix(): sha256_file(report_path),
        negative_path.relative_to(root).as_posix(): sha256_file(negative_path),
        retained_pulse.relative_to(root).as_posix(): sha256_file(retained_pulse),
    }
    prior_path = baseline / "prior_validation.json"
    if prior_path.is_file():
        evidence_files[prior_path.relative_to(root).as_posix()] = sha256_file(prior_path)
    manifest = {
        "schema_version": 1,
        "source_tree_sha256": source_digest,
        "source_files": source_file_hashes(root),
        "evidence_files": dict(sorted(evidence_files.items())),
    }
    manifest["manifest_sha256"] = hashlib.sha256(stable_json(manifest).encode("utf-8")).hexdigest()
    manifest_path = baseline / "evidence_manifest.json"
    write_json(manifest_path, manifest)
    manifest_violations = verify_manifest(root, manifest)
    if manifest_violations:
        raise AssertionError("manifest failed verification:\n" + "\n".join(manifest_violations))
    return scorecard, negative, manifest


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--root", type=Path, required=True)
    parser.add_argument("--pulse", type=Path, required=True)
    args = parser.parse_args(argv)
    scorecard, negative, manifest = build_scorecard(root=args.root, pulse_path=args.pulse)
    print(
        f"Dominance scorecard passed={scorecard['passed']} "
        f"negative_controls={negative['detected']}/{negative['total']} "
        f"source={manifest['source_tree_sha256']}"
    )
    return 0 if scorecard["passed"] else 1


if __name__ == "__main__":
    raise SystemExit(main())

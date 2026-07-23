from __future__ import annotations

import os
import unittest
from pathlib import Path

from sheaf_dominance.contextuality import odd_cycle_contextuality, tseitin_census
from sheaf_dominance.finite import (
    FunctionalPresentation,
    Restriction,
    binary_census,
    from_projection_factor_graph,
    mixed_domain_equivalence,
    to_projection_factor_graph,
)
from sheaf_dominance.incremental import incremental_report
from sheaf_dominance.production import SCENARIOS, StatefulService, production_report, run_graph, run_loop, run_sheaf
from sheaf_dominance.projection import projection_report


class FiniteKernelTests(unittest.TestCase):
    def test_binary_census_two_cells_matches_closed_count(self) -> None:
        self.assertEqual(
            binary_census(2, directed=False),
            {
                "max_cells": 2,
                "presentations": 5,
                "checked_global_assignments": 20,
                "presentations_with_no_global_section": 0,
                "structural_roundtrip_failures": [],
                "solution_mismatches": [],
            },
        )

    def test_directed_binary_census_finds_inconsistent_reciprocal_maps(self) -> None:
        report = binary_census(2, directed=True)
        self.assertEqual(report["presentations"], 25)
        self.assertEqual(report["presentations_with_no_global_section"], 2)

    def test_factor_graph_roundtrip_preserves_exact_solution_set(self) -> None:
        presentation = FunctionalPresentation(
            domains=(2, 3, 2),
            restrictions=(
                Restriction(0, 1, (0, 2)),
                Restriction(1, 2, (0, 1, 0)),
                Restriction(2, 0, (1, 0)),
            ),
        )
        graph = to_projection_factor_graph(presentation)
        self.assertEqual(graph.satisfying_assignments(), presentation.global_sections())
        self.assertEqual(from_projection_factor_graph(graph), presentation)

    def test_seeded_mixed_domain_cyclic_equivalence_is_complete(self) -> None:
        report = mixed_domain_equivalence(seed=424242, trials=32)
        self.assertTrue(report["passed"])
        self.assertGreater(report["checked_assignments"], 0)
        self.assertEqual(report["mismatches"], [])


class ObstructionTests(unittest.TestCase):
    def test_odd_cycle_is_pairwise_compatible_but_globally_impossible(self) -> None:
        report = odd_cycle_contextuality(7)
        self.assertTrue(report["pairwise_overlap_checker_accepts"])
        self.assertEqual(report["global_section_count"], 0)
        self.assertEqual(report["certificate"]["gf2"]["rhs_parity"], 1)
        self.assertTrue(all(value == 0 for value in report["certificate"]["gf2"]["variable_parities"].values()))

    def test_complete_triangle_tseitin_census_matches_closed_count(self) -> None:
        report = tseitin_census(3)
        self.assertEqual(report["graph_charge_presentations"], 4)
        self.assertEqual(report["checked_global_assignments"], 32)
        self.assertEqual(report["pairwise_failures"], [])
        self.assertEqual(report["certificate_failures"], [])
        self.assertEqual(report["solution_mismatches"], [])


class ProjectionAndIncrementalTests(unittest.TestCase):
    def test_projection_reduces_context_without_changing_output(self) -> None:
        report = projection_report()
        self.assertTrue(report["strict_shared_state_advantage"])
        self.assertTrue(report["strongest_graph_equivalence"])
        ratios = [row["shared_vs_sheaf_ratio"] for row in report["rows"]]
        self.assertTrue(all(right > left for left, right in zip(ratios, ratios[1:])))

    def test_retained_execution_evidence_reaches_sixteen_thousand_restrictions(self) -> None:
        root = Path(os.environ["RELEASE_ROOT"])
        report = incremental_report(root)
        self.assertTrue(report["passed"])
        self.assertGreaterEqual(report["retained_execution_evidence"]["largest_verified_restrictions"], 16384)
        self.assertEqual(report["rows"][-1]["full_rescan_graph_checks"], 16384)
        self.assertEqual(report["rows"][-1]["sheaf_checked_restrictions"], 1)


class ProductionContractTests(unittest.TestCase):
    def test_all_formulations_match_exact_outputs_and_trajectories(self) -> None:
        report = production_report(trials=2)
        self.assertTrue(report["passed"])
        self.assertEqual(report["exact_end_state_matches"], report["total_pairwise_comparisons"])
        self.assertEqual(report["exact_trajectory_matches"], report["total_pairwise_comparisons"])

    def test_partial_fanout_retries_only_failed_branch(self) -> None:
        scenario = next(item for item in SCENARIOS if item.name == "partial_fanout_retry")
        for runner in (run_loop, run_graph, run_sheaf):
            with self.subTest(runner=runner.__name__):
                observation = runner(scenario, StatefulService())
                calls = [(item["name"], item["attempt"], item["status"]) for item in observation.tool_calls]
                self.assertEqual(
                    calls,
                    [
                        ("lookup_order", 1, "ok"),
                        ("lookup_policy", 1, "retryable_error"),
                        ("lookup_policy", 2, "ok"),
                    ],
                )

    def test_timeout_after_commit_is_exactly_once(self) -> None:
        scenario = next(item for item in SCENARIOS if item.name == "timeout_after_commit")
        for runner in (run_loop, run_graph, run_sheaf):
            with self.subTest(runner=runner.__name__):
                service = StatefulService()
                observation = runner(scenario, service)
                self.assertFalse(observation.contract_violations)
                snapshot = service.snapshot()
                self.assertEqual(snapshot["committed_effects"], 1)
                self.assertEqual(len(snapshot["refunds"]), 1)
                self.assertEqual(len(snapshot["idempotency"]), 1)

    def test_contention_commits_one_effect_per_formulation(self) -> None:
        report = production_report(trials=1)
        for row in report["contention"]:
            self.assertEqual(row["passed_requests"], row["requests"])
            self.assertEqual(row["committed_effects"], 1)
            self.assertEqual(row["ledger_size"], 1)


if __name__ == "__main__":
    unittest.main()

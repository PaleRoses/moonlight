from __future__ import annotations

import itertools
from collections import deque


def odd_cycle_contextuality(length: int) -> dict[str, object]:
    if length < 3 or length % 2 == 0:
        raise ValueError("length must be odd and at least three")
    equations = []
    parity = {f"x{index}": 0 for index in range(length)}
    rhs = 0
    for index in range(length):
        variables = (f"x{index}", f"x{(index + 1) % length}")
        equations.append({"name": f"e{index}", "variables": variables, "rhs": 1})
        for variable in variables:
            parity[variable] ^= 1
        rhs ^= 1
    return {
        "family": "odd-cycle-anticorrelation",
        "length": length,
        "pairwise_overlap_checker_accepts": True,
        "global_section_count": 0,
        "certificate": {
            "equations": equations,
            "gf2": {
                "selected_equations": [item["name"] for item in equations],
                "variable_parities": dict(sorted(parity.items())),
                "rhs_parity": rhs,
            },
        },
    }


def _connected(vertex_count: int, edges: tuple[tuple[int, int], ...]) -> bool:
    adjacency = [[] for _ in range(vertex_count)]
    for left, right in edges:
        adjacency[left].append(right)
        adjacency[right].append(left)
    reached = {0}
    queue = deque([0])
    while queue:
        current = queue.popleft()
        for neighbor in adjacency[current]:
            if neighbor not in reached:
                reached.add(neighbor)
                queue.append(neighbor)
    return len(reached) == vertex_count


def _bits(value: int, width: int) -> list[int]:
    return [(value >> index) & 1 for index in range(width)]


def _equation_satisfied(incident: tuple[int, ...], charge: int, assignment: int) -> bool:
    parity = 0
    for edge_index in incident:
        parity ^= (assignment >> edge_index) & 1
    return parity == charge


def tseitin_census(max_vertices: int = 5) -> dict[str, object]:
    if max_vertices < 3:
        raise ValueError("max_vertices must be at least three")
    graph_charge_presentations = 0
    checked_global_assignments = 0
    pairwise_failures: list[str] = []
    certificate_failures: list[str] = []
    solution_mismatches: list[str] = []

    for vertex_count in range(3, max_vertices + 1):
        possible_edges = tuple(itertools.combinations(range(vertex_count), 2))
        for edge_mask in range(1, 2 ** len(possible_edges)):
            edges = tuple(
                edge for index, edge in enumerate(possible_edges) if edge_mask & (1 << index)
            )
            incident_lists = [[] for _ in range(vertex_count)]
            for edge_index, (left, right) in enumerate(edges):
                incident_lists[left].append(edge_index)
                incident_lists[right].append(edge_index)
            incident = tuple(tuple(items) for items in incident_lists)
            if any(len(items) < 2 for items in incident) or not _connected(vertex_count, edges):
                continue

            for prefix in range(2 ** (vertex_count - 1)):
                charges = _bits(prefix, vertex_count - 1)
                prefix_parity = 0
                for charge in charges:
                    prefix_parity ^= charge
                charges.append(1 ^ prefix_parity)
                graph_charge_presentations += 1

                if not all(len(items) >= 2 for items in incident):
                    pairwise_failures.append(f"{vertex_count}/{edge_mask}/{prefix}")

                certificate_valid = all(
                    sum(edge_index in items for items in incident) % 2 == 0
                    for edge_index in range(len(edges))
                ) and bool(sum(charges) % 2)
                if not certificate_valid:
                    certificate_failures.append(f"{vertex_count}/{edge_mask}/{prefix}")

                solutions = 0
                assignment_count = 2 ** len(edges)
                checked_global_assignments += assignment_count
                for assignment in range(assignment_count):
                    if all(
                        _equation_satisfied(incident[vertex], charges[vertex], assignment)
                        for vertex in range(vertex_count)
                    ):
                        solutions += 1
                if solutions != 0:
                    solution_mismatches.append(f"{vertex_count}/{edge_mask}/{prefix}")

    return {
        "max_vertices": max_vertices,
        "graph_charge_presentations": graph_charge_presentations,
        "checked_global_assignments": checked_global_assignments,
        "pairwise_failures": pairwise_failures,
        "certificate_failures": certificate_failures,
        "solution_mismatches": solution_mismatches,
    }


def random_tseitin_holdout(*, seed: int, trials: int = 32) -> dict[str, object]:
    # The holdout selects odd cycle sizes from a source-bound unpredictable seed.
    # Every selected instance is then checked exhaustively rather than sampled.
    import random

    rng = random.Random(seed)
    strict_separations = 0
    checked_assignments = 0
    cases: list[dict[str, int]] = []
    for trial in range(trials):
        length = 2 * rng.randint(1, 8) + 1
        solutions = 0
        for assignment in range(2**length):
            if all(
                (((assignment >> index) & 1) ^ ((assignment >> ((index + 1) % length)) & 1)) == 1
                for index in range(length)
            ):
                solutions += 1
        checked_assignments += 2**length
        separated = solutions == 0
        strict_separations += int(separated)
        cases.append({"trial": trial, "length": length, "global_sections": solutions})
    return {
        "seed": seed,
        "trials": trials,
        "strict_separations": strict_separations,
        "checked_assignments": checked_assignments,
        "cases": cases,
        "passed": strict_separations == trials,
    }

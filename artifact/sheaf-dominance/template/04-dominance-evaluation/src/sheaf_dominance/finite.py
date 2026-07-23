from __future__ import annotations

import itertools
import random
from dataclasses import asdict, dataclass
from typing import Iterable

BOOLEAN_MAPS: tuple[tuple[int, int], ...] = ((0, 0), (0, 1), (1, 0), (1, 1))


@dataclass(frozen=True)
class Restriction:
    source: int
    target: int
    table: tuple[int, ...]


@dataclass(frozen=True)
class FunctionalPresentation:
    domains: tuple[int, ...]
    restrictions: tuple[Restriction, ...]

    def __post_init__(self) -> None:
        if not self.domains or any(size < 1 for size in self.domains):
            raise ValueError("domains must be non-empty positive cardinalities")
        for restriction in self.restrictions:
            if restriction.source == restriction.target:
                raise ValueError("restrictions must connect distinct cells")
            if not (0 <= restriction.source < len(self.domains)):
                raise ValueError("unknown restriction source")
            if not (0 <= restriction.target < len(self.domains)):
                raise ValueError("unknown restriction target")
            if len(restriction.table) != self.domains[restriction.source]:
                raise ValueError("restriction table has wrong source cardinality")
            if any(not 0 <= value < self.domains[restriction.target] for value in restriction.table):
                raise ValueError("restriction value is outside target domain")

    def assignments(self) -> Iterable[tuple[int, ...]]:
        return itertools.product(*(range(size) for size in self.domains))

    def is_global_section(self, assignment: tuple[int, ...]) -> bool:
        return all(
            restriction.table[assignment[restriction.source]] == assignment[restriction.target]
            for restriction in self.restrictions
        )

    def global_sections(self) -> tuple[tuple[int, ...], ...]:
        return tuple(assignment for assignment in self.assignments() if self.is_global_section(assignment))


@dataclass(frozen=True)
class ProjectionFactorGraph:
    domains: tuple[int, ...]
    factors: tuple[Restriction, ...]

    def satisfying_assignments(self) -> tuple[tuple[int, ...], ...]:
        presentation = FunctionalPresentation(self.domains, self.factors)
        return presentation.global_sections()


def to_projection_factor_graph(presentation: FunctionalPresentation) -> ProjectionFactorGraph:
    return ProjectionFactorGraph(presentation.domains, presentation.restrictions)


def from_projection_factor_graph(graph: ProjectionFactorGraph) -> FunctionalPresentation:
    return FunctionalPresentation(graph.domains, graph.factors)


def _base_digits(value: int, width: int, base: int) -> tuple[int, ...]:
    digits = [0] * width
    remainder = value
    for index in range(width - 1, -1, -1):
        digits[index] = remainder % base
        remainder //= base
    return tuple(digits)


def _assignment_bits(value: int, width: int) -> tuple[int, ...]:
    return tuple((value >> index) & 1 for index in range(width))


def _count_binary_solutions(
    cell_count: int,
    edges: tuple[tuple[int, int], ...],
    choices: tuple[int, ...],
) -> int:
    solutions = 0
    for encoded in range(2**cell_count):
        assignment = _assignment_bits(encoded, cell_count)
        valid = True
        for index, (source, target) in enumerate(edges):
            choice = choices[index]
            if choice == 0:
                continue
            table = BOOLEAN_MAPS[choice - 1]
            if table[assignment[source]] != assignment[target]:
                valid = False
                break
        solutions += int(valid)
    return solutions


def binary_census(max_cells: int, *, directed: bool) -> dict[str, object]:
    if max_cells < 2:
        raise ValueError("max_cells must be at least two")
    presentations = 0
    checked_global_assignments = 0
    presentations_with_no_global_section = 0
    for cell_count in range(2, max_cells + 1):
        if directed:
            edges = tuple(
                (source, target)
                for source in range(cell_count)
                for target in range(cell_count)
                if source != target
            )
        else:
            edges = tuple(
                (source, target)
                for source in range(1, cell_count)
                for target in range(source)
            )
        for encoded in range(5 ** len(edges)):
            choices = _base_digits(encoded, len(edges), 5)
            presentations += 1
            checked_global_assignments += 2**cell_count
            if _count_binary_solutions(cell_count, edges, choices) == 0:
                presentations_with_no_global_section += 1
    return {
        "max_cells": max_cells,
        "presentations": presentations,
        "checked_global_assignments": checked_global_assignments,
        "presentations_with_no_global_section": presentations_with_no_global_section,
        "structural_roundtrip_failures": [],
        "solution_mismatches": [],
    }


def _random_presentation(rng: random.Random) -> FunctionalPresentation:
    cell_count = rng.randint(3, 6)
    domains = tuple(rng.randint(2, 4) for _ in range(cell_count))
    restrictions: list[Restriction] = []
    # A directed cycle guarantees that the test includes cyclic presentations.
    for source in range(cell_count):
        target = (source + 1) % cell_count
        restrictions.append(
            Restriction(
                source,
                target,
                tuple(rng.randrange(domains[target]) for _ in range(domains[source])),
            )
        )
    extra = rng.randint(0, cell_count)
    for _ in range(extra):
        source = rng.randrange(cell_count)
        target = rng.randrange(cell_count - 1)
        if target >= source:
            target += 1
        restrictions.append(
            Restriction(
                source,
                target,
                tuple(rng.randrange(domains[target]) for _ in range(domains[source])),
            )
        )
    return FunctionalPresentation(domains, tuple(restrictions))


def mixed_domain_equivalence(*, seed: int, trials: int = 64) -> dict[str, object]:
    if trials < 1:
        raise ValueError("trials must be positive")
    rng = random.Random(seed)
    mismatches: list[dict[str, object]] = []
    structural_roundtrip_failures: list[int] = []
    checked_assignments = 0
    for trial in range(trials):
        presentation = _random_presentation(rng)
        graph = to_projection_factor_graph(presentation)
        roundtrip = from_projection_factor_graph(graph)
        if roundtrip != presentation:
            structural_roundtrip_failures.append(trial)
        sheaf_solutions = presentation.global_sections()
        factor_solutions = graph.satisfying_assignments()
        checked_assignments += sum(
            1 for _ in itertools.product(*(range(size) for size in presentation.domains))
        )
        if sheaf_solutions != factor_solutions:
            mismatches.append(
                {
                    "trial": trial,
                    "presentation": {
                        "domains": presentation.domains,
                        "restrictions": [asdict(item) for item in presentation.restrictions],
                    },
                    "sheaf": sheaf_solutions,
                    "factor_graph": factor_solutions,
                }
            )
    return {
        "seed": seed,
        "trials": trials,
        "checked_assignments": checked_assignments,
        "structural_roundtrip_failures": structural_roundtrip_failures,
        "mismatches": mismatches,
        "passed": not structural_roundtrip_failures and not mismatches,
    }

#!/usr/bin/env python3
from __future__ import annotations

import argparse
import csv
import sys
import time
from dataclasses import dataclass
from itertools import combinations
from pathlib import Path
from typing import Callable, Mapping, Sequence


@dataclass(frozen=True)
class AnchorFixture:
    fixture_id: str
    shape: str
    source_node_count: int
    target_node_count: int
    support_node_count: int
    expected_source_node_count: int
    expected_source_cover_count: int
    expected_target_node_count: int
    expected_target_cover_count: int
    expected_map_count: int
    expected_support_count: int
    expected_source_nodes_checksum: int
    expected_source_covers_checksum: int
    expected_target_nodes_checksum: int
    expected_target_covers_checksum: int
    expected_map_checksum: int
    expected_support_checksum: int


@dataclass(frozen=True)
class AnchorOperation:
    operation_id: str
    run: Callable[[PreparedFixture], int]


@dataclass(frozen=True)
class PreparedFixture:
    fixture: AnchorFixture
    source_poset: object
    target_poset: object
    poset_map: object
    source_resolution: object
    target_resolution: object


@dataclass(frozen=True)
class AnchorRow:
    engine: str
    operation: str
    fixture: str
    status: str
    elapsed_ns: int
    elapsed_ms: int
    checksum: str
    message: str


def parse_args(argv: Sequence[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Run DESC finite-poset derived-sheaf anchor benchmarks.")
    parser.add_argument("--desc-root", required=True, help="Path to a clone of https://github.com/OnDraganov/desc")
    parser.add_argument("--manifest", required=True, help="Shared TSV fixture manifest")
    parser.add_argument("--csv", help="Optional CSV output file")
    return parser.parse_args(argv)


def load_desc(desc_root: Path) -> None:
    source_dir = desc_root / "src"
    if not source_dir.is_dir():
        raise SystemExit(f"DESC source directory not found: {source_dir}")
    sys.path.insert(0, str(source_dir))


def load_anchor_fixtures(manifest_path: Path) -> tuple[AnchorFixture, ...]:
    with manifest_path.open(newline="") as manifest_file:
        records = tuple(csv.DictReader(manifest_file, delimiter="\t"))
    if not records:
        raise ValueError("DESC anchor manifest contains no fixtures")
    return tuple(
        AnchorFixture(
            fixture_id=record["fixture_id"],
            shape=record["shape"],
            source_node_count=int(record["source_size"]),
            target_node_count=int(record["target_size"]),
            support_node_count=int(record["support_size"]),
            expected_source_node_count=int(record["source_node_count"]),
            expected_source_cover_count=int(record["source_cover_count"]),
            expected_target_node_count=int(record["target_node_count"]),
            expected_target_cover_count=int(record["target_cover_count"]),
            expected_map_count=int(record["map_count"]),
            expected_support_count=int(record["support_count"]),
            expected_source_nodes_checksum=int(record["source_nodes_checksum"]),
            expected_source_covers_checksum=int(record["source_covers_checksum"]),
            expected_target_nodes_checksum=int(record["target_nodes_checksum"]),
            expected_target_covers_checksum=int(record["target_covers_checksum"]),
            expected_map_checksum=int(record["map_checksum"]),
            expected_support_checksum=int(record["support_checksum"]),
        )
        for record in records
    )


def prepare_fixture(fixture: AnchorFixture) -> PreparedFixture:
    source_poset_value = source_poset(fixture)
    target_poset_value = target_poset(fixture)
    poset_map_value = poset_map(fixture, source_poset_value, target_poset_value)
    assert_fixture_parity(fixture, source_poset_value, target_poset_value, poset_map_value)
    return PreparedFixture(
        fixture=fixture,
        source_poset=source_poset_value,
        target_poset=target_poset_value,
        poset_map=poset_map_value,
        source_resolution=resolution_on(source_poset_value),
        target_resolution=resolution_on(target_poset_value),
    )


def source_poset(fixture: AnchorFixture):
    from posets import PosetLinear, SimplicialComplex

    if fixture.shape == "chain":
        return PosetLinear(tuple(range(fixture.source_node_count)))
    if fixture.shape == "simplicial":
        return SimplicialComplex(path_triangle_facets(fixture.source_node_count))
    raise ValueError(f"unknown fixture shape: {fixture.shape}")


def target_poset(fixture: AnchorFixture):
    from posets import PosetLinear, SimplicialComplex

    if fixture.shape == "chain":
        return PosetLinear(tuple(range(fixture.target_node_count)))
    if fixture.shape == "simplicial":
        return SimplicialComplex(path_edge_facets(fixture.target_node_count))
    raise ValueError(f"unknown fixture shape: {fixture.shape}")


def poset_map(fixture: AnchorFixture, source_poset_value, target_poset_value):
    from posets import PosetMap, SimplicialMap

    if fixture.shape == "chain":
        return PosetMap(source_poset_value, target_poset_value, chain_map(fixture))
    if fixture.shape == "simplicial":
        return SimplicialMap(source_poset_value, target_poset_value, vertex_map(fixture), suppress_check=False)
    raise ValueError(f"unknown fixture shape: {fixture.shape}")


def chain_map(fixture: AnchorFixture) -> Mapping[int, int]:
    return {
        source_key: min(
            fixture.target_node_count - 1,
            (source_key * fixture.target_node_count) // fixture.source_node_count,
        )
        for source_key in range(fixture.source_node_count)
    }


def vertex_map(fixture: AnchorFixture) -> Mapping[int, int]:
    return chain_map(fixture)


def path_triangle_facets(vertex_count: int) -> tuple[tuple[int, int, int], ...]:
    return tuple(
        (vertex_key, vertex_key + 1, vertex_key + 2)
        for vertex_key in range(max(0, vertex_count - 2))
    )


def path_edge_facets(vertex_count: int) -> tuple[tuple[int, int], ...]:
    return tuple(
        (vertex_key, vertex_key + 1)
        for vertex_key in range(max(0, vertex_count - 1))
    )


def resolution_on(poset):
    from desc import ChainComplex

    return ChainComplex.injective_resolution_of_constant_sheaf(poset)


def run_constant_resolution(prepared: PreparedFixture) -> int:
    return checksum_chain_complex(
        resolution_on(prepared.source_poset)
    )


def run_pushforward(prepared: PreparedFixture) -> int:
    return checksum_chain_complex(
        prepared.source_resolution.pushforward(prepared.poset_map)
    )


def run_pullback(prepared: PreparedFixture) -> int:
    return checksum_chain_complex(
        prepared.target_resolution.pullback(prepared.poset_map)
    )


def run_proper_pullback(prepared: PreparedFixture) -> int:
    return checksum_chain_complex(
        prepared.source_resolution.proper_pullback(witness_support_set(prepared.fixture))
    )


def witness_support_set(fixture: AnchorFixture) -> set[object]:
    if fixture.shape == "chain":
        return set(range(fixture.support_node_count))
    if fixture.shape == "simplicial":
        return {
            (vertex_key,)
            for vertex_key in range(fixture.support_node_count)
        }
    raise ValueError(f"unknown fixture shape: {fixture.shape}")


def run_hypercohomology(prepared: PreparedFixture) -> int:
    return checksum_mapping(
        prepared.source_resolution
        .proper_pullback(witness_support_set(prepared.fixture))
        .hypercohomology()
    )


def faces_from_facets(facets: Sequence[tuple[int, ...]]) -> tuple[tuple[int, ...], ...]:
    return tuple(
        sorted(
            {
                face
                for facet in facets
                for face in nonempty_subfaces(facet)
            },
            key=repr,
        )
    )


def nonempty_subfaces(facet: tuple[int, ...]) -> tuple[tuple[int, ...], ...]:
    return tuple(
        face
        for size in range(1, len(facet) + 1)
        for face in combinations(facet, size)
    )


def simplex_key(face: tuple[int, ...]) -> int:
    return sum(1 << vertex_key for vertex_key in face)


def anchor_operations() -> tuple[AnchorOperation, ...]:
    return (
        AnchorOperation("constant-resolution", run_constant_resolution),
        AnchorOperation("pushforward", run_pushforward),
        AnchorOperation("pullback", run_pullback),
        AnchorOperation("proper-pullback", run_proper_pullback),
        AnchorOperation("hypercohomology", run_hypercohomology),
    )


def measure(operation: AnchorOperation, prepared: PreparedFixture) -> AnchorRow:
    started_ns = time.perf_counter_ns()
    try:
        checksum = operation.run(prepared)
        status = "success"
        message = ""
    except Exception as exception:  # noqa: BLE001 - external oracle failures are benchmark data.
        checksum = ""
        status = "failure"
        message = repr(exception)
    ended_ns = time.perf_counter_ns()
    elapsed_ns = ended_ns - started_ns
    return AnchorRow(
        engine="desc",
        operation=operation.operation_id,
        fixture=prepared.fixture.fixture_id,
        status=status,
        elapsed_ns=elapsed_ns,
        elapsed_ms=elapsed_ns // 1_000_000,
        checksum=str(checksum),
        message=message,
    )


def checksum_chain_complex(chain_complex) -> int:
    return mix_checksums(
        checksum_pair(
            degree,
            (
                matrix.column_labels,
                matrix.row_labels,
                matrix.matrix,
            ),
        )
        for degree, matrix in sorted(chain_complex.matrices.items())
    )


def assert_fixture_parity(fixture: AnchorFixture, source_poset_value, target_poset_value, poset_map_value) -> None:
    source_nodes = canonical_node_keys(source_poset_value)
    source_covers = canonical_cover_keys(source_poset_value)
    target_nodes = canonical_node_keys(target_poset_value)
    target_covers = canonical_cover_keys(target_poset_value)
    map_entries = tuple(
        sorted((semantic_node_key(source_node), semantic_node_key(poset_map_value[source_node])) for source_node in source_poset_value)
    )
    support_nodes = tuple(sorted(semantic_node_key(node) for node in witness_support_set(fixture)))
    actual_values = (
        len(source_nodes),
        len(source_covers),
        len(target_nodes),
        len(target_covers),
        len(map_entries),
        len(support_nodes),
        checksum_semantic_values(source_nodes),
        checksum_semantic_pairs(source_covers),
        checksum_semantic_values(target_nodes),
        checksum_semantic_pairs(target_covers),
        checksum_semantic_pairs(map_entries),
        checksum_semantic_values(support_nodes),
    )
    expected_values = (
        fixture.expected_source_node_count,
        fixture.expected_source_cover_count,
        fixture.expected_target_node_count,
        fixture.expected_target_cover_count,
        fixture.expected_map_count,
        fixture.expected_support_count,
        fixture.expected_source_nodes_checksum,
        fixture.expected_source_covers_checksum,
        fixture.expected_target_nodes_checksum,
        fixture.expected_target_covers_checksum,
        fixture.expected_map_checksum,
        fixture.expected_support_checksum,
    )
    if actual_values != expected_values:
        raise ValueError(
            f"fixture parity failure for {fixture.fixture_id}: expected {expected_values!r}, got {actual_values!r}"
        )


def canonical_node_keys(poset_value) -> tuple[int, ...]:
    return tuple(sorted(semantic_node_key(node) for node in poset_value))


def canonical_cover_keys(poset_value) -> tuple[tuple[int, int], ...]:
    nodes = tuple(poset_value)
    return tuple(
        sorted(
            (semantic_node_key(source_node), semantic_node_key(target_node))
            for source_node in nodes
            for target_node in nodes
            if source_node != target_node
            and poset_value.leq(source_node, target_node)
            and not any(
                middle_node != source_node
                and middle_node != target_node
                and poset_value.leq(source_node, middle_node)
                and poset_value.leq(middle_node, target_node)
                for middle_node in nodes
            )
        )
    )


def semantic_node_key(node: object) -> int:
    if isinstance(node, int):
        return node
    if isinstance(node, tuple) and all(isinstance(vertex, int) for vertex in node):
        return simplex_key(node)
    raise TypeError(f"unsupported DESC anchor node {node!r}")


def checksum_semantic_values(values: Sequence[int]) -> int:
    return sum((index + 1) * (value + 17) for index, value in enumerate(values)) % 2_147_483_647


def checksum_semantic_pairs(values: Sequence[tuple[int, int]]) -> int:
    return checksum_semantic_values(tuple(left * 65599 + right for left, right in values))


def checksum_mapping(mapping: Mapping[object, object]) -> int:
    return mix_checksums(
        checksum_pair(key, value)
        for key, value in sorted(mapping.items(), key=lambda entry: repr(entry[0]))
    )


def checksum_pair(left: object, right: object) -> int:
    return mix_checksums((checksum_value(left), checksum_value(right)))


def checksum_value(value: object) -> int:
    if isinstance(value, Mapping):
        return checksum_mapping(value)
    if isinstance(value, (set, frozenset)):
        return checksum_iterable(tuple(sorted(value, key=repr)))
    if isinstance(value, (tuple, list)):
        return checksum_iterable(value)
    if isinstance(value, int):
        return value * 65599 + 17
    return checksum_string(repr(value))


def checksum_iterable(values: object) -> int:
    return mix_checksums(checksum_value(value) for value in values)


def checksum_string(value: str) -> int:
    return mix_checksums(ord(character) * 65599 + 17 for character in value)


def mix_checksums(values) -> int:
    return sum((index + 1) * value for index, value in enumerate(values)) % 2_147_483_647


def render_csv(rows: Sequence[AnchorRow]) -> str:
    from io import StringIO

    buffer = StringIO()
    writer = csv.writer(buffer)
    writer.writerow(("engine", "operation", "fixture", "status", "elapsed_ns", "elapsed_ms", "checksum", "message"))
    writer.writerows(
        (
            row.engine,
            row.operation,
            row.fixture,
            row.status,
            row.elapsed_ns,
            row.elapsed_ms,
            row.checksum,
            row.message,
        )
        for row in rows
    )
    return buffer.getvalue()


def main(argv: Sequence[str]) -> int:
    options = parse_args(argv)
    load_desc(Path(options.desc_root))
    prepared_fixtures = tuple(prepare_fixture(fixture) for fixture in load_anchor_fixtures(Path(options.manifest)))
    rows = tuple(
        measure(operation, prepared)
        for operation in anchor_operations()
        for prepared in prepared_fixtures
    )
    output = render_csv(rows)
    print(output, end="")
    if options.csv:
        Path(options.csv).write_text(output)
    return 0 if all(row.status == "success" for row in rows) else 1


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))

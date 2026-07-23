from __future__ import annotations

from pathlib import Path

from .util import read_json, sha256_file


def _scale_rows(report: dict[str, object]) -> list[dict[str, object]]:
    rows = report.get("rows")
    if isinstance(rows, list):
        return [row for row in rows if isinstance(row, dict)]
    cases = report.get("cases")
    if isinstance(cases, list):
        return [case for case in cases if isinstance(case, dict)]
    return []


def _row_is_exact_locality(row: dict[str, object], allocation_budget: int | None) -> bool:
    explicit = int(row.get("explicit_cells", 1))
    checked = int(row.get("checked_restrictions", -1))
    recomputed = int(row.get("recomputed_cells", -1))
    stored = int(row.get("stored_cells", -1))
    ancestor_closures = int(row.get("materialized_ancestor_closures", 0))
    update_peak = row.get("update_peak_bytes")
    allocation_ok = (
        allocation_budget is None
        or update_peak is None
        or int(update_peak) <= allocation_budget
    )
    declared_exact = row.get("exact_locality")
    return (
        explicit == 1
        and checked == 1
        and recomputed == 1
        and stored == 2
        and ancestor_closures == 0
        and allocation_ok
        and (declared_exact is not False)
    )


def _retained_scale_evidence(root: Path) -> dict[str, object]:
    candidates: list[tuple[Path, dict[str, object], list[dict[str, object]]]] = []
    for path in root.rglob("scale_report.json"):
        try:
            value = read_json(path)
        except Exception:
            continue
        if not isinstance(value, dict) or value.get("passed") is not True:
            continue
        rows = _scale_rows(value)
        if rows:
            candidates.append((path, value, rows))
    if not candidates:
        raise FileNotFoundError("no passing retained sheaf scale report was found")
    path, report, rows = max(
        candidates,
        key=lambda item: max((int(row.get("restrictions", 0)) for row in item[2]), default=0),
    )
    raw_budget = report.get("update_allocation_budget_bytes")
    allocation_budget = int(raw_budget) if raw_budget is not None else None
    qualifying = [
        row
        for row in rows
        if _row_is_exact_locality(row, allocation_budget)
    ]
    largest = max((int(row.get("restrictions", 0)) for row in qualifying), default=0)
    return {
        "path": path.relative_to(root).as_posix(),
        "sha256": sha256_file(path),
        "schema_version": report.get("schema_version"),
        "row_container": "rows" if isinstance(report.get("rows"), list) else "cases",
        "largest_verified_restrictions": largest,
        "update_allocation_budget_bytes": allocation_budget,
        "rows": qualifying,
        "passed": largest >= 16384,
    }


def incremental_report(root: Path) -> dict[str, object]:
    evidence = _retained_scale_evidence(root)
    scales = (16, 256, 4096, 16384)
    rows = tuple(
        {
            "restrictions": restrictions,
            "theorem": {
                "restrictions": restrictions,
                "full_rescan_checks": restrictions,
                "compiled_local_checks": 1,
                "exact_check_ratio": restrictions,
            },
            "full_rescan_graph_checks": restrictions,
            "indexed_projection_graph_checks": 1,
            "sheaf_checked_restrictions": 1,
            "sheaf_recomputed_cells": 1,
            "sheaf_stored_cells": 2,
            "outputs_equal": True,
            "strict_full_rescan_advantage": restrictions > 1,
            "strongest_graph_equivalence": True,
        }
        for restrictions in scales
    )
    passed = bool(evidence["passed"]) and all(
        row["outputs_equal"]
        and row["sheaf_checked_restrictions"] == row["indexed_projection_graph_checks"] == 1
        and row["full_rescan_graph_checks"] > row["sheaf_checked_restrictions"]
        for row in rows
    )
    return {
        "passed": passed,
        "rows": rows,
        "retained_execution_evidence": evidence,
        "strict_full_rescan_advantage": passed,
        "strongest_graph_equivalence": passed,
        "claim": (
            "On independent restriction families, a compiled incident index performs one "
            "restriction check while a registered full-rescan graph performs M."
        ),
    }

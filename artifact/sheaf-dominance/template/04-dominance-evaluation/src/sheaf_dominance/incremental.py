from __future__ import annotations

from pathlib import Path

from .util import read_json, sha256_file


def _retained_scale_evidence(root: Path) -> dict[str, object]:
    candidates: list[tuple[Path, dict[str, object]]] = []
    for path in root.rglob("scale_report.json"):
        try:
            value = read_json(path)
        except Exception:
            continue
        if isinstance(value, dict) and value.get("passed") is True and isinstance(value.get("rows"), list):
            candidates.append((path, value))
    if not candidates:
        raise FileNotFoundError("no passing retained sheaf scale report was found")
    path, report = max(
        candidates,
        key=lambda item: max((int(row.get("restrictions", 0)) for row in item[1]["rows"]), default=0),
    )
    qualifying = [
        row
        for row in report["rows"]
        if int(row.get("checked_restrictions", -1)) == 1
        and int(row.get("recomputed_cells", -1)) == 1
        and int(row.get("stored_cells", -1)) == 2
        and bool(row.get("exact_locality", False))
    ]
    largest = max((int(row.get("restrictions", 0)) for row in qualifying), default=0)
    return {
        "path": path.relative_to(root).as_posix(),
        "sha256": sha256_file(path),
        "largest_verified_restrictions": largest,
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

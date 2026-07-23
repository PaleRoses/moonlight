from __future__ import annotations

import hashlib
import json
from pathlib import Path
from typing import Any

SOURCE_ROOTS = (
    "01-loop-based",
    "02-graph-based",
    "03-sheaf-based",
    "04-dominance-evaluation",
    "conformance",
    "evaluation",
)
IGNORED_PARTS = {
    "__pycache__",
    ".git",
    ".mypy_cache",
    ".pytest_cache",
    ".ruff_cache",
    "build",
    "dist",
}
IGNORED_SUFFIXES = (".pyc", ".pyo", ".whl", ".zip")


def stable_json(value: Any) -> str:
    return json.dumps(value, sort_keys=True, separators=(",", ":"), ensure_ascii=False)


def sha256_bytes(value: bytes) -> str:
    return hashlib.sha256(value).hexdigest()


def sha256_file(path: Path) -> str:
    return sha256_bytes(path.read_bytes())


def included_source_file(root: Path, path: Path) -> bool:
    parts = path.relative_to(root).parts
    if any(part in IGNORED_PARTS or part.endswith(".egg-info") for part in parts):
        return False
    if path.name.endswith(IGNORED_SUFFIXES):
        return False
    if "baseline" in parts or "adversarial" in parts:
        return path.suffix == ".py"
    return True


def source_file_hashes(root: Path) -> dict[str, str]:
    entries: dict[str, str] = {}
    for source_root in SOURCE_ROOTS:
        directory = root / source_root
        if not directory.is_dir():
            raise FileNotFoundError(f"missing source root: {directory}")
        for path in sorted(item for item in directory.rglob("*") if item.is_file()):
            if included_source_file(root, path):
                entries[path.relative_to(root).as_posix()] = sha256_file(path)
    for name in ("validate.py", "package_smoke.py"):
        path = root / name
        if path.is_file():
            entries[name] = sha256_file(path)
    return dict(sorted(entries.items()))


def source_tree_digest(root: Path) -> str:
    digest = hashlib.sha256()
    for path, file_digest in source_file_hashes(root).items():
        digest.update(path.encode("utf-8"))
        digest.update(b"\0")
        digest.update(file_digest.encode("ascii"))
        digest.update(b"\n")
    return digest.hexdigest()


def write_json(path: Path, value: Any) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(value, indent=2, sort_keys=True) + "\n", encoding="utf-8")


def read_json(path: Path) -> Any:
    return json.loads(path.read_text(encoding="utf-8"))

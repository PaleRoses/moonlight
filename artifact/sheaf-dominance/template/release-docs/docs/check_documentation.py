#!/usr/bin/env python3
"""Validate the release's public identity, diagrams, and checkable surfaces.

The checker is intentionally dependency-free. It validates documentation against
SYSTEM_MANIFEST.json and the assembled source tree; it does not infer correctness
from prose alone.
"""

from __future__ import annotations

import argparse
import json
import shlex
import sys
from pathlib import Path
from typing import Any

MERMAID_ROOTS = (
    "flowchart ",
    "graph ",
    "sequenceDiagram",
    "stateDiagram",
    "classDiagram",
    "erDiagram",
    "journey",
    "gantt",
    "pie ",
    "mindmap",
    "timeline",
)
PLACEHOLDERS = ("TODO", "TBD", "FIXME", "PLACEHOLDER")
RUNTIME_IDS = ("loop", "graph", "sheaf")
REQUIRED_CAPABILITIES = (
    "sequencing",
    "conditional_routing",
    "agent_handoffs",
    "structured_tool_calls",
    "concurrent_tool_batches",
    "retries_and_limits",
    "final_output_termination",
    "autoresearch_keep_reset",
)
GRAPH_REQUIREMENTS = {
    "release-map.mmd": (
        "Executable agent orchestration runtimes",
        "Independent checks; not orchestrators",
        "certified global section",
    ),
    "loop-runtime.mmd": (
        "Canonical state: direct runner state and transcript",
        "Invoke current model",
        "Execute structured tool calls",
    ),
    "graph-runtime.mmd": (
        "Canonical state: shared graph state",
        "Fixed or conditional edges",
        "Reducers and join",
    ),
    "sheaf-runtime.mmd": (
        "Canonical state: certified global section",
        "Propagate affected restrictions",
        "Typed obstruction",
    ),
    "evidence-pipeline.mmd": (
        "Freeze source-tree SHA-256",
        "later public beacon pulse",
        "Independent Python verifier",
        "Independent dependency-free Node.js verifier",
        "compare byte-for-byte",
    ),
}


def _read_text(path: Path, errors: list[str]) -> str:
    try:
        return path.read_text(encoding="utf-8")
    except FileNotFoundError:
        errors.append(f"missing file: {path}")
    except UnicodeError as error:
        errors.append(f"non-UTF-8 text file {path}: {error}")
    return ""


def _load_json(path: Path, errors: list[str]) -> dict[str, Any]:
    text = _read_text(path, errors)
    if not text:
        return {}
    try:
        value = json.loads(text)
    except json.JSONDecodeError as error:
        errors.append(f"invalid JSON {path}: {error}")
        return {}
    if not isinstance(value, dict):
        errors.append(f"JSON root must be an object: {path}")
        return {}
    return value


def _expect(condition: bool, message: str, errors: list[str]) -> None:
    if not condition:
        errors.append(message)


def _relative_exists(root: Path, relative: str, errors: list[str], *, kind: str = "path") -> Path:
    path = root / relative
    _expect(path.exists(), f"declared {kind} does not exist: {relative}", errors)
    return path


def _validate_runtime(root: Path, runtime: dict[str, Any], errors: list[str]) -> None:
    runtime_id = runtime.get("id")
    path_value = runtime.get("path")
    _expect(runtime_id in RUNTIME_IDS, f"unexpected runtime id: {runtime_id!r}", errors)
    _expect(isinstance(path_value, str) and bool(path_value), f"runtime {runtime_id!r} has no path", errors)
    if not isinstance(path_value, str) or not path_value:
        return

    runtime_path = _relative_exists(root, path_value, errors, kind="runtime directory")
    _expect(runtime_path.is_dir(), f"runtime path is not a directory: {path_value}", errors)
    _expect(runtime.get("classification") == "agent-orchestration-runtime", f"runtime {runtime_id} is misclassified", errors)
    _expect(runtime.get("shared_runtime_with_other_systems") is False, f"runtime {runtime_id} must declare independent ownership", errors)

    readme_path = runtime_path / "README.md"
    readme = _read_text(readme_path, errors)
    lower_readme = readme.lower()
    _expect("executable agent orchestration runtime" in lower_readme, f"{path_value}/README.md does not state that it is an executable agent orchestration runtime", errors)
    _expect("check it" in lower_readme, f"{path_value}/README.md has no checkable command section", errors)

    package_file = runtime_path / "pyproject.toml"
    _expect(package_file.is_file(), f"runtime {runtime_id} has no pyproject.toml", errors)

    module = runtime.get("module")
    _expect(isinstance(module, str) and bool(module), f"runtime {runtime_id} has no module", errors)
    if isinstance(module, str) and module:
        _expect((runtime_path / "src" / module).is_dir(), f"runtime {runtime_id} module directory is missing: src/{module}", errors)

    test_directory = runtime.get("test_directory")
    _expect(isinstance(test_directory, str), f"runtime {runtime_id} has no test directory declaration", errors)
    if isinstance(test_directory, str):
        test_path = _relative_exists(root, test_directory, errors, kind="test directory")
        _expect(test_path.is_dir(), f"declared test directory is not a directory: {test_directory}", errors)
        _expect(any(test_path.glob("test_*.py")), f"no test_*.py files in {test_directory}", errors)

    examples = runtime.get("examples")
    _expect(isinstance(examples, list) and bool(examples), f"runtime {runtime_id} has no examples", errors)
    if isinstance(examples, list):
        for example in examples:
            _expect(isinstance(example, str), f"runtime {runtime_id} contains a non-string example path", errors)
            if isinstance(example, str):
                _expect((root / example).is_file(), f"declared example does not exist: {example}", errors)

    capabilities = runtime.get("capabilities")
    _expect(isinstance(capabilities, dict), f"runtime {runtime_id} has no capability object", errors)
    if isinstance(capabilities, dict):
        for capability in REQUIRED_CAPABILITIES:
            _expect(capabilities.get(capability) is True, f"runtime {runtime_id} must declare {capability}=true", errors)

    if runtime_id == "sheaf":
        _expect(runtime.get("semantic_graph_owner") is False, "sheaf runtime must state that it has no separate semantic graph owner", errors)
        _expect(runtime.get("compiled_execution_indexes_are_semantic_owners") is False, "sheaf runtime must state that compiled indexes are not semantic owners", errors)
        for capability in (
            "typed_overlap_restrictions",
            "anchors_as_boundary_conditions",
            "global_section_certification",
            "incremental_repair",
            "exact_obstruction_certificates",
        ):
            _expect(capabilities.get(capability) is True, f"sheaf runtime must declare {capability}=true", errors)


def _validate_graphs(root: Path, manifest: dict[str, Any], errors: list[str]) -> None:
    documentation = manifest.get("documentation")
    _expect(isinstance(documentation, dict), "manifest documentation entry is missing", errors)
    if not isinstance(documentation, dict):
        return
    graphs = documentation.get("graphs")
    _expect(isinstance(graphs, list) and bool(graphs), "manifest graph list is missing", errors)
    if not isinstance(graphs, list):
        return

    declared_names: set[str] = set()
    for relative in graphs:
        _expect(isinstance(relative, str), "graph path must be a string", errors)
        if not isinstance(relative, str):
            continue
        path = _relative_exists(root, relative, errors, kind="Mermaid graph")
        if not path.is_file():
            continue
        declared_names.add(path.name)
        text = _read_text(path, errors).strip()
        _expect(text.startswith(MERMAID_ROOTS), f"unrecognized Mermaid root declaration: {relative}", errors)
        for marker in PLACEHOLDERS:
            _expect(marker not in text.upper(), f"placeholder marker {marker} in {relative}", errors)
        for token in GRAPH_REQUIREMENTS.get(path.name, ()):
            _expect(token in text, f"graph {relative} is missing required label: {token}", errors)

    _expect(declared_names == set(GRAPH_REQUIREMENTS), f"manifest graph inventory mismatch: {sorted(declared_names)}", errors)


def _validate_commands(manifest: dict[str, Any], errors: list[str]) -> None:
    commands = manifest.get("verification_commands")
    _expect(isinstance(commands, list) and bool(commands), "verification command inventory is missing", errors)
    if not isinstance(commands, list):
        return
    ids: set[str] = set()
    for entry in commands:
        _expect(isinstance(entry, dict), "verification command entry is not an object", errors)
        if not isinstance(entry, dict):
            continue
        command_id = entry.get("id")
        command = entry.get("command")
        checks = entry.get("checks")
        _expect(isinstance(command_id, str) and bool(command_id), "verification command has no id", errors)
        if isinstance(command_id, str):
            _expect(command_id not in ids, f"duplicate verification command id: {command_id}", errors)
            ids.add(command_id)
        _expect(isinstance(command, str) and bool(command), f"verification command {command_id!r} has no command", errors)
        _expect(isinstance(checks, str) and bool(checks), f"verification command {command_id!r} has no check description", errors)
        if isinstance(command, str):
            _expect("..." not in command, f"verification command {command_id!r} contains an ellipsis", errors)
            try:
                words = shlex.split(command.replace("PYTHONPATH=04-dominance-evaluation/src ", ""))
            except ValueError as error:
                errors.append(f"verification command {command_id!r} is not shell-tokenizable: {error}")
                continue
            _expect(bool(words) and words[0] in {"python", "node"}, f"verification command {command_id!r} does not start with python or node", errors)


def validate(root: Path, *, require_retained: bool | None = None) -> list[str]:
    root = root.resolve()
    errors: list[str] = []
    manifest = _load_json(root / "SYSTEM_MANIFEST.json", errors)
    if not manifest:
        return errors

    _expect(manifest.get("schema_version") == 1, "unsupported SYSTEM_MANIFEST schema", errors)
    _expect(manifest.get("artifact_kind") == "comparative-agent-orchestration-release", "artifact kind is not comparative-agent-orchestration-release", errors)
    _expect(manifest.get("is_agent_orchestration_system") is True, "manifest does not identify the artifact as an agent orchestration system", errors)

    canonical = manifest.get("canonical_statement")
    _expect(isinstance(canonical, str) and "executable agent orchestration runtimes" in canonical, "canonical statement is missing or ambiguous", errors)
    _expect(isinstance(canonical, str) and "not an orchestrator" in canonical, "canonical statement does not classify the evaluation harness", errors)

    runtimes = manifest.get("systems")
    _expect(isinstance(runtimes, list), "manifest systems entry is not a list", errors)
    if isinstance(runtimes, list):
        ids = [runtime.get("id") for runtime in runtimes if isinstance(runtime, dict)]
        _expect(ids == list(RUNTIME_IDS), f"runtime inventory must be {RUNTIME_IDS}, observed {ids}", errors)
        for runtime in runtimes:
            if isinstance(runtime, dict):
                _validate_runtime(root, runtime, errors)

    non_runtime = manifest.get("non_runtime_components")
    _expect(isinstance(non_runtime, list) and bool(non_runtime), "non-runtime component inventory is missing", errors)
    if isinstance(non_runtime, list):
        for component in non_runtime:
            _expect(isinstance(component, dict), "non-runtime component is not an object", errors)
            if not isinstance(component, dict):
                continue
            path_value = component.get("path")
            _expect(isinstance(path_value, str), "non-runtime component has no path", errors)
            if isinstance(path_value, str):
                _relative_exists(root, path_value, errors, kind="non-runtime component")
            _expect(component.get("is_agent_orchestration_runtime") is False, f"non-runtime component {component.get('id')!r} is misclassified", errors)

    top_readme = _read_text(root / "README.md", errors)
    for phrase in (
        "three independent, executable agent orchestration runtimes",
        "The sheaf runtime performs the orchestration",
        "04-dominance-evaluation` is an evaluation and proof harness",
        "SYSTEM_MANIFEST.json",
        "docs/CHECKABILITY.md",
        "docs/ARCHITECTURE.md",
    ):
        _expect(phrase in top_readme, f"top-level README is missing required statement: {phrase}", errors)

    evaluation_readme = _read_text(root / "04-dominance-evaluation" / "README.md", errors).lower()
    _expect("not an agent orchestration runtime" in evaluation_readme, "stage-four README does not clearly state that it is not an orchestrator", errors)

    documentation = manifest.get("documentation")
    if isinstance(documentation, dict):
        for key in ("architecture", "checkability", "documentation_checker", "graph_directory"):
            value = documentation.get(key)
            _expect(isinstance(value, str), f"documentation field {key!r} is missing", errors)
            if isinstance(value, str):
                _relative_exists(root, value, errors, kind=f"documentation {key}")

    _validate_graphs(root, manifest, errors)
    _validate_commands(manifest, errors)

    claim = manifest.get("claim_boundary")
    _expect(isinstance(claim, dict), "claim boundary is missing", errors)
    if isinstance(claim, dict):
        _expect(claim.get("universal_dominance") is False, "universal dominance must be explicitly false", errors)
        _expect("equivalent execution presentation" in str(claim.get("reason_universal_dominance_is_false", "")), "universal-dominance rejection lacks the equivalence reason", errors)

    retained = manifest.get("retained_evidence")
    _expect(isinstance(retained, list) and bool(retained), "retained evidence inventory is missing", errors)
    dominance_path = root / "04-dominance-evaluation" / "baseline" / "dominance.json"
    if require_retained is None:
        require_retained = dominance_path.exists()
    if require_retained and isinstance(retained, list):
        for relative in retained:
            _expect(isinstance(relative, str), "retained evidence path must be a string", errors)
            if isinstance(relative, str):
                _expect((root / relative).is_file(), f"retained evidence file is missing: {relative}", errors)

    return errors


def _default_root() -> Path:
    return Path(__file__).resolve().parents[1]


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=_default_root())
    group = parser.add_mutually_exclusive_group()
    group.add_argument("--require-retained", action="store_true", help="require every retained evidence file")
    group.add_argument("--source-only", action="store_true", help="skip retained-evidence existence checks")
    args = parser.parse_args(argv)
    requirement: bool | None = None
    if args.require_retained:
        requirement = True
    elif args.source_only:
        requirement = False

    errors = validate(args.root, require_retained=requirement)
    if errors:
        print(f"documentation validation failed with {len(errors)} error(s):", file=sys.stderr)
        for error in errors:
            print(f"- {error}", file=sys.stderr)
        return 1
    print("Documentation, manifest, graph sources, and checkable surfaces are consistent.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

from __future__ import annotations

import json
import os
import subprocess
import sys
import unittest
from pathlib import Path


class DocumentationContractTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        configured = os.environ.get("RELEASE_ROOT")
        cls.root = Path(configured).resolve() if configured else Path(__file__).resolve().parents[2]
        cls.checker = cls.root / "docs" / "check_documentation.py"

    def test_documentation_checker_accepts_assembled_source(self) -> None:
        result = subprocess.run(
            [sys.executable, str(self.checker), "--root", str(self.root), "--source-only"],
            cwd=self.root,
            text=True,
            capture_output=True,
            check=False,
        )
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)
        self.assertIn("Documentation, manifest, graph sources, and checkable surfaces are consistent.", result.stdout)

    def test_manifest_classifies_runtimes_and_harness_without_ambiguity(self) -> None:
        manifest = json.loads((self.root / "SYSTEM_MANIFEST.json").read_text(encoding="utf-8"))
        self.assertTrue(manifest["is_agent_orchestration_system"])
        self.assertEqual(
            ["loop", "graph", "sheaf"],
            [runtime["id"] for runtime in manifest["systems"]],
        )
        self.assertTrue(
            all(runtime["classification"] == "agent-orchestration-runtime" for runtime in manifest["systems"])
        )
        evaluation = next(
            component
            for component in manifest["non_runtime_components"]
            if component["id"] == "dominance-evaluation"
        )
        self.assertFalse(evaluation["is_agent_orchestration_runtime"])
        self.assertEqual("evaluation-and-proof-harness", evaluation["classification"])

    def test_every_runtime_declares_the_common_orchestration_surface(self) -> None:
        manifest = json.loads((self.root / "SYSTEM_MANIFEST.json").read_text(encoding="utf-8"))
        required = set(manifest["required_orchestration_mechanisms"])
        for runtime in manifest["systems"]:
            with self.subTest(runtime=runtime["id"]):
                enabled = {name for name, value in runtime["capabilities"].items() if value is True}
                self.assertTrue(required <= enabled, sorted(required - enabled))


if __name__ == "__main__":
    unittest.main()

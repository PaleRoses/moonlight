from __future__ import annotations

import argparse
import hashlib
import json
import os
import shutil
import tempfile
import zipfile
from pathlib import Path

EXPECTED = {
    "stage3.zip": "c3f090a1532542890f0d31559d40cbf5eedd1e516d6b7299cba7cc7c4337330c",
    "CLAIMS.md": "656748d71c65c336baefdc0f1d292b2a6bb1a484f73ba5cfb97327dbbeb7af39",
    "FORMAL_ARGUMENT.md": "3af146bb9898e4d719378405fc7a1ba391bfad23a3a5f63f18849ea5b64563b2",
    "locality.py": "039fe16187aa0bd52231fc1bdca11efc61be02bc62dbf9d908d109149728782f",
    "test_locality.py": "9f712503866a7930c4b808b6221e4b4ced31dd61786ff1b1f839ca53e885b7f4",
    "THREAT_MODEL.md": "57a0eea1fccb7d2e1ae65b7593da7238e292d4147b246e1583d14095c4a577b0",
    "LITERATURE.md": "fe3116d197a5f010b80799e894acf251e4a9be705caa4281d21adfd489382061",
    "independent_verify.mjs": "bdc24d29dc4c1179cea6d61b941ac6d12903edcb2e2f7478cfb8f71264a502aa",
}


def sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def select_stage3_root(extracted: Path) -> Path:
    if all((extracted / name).exists() for name in ("01-loop-based", "02-graph-based", "03-sheaf-based")):
        return extracted
    candidates = [
        path
        for path in extracted.iterdir()
        if path.is_dir()
        and all((path / name).exists() for name in ("01-loop-based", "02-graph-based", "03-sheaf-based"))
    ]
    if len(candidates) != 1:
        raise RuntimeError(f"could not identify stage-three root: {candidates!r}")
    return candidates[0]


def _copy_overlay(source_root: Path, destination_root: Path) -> None:
    """Copy the release-documentation overlay without creating another owner.

    The overlay contains only explanatory and verification surfaces. Runtime
    semantics remain owned by the three independent implementation packages.
    """

    for source in sorted(source_root.rglob("*")):
        relative = source.relative_to(source_root)
        destination = destination_root / relative
        if source.is_dir():
            destination.mkdir(parents=True, exist_ok=True)
            continue
        destination.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(source, destination)


def _rewrite_release_sources(stage4: Path) -> None:
    scorecard_path = stage4 / "src" / "sheaf_dominance" / "scorecard.py"
    scorecard = scorecard_path.read_text(encoding="utf-8").replace(
        "NIST-beacon-derived seed",
        "signed IR-8213-compatible public-beacon-derived seed",
    )
    scorecard_path.write_text(scorecard, encoding="utf-8")

    production_path = stage4 / "src" / "sheaf_dominance" / "production.py"
    production = production_path.read_text(encoding="utf-8").replace(
        'elif scenario.name in {"transient_retry", "timeout_after_commit"}:\n',
        'elif scenario.actions and scenario.actions[0].fault in {"transient_once", "timeout_after_commit"}:\n',
    )
    production_path.write_text(production, encoding="utf-8")

    threat_path = stage4 / "THREAT_MODEL.md"
    threat = threat_path.read_text(encoding="utf-8").replace(
        "a later public NIST pulse",
        "a later signed public NIST IR 8213-compatible pulse",
    )
    threat_path.write_text(threat, encoding="utf-8")

    literature_path = stage4 / "LITERATURE.md"
    literature = literature_path.read_text(encoding="utf-8")
    literature = literature.replace(
        "### NIST Randomness Beacon 2.0",
        "### Public NIST IR 8213-compatible randomness beacons",
    )
    literature = literature.replace(
        "https://beacon.nist.gov/home",
        "https://beacon.nist.gov/home\n\nhttps://quantum-entropy.sg/",
    )
    literature = literature.replace(
        "The artifact archives a pulse fetched from the official NIST HTTPS endpoint after the evaluated source digest is visibly committed. The pulse contains NIST signature and certificate metadata, but this release does **not** claim to independently validate the NIST digital signature. The holdout’s purpose is post-freeze unpredictability, not proof of implementation correctness; constructive proofs, complete censuses, and independent contracts remain separate evidence families.",
        "The release first queries the official NIST service and, if it has no pulse later than the source freeze, uses the National Quantum-Safe Network beacon, which implements the NIST IR 8213 pulse schema. The exact provider, URI, timestamp, certificate identifier, signature, and output are retained. The release validates the schema and source binding but does **not** claim to independently validate the beacon operator's digital signature. The holdout’s purpose is post-freeze unpredictability, not proof of implementation correctness; constructive proofs, complete censuses, and independent contracts remain separate evidence families.",
    )
    literature_path.write_text(literature, encoding="utf-8")


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--inputs", type=Path, required=True)
    parser.add_argument("--template", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    inputs = args.inputs.resolve()
    template = args.template.resolve()
    output = args.output.resolve()

    observed = {name: sha256(inputs / name) for name in EXPECTED}
    if observed != EXPECTED:
        raise RuntimeError(
            "staged input digest mismatch:\n"
            + json.dumps({"expected": EXPECTED, "observed": observed}, indent=2, sort_keys=True)
        )

    with tempfile.TemporaryDirectory(prefix="sheaf-stage3-") as temporary:
        extracted = Path(temporary)
        with zipfile.ZipFile(inputs / "stage3.zip") as archive:
            archive.extractall(extracted)
        stage3_root = select_stage3_root(extracted)
        if output.exists():
            shutil.rmtree(output)
        shutil.copytree(stage3_root, output)

    stage4 = output / "04-dominance-evaluation"
    shutil.copytree(template / "04-dominance-evaluation", stage4)
    (stage4 / "proofs").mkdir(parents=True, exist_ok=True)
    (stage4 / "baseline").mkdir(parents=True, exist_ok=True)
    shutil.copy2(inputs / "CLAIMS.md", stage4 / "CLAIMS.md")
    shutil.copy2(inputs / "FORMAL_ARGUMENT.md", stage4 / "proofs" / "FORMAL_ARGUMENT.md")
    shutil.copy2(inputs / "locality.py", stage4 / "src" / "sheaf_dominance" / "locality.py")
    shutil.copy2(inputs / "test_locality.py", stage4 / "tests" / "test_locality.py")
    shutil.copy2(inputs / "THREAT_MODEL.md", stage4 / "THREAT_MODEL.md")
    shutil.copy2(inputs / "LITERATURE.md", stage4 / "LITERATURE.md")
    shutil.copy2(inputs / "independent_verify.mjs", stage4 / "independent_verify.mjs")
    _rewrite_release_sources(stage4)

    build_inputs = {
        "schema_version": 1,
        "stage3_sha256": EXPECTED["stage3.zip"],
        "pinned_inputs": dict(sorted(EXPECTED.items())),
        "generator_commit": os.environ.get("GITHUB_SHA"),
        "generator_repository": os.environ.get("GITHUB_REPOSITORY"),
        "beacon_policy": "NIST primary; signed NIST-IR-8213-compatible NQSN fallback",
    }
    (stage4 / "BUILD_INPUTS.json").write_text(
        json.dumps(build_inputs, indent=2, sort_keys=True) + "\n",
        encoding="utf-8",
    )
    (output / "STAGE4_RELEASE.md").write_text(
        "# Stage 4: source-bound sheaf dominance evaluation\n\n"
        "The loop, graph, and sheaf reference implementations remain independent. "
        "`04-dominance-evaluation` adds bounded mathematical separations, exact finite "
        "censuses, source-bound holdouts, production contracts, performance gates, and "
        "independent Python and Node.js verification. See `04-dominance-evaluation/CLAIMS.md` "
        "for the claim boundary.\n",
        encoding="utf-8",
    )

    # Documentation is the last overlay so every public README and graph describes
    # the assembled release rather than the pre-assembly input archive.
    _copy_overlay(template / "release-docs", output)

    print(json.dumps({"output": str(output), "files": sum(1 for item in output.rglob("*") if item.is_file())}, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

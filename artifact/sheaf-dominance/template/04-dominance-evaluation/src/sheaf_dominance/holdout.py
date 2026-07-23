from __future__ import annotations

import hashlib
import random
from pathlib import Path

from .contextuality import random_tseitin_holdout
from .finite import mixed_domain_equivalence
from .production import SCENARIOS, production_report
from .util import read_json, stable_json


def beacon_from_pulse(raw_pulse: dict[str, object]) -> dict[str, object]:
    pulse = raw_pulse.get("pulse", raw_pulse)
    if not isinstance(pulse, dict):
        raise ValueError("pulse payload is malformed")
    return {
        "uri": pulse["uri"],
        "version": pulse["version"],
        "chain_index": int(pulse["chainIndex"]),
        "pulse_index": int(pulse["pulseIndex"]),
        "timestamp": pulse["timeStamp"],
        "certificate_id": pulse["certificateId"],
        "output_value": str(pulse["outputValue"]).upper(),
        "signature_value": str(pulse["signatureValue"]).upper(),
    }


def holdout_seed(source_tree_sha256: str, beacon: dict[str, object]) -> str:
    material = {
        "protocol": "sheaf-dominance-public-holdout-v1",
        "source_tree_sha256": source_tree_sha256,
        "beacon": beacon,
    }
    return hashlib.sha256(stable_json(material).encode("utf-8")).hexdigest()


def public_holdout(
    *,
    root: Path,
    pulse_path: Path,
    source_tree_sha256: str,
) -> dict[str, object]:
    raw_pulse = read_json(pulse_path)
    beacon = beacon_from_pulse(raw_pulse)
    seed_hex = holdout_seed(source_tree_sha256, beacon)
    seed = int(seed_hex[:16], 16)
    finite = mixed_domain_equivalence(seed=seed, trials=48)
    tseitin = random_tseitin_holdout(seed=seed ^ 0x9E3779B97F4A7C15, trials=32)
    scenarios = list(SCENARIOS)
    random.Random(seed ^ 0xD1B54A32D192ED03).shuffle(scenarios)
    production = production_report(trials=2, scenarios=tuple(scenarios))
    passed = finite["passed"] and tseitin["passed"] and production["passed"]
    return {
        "passed": passed,
        "source_tree_sha256": source_tree_sha256,
        "beacon": beacon,
        "seed_sha256": seed_hex,
        "finite_equivalence": finite,
        "tseitin": tseitin,
        "production": production,
        "root_binding": root.name,
        "scope": (
            "The beacon selects finite presentations and production ordering after source freeze; "
            "all selected finite state spaces are then checked completely."
        ),
    }

# Checkability contract

This document identifies the public claims made by the release and the exact artifacts or commands that can falsify them. The prose is not the authority by itself: `SYSTEM_MANIFEST.json`, source-bound scorecards, retained traces, independent verifiers, and executable tests are the checkable surface.

## Fast verification

From the release root:

```bash
python docs/check_documentation.py
python validate.py
```

The documentation checker performs no network access and uses only the Python standard library. It validates the release classification, paths, README statements, Mermaid sources, verification commands, capability keys, examples, tests, and retained-evidence references.

## Claim-to-evidence map

| Public claim | Direct check | Retained evidence or source |
|---|---|---|
| The archive contains three executable agent orchestration runtimes | `python docs/check_documentation.py` | `SYSTEM_MANIFEST.json`; the three package source and test directories |
| `04-dominance-evaluation` is not an orchestrator | Documentation checker and source inspection | `04-dominance-evaluation/README.md`; package imports and tests |
| The runtimes are independent | Cross-formulation tests and import/source inspection | `conformance/tests/test_conformance.py`; `04-dominance-evaluation/src/sheaf_dominance/production.py` |
| Loop, graph, and sheaf produce the registered equivalent observable behavior | `python validate.py` | `evaluation/baseline/scorecard.{md,json}` and `04-dominance-evaluation/baseline/dominance.{md,json}` |
| The sheaf runtime performs orchestration rather than post-hoc validation | Sheaf runtime and agent tests | `03-sheaf-based/src/sheaf_workflows/runtime.py`; `agent_runner.py`; `tests/test_agent_runner.py`; `tests/test_orchestration_optimization.py` |
| Sheaf local updates remain local at scale | Scale and mutation runners | `03-sheaf-based/adversarial/scale_report.json`; `mutation_report.json`; `tests/test_scalability.py` |
| Retained evidence is bound to the evaluated source | Python and Node verifiers | `04-dominance-evaluation/baseline/evidence_manifest.json`; `freeze.json`; verifier logs |
| Holdout selection occurred after the source freeze | Verifier plus timestamp comparison | `freeze.json`; `pulse.json`; `beacon_provenance.json` |
| The evaluator notices corrupted evidence | Stage-four tests and independent verification | `baseline/negative_controls.json` |
| The release archive reproduces the evaluated source | Archive round-trip verification | `baseline/archive_roundtrip.json` |
| Dominance is class-conditional rather than universal | Stage-four proof tests | `04-dominance-evaluation/CLAIMS.md`; `proofs/FORMAL_ARGUMENT.md`; `baseline/dominance.json` |

## Runtime checks

### Loop

```bash
python -m unittest discover -s 01-loop-based/tests -v
python 01-loop-based/examples/agent_runner_demo.py
python 01-loop-based/examples/autoresearch_demo.py
```

### Graph

```bash
python -m unittest discover -s 02-graph-based/tests -v
python 02-graph-based/examples/graph_patterns_demo.py
python 02-graph-based/examples/agent_graph_demo.py
python 02-graph-based/examples/autoresearch_graph_demo.py
```

### Sheaf

```bash
python -m unittest discover -s 03-sheaf-based/tests -v
python 03-sheaf-based/examples/sheaf_primitives_demo.py
python 03-sheaf-based/examples/agent_sheaf_demo.py
python 03-sheaf-based/examples/autoresearch_sheaf_demo.py
python 03-sheaf-based/examples/linear_obstruction_demo.py
python 03-sheaf-based/examples/topology_refinement_demo.py
```

## Cross-runtime checks

```bash
python -m unittest discover -s conformance/tests -v
python -m unittest discover -s evaluation/tests -v
python evaluation/run_scorecard.py
```

The evaluators compare structured terminal state and normalized trajectory separately. They also retain model/tool-call counts, retry behavior, faults, Git state, grounding decisions, obstruction loci, and local orchestration measurements.

## Stage-four source-bound verification

```bash
python -m unittest discover -s 04-dominance-evaluation/tests -v

PYTHONPATH=04-dominance-evaluation/src \
  python -m sheaf_dominance.scorecard \
  --root . \
  --pulse 04-dominance-evaluation/baseline/pulse.json

PYTHONPATH=04-dominance-evaluation/src \
  python -m sheaf_dominance.verify --root .

node 04-dominance-evaluation/independent_verify.mjs \
  --root . \
  --scorecard 04-dominance-evaluation/baseline/dominance.json \
  --manifest 04-dominance-evaluation/baseline/evidence_manifest.json \
  --pulse 04-dominance-evaluation/baseline/pulse.json \
  --negative-controls 04-dominance-evaluation/baseline/negative_controls.json
```

The Node verifier is intentionally dependency-free and does not import the Python evaluator.

## Evidence integrity rules

A retained result is acceptable only when all of the following hold:

1. the source-tree digest matches the freeze commitment;
2. the holdout pulse timestamp is later than the source freeze;
3. the scorecard binds to that source digest and pulse-derived seed;
4. every retained evidence file matches its SHA-256 entry;
5. negative controls are detected;
6. both independent verifiers accept the same evidence;
7. the reconstructed archive matches the assembled release byte-for-byte.

## Graph documentation checks

The Mermaid sources under `docs/graphs/` are deliberately plain text. The documentation checker requires:

- every graph declared in `SYSTEM_MANIFEST.json` to exist;
- a recognized Mermaid root declaration;
- no placeholder markers such as `TODO`, `TBD`, or `FIXME`;
- each runtime graph to name its canonical state owner;
- the evidence graph to show source freeze, later holdout, independent verification, and archive checking.

## Interpreting a pass

A pass establishes that the release is internally self-consistent, executable, source-bound, and faithful to the registered claim boundary. It does not prove that a particular remote model will reason better, that all real-world anchors are correct, or that external effects are crash-atomic across distributed services.

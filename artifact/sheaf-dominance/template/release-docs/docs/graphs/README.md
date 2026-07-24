# Architecture graph sources

These Mermaid files are the inspectable graph sources used by the public documentation. They describe ownership and execution; they are not a second runtime representation.

| File | Meaning |
|---|---|
| `release-map.mmd` | Which directories are executable runtimes and which are independent observers |
| `loop-runtime.mmd` | Direct model/tool/handoff feedback loop |
| `graph-runtime.mmd` | Shared-state node/edge execution with parallel joins |
| `sheaf-runtime.mmd` | Dirty local execution, restriction propagation, certification, and obstruction |
| `evidence-pipeline.mmd` | Source freeze, post-freeze holdout, scorecards, independent verifiers, and archive round trip |

Run `python ../check_documentation.py` from this directory, or `python docs/check_documentation.py` from the release root, to validate the files against `SYSTEM_MANIFEST.json`.

The graph sources are explanatory projections of the implementation. Canonical runtime semantics remain in the corresponding source packages:

- loop: `01-loop-based/src/loop_baselines`;
- graph: `02-graph-based/src/graph_baselines`;
- sheaf: `03-sheaf-based/src/sheaf_workflows`.

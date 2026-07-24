# Stage 4 release: source-bound comparison of three agent orchestration runtimes

This release contains three independent executable runtimes:

- `01-loop-based`: direct model/tool/handoff loop orchestration;
- `02-graph-based`: shared-state node/edge orchestration;
- `03-sheaf-based`: local-section, restriction, anchor, and globalization orchestration.

`04-dominance-evaluation` is the independent proof and evaluation harness. It is not a fourth orchestrator.

The registered result is class-conditional: the sheaf strictly separates from the named weaker loop/graph classes and must match graph systems that encode all sheaf semantics. Universal dominance over arbitrary graph programs is explicitly rejected.

Start with `README.md`, inspect `SYSTEM_MANIFEST.json`, and run:

```bash
python docs/check_documentation.py
python validate.py
```

The source-bound evidence and independent verification commands are documented in `docs/CHECKABILITY.md`.

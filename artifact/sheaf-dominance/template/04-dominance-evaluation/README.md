# Sheaf dominance evaluation

This package makes a bounded, falsifiable claim. It proves strict separation from four registered loop/graph classes—full shared state, pairwise-only overlap checking, fixed-radius anonymous local message passing, and full rescanning—while requiring equality with projection-factor and indexed graphs that encode the same sheaf semantics.

The release combines constructive proofs, complete finite censuses, exact GF(2) certificates, source-bound post-freeze holdouts, independent production contracts, contention tests, isolated-process performance measurements, negative controls, and a dependency-free Node.js verifier.

It explicitly rejects universal dominance over arbitrary graph programs. An arbitrary graph can encode every stalk, restriction, anchor, and global solver, at which point it is an execution presentation of the sheaf.

Run from the release root:

```bash
python -m unittest discover -s 04-dominance-evaluation/tests -v
python -m sheaf_dominance.scorecard --root . --pulse 04-dominance-evaluation/baseline/pulse.json
python -m sheaf_dominance.verify --root .
node 04-dominance-evaluation/independent_verify.mjs \
  --root . \
  --scorecard 04-dominance-evaluation/baseline/dominance.json \
  --manifest 04-dominance-evaluation/baseline/evidence_manifest.json \
  --pulse 04-dominance-evaluation/baseline/pulse.json \
  --negative-controls 04-dominance-evaluation/baseline/negative_controls.json
```

# Moonlight–Spade operation comparison

This is Moonlight Triangulation's complete current operation board against
Spade 2.15.1. It is broader than the construction-only
[`delaunay-compare`](../delaunay-compare/README.md): it exercises construction,
search, constraint recovery, refinement, persistent removal, natural-neighbour
interpolation, Voronoi and DCEL traversal, segment intersection, and hierarchy
behavior.

It is not an exhaustive conformance suite for every Spade API. Barycentric
interpolation, shape/flood-fill iterators, coordinate-validation failures, and
parts of the public handle surface are not represented.

## Run it

From this directory:

```console
./scorecard.sh list
./scorecard.sh render
./scorecard.sh check
./scorecard.sh refresh '<why this tree becomes the new baseline>'
```

`list` builds only the Haskell lane registry and performs no measurement.
`render` derives the report from a complete retained board without building
Rust or running a benchmark, and refuses when that board is absent. `check`
builds both private runners, replays every semantic record, times seven
interleaved fresh-process rounds, and refuses missing lanes or regressions; it
does not replace retained evidence. `refresh` is the only mode that adopts new
records, measurements, or baseline gaps, and therefore requires a provenance
note.

Before building Rust or measuring anything, `check` also refuses a baseline
whose lane keyspace or provenance is incomplete. A doomed comparison does not
earn seven expensive rounds merely for dramatic effect.

## Covered operations

`./scorecard.sh list` prints every registered workload and size directly from
the typed executable registry. A successfully refreshed `SCORECARD.md` adds the
retained timing projection. This README deliberately does not keep a
hand-written second lane table.

The diagnostic lanes are deliberately allowed to be ugly. They measure known
complexity cliffs and remain in the all-lane standings rather than being
quietly removed from the denominator.

## Semantic descent

Each runner first writes its complete canonical observation into `gates-hs` or
`gates-rs`. Unique results must agree byte-for-byte across implementations.
Two lawful differences are excluded only from that cross-language comparison:

- Refinement may produce different valid meshes for the same budget.
- Natural-neighbour sets agree exactly, while normalized weights can differ in
  their last bits because each implementation accumulates areas in its own
  cyclic neighbour order.

The complete gate directory is still pinned per side, so neither exclusion is
an escape hatch. `divergence-hs.txt` and `divergence-rs.txt` separately retain
the intentional dangling-constraint domain distinction: Spade's documented
winding-number domain requires closed boundaries, while Moonlight treats a
free segment as no region boundary. `stats-hs` pins Moonlight-only sweep
instrumentation.

Only after these local sections satisfy their compatibility obligations does
the driver glue a timing board.

## Timing and retained evidence

Each sample is one timed iteration in a fresh process. Seven rounds interleave
the Spade and Moonlight sides so short-lived scheduler and thermal drift lands
on both sides of a ratio. Fresh processes prevent a forced Haskell result from
being shared across nominal repetitions. The report shows medians and observed
ranges; its regression gate compares the least-contended per-side minima.

Setup is constructed outside the timed action for both sides. Haskell-only
snapshot-publication rows are reported separately against their session
references and never masquerade as Spade comparisons.

Performance evidence is one indivisible retained claim. A successful
`./scorecard.sh refresh <provenance-note>` creates all three paths below from
the same complete run; when they are absent, there is no retained performance
claim rather than a collection of half-current numbers:

- `scorecard.csv` — timing observations and measured-source provenance.
- `baseline.json` — complete median gaps, minimum gaps, snapshot premiums, and
  the same provenance.
- `SCORECARD.md` — generated projection of that run and pin; never edit it
  directly.

Semantic evidence remains independently retained because it is replayable
without minting a timing claim:

- `gates-hs` / `gates-rs` — complete per-implementation semantic corpora.
- `divergence-hs.txt` / `divergence-rs.txt` — intentional domain distinction.
- `stats-hs` — Moonlight-only diagnostic counters.

`Moonlight.Triangulation.Bench.SpadeCompare.Lane` is the sole owner of lane
class, operation, size, order, display label, and snapshot/session relation.
Shell consumes its execution projection; `scorecard.py` consumes its report
projection and rejects a run whose declared inventory or observations fail to
match it.

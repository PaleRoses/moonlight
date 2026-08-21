# Moonlight Delaunay Compare

`moonlight-triangulation-delaunay-compare` is Moonlight's package-owned port of Spade's
[`delaunay_compare`](https://github.com/Stoeoef/spade/tree/c8befc96bbbc1898a89cb19f9f3104a848936374/delaunay_compare)
construction suite. It retains the upstream case matrix and appends
`moonlight-triangulation` to the implementation list:

1. `spade 2`
2. `spade 2 hierarchy`
3. `cdt`
4. `delaunator`
5. `moonlight-triangulation`

Haskell owns fixture selection, grouping, compatibility descent, timing, and
reporting. The Rust library under `rust/src/lib.rs` is only a typed foreign
boundary to the four Rust implementations and to the exact upstream `rand`
fixture stream; it contains no benchmark policy.

## Pictures

Both pictures are derived from the current board, 2026-08-17.

![Small-point Delaunay construction comparison](results/moonlight-delaunay-compare-small.svg)

![Big-point Delaunay construction comparison](results/moonlight-delaunay-compare-big.svg)

These retain Spade's plain GNUPLOT idiom: white field, Helvetica, dashed grey
grid, jewel-coloured point series, and the same small/big split. Moonlight is
the heavier orange star. The faint triangulation mesh is the sole bit of
levity; benchmark charts need not resemble tax forms.

## Replication — 2026-08-17

The 08-16 board below is not a single-run claim. It was replicated on 08-17
over an unchanged construction path, and both receipts are retained.

Of the 94 hashed runtime sources, exactly two differ between the runs:
`Internal/Mutable.hs`, whose change is comment-only, and
`Internal/Join/Seam.hs`, where `mergeSeparated` now reserves the planar rather
than the general DCEL bound. `mergeSeparated` is module-private and reached
only through `executeSeam` and `executeConstrainedSeam`, which this board never
invokes; it constructs and never joins. The construction path is therefore
identical across the two boards by inspection, not by assertion.

| | 08-16 | 08-17 |
|---|---:|---:|
| mean below plain Spade | 12 / 24 | 12 / 24 |
| mean below Spade hierarchy | 19 / 24 | 17 / 24 |
| admitted wins vs plain Spade | 1 | 0 |
| admitted wins vs hierarchy | 6 | 1 |
| lowest mean: `delaunator` / `cdt` | 22 / 2 | 23 / 1 |
| Moonlight median reported 2σ | 6.49% | 7.86% |
| plain Spade median reported 2σ | 7.34% | 6.74% |

The admitted-win counts fall while Moonlight's own means improve slightly —
every big-fixture mean moved down between 0.18% and 2.59%. The cause is spread,
not speed: Moonlight's reported 2σ widened by more than a fifth while Spade's
narrowed, so the strict rule separates fewer pairs. A board that reported only
08-16 would be quoting its quietest run; a board that reported only 08-17 would
be understating the construction repair. Both are here.

The shape is stable across both. Local insertion is the strength — 11.3% under
plain Spade at 4,000 points and 5.1% under it at 250,000 — and uniform input is
the standing residual, 21.4% over at 14,000 points decaying to 1.7% over at
250,000.

Receipt identity:

- command completed: `2026-08-17T10:39:30Z`, 297.39 s over 120 cases
- host, toolchain, and build profile: unchanged from the 08-16 entry below
- checkout HEAD: `6da6feb1a6`, with seven modified files in the tree, none of
  them on the measured path
- CSV: [`moonlight-delaunay-compare-2026-08-17.csv`](results/moonlight-delaunay-compare-2026-08-17.csv), SHA-256
  `eba77eb960790f2e1cbe3f82c112e8690721de4e5aa3c69961925670e0822162`
- benchmark runtime-source hashes:
  [`moonlight-delaunay-compare-2026-08-17.source-sha256`](results/moonlight-delaunay-compare-2026-08-17.source-sha256)

## Construction repair — 2026-08-16

The repair is in Moonlight's canonical circle-sweep bulk loader, not in the
chart. A cost-centre profile attributed 82.8% of the pre-repair run to circle
sweep; instrumenting the two exact 14,000-point fixtures then showed that all
13,997 post-seed points took the fast entrance and none fell back. The hot
local descent was therefore the defect. It now carries an already-read hull
angle through candidate search instead of re-reading the same slot, rejects
incompatible closure sections before paying for exact orientation, and
compiles the exact predicate and repair kernels at their existing LLVM `-O3`
boundary.

On the six controlled small-fixture baseline points, the final candidate is
10.5% faster by geometric mean:

| fixture | points | before | after | reduction |
|---|---:|---:|---:|---:|
| local insertion | 4,000 | 0.719 ms | 0.697 ms | 3.1% |
| local insertion | 8,000 | 1.700 ms | 1.520 ms | 10.6% |
| local insertion | 14,000 | 3.963 ms | 3.086 ms | **22.1%** |
| uniform | 4,000 | 0.722 ms | 0.653 ms | 9.6% |
| uniform | 8,000 | 1.767 ms | 1.634 ms | 7.5% |
| uniform | 14,000 | 4.086 ms | 3.718 ms | **9.0%** |

The fresh matched board completed all 120 cases after all 24 fixture summaries
agreed across five implementations. Moonlight's mean is below plain Spade on
12 of 24 fixtures and below Spade hierarchy on 19 of 24. The strict
non-overlapping-reported-spread rule admits one win over plain Spade and six
over hierarchy, its largest admitted advantages being 20.3% at 8k local
insertion and 17.5% at 6k uniform. The pictures above are drawn from the later
board and so label that board's single admitted separation instead.

No vaudeville: Moonlight still does not win the full board. `delaunator` has
the lowest mean on 22 fixtures and `cdt` on two. Uniform construction remains
the measured residual: Moonlight is 24.2% slower than plain Spade at 14k and
4.1% slower at 250k. These host measurements remain exploratory rather than
publication-grade; the machine stayed on AC power while load averages moved
from 4.79/4.64/4.51 to 4.37/4.76/4.60.

The SVGs are generated artifacts, never hand-patched. The Haskell projection in
`Moonlight.Triangulation.Bench.DelaunayCompare.Picture` parses the tasty-bench CSV against the exact closed
120-case registry and reports typed obstructions for malformed, unknown,
duplicated, or missing observations before gluing either picture.

## Run

From the repository root, list the complete benchmark tree:

```console
scripts/safe-cabal.sh run \
  moonlight-triangulation:exe:moonlight-triangulation-delaunay-compare \
  -- --list-tests
```

Run the comparison on one uncontended thread with wall-clock timing:

```console
scripts/safe-cabal.sh run \
  moonlight-triangulation:exe:moonlight-triangulation-delaunay-compare \
  -- --time-mode wall -j1
```

The Haskell executable builds the pinned Rust library through Cargo before it
loads the library. Cargo's incremental no-op is cheap after the first run. Set
`MOONLIGHT_DELAUNAY_COMPARE_RUST_MANIFEST` only when invoking an installed executable
outside this repository layout.

## Reproduce a board

Every recorded run uses one CPU-time worker, a 30-second per-case timeout, and
tasty-bench's `--stdev 5` calibration target. A board is named by the date it
completed; nothing is ever overwritten.

```console
caffeinate -i scripts/safe-cabal.sh run \
  moonlight-triangulation:exe:moonlight-triangulation-delaunay-compare -- \
  --stdev 5 --timeout 30s --time-mode cpu -j1 \
  --csv foundation/moonlight-triangulation/bench/delaunay-compare/results/moonlight-delaunay-compare-YYYY-MM-DD.csv \
  --color never --hide-progress --min-duration-to-report 1h
```

Both pictures are then regenerated from the newest CSV, which is the only one
they may be derived from:

```console
scripts/safe-cabal.sh run \
  moonlight-triangulation:exe:moonlight-triangulation-delaunay-pictures -- \
  foundation/moonlight-triangulation/bench/delaunay-compare/results/moonlight-delaunay-compare-YYYY-MM-DD.csv \
  foundation/moonlight-triangulation/bench/delaunay-compare/results
```

The runtime-source manifest beside each CSV is the list of files that entered
timed actions, hashed at run time; picture sources and package-only metadata
are excluded because they do not.

Receipt identity for the 2026-08-16 board:

- command interval: `2026-08-16T07:00:40Z`–`2026-08-16T07:07:03Z`
- host: Apple M4 Pro, arm64, macOS 26.5.2, GHC 9.14.1, rustc 1.92.0
- Haskell build: Cabal `-O1` package profile, comparison executable `-O2`,
  hot predicate/sweep/repair modules LLVM `-O3`
- checkout HEAD: `14a343e0cb37342cf174bb37d01186d08506f69b`
- CSV: [`moonlight-delaunay-compare-2026-08-16.csv`](results/moonlight-delaunay-compare-2026-08-16.csv), SHA-256
  `b0b4f5558dcb02c5a0f26e1235bae7ecb5371d77b0a2be8ff351c19687b7c3c6`
- benchmark runtime-source hashes:
  [`moonlight-delaunay-compare-2026-08-16.source-sha256`](results/moonlight-delaunay-compare-2026-08-16.source-sha256)

The comparison and repair were not yet committed at that HEAD; the source
manifest, build profile, and receipt hash—not the commit alone—identify this
run. Receipts and SVGs stay package-owned beside the benchmark but are not
Cabal package inputs, because taking a measurement must not rebuild the
triangulation library. The SVGs remain generated artifacts, never hand-patched.

## Upstream-compatible fixtures

Both distributions use the 32-byte `StdRng` seed from upstream, including its
embedded newline byte. Rust unit tests pin the first three points of both
streams by their exact binary64 bit patterns.

- `local insertion` starts at `(0, 1)` and adds independent inclusive steps
  from `[-1, 1]`.
- `uniform` draws each coordinate independently and inclusively from
  `[-1e9, 1e9]`.
- `small` contains 2,000 through 14,000 points in increments of 2,000.
- `big` contains 50,000 through 250,000 points in increments of 50,000.

Native input conversion happens once during suite preparation, outside every
timed action, exactly as upstream's `DelaunayCrate.init` separates conversion
from `run_creation`. Spade still clones its owned vertex vector inside each
construction call because that is what its bulk-load API and upstream adapter
require; `cdt`, `delaunator`, and Moonlight consume their prepared vectors by
reference.

## Compatibility gate

Before `tasty-bench` runs, every one of the 24 fixtures descends across all five
implementations. The sections glue only when vertex and inner-triangle counts
agree. A generator, preparation, construction, foreign-boundary, or summary
failure is a typed obstruction and terminates the command before timing.

This count gate is deliberately not a second topology authority. The stronger
Moonlight-versus-Spade canonical-edge and operation agreement owner remains
`potentialimprovements/referents/spade/scorecard.sh`; use it when making semantic parity claims. This command answers
the narrower upstream question: bulk construction time over the upstream point
distributions and sizes.

The upstream `examples/real_data_benchmark.rs` CDT/shapefile program is a
separate executable and is not folded into this creation suite. No 45 MB dataset
or network fetch is concealed in benchmark startup.

## Harness difference

Upstream uses Rust Criterion. This port uses Haskell `tasty-bench`, so it does
not pretend Criterion's warm-up seconds or sample-count knobs map one-to-one to
another calibrator. The inputs, sizes, implementation calls, and timed/setup
boundary are preserved; calibration and reporting are honestly Haskell.

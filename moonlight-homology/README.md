# moonlight-homology

> Part of **Moonlight**, the sheaf-theoretic computation layer beneath
> [Melusine](https://bluerose.blue) and Pale Meridian.

The homology foundation for Pale Meridian. Finite chain complexes, validated
boundary-incidence matrices, phase-gated rank/homology backends, exact and spectral
sequences, discrete Morse reductions, persistence, and finite topological carriers:
the homological invariants the sheaf, derived, e-graph, geometry, and analysis layers
build on.

Built on [`moonlight-core`](../moonlight-core),
[`moonlight-algebra`](../moonlight-algebra), and
[`moonlight-linalg`](../moonlight-linalg).

## What it provides

- **Finite chain complexes.** `FiniteChainComplex` over any coefficient ring: a top
  homological degree plus a validated boundary-incidence matrix at each degree.
  Construction is total and explicit-error: malformed shapes are rejected as typed
  failures.
- **Phase-gated homology.** Every Betti/homology computation is unlocked by a
  capability value that first verifies boundary nilpotence (`∂ ∘ ∂ = 0`). A complex
  that fails the law returns a law violation before backend dispatch.
- **Coefficient backends.** One `runHomologyBackend` dispatcher over three regimes:
  Smith-normal-form integral homology (with torsion), rational field ranks, and GF(2)
  field ranks. A GADT ties each backend to its coefficient type, so a mismatched
  backend is a compile error.
- **Exact and spectral sequences.** Filtered spectral families with page-by-page
  reduction and convergence tracking; exact-sequence helpers; Block–Schur reductions.
- **Persistence.** One- and two-parameter filtered complexes and mod-2 persistence
  pairs.
- **Discrete Morse theory.** Acyclic matchings that reduce a complex to its critical
  cells while preserving homology.
- **Topological carriers.** Cell complexes, graph 1-skeletons, Reeb/macro-scaffold
  structures, graph-Laplacian spectral modes, an observation EDSL over topology
  witnesses, and declarative topological constraints.

## Key operations

### Build a finite chain complex

The foundational object is the finite chain complex: a top degree together with a
validated boundary-incidence matrix at each degree. Present a circle as a triangle:
three vertices, three oriented edges glued head to tail. The degree-1 boundary sends
each oriented edge to `head − tail`; every degree above 1 is empty.

```haskell
{-# LANGUAGE DataKinds #-}

import Moonlight.Homology

circle :: Either BoundaryIncidenceShapeError (FiniteChainComplex Rational)
circle = do
  d1 <-
    mkBoundaryIncidence 3 3
      [ mkBoundaryEntry 0 0 (-1), mkBoundaryEntry 0 1 1,
        mkBoundaryEntry 1 1 (-1), mkBoundaryEntry 1 2 1,
        mkBoundaryEntry 2 2 (-1), mkBoundaryEntry 2 0 1
      ]
  pure $
    mkFiniteChainComplex (HomologicalDegree 1) $ \degree ->
      case degree of
        HomologicalDegree 1 -> d1
        HomologicalDegree 0 -> emptyBoundaryIncidenceOf 3 0
        _                   -> emptyBoundaryIncidence
```

`mkBoundaryIncidence sourceDim targetDim entries` builds the validated matrix of ∂ₙ,
the boundary map from the `sourceDim` cells of degree *n* to the `targetDim` cells of
degree *n − 1*. Each `mkBoundaryEntry source target coefficient` is one nonzero
incidence: the degree-*n* cell `source` contains the degree-*(n − 1)* cell `target` in
its boundary with that `coefficient`. Edge 0, entries `(0, 0, -1)` and
`(0, 1, 1)`, encodes ∂(edge₀) = vertex₁ − vertex₀, running from vertex 0 (tail, `-1`)
to vertex 1 (head, `+1`); edges 1 and 2 close the loop v₀ → v₁ → v₂ → v₀. Construction
is total: mismatched shapes are rejected as `BoundaryIncidenceShapeError`. `emptyBoundaryIncidenceOf sourceDim targetDim` is the zero map of that shape
(here ∂₀, three vertices to nothing), and `emptyBoundaryIncidence` the empty map used
above the top degree. `mkFiniteChainComplex topDegree atDegree` then assembles the
complex from its boundary at each degree: a top degree and one `∂` per degree.

### Betti numbers over a field

`computeBettiNumbers` is phase-gated: it verifies boundary nilpotence before any rank
backend runs, so a `BettiCapability` is the only key that unlocks the count. For the
circle, `fmap freeRank` on the result is `[1, 1]`: b₀ = 1 (one component), b₁ = 1
(one loop).

```haskell
betti :: FiniteChainComplex Rational -> Either HomologyFailure [HomologyGroup Rational]
betti =
  computeBettiNumbers
    (fieldBettiCapability RationalFieldRankBackend :: BettiCapability 'Phase2 Rational)
```

The gate is total: a malformed complex yields `Left (InvalidTopologyInput …)`, and a
non-nilpotent boundary yields `Left (LawViolation ChainNilpotenceLaw)`. A
`BettiCapability` is required for the count.

### Integral homology and torsion

Field ranks see only free rank; torsion is invisible to them. To recover the full
finitely-generated decomposition, run the Smith-normal-form backend. The real
projective plane RP² is the canonical witness: one cell in each degree 0, 1, and 2,
with the 2-cell attached by a degree-2 map, giving H₁(RP²) = ℤ/2.

```haskell
{-# LANGUAGE DataKinds #-}

import Moonlight.Homology

realProjectivePlane :: Either BoundaryIncidenceShapeError (FiniteChainComplex Integer)
realProjectivePlane = do
  d2 <- mkBoundaryIncidence 1 1 [mkBoundaryEntry 0 0 2]
  pure $
    mkFiniteChainComplex (HomologicalDegree 2) $ \degree ->
      case degree of
        HomologicalDegree 2 -> d2
        HomologicalDegree 1 -> emptyBoundaryIncidenceOf 1 1
        HomologicalDegree 0 -> emptyBoundaryIncidenceOf 1 0
        _                   -> emptyBoundaryIncidence

integralHomology ::
  FiniteChainComplex Integer -> Either HomologyFailure [HomologyGroup Integer]
integralHomology =
  runHomologyBackend (IntegralSmithBackend :: HomologyBackend Integer Integer)
```

On the result, `fmap freeRank` is `[1, 0, 0]` and `fmap torsionInvariants` is
`[[], [2], []]`: the ℤ/2 in degree 1 missed by rational and mod-2 Betti counts.

### Choosing a rank backend

`runHomologyBackend` unifies all three coefficient regimes behind one call. The
`HomologyBackend` GADT ties each backend to the coefficient type it accepts, so the
compiler rejects a backend applied to the wrong complex.

| Backend | Complex | Result |
| --- | --- | --- |
| `IntegralSmithBackend` | `FiniteChainComplex` over any `Integral` | full groups with `torsionInvariants` |
| `RationalRankBackend` | `FiniteChainComplex Rational` | rational Betti (`freeRank`) |
| `GF2RankBackend` | `FiniteChainComplex GF2` | mod-2 Betti (`freeRank`) |

`homologyBackendTag` recovers the `HomologyBackendTag` for logging or downstream
dispatch.

### Beyond Betti

The same finite chain complex feeds the higher invariants. Each is reachable from the
`Moonlight.Homology` umbrella, or from the narrower module noted below.

- **Persistence.** `mkFilteredFiniteChainComplex` builds a filtered complex;
  `mod2PersistentPairs` reads its birth/death pairs, and `BiPersistencePair` carries
  the two-parameter case. In `Moonlight.Homology.Persistence`.
- **Spectral sequences.** `mkSpectralSource` and `spectralFamilyPages` produce the
  page-by-page family; `spectralFamilyLimitPage`, `spectralFamilyStableFrom`, and
  `convergenceDepth` track convergence. In `Moonlight.Homology.Sequence`.
- **Discrete Morse.** `morseComplex` (and `morseComplexWith` /
  `refinedMorseComplex`) reduce a complex to its critical cells while preserving
  homology; `refinedMatchingCriticalCells` and `finalRefinedCriticalCellCount` read
  the reduction.
- **Topological carriers & constraints.** `mkCellCarrier` and graph skeletons build
  topology witnesses; macro-scaffold observers (`observeBettiVector`,
  `observeIntegralHomology`, `observeHarmonicCount`) interrogate them; and
  `evaluateTopologicalConstraint` checks a declarative `TopologicalConstraint`.

## Components

The pure core is carved into four private domain sublibraries along an acyclic
dependency DAG (`chain ← matrix ← topology ← sequence`), with a public entry point
and a public law harness:

- **`moonlight-homology-chain`**: base vocabulary and chain algebra: degrees, groups,
  phases, failures, cell carriers, filtration values, the `Chain` algebra, reductions,
  graded torsion, and finite abelian groups.
- **`moonlight-homology-matrix`**: boundary matrices and rank: boundary incidence,
  Smith normal form, sparse and validated matrices, field and GF(2) rank backends, the
  phase-gated Betti reducer, and effective homology.
- **`moonlight-homology-topology`**: the topology subsystem: cell complexes, graph
  skeletons, Reeb/macro-scaffold structures, discrete Morse, persistence,
  graph-Laplacian spectral modes, observers, and the integral-homology backend
  dispatcher.
- **`moonlight-homology-sequence`**: exact sequences and filtered spectral sequences.
- **`moonlight-homology`**: the public entry point below.
- **`moonlight-homology-laws`**: public law harness: boundary nilpotence, reduction,
  normalization, determinism.

Downstream packages import the public modules below.

## Public modules

| Module | Surface |
| --- | --- |
| `Moonlight.Homology` | Broad convenience surface over every module below. |
| `Moonlight.Homology.Boundary` | Boundary incidence, finite chain complexes, linear-algebra and Smith-normal-form helpers. |
| `Moonlight.Homology.Boundary.GraphGF2` | GF(2) boundary construction from graph data. |
| `Moonlight.Homology.Chain` | Degrees, groups, reductions, effective homology, graded torsion, phase-gated witnesses. |
| `Moonlight.Homology.Matrix` | Validated matrix construction and projections. |
| `Moonlight.Homology.Rank` | Field and GF(2) rank backends; Betti-capability construction. |
| `Moonlight.Homology.Rank.Field` | Rational/field rank-backend surface. |
| `Moonlight.Homology.Rank.GF2` | GF(2) rank-backend surface. |
| `Moonlight.Homology.Backend` | The `HomologyBackend` dispatcher: Smith / rational / GF(2). |
| `Moonlight.Homology.Sequence` | Exact and spectral sequences, Block–Schur reductions, graph spectral helpers. |
| `Moonlight.Homology.Topology` | Cell complexes, graph skeletons, macro-scaffolds, discrete Morse, persistence values, observers, and constraints. |
| `Moonlight.Homology.Persistence` | Filtered complexes and mod-2 persistence pairs. |
| `Moonlight.Homology.Effect.Laws` | Boundary-nilpotence and reduction law harnesses. |
| `Moonlight.Homology.Effect.Determinism` | Deterministic fingerprints for bases, incidences, and complexes. |

## Benchmarks

`tasty-bench` covers boundary construction, rank backends, reductions, and persistence helpers.

## License

MIT; see [`LICENSE`](./LICENSE). Third-party notes in
[`THIRD_PARTY_NOTICES.md`](./THIRD_PARTY_NOTICES.md).

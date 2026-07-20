# moonlight-egraph-saturation

> Part of **Moonlight**, the sheaf-theoretic computation layer beneath
> [Melusine](https://bluerose.blue) and Pale Meridian.

`moonlight-egraph-saturation` adds advanced saturation backends over the stateful matching seam in `Moonlight.EGraph.Pure.Saturation.Matching`.

## Package boundary

This package owns:

- the e-graph adapter for cohomological saturation
- backend constructors that target `MatchingAlgebra` and `MatchingStrategy` from `moonlight-egraph`

Generic obstruction/cache/policy surfaces now live in `moonlight-saturation`.

This package supplies additional backends over the shared core seam.

## Quick start

A cohomological backend is a sheaf `SectionCertificationAlgebra`, the certification
context aliased here as `EGraphSectionCertification`, paired with a
`CohomologicalPolicy` from `moonlight-sheaf`. `mkCohomologicalBackend` assembles the
pair; `cohomologicalMatchingStrategy` lifts it to a `MatchingStrategy`, the stateful
matching seam that `moonlight-egraph` saturation already consumes. Drop the resulting
strategy into the saturation configuration's `scMatchingStrategy` slot. The rest of
the fixpoint loop is untouched.

```haskell
import Moonlight.Core (ConstructorTag, HasConstructorTag, ZipMatch)
import Moonlight.EGraph.Introspection.Core.Rewrite (RewriteMorphism)
import Moonlight.EGraph.Pure.Saturation.Matching (MatchingStrategy)
import Moonlight.EGraph.Saturation.Cohomological.Backend.Instance (mkCohomologicalBackend)
import Moonlight.EGraph.Saturation.Cohomological.Backend.Matching (cohomologicalMatchingStrategy)
import Moonlight.EGraph.Saturation.Cohomological.Types (EGraphSectionCertification, SheafCapabilityAtom)
import Moonlight.Sheaf.Obstruction (CohomologicalPolicy)

cohomologicalStrategy ::
  (HasConstructorTag f, ZipMatch f, Show (ConstructorTag f), Show (f ()), Eq (RewriteMorphism f)) =>
  EGraphSectionCertification c f ->
  CohomologicalPolicy ->
  MatchingStrategy c SheafCapabilityAtom f a
cohomologicalStrategy context policy =
  cohomologicalMatchingStrategy (mkCohomologicalBackend context policy)
```

The obstruction oracle is sound but incomplete: a non-vanishing obstruction may reject
a candidate root, but a vanishing one never certifies consistency, so matching still
runs underneath. To carry a rewrite-system witness on the backend before it is lifted,
compose `withRewriteSystemWitness` between construction and the strategy call.

## Module structure

- `Moonlight.EGraph.Saturation.Cohomological.Types`
  - owns e-graph-specific occurrence and modality payloads (`PatternOccurrence`, capability environments, cache-policy selection)
  - uses `SectionCertificationAlgebra` from `moonlight-sheaf` for the sheaf certification context
- `Moonlight.EGraph.Saturation.Cohomological.Backend.Instance`
  - e-graph cohomological backend context and construction surface
- `Moonlight.EGraph.Saturation.Cohomological.Backend.Matching`
  - current `MatchingAlgebra` and `MatchingStrategy` constructors for cohomological backends
- `Moonlight.EGraph.Saturation.Cohomological.Backend.Modality`
  - modality constraints and projection policy for the backend

## Migration path

### Phase 1

Use `cohomologicalMatchingStrategy` to plug a cohomological backend into `moonlight-egraph` saturation through the shared stateful matching seam.

### Phase 2

Implement fact-sensitive cohomological pruning and cache canonicalization on rebuild.

### Phase 3

The generic fixed-point kernel and generic cohomological obstruction machinery now live in `moonlight-saturation`. This package is the e-graph adapter surface over that shared core.

## Benchmarks

The egraph benchmark corpus is one target grouped by owner; the saturation package keeps only its external comparison lane.

- Egraph corpus lanes, including context/sheaf stress and relational matching:
  `cabal bench moonlight-egraph:moonlight-egraph-bench`
- Current Moonlight Front saturation vs external egglog stays here:
  `MOONLIGHT_EGRAPH_SATURATION_BENCH_COMPARE_EGGLOG_SCALE=100 cabal run -j1 moonlight-egraph-saturation:bench:moonlight-egraph-saturation-bench`

The egglog comparison uses `EGGLOG_BIN` when set, otherwise `egglog` from `PATH`.
It reports Moonlight Front runtime separately from egglog program preparation and egglog execution so process and parser cost stay visible.

## Source-backed constraints

- Saturation semantics should remain above the matcher. Equality saturation is characterized as a least fixpoint over matching, insertion, and rebuilding rather than as one fused procedure.
  - [Semantic Foundations of Equality Saturation (ICDT 2025)](https://doi.org/10.4230/LIPIcs.ICDT.2025.11)
  - [PDF](https://drops.dagstuhl.de/storage/00lipics/lipics-vol328-icdt2025/LIPIcs.ICDT.2025.11/LIPIcs.ICDT.2025.11.pdf)

- Pattern compilation should target a backend-neutral conjunctive-query shape so backtracking, relational, and future stateful engines share the same rule compilation surface.
  - [Relational E-Matching (POPL 2022)](https://doi.org/10.1145/3498696)
  - [Open PDF](https://effect.systems/doc/popl-2022-gj/paper.pdf)

- Incremental matcher state belongs to the backend seam rather than to ad hoc scheduler state.
  - [Better Together: Unifying Datalog and Equality Saturation (PLDI 2023)](https://effect.systems/doc/pldi-2023-egglog/paper.pdf)

- Cohomological pruning must be modeled as a sound-but-incomplete obstruction oracle. Non-vanishing obstruction may reject a branch; vanishing obstruction does not certify consistency.
  - [Sheaf Theory, cohomology chapter](https://doi.org/10.1017/CBO9780511661761.007)
  - [The Cohomology of Non-Locality and Contextuality](https://doi.org/10.4204/EPTCS.95.1)
  - [Higher Cohomology in Contextuality](https://doi.org/10.4204/EPTCS.236.2)

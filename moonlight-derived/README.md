# moonlight-derived

> Part of **Moonlight**, the sheaf-theoretic computation layer beneath
> [Melusine](https://bluerose.blue) and Pale Meridian.

Derived-sheaf computation substrate for Pale Meridian: finite-poset caches,
labeled block-matrix chains, gluing and minimization routines, six-functor
operations, Morse computation, and microsupport.

`moonlight-derived` builds on [`moonlight-category`](../moonlight-category),
[`moonlight-homology`](../moonlight-homology), and [`moonlight-linalg`](../moonlight-linalg).
`moonlight-derived` lowers validated finite categories and site manifests into an
internal finite-poset cache for derived algorithms.

## Quick start

The core element is `DerivedPoset`: the finite-poset cache the derived algorithms
run on. `Moonlight.Derived.Site` lowers a source of order edges into one, validates
against self-loops and cycles, and answers reachability, star, and closure queries in
their checked, typed-failure forms.

```haskell
import Data.IntSet (IntSet)
import Moonlight.Derived.Site
  ( DerivedPoset
  , Node (..)
  , mkPosetFromOrderEdges
  , leqChecked
  , starChecked
  )
import Moonlight.Derived.Failure (DerivedFailure)

diamond :: Either DerivedFailure DerivedPoset
diamond =
  mkPosetFromOrderEdges
    [Node 0, Node 1, Node 2, Node 3]
    [ (Node 0, Node 1), (Node 0, Node 2)
    , (Node 1, Node 3), (Node 2, Node 3) ]

bottomReachesTop :: Either DerivedFailure Bool
bottomReachesTop = diamond >>= \poset -> leqChecked poset (Node 0) (Node 3)

starOfBottom :: Either DerivedFailure IntSet
starOfBottom = diamond >>= \poset -> starChecked poset (Node 0)
```

Already-validated `moonlight-category` owners lower directly: `derivedPosetFromFinCat`
takes a thin `FinCat` and `derivedPosetFromSiteManifest` takes a `SiteManifest`, both
producing the same `Either DerivedFailure DerivedPoset`.

## Architecture

- The **implementation sublibraries** (`carriers`, `linalg`, `gluing`, `functor`,
  `morse`, and `global`) own the closed derived kernel: the finite-poset cache,
  labeled matrices, injective complexes, functor implementations, gluing/minimization,
  Morse/microsupport routines, and pruning gates. The public entry point reaches them.
- **`moonlight-derived`** exposes stable entry modules and hides raw implementation
  module paths.
- **`moonlight-derived-laws`** is the law harness for poset, matrix, complex,
  functor, and determinism checks.

## Public modules

| Module | Surface |
| --- | --- |
| `Moonlight.Derived.Site` | `DerivedPoset`, checked lowering from order edges, `FinCat`, and `SiteManifest`, order-complex and microsupport entrypoints. |
| `Moonlight.Derived.Matrix` | Opaque dense/block matrix types with checked constructors and checked matrix operations. |
| `Moonlight.Derived.Complex` | Opaque injective complexes and normalized derived objects. |
| `Moonlight.Derived.Presentation.Builder` | Sole public authoring EDSL for site-lawful derived objects. |
| `Moonlight.Derived.Gluing` | Peeling, exactification, and resolution routines. |
| `Moonlight.Derived.Functor` | Push/pull, proper/exceptional variants, closed support, tensor, Verdier dual, and Quillen-A witnesses. |
| `Moonlight.Derived.Triangulated` | Derived maps, shifts, cones, triangles, truncation, and internal Hom. |
| `Moonlight.Derived.Morse` | Poset cohomology, hypercohomology, support, criticality, and pipeline helpers. |
| `Moonlight.Derived.Pruning` | Laplacian, spectral, and Verdier pruning gates. |
| `Moonlight.Derived.Failure` | Closed `DerivedFailure` ADT and boundary conversion to `MoonlightError`. |

Public import paths are the modules above. Internal implementation modules live
under `Moonlight.Derived.Pure.*` in the implementation sublibraries.

## Typed failures

Expected construction and validation failures are values. `DerivedFailure` covers
poset self-loops/cycles/unknown nodes, non-thin category lowering, matrix shape and
metadata mismatches, complex law failures, invalid projections/support, and tensor
layout mismatches. Public construction APIs return `Either DerivedFailure ...` where
this package owns the invariant. Existing Moonlight-facing APIs translate only at
package boundaries.

## Benchmarks

Default short sweep:

```sh
cabal bench moonlight-derived:moonlight-derived-bench -j1 --benchmark-options='--timeout=1s --csv=moonlight-derived-bench-m4-pro-default.csv'
```

Full hostile probe sweep:

```sh
cabal run moonlight-derived:moonlight-derived-bench-probe -- --all-hostile --large --csv moonlight-derived-hostile-m4-pro.csv
```


## Validation

```sh
cabal build moonlight-derived -j1
cabal build moonlight-derived:moonlight-derived-laws -j1
cabal test moonlight-derived:moonlight-derived-test moonlight-derived:moonlight-derived-public-surface -j1
cabal build moonlight-derived:moonlight-derived-bench -j1
```

If upstream `moonlight-linalg` or `moonlight-homology` fails first, fix that owner
before claiming `moonlight-derived` readiness.

## License

MIT; see [`LICENSE`](./LICENSE). Third-party notices are recorded in
[`THIRD_PARTY_NOTICES.md`](./THIRD_PARTY_NOTICES.md).

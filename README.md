# Moonlight

Computational categorical mathematical foundations for Haskell.

This repository is the public mirror of Melusine Nebula and the local Moonlight
packages needed to rebuild it from source. The mirror is generated from Pale
Meridian with josh, so package directories are flat and the dependency closure
is kept together.

## Packages

The mirror contains `melusine-nebula` plus the 27 local packages below it.

| Package | Description |
|---|---|
| `moonlight-category` | Total categorical foundation: abstract category laws, finite categories, sites, indexed categories, and simplicial structures. |
| `moonlight-core` | Shared mathematical basis: numeric classes, structural identity, orders, patterns, fixpoints, union-find, finite registries. |
| `moonlight-discrete` | Finite and discrete structures: automata, Boolean constraints, CSP search, typed optic addressing, graph vocabularies, graph deltas, and repair indices. |
| `moonlight-saturation-core` | Generic fixed-point saturation kernel. |
| `moonlight-pale` | Shared diagnostics, law-test, and GHC/HIE source-surface support library, as public sublibraries. |
| `moonlight-linalg` | Typed dense and sparse linear algebra: matrices, GF(2), Smith normal form, symmetry-indexed operators, and spectral solvers. |
| `moonlight-differential` | Differential maintenance library for incremental runtime state. |
| `moonlight-delta` | Boundary-aware delta calculus for patches and incremental change. |
| `moonlight-control` | Typed control algebra for gates, scheduling, homology support, and deterministic engines. |
| `moonlight-algebra` | Law-governed algebraic structures: semigroups, monoids, groups, lattices, domains, modules, vector spaces, modular arithmetic, free constructions, and checked finite lattices. |
| `moonlight-flow-key` | Stable relational keys, tuple roles, scopes, and digests. |
| `moonlight-probability` | Validated probabilities, log-domain arithmetic, pure sampling, categorical distributions, entropy, and wrappers over statistics. |
| `moonlight-egraph` | Exact e-graph kernel strata: core classes, contexts, extraction, rewrite integration, homology, relational analysis, and pure saturation. |
| `moonlight-flow-model` | Relational model deltas and schema vocabulary. |
| `moonlight-flow-plan` | Relational query planning ownership. |
| `moonlight-homology` | Chain complexes, validated boundary matrices, rank backends, Betti numbers, spectral sequences, discrete Morse reductions, and persistence helpers. |
| `moonlight-flow-storage` | Relational physical storage representations. |
| `moonlight-flow-execution` | Relational execution contracts and algorithms. |
| `moonlight-sheaf` | Site-indexed sheaves, cosheaves, descent, runtime gluing, cochains, repair, and obstruction tracking. |
| `moonlight-derived` | Closed finite-poset complexes, labeled block-matrix chains, gluing and minimization routines, six-functor operations, Morse computation, and microsupport. |
| `moonlight-saturation` | Fixed-point saturation kernels, host protocols, obstruction algebras, and context-aware saturation runtime. |
| `moonlight-rewrite` | Rewrite algebra, runtime, DSL, proof context, relational backend, and front-end integration. |
| `moonlight-egraph-fuzzy` | Optional fuzzy and simplicial refinement layers for Moonlight e-graphs. |
| `moonlight-rewrite-relational-front` | E-graph-backed relational front for Moonlight rewrites. |
| `moonlight-geometry` | Certificate-bearing SDF algebra, metrics, connections, curvature, geodesics, holonomy, robust predicates, meshing, and rewrite lawfulness. |
| `moonlight-analysis` | Dual-number forward AD, convergence contracts, and deterministic numerical solvers. |
| `moonlight-egraph-introspection` | Rewrite-nerve bridge from finite rewrite categories to simplicial, sheaf, and homological summaries. |
| `melusine-nebula` | Source-ingesting context e-graph tool for Haskell modules, with saturation, extraction, diagnostics, candidate generation, and write-back support. |

## Building

```sh
cabal build all
cabal build melusine-nebula:exe:melusine-nebula
```

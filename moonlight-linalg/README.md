# moonlight-linalg

> Part of **Moonlight**, the sheaf-theoretic computation layer beneath
> [Melusine](https://bluerose.blue) and Pale Meridian.

Typed dense, sparse, finite-field, and Krylov linear algebra for Pale Meridian's
foundation packages.

`moonlight-linalg` is Moonlight's numerical linear-algebra tier. Building on
[`moonlight-core`](../moonlight-core),
[`moonlight-algebra`](../moonlight-algebra), and
`moonlight-discrete:automata`, it provides the matrix, vector,
GF(2), sparse-storage, Smith-normal-form, eigen, and Krylov machinery used by
homology, analysis, sheaf, geometry, and solver packages.

The package intentionally speaks Moonlight's own shape/domain failure vocabulary.
Callers get typed `MoonlightError` failures for malformed matrices, invalid solver
configuration, and incompatible dimensions instead of partial indexing or runtime
bottoms.

## Quick start

Dense matrices carry their shape and scalar as type indices: `Matrix r c a` is an
`r`-by-`c` matrix over `a`. Construction is validated: `fromListMatrix` returns
`Either MoonlightError` rather than bottoming on a wrong element count, and a
product's shared inner dimension is fixed at the type level, so an entire
computation composes in `Either MoonlightError`.

```haskell
import Moonlight.LinAlg (Matrix, fromListMatrix, mult, toListMatrix)
import Moonlight.Core (MoonlightError)

product22 :: Either MoonlightError (Matrix 2 2 Double)
product22 = do
  left  <- fromListMatrix @2 @2 @Double [1.0, 2.0, 3.0, 4.0]
  right <- fromListMatrix @2 @2 @Double [2.0, 0.0, 1.0, 2.0]
  mult left right
```

Here `mult :: Matrix r m a -> Matrix m c a -> Either MoonlightError (Matrix r c a)`
forces the two matrices to meet on `m`; the result reads back as
`toListMatrix <$> product22 == Right [4.0, 4.0, 10.0, 8.0]`.

## What it provides

- **Dense row validation.** `Moonlight.LinAlg.Dense` exposes validated rectangular row
  authoring and typed transpose, zip, product, and concatenation helpers. This is
  the shape/error authoring surface for dense rows.
- **Core numeric surfaces.** `Moonlight.LinAlg.Dense` owns typed vectors, matrices,
  dynamic dense carriers, primitives, and decompositions; `Moonlight.LinAlg.Geometry`
  owns `Vec2`, `Vec3`, AABB/AABB2, frames, affine transforms, and compact symmetric
  2D/3D carriers.
- **Finite-field and bit-packed storage.** `Moonlight.LinAlg.Dense` exposes GF(2)
  values and packed bit matrices for boundary and incidence computations.
- **Sparse operators.** COO/CSR/CSC/Packed sparse encodings, sealed
  preconditioner selection, and solver helpers for graph and mesh operators.
- **Algebraic matrix backends.** Row-reduction, PLU, RREF, Smith normal form,
  symmetric eigen kernels, and domain-level operations.
- **Projected Krylov structure.** Arnoldi, Lanczos, block Lanczos, validated
  decompositions, and projected tridiagonal/block-tridiagonal carriers. Krylov owns
  execution machinery; spectral requests own eigenvalue/eigenpair demand.
- **Structured selected spectra.** Path-Laplacian and symmetric-tridiagonal
  selected-mode queries take the certified structured fast path instead of paying
  for dense eigensolve or ambient eigenvector lifting when values are enough.
  Moonlight recognizes certified operator structure and computes only the requested spectral data.
- **Native LAPACK boundary.** `Moonlight.LinAlg.Native` exposes effectful LAPACK-backed
  dense symmetric eigensolves, selected tridiagonal/block-tridiagonal eigensolves,
  and least-squares solves for benchmark and downstream code that explicitly wants
  the platform numerical library instead of the pure Moonlight fallback.

## Public modules

| Module | Surface |
| --- | --- |
| `Moonlight.LinAlg` | Broad public surface for dense, sparse, operator, spectral, domain, geometry, statics, and immutable Krylov modules. |
| `Moonlight.LinAlg.Dense` | Dense vectors/matrices, validated dense-row authoring, GF(2), exterior algebra, basic operations, decompositions, field operations, direct solvers, and primitives. |
| `Moonlight.LinAlg.Sparse` | Sparse matrix carriers, packed sparse operators, sealed preconditioner families, and sparse iterative solvers. |
| `Moonlight.LinAlg.Operator` | Abstract affine-normalized linear operators with explicit self-adjoint construction boundaries. |
| `Moonlight.LinAlg.Spectral` | Eigenvalue/eigenpair requests and contiguous result views, dispatched by demand and operator structure above Krylov. |
| `Moonlight.LinAlg.Krylov` | Public Arnoldi/Lanczos decomposition, projected tridiagonal/block-tridiagonal carriers, and block Lanczos surface. |
| `Moonlight.LinAlg.Native` | Effectful native LAPACK backend boundary. On macOS it links Accelerate; elsewhere it expects BLAS/LAPACK libraries. |
| `Moonlight.LinAlg.Domain` | Domain-level algebraic operations, including Smith normal form. |
| `Moonlight.LinAlg.Geometry` | `Vec2`, `Vec3`, AABB/AABB2, frames, affine transforms, and compact symmetric 2D/3D carriers. |
| `Moonlight.LinAlg.Statics` | Statics types, assembly, equilibrium compilation, and support checking. |

The `Moonlight.LinAlg.Pure.*`, `Moonlight.LinAlg.Internal.*`, and
`Moonlight.LinAlg.Effect.*` leaves live in graded implementation sublibraries
(`carrier`, `structured`, `eigen`, `geometry`, `dense`, `domain`, `sparse`,
`statics`, `spectral`, `native`), with the dependency DAG cabal-enforced and
native linkage confined to `moonlight-linalg-native`. Public callers use
the public modules above; the slice modules define implementation ownership behind that
public vocabulary.

## Benchmark artifacts

Repository tooling generates benchmark artifacts:

```sh
scripts/tooling/generate_moonlight_linalg_bench_artifacts.py
```

The generator runs `cabal test moonlight-linalg-test -j1`, then runs the
short default `moonlight-linalg-bench` target with a CSV `tasty-bench` report and
renders SVG artifacts under `/tmp` by default. Use `--output-dir` to choose a
destination.

The default bench is decomposed across dense-row validation, dense decompositions/solvers,
sparse storage, sparse iterative solvers, domain algebra, GF(2), exterior powers,
geometry/statics, spectral demand dispatch, sparse Krylov, native LAPACK, and
structured projected block eigensolve. Heavier strata stay opt-in:

- `--broad-medium` / `MOONLIGHT_LINALG_BENCH_ENABLE_BROAD_MEDIUM=1`
- `--broad-large` / `MOONLIGHT_LINALG_BENCH_ENABLE_BROAD_LARGE=1`
- `--sparse-large` / `MOONLIGHT_LINALG_BENCH_ENABLE_SPARSE_LARGE=1`
- `--include-100k` / `MOONLIGHT_LINALG_BENCH_ENABLE_100K=1`
- `--projected-medium` / `MOONLIGHT_LINALG_BENCH_ENABLE_PROJECTED_MEDIUM=1`
- `--large-projected` / `MOONLIGHT_LINALG_BENCH_ENABLE_PROJECTED_LARGE=1`
- `--native-large` / `MOONLIGHT_LINALG_BENCH_ENABLE_NATIVE_LARGE=1`

Use `--diagnostic-sweep` for the medium broad rows, 50k sparse row, and
144-dimensional projected rows.

The default native LAPACK group keeps small DSYEV rows and a small DSTEMR
selected-tridiagonal row. The 10k DSTEMR path-Laplacian row is opt-in because it
is a native-boundary stress case for deeper runs.

For a fast local sanity sweep, skip the default calibrated sampling ceremony:

```sh
cabal bench moonlight-linalg:moonlight-linalg-bench -j1 --benchmark-options='--once'
```

This executes every default benchmark row once through the same workload owners
and reports the slowest rows. Use the calibrated default only when the numbers
are going into evidence.

Benchmark outputs are generated explicitly for each measurement run.

## Relationship to external linear-algebra packages

General-purpose Haskell linear algebra packages are better choices for ordinary
numerical applications. `moonlight-linalg` exists because Pale Meridian needs compact
compiler-local carriers, GF(2) and integer-domain hooks, exact shape/domain failures,
and structured Krylov/projected-operator types that compose with the rest of the
Moonlight foundation stack. Its strongest hot path is selected structured spectra,
especially path-Laplacian/tridiagonal modes. Dense nested rows serve validated
authoring. The native LAPACK boundary is deliberately effectful and isolated from
pure APIs.

## License

MIT; see [`LICENSE`](./LICENSE). Third-party attribution is recorded in
[`THIRD_PARTY_NOTICES.md`](./THIRD_PARTY_NOTICES.md).

# moonlight-algebra

> Part of **Moonlight**, the sheaf-theoretic computation layer beneath
> [Melusine](https://bluerose.blue) and Pale Meridian.

The law-governed algebraic tower the rest of the cathedral stands on.

`moonlight-algebra` is Moonlight's algebraic tier. Building on
[`moonlight-core`](../moonlight-core), it supplies a tower of
pure, law-governed algebraic structures: standard semigroups/monoids with
operation-selecting additive and multiplicative wrappers, group refinements, the
lattice hierarchy up to Heyting and Boolean algebras, integral/GCD/Euclidean
domain refinements, modules and vector spaces, modular arithmetic, number
theory, free structures, and sparse vectors. Each structure is a thin type
class or newtype boundary; its laws are stated in the module header and
exercised by the private law-suite sublibrary.

## Relationship to `moonlight-core`

`moonlight-core` owns the operation-bearing numeric tower: `AdditiveMonoid`,
`AdditiveGroup`, `MultiplicativeMonoid`, `Semiring`, `Ring`, `CommutativeRing`,
and `Field`. `Moonlight.Algebra.Pure.Ring` re-exports the law-only semiring and
commutative-ring classes from core and adds only the stronger domain refinements:
`IntegralDomain`, `GCDDomain`, `EuclideanDomain`, and
`CanonicalEuclideanDomain`. There is one arithmetic vocabulary; the ring layer
reuses core's `zero`, `one`, `add`, and `mul`.

## What it provides

- **Groups, free structures and actions.** Standard `Semigroup`/`Monoid`,
  `Group`/`AbelianGroup` law refinements, `Additive` and `Multiplicative`
  wrappers for carriers with several lawful operations, free monoids and free
  abelian groups, the two-element sign/orientation group (ℤ/2), and monoid
  actions on a carrier with a group-acting refinement.
- **The lattice hierarchy.** Join/meet semilattices up to Heyting and Boolean
  algebras. Compiled finite lattices live in the public
  `moonlight-algebra:finite-lattice` sublibrary.
- **Rings and arithmetic.** Semiring → commutative ring → integral/GCD/Euclidean
  domains; modular arithmetic (`Zn`); quotient rings `R/(n)` of a Euclidean
  domain; number theory and gcd; and univariate polynomials over a coefficient
  ring (Horner evaluation, a free module on monomial degrees).
- **Modules and magnitudes.** Modules, free modules, vector spaces, bilinear
  spaces over a field, and real-valued magnitudes for obstructions.
- **Constructions.** Power-set lattices, finite *n*-fold product algebras with
  coordinatewise structure, quotients, and sparse vectors with finite support.

## Tiny usage examples

```haskell
import Moonlight.Algebra.Pure.Group

difference :: Additive Integer
difference = groupDifference (Additive 3) (Additive 5)
```

Sparse kernels:

```haskell
compiled = compileSparseLinearMap 128 (\i -> [(i, 1), (i + 1, 2)])
```

## Public modules

| Module | Surface |
| --- | --- |
| `Moonlight.Algebra` | Umbrella re-export of the whole tower, plus the operation-bearing numeric classes from `moonlight-core`. |
| `Moonlight.Algebra.Pure.Group` | Standard `Semigroup`/`Monoid`, `Group`/`AbelianGroup`, and `Additive`/`Multiplicative` wrappers. |
| `Moonlight.Algebra.Pure.FreeMonoid`, `.FreeAbelianGroup` | Free monoids and free abelian groups. |
| `Moonlight.Algebra.Pure.Action` | Monoid actions (`Action`) and their group-acting refinement (`InvertibleAction`). |
| `Moonlight.Algebra.Pure.Orientation` | The two-element sign/orientation group ℤ/2 with a direct standard monoid/group instance. |
| `Moonlight.Algebra.Pure.Lattice` | Join/meet semilattices up to Heyting and Boolean algebras. |
| `Moonlight.FiniteLattice.*` | Public `finite-lattice` sublibrary: checked finite-order compilation, dense query plans, resident keys, Heyting implication, fixpoints, covers, presentation builders, and support kernels. |
| `Moonlight.Algebra.Pure.Ring` | Re-exported semiring/commutative-ring law classes plus integral/GCD/Euclidean/canonical-domain refinements. |
| `Moonlight.Algebra.Pure.Zn`, `.Quotient` | Modular arithmetic and quotient rings `R/(n)`. |
| `Moonlight.Algebra.Pure.NumberTheory`, `.GCD` | Number theory and gcd. |
| `Moonlight.Algebra.Pure.Polynomial` | Univariate polynomials over a coefficient ring; a free module on monomial degrees. |
| `Moonlight.Algebra.Pure.Module` | Modules, free modules, vector spaces, and bilinear spaces over a field. |
| `Moonlight.Algebra.Pure.Magnitude` | Real-valued magnitudes for obstructions. |
| `Moonlight.Algebra.Pure.PowerSet`, `.Product` | Power-set lattices and finite *n*-fold product algebras (coordinatewise structure). |
| `Moonlight.Algebra.Pure.SparseVec` | Sparse vectors with finite support. |

All `Moonlight.Algebra.Pure.*` leaves are exposed by the public `abstract`
sublibrary and gathered by the `Moonlight.Algebra` surface. The unsafe GCD witness
(`Moonlight.Algebra.Unsafe.GCDWitness`) lives in the private
`moonlight-algebra-internal` sublibrary, the compiled finite lattice realization
lives in the public `finite-lattice` sublibrary, and the law suite
(`Moonlight.Algebra.Effect.Laws`, `Moonlight.Algebra.Test.Generators`) lives in
the private `moonlight-algebra-laws` sublibrary.

## License

MIT; see [`LICENSE`](./LICENSE).

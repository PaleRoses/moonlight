# moonlight-analysis

> Part of **Moonlight**, the sheaf-theoretic computation layer beneath
> [Melusine](https://bluerose.blue) and Pale Meridian.

Numerical analysis and automatic differentiation substrate for Pale Meridian.

## Quick start

The core primitive is the dual number: `Dual s a` carries a value alongside its
tangent, so an ordinary numeric expression differentiates itself as it evaluates.
A function written once against the ring interface runs unchanged on dual numbers,
and `diff` recovers both the value and its exact derivative from a single forward
pass.

```haskell
import Moonlight.Analysis (Dual, diff, derivative, sinDual, expDual)
import Moonlight.Core (Ring, mul)

cubic :: Ring a => Dual s a -> Dual s a
cubic x = mul x (mul x x)

valueAndSlope :: (Double, Double)
valueAndSlope = diff cubic 2.0

composedSlope :: Double
composedSlope = derivative (expDual . sinDual) 0.37
```

Here `valueAndSlope` is `(8.0, 12.0)`: the value of `x³` at `2` and its
derivative `3x²`. `composedSlope` is `cos 0.37 * exp (sin 0.37)`: the chain
rule falls out of composition, with nothing differenced numerically.

The same differentiable function drives the deterministic solvers. `findRootNewton`
takes a `forall s. Dual s a -> Dual s a` and draws each Newton step's derivative
straight from the tangent, returning an explicit `Termination` verdict rather than
a bare number.

```haskell
import Moonlight.Analysis
  ( Dual, liftDual, findRootNewton, mkIterationLimit
  , Tolerance (AbsTolBound), Termination )
import Moonlight.Core (MoonlightError, absTol, mul, sub)

sqrtTwo :: Dual s Double -> Dual s Double
sqrtTwo x = sub (mul x x) (liftDual 2.0)

solveSqrtTwo :: Either MoonlightError (Termination Double)
solveSqrtTwo = do
  tol   <- absTol 1e-12
  limit <- mkIterationLimit 30
  pure (findRootNewton (AbsTolBound tol) limit sqrtTwo 1.5)
```

Tolerances and iteration limits are built through checked smart constructors, so
`solveSqrtTwo` evaluates to `Right (Converged 1.4142135623730951)`.

## Build policy

This package follows project-level Cabal policy from the repository `cabal.project`.

Current defaults are:

- `tests: True`: test suites are always included in solver plans.
- `benchmarks: False`: benchmarks are opt-in.

This keeps verification mandatory while preventing benchmark components from inflating routine build/test cycles.

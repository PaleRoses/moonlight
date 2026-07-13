# moonlight-saturation

> Part of **Moonlight**, the sheaf-theoretic computation layer beneath
> [Melusine](https://bluerose.blue) and Pale Meridian.

`moonlight-saturation` is Moonlight's canonical fixed-point saturation package.
Its generic kernel, protocol, obstruction algebra, and context-aware public
runtime are one Cabal package with named public sublibraries.

## Quick start

The core element is context-aware fixed-point saturation: author a rewrite
program in the scoped source EDSL, pin it to a `PlanSpec`, and drive it over a
host carrier until no further rule fires. The runtime is parametric over a host
carrier interface `u`: the type families and system classes of
`Moonlight.Saturation.Substrate`, supplied by a backend such as the e-graph
engine. The caller provides rule sources, matching strategy, and carrier. The
public entry point `Moonlight.Saturation` re-exports the source EDSL, the plan
spec, and the context driver together.

```haskell
import Moonlight.Saturation

remediation :: RewriteSystem u => SatRuleSource u -> ProgramM u ()
remediation rule =
  base $ do
    ruleId <- rewrite rule
    activateBaseRewrite ruleId

saturate ::
  ( RebuildSystem u,
    GraphApply u,
    Ord (SatRuleKey u),
    Ord (SatContext u),
    Ord (SatClassId u),
    Eq (SatFactStore u),
    Semigroup (SatFactIndex u)
  ) =>
  SatMatchStrategy u ->
  SatRuleSource u ->
  SatGraph u ->
  Either (SaturationError u (SatRuleKey u)) (SaturationTermination, Int)
saturate strategy rule carrier = do
  let spec    = defaultPlanSpec (SaturationBudget 4 32) strategy
      runSpec = plainContextRunSpec spec mempty
  result <- runContextProgram runSpec (remediation rule) carrier
  let report = crrResult result
  pure (srResult report, reportMatchesApplied report)
```

`runContextProgram` compiles the program against the plan and runs it through the
plain runtime policy; `crrResult` yields the `SaturationReport`, whose `srResult`
is `ReachedFixedPoint` once the frontier is exhausted and `reportMatchesApplied`
counts the rewrites committed. A `mempty` termination goal saturates to the
fixed point; a nontrivial goal stops the run as soon as the carrier satisfies it.

## Components

```text
moonlight-saturation:core
  fixed-point saturation kernel

moonlight-saturation:protocol
  substrate, matching, application, and rebuild interfaces

moonlight-saturation
  context-aware runtime and support layer

moonlight-saturation:obstruction
  cohomological obstruction algebras over the saturation protocol
```

Dependency shape:

```text
moonlight-saturation:core
  -> moonlight-saturation:protocol
       -> moonlight-saturation
       -> moonlight-saturation:obstruction
```

E-graph-specific saturation belongs to the e-graph saturation backend.

## Use

Depend on the narrow component you import from:

```cabal
build-depends:
    moonlight-saturation:core
  , moonlight-saturation:protocol
```

The Haskell module imports stay ordinary:

```haskell
import Moonlight.Saturation.Core
import Moonlight.Saturation.Substrate
```

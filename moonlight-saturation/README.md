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

The `protocol` role is carried by the `Moonlight.Saturation.Substrate` and
`Moonlight.Saturation.Matching` module names themselves; there is deliberately
no `Moonlight.Saturation.Protocol` namespace (decision recorded 2026-07-15).

E-graph-specific saturation belongs to the e-graph saturation backend.

## Tests

Each public component owns a local suite, and `moonlight-saturation-test` glues
those same suite values into the package-wide run:

```text
moonlight-saturation-core-test
moonlight-saturation-protocol-test
moonlight-saturation-context-test
moonlight-saturation-obstruction-test
moonlight-saturation-test              # aggregate
```

From `compiler/`, run the complete package surface with:

```sh
cabal test moonlight-saturation:moonlight-saturation-test -j1
```

Reusable semantic fixtures live in `test/support`; component directories own
only their local specifications and suite aggregation. In particular, the
protocol lane exercises the `MatchingAlgebra` single-query, batch, scope
transport, unit-wrapper, and typed-obstruction paths rather than borrowing
coverage from a downstream backend. The aggregate covers deterministic core
termination-path tests and obstruction tests for incremental merge, inverse
lookup, live refresh, exact search, and regional folding.

## Benchmarks

The benchmark topology mirrors the test topology:

```text
moonlight-saturation-core-bench
moonlight-saturation-protocol-bench
moonlight-saturation-context-bench
moonlight-saturation-obstruction-bench
moonlight-saturation-bench             # aggregate
```

List every benchmark without sampling it:

```sh
cabal bench moonlight-saturation:moonlight-saturation-bench -j1 \
  --benchmark-options='--list-tests'
```

Every benchmark case performs a pure semantic preflight before `tasty-bench`
starts. A rejected fixture or unexpected result is a typed
`BenchmarkObstruction` interpreted as a failing process at the executable
boundary; failures are never converted to numeric sentinels and never timed as
successful work. Prepared inputs are fully forced through `env`, and timed
results are forced through `nf`.

The benchmark matrix measures these authoritative owners directly:

- `core`: goal, planned-stop, post-rebuild convergence, and advance-only
  iteration-limit paths through `runSaturation`;
- `protocol`: single preparation, single execution, prepared batches, scope
  transport, and disjoint/overlapping context descent and gluing;
- `context`: contextual source compilation, all-admitted and mixed-admission
  scheduling, and cold contextual plan execution with fact derivation;
- `obstruction`: sparse and half-replacement aggregate merge, sparse and dense
  inverse lookup, live-pruning refresh, exhaustive and lower-bound-pruned
  feasible-family search, and a mixed accepted/rejected regional fold.

Fixture construction, source compilation for runtime inputs, candidate-space
construction, and list/map forcing happen outside the timed action. The
benchmark digest consumes every result element relevant to the named owner, so
lazy residue cannot masquerade as speed.

The Apple M4 Pro baseline, environment, and reproduction command are recorded
in [`docs/BENCHMARKS-m4-pro.md`](docs/BENCHMARKS-m4-pro.md).

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

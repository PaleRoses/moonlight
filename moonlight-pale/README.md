# pale

> Part of **Moonlight**, the sheaf-theoretic computation layer beneath
> [Melusine](https://bluerose.blue) and Pale Meridian.

`pale` is Moonlight's **shared support library** for the mathematical foundation
packages (`moonlight-core`, `moonlight-category`, `moonlight-delta`, …). It carries
the cross-cutting machinery those packages all reach for: a diagnostic algebra, a
property/law-testing surface, and the GHC/HIE source-reading front-end, factored out
of the foundation so every package can share one implementation.

It ships as a family of public sublibraries, each with a single role, so a consumer
depends exactly on the surface it needs.

## What it provides

- **A diagnostic algebra** (`diagnostic`). Severities, an accumulating `Diagnosed`
  writer, and a sheaf-decomposed vocabulary of run outcomes: projection/restriction
  propagation over a site, replay statistics carried in validated refinement types,
  rewrite and saturation traces, gluing summaries, and whole-object structural
  summaries. Pure `base` + `containers`.
- **A test surface** (`test`, `test-surface`, `test-laws`). Site-shaped assertions,
  fixtures, resource-path handling and runners; an import-discipline registry that
  checks sheaf-layer boundaries; and algebraic law predicates (`Semigroup`, `Monoid`,
  lattice, restriction) with a law-suite DSL that names each law it checks.
- **A GHC/HIE source surface** (`ghc-surface`). Parsing Haskell source into a
  scoped, normalized expression algebra with structural equivalence and faithful
  rendering; `.hie` reading, source-key indexing, and a type-word oracle; and a
  module-surface summary. This is the sublibrary that speaks `ghc`.
- **Compile-diagnostic snapshots** (`diagnostic-ghc`). Driving the compiler to
  capture and serialize compile diagnostics for tests that assert on them.

## Public sublibraries

| Sublibrary | Depends on | Surface |
| --- | --- | --- |
| `pale:diagnostic` | `base`, `containers` | The diagnostic algebra, decomposed by sheaf role: `Pale.Diagnostic.Site.*` (severity, boundary, homotopy, cohomology), `Pale.Diagnostic.Section.*` (propagation, replay, rewrite, saturation), `Pale.Diagnostic.Gluing.*` (outcome summaries and propagation reports), `Pale.Diagnostic.Global.Summary` (structural summaries), and `Pale.Diagnostic.Derived.Rewrite` (computed rewrite/transition summaries). |
| `pale:test` | `tasty`, `hedgehog`, `QuickCheck` | Site-shaped test assertions, fixtures, runners, resource paths, an `Either`-returning global assertion, and a bounded-recursion bridge. |
| `pale:test-surface` | `pale:test`, `pale:ghc-surface` | Import-discipline checks over the sheaf layering (`Pale.Test.Gluing.Discipline`, `Pale.Test.Gluing.Registry`). |
| `pale:test-laws` | `pale:test`, `moonlight-core` | Algebraic law predicates (`Pale.Test.Laws.Algebraic`, `.Lattice`, `.Restriction`) and the `Pale.Test.LawSuite` DSL. |
| `pale:ghc-surface` | `ghc`, `moonlight-core` | The GHC/HIE source-surface: `Pale.Ghc.Expr` (scoped/normalized expression algebra + render), `Pale.Ghc.Hie.*` (reading, source keys, type words), `Pale.Ghc.ModuleSurface`. |
| `pale:diagnostic-ghc` | `pale:test`, `process` | `Pale.TestSupport.CompileDiagnostics`: compile-diagnostic snapshot support. |

## The diagnostic algebra

The `diagnostic` sublibrary is the piece the foundation reaches for most. Its center
is a small accumulating writer over a diagnostic type of the caller's choosing:

```haskell
import Pale.Diagnostic.Site.Core

data Note = Shadowed String | Unused String
  deriving stock (Eq, Show)

severityOf :: Note -> DiagnosticSeverity
severityOf Shadowed{} = DiagWarning
severityOf Unused{}   = DiagInfo

analyze :: Diagnosed Note Int
analyze = do
  emitDiagnostic (Shadowed "x")
  emitDiagnostic (Unused "Data.List")
  pure 42
```

`runDiagnosed analyze` returns `(42, [Shadowed "x", Unused "Data.List"])`; the value
and its accumulated notes travel together, and `filterBySeverity severityOf DiagWarning`
keeps only the notes at or above a threshold.

Everything above the `Site` layer is decomposed the same way the sheaf theory
decomposes: local *sections* (a single projection/restriction run) glue into
*reports*, and reports fold to *global* structural summaries. Fallible arithmetic,
counts, nanoseconds, and rates, is carried in validated refinement types
(`NonNegativeCount`, `Nanoseconds`, `Rate`) whose smart constructors return typed
errors rather than admitting a nonsensical value.

## Naming

The package is named `pale`, and its modules live under the `Pale.*` namespace,
distinct from the foundation's `Moonlight.*`.

## License

MIT; see [`LICENSE`](./LICENSE).

# moonlight-egraph

> Part of **Moonlight**, the sheaf-theoretic computation layer beneath
> [Melusine](https://bluerose.blue) and Pale Meridian.

`moonlight-egraph` is Moonlight's equality-saturation engine: a deterministic,
pure e-graph kernel; a relational lowering with worst-case-optimal matching; a
saturation engine with budgets, guards, and e-class analyses; and a
context-sensitive layer in which equalities, rewrite rules, and assumed facts
live at objects of a finite context lattice and glue across it by sheaf
descent. Laws are authored as literal equation strings over your own language
functor and compile to relational query plans.

## Prior art

The rewrite-rule discipline, e-class analyses, and cost-based extraction
follow [egg](https://egraphs-good.github.io/). The relational lowering follows
[egglog](https://github.com/egraphs-good/egglog). The context layer
generalizes colored e-graphs from a forest of assumption branches to an
arbitrary finite lattice, with derived fibers and descent-gated merges.
Citations are in the acknowledgements.

## What it provides

- **A deterministic pure kernel.** Hash-consing, congruence-closure rebuild,
  structural stores, and a delta calculus. Every operation is total and
  explicit-error.
- **E-class analyses.** Semilattice-valued analyses with lawful joins
  (`Moonlight.EGraph.Pure.Analysis`), readable by guards during saturation,
  so rules can fire on analytic knowledge rather than syntactic shape.
- **Relational matching.** Patterns compile to conjunctive queries over the
  node relation, answered by generic join, with incremental rematch after a
  change proportional to the dirty set.
- **Equality saturation.** Budgeted, deterministically scheduled rounds;
  conditional rules through capability guards (analysis-backed predicates)
  and fact guards (a fact store fed by fact rules); pluggable matching
  strategies.
- **Extraction.** Cost-algebra extraction over stable snapshots, with
  caching, guides, anti-unification, and post-extraction rewriting.
- **The context layer.** E-graphs indexed by a finite context lattice:
  equalities authored at single contexts and visible on the up-set, matching
  whose cost scales with authored differences rather than
  |lattice| × |graph|, merges gated by sheaf descent with typed obstruction
  certificates, and symbolic powerset sites for assumption lattices of up to
  2⁶² contexts without materialization.
- **Homological descent helpers.** Representatives, gerbes, and the descent
  computations the context layer's admissibility gate consumes.
- **Reusable test algebras.** Example languages, context-lattice fixtures,
  and a saturation harness for consumers' law suites.

## Kernel and public sublibraries

| Sublibrary | Surface |
| --- | --- |
| `moonlight-egraph-core` | Dependency-minimal kernel types, hash-consing, rebuild, analyses, the base extraction stack, deltas and change tracking. |
| `moonlight-egraph:relational` | The relational lowering: node relation sources and direct/WCOJ matching. |
| `moonlight-egraph:context` | Context-lattice e-graphs: authored fibers, the persistent regional quotient, annotated views, descent queries, and proof carriers. |
| `moonlight-egraph:homology` | Representatives, gerbes, and homological descent. |
| `moonlight-egraph:extraction` | Extraction guides, anti-unification, extraction-time rewriting. |
| `moonlight-egraph:rewrite` | Guard evaluation, compiled guard regions, rule instantiation, the rewrite program runner. |
| `moonlight-egraph:pure-saturation` | The saturation engine: substrate, query compilation, scheduling front, packed plans, seeds, observation logic. |
| `moonlight-egraph:session` | The session calculus over saturation runs. |
| `moonlight-egraph:test-algebras` | Example languages, context fixtures, assertions, and the `Moonlight.EGraph.Test.Saturation` harness. |

The equation-authoring front ships in the companion package
[`moonlight-egraph-introspection`](../moonlight-egraph-introspection)
(`Moonlight.EGraph.Introspection.Core.Equation`); the complete worked consumer
is [`moonlight-surface`](../moonlight-surface), from which every example below
is taken.

## Equational law authoring

You do not build rewrite rules from pattern combinators. You define a language
functor, hand the equation front a symbol table, and write laws as equations.

A language functor is an ordinary base functor with the substrate's
constructor-tag and zip instances (mirror any test algebra):

```haskell
data SurfaceF a
  = SurfaceLit !Double
  | SurfaceVec !a !a !a
  | SurfaceSphere !a
  | SurfaceCube !a
  | SurfaceTranslate !a !a
  | SurfaceScale !a !a
  | SurfaceUnion !a !a
  | SurfaceInter !a !a
  | SurfaceDiff !a !a
  ...
```

`applicativeEquationFront` derives a prefix grammar (identifiers, application
by juxtaposition, parentheses, literals) from two functions: a node builder
and a literal reader. Every failure is a typed refusal naming the token and
position.

```haskell
surfaceEquationFront ::
  EquationFront (ApplicativeEquationError SurfaceBuildRefusal) (Pattern SurfaceF) () SurfaceF
surfaceEquationFront =
  applicativeEquationFront buildSurfaceNode readSurfaceLiteral

buildSurfaceNode :: String -> [Pattern SurfaceF] -> Either SurfaceBuildRefusal (Pattern SurfaceF)
buildSurfaceNode symbol children =
  case (symbol, children) of
    ("union", [left, right]) -> Right (PatternNode (SurfaceUnion left right))
    ("translate", [vector, body]) -> Right (PatternNode (SurfaceTranslate vector body))
    ...
```

Laws are then strings. Declared names become pattern variables; everything
else goes through the symbol table:

```haskell
surfaceUnionCommutativityRule =
  surfaceEquationRule surfaceUnionCommutativityLawId 0 ["a", "b"]
    "union a b = union b a"

surfaceTranslateUnionHoistRule =
  surfaceEquationRule surfaceTranslateUnionHoistLawId 0 ["v", "a", "b"]
    "union (translate v a) (translate v b) = translate v (union a b)"

surfaceTranslateComposeRule =
  surfaceEquationRule surfaceTranslateComposeLawId 0 ["u", "v", "x"]
    "translate u (translate v x) = translate (vadd u v) x"
```

Languages with binders use the full `EquationFront` record instead of the
derived grammar; the Haskell-expression front in
`moonlight-egraph-introspection` is the reference instance.

## Running saturation and extraction

```haskell
factored :: [SurfaceRewriteRule] -> Fix SurfaceF -> Maybe SurfaceView
factored rules term =
  let (rootClass, graph) = addTerm term (emptyEGraph surfaceAnalysis)
   in case saturate @SurfaceCapability (SaturationBudget 8 2048) rules graph of
        Left _ -> Nothing
        Right report ->
          viewSurface . erTerm
            <$> extractSurface (saturationReportBaseGraph report) rootClass

extractSurface :: EGraph SurfaceF SurfaceAnalysis -> ClassId -> Maybe (ExtractionResult SurfaceF Int)
extractSurface graph rootClass =
  stableExtractionSnapshotFromEGraph graph >>= extract surfaceCost rootClass
```

Under node-count cost, a construction that repeats a transform across two
siblings saturates and extracts to the factored form. Extraction stays
symbolic; where the value analysis knows a subterm, a pure post-extraction
fold (`surfaceReify`) materializes it.

## Guards: analyses and facts

Conditional laws attach a guard to the compiled rule. A **capability** guard
consults a resolver you supply, typically a projection of the e-class
analysis on the current graph, so `translate v x = x` fires when `v` is
analytically known to be zero even when it is syntactically opaque:

```haskell
surfaceTranslateIdentityRule =
  fmap (withCapability SurfaceKnownZero 0) $
    surfaceEquationRule surfaceTranslateIdentityLawId 0 ["v", "x"]
      "translate v x = x"
```

A **fact** guard consults the fact store, populated directly or by fact rules
during saturation. This is how soundness side-conditions are expressed: the
scale-hoist over intersection and difference is valid exactly when the scale
is injective, so the law is gated on a non-degeneracy fact and fires only
where that fact is present:

```haskell
surfaceScaleDiffHoistRule =
  fmap withNonDegenerateScale $
    hoistRule surfaceScaleDiffHoistLawId "diff" "scale"
```

## Context lattices

The same universe generalizes from `TrivialContext` to any finite context
lattice: equalities are authored at contexts and become visible on the
up-set, rules and facts carry per-context support, and cross-context gluing
is admitted or refused by descent. An assumption is a fact rule supported at
the assuming context: a law gated on that fact fires on the support's up-set
and refuses elsewhere. The worked spec is `ContextSupportedFactsSpec` in
`moonlight-surface`, and the step-by-step consumer recipe is
[`moonlight-surface/docs/CONTEXT-RECIPE.md`](../moonlight-surface/docs/CONTEXT-RECIPE.md).

## Performance

Benchmarks against egg-family referents, with how to run them, are in
[`docs/BENCHMARKS-m4-pro.md`](./docs/BENCHMARKS-m4-pro.md). Design records
for the engine's optimizations are in [`docs/`](./docs).

## Acknowledgements

The e-graph discipline of rewrite rules, e-class analyses, and extraction is
egg's, and the relational lowering is egglog's; this implementation is
independent, but the conceptual debts are real and gratefully acknowledged.

> Max Willsey, Chandrakana Nandi, Yisu Remy Wang, Oliver Flatt, Zachary
> Tatlock, and Pavel Panchekha.
> "egg: Fast and Extensible Equality Saturation." POPL 2021.
> <https://doi.org/10.1145/3434304>

> Yihong Zhang, Yisu Remy Wang, Oliver Flatt, David Cao, Philip Zucker, Eli
> Rosenthal, Zachary Tatlock, and Max Willsey.
> "Better Together: Unifying Datalog and Equality Saturation." PLDI 2023.
> <https://doi.org/10.1145/3591239>

> Eytan Singher and Shachar Itzhaky.
> "Colored E-Graph: Equality Reasoning with Conditions." arXiv:2305.19203.
> <https://arxiv.org/abs/2305.19203>

> Chandrakana Nandi, Max Willsey, Adam Anderson, James R. Wilcox, Eva
> Darulova, Dan Grossman, and Zachary Tatlock.
> "Synthesizing Structured CAD Models with Equality Saturation and Inverse
> Transformations." PLDI 2020.
> <https://doi.org/10.1145/3385412.3386012>

## License

MIT, as declared in [`moonlight-egraph.cabal`](./moonlight-egraph.cabal).

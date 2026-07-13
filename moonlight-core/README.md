# moonlight-core

> Part of **Moonlight**, the sheaf-theoretic computation layer beneath
> [Melusine](https://bluerose.blue) and Pale Meridian.

`moonlight-core` is Moonlight's foundation tier — the layer with nothing beneath it.
It is the total vocabulary every layer above shares: numeric classes, structural
identity, orders, patterns, fixpoints, persistent union-find, finite registries, a
term database, and a host-neutral e-graph program algebra.

## Main idea

One import — `Moonlight.Core` — is that shared vocabulary, and every name in it is
*total*. Where a lesser prelude would throw, bottom out, or return a silent default,
this vocabulary returns a value that names the failure in its type: `tryInv` gives a
`Maybe`, `makeSet` an `Either UnionFindAllocationError`, `fixpointBounded` an
`Either (FixpointDivergence a)`, `mkTotalRegistry` an `Either [missingKey]`. A
function that always keeps its promise — `magnitude`, `neg`, `union` — returns a bare
value; a function that cannot is honest about it in the signature. Nothing partial
hides behind a total-looking type.

There is exactly one door out of that discipline, and it is labelled. Importing
`Moonlight.Core.Unsound` by name is how you assert a refinement the checker cannot
see and take responsibility for it. Everything reachable through the umbrella is safe
by construction.

## Use

```haskell
import Moonlight.Core

reciprocalOfThree :: Maybe Double
reciprocalOfThree = tryInv 3

size :: Double
size = magnitude (neg (2.5 :: Double))
```

## Cookbook

Each recipe shows the main idea in practice: the total path returns a value, the
failure path returns a typed sum. Every snippet below type-checks and evaluates
against the public `Moonlight.Core` surface.

### Persistent union-find

`makeSet` mints fresh classes through a checked allocation boundary, `union`
merges them, and the structure is persistent; older versions stay valid after a
merge.

```haskell
import Moonlight.Core

partition :: Either UnionFindAllocationError (Bool, Bool)
partition = do
  (a, uf1) <- makeSet emptyUnionFind
  (b, uf2) <- makeSet uf1
  (c, uf3) <- makeSet uf2
  let uf = union a b uf3
  pure (equivalent a b uf, equivalent a c uf)   -- Right (True, False)
```

### Bounded fixpoints

`fixpointBounded` iterates a step to a fixed point under an explicit fuel bound.
Non-convergence returns a typed `Left`.

```haskell
import Moonlight.Core

converge :: Either (FixpointDivergence Int) Int
converge = fixpointBounded 100 (\n -> n `div` 2) 37   -- Right 0

diverges :: Either (FixpointDivergence Int) Int
diverges = fixpointBounded 5 (+1) 0                   -- Left (out of fuel)
```

### Checked total maps

`mkTotalRegistry` demands every key of a finite universe; a gap is a typed
`Left [missingKeys]`. In exchange, `lookupTotal` returns a bare value once the
registry is constructed.

```haskell
import Moonlight.Core
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map

data Slot = Alpha | Beta | Gamma
  deriving (Eq, Ord, Show)

instance FiniteUniverse Slot where
  finiteUniverse = Alpha :| [Beta, Gamma]

palette :: Either [Slot] (TotalRegistry Slot String)
palette = mkTotalRegistry (Map.fromList [(Alpha, "a"), (Beta, "b"), (Gamma, "c")])

labelOfBeta :: Either [Slot] String
labelOfBeta = fmap (\reg -> lookupTotal reg Beta) palette   -- Right "b"
-- mkTotalRegistry (Map.fromList [(Alpha, "a")])  ==  Left [Beta, Gamma]
```

### The matching seam

`ZipMatch` is one-layer structural matching: `zipMatch` succeeds iff two nodes share a
shape, pairing their children positionally. It is the seam the e-graph and rewriting
layers are built on.

```haskell
import Moonlight.Core

data ExprF a = Lit Int | Add a a
  deriving (Functor, Foldable, Traversable, Eq, Ord, Show)

instance ZipMatch ExprF where
  zipMatch (Lit m)     (Lit n)     | m == n = Just (Lit m)
  zipMatch (Add a1 b1) (Add a2 b2)          = Just (Add (a1, a2) (b1, b2))
  zipMatch _           _                    = Nothing

aligned :: Maybe (ExprF (Int, Int))
aligned = zipMatch (Add 1 2) (Add 10 20)   -- Just (Add (1,10) (2,20))
```

## Surface & boundaries

Most downstream code imports the umbrella. Lower-level packages may depend on one
of the deliberately public slices instead.

| Cabal dependency | Import | What you get |
| --- | --- | --- |
| `moonlight-core` | `Moonlight.Core` | The ordinary total vocabulary. This is the default. |
| `moonlight-core:moonlight-core-syntax` | `Moonlight.Core.Pattern.AntiUnify` | Patterns and term anti-unification without the full umbrella. |
| `moonlight-core:moonlight-core-automata` | `Moonlight.Core.Pattern.Automata` or `.Kernel` | The bottom-up automata substrate and compiled matcher. Depend on syntax too when naming its types directly. |
| `moonlight-core:moonlight-core-egraph-program` | `Moonlight.Core.EGraph.Program` | The host-neutral e-graph program algebra without the rest of the foundation. |
| `moonlight-core` | `Moonlight.Core.Unsound` | The explicit trust boundary; see [Main idea](#main-idea). |

The private implementation slices remain **basis**, **numeric**, **solver**, and
**term**. Syntax, automata, and e-graph programs are public because other foundation
packages consume those exact owners. The umbrella still re-exports the ordinary
syntax and e-graph-program vocabulary; automata remain an explicit expert import.
Read each module's Haddock for full signatures and laws.

## Test

```bash
cabal test moonlight-core:moonlight-core-test
```

## License

MIT. See `LICENSE`.

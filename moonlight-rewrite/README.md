# moonlight-rewrite

> Part of **Moonlight**, the sheaf-theoretic computation layer beneath
> [Melusine](https://bluerose.blue) and Pale Meridian.

`moonlight-rewrite` is the canonical rewrite package for Pale Meridian.
It owns the rewrite algebra, runtime core, checked rewrite systems, DSL,
proof-context integration, and relational execution. The stable
`Moonlight.Rewrite` face lives in the package's default library.

Consumers depend on `moonlight-rewrite` and import `Moonlight.Rewrite`.
Specialized consumers depend on a named sublibrary and import its facade;
defining modules behind those facades are not cross-sublibrary APIs.

## Quick start

At the front door, this package turns a typed rewrite program into a checked
one. You declare a signature, author a `Program` of named rules with the
`Moonlight.Rewrite.DSL` surface, and elaborate it with `compileProgramRuleSet`
into a `CanonicalProgram`, where duplicate names, unbound
variables, and malformed bodies become typed `ProgramError`s rather than
runtime surprises. `ruleBi` records a bidirectional rule as two deterministic
named directions.

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

import Data.Map.Strict qualified as Map
import GHC.TypeLits (Symbol)
import Moonlight.Core (ZipMatch (..))
import Moonlight.Rewrite.DSL
  ( HTraversable (..),
    NoGuardAtom,
    Node (..),
    Program,
    ProgramError,
    RewriteSignature (..),
    SortWitness (..),
    Term,
    canonicalRuleVariables,
    compileProgramRuleSet,
    node,
    program,
    ruleBi,
  )
import Moonlight.Rewrite.System (ruleNameString)

data ExprTag = LitATag | LitBTag
  deriving stock (Eq, Ord, Show)

data ExprSig (result :: Symbol) child where
  LitA :: ExprSig "Expr" child
  LitB :: ExprSig "Expr" child

instance HTraversable ExprSig where
  htraverseWithSort _ = \case
    LitA -> pure LitA
    LitB -> pure LitB

instance RewriteSignature ExprSig where
  type NodeTag ExprSig = ExprTag

  nodeTag = \case
    LitA -> LitATag
    LitB -> LitBTag

  nodeTagDigest _ = \case
    LitATag -> 1
    LitBTag -> 2

  nodeResultSort = \case
    LitA -> SortWitness
    LitB -> SortWitness

instance ZipMatch (Node ExprSig) where
  zipMatch (Node LitA) (Node LitA) = Just (Node LitA)
  zipMatch (Node LitB) (Node LitB) = Just (Node LitB)
  zipMatch _ _ = Nothing

litA, litB :: Term ExprSig "Expr"
litA = node LitA
litB = node LitB

swapProgram :: Program ExprSig NoGuardAtom
swapProgram =
  program (ruleBi "swap" litA litB)

compiledDirections :: Either (ProgramError ExprSig) [String]
compiledDirections =
  fmap
    (fmap ruleNameString . Map.keys . canonicalRuleVariables . fst)
    (compileProgramRuleSet swapProgram)
```

`compiledDirections` evaluates to `Right ["swap.bwd", "swap.fwd"]`: the
bidirectional rule has been elaborated, name-checked, and split into its two
directed rules. `compileProgramRuleSet` returns that checked program alongside a
derived `RulePlanSet`; the strata below turn it into matching and application.

## Use

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

import Moonlight.Rewrite

data TinyTag = TinyLit | TinyWrap
  deriving stock (Eq, Ord, Show)

data TinySig result child where
  Lit :: TinySig "Expr" child
  Wrap :: child "Expr" -> TinySig "Expr" child

instance HTraversable TinySig where
  htraverseWithSort transform = \case
    Lit -> pure Lit
    Wrap child -> Wrap <$> transform sortWitness child

instance RewriteSignature TinySig where
  type NodeTag TinySig = TinyTag

  nodeTag = \case
    Lit -> TinyLit
    Wrap _ -> TinyWrap

  nodeTagDigest _ = \case
    TinyLit -> 1
    TinyWrap -> 2

  nodeResultSort = \case
    Lit -> sortWitness
    Wrap _ -> sortWitness

lit :: Term TinySig "Expr"
lit =
  node Lit

wrap :: Term TinySig "Expr" -> Term TinySig "Expr"
wrap term =
  node (Wrap term)

unwrapProgram :: Program TinySig NoGuardAtom
unwrapProgram =
  program $
    rule
      "unwrap"
      ( forall_
          (bind (symbolToken @"x") (symbolToken @"Expr"))
          (wrap (var (symbolToken @"x") (symbolToken @"Expr")) ==> var (symbolToken @"x") (symbolToken @"Expr"))
      )

seedHost :: Either HostBuildError (Int, Int, Int)
seedHost =
  fmap
    (\(host, rootClass) -> (hostRevision host, hostClassCount host, classIdKey rootClass))
    (hostFromTerm 0 (wrap lit))
```

Specialized consumers use the owning stratum facade:

```haskell
import Moonlight.Rewrite.Algebra
import Moonlight.Rewrite.Runtime
import Moonlight.Rewrite.System
import Moonlight.Rewrite.DSL
import Moonlight.Rewrite.ProofContext
import Moonlight.Rewrite.Relational
```

## Quality gates

- expected failures are typed errors and obstructions;
- guards lower to CNF and carry evidence for satisfied literals;
- fact closure matches each rule once and propagates deltas through watched
  ground fact literals, each of which flips at most once;
- host construction and extraction reject invalid terms with typed errors;
- `Moonlight.Rewrite` is the stable import surface;
- law suites live with the algebra, core, system, front, relational, and
  relational-front strata.

## License

MIT. See `LICENSE`.

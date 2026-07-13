{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Data.Foldable (toList)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import GHC.TypeLits (Symbol)
import Hedgehog qualified
import Hedgehog.Gen qualified as Gen
import Moonlight.Core (ZipMatch (..), sameNodeShape)
import Moonlight.Rewrite.DSL (CanonicalProgram, canonicalRuleVariables, compileProgramRuleSet, ruleVariableMap)
import Moonlight.Rewrite.DSL (ProgramError (..))
import Moonlight.Rewrite.DSL (Program, context, program, rule, ruleBi)
import Moonlight.Rewrite.DSL (ContextNameError (..), contextName, contextNameString)
import Moonlight.Rewrite.DSL qualified as DSL
import Moonlight.Rewrite.DSL
  ( HTraversable (..),
    K (..),
    Node (..),
    RewriteSignature (..),
    SortWitness (..),
  )
import Moonlight.Rewrite.DSL
  ( SymbolToken,
    Term,
    node,
    someTypedVarName,
    sortNameString,
    symbolToken,
    typedVar,
    typedVarSort,
    var,
  )
import Moonlight.Rewrite.System (ruleNameString)

data TinyTag
  = TinyA
  | TinyB
  | TinyPairTag
  deriving stock (Eq, Ord, Show)

data TinySig (result :: Symbol) r where
  LitA :: TinySig "Expr" r
  LitB :: TinySig "Expr" r
  TinyPair :: r "Expr" -> r "Expr" -> TinySig "Expr" r

instance HTraversable TinySig where
  htraverseWithSort transform = \case
    LitA -> pure LitA
    LitB -> pure LitB
    TinyPair leftTerm rightTerm ->
      TinyPair
        <$> transform SortWitness leftTerm
        <*> transform SortWitness rightTerm

instance RewriteSignature TinySig where
  type NodeTag TinySig = TinyTag

  nodeTag = \case
    LitA -> TinyA
    LitB -> TinyB
    TinyPair {} -> TinyPairTag

  nodeTagDigest _ = \case
    TinyA -> 1
    TinyB -> 2
    TinyPairTag -> 3

  nodeResultSort = \case
    LitA -> SortWitness
    LitB -> SortWitness
    TinyPair {} -> SortWitness

instance ZipMatch (Node TinySig) where
  zipMatch leftNode rightNode =
    case (leftNode, rightNode) of
      (Node LitA, Node LitA) -> Just (Node LitA)
      (Node LitB, Node LitB) -> Just (Node LitB)
      (Node (TinyPair leftA leftB), Node (TinyPair rightA rightB)) ->
        Just (Node (TinyPair (zipChild leftA rightA) (zipChild leftB rightB)))
      _ -> Nothing
    where
      zipChild :: K left sort -> K right sort -> K (left, right) sort
      zipChild (K leftChild) (K rightChild) =
        K (leftChild, rightChild)

main :: IO ()
main = do
  assertEqual "context names trim at the DSL boundary" (Right "local") (fmap contextNameString (contextName " local "))
  assertEqual "context names reject empty input" (Left EmptyContextName) (contextName "  ")
  assertEqual "context names reject invalid identifiers" (Left InvalidContextName) (contextName "bad name")
  assertEqual "context names admit slash-separated scope paths" (Right "capture-program/function/body") (fmap contextNameString (contextName "capture-program/function/body"))
  assertEqual "context names reject empty path segments" (Left InvalidContextName) (contextName "a//b")
  assertEqual "context names reject leading path separators" (Left InvalidContextName) (contextName "/a")
  assertEqual "context names reject trailing path separators" (Left InvalidContextName) (contextName "a/")
  assertEqual "sort names render only through the opaque boundary" "expr" (sortNameString (typedVarSort exprVar))
  assertDuplicateRulesReject
  assertDuplicateContextsReject
  assertScopedContextProgramElaborates
  assertBidirectionalRuleDeclaresNamedDirections
  assertNestedForallAccumulatesBinders
  assertDuplicateNestedForallRejects
  assertTinyZipMatchPairsChildren
  where
    exprVar = typedVar (symbolToken :: SymbolToken "x") (symbolToken :: SymbolToken "expr")

assertTinyZipMatchPairsChildren :: IO ()
assertTinyZipMatchPairsChildren = do
  passed <-
    Hedgehog.check
      ( Hedgehog.withTests 100 $
          Hedgehog.property $ do
            leftNode <- Hedgehog.forAll genTinyNode
            rightNode <- Hedgehog.forAll genTinyNode
            case zipMatch leftNode rightNode of
              Just zipped -> do
                Hedgehog.assert (sameNodeShape leftNode rightNode)
                toList zipped
                  Hedgehog.=== zip (toList leftNode) (toList rightNode)
              Nothing ->
                Hedgehog.assert (not (sameNodeShape leftNode rightNode))
      )
  if passed
    then pure ()
    else fail "TinySig zipMatch violated the child-pairing invariant"
  where
    genTinyNode :: Hedgehog.Gen (Node TinySig Int)
    genTinyNode =
      Gen.element [Node LitA, Node LitB, Node (TinyPair (K 0) (K 1))]

litA :: Term TinySig "Expr"
litA =
  node LitA

litB :: Term TinySig "Expr"
litB =
  node LitB

varX :: Term TinySig "Expr"
varX =
  var (symbolToken :: SymbolToken "x") (symbolToken :: SymbolToken "Expr")

varY :: Term TinySig "Expr"
varY =
  var (symbolToken :: SymbolToken "y") (symbolToken :: SymbolToken "Expr")

pairTerm :: Term TinySig "Expr" -> Term TinySig "Expr" -> Term TinySig "Expr"
pairTerm leftTerm rightTerm =
  node (TinyPair leftTerm rightTerm)

assertDuplicateRulesReject :: IO ()
assertDuplicateRulesReject =
  case checkProgram duplicateRules of
    Left ProgramDuplicateRuleNames {} ->
      pure ()
    Left otherError ->
      fail ("duplicate rule names should be rejected, got " <> show otherError)
    Right _ ->
      fail "duplicate rule names should not elaborate"
  where
    duplicateRules =
      ( program $ do
          rule "same" (litA DSL.==> litB)
          rule "same" (litB DSL.==> litA)
      )
        :: Program TinySig DSL.NoGuardAtom

assertDuplicateContextsReject :: IO ()
assertDuplicateContextsReject =
  case checkProgram duplicateContexts of
    Left ProgramDuplicateContextNames {} ->
      pure ()
    Left otherError ->
      fail ("duplicate context names should be rejected, got " <> show otherError)
    Right _ ->
      fail "duplicate context names should not elaborate"
  where
    duplicateContexts =
      ( program $ do
          context "local"
          context "local"
          rule "ok" (litA DSL.==> litB)
      )
        :: Program TinySig DSL.NoGuardAtom

assertScopedContextProgramElaborates :: IO ()
assertScopedContextProgramElaborates =
  case checkProgram scopedProgram of
    Left programError ->
      fail ("scoped context program should elaborate, got " <> show programError)
    Right canonicalProgram ->
      assertEqual
        "scoped context program records the elaborated rule"
        ["local-rule"]
        (fmap ruleNameString (Map.keys (canonicalRuleVariables canonicalProgram)))
  where
    scopedProgram =
      ( program $ do
          context "local"
          rule "local-rule" (DSL.at "local" (litA DSL.==> litB))
      )
        :: Program TinySig DSL.NoGuardAtom

assertBidirectionalRuleDeclaresNamedDirections :: IO ()
assertBidirectionalRuleDeclaresNamedDirections =
  case checkProgram bidirectionalProgram of
    Left programError ->
      fail ("bidirectional rule should elaborate, got " <> show programError)
    Right canonicalProgram ->
      assertEqual
        "bidirectional rule expands into deterministic named directions"
        ["swap.bwd", "swap.fwd"]
        (fmap ruleNameString (Map.keys (canonicalRuleVariables canonicalProgram)))
  where
    bidirectionalProgram =
      (program (ruleBi "swap" litA litB)) :: Program TinySig DSL.NoGuardAtom

assertNestedForallAccumulatesBinders :: IO ()
assertNestedForallAccumulatesBinders =
  case checkProgram nestedForallProgram of
    Left programError ->
      fail ("nested forall_ should elaborate, got " <> show programError)
    Right canonicalProgram ->
      case Map.elems (canonicalRuleVariables canonicalProgram) of
        [ruleVariables] ->
          assertEqual
            "nested forall_ records the union of binders"
            ["x", "y"]
            ( List.sort
                ( fmap
                    someTypedVarName
                    (Map.elems (ruleVariableMap ruleVariables))
                )
            )
        otherVariables ->
          fail ("nested forall_ should elaborate one rule, got " <> show (length otherVariables))
  where
    nestedForallProgram =
      ( program $
          rule
            "quantified"
            ( DSL.forall_
                (DSL.bind (symbolToken :: SymbolToken "x") (symbolToken :: SymbolToken "Expr"))
                ( DSL.forall_
                    (DSL.bind (symbolToken :: SymbolToken "y") (symbolToken :: SymbolToken "Expr"))
                    (pairTerm varX varY DSL.==> pairTerm varY varX)
                )
            )
      )
        :: Program TinySig DSL.NoGuardAtom

assertDuplicateNestedForallRejects :: IO ()
assertDuplicateNestedForallRejects =
  case checkProgram duplicateNestedForallProgram of
    Left ProgramDuplicateRuleBinders {} ->
      pure ()
    Left otherError ->
      fail ("duplicate nested forall_ binder should be rejected, got " <> show otherError)
    Right _ ->
      fail "duplicate nested forall_ binder should not elaborate"
  where
    duplicateNestedForallProgram =
      ( program $
          rule
            "duplicate-quantifier"
            ( DSL.forall_
                (DSL.bind (symbolToken :: SymbolToken "x") (symbolToken :: SymbolToken "Expr"))
                ( DSL.forall_
                    (DSL.bind (symbolToken :: SymbolToken "x") (symbolToken :: SymbolToken "Expr"))
                    (pairTerm varX varX DSL.==> pairTerm varX varX)
                )
            )
      )
        :: Program TinySig DSL.NoGuardAtom

assertEqual :: (Eq value, Show value) => String -> value -> value -> IO ()
assertEqual label expected actual =
  if expected == actual
    then pure ()
    else fail (label <> " expected " <> show expected <> " but got " <> show actual)

checkProgram ::
  (RewriteSignature sig, ZipMatch (Node sig), DSL.RewriteGuardAtom atom, Ord (DSL.GuardCapabilityKey atom), Ord (NodeTag sig)) =>
  Program sig atom ->
  Either (ProgramError sig) (CanonicalProgram sig atom)
checkProgram =
  fmap fst . compileProgramRuleSet

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Data.Foldable (toList)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import GHC.TypeLits (Symbol)
import Hedgehog qualified
import Hedgehog.Gen qualified as Gen
import Moonlight.Core (Language, ZipMatch (..), sameNodeShape)
import Moonlight.Pale.Test.Site.Assertion (expectRightWithLabel)
import Moonlight.Rewrite.DSL qualified as DSL
import Moonlight.Rewrite.DSL
  ( CanonicalProgram,
    ContextNameError (..),
    HTraversable (..),
    K (..),
    Node (..),
    Program,
    ProgramError (..),
    RewriteSignature (..),
    SomeRewriteError (..),
    SortWitness (..),
    SymbolToken,
    Term,
    canonicalCheckedSystem,
    compileProgramRuleSet,
    compose,
    context,
    contextName,
    contextNameString,
    macro,
    node,
    program,
    rule,
    ruleBi,
    sortNameString,
    symbolToken,
    typedVar,
    typedVarSort,
    var,
    (>>>),
  )
import Moonlight.Rewrite.System
  ( CheckedRewrite,
    RewriteError (..),
    RuleVariableMetadataError (..),
    checkedRewriteName,
    checkedRewriteVariables,
    checkedRewrites,
    checkedRuleNames,
    ruleNameString,
    ruleVariableMap,
    ruleVariableName,
  )
import Test.Tasty.HUnit (assertEqual)

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
  assertVariableMacrosCarryComposedMetadata
  assertProjectedMacroVariablesDisappear
  assertSortIncompatibleMacroFailsTyped
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
assertScopedContextProgramElaborates = do
  canonicalProgram <-
    expectTinyProgram "scoped context program" scopedProgram
  assertEqual
    "scoped context program records the elaborated rule"
    ["local-rule"]
    (fmap ruleNameString (checkedRuleNames (canonicalCheckedSystem canonicalProgram)))
  where
    scopedProgram =
      ( program $ do
          context "local"
          rule "local-rule" (DSL.at "local" (litA DSL.==> litB))
      )
        :: Program TinySig DSL.NoGuardAtom

assertBidirectionalRuleDeclaresNamedDirections :: IO ()
assertBidirectionalRuleDeclaresNamedDirections = do
  canonicalProgram <-
    expectTinyProgram "bidirectional rule" bidirectionalProgram
  assertEqual
    "bidirectional rule expands into deterministic named directions"
    ["swap.bwd", "swap.fwd"]
    (fmap ruleNameString (checkedRuleNames (canonicalCheckedSystem canonicalProgram)))
  where
    bidirectionalProgram =
      program (ruleBi "swap" litA litB) :: Program TinySig DSL.NoGuardAtom

assertNestedForallAccumulatesBinders :: IO ()
assertNestedForallAccumulatesBinders = do
  canonicalProgram <-
    expectTinyProgram "nested forall_" nestedForallProgram
  case checkedRewrites (canonicalCheckedSystem canonicalProgram) of
    [checkedRewrite] ->
      assertEqual
        "nested forall_ records the union of binders"
        ["x", "y"]
        (checkedRewriteVariableNames checkedRewrite)
    otherRewrites ->
      fail ("nested forall_ should elaborate one rule, got " <> show (length otherRewrites))
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

assertVariableMacrosCarryComposedMetadata :: IO ()
assertVariableMacrosCarryComposedMetadata = do
  canonicalProgram <-
    expectTinyProgram "variable-bearing macros" variableMacroProgram
  assertRewriteVariableNames canonicalProgram "unwrap-wrap" ["x"]
  assertRewriteVariableNames canonicalProgram "round-trip" ["x"]

assertProjectedMacroVariablesDisappear :: IO ()
assertProjectedMacroVariablesDisappear = do
  canonicalProgram <-
    expectTinyProgram "projected-variable macro" projectedVariableMacroProgram
  assertRewriteVariableNames canonicalProgram "specialize-a" []

assertSortIncompatibleMacroFailsTyped :: IO ()
assertSortIncompatibleMacroFailsTyped =
  case checkProgram incompatibleSortMacroProgram of
    Left
      ( ProgramRewriteError
          (SomeRewriteError (RewriteVariableMetadataFailure (RuleVariableSortConflict _ leftSort rightSort)))
        ) ->
        assertEqual
          "sort-incompatible composition retains both typed sorts"
          ["Expr", "Flag"]
          [sortNameString leftSort, sortNameString rightSort]

    Left programError ->
      fail ("expected typed macro sort conflict, got " <> show programError)

    Right _ ->
      fail "sort-incompatible macro composition should fail"

assertRewriteVariableNames ::
  CanonicalProgram TinySig DSL.NoGuardAtom ->
  String ->
  [String] ->
  IO ()
assertRewriteVariableNames canonicalProgram rewriteName expectedNames =
  case
    List.find
      ((== rewriteName) . ruleNameString . checkedRewriteName)
      (checkedRewrites (canonicalCheckedSystem canonicalProgram))
  of
    Nothing ->
      fail ("missing checked rewrite " <> rewriteName)

    Just checkedRewrite ->
      assertEqual
        ("typed variables for " <> rewriteName)
        expectedNames
        (checkedRewriteVariableNames checkedRewrite)

checkedRewriteVariableNames :: (Language f, Ord capability) => CheckedRewrite capability f -> [String]
checkedRewriteVariableNames =
  List.sort
    . mapMaybe ruleVariableName
    . Map.elems
    . ruleVariableMap
    . checkedRewriteVariables

variableMacroProgram :: Program TinySig DSL.NoGuardAtom
variableMacroProgram =
  program $ do
    rule
      "unwrap-x"
      ( DSL.forall_
          (DSL.bind (symbolToken :: SymbolToken "x") (symbolToken :: SymbolToken "Expr"))
          (pairTerm varX litA DSL.==> varX)
      )
    rule
      "wrap-y"
      ( DSL.forall_
          (DSL.bind (symbolToken :: SymbolToken "y") (symbolToken :: SymbolToken "Expr"))
          (varY DSL.==> pairTerm varY litB)
      )
    rule
      "unwrap-z"
      ( DSL.forall_
          (DSL.bind (symbolToken :: SymbolToken "z") (symbolToken :: SymbolToken "Expr"))
          (pairTerm (var (symbolToken :: SymbolToken "z") (symbolToken :: SymbolToken "Expr")) litB DSL.==> var (symbolToken :: SymbolToken "z") (symbolToken :: SymbolToken "Expr"))
      )
    macro "unwrap-wrap" (compose "unwrap-x" >>> compose "wrap-y")
    macro "round-trip" (compose "unwrap-wrap" >>> compose "unwrap-z")

projectedVariableMacroProgram :: Program TinySig DSL.NoGuardAtom
projectedVariableMacroProgram =
  program $ do
    rule
      "expr-id"
      ( DSL.forall_
          (DSL.bind (symbolToken :: SymbolToken "x") (symbolToken :: SymbolToken "Expr"))
          (varX DSL.==> varX)
      )
    rule "a-to-b" (litA DSL.==> litB)
    macro "specialize-a" (compose "expr-id" >>> compose "a-to-b")

incompatibleSortMacroProgram :: Program TinySig DSL.NoGuardAtom
incompatibleSortMacroProgram =
  program $ do
    rule
      "expr-id"
      ( DSL.forall_
          (DSL.bind (symbolToken :: SymbolToken "x") (symbolToken :: SymbolToken "Expr"))
          (varX DSL.==> varX)
      )
    rule
      "flag-id"
      ( DSL.forall_
          (DSL.bind (symbolToken :: SymbolToken "flag") (symbolToken :: SymbolToken "Flag"))
          ( flagVariable DSL.==> flagVariable
          )
      )
    macro "bad-sorts" (compose "expr-id" >>> compose "flag-id")
  where
    flagVariable :: Term TinySig "Flag"
    flagVariable =
      var (symbolToken :: SymbolToken "flag") (symbolToken :: SymbolToken "Flag")

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

expectTinyProgram ::
  String ->
  Program TinySig DSL.NoGuardAtom ->
  IO (CanonicalProgram TinySig DSL.NoGuardAtom)
expectTinyProgram label =
  expectRightWithLabel (label <> " should elaborate") . checkProgram

checkProgram ::
  (RewriteSignature sig, ZipMatch (Node sig), DSL.RewriteGuardAtom atom, Ord (DSL.GuardCapabilityKey atom), Ord (NodeTag sig)) =>
  Program sig atom ->
  Either (ProgramError sig) (CanonicalProgram sig atom)
checkProgram =
  fmap fst . compileProgramRuleSet

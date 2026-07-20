{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Moonlight.EGraph.Pure.Saturation.FrontSpec
  ( tests,
  )
where

import Data.Map.Strict qualified as Map
import Moonlight.EGraph.Pure.Extraction
  ( ExtractionResult (..),
  )
import Moonlight.EGraph.Pure.Saturation.Front
import Moonlight.EGraph.Effect.CoveringSurface (SurfaceKind)
import Moonlight.EGraph.Pure.Saturation.Front.PackedNode (PackedNode)
import Moonlight.EGraph.Saturation.Context.State (SaturatingContextEGraph)
import Moonlight.EGraph.Test.Case (HUnitCase (..), hunitCases)
import Moonlight.EGraph.Test.Front.Tiny
import Moonlight.Rewrite.DSL
  ( Node,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( Assertion,
    assertFailure,
    (@?=),
  )

data ArithResult = ArithResult
  { arStartIsX :: !Bool,
    arBestStart :: !(MaybeExtraction Int)
  }

data ReportShape = ReportShape
  { rsResult :: !(Maybe FrontTinyView),
    rsSeedNames :: ![String],
    rsScheduleCount :: !Int
  }
  deriving (Eq, Show)

tests :: TestTree
tests =
  testGroup "egraph-front" . hunitCases $
    [ HUnitCase "auto-closes add-zero variables and checks named def" namedLetCheck,
      HUnitCase "extracts a named def without exposing class ids" namedLetExtraction,
      HUnitCase "compiled front reuses plans and accepts runtime seed terms" compiledFrontRuntimeSeedsMatchOneShot,
      HUnitCase "batches many global defs without losing seed names" batchedGlobalDefsCheck,
      HUnitCase "context-scoped rule lowers through the typed context registry" contextScopedCompile,
      HUnitCase "relation fact guard lowers without fact ids or tuples" relationFactGuardCheck,
      HUnitCase "invalid context names are rejected before compile" invalidContextNameRejected,
      HUnitCase "invalid ruleset names are rejected before compile" invalidRulesetNameRejected,
      HUnitCase "invalid seed names are rejected before staging" invalidSeedNameRejected,
      HUnitCase "invalid relation names are rejected before compile" invalidRelationNameRejected,
      HUnitCase "duplicate seed names are typed front errors" duplicateSeedNameRejected,
      HUnitCase "duplicate relation names are typed front errors" duplicateRelationNameRejected,
      HUnitCase "duplicate ruleset names are typed front errors" duplicateRulesetNameRejected
    ]

namedLetCheck :: Assertion
namedLetCheck =
  withEmptyFrontGraph $ \emptyFrontGraph -> do
    report <- expectFront (runEGraphFront arithProgram emptyFrontGraph)
    arStartIsX (efrResult report) @?= True

namedLetExtraction :: Assertion
namedLetExtraction =
  withEmptyFrontGraph $ \emptyFrontGraph -> do
    report <- expectFront (runEGraphFront arithProgram emptyFrontGraph)
    case arBestStart (efrResult report) of
      Just extraction -> viewFrontTinyTerm (erTerm extraction) @?= SymView "x"
      Nothing -> assertFailure "expected named extraction result"

contextScopedCompile :: Assertion
contextScopedCompile = do
  expectCompiled contextProgram

relationFactGuardCheck :: Assertion
relationFactGuardCheck =
  withEmptyFrontGraph $ \emptyFrontGraph -> do
    report <- expectFront (runEGraphFront relationProgram emptyFrontGraph)
    efrResult report @?= True

batchedGlobalDefsCheck :: Assertion
batchedGlobalDefsCheck =
  withEmptyFrontGraph $ \emptyFrontGraph -> do
    report <- expectFront (runEGraphFront batchedSeedProgram emptyFrontGraph)
    efrResult report @?= True

compiledFrontRuntimeSeedsMatchOneShot :: Assertion
compiledFrontRuntimeSeedsMatchOneShot =
  withEmptyFrontGraph $ \emptyFrontGraph -> do
    compiled <- expectFront (compileEGraphFront (runtimeSeedProgram runtimeSeedX))
    assertCompiledRuntimeSeedMatchesOneShot emptyFrontGraph compiled runtimeSeedX
    assertCompiledRuntimeSeedMatchesOneShot emptyFrontGraph compiled runtimeSeedY

assertCompiledRuntimeSeedMatchesOneShot ::
  SaturatingContextEGraph owner SurfaceKind (PackedNode FrontTinySig) NodeCount FrontTinyContext ->
  CompiledEGraphFront owner FrontTinySig NodeCount FrontTinyContext (MaybeExtraction Int) ->
  Term FrontTinySig "Expr" ->
  Assertion
assertCompiledRuntimeSeedMatchesOneShot emptyFrontGraph compiled seedTerm = do
  oneShot <- expectFront (runEGraphFront (runtimeSeedProgram seedTerm) emptyFrontGraph)
  compiledRun <-
    expectFront
      ( runCompiledEGraphFront
          compiled
          emptyFrontGraph
          [frontSeedTerm @"start" seedTerm]
      )
  reportShape compiledRun @?= reportShape oneShot

duplicateSeedNameRejected :: Assertion
duplicateSeedNameRejected = do
  case compileEGraphFront duplicateSeedProgram of
    Left (EGraphFrontDuplicateSeed seedName) ->
      frontSeedNameString seedName @?= "same-seed"
    Left err ->
      assertFailure ("expected duplicate seed error, saw " <> frontErrorMessage err)
    Right _ ->
      assertFailure "expected duplicate seed rejection"

duplicateRelationNameRejected :: Assertion
duplicateRelationNameRejected = do
  case compileEGraphFront duplicateRelationProgram of
    Left (EGraphFrontDuplicateRelation relationName) ->
      frontRelationNameString relationName @?= "same-relation"
    Left err ->
      assertFailure ("expected duplicate relation error, saw " <> frontErrorMessage err)
    Right _ ->
      assertFailure "expected duplicate relation rejection"

duplicateRulesetNameRejected :: Assertion
duplicateRulesetNameRejected = do
  case compileEGraphFront duplicateRulesetProgram of
    Left (EGraphFrontDuplicateRuleset rulesetName) ->
      frontRulesetNameString rulesetName @?= "same-ruleset"
    Left err ->
      assertFailure ("expected duplicate ruleset error, saw " <> frontErrorMessage err)
    Right _ ->
      assertFailure "expected duplicate ruleset rejection"

invalidContextNameRejected :: Assertion
invalidContextNameRejected = do
  case compileEGraphFront invalidContextProgram of
    Left (EGraphFrontInvalidContextName rawName _) ->
      rawName @?= "bad context"
    Left err ->
      assertFailure ("expected invalid context error, saw " <> frontErrorMessage err)
    Right _ ->
      assertFailure "expected invalid context rejection"

invalidRulesetNameRejected :: Assertion
invalidRulesetNameRejected = do
  case compileEGraphFront invalidRulesetProgram of
    Left (EGraphFrontInvalidRulesetName rawName _) ->
      rawName @?= "bad ruleset"
    Left err ->
      assertFailure ("expected invalid ruleset error, saw " <> frontErrorMessage err)
    Right _ ->
      assertFailure "expected invalid ruleset rejection"

invalidSeedNameRejected :: Assertion
invalidSeedNameRejected = do
  case compileEGraphFront invalidSeedProgram of
    Left (EGraphFrontInvalidSeedName rawName _) ->
      rawName @?= "bad seed"
    Left err ->
      assertFailure ("expected invalid seed error, saw " <> frontErrorMessage err)
    Right _ ->
      assertFailure "expected invalid seed rejection"

invalidRelationNameRejected :: Assertion
invalidRelationNameRejected = do
  case compileEGraphFront invalidRelationProgram of
    Left (EGraphFrontInvalidRelationName rawName _) ->
      rawName @?= "bad relation"
    Left err ->
      assertFailure ("expected invalid relation error, saw " <> frontErrorMessage err)
    Right _ ->
      assertFailure "expected invalid relation rejection"

arithProgram ::
  EGraphFront
    'Authored
    owner
    FrontTinySig
    NodeCount
    FrontTinyContext
    ArithResult
arithProgram =
  egraph $ do
    simplify <- ruleset @"simplify" $ do
      rewrite @"add-zero" $
        add #x zero ==> #x
      rewrite @"mul-one" $
        mul one #x ==> #x
      birewrite @"add-commute" (add #x #y) (add #y #x)

    start <- def @"start" $
      add (mul one (sym "x")) zero

    run $
      runFor defaultBudget simplify

    checkOutput <- check @"start-is-x" (start === sym "x")
    extractOutput <- extract @"best-start" termSize start
    pure (ArithResult <$> checkOutput <*> extractOutput)

runtimeSeedProgram ::
  Term FrontTinySig "Expr" ->
  EGraphFront
    'Authored
    owner
    FrontTinySig
    NodeCount
    FrontTinyContext
    (MaybeExtraction Int)
runtimeSeedProgram seedTerm =
  egraph $ do
    simplify <- ruleset @"runtime-simplify" $ do
      rewrite @"runtime-add-zero" $
        add #x zero ==> #x
      rewrite @"runtime-mul-one" $
        mul one #x ==> #x

    start <- def @"start" seedTerm

    run $
      runFor defaultBudget simplify

    extract @"runtime-best-start" termSize start

runtimeSeedX :: Term FrontTinySig "Expr"
runtimeSeedX =
  add (mul one (sym "x")) zero

runtimeSeedY :: Term FrontTinySig "Expr"
runtimeSeedY =
  add (mul one (sym "y")) zero

contextProgram :: EGraphFront 'Authored owner FrontTinySig NodeCount FrontTinyContext ()
contextProgram =
  egraph $ do
    rain <- context @"rain" Rain
    _ <- ruleset @"contextual" $ do
      rewrite @"rain-add-zero" $
        atContext rain (add #x zero ==> #x)
    pure done

relationProgram :: EGraphFront 'Authored owner FrontTinySig NodeCount FrontTinyContext Bool
relationProgram =
  egraph $ do
    isZero <- relation @"is-zero"
    simplify <- ruleset @"guarded" $ do
      factRule @"zero-fact" isZero zero
      rewrite @"guarded-add-zero" $
        (add #x #y ==> #x) `when_` has isZero #y

    zeroRef <- def @"zero-fact-seed" zero
    fact isZero zeroRef

    start <- def @"guarded-start-term" $
      add (sym "x") zero

    run $
      runFor defaultBudget simplify

    check @"guarded-start" (start === sym "x")

batchedSeedProgram :: EGraphFront 'Authored owner FrontTinySig NodeCount FrontTinyContext Bool
batchedSeedProgram =
  egraph $ do
    simplify <- ruleset @"batch-simplify" $ do
      rewrite @"batch-add-zero" $
        add #x zero ==> #x

    _ <- def @"batch-a" (add (sym "a") zero)
    _ <- def @"batch-b" (mul one (sym "b"))
    start <- def @"batch-start" (add (sym "x") zero)

    run $
      runFor defaultBudget simplify

    check @"batch-start-is-x" (start === sym "x")

duplicateSeedProgram :: EGraphFront 'Authored owner FrontTinySig NodeCount FrontTinyContext ()
duplicateSeedProgram =
  egraph $ do
    _ <- defNamed "same-seed" zero
    _ <- defNamed "same-seed" one
    pure done

duplicateRelationProgram :: EGraphFront 'Authored owner FrontTinySig NodeCount FrontTinyContext ()
duplicateRelationProgram =
  egraph $ do
    _ <- relationNamed "same-relation" :: EGraphFrontM FrontTinySig NodeCount FrontTinyContext (RelationRef '["Expr"])
    _ <- relationNamed "same-relation" :: EGraphFrontM FrontTinySig NodeCount FrontTinyContext (RelationRef '["Expr"])
    pure done

duplicateRulesetProgram :: EGraphFront 'Authored owner FrontTinySig NodeCount FrontTinyContext ()
duplicateRulesetProgram =
  egraph $ do
    _ <- rulesetNamed "same-ruleset" (rewriteNamed "one" (add #x zero ==> #x))
    _ <- rulesetNamed "same-ruleset" (rewriteNamed "two" (mul one #x ==> #x))
    pure done

invalidContextProgram :: EGraphFront 'Authored owner FrontTinySig NodeCount FrontTinyContext ()
invalidContextProgram =
  egraph $ do
    _ <- contextNamed "bad context" Rain
    pure done

invalidRulesetProgram :: EGraphFront 'Authored owner FrontTinySig NodeCount FrontTinyContext ()
invalidRulesetProgram =
  egraph $ do
    _ <- rulesetNamed "bad ruleset" (rewriteNamed "one" (add #x zero ==> #x))
    pure done

invalidSeedProgram :: EGraphFront 'Authored owner FrontTinySig NodeCount FrontTinyContext ()
invalidSeedProgram =
  egraph $ do
    _ <- defNamed "bad seed" zero
    pure done

invalidRelationProgram :: EGraphFront 'Authored owner FrontTinySig NodeCount FrontTinyContext ()
invalidRelationProgram =
  egraph $ do
    _ <- relationNamed "bad relation" :: EGraphFrontM FrontTinySig NodeCount FrontTinyContext (RelationRef '["Expr"])
    pure done

type MaybeExtraction cost =
  Maybe (ExtractionResult (Node FrontTinySig) cost)

reportShape :: EGraphFrontReport owner FrontTinySig NodeCount FrontTinyContext (MaybeExtraction Int) -> ReportShape
reportShape report =
  ReportShape
    { rsResult = viewFrontTinyTerm . erTerm <$> efrResult report,
      rsSeedNames = fmap frontSeedNameString (Map.keys (efrSeedClasses report)),
      rsScheduleCount = length (efrScheduleReports report)
    }

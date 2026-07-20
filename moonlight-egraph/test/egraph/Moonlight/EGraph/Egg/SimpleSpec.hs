{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Moonlight.EGraph.Egg.SimpleSpec
  ( tests,
  )
where

import Moonlight.EGraph.Pure.Extraction (ExtractionResult (..))
import Moonlight.EGraph.Pure.Saturation.Front
import Moonlight.EGraph.Pure.Saturation.Logic.Run (EGraphLogicReport (..))
import Moonlight.EGraph.Test.Front.Tiny
import Moonlight.Rewrite.DSL (Node)
import Moonlight.Saturation.Context.Runtime.Report (srResult)
import Moonlight.Saturation.Core (SaturationTermination (ReachedFixedPoint))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "egg-simple"
    [ testCase "(* 0 42) simplifies to 0" $
        simplify (mul zero (num 42)) (NumView 0),
      testCase "(+ 0 (* 1 foo)) simplifies to foo" $
        simplify (add zero (mul one (sym "foo"))) (SymView "foo"),
      testCase "saturation reaches fixed point on simple front rules" $
        fixedPoint (mul zero (num 42))
    ]

simplify :: Term FrontTinySig "Expr" -> FrontTinyView -> Assertion
simplify term expected =
  runSimpleProgram term $ \report -> do
    case efrResult report of
      Just result -> viewFrontTinyTerm (erTerm result) @?= expected
      Nothing -> assertFailure "expected named extraction result"

fixedPoint :: Term FrontTinySig "Expr" -> Assertion
fixedPoint term =
  runSimpleProgram term $ \report -> do
    case efrScheduleReports report of
      logicReport : _ ->
        srResult (elrSaturation logicReport) @?= ReachedFixedPoint
      [] ->
        assertFailure "expected one scheduled saturation report"

runSimpleProgram ::
  Term FrontTinySig "Expr" ->
  (forall owner. EGraphFrontReport owner FrontTinySig NodeCount FrontTinyContext (MaybeExtraction Int) -> IO result) ->
  IO result
runSimpleProgram term useReport =
  withEmptyFrontGraph $ \emptyFrontGraph ->
    expectFront (runEGraphFront (simpleProgram term) emptyFrontGraph) >>= useReport

simpleProgram :: Term FrontTinySig "Expr" -> EGraphFront 'Authored owner FrontTinySig NodeCount FrontTinyContext (MaybeExtraction Int)
simpleProgram startTerm =
  egraph $ do
    simplifyRules <- ruleset @"simple" simpleArithmeticRules
    start <- def @"start" startTerm

    run $
      runFor defaultBudget simplifyRules

    extract @"best-start" termSize start

type MaybeExtraction cost =
  Maybe (ExtractionResult (Node FrontTinySig) cost)

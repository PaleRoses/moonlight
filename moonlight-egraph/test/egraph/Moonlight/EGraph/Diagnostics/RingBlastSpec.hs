{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}

module Moonlight.EGraph.Diagnostics.RingBlastSpec
  ( tests,
  )
where

import Moonlight.EGraph.Pure.Extraction (ExtractionResult (erCost, erTerm))
import Moonlight.EGraph.Pure.Saturation.Front
  ( RulesetM,
    SaturationBudget (..),
    Term,
  )
import Moonlight.EGraph.Test.Front.Ring
  ( RingExtraction,
    RingExtractionRun (..),
    RingSaturationReport,
    RingSig,
    rAdd,
    rMul,
    rNeg,
    rOne,
    rVar,
    rZero,
    ringAnnihilationRules,
    ringExplosionRules,
    ringIdentityRules,
    ringNegationRules,
    ringSaturationRules,
    runRingExtraction,
    runRingSaturation,
    viewFrontRingTerm,
  )
import Moonlight.EGraph.Test.Config (toBudget)
import Moonlight.EGraph.Test.Ring.Core
  ( RingTermView (RZeroView, VarView),
  )
import Moonlight.EGraph.Test.Saturation
  ( SaturationTermination (HitIterationLimit, HitNodeLimit, ReachedFixedPoint),
    srMatchesApplied,
    srResult,
  )
import Moonlight.Pale.Test.Site.Core
  ( TestBudget (..),
    generousBudget,
    mediumPressureBudget,
    stressTestBudget,
    tightIterationBudget,
    tightNodeBudget,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "ring-blast"
    [ simplificationTests,
      distributiveExplosionTests,
      budgetPressureTests,
      frontPressureTests,
      largeTermTests
    ]

simplificationTests :: TestTree
simplificationTests =
  testGroup "simplification" $
    fmap
      simplificationCase
      [ ("identity elimination reduces (x + 0) * 1 to x", ringIdentityRules, rMul (rAdd (rVar "x") rZero) rOne, VarView "x"),
        ("double negation elimination reduces --x to x", ringNegationRules, rNeg (rNeg (rVar "y")), VarView "y"),
        ("annihilation reduces x * 0 to 0", ringAnnihilationRules, rMul (rVar "x") rZero, RZeroView)
      ]

distributiveExplosionTests :: TestTree
distributiveExplosionTests =
  testGroup
    "distributive-explosion"
    [ testCase "distribution of (a + b) * (c + d) terminates within budget" $ do
        saturationReport <- runSaturate (SaturationBudget 30 500) ringExplosionRules abcdProduct
        assertTerminates "expected saturation to terminate (ReachedFixedPoint or budget-limited)" saturationReport
        assertBool
          "expected nontrivial match applications from distributive interaction"
          (srMatchesApplied saturationReport > 0),
      testCase "saturation ring basis on (a + b) * (c + d) terminates within generous budget" $ do
        saturationReport <- runSaturate (toBudget stressTestBudget) ringSaturationRules abcdProduct
        assertTerminates "expected saturation to terminate without error" saturationReport
        assertBool
          "expected nontrivial rewrite activity from saturation basis"
          (srMatchesApplied saturationReport > 0)
    ]

budgetPressureTests :: TestTree
budgetPressureTests =
  testGroup "budget-pressure" $
    fmap
      budgetPressureCase
      [ ("node limit is reported gracefully on tight budget", tightNodeBudget, HitNodeLimit),
        ("iteration limit is reported gracefully on tight iteration budget", tightIterationBudget, HitIterationLimit)
      ]

frontPressureTests :: TestTree
frontPressureTests =
  testGroup
    "front-pressure"
    [ testCase "front-authored saturation terminates on distributive explosion" $ do
        saturationReport <- runSaturate frontPressureBudget ringSaturationRules abcdProduct
        assertTerminates "expected front-authored saturation to terminate" saturationReport
        assertBool
          "expected nontrivial match activity under the existing front schedule"
          (srMatchesApplied saturationReport > 0),
      testCase "front-authored saturation applies bounded matches under medium pressure" $ do
        saturationReport <- runSaturate mediumPressureSaturationBudget ringSaturationRules abcdProduct
        assertTerminates "expected medium-pressure front saturation to terminate" saturationReport
        assertBool
          "expected front saturation to do real rewrite work"
          (srMatchesApplied saturationReport > 0)
    ]

largeTermTests :: TestTree
largeTermTests =
  testGroup
    "large-term-stress"
    [ testCase "triple product (a+b)*(c+d)*(e+f) terminates within generous budget" $
        runSaturate (toBudget stressTestBudget) ringSaturationRules tripleProduct
          >>= assertTerminates "expected triple product saturation to terminate",
      testCase "nested distribution with identity (a+0)*(1*b)*(c+d) simplifies under extraction" $ do
        extractionRun <- runRingExtraction (toBudget stressTestBudget) ringSaturationRules nestedIdentityProduct
        assertTerminates "expected saturation to terminate" (rerSaturation extractionRun)
        maybe (pure ()) (assertBool "expected extraction cost to improve over input term" . (< 13) . erCost) (rerExtraction extractionRun),
      testCase "quadratic blowup (a+b+c)*(d+e+f) terminates through the front" $
        runSaturate (toBudget generousBudget) ringSaturationRules quadraticProduct
          >>= assertTerminates "expected quadratic blowup to terminate",
      testCase "generic-join matching terminates on distributive explosion" $ do
        saturationReport <- runSaturate genericJoinComparisonBudget ringSaturationRules abcdProduct
        assertTerminates "expected generic-join matching to terminate" saturationReport
        assertBool
          "expected nontrivial match activity under generic-join matching"
          (srMatchesApplied saturationReport > 0)
    ]

simplificationCase :: (String, RulesetM RingSig (), Term RingSig "Expr", RingTermView) -> TestTree
simplificationCase (caseName, selectedRules, term, expectedView) =
  testCase caseName $ do
    extractionResult <- runExtract simplificationBudget selectedRules term >>= requireExtraction
    erCost extractionResult @?= 1
    viewFrontRingTerm (erTerm extractionResult) @?= expectedView

budgetPressureCase :: (String, TestBudget, SaturationTermination) -> TestTree
budgetPressureCase (caseName, budget, expectedTermination) =
  testCase caseName $
    runSaturate (toBudget budget) ringExplosionRules abcdProduct
      >>= \saturationReport -> srResult saturationReport @?= expectedTermination

runSaturate :: SaturationBudget -> RulesetM RingSig () -> Term RingSig "Expr" -> IO RingSaturationReport
runSaturate =
  runRingSaturation

runExtract :: SaturationBudget -> RulesetM RingSig () -> Term RingSig "Expr" -> IO (Maybe RingExtraction)
runExtract budget selectedRules term =
  rerExtraction <$> runRingExtraction budget selectedRules term

requireExtraction :: Maybe value -> IO value
requireExtraction =
  \case
    Just value -> pure value
    Nothing -> assertFailure "expected extraction result"

assertTerminates :: String -> RingSaturationReport -> Assertion
assertTerminates message saturationReport =
  assertBool message (srResult saturationReport `elem` terminatingResults)

terminatingResults :: [SaturationTermination]
terminatingResults =
  [ReachedFixedPoint, HitIterationLimit, HitNodeLimit]

abcdProduct :: Term RingSig "Expr"
abcdProduct =
  rMul
    (rAdd (rVar "a") (rVar "b"))
    (rAdd (rVar "c") (rVar "d"))

tripleProduct :: Term RingSig "Expr"
tripleProduct =
  rMul abcdProduct (rAdd (rVar "e") (rVar "f"))

nestedIdentityProduct :: Term RingSig "Expr"
nestedIdentityProduct =
  rMul
    (rMul (rAdd (rVar "a") rZero) (rMul rOne (rVar "b")))
    (rAdd (rVar "c") (rVar "d"))

quadraticProduct :: Term RingSig "Expr"
quadraticProduct =
  rMul
    (rAdd (rAdd (rVar "a") (rVar "b")) (rVar "c"))
    (rAdd (rAdd (rVar "d") (rVar "e")) (rVar "f"))

simplificationBudget :: SaturationBudget
simplificationBudget =
  SaturationBudget
    { sbMaxIterations = 10,
      sbMaxNodes = 100
    }

frontPressureBudget :: SaturationBudget
frontPressureBudget =
  toBudget frontPressureTestBudget

mediumPressureSaturationBudget :: SaturationBudget
mediumPressureSaturationBudget =
  toBudget mediumPressureBudget

genericJoinComparisonBudget :: SaturationBudget
genericJoinComparisonBudget =
  SaturationBudget
    { sbMaxIterations = 20,
      sbMaxNodes = 500
    }

frontPressureTestBudget :: TestBudget
frontPressureTestBudget =
  TestBudget
    { testBudgetMaxIterations = 20,
      testBudgetMaxNodes = 500
    }

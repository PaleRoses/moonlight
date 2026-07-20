{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Saturation.Property
  ( saturationAlwaysTerminates,
    saturationMonotone,
    saturationOracleAgreement,
    iterationLimitTight,
    nodeLimitTight,
    goalTerminatesEarly,
    advanceWithoutApplyIdempotent,
    saturationProperties,
  )
where

import Moonlight.Saturation.Core
  ( SaturationBudget (..),
    SaturationRun (..),
    SaturationTermination (..),
    runSaturation,
  )
import Moonlight.Saturation.Gen
  ( genFactTarget,
  )
import Moonlight.Saturation.Oracle
  ( oracleSaturatedFacts,
  )
import Moonlight.Saturation.Test.CoreFixture
  ( ToyState (..),
    idleKernel,
    initialToy,
    monotone,
    toyKernel,
  )
import Test.QuickCheck
  ( Property,
    conjoin,
    counterexample,
    forAll,
    property,
    (===),
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

-- Proves semantic-surface invariant: every run reports a terminal result.
saturationAlwaysTerminates :: Property
saturationAlwaysTerminates =
  forAll genFactTarget $ \target ->
    case runSaturation (SaturationBudget (max 1 target + 2) (max 1 target + 1)) toyKernel (initialToy target) of
      Left err -> counterexample err (property False)
      Right run -> property (srTermination run `elem` [ReachedFixedPoint, ReachedGoal, HitIterationLimit, HitNodeLimit])

-- Proves semantic-surface invariant: facts only grow in the no-retraction engine model.
saturationMonotone :: Property
saturationMonotone =
  forAll genFactTarget $ \target ->
    case runSaturation (SaturationBudget (max 1 target + 2) (max 1 target + 1)) toyKernel (initialToy target) of
      Left err -> counterexample err (property False)
      Right run ->
        conjoin
          [ tsFacts (srFinalState run) === oracleSaturatedFacts target,
            property (monotone (reverse (tsObserved (srFinalState run))))
          ]

-- Proves semantic-surface invariant: the engine agrees with an independent
-- apply-one-fact-until-target arithmetic oracle on this terminating rule family.
saturationOracleAgreement :: Property
saturationOracleAgreement =
  forAll genFactTarget $ \target ->
    case runSaturation (SaturationBudget (max 1 target + 4) (max 1 target + 1)) toyKernel (initialToy target) of
      Left err -> counterexample err (property False)
      Right run ->
        tsFacts (srFinalState run) === oracleSaturatedFacts target

iterationLimitTight :: Property
iterationLimitTight =
  forAll genFactTarget $ \target ->
    let budget = SaturationBudget 1 (max 1 target + 1)
     in case runSaturation budget toyKernel (initialToy (max 2 target)) of
          Left err -> counterexample err (property False)
          Right run ->
            conjoin
              [ srTermination run === HitIterationLimit,
                tsIteration (srFinalState run) === sbMaxIterations budget
              ]

nodeLimitTight :: Property
nodeLimitTight =
  forAll genFactTarget $ \target ->
    let budget = SaturationBudget (max 2 target + 2) 0
     in case runSaturation budget toyKernel (initialToy (max 1 target)) of
          Left err -> counterexample err (property False)
          Right run ->
            conjoin
              [ srTermination run === HitNodeLimit,
                tsFacts (srFinalState run) === 1,
                tsIteration (srFinalState run) === 1
              ]

goalTerminatesEarly :: Property
goalTerminatesEarly =
  forAll genFactTarget $ \target ->
    case runSaturation (SaturationBudget (max 1 target + 10) (max 1 target + 1)) toyKernel (initialToy target) of
      Left err -> counterexample err (property False)
      Right run ->
        conjoin
          [ srTermination run === ReachedGoal,
            tsFacts (srFinalState run) === target
          ]

advanceWithoutApplyIdempotent :: Property
advanceWithoutApplyIdempotent =
  forAll genFactTarget $ \target ->
    let iterations = max 1 target
     in case runSaturation (SaturationBudget iterations 0) idleKernel (initialToy target) of
          Left err -> counterexample err (property False)
          Right run ->
            conjoin
              [ srTermination run === HitIterationLimit,
                tsIteration (srFinalState run) === iterations,
                tsFacts (srFinalState run) === 0,
                tsObserved (srFinalState run) === [0]
              ]

saturationProperties :: TestTree
saturationProperties =
  testGroup
    "saturation"
    [ testProperty "termination always observable" saturationAlwaysTerminates,
      testProperty "monotone in fact set" saturationMonotone,
      testProperty "agrees with independent oracle on terminating rules" saturationOracleAgreement,
      testProperty "iteration limit tight" iterationLimitTight,
      testProperty "node limit tight" nodeLimitTight,
      testProperty "goal terminates early" goalTerminatesEarly,
      testProperty "advance without apply idempotent" advanceWithoutApplyIdempotent
    ]

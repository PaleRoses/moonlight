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

import Data.List.NonEmpty qualified as NonEmpty
import Moonlight.Saturation.Core
  ( ApplyOutcome (..),
    RebuildOutcome (..),
    RoundPlan (..),
    SaturationBudget (..),
    SaturationKernel (..),
    SaturationRun (..),
    SaturationTermination (..),
    TerminationGoal (..),
    runSaturation,
  )
import Moonlight.Saturation.Gen
  ( genFactTarget,
  )
import Moonlight.Saturation.Oracle
  ( oracleSaturatedFacts,
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

data ToyState = ToyState
  { tsIteration :: !Int,
    tsFacts :: !Int,
    tsTarget :: !Int,
    tsObserved :: ![Int]
  }
  deriving stock (Eq, Show)

data ToyRound = ToyRound
  { trInput :: !ToyState,
    trMatches :: ![Int]
  }
  deriving stock (Eq, Show)

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

initialToy :: Int -> ToyState
initialToy target =
  ToyState {tsIteration = 0, tsFacts = 0, tsTarget = max 0 target, tsObserved = [0]}

toyKernel :: SaturationKernel ToyState ToyRound Int Int String
toyKernel =
  SaturationKernel
    { skIterationOf = tsIteration,
      skNodeCountOf = tsFacts,
      skGoal = TerminationGoal (\state -> tsFacts state >= tsTarget state),
      skPlanRound = \state ->
        let roundValue =
              ToyRound
                { trInput = state,
                  trMatches = [tsFacts state + 1 | tsFacts state < tsTarget state]
                }
         in case NonEmpty.nonEmpty (trMatches roundValue) of
              Just matches -> Right (ApplyRound roundValue (trInput roundValue) matches)
              Nothing -> Right (StopRound (trInput roundValue)),
      skApply = \matches state ->
        Right
          ApplyOutcome
            { aoEffect = NonEmpty.length matches,
              aoState = state {tsFacts = tsFacts state + NonEmpty.length matches}
            },
      skRebuild = \roundValue _applied state ->
        Right
          RebuildOutcome
            { roRound = roundValue,
              roState = state {tsObserved = tsFacts state : tsObserved state}
            },
      skCommit = \_roundValue _applied state -> advance state,
      skConverged = \_roundValue _state -> False
    }

idleKernel :: SaturationKernel ToyState ToyRound Int Int String
idleKernel =
  toyKernel
    { skGoal = mempty,
      skPlanRound = \state -> Right (AdvanceRound (advance state))
    }

advance :: ToyState -> ToyState
advance state =
  state {tsIteration = tsIteration state + 1}

monotone :: [Int] -> Bool
monotone values =
  case values of
    [] -> True
    [_] -> True
    left : right : rest -> left <= right && monotone (right : rest)

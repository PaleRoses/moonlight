{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Differential.Effect.Harness.TimeFrontier
  ( runtimeTimeScopeLaws,
    runtimeFrontierStoresProductAntichains,
    localFactConstructionAndAntichainLaws,
    capabilityDowngradeMonotoneAccepted,
    capabilityAdvanceRegressionTyped,
  )
where

import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Numeric.Natural
  ( Natural,
  )

import Moonlight.Core
  ( BoundaryOps (..),
    PartialOrder,
  )
import Moonlight.Delta.Frontier
  ( upperFrontierPoints,
  )
import Moonlight.Differential.Fact.Local
  ( carrierContexts,
    emptyFactAntichain,
    insertAntichain,
    laCarrier,
    laSupport,
    membersAntichain,
    mkLocalAddress,
    mkLocalFact,
  )
import Moonlight.Differential.Frontier
  ( RuntimeInvalidCapabilityAdvance (..),
    downgradeRuntimeCapability,
    emptyRuntimeFrontier,
    frontierAdvanceVisibleMin,
    frontierVisibleAntichainForContext,
    mintRootRuntimeCapability,
    runtimeCapabilityTime,
  )
import Moonlight.Differential.Proposition
  ( PropositionKey (..),
  )
import Moonlight.Differential.Time
  ( RuntimeTime,
    delayRuntimeTimeFeedback,
    emptyRuntimeScope,
    enterRuntimeTimeScope,
    frontierStamp,
    isDescendantOf,
    leaveRuntimeTimeScope,
    rtScope,
    runtimeTimeSameScopeLeq,
    runtimeTimeSameScopeLt,
  )
import Moonlight.Differential.Time qualified as DifferentialTime
import Moonlight.FiniteLattice
  ( ContextOrderDecl,
    compileContextLattice,
    contextOrderDecl,
    supportBasis,
    supportGenerators,
  )
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    assertFailure,
  )

capabilityDowngradeMonotoneAccepted ::
  (Eq ctx, PartialOrder epoch, PartialOrder phase) =>
  RuntimeTime ctx epoch phase ->
  RuntimeTime ctx epoch phase ->
  Bool
capabilityDowngradeMonotoneAccepted sourceTime targetTime =
  not (runtimeTimeSameScopeLeq sourceTime targetTime)
    || ( case downgradeRuntimeCapability targetTime (mintRootRuntimeCapability sourceTime) of
           Right capability ->
             runtimeCapabilityTime capability == targetTime
           Left _ ->
             False
       )

capabilityAdvanceRegressionTyped ::
  (Eq ctx, PartialOrder epoch, PartialOrder phase) =>
  RuntimeTime ctx epoch phase ->
  RuntimeTime ctx epoch phase ->
  Bool
capabilityAdvanceRegressionTyped sourceTime targetTime =
  runtimeTimeSameScopeLeq sourceTime targetTime
    || ( case downgradeRuntimeCapability targetTime (mintRootRuntimeCapability sourceTime) of
           Left refusal ->
             ricaSourceTime refusal == sourceTime
               && ricaTargetTime refusal == targetTime
           Right _ ->
             False
       )

runtimeTimeScopeLaws :: Assertion
runtimeTimeScopeLaws = do
  let rootTime =
        runtimeTime 0 10
      childTime =
        enterRuntimeTimeScope 7 rootTime
  assertEqual
    "leaveRuntimeTimeScope reverses enterRuntimeTimeScope"
    (Just rootTime)
    (leaveRuntimeTimeScope childTime)
  assertBool
    "entered scope is a descendant of the root scope"
    (isDescendantOf (rtScope rootTime) (rtScope childTime))
  assertBool
    "semantic time order rejects different scopes"
    (not (runtimeTimeSameScopeLeq rootTime childTime))
  case delayRuntimeTimeFeedback rootTime of
    Nothing ->
      assertFailure "expected feedback delay to advance frontier stamp"
    Just delayedTime ->
      assertBool
        "feedback delay advances within the same runtime context and scope"
        (runtimeTimeSameScopeLt rootTime delayedTime)

runtimeFrontierStoresProductAntichains :: Assertion
runtimeFrontierStoresProductAntichains = do
  let leftBranch =
        runtimeProductTime (1, 0) 0
      rightBranch =
        runtimeProductTime (0, 1) 0
      joinedBranch =
        runtimeProductTime (2, 1) 0
      branchedFrontier =
        frontierAdvanceVisibleMin rightBranch $
          frontierAdvanceVisibleMin leftBranch emptyRuntimeFrontier
      joinedFrontier =
        frontierAdvanceVisibleMin joinedBranch branchedFrontier
  assertEqual
    "incomparable visible cutoffs survive as an antichain"
    (Set.fromList [leftBranch, rightBranch])
    (Set.fromList (upperFrontierPoints (frontierVisibleAntichainForContext 0 branchedFrontier)))
  assertEqual
    "a later product-time cutoff dominates both old branches"
    (Set.singleton joinedBranch)
    (Set.fromList (upperFrontierPoints (frontierVisibleAntichainForContext 0 joinedFrontier)))

runtimeProductTime ::
  (Natural, Natural) ->
  Word ->
  RuntimeTime Int Int (Natural, Natural)
runtimeProductTime phaseValue stamp =
  DifferentialTime.runtimeTime
    0
    emptyRuntimeScope
    0
    phaseValue
    (frontierStamp (fromIntegral stamp))

data LocalFactCtx
  = LocalFactBottom
  | LocalFactLeft
  | LocalFactTop
  deriving stock (Eq, Ord, Show)

newtype LocalFactBoundary = LocalFactBoundary
  { unLocalFactBoundary :: Set Int
  }
  deriving stock (Eq, Show)

instance BoundaryOps LocalFactBoundary where
  type BoundaryOverlap LocalFactBoundary = Set Int

  overlapBetweenBoundary (LocalFactBoundary left) (LocalFactBoundary right) =
    Set.intersection left right

  restrictBoundaryRaw overlapValue (LocalFactBoundary boundaryValue) =
    LocalFactBoundary (Set.intersection overlapValue boundaryValue)

  compatibleBoundaryRaw leftBoundary rightBoundary =
    if leftBoundary == rightBoundary
      then Right leftBoundary
      else Left leftBoundary

  subsumesBoundaryRaw (LocalFactBoundary left) (LocalFactBoundary right) =
    Set.isSubsetOf right left

localFactConstructionAndAntichainLaws :: Assertion
localFactConstructionAndAntichainLaws =
  case compileContextLattice localFactUniverse localFactOrder of
    Left compileError ->
      assertFailure ("local fact fixture lattice failed: " <> show compileError)
    Right latticeValue -> do
      leftSupport <-
        assertRight
          "left support"
          (supportBasis latticeValue [LocalFactLeft, LocalFactTop])
      topSupport <-
        assertRight
          "top support"
          (supportBasis latticeValue [LocalFactTop])
      leftAddress <-
        assertRight
          "left local address"
          ( mkLocalAddress
              latticeValue
              (PropositionKey (7 :: Int))
              leftSupport
          )
      topAddress <-
        assertRight
          "top local address"
          ( mkLocalAddress
              latticeValue
              (PropositionKey (7 :: Int))
              topSupport
          )
      assertEqual
        "mkLocalAddress minimizes redundant support generators"
        [LocalFactLeft]
        (supportGenerators (laSupport leftAddress))
      assertEqual
        "mkLocalAddress caches carrier closure"
        (Set.fromList [LocalFactLeft, LocalFactTop])
        (carrierContexts (laCarrier leftAddress))
      let weakerFact =
            mkLocalFact leftAddress (LocalFactBoundary (Set.singleton 1)) ()
          strongerFact =
            mkLocalFact topAddress (LocalFactBoundary (Set.fromList [1, 2])) ()
          incomparableFact =
            mkLocalFact leftAddress (LocalFactBoundary (Set.singleton 9)) ()
      assertEqual
        "stronger local fact deletes dominated fact in its proposition bucket"
        [strongerFact]
        (membersAntichain (insertAntichain strongerFact (insertAntichain weakerFact emptyFactAntichain)))
      assertEqual
        "incomparable local facts are retained"
        2
        (length (membersAntichain (insertAntichain incomparableFact (insertAntichain weakerFact emptyFactAntichain))))

localFactUniverse :: Set LocalFactCtx
localFactUniverse =
  Set.fromList [LocalFactBottom, LocalFactLeft, LocalFactTop]

localFactOrder :: ContextOrderDecl LocalFactCtx
localFactOrder =
  contextOrderDecl
    LocalFactTop
    LocalFactBottom
    [ (LocalFactBottom, LocalFactLeft),
      (LocalFactLeft, LocalFactTop)
    ]

runtimeTime :: Int -> Word -> RuntimeTime Int Int Int
runtimeTime contextValue stamp =
  DifferentialTime.runtimeTime
    contextValue
    emptyRuntimeScope
    0
    0
    (frontierStamp (fromIntegral stamp))

assertRight :: Show err => String -> Either err value -> IO value
assertRight label eitherValue =
  case eitherValue of
    Left err ->
      assertFailure (label <> " failed: " <> show err)
    Right value ->
      pure value

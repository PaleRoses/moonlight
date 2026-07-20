{-# LANGUAGE TypeApplications #-}

module Moonlight.Saturation.Test.ContextWorkload
  ( ContextMatchProfile (..),
    TestContextInputs,
    ContextMatchingWorkload (..),
    contextMatchingWorkload,
    testSupportedMatch,
    testSaturationRoundView,
  )
where

import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Saturation.Context.Program.View
import Moonlight.Saturation.Substrate
import Moonlight.Saturation.Test.ContextFixture

data ContextMatchProfile = DisjointContextMatches | OverlappingContextMatches
  deriving stock (Eq, Ord, Show)

type TestContextInputs = Map TestContext (IntSet, IntSet, [TestRule])

data ContextMatchingWorkload = ContextMatchingWorkload
  { contextMatchingGraph :: !(SatGraph TestSubstrate),
    contextMatchingInputs :: !TestContextInputs
  }
  deriving stock (Show)

contextMatchingWorkload :: ContextMatchProfile -> Int -> ContextMatchingWorkload
contextMatchingWorkload profile size =
  let roots = [1 .. max 0 size]
   in ContextMatchingWorkload
        (graphFromClasses roots)
        (Map.fromList ((BaseContext, emptyContextInput) : fmap (contextInput profile roots) indexedContexts))

contextInput :: ContextMatchProfile -> [Int] -> (Int, TestContext) -> (TestContext, (IntSet, IntSet, [TestRule]))
contextInput profile roots indexedContext@(_offset, contextValue) =
  (contextValue, (IntSet.empty, IntSet.empty, [matchingRule profile roots indexedContext]))

matchingRule :: ContextMatchProfile -> [Int] -> (Int, TestContext) -> TestRule
matchingRule profile roots (offset, contextValue) =
  case profile of
    DisjointContextMatches ->
      makeContextRule
        (101 + offset)
        contextValue
        (filter (\root -> (root - 1) `mod` length indexedContexts == offset) roots)
        False
        noEffect
    OverlappingContextMatches ->
      (makeContextRule 29 LeftContext [] False noEffect)
        { trContextRoots = Map.fromList [(context, roots) | (_index, context) <- indexedContexts]
        }

emptyContextInput :: (IntSet, IntSet, [TestRule])
emptyContextInput = (IntSet.empty, IntSet.empty, [])

indexedContexts :: [(Int, TestContext)]
indexedContexts = zip [0 ..] [LeftContext, RightContext, TopContext]

testSupportedMatch :: Int -> Int -> TestContext -> TestSupportedMatch
testSupportedMatch ruleKey rootClass contextValue =
  supportedFor contextValue (TestMatch (makeBaseRule ruleKey [rootClass] False noEffect) rootClass)

testSaturationRoundView :: Int -> TestGraph -> SaturationRoundView TestSubstrate
testSaturationRoundView iteration graph =
  SaturationRoundView
    { srvIteration = iteration,
      srvGraph = graph,
      srvBaseGraph = graph,
      srvFacts = IntSet.empty,
      srvFactDerivations = IntSet.empty,
      srvFactsChanged = False,
      srvFactRoundCount = 0,
      srvBaseEligibleMatchCount = 0,
      srvContextEligibleMatchCount = 0,
      srvAggregatedEligibleMatchCount = 0,
      srvContextRevision = 0
    }

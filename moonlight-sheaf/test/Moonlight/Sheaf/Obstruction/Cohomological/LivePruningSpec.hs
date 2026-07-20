module Moonlight.Sheaf.Obstruction.Cohomological.LivePruningSpec
  ( tests,
  )
where

import Data.Set qualified as Set
import Moonlight.Core (RegionNodeId (..))
import Moonlight.Derived.Site (FinObjectId (..))
import Moonlight.Sheaf.Obstruction.Cohomological.Evidence.LivePruning
  ( liveMicrosupportPruningEvidence,
    nonCriticalNodesFromLiveMicrosupport,
    recomputeLiveMicrosupport,
    staticComponentIndex,
    updateLiveMicrosupport,
  )
import Moonlight.Sheaf.Site.Construction.Nerve
  ( CellKey (ckOrdinal),
    nerveCellKey,
    nerveSiteCells,
    siteCellsAtDimension,
  )
import Moonlight.Sheaf.TestFixture.Site
  ( sampleNerveSite,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )

tests :: TestTree
tests =
  testGroup
    "live pruning"
    [ testCase "empty seed has empty live pruning obstruction evidence" testEmptySeedEvidence,
      testCase "seeded recomputation does not fabricate nodes outside the seed support" testSeededRecomputationSupport,
      testCase "incremental update agrees with the full recompute oracle across seed transitions" testUpdateMatchesRecomputeOracle
    ]

testEmptySeedEvidence :: Assertion
testEmptySeedEvidence =
  let liveMicrosupport =
        recomputeLiveMicrosupport
          nodeRegionNodeId
          cellRegionNodeId
          (staticComponentIndex sampleNerveSite)
          Set.empty
   in do
        assertEqual
          "expected empty seed support to produce no noncritical nodes"
          Set.empty
          (nonCriticalNodesFromLiveMicrosupport liveMicrosupport)
        assertBool
          "expected empty seed support to produce no pruning evidence"
          (null (liveMicrosupportPruningEvidence liveMicrosupport))

testSeededRecomputationSupport :: Assertion
testSeededRecomputationSupport =
  case siteCellsAtDimension sampleNerveSite 0 of
    [] ->
      assertFailure "expected the generic nerve fixture to contain a 0-cell"
    rootCell : _ ->
      let seededCellKeys =
            Set.singleton (nerveCellKey rootCell)
          seededNodeIds =
            Set.map (RegionNodeId . ckOrdinal) seededCellKeys
          liveMicrosupport =
            recomputeLiveMicrosupport
              nodeRegionNodeId
              cellRegionNodeId
              (staticComponentIndex sampleNerveSite)
              seededCellKeys
          nonCriticalNodes =
            nonCriticalNodesFromLiveMicrosupport liveMicrosupport
       in assertBool
            "expected live pruning to stay inside the seeded support"
            (nonCriticalNodes `Set.isSubsetOf` seededNodeIds)

testUpdateMatchesRecomputeOracle :: Assertion
testUpdateMatchesRecomputeOracle =
  let staticIndex =
        staticComponentIndex sampleNerveSite
      allCellKeys =
        fmap nerveCellKey (nerveSiteCells sampleNerveSite)
      seedSets =
        fmap
          Set.fromList
          [ [],
            take 1 allCellKeys,
            take 2 allCellKeys,
            drop 1 allCellKeys,
            take 1 (reverse allCellKeys),
            allCellKeys
          ]
      recomputeAt =
        recomputeLiveMicrosupport nodeRegionNodeId cellRegionNodeId staticIndex
      updateAt =
        updateLiveMicrosupport nodeRegionNodeId cellRegionNodeId staticIndex
   in sequence_
        [ assertEqual
            ( "expected update from "
                <> show (Set.map ckOrdinal priorSeeds)
                <> " to "
                <> show (Set.map ckOrdinal nextSeeds)
                <> " to agree with the full recompute oracle"
            )
            (recomputeAt nextSeeds)
            (updateAt nextSeeds (recomputeAt priorSeeds))
        | priorSeeds <- seedSets,
          nextSeeds <- seedSets
        ]

nodeRegionNodeId :: FinObjectId -> Maybe RegionNodeId
nodeRegionNodeId (FinObjectId ordinalValue) =
  Just (RegionNodeId ordinalValue)

cellRegionNodeId :: CellKey -> Maybe RegionNodeId
cellRegionNodeId =
  Just . RegionNodeId . ckOrdinal

module Moonlight.Sheaf.Obstruction.PruningSpec
  ( tests,
  )
where

import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Core
  ( RegionNodeId (..),
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Evidence.Pruning
  ( CohomologicalPruningGates (..),
    CohomologicalPruningFootprint (..),
    CohomologicalPruningObstruction (..),
    PruningEvidence (..),
    buildPruningGates,
    keepRegion,
    keepSeed,
    prunedRegions,
    prunedSeeds,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Types
  ( CandidateRegion,
    CandidateRegionSeed,
    RegionScale (FineRegion),
    mkCandidateRegionSeedWithContext,
    mkCandidateRegionWithNode,
  )
import Moonlight.Sheaf.Site.Stalk.Interface
  ( WitnessClass (..),
  )
import Moonlight.Sheaf.Pruning
  ( PruningCertificate (pcFootprint),
    PruningReport (prPruned),
    pruningDecisionAllowed,
    pruningDecisionRejectedList,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    testCase,
    (@?=),
  )

tests :: TestTree
tests =
  testGroup
    "pruning"
    [ testCase "empty evidence keeps seeds and regions" testEmptyEvidenceKeepsEverything,
      testCase "microsupport evidence rejects matching seed and region nodes" testMicrosupportEvidenceRejectsMatchingNodes,
      testCase "context evidence respects seed context ordinals" testContextEvidenceRespectsOrdinals,
      testCase "witness evidence rejects only obstructed nodes" testWitnessEvidenceRejectsOnlyObstructedNodes,
      testCase "composed evidence combines by conjunction" testComposedEvidenceCombinesByConjunction,
      testCase "same-family evidence composes by support lattice operations" testSameFamilyEvidenceComposesBySupportLattices,
      testCase "report helpers agree with direct gates" testReportHelpersAgreeWithDirectGates
    ]

testEmptyEvidenceKeepsEverything :: Assertion
testEmptyEvidenceKeepsEverything =
  let gates :: CohomologicalPruningGates Int
      gates = buildPruningGates []
      seedValue = seedAt 3 (Just 1)
      regionValue = regionAt 3
   in do
        assertBool "seed should survive empty pruning evidence" (seedAllowed gates seedValue)
        filteredRegions gates [regionValue] @?= [regionValue]

testMicrosupportEvidenceRejectsMatchingNodes :: Assertion
testMicrosupportEvidenceRejectsMatchingNodes =
  let gates :: CohomologicalPruningGates Int
      gates = buildPruningGates [MicrosupportNonCritical (Set.singleton (RegionNodeId 7))]
      rejectedSeed = seedAt 7 (Just 1)
      keptSeed = seedAt 8 (Just 1)
      rejectedRegion = regionAt 7
      keptRegion = regionAt 8
   in do
        assertBool "matching non-critical seed should be rejected" (not (seedAllowed gates rejectedSeed))
        seedObstructions gates rejectedSeed @?= [MicrosupportNonCriticalObstruction (RegionNodeId 7)]
        regionObstructions gates rejectedRegion @?= [MicrosupportNonCriticalObstruction (RegionNodeId 7)]
        assertBool "other seed should survive" (seedAllowed gates keptSeed)
        filteredRegions gates [rejectedRegion, keptRegion] @?= [keptRegion]

testContextEvidenceRespectsOrdinals :: Assertion
testContextEvidenceRespectsOrdinals =
  let gates :: CohomologicalPruningGates Int
      gates = buildPruningGates [ContextRelevant (Set.fromList [1, 3])]
   in do
        assertBool "relevant context should survive" (seedAllowed gates (seedAt 5 (Just 3)))
        assertBool "missing context should remain conservatively alive" (seedAllowed gates (seedAt 5 Nothing))
        seedObstructions gates (seedAt 5 (Just 2)) @?= [ContextIrrelevantObstruction 2]

testWitnessEvidenceRejectsOnlyObstructedNodes :: Assertion
testWitnessEvidenceRejectsOnlyObstructedNodes =
  let gates :: CohomologicalPruningGates Int
      gates =
        buildPruningGates
          [ WitnessClassification
              ( Map.fromList
                  [ (RegionNodeId 2, WitnessTerminal),
                    (RegionNodeId 3, WitnessComposed),
                    (RegionNodeId 4, WitnessObstructed)
                  ]
              )
          ]
   in do
        assertBool "terminal witness should survive" (seedAllowed gates (seedAt 2 (Just 1)))
        assertBool "composed witness should survive" (seedAllowed gates (seedAt 3 (Just 1)))
        seedObstructions gates (seedAt 4 (Just 1)) @?= [WitnessObstructedObstruction (RegionNodeId 4)]
        regionObstructions gates (regionAt 4) @?= [WitnessObstructedObstruction (RegionNodeId 4)]
        assertBool "unknown witness should survive" (seedAllowed gates (seedAt 5 (Just 1)))

testComposedEvidenceCombinesByConjunction :: Assertion
testComposedEvidenceCombinesByConjunction =
  let gates :: CohomologicalPruningGates Int
      gates =
        buildPruningGates
          [ MicrosupportNonCritical (Set.singleton (RegionNodeId 9)),
            ContextRelevant (Set.singleton 1)
          ]
   in do
        seedObstructions gates (seedAt 9 (Just 1)) @?= [MicrosupportNonCriticalObstruction (RegionNodeId 9)]
        seedObstructions gates (seedAt 8 (Just 2)) @?= [ContextIrrelevantObstruction 2]
        assertBool "seed satisfying both gates should survive" (seedAllowed gates (seedAt 8 (Just 1)))

testSameFamilyEvidenceComposesBySupportLattices :: Assertion
testSameFamilyEvidenceComposesBySupportLattices =
  let gates :: CohomologicalPruningGates Int
      gates =
        buildPruningGates
          [ MicrosupportNonCritical (Set.singleton (RegionNodeId 1)),
            MicrosupportNonCritical (Set.singleton (RegionNodeId 2)),
            ContextRelevant (Set.fromList [1, 2]),
            ContextRelevant (Set.fromList [2, 3]),
            WitnessClassification (Map.singleton (RegionNodeId 4) WitnessTerminal),
            WitnessClassification (Map.singleton (RegionNodeId 4) WitnessObstructed)
          ]
   in do
        seedObstructions gates (seedAt 1 (Just 2)) @?= [MicrosupportNonCriticalObstruction (RegionNodeId 1)]
        seedObstructions gates (seedAt 2 (Just 2)) @?= [MicrosupportNonCriticalObstruction (RegionNodeId 2)]
        assertBool "context support should compose by intersection" (seedAllowed gates (seedAt 3 (Just 2)))
        seedObstructions gates (seedAt 3 (Just 1)) @?= [ContextIrrelevantObstruction 1]
        seedObstructions gates (seedAt 3 (Just 3)) @?= [ContextIrrelevantObstruction 3]
        seedObstructions gates (seedAt 4 (Just 2)) @?= [WitnessObstructedObstruction (RegionNodeId 4)]

testReportHelpersAgreeWithDirectGates :: Assertion
testReportHelpersAgreeWithDirectGates =
  let gates :: CohomologicalPruningGates Int
      gates =
        buildPruningGates
          [ MicrosupportNonCritical (Set.singleton (RegionNodeId 7))
          ]
      keptSeed = seedAt 8 (Just 1)
      rejectedSeed = seedAt 7 (Just 1)
      keptRegion = regionAt 8
      rejectedRegion = regionAt 7
   in do
        keepSeed gates keptSeed @?= True
        keepSeed gates rejectedSeed @?= False
        keepRegion gates keptRegion @?= True
        keepRegion gates rejectedRegion @?= False
        fmap fst (prPruned (prunedSeeds gates [keptSeed, rejectedSeed])) @?= [rejectedSeed]
        fmap fst (prPruned (prunedRegions gates [keptRegion, rejectedRegion])) @?= [rejectedRegion]
        fmap (cpfSeedNodes . pcFootprint . snd) (prPruned (prunedSeeds gates [keptSeed, rejectedSeed]))
          @?= [Set.singleton (RegionNodeId 7)]
        fmap (cpfRegionNodes . pcFootprint . snd) (prPruned (prunedRegions gates [keptRegion, rejectedRegion]))
          @?= [Set.singleton (RegionNodeId 7)]

seedAt :: Int -> Maybe Int -> CandidateRegionSeed Int
seedAt nodeOrdinal =
  mkCandidateRegionSeedWithContext 11 (RegionNodeId nodeOrdinal) 101

regionAt :: Int -> CandidateRegion Int
regionAt nodeOrdinal =
  mkCandidateRegionWithNode 11 IntSet.empty 0 FineRegion (RegionNodeId nodeOrdinal) 101

seedAllowed ::
  CohomologicalPruningGates root ->
  CandidateRegionSeed root ->
  Bool
seedAllowed gates =
  pruningDecisionAllowed . cpgSeedDecision gates

seedObstructions ::
  CohomologicalPruningGates root ->
  CandidateRegionSeed root ->
  [CohomologicalPruningObstruction]
seedObstructions gates =
  pruningDecisionRejectedList . cpgSeedDecision gates

regionObstructions ::
  CohomologicalPruningGates root ->
  CandidateRegion root ->
  [CohomologicalPruningObstruction]
regionObstructions gates =
  pruningDecisionRejectedList . cpgRegionDecision gates

filteredRegions ::
  CohomologicalPruningGates root ->
  [CandidateRegion root] ->
  [CandidateRegion root]
filteredRegions gates =
  filter (pruningDecisionAllowed . cpgRegionDecision gates)

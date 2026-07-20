module Moonlight.EGraph.Saturation.GoldenPathSpec.Pruning
  ( tests
  )
where

import Control.Monad (foldM)
import Moonlight.Pale.Ghc.Expr (ScopeCtx)
import Moonlight.Sheaf.Pruning (pruningDecisionAllowed)
import Data.Foldable (forM_)
import Data.IntSet qualified as IntSet
import Data.List (minimumBy)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Data.Set qualified as Set
import Data.Word (Word64)
import Data.Fix (Fix)
import GHC.Stats (RTSStats (..), getRTSStats, getRTSStatsEnabled)
import System.Mem (performMajorGC)
import System.CPUTime (getCPUTime)
import Moonlight.Core (RegionNodeId (..))
import Moonlight.Saturation.Matching qualified as GenericMatching
import Moonlight.EGraph.Saturation.Cohomological.Backend.Instance
import Moonlight.EGraph.Saturation.Cohomological.Backend.Matching
import Moonlight.EGraph.Saturation.Cohomological.Backend.Seed
  ( materializeSeedWithPruning
  )
import Moonlight.Sheaf.Obstruction
  ( CohomologicalPruningGates (..),
    CohomologicalProfile (ConservativeProfile),
    PruningEvidence (..),
    buildPruningGates,
    profilePolicy
  )
import Moonlight.Sheaf.Obstruction
  ( CandidateRegionSeed (crsFingerprint, crsNodeId, crsRoot),
    mkCandidateRegionSeed
  )
import Moonlight.Saturation.Obstruction.Cohomological.Seed
  ( candidateRegionSeedKey,
    seedFrontierPlanFromList,
  )
import Moonlight.EGraph.Saturation.GoldenPathSpec.Prelude
import Moonlight.EGraph.Pure.Context (cegSite)
import Moonlight.Sheaf.Twist.SupportedRuleSpec qualified as SheafTwist
import Moonlight.Sheaf.Context.Site (UnitContextSiteOwner)

classesEquivalent :: EGraph f a -> ClassId -> ClassId -> Bool
classesEquivalent graph leftClass rightClass =
  canonicalizeClassId graph leftClass == canonicalizeClassId graph rightClass


measureAllocatedBytes :: IO value -> IO (value, Word64)
measureAllocatedBytes action = do
  performMajorGC
  beforeStats <- requireRtsStats
  value <- action
  performMajorGC
  afterStats <- requireRtsStats
  pure (value, allocated_bytes afterStats - allocated_bytes beforeStats)

requireRtsStats :: IO RTSStats
requireRtsStats = do
  enabled <- getRTSStatsEnabled
  if enabled
    then getRTSStats
    else assertFailure "RTS stats are disabled; run the test with +RTS -T or compile it with -with-rtsopts=-T"

data PairScaleFixtureError
  = PairScaleFixtureMissingZero
  | PairScaleFixtureUnpairedClass !ClassId
  deriving stock (Eq, Show)

pairScaleTerms :: Int -> [Fix TestF]
pairScaleTerms pairCount =
  litTerm 0
    : foldMap
      ( \ordinal ->
          let inputTerm = litTerm ordinal
           in [inputTerm, pairTerm inputTerm (litTerm 0)]
      )
      [1 .. pairCount]

pairScaleClassAssignments ::
  [ClassId] ->
  Either PairScaleFixtureError (ClassId, [(Int, ClassId, ClassId)])
pairScaleClassAssignments classIds =
  case classIds of
    [] ->
      Left PairScaleFixtureMissingZero
    zeroClass : pairClassIds ->
      fmap ((,) zeroClass) (assignPairs 1 pairClassIds)
  where
    assignPairs _ [] =
      Right []
    assignPairs ordinal (leftRoot : pairClass : remainingClassIds) =
      fmap
        ((ordinal, pairClass, leftRoot) :)
        (assignPairs (ordinal + 1) remainingClassIds)
    assignPairs _ [unpairedClass] =
      Left (PairScaleFixtureUnpairedClass unpairedClass)

seedInterpreterFrom ::
  [CandidateRegionSeed ClassId] ->
  SeedInterpreter request seedPattern frontier ClassId
seedInterpreterFrom mockSeeds =
  SeedInterpreter
    { siSeedPlan = \_ _ -> seedFrontierPlanFromList mockSeeds,
      siFrontierSeedPlan = \_ _ _ -> seedFrontierPlanFromList mockSeeds,
      siRefineSeedPlan = \_ _ _ -> seedFrontierPlanFromList [],
      siMaterializeSeed = \_ _ seed -> Just (mkFineRegion (crsRoot seed) (crsFingerprint seed)),
      siSeedKey = candidateRegionSeedKey,
      siSeedsForRootsPlan = \rootKeys _ _ ->
        seedFrontierPlanFromList (filter (\seed -> IntSet.member (classIdKey (crsRoot seed)) rootKeys) mockSeeds),
      siSeedsForNodesPlan = \nodeIds _ _ ->
        seedFrontierPlanFromList (filter (\seed -> Set.member (crsNodeId seed) nodeIds) mockSeeds),
      siSeedsForKeysPlan = \seedKeys _ _ ->
        seedFrontierPlanFromList (filter (\seed -> Set.member (candidateRegionSeedKey seed) seedKeys) mockSeeds)
    }

tests :: TestTree
tests =
  testGroup
    "CohomologicalPruning"
    [ testCase "obstructed root: Pair(1,0) ≠ 1 because pruning blocks the rewrite" $
        withFiveTestTerms (litTerm 1) (litTerm 0) (pairTerm (litTerm 1) (litTerm 0)) (litTerm 2) (pairTerm (litTerm 2) (litTerm 0)) $ \oneClass zeroClass pairOneClass twoClass pairTwoClass graph5 ->
          withGoldenProofGraph graph5 $ \proofGraph0 -> do
              let backend =
                    mkExactWitnessBackend
                      ( obstructingContext
                          pairOneClass
                          (oneClass, zeroClass)
                          pairTwoClass
                          (twoClass, zeroClass)
                      )
              localFamily <-
                expectRight
                  ( SheafTwist.supportedRuleBook
                      (cegSite (sceContextGraph (pgGraph proofGraph0)))
                      [ SheafTwist.SupportedRuleSpec
                          { SheafTwist.srsSupport = principalSupport LocalScope,
                            SheafTwist.srsRule = pairIdentityRule
                          }
                      ]
                  )
              report <-
                expectRight
                  (runGoldenSupportCase (goldenCohomologicalSaturationConfig backend) localFamily proofGraph0)
              let resultGraph = sceContextGraph (pgGraph (srCarrier report))
              classesEquivalentAt LocalScope pairOneClass oneClass resultGraph
                @?= False,
      testCase "non-obstructed root: Pair(2,0) = 2 because pruning allows the rewrite" $
        withFiveTestTerms (litTerm 1) (litTerm 0) (pairTerm (litTerm 1) (litTerm 0)) (litTerm 2) (pairTerm (litTerm 2) (litTerm 0)) $ \oneClass zeroClass pairOneClass twoClass pairTwoClass graph5 ->
          withGoldenProofGraph graph5 $ \proofGraph0 -> do
              let backend =
                    mkExactWitnessBackend
                      ( obstructingContext
                          pairOneClass
                          (oneClass, zeroClass)
                          pairTwoClass
                          (twoClass, zeroClass)
                      )
              localFamily <-
                expectRight
                  ( SheafTwist.supportedRuleBook
                      (cegSite (sceContextGraph (pgGraph proofGraph0)))
                      [ SheafTwist.SupportedRuleSpec
                          { SheafTwist.srsSupport = principalSupport LocalScope,
                            SheafTwist.srsRule = pairIdentityRule
                          }
                      ]
                  )
              report <-
                expectRight
                  (runGoldenSupportCase (goldenCohomologicalSaturationConfig backend) localFamily proofGraph0)
              let resultGraph = sceContextGraph (pgGraph (srCarrier report))
              classesEquivalentAt LocalScope pairTwoClass twoClass resultGraph
                @?= True,
      testCase "permissive context: cohomological matching produces same equivalences as generic-join" $
        withThreeTestTerms (litTerm 1) (litTerm 0) (pairTerm (litTerm 1) (litTerm 0)) $ \oneClass zeroClass pairClass graph3 ->
          withGoldenProofGraph graph3 $ \proofGraph0 ->
            let backend = mkExactWitnessBackend (singleRootContext pairClass oneClass zeroClass)
         in do
              localFamily <-
                expectRight
                  ( SheafTwist.supportedRuleBook
                      (cegSite (sceContextGraph (pgGraph proofGraph0)))
                      [ SheafTwist.SupportedRuleSpec
                          { SheafTwist.srsSupport = principalSupport LocalScope,
                            SheafTwist.srsRule = pairIdentityRule
                          }
                      ]
                  )
              cohomologicalReport <-
                expectRight
                  (runGoldenSupportCase (goldenCohomologicalSaturationConfig backend) localFamily proofGraph0)
              recursiveReport <-
                expectRight
                  (runGoldenSupportCase goldenGenericJoinSaturationConfig localFamily proofGraph0)
              let cohomGraph = sceContextGraph (pgGraph (srCarrier cohomologicalReport))
                  recurGraph = sceContextGraph (pgGraph (srCarrier recursiveReport))
              classesEquivalentAt LocalScope pairClass oneClass cohomGraph
                @?= classesEquivalentAt LocalScope pairClass oneClass recurGraph
              classesEquivalentAt GlobalScope pairClass oneClass cohomGraph
                @?= classesEquivalentAt GlobalScope pairClass oneClass recurGraph,
      testCase "live-pruned matching path: resolution-backed seeds materialize runtime regions" $
        withThreeTestTerms (litTerm 1) (litTerm 0) (pairTerm (litTerm 1) (litTerm 0)) $ \oneClass _zeroClass pairClass graph3 ->
          let context = singleRootContext pairClass oneClass _zeroClass
              pairSeed = mkCandidateRegionSeed pairClass (RegionNodeId (classIdKey pairClass)) (classIdKey pairClass)
              oneSeed = mkCandidateRegionSeed oneClass (RegionNodeId (classIdKey oneClass)) (classIdKey oneClass)
              mockSeeds = [pairSeed, oneSeed]
              seedInterp = seedInterpreterFrom mockSeeds
              baseConfig :: EGraphSaturationConfig UnitContextSiteOwner ScopeCtx TestF () ()
              baseConfig =
                SaturationConfig
                  { scBudget = goldenSaturationBudget,
                    scMatchingStrategy = GenericJoinMatching,
                    scSchedulerConfig = backoffSchedulerConfig (backoffConfig 1000 5)
                  }
         in do
              rewriteSystem <- expectRight pairWitnessRewriteSystem
              let backend = mkExactWitnessBackendWithRewriteSystem rewriteSystem context
                  prepared = prepareCohomologicalBackend backend
                  preparedWithSeeds =
                    prepared
                      { pcbConfiguration = (pcbConfiguration prepared) {cbSeedInterpreter = Just seedInterp}
                      }
                  prunedConfig :: EGraphSaturationConfig UnitContextSiteOwner ScopeCtx TestF () ()
                  prunedConfig =
                    baseConfig
                      { scMatchingStrategy =
                          livePrunedCohomologicalMatchingStrategy
                            preparedWithSeeds
                            emptyGuardCapabilityResolver
                      }
              assertBool "resolution exists" (case pcbResolution preparedWithSeeds of Nothing -> False; Just _ -> True)
              prunedReport <- expectRight (saturateWith prunedConfig [pairIdentityRule] graph3)
              plainReport <- expectRight (saturateWith baseConfig [pairIdentityRule] graph3)
              assertBool "pruned: matches applied" (srMatchesApplied prunedReport > 0)
              assertBool "plain: matches applied" (srMatchesApplied plainReport > 0)
              classesEquivalent (saturationReportBaseGraph prunedReport) pairClass oneClass
                @?= True
              classesEquivalent (saturationReportBaseGraph plainReport) pairClass oneClass
                @?= True,
      testCase "hierarchical pruning refuses unseeded exhaustive region enumeration" $
        withThreeTestTerms (litTerm 1) (litTerm 0) (pairTerm (litTerm 1) (litTerm 0)) $ \oneClass zeroClass pairClass graph3 ->
          let context = singleRootContext pairClass oneClass zeroClass
         in do
              rewriteSystem <- expectRight pairWitnessRewriteSystem
              compiledQuery <- expectRight (compileQuery (PatternVar queryVar0))
              let backend =
                    mkBackendWithRewriteSystem
                      rewriteSystem
                      context
                      (profilePolicy ConservativeProfile)
                  prepared = prepareCohomologicalBackend backend
                  algebra = cohomologicalMatchingAlgebra (pcbConfiguration prepared)
                  request = mkMatchingRequest compiledQuery
                  world = mkMatchingWorld graph3
                  result =
                    runFullMatchingQuery
                      algebra
                      (GenericMatching.maInitialState algebra)
                      world
                      request
              case result of
                Left EGraphMatchingHierarchicalPruningWithoutSeedFrontier ->
                  pure ()
                Left obstruction ->
                  assertFailure ("expected missing seed frontier obstruction, received " <> show obstruction)
                Right _ ->
                  assertFailure "expected hierarchical pruning without a seed frontier to refuse exhaustive enumeration",
      testCase "live-pruned cohomological backend at scale: 250 structural roots, not slower than plain generic join" $
        withTestTerms (pairScaleTerms 250) $ \classIds graphFinal -> do
          (zeroClass, pairsWithLeftRoot) <-
            expectRight (pairScaleClassAssignments classIds)
          let pairAssignments =
                fmap
                  (\(_, pairClass, leftRoot) -> (pairClass, (leftRoot, zeroClass)))
                  pairsWithLeftRoot
              expectedEquivalences = pairsWithLeftRoot
              context = manyPairContext pairAssignments
              baseConfig :: EGraphSaturationConfig UnitContextSiteOwner ScopeCtx TestF () ()
              baseConfig =
                SaturationConfig
                  { scBudget = SaturationBudget {sbMaxIterations = 50, sbMaxNodes = 125000},
                    scMatchingStrategy = GenericJoinMatching,
                    scSchedulerConfig = backoffSchedulerConfig (backoffConfig 1000 5)
                  }
          rewriteSystem <- expectRight pairWitnessRewriteSystem
          let backend = mkExactWitnessBackendWithRewriteSystem rewriteSystem context
              prepared = prepareCohomologicalBackend backend
              preparedWithSeeds = prepared
              prunedConfig :: EGraphSaturationConfig UnitContextSiteOwner ScopeCtx TestF () ()
              prunedConfig =
                baseConfig
                  { scMatchingStrategy =
                      livePrunedCohomologicalMatchingStrategy
                        preparedWithSeeds
                        emptyGuardCapabilityResolver
                  }
          assertBool "resolution exists" (case pcbResolution preparedWithSeeds of Nothing -> False; Just _ -> True)
          let rules = [pairIdentityRule]
              timedSaturation configValue = do
                startTime <- getCPUTime
                reportValue <- expectRight (saturateWith configValue rules graphFinal)
                let matchCount = srMatchesApplied reportValue
                    nodeCount = eGraphNodeCount (saturationReportBaseGraph reportValue)
                    resultGraph = saturationReportBaseGraph reportValue
                    equivalentPairCount =
                      length
                        ( filter
                            ( \(_, pairClass, leftRoot) ->
                                classesEquivalent resultGraph pairClass leftRoot
                            )
                            expectedEquivalences
                        )
                matchCount `seq` nodeCount `seq` equivalentPairCount `seq` pure ()
                endTime <- getCPUTime
                pure (reportValue, endTime - startTime, matchCount, nodeCount, equivalentPairCount)
              profiledTimedSaturation configValue =
                measureAllocatedBytes (timedSaturation configValue)
              bestTimedSaturation configValue = do
                timings <- traverse (const (profiledTimedSaturation configValue)) [1 .. 3 :: Int]
                pure (minimumBy (comparing (\((_, elapsed, _, _, _), _) -> elapsed)) timings)
              elapsedMs elapsed = fromIntegral elapsed / 1e9 :: Double
          ((prunedReport, prunedElapsed, prunedMatches, prunedNodes, prunedEquivalentPairs), prunedAllocatedBytes) <- bestTimedSaturation prunedConfig
          ((plainReport, plainElapsed, plainMatches, plainNodes, plainEquivalentPairs), plainAllocatedBytes) <- bestTimedSaturation baseConfig
          let prunedMs = elapsedMs prunedElapsed
              plainMs = elapsedMs plainElapsed
              speedup = plainMs / max 0.001 prunedMs
              timingToleranceMs = max 5.0 (plainMs * 0.20)
              report =
                "live-pruned=" <> show prunedMatches <> " matches/" <> show prunedNodes <> " nodes in " <> show prunedMs <> "ms"
                  <> " allocating " <> show prunedAllocatedBytes <> "B"
                  <> " (" <> show prunedEquivalentPairs <> " checked rewrites)"
                  <> " | plain=" <> show plainMatches <> " matches/" <> show plainNodes <> " nodes in " <> show plainMs <> "ms"
                  <> " allocating " <> show plainAllocatedBytes <> "B"
                  <> " (" <> show plainEquivalentPairs <> " checked rewrites)"
                  <> " | speedup=" <> show speedup <> "x"
                  <> " | tolerance=" <> show timingToleranceMs <> "ms"
          assertBool (report <> " — live-pruned backend should produce <= plain matches") (prunedMatches <= plainMatches)
          assertBool (report <> " — RTS allocation telemetry should be enabled for this memory-pressure fixture") (prunedAllocatedBytes > 0 && plainAllocatedBytes > 0)
          assertBool (report <> " — live-pruned backend should not be materially slower than plain generic join") (prunedMs <= plainMs + timingToleranceMs)
          assertBool (report <> " — live-pruned: matches applied") (prunedMatches > 0)
          assertBool (report <> " — plain: matches applied") (plainMatches > 0)
          assertEqual "live-pruned: every structural pair rewrites to its left root" (length expectedEquivalences) prunedEquivalentPairs
          assertEqual "plain: every structural pair rewrites to its left root" (length expectedEquivalences) plainEquivalentPairs
          let plainGraph = saturationReportBaseGraph plainReport
              prunedGraph = saturationReportBaseGraph prunedReport
          forM_ expectedEquivalences $ \(ordinal, pairClass, leftRoot) -> do
            assertBool
              ("plain: structural pair #" <> show ordinal <> " rewrites to its left root | " <> report)
              (classesEquivalent plainGraph pairClass leftRoot)
            assertBool
              ("live-pruned: structural pair #" <> show ordinal <> " rewrites to its left root | " <> report)
              (classesEquivalent prunedGraph pairClass leftRoot),
      testCase "gate algebra at scale: 10 seeds, 4 critical, 6 non-critical" $
        let criticalNodes = Set.fromList (fmap RegionNodeId [0, 1, 2, 3])
            nonCriticalNodes = Set.fromList (fmap RegionNodeId [4, 5, 6, 7, 8, 9])
            gates = buildPruningGates [MicrosupportNonCritical nonCriticalNodes]
            mkSeed n = mkCandidateRegionSeed (ClassId n) (RegionNodeId n) 0
            criticalSeeds = fmap mkSeed [0 .. 3]
            nonCriticalSeeds = fmap mkSeed [4 .. 9]
            seedAllowed = pruningDecisionAllowed . cpgSeedDecision gates
            allSeeds = criticalSeeds ++ nonCriticalSeeds
            survivingSeeds = filter seedAllowed allSeeds
         in do
              compiledQuery <- expectRight (compileQuery (PatternVar queryVar0))
              let request = mkMatchingRequest compiledQuery
                  queryPattern = PatternVar queryVar0
                  materializedRegions =
                    mapMaybe
                      (materializeSeedWithPruning gates (seedInterpreterFrom allSeeds) request queryPattern)
                      allSeeds
              assertBool
                "all 4 critical seeds pass the gate"
                (all seedAllowed criticalSeeds)
              assertBool
                "all 6 non-critical seeds are pruned"
                (not (any seedAllowed nonCriticalSeeds))
              assertEqual
                "exactly 4 of 10 seeds survive"
                4
                (length survivingSeeds)
              assertEqual
                "the surviving seed frontier is exactly the critical microsupport"
                criticalNodes
                (Set.fromList (fmap crsNodeId survivingSeeds))
              assertEqual
                "only the 4 critical seeds are materialized into e-graph regions"
                4
                (length materializedRegions)
    ]

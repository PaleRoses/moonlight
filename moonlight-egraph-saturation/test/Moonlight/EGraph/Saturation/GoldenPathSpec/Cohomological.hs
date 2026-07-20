module Moonlight.EGraph.Saturation.GoldenPathSpec.Cohomological
  ( tests,
  )
where

import Moonlight.Saturation.Matching qualified as GenericMatching
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.EGraph.Saturation.GoldenPathSpec.Prelude
import Moonlight.EGraph.Saturation.Cohomological.Backend.Matching
  ( cohomologicalMatchingAlgebra,
  )
import Moonlight.EGraph.Saturation.Cohomological.Types
  ( PatternOccurrence (..),
    cachePolicyFromEnvironmentFingerprint,
  )
import Moonlight.Sheaf.Obstruction
  ( CandidateRegion (crRoot),
    CandidateStalk (..),
    OccurrenceId (..),
    emptyCapabilityLabelAlgebra,
    emptyTypedCapabilityEnvironment,
    mkSectionCertificationAlgebraWithCachePolicy,
    regionCarrierPlanFromList,
  )
import Moonlight.Saturation.Obstruction.Cohomological.Aggregate
  ( RequestAggregateSummary (rasRootResolutions),
    foldRootResolution,
  )
import Moonlight.Saturation.Obstruction.Cohomological.LivePruning
  ( LivePruningState (lpsRequests),
  )

tests :: TestTree
tests =
  testGroup
    "CohomologicalSemantics"
    [ testCase "empty stalk ⟹ root excluded: vanishing local section at an occurrence obstructs the region" $
        withTwoTestTerms (litTerm 1) (litTerm 2) $ \leftRoot rightRoot graph2 ->
          let context =
                mkSectionCertificationAlgebraWithCachePolicy
                (emptyTypedCapabilityEnvironment emptyCapabilityLabelAlgebra)
                ( \patternValue ->
                    [ PatternOccurrence
                        { poId = OccurrenceId 0,
                          poPath = [],
                          poPattern = patternValue,
                          poBoundVariable = Just queryVar0
                        }
                    ]
                )
                (\_ _ -> regionCarrierPlanFromList [mkFineRegion leftRoot 501, mkFineRegion rightRoot 502])
                (\_ _ _ -> [])
                ( \_ _ regionValue ->
                    if crRoot regionValue == leftRoot
                      then CandidateStalk IntSet.empty
                      else CandidateStalk (IntSet.singleton (classIdKey rightRoot))
                )
                (\_ _ _ -> CandidateStalk (IntSet.singleton (classIdKey rightRoot)))
                (const 51)
                (\request -> cachePolicyFromEnvironmentFingerprint (GenericMatching.qrSite request) Nothing)
              backend = mkExactWitnessBackend context
              algebra = cohomologicalMatchingAlgebra backend
         in do
              compiledQuery <- expectRight (compileQuery (PatternVar queryVar0))
              let request = mkMatchingRequest compiledQuery
                  world = mkMatchingWorld graph2
              (_, matches) <- expectRight (runFullMatchingQuery algebra (GenericMatching.maInitialState algebra) world request)
              let matchedRoots = Set.fromList (fmap fst matches)
              assertBool
                "leftRoot must be excluded (empty stalk = degenerate H¹ obstruction)"
                (not (Set.member leftRoot matchedRoots))
              assertBool
                "rightRoot must be included (non-empty stalk = consistent section)"
                (Set.member rightRoot matchedRoots),
      testCase "non-empty stalk ⟹ root included: consistent local sections produce a match" $
        withTwoTestTerms (litTerm 1) (litTerm 2) $ \leftRoot rightRoot graph2 ->
          let context = propertyContext leftRoot rightRoot
              backend = mkExactWitnessBackend context
              algebra = cohomologicalMatchingAlgebra backend
         in do
              compiledQuery <- expectRight (compileQuery (PatternVar queryVar0))
              let request = mkMatchingRequest compiledQuery
                  world = mkMatchingWorld graph2
              (_, matches) <- expectRight (runFullMatchingQuery algebra (GenericMatching.maInitialState algebra) world request)
              let matchedRoots = Set.fromList (fmap fst matches)
              assertBool
                "leftRoot must be included (non-empty stalk at all occurrences)"
                (Set.member leftRoot matchedRoots)
              assertBool
                "rightRoot must be included (non-empty stalk at all occurrences)"
                (Set.member rightRoot matchedRoots),
      testCase "cohomological ⊆ fallback: sheaf filtering only removes, never adds matches" $
        withTwoTestTerms (litTerm 1) (litTerm 2) $ \leftRoot rightRoot graph2 ->
          let context =
                mkSectionCertificationAlgebraWithCachePolicy
                (emptyTypedCapabilityEnvironment emptyCapabilityLabelAlgebra)
                ( \patternValue ->
                    [ PatternOccurrence
                        { poId = OccurrenceId 0,
                          poPath = [],
                          poPattern = patternValue,
                          poBoundVariable = Just queryVar0
                        }
                    ]
                )
                (\_ _ -> regionCarrierPlanFromList [mkFineRegion leftRoot 601, mkFineRegion rightRoot 602])
                (\_ _ _ -> [])
                ( \_ _ regionValue ->
                    CandidateStalk (IntSet.singleton (classIdKey (crRoot regionValue)))
                )
                ( \_ _ _ ->
                    CandidateStalk (IntSet.fromList [classIdKey leftRoot, classIdKey rightRoot])
                )
                (const 61)
                (\request -> cachePolicyFromEnvironmentFingerprint (GenericMatching.qrSite request) Nothing)
              backend = mkExactWitnessBackend context
              algebra = cohomologicalMatchingAlgebra backend
         in do
              compiledQuery <- expectRight (compileQuery (PatternVar queryVar0))
              let request = mkMatchingRequest compiledQuery
                  world = mkMatchingWorld graph2
              (_, cohomMatches) <- expectRight (runFullMatchingQuery algebra (GenericMatching.maInitialState algebra) world request)
              let cohomRoots = Set.fromList (fmap fst cohomMatches)
                  fallbackRootsResult =
                    fmap (Set.fromList . fmap fst) (wcojMatchCompiledWithRoots compiledQuery graph2)
              fallbackRoots <- expectRight fallbackRootsResult
              assertBool
                "cohomological roots must be a subset of fallback roots"
                (Set.isSubsetOf cohomRoots fallbackRoots),
      testCase "obstruction witness: empty stalk region caches an obstructed summary, consistent region caches a feasible summary" $
        withTwoTestTerms (litTerm 1) (litTerm 2) $ \leftRoot rightRoot graph2 ->
          let context =
                mkSectionCertificationAlgebraWithCachePolicy
                (emptyTypedCapabilityEnvironment emptyCapabilityLabelAlgebra)
                ( \patternValue ->
                    [ PatternOccurrence
                        { poId = OccurrenceId 0,
                          poPath = [],
                          poPattern = patternValue,
                          poBoundVariable = Just queryVar0
                        }
                    ]
                )
                (\_ _ -> regionCarrierPlanFromList [mkFineRegion leftRoot 801, mkFineRegion rightRoot 802])
                (\_ _ _ -> [])
                ( \_ _ regionValue ->
                    if crRoot regionValue == leftRoot
                      then CandidateStalk IntSet.empty
                      else CandidateStalk (IntSet.singleton (classIdKey rightRoot))
                )
                (\_ _ _ -> CandidateStalk (IntSet.singleton (classIdKey rightRoot)))
                (const 81)
                (\request -> cachePolicyFromEnvironmentFingerprint (GenericMatching.qrSite request) Nothing)
              backend = mkExactWitnessBackend context
              algebra = cohomologicalMatchingAlgebra backend
              classifyResolution =
                foldRootResolution
                  (const "obstructed")
                  (const "resolved-exact")
                  (const "infeasible")
                  (\_ _ -> "unresolved")
           in do
                compiledQuery <- expectRight (compileQuery (PatternVar queryVar0))
                let request = mkMatchingRequest compiledQuery
                    world = mkMatchingWorld graph2
                (updatedCache, _) <-
                  expectRight (runFullMatchingQuery algebra (GenericMatching.maInitialState algebra) world request)
                let verdictClasses =
                      [ classifyResolution rootResolution
                      | requestState <- Map.elems (lpsRequests updatedCache),
                        rootResolution <- Map.elems (rasRootResolutions requestState)
                      ]
                assertBool
                  "live-pruning state must contain at least one obstructed root summary"
                  ("obstructed" `elem` verdictClasses)
                assertBool
                  "live-pruning state must contain at least one exact-resolved root summary"
                  ("resolved-exact" `elem` verdictClasses),
      testCase "idempotence: repeated queries on an unchanged graph produce identical matches" $
        withTwoTestTerms (litTerm 1) (litTerm 2) $ \leftRoot rightRoot graph2 ->
          let context =
                mkSectionCertificationAlgebraWithCachePolicy
                (emptyTypedCapabilityEnvironment emptyCapabilityLabelAlgebra)
                ( \patternValue ->
                    [ PatternOccurrence
                        { poId = OccurrenceId 0,
                          poPath = [],
                          poPattern = patternValue,
                          poBoundVariable = Just queryVar0
                        }
                    ]
                )
                (\_ _ -> regionCarrierPlanFromList [mkFineRegion leftRoot 701, mkFineRegion rightRoot 702])
                (\_ _ _ -> [])
                ( \_ _ regionValue ->
                    if crRoot regionValue == leftRoot
                      then CandidateStalk IntSet.empty
                      else CandidateStalk (IntSet.singleton (classIdKey rightRoot))
                )
                (\_ _ _ -> CandidateStalk (IntSet.singleton (classIdKey rightRoot)))
                (const 71)
                (\request -> cachePolicyFromEnvironmentFingerprint (GenericMatching.qrSite request) Nothing)
              backend = mkExactWitnessBackend context
              algebra = cohomologicalMatchingAlgebra backend
         in do
              compiledQuery <- expectRight (compileQuery (PatternVar queryVar0))
              let request = mkMatchingRequest compiledQuery
                  world = mkMatchingWorld graph2
              (cacheAfterFirst, firstMatches) <-
                expectRight (runFullMatchingQuery algebra (GenericMatching.maInitialState algebra) world request)
              (_, secondMatches) <-
                expectRight (runFullMatchingQuery algebra cacheAfterFirst world request)
              firstMatches @?= secondMatches
    ]

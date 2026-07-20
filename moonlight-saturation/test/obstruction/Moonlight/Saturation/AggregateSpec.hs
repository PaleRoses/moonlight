module Moonlight.Saturation.AggregateSpec
  ( aggregateTests,
  )
where

import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Saturation.Obstruction.Cohomological.Aggregate
  ( RequestAggregateProjection (..),
    RequestAggregateSummary (..),
    RootFallbackDecision (..),
    RootObservation (..),
    emptyRequestAggregateSummary,
    mergeRequestAggregateSummaries,
    requestAggregateSupportRoots,
    rootFallbackDecision,
    rootEvidenceObservations,
    rootResolutionCoverage,
    rootResolutionEvidence,
    rootResolutionFromRegionOutcome,
    rootResolutionFromRegionTraversal,
    rootResolvedExact,
    rootsSupportedByKeys,
    insertRequestAggregateSummaryWith,
  )
import Moonlight.Saturation.Test.ObstructionFixture
  ( AggregateLookupFixture (..),
    AggregateLookupProfile (DenseImpactedKeys),
    AggregateMergeFixture (..),
    AggregateReplacementProfile (HalfReplacement),
    aggregateLookupFixture,
    aggregateMergeFixture,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Section.Region
  ( RegionAnalysisOutcome (..),
    RegionExactness (..),
    RegionTraversalSummary (..),
    mkRegionExactCoverage,
    recCoverage,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Section
  ( SectionCoverage (..),
    SectionFeasibilityFailure (..),
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( (@?=),
    testCase,
  )

data ProbeSummary = ProbeSummary
  { psRoot :: !Int,
    psSupportKeys :: !IntSet.IntSet,
    psCoverage :: !(Set.Set Int)
  }
  deriving stock (Eq, Show)

probeProjection :: RequestAggregateProjection ProbeSummary Int () (Set.Set Int)
probeProjection =
  RequestAggregateProjection
    { rapRoot = psRoot,
      rapSupportKeys = psSupportKeys,
      rapResolution = rootResolvedExact . psCoverage
    }

aggregateTests :: TestTree
aggregateTests =
  testGroup
    "request aggregate"
    [ testCase "region outcome resolution preserves exact fallback semantics" $
        let decisionFor ::
              RegionAnalysisOutcome String String () ->
              [Int] ->
              RootFallbackDecision
            decisionFor outcome coverage =
              rootFallbackDecision
                (rootResolutionFromRegionOutcome (not . null) outcome coverage)
            truncatedResolution =
              rootResolutionFromRegionOutcome (not . null) (RegionAnalysisTruncated "partial") ([] :: [Int])
         in do
              decisionFor (RegionAnalysisExact ExactCoverageFeasible) [1]
                @?= FallbackResolvedExactMatch
              decisionFor (RegionAnalysisExact (ExactCoverageInfeasible EmptySupport)) []
                @?= FallbackInfeasible
              decisionFor (RegionAnalysisExact (ExactCoverageInfeasible (CoverageGap "missing"))) []
                @?= FallbackInfeasible
              decisionFor (RegionAnalysisExact ExactCoverageSkipped) []
                @?= FallbackUnresolved
              decisionFor (RegionAnalysisTruncated "partial") []
                @?= FallbackUnresolved
              rootEvidenceObservations (rootResolutionEvidence truncatedResolution)
                @?= [ExactTruncatedObserved "partial"]
              decisionFor (RegionAnalysisPruned ()) []
                @?= FallbackObstructed
              decisionFor (RegionAnalysisObstructed "certified") []
                @?= FallbackObstructed,
      testCase "region traversal projection resolves through selected coverage" $
        let traversalSummary ::
              RegionTraversalSummary () Int String () ()
            traversalSummary =
              RegionTraversalSummary
                { rtsRegion = (),
                  rtsOutcome = RegionAnalysisExact ExactCoverageFeasible,
                  rtsCoverage =
                    mkRegionExactCoverage
                      ExactCoverageFeasible
                      SectionCoverage
                        { scMatches = [7],
                          scLoweringGaps = []
                        },
                  rtsMeasures = []
                }
         in rootFallbackDecision
              ( rootResolutionFromRegionTraversal
                  (not . null . scMatches)
                  recCoverage
                  traversalSummary
              )
              @?= FallbackResolvedExactMatch,
      testCase "projection insertion updates support roots incrementally" $
        let aggregate =
              insertRequestAggregateSummaryWith
                probeProjection
                ProbeSummary
                  { psRoot = 1,
                    psSupportKeys = IntSet.fromList [11],
                    psCoverage = Set.singleton 2
                  }
                ( insertRequestAggregateSummaryWith
                    probeProjection
                    ProbeSummary
                      { psRoot = 1,
                        psSupportKeys = IntSet.fromList [10],
                        psCoverage = Set.singleton 1
                      }
                    emptyRequestAggregateSummary
                )
         in do
              fmap rootResolutionCoverage (Map.lookup 1 (rasRootResolutions aggregate))
                @?= Just (Set.fromList [1, 2])
              rootsSupportedByKeys (IntSet.fromList [10]) aggregate
                @?= Set.singleton 1
              rootsSupportedByKeys (IntSet.fromList [11]) aggregate
                @?= Set.singleton 1,
      testCase "dirty merge updates inverse support index like a full rebuild" $
        let priorSupport =
              Map.fromList
                [ (1, IntSet.fromList [10, 11]),
                  (2, IntSet.fromList [20]),
                  (3, IntSet.fromList [30])
                ]
            prior :: RequestAggregateSummary Int () String
            prior =
              RequestAggregateSummary
                { rasRootResolutions =
                    Map.fromList
                      [ (1, rootResolvedExact "old-1"),
                        (2, rootResolvedExact "old-2"),
                        (3, rootResolvedExact "old-3")
                      ],
                  rasRootSupport = priorSupport,
                  rasSupportRoots = requestAggregateSupportRoots priorSupport
                }
            updatedSupport =
              Map.fromList
                [ (2, IntSet.fromList [21]),
                  (4, IntSet.fromList [40])
                ]
            updated :: RequestAggregateSummary Int () String
            updated =
              RequestAggregateSummary
                { rasRootResolutions =
                    Map.fromList
                      [ (2, rootResolvedExact "new-2"),
                        (4, rootResolvedExact "new-4")
                      ],
                  rasRootSupport = updatedSupport,
                  rasSupportRoots = requestAggregateSupportRoots updatedSupport
                }
            affectedRoots =
              Set.fromList [2, 3]
            expectedRootSupport =
              updatedSupport <> Map.withoutKeys priorSupport affectedRoots
            expected :: RequestAggregateSummary Int () String
            expected =
              RequestAggregateSummary
                { rasRootResolutions =
                    rasRootResolutions updated
                      <> Map.withoutKeys (rasRootResolutions prior) affectedRoots,
                  rasRootSupport = expectedRootSupport,
                  rasSupportRoots = requestAggregateSupportRoots expectedRootSupport
                }
            merged =
              mergeRequestAggregateSummaries affectedRoots prior updated
         in do
              merged @?= expected
              rootsSupportedByKeys (IntSet.fromList [20, 30]) merged
                @?= Set.empty
              rootsSupportedByKeys (IntSet.fromList [21]) merged
                @?= Set.singleton 2
              rootsSupportedByKeys (IntSet.fromList [40]) merged
                @?= Set.singleton 4,
      testCase "scaled aggregate fixtures preserve full-rebuild and inverse-index oracles" $
        let mergeFixture = aggregateMergeFixture HalfReplacement 64
            lookupFixture = aggregateLookupFixture DenseImpactedKeys 64
         in do
              mergeRequestAggregateSummaries
                (aggregateMergeAffectedRoots mergeFixture)
                (aggregateMergePrior mergeFixture)
                (aggregateMergeUpdated mergeFixture)
                @?= aggregateMergeExpected mergeFixture
              rootsSupportedByKeys
                (aggregateLookupImpactedKeys lookupFixture)
                (aggregateLookupSummary lookupFixture)
                @?= aggregateLookupExpectedRoots lookupFixture
    ]

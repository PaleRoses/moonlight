module Moonlight.Control.PerturbationSpec
  ( perturbationTests,
  )
where

import Data.Function ((&))
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Moonlight.Homology.Topology (Graph1Skeleton, graphFromEdgeSupports)
import Moonlight.Control.Schedule
  ( ScheduleOrder (BackoffByGroup),
    SchedulerConfig (..),
    TracePolicy (TraceAll),
    backoffConfig,
    defaultSchedulerConfig,
  )
import Moonlight.Control.Weight (emptyPriorityProfile)
import Moonlight.Control.Scheduling.Perturbation
  ( MicrolocalMerge,
    PerturbationSample (..),
    influenceEdgeSupports,
    microlocalInvalidationNeighborhood,
    microlocalSpectralInvalidation,
    microlocalSpectralRefreshRequired,
    mkMicrolocalMerge,
    perturbationSample,
    perturbationSampleWithMicrolocalGate,
    sampleObservedEdgeCoverage,
  )
import Moonlight.Control.Scheduling.Successor
  ( BackoffInfluenceEnvelope (..),
    InfluenceComplex (..),
    SchedulerInfluence (..),
    SuccessorComplex (..),
    SuccessorEdge (..),
    SuccessorNode (..),
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

perturbationTests :: TestTree
perturbationTests =
  testGroup
    "perturbation"
    [ testCase "perturbation shadows contract spectral gap under backoff" testPerturbationShadowGap,
      testCase "influence supports preserve the backoff envelope weight" testInfluenceSupportsPreserveBackoffWeight,
      testCase "microlocal gate skips spectral sampling for clean structural dirt" testMicrolocalGateSkipsCleanSpectral,
      testCase "microlocal gate skips spectral sampling for acyclic local dirt" testMicrolocalGateSkipsAcyclicSpectral,
      testCase "microlocal gate requires spectral sampling when a local merge closes a cycle" testMicrolocalGateRequiresCycleSpectral
    ]

testPerturbationShadowGap :: IO ()
testPerturbationShadowGap =
  case (sampleFor DeterministicInfluence, sampleFor backoffInfluence) of
    (Right deterministicSample, Right backoffSample) -> do
      assertEqual "expected one structural edge" 1 (psStructuralEdgeCount deterministicSample)
      assertEqual "expected one effective edge under deterministic weighting" 1 (psEffectiveEdgeCount deterministicSample)
      assertEqual "expected observed edge coverage to be derived from the sample" (Just 1.0) (sampleObservedEdgeCoverage deterministicSample)
      assertBool
        "expected deterministic scheduling to preserve a larger spectral gap than the backoff shadow"
        (fromMaybe (-1.0) (psSpectralGap deterministicSample) > fromMaybe (-1.0) (psSpectralGap backoffSample))
    (Left failureValue, _) ->
      assertFailure ("unexpected deterministic perturbation failure: " <> show failureValue)
    (_, Left failureValue) ->
      assertFailure ("unexpected backoff perturbation failure: " <> show failureValue)

testInfluenceSupportsPreserveBackoffWeight :: IO ()
testInfluenceSupportsPreserveBackoffWeight =
  case influenceEdgeSupports (simpleInfluenceComplex backoffSchedulerConfig backoffInfluence) (const 1.0) of
    [(_sourceIndex, _targetIndex, effectiveWeight)] ->
      assertBool
        "expected one match across a three-round envelope to retain one-third support"
        (abs (effectiveWeight - (1.0 / 3.0)) < 1.0e-12)
    supports ->
      assertFailure ("expected exactly one weighted influence support, received: " <> show supports)

testMicrolocalGateSkipsCleanSpectral :: IO ()
testMicrolocalGateSkipsCleanSpectral = do
  cleanInvalidation <-
    expectRight
      (microlocalSpectralInvalidation 2 [(0, 1, 1.0)] [])
  assertBool
    "no structural microlocal dirt should not request spectral sampling"
    (not (microlocalSpectralRefreshRequired cleanInvalidation))
  sampleValue <-
    expectRight
      ( perturbationSampleWithMicrolocalGate
          cleanInvalidation
          ()
          deterministicSchedulerConfig
          2
          1
          (Just 1)
          [(0, 1, 1.0)]
      )
  assertEqual "clean microlocal gate leaves spectral gap absent" Nothing (psSpectralGap sampleValue)
  assertBool "clean microlocal gate does not build leading modes" (null (psLeadingModes sampleValue))

testMicrolocalGateSkipsAcyclicSpectral :: IO ()
testMicrolocalGateSkipsAcyclicSpectral = do
  acyclicMerge <- expectMicrolocalMerge 1 2
  acyclicInvalidation <-
    expectRight
      (microlocalSpectralInvalidation 3 [(0, 1, 1.0)] [acyclicMerge])
  assertBool
    "local structural dirt that remains tree-shaped should not request spectral sampling"
    (not (microlocalSpectralRefreshRequired acyclicInvalidation))
  sampleValue <-
    expectRight
      ( perturbationSampleWithMicrolocalGate
          acyclicInvalidation
          ()
          deterministicSchedulerConfig
          3
          1
          (Just 1)
          [(0, 1, 1.0)]
      )
  assertEqual "acyclic microlocal gate leaves spectral gap absent" Nothing (psSpectralGap sampleValue)
  assertBool "acyclic microlocal gate does not build leading modes" (null (psLeadingModes sampleValue))

testMicrolocalGateRequiresCycleSpectral :: IO ()
testMicrolocalGateRequiresCycleSpectral = do
  cycleMerge <- expectMicrolocalMerge 0 2
  dirtyInvalidation <-
    expectRight
      (microlocalSpectralInvalidation 3 pathSupports [cycleMerge])
  assertBool
    "path endpoints already connected by a local section should request spectral sampling"
    (microlocalSpectralRefreshRequired dirtyInvalidation)
  assertEqual
    "cycle-producing local dirt records only the touched neighborhood"
    (IntSet.fromList [0, 1, 2])
    (microlocalInvalidationNeighborhood dirtyInvalidation)
  sampleValue <-
    expectRight
      ( perturbationSampleWithMicrolocalGate
          dirtyInvalidation
          ()
          deterministicSchedulerConfig
          3
          2
          (Just 2)
          pathSupports
      )
  assertBool "cycle-producing microlocal dirt computes a spectral gap" (maybe False (> 0.0) (psSpectralGap sampleValue))
  assertBool "cycle-producing microlocal dirt computes leading modes" (not (null (psLeadingModes sampleValue)))

deterministicSchedulerConfig :: SchedulerConfig Int
deterministicSchedulerConfig =
  defaultSchedulerConfig {scTracePolicy = TraceAll}

backoffSchedulerConfig :: SchedulerConfig Int
backoffSchedulerConfig =
  SchedulerConfig
    { scOrder = BackoffByGroup (backoffConfig 1 2),
      scTracePolicy = TraceAll,
      scPriorityProfile = emptyPriorityProfile
    }

backoffInfluence :: SchedulerInfluence
backoffInfluence =
  BackoffInfluence
    BackoffInfluenceEnvelope
      { bieMatchLimit = 1,
        bieCooldownRounds = 2,
        bieSharedOutgoingEdges = 1
      }

sampleFor :: SchedulerInfluence -> Either String (PerturbationSample () Int)
sampleFor influenceValue =
  perturbationSample
    ()
    schedulerConfig
    2
    1
    (Just 1)
    (influenceEdgeSupports (simpleInfluenceComplex schedulerConfig influenceValue) (const 1.0))
    & either (Left . show) Right
  where
    schedulerConfig =
      case influenceValue of
        DeterministicInfluence -> deterministicSchedulerConfig
        BackoffInfluence _ -> backoffSchedulerConfig

simpleInfluenceComplex :: SchedulerConfig Int -> SchedulerInfluence -> InfluenceComplex Int String Int Int () ()
simpleInfluenceComplex schedulerConfig influenceValue =
  InfluenceComplex
    { ricSuccessorComplex = successorComplex,
      ricSchedulerConfig = schedulerConfig,
      ricEdgeInfluences = [(successorEdge, influenceValue)],
      ricGradedObstructionClusters = []
    }

successorComplex :: SuccessorComplex String Int Int () ()
successorComplex =
  SuccessorComplex
    { rscNodes = [sourceNode, targetNode],
      rscEdges = [successorEdge],
      rscCompositionObstructions = [],
      rscNodeOrdinals = Map.fromList [(("root", 0), 0), (("root", 1), 1)],
      rscNodeIndex = Map.fromList [(("root", 0), sourceNode), (("root", 1), targetNode)],
      rscEdgeIndex = Map.fromList [((("root", 0), ("root", 1)), successorEdge)],
      rscOutgoingEdgeCounts = Map.fromList [(("root", 0), 1)],
      rscUndirectedSkeleton = undirectedSkeleton
    }

sourceNode :: SuccessorNode String Int Int
sourceNode =
  SuccessorNode
    { snContext = "root",
      snRule = 0,
      snRuntimeRuleIdentity = 0
    }

targetNode :: SuccessorNode String Int Int
targetNode =
  SuccessorNode
    { snContext = "root",
      snRule = 1,
      snRuntimeRuleIdentity = 1
    }

successorEdge :: SuccessorEdge String Int Int ()
successorEdge =
  SuccessorEdge
    { seSource = sourceNode,
      seTarget = targetNode,
      seComposite = ()
    }

undirectedSkeleton :: Graph1Skeleton
undirectedSkeleton =
  graphFromEdgeSupports 2 [(0, 1)]

pathSupports :: [(Int, Int, Double)]
pathSupports =
  [(0, 1, 1.0), (1, 2, 1.0)]

expectMicrolocalMerge :: Int -> Int -> IO MicrolocalMerge
expectMicrolocalMerge leftCell rightCell =
  case mkMicrolocalMerge leftCell rightCell of
    Just mergeValue -> pure mergeValue
    Nothing ->
      assertFailure
        ("expected non-negative microlocal merge endpoints: " <> show (leftCell, rightCell))

expectRight :: Show errorValue => Either errorValue value -> IO value
expectRight result =
  case result of
    Right value -> pure value
    Left errorValue -> assertFailure ("unexpected Left: " <> show errorValue)

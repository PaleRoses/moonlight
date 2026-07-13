module Moonlight.Control.SuccessorSpec
  ( successorTests,
  )
where

import Algebra.Graph.AdjacencyMap qualified as AdjacencyMap
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Control.Schedule
  ( ScheduleOrder (BackoffByGroup),
    SchedulerConfig (..),
    TracePolicy (TraceAll),
    backoffConfig,
    defaultSchedulerConfig,
  )
import Moonlight.Control.Weight
  ( comparePriorityEvidence,
    emptyPriorityProfile,
    lookupPriorityEvidence,
    nonCriticalPriorityRank,
    priorityEvidence,
  )
import Moonlight.Control.Scheduling.Successor
  ( BackoffInfluenceEnvelope (..),
    GradedObstructionCluster (..),
    InfluenceComplex (..),
    SchedulerInfluence (..),
    SuccessorAlgebra (..),
    SuccessorCompositionObstruction (..),
    SuccessorComplex (..),
    SuccessorEdge (..),
    SuccessorNode (..),
    buildInfluenceComplex,
    buildSuccessorComplex,
    findSuccessorEdge,
    findSuccessorNode,
    influenceFromSuccessorComplex,
    successorAdjacencyMap,
    successorInfluencePriorityObservation,
  )
import Moonlight.Control.Scheduling.Tower (spectralSchedulingPriorityObservation)
import Moonlight.Control.Scheduling.Successor.Runtime
  ( RuleRuntimeProjection (..),
    RuntimeInfluenceEvidence (..),
    RuntimeAnnotatedSuccessorComplex,
    RuntimeWeightedEdge (..),
    runtimeAnnotatedSuccessorComplexWithProjection,
    runtimeTransitionPriorityObservation,
    runtimeWeightedEdges,
  )
import Moonlight.Homology (HomologicalDegree (..))
import Moonlight.Homology.Topology (Graph1Skeleton, GraphEdge (..), graphEdges, graphFromEdgeSupports)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)

data TestContext = TestContext
  deriving stock (Eq, Ord, Show)

data TestRule = TestRule
  { trKey :: Int,
    trRuntime :: Int
  }
  deriving stock (Show)

instance Eq TestRule where
  leftRule == rightRule =
    trKey leftRule == trKey rightRule

instance Ord TestRule where
  compare leftRule rightRule =
    compare (trKey leftRule) (trKey rightRule)

data TestCompositionObstruction
  = NotComposable
  | NoStructuralSuccessor
  deriving stock (Eq, Show)

data RuntimeNode = RuntimeNode
  { rnRule :: !Int
  }
  deriving stock (Eq, Ord, Show)

data RuntimeEdge = RuntimeEdge
  { reSource :: !RuntimeNode,
    reTarget :: !RuntimeNode
  }
  deriving stock (Eq, Ord, Show)

data RuntimeOutcome = RuntimeOutcome
  { roRule :: !Int
  }
  deriving stock (Eq, Ord, Show)

data RuntimeTransition = RuntimeTransition
  { rtSourceRule :: !Int,
    rtTargetRule :: !Int,
    rtObservedCount :: !Int
  }
  deriving stock (Eq, Ord, Show)

successorTests :: TestTree
successorTests =
  testGroup
    "successor"
    [ testCase "deduplicates nodes by context and rule key" testKeyedNodeDedupe,
      testCase "deduplicates edges by endpoint keys" testKeyedEdgeDedupe,
      testCase "records typed composition obstructions" testTypedCompositionObstructions,
      testCase "uses candidate target index before attempting composition" testCandidateTargetIndexPrunesCompositionAttempts,
      testCase "indexes canonical nodes and edges by successor key" testCanonicalLookupIndex,
      testCase "indexes edge supports from canonical nodes" testKeyedEdgeSupportIndex,
      testCase "preserves empty and isolated graph skeletons" testEmptyAndIsolatedSkeletons,
      testCase "exports directed successor adjacency over canonical keys" testSuccessorAdjacencyMap,
      testCase "overlays scheduler influence from a cached successor carrier" testInfluenceFromCachedSuccessorComplex,
      testCase "counts outgoing influence from deduplicated edges" testDeduplicatedOutgoingInfluence,
      testCase "successor influence observation contributes structural evidence to source rules" testSuccessorInfluencePriorityObservation,
      testCase "tower influence keeps pass, norm, and width on distinct evidence axes" testTowerInfluencePriorityEvidenceAxes,
      testCase "runtime transition observation contributes observed transition evidence to source rules" testRuntimeTransitionPriorityObservation,
      testCase "runtime annotation aggregates duplicate rule keys without overwriting" testRuntimeAnnotationAggregatesDuplicateKeys
    ]

testKeyedNodeDedupe :: IO ()
testKeyedNodeDedupe =
  assertEqual
    "expected duplicate rule keys to keep the first runtime identity"
    [10, 20, 30]
    (fmap snRuntimeRuleIdentity (rscNodes duplicateSuccessorComplex))

testKeyedEdgeDedupe :: IO ()
testKeyedEdgeDedupe =
  assertEqual
    "expected endpoint-key edge dedupe to ignore later composite variants"
    [(20, 10), (30, 10)]
    (fmap seComposite (rscEdges duplicateSuccessorComplex))

testTypedCompositionObstructions :: IO ()
testTypedCompositionObstructions =
  assertEqual
    "expected failed local compositions to remain typed obstruction witnesses"
    [ (1, 1, 1, NotComposable),
      (2, 2, 1, NotComposable),
      (2, 2, 2, NotComposable),
      (2, 2, 3, NotComposable),
      (3, 3, 1, NotComposable),
      (3, 3, 2, NotComposable),
      (3, 3, 3, NotComposable)
    ]
    (fmap obstructionFingerprint (rscCompositionObstructions duplicateSuccessorComplex))

testCandidateTargetIndexPrunesCompositionAttempts :: IO ()
testCandidateTargetIndexPrunesCompositionAttempts = do
  assertEqual
    "expected indexed candidate targets to preserve successor edges"
    (rscEdges duplicateSuccessorComplex)
    (rscEdges indexedDuplicateSuccessorComplex)
  assertEqual
    "expected indexed candidate targets to avoid impossible composition attempts"
    []
    (rscCompositionObstructions indexedDuplicateSuccessorComplex)

testCanonicalLookupIndex :: IO ()
testCanonicalLookupIndex =
  case
    ( findSuccessorNode duplicateSuccessorComplex TestContext (TestRule 1 999),
      findSuccessorNode duplicateSuccessorComplex TestContext (TestRule 2 999)
    ) of
    (Just sourceNode, Just targetNode) -> do
      assertEqual
        "expected node lookup to return the canonical first runtime identity"
        10
        (snRuntimeRuleIdentity sourceNode)
      assertEqual
        "expected edge lookup to use successor endpoint keys"
        (Just (20, 10))
        (fmap seComposite (findSuccessorEdge duplicateSuccessorComplex sourceNode targetNode))
    _ ->
      assertFailure "expected canonical successor nodes to be indexed"

testKeyedEdgeSupportIndex :: IO ()
testKeyedEdgeSupportIndex =
  assertEqual
    "expected skeleton edge supports to reference canonical node indices"
    [(0, 0, 1), (1, 0, 2)]
    (fmap graphEdgeTuple (graphEdges (rscUndirectedSkeleton duplicateSuccessorComplex)))

testEmptyAndIsolatedSkeletons :: IO ()
testEmptyAndIsolatedSkeletons = do
  assertEqual
    "expected empty graph to have no carrier vertices"
    emptySkeleton
    (rscUndirectedSkeleton (buildSuccessorComplex isolatedAlgebra []))
  assertEqual
    "expected isolated nodes to survive with empty adjacency sets"
    isolatedSkeleton
    (rscUndirectedSkeleton (buildSuccessorComplex isolatedAlgebra isolatedRules))

testSuccessorAdjacencyMap :: IO ()
testSuccessorAdjacencyMap =
  assertEqual
    "expected directed adjacency to preserve canonical successor keys"
    duplicateDirectedAdjacency
    ( Map.fromList
        . fmap (fmap Set.fromList)
        . AdjacencyMap.adjacencyList
        $ successorAdjacencyMap duplicateSuccessorComplex
    )

testInfluenceFromCachedSuccessorComplex :: IO ()
testInfluenceFromCachedSuccessorComplex = do
  let overlay =
        influenceFromSuccessorComplex
          backoffSchedulerConfig
          duplicateSuccessorComplex
  assertEqual
    "expected cached overlay to preserve canonical successor nodes"
    (rscNodes duplicateSuccessorComplex)
    (rscNodes (ricSuccessorComplex overlay))
  assertEqual
    "expected cached overlay to preserve canonical successor edges"
    (rscEdges duplicateSuccessorComplex)
    (rscEdges (ricSuccessorComplex overlay))
  assertEqual
    "expected cached overlay to derive the same outgoing influence"
    [Just 2, Just 2]
    (fmap (sharedOutgoingEdges . snd) (ricEdgeInfluences overlay))

testDeduplicatedOutgoingInfluence :: IO ()
testDeduplicatedOutgoingInfluence =
  assertEqual
    "expected duplicate raw edges not to inflate outgoing backoff influence"
    [Just 2, Just 2]
    (fmap (sharedOutgoingEdges . snd) (ricEdgeInfluences duplicateInfluenceComplex))


testSuccessorInfluencePriorityObservation :: IO ()
testSuccessorInfluencePriorityObservation = do
  let backoffProfile =
        successorInfluencePriorityObservation
          Just
          id
          duplicateInfluenceComplex
      deterministicProfile =
        successorInfluencePriorityObservation
          Just
          id
          (influenceFromSuccessorComplex defaultSchedulerConfig duplicateSuccessorComplex)
  assertEqual
    "expected the backoff envelope to attenuate two source edges in thousandths"
    (priorityEvidence 334 0 0 nonCriticalPriorityRank)
    (lookupPriorityEvidence 10 backoffProfile)
  assertEqual
    "expected deterministic influence to retain full structural evidence per edge"
    (priorityEvidence 2000 0 0 nonCriticalPriorityRank)
    (lookupPriorityEvidence 10 deterministicProfile)
  assertEqual
    "expected target-only runtime rule to have no source structural influence"
    mempty
    (lookupPriorityEvidence 20 backoffProfile)

testTowerInfluencePriorityEvidenceAxes :: IO ()
testTowerInfluencePriorityEvidenceAxes = do
  let profile =
        spectralSchedulingPriorityObservation
          Just
          id
          towerInfluenceComplex
      lowerDegreeEvidence =
        lookupPriorityEvidence 10 profile
      widerHigherDegreeEvidence =
        lookupPriorityEvidence 20 profile
  assertEqual
    "expected pass rank to occupy the transition axis independently"
    (priorityEvidence 0 2 0 nonCriticalPriorityRank)
    lowerDegreeEvidence
  assertEqual
    "expected cocycle norm and cluster width to remain distinct"
    (priorityEvidence 1 1 2 nonCriticalPriorityRank)
    widerHigherDegreeEvidence
  assertEqual
    "expected the authoritative evidence key to prioritize the lower-degree cluster"
    LT
    (comparePriorityEvidence lowerDegreeEvidence widerHigherDegreeEvidence)

testRuntimeTransitionPriorityObservation :: IO ()
testRuntimeTransitionPriorityObservation = do
  let profile =
        runtimeTransitionPriorityObservation
          (Just . rnRule)
          runtimeTransitionCount
          snd
          reSource
          runtimeAnnotatedComplex
  assertEqual
    "expected observed edge weight to prioritize the source rule"
    (priorityEvidence 0 3 0 nonCriticalPriorityRank)
    (lookupPriorityEvidence 1 profile)
  assertEqual
    "expected second observed edge weight to prioritize its source rule"
    (priorityEvidence 0 5 0 nonCriticalPriorityRank)
    (lookupPriorityEvidence 2 profile)
  assertEqual
    "expected unobserved source rule to have no transition priority"
    mempty
    (lookupPriorityEvidence 3 profile)

testRuntimeAnnotationAggregatesDuplicateKeys :: IO ()
testRuntimeAnnotationAggregatesDuplicateKeys = do
  let profile =
        runtimeTransitionPriorityObservation
          (Just . rnRule)
          runtimeTransitionCount
          snd
          reSource
          duplicateRuntimeAnnotatedComplex
  assertEqual
    "expected duplicate transition keys to aggregate observed counts"
    (priorityEvidence 0 10 0 nonCriticalPriorityRank)
    (lookupPriorityEvidence 1 profile)
  case runtimeWeightedEdges snd duplicateRuntimeAnnotatedComplex of
    [weightedEdge] ->
      let evidence = rweEvidence weightedEdge
       in do
            assertEqual
              "expected duplicate source outcomes to remain visible"
              [1, 1]
              (maybe [] (fmap roRule . NonEmpty.toList) (rieSourceOutcomes evidence))
            assertEqual
              "expected duplicate transitions to preserve source order"
              [3, 7]
              (fmap rtObservedCount (NonEmpty.toList (rieTransitions evidence)))
    _ ->
      assertFailure "expected exactly one duplicate-key weighted edge"

runtimeAnnotatedComplex ::
  RuntimeAnnotatedSuccessorComplex
    ([RuntimeNode], [RuntimeEdge])
    [RuntimeOutcome]
    [RuntimeTransition]
    (NonEmpty RuntimeOutcome)
    (NonEmpty RuntimeTransition)
runtimeAnnotatedComplex =
  runtimeAnnotatedSuccessorComplexWithProjection
    (RuleRuntimeProjection (Just . rnRule))
    id
    roRule
    id
    rtSourceRule
    rtTargetRule
    fst
    snd
    reSource
    reTarget
    runtimeOutcomes
    runtimeTransitions
    (runtimeNodes, runtimeEdges)

duplicateRuntimeAnnotatedComplex ::
  RuntimeAnnotatedSuccessorComplex
    ([RuntimeNode], [RuntimeEdge])
    [RuntimeOutcome]
    [RuntimeTransition]
    (NonEmpty RuntimeOutcome)
    (NonEmpty RuntimeTransition)
duplicateRuntimeAnnotatedComplex =
  runtimeAnnotatedSuccessorComplexWithProjection
    (RuleRuntimeProjection (Just . rnRule))
    id
    roRule
    id
    rtSourceRule
    rtTargetRule
    fst
    snd
    reSource
    reTarget
    [RuntimeOutcome 1, RuntimeOutcome 1, RuntimeOutcome 2]
    [RuntimeTransition 1 2 3, RuntimeTransition 1 2 7]
    ([RuntimeNode 1, RuntimeNode 2], [RuntimeEdge (RuntimeNode 1) (RuntimeNode 2)])

runtimeNodes :: [RuntimeNode]
runtimeNodes =
  fmap RuntimeNode [1, 2, 3]

runtimeEdges :: [RuntimeEdge]
runtimeEdges =
  [ RuntimeEdge (RuntimeNode 1) (RuntimeNode 2),
    RuntimeEdge (RuntimeNode 2) (RuntimeNode 1),
    RuntimeEdge (RuntimeNode 3) (RuntimeNode 1)
  ]

runtimeOutcomes :: [RuntimeOutcome]
runtimeOutcomes =
  fmap RuntimeOutcome [1, 2]

runtimeTransitions :: [RuntimeTransition]
runtimeTransitions =
  [ RuntimeTransition 1 2 3,
    RuntimeTransition 2 1 5
  ]

runtimeTransitionCount :: NonEmpty RuntimeTransition -> Int
runtimeTransitionCount =
  sum . fmap rtObservedCount

duplicateSuccessorComplex :: SuccessorComplex TestContext TestRule Int (Int, Int) TestCompositionObstruction
duplicateSuccessorComplex =
  buildSuccessorComplex duplicateAlgebra duplicateRules

indexedDuplicateSuccessorComplex :: SuccessorComplex TestContext TestRule Int (Int, Int) TestCompositionObstruction
indexedDuplicateSuccessorComplex =
  buildSuccessorComplex indexedDuplicateAlgebra duplicateRules

duplicateInfluenceComplex :: InfluenceComplex Int TestContext TestRule Int (Int, Int) TestCompositionObstruction
duplicateInfluenceComplex =
  buildInfluenceComplex backoffSchedulerConfig duplicateAlgebra duplicateRules

towerInfluenceComplex :: InfluenceComplex Int TestContext TestRule Int (Int, Int) TestCompositionObstruction
towerInfluenceComplex =
  duplicateInfluenceComplex
    { ricGradedObstructionClusters =
        [ GradedObstructionCluster
            { gocDegree = HomologicalDegree 1,
              gocRules = [10],
              gocCocycleNorm = 0
            },
          GradedObstructionCluster
            { gocDegree = HomologicalDegree 2,
              gocRules = [20, 30],
              gocCocycleNorm = 2
            }
        ]
    }

duplicateRules :: [TestRule]
duplicateRules =
  [ TestRule {trKey = 1, trRuntime = 10},
    TestRule {trKey = 1, trRuntime = 99},
    TestRule {trKey = 2, trRuntime = 20},
    TestRule {trKey = 3, trRuntime = 30}
  ]

isolatedRules :: [TestRule]
isolatedRules =
  [ TestRule {trKey = 4, trRuntime = 40},
    TestRule {trKey = 5, trRuntime = 50}
  ]

duplicateAlgebra :: SuccessorAlgebra [TestRule] TestContext TestRule Int (Int, Int) TestCompositionObstruction
duplicateAlgebra =
  baseAlgebra
    { saComposeRules = composeDuplicateRule
    }

indexedDuplicateAlgebra :: SuccessorAlgebra [TestRule] TestContext TestRule Int (Int, Int) TestCompositionObstruction
indexedDuplicateAlgebra =
  baseAlgebra
    { saCandidateTargetRules = indexedDuplicateCandidates,
      saComposeRules = composeDuplicateRule
    }

isolatedAlgebra :: SuccessorAlgebra [TestRule] TestContext TestRule Int (Int, Int) TestCompositionObstruction
isolatedAlgebra =
  baseAlgebra
    { saComposeRules = \_ _ _ _ -> Left NoStructuralSuccessor
    }

baseAlgebra :: SuccessorAlgebra [TestRule] TestContext TestRule Int (Int, Int) TestCompositionObstruction
baseAlgebra =
  SuccessorAlgebra
    { saContexts = const [TestContext],
      saContextLeq = \_ leftContext rightContext -> leftContext == rightContext,
      saRulesInContext = const,
      saCandidateTargetRules = \_ _ targetRules _ -> targetRules,
      saRestrictRule = \_ _ _ ruleValue -> Just ruleValue,
      saComposeRules = composeDuplicateRule,
      saRuntimeRule = \_ ruleValue -> trRuntime ruleValue
    }

indexedDuplicateCandidates :: [TestRule] -> TestContext -> [TestRule] -> TestRule -> [TestRule]
indexedDuplicateCandidates _ _ targetRules restrictedSourceRule =
  targetRules
    & filter
      ( \targetRule ->
          trKey restrictedSourceRule == 1
            && trKey targetRule `elem` [2, 3]
      )

composeDuplicateRule :: [TestRule] -> TestContext -> TestRule -> TestRule -> Either TestCompositionObstruction (Int, Int)
composeDuplicateRule _ _ targetRule sourceRule
  | trKey sourceRule == 1 && trKey targetRule `elem` [2, 3] =
      Right (trRuntime targetRule, trRuntime sourceRule)
  | otherwise = Left NotComposable

backoffSchedulerConfig :: SchedulerConfig Int
backoffSchedulerConfig =
  SchedulerConfig
    { scOrder = BackoffByGroup (backoffConfig 1 2),
      scTracePolicy = TraceAll,
      scPriorityProfile = emptyPriorityProfile
    }

graphEdgeTuple :: GraphEdge -> (Int, Int, Int)
graphEdgeTuple edgeValue =
  (graphEdgeIndex edgeValue, graphEdgeSource edgeValue, graphEdgeTarget edgeValue)

sharedOutgoingEdges :: SchedulerInfluence -> Maybe Int
sharedOutgoingEdges influenceValue =
  case influenceValue of
    DeterministicInfluence -> Nothing
    BackoffInfluence envelopeValue -> Just (bieSharedOutgoingEdges envelopeValue)

obstructionFingerprint :: SuccessorCompositionObstruction TestContext TestRule TestCompositionObstruction -> (Int, Int, Int, TestCompositionObstruction)
obstructionFingerprint obstructionValue =
  ( trKey (scoSourceRule obstructionValue),
    trKey (scoRestrictedSourceRule obstructionValue),
    trKey (scoTargetRule obstructionValue),
    scoCompositionObstruction obstructionValue
  )

emptySkeleton :: Graph1Skeleton
emptySkeleton =
  graphFromEdgeSupports 0 []

isolatedSkeleton :: Graph1Skeleton
isolatedSkeleton =
  graphFromEdgeSupports 2 []

duplicateDirectedAdjacency :: Map.Map (TestContext, TestRule) (Set.Set (TestContext, TestRule))
duplicateDirectedAdjacency =
  Map.fromList
    [ (ruleKey 1 10, Set.fromList [ruleKey 2 20, ruleKey 3 30]),
      (ruleKey 2 20, Set.empty),
      (ruleKey 3 30, Set.empty)
    ]

ruleKey :: Int -> Int -> (TestContext, TestRule)
ruleKey key runtime =
  (TestContext, TestRule {trKey = key, trRuntime = runtime})

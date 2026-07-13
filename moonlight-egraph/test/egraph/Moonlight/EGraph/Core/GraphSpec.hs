module Moonlight.EGraph.Core.GraphSpec
  ( tests,
  )
where

import Moonlight.EGraph.Effect.CoveringSurface (SurfaceKind)
import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Core (UnionFindAllocationError)
import Moonlight.EGraph.Pure.Context (contextMerge)
import Moonlight.EGraph.Pure.Context.Core
  ( ContextEGraph,
    requireCachedContextPayloadFor,
  )
import Moonlight.EGraph.Pure.Analysis (AnalysisSpec (..))
import Moonlight.EGraph.Pure.Kernel.HashCons (addTerm)
import Moonlight.EGraph.Pure.Rebuild (merge, rebuild)
import Moonlight.EGraph.Pure.Saturation.Rebuild.Internal
  ( RoundRebuildReport (rrrGraph, rrrRebuildDelta),
    runRoundRebuildReport,
  )
import Moonlight.EGraph.Pure.Types
  ( ClassId,
    canonicalizeClassId,
    classIdKey,
    eGraphAnalysis,
    emptyEGraph,
  )
import Moonlight.EGraph.Pure.Types.Internal
  ( EGraph (..),
    bumpEGraphRevision,
  )
import Moonlight.EGraph.Saturation.Context.State
  ( emptySaturatingContextEGraph,
    sceContextGraph,
  )
import Moonlight.EGraph.Test.Arith.Core (ArithF (..), NodeCount)
import Moonlight.EGraph.Test.Arith.Fixture
  ( classOfArith,
    five,
    four,
    one,
    onePlusFour,
    onePlusTwo,
    seedArithTerms,
    three,
    threePlusFour,
    two,
  )
import Moonlight.EGraph.Test.Case (HUnitCase (..), hunitCases)
import Moonlight.EGraph.Test.Context.ThreeLevel (Scope (ModuleCtx))
import Moonlight.EGraph.Test.Context.ThreeLevelArith (moduleContextGraph)
import Moonlight.Graph.Pure.LocalTopology (cyclicCellsFromChildrenInt)
import Moonlight.Sheaf.Context.Algebra (contextClassAt)
import Moonlight.Pale.Test.Site.Assertion (withResult)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, (@?=))

tests :: TestTree
tests =
  testGroup
    "Rebuild.Graph"
    [ tarjanTests,
      ordinaryRebuildAnalysisTests,
      roundRebuildLocalMergeTests
    ]

ordinaryRebuildAnalysisTests :: TestTree
ordinaryRebuildAnalysisTests =
  testGroup "Ordinary rebuild analysis" . hunitCases $
    [ HUnitCase "canonical union roots preserve seeded evidence while joining structural recomputation" $
        let graph0 = emptyEGraph seededAnalysisSpec
         in withResult (addTerm onePlusTwo graph0) $ \(leftParent, graph1) ->
              withResult (addTerm threePlusFour graph1) $ \(rightParent, graph2) ->
                let seededGraph =
                      seedExternalEvidence rightParent 202
                        (seedExternalEvidence leftParent 101 graph2)
                    rebuiltGraph = rebuild (merge leftParent rightParent seededGraph)
                    representative = canonicalizeClassId rebuiltGraph leftParent
                 in IntMap.lookup (classIdKey representative) (eGraphAnalysis rebuiltGraph)
              @?= Just
                SeededAnalysis
                  { saStructuralObservations = Set.fromList [1, 2, 3, 4],
                    saExternalEvidence = Set.fromList [101, 202]
                  }
    ]

data SeededAnalysis = SeededAnalysis
  { saStructuralObservations :: !(Set Int),
    saExternalEvidence :: !(Set Int)
  }
  deriving stock (Eq, Show)

seededAnalysisSpec :: AnalysisSpec ArithF SeededAnalysis
seededAnalysisSpec =
  AnalysisSpec
    { asMake = \arithNode ->
        SeededAnalysis
          { saStructuralObservations = foldMap saStructuralObservations arithNode <> nodeObservation arithNode,
            saExternalEvidence = Set.empty
          },
      asJoin = joinSeededAnalysis,
      asJoinChanged = \existing incoming ->
        let joined = joinSeededAnalysis existing incoming
         in (joined, joined /= existing)
    }
  where
    nodeObservation :: ArithF a -> Set Int
    nodeObservation arithNode =
      case arithNode of
        Num value -> Set.singleton value
        Var index -> Set.singleton (negate index - 1)
        Add {} -> Set.empty
        Mul {} -> Set.empty
        Neg {} -> Set.empty

joinSeededAnalysis :: SeededAnalysis -> SeededAnalysis -> SeededAnalysis
joinSeededAnalysis leftAnalysis rightAnalysis =
  SeededAnalysis
    { saStructuralObservations =
        Set.union
          (saStructuralObservations leftAnalysis)
          (saStructuralObservations rightAnalysis),
      saExternalEvidence =
        Set.union
          (saExternalEvidence leftAnalysis)
          (saExternalEvidence rightAnalysis)
    }

seedExternalEvidence :: ClassId -> Int -> EGraph ArithF SeededAnalysis -> EGraph ArithF SeededAnalysis
seedExternalEvidence classId evidence graph =
  bumpEGraphRevision
    ( graph
      { egAnalysis =
          IntMap.adjust
            (\analysisValue -> analysisValue {saExternalEvidence = Set.insert evidence (saExternalEvidence analysisValue)})
            (classIdKey classId)
            (egAnalysis graph)
      }
    )

tarjanTests :: TestTree
tarjanTests =
  testGroup "Tarjan SCC" . hunitCases $
    [ HUnitCase "acyclic diamond has no cyclic keys" $
        assertCyclicKeys [] [(0, [1, 2]), (1, [3]), (2, [3])],
      HUnitCase "simple 2-cycle detected" $
        assertCyclicKeys [0, 1] [(0, [1]), (1, [0])],
      HUnitCase "self-loop detected" $
        assertCyclicKeys [0] [(0, [0])],
      HUnitCase "mixed cyclic and acyclic" $
        assertCyclicKeys [0, 1, 2] [(0, [1]), (1, [2]), (2, [0]), (3, [0])],
      HUnitCase "empty graph has no cyclic keys" $
        assertCyclicKeys [] []
    ]

roundRebuildLocalMergeTests :: TestTree
roundRebuildLocalMergeTests =
  testGroup "Round rebuild local merge canonicalization" . hunitCases $
    [ HUnitCase "raw local merge order stores the same canonical closure" $
        withResult roundRebuildSeed $ \(seed, _) ->
          withResult (runRoundRebuildPair seed) $ \reports ->
              let leftGraph = rebuiltContextGraph (rrpLeftReport reports)
                  rightGraph = rebuiltContextGraph (rrpRightReport reports)
                  probeClasses = [rsClass1 seed, rsClass2 seed, rsClass3 seed]
                  leftRepresentatives = traverse (\classId -> contextClassAt ModuleCtx classId leftGraph) probeClasses
                  rightRepresentatives = traverse (\classId -> contextClassAt ModuleCtx classId rightGraph) probeClasses
               in do
                    leftRepresentatives @?= rightRepresentatives
                    fmap allEqual leftRepresentatives @?= Right True
                    rrrRebuildDelta (rrpLeftReport reports)
                      @?= rrrRebuildDelta (rrpRightReport reports)
                    requireCachedContextPayloadFor ModuleCtx leftGraph @?= requireCachedContextPayloadFor ModuleCtx rightGraph,
      HUnitCase "canonical local closure makes parent projection order-independent" $
        withResult roundRebuildSeed $ \(seed, _) ->
          withResult (runRoundRebuildPair seed) $ \reports ->
              let leftGraph = rebuiltContextGraph (rrpLeftReport reports)
                  rightGraph = rebuiltContextGraph (rrpRightReport reports)
               in do
                    requireCachedContextPayloadFor ModuleCtx leftGraph @?= requireCachedContextPayloadFor ModuleCtx rightGraph
                    contextClassAt ModuleCtx (rsParent1 seed) leftGraph @?= contextClassAt ModuleCtx (rsParent3 seed) leftGraph
                    contextClassAt ModuleCtx (rsParent1 seed) rightGraph @?= contextClassAt ModuleCtx (rsParent3 seed) rightGraph
    ]

allEqual :: Eq a => [a] -> Bool
allEqual values =
  case values of
    [] -> True
    firstValue : remainingValues -> all (== firstValue) remainingValues

assertCyclicKeys :: [Int] -> [(Int, [Int])] -> Assertion
assertCyclicKeys expectedKeys childEdges =
  cyclicCellsFromChildrenInt (childMulti childEdges) @?= IntSet.fromList expectedKeys

childMulti :: [(Int, [Int])] -> IntMap (IntMap Int)
childMulti =
  IntMap.fromList . fmap (\(parent, children) -> (parent, IntMap.fromList (fmap (\child -> (child, 1)) children)))

data RoundRebuildSeed = RoundRebuildSeed
  { rsClass1 :: !ClassId,
    rsClass2 :: !ClassId,
    rsClass3 :: !ClassId,
    rsParent1 :: !ClassId,
    rsParent3 :: !ClassId,
    rsGraphWithPendingMerge :: !(ContextEGraph ArithF NodeCount Scope)
  }

data RoundRebuildPair = RoundRebuildPair
  { rrpLeftReport :: !(RoundRebuildReport SurfaceKind ArithF NodeCount Scope),
    rrpRightReport :: !(RoundRebuildReport SurfaceKind ArithF NodeCount Scope)
  }

roundRebuildSeed :: Either UnionFindAllocationError (RoundRebuildSeed, [(ClassId, ClassId)])
roundRebuildSeed = do
  graph <- seedArithTerms [one, two, three, four, five, onePlusFour, threePlusFour]
  class1 <- classOfArith one graph
  class2 <- classOfArith two graph
  class3 <- classOfArith three graph
  class4 <- classOfArith four graph
  class5 <- classOfArith five graph
  parent1 <- classOfArith onePlusFour graph
  parent3 <- classOfArith threePlusFour graph
  pure
    ( RoundRebuildSeed
        { rsClass1 = class1,
          rsClass2 = class2,
          rsClass3 = class3,
          rsParent1 = parent1,
          rsParent3 = parent3,
          rsGraphWithPendingMerge = moduleContextGraph (merge class4 class5 graph)
        },
      [(class2, class1), (class3, class1)]
    )

runRoundRebuildPair :: RoundRebuildSeed -> Either String RoundRebuildPair
runRoundRebuildPair seed = do
  leftGraph <- roundRebuildContextGraph (leftLocalUnions seed) seed
  rightGraph <- roundRebuildContextGraph (rightLocalUnions seed) seed
  leftReport <- first show (runRoundRebuildReport (emptySaturatingContextEGraph leftGraph))
  rightReport <- first show (runRoundRebuildReport (emptySaturatingContextEGraph rightGraph))
  Right (RoundRebuildPair leftReport rightReport)

leftLocalUnions :: RoundRebuildSeed -> [(ClassId, ClassId)]
leftLocalUnions seed =
  [(rsClass1 seed, rsClass2 seed), (rsClass2 seed, rsClass3 seed)]

rightLocalUnions :: RoundRebuildSeed -> [(ClassId, ClassId)]
rightLocalUnions seed =
  [(rsClass2 seed, rsClass3 seed), (rsClass1 seed, rsClass2 seed)]

rebuiltContextGraph :: RoundRebuildReport SurfaceKind ArithF NodeCount Scope -> ContextEGraph ArithF NodeCount Scope
rebuiltContextGraph =
  sceContextGraph . rrrGraph

roundRebuildContextGraph ::
  [(ClassId, ClassId)] ->
  RoundRebuildSeed ->
  Either String (ContextEGraph ArithF NodeCount Scope)
roundRebuildContextGraph localUnions seed =
  foldM
    (\contextGraph (leftClassId, rightClassId) -> first show (contextMerge ModuleCtx leftClassId rightClassId contextGraph))
    (rsGraphWithPendingMerge seed)
    localUnions

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.EGraph.Pure.Extraction.Worklist
  ( ChoiceParentIndex,
    choiceParentIndex,
    WorklistSeed (..),
    worklistChoices,
    worklistChoicesTotal,
    ChoiceLabelAlgebra (..),
    choiceLabels,
    choiceKey,
    chooseBetterChoice,
    nodeChoice,
    candidateChoices,
    extractionConvergenceReport,
  )
where

import Data.Foldable (toList)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, maybeToList)
import Moonlight.Core (Language)
import Moonlight.EGraph.Pure.Extraction.Core
  ( AnalysisCostAlgebra (..),
    BestChoice (..),
    ExtractionConvergenceReport (..),
    ExtractionFixpointBudget (..),
    ExtractionTable,
    extractionCanonicalClass,
    extractionClassAnalysis,
    extractionClassNodes,
    extractionClasses,
    lookupExtractionClass,
  )
import Moonlight.EGraph.Pure.Types
  ( ClassId (..),
    ENode (..),
    classIdKey,
  )
import Numeric.Natural (Natural)

choiceKey :: BestChoice f cost -> (cost, Int, ENode f)
choiceKey bestChoice =
  (bcCost bestChoice, bcSize bestChoice, bcNode bestChoice)

chooseBetterChoice :: (Language f, Ord cost) => BestChoice f cost -> Maybe (BestChoice f cost) -> Maybe (BestChoice f cost)
chooseBetterChoice newChoice maybeCurrentChoice =
  Just
    ( maybe
        newChoice
        (\currentChoice -> if choiceKey newChoice < choiceKey currentChoice then newChoice else currentChoice)
        maybeCurrentChoice
    )

nodeChoice ::
  Language f =>
  AnalysisCostAlgebra f a cost ->
  ExtractionTable f a ->
  IntMap (Maybe (BestChoice f cost)) ->
  a ->
  ENode f ->
  Maybe (BestChoice f cost)
nodeChoice costAlgebraValue table currentChoices classAnalysis nodeValue@(ENode childClassIds) = do
  childData <- traverse lookupChildData childClassIds
  let childPairs = fmap (\(childAnalysis, bestChoice) -> (childAnalysis, bcCost bestChoice)) childData
      childSizes = fmap (\(_, bestChoice) -> bcSize bestChoice) childData
  pure
    BestChoice
      { bcCost = analysisCostAlgebra costAlgebraValue classAnalysis childPairs,
        bcSize = 1 + sum childSizes,
        bcNode = nodeValue
      }
  where
    lookupChildData childClassId = do
      canonicalChildId <- extractionCanonicalClass table childClassId
      childEClass <- lookupExtractionClass table canonicalChildId
      childChoice <-
        IntMap.findWithDefault Nothing (classIdKey canonicalChildId) currentChoices
      pure (extractionClassAnalysis childEClass, childChoice)

candidateChoices :: Language f => AnalysisCostAlgebra f a cost -> ExtractionTable f a -> IntMap (Maybe (BestChoice f cost)) -> ClassId -> [BestChoice f cost]
candidateChoices costAlgebraValue table currentChoices classId =
  foldMap
    ( \eClass ->
        extractionClassNodes eClass
          >>= maybeToList . nodeChoice costAlgebraValue table currentChoices (extractionClassAnalysis eClass)
    )
    (lookupExtractionClass table classId)

extractionConvergenceReport ::
  ExtractionFixpointBudget ->
  ExtractionTable f a ->
  IntMap (Maybe (BestChoice f cost)) ->
  ExtractionConvergenceReport
extractionConvergenceReport budget table choices =
  let totalClassCount =
        IntMap.size (extractionClasses table)
      resolvedClassCount =
        IntMap.size (IntMap.filter isJust choices)
   in ExtractionConvergenceReport
        { ecrBudget = budget,
          ecrTotalClassCount = totalClassCount,
          ecrResolvedClassCount = resolvedClassCount,
          ecrUnresolvedClassCount = totalClassCount - resolvedClassCount
        }

type ChoiceParentIndex = IntMap IntSet

choiceParentIndex :: Language f => ExtractionTable f a -> ChoiceParentIndex
choiceParentIndex table =
  IntMap.foldlWithKey' addClassParents IntMap.empty (extractionClasses table)
  where
    addClassParents index classKey eClass =
      foldl' (addNodeParents classKey) index (extractionClassNodes eClass)

    addNodeParents classKey index (ENode childClassIds) =
      foldl'
        ( \current childClassId ->
            case extractionCanonicalClass table childClassId of
              Nothing ->
                current
              Just canonicalChildId ->
                IntMap.insertWith
                  IntSet.union
                  (classIdKey canonicalChildId)
                  (IntSet.singleton classKey)
                  current
        )
        index
        (toList childClassIds)

type WorklistSeed :: Type
data WorklistSeed
  = SeedAllClasses
  | SeedClasses !IntSet
  deriving stock (Eq, Show)

-- | Knuth's generalization of Dijkstra to superior hypergraph problems
-- (Knuth 1977).  PRECONDITION: the cost algebra must be strictly superior —
-- every node's cost strictly exceeds the cost of each of its children.  Size
-- and depth style algebras satisfy this; under it, pop order is non-decreasing
-- in cost, every candidate tied at the popped cost is already enqueued, and
-- the full 'choiceKey' tie-break of the round fixpoint is reproduced exactly.
-- The budget caps finalizations; termination itself is structural (each
-- hyperedge fires exactly once), so exhaustion reports a resource bound, not
-- non-convergence.
worklistChoices ::
  (Language f, Ord cost) =>
  ExtractionFixpointBudget ->
  AnalysisCostAlgebra f a cost ->
  ExtractionTable f a ->
  IntMap (Maybe (BestChoice f cost)) ->
  WorklistSeed ->
  Either ExtractionConvergenceReport (IntMap (Maybe (BestChoice f cost)))
worklistChoices budget costAlgebraValue table boundary seed =
  case runWorklist (Just (extractionFixpointBudgetRounds budget)) costAlgebraValue table boundary seed of
    (choices, BudgetExhausted) ->
      Left (extractionConvergenceReport budget table choices)
    (choices, Converged) ->
      Right choices

worklistChoicesTotal ::
  (Language f, Ord cost) =>
  AnalysisCostAlgebra f a cost ->
  ExtractionTable f a ->
  IntMap (Maybe (BestChoice f cost)) ->
  WorklistSeed ->
  IntMap (Maybe (BestChoice f cost))
worklistChoicesTotal costAlgebraValue table boundary seed =
  fst (runWorklist Nothing costAlgebraValue table boundary seed)

type WorklistOutcome :: Type
data WorklistOutcome
  = Converged
  | BudgetExhausted
  deriving stock (Eq, Show)

type ChoiceEdge :: (Type -> Type) -> Type
data ChoiceEdge f = ChoiceEdge
  { ceHeadKey :: !Int,
    ceNode :: !(ENode f),
    ceChildKeys :: !IntSet
  }

type LoopState :: (Type -> Type) -> Type -> Type
data LoopState f cost = LoopState
  { lsFinal :: !(IntMap (Maybe (BestChoice f cost))),
    lsFinalized :: !IntSet,
    lsCounters :: !(IntMap Int),
    lsTentative :: !(IntMap (BestChoice f cost)),
    lsQueue :: !(Map (cost, Int, ENode f, Int) (BestChoice f cost)),
    lsDemand :: !(Maybe IntSet)
  }

runWorklist ::
  forall f a cost.
  (Language f, Ord cost) =>
  Maybe Natural ->
  AnalysisCostAlgebra f a cost ->
  ExtractionTable f a ->
  IntMap (Maybe (BestChoice f cost)) ->
  WorklistSeed ->
  (IntMap (Maybe (BestChoice f cost)), WorklistOutcome)
runWorklist popCap costAlgebraValue table boundary seed =
  go popCap (offerReadyEdges initialReadyEdges initialState)
  where
    classes =
      extractionClasses table

    boundaryFinal =
      IntMap.filter isJust (IntMap.intersection boundary classes)

    initialFinal =
      IntMap.union boundaryFinal (Nothing <$ classes)

    initialFinalized =
      IntMap.keysSet boundaryFinal

    edges =
      tableEdges table

    edgesByChild =
      IntMap.foldlWithKey' addEdgeChildren IntMap.empty edges

    addEdgeChildren :: IntMap [Int] -> Int -> ChoiceEdge f -> IntMap [Int]
    addEdgeChildren index edgeId edge =
      IntSet.foldl'
        ( \current childKey ->
            IntMap.insertWith (<>) childKey [edgeId] current
        )
        index
        (ceChildKeys edge)

    initialCounters =
      fmap
        (\edge -> IntSet.size (IntSet.difference (ceChildKeys edge) initialFinalized))
        edges

    initialReadyEdges =
      IntMap.keys (IntMap.filter (== 0) initialCounters)

    initialDemand =
      case seed of
        SeedAllClasses ->
          Nothing
        SeedClasses wanted ->
          Just
            ( IntSet.difference
                (canonicalKeySet wanted)
                initialFinalized
            )

    canonicalKeySet =
      IntSet.foldl'
        ( \current key ->
            case extractionCanonicalClass table (ClassId key) of
              Just canonicalId
                | IntMap.member (classIdKey canonicalId) classes ->
                    IntSet.insert (classIdKey canonicalId) current
              _ ->
                current
        )
        IntSet.empty

    initialState =
      LoopState
        { lsFinal = initialFinal,
          lsFinalized = initialFinalized,
          lsCounters = initialCounters,
          lsTentative = IntMap.empty,
          lsQueue = Map.empty,
          lsDemand = initialDemand
        }

    offerReadyEdges edgeIds state =
      foldl' offerEdge state edgeIds

    offerEdge state edgeId =
      case IntMap.lookup edgeId edges of
        Nothing ->
          state
        Just edge
          | IntSet.member (ceHeadKey edge) (lsFinalized state) ->
              state
          | otherwise ->
              maybe state (offerChoice state (ceHeadKey edge)) (edgeChoice state edge)

    edgeChoice state edge = do
      eClass <- IntMap.lookup (ceHeadKey edge) classes
      nodeChoice
        costAlgebraValue
        table
        (lsFinal state)
        (extractionClassAnalysis eClass)
        (ceNode edge)

    offerChoice :: LoopState f cost -> Int -> BestChoice f cost -> LoopState f cost
    offerChoice state classKey choice =
      let improves =
            maybe
              True
              (\current -> choiceKey choice < choiceKey current)
              (IntMap.lookup classKey (lsTentative state))
       in if improves
            then
              let (cost, size, node) = choiceKey choice
               in state
                    { lsTentative = IntMap.insert classKey choice (lsTentative state),
                      lsQueue = Map.insert (cost, size, node, classKey) choice (lsQueue state)
                    }
            else state

    go remaining state
      | maybe False IntSet.null (lsDemand state) =
          (lsFinal state, Converged)
      | otherwise =
          case Map.minViewWithKey (lsQueue state) of
            Nothing ->
              (lsFinal state, Converged)
            Just (((cost, size, node, classKey), choice), restQueue)
              | staleEntry state classKey (cost, size, node) ->
                  go remaining state {lsQueue = restQueue}
              | remaining == Just 0 ->
                  (lsFinal state, BudgetExhausted)
              | otherwise ->
                  go
                    (fmap (subtract 1) remaining)
                    (finalize state {lsQueue = restQueue} classKey choice)

    staleEntry :: LoopState f cost -> Int -> (cost, Int, ENode f) -> Bool
    staleEntry state classKey poppedKey =
      IntSet.member classKey (lsFinalized state)
        || maybe True ((/= poppedKey) . choiceKey) (IntMap.lookup classKey (lsTentative state))

    finalize state classKey choice =
      let finalizedState =
            state
              { lsFinal = IntMap.insert classKey (Just choice) (lsFinal state),
                lsFinalized = IntSet.insert classKey (lsFinalized state),
                lsTentative = IntMap.delete classKey (lsTentative state),
                lsDemand = fmap (IntSet.delete classKey) (lsDemand state)
              }
          dependentEdges =
            IntMap.findWithDefault [] classKey edgesByChild
          (decrementedState, readyEdges) =
            foldl' decrementEdge (finalizedState, []) dependentEdges
       in offerReadyEdges readyEdges decrementedState

    decrementEdge :: (LoopState f cost, [Int]) -> Int -> (LoopState f cost, [Int])
    decrementEdge (state, ready) edgeId =
      case IntMap.lookup edgeId (lsCounters state) of
        Just count
          | count > 0 ->
              let nextCount = count - 1
                  nextState =
                    state {lsCounters = IntMap.insert edgeId nextCount (lsCounters state)}
               in if nextCount == 0
                    then (nextState, edgeId : ready)
                    else (nextState, ready)
        _ ->
          (state, ready)

tableEdges :: Language f => ExtractionTable f a -> IntMap (ChoiceEdge f)
tableEdges table =
  IntMap.fromList
    (zip [0 ..] (IntMap.foldlWithKey' addClassEdges [] (extractionClasses table)))
  where
    addClassEdges acc classKey eClass =
      foldl'
        ( \current nodeValue ->
            maybe current (: current) (classEdge classKey nodeValue)
        )
        acc
        (extractionClassNodes eClass)

    classEdge classKey nodeValue@(ENode childClassIds) = do
      canonicalChildren <-
        traverse
          (fmap classIdKey . extractionCanonicalClass table)
          (toList childClassIds)
      pure
        ChoiceEdge
          { ceHeadKey = classKey,
            ceNode = nodeValue,
            ceChildKeys = IntSet.fromList canonicalChildren
          }

type ChoiceLabelAlgebra :: (Type -> Type) -> Type -> Type
data ChoiceLabelAlgebra f label = ChoiceLabelAlgebra
  { choiceLabelNode :: ENode f -> label,
    choiceLabelJoin :: label -> label -> label
  }

-- | Derived labels over finalized best choices, computed in ascending
-- 'choiceKey' order: strict superiority makes every chosen child strictly
-- cheaper than its parent, so the cost order is a topological order of the
-- chosen-term dag and child labels are always present when a class is reached.
choiceLabels ::
  (Language f, Ord cost) =>
  ExtractionTable f a ->
  IntMap (Maybe (BestChoice f cost)) ->
  ChoiceLabelAlgebra f label ->
  IntMap label
choiceLabels table choices algebraValue =
  foldl' addLabel IntMap.empty orderedFinalized
  where
    orderedFinalized =
      sortOn
        (choiceKey . snd)
        [(classKey, choice) | (classKey, Just choice) <- IntMap.toList choices]

    addLabel labels (classKey, choice) =
      case childLabels labels (bcNode choice) of
        Nothing ->
          labels
        Just labelValues ->
          IntMap.insert
            classKey
            ( foldl'
                (choiceLabelJoin algebraValue)
                (choiceLabelNode algebraValue (bcNode choice))
                labelValues
            )
            labels

    childLabels labels (ENode childClassIds) =
      traverse
        ( \childClassId -> do
            canonicalChildId <- extractionCanonicalClass table childClassId
            IntMap.lookup (classIdKey canonicalChildId) labels
        )
        (toList childClassIds)

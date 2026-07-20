{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.EGraph.Pure.Extraction.Worklist
  ( ChoiceParentIndex,
    choiceParentIndex,
    worklistChoices,
    worklistChoicesTotal,
    worklistSuperiorChoices,
    choiceKey,
    chooseBetterChoice,
    nodeChoice,
    candidateChoices,
    extractionBudgetExhaustion,
  )
where

import Data.Foldable (toList)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Function (fix)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, maybeToList)
import Moonlight.Core
  ( FixpointDivergence (..),
    Language,
    fixpointBounded,
  )
import Moonlight.EGraph.Pure.Extraction.Core
  ( AnalysisCostAlgebra (..),
    BestChoice (..),
    ExtractionBudgetExhaustion (..),
    ExtractionWorkBudget (..),
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

extractionBudgetExhaustion ::
  ExtractionWorkBudget ->
  Natural ->
  ExtractionTable f a ->
  IntMap (Maybe (BestChoice f cost)) ->
  ExtractionBudgetExhaustion
extractionBudgetExhaustion budget consumedWorkSteps table choices =
  let totalClassCount =
        IntMap.size (extractionClasses table)
      resolvedClassCount =
        IntMap.size (IntMap.filter isJust choices)
   in ExtractionBudgetExhaustion
        { ebeBudget = budget,
          ebeConsumedWorkSteps = consumedWorkSteps,
          ebeTotalClassCount = totalClassCount,
          ebeResolvedClassCount = resolvedClassCount,
          ebeUnresolvedClassCount = totalClassCount - resolvedClassCount
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

-- | Authoritative bounded extraction over the accepted general cost-algebra
-- domain. A Knuth class finalization consumes one work step. If an evaluated
-- hyperedge violates strict superiority, extraction descends immediately to
-- round improvement, where each whole-table pass consumes one step from the
-- same remaining budget.
worklistChoices ::
  (Language f, Ord cost) =>
  ExtractionWorkBudget ->
  AnalysisCostAlgebra f a cost ->
  ExtractionTable f a ->
  IntMap (Maybe (BestChoice f cost)) ->
  Either ExtractionBudgetExhaustion (IntMap (Maybe (BestChoice f cost)))
worklistChoices budget costAlgebraValue table boundary =
  case runWorklist (Just workLimit) costAlgebraValue table boundary of
    WorklistBudgetExhausted choices consumedWorkSteps ->
      Left (extractionBudgetExhaustion budget consumedWorkSteps table choices)
    WorklistConverged choices ->
      Right choices
    WorklistGeneralCost consumedWorkSteps ->
      case roundChoicesBounded (workLimit - consumedWorkSteps) costAlgebraValue table boundary of
        Left divergence ->
          Left
            ( extractionBudgetExhaustion
                budget
                workLimit
                table
                (fixpointDivergenceLast divergence)
            )
        Right choices ->
          Right choices
  where
    workLimit =
      extractionWorkBudgetSteps budget

worklistChoicesTotal ::
  (Language f, Ord cost) =>
  AnalysisCostAlgebra f a cost ->
  ExtractionTable f a ->
  IntMap (Maybe (BestChoice f cost)) ->
  IntMap (Maybe (BestChoice f cost))
worklistChoicesTotal costAlgebraValue table boundary =
  maybe
    (roundChoicesTotal costAlgebraValue table boundary)
    id
    (worklistSuperiorChoices costAlgebraValue table boundary)

-- | Return the fast section only when every evaluated edge proves strict
-- superiority. General-cost observations decline the optimization so a
-- caller can descend to its exact dependency cover before round improvement.
worklistSuperiorChoices ::
  (Language f, Ord cost) =>
  AnalysisCostAlgebra f a cost ->
  ExtractionTable f a ->
  IntMap (Maybe (BestChoice f cost)) ->
  Maybe (IntMap (Maybe (BestChoice f cost)))
worklistSuperiorChoices costAlgebraValue table boundary =
  case runWorklist Nothing costAlgebraValue table boundary of
    WorklistConverged choices -> Just choices
    WorklistGeneralCost _consumedWorkSteps -> Nothing
    WorklistBudgetExhausted _choices _consumedWorkSteps -> Nothing

roundChoicesBounded ::
  (Language f, Ord cost) =>
  Natural ->
  AnalysisCostAlgebra f a cost ->
  ExtractionTable f a ->
  IntMap (Maybe (BestChoice f cost)) ->
  Either
    (FixpointDivergence (IntMap (Maybe (BestChoice f cost))))
    (IntMap (Maybe (BestChoice f cost)))
roundChoicesBounded workSteps costAlgebraValue table boundary =
  fixpointBounded
    workSteps
    (improveChoices costAlgebraValue table boundaryKeys)
    (initialRoundChoices table boundary)
  where
    boundaryKeys =
      IntMap.keysSet (IntMap.filter isJust (IntMap.intersection boundary (extractionClasses table)))

roundChoicesTotal ::
  (Language f, Ord cost) =>
  AnalysisCostAlgebra f a cost ->
  ExtractionTable f a ->
  IntMap (Maybe (BestChoice f cost)) ->
  IntMap (Maybe (BestChoice f cost))
roundChoicesTotal costAlgebraValue table boundary =
  fix converge (initialRoundChoices table boundary)
  where
    boundaryKeys =
      IntMap.keysSet (IntMap.filter isJust (IntMap.intersection boundary (extractionClasses table)))

    improve =
      improveChoices costAlgebraValue table boundaryKeys

    converge continue currentChoices =
      let nextChoices = improve currentChoices
       in if nextChoices == currentChoices
            then currentChoices
            else nextChoices `seq` continue nextChoices

initialRoundChoices ::
  ExtractionTable f a ->
  IntMap (Maybe (BestChoice f cost)) ->
  IntMap (Maybe (BestChoice f cost))
initialRoundChoices table boundary =
  IntMap.union
    (IntMap.filter isJust (IntMap.intersection boundary (extractionClasses table)))
    (Nothing <$ extractionClasses table)

improveChoices ::
  (Language f, Ord cost) =>
  AnalysisCostAlgebra f a cost ->
  ExtractionTable f a ->
  IntSet ->
  IntMap (Maybe (BestChoice f cost)) ->
  IntMap (Maybe (BestChoice f cost))
improveChoices costAlgebraValue table boundaryKeys currentChoices =
  IntMap.mapWithKey improveClass (extractionClasses table)
  where
    improveClass classKey _extractionClass
      | IntSet.member classKey boundaryKeys =
          IntMap.findWithDefault Nothing classKey currentChoices
      | otherwise =
          foldr
            chooseBetterChoice
            (IntMap.findWithDefault Nothing classKey currentChoices)
            (candidateChoices costAlgebraValue table currentChoices (ClassId classKey))

type CostCompatibility :: Type
data CostCompatibility
  = StrictlySuperior
  | GeneralCostObserved
  deriving stock (Eq, Show)

type WorklistResult :: (Type -> Type) -> Type -> Type
data WorklistResult f cost
  = WorklistConverged !(IntMap (Maybe (BestChoice f cost)))
  | WorklistGeneralCost !Natural
  | WorklistBudgetExhausted !(IntMap (Maybe (BestChoice f cost))) !Natural

type ChoiceEdge :: (Type -> Type) -> Type
data ChoiceEdge f = ChoiceEdge
  { ceHeadKey :: !Int,
    ceNode :: !(ENode f),
    ceChildKeys :: !IntSet
  }

type LoopState :: (Type -> Type) -> Type -> Type
data LoopState f cost = LoopState
  { lsFinal :: !(IntMap (Maybe (BestChoice f cost))),
    lsCounters :: !(IntMap Int),
    lsTentative :: !(IntMap (BestChoice f cost)),
    lsQueue :: !(Map (cost, Int, ENode f, Int) (BestChoice f cost)),
    lsCompatibility :: !CostCompatibility
  }

runWorklist ::
  forall f a cost.
  (Language f, Ord cost) =>
  Maybe Natural ->
  AnalysisCostAlgebra f a cost ->
  ExtractionTable f a ->
  IntMap (Maybe (BestChoice f cost)) ->
  WorklistResult f cost
runWorklist popCap costAlgebraValue table boundary =
  go popCap 0 (offerReadyEdges initialReadyEdges initialState)
  where
    classes =
      extractionClasses table

    boundaryFinal =
      IntMap.filter isJust (IntMap.intersection boundary classes)

    initialFinal =
      IntMap.union boundaryFinal (Nothing <$ classes)

    boundaryKeys =
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
        (\edge -> IntSet.size (IntSet.difference (ceChildKeys edge) boundaryKeys))
        edges

    initialReadyEdges =
      IntMap.keys (IntMap.filter (== 0) initialCounters)

    initialState =
      LoopState
        { lsFinal = initialFinal,
          lsCounters = initialCounters,
          lsTentative = IntMap.empty,
          lsQueue = Map.empty,
          lsCompatibility = StrictlySuperior
        }

    offerReadyEdges edgeIds state =
      foldl' offerEdge state edgeIds

    offerEdge state edgeId =
      case IntMap.lookup edgeId edges of
        Nothing ->
          state
        Just edge ->
          case edgeChoice state edge of
            Nothing ->
              state
            Just choice ->
              let observedState = observeCompatibility state edge choice
               in if isJust (IntMap.findWithDefault Nothing (ceHeadKey edge) (lsFinal state))
                    then observedState
                    else offerChoice observedState (ceHeadKey edge) choice

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

    go remaining consumedWorkSteps state
      | GeneralCostObserved <- lsCompatibility state =
          WorklistGeneralCost consumedWorkSteps
      | otherwise =
          case Map.minViewWithKey (lsQueue state) of
            Nothing ->
              WorklistConverged (lsFinal state)
            Just (((cost, size, node, classKey), choice), restQueue)
              | staleEntry state classKey (cost, size, node) ->
                  go remaining consumedWorkSteps state {lsQueue = restQueue}
              | remaining == Just 0 ->
                  WorklistBudgetExhausted (lsFinal state) consumedWorkSteps
              | otherwise ->
                  go
                    (fmap (subtract 1) remaining)
                    (consumedWorkSteps + 1)
                    (finalize state {lsQueue = restQueue} classKey choice)

    staleEntry :: LoopState f cost -> Int -> (cost, Int, ENode f) -> Bool
    staleEntry state classKey poppedKey =
      isJust (IntMap.findWithDefault Nothing classKey (lsFinal state))
        || maybe True ((/= poppedKey) . choiceKey) (IntMap.lookup classKey (lsTentative state))

    finalize state classKey choice =
      let finalizedState =
            state
              { lsFinal = IntMap.insert classKey (Just choice) (lsFinal state),
                lsTentative = IntMap.delete classKey (lsTentative state)
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

    observeCompatibility state edge choice =
      state
        { lsCompatibility =
            if edgeIsStrictlySuperior state edge choice
              then lsCompatibility state
              else GeneralCostObserved
        }

    edgeIsStrictlySuperior state edge choice =
      IntSet.foldr
        ( \childKey childrenAreCheaper ->
            maybe
              False
              ((bcCost choice >) . bcCost)
              (IntMap.findWithDefault Nothing childKey (lsFinal state))
              && childrenAreCheaper
        )
        True
        (ceChildKeys edge)

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

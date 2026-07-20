{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Flow.Execution.Factor.NodePlan
  ( FactorNodePlan (..),
    ensureRootFactor,
    ensureAllBagBeliefs,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Maybe
  ( maybeToList,
  )
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Moonlight.Delta.Patch qualified as CorePatch
import Control.Monad (foldM)
import Data.Traversable qualified as Traversable
import Moonlight.Flow.Execution.Dense.Plan
  ( DenseArrangementId (..),
    DenseJoinPlanError,
    SourceBundle (..),
    denseAtomSource,
    denseFactorSource,
  )
import Moonlight.Flow.Execution.Factor.Core
  ( Factor,
    singletonFactor,
  )
import Moonlight.Flow.Execution.Factor.Delta
  ( FactorDelta,
  )
import Moonlight.Flow.Execution.Factor.Incremental
  ( buildFactorFromSourceBundles,
    updateFactorIncremental,
  )
import Moonlight.Flow.Execution.Factor.Types
import Moonlight.Flow.Execution.Observe.Provenance.Types
  ( ProvArena,
  )
import Moonlight.Flow.Execution.Observe.Telemetry
  ( IncrementalUpdateTrace (..),
    NodeAction (..),
    NodeMaintenance (..),
    emptyIncrementalUpdateTrace,
    recordNodeMaintenance,
  )
import Moonlight.Differential.Row.Delta
  ( RowDelta
  )
import Moonlight.Differential.Row.Patch
  ( emptyShapedPatch,
    plainRowPatchChangeMap,
    shapedPatchSupport,
    shapedPatchNull,
    ShapedPatch (..),
  )
import Moonlight.Differential.Row.Tuple
import Moonlight.Flow.Plan.Query.Core
import Moonlight.Differential.Index.IndexedRows
  ( indexedRowsPayloadMap,
    indexedRowsLayout,
  )
import Moonlight.Flow.Storage.Relation
  ( Relation (..),
  )
import Moonlight.Flow.Storage.Store
import Moonlight.Flow.Storage.View

type MessageInput :: Type
data MessageInput = MessageInput
  { miFactor :: !Factor,
    miDelta :: !FactorDelta
  }
  deriving stock (Eq, Show)

type FactorNodePlan :: Type
data FactorNodePlan = FactorNodePlan
  { fnpNode :: !FactorNode,
    fnpSchema :: ![SlotId],
    fnpSources :: !(FactorFrame -> [SourceBundle]),
    fnpInputChanged :: !(FactorFrame -> Bool)
  }

ensureFactorNode :: FactorNodePlan -> FactorFrame -> Either DenseJoinPlanError (FactorFrame, Factor, FactorDelta)
ensureFactorNode plan frame0 =
  case Map.lookup node (fcFactors cache0) of
    Just entry
      | not nodeChanged ->
          Right (recordFrameNode node NodeReused emptyIncrementalTrace frame0, feFactor entry, feDelta entry)
      | not inputChanged ->
          let frame1 = clearFrameDirtyNode node frame0
           in Right (recordFrameNode node NodeReused emptyIncrementalTrace frame1, feFactor entry, feDelta entry)
      | otherwise -> do
          let bundles = fnpSources plan frame0
          (arena1, newFactor, newContributions, factorDelta, updateTrace) <-
            updateFactorIncremental
              (ffRepairTelemetry frame0)
              schema
              bundles
              (feFactor entry)
              (feContributions entry)
              (fcArena cache0)
          let frame1 =
                setFrameFactor
                  node
                  FactorEntry
                    { feFactor = newFactor,
                      feDelta = factorDelta,
                      feContributions = newContributions
                    }
                  (setFrameArena arena1 (clearFrameDirtyNode node frame0))
          pure (recordFrameNode node NodePatched updateTrace frame1, newFactor, factorDelta)
    Nothing -> do
      let bundles = fnpSources plan frame0
      (arena1, factor, contributions) <- buildFactorFromSourceBundles schema bundles (fcArena cache0)
      let factorDelta = emptyShapedPatch schema
          frame1 =
            setFrameFactor
              node
              FactorEntry
                { feFactor = factor,
                  feDelta = factorDelta,
                  feContributions = contributions
                }
              (setFrameArena arena1 (clearFrameDirtyNode node frame0))
      pure (recordFrameNode node NodeBuilt emptyIncrementalTrace frame1, factor, factorDelta)
  where
    node = fnpNode plan
    schema = fnpSchema plan
    cache0 = ffCache frame0
    dirty = Set.member node (ffDirtyNodes frame0)
    inputChanged = fnpInputChanged plan frame0
    nodeChanged = dirty || inputChanged
    emptyIncrementalTrace =
      emptyIncrementalUpdateTrace
{-# INLINE ensureFactorNode #-}

setFrameArena :: ProvArena -> FactorFrame -> FactorFrame
setFrameArena arena frame =
  frame
    { ffCache = (ffCache frame) {fcArena = arena}
    }
{-# INLINE setFrameArena #-}

setFrameFactor :: FactorNode -> FactorEntry -> FactorFrame -> FactorFrame
setFrameFactor node entry frame =
  frame
    { ffCache = factorCacheInsert node entry (ffCache frame),
      ffDeltaNodes =
        if shapedPatchNull (feDelta entry)
          then ffDeltaNodes frame
          else Set.insert node (ffDeltaNodes frame)
    }
{-# INLINE setFrameFactor #-}

clearFrameDirtyNode :: FactorNode -> FactorFrame -> FactorFrame
clearFrameDirtyNode node frame =
  frame
    { ffDirtyNodes = Set.delete node (ffDirtyNodes frame)
    }
{-# INLINE clearFrameDirtyNode #-}

recordFrameNode :: FactorNode -> NodeAction -> IncrementalUpdateTrace -> FactorFrame -> FactorFrame
recordFrameNode node action traceValue frame =
  frame
    { ffMetrics =
        recordNodeMaintenance
          node
          NodeMaintenance
            { nmAction = action,
              nmAffectedKeys = iutAffectedKeys traceValue,
              nmRecomputedCells = iutRecomputedCells traceValue,
              nmWorkKeys = iutWorkKeys traceValue,
              nmJoinRuns = iutJoinRuns traceValue,
              nmJoinLeaves = iutJoinLeaves traceValue,
              nmRepairTelemetry = iutRepairTelemetry traceValue
            }
          (ffMetrics frame)
    }
{-# INLINE recordFrameNode #-}

localBagSchema :: Store -> DecompBag -> [SlotId]
localBagSchema store bag =
  orderedSlotNub
    [ sid
      | atomKey <- IntSet.toList (dbAtoms bag),
        Just pr <- [IntMap.lookup atomKey (storeRelations store)],
        sid <- Vector.toList (indexedRowsLayout (relRows pr))
    ]
{-# INLINE localBagSchema #-}

ensureLocalFactor :: DecompPlan -> BagId -> FactorFrame -> Either DenseJoinPlanError (FactorFrame, Factor, FactorDelta)
ensureLocalFactor decomp bagId frame =
  case IntMap.lookup (unBagId bagId) (dpBags decomp) of
    Nothing -> Right (frame, singletonFactor, emptyShapedPatch [])
    Just bag -> ensureFactorNode (localBagNodePlanAtView (fiStore (ffInput frame)) (fiView (ffInput frame)) bagId bag) frame
{-# INLINE ensureLocalFactor #-}

localBagNodePlanAtView :: Store -> View -> BagId -> DecompBag -> FactorNodePlan
localBagNodePlanAtView store _view bagId bag =
  FactorNodePlan
    { fnpNode = FactorNodeBag bagId,
      fnpSchema = localBagSchema store bag,
      fnpSources =
        \frame ->
          atomSourceBundlesForBag
            (fiStore (ffInput frame))
            (fiView (ffInput frame))
            bag
            (IntMap.restrictKeys (fiAtomDeltas (ffInput frame)) (dbAtoms bag)),
      fnpInputChanged =
        \frame -> not (IntMap.null (IntMap.restrictKeys (fiAtomDeltas (ffInput frame)) (dbAtoms bag)))
    }
{-# INLINE localBagNodePlanAtView #-}

atomSourceBundlesForBag :: Store -> View -> DecompBag -> IntMap RowDelta -> [SourceBundle]
atomSourceBundlesForBag store view bag inputDelta =
  [ let maybeDelta =
          IntMap.lookup atomKey inputDelta
     in SourceBundle
          { sbCurrent = denseAtomSource (DenseArrangementId sourceId) store view (mkAtomId atomKey),
            sbDirtyKeys =
              maybe
                Set.empty
                dirtyKeysFromAtomDelta
                maybeDelta
          }
    | (sourceId, atomKey) <- zip [0 :: Int ..] (IntSet.toList (dbAtoms bag))
  ]
{-# INLINE atomSourceBundlesForBag #-}

dirtyKeysFromAtomDelta :: RowDelta -> Set.Set AssignmentTupleKey
dirtyKeysFromAtomDelta delta =
  Set.fromList (coerceTupleKey <$> Map.keys (plainRowPatchChangeMap delta))
{-# INLINE dirtyKeysFromAtomDelta #-}

messageInputChanged :: [MessageInput] -> Bool
messageInputChanged =
  any (not . shapedPatchNull . miDelta)
{-# INLINE messageInputChanged #-}

messageSourceBundles :: [MessageInput] -> [SourceBundle]
messageSourceBundles =
  zipWith messageSourceBundle [0 :: Int ..]
{-# INLINE messageSourceBundles #-}

messageSourceBundle :: Int -> MessageInput -> SourceBundle
messageSourceBundle sourceId input =
  SourceBundle
    { sbCurrent = denseFactorSource (DenseArrangementId sourceId) (miFactor input),
      sbDirtyKeys = shapedPatchSupport (miDelta input)
    }
{-# INLINE messageSourceBundle #-}

messageNodePlan :: FactorNode -> [SlotId] -> [MessageInput] -> FactorNodePlan
messageNodePlan node schema inputs =
  FactorNodePlan
    { fnpNode = node,
      fnpSchema = schema,
      fnpSources = const (messageSourceBundles inputs),
      fnpInputChanged = const (messageInputChanged inputs)
    }
{-# INLINE messageNodePlan #-}

ensureMessage :: DecompPlan -> BagId -> BagId -> FactorFrame -> Either DenseJoinPlanError (FactorFrame, Factor, FactorDelta)
ensureMessage decomp child parent frame0 = do
  (frame1, inputs) <- collectMessageInputs decomp child frame0
  let sep = Map.findWithDefault [] (child, parent) (dpSeparator decomp)
  ensureFactorNode (messageNodePlan (FactorNodeSeparator child parent) sep inputs) frame1
{-# INLINE ensureMessage #-}

ensureRootFactor :: DecompPlan -> FactorFrame -> Either DenseJoinPlanError (FactorFrame, Factor, FactorDelta)
ensureRootFactor decomp frame0 = do
  let root = dpRoot decomp
  (frame1, inputs) <- collectMessageInputs decomp root frame0
  ensureFactorNode (messageNodePlan FactorNodeRoot [] inputs) frame1
{-# INLINE ensureRootFactor #-}

ensureBagBelief :: DecompPlan -> BagId -> FactorFrame -> Either DenseJoinPlanError (FactorFrame, Factor, FactorDelta)
ensureBagBelief decomp bagId frame0 = do
  (frame1, inputs) <- collectMessageInputs decomp bagId frame0
  let schema =
        orderedSlotNub
          (concatMap (Vector.toList . indexedRowsLayout . miFactor) inputs)
  (frame2, belief, beliefDelta) <- ensureFactorNode (messageNodePlan (FactorNodeBagBelief bagId) schema inputs) frame1
  let frame3 = refreshParentSepIndex decomp bagId belief beliefDelta frame2
  pure (frame3, belief, beliefDelta)
{-# INLINE ensureBagBelief #-}

collectMessageInputs :: DecompPlan -> BagId -> FactorFrame -> Either DenseJoinPlanError (FactorFrame, [MessageInput])
collectMessageInputs decomp bagId frame0 = do
  let children = IntMap.findWithDefault [] (unBagId bagId) (dpChildren decomp)
  (frame1, localFactor, localDelta) <- ensureLocalFactor decomp bagId frame0
  (frame2, childInputs) <- Traversable.mapAccumM gather frame1 children
  pure (frame2, MessageInput localFactor localDelta : childInputs)
  where
    gather frameAcc child = do
      (frameNext, message, messageDelta) <- ensureMessage decomp child bagId frameAcc
      pure (frameNext, MessageInput message messageDelta)
{-# INLINE collectMessageInputs #-}

ensureAllBagBeliefs :: DecompPlan -> BagId -> FactorFrame -> Either DenseJoinPlanError FactorFrame
ensureAllBagBeliefs decomp bagId frame0 = do
  let children = IntMap.findWithDefault [] (unBagId bagId) (dpChildren decomp)
  frame1 <- foldM (\frame child -> ensureAllBagBeliefs decomp child frame) frame0 children
  (frame2, _belief, _delta) <- ensureBagBelief decomp bagId frame1
  pure frame2
{-# INLINE ensureAllBagBeliefs #-}

refreshParentSepIndex :: DecompPlan -> BagId -> Factor -> FactorDelta -> FactorFrame -> FactorFrame
refreshParentSepIndex decomp bagId factor delta frame =
  case parentSepSpec decomp bagId factor of
    Nothing ->
      deleteParentSepIndex bagId frame
    Just spec
      | shapedPatchNull delta,
        Map.member bagId (fcParentSepIndexes (ffCache frame)) ->
          frame
      | shapedPatchNull delta ->
          upsertParentSepIndex bagId (buildParentSepIndexFromSpec spec factor) frame
      | Just current <- Map.lookup bagId (fcParentSepIndexes (ffCache frame)),
        psiSeparator current == pssSeparator spec ->
          upsertParentSepIndex
            bagId
            (patchParentSepIndex spec current delta)
            frame
      | otherwise ->
          upsertParentSepIndex bagId (buildParentSepIndexFromSpec spec factor) frame
{-# INLINE refreshParentSepIndex #-}

type ParentSepSpec :: Type
data ParentSepSpec = ParentSepSpec
  { pssFactorSchema :: ![SlotId],
    pssSeparator :: ![SlotId]
  }
  deriving stock (Eq, Show)

parentSepSpec :: DecompPlan -> BagId -> Factor -> Maybe ParentSepSpec
parentSepSpec decomp bagId factor = do
  parent <- IntMap.lookup (unBagId bagId) (dpParent decomp)
  sep <- Map.lookup (bagId, parent) (dpSeparator decomp)
  pure
    ParentSepSpec
      { pssFactorSchema = Vector.toList (indexedRowsLayout factor),
        pssSeparator = sep
      }
{-# INLINE parentSepSpec #-}

deleteParentSepIndex :: BagId -> FactorFrame -> FactorFrame
deleteParentSepIndex bagId frame =
  frame
    { ffCache =
        (ffCache frame)
          { fcParentSepIndexes =
              Map.delete bagId (fcParentSepIndexes (ffCache frame))
          }
    }
{-# INLINE deleteParentSepIndex #-}

upsertParentSepIndex :: BagId -> ParentSepIndex -> FactorFrame -> FactorFrame
upsertParentSepIndex bagId index frame =
  frame
    { ffCache =
        (ffCache frame)
          { fcParentSepIndexes =
              Map.insert bagId index (fcParentSepIndexes (ffCache frame))
          }
    }
{-# INLINE upsertParentSepIndex #-}

buildParentSepIndexFromSpec :: ParentSepSpec -> Factor -> ParentSepIndex
buildParentSepIndexFromSpec spec factor =
  ParentSepIndex
    { psiSeparator = pssSeparator spec,
      psiRowsBySeparator =
        Map.fromListWith Set.union
          [ (sepKey, Set.singleton key)
            | key <- Map.keys (indexedRowsPayloadMap factor),
              sepKey <- maybeToList (projectAssignmentKey (pssFactorSchema spec) (pssSeparator spec) key)
          ]
    }
{-# INLINE buildParentSepIndexFromSpec #-}

patchParentSepIndex :: ParentSepSpec -> ParentSepIndex -> FactorDelta -> ParentSepIndex
patchParentSepIndex spec parentIndex delta =
  CorePatch.foldWithKey'
    (\currentIndex _key -> currentIndex)
    (\currentIndex key _newValue -> insertParentSepIndexRow spec key currentIndex)
    (\currentIndex key _oldValue -> deleteParentSepIndexRow spec key currentIndex)
    (\currentIndex key _oldValue _newValue ->
       insertParentSepIndexRow spec key (deleteParentSepIndexRow spec key currentIndex))
    parentIndex
    (spdDelta delta)
{-# INLINE patchParentSepIndex #-}

insertParentSepIndexRow :: ParentSepSpec -> AssignmentTupleKey -> ParentSepIndex -> ParentSepIndex
insertParentSepIndexRow spec key index =
  case projectAssignmentKey (pssFactorSchema spec) (pssSeparator spec) key of
    Nothing ->
      index
    Just sepKey ->
      index
        { psiRowsBySeparator =
            Map.insertWith
              Set.union
              sepKey
              (Set.singleton key)
              (psiRowsBySeparator index)
        }
{-# INLINE insertParentSepIndexRow #-}

deleteParentSepIndexRow :: ParentSepSpec -> AssignmentTupleKey -> ParentSepIndex -> ParentSepIndex
deleteParentSepIndexRow spec key index =
  case projectAssignmentKey (pssFactorSchema spec) (pssSeparator spec) key of
    Nothing ->
      index
    Just sepKey ->
      index
        { psiRowsBySeparator =
            Map.update
              (nonEmptySet . Set.delete key)
              sepKey
              (psiRowsBySeparator index)
        }
{-# INLINE deleteParentSepIndexRow #-}

nonEmptySet :: Set.Set a -> Maybe (Set.Set a)
nonEmptySet values
  | Set.null values =
      Nothing
  | otherwise =
      Just values
{-# INLINE nonEmptySet #-}

projectAssignmentKey :: [SlotId] -> [SlotId] -> AssignmentTupleKey -> Maybe AssignmentTupleKey
projectAssignmentKey sourceSchema targetSchema key =
  tupleKeyFromRepKeys <$> traverse readTarget targetSchema
  where
    sourceIndex =
      IntMap.fromList
        [ (slotIdKey sid, ix)
          | (ix, sid) <- zip [0 :: Int ..] sourceSchema
        ]
    readTarget sid = do
      ix <- IntMap.lookup (slotIdKey sid) sourceIndex
      tupleKeyIndex key ix
{-# INLINE projectAssignmentKey #-}

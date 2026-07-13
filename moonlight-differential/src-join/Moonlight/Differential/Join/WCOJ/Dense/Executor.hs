{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- | Dense-ST physical WCOJ executor over differential delta sources.
module Moonlight.Differential.Join.WCOJ.Dense.Executor
  ( DenseSelectedOutput (..),
    DenseDeltaProblem (..),
    DenseFrame,
    DenseLeaf,
    DeltaDenseFrame,
    DeltaUndoTarget (..),
    foldDenseSourceIndexesM,
    foldDenseWCOJ,
    foldDenseDeltaWCOJ,
    readDenseEnv,
    readDeltaEnv,
    readDenseFeasible,
    readDeltaFullFeasible,
    denseLeafTupleKey,
    denseDeltaLeafTupleKey,
  )
where

import Control.Monad.ST
  ( ST,
    runST,
  )
import Data.Foldable qualified as Foldable
import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet
  ( IntSet,
  )
import Data.IntSet qualified as IntSet
import Data.Kind
  ( Type,
  )
import Data.Primitive.PrimArray
  ( PrimArray,
  )
import Data.Primitive.PrimArray qualified as PrimArray
import Data.Primitive.PrimVar qualified as PrimVar
import Data.Primitive.SmallArray
  ( SmallArray,
  )
import Data.Primitive.SmallArray qualified as SmallArray
import Data.Word
  ( Word64,
  )
import Moonlight.Differential.Index.RowId
  ( rowIdInt,
  )
import Moonlight.Differential.Index.RowIdSet
  ( RowIdSet,
  )
import Moonlight.Differential.Index.RowSet
  ( RowSet,
    RowSetRestriction (..),
    emptyRowSet,
    rowSetFoldl',
    rowSetIntersection,
    rowSetIntersectionWithRowIdSetChanged,
    rowSetIntersectsRowIdSet,
    rowSetNull,
    rowSetSize,
  )
import Moonlight.Differential.Join.WCOJ.Delta
  ( DeltaJoinSource (..),
  )
import Moonlight.Differential.Join.WCOJ.Dense.BitSet
  ( bitWordsForSlots,
    unsafeClearSlotBit,
    unsafeSetSlotBit,
    unsafeTestSlotBit,
    validSlotKey,
  )
import Moonlight.Differential.Join.WCOJ.Dense.Descent
  ( descendSlots,
  )
import Moonlight.Differential.Row.Tuple
  ( RepKey (..),
    TupleKey,
    tupleKeyFromRepKeys,
  )

type DenseSelectedOutput :: Type
data DenseSelectedOutput = DenseSelectedOutput
  { dsoRows :: !RowSet,
    dsoRowsBySlotValue :: !(IntMap (IntMap RowIdSet))
  }

type DenseDeltaProblem :: Type
data DenseDeltaProblem = DenseDeltaProblem
  { ddpSlotUniverse :: {-# UNPACK #-} !Int,
    ddpSources :: !(SmallArray DeltaJoinSource),
    ddpSourcesBySlot :: !(IntMap IntSet),
    ddpFullSchema :: !(PrimArray Int),
    ddpOutputSchema :: !(PrimArray Int),
    ddpSelectedOutput :: !(Maybe DenseSelectedOutput),
    ddpStaticRank :: !(IntMap Int)
  }

data DenseFrame s = DenseFrame
  { dfProblem :: !DenseDeltaProblem,
    dfEnvValues :: !(PrimArray.MutablePrimArray s Int),
    dfEnvBoundBits :: !(PrimArray.MutablePrimArray s Word64),
    dfFeasible :: !(SmallArray.SmallMutableArray s RowSet),
    dfSelectedRows :: !(SmallArray.SmallMutableArray s RowSet),
    dfUndoSources :: !(PrimArray.MutablePrimArray s Int),
    dfUndoRows :: !(SmallArray.SmallMutableArray s RowSet),
    dfUndoTop :: !(PrimVar.PrimVar s Int),
    dfDomainCache :: !(SmallArray.SmallMutableArray s IntSet),
    dfDomainValidBits :: !(PrimArray.MutablePrimArray s Word64),
    dfAllSourcesWitnessed :: !Bool
  }

type DenseLeaf s = DenseFrame s

data DeltaDenseFrame s = DeltaDenseFrame
  { ddfProblem :: !DenseDeltaProblem,
    ddfEnvValues :: !(PrimArray.MutablePrimArray s Int),
    ddfEnvBoundBits :: !(PrimArray.MutablePrimArray s Word64),
    ddfFullFeasible :: !(SmallArray.SmallMutableArray s RowSet),
    ddfDirtyFeasible :: !(SmallArray.SmallMutableArray s RowSet),
    ddfSelectedRows :: !(SmallArray.SmallMutableArray s RowSet),
    ddfDirtyLiveCount :: !(PrimVar.PrimVar s Int),
    ddfUndoSources :: !(PrimArray.MutablePrimArray s Int),
    ddfUndoRows :: !(SmallArray.SmallMutableArray s RowSet),
    ddfUndoTop :: !(PrimVar.PrimVar s Int),
    ddfDomainCache :: !(SmallArray.SmallMutableArray s IntSet),
    ddfDomainValidBits :: !(PrimArray.MutablePrimArray s Word64),
    ddfAllFullSourcesWitnessed :: !Bool
  }

data DeltaUndoTarget
  = DeltaUndoSelected
  | DeltaUndoFullSource !Int
  | DeltaUndoDirtySource !Int
  | DeltaUndoInvalid
  deriving stock (Eq, Show)

selectedUndoSource :: Int
selectedUndoSource =
  -1
{-# INLINE selectedUndoSource #-}

sourceUndoCapacityForProblem :: DenseDeltaProblem -> Int
sourceUndoCapacityForProblem problem =
  PrimArray.foldlPrimArray' step 0 (ddpFullSchema problem)
  where
    step !acc !slotKey =
      acc
        + IntSet.size
          (IntMap.findWithDefault IntSet.empty slotKey (ddpSourcesBySlot problem))
{-# INLINE sourceUndoCapacityForProblem #-}

selectedUndoCapacityForProblem :: DenseDeltaProblem -> Int
selectedUndoCapacityForProblem problem =
  case ddpSelectedOutput problem of
    Nothing ->
      0
    Just _selected ->
      PrimArray.sizeofPrimArray (ddpOutputSchema problem)
{-# INLINE selectedUndoCapacityForProblem #-}

undoCapacityForProblem :: DenseDeltaProblem -> Int
undoCapacityForProblem problem =
  sourceUndoCapacityForProblem problem + selectedUndoCapacityForProblem problem
{-# INLINE undoCapacityForProblem #-}

deltaUndoCapacityForProblem :: DenseDeltaProblem -> Int
deltaUndoCapacityForProblem problem =
  2 * sourceUndoCapacityForProblem problem + selectedUndoCapacityForProblem problem
{-# INLINE deltaUndoCapacityForProblem #-}

selectedInitialRows :: DenseDeltaProblem -> RowSet
selectedInitialRows problem =
  case ddpSelectedOutput problem of
    Nothing ->
      emptyRowSet
    Just selected ->
      dsoRows selected
{-# INLINE selectedInitialRows #-}

sourceAt :: DenseDeltaProblem -> Int -> Maybe DeltaJoinSource
sourceAt problem sourceId =
  let sources = ddpSources problem
   in if sourceId < 0 || sourceId >= SmallArray.sizeofSmallArray sources
        then Nothing
        else Just (SmallArray.indexSmallArray sources sourceId)
{-# INLINE sourceAt #-}

foldDenseSourceIndexesM ::
  DenseDeltaProblem ->
  acc ->
  (acc -> Int -> ST s acc) ->
  ST s acc
foldDenseSourceIndexesM problem !initial step =
  go 0 initial
  where
    !sourceCount =
      SmallArray.sizeofSmallArray (ddpSources problem)

    go !ix !acc
      | ix >= sourceCount =
          pure acc
      | otherwise = do
          acc' <- step acc ix
          go (ix + 1) acc'
{-# INLINE foldDenseSourceIndexesM #-}

allSourcesWitnessedInitial :: DenseDeltaProblem -> Bool
allSourcesWitnessedInitial problem =
  go 0
  where
    !sourceCount =
      SmallArray.sizeofSmallArray (ddpSources problem)

    go !ix
      | ix >= sourceCount =
          True
      | otherwise =
          let src = SmallArray.indexSmallArray (ddpSources problem) ix
           in not (rowSetNull (deltaSourceRows src)) && go (ix + 1)
{-# INLINE allSourcesWitnessedInitial #-}

restrictRowsBySlotValueChanged ::
  DeltaJoinSource ->
  Int ->
  RepKey ->
  RowSet ->
  RowSetRestriction
restrictRowsBySlotValueChanged src slotKey (RepKey repKey) rows =
  case IntMap.lookup slotKey (deltaSourceValueIndex src) >>= IntMap.lookup repKey of
    Nothing ->
      RowSetRestrictionEmpty
    Just bucket ->
      rowSetIntersectionWithRowIdSetChanged bucket rows
{-# INLINE restrictRowsBySlotValueChanged #-}

slotValuesFromRows ::
  DeltaJoinSource ->
  Int ->
  RowSet ->
  IntSet
slotValuesFromRows src slotKey rows =
  case IntMap.lookup slotKey (deltaSourceValueIndex src) of
    Nothing ->
      IntSet.empty
    Just byRep
      | rowSetSize rows <= 64 || rowSetSize rows * 4 <= IntMap.size byRep ->
          rowSetFoldl'
            ( \acc rowId ->
                case deltaSourceValueAt src slotKey (rowIdInt rowId) of
                  Nothing -> acc
                  Just key -> IntSet.insert key acc
            )
            IntSet.empty
            rows
      | otherwise ->
          IntMap.foldlWithKey'
            ( \acc repKey bucket ->
                if rowSetIntersectsRowIdSet bucket rows
                  then IntSet.insert repKey acc
                  else acc
            )
            IntSet.empty
            byRep
{-# INLINE slotValuesFromRows #-}

selectedSlotValuesFromRows ::
  IntMap RowIdSet ->
  RowSet ->
  IntSet
selectedSlotValuesFromRows byRep rows =
  IntMap.foldlWithKey'
    ( \acc repKey bucket ->
        if rowSetIntersectsRowIdSet bucket rows
          then IntSet.insert repKey acc
          else acc
    )
    IntSet.empty
    byRep
{-# INLINE selectedSlotValuesFromRows #-}

combineCandidateDomains ::
  Maybe IntSet ->
  Maybe IntSet ->
  IntSet
combineCandidateDomains left right =
  case (left, right) of
    (Nothing, Nothing) ->
      IntSet.empty
    (Just domain, Nothing) ->
      domain
    (Nothing, Just domain) ->
      domain
    (Just leftDomain, Just rightDomain) ->
      IntSet.intersection leftDomain rightDomain
{-# INLINE combineCandidateDomains #-}

readFrameEnv ::
  DenseDeltaProblem ->
  PrimArray.MutablePrimArray s Int ->
  PrimArray.MutablePrimArray s Word64 ->
  Int ->
  ST s (Maybe RepKey)
readFrameEnv problem envValues envBoundBits slotKey
  | not (validSlotKey (ddpSlotUniverse problem) slotKey) =
      pure Nothing
  | otherwise = do
      bound <- unsafeTestSlotBit envBoundBits slotKey
      if not bound
        then pure Nothing
        else do
          value <- PrimArray.readPrimArray envValues slotKey
          pure (Just (RepKey value))
{-# INLINE readFrameEnv #-}

writeFrameEnv ::
  PrimArray.MutablePrimArray s Int ->
  PrimArray.MutablePrimArray s Word64 ->
  Int ->
  RepKey ->
  ST s ()
writeFrameEnv envValues envBoundBits slotKey (RepKey value) = do
  PrimArray.writePrimArray envValues slotKey value
  unsafeSetSlotBit envBoundBits slotKey
{-# INLINE writeFrameEnv #-}

clearFrameEnv ::
  DenseDeltaProblem ->
  PrimArray.MutablePrimArray s Word64 ->
  Int ->
  ST s ()
clearFrameEnv problem envBoundBits slotKey
  | not (validSlotKey (ddpSlotUniverse problem) slotKey) =
      pure ()
  | otherwise =
      unsafeClearSlotBit envBoundBits slotKey
{-# INLINE clearFrameEnv #-}

readSelectedRowsAt :: SmallArray.SmallMutableArray s RowSet -> ST s RowSet
readSelectedRowsAt selectedRows =
  SmallArray.readSmallArray selectedRows 0
{-# INLINE readSelectedRowsAt #-}

writeSelectedRowsAt :: SmallArray.SmallMutableArray s RowSet -> RowSet -> ST s ()
writeSelectedRowsAt selectedRows =
  SmallArray.writeSmallArray selectedRows 0
{-# INLINE writeSelectedRowsAt #-}

undoMarkAt :: PrimVar.PrimVar s Int -> ST s Int
undoMarkAt undoTop =
  PrimVar.readPrimVar undoTop
{-# INLINE undoMarkAt #-}

writeUndoTopAt :: PrimVar.PrimVar s Int -> Int -> ST s ()
writeUndoTopAt undoTop =
  PrimVar.writePrimVar undoTop
{-# INLINE writeUndoTopAt #-}

pushUndoRowAt ::
  PrimArray.MutablePrimArray s Int ->
  SmallArray.SmallMutableArray s RowSet ->
  PrimVar.PrimVar s Int ->
  Int ->
  RowSet ->
  ST s ()
pushUndoRowAt undoSources undoRows undoTop sourceMarker oldRows = do
  top <- undoMarkAt undoTop
  PrimArray.writePrimArray undoSources top sourceMarker
  SmallArray.writeSmallArray undoRows top oldRows
  writeUndoTopAt undoTop (top + 1)
{-# INLINE pushUndoRowAt #-}

invalidateFrameDomain ::
  DenseDeltaProblem ->
  PrimArray.MutablePrimArray s Word64 ->
  Int ->
  ST s ()
invalidateFrameDomain problem domainValidBits slotKey
  | not (validSlotKey (ddpSlotUniverse problem) slotKey) =
      pure ()
  | otherwise =
      unsafeClearSlotBit domainValidBits slotKey
{-# INLINE invalidateFrameDomain #-}

invalidateFrameSourceDomains ::
  DenseDeltaProblem ->
  PrimArray.MutablePrimArray s Word64 ->
  DeltaJoinSource ->
  ST s ()
invalidateFrameSourceDomains problem domainValidBits src =
  IntMap.foldrWithKey
    ( \slotKey _ action ->
        invalidateFrameDomain problem domainValidBits slotKey >> action
    )
    (pure ())
    (deltaSourceValueIndex src)
{-# INLINE invalidateFrameSourceDomains #-}

invalidateFrameSourceDomainsById ::
  DenseDeltaProblem ->
  PrimArray.MutablePrimArray s Word64 ->
  Int ->
  ST s ()
invalidateFrameSourceDomainsById problem domainValidBits sourceId =
  maybe
    (pure ())
    (invalidateFrameSourceDomains problem domainValidBits)
    (sourceAt problem sourceId)
{-# INLINE invalidateFrameSourceDomainsById #-}

invalidateFrameSelectedDomains ::
  DenseDeltaProblem ->
  PrimArray.MutablePrimArray s Word64 ->
  ST s ()
invalidateFrameSelectedDomains problem domainValidBits =
  case ddpSelectedOutput problem of
    Nothing ->
      pure ()
    Just _selected ->
      PrimArray.traversePrimArray_
        (invalidateFrameDomain problem domainValidBits)
        (ddpOutputSchema problem)
{-# INLINE invalidateFrameSelectedDomains #-}

bindSelectedSlotWith ::
  DenseDeltaProblem ->
  (Int -> Int -> ST s ()) ->
  (RowSet -> ST s ()) ->
  ST s RowSet ->
  (RowSet -> ST s ()) ->
  ST s () ->
  Int ->
  RepKey ->
  Int ->
  ST s Bool
bindSelectedSlotWith problem rollbackSlot pushSelectedUndo readSelectedRows writeSelectedRows invalidateSelected slotKey (RepKey repKey) mark =
  case ddpSelectedOutput problem >>= \selected -> IntMap.lookup slotKey (dsoRowsBySlotValue selected) of
    Nothing ->
      pure True
    Just byRep ->
      case IntMap.lookup repKey byRep of
        Nothing -> do
          rollbackSlot slotKey mark
          pure False
        Just bucket -> do
          oldRows <- readSelectedRows
          case rowSetIntersectionWithRowIdSetChanged bucket oldRows of
            RowSetRestrictionEmpty -> do
              rollbackSlot slotKey mark
              pure False
            RowSetRestrictionUnchanged ->
              pure True
            RowSetRestrictionChanged newRows -> do
              pushSelectedUndo oldRows
              writeSelectedRows newRows
              invalidateSelected
              pure True
{-# INLINE bindSelectedSlotWith #-}

computeCandidateDomainWith ::
  DenseDeltaProblem ->
  (Int -> ST s RowSet) ->
  ST s RowSet ->
  Int ->
  ST s IntSet
computeCandidateDomainWith problem readSourceRows readSelectedRows slotKey = do
  sourceDomain <- computeSourceCandidateDomainWith problem readSourceRows slotKey
  selectedDomain <- computeSelectedCandidateDomainWith problem readSelectedRows slotKey
  pure (combineCandidateDomains sourceDomain selectedDomain)
{-# INLINE computeCandidateDomainWith #-}

computeSourceCandidateDomainWith ::
  DenseDeltaProblem ->
  (Int -> ST s RowSet) ->
  Int ->
  ST s (Maybe IntSet)
computeSourceCandidateDomainWith problem readSourceRows slotKey =
  case IntMap.lookup slotKey (ddpSourcesBySlot problem) of
    Nothing ->
      pure Nothing
    Just sourceIds ->
      IntSet.foldr step finish sourceIds Nothing
  where
    finish :: Maybe IntSet -> ST s (Maybe IntSet)
    finish =
      pure

    step sourceId rest acc =
      case sourceAt problem sourceId of
        Nothing ->
          pure (Just IntSet.empty)
        Just src -> do
          rows <- readSourceRows sourceId
          let !sourceDomain =
                slotValuesFromRows src slotKey rows
              !domain =
                case acc of
                  Nothing ->
                    sourceDomain
                  Just current ->
                    IntSet.intersection current sourceDomain
          if IntSet.null domain
            then pure (Just IntSet.empty)
            else rest (Just domain)
{-# INLINE computeSourceCandidateDomainWith #-}

computeSelectedCandidateDomainWith ::
  DenseDeltaProblem ->
  ST s RowSet ->
  Int ->
  ST s (Maybe IntSet)
computeSelectedCandidateDomainWith problem readSelectedRows slotKey =
  case ddpSelectedOutput problem >>= \selected -> IntMap.lookup slotKey (dsoRowsBySlotValue selected) of
    Nothing ->
      pure Nothing
    Just byRep -> do
      rows <- readSelectedRows
      pure (Just (selectedSlotValuesFromRows byRep rows))
{-# INLINE computeSelectedCandidateDomainWith #-}

candidateDomainWith ::
  DenseDeltaProblem ->
  SmallArray.SmallMutableArray s IntSet ->
  PrimArray.MutablePrimArray s Word64 ->
  (Int -> ST s IntSet) ->
  Int ->
  ST s IntSet
candidateDomainWith problem domainCache domainValidBits compute slotKey
  | not (validSlotKey (ddpSlotUniverse problem) slotKey) =
      pure IntSet.empty
  | otherwise = do
      valid <- unsafeTestSlotBit domainValidBits slotKey
      if valid
        then SmallArray.readSmallArray domainCache slotKey
        else do
          domain <- compute slotKey
          SmallArray.writeSmallArray domainCache slotKey domain
          unsafeSetSlotBit domainValidBits slotKey
          pure domain
{-# INLINE candidateDomainWith #-}

leafTupleKeyWith ::
  (Int -> ST s (Maybe RepKey)) ->
  [Int] ->
  ST s (Maybe (TupleKey tupleRole))
leafTupleKeyWith readSlot schema = do
  values <- traverse readSlot schema
  pure (tupleKeyFromRepKeys <$> sequenceA values)
{-# INLINE leafTupleKeyWith #-}

foldDenseWCOJ ::
  forall acc.
  DenseDeltaProblem ->
  (forall s. DenseLeaf s -> acc -> ST s acc) ->
  acc ->
  acc
foldDenseWCOJ problem leaf initial =
  runST $ do
    let !slotUniverse = ddpSlotUniverse problem
        !sourceCount = SmallArray.sizeofSmallArray (ddpSources problem)
        !bitWordCount = bitWordsForSlots slotUniverse
        !undoCapacity = undoCapacityForProblem problem

    envValues <- PrimArray.newPrimArray slotUniverse
    envBoundBits <- PrimArray.newPrimArray bitWordCount
    PrimArray.setPrimArray envValues 0 slotUniverse 0
    PrimArray.setPrimArray envBoundBits 0 bitWordCount (0 :: Word64)

    feasible <-
      SmallArray.thawSmallArray
        (SmallArray.mapSmallArray' deltaSourceRows (ddpSources problem))
        0
        sourceCount

    selectedRows <- SmallArray.newSmallArray 1 (selectedInitialRows problem)

    undoSources <- PrimArray.newPrimArray undoCapacity
    undoRows <- SmallArray.newSmallArray undoCapacity emptyRowSet
    undoTop <- PrimVar.newPrimVar 0

    domainCache <- SmallArray.newSmallArray slotUniverse IntSet.empty
    domainValidBits <- PrimArray.newPrimArray bitWordCount
    PrimArray.setPrimArray domainValidBits 0 bitWordCount (0 :: Word64)

    let frame =
          DenseFrame
            { dfProblem = problem,
              dfEnvValues = envValues,
              dfEnvBoundBits = envBoundBits,
              dfFeasible = feasible,
              dfSelectedRows = selectedRows,
              dfUndoSources = undoSources,
              dfUndoRows = undoRows,
              dfUndoTop = undoTop,
              dfDomainCache = domainCache,
              dfDomainValidBits = domainValidBits,
              dfAllSourcesWitnessed = allSourcesWitnessedInitial problem
            }
        !unbound =
          PrimArray.foldlPrimArray'
            (\ !acc !slotKey -> IntSet.insert slotKey acc)
            IntSet.empty
            (ddpFullSchema problem)

    descendSlots
      frame
      unbound
      initial
      chooseNextSlot
      undoMark
      (\leafFrame slotKey repKey mark -> bindSlot leafFrame slotKey (RepKey repKey) mark)
      rollback
      (\leafFrame acc -> do
          witnessed <- allSourcesWitnessed leafFrame
          if witnessed then leaf leafFrame acc else pure acc
      )

readDenseEnv :: DenseFrame s -> Int -> ST s (Maybe RepKey)
readDenseEnv frame =
  readFrameEnv (dfProblem frame) (dfEnvValues frame) (dfEnvBoundBits frame)
{-# INLINE readDenseEnv #-}

writeEnv :: DenseFrame s -> Int -> RepKey -> ST s ()
writeEnv frame =
  writeFrameEnv (dfEnvValues frame) (dfEnvBoundBits frame)
{-# INLINE writeEnv #-}

clearEnv :: DenseFrame s -> Int -> ST s ()
clearEnv frame =
  clearFrameEnv (dfProblem frame) (dfEnvBoundBits frame)
{-# INLINE clearEnv #-}

readDenseFeasible :: DenseFrame s -> Int -> ST s RowSet
readDenseFeasible frame =
  SmallArray.readSmallArray (dfFeasible frame)
{-# INLINE readDenseFeasible #-}

writeFeasible :: DenseFrame s -> Int -> RowSet -> ST s ()
writeFeasible frame =
  SmallArray.writeSmallArray (dfFeasible frame)
{-# INLINE writeFeasible #-}

readSelectedRows :: DenseFrame s -> ST s RowSet
readSelectedRows =
  readSelectedRowsAt . dfSelectedRows
{-# INLINE readSelectedRows #-}

writeSelectedRows :: DenseFrame s -> RowSet -> ST s ()
writeSelectedRows =
  writeSelectedRowsAt . dfSelectedRows
{-# INLINE writeSelectedRows #-}

undoMark :: DenseFrame s -> ST s Int
undoMark =
  undoMarkAt . dfUndoTop
{-# INLINE undoMark #-}

writeUndoTop :: DenseFrame s -> Int -> ST s ()
writeUndoTop =
  writeUndoTopAt . dfUndoTop
{-# INLINE writeUndoTop #-}

pushUndoRow :: DenseFrame s -> Int -> RowSet -> ST s ()
pushUndoRow frame =
  pushUndoRowAt (dfUndoSources frame) (dfUndoRows frame) (dfUndoTop frame)
{-# INLINE pushUndoRow #-}

invalidateSourceDomains :: DenseFrame s -> DeltaJoinSource -> ST s ()
invalidateSourceDomains frame =
  invalidateFrameSourceDomains (dfProblem frame) (dfDomainValidBits frame)
{-# INLINE invalidateSourceDomains #-}

invalidateSourceDomainsById :: DenseFrame s -> Int -> ST s ()
invalidateSourceDomainsById frame =
  invalidateFrameSourceDomainsById (dfProblem frame) (dfDomainValidBits frame)
{-# INLINE invalidateSourceDomainsById #-}

invalidateSelectedDomains :: DenseFrame s -> ST s ()
invalidateSelectedDomains frame =
  invalidateFrameSelectedDomains (dfProblem frame) (dfDomainValidBits frame)
{-# INLINE invalidateSelectedDomains #-}

pushSelectedUndoRow :: DenseFrame s -> RowSet -> ST s ()
pushSelectedUndoRow frame =
  pushUndoRow frame selectedUndoSource
{-# INLINE pushSelectedUndoRow #-}

bindSelectedSlot ::
  DenseFrame s ->
  Int ->
  RepKey ->
  Int ->
  ST s Bool
bindSelectedSlot frame =
  bindSelectedSlotWith
    (dfProblem frame)
    (rollback frame)
    (pushSelectedUndoRow frame)
    (readSelectedRows frame)
    (writeSelectedRows frame)
    (invalidateSelectedDomains frame)
{-# INLINE bindSelectedSlot #-}

rollbackRowsTo :: DenseFrame s -> Int -> ST s ()
rollbackRowsTo frame mark = do
  top <- undoMark frame
  let go !ix
        | ix <= mark =
            writeUndoTop frame mark
        | otherwise = do
            let !entryIx = ix - 1
            sourceId <- PrimArray.readPrimArray (dfUndoSources frame) entryIx
            oldRows <- SmallArray.readSmallArray (dfUndoRows frame) entryIx
            if sourceId == selectedUndoSource
              then do
                writeSelectedRows frame oldRows
                invalidateSelectedDomains frame
              else do
                writeFeasible frame sourceId oldRows
                invalidateSourceDomainsById frame sourceId
            go entryIx
  go top
{-# INLINE rollbackRowsTo #-}

bindSlot ::
  DenseFrame s ->
  Int ->
  RepKey ->
  Int ->
  ST s Bool
bindSlot frame slotKey rep mark
  | not (validSlotKey (ddpSlotUniverse (dfProblem frame)) slotKey) =
      pure False
  | otherwise = do
      writeEnv frame slotKey rep
      selectedOk <- bindSelectedSlot frame slotKey rep mark
      if not selectedOk
        then pure False
        else
          case IntMap.lookup slotKey (ddpSourcesBySlot (dfProblem frame)) of
            Nothing ->
              pure True
            Just touched ->
              bindTouched (IntSet.toAscList touched)
  where
    bindTouched [] =
      pure True
    bindTouched (sourceId : rest) =
      case sourceAt (dfProblem frame) sourceId of
        Nothing ->
          bindTouched rest
        Just src -> do
          oldRows <- readDenseFeasible frame sourceId
          case restrictRowsBySlotValueChanged src slotKey rep oldRows of
            RowSetRestrictionEmpty -> do
              rollback frame slotKey mark
              pure False
            RowSetRestrictionUnchanged ->
              bindTouched rest
            RowSetRestrictionChanged newRows -> do
              pushUndoRow frame sourceId oldRows
              writeFeasible frame sourceId newRows
              invalidateSourceDomains frame src
              bindTouched rest
{-# INLINE bindSlot #-}

rollback :: DenseFrame s -> Int -> Int -> ST s ()
rollback frame slotKey undo = do
  clearEnv frame slotKey
  rollbackRowsTo frame undo
{-# INLINE rollback #-}

computeCandidateDomain ::
  DenseFrame s ->
  Int ->
  ST s IntSet
computeCandidateDomain frame =
  computeCandidateDomainWith (dfProblem frame) (readDenseFeasible frame) (readSelectedRows frame)
{-# INLINE computeCandidateDomain #-}

candidateDomain ::
  DenseFrame s ->
  Int ->
  ST s IntSet
candidateDomain frame =
  candidateDomainWith
    (dfProblem frame)
    (dfDomainCache frame)
    (dfDomainValidBits frame)
    (computeCandidateDomain frame)
{-# INLINE candidateDomain #-}

chooseNextSlot ::
  DenseFrame s ->
  IntSet ->
  ST s (Maybe (Int, IntSet))
chooseNextSlot frame unbound =
  IntSet.foldr step pure unbound Nothing
  where
    step slotKey rest best = do
      domain <- candidateDomain frame slotKey
      if IntSet.null domain
        then pure (Just (slotKey, domain))
        else rest (selectBest best (slotKey, domain))

    selectBest Nothing candidate =
      Just candidate
    selectBest (Just best) candidate
      | score candidate < score best =
          Just candidate
      | otherwise =
          Just best

    score (slotKey, domain) =
      ( IntSet.size domain,
        negate (IntSet.size (IntMap.findWithDefault IntSet.empty slotKey (ddpSourcesBySlot (dfProblem frame)))),
        IntMap.findWithDefault maxBound slotKey (ddpStaticRank (dfProblem frame)),
        slotKey
      )
{-# INLINE chooseNextSlot #-}

allSourcesWitnessed :: DenseFrame s -> ST s Bool
allSourcesWitnessed frame =
  pure (dfAllSourcesWitnessed frame)
{-# INLINE allSourcesWitnessed #-}

denseLeafTupleKey ::
  [Int] ->
  DenseLeaf s ->
  ST s (Maybe (TupleKey tupleRole))
denseLeafTupleKey schema frame =
  leafTupleKeyWith (readDenseEnv frame) schema
{-# INLINE denseLeafTupleKey #-}

foldDenseDeltaWCOJ ::
  forall acc.
  DenseDeltaProblem ->
  (forall s. DeltaDenseFrame s -> acc -> ST s acc) ->
  acc ->
  acc
foldDenseDeltaWCOJ problem leaf initial =
  runST $ do
    let !slotUniverse = ddpSlotUniverse problem
        !sourceCount = SmallArray.sizeofSmallArray (ddpSources problem)
        !bitWordCount = bitWordsForSlots slotUniverse
        !undoCapacity = deltaUndoCapacityForProblem problem
        !fullInitial = SmallArray.mapSmallArray' deltaSourceRows (ddpSources problem)
        !dirtyInitial = deltaInitialDirtyRows problem
        !dirtyLiveInitial = countNonEmptyRows dirtyInitial

    envValues <- PrimArray.newPrimArray slotUniverse
    envBoundBits <- PrimArray.newPrimArray bitWordCount
    PrimArray.setPrimArray envValues 0 slotUniverse 0
    PrimArray.setPrimArray envBoundBits 0 bitWordCount (0 :: Word64)

    fullFeasible <- SmallArray.thawSmallArray fullInitial 0 sourceCount
    dirtyFeasible <- SmallArray.thawSmallArray dirtyInitial 0 sourceCount
    selectedRows <- SmallArray.newSmallArray 1 (selectedInitialRows problem)

    dirtyLiveCount <- PrimVar.newPrimVar dirtyLiveInitial

    undoSources <- PrimArray.newPrimArray undoCapacity
    undoRows <- SmallArray.newSmallArray undoCapacity emptyRowSet
    undoTop <- PrimVar.newPrimVar 0

    domainCache <- SmallArray.newSmallArray slotUniverse IntSet.empty
    domainValidBits <- PrimArray.newPrimArray bitWordCount
    PrimArray.setPrimArray domainValidBits 0 bitWordCount (0 :: Word64)

    let frame =
          DeltaDenseFrame
            { ddfProblem = problem,
              ddfEnvValues = envValues,
              ddfEnvBoundBits = envBoundBits,
              ddfFullFeasible = fullFeasible,
              ddfDirtyFeasible = dirtyFeasible,
              ddfSelectedRows = selectedRows,
              ddfDirtyLiveCount = dirtyLiveCount,
              ddfUndoSources = undoSources,
              ddfUndoRows = undoRows,
              ddfUndoTop = undoTop,
              ddfDomainCache = domainCache,
              ddfDomainValidBits = domainValidBits,
              ddfAllFullSourcesWitnessed = allSourcesWitnessedInitial problem
            }
        !unbound =
          PrimArray.foldlPrimArray'
            (\ !acc !slotKey -> IntSet.insert slotKey acc)
            IntSet.empty
            (ddpFullSchema problem)

    descendSlots
      frame
      unbound
      initial
      chooseNextDeltaSlot
      deltaUndoMark
      (\leafFrame slotKey repKey mark -> bindSlotDelta leafFrame slotKey (RepKey repKey) mark)
      rollbackDelta
      (\leafFrame acc -> do
          dirtyLive <- readDeltaDirtyLiveCount leafFrame
          if ddfAllFullSourcesWitnessed leafFrame && dirtyLive > 0
            then leaf leafFrame acc
            else pure acc
      )

deltaInitialDirtyRows :: DenseDeltaProblem -> SmallArray RowSet
deltaInitialDirtyRows problem =
  SmallArray.mapSmallArray'
    (\src -> rowSetIntersection (deltaSourceRows src) (deltaSourceDirtyRows src))
    (ddpSources problem)
{-# INLINE deltaInitialDirtyRows #-}

countNonEmptyRows :: SmallArray RowSet -> Int
countNonEmptyRows rows =
  Foldable.foldl' countRow 0 rows
  where
    countRow :: Int -> RowSet -> Int
    countRow !acc rowSet
      | rowSetNull rowSet =
          acc
      | otherwise =
          acc + 1
{-# INLINE countNonEmptyRows #-}

deltaDirtyUndoSource :: Int -> Int
deltaDirtyUndoSource sourceId =
  negate sourceId - 2
{-# INLINE deltaDirtyUndoSource #-}

decodeDeltaUndoSource :: Int -> DeltaUndoTarget
decodeDeltaUndoSource marker
  | marker == selectedUndoSource =
      DeltaUndoSelected
  | marker >= 0 =
      DeltaUndoFullSource marker
  | marker <= -2 =
      DeltaUndoDirtySource (negate marker - 2)
  | otherwise =
      DeltaUndoInvalid
{-# INLINE decodeDeltaUndoSource #-}

readDeltaEnv :: DeltaDenseFrame s -> Int -> ST s (Maybe RepKey)
readDeltaEnv frame =
  readFrameEnv (ddfProblem frame) (ddfEnvValues frame) (ddfEnvBoundBits frame)
{-# INLINE readDeltaEnv #-}

writeDeltaEnv :: DeltaDenseFrame s -> Int -> RepKey -> ST s ()
writeDeltaEnv frame =
  writeFrameEnv (ddfEnvValues frame) (ddfEnvBoundBits frame)
{-# INLINE writeDeltaEnv #-}

clearDeltaEnv :: DeltaDenseFrame s -> Int -> ST s ()
clearDeltaEnv frame =
  clearFrameEnv (ddfProblem frame) (ddfEnvBoundBits frame)
{-# INLINE clearDeltaEnv #-}

readDeltaFullFeasible :: DeltaDenseFrame s -> Int -> ST s RowSet
readDeltaFullFeasible frame =
  SmallArray.readSmallArray (ddfFullFeasible frame)
{-# INLINE readDeltaFullFeasible #-}

writeDeltaFullFeasible :: DeltaDenseFrame s -> Int -> RowSet -> ST s ()
writeDeltaFullFeasible frame =
  SmallArray.writeSmallArray (ddfFullFeasible frame)
{-# INLINE writeDeltaFullFeasible #-}

readDeltaDirtyFeasible :: DeltaDenseFrame s -> Int -> ST s RowSet
readDeltaDirtyFeasible frame =
  SmallArray.readSmallArray (ddfDirtyFeasible frame)
{-# INLINE readDeltaDirtyFeasible #-}

writeDeltaDirtyFeasible :: DeltaDenseFrame s -> Int -> RowSet -> ST s ()
writeDeltaDirtyFeasible frame sourceId newRows = do
  oldRows <- readDeltaDirtyFeasible frame sourceId
  let !oldLive =
        not (rowSetNull oldRows)
      !newLive =
        not (rowSetNull newRows)
      !liveDelta =
        case (oldLive, newLive) of
          (False, True) -> 1
          (True, False) -> -1
          _ -> 0
  if liveDelta == 0
    then pure ()
    else PrimVar.modifyPrimVar (ddfDirtyLiveCount frame) (+ liveDelta)
  SmallArray.writeSmallArray (ddfDirtyFeasible frame) sourceId newRows
{-# INLINE writeDeltaDirtyFeasible #-}

readDeltaDirtyLiveCount :: DeltaDenseFrame s -> ST s Int
readDeltaDirtyLiveCount frame =
  PrimVar.readPrimVar (ddfDirtyLiveCount frame)
{-# INLINE readDeltaDirtyLiveCount #-}

readDeltaSelectedRows :: DeltaDenseFrame s -> ST s RowSet
readDeltaSelectedRows =
  readSelectedRowsAt . ddfSelectedRows
{-# INLINE readDeltaSelectedRows #-}

writeDeltaSelectedRows :: DeltaDenseFrame s -> RowSet -> ST s ()
writeDeltaSelectedRows =
  writeSelectedRowsAt . ddfSelectedRows
{-# INLINE writeDeltaSelectedRows #-}

deltaUndoMark :: DeltaDenseFrame s -> ST s Int
deltaUndoMark =
  undoMarkAt . ddfUndoTop
{-# INLINE deltaUndoMark #-}

writeDeltaUndoTop :: DeltaDenseFrame s -> Int -> ST s ()
writeDeltaUndoTop =
  writeUndoTopAt . ddfUndoTop
{-# INLINE writeDeltaUndoTop #-}

pushDeltaUndoRow :: DeltaDenseFrame s -> Int -> RowSet -> ST s ()
pushDeltaUndoRow frame =
  pushUndoRowAt (ddfUndoSources frame) (ddfUndoRows frame) (ddfUndoTop frame)
{-# INLINE pushDeltaUndoRow #-}

invalidateDeltaSourceDomains :: DeltaDenseFrame s -> DeltaJoinSource -> ST s ()
invalidateDeltaSourceDomains frame =
  invalidateFrameSourceDomains (ddfProblem frame) (ddfDomainValidBits frame)
{-# INLINE invalidateDeltaSourceDomains #-}

invalidateDeltaSourceDomainsById :: DeltaDenseFrame s -> Int -> ST s ()
invalidateDeltaSourceDomainsById frame =
  invalidateFrameSourceDomainsById (ddfProblem frame) (ddfDomainValidBits frame)
{-# INLINE invalidateDeltaSourceDomainsById #-}

invalidateDeltaSelectedDomains :: DeltaDenseFrame s -> ST s ()
invalidateDeltaSelectedDomains frame =
  invalidateFrameSelectedDomains (ddfProblem frame) (ddfDomainValidBits frame)
{-# INLINE invalidateDeltaSelectedDomains #-}

bindSelectedSlotDelta ::
  DeltaDenseFrame s ->
  Int ->
  RepKey ->
  Int ->
  ST s Bool
bindSelectedSlotDelta frame =
  bindSelectedSlotWith
    (ddfProblem frame)
    (rollbackDelta frame)
    (pushDeltaUndoRow frame selectedUndoSource)
    (readDeltaSelectedRows frame)
    (writeDeltaSelectedRows frame)
    (invalidateDeltaSelectedDomains frame)
{-# INLINE bindSelectedSlotDelta #-}

rollbackDeltaRowsTo :: DeltaDenseFrame s -> Int -> ST s ()
rollbackDeltaRowsTo frame mark = do
  top <- deltaUndoMark frame
  let restore !entryIx = do
        marker <- PrimArray.readPrimArray (ddfUndoSources frame) entryIx
        oldRows <- SmallArray.readSmallArray (ddfUndoRows frame) entryIx
        case decodeDeltaUndoSource marker of
          DeltaUndoSelected -> do
            writeDeltaSelectedRows frame oldRows
            invalidateDeltaSelectedDomains frame
          DeltaUndoFullSource sourceId -> do
            writeDeltaFullFeasible frame sourceId oldRows
            invalidateDeltaSourceDomainsById frame sourceId
          DeltaUndoDirtySource sourceId ->
            writeDeltaDirtyFeasible frame sourceId oldRows
          DeltaUndoInvalid ->
            pure ()
      go !ix
        | ix <= mark =
            writeDeltaUndoTop frame mark
        | otherwise =
            restore (ix - 1) >> go (ix - 1)
  go top
{-# INLINE rollbackDeltaRowsTo #-}

rollbackDelta :: DeltaDenseFrame s -> Int -> Int -> ST s ()
rollbackDelta frame slotKey undo = do
  clearDeltaEnv frame slotKey
  rollbackDeltaRowsTo frame undo
{-# INLINE rollbackDelta #-}

bindSlotDelta ::
  DeltaDenseFrame s ->
  Int ->
  RepKey ->
  Int ->
  ST s Bool
bindSlotDelta frame slotKey rep mark
  | not (validSlotKey (ddpSlotUniverse (ddfProblem frame)) slotKey) =
      pure False
  | otherwise = do
      writeDeltaEnv frame slotKey rep
      selectedOk <- bindSelectedSlotDelta frame slotKey rep mark
      if not selectedOk
        then pure False
        else
          case IntMap.lookup slotKey (ddpSourcesBySlot (ddfProblem frame)) of
            Nothing ->
              requireDeltaDirty frame slotKey mark
            Just touched -> do
              touchedOk <- bindDeltaTouchedSources frame slotKey rep mark (IntSet.toAscList touched)
              if touchedOk
                then requireDeltaDirty frame slotKey mark
                else pure False
{-# INLINE bindSlotDelta #-}

requireDeltaDirty :: DeltaDenseFrame s -> Int -> Int -> ST s Bool
requireDeltaDirty frame slotKey mark = do
  dirtyLive <- readDeltaDirtyLiveCount frame
  if dirtyLive > 0
    then pure True
    else do
      rollbackDelta frame slotKey mark
      pure False
{-# INLINE requireDeltaDirty #-}

bindDeltaTouchedSources ::
  DeltaDenseFrame s ->
  Int ->
  RepKey ->
  Int ->
  [Int] ->
  ST s Bool
bindDeltaTouchedSources frame slotKey rep mark =
  foldr bindTouchedSource (pure True)
  where
    bindTouchedSource sourceId rest =
      case sourceAt (ddfProblem frame) sourceId of
        Nothing ->
          rest
        Just src -> do
          fullOk <- restrictDeltaFullSource frame src sourceId slotKey rep mark
          if not fullOk
            then pure False
            else do
              restrictDeltaDirtySource frame src sourceId slotKey rep
              rest
{-# INLINE bindDeltaTouchedSources #-}

restrictDeltaFullSource ::
  DeltaDenseFrame s ->
  DeltaJoinSource ->
  Int ->
  Int ->
  RepKey ->
  Int ->
  ST s Bool
restrictDeltaFullSource frame src sourceId slotKey rep mark = do
  oldRows <- readDeltaFullFeasible frame sourceId
  case restrictRowsBySlotValueChanged src slotKey rep oldRows of
    RowSetRestrictionEmpty -> do
      rollbackDelta frame slotKey mark
      pure False
    RowSetRestrictionUnchanged ->
      pure True
    RowSetRestrictionChanged newRows -> do
      pushDeltaUndoRow frame sourceId oldRows
      writeDeltaFullFeasible frame sourceId newRows
      invalidateDeltaSourceDomains frame src
      pure True
{-# INLINE restrictDeltaFullSource #-}

restrictDeltaDirtySource ::
  DeltaDenseFrame s ->
  DeltaJoinSource ->
  Int ->
  Int ->
  RepKey ->
  ST s ()
restrictDeltaDirtySource frame src sourceId slotKey rep = do
  oldRows <- readDeltaDirtyFeasible frame sourceId
  if rowSetNull oldRows
    then pure ()
    else
      case restrictRowsBySlotValueChanged src slotKey rep oldRows of
        RowSetRestrictionEmpty -> do
          pushDeltaUndoRow frame (deltaDirtyUndoSource sourceId) oldRows
          writeDeltaDirtyFeasible frame sourceId emptyRowSet
        RowSetRestrictionUnchanged ->
          pure ()
        RowSetRestrictionChanged newRows -> do
          pushDeltaUndoRow frame (deltaDirtyUndoSource sourceId) oldRows
          writeDeltaDirtyFeasible frame sourceId newRows
{-# INLINE restrictDeltaDirtySource #-}

computeDeltaCandidateDomain ::
  DeltaDenseFrame s ->
  Int ->
  ST s IntSet
computeDeltaCandidateDomain frame =
  computeCandidateDomainWith (ddfProblem frame) (readDeltaFullFeasible frame) (readDeltaSelectedRows frame)
{-# INLINE computeDeltaCandidateDomain #-}

deltaFullCandidateDomain ::
  DeltaDenseFrame s ->
  Int ->
  ST s IntSet
deltaFullCandidateDomain frame =
  candidateDomainWith
    (ddfProblem frame)
    (ddfDomainCache frame)
    (ddfDomainValidBits frame)
    (computeDeltaCandidateDomain frame)
{-# INLINE deltaFullCandidateDomain #-}

chooseNextDeltaSlot ::
  DeltaDenseFrame s ->
  IntSet ->
  ST s (Maybe (Int, IntSet))
chooseNextDeltaSlot frame unbound =
  fmap (fmap dropScore) $
    IntSet.foldr step pure unbound Nothing
  where
    dropScore ::
      (Int, IntSet, (Int, Int, Int, Int, Int)) ->
      (Int, IntSet)
    dropScore (slotKey, domain, _scoreValue) =
      (slotKey, domain)

    step slotKey rest best = do
      fullDomain <- deltaFullCandidateDomain frame slotKey
      dirtyIncidence <- deltaDirtySourceIncidence frame slotKey
      dirtyLive <- readDeltaDirtyLiveCount frame
      dirtyDomain <- dirtyRestrictedDomain frame slotKey dirtyLive dirtyIncidence fullDomain
      let !domain =
            dirtyDomain
          !candidate =
            (slotKey, domain, deltaSlotScore slotKey domain dirtyIncidence)
      if IntSet.null domain
        then pure (Just candidate)
        else rest (selectBest best candidate)

    selectBest ::
      Maybe (Int, IntSet, (Int, Int, Int, Int, Int)) ->
      (Int, IntSet, (Int, Int, Int, Int, Int)) ->
      Maybe (Int, IntSet, (Int, Int, Int, Int, Int))
    selectBest Nothing candidate =
      Just candidate
    selectBest (Just best@(_, _, bestScore)) candidate@(_, _, candidateScore)
      | candidateScore < bestScore =
          Just candidate
      | otherwise =
          Just best

    deltaSlotScore slotKey domain dirtyIncidence =
      ( IntSet.size domain,
        negate dirtyIncidence,
        negate (IntSet.size (IntMap.findWithDefault IntSet.empty slotKey (ddpSourcesBySlot (ddfProblem frame)))),
        IntMap.findWithDefault maxBound slotKey (ddpStaticRank (ddfProblem frame)),
        slotKey
      )
{-# INLINE chooseNextDeltaSlot #-}

dirtyRestrictedDomain ::
  DeltaDenseFrame s ->
  Int ->
  Int ->
  Int ->
  IntSet ->
  ST s IntSet
dirtyRestrictedDomain frame slotKey dirtyLive dirtyIncidence fullDomain
  | dirtyLive > 0 && dirtyIncidence == dirtyLive && dirtyIncidence > 0 = do
      dirtyDomain <- deltaDirtyTouchedUnionDomain frame slotKey
      pure (IntSet.intersection fullDomain dirtyDomain)
  | otherwise =
      pure fullDomain
{-# INLINE dirtyRestrictedDomain #-}

deltaDirtySourceIncidence ::
  DeltaDenseFrame s ->
  Int ->
  ST s Int
deltaDirtySourceIncidence frame slotKey =
  case IntMap.lookup slotKey (ddpSourcesBySlot (ddfProblem frame)) of
    Nothing ->
      pure 0
    Just sourceIds ->
      foldr countDirtySource (pure 0) (IntSet.toAscList sourceIds)
  where
    countDirtySource sourceId rest = do
      acc <- rest
      rows <- readDeltaDirtyFeasible frame sourceId
      pure $
        if rowSetNull rows
          then acc
          else acc + 1
{-# INLINE deltaDirtySourceIncidence #-}

deltaDirtyTouchedUnionDomain ::
  DeltaDenseFrame s ->
  Int ->
  ST s IntSet
deltaDirtyTouchedUnionDomain frame slotKey =
  case IntMap.lookup slotKey (ddpSourcesBySlot (ddfProblem frame)) of
    Nothing ->
      pure IntSet.empty
    Just sourceIds ->
      foldr unionDirtyDomain (pure IntSet.empty) (IntSet.toAscList sourceIds)
  where
    unionDirtyDomain sourceId rest =
      case sourceAt (ddfProblem frame) sourceId of
        Nothing ->
          rest
        Just src -> do
          rows <- readDeltaDirtyFeasible frame sourceId
          acc <- rest
          pure $
            if rowSetNull rows
              then acc
              else IntSet.union (slotValuesFromRows src slotKey rows) acc
{-# INLINE deltaDirtyTouchedUnionDomain #-}

denseDeltaLeafTupleKey ::
  [Int] ->
  DeltaDenseFrame s ->
  ST s (Maybe (TupleKey tupleRole))
denseDeltaLeafTupleKey schema frame =
  leafTupleKeyWith (readDeltaEnv frame) schema
{-# INLINE denseDeltaLeafTupleKey #-}

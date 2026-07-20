{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

-- | Descent execution over section stores: prepared kernels, the ST
-- transaction bracket, and per-delta validation at the scale of the dirty
-- set.
module Moonlight.Sheaf.Section.Store.Descent.Execute
  ( descendPreparedLocalKeyed,
    descendAlgebraPreparedLocalKeyed,
    descendPreparedLocalKeyedBatch,
    descendAlgebraPreparedLocalKeyedBatch,
    prepareSectionProgram,
    prepareSectionObjectProgram,
    preparedSectionProgramLength,
    descendPreparedSectionProgram,
    descendAlgebraPreparedSectionProgram,
    SectionDescentTransaction,
    runSectionDescentTransaction,
    runAlgebraSectionDescentTransaction,
    transactKeyedSectionDelta,
    transactStalkAt,
  )
where

import Control.Monad.ST (ST, runST)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Foldable (traverse_)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Vector qualified as Vector
import Data.Vector.Mutable qualified as Mutable
import Data.Vector.Unboxed qualified as UVector
import Moonlight.Delta.Scope
  ( Scope,
    cleanScope,
    dirtyScope,
    foldScope,
    scopeNull,
    unionScope,
  )
import Moonlight.Sheaf.Section.Morphism
  ( checkRestriction,
    rTarget,
    restrictApply,
    restrictionMismatches,
  )
import Moonlight.Sheaf.Section.ObjectIndex
  ( ObjectKey (..),
    unObjectKey,
  )
import Moonlight.Sheaf.Section.Stalk
  ( StalkAlgebra,
    stalkMismatches,
  )
import Moonlight.Sheaf.Section.Store.Descent.FastPath.Internal
  ( applyFastDescentKernelUnit,
    compileFastEditKernel,
    fastEditProgramAt,
    fastEditKernelAt,
    runPreparedFinalFastValueStreamDescent,
    runPreparedFinalFastValueStreamDescentWithAlgebra,
  )
import Moonlight.Sheaf.Section.Store.Descent.Frontier
  ( DenseDescentArena (..),
    DenseGenerationFrontier,
    DenseRestrictionValidity,
    clearDenseDescentArena,
    clearDenseGenerationFrontier,
    denseGenerationFrontierNull,
    denseGenerationFrontierScope,
    foldDenseGenerationFrontierM,
    insertDenseGenerationFrontier,
    insertDenseGenerationIntSet,
    insertDenseGenerationRange,
    insertDenseGenerationVector,
    markRestrictionInvalid,
    markRestrictionValid,
    mergeDenseGenerationFrontier,
    newDenseDescentArena,
    newDenseGenerationFrontier,
    restrictionIsValid,
  )
import Moonlight.Sheaf.Section.Store.Descent.Rows
  ( foldPreparedRowsForObjectFrontierM,
    foldRestrictionRowsForIdsM,
    objectRestrictionIdsAt,
    restrictionRowForId,
  )
import Moonlight.Sheaf.Section.Store.Internal
  ( PreparedSectionInstruction (..),
    PreparedSectionProgram (..),
    advanceTotalSectionStore,
    sfepDirtyKeys,
  )
import Moonlight.Sheaf.Section.Store.State
  ( deltaScopeIsSingleton,
    denseSectionSize,
    validateScopeOrdinals,
    validateDescentAssignments,
  )
import Moonlight.Sheaf.Section.Store.Types

data DenseDescentSession s owner cell witness stalk mismatch repairObstruction = DenseDescentSession
  { ddsFastKernelSource :: !(DenseDescentKernelSource owner cell witness stalk mismatch repairObstruction),
    ddsPreparedDescent :: !(PreparedSectionDescent owner cell witness),
    ddsStalkAlgebra :: !(StalkAlgebra witness stalk mismatch repairObstruction),
    ddsMutableValues :: !(Mutable.MVector s stalk),
    ddsDirtyObjects :: !(DenseGenerationFrontier s),
    ddsArena :: !(DenseDescentArena s)
  }

data DenseDescentContext s owner cell witness stalk mismatch repairObstruction = DenseDescentContext
  { ddcSession :: !(DenseDescentSession s owner cell witness stalk mismatch repairObstruction),
    ddcRowMode :: !SectionDescentRowMode,
    ddcPinnedTargets :: !(PinnedDescentTarget stalk)
  }

data DenseDescentKernelSource owner cell witness stalk mismatch repairObstruction
  = CompileFastKernelsOnDemand
      !(PreparedSectionDescent owner cell witness)
      !(StalkAlgebra witness stalk mismatch repairObstruction)
  | UsePreparedFastKernels
      !(AlgebraPreparedSectionDescent owner cell witness stalk mismatch repairObstruction)

descendPreparedLocalKeyed ::
  Ord cell =>
  PreparedSectionDescent owner cell witness ->
  StalkAlgebra witness stalk mismatch repairObstruction ->
  SectionDescentObservation ->
  KeyedSectionDelta owner stalk ->
  TotalSectionStore owner cell stalk ->
  Either (SectionDescentError cell stalk mismatch) (SectionDescentResult owner cell stalk)
descendPreparedLocalKeyed preparedDescent stalkAlgebra observation delta store = do
  descentProgram <- descentProgramForSingleObservation observation store delta
  runPreparedDenseSectionProgramWithKernelSource
    (CompileFastKernelsOnDemand preparedDescent stalkAlgebra)
    descentProgram
    store

descendAlgebraPreparedLocalKeyed ::
  Ord cell =>
  AlgebraPreparedSectionDescent owner cell witness stalk mismatch repairObstruction ->
  SectionDescentObservation ->
  KeyedSectionDelta owner stalk ->
  TotalSectionStore owner cell stalk ->
  Either (SectionDescentError cell stalk mismatch) (SectionDescentResult owner cell stalk)
descendAlgebraPreparedLocalKeyed algebraPreparedDescent observation delta store = do
  descentProgram <- descentProgramForSingleObservation observation store delta
  runPreparedDenseSectionProgramWithAlgebra algebraPreparedDescent descentProgram store

descendPreparedLocalKeyedBatch ::
  Ord cell =>
  PreparedSectionDescent owner cell witness ->
  StalkAlgebra witness stalk mismatch repairObstruction ->
  SectionDescentObservation ->
  [KeyedSectionDelta owner stalk] ->
  TotalSectionStore owner cell stalk ->
  Either (SectionDescentError cell stalk mismatch) (SectionDescentResult owner cell stalk)
descendPreparedLocalKeyedBatch preparedDescent stalkAlgebra observation deltas store =
  case deltas of
    [delta] ->
      descendPreparedLocalKeyed preparedDescent stalkAlgebra observation delta store
    _ -> do
      descentProgram <- descentProgramForObservation observation store deltas
      runPreparedDenseSectionProgramWithKernelSource
        (CompileFastKernelsOnDemand preparedDescent stalkAlgebra)
        descentProgram
        store

descendAlgebraPreparedLocalKeyedBatch ::
  Ord cell =>
  AlgebraPreparedSectionDescent owner cell witness stalk mismatch repairObstruction ->
  SectionDescentObservation ->
  [KeyedSectionDelta owner stalk] ->
  TotalSectionStore owner cell stalk ->
  Either (SectionDescentError cell stalk mismatch) (SectionDescentResult owner cell stalk)
descendAlgebraPreparedLocalKeyedBatch algebraPreparedDescent observation deltas store =
  case deltas of
    [delta] ->
      descendAlgebraPreparedLocalKeyed algebraPreparedDescent observation delta store
    _ -> do
      descentProgram <- descentProgramForObservation observation store deltas
      runPreparedDenseSectionProgramWithAlgebra algebraPreparedDescent descentProgram store

prepareSectionProgram ::
  PreparedSectionDescent owner cell witness ->
  [KeyedSectionEdit owner stalk] ->
  Either (SectionStoreError cell) (PreparedSectionProgram owner stalk)
prepareSectionProgram preparedDescent edits = do
  instructions <- traverse (prepareSectionEditInstruction preparedDescent) edits
  pure
    PreparedSectionProgram
      { pspObjectCount = psdObjectCount preparedDescent,
        pspInstructions = Vector.fromList instructions
      }

prepareSectionObjectProgram ::
  PreparedSectionDescent owner cell witness ->
  ObjectKey ->
  Vector.Vector stalk ->
  Either (SectionStoreError cell) (PreparedSectionProgram owner stalk)
prepareSectionObjectProgram preparedDescent objectKey assignedValues
  | objectOrdinal < 0 || objectOrdinal >= psdObjectCount preparedDescent =
      Left (SectionStoreUnknownObjectKey objectKey)
  | otherwise =
      Right
        PreparedSectionProgram
          { pspObjectCount = psdObjectCount preparedDescent,
            pspInstructions =
              if Vector.null assignedValues
                then Vector.empty
                else Vector.singleton (PreparedSectionValueStream objectOrdinal assignedValues)
          }
  where
    objectOrdinal =
      unObjectKey objectKey

preparedSectionProgramLength :: PreparedSectionProgram owner stalk -> Int
preparedSectionProgramLength =
  Vector.foldl' (\count instruction -> count + preparedSectionInstructionLength instruction) 0
    . pspInstructions

preparedSectionInstructionLength :: PreparedSectionInstruction stalk -> Int
preparedSectionInstructionLength instruction =
  case instruction of
    PreparedSectionAssign _ _ ->
      1
    PreparedSectionValueStream _ assignedValues ->
      Vector.length assignedValues
    PreparedSectionDelta _ _ ->
      1

prepareSectionEditInstruction ::
  PreparedSectionDescent owner cell witness ->
  KeyedSectionEdit owner stalk ->
  Either (SectionStoreError cell) (PreparedSectionInstruction stalk)
prepareSectionEditInstruction preparedDescent edit
  | objectOrdinal < 0 || objectOrdinal >= psdObjectCount preparedDescent =
      Left (SectionStoreUnknownObjectKey objectKey)
  | otherwise =
      Right (PreparedSectionAssign objectOrdinal (kseValue edit))
  where
    objectKey =
      kseObjectKey edit
    objectOrdinal =
      unObjectKey objectKey

descendPreparedSectionProgram ::
  Ord cell =>
  PreparedSectionDescent owner cell witness ->
  StalkAlgebra witness stalk mismatch repairObstruction ->
  PreparedSectionProgram owner stalk ->
  TotalSectionStore owner cell stalk ->
  Either (SectionDescentError cell stalk mismatch) (SectionDescentResult owner cell stalk)
descendPreparedSectionProgram preparedDescent stalkAlgebra program store = do
  runPreparedDenseSectionProgramWithKernelSource
    (CompileFastKernelsOnDemand preparedDescent stalkAlgebra)
    program
    store

descendAlgebraPreparedSectionProgram ::
  Ord cell =>
  AlgebraPreparedSectionDescent owner cell witness stalk mismatch repairObstruction ->
  PreparedSectionProgram owner stalk ->
  TotalSectionStore owner cell stalk ->
  Either (SectionDescentError cell stalk mismatch) (SectionDescentResult owner cell stalk)
descendAlgebraPreparedSectionProgram algebraPreparedDescent program store = do
  runPreparedDenseSectionProgramWithAlgebra algebraPreparedDescent program store

data SectionDescentTransaction s owner cell witness stalk mismatch repairObstruction = SectionDescentTransaction
  { sdtSession :: !(DenseDescentSession s owner cell witness stalk mismatch repairObstruction),
    sdtAccumulator :: !(STRef s SectionDescentAccumulator),
    sdtObjectCount :: !Int
  }

runSectionDescentTransaction ::
  PreparedSectionDescent owner cell witness ->
  StalkAlgebra witness stalk mismatch repairObstruction ->
  TotalSectionStore owner cell stalk ->
  ( forall s.
    SectionDescentTransaction s owner cell witness stalk mismatch repairObstruction ->
    ST s (Either (SectionDescentError cell stalk mismatch) result)
  ) ->
  Either (SectionDescentError cell stalk mismatch) (result, SectionDescentResult owner cell stalk)
runSectionDescentTransaction preparedDescent stalkAlgebra =
  runSectionDescentTransactionWithKernelSource (CompileFastKernelsOnDemand preparedDescent stalkAlgebra)

runAlgebraSectionDescentTransaction ::
  AlgebraPreparedSectionDescent owner cell witness stalk mismatch repairObstruction ->
  TotalSectionStore owner cell stalk ->
  ( forall s.
    SectionDescentTransaction s owner cell witness stalk mismatch repairObstruction ->
    ST s (Either (SectionDescentError cell stalk mismatch) result)
  ) ->
  Either (SectionDescentError cell stalk mismatch) (result, SectionDescentResult owner cell stalk)
runAlgebraSectionDescentTransaction algebraPreparedDescent =
  runSectionDescentTransactionWithKernelSource (UsePreparedFastKernels algebraPreparedDescent)

runSectionDescentTransactionWithKernelSource ::
  DenseDescentKernelSource owner cell witness stalk mismatch repairObstruction ->
  TotalSectionStore owner cell stalk ->
  ( forall s.
    SectionDescentTransaction s owner cell witness stalk mismatch repairObstruction ->
    ST s (Either (SectionDescentError cell stalk mismatch) result)
  ) ->
  Either (SectionDescentError cell stalk mismatch) (result, SectionDescentResult owner cell stalk)
runSectionDescentTransactionWithKernelSource kernelSource store transactValue = do
  case
    runST
      ( do
          mutableValues <- Vector.thaw values
          dirtyObjects <- newDenseGenerationFrontier rowCount
          arena <- newDenseDescentArena rowCount restrictionCountValue
          accumulatorRef <- newSTRef initialDescentAccumulator
          transactionOutcome <-
            transactValue
              SectionDescentTransaction
                { sdtSession =
                    DenseDescentSession
                      { ddsFastKernelSource = kernelSource,
                        ddsPreparedDescent = preparedDescent,
                        ddsStalkAlgebra = kernelSourceStalkAlgebra kernelSource,
                        ddsMutableValues = mutableValues,
                        ddsDirtyObjects = dirtyObjects,
                        ddsArena = arena
                      },
                  sdtAccumulator = accumulatorRef,
                  sdtObjectCount = rowCount
                }
          case transactionOutcome of
            Left descentError ->
              pure (Left descentError)
            Right resultValue -> do
              descentAccumulator <- readSTRef accumulatorRef
              frozenValues <- Vector.freeze mutableValues
              descentScope <- dirtyScopeFromAccumulator dirtyObjects descentAccumulator
              pure (Right (resultValue, DenseSection frozenValues, descentAccumulator, descentScope))
      )
    of
    Left descentError ->
      Left descentError
    Right (resultValue, denseValues, descentAccumulator, descentScope) ->
      Right
        ( resultValue,
          SectionDescentResult
            { sdrSection =
                advanceTotalSectionStore denseValues descentScope store,
              sdrObservedSteps = sdaObservedSteps descentAccumulator
            }
        )
  where
    DenseSection values =
      totalSectionDenseValues store
    preparedDescent =
      kernelSourcePreparedDescent kernelSource
    rowCount =
      psdObjectCount preparedDescent
    restrictionCountValue =
      Vector.length (psdRowsByRestrictionId preparedDescent)

transactKeyedSectionDelta ::
  Ord cell =>
  SectionDescentTransaction s owner cell witness stalk mismatch repairObstruction ->
  KeyedSectionDelta owner stalk ->
  ST s (Either (SectionDescentError cell stalk mismatch) ())
transactKeyedSectionDelta transaction delta =
  case
    mapStoreError (validateScopeOrdinals (sdtObjectCount transaction) (ksdExtent delta))
      *> (() <$ mapStoreError (validateDescentAssignments (sdtObjectCount transaction) (ksdAssignments delta)))
      *> preflightDeltaPinnedConflicts
        (ddsPreparedDescent session)
        (ddsStalkAlgebra session)
        (ksdAssignments delta)
    of
    Left descentError ->
      pure (Left descentError)
    Right () -> do
      accumulator <- readSTRef (sdtAccumulator transaction)
      applied <- applyDenseDescentInstruction session (Right accumulator) (deltaInstruction delta)
      case applied of
        Left descentError ->
          pure (Left descentError)
        Right advancedAccumulator ->
          Right () <$ writeSTRef (sdtAccumulator transaction) advancedAccumulator
  where
    session =
      sdtSession transaction
{-# INLINE transactKeyedSectionDelta #-}

transactStalkAt ::
  SectionDescentTransaction s owner cell witness stalk mismatch repairObstruction ->
  ObjectKey ->
  ST s (Either (SectionDescentError cell stalk mismatch) stalk)
transactStalkAt transaction objectKey
  | objectOrdinal < 0 || objectOrdinal >= sdtObjectCount transaction =
      pure (Left (SectionDescentStoreFailed (SectionStoreUnknownObjectKey objectKey)))
  | otherwise =
      Right <$> Mutable.read (ddsMutableValues (sdtSession transaction)) objectOrdinal
  where
    objectOrdinal =
      unObjectKey objectKey

validatePreparedSectionProgram ::
  PreparedSectionDescent owner cell witness ->
  PreparedSectionProgram owner stalk ->
  Either (SectionDescentError cell stalk mismatch) ()
validatePreparedSectionProgram preparedDescent program
  | pspObjectCount program /= psdObjectCount preparedDescent =
      Left
        ( SectionDescentStoreFailed
            (SectionStoreObjectCountMismatch (pspObjectCount program) (psdObjectCount preparedDescent))
        )
  | otherwise =
      traverse_
        (validatePreparedSectionInstruction (psdObjectCount preparedDescent))
        (pspInstructions program)

validatePreparedSectionInstruction ::
  Int ->
  PreparedSectionInstruction stalk ->
  Either (SectionDescentError cell stalk mismatch) ()
validatePreparedSectionInstruction rowCount instruction =
  case instruction of
    PreparedSectionAssign objectOrdinal _assignedValue ->
      validatePreparedOrdinal rowCount objectOrdinal
    PreparedSectionValueStream objectOrdinal _assignedValues ->
      validatePreparedOrdinal rowCount objectOrdinal
    PreparedSectionDelta scope assignments ->
      mapStoreError (validateScopeOrdinals rowCount scope)
        *> (() <$ mapStoreError (validateDescentAssignments rowCount assignments))

descentProgramForObservation ::
  SectionDescentObservation ->
  TotalSectionStore owner cell stalk ->
  [KeyedSectionDelta owner stalk] ->
  Either (SectionDescentError cell stalk mismatch) (PreparedSectionProgram owner stalk)
descentProgramForObservation observation store deltas =
  case observation of
    ObserveEachStep ->
      pure
        PreparedSectionProgram
          { pspObjectCount = denseSectionSize (totalSectionDenseValues store),
            pspInstructions = Vector.fromList (fmap deltaInstruction deltas)
          }
    ObserveFinalSection ->
      finalSectionDescentProgram
        (denseSectionSize (totalSectionDenseValues store))
        deltas

descentProgramForSingleObservation ::
  SectionDescentObservation ->
  TotalSectionStore owner cell stalk ->
  KeyedSectionDelta owner stalk ->
  Either (SectionDescentError cell stalk mismatch) (PreparedSectionProgram owner stalk)
descentProgramForSingleObservation observation store delta =
  case observation of
    ObserveEachStep ->
      pure
        PreparedSectionProgram
          { pspObjectCount = denseSectionSize (totalSectionDenseValues store),
            pspInstructions = Vector.singleton (deltaInstruction delta)
          }
    ObserveFinalSection ->
      pure
        ( finalSectionStateToProgram
            (denseSectionSize (totalSectionDenseValues store))
            (advanceFinalSectionDeltaState FinalSectionDeltaEmpty delta)
        )

data CoalescedSectionDelta stalk = CoalescedSectionDelta
  { csdAssignments :: !(IntMap stalk),
    csdExtent :: !(Scope IntSet)
  }

data FinalSectionDeltaState stalk
  = FinalSectionDeltaEmpty
  | FinalSectionDeltaSingleton !Int !stalk
  | FinalSectionDeltaCoalesced !(CoalescedSectionDelta stalk)

finalSectionDescentProgram ::
  Int ->
  [KeyedSectionDelta owner stalk] ->
  Either (SectionDescentError cell stalk mismatch) (PreparedSectionProgram owner stalk)
finalSectionDescentProgram objectCountValue deltas =
  finalSectionStateToProgram objectCountValue
    <$> List.foldl'
      finalSectionDeltaStep
      (Right FinalSectionDeltaEmpty)
      deltas

finalSectionDeltaStep ::
  Either (SectionDescentError cell stalk mismatch) (FinalSectionDeltaState stalk) ->
  KeyedSectionDelta owner stalk ->
  Either (SectionDescentError cell stalk mismatch) (FinalSectionDeltaState stalk)
finalSectionDeltaStep stateResult delta =
  case stateResult of
    Left descentError ->
      Left descentError
    Right stateValue ->
      Right (advanceFinalSectionDeltaState stateValue delta)

advanceFinalSectionDeltaState ::
  FinalSectionDeltaState stalk ->
  KeyedSectionDelta owner stalk ->
  FinalSectionDeltaState stalk
advanceFinalSectionDeltaState stateValue delta =
  case stateValue of
    FinalSectionDeltaCoalesced coalesced ->
      FinalSectionDeltaCoalesced (coalesceDeltaUnchecked coalesced delta)
    FinalSectionDeltaEmpty ->
      case singletonDeltaShape delta of
        FinalSectionDeltaEmpty ->
          FinalSectionDeltaEmpty
        singletonState@(FinalSectionDeltaSingleton _ _) ->
          singletonState
        FinalSectionDeltaCoalesced _ ->
          FinalSectionDeltaCoalesced (coalesceDeltaUnchecked emptyCoalescedSectionDelta delta)
    FinalSectionDeltaSingleton objectOrdinal stalk ->
      case singletonDeltaShape delta of
        FinalSectionDeltaEmpty ->
          stateValue
        nextState@(FinalSectionDeltaSingleton nextObjectOrdinal _)
          | nextObjectOrdinal == objectOrdinal ->
              nextState
          | otherwise ->
              FinalSectionDeltaCoalesced (coalesceDeltaUnchecked (singletonCoalescedSectionDelta objectOrdinal stalk) delta)
        FinalSectionDeltaCoalesced _ ->
          FinalSectionDeltaCoalesced (coalesceDeltaUnchecked (singletonCoalescedSectionDelta objectOrdinal stalk) delta)

finalSectionStateToProgram ::
  Int ->
  FinalSectionDeltaState stalk ->
  PreparedSectionProgram owner stalk
finalSectionStateToProgram objectCountValue stateValue =
  PreparedSectionProgram
    { pspObjectCount = objectCountValue,
      pspInstructions =
        case stateValue of
          FinalSectionDeltaEmpty ->
            Vector.empty
          FinalSectionDeltaSingleton objectOrdinal stalk ->
            Vector.singleton (PreparedSectionAssign objectOrdinal stalk)
          FinalSectionDeltaCoalesced coalesced
            | IntMap.null (csdAssignments coalesced) && scopeNull (csdExtent coalesced) ->
                Vector.empty
            | otherwise ->
                Vector.singleton
                  (PreparedSectionDelta (csdExtent coalesced) (csdAssignments coalesced))
    }

deltaInstruction :: KeyedSectionDelta owner stalk -> PreparedSectionInstruction stalk
deltaInstruction delta =
  PreparedSectionDelta (ksdExtent delta) (ksdAssignments delta)

singletonDeltaShape :: KeyedSectionDelta owner stalk -> FinalSectionDeltaState stalk
singletonDeltaShape delta =
  case IntMap.minViewWithKey (ksdAssignments delta) of
    Nothing
      | scopeNull (ksdExtent delta) ->
          FinalSectionDeltaEmpty
      | otherwise ->
          FinalSectionDeltaCoalesced emptyCoalescedSectionDelta
    Just ((objectOrdinal, stalk), remainingAssignments)
      | IntMap.null remainingAssignments,
        deltaScopeIsSingleton objectOrdinal (ksdExtent delta) ->
          FinalSectionDeltaSingleton objectOrdinal stalk
      | otherwise ->
          FinalSectionDeltaCoalesced emptyCoalescedSectionDelta

emptyCoalescedSectionDelta :: CoalescedSectionDelta stalk
emptyCoalescedSectionDelta =
  CoalescedSectionDelta
    { csdAssignments = IntMap.empty,
      csdExtent = cleanScope
    }

singletonCoalescedSectionDelta :: Int -> stalk -> CoalescedSectionDelta stalk
singletonCoalescedSectionDelta objectOrdinal stalk =
  CoalescedSectionDelta
    { csdAssignments = IntMap.singleton objectOrdinal stalk,
      csdExtent = dirtyScope (IntSet.singleton objectOrdinal)
    }

coalesceDeltaUnchecked :: CoalescedSectionDelta stalk -> KeyedSectionDelta owner stalk -> CoalescedSectionDelta stalk
coalesceDeltaUnchecked coalesced delta =
  CoalescedSectionDelta
    { csdAssignments = IntMap.union (ksdAssignments delta) (csdAssignments coalesced),
      csdExtent = unionScope (csdExtent coalesced) (ksdExtent delta)
    }

runPreparedDenseSectionProgramWithAlgebra ::
  Ord cell =>
  AlgebraPreparedSectionDescent owner cell witness stalk mismatch repairObstruction ->
  PreparedSectionProgram owner stalk ->
  TotalSectionStore owner cell stalk ->
  Either (SectionDescentError cell stalk mismatch) (SectionDescentResult owner cell stalk)
runPreparedDenseSectionProgramWithAlgebra algebraPreparedDescent =
  runPreparedDenseSectionProgramWithKernelSource (UsePreparedFastKernels algebraPreparedDescent)

runPreparedDenseSectionProgramWithKernelSource ::
  Ord cell =>
  DenseDescentKernelSource owner cell witness stalk mismatch repairObstruction ->
  PreparedSectionProgram owner stalk ->
  TotalSectionStore owner cell stalk ->
  Either (SectionDescentError cell stalk mismatch) (SectionDescentResult owner cell stalk)
runPreparedDenseSectionProgramWithKernelSource kernelSource program store = do
  let preparedDescent =
        kernelSourcePreparedDescent kernelSource
      stalkAlgebra =
        kernelSourceStalkAlgebra kernelSource
  validatePreparedSectionProgram preparedDescent program
  if Vector.null instructions
    then Right SectionDescentResult {sdrSection = store, sdrObservedSteps = 0}
    else
      case singleObjectProgramValuesUnchecked instructions of
        Just (objectOrdinal, assignedValues) ->
          case fastEditProgramAt objectOrdinal preparedDescent of
            Just editProgram ->
              runPreparedFinalFastValueStreamDescentWithKernelSource
                kernelSource
                objectOrdinal
                editProgram
                assignedValues
                (Vector.length assignedValues)
                store
            Nothing ->
              runPreparedDenseSectionProgramGenericWithKernelSource kernelSource program store
        Nothing -> do
          preflightPreparedProgramPinnedConflicts preparedDescent stalkAlgebra program
          runPreparedDenseSectionProgramGenericWithKernelSource kernelSource program store
  where
    instructions =
      pspInstructions program

kernelSourcePreparedDescent ::
  DenseDescentKernelSource owner cell witness stalk mismatch repairObstruction ->
  PreparedSectionDescent owner cell witness
kernelSourcePreparedDescent kernelSource =
  case kernelSource of
    CompileFastKernelsOnDemand preparedDescent _ ->
      preparedDescent
    UsePreparedFastKernels algebraPreparedDescent ->
      apsdPreparedDescent algebraPreparedDescent
{-# INLINE kernelSourcePreparedDescent #-}

kernelSourceStalkAlgebra ::
  DenseDescentKernelSource owner cell witness stalk mismatch repairObstruction ->
  StalkAlgebra witness stalk mismatch repairObstruction
kernelSourceStalkAlgebra kernelSource =
  case kernelSource of
    CompileFastKernelsOnDemand _ stalkAlgebra ->
      stalkAlgebra
    UsePreparedFastKernels algebraPreparedDescent ->
      apsdStalkAlgebra algebraPreparedDescent
{-# INLINE kernelSourceStalkAlgebra #-}

kernelSourceFastKernel ::
  Int ->
  SectionFastEditProgram owner cell witness ->
  DenseDescentKernelSource owner cell witness stalk mismatch repairObstruction ->
  Either (SectionDescentError cell stalk mismatch) (SectionFastEditKernel owner stalk)
kernelSourceFastKernel objectOrdinal editProgram kernelSource =
  case kernelSource of
    CompileFastKernelsOnDemand preparedDescent stalkAlgebra ->
      compileFastEditKernel preparedDescent stalkAlgebra editProgram
    UsePreparedFastKernels algebraPreparedDescent ->
      fastEditKernelAt objectOrdinal algebraPreparedDescent >>= \maybeFastKernel ->
        case maybeFastKernel of
          Just fastKernel ->
            Right fastKernel
          Nothing ->
            Left (SectionDescentStoreFailed (SectionStoreUnknownObjectKey (ObjectKey objectOrdinal)))
{-# INLINE kernelSourceFastKernel #-}

runPreparedFinalFastValueStreamDescentWithKernelSource ::
  DenseDescentKernelSource owner cell witness stalk mismatch repairObstruction ->
  Int ->
  SectionFastEditProgram owner cell witness ->
  Vector.Vector stalk ->
  Int ->
  TotalSectionStore owner cell stalk ->
  Either (SectionDescentError cell stalk mismatch) (SectionDescentResult owner cell stalk)
runPreparedFinalFastValueStreamDescentWithKernelSource kernelSource objectOrdinal editProgram assignedValues assignedCount store =
  case kernelSource of
    CompileFastKernelsOnDemand preparedDescent stalkAlgebra ->
      runPreparedFinalFastValueStreamDescent
        preparedDescent
        stalkAlgebra
        objectOrdinal
        editProgram
        assignedValues
        assignedCount
        store
    UsePreparedFastKernels algebraPreparedDescent ->
      runPreparedFinalFastValueStreamDescentWithAlgebra
        algebraPreparedDescent
        objectOrdinal
        assignedValues
        assignedCount
        store

singleObjectProgramValuesUnchecked ::
  Vector.Vector (PreparedSectionInstruction stalk) ->
  Maybe (Int, Vector.Vector stalk)
singleObjectProgramValuesUnchecked instructions
  | Vector.length instructions == 1 =
      case Vector.unsafeHead instructions of
        PreparedSectionValueStream objectOrdinal assignedValues ->
          Just (objectOrdinal, assignedValues)
        instruction -> do
          (objectOrdinal, assignedValue) <- singletonInstructionAssignmentUnchecked instruction
          pure (objectOrdinal, Vector.singleton assignedValue)
  | otherwise =
      case Vector.uncons instructions of
        Nothing ->
          Nothing
        Just (firstInstruction, _remainingInstructions) -> do
          (objectOrdinal, _firstValue) <- singletonInstructionAssignmentUnchecked firstInstruction
          assignedValues <- traverse (singletonInstructionValueAtUnchecked objectOrdinal) instructions
          pure (objectOrdinal, assignedValues)

singletonInstructionAssignmentUnchecked ::
  PreparedSectionInstruction stalk ->
  Maybe (Int, stalk)
singletonInstructionAssignmentUnchecked instruction =
  case instruction of
    PreparedSectionAssign objectOrdinal stalk ->
      Just (objectOrdinal, stalk)
    PreparedSectionValueStream objectOrdinal assignedValues
      | Vector.length assignedValues == 1 ->
          Just (objectOrdinal, Vector.unsafeHead assignedValues)
    PreparedSectionValueStream _ _ ->
      Nothing
    PreparedSectionDelta scope assignments ->
      case singletonDescentAssignmentUnchecked assignments of
        Just assignment@(objectOrdinal, _)
          | deltaScopeIsSingleton objectOrdinal scope ->
              Just assignment
        _ ->
          Nothing

singletonInstructionValueAtUnchecked ::
  Int ->
  PreparedSectionInstruction stalk ->
  Maybe stalk
singletonInstructionValueAtUnchecked objectOrdinal instruction =
  case singletonInstructionAssignmentUnchecked instruction of
    Just (assignmentOrdinal, assignedValue)
      | assignmentOrdinal == objectOrdinal ->
          Just assignedValue
    _ ->
      Nothing

singletonDescentAssignmentUnchecked :: IntMap stalk -> Maybe (Int, stalk)
singletonDescentAssignmentUnchecked assignments =
  case IntMap.minViewWithKey assignments of
    Just (assignment, remainingAssignments)
      | IntMap.null remainingAssignments ->
          Just assignment
    _ ->
      Nothing

preflightPreparedProgramPinnedConflicts ::
  PreparedSectionDescent owner cell witness ->
  StalkAlgebra witness stalk mismatch repairObstruction ->
  PreparedSectionProgram owner stalk ->
  Either (SectionDescentError cell stalk mismatch) ()
preflightPreparedProgramPinnedConflicts preparedDescent stalkAlgebra program =
  Vector.foldM' preflightInstruction () (pspInstructions program)
  where
    preflightInstruction () instruction =
      case instruction of
        PreparedSectionDelta _ assignments ->
          preflightDeltaPinnedConflicts preparedDescent stalkAlgebra assignments
        PreparedSectionAssign _ _ ->
          Right ()
        PreparedSectionValueStream _ _ ->
          Right ()

preflightDeltaPinnedConflicts ::
  PreparedSectionDescent owner cell witness ->
  StalkAlgebra witness stalk mismatch repairObstruction ->
  IntMap stalk ->
  Either (SectionDescentError cell stalk mismatch) ()
preflightDeltaPinnedConflicts preparedDescent stalkAlgebra assignments
  | IntMap.size assignments < 2 =
      Right ()
  | otherwise =
      preflightAssignedPinnedConflicts preparedDescent stalkAlgebra assignments

preflightAssignedPinnedConflicts ::
  PreparedSectionDescent owner cell witness ->
  StalkAlgebra witness stalk mismatch repairObstruction ->
  IntMap stalk ->
  Either (SectionDescentError cell stalk mismatch) ()
preflightAssignedPinnedConflicts preparedDescent stalkAlgebra assignments =
  IntMap.foldlWithKey' preflightSource (Right ()) assignments
  where
    preflightSource state sourceOrdinal sourceValue =
      case state of
        Left descentError ->
          Left descentError
        Right () -> do
          preflightSourcePinnedConflicts
            preparedDescent
            stalkAlgebra
            (PinnedDescentAssignments assignments)
            sourceOrdinal
            sourceValue

preflightSourcePinnedConflicts ::
  PreparedSectionDescent owner cell witness ->
  StalkAlgebra witness stalk mismatch repairObstruction ->
  PinnedDescentTarget stalk ->
  Int ->
  stalk ->
  Either (SectionDescentError cell stalk mismatch) ()
preflightSourcePinnedConflicts preparedDescent stalkAlgebra pinnedTargets sourceOrdinal sourceValue =
  UVector.foldl'
    preflightRestriction
    (Right ())
    (objectRestrictionIdsAt sourceOrdinal (psdvOutgoingRestrictionIdsByObject (psdViews preparedDescent)))
  where
    preflightRestriction state restrictionKey =
      case state of
        Left descentError ->
          Left descentError
        Right () -> do
          row <- restrictionRowForId preparedDescent restrictionKey
          case lookupPinnedDescentTarget (sdrTargetOrdinal row) pinnedTargets of
            Nothing ->
              Right ()
            Just pinnedValue ->
              pinnedRestrictionConflict
                stalkAlgebra
                row
                (restrictApply stalkAlgebra (sdrRestriction row) sourceValue)
                pinnedValue

pinnedRestrictionConflict ::
  StalkAlgebra witness stalk mismatch repairObstruction ->
  SectionDescentRestrictionRow cell witness ->
  stalk ->
  stalk ->
  Either (SectionDescentError cell stalk mismatch) ()
pinnedRestrictionConflict stalkAlgebra row restrictedValue pinnedValue =
  case stalkMismatches stalkAlgebra restrictedValue pinnedValue of
    [] ->
      Right ()
    mismatches ->
      Left
        ( SectionDescentPinnedConflict
            (rTarget (sdrRestriction row))
            pinnedValue
            restrictedValue
            mismatches
        )

lookupPinnedDescentTarget :: Int -> PinnedDescentTarget stalk -> Maybe stalk
lookupPinnedDescentTarget targetOrdinal pinnedTargets =
  case pinnedTargets of
    PinnedDescentAssignments assignments ->
      IntMap.lookup targetOrdinal assignments
    PinnedDescentSingleton pinnedOrdinal pinnedValue
      | targetOrdinal == pinnedOrdinal ->
          Just pinnedValue
      | otherwise ->
          Nothing

validatePreparedOrdinal ::
  Int ->
  Int ->
  Either (SectionDescentError cell stalk mismatch) ()
validatePreparedOrdinal rowCount objectOrdinal
  | objectOrdinal < 0 || objectOrdinal >= rowCount =
      Left (SectionDescentStoreFailed (SectionStoreUnknownObjectKey (ObjectKey objectOrdinal)))
  | otherwise =
      Right ()

mapStoreError ::
  Either (SectionStoreError cell) value ->
  Either (SectionDescentError cell stalk mismatch) value
mapStoreError =
  either (Left . SectionDescentStoreFailed) Right

runPreparedDenseSectionProgramGenericWithKernelSource ::
  Ord cell =>
  DenseDescentKernelSource owner cell witness stalk mismatch repairObstruction ->
  PreparedSectionProgram owner stalk ->
  TotalSectionStore owner cell stalk ->
  Either (SectionDescentError cell stalk mismatch) (SectionDescentResult owner cell stalk)
runPreparedDenseSectionProgramGenericWithKernelSource kernelSource program store =
  runDenseSectionMutationWithKernelSource kernelSource store $ \session ->
    Vector.foldM'
      (applyDenseDescentInstruction session)
      (Right initialDescentAccumulator)
      (pspInstructions program)

initialDescentAccumulator :: SectionDescentAccumulator
initialDescentAccumulator =
  SectionDescentAccumulator
    { sdaDirtyCoverage = DescentDirtyObjects,
      sdaObservedSteps = 0
    }

runDenseSectionMutationWithKernelSource ::
  DenseDescentKernelSource owner cell witness stalk mismatch repairObstruction ->
  TotalSectionStore owner cell stalk ->
  ( forall s.
    DenseDescentSession s owner cell witness stalk mismatch repairObstruction ->
    ST s (Either (SectionDescentError cell stalk mismatch) SectionDescentAccumulator)
  ) ->
  Either (SectionDescentError cell stalk mismatch) (SectionDescentResult owner cell stalk)
runDenseSectionMutationWithKernelSource kernelSource store mutateSession =
  case
    runST
      ( do
          mutableValues <- Vector.thaw values
          dirtyObjects <- newDenseGenerationFrontier rowCount
          arena <- newDenseDescentArena rowCount restrictionCountValue
          descentResult <-
            mutateSession
              DenseDescentSession
                { ddsFastKernelSource = kernelSource,
                  ddsPreparedDescent = preparedDescent,
                  ddsStalkAlgebra = kernelSourceStalkAlgebra kernelSource,
                  ddsMutableValues = mutableValues,
                  ddsDirtyObjects = dirtyObjects,
                  ddsArena = arena
                }
          case descentResult of
            Left descentError ->
              pure (Left descentError)
            Right descentAccumulator -> do
              frozenValues <- Vector.freeze mutableValues
              descentScope <- dirtyScopeFromAccumulator dirtyObjects descentAccumulator
              pure (Right (DenseSection frozenValues, descentAccumulator, descentScope))
      )
  of
    Left descentError ->
      Left descentError
    Right (denseValues, descentAccumulator, descentScope) ->
      let nextSection =
            advanceTotalSectionStore denseValues descentScope store
       in Right
            SectionDescentResult
              { sdrSection = nextSection,
                sdrObservedSteps = sdaObservedSteps descentAccumulator
              }
  where
    DenseSection values =
      totalSectionDenseValues store
    preparedDescent =
      kernelSourcePreparedDescent kernelSource
    rowCount =
      psdObjectCount preparedDescent
    restrictionCountValue =
      Vector.length (psdRowsByRestrictionId preparedDescent)

dirtyScopeFromAccumulator ::
  DenseGenerationFrontier s ->
  SectionDescentAccumulator ->
  ST s (Scope IntSet)
dirtyScopeFromAccumulator dirtyObjects accumulator =
  denseGenerationFrontierScope (dirtyCoverageIsFull (sdaDirtyCoverage accumulator)) dirtyObjects
{-# INLINE dirtyScopeFromAccumulator #-}

recordDescentStepCompleted ::
  DescentDirtyCoverage ->
  SectionDescentAccumulator ->
  SectionDescentAccumulator
recordDescentStepCompleted dirtyCoverage accumulator =
  accumulator
    { sdaDirtyCoverage = joinDirtyCoverage (sdaDirtyCoverage accumulator) dirtyCoverage,
      sdaObservedSteps = sdaObservedSteps accumulator + 1
    }
{-# INLINE recordDescentStepCompleted #-}

dirtyCoverageIsFull :: DescentDirtyCoverage -> Bool
dirtyCoverageIsFull coverage =
  case coverage of
    DescentDirtyObjects ->
      False
    DescentDirtyFull ->
      True
{-# INLINE dirtyCoverageIsFull #-}

joinDirtyCoverage :: DescentDirtyCoverage -> DescentDirtyCoverage -> DescentDirtyCoverage
joinDirtyCoverage leftCoverage rightCoverage =
  case (leftCoverage, rightCoverage) of
    (DescentDirtyFull, _) ->
      DescentDirtyFull
    (_, DescentDirtyFull) ->
      DescentDirtyFull
    _ ->
      DescentDirtyObjects
{-# INLINE joinDirtyCoverage #-}

spendFrontierClosureBudget :: FrontierClosureBudget -> Maybe FrontierClosureBudget
spendFrontierClosureBudget (FrontierClosureBudget budget)
  | budget <= 0 = Nothing
  | otherwise = Just (FrontierClosureBudget (budget - 1))
{-# INLINE spendFrontierClosureBudget #-}

applyDenseDescentInstruction ::
  Ord cell =>
  DenseDescentSession s owner cell witness stalk mismatch repairObstruction ->
  Either (SectionDescentError cell stalk mismatch) SectionDescentAccumulator ->
  PreparedSectionInstruction stalk ->
  ST s (Either (SectionDescentError cell stalk mismatch) SectionDescentAccumulator)
applyDenseDescentInstruction session descentState instruction =
  case descentState of
    Left descentError ->
      pure (Left descentError)
    Right accumulator ->
      case instruction of
        PreparedSectionAssign objectOrdinal assignedValue -> do
          stepResult <- applyDenseDescentAssignment session objectOrdinal assignedValue
          pure (fmap (`recordDescentStepCompleted` accumulator) stepResult)
        PreparedSectionValueStream objectOrdinal assignedValues ->
          applyDenseDescentValueStream session objectOrdinal assignedValues accumulator
        PreparedSectionDelta scope assignments -> do
          stepResult <- applyDenseDescentDeltaFields session scope assignments
          pure (fmap (`recordDescentStepCompleted` accumulator) stepResult)

applyDenseDescentValueStream ::
  Ord cell =>
  DenseDescentSession s owner cell witness stalk mismatch repairObstruction ->
  Int ->
  Vector.Vector stalk ->
  SectionDescentAccumulator ->
  ST s (Either (SectionDescentError cell stalk mismatch) SectionDescentAccumulator)
applyDenseDescentValueStream session objectOrdinal assignedValues =
  applyFromIndex 0
  where
    valueCount =
      Vector.length assignedValues

    applyFromIndex index accumulator
      | index >= valueCount =
          pure (Right accumulator)
      | otherwise = do
          stepResult <-
            applyDenseDescentAssignment session objectOrdinal (Vector.unsafeIndex assignedValues index)
          case stepResult of
            Left descentError ->
              pure (Left descentError)
            Right coverage ->
              applyFromIndex (index + 1) (recordDescentStepCompleted coverage accumulator)

applyDenseDescentAssignment ::
  Ord cell =>
  DenseDescentSession s owner cell witness stalk mismatch repairObstruction ->
  Int ->
  stalk ->
  ST s (Either (SectionDescentError cell stalk mismatch) DescentDirtyCoverage)
applyDenseDescentAssignment session objectOrdinal assignedValue = do
  Mutable.write (ddsMutableValues session) objectOrdinal assignedValue
  descentStepFromObject
    session
    objectOrdinal
    DescentOutgoingRows
    (PinnedDescentSingleton objectOrdinal assignedValue)
    (dirtyScope (IntSet.singleton objectOrdinal))
{-# INLINE applyDenseDescentAssignment #-}

applyDenseDescentDeltaFields ::
  Ord cell =>
  DenseDescentSession s owner cell witness stalk mismatch repairObstruction ->
  Scope IntSet ->
  IntMap stalk ->
  ST s (Either (SectionDescentError cell stalk mismatch) DescentDirtyCoverage)
applyDenseDescentDeltaFields session scope assignments =
  case singletonDescentAssignmentUnchecked assignments of
    Just (objectOrdinal, assignedValue)
      | deltaScopeIsSingleton objectOrdinal scope -> do
          Mutable.write (ddsMutableValues session) objectOrdinal assignedValue
          descentStepFromObject
            session
            objectOrdinal
            DescentIncidentRows
            (PinnedDescentAssignments assignments)
            objectScope
    _ -> do
      traverse_ (uncurry (Mutable.write (ddsMutableValues session))) (IntMap.toAscList assignments)
      closeDenseDescentFrontierFromScope
        DenseDescentContext
          { ddcSession = session,
            ddcRowMode = DescentIncidentRows,
            ddcPinnedTargets = PinnedDescentAssignments assignments
          }
        objectScope
  where
    objectScope =
      unionScope scope (normalizeDirtyAssignmentScope assignments)

descentStepFromObject ::
  Ord cell =>
  DenseDescentSession s owner cell witness stalk mismatch repairObstruction ->
  Int ->
  SectionDescentRowMode ->
  PinnedDescentTarget stalk ->
  Scope IntSet ->
  ST s (Either (SectionDescentError cell stalk mismatch) DescentDirtyCoverage)
descentStepFromObject session objectOrdinal rowMode pinnedTargets initialScope =
  case fastEditProgramAt objectOrdinal (ddsPreparedDescent session) of
    Just editProgram -> do
      fastResult <-
        applyFastDescentProgramUnit
          (ddsFastKernelSource session)
          (ddsMutableValues session)
          objectOrdinal
          editProgram
      traverse_ (const (insertDenseGenerationVector (ddsDirtyObjects session) (sfepDirtyKeys editProgram))) fastResult
      pure (DescentDirtyObjects <$ fastResult)
    Nothing ->
      closeDenseDescentFrontierFromScope
        DenseDescentContext
          { ddcSession = session,
            ddcRowMode = rowMode,
            ddcPinnedTargets = pinnedTargets
          }
        initialScope
{-# INLINE descentStepFromObject #-}

applyFastDescentProgramUnit ::
  DenseDescentKernelSource owner cell witness stalk mismatch repairObstruction ->
  Mutable.MVector s stalk ->
  Int ->
  SectionFastEditProgram owner cell witness ->
  ST s (Either (SectionDescentError cell stalk mismatch) ())
applyFastDescentProgramUnit kernelSource mutableValues objectOrdinal editProgram =
  case kernelSourceFastKernel objectOrdinal editProgram kernelSource of
    Left descentError ->
      pure (Left descentError)
    Right fastKernel ->
      Right <$> applyFastDescentKernelUnit mutableValues fastKernel
{-# INLINE applyFastDescentProgramUnit #-}

normalizeDirtyAssignmentScope :: IntMap stalk -> Scope IntSet
normalizeDirtyAssignmentScope =
  dirtyScope . IntMap.keysSet

closeDenseDescentFrontierFromScope ::
  Ord cell =>
  DenseDescentContext s owner cell witness stalk mismatch repairObstruction ->
  Scope IntSet ->
  ST s (Either (SectionDescentError cell stalk mismatch) DescentDirtyCoverage)
closeDenseDescentFrontierFromScope descentContext initialScope = do
  clearDenseDescentArena arena
  dirtyCoverage <- seedDenseDescentFrontiers rowCount initialScope (ddaFrontier arena) (ddaLocalDirtyObjects arena)
  convergence <-
    closeDenseDescentFrontier
      descentContext
      (psdFrontierClosureBudget (ddsPreparedDescent session))
      (ddaFrontier arena)
      (ddaNextFrontier arena)
  case convergence of
    Left descentError ->
      pure (Left descentError)
    Right () -> do
      certification <- certifyDenseDescentFrontier descentContext dirtyCoverage
      case certification of
        Left descentError ->
          pure (Left descentError)
        Right () -> do
          mergeDenseGenerationFrontier (ddsDirtyObjects session) (ddaLocalDirtyObjects arena)
          pure (Right dirtyCoverage)
  where
    session =
      ddcSession descentContext
    arena =
      ddsArena session
    rowCount =
      psdObjectCount (ddsPreparedDescent session)

seedDenseDescentFrontiers ::
  Int ->
  Scope IntSet ->
  DenseGenerationFrontier s ->
  DenseGenerationFrontier s ->
  ST s DescentDirtyCoverage
seedDenseDescentFrontiers rowCount initialScope frontier dirtyObjects =
  foldScope
    (pure DescentDirtyObjects)
    ( \dirtyObjectKeys -> do
      insertDenseGenerationIntSet frontier dirtyObjectKeys
      insertDenseGenerationIntSet dirtyObjects dirtyObjectKeys
      pure DescentDirtyObjects
    )
    ( do
      insertDenseGenerationRange frontier rowCount
      insertDenseGenerationRange dirtyObjects rowCount
      pure DescentDirtyFull
    )
    initialScope

closeDenseDescentFrontier ::
  DenseDescentContext s owner cell witness stalk mismatch repairObstruction ->
  FrontierClosureBudget ->
  DenseGenerationFrontier s ->
  DenseGenerationFrontier s ->
  ST s (Either (SectionDescentError cell stalk mismatch) ())
closeDenseDescentFrontier descentContext frontierClosureBudget frontier nextFrontier = do
  frontierIsEmpty <- denseGenerationFrontierNull frontier
  if frontierIsEmpty
    then pure (Right ())
    else case spendFrontierClosureBudget frontierClosureBudget of
      Nothing -> do
        frontierScope <- denseGenerationFrontierScope False frontier
        pure (Left (SectionDescentFrontierDidNotConverge frontierScope))
      Just nextBudget -> do
        clearDenseGenerationFrontier nextFrontier
        propagationResult <-
          foldPreparedRowsForObjectFrontierM
            (ddcRowMode descentContext)
            (ddsPreparedDescent (ddcSession descentContext))
            frontier
            (propagateDenseRestrictionRow descentContext nextFrontier)
            (Right ())
        case propagationResult of
          Left descentError ->
            pure (Left descentError)
          Right () ->
            closeDenseDescentFrontier descentContext nextBudget nextFrontier frontier

propagateDenseRestrictionRow ::
  DenseDescentContext s owner cell witness stalk mismatch repairObstruction ->
  DenseGenerationFrontier s ->
  Either (SectionDescentError cell stalk mismatch) () ->
  SectionDescentRestrictionRow cell witness ->
  ST s (Either (SectionDescentError cell stalk mismatch) ())
propagateDenseRestrictionRow descentContext nextFrontier propagationState row =
  case propagationState of
    Left descentError ->
      pure (Left descentError)
    Right () -> do
      sourceValueResult <- readDescentOrdinal rowCount mutableValues (sdrSourceOrdinal row) (sdrSourceKey row)
      case sourceValueResult of
        Left descentError ->
          pure (Left descentError)
        Right sourceValue -> do
          let targetOrdinal = sdrTargetOrdinal row
              !nextTargetValue = restrictApply stalkAlgebra (sdrRestriction row) sourceValue
          case lookupPinnedDescentTarget targetOrdinal (ddcPinnedTargets descentContext) of
            Just pinnedValue ->
              case pinnedRestrictionConflict stalkAlgebra row nextTargetValue pinnedValue of
                Left descentError ->
                  pure (Left descentError)
                Right () ->
                  markCurrentRestrictionValid
            Nothing -> do
              targetValueResult <- readDescentOrdinal rowCount mutableValues targetOrdinal (sdrTargetKey row)
              case targetValueResult of
                Left descentError ->
                  pure (Left descentError)
                Right targetValue
                  | null (stalkMismatches stalkAlgebra nextTargetValue targetValue) ->
                      markCurrentRestrictionValid
                  | otherwise -> do
                      Mutable.write mutableValues targetOrdinal nextTargetValue
                      insertDenseGenerationFrontier nextFrontier targetOrdinal
                      insertDenseGenerationFrontier (ddaLocalDirtyObjects arena) targetOrdinal
                      invalidateDenseRestrictionsForObject (ddsPreparedDescent session) (ddaValidRestrictions arena) targetOrdinal
                      markCurrentRestrictionValid
  where
    session =
      ddcSession descentContext
    arena =
      ddsArena session
    stalkAlgebra =
      ddsStalkAlgebra session
    mutableValues =
      ddsMutableValues session
    rowCount =
      psdObjectCount (ddsPreparedDescent session)
    markCurrentRestrictionValid =
      Right () <$ markRestrictionValid (ddaValidRestrictions arena) (sdrRestrictionKey row)

invalidateDenseRestrictionsForObject ::
  PreparedSectionDescent owner cell witness ->
  DenseRestrictionValidity s ->
  Int ->
  ST s ()
invalidateDenseRestrictionsForObject preparedDescent validRestrictions objectOrdinal =
  UVector.foldM'
    (\() restrictionKey -> markRestrictionInvalid validRestrictions restrictionKey)
    ()
    (objectRestrictionIdsAt objectOrdinal (psdvIncidentRestrictionIdsByObject (psdViews preparedDescent)))

certifyDenseDescentFrontier ::
  Ord cell =>
  DenseDescentContext s owner cell witness stalk mismatch repairObstruction ->
  DescentDirtyCoverage ->
  ST s (Either (SectionDescentError cell stalk mismatch) ())
certifyDenseDescentFrontier descentContext dirtyCoverage = do
  clearDenseGenerationFrontier restrictionFrontier
  if dirtyCoverageIsFull dirtyCoverage
    then insertDenseGenerationVector restrictionFrontier (psdvAllRestrictionIds (psdViews preparedDescent))
    else
      foldDenseGenerationFrontierM
        ( \() objectOrdinal ->
            insertDenseGenerationVector
              restrictionFrontier
              (objectRestrictionIdsAt objectOrdinal (psdvIncidentRestrictionIdsByObject (psdViews preparedDescent)))
        )
        ()
        (ddaLocalDirtyObjects arena)
  rejectionResult <-
    foldRestrictionRowsForIdsM
      preparedDescent
      restrictionFrontier
      (certifyDenseRestrictionRow descentContext)
      (Right Map.empty)
  pure $
    case rejectionResult of
      Left descentError ->
        Left descentError
      Right rejections
        | Map.null rejections ->
            Right ()
        | otherwise ->
            Left (SectionDescentRejected rejections)
  where
    arena =
      ddsArena (ddcSession descentContext)
    preparedDescent =
      ddsPreparedDescent (ddcSession descentContext)
    restrictionFrontier =
      ddaRestrictionFrontier arena

certifyDenseRestrictionRow ::
  Ord cell =>
  DenseDescentContext s owner cell witness stalk mismatch repairObstruction ->
  Either (SectionDescentError cell stalk mismatch) (Map cell [mismatch]) ->
  SectionDescentRestrictionRow cell witness ->
  ST s (Either (SectionDescentError cell stalk mismatch) (Map cell [mismatch]))
certifyDenseRestrictionRow descentContext certification row =
  case certification of
    Left descentError ->
      pure (Left descentError)
    Right rejections -> do
      isValid <- restrictionIsValid (ddaValidRestrictions (ddsArena session)) (sdrRestrictionKey row)
      if isValid
        then pure (Right rejections)
        else do
          sourceValueResult <- readDescentOrdinal rowCount mutableValues (sdrSourceOrdinal row) (sdrSourceKey row)
          targetValueResult <- readDescentOrdinal rowCount mutableValues (sdrTargetOrdinal row) (sdrTargetKey row)
          pure $ do
            sourceValue <- sourceValueResult
            targetValue <- targetValueResult
            Right (accumulateRestrictionRejection (ddsStalkAlgebra session) row sourceValue targetValue rejections)
  where
    session =
      ddcSession descentContext
    mutableValues =
      ddsMutableValues session
    rowCount =
      psdObjectCount (ddsPreparedDescent session)

accumulateRestrictionRejection ::
  Ord cell =>
  StalkAlgebra witness stalk mismatch repairObstruction ->
  SectionDescentRestrictionRow cell witness ->
  stalk ->
  stalk ->
  Map cell [mismatch] ->
  Map cell [mismatch]
accumulateRestrictionRejection stalkAlgebra row sourceValue targetValue rejections =
  case restrictionMismatches (checkRestriction stalkAlgebra (sdrRestriction row) sourceValue targetValue) of
    [] ->
      rejections
    mismatches ->
      Map.insertWith (flip (<>)) (rTarget (sdrRestriction row)) mismatches rejections

readDescentOrdinal ::
  Int ->
  Mutable.MVector s stalk ->
  Int ->
  ObjectKey ->
  ST s (Either (SectionDescentError cell stalk mismatch) stalk)
readDescentOrdinal rowCount mutableValues ordinal key
  | ordinal < 0 || ordinal >= rowCount =
      pure (Left (SectionDescentStoreFailed (SectionStoreUnknownObjectKey key)))
  | otherwise =
      Right <$> Mutable.read mutableValues ordinal

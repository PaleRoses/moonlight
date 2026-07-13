{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Moonlight.Sheaf.Section.Store.Descent.FastPath
  ( fastEditProgramsByObject,
    fastEditProgramAt,
    prepareAlgebraSectionDescent,
    fastEditKernelAt,
    runPreparedFinalFastValueStreamDescentWithAlgebra,
    runPreparedFinalFastValueStreamDescent,
    applyFastDescentPreparedProgramUnit,
    applyFastDescentEditProgramUnit,
    compileFastEditKernel,
    applyFastDescentKernelUnit,
  )
where

import Control.Monad.ST (ST, runST)
import Data.IntSet qualified as IntSet
import Data.Vector qualified as Vector
import Data.Vector.Mutable qualified as Mutable
import Data.Vector.Unboxed qualified as UVector
import Moonlight.Delta.Scope
  ( Scope,
    dirtyScope,
  )
import Moonlight.Sheaf.Section.Morphism
  ( rWitness,
  )
import Moonlight.Sheaf.Section.ObjectIndex
  ( ObjectKey (..),
  )
import Moonlight.Sheaf.Section.Stalk
  ( StalkAlgebra (..),
    StalkRestrictionKernel (..),
  )
import Moonlight.Sheaf.Section.Store.Descent.Rows
  ( objectRestrictionIdsAt,
    preparedRestrictionRowAt,
    restrictionRowForId,
  )
import Moonlight.Sheaf.Section.Store.Internal
  ( advanceTotalSectionStore,
  )
import Moonlight.Sheaf.Section.Store.Types

fastEditProgramsByObject ::
  PreparedSectionDescent cell witness ->
  Vector.Vector (Maybe (SectionFastEditProgram cell witness))
fastEditProgramsByObject preparedDescent =
  Vector.generate (psdObjectCount preparedDescent) (fastEditProgramForObjectOrdinal preparedDescent)

fastEditProgramAt ::
  Int ->
  PreparedSectionDescent cell witness ->
  Maybe (SectionFastEditProgram cell witness)
fastEditProgramAt objectOrdinal preparedDescent =
  case psdvFastEditProgramsByObject (psdViews preparedDescent) Vector.!? objectOrdinal of
    Just maybeProgram -> maybeProgram
    Nothing -> Nothing
{-# INLINE fastEditProgramAt #-}

prepareAlgebraSectionDescent ::
  PreparedSectionDescent cell witness ->
  StalkAlgebra witness stalk mismatch repairObstruction ->
  AlgebraPreparedSectionDescent cell witness stalk mismatch repairObstruction
prepareAlgebraSectionDescent preparedDescent stalkAlgebra =
  AlgebraPreparedSectionDescent
    { apsdPreparedDescent = preparedDescent,
      apsdStalkAlgebra = stalkAlgebra,
      apsdFastEditKernelsByObject =
        fmap
          (fmap (compileFastEditKernel preparedDescent stalkAlgebra))
          (psdvFastEditProgramsByObject (psdViews preparedDescent))
    }

fastEditKernelAt ::
  Int ->
  AlgebraPreparedSectionDescent cell witness stalk mismatch repairObstruction ->
  Either (SectionDescentError cell stalk mismatch) (Maybe (SectionFastEditKernel stalk))
fastEditKernelAt objectOrdinal algebraPreparedDescent =
  case apsdFastEditKernelsByObject algebraPreparedDescent Vector.!? objectOrdinal of
    Just Nothing ->
      Right Nothing
    Just (Just kernelResult) ->
      Just <$> kernelResult
    Nothing ->
      Right Nothing
{-# INLINE fastEditKernelAt #-}

runPreparedFinalFastValueStreamDescentWithAlgebra ::
  AlgebraPreparedSectionDescent cell witness stalk mismatch repairObstruction ->
  Int ->
  Vector.Vector stalk ->
  Int ->
  TotalSectionStore cell stalk ->
  Either (SectionDescentError cell stalk mismatch) (SectionDescentResult cell stalk)
runPreparedFinalFastValueStreamDescentWithAlgebra algebraPreparedDescent objectOrdinal assignedValues assignedCount store =
  case finalAssignedValue assignedValues of
    Nothing ->
      Right
        SectionDescentResult
          { sdrSection = store,
            sdrObservedSteps = 0
          }
    Just assignedValue -> do
      editProgram <- fastEditProgramForPreparedObject algebraPreparedDescent objectOrdinal
      fastKernel <- fastEditKernelForPreparedObject algebraPreparedDescent objectOrdinal
      let denseValues =
            runST
              ( do
                  mutableValues <- Vector.thaw values
                  Mutable.write mutableValues objectOrdinal assignedValue
                  applyAssignedValueKernel objectOrdinal fastKernel mutableValues assignedValue
                  frozenValues <- Vector.freeze mutableValues
                  pure (DenseSection frozenValues)
              )
          nextSection =
            advanceTotalSectionStore
              denseValues
              (dirtyScopeFromOrdinalVector (sfepDirtyKeys editProgram))
              store
      Right
        SectionDescentResult
          { sdrSection = nextSection,
            sdrObservedSteps = assignedCount
          }
  where
    DenseSection values =
      totalSectionDenseValues store

runPreparedFinalFastValueStreamDescent ::
  PreparedSectionDescent cell witness ->
  StalkAlgebra witness stalk mismatch repairObstruction ->
  Int ->
  SectionFastEditProgram cell witness ->
  Vector.Vector stalk ->
  Int ->
  TotalSectionStore cell stalk ->
  Either (SectionDescentError cell stalk mismatch) (SectionDescentResult cell stalk)
runPreparedFinalFastValueStreamDescent preparedDescent stalkAlgebra objectOrdinal editProgram assignedValues assignedCount store =
  case finalAssignedValue assignedValues of
    Nothing ->
      Right
        SectionDescentResult
          { sdrSection = store,
            sdrObservedSteps = 0
          }
    Just assignedValue -> do
      fastKernel <- compileFastEditKernel preparedDescent stalkAlgebra editProgram
      let denseValues =
            runST
              ( do
                  mutableValues <- Vector.thaw values
                  Mutable.write mutableValues objectOrdinal assignedValue
                  applyAssignedValueKernel objectOrdinal fastKernel mutableValues assignedValue
                  frozenValues <- Vector.freeze mutableValues
                  pure (DenseSection frozenValues)
              )
          nextSection =
            advanceTotalSectionStore
              denseValues
              (dirtyScopeFromOrdinalVector (sfepDirtyKeys editProgram))
              store
      Right
        SectionDescentResult
          { sdrSection = nextSection,
            sdrObservedSteps = assignedCount
          }
  where
    DenseSection values =
      totalSectionDenseValues store

finalAssignedValue :: Vector.Vector stalk -> Maybe stalk
finalAssignedValue assignedValues =
  assignedValues Vector.!? (Vector.length assignedValues - 1)
{-# INLINE finalAssignedValue #-}

dirtyScopeFromOrdinalVector :: UVector.Vector Int -> Scope IntSet.IntSet
dirtyScopeFromOrdinalVector =
  dirtyScope . IntSet.fromList . UVector.toList

applyAssignedValueKernel ::
  Int ->
  SectionFastEditKernel stalk ->
  Mutable.MVector s stalk ->
  stalk ->
  ST s ()
applyAssignedValueKernel objectOrdinal fastKernel mutableValues assignedValue =
  case (Vector.length fastKernel, fastKernel Vector.!? 0) of
    (0, _) ->
      pure ()
    (1, Just (SectionFastEditCopyStep sourceOrdinal targetOrdinal))
      | sourceOrdinal == objectOrdinal ->
          Mutable.write mutableValues targetOrdinal assignedValue
    (1, Just (SectionFastEditMapStep restrictValue sourceOrdinal targetOrdinal))
      | sourceOrdinal == objectOrdinal ->
          Mutable.write mutableValues targetOrdinal (restrictValue assignedValue)
    _ ->
      applyFastDescentKernelUnit mutableValues fastKernel
{-# INLINE applyAssignedValueKernel #-}

applyFastDescentEditProgramUnit ::
  PreparedSectionDescent cell witness ->
  StalkAlgebra witness stalk mismatch repairObstruction ->
  Mutable.MVector s stalk ->
  SectionFastEditProgram cell witness ->
  ST s (Either (SectionDescentError cell stalk mismatch) ())
applyFastDescentEditProgramUnit preparedDescent stalkAlgebra mutableValues editProgram =
  case compileFastEditKernel preparedDescent stalkAlgebra editProgram of
    Left descentError ->
      pure (Left descentError)
    Right fastKernel ->
      Right <$> applyFastDescentKernelUnit mutableValues fastKernel

applyFastDescentPreparedProgramUnit ::
  AlgebraPreparedSectionDescent cell witness stalk mismatch repairObstruction ->
  Mutable.MVector s stalk ->
  Int ->
  SectionFastEditProgram cell witness ->
  ST s (Either (SectionDescentError cell stalk mismatch) ())
applyFastDescentPreparedProgramUnit algebraPreparedDescent mutableValues objectOrdinal _editProgram =
  case fastEditKernelForPreparedObject algebraPreparedDescent objectOrdinal of
    Left descentError ->
      pure (Left descentError)
    Right fastKernel ->
      Right <$> applyFastDescentKernelUnit mutableValues fastKernel

compileFastEditKernel ::
  PreparedSectionDescent cell witness ->
  StalkAlgebra witness stalk mismatch repairObstruction ->
  SectionFastEditProgram cell witness ->
  Either (SectionDescentError cell stalk mismatch) (SectionFastEditKernel stalk)
compileFastEditKernel preparedDescent stalkAlgebra editProgram =
  traverse
    (fmap (compileFastEditKernelRow stalkAlgebra) . restrictionRowForId preparedDescent)
    (Vector.convert (sfepRestrictionIds editProgram) :: Vector.Vector Int)

compileFastEditKernelRow ::
  StalkAlgebra witness stalk mismatch repairObstruction ->
  SectionDescentRestrictionRow cell witness ->
  SectionFastEditKernelStep stalk
compileFastEditKernelRow stalkAlgebra row =
  case saRestrictionKernel stalkAlgebra (rWitness (sdrRestriction row)) of
    StalkRestrictionIdentity ->
      SectionFastEditCopyStep (sdrSourceOrdinal row) (sdrTargetOrdinal row)
    StalkRestrictionMap restrictValue ->
      SectionFastEditMapStep restrictValue (sdrSourceOrdinal row) (sdrTargetOrdinal row)
{-# INLINE compileFastEditKernelRow #-}

applyFastDescentKernelUnit ::
  Mutable.MVector s stalk ->
  SectionFastEditKernel stalk ->
  ST s ()
applyFastDescentKernelUnit mutableValues fastKernel =
  Vector.mapM_ (applyFastDescentKernelStepValue mutableValues) fastKernel
{-# INLINE applyFastDescentKernelUnit #-}

applyFastDescentKernelStepValue ::
  Mutable.MVector s stalk ->
  SectionFastEditKernelStep stalk ->
  ST s ()
applyFastDescentKernelStepValue mutableValues step =
  case step of
    SectionFastEditCopyStep sourceOrdinal targetOrdinal -> do
      sourceValue <- Mutable.read mutableValues sourceOrdinal
      Mutable.write mutableValues targetOrdinal sourceValue
    SectionFastEditMapStep restrictValue sourceOrdinal targetOrdinal -> do
      sourceValue <- Mutable.read mutableValues sourceOrdinal
      Mutable.write mutableValues targetOrdinal (restrictValue sourceValue)
{-# INLINE applyFastDescentKernelStepValue #-}

fastEditProgramForObjectOrdinal ::
  PreparedSectionDescent cell witness ->
  Int ->
  Maybe (SectionFastEditProgram cell witness)
fastEditProgramForObjectOrdinal preparedDescent objectOrdinal
  | objectHasIncomingRestrictions preparedDescent objectOrdinal =
      Nothing
  | otherwise =
      closeFastEditProgram
        preparedDescent
        FastEditProgramState
          { fepsVisitedObjects = IntSet.singleton objectOrdinal,
            fepsVisitedRows = IntSet.empty,
            fepsRestrictionIds = []
          }
        [objectOrdinal]
        >>= fastEditProgramFromState (psdObjectCount preparedDescent)

data FastEditProgramState cell witness = FastEditProgramState
  { fepsVisitedObjects :: !IntSet.IntSet,
    fepsVisitedRows :: !IntSet.IntSet,
    fepsRestrictionIds :: ![Int]
  }

fastEditProgramFromState :: Int -> FastEditProgramState cell witness -> Maybe (SectionFastEditProgram cell witness)
fastEditProgramFromState _ programState =
  Just
    SectionFastEditProgram
      { sfepRestrictionIds = UVector.fromList (reverse (fepsRestrictionIds programState)),
        sfepDirtyKeys = UVector.fromList (IntSet.toAscList (fepsVisitedObjects programState))
      }

rowOrdinalsInBounds :: Int -> SectionDescentRestrictionRow cell witness -> Bool
rowOrdinalsInBounds rowCount row
  | sourceOrdinal < 0 || sourceOrdinal >= rowCount =
      False
  | targetOrdinal < 0 || targetOrdinal >= rowCount =
      False
  | otherwise =
      True
  where
    sourceOrdinal =
      sdrSourceOrdinal row
    targetOrdinal =
      sdrTargetOrdinal row

closeFastEditProgram ::
  PreparedSectionDescent cell witness ->
  FastEditProgramState cell witness ->
  [Int] ->
  Maybe (FastEditProgramState cell witness)
closeFastEditProgram preparedDescent programState frontier =
  case frontier of
    [] ->
      Just programState
    objectOrdinal : remainingFrontier -> do
      (nextFrontier, nextProgramState) <-
        UVector.foldM'
          (extendFastEditProgramFromRestrictionId preparedDescent)
          (remainingFrontier, programState)
          (objectRestrictionIdsAt objectOrdinal (psdvOutgoingRestrictionIdsByObject (psdViews preparedDescent)))
      closeFastEditProgram preparedDescent nextProgramState nextFrontier

extendFastEditProgramFromRestrictionId ::
  PreparedSectionDescent cell witness ->
  ([Int], FastEditProgramState cell witness) ->
  Int ->
  Maybe ([Int], FastEditProgramState cell witness)
extendFastEditProgramFromRestrictionId preparedDescent stateValue restrictionKey = do
  row <- preparedRestrictionRowAt preparedDescent restrictionKey
  extendFastEditProgramFromRow preparedDescent stateValue row

extendFastEditProgramFromRow ::
  PreparedSectionDescent cell witness ->
  ([Int], FastEditProgramState cell witness) ->
  SectionDescentRestrictionRow cell witness ->
  Maybe ([Int], FastEditProgramState cell witness)
extendFastEditProgramFromRow preparedDescent (frontier, programState) row
  | IntSet.member restrictionKey (fepsVisitedRows programState) =
      Just (frontier, programState)
  | not (rowOrdinalsInBounds (psdObjectCount preparedDescent) row) =
      Nothing
  | IntSet.member targetOrdinal (fepsVisitedObjects programState) =
      Nothing
  | not (targetHasOnlyIncomingRestriction preparedDescent targetOrdinal restrictionKey) =
      Nothing
  | otherwise =
      Just
        ( targetOrdinal : frontier,
          programState
            { fepsVisitedObjects = IntSet.insert targetOrdinal (fepsVisitedObjects programState),
              fepsVisitedRows = IntSet.insert restrictionKey (fepsVisitedRows programState),
              fepsRestrictionIds = restrictionKey : fepsRestrictionIds programState
            }
        )
  where
    restrictionKey =
      sdrRestrictionKey row
    targetOrdinal =
      sdrTargetOrdinal row

objectHasIncomingRestrictions ::
  PreparedSectionDescent cell witness ->
  Int ->
  Bool
objectHasIncomingRestrictions preparedDescent objectOrdinal =
  not (UVector.null (objectRestrictionIdsAt objectOrdinal (psdvIncomingRestrictionIdsByObject (psdViews preparedDescent))))

targetHasOnlyIncomingRestriction ::
  PreparedSectionDescent cell witness ->
  Int ->
  Int ->
  Bool
targetHasOnlyIncomingRestriction preparedDescent targetOrdinal restrictionKey =
  ordinalVectorIsSingleton restrictionKey (objectRestrictionIdsAt targetOrdinal (psdvIncomingRestrictionIdsByObject (psdViews preparedDescent)))

ordinalVectorIsSingleton :: Int -> UVector.Vector Int -> Bool
ordinalVectorIsSingleton ordinal vectorValue =
  UVector.length vectorValue == 1 && UVector.elem ordinal vectorValue

fastEditProgramForPreparedObject ::
  AlgebraPreparedSectionDescent cell witness stalk mismatch repairObstruction ->
  Int ->
  Either (SectionDescentError cell stalk mismatch) (SectionFastEditProgram cell witness)
fastEditProgramForPreparedObject algebraPreparedDescent objectOrdinal =
  case fastEditProgramAt objectOrdinal (apsdPreparedDescent algebraPreparedDescent) of
    Just editProgram ->
      Right editProgram
    Nothing ->
      Left (SectionDescentStoreFailed (SectionStoreUnknownObjectKey (ObjectKey objectOrdinal)))
{-# INLINE fastEditProgramForPreparedObject #-}

fastEditKernelForPreparedObject ::
  AlgebraPreparedSectionDescent cell witness stalk mismatch repairObstruction ->
  Int ->
  Either (SectionDescentError cell stalk mismatch) (SectionFastEditKernel stalk)
fastEditKernelForPreparedObject algebraPreparedDescent objectOrdinal =
  fastEditKernelAt objectOrdinal algebraPreparedDescent >>= \case
    Just fastKernel ->
      Right fastKernel
    Nothing ->
      Left (SectionDescentStoreFailed (SectionStoreUnknownObjectKey (ObjectKey objectOrdinal)))
{-# INLINE fastEditKernelForPreparedObject #-}

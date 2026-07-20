{-# LANGUAGE BangPatterns #-}

-- | Precompiled fast-edit programs and kernels for algebra-prepared descent.
module Moonlight.Sheaf.Section.Store.Descent.FastPath.Internal
  ( fastEditProgramsByObject,
    fastEditProgramAt,
    fastEditKernelAt,
    runPreparedFinalFastValueStreamDescentWithAlgebra,
    runPreparedFinalFastValueStreamDescent,
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
  ( SectionFastEditKernel (SectionFastEditKernelInternal),
    SectionFastEditKernelStep (SectionFastEditCopyStepInternal, SectionFastEditMapStepInternal),
    SectionFastEditProgram (SectionFastEditProgramInternal),
    advanceTotalSectionStore,
    algebraPreparedFastEditKernelsInternal,
    preparedSectionDescentFastEditProgramsInternal,
    sectionFastEditKernelStepsInternal,
    sfepDirtyKeys,
    sfepRestrictionIds,
  )
import Moonlight.Sheaf.Section.Store.Types

fastEditProgramsByObject ::
  PreparedSectionDescent owner cell witness ->
  Vector.Vector (Maybe (SectionFastEditProgram owner cell witness))
fastEditProgramsByObject preparedDescent =
  Vector.generate (psdObjectCount preparedDescent) (fastEditProgramForObjectOrdinal preparedDescent)

fastEditProgramAt ::
  Int ->
  PreparedSectionDescent owner cell witness ->
  Maybe (SectionFastEditProgram owner cell witness)
fastEditProgramAt objectOrdinal preparedDescent =
  case preparedSectionDescentFastEditProgramsInternal (psdViews preparedDescent) Vector.!? objectOrdinal of
    Just maybeProgram -> maybeProgram
    Nothing -> Nothing
{-# INLINE fastEditProgramAt #-}

fastEditKernelAt ::
  Int ->
  AlgebraPreparedSectionDescent owner cell witness stalk mismatch repairObstruction ->
  Either (SectionDescentError cell stalk mismatch) (Maybe (SectionFastEditKernel owner stalk))
fastEditKernelAt objectOrdinal algebraPreparedDescent =
  case algebraPreparedFastEditKernelsInternal algebraPreparedDescent Vector.!? objectOrdinal of
    Just Nothing ->
      Right Nothing
    Just (Just kernelResult) ->
      Just <$> kernelResult
    Nothing ->
      Right Nothing
{-# INLINE fastEditKernelAt #-}

runPreparedFinalFastValueStreamDescentWithAlgebra ::
  AlgebraPreparedSectionDescent owner cell witness stalk mismatch repairObstruction ->
  Int ->
  Vector.Vector stalk ->
  Int ->
  TotalSectionStore owner cell stalk ->
  Either (SectionDescentError cell stalk mismatch) (SectionDescentResult owner cell stalk)
runPreparedFinalFastValueStreamDescentWithAlgebra algebraPreparedDescent objectOrdinal assignedValues assignedCount store =
  runPreparedFinalFastValueStreamDescentWithCompiledEdit
    (preparedFastEditForObject algebraPreparedDescent objectOrdinal)
    objectOrdinal
    assignedValues
    assignedCount
    store

runPreparedFinalFastValueStreamDescent ::
  PreparedSectionDescent owner cell witness ->
  StalkAlgebra witness stalk mismatch repairObstruction ->
  Int ->
  SectionFastEditProgram owner cell witness ->
  Vector.Vector stalk ->
  Int ->
  TotalSectionStore owner cell stalk ->
  Either (SectionDescentError cell stalk mismatch) (SectionDescentResult owner cell stalk)
runPreparedFinalFastValueStreamDescent preparedDescent stalkAlgebra objectOrdinal editProgram assignedValues assignedCount store =
  runPreparedFinalFastValueStreamDescentWithCompiledEdit
    ( fmap
        (\fastKernel -> (editProgram, fastKernel))
        (compileFastEditKernel preparedDescent stalkAlgebra editProgram)
    )
    objectOrdinal
    assignedValues
    assignedCount
    store

runPreparedFinalFastValueStreamDescentWithCompiledEdit ::
  Either
    (SectionDescentError cell stalk mismatch)
    (SectionFastEditProgram owner cell witness, SectionFastEditKernel owner stalk) ->
  Int ->
  Vector.Vector stalk ->
  Int ->
  TotalSectionStore owner cell stalk ->
  Either (SectionDescentError cell stalk mismatch) (SectionDescentResult owner cell stalk)
runPreparedFinalFastValueStreamDescentWithCompiledEdit compiledEdit objectOrdinal assignedValues assignedCount store =
  case finalAssignedValue assignedValues of
    Nothing ->
      Right
        SectionDescentResult
          { sdrSection = store,
            sdrObservedSteps = 0
          }
    Just assignedValue -> do
      (editProgram, fastKernel) <- compiledEdit
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
  SectionFastEditKernel owner stalk ->
  Mutable.MVector s stalk ->
  stalk ->
  ST s ()
applyAssignedValueKernel objectOrdinal fastKernel mutableValues assignedValue =
  case (Vector.length kernelSteps, kernelSteps Vector.!? 0) of
    (0, _) ->
      pure ()
    (1, Just (SectionFastEditCopyStepInternal sourceOrdinal targetOrdinal))
      | sourceOrdinal == objectOrdinal ->
          Mutable.write mutableValues targetOrdinal assignedValue
    (1, Just (SectionFastEditMapStepInternal restrictValue sourceOrdinal targetOrdinal))
      | sourceOrdinal == objectOrdinal ->
          Mutable.write mutableValues targetOrdinal (restrictValue assignedValue)
    _ ->
      applyFastDescentKernelUnit mutableValues fastKernel
  where
    kernelSteps =
      sectionFastEditKernelStepsInternal fastKernel
{-# INLINE applyAssignedValueKernel #-}

compileFastEditKernel ::
  PreparedSectionDescent owner cell witness ->
  StalkAlgebra witness stalk mismatch repairObstruction ->
  SectionFastEditProgram owner cell witness ->
  Either (SectionDescentError cell stalk mismatch) (SectionFastEditKernel owner stalk)
compileFastEditKernel preparedDescent stalkAlgebra editProgram =
  fmap SectionFastEditKernelInternal $
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
      SectionFastEditCopyStepInternal (sdrSourceOrdinal row) (sdrTargetOrdinal row)
    StalkRestrictionMap restrictValue ->
      SectionFastEditMapStepInternal restrictValue (sdrSourceOrdinal row) (sdrTargetOrdinal row)
{-# INLINE compileFastEditKernelRow #-}

applyFastDescentKernelUnit ::
  Mutable.MVector s stalk ->
  SectionFastEditKernel owner stalk ->
  ST s ()
applyFastDescentKernelUnit mutableValues fastKernel =
  Vector.mapM_
    (applyFastDescentKernelStepValue mutableValues)
    (sectionFastEditKernelStepsInternal fastKernel)
{-# INLINE applyFastDescentKernelUnit #-}

applyFastDescentKernelStepValue ::
  Mutable.MVector s stalk ->
  SectionFastEditKernelStep stalk ->
  ST s ()
applyFastDescentKernelStepValue mutableValues step =
  case step of
    SectionFastEditCopyStepInternal sourceOrdinal targetOrdinal -> do
      sourceValue <- Mutable.read mutableValues sourceOrdinal
      Mutable.write mutableValues targetOrdinal sourceValue
    SectionFastEditMapStepInternal restrictValue sourceOrdinal targetOrdinal -> do
      sourceValue <- Mutable.read mutableValues sourceOrdinal
      Mutable.write mutableValues targetOrdinal (restrictValue sourceValue)
{-# INLINE applyFastDescentKernelStepValue #-}

fastEditProgramForObjectOrdinal ::
  PreparedSectionDescent owner cell witness ->
  Int ->
  Maybe (SectionFastEditProgram owner cell witness)
fastEditProgramForObjectOrdinal preparedDescent objectOrdinal
  | objectHasIncomingRestrictions preparedDescent objectOrdinal =
      Nothing
  | otherwise =
      fmap fastEditProgramFromState $
        closeFastEditProgram
          preparedDescent
          FastEditProgramState
            { fepsVisitedObjects = IntSet.singleton objectOrdinal,
              fepsVisitedRows = IntSet.empty,
              fepsRestrictionIds = []
            }
          [objectOrdinal]

data FastEditProgramState = FastEditProgramState
  { fepsVisitedObjects :: !IntSet.IntSet,
    fepsVisitedRows :: !IntSet.IntSet,
    fepsRestrictionIds :: ![Int]
  }

fastEditProgramFromState :: FastEditProgramState -> SectionFastEditProgram owner cell witness
fastEditProgramFromState programState =
  SectionFastEditProgramInternal
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
  PreparedSectionDescent owner cell witness ->
  FastEditProgramState ->
  [Int] ->
  Maybe FastEditProgramState
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
  PreparedSectionDescent owner cell witness ->
  ([Int], FastEditProgramState) ->
  Int ->
  Maybe ([Int], FastEditProgramState)
extendFastEditProgramFromRestrictionId preparedDescent stateValue restrictionKey = do
  row <- preparedRestrictionRowAt preparedDescent restrictionKey
  extendFastEditProgramFromRow preparedDescent stateValue row

extendFastEditProgramFromRow ::
  PreparedSectionDescent owner cell witness ->
  ([Int], FastEditProgramState) ->
  SectionDescentRestrictionRow cell witness ->
  Maybe ([Int], FastEditProgramState)
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
  PreparedSectionDescent owner cell witness ->
  Int ->
  Bool
objectHasIncomingRestrictions preparedDescent objectOrdinal =
  not (UVector.null (objectRestrictionIdsAt objectOrdinal (psdvIncomingRestrictionIdsByObject (psdViews preparedDescent))))

targetHasOnlyIncomingRestriction ::
  PreparedSectionDescent owner cell witness ->
  Int ->
  Int ->
  Bool
targetHasOnlyIncomingRestriction preparedDescent targetOrdinal restrictionKey =
  ordinalVectorIsSingleton restrictionKey (objectRestrictionIdsAt targetOrdinal (psdvIncomingRestrictionIdsByObject (psdViews preparedDescent)))

ordinalVectorIsSingleton :: Int -> UVector.Vector Int -> Bool
ordinalVectorIsSingleton ordinal vectorValue =
  UVector.length vectorValue == 1 && UVector.elem ordinal vectorValue

preparedFastEditForObject ::
  AlgebraPreparedSectionDescent owner cell witness stalk mismatch repairObstruction ->
  Int ->
  Either
    (SectionDescentError cell stalk mismatch)
    (SectionFastEditProgram owner cell witness, SectionFastEditKernel owner stalk)
preparedFastEditForObject algebraPreparedDescent objectOrdinal = do
  editProgram <- requirePreparedValueAt objectOrdinal (fastEditProgramAt objectOrdinal (apsdPreparedDescent algebraPreparedDescent))
  fastKernel <- fastEditKernelAt objectOrdinal algebraPreparedDescent >>= requirePreparedValueAt objectOrdinal
  pure (editProgram, fastKernel)
{-# INLINE preparedFastEditForObject #-}

requirePreparedValueAt :: Int -> Maybe value -> Either (SectionDescentError cell stalk mismatch) value
requirePreparedValueAt objectOrdinal =
  maybe
    (Left (SectionDescentStoreFailed (SectionStoreUnknownObjectKey (ObjectKey objectOrdinal))))
    Right
{-# INLINE requirePreparedValueAt #-}

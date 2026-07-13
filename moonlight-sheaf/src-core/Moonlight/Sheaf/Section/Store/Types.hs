{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Sheaf.Section.Store.Types
  ( SectionEpoch (..),
    DenseSection (..),
    SparseSection (..),
    TotalSectionStore,
    PartialSectionStore,
    mkTotalSectionStore,
    mkPartialSectionStore,
    emptyTotalSectionStoreWith,
    emptyPartialSectionStore,
    totalSectionModelFingerprint,
    totalSectionModelVersion,
    totalSectionDenseValues,
    totalSectionEpoch,
    totalSectionExtent,
    partialSectionModelFingerprint,
    partialSectionModelVersion,
    partialSectionSparseValues,
    partialSectionEpoch,
    partialSectionExtent,
    SectionDelta (..),
    KeyedSectionDelta (..),
    KeyedSectionEdit (..),
    SectionConstructionError (..),
    SectionLookupError (..),
    SectionUpdateError (..),
    SectionStoreError (..),
    SectionRestrictionResult (..),
    SectionDescentObservation (..),
    SectionDescentPreparationError (..),
    SectionDescentError (..),
    SectionDescentResult (..),
    SectionDescentRestrictionRow (..),
    PreparedSectionDescent (..),
    PreparedSectionDescentViews (..),
    AlgebraPreparedSectionDescent (..),
    SectionDescentRowMode (..),
    PinnedDescentTarget (..),
    FrontierClosureBudget (..),
    DescentDirtyCoverage (..),
    SectionDescentAccumulator (..),
    PreparedSectionProgram (..),
    PreparedSectionInstruction (..),
    SectionFastEditProgram (..),
    SectionFastEditKernel,
    SectionFastEditKernelStep (..),
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.Unboxed qualified as UVector
import Moonlight.Delta.Scope
  ( Scope,
    cleanScope,
    dirtyScope,
  )
import Moonlight.Sheaf.Index.Dense
  ( denseIndexKeyOf,
  )
import Moonlight.Sheaf.Section.Model
  ( ModelFingerprint,
    SheafModel,
    modelCells,
    sheafModelFingerprint,
    sheafModelObjects,
    sheafModelVersion,
  )
import Moonlight.Sheaf.Section.Morphism
  ( Restriction,
    RestrictionCheck,
    RestrictionId,
  )
import Moonlight.Sheaf.Section.ObjectIndex
  ( ObjectKey (..),
    SheafModelVersion,
  )
import Moonlight.Sheaf.Section.Stalk
  ( StalkAlgebra,
  )
import Moonlight.Sheaf.Section.Store.Internal
  ( DenseSection (..),
    PartialSectionStore (..),
    SectionEpoch (..),
    SparseSection (..),
    TotalSectionStore (..),
  )

data SectionDelta cell stalk = SectionDelta
  { sdModelFingerprint :: !ModelFingerprint,
    sdModelVersion :: !SheafModelVersion,
    sdAssignments :: !(Map cell stalk)
  }
  deriving stock (Eq, Show)

data KeyedSectionDelta stalk = KeyedSectionDelta
  { ksdModelFingerprint :: !ModelFingerprint,
    ksdModelVersion :: !SheafModelVersion,
    ksdExtent :: !(Scope IntSet),
    ksdAssignments :: !(IntMap stalk)
  }
  deriving stock (Eq, Show)

data KeyedSectionEdit stalk = KeyedSectionEdit
  { kseModelFingerprint :: !ModelFingerprint,
    kseModelVersion :: !SheafModelVersion,
    kseObjectKey :: !ObjectKey,
    kseValue :: !stalk
  }
  deriving stock (Eq, Show)

data SectionConstructionError cell = SectionConstructionError
  { sceMissingCells :: !(Set cell),
    sceExtraCells :: !(Set cell)
  }
  deriving stock (Eq, Show)

data SectionLookupError cell
  = SectionLookupOutOfBasis !cell
  | SectionLookupInvariantMissing !cell
  | SectionLookupModelFingerprintMismatch !ModelFingerprint !ModelFingerprint
  deriving stock (Eq, Show)

data SectionUpdateError cell
  = SectionUpdateOutOfBasis !cell
  | SectionUpdateInvariantMissing !cell
  | SectionUpdateModelFingerprintMismatch !ModelFingerprint !ModelFingerprint
  deriving stock (Eq, Show)

data SectionStoreError cell
  = SectionStoreConstructionFailed !(SectionConstructionError cell)
  | SectionStoreUnknownCell !cell
  | SectionStoreUnknownObjectKey !ObjectKey
  | SectionStoreObjectCountMismatch !Int !Int
  | SectionStoreModelFingerprintMismatch !ModelFingerprint !ModelFingerprint
  | SectionStoreInvariantMissing !cell
  deriving stock (Eq, Show)

data SectionRestrictionResult cell stalk mismatch
  = SectionRestrictionSatisfied
  | SectionRestrictionMismatch !(RestrictionCheck stalk mismatch)
  deriving stock (Eq, Show)

data SectionDescentObservation
  = ObserveFinalSection
  | ObserveEachStep
  deriving stock (Eq, Ord, Show, Read)

data SectionDescentPreparationError
  = SectionDescentPreparationRestrictionMissing !RestrictionId
  deriving stock (Eq, Show)

data SectionDescentError cell stalk mismatch
  = SectionDescentStoreFailed !(SectionStoreError cell)
  | SectionDescentRestrictionMissing !RestrictionId
  | SectionDescentPinnedConflict !cell !stalk !stalk ![mismatch]
  | SectionDescentRejected !(Map cell [mismatch])
  | SectionDescentFrontierDidNotConverge !(Scope IntSet)
  deriving stock (Eq, Show)

data SectionDescentResult cell stalk = SectionDescentResult
  { sdrSection :: !(TotalSectionStore cell stalk),
    sdrObservedSteps :: !Int
  }
  deriving stock (Eq, Show)

data SectionDescentRestrictionRow cell witness = SectionDescentRestrictionRow
  { sdrRestrictionKey :: !Int,
    sdrRestriction :: !(Restriction cell witness),
    sdrSourceKey :: !ObjectKey,
    sdrTargetKey :: !ObjectKey,
    sdrSourceOrdinal :: !Int,
    sdrTargetOrdinal :: !Int
  }

data PreparedSectionDescent cell witness = PreparedSectionDescent
  { psdModelFingerprint :: !ModelFingerprint,
    psdModelVersion :: !SheafModelVersion,
    psdObjectCount :: !Int,
    psdFrontierClosureBudget :: !FrontierClosureBudget,
    psdRowsByRestrictionId :: !(Vector (SectionDescentRestrictionRow cell witness)),
    psdViews :: !(PreparedSectionDescentViews cell witness)
  }

data PreparedSectionDescentViews cell witness = PreparedSectionDescentViews
  { psdvIncidentRestrictionIdsByObject :: !(Vector (UVector.Vector Int)),
    psdvIncomingRestrictionIdsByObject :: !(Vector (UVector.Vector Int)),
    psdvOutgoingRestrictionIdsByObject :: !(Vector (UVector.Vector Int)),
    psdvFastEditProgramsByObject :: !(Vector (Maybe (SectionFastEditProgram cell witness))),
    psdvAllRestrictionIds :: !(UVector.Vector Int)
  }

data AlgebraPreparedSectionDescent cell witness stalk mismatch repairObstruction = AlgebraPreparedSectionDescent
  { apsdPreparedDescent :: !(PreparedSectionDescent cell witness),
    apsdStalkAlgebra :: !(StalkAlgebra witness stalk mismatch repairObstruction),
    apsdFastEditKernelsByObject :: !(Vector (Maybe (Either (SectionDescentError cell stalk mismatch) (SectionFastEditKernel stalk))))
  }

data SectionDescentRowMode
  = DescentIncidentRows
  | DescentOutgoingRows

data PinnedDescentTarget stalk
  = PinnedDescentAssignments !(IntMap stalk)
  | PinnedDescentSingleton !Int !stalk

newtype FrontierClosureBudget = FrontierClosureBudget
  { unFrontierClosureBudget :: Int
  }
  deriving stock (Eq, Ord, Show, Read)

data DescentDirtyCoverage
  = DescentDirtyObjects
  | DescentDirtyFull
  deriving stock (Eq, Ord, Show, Read)

data SectionDescentAccumulator = SectionDescentAccumulator
  { sdaDirtyCoverage :: !DescentDirtyCoverage,
    sdaObservedSteps :: !Int
  }

data PreparedSectionProgram stalk = PreparedSectionProgram
  { pspModelFingerprint :: !ModelFingerprint,
    pspModelVersion :: !SheafModelVersion,
    pspObjectCount :: !Int,
    pspInstructions :: !(Vector (PreparedSectionInstruction stalk))
  }
  deriving stock (Eq, Show)

data PreparedSectionInstruction stalk
  = PreparedSectionAssign !Int !stalk
  | PreparedSectionValueStream !Int !(Vector stalk)
  | PreparedSectionDelta !(Scope IntSet) !(IntMap stalk)
  deriving stock (Eq, Show)

data SectionFastEditProgram cell witness = SectionFastEditProgram
  { sfepRestrictionIds :: !(UVector.Vector Int),
    sfepDirtyKeys :: !(UVector.Vector Int)
  }

type SectionFastEditKernel stalk = Vector (SectionFastEditKernelStep stalk)

data SectionFastEditKernelStep stalk
  = SectionFastEditCopyStep !Int !Int
  | SectionFastEditMapStep !(stalk -> stalk) !Int !Int

mkTotalSectionStore ::
  Ord cell =>
  SheafModel cell witness ->
  Map cell stalk ->
  Either (SectionConstructionError cell) (TotalSectionStore cell stalk)
mkTotalSectionStore model entries = do
  values <- denseSectionFromEntries model entries
  pure
    TotalSectionStore
      { tssModelFingerprint = sheafModelFingerprint model,
        tssModelVersion = sheafModelVersion model,
        tssValues = values,
        tssExtent = cleanScope,
        tssEpoch = SectionEpoch 0
      }

mkPartialSectionStore ::
  Ord cell =>
  SheafModel cell witness ->
  Map cell stalk ->
  Either (SectionStoreError cell) (PartialSectionStore cell stalk)
mkPartialSectionStore model entries = do
  extent <- objectExtentForAssignments model entries
  pure
    PartialSectionStore
      { pssModelFingerprint = sheafModelFingerprint model,
        pssModelVersion = sheafModelVersion model,
        pssValues = SparseSection entries,
        pssExtent = extent,
        pssEpoch = SectionEpoch 0
      }

emptyTotalSectionStoreWith ::
  SheafModel cell witness ->
  (cell -> stalk) ->
  TotalSectionStore cell stalk
emptyTotalSectionStoreWith model initialize =
  TotalSectionStore
    { tssModelFingerprint = sheafModelFingerprint model,
      tssModelVersion = sheafModelVersion model,
      tssValues = DenseSection (Vector.fromList (fmap initialize (modelCells model))),
      tssExtent = cleanScope,
      tssEpoch = SectionEpoch 0
    }

emptyPartialSectionStore :: SheafModel cell witness -> PartialSectionStore cell stalk
emptyPartialSectionStore model =
  PartialSectionStore
    { pssModelFingerprint = sheafModelFingerprint model,
      pssModelVersion = sheafModelVersion model,
      pssValues = SparseSection Map.empty,
      pssExtent = cleanScope,
      pssEpoch = SectionEpoch 0
    }

totalSectionModelFingerprint :: TotalSectionStore cell stalk -> ModelFingerprint
totalSectionModelFingerprint =
  tssModelFingerprint
{-# INLINE totalSectionModelFingerprint #-}

totalSectionModelVersion :: TotalSectionStore cell stalk -> SheafModelVersion
totalSectionModelVersion =
  tssModelVersion
{-# INLINE totalSectionModelVersion #-}

totalSectionDenseValues :: TotalSectionStore cell stalk -> DenseSection stalk
totalSectionDenseValues =
  tssValues
{-# INLINE totalSectionDenseValues #-}

totalSectionEpoch :: TotalSectionStore cell stalk -> SectionEpoch
totalSectionEpoch =
  tssEpoch
{-# INLINE totalSectionEpoch #-}

totalSectionExtent :: TotalSectionStore cell stalk -> Scope IntSet
totalSectionExtent =
  tssExtent
{-# INLINE totalSectionExtent #-}

partialSectionModelFingerprint :: PartialSectionStore cell stalk -> ModelFingerprint
partialSectionModelFingerprint =
  pssModelFingerprint
{-# INLINE partialSectionModelFingerprint #-}

partialSectionModelVersion :: PartialSectionStore cell stalk -> SheafModelVersion
partialSectionModelVersion =
  pssModelVersion
{-# INLINE partialSectionModelVersion #-}

partialSectionSparseValues :: PartialSectionStore cell stalk -> SparseSection cell stalk
partialSectionSparseValues =
  pssValues
{-# INLINE partialSectionSparseValues #-}

partialSectionEpoch :: PartialSectionStore cell stalk -> SectionEpoch
partialSectionEpoch =
  pssEpoch
{-# INLINE partialSectionEpoch #-}

partialSectionExtent :: PartialSectionStore cell stalk -> Scope IntSet
partialSectionExtent =
  pssExtent
{-# INLINE partialSectionExtent #-}

denseSectionFromEntries ::
  Ord cell =>
  SheafModel cell witness ->
  Map cell stalk ->
  Either (SectionConstructionError cell) (DenseSection stalk)
denseSectionFromEntries model entries =
  let expectedCells = Set.fromList (modelCells model)
      actualCells = Map.keysSet entries
      missingCells = Set.difference expectedCells actualCells
      extraCells = Set.difference actualCells expectedCells
   in if Set.null missingCells && Set.null extraCells
        then DenseSection . Vector.fromList <$> traverse lookupCell (modelCells model)
        else
          Left
            SectionConstructionError
              { sceMissingCells = missingCells,
                sceExtraCells = extraCells
              }
  where
    lookupCell cell =
      case Map.lookup cell entries of
        Just stalk -> Right stalk
        Nothing ->
          Left
            SectionConstructionError
              { sceMissingCells = Set.singleton cell,
                sceExtraCells = Set.empty
              }

objectExtentForAssignments ::
  Ord cell =>
  SheafModel cell witness ->
  Map cell stalk ->
  Either (SectionStoreError cell) (Scope IntSet)
objectExtentForAssignments model entries =
  dirtyScope . IntSet.fromList <$> traverse objectKeyForCell (Map.keys entries)
  where
    objectKeyForCell cell =
      case denseIndexKeyOf cell (sheafModelObjects model) of
        Just (ObjectKey objectKey) ->
          Right objectKey
        Nothing ->
          Left (SectionStoreUnknownCell cell)

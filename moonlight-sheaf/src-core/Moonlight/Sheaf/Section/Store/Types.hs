{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RoleAnnotations #-}

-- | Section store carriers: epochs, dense and sparse sections, total and
-- partial stores.
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
    totalSectionDenseValues,
    totalSectionEpoch,
    totalSectionExtent,
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
    SectionDescentRestrictionRow,
    sdrRestrictionKey,
    sdrRestriction,
    sdrSourceKey,
    sdrTargetKey,
    sdrSourceOrdinal,
    sdrTargetOrdinal,
    PreparedSectionDescent,
    psdObjectCount,
    psdFrontierClosureBudget,
    psdRowsByRestrictionId,
    psdViews,
    PreparedSectionDescentViews,
    psdvIncidentRestrictionIdsByObject,
    psdvIncomingRestrictionIdsByObject,
    psdvOutgoingRestrictionIdsByObject,
    psdvAllRestrictionIds,
    AlgebraPreparedSectionDescent,
    apsdPreparedDescent,
    apsdStalkAlgebra,
    SectionDescentRowMode (..),
    PinnedDescentTarget (..),
    FrontierClosureBudget (..),
    DescentDirtyCoverage (..),
    SectionDescentAccumulator (..),
    PreparedSectionProgram,
    PreparedSectionInstruction,
    SectionFastEditProgram,
    SectionFastEditKernel,
    SectionFastEditKernelStep,
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
  ( SheafModel,
    modelCells,
    sheafModelObjects,
  )
import Moonlight.Sheaf.Section.Morphism (Restriction)
import Moonlight.Sheaf.Section.ObjectIndex
  ( ObjectKey (..),
  )
import Moonlight.Sheaf.Section.Stalk (StalkAlgebra)
import Moonlight.Sheaf.Section.Store.Internal
  ( AlgebraPreparedSectionDescent,
    DenseSection (..),
    DescentDirtyCoverage (..),
    FrontierClosureBudget (..),
    KeyedSectionDelta (..),
    KeyedSectionEdit (..),
    PartialSectionStore (..),
    PinnedDescentTarget (..),
    PreparedSectionDescent,
    PreparedSectionDescentViews,
    PreparedSectionInstruction,
    PreparedSectionProgram,
    SectionConstructionError (..),
    SectionDelta (..),
    SectionDescentAccumulator (..),
    SectionDescentError (..),
    SectionDescentObservation (..),
    SectionDescentPreparationError (..),
    SectionDescentRestrictionRow,
    SectionDescentResult (..),
    SectionDescentRowMode (..),
    SectionEpoch (..),
    SectionFastEditKernel,
    SectionFastEditKernelStep,
    SectionFastEditProgram,
    SectionLookupError (..),
    SectionRestrictionResult (..),
    SectionStoreError (..),
    SectionUpdateError (..),
    SparseSection (..),
    TotalSectionStore (..),
    algebraPreparedSectionDescentInternal,
    algebraPreparedStalkAlgebraInternal,
    preparedSectionDescentAllRestrictionIdsInternal,
    preparedSectionDescentFrontierClosureBudgetInternal,
    preparedSectionDescentIncidentRestrictionIdsInternal,
    preparedSectionDescentIncomingRestrictionIdsInternal,
    preparedSectionDescentObjectCountInternal,
    preparedSectionDescentOutgoingRestrictionIdsInternal,
    preparedSectionDescentRowsByRestrictionIdInternal,
    preparedSectionDescentViewsInternal,
    sectionDescentRowRestrictionInternal,
    sectionDescentRowRestrictionKeyInternal,
    sectionDescentRowSourceKeyInternal,
    sectionDescentRowSourceOrdinalInternal,
    sectionDescentRowTargetKeyInternal,
    sectionDescentRowTargetOrdinalInternal,
  )

sdrRestrictionKey :: SectionDescentRestrictionRow cell witness -> Int
sdrRestrictionKey =
  sectionDescentRowRestrictionKeyInternal

sdrRestriction :: SectionDescentRestrictionRow cell witness -> Restriction cell witness
sdrRestriction =
  sectionDescentRowRestrictionInternal

sdrSourceKey :: SectionDescentRestrictionRow cell witness -> ObjectKey
sdrSourceKey =
  sectionDescentRowSourceKeyInternal

sdrTargetKey :: SectionDescentRestrictionRow cell witness -> ObjectKey
sdrTargetKey =
  sectionDescentRowTargetKeyInternal

sdrSourceOrdinal :: SectionDescentRestrictionRow cell witness -> Int
sdrSourceOrdinal =
  sectionDescentRowSourceOrdinalInternal

sdrTargetOrdinal :: SectionDescentRestrictionRow cell witness -> Int
sdrTargetOrdinal =
  sectionDescentRowTargetOrdinalInternal

psdObjectCount :: PreparedSectionDescent owner cell witness -> Int
psdObjectCount =
  preparedSectionDescentObjectCountInternal

psdFrontierClosureBudget :: PreparedSectionDescent owner cell witness -> FrontierClosureBudget
psdFrontierClosureBudget =
  preparedSectionDescentFrontierClosureBudgetInternal

psdRowsByRestrictionId ::
  PreparedSectionDescent owner cell witness ->
  Vector (SectionDescentRestrictionRow cell witness)
psdRowsByRestrictionId =
  preparedSectionDescentRowsByRestrictionIdInternal

psdViews :: PreparedSectionDescent owner cell witness -> PreparedSectionDescentViews owner cell witness
psdViews =
  preparedSectionDescentViewsInternal

psdvIncidentRestrictionIdsByObject ::
  PreparedSectionDescentViews owner cell witness ->
  Vector (UVector.Vector Int)
psdvIncidentRestrictionIdsByObject =
  preparedSectionDescentIncidentRestrictionIdsInternal

psdvIncomingRestrictionIdsByObject ::
  PreparedSectionDescentViews owner cell witness ->
  Vector (UVector.Vector Int)
psdvIncomingRestrictionIdsByObject =
  preparedSectionDescentIncomingRestrictionIdsInternal

psdvOutgoingRestrictionIdsByObject ::
  PreparedSectionDescentViews owner cell witness ->
  Vector (UVector.Vector Int)
psdvOutgoingRestrictionIdsByObject =
  preparedSectionDescentOutgoingRestrictionIdsInternal

psdvAllRestrictionIds :: PreparedSectionDescentViews owner cell witness -> UVector.Vector Int
psdvAllRestrictionIds =
  preparedSectionDescentAllRestrictionIdsInternal

apsdPreparedDescent ::
  AlgebraPreparedSectionDescent owner cell witness stalk mismatch repairObstruction ->
  PreparedSectionDescent owner cell witness
apsdPreparedDescent =
  algebraPreparedSectionDescentInternal

apsdStalkAlgebra ::
  AlgebraPreparedSectionDescent owner cell witness stalk mismatch repairObstruction ->
  StalkAlgebra witness stalk mismatch repairObstruction
apsdStalkAlgebra =
  algebraPreparedStalkAlgebraInternal

mkTotalSectionStore ::
  Ord cell =>
  SheafModel owner cell witness ->
  Map cell stalk ->
  Either (SectionConstructionError cell) (TotalSectionStore owner cell stalk)
mkTotalSectionStore model entries = do
  values <- denseSectionFromEntries model entries
  pure
    TotalSectionStore
      { tssValues = values,
        tssExtent = cleanScope,
        tssEpoch = SectionEpoch 0
      }

mkPartialSectionStore ::
  Ord cell =>
  SheafModel owner cell witness ->
  Map cell stalk ->
  Either (SectionStoreError cell) (PartialSectionStore owner cell stalk)
mkPartialSectionStore model entries = do
  extent <- objectExtentForAssignments model entries
  pure
    PartialSectionStore
      { pssValues = SparseSection entries,
        pssExtent = extent,
        pssEpoch = SectionEpoch 0
      }

emptyTotalSectionStoreWith ::
  SheafModel owner cell witness ->
  (cell -> stalk) ->
  TotalSectionStore owner cell stalk
emptyTotalSectionStoreWith model initialize =
  TotalSectionStore
    { tssValues = DenseSection (Vector.fromList (fmap initialize (modelCells model))),
      tssExtent = cleanScope,
      tssEpoch = SectionEpoch 0
    }

emptyPartialSectionStore :: SheafModel owner cell witness -> PartialSectionStore owner cell stalk
emptyPartialSectionStore model =
  PartialSectionStore
    { pssValues = SparseSection Map.empty,
      pssExtent = cleanScope,
      pssEpoch = SectionEpoch 0
    }

totalSectionDenseValues :: TotalSectionStore owner cell stalk -> DenseSection stalk
totalSectionDenseValues =
  tssValues
{-# INLINE totalSectionDenseValues #-}

totalSectionEpoch :: TotalSectionStore owner cell stalk -> SectionEpoch
totalSectionEpoch =
  tssEpoch
{-# INLINE totalSectionEpoch #-}

totalSectionExtent :: TotalSectionStore owner cell stalk -> Scope IntSet
totalSectionExtent =
  tssExtent
{-# INLINE totalSectionExtent #-}

partialSectionSparseValues :: PartialSectionStore owner cell stalk -> SparseSection cell stalk
partialSectionSparseValues =
  pssValues
{-# INLINE partialSectionSparseValues #-}

partialSectionEpoch :: PartialSectionStore owner cell stalk -> SectionEpoch
partialSectionEpoch =
  pssEpoch
{-# INLINE partialSectionEpoch #-}

partialSectionExtent :: PartialSectionStore owner cell stalk -> Scope IntSet
partialSectionExtent =
  pssExtent
{-# INLINE partialSectionExtent #-}

denseSectionFromEntries ::
  Ord cell =>
  SheafModel owner cell witness ->
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
  SheafModel owner cell witness ->
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

module Moonlight.Sheaf.Section.Store.State
  ( mkTotalSectionStore,
    mkPartialSectionStore,
    emptyTotalSectionStoreWith,
    emptyPartialSectionStore,
    totalStalkAt,
    totalStalkAtKey,
    partialStalkAt,
    totalSectionEntries,
    partialSectionEntries,
    assignLocal,
    assignLocalKeyed,
    assignLocalKeyedBatch,
    validateKeyedAssignments,
    validateDescentAssignments,
    validateSingletonDescentAssignment,
    validateScopeOrdinals,
    deltaScopeIsSingleton,
    keyedDeltaObjectScope,
    updateStalkAtChecked,
    totalSectionEpoch,
    totalSectionExtent,
    partialSectionEpoch,
    partialSectionExtent,
    denseSectionSize,
    evaluateRestrictionInSection,
    keyedDeltaFromSemanticDelta,
    applyDenseAssignments,
    applyDenseAssignmentsByOrdinal,
    denseStalkAt,
  )
where

import Control.Monad.ST (ST)
import Data.Bifunctor (first)
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
import Moonlight.Delta.Scope
  ( Scope,
    dirtyScope,
    foldScope,
    unionScope,
  )
import Moonlight.Sheaf.Index.Dense
  ( denseIndexKeyOf,
  )
import Moonlight.Sheaf.Section.Condition
  ( restrictionCheckEntry,
  )
import Moonlight.Sheaf.Section.Model
  ( SheafModel,
    modelCells,
    sheafModelFingerprint,
    sheafModelObjects,
  )
import Moonlight.Sheaf.Section.Morphism
  ( Restriction,
    checkRestriction,
    rSource,
    rTarget,
  )
import Moonlight.Sheaf.Section.ObjectIndex
  ( ObjectKey (..),
    unObjectKey,
  )
import Moonlight.Sheaf.Section.Stalk
  ( StalkAlgebra,
  )
import Moonlight.Sheaf.Section.Store.Internal
  ( advanceTotalSectionStore,
  )
import Moonlight.Sheaf.Section.Store.Types

totalStalkAt ::
  Ord cell =>
  SheafModel cell witness ->
  cell ->
  TotalSectionStore cell stalk ->
  Either (SectionLookupError cell) stalk
totalStalkAt model cell store
  | sheafModelFingerprint model /= totalSectionModelFingerprint store =
      Left
        ( SectionLookupModelFingerprintMismatch
            (sheafModelFingerprint model)
            (totalSectionModelFingerprint store)
        )
  | otherwise =
      case denseIndexKeyOf cell (sheafModelObjects model) of
        Nothing ->
          Left (SectionLookupOutOfBasis cell)
        Just key ->
          case denseStalkAt key (totalSectionDenseValues store) of
            Just stalk -> Right stalk
            Nothing -> Left (SectionLookupInvariantMissing cell)

totalStalkAtKey ::
  ObjectKey ->
  TotalSectionStore cell stalk ->
  Either (SectionStoreError cell) stalk
totalStalkAtKey key store =
  case denseStalkAt key (totalSectionDenseValues store) of
    Just stalk ->
      Right stalk
    Nothing ->
      Left (SectionStoreUnknownObjectKey key)

partialStalkAt :: Ord cell => cell -> PartialSectionStore cell stalk -> Maybe stalk
partialStalkAt cell =
  Map.lookup cell . unSparseSection . partialSectionSparseValues

totalSectionEntries ::
  Ord cell =>
  SheafModel cell witness ->
  TotalSectionStore cell stalk ->
  Either (SectionStoreError cell) (Map cell stalk)
totalSectionEntries model store
  | sheafModelFingerprint model /= totalSectionModelFingerprint store =
      Left
        ( SectionStoreModelFingerprintMismatch
            (sheafModelFingerprint model)
            (totalSectionModelFingerprint store)
        )
  | otherwise =
      traverseEntries (modelCells model) (totalSectionDenseValues store)

partialSectionEntries :: PartialSectionStore cell stalk -> Map cell stalk
partialSectionEntries =
  unSparseSection . partialSectionSparseValues

assignLocal ::
  Ord cell =>
  SheafModel cell witness ->
  SectionDelta cell stalk ->
  TotalSectionStore cell stalk ->
  Either (SectionStoreError cell) (TotalSectionStore cell stalk)
assignLocal model delta store
  | sheafModelFingerprint model /= totalSectionModelFingerprint store =
      Left
        ( SectionStoreModelFingerprintMismatch
            (sheafModelFingerprint model)
            (totalSectionModelFingerprint store)
        )
  | sheafModelFingerprint model /= sdModelFingerprint delta =
      Left
        ( SectionStoreModelFingerprintMismatch
            (sheafModelFingerprint model)
            (sdModelFingerprint delta)
        )
  | Map.null (sdAssignments delta) =
      Right store
  | otherwise = do
      keyedDelta <- keyedDeltaFromSemanticDelta model delta
      assignLocalKeyed keyedDelta store

assignLocalKeyed ::
  KeyedSectionDelta stalk ->
  TotalSectionStore cell stalk ->
  Either (SectionStoreError cell) (TotalSectionStore cell stalk)
assignLocalKeyed delta =
  assignLocalKeyedBatch [delta]

assignLocalKeyedBatch ::
  [KeyedSectionDelta stalk] ->
  TotalSectionStore cell stalk ->
  Either (SectionStoreError cell) (TotalSectionStore cell stalk)
assignLocalKeyedBatch deltas store = do
  traverse_ validateAssignmentDeltaFingerprint deltas
  let assignments = batchKeyedAssignments deltas
  if IntMap.null assignments
    then Right store
    else assignValidatedKeyedAssignments assignments store
  where
    validateAssignmentDeltaFingerprint delta
      | ksdModelFingerprint delta /= totalSectionModelFingerprint store =
          Left
            ( SectionStoreModelFingerprintMismatch
                (totalSectionModelFingerprint store)
                (ksdModelFingerprint delta)
            )
      | otherwise =
          Right ()

assignValidatedKeyedAssignments ::
  IntMap stalk ->
  TotalSectionStore cell stalk ->
  Either (SectionStoreError cell) (TotalSectionStore cell stalk)
assignValidatedKeyedAssignments assignments store = do
  keyedAssignments <- validateKeyedAssignments (totalSectionDenseValues store) assignments
  pure
    ( advanceTotalSectionStore
        (applyDenseAssignmentsByOrdinal keyedAssignments (totalSectionDenseValues store))
        (dirtyScope (IntMap.keysSet assignments))
        store
    )

batchKeyedAssignments :: [KeyedSectionDelta stalk] -> IntMap stalk
batchKeyedAssignments =
  List.foldl'
    ( \assignments delta ->
        IntMap.union (ksdAssignments delta) assignments
    )
    IntMap.empty

keyedDeltaObjectScope :: KeyedSectionDelta stalk -> Scope IntSet
keyedDeltaObjectScope delta =
  unionScope
    (ksdExtent delta)
    (dirtyScope (IntMap.keysSet (ksdAssignments delta)))

validateKeyedAssignments ::
  DenseSection stalk ->
  IntMap stalk ->
  Either (SectionStoreError cell) [(Int, stalk)]
validateKeyedAssignments (DenseSection values) assignments =
  traverse validateAssignment (IntMap.toAscList assignments)
  where
    validateAssignment assignment@(ordinal, _)
      | ordinal < 0 || ordinal >= Vector.length values =
          Left (SectionStoreUnknownObjectKey (ObjectKey ordinal))
      | otherwise =
          Right assignment

validateDescentAssignments ::
  Int ->
  IntMap stalk ->
  Either (SectionStoreError cell) [(Int, stalk)]
validateDescentAssignments rowCount assignments =
  traverse validateAssignment (IntMap.toAscList assignments)
  where
    validateAssignment assignment@(ordinal, _)
      | ordinal < 0 || ordinal >= rowCount =
          Left (SectionStoreUnknownObjectKey (ObjectKey ordinal))
      | otherwise =
          Right assignment

validateSingletonDescentAssignment ::
  Int ->
  IntMap stalk ->
  Either (SectionStoreError cell) (Maybe (Int, stalk))
validateSingletonDescentAssignment rowCount assignments =
  case IntMap.minViewWithKey assignments of
    Nothing ->
      Right Nothing
    Just (assignment@(ordinal, _), remainingAssignments)
      | ordinal < 0 || ordinal >= rowCount ->
          Left (SectionStoreUnknownObjectKey (ObjectKey ordinal))
      | IntMap.null remainingAssignments ->
          Right (Just assignment)
      | otherwise ->
          Right Nothing

validateScopeOrdinals ::
  Int ->
  Scope IntSet ->
  Either (SectionStoreError cell) ()
validateScopeOrdinals rowCount scopeValue =
  foldScope
    (Right ())
    (traverse_ validateOrdinal . IntSet.toAscList)
    (Right ())
    scopeValue
  where
    validateOrdinal ordinal
      | ordinal < 0 || ordinal >= rowCount =
          Left (SectionStoreUnknownObjectKey (ObjectKey ordinal))
      | otherwise =
          Right ()

deltaScopeIsSingleton :: Int -> Scope IntSet -> Bool
deltaScopeIsSingleton objectOrdinal scopeValue =
  foldScope
    False
    (\dirtyKeys -> IntSet.member objectOrdinal dirtyKeys && IntSet.size dirtyKeys == 1)
    False
    scopeValue

updateStalkAtChecked ::
  Ord cell =>
  SheafModel cell witness ->
  cell ->
  (stalk -> stalk) ->
  TotalSectionStore cell stalk ->
  Either (SectionUpdateError cell) (TotalSectionStore cell stalk)
updateStalkAtChecked model cell transform store =
  case totalStalkAt model cell store of
    Left (SectionLookupOutOfBasis _) ->
      Left (SectionUpdateOutOfBasis cell)
    Left (SectionLookupInvariantMissing _) ->
      Left (SectionUpdateInvariantMissing cell)
    Left (SectionLookupModelFingerprintMismatch expected actual) ->
      Left (SectionUpdateModelFingerprintMismatch expected actual)
    Right stalk ->
      case denseIndexKeyOf cell (sheafModelObjects model) of
        Nothing -> Left (SectionUpdateOutOfBasis cell)
        Just key ->
          Right
            ( advanceTotalSectionStore
                (applyDenseAssignments [(key, transform stalk)] (totalSectionDenseValues store))
                (dirtyScope (IntSet.singleton (unObjectKey key)))
                store
            )

denseSectionSize :: DenseSection stalk -> Int
denseSectionSize =
  Vector.length . unDenseSection

evaluateRestrictionInSection ::
  Ord cell =>
  StalkAlgebra witness stalk mismatch repairObstruction ->
  SheafModel cell witness ->
  TotalSectionStore cell stalk ->
  Restriction cell witness ->
  Either (SectionLookupError cell) (SectionRestrictionResult cell stalk mismatch)
evaluateRestrictionInSection stalkAlgebra model section restriction = do
  sourceStalk <- totalStalkAt model (rSource restriction) section
  targetStalk <- totalStalkAt model (rTarget restriction) section
  let restrictionCheck = checkRestriction stalkAlgebra restriction sourceStalk targetStalk
  pure $
    case restrictionCheckEntry () restrictionCheck of
      Nothing ->
        SectionRestrictionSatisfied
      Just _ ->
        SectionRestrictionMismatch restrictionCheck

keyedDeltaFromSemanticDelta ::
  Ord cell =>
  SheafModel cell witness ->
  SectionDelta cell stalk ->
  Either (SectionStoreError cell) (KeyedSectionDelta stalk)
keyedDeltaFromSemanticDelta model delta =
  fmap
    ( \assignments ->
        let keyedAssignments = IntMap.fromList (fmap (first unObjectKey) assignments)
         in KeyedSectionDelta
              { ksdModelFingerprint = sdModelFingerprint delta,
                ksdModelVersion = sdModelVersion delta,
                ksdExtent = dirtyScope (IntMap.keysSet keyedAssignments),
                ksdAssignments = keyedAssignments
              }
    )
    (traverse keyAssignment (Map.toAscList (sdAssignments delta)))
  where
    keyAssignment (cell, stalk) =
      case denseIndexKeyOf cell (sheafModelObjects model) of
        Just key -> Right (key, stalk)
        Nothing -> Left (SectionStoreUnknownCell cell)

applyDenseAssignments :: [(ObjectKey, stalk)] -> DenseSection stalk -> DenseSection stalk
applyDenseAssignments assignments (DenseSection values) =
  applyDenseAssignmentsByOrdinal (fmap (first unObjectKey) assignments) (DenseSection values)

applyDenseAssignmentsByOrdinal :: forall stalk. [(Int, stalk)] -> DenseSection stalk -> DenseSection stalk
applyDenseAssignmentsByOrdinal assignments (DenseSection values) =
  DenseSection (Vector.modify writeAssignments values)
  where
    writeAssignments :: Mutable.MVector s stalk -> ST s ()
    writeAssignments mutableValues =
      traverse_ (uncurry (Mutable.write mutableValues)) assignments

traverseEntries ::
  Ord cell =>
  [cell] ->
  DenseSection stalk ->
  Either (SectionStoreError cell) (Map cell stalk)
traverseEntries cells (DenseSection values) =
  Map.fromList <$> traverse entryAt (zip [0 ..] cells)
  where
    entryAt (ordinal, cell) =
      case values Vector.!? ordinal of
        Just stalk -> Right (cell, stalk)
        Nothing -> Left (SectionStoreInvariantMissing cell)

denseStalkAt :: ObjectKey -> DenseSection stalk -> Maybe stalk
denseStalkAt key (DenseSection values) =
  values Vector.!? unObjectKey key

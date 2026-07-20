{-# LANGUAGE RoleAnnotations #-}

module Moonlight.Sheaf.Section.Certified
  ( GlobalSection,
    SectionCertification (..),
    SectionCertificationError (..),
    SectionCertificationFailure (..),
    globalUnderlyingSection,
    certifySectionCompatibility,
    certifyPreparedSectionCompatibility,
    certifyAlgebraPreparedSectionCompatibility,
    certifySectionExtentCompatibility,
    certifyPreparedSectionExtentCompatibility,
    certifyAlgebraPreparedSectionExtentCompatibility,
    mkGlobalSection,
  )
where

import Data.Bifunctor (first)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Vector qualified as Vector
import Data.Vector.Unboxed qualified as UVector
import Moonlight.Delta.Scope
  ( Scope,
    cleanScope,
    dirtyScope,
    foldScope,
    fullScope,
  )
import Moonlight.Sheaf.Section.Condition
  ( restrictionCheckEntry,
  )
import Moonlight.Sheaf.Section.Model
  ( SheafModel,
  )
import Moonlight.Sheaf.Section.Morphism
  ( RestrictionId (..),
    checkRestriction,
    rSource,
    rTarget,
  )
import Moonlight.Sheaf.Section.Store.Descent.Prepare
  ( prepareSectionDescent,
  )
import Moonlight.Sheaf.Section.Stalk
  ( StalkAlgebra,
  )
import Moonlight.Sheaf.Section.Store.State
  ( validateScopeOrdinals,
  )
import Moonlight.Sheaf.Section.Store.Types

type GlobalSection :: Type -> Type -> Type -> Type
newtype GlobalSection owner cell stalk = GlobalSection
  { unGlobalSectionInternal :: TotalSectionStore owner cell stalk
  }
  deriving stock (Eq, Show)

type role GlobalSection nominal nominal representational

globalUnderlyingSection :: GlobalSection owner cell stalk -> TotalSectionStore owner cell stalk
globalUnderlyingSection =
  unGlobalSectionInternal

type SectionCertification :: Type -> Type -> Type
data SectionCertification cell mismatch
  = SectionCertified
  | SectionRejected !(Map cell [mismatch])
  deriving stock (Eq, Show)

type SectionCertificationError :: Type -> Type
data SectionCertificationError cell
  = SectionCertificationLookupFailed !(SectionLookupError cell)
  | SectionCertificationRestrictionMissing !RestrictionId
  | SectionCertificationStoreFailed !(SectionStoreError cell)
  | SectionCertificationDescentPreparationFailed !SectionDescentPreparationError
  deriving stock (Eq, Show)

type SectionCertificationFailure :: Type -> Type -> Type
data SectionCertificationFailure cell mismatch
  = SectionCertificationInfrastructureFailed !(SectionCertificationError cell)
  | SectionCertificationSemanticallyRejected !(Map cell [mismatch])
  deriving stock (Eq, Show)

certifySectionCompatibility ::
  Ord cell =>
  SheafModel owner cell witness ->
  StalkAlgebra witness stalk mismatch repairObstruction ->
  TotalSectionStore owner cell stalk ->
  Either (SectionCertificationError cell) (SectionCertification cell mismatch)
certifySectionCompatibility model stalkAlgebra section =
  first SectionCertificationDescentPreparationFailed (prepareSectionDescent model)
    >>= \preparedDescent ->
      certifyPreparedSectionCompatibility preparedDescent stalkAlgebra section

certifyPreparedSectionCompatibility ::
  Ord cell =>
  PreparedSectionDescent owner cell witness ->
  StalkAlgebra witness stalk mismatch repairObstruction ->
  TotalSectionStore owner cell stalk ->
  Either (SectionCertificationError cell) (SectionCertification cell mismatch)
certifyPreparedSectionCompatibility preparedDescent stalkAlgebra =
  certifyPreparedSectionCompatibilityWith preparedDescent stalkAlgebra

certifyAlgebraPreparedSectionCompatibility ::
  Ord cell =>
  AlgebraPreparedSectionDescent owner cell witness stalk mismatch repairObstruction ->
  TotalSectionStore owner cell stalk ->
  Either (SectionCertificationError cell) (SectionCertification cell mismatch)
certifyAlgebraPreparedSectionCompatibility algebraPreparedDescent =
  certifyPreparedSectionCompatibilityWith
    (apsdPreparedDescent algebraPreparedDescent)
    (apsdStalkAlgebra algebraPreparedDescent)

certifyPreparedSectionCompatibilityWith ::
  Ord cell =>
  PreparedSectionDescent owner cell witness ->
  StalkAlgebra witness stalk mismatch repairObstruction ->
  TotalSectionStore owner cell stalk ->
  Either (SectionCertificationError cell) (SectionCertification cell mismatch)
certifyPreparedSectionCompatibilityWith preparedDescent stalkAlgebra section =
  certifyPreparedRestrictionVector stalkAlgebra section (psdRowsByRestrictionId preparedDescent)

certifySectionExtentCompatibility ::
  Ord cell =>
  SheafModel owner cell witness ->
  StalkAlgebra witness stalk mismatch repairObstruction ->
  Scope IntSet ->
  TotalSectionStore owner cell stalk ->
  Either (SectionCertificationError cell) (SectionCertification cell mismatch)
certifySectionExtentCompatibility model stalkAlgebra objectExtent section =
  first SectionCertificationDescentPreparationFailed (prepareSectionDescent model)
    >>= \preparedDescent ->
      certifyPreparedSectionExtentCompatibility preparedDescent stalkAlgebra objectExtent section

certifyPreparedSectionExtentCompatibility ::
  Ord cell =>
  PreparedSectionDescent owner cell witness ->
  StalkAlgebra witness stalk mismatch repairObstruction ->
  Scope IntSet ->
  TotalSectionStore owner cell stalk ->
  Either (SectionCertificationError cell) (SectionCertification cell mismatch)
certifyPreparedSectionExtentCompatibility preparedDescent stalkAlgebra =
  certifyPreparedSectionExtentCompatibilityWith preparedDescent stalkAlgebra

certifyAlgebraPreparedSectionExtentCompatibility ::
  Ord cell =>
  AlgebraPreparedSectionDescent owner cell witness stalk mismatch repairObstruction ->
  Scope IntSet ->
  TotalSectionStore owner cell stalk ->
  Either (SectionCertificationError cell) (SectionCertification cell mismatch)
certifyAlgebraPreparedSectionExtentCompatibility algebraPreparedDescent =
  certifyPreparedSectionExtentCompatibilityWith
    (apsdPreparedDescent algebraPreparedDescent)
    (apsdStalkAlgebra algebraPreparedDescent)

certifyPreparedSectionExtentCompatibilityWith ::
  Ord cell =>
  PreparedSectionDescent owner cell witness ->
  StalkAlgebra witness stalk mismatch repairObstruction ->
  Scope IntSet ->
  TotalSectionStore owner cell stalk ->
  Either (SectionCertificationError cell) (SectionCertification cell mismatch)
certifyPreparedSectionExtentCompatibilityWith preparedDescent stalkAlgebra objectExtent section = do
  first SectionCertificationStoreFailed (validateScopeOrdinals (psdObjectCount preparedDescent) objectExtent)
  foldScope
    (Right SectionCertified)
    (certifyPreparedRestrictionIds preparedDescent stalkAlgebra section)
    (certifyPreparedRestrictionVector stalkAlgebra section (psdRowsByRestrictionId preparedDescent))
    (restrictionExtentForPreparedObjectExtent preparedDescent objectExtent)

certifyPreparedRestrictionIds ::
  Ord cell =>
  PreparedSectionDescent owner cell witness ->
  StalkAlgebra witness stalk mismatch repairObstruction ->
  TotalSectionStore owner cell stalk ->
  IntSet ->
  Either (SectionCertificationError cell) (SectionCertification cell mismatch)
certifyPreparedRestrictionIds preparedDescent stalkAlgebra section restrictionIds =
  certifyRestrictionMap
    <$> IntSet.foldr
      (certifyPreparedRestrictionId preparedDescent stalkAlgebra section)
      (Right Map.empty)
      restrictionIds

certifyPreparedRestrictionId ::
  Ord cell =>
  PreparedSectionDescent owner cell witness ->
  StalkAlgebra witness stalk mismatch repairObstruction ->
  TotalSectionStore owner cell stalk ->
  Int ->
  Either (SectionCertificationError cell) (Map cell [mismatch]) ->
  Either (SectionCertificationError cell) (Map cell [mismatch])
certifyPreparedRestrictionId preparedDescent stalkAlgebra section restrictionKey rejectionsResult = do
  row <- preparedRestrictionRowForCertification preparedDescent restrictionKey
  restrictionResult <- preparedRestrictionCertificationResult stalkAlgebra section row
  rejections <- rejectionsResult
  Right (accumulateCertificationResult restrictionResult rejections)

certifyPreparedRestrictionVector ::
  Ord cell =>
  StalkAlgebra witness stalk mismatch repairObstruction ->
  TotalSectionStore owner cell stalk ->
  Vector.Vector (SectionDescentRestrictionRow cell witness) ->
  Either (SectionCertificationError cell) (SectionCertification cell mismatch)
certifyPreparedRestrictionVector stalkAlgebra section rows =
  certifyRestrictionMap
    <$> Vector.foldM'
      (accumulatePreparedRestrictionCertification stalkAlgebra section)
      Map.empty
      rows

accumulatePreparedRestrictionCertification ::
  Ord cell =>
  StalkAlgebra witness stalk mismatch repairObstruction ->
  TotalSectionStore owner cell stalk ->
  Map cell [mismatch] ->
  SectionDescentRestrictionRow cell witness ->
  Either (SectionCertificationError cell) (Map cell [mismatch])
accumulatePreparedRestrictionCertification stalkAlgebra section rejections row =
  (\restrictionResult -> accumulateCertificationResult restrictionResult rejections)
    <$> preparedRestrictionCertificationResult stalkAlgebra section row

accumulateCertificationResult ::
  Ord cell =>
  Maybe (cell, [mismatch]) ->
  Map cell [mismatch] ->
  Map cell [mismatch]
accumulateCertificationResult restrictionResult rejections =
  case restrictionResult of
    Nothing ->
      rejections
    Just (cell, mismatches) ->
      Map.insertWith (flip (<>)) cell mismatches rejections

certifyRestrictionMap ::
  Map cell [mismatch] ->
  SectionCertification cell mismatch
certifyRestrictionMap mismatchMap =
  if Map.null mismatchMap
    then SectionCertified
    else SectionRejected mismatchMap

mkGlobalSection ::
  Ord cell =>
  SheafModel owner cell witness ->
  StalkAlgebra witness stalk mismatch repairObstruction ->
  TotalSectionStore owner cell stalk ->
  Either (SectionCertificationFailure cell mismatch) (GlobalSection owner cell stalk)
mkGlobalSection model stalkAlgebra section =
  case certifySectionCompatibility model stalkAlgebra section of
    Right SectionCertified ->
      Right (GlobalSection section)
    Right (SectionRejected mismatches) ->
      Left (SectionCertificationSemanticallyRejected mismatches)
    Left certificationError ->
      Left (SectionCertificationInfrastructureFailed certificationError)

preparedRestrictionCertificationResult ::
  StalkAlgebra witness stalk mismatch repairObstruction ->
  TotalSectionStore owner cell stalk ->
  SectionDescentRestrictionRow cell witness ->
  Either
    (SectionCertificationError cell)
    (Maybe (cell, [mismatch]))
preparedRestrictionCertificationResult stalkAlgebra section row = do
  sourceStalk <-
    denseCertificationStalkAt
      rowCount
      values
      (sdrSourceOrdinal row)
      (rSource (sdrRestriction row))
  targetStalk <-
    denseCertificationStalkAt
      rowCount
      values
      (sdrTargetOrdinal row)
      (rTarget (sdrRestriction row))
  let restriction =
        sdrRestriction row
      check =
        checkRestriction stalkAlgebra restriction sourceStalk targetStalk
  pure (restrictionCheckEntry (rTarget restriction) check)
  where
    DenseSection values =
      totalSectionDenseValues section
    rowCount =
      Vector.length values

denseCertificationStalkAt ::
  Int ->
  Vector.Vector stalk ->
  Int ->
  cell ->
  Either (SectionCertificationError cell) stalk
denseCertificationStalkAt rowCount values ordinal cell
  | ordinal < 0 || ordinal >= rowCount =
      Left (SectionCertificationLookupFailed (SectionLookupInvariantMissing cell))
  | otherwise =
      case values Vector.!? ordinal of
        Just stalk ->
          Right stalk
        Nothing ->
          Left (SectionCertificationLookupFailed (SectionLookupInvariantMissing cell))

preparedRestrictionRowForCertification ::
  PreparedSectionDescent owner cell witness ->
  Int ->
  Either (SectionCertificationError cell) (SectionDescentRestrictionRow cell witness)
preparedRestrictionRowForCertification preparedDescent restrictionKey =
  case psdRowsByRestrictionId preparedDescent Vector.!? restrictionKey of
    Just row ->
      Right row
    Nothing ->
      Left (SectionCertificationRestrictionMissing (RestrictionId restrictionKey))

restrictionExtentForPreparedObjectExtent ::
  PreparedSectionDescent owner cell witness ->
  Scope IntSet ->
  Scope IntSet
restrictionExtentForPreparedObjectExtent preparedDescent =
  foldScope
    cleanScope
    (foldScopeDirty preparedDescent)
    fullScope

foldScopeDirty ::
  PreparedSectionDescent owner cell witness ->
  IntSet ->
  Scope IntSet
foldScopeDirty preparedDescent objectKeys =
  dirtyScope
    ( IntSet.foldl'
        ( \restrictionIds objectOrdinal ->
            IntSet.union restrictionIds (preparedIncidentRestrictionIds preparedDescent objectOrdinal)
        )
        IntSet.empty
        objectKeys
    )

preparedIncidentRestrictionIds :: PreparedSectionDescent owner cell witness -> Int -> IntSet
preparedIncidentRestrictionIds preparedDescent objectOrdinal =
  case psdvIncidentRestrictionIdsByObject (psdViews preparedDescent) Vector.!? objectOrdinal of
    Just restrictionIds ->
      UVector.foldl' (\incidentIds restrictionKey -> IntSet.insert restrictionKey incidentIds) IntSet.empty restrictionIds
    Nothing ->
      IntSet.empty

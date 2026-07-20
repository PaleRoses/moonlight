-- | Restriction-row traversals: object-scoped and frontier-scoped monadic
-- folds.
module Moonlight.Sheaf.Section.Store.Descent.Rows
  ( objectRestrictionIdsAt,
    restrictionIdsByObjectForMode,
    foldPreparedRowsForObjectFrontierM,
    foldRestrictionRowsForIdsM,
    foldRestrictionRowsForVectorM,
    foldRestrictionRowIdM,
    restrictionRowForId,
    preparedRestrictionRowAt,
  )
where

import Control.Monad.ST (ST)
import Data.Vector qualified as Vector
import Data.Vector.Unboxed qualified as UVector
import Moonlight.Sheaf.Section.Morphism
  ( RestrictionId (..),
  )
import Moonlight.Sheaf.Section.Store.Descent.Frontier
  ( DenseGenerationFrontier,
    foldDenseGenerationFrontierM,
  )
import Moonlight.Sheaf.Section.Store.Types

objectRestrictionIdsAt :: Int -> Vector.Vector (UVector.Vector Int) -> UVector.Vector Int
objectRestrictionIdsAt objectOrdinal restrictionIdsByObject =
  case restrictionIdsByObject Vector.!? objectOrdinal of
    Just restrictionIds -> restrictionIds
    Nothing -> UVector.empty
{-# INLINE objectRestrictionIdsAt #-}

restrictionIdsByObjectForMode ::
  SectionDescentRowMode ->
  PreparedSectionDescent owner cell witness ->
  Vector.Vector (UVector.Vector Int)
restrictionIdsByObjectForMode rowMode preparedDescent =
  case rowMode of
    DescentIncidentRows ->
      psdvIncidentRestrictionIdsByObject (psdViews preparedDescent)
    DescentOutgoingRows ->
      psdvOutgoingRestrictionIdsByObject (psdViews preparedDescent)

foldPreparedRowsForObjectFrontierM ::
  SectionDescentRowMode ->
  PreparedSectionDescent owner cell witness ->
  DenseGenerationFrontier s ->
  (Either (SectionDescentError cell stalk mismatch) acc -> SectionDescentRestrictionRow cell witness -> ST s (Either (SectionDescentError cell stalk mismatch) acc)) ->
  Either (SectionDescentError cell stalk mismatch) acc ->
  ST s (Either (SectionDescentError cell stalk mismatch) acc)
foldPreparedRowsForObjectFrontierM rowMode preparedDescent objectFrontier step initial =
  foldDenseGenerationFrontierM
    ( \rowState objectKey ->
        foldRestrictionRowsForVectorM
          preparedDescent
          (objectRestrictionIdsAt objectKey (restrictionIdsByObjectForMode rowMode preparedDescent))
          step
          rowState
    )
    initial
    objectFrontier

foldRestrictionRowsForIdsM ::
  PreparedSectionDescent owner cell witness ->
  DenseGenerationFrontier s ->
  (Either (SectionDescentError cell stalk mismatch) acc -> SectionDescentRestrictionRow cell witness -> ST s (Either (SectionDescentError cell stalk mismatch) acc)) ->
  Either (SectionDescentError cell stalk mismatch) acc ->
  ST s (Either (SectionDescentError cell stalk mismatch) acc)
foldRestrictionRowsForIdsM preparedDescent restrictionIds step initial =
  foldDenseGenerationFrontierM foldRestrictionId initial restrictionIds
  where
    foldRestrictionId rowState restrictionKey =
      foldRestrictionRowIdM preparedDescent step rowState restrictionKey

foldRestrictionRowsForVectorM ::
  PreparedSectionDescent owner cell witness ->
  UVector.Vector Int ->
  (Either (SectionDescentError cell stalk mismatch) acc -> SectionDescentRestrictionRow cell witness -> ST s (Either (SectionDescentError cell stalk mismatch) acc)) ->
  Either (SectionDescentError cell stalk mismatch) acc ->
  ST s (Either (SectionDescentError cell stalk mismatch) acc)
foldRestrictionRowsForVectorM preparedDescent restrictionIds step initial =
  UVector.foldM' (foldRestrictionRowIdM preparedDescent step) initial restrictionIds

foldRestrictionRowIdM ::
  PreparedSectionDescent owner cell witness ->
  (Either (SectionDescentError cell stalk mismatch) acc -> SectionDescentRestrictionRow cell witness -> ST s (Either (SectionDescentError cell stalk mismatch) acc)) ->
  Either (SectionDescentError cell stalk mismatch) acc ->
  Int ->
  ST s (Either (SectionDescentError cell stalk mismatch) acc)
foldRestrictionRowIdM preparedDescent step rowState restrictionKey =
  case rowState of
    Left descentError ->
      pure (Left descentError)
    Right _ ->
      case restrictionRowForId preparedDescent restrictionKey of
        Right row ->
          step rowState row
        Left descentError ->
          pure (Left descentError)

restrictionRowForId ::
  PreparedSectionDescent owner cell witness ->
  Int ->
  Either (SectionDescentError cell stalk mismatch) (SectionDescentRestrictionRow cell witness)
restrictionRowForId preparedDescent restrictionKey =
  case preparedRestrictionRowAt preparedDescent restrictionKey of
    Just row ->
      Right row
    Nothing ->
      Left (SectionDescentRestrictionMissing (RestrictionId restrictionKey))

preparedRestrictionRowAt ::
  PreparedSectionDescent owner cell witness ->
  Int ->
  Maybe (SectionDescentRestrictionRow cell witness)
preparedRestrictionRowAt preparedDescent restrictionKey =
  psdRowsByRestrictionId preparedDescent Vector.!? restrictionKey
{-# INLINE preparedRestrictionRowAt #-}

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Sheaf.Section.Restriction
  ( RestrictionIndex,
    RestrictionKindView (..),
    RestrictionIndexError (..),
    emptyRestrictionIndex,
    buildRestrictionIndex,
    buildRestrictionIndexWithDenseKeys,
    updateRestrictionIndexEntriesWithDenseKeys,
    restrictionKindView,
    restrictionCount,
    restrictionIds,
    restrictionEntries,
    lookupRestriction,
    restrictionsAlong,
    restrictionsFrom,
    restrictionsTo,
    restrictionsByKind,
    incidenceRestrictions,
    portalRestrictions,
    restrictionMultiplicityByArrow,
    restrictionEndpointKeys,
    restrictionEndpointKeyMap,
    restrictionIdsByArrowKey,
    restrictionOutgoingByObject,
    restrictionIncomingByObject,
  )
where

import Data.Bifunctor (first)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Differential.Index.Arrow
  ( ArrowIndex,
    ArrowIndexError (..),
    IndexedArrow (..),
    arrowEndpoint,
    arrowEndpointMap,
    arrowIdsByEndpointPair,
    arrowIdsBySource,
    arrowIdsByTarget,
    arrowIndexCount,
    arrowIndexEntries,
    arrowIndexIds,
    arrowsAlong,
    arrowsFrom,
    arrowsTo,
    buildIndexedArrowIndex,
    emptyArrowIndex,
    lookupArrow,
    replaceIndexedArrows,
  )
import Moonlight.Sheaf.Index.Dense
  ( denseIndexKeyOf,
  )
import Moonlight.Sheaf.Section.Morphism
  ( Restriction (..),
    RestrictionArrow (..),
    RestrictionId (..),
    RestrictionKind (..),
    RestrictionParts (..),
    RestrictionPresentation,
    isIncidenceRestriction,
    isPortalRestriction,
    restrictionArrow,
  )
import Moonlight.Sheaf.Section.ObjectIndex
  ( ObjectIndex,
    ObjectKey (..),
  )

type RestrictionIndexError :: Type -> Type
data RestrictionIndexError cell
  = RestrictionUnknownSource !cell
  | RestrictionUnknownTarget !cell
  | RestrictionUnknownId !RestrictionId
  | RestrictionDuplicateId !RestrictionId
  | RestrictionNonDenseId !RestrictionId !RestrictionId
  | RestrictionZeroIncidenceCoefficient !cell !cell
  deriving stock (Eq, Show)

type RestrictionIndex :: Type -> Type -> Type
data RestrictionIndex cell witness = RestrictionIndex
  { riArrows :: !(ArrowIndex ObjectKey (Restriction cell witness)),
    riKindView :: !(RestrictionKindView cell witness)
  }
  deriving stock (Eq, Show)

type RestrictionKindView :: Type -> Type -> Type
data RestrictionKindView cell witness = RestrictionKindView
  { rkvRestrictionsByExactKind :: !(Map RestrictionKind [Restriction cell witness]),
    rkvIncidenceRestrictions :: ![Restriction cell witness],
    rkvPortalRestrictions :: ![Restriction cell witness]
  }
  deriving stock (Eq, Show)

emptyRestrictionIndex :: RestrictionIndex cell witness
emptyRestrictionIndex =
  RestrictionIndex
    { riArrows = emptyArrowIndex Set.empty,
      riKindView = emptyRestrictionKindView
    }

emptyRestrictionKindView :: RestrictionKindView cell witness
emptyRestrictionKindView =
  RestrictionKindView
    { rkvRestrictionsByExactKind = Map.empty,
      rkvIncidenceRestrictions = [],
      rkvPortalRestrictions = []
    }

buildRestrictionIndex ::
  forall cell morphism witness.
  Ord cell =>
  ObjectIndex cell ->
  RestrictionPresentation morphism cell witness ->
  [morphism] ->
  Either (RestrictionIndexError cell) (RestrictionIndex cell witness)
buildRestrictionIndex objects =
  buildRestrictionIndexWithDenseKeys
    (fmap unObjectKey . (`denseIndexKeyOf` objects))

buildRestrictionIndexWithDenseKeys ::
  forall cell morphism witness.
  (cell -> Maybe Int) ->
  RestrictionPresentation morphism cell witness ->
  [morphism] ->
  Either (RestrictionIndexError cell) (RestrictionIndex cell witness)
buildRestrictionIndexWithDenseKeys cellKey present morphisms = do
  indexedRestrictions <-
    traverse
      (materializeRestrictionEntry cellKey present)
      (zip [0 :: Int ..] morphisms)
  indexRestrictions indexedRestrictions

updateRestrictionIndexEntriesWithDenseKeys ::
  forall cell witness.
  (cell -> Maybe Int) ->
  IntSet ->
  RestrictionPresentation (Restriction cell witness) cell witness ->
  RestrictionIndex cell witness ->
  Either (RestrictionIndexError cell) (RestrictionIndex cell witness)
updateRestrictionIndexEntriesWithDenseKeys cellKey restrictionKeys remapRestriction indexValue = do
  updates <- traverse prepareUpdate (IntSet.toAscList restrictionKeys)
  arrowIndex <-
    liftArrowIndexError (replaceIndexedArrows updates (riArrows indexValue))
  pure
    indexValue
      { riArrows = arrowIndex,
        riKindView = restrictionKindViewFromEntries (arrowIndexEntries arrowIndex)
      }
  where
    prepareUpdate ::
      Int ->
      Either
        (RestrictionIndexError cell)
        (IndexedArrow ObjectKey (Restriction cell witness))
    prepareUpdate restrictionKey =
      case lookupRestriction (RestrictionId restrictionKey) indexValue of
        Nothing ->
          Left (RestrictionUnknownId (RestrictionId restrictionKey))
        Just oldRestriction -> do
          newArrow <-
            materializeRestrictionParts
              cellKey
              (RestrictionId restrictionKey)
              (remapRestriction oldRestriction)
          pure newArrow

materializeRestrictionEntry ::
  (cell -> Maybe Int) ->
  RestrictionPresentation morphism cell witness ->
  (Int, morphism) ->
  Either (RestrictionIndexError cell) (IndexedArrow ObjectKey (Restriction cell witness))
materializeRestrictionEntry cellKey present (ordinal, morphism) =
  materializeRestrictionParts cellKey (RestrictionId ordinal) (present morphism)

materializeRestrictionParts ::
  (cell -> Maybe Int) ->
  RestrictionId ->
  RestrictionParts cell witness ->
  Either (RestrictionIndexError cell) (IndexedArrow ObjectKey (Restriction cell witness))
materializeRestrictionParts cellKey restrictionId parts = do
  sourceKey <- lookupObjectKey cellKey RestrictionUnknownSource (partSource parts)
  targetKey <- lookupObjectKey cellKey RestrictionUnknownTarget (partTarget parts)
  pure
    IndexedArrow
      { iaId = unRestrictionId restrictionId,
        iaSource = sourceKey,
        iaTarget = targetKey,
        iaEdge =
          Restriction
            { rId = restrictionId,
              rKind = partKind parts,
              rSource = partSource parts,
              rTarget = partTarget parts,
              rWitness = partWitness parts
            }
      }

lookupObjectKey ::
  (cell -> Maybe Int) ->
  (cell -> RestrictionIndexError cell) ->
  cell ->
  Either (RestrictionIndexError cell) ObjectKey
lookupObjectKey cellKey unknownError cell =
  case cellKey cell of
    Just key -> Right (ObjectKey key)
    Nothing -> Left (unknownError cell)

indexRestrictions :: [IndexedArrow ObjectKey (Restriction cell witness)] -> Either (RestrictionIndexError cell) (RestrictionIndex cell witness)
indexRestrictions indexedRestrictions = do
  arrowIndex <- liftArrowIndexError (buildIndexedArrowIndex Set.empty indexedRestrictions)
  pure
    emptyRestrictionIndex
      { riArrows = arrowIndex,
        riKindView = restrictionKindViewFromEntries (arrowIndexEntries arrowIndex)
      }

liftArrowIndexError :: Either ArrowIndexError value -> Either (RestrictionIndexError cell) value
liftArrowIndexError =
  first restrictionErrorFromArrowError

restrictionErrorFromArrowError :: ArrowIndexError -> RestrictionIndexError cell
restrictionErrorFromArrowError arrowError =
  case arrowError of
    ArrowIndexNonDenseId expectedId actualId ->
      RestrictionNonDenseId (RestrictionId expectedId) (RestrictionId actualId)
    ArrowIndexDuplicateId restrictionKey ->
      RestrictionDuplicateId (RestrictionId restrictionKey)
    ArrowIndexUnknownId restrictionKey ->
      RestrictionUnknownId (RestrictionId restrictionKey)

restrictionCount :: RestrictionIndex cell witness -> Int
restrictionCount =
  arrowIndexCount . riArrows

restrictionIds :: RestrictionIndex cell witness -> [RestrictionId]
restrictionIds =
  fmap RestrictionId . arrowIndexIds . riArrows

restrictionEntries :: RestrictionIndex cell witness -> [Restriction cell witness]
restrictionEntries =
  arrowIndexEntries . riArrows

restrictionKindView :: RestrictionIndex cell witness -> RestrictionKindView cell witness
restrictionKindView =
  riKindView

lookupRestriction ::
  RestrictionId ->
  RestrictionIndex cell witness ->
  Maybe (Restriction cell witness)
lookupRestriction (RestrictionId restrictionKey) =
  lookupArrow restrictionKey . riArrows

restrictionsAlong ::
  Ord cell =>
  ObjectIndex cell ->
  RestrictionArrow cell ->
  RestrictionIndex cell witness ->
  [Restriction cell witness]
restrictionsAlong objects arrow indexValue =
  case (denseIndexKeyOf (restrictFrom arrow) objects, denseIndexKeyOf (restrictTo arrow) objects) of
    (Just sourceKey, Just targetKey) ->
      arrowsAlong sourceKey targetKey (riArrows indexValue)
    _ -> []

restrictionsFrom ::
  Ord cell =>
  ObjectIndex cell ->
  cell ->
  RestrictionIndex cell witness ->
  [Restriction cell witness]
restrictionsFrom objects cell indexValue =
  case denseIndexKeyOf cell objects of
    Just sourceKey -> arrowsFrom sourceKey (riArrows indexValue)
    Nothing -> []

restrictionsTo ::
  Ord cell =>
  ObjectIndex cell ->
  cell ->
  RestrictionIndex cell witness ->
  [Restriction cell witness]
restrictionsTo objects cell indexValue =
  case denseIndexKeyOf cell objects of
    Just targetKey -> arrowsTo targetKey (riArrows indexValue)
    Nothing -> []

restrictionsByKind ::
  RestrictionKind ->
  RestrictionIndex cell witness ->
  [Restriction cell witness]
restrictionsByKind kindValue =
  Map.findWithDefault [] kindValue . rkvRestrictionsByExactKind . riKindView

incidenceRestrictions :: RestrictionIndex cell witness -> [Restriction cell witness]
incidenceRestrictions =
  rkvIncidenceRestrictions . riKindView

portalRestrictions :: RestrictionIndex cell witness -> [Restriction cell witness]
portalRestrictions =
  rkvPortalRestrictions . riKindView

restrictionKindViewFromEntries :: [Restriction cell witness] -> RestrictionKindView cell witness
restrictionKindViewFromEntries restrictions =
  RestrictionKindView
    { rkvRestrictionsByExactKind =
        Map.fromListWith
          (flip (<>))
          [ (rKind restriction, [restriction])
            | restriction <- restrictions
          ],
      rkvIncidenceRestrictions =
        List.filter (isIncidenceRestriction . rKind) restrictions,
      rkvPortalRestrictions =
        List.filter (isPortalRestriction . rKind) restrictions
    }

restrictionMultiplicityByArrow :: Ord cell => RestrictionIndex cell witness -> Map (RestrictionArrow cell) Int
restrictionMultiplicityByArrow =
  Map.fromListWith (+)
    . fmap (\restriction -> (restrictionArrow restriction, 1))
    . restrictionEntries

restrictionEndpointKeys :: RestrictionId -> RestrictionIndex cell witness -> Maybe (ObjectKey, ObjectKey)
restrictionEndpointKeys (RestrictionId restrictionKey) =
  arrowEndpoint restrictionKey . riArrows

restrictionEndpointKeyMap :: RestrictionIndex cell witness -> IntMap (ObjectKey, ObjectKey)
restrictionEndpointKeyMap =
  arrowEndpointMap . riArrows

restrictionIdsByArrowKey :: RestrictionIndex cell witness -> Map (ObjectKey, ObjectKey) IntSet
restrictionIdsByArrowKey =
  arrowIdsByEndpointPair . riArrows

restrictionOutgoingByObject :: RestrictionIndex cell witness -> IntMap IntSet
restrictionOutgoingByObject =
  objectBucketMap . arrowIdsBySource . riArrows

restrictionIncomingByObject :: RestrictionIndex cell witness -> IntMap IntSet
restrictionIncomingByObject =
  objectBucketMap . arrowIdsByTarget . riArrows

objectBucketMap :: Map ObjectKey IntSet -> IntMap IntSet
objectBucketMap =
  IntMap.fromList . fmap (first unObjectKey) . Map.toList

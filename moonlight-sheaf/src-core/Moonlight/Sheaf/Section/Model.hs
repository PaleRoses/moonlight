{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- | Compiled sheaf models with generative exact ownership and a separate
-- diagnostic digest for their dense layout.
module Moonlight.Sheaf.Section.Model
  ( ModelLayoutDigest,
    SheafModel,
    SheafModelBuildError (..),
    SheafModelVersion (..),
    withEmptySheafModel,
    withPreparedSheafModel,
    sheafModelLayoutDigest,
    sheafModelVersion,
    sheafModelObjects,
    sheafModelBasis,
    sheafModelRestrictions,
    modelCells,
    modelRestrictionsTo,
  )
where

import Data.Bifunctor (first)
import Data.Kind (Type)
import Data.Word (Word64)
import Moonlight.Core
  ( StableHashDigest,
    stableHashEncodingChunks,
    stableHashEncodingWord64LE,
  )
import Moonlight.Sheaf.Kernel.Basis
  ( SheafBasis,
    mkSheafBasis,
  )
import Moonlight.Sheaf.Index.Dense
  ( denseIndexCount,
    denseIndexValues,
  )
import Moonlight.Sheaf.Section.Morphism
  ( Restriction,
    RestrictionId (..),
    RestrictionKind (..),
    RestrictionPresentation,
    incidenceCoefficientValue,
    rId,
    rKind,
  )
import Moonlight.Sheaf.Section.ObjectIndex
  ( ObjectIndex,
    ObjectKey (..),
    SheafModelVersion (..),
  )
import Moonlight.Sheaf.Section.Restriction
  ( RestrictionIndex,
    RestrictionIndexError,
    buildRestrictionIndex,
    emptyRestrictionIndex,
    restrictionEndpointKeys,
    restrictionEntries,
    restrictionsTo,
  )

type ModelLayoutDigest :: Type
newtype ModelLayoutDigest = ModelLayoutDigest StableHashDigest
  deriving stock (Eq, Ord, Show)

type SheafModel :: Type -> Type -> Type -> Type
data SheafModel owner cell witness = SheafModel
  { smLayoutDigest :: !ModelLayoutDigest,
    smVersion :: !SheafModelVersion,
    smObjects :: !(ObjectIndex cell),
    smBasis :: !(SheafBasis cell),
    smRestrictions :: !(RestrictionIndex cell witness)
  }
  deriving stock (Eq, Show)

type role SheafModel nominal nominal representational

type SheafModelBuildError :: Type -> Type
data SheafModelBuildError cell
  = SheafModelRestrictionBuildError !(RestrictionIndexError cell)
  deriving stock (Eq, Show)

withEmptySheafModel ::
  Ord cell =>
  SheafModelVersion ->
  ObjectIndex cell ->
  (forall owner. SheafModel owner cell witness -> result) ->
  result
withEmptySheafModel version objects useModel =
  useModel
    SheafModel
      { smLayoutDigest = digestModelLayout version (denseIndexCount objects) emptyRestrictionIndex,
        smVersion = version,
        smObjects = objects,
        smBasis = mkSheafBasis (denseIndexValues objects),
        smRestrictions = emptyRestrictionIndex
      }

withPreparedSheafModel ::
  Ord cell =>
  SheafModelVersion ->
  ObjectIndex cell ->
  RestrictionPresentation morphism cell witness ->
  [morphism] ->
  (forall owner. SheafModel owner cell witness -> result) ->
  Either (SheafModelBuildError cell) result
withPreparedSheafModel version objects present morphisms useModel =
  useModel <$> buildSheafModel version objects present morphisms

buildSheafModel ::
  Ord cell =>
  SheafModelVersion ->
  ObjectIndex cell ->
  RestrictionPresentation morphism cell witness ->
  [morphism] ->
  Either (SheafModelBuildError cell) (SheafModel owner cell witness)
buildSheafModel version objects present morphisms = do
  restrictions <-
    first SheafModelRestrictionBuildError
      (buildRestrictionIndex objects present morphisms)
  pure
    SheafModel
      { smLayoutDigest = digestModelLayout version (denseIndexCount objects) restrictions,
        smVersion = version,
        smObjects = objects,
        smBasis = mkSheafBasis (denseIndexValues objects),
        smRestrictions = restrictions
      }

digestModelLayout :: SheafModelVersion -> Int -> RestrictionIndex cell witness -> ModelLayoutDigest
digestModelLayout version objectCount restrictions =
  ModelLayoutDigest
    ( stableHashEncodingChunks
        stableHashEncodingWord64LE
        ( fromIntegral (unSheafModelVersion version)
            : fromIntegral objectCount
            : concatMap (restrictionDigestWords restrictions) (restrictionEntries restrictions)
        )
    )

restrictionDigestWords :: RestrictionIndex cell witness -> Restriction cell witness -> [Word64]
restrictionDigestWords restrictions restriction =
  case restrictionEndpointKeys (rId restriction) restrictions of
    Nothing ->
      []
    Just (sourceKey, targetKey) ->
      [ fromIntegral (unRestrictionId (rId restriction)),
        fromIntegral (unObjectKey sourceKey),
        fromIntegral (unObjectKey targetKey)
      ]
        <> restrictionKindDigestWords (rKind restriction)

restrictionKindDigestWords :: RestrictionKind -> [Word64]
restrictionKindDigestWords restrictionKind =
  case restrictionKind of
    IncidenceRestriction coefficient ->
      [0, fromIntegral (incidenceCoefficientValue coefficient)]
    PortalRestriction ->
      [1, 0]

sheafModelLayoutDigest :: SheafModel owner cell witness -> ModelLayoutDigest
sheafModelLayoutDigest =
  smLayoutDigest
{-# INLINE sheafModelLayoutDigest #-}

sheafModelVersion :: SheafModel owner cell witness -> SheafModelVersion
sheafModelVersion =
  smVersion

sheafModelObjects :: SheafModel owner cell witness -> ObjectIndex cell
sheafModelObjects =
  smObjects

sheafModelBasis :: SheafModel owner cell witness -> SheafBasis cell
sheafModelBasis =
  smBasis

sheafModelRestrictions :: SheafModel owner cell witness -> RestrictionIndex cell witness
sheafModelRestrictions =
  smRestrictions

modelCells :: SheafModel owner cell witness -> [cell]
modelCells =
  denseIndexValues . smObjects

modelRestrictionsTo ::
  Ord cell =>
  cell ->
  SheafModel owner cell witness ->
  [Restriction cell witness]
modelRestrictionsTo cell model =
  restrictionsTo (smObjects model) cell (smRestrictions model)

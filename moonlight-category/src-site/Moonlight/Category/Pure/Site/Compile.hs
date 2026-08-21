-- | Compilation of a thin site presentation to a runtime-validated finite category,
-- with object and morphism lookup.
module Moonlight.Category.Pure.Site.Compile
  ( ThinSitePresentation (..),
    ThinSiteKernel,
    thinSiteKernelManifest,
    thinSiteKernelCodomain,
    thinSiteKernelObjectIds,
    ThinSiteLookupError (..),
    thinSitePresentation,
    thinPresentationToFinCat,
    thinSiteKernel,
    thinSiteFinObject,
    thinSiteFinMorphism,
    thinSiteFinMorphismByEndpoints,
    siteImportsAsFinCat,
  )
where

import Data.Kind (Type)
import Data.Bifunctor (first)
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import Moonlight.Category.Pure.Category (Category (identity))
import Moonlight.Category.Pure.FinCat
  ( FinCat,
    FinCatError,
    FinCatValidationError,
    FinMor,
    FinMorphismId (..),
    FinObjectId (..),
    FinObj,
    mkFinCat,
    mkFinMorphism,
    mkFinObject,
    denseThinEndpointMorphismsFromCategory,
    finCatExplicitCompositionMapView,
    finCatMorphismIdByEndpoints,
    trustedDenseThinFinCatFromReachabilityRows,
  )
import Moonlight.Category.Pure.Site.Core (SiteFinCatError (..), SiteManifest)
import Moonlight.Category.Pure.Site.Manifest
  ( validateSiteImportManifest,
    validateSiteManifestDetailed,
    validatedSiteObjectVector,
    validatedSiteReachabilityRows,
  )

type ThinSitePresentation :: Type -> Type
data ThinSitePresentation obj = ThinSitePresentation
  { thinPresentationObjectIds :: Map obj FinObjectId,
    thinPresentationPairIds :: Map (obj, obj) FinMorphismId,
    thinPresentationObjects :: Set FinObjectId,
    thinPresentationMorphisms :: Map (FinObjectId, FinObjectId) [FinMorphismId],
    thinPresentationComposition :: Map (FinMorphismId, FinMorphismId) FinMorphismId
  }

type ThinSiteKernel :: Type -> Type
data ThinSiteKernel obj = ThinSiteKernel
  { thinSiteKernelManifest :: SiteManifest obj,
    thinSiteKernelCodomain :: FinCat,
    thinSiteKernelObjectIds :: Map obj FinObjectId
  }
  deriving stock (Eq, Show)

type ThinSiteLookupError :: Type -> Type
data ThinSiteLookupError obj
  = ThinSiteUnknownObject obj
  | ThinSiteCodomainObjectMissing FinObjectId
  | ThinSiteUnknownMorphismPair obj obj
  | ThinSiteCodomainMorphismMissing FinMorphismId
  | ThinSiteCodomainMorphismInvalid FinCatError
  deriving stock (Eq, Show)

-- | Builds the explicit presentation, including the materialized composition
-- table via 'finCatExplicitCompositionMapView' — an output-bound @Θ(n³)@ witness
-- for a linear site on @n@ objects, dominating the @Θ(n²/w)@ dense validation
-- that precedes it. The record fields are lazy, so the cubic table is only paid
-- when 'thinPresentationComposition' is forced. Callers that need composition
-- queries rather than the explicit witness should use 'thinSiteKernel', which
-- stays on the dense handle and answers composition in
-- @O(1)@ without materializing.
thinSitePresentation :: ThinSiteKernel obj -> ThinSitePresentation obj
thinSitePresentation kernel =
  let objectIds = thinSiteKernelObjectIds kernel
      codomain = thinSiteKernelCodomain kernel
      objectSet = thinSiteFinObjectSet objectIds
      endpointPairIds = denseThinEndpointMorphismsFromCategory codomain
   in ThinSitePresentation
        { thinPresentationObjectIds = objectIds,
          thinPresentationPairIds = thinSitePairIdsFromEndpoints objectIds endpointPairIds,
          thinPresentationObjects = objectSet,
          thinPresentationMorphisms = thinSiteMorphismMap endpointPairIds,
          thinPresentationComposition = finCatExplicitCompositionMapView codomain
        }

thinSitePairIdsFromEndpoints :: Map obj FinObjectId -> Map (FinObjectId, FinObjectId) FinMorphismId -> Map (obj, obj) FinMorphismId
thinSitePairIdsFromEndpoints objectIds endpointPairIds =
  objectIds
    & Map.toAscList
    >>= ( \(sourceObject, sourceId) ->
            objectIds
              & Map.toAscList
              >>= ( \(targetObject, targetId) ->
                      case Map.lookup (sourceId, targetId) endpointPairIds of
                        Nothing -> []
                        Just morphismId -> [((sourceObject, targetObject), morphismId)]
                  )
        )
    & Map.fromDistinctAscList

thinSiteObjectIds :: Ord obj => [obj] -> Map obj FinObjectId
thinSiteObjectIds objects =
  objects
    & zip [0 ..]
    & fmap (\(idx, obj) -> (obj, FinObjectId idx))
    & Map.fromList

thinSiteFinObjectSet :: Map obj FinObjectId -> Set FinObjectId
thinSiteFinObjectSet objectIds =
  objectIds
    & Map.elems
    & Set.fromList

thinSiteMorphismMap :: Map (FinObjectId, FinObjectId) FinMorphismId -> Map (FinObjectId, FinObjectId) [FinMorphismId]
thinSiteMorphismMap =
  fmap (: [])

thinPresentationToFinCat ::
  ThinSitePresentation obj ->
  Either (NonEmpty FinCatValidationError) FinCat
thinPresentationToFinCat presentation =
  mkFinCat
    (thinPresentationObjects presentation)
    (thinPresentationMorphisms presentation)
    (thinPresentationComposition presentation)

thinSiteKernel :: Ord obj => SiteManifest obj -> Either (SiteFinCatError obj) (ThinSiteKernel obj)
thinSiteKernel manifest =
  case validateSiteManifestDetailed manifest of
    Right validatedManifest ->
      let objectIds = thinSiteObjectIds (Vector.toList (validatedSiteObjectVector validatedManifest))
          codomain =
            trustedDenseThinFinCatFromReachabilityRows
              (thinSiteFinObjectSet objectIds)
              (validatedSiteReachabilityRows validatedManifest)
       in Right
            ThinSiteKernel
              { thinSiteKernelManifest = manifest,
                thinSiteKernelCodomain = codomain,
                thinSiteKernelObjectIds = objectIds
              }
    Left errors ->
      Left (SiteManifestInvalid errors)

thinSiteFinObject :: Ord obj => ThinSiteKernel obj -> obj -> Either (ThinSiteLookupError obj) FinObj
thinSiteFinObject kernel objectValue =
  case Map.lookup objectValue (thinSiteKernelObjectIds kernel) of
    Nothing ->
      Left (ThinSiteUnknownObject objectValue)
    Just objectId ->
      case mkFinObject (thinSiteKernelCodomain kernel) objectId of
        Left _ ->
          Left (ThinSiteCodomainObjectMissing objectId)
        Right finObject ->
          Right finObject

thinSiteFinMorphism :: Ord obj => ThinSiteKernel obj -> NonEmpty obj -> Either (ThinSiteLookupError obj) FinMor
thinSiteFinMorphism kernel nodes =
  thinSiteFinMorphismByEndpoints
    kernel
    (NonEmpty.head nodes)
    (NonEmpty.last nodes)

thinSiteFinMorphismByEndpoints ::
  Ord obj =>
  ThinSiteKernel obj ->
  obj ->
  obj ->
  Either (ThinSiteLookupError obj) FinMor
thinSiteFinMorphismByEndpoints kernel sourceValue targetValue =
  if sourceValue == targetValue
    then do
      sourceObject <- thinSiteFinObject kernel sourceValue
      first ThinSiteCodomainMorphismInvalid (identity (thinSiteKernelCodomain kernel) sourceObject)
    else
      case thinSiteMorphismIdByEndpoints kernel sourceValue targetValue of
        Nothing ->
          Left (ThinSiteUnknownMorphismPair sourceValue targetValue)
        Just morId ->
          case mkFinMorphism (thinSiteKernelCodomain kernel) morId of
            Left _ ->
              Left (ThinSiteCodomainMorphismMissing morId)
            Right finMorphism ->
              Right finMorphism

thinSiteMorphismIdByEndpoints :: Ord obj => ThinSiteKernel obj -> obj -> obj -> Maybe FinMorphismId
thinSiteMorphismIdByEndpoints kernel sourceValue targetValue = do
  sourceId <- Map.lookup sourceValue (thinSiteKernelObjectIds kernel)
  targetId <- Map.lookup targetValue (thinSiteKernelObjectIds kernel)
  finCatMorphismIdByEndpoints (thinSiteKernelCodomain kernel) sourceId targetId

-- | Compile only the import category. Cover axioms are deliberately outside this
-- boundary; use 'thinSiteKernel' when a validated site is required.
siteImportsAsFinCat :: Ord obj => SiteManifest obj -> Either (SiteFinCatError obj) FinCat
siteImportsAsFinCat manifest =
  case validateSiteImportManifest manifest of
    Right validatedManifest ->
      let objectIds = thinSiteObjectIds (Vector.toList (validatedSiteObjectVector validatedManifest))
       in Right
            ( trustedDenseThinFinCatFromReachabilityRows
                (thinSiteFinObjectSet objectIds)
                (validatedSiteReachabilityRows validatedManifest)
            )
    Left errors ->
      Left (SiteManifestInvalid errors)

{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Cosheaf.Support
  ( SupportCarrier,
    scHasAny,
    scContains,
    supportCarrierItems,
    supportCarrierCount,
    CosheafSupportCertificate (..),
    CosheafSupportPlan,
    cspMaxDegree,
    cspObjects,
    cspMorphisms,
    cspCostalkKeys,
    cspNerveRows,
    cspChainCells,
    cspFootprintMeasures,
    cspCertificate,
    CosheafSupportFailure (..),
    PreparedCosheafSupport,
    pcsCosheaf,
    pcsPlan,
    pcsCorestrictions,
    pcsCostalkKeysByObject,
    cosheafSupportPlanFromKeys,
    prepareCosheafSupport,
    fullFiniteCosheafChainPreparedSupport,
    supportedCorestrictions,
    validateCosheafSupportPlan,
    fullFiniteCosheafChainSupportPlan,
    h0SupportPlan,
    h0PreparedSupport,
    homologyWindowSupportPlan,
    homologyWindowPreparedSupport,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Foldable (traverse_)
import Data.Kind (Type)
import Moonlight.Cosheaf.Chain.Finite.Types
  ( CosheafChainBasisKey,
    CosheafChainFailure,
    CosheafNerveChainKey,
  )
import Moonlight.Cosheaf.Finite
  ( CompiledCorestriction,
    CostalkKey (..),
    FiniteCostalk,
    FiniteCosheaf,
    ccMorphism,
    ccMorphismKey,
    ccSourceObjectKey,
    ccSourceToTarget,
    ccTargetObjectKey,
    fcCostalks,
    fcSiteIndex,
    finiteCostalkKeyIntSet,
    finiteCostalkKeys,
    finiteCosheafCorestrictions,
  )
import Moonlight.Cosheaf.SiteIndex
  ( CosheafMorphismKey,
    cosheafSiteObjectIndex,
  )
import Moonlight.Cosheaf.Support.Carrier
  ( SupportCarrier,
    scContains,
    scHasAny,
    supportCarrierCount,
    supportCarrierFromList,
    supportCarrierItems,
  )
import Moonlight.Cosheaf.Support.Footprint
  ( supportFootprintMeasure,
  )
import Moonlight.Homology
  ( HomologicalDegree (..),
  )
import Moonlight.Sheaf.Footprint
  ( FootprintMeasure,
    FootprintMeasureUnit (..),
  )
import Moonlight.Sheaf.Index.Dense
  ( denseIndexKeys,
  )
import Moonlight.Sheaf.Section.ObjectIndex
  ( ObjectKey (..),
  )
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism (..),
    Site (..),
  )
import Numeric.Natural (Natural)

type CosheafSupportCertificate :: Type
data CosheafSupportCertificate
  = FullCosheafSupport
  | HomologyWindowSupport !HomologicalDegree
  | H0Support
  | BoundaryClosedSupport
  deriving stock (Eq, Ord, Show, Read)

type CosheafSupportPlan :: Type
data CosheafSupportPlan = CosheafSupportPlan
  { cspMaxDegree :: !HomologicalDegree,
    cspObjects :: !(SupportCarrier ObjectKey),
    cspMorphisms :: !(SupportCarrier CosheafMorphismKey),
    cspCostalkKeys :: !(SupportCarrier (ObjectKey, CostalkKey)),
    cspNerveRows :: !(Maybe (SupportCarrier CosheafNerveChainKey)),
    cspChainCells :: !(Maybe (SupportCarrier CosheafChainBasisKey)),
    cspFootprintMeasures :: ![FootprintMeasure Natural],
    cspCertificate :: !CosheafSupportCertificate
  }

type CosheafSupportFailure :: Type -> Type -> Type -> Type
data CosheafSupportFailure obj mor value
  = CosheafSupportDegreeTooLarge !Natural
  | CosheafSupportObjectUnknown !ObjectKey
  | CosheafSupportMorphismUnknown !CosheafMorphismKey
  | CosheafSupportCostalkObjectPruned !ObjectKey !CostalkKey
  | CosheafSupportCostalkUnknown !ObjectKey !CostalkKey
  | CosheafSupportMorphismEndpointPruned !(CheckedMorphism obj mor)
  | CosheafSupportCorestrictionExits !(CheckedMorphism obj mor) !CostalkKey !CostalkKey
  | CosheafSupportChainFailed !(CosheafChainFailure obj mor value)
  deriving stock (Eq, Show)

type PreparedCosheafSupport :: Type -> Type -> Type
data PreparedCosheafSupport site value = PreparedCosheafSupport
  { preparedCosheafSupportCosheafInternal :: !(FiniteCosheaf site value),
    preparedCosheafSupportPlanInternal :: !CosheafSupportPlan,
    preparedCosheafSupportCorestrictionsInternal :: ![CompiledCorestriction (SiteObject site) (SiteMorphism site)],
    preparedCosheafSupportCostalkKeysInternal :: !(IntMap IntSet)
  }

pcsCosheaf :: PreparedCosheafSupport site value -> FiniteCosheaf site value
pcsCosheaf =
  preparedCosheafSupportCosheafInternal

pcsPlan :: PreparedCosheafSupport site value -> CosheafSupportPlan
pcsPlan =
  preparedCosheafSupportPlanInternal

pcsCorestrictions ::
  PreparedCosheafSupport site value ->
  [CompiledCorestriction (SiteObject site) (SiteMorphism site)]
pcsCorestrictions =
  preparedCosheafSupportCorestrictionsInternal

pcsCostalkKeysByObject :: PreparedCosheafSupport site value -> IntMap IntSet
pcsCostalkKeysByObject =
  preparedCosheafSupportCostalkKeysInternal

cosheafSupportPlanFromKeys ::
  Natural ->
  [ObjectKey] ->
  [CosheafMorphismKey] ->
  [(ObjectKey, CostalkKey)] ->
  FiniteCosheaf site value ->
  Either
    (CosheafSupportFailure (SiteObject site) (SiteMorphism site) value)
    CosheafSupportPlan
cosheafSupportPlanFromKeys =
  cosheafSupportPlanWithCertificateFromKeys BoundaryClosedSupport

cosheafSupportPlanWithCertificateFromKeys ::
  CosheafSupportCertificate ->
  Natural ->
  [ObjectKey] ->
  [CosheafMorphismKey] ->
  [(ObjectKey, CostalkKey)] ->
  FiniteCosheaf site value ->
  Either
    (CosheafSupportFailure (SiteObject site) (SiteMorphism site) value)
    CosheafSupportPlan
cosheafSupportPlanWithCertificateFromKeys certificate maxDegreeValue objectKeysValue morphismKeysValue costalkKeysValue cosheaf =
  pcsPlan
    <$> cosheafPreparedSupportWithCertificateFromKeys
      certificate
      maxDegreeValue
      objectKeysValue
      morphismKeysValue
      costalkKeysValue
      cosheaf

cosheafPreparedSupportWithCertificateFromKeys ::
  CosheafSupportCertificate ->
  Natural ->
  [ObjectKey] ->
  [CosheafMorphismKey] ->
  [(ObjectKey, CostalkKey)] ->
  FiniteCosheaf site value ->
  Either
    (CosheafSupportFailure (SiteObject site) (SiteMorphism site) value)
    (PreparedCosheafSupport site value)
cosheafPreparedSupportWithCertificateFromKeys certificate maxDegreeValue objectKeysValue morphismKeysValue costalkKeysValue cosheaf = do
  maxDegreeValueInt <- naturalToBoundedInt maxDegreeValue
  let plan =
        supportPlanFromCarriers
          (HomologicalDegree maxDegreeValueInt)
          certificate
          (supportCarrierFromList objectKeysValue)
          (supportCarrierFromList morphismKeysValue)
          (supportCarrierFromList costalkKeysValue)
          Nothing
          Nothing
          (finiteSupportFootprintMeasures cosheaf objectKeysValue morphismKeysValue costalkKeysValue)
  prepareCosheafSupport cosheaf plan

fullFiniteCosheafChainSupportPlan ::
  Natural ->
  FiniteCosheaf site value ->
  Either
    (CosheafSupportFailure (SiteObject site) (SiteMorphism site) value)
    CosheafSupportPlan
fullFiniteCosheafChainSupportPlan maxDegreeValue cosheaf =
  cosheafSupportPlanWithCertificateFromKeys
    FullCosheafSupport
    maxDegreeValue
    (allObjectKeys cosheaf)
    (fmap ccMorphismKey (finiteCosheafCorestrictions cosheaf))
    (allCostalkKeys cosheaf)
    cosheaf

fullFiniteCosheafChainPreparedSupport ::
  Natural ->
  FiniteCosheaf site value ->
  Either
    (CosheafSupportFailure (SiteObject site) (SiteMorphism site) value)
    (PreparedCosheafSupport site value)
fullFiniteCosheafChainPreparedSupport maxDegreeValue cosheaf =
  cosheafPreparedSupportWithCertificateFromKeys
    FullCosheafSupport
    maxDegreeValue
    (allObjectKeys cosheaf)
    (fmap ccMorphismKey (finiteCosheafCorestrictions cosheaf))
    (allCostalkKeys cosheaf)
    cosheaf

h0SupportPlan ::
  FiniteCosheaf site value ->
  Either
    (CosheafSupportFailure (SiteObject site) (SiteMorphism site) value)
    CosheafSupportPlan
h0SupportPlan cosheaf =
  cosheafSupportPlanWithCertificateFromKeys
    H0Support
    1
    (allObjectKeys cosheaf)
    (fmap ccMorphismKey (finiteCosheafCorestrictions cosheaf))
    (allCostalkKeys cosheaf)
    cosheaf

h0PreparedSupport ::
  FiniteCosheaf site value ->
  Either
    (CosheafSupportFailure (SiteObject site) (SiteMorphism site) value)
    (PreparedCosheafSupport site value)
h0PreparedSupport cosheaf =
  cosheafPreparedSupportWithCertificateFromKeys
    H0Support
    1
    (allObjectKeys cosheaf)
    (fmap ccMorphismKey (finiteCosheafCorestrictions cosheaf))
    (allCostalkKeys cosheaf)
    cosheaf

homologyWindowSupportPlan ::
  HomologicalDegree ->
  FiniteCosheaf site value ->
  Either
    (CosheafSupportFailure (SiteObject site) (SiteMorphism site) value)
    CosheafSupportPlan
homologyWindowSupportPlan degreeValue@(HomologicalDegree degreeInt) cosheaf =
  cosheafSupportPlanWithCertificateFromKeys
    (HomologyWindowSupport degreeValue)
    (fromIntegral (max 1 (degreeInt + 1)))
    (allObjectKeys cosheaf)
    (fmap ccMorphismKey (finiteCosheafCorestrictions cosheaf))
    (allCostalkKeys cosheaf)
    cosheaf

homologyWindowPreparedSupport ::
  HomologicalDegree ->
  FiniteCosheaf site value ->
  Either
    (CosheafSupportFailure (SiteObject site) (SiteMorphism site) value)
    (PreparedCosheafSupport site value)
homologyWindowPreparedSupport degreeValue@(HomologicalDegree degreeInt) cosheaf =
  cosheafPreparedSupportWithCertificateFromKeys
    (HomologyWindowSupport degreeValue)
    (fromIntegral (max 1 (degreeInt + 1)))
    (allObjectKeys cosheaf)
    (fmap ccMorphismKey (finiteCosheafCorestrictions cosheaf))
    (allCostalkKeys cosheaf)
    cosheaf

validateCosheafSupportPlan ::
  FiniteCosheaf site value ->
  CosheafSupportPlan ->
  Either
    (CosheafSupportFailure (SiteObject site) (SiteMorphism site) value)
    ()
validateCosheafSupportPlan cosheaf plan = do
  _ <- prepareCosheafSupport cosheaf plan
  pure ()

prepareCosheafSupport ::
  FiniteCosheaf site value ->
  CosheafSupportPlan ->
  Either
    (CosheafSupportFailure (SiteObject site) (SiteMorphism site) value)
    (PreparedCosheafSupport site value)
prepareCosheafSupport cosheaf plan = do
  traverse_ (validateObjectKnown knownObjectKeys) (supportCarrierItems (cspObjects plan))
  traverse_ (validateMorphismKnown knownMorphismKeys) (supportCarrierItems (cspMorphisms plan))
  traverse_ (validateCostalkKey cosheaf plan) retainedCostalkKeys
  traverse_ (validateCorestriction plan retainedCostalkKeysByObject) retainedCorestrictions
  pure
    PreparedCosheafSupport
      { preparedCosheafSupportCosheafInternal = cosheaf,
        preparedCosheafSupportPlanInternal = plan,
        preparedCosheafSupportCorestrictionsInternal = retainedCorestrictions,
        preparedCosheafSupportCostalkKeysInternal = retainedCostalkKeysByObject
      }
  where
    knownObjectKeys =
      supportCarrierFromList (allObjectKeys cosheaf)

    knownMorphismKeys =
      supportCarrierFromList (fmap ccMorphismKey (finiteCosheafCorestrictions cosheaf))

    retainedCostalkKeys =
      supportCarrierItems (cspCostalkKeys plan)

    retainedCostalkKeysByObject =
      costalkKeysByObject retainedCostalkKeys

    retainedCorestrictions =
      supportedCorestrictions cosheaf plan

supportPlanFromCarriers ::
  HomologicalDegree ->
  CosheafSupportCertificate ->
  SupportCarrier ObjectKey ->
  SupportCarrier CosheafMorphismKey ->
  SupportCarrier (ObjectKey, CostalkKey) ->
  Maybe (SupportCarrier CosheafNerveChainKey) ->
  Maybe (SupportCarrier CosheafChainBasisKey) ->
  [FootprintMeasure Natural] ->
  CosheafSupportPlan
supportPlanFromCarriers maxDegreeValue certificate objectCarrier morphismCarrier costalkCarrier rowCarrier cellCarrier measures =
  CosheafSupportPlan
    { cspMaxDegree = maxDegreeValue,
      cspObjects = objectCarrier,
      cspMorphisms = morphismCarrier,
      cspCostalkKeys = costalkCarrier,
      cspNerveRows = rowCarrier,
      cspChainCells = cellCarrier,
      cspFootprintMeasures = measures,
      cspCertificate = certificate
    }

naturalToBoundedInt :: Natural -> Either (CosheafSupportFailure obj mor value) Int
naturalToBoundedInt value
  | value <= fromIntegral (maxBound :: Int) =
      Right (fromIntegral value)
  | otherwise =
      Left (CosheafSupportDegreeTooLarge value)

allObjectKeys :: FiniteCosheaf site value -> [ObjectKey]
allObjectKeys =
  denseIndexKeys . cosheafSiteObjectIndex . fcSiteIndex

allCostalkKeys :: FiniteCosheaf site value -> [(ObjectKey, CostalkKey)]
allCostalkKeys cosheaf =
  foldMap costalkKeyPairs (IntMap.toAscList (fcCostalks cosheaf))
  where
    costalkKeyPairs :: (Int, FiniteCostalk obj value) -> [(ObjectKey, CostalkKey)]
    costalkKeyPairs (unObjectKeyValue, costalkValue) =
      fmap (ObjectKey unObjectKeyValue,) (finiteCostalkKeys costalkValue)

validateObjectKnown ::
  SupportCarrier ObjectKey ->
  ObjectKey ->
  Either (CosheafSupportFailure obj mor value) ()
validateObjectKnown knownObjectKeys objectKey =
  if scContains knownObjectKeys objectKey
    then Right ()
    else Left (CosheafSupportObjectUnknown objectKey)

validateMorphismKnown ::
  SupportCarrier CosheafMorphismKey ->
  CosheafMorphismKey ->
  Either (CosheafSupportFailure obj mor value) ()
validateMorphismKnown knownMorphismKeys morphismKey =
  if scContains knownMorphismKeys morphismKey
    then Right ()
    else Left (CosheafSupportMorphismUnknown morphismKey)

validateCostalkKey ::
  FiniteCosheaf site value ->
  CosheafSupportPlan ->
  (ObjectKey, CostalkKey) ->
  Either (CosheafSupportFailure obj mor value) ()
validateCostalkKey cosheaf plan (objectKey, costalkKey)
  | not (scContains (cspObjects plan) objectKey) =
      Left (CosheafSupportCostalkObjectPruned objectKey costalkKey)
  | otherwise =
      case IntMap.lookup (unObjectKey objectKey) (fcCostalks cosheaf) of
        Nothing ->
          Left (CosheafSupportCostalkUnknown objectKey costalkKey)
        Just costalkValue ->
          if IntSet.member (unCostalkKey costalkKey) (finiteCostalkKeyIntSet costalkValue)
            then Right ()
            else Left (CosheafSupportCostalkUnknown objectKey costalkKey)

supportedCorestrictions :: FiniteCosheaf site value -> CosheafSupportPlan -> [CompiledCorestriction (SiteObject site) (SiteMorphism site)]
supportedCorestrictions cosheaf plan =
  filter
    (\corestrictionValue -> scContains (cspMorphisms plan) (ccMorphismKey corestrictionValue))
    (finiteCosheafCorestrictions cosheaf)

validateCorestriction ::
  CosheafSupportPlan ->
  IntMap IntSet ->
  CompiledCorestriction obj mor ->
  Either (CosheafSupportFailure obj mor value) ()
validateCorestriction plan retainedCostalkKeysByObject corestrictionValue = do
  if scContains (cspObjects plan) (ccSourceObjectKey corestrictionValue)
    && scContains (cspObjects plan) (ccTargetObjectKey corestrictionValue)
    then Right ()
    else Left (CosheafSupportMorphismEndpointPruned (ccMorphism corestrictionValue))
  traverse_
    validateRetainedSourceKey
    (retainedCostalkKeysAtObject (ccSourceObjectKey corestrictionValue) retainedCostalkKeysByObject)
  pure ()
  where
    validateRetainedSourceKey sourceKey =
      case IntMap.lookup (unCostalkKey sourceKey) (ccSourceToTarget corestrictionValue) of
        Nothing ->
          Left (CosheafSupportCostalkUnknown (ccSourceObjectKey corestrictionValue) sourceKey)
        Just targetKey ->
          if retainedCostalkKeyAtObject (ccTargetObjectKey corestrictionValue) targetKey retainedCostalkKeysByObject
            then Right ()
            else Left (CosheafSupportCorestrictionExits (ccMorphism corestrictionValue) sourceKey targetKey)

costalkKeysByObject :: [(ObjectKey, CostalkKey)] -> IntMap IntSet
costalkKeysByObject =
  IntMap.fromListWith
    IntSet.union
    . fmap
      ( \(objectKey, costalkKey) ->
          (unObjectKey objectKey, IntSet.singleton (unCostalkKey costalkKey))
      )

retainedCostalkKeysAtObject :: ObjectKey -> IntMap IntSet -> [CostalkKey]
retainedCostalkKeysAtObject objectKey =
  maybe [] (fmap CostalkKey . IntSet.toAscList) . IntMap.lookup (unObjectKey objectKey)

retainedCostalkKeyAtObject :: ObjectKey -> CostalkKey -> IntMap IntSet -> Bool
retainedCostalkKeyAtObject objectKey costalkKey =
  maybe False (IntSet.member (unCostalkKey costalkKey)) . IntMap.lookup (unObjectKey objectKey)

finiteSupportFootprintMeasures ::
  FiniteCosheaf site value ->
  [ObjectKey] ->
  [CosheafMorphismKey] ->
  [(ObjectKey, CostalkKey)] ->
  [FootprintMeasure Natural]
finiteSupportFootprintMeasures cosheaf retainedObjects retainedMorphisms retainedCostalks =
  [ supportFootprintMeasure ContextOrdinalUnit totalObjectCount retainedObjectCount,
    supportFootprintMeasure CoboundaryRestrictionUnit totalMorphismCount retainedMorphismCount,
    supportFootprintMeasure SupportCellUnit totalCostalkCount retainedCostalkCount
  ]
  where
    totalObjectCount =
      fromIntegral (length (allObjectKeys cosheaf))

    retainedObjectCount =
      supportCarrierCount (supportCarrierFromList retainedObjects)

    totalMorphismCount =
      fromIntegral (length (finiteCosheafCorestrictions cosheaf))

    retainedMorphismCount =
      supportCarrierCount (supportCarrierFromList retainedMorphisms)

    totalCostalkCount =
      fromIntegral (length (allCostalkKeys cosheaf))

    retainedCostalkCount =
      supportCarrierCount (supportCarrierFromList retainedCostalks)

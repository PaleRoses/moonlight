{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Sheaf.Site.Plan
  ( CoverSlotKey (..),
    CoverSlot,
    coverSlotKey,
    coverSlotArrow,
    EffectiveCoverPlan,
    EffectiveCoverPlanFailure (..),
    PullbackCoverSlotPlan,
    pcspOriginalSlot,
    pcspPulledSlot,
    pcspOriginalArrow,
    pcspPulledArrow,
    pcspRestrictToOriginal,
    pcspPullbackSquare,
    PullbackCoverPlan,
    pcpAlong,
    pcpOriginalCover,
    pcpPulledCover,
    pcpSlotPlans,
    CrossCoverOverlapPlan,
    ccopLeftSlot,
    ccopRightSlot,
    ccopLeftArrow,
    ccopRightArrow,
    ccopPullbackSquare,
    CommonRefinementPlan,
    crpTarget,
    crpLeftCover,
    crpRightCover,
    crpCrossOverlaps,
    CoverId (..),
    OverlapPlan,
    opLeftSlot,
    opRightSlot,
    opLeftArrow,
    opRightArrow,
    opPullbackSquare,
    CoverPlan,
    cpCoverId,
    cpEffectiveCover,
    cpTargetKey,
    cpSourceKeys,
    SitePlans,
    spCoversById,
    spCoverIdsByTarget,
    SitePlanBuildError (..),
    effectiveCoverFamily,
    effectiveCoverSlots,
    effectiveCoverOverlapPlans,
    effectiveCoverSlotKeys,
    effectiveCoverSlotArrows,
    effectiveCoverSlotSources,
    effectiveCoverSlotCount,
    prepareEffectiveCoverPlan,
    identityEffectiveCoverPlan,
    preparePullbackCoverPlan,
    canonicalizePulledCoverPlan,
    pullbackEffectiveCoverPlanAlong,
    prepareCommonRefinementPlan,
    emptySitePlans,
    prepareSitePlans,
    siteCoverPlansAt,
    siteCoverPlanById,
  )
where

import Data.Bifunctor (first)
import Data.Foldable (toList)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.List (tails)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Moonlight.Core (DenseKey (..))
import Moonlight.Sheaf.Index.Dense (denseIndexKeyOf)
import Moonlight.Sheaf.Section.ObjectIndex
  ( ObjectIndex,
    ObjectKey (..),
  )
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism (..),
    CoverConstructionError,
    CoveringFamily,
    PullbackSquare (..),
    Site (..),
    SiteConstructionError,
    coverArrows,
    coverSources,
    coverTarget,
    identityCover,
    mkCoveringFamily,
    pullbackPair,
  )

type CoverSlotKey :: Type
newtype CoverSlotKey = CoverSlotKey
  { unCoverSlotKey :: Int
  }
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (Enum)

instance DenseKey CoverSlotKey where
  encodeDenseKey =
    unCoverSlotKey
  {-# INLINE encodeDenseKey #-}

  decodeDenseKey =
    CoverSlotKey
  {-# INLINE decodeDenseKey #-}

type CoverSlot :: Type -> Type -> Type
data CoverSlot obj mor = CoverSlot
  { coverSlotKeyInternal :: !CoverSlotKey,
    coverSlotArrowInternal :: !(CheckedMorphism obj mor)
  }
  deriving stock (Eq, Ord, Show)

type EffectiveCoverPlan :: Type -> Type -> Type
data EffectiveCoverPlan obj mor = EffectiveCoverPlan
  { ecpCover :: !(CoveringFamily obj mor),
    ecpSlots :: !(IntMap (CoverSlot obj mor)),
    ecpOverlapPlans :: ![OverlapPlan obj mor]
  }
  deriving stock (Eq, Show)

type EffectiveCoverPlanFailure :: Type -> Type -> Type
data EffectiveCoverPlanFailure obj mor
  = EffectiveCoverPlanTargetMismatch !obj !obj
  | EffectiveCoverPlanInternalEmptyCover
  | EffectiveCoverPlanMissingPullback
      !(CheckedMorphism obj mor)
      !(CheckedMorphism obj mor)
  | EffectiveCoverPlanMalformedPullbackLeg
      !(PullbackSquare obj mor)
  | EffectiveCoverPlanPulledCoverMalformed
      !(CoverConstructionError obj)
  | EffectiveCoverPlanSiteConstructionFailed
      !(SiteConstructionError obj mor)
  | EffectiveCoverPlanCommonRefinementMissing
      !(CoveringFamily obj mor)
      !(CoveringFamily obj mor)
  | EffectiveCoverPlanCanonicalPulledCoverMismatch
      !(CoveringFamily obj mor)
      !(CoveringFamily obj mor)
  deriving stock (Eq, Show)

type PullbackCoverSlotPlan :: Type -> Type -> Type
data PullbackCoverSlotPlan obj mor = PullbackCoverSlotPlan
  { pullbackCoverSlotOriginalSlotInternal :: !CoverSlotKey,
    pullbackCoverSlotPulledSlotInternal :: !CoverSlotKey,
    pullbackCoverSlotOriginalArrowInternal :: !(CheckedMorphism obj mor),
    pullbackCoverSlotPulledArrowInternal :: !(CheckedMorphism obj mor),
    pullbackCoverSlotRestrictToOriginalInternal :: !(CheckedMorphism obj mor),
    pullbackCoverSlotSquareInternal :: !(PullbackSquare obj mor)
  }
  deriving stock (Eq, Show)

type PullbackCoverPlan :: Type -> Type -> Type
data PullbackCoverPlan obj mor = PullbackCoverPlan
  { pullbackCoverAlongInternal :: !(CheckedMorphism obj mor),
    pullbackCoverOriginalInternal :: !(EffectiveCoverPlan obj mor),
    pullbackCoverPulledInternal :: !(EffectiveCoverPlan obj mor),
    pullbackCoverSlotsInternal :: !(IntMap (PullbackCoverSlotPlan obj mor))
  }
  deriving stock (Eq, Show)

type CrossCoverOverlapPlan :: Type -> Type -> Type
data CrossCoverOverlapPlan obj mor = CrossCoverOverlapPlan
  { crossCoverOverlapLeftSlotInternal :: !CoverSlotKey,
    crossCoverOverlapRightSlotInternal :: !CoverSlotKey,
    crossCoverOverlapLeftArrowInternal :: !(CheckedMorphism obj mor),
    crossCoverOverlapRightArrowInternal :: !(CheckedMorphism obj mor),
    crossCoverOverlapSquareInternal :: !(PullbackSquare obj mor)
  }
  deriving stock (Eq, Show)

type CommonRefinementPlan :: Type -> Type -> Type
data CommonRefinementPlan obj mor = CommonRefinementPlan
  { commonRefinementTargetInternal :: !obj,
    commonRefinementLeftCoverInternal :: !(EffectiveCoverPlan obj mor),
    commonRefinementRightCoverInternal :: !(EffectiveCoverPlan obj mor),
    commonRefinementCrossOverlapsInternal :: ![CrossCoverOverlapPlan obj mor]
  }
  deriving stock (Eq, Show)

type CoverId :: Type
newtype CoverId = CoverId
  { unCoverId :: Int
  }
  deriving stock (Eq, Ord, Show, Read)

type OverlapPlan :: Type -> Type -> Type
data OverlapPlan obj mor = OverlapPlan
  { overlapLeftSlotInternal :: !CoverSlotKey,
    overlapRightSlotInternal :: !CoverSlotKey,
    overlapLeftArrowInternal :: !(CheckedMorphism obj mor),
    overlapRightArrowInternal :: !(CheckedMorphism obj mor),
    overlapSquareInternal :: !(PullbackSquare obj mor)
  }
  deriving stock (Eq, Show)

type CoverPlan :: Type -> Type -> Type
data CoverPlan obj mor = CoverPlan
  { coverPlanIdInternal :: !CoverId,
    coverPlanEffectiveCoverInternal :: !(EffectiveCoverPlan obj mor),
    coverPlanTargetKeyInternal :: !ObjectKey,
    coverPlanSourceKeysInternal :: ![ObjectKey]
  }
  deriving stock (Eq, Show)

type SitePlans :: Type -> Type -> Type
data SitePlans obj mor = SitePlans
  { siteCoversByIdInternal :: !(IntMap (CoverPlan obj mor)),
    siteCoverIdsByTargetInternal :: !(IntMap IntSet)
  }
  deriving stock (Eq, Show)

coverSlotKey :: CoverSlot obj mor -> CoverSlotKey
coverSlotKey = coverSlotKeyInternal

coverSlotArrow :: CoverSlot obj mor -> CheckedMorphism obj mor
coverSlotArrow = coverSlotArrowInternal

pcspOriginalSlot :: PullbackCoverSlotPlan obj mor -> CoverSlotKey
pcspOriginalSlot = pullbackCoverSlotOriginalSlotInternal

pcspPulledSlot :: PullbackCoverSlotPlan obj mor -> CoverSlotKey
pcspPulledSlot = pullbackCoverSlotPulledSlotInternal

pcspOriginalArrow :: PullbackCoverSlotPlan obj mor -> CheckedMorphism obj mor
pcspOriginalArrow = pullbackCoverSlotOriginalArrowInternal

pcspPulledArrow :: PullbackCoverSlotPlan obj mor -> CheckedMorphism obj mor
pcspPulledArrow = pullbackCoverSlotPulledArrowInternal

pcspRestrictToOriginal :: PullbackCoverSlotPlan obj mor -> CheckedMorphism obj mor
pcspRestrictToOriginal = pullbackCoverSlotRestrictToOriginalInternal

pcspPullbackSquare :: PullbackCoverSlotPlan obj mor -> PullbackSquare obj mor
pcspPullbackSquare = pullbackCoverSlotSquareInternal

pcpAlong :: PullbackCoverPlan obj mor -> CheckedMorphism obj mor
pcpAlong = pullbackCoverAlongInternal

pcpOriginalCover :: PullbackCoverPlan obj mor -> EffectiveCoverPlan obj mor
pcpOriginalCover = pullbackCoverOriginalInternal

pcpPulledCover :: PullbackCoverPlan obj mor -> EffectiveCoverPlan obj mor
pcpPulledCover = pullbackCoverPulledInternal

pcpSlotPlans :: PullbackCoverPlan obj mor -> IntMap (PullbackCoverSlotPlan obj mor)
pcpSlotPlans = pullbackCoverSlotsInternal

ccopLeftSlot :: CrossCoverOverlapPlan obj mor -> CoverSlotKey
ccopLeftSlot = crossCoverOverlapLeftSlotInternal

ccopRightSlot :: CrossCoverOverlapPlan obj mor -> CoverSlotKey
ccopRightSlot = crossCoverOverlapRightSlotInternal

ccopLeftArrow :: CrossCoverOverlapPlan obj mor -> CheckedMorphism obj mor
ccopLeftArrow = crossCoverOverlapLeftArrowInternal

ccopRightArrow :: CrossCoverOverlapPlan obj mor -> CheckedMorphism obj mor
ccopRightArrow = crossCoverOverlapRightArrowInternal

ccopPullbackSquare :: CrossCoverOverlapPlan obj mor -> PullbackSquare obj mor
ccopPullbackSquare = crossCoverOverlapSquareInternal

crpTarget :: CommonRefinementPlan obj mor -> obj
crpTarget = commonRefinementTargetInternal

crpLeftCover :: CommonRefinementPlan obj mor -> EffectiveCoverPlan obj mor
crpLeftCover = commonRefinementLeftCoverInternal

crpRightCover :: CommonRefinementPlan obj mor -> EffectiveCoverPlan obj mor
crpRightCover = commonRefinementRightCoverInternal

crpCrossOverlaps :: CommonRefinementPlan obj mor -> [CrossCoverOverlapPlan obj mor]
crpCrossOverlaps = commonRefinementCrossOverlapsInternal

opLeftSlot :: OverlapPlan obj mor -> CoverSlotKey
opLeftSlot = overlapLeftSlotInternal

opRightSlot :: OverlapPlan obj mor -> CoverSlotKey
opRightSlot = overlapRightSlotInternal

opLeftArrow :: OverlapPlan obj mor -> CheckedMorphism obj mor
opLeftArrow = overlapLeftArrowInternal

opRightArrow :: OverlapPlan obj mor -> CheckedMorphism obj mor
opRightArrow = overlapRightArrowInternal

opPullbackSquare :: OverlapPlan obj mor -> PullbackSquare obj mor
opPullbackSquare = overlapSquareInternal

cpCoverId :: CoverPlan obj mor -> CoverId
cpCoverId = coverPlanIdInternal

cpEffectiveCover :: CoverPlan obj mor -> EffectiveCoverPlan obj mor
cpEffectiveCover = coverPlanEffectiveCoverInternal

cpTargetKey :: CoverPlan obj mor -> ObjectKey
cpTargetKey = coverPlanTargetKeyInternal

cpSourceKeys :: CoverPlan obj mor -> [ObjectKey]
cpSourceKeys = coverPlanSourceKeysInternal

spCoversById :: SitePlans obj mor -> IntMap (CoverPlan obj mor)
spCoversById = siteCoversByIdInternal

spCoverIdsByTarget :: SitePlans obj mor -> IntMap IntSet
spCoverIdsByTarget = siteCoverIdsByTargetInternal

type SitePlanBuildError :: Type -> Type -> Type
data SitePlanBuildError obj mor
  = SitePlanUnknownCoverTarget !obj
  | SitePlanUnknownCoverSource !CoverId !obj
  | SitePlanEffectiveCoverFailed
      !CoverId
      !(EffectiveCoverPlanFailure obj mor)
  deriving stock (Eq, Show)

effectiveCoverFamily :: EffectiveCoverPlan obj mor -> CoveringFamily obj mor
effectiveCoverFamily =
  ecpCover
{-# INLINE effectiveCoverFamily #-}

effectiveCoverSlots :: EffectiveCoverPlan obj mor -> IntMap (CoverSlot obj mor)
effectiveCoverSlots =
  ecpSlots
{-# INLINE effectiveCoverSlots #-}

effectiveCoverOverlapPlans :: EffectiveCoverPlan obj mor -> [OverlapPlan obj mor]
effectiveCoverOverlapPlans =
  ecpOverlapPlans
{-# INLINE effectiveCoverOverlapPlans #-}

effectiveCoverSlotKeys :: EffectiveCoverPlan obj mor -> [CoverSlotKey]
effectiveCoverSlotKeys =
  fmap coverSlotKey . IntMap.elems . ecpSlots
{-# INLINE effectiveCoverSlotKeys #-}

effectiveCoverSlotArrows :: EffectiveCoverPlan obj mor -> [CheckedMorphism obj mor]
effectiveCoverSlotArrows =
  fmap coverSlotArrow . IntMap.elems . ecpSlots
{-# INLINE effectiveCoverSlotArrows #-}

effectiveCoverSlotSources :: EffectiveCoverPlan obj mor -> [obj]
effectiveCoverSlotSources =
  fmap (cmSource . coverSlotArrow) . IntMap.elems . ecpSlots
{-# INLINE effectiveCoverSlotSources #-}

effectiveCoverSlotCount :: EffectiveCoverPlan obj mor -> Int
effectiveCoverSlotCount =
  IntMap.size . ecpSlots
{-# INLINE effectiveCoverSlotCount #-}

prepareEffectiveCoverPlan ::
  Site site =>
  site ->
  CoveringFamily (SiteObject site) (SiteMorphism site) ->
  Either
    (EffectiveCoverPlanFailure (SiteObject site) (SiteMorphism site))
    (EffectiveCoverPlan (SiteObject site) (SiteMorphism site))
prepareEffectiveCoverPlan site coverValue = do
  let slots = coverSlots coverValue
  overlapPlans <-
    traverse
      overlapPlanFor
      (slotPairs slots)
  pure
    EffectiveCoverPlan
      { ecpCover = coverValue,
        ecpSlots = IntMap.fromList [(unCoverSlotKey (coverSlotKey slot), slot) | slot <- slots],
        ecpOverlapPlans = overlapPlans
      }
  where
    overlapPlanFor (leftSlot, rightSlot) =
      case pullbackPair site (coverSlotArrow leftSlot) (coverSlotArrow rightSlot) of
        Nothing ->
          Left
            ( EffectiveCoverPlanMissingPullback
                (coverSlotArrow leftSlot)
                (coverSlotArrow rightSlot)
            )
        Just square ->
          Right
            OverlapPlan
              { overlapLeftSlotInternal = coverSlotKey leftSlot,
                overlapRightSlotInternal = coverSlotKey rightSlot,
                overlapLeftArrowInternal = coverSlotArrow leftSlot,
                overlapRightArrowInternal = coverSlotArrow rightSlot,
                overlapSquareInternal = square
              }

identityEffectiveCoverPlan ::
  Site site =>
  site ->
  SiteObject site ->
  EffectiveCoverPlan (SiteObject site) (SiteMorphism site)
identityEffectiveCoverPlan site objectValue =
  let coverValue = identityCover site objectValue
      slot =
        case coverArrows coverValue of
          arrow :| _ ->
            CoverSlot
              { coverSlotKeyInternal = CoverSlotKey 0,
                coverSlotArrowInternal = arrow
              }
   in EffectiveCoverPlan
        { ecpCover = coverValue,
          ecpSlots = IntMap.singleton 0 slot,
          ecpOverlapPlans = []
        }

preparePullbackCoverPlan ::
  Site site =>
  site ->
  CheckedMorphism (SiteObject site) (SiteMorphism site) ->
  EffectiveCoverPlan (SiteObject site) (SiteMorphism site) ->
  Either
    (EffectiveCoverPlanFailure (SiteObject site) (SiteMorphism site))
    (PullbackCoverPlan (SiteObject site) (SiteMorphism site))
preparePullbackCoverPlan site alongMorphism originalPlan
  | coverTarget (ecpCover originalPlan) /= cmTarget alongMorphism =
      Left
        ( EffectiveCoverPlanTargetMismatch
            (coverTarget (ecpCover originalPlan))
            (cmTarget alongMorphism)
        )
  | otherwise = do
      originalSlots <-
        maybe
          (Left EffectiveCoverPlanInternalEmptyCover)
          Right
          (NonEmpty.nonEmpty (IntMap.elems (ecpSlots originalPlan)))
      pulledSlotPlans <-
        traverse preparePulledSlot originalSlots
      pulledCover <-
        first EffectiveCoverPlanPulledCoverMalformed $
          mkCoveringFamily
            (cmSource alongMorphism)
            (fmap pcspPulledArrow pulledSlotPlans)
      pulledEffectivePlan <-
        prepareEffectiveCoverPlan site pulledCover
      pure
        PullbackCoverPlan
          { pullbackCoverAlongInternal = alongMorphism,
            pullbackCoverOriginalInternal = originalPlan,
            pullbackCoverPulledInternal = pulledEffectivePlan,
            pullbackCoverSlotsInternal =
              IntMap.fromList
                [ (unCoverSlotKey (pcspOriginalSlot slotPlan), slotPlan)
                | slotPlan <- NonEmpty.toList pulledSlotPlans
                ]
          }
  where
    preparePulledSlot originalSlot =
      case pullbackPair site (coverSlotArrow originalSlot) alongMorphism of
        Nothing ->
          Left
            ( EffectiveCoverPlanMissingPullback
                (coverSlotArrow originalSlot)
                alongMorphism
            )
        Just square
          | cmSource (psToLeft square) /= psApex square ->
              Left (EffectiveCoverPlanMalformedPullbackLeg square)
          | cmSource (psToRight square) /= psApex square ->
              Left (EffectiveCoverPlanMalformedPullbackLeg square)
          | cmTarget (psToLeft square) /= cmSource (coverSlotArrow originalSlot) ->
              Left (EffectiveCoverPlanMalformedPullbackLeg square)
          | cmTarget (psToRight square) /= cmSource alongMorphism ->
              Left (EffectiveCoverPlanMalformedPullbackLeg square)
          | otherwise ->
              Right
                PullbackCoverSlotPlan
                  { pullbackCoverSlotOriginalSlotInternal = coverSlotKey originalSlot,
                    pullbackCoverSlotPulledSlotInternal = coverSlotKey originalSlot,
                    pullbackCoverSlotOriginalArrowInternal = coverSlotArrow originalSlot,
                    pullbackCoverSlotPulledArrowInternal = psToRight square,
                    pullbackCoverSlotRestrictToOriginalInternal = psToLeft square,
                    pullbackCoverSlotSquareInternal = square
                  }

canonicalizePulledCoverPlan ::
  (Eq obj, Eq mor) =>
  EffectiveCoverPlan obj mor ->
  PullbackCoverPlan obj mor ->
  Either (EffectiveCoverPlanFailure obj mor) (PullbackCoverPlan obj mor)
canonicalizePulledCoverPlan canonicalPulledCover pullbackPlan
  | effectiveCoverFamily canonicalPulledCover == effectiveCoverFamily (pcpPulledCover pullbackPlan) =
      Right
        pullbackPlan
          { pullbackCoverPulledInternal = canonicalPulledCover
          }
  | otherwise =
      Left
        ( EffectiveCoverPlanCanonicalPulledCoverMismatch
            (effectiveCoverFamily (pcpPulledCover pullbackPlan))
            (effectiveCoverFamily canonicalPulledCover)
        )

pullbackEffectiveCoverPlanAlong ::
  Site site =>
  site ->
  CheckedMorphism (SiteObject site) (SiteMorphism site) ->
  EffectiveCoverPlan (SiteObject site) (SiteMorphism site) ->
  Either
    (EffectiveCoverPlanFailure (SiteObject site) (SiteMorphism site))
    (EffectiveCoverPlan (SiteObject site) (SiteMorphism site))
pullbackEffectiveCoverPlanAlong site alongMorphism =
  fmap pcpPulledCover . preparePullbackCoverPlan site alongMorphism
{-# INLINE pullbackEffectiveCoverPlanAlong #-}

prepareCommonRefinementPlan ::
  Site site =>
  site ->
  EffectiveCoverPlan (SiteObject site) (SiteMorphism site) ->
  EffectiveCoverPlan (SiteObject site) (SiteMorphism site) ->
  Either
    (EffectiveCoverPlanFailure (SiteObject site) (SiteMorphism site))
    (CommonRefinementPlan (SiteObject site) (SiteMorphism site))
prepareCommonRefinementPlan site leftPlan rightPlan
  | coverTarget (ecpCover leftPlan) /= coverTarget (ecpCover rightPlan) =
      Left
        ( EffectiveCoverPlanTargetMismatch
            (coverTarget (ecpCover leftPlan))
            (coverTarget (ecpCover rightPlan))
        )
  | otherwise = do
      crossOverlaps <-
        traverse
          crossOverlapFor
          [ (leftSlot, rightSlot)
          | leftSlot <- IntMap.elems (ecpSlots leftPlan),
            rightSlot <- IntMap.elems (ecpSlots rightPlan)
          ]
      pure
        CommonRefinementPlan
          { commonRefinementTargetInternal = coverTarget (ecpCover leftPlan),
            commonRefinementLeftCoverInternal = leftPlan,
            commonRefinementRightCoverInternal = rightPlan,
            commonRefinementCrossOverlapsInternal = crossOverlaps
          }
  where
    crossOverlapFor (leftSlot, rightSlot) =
      case pullbackPair site (coverSlotArrow leftSlot) (coverSlotArrow rightSlot) of
        Nothing ->
          Left
            ( EffectiveCoverPlanMissingPullback
                (coverSlotArrow leftSlot)
                (coverSlotArrow rightSlot)
            )
        Just square ->
          Right
            CrossCoverOverlapPlan
              { crossCoverOverlapLeftSlotInternal = coverSlotKey leftSlot,
                crossCoverOverlapRightSlotInternal = coverSlotKey rightSlot,
                crossCoverOverlapLeftArrowInternal = coverSlotArrow leftSlot,
                crossCoverOverlapRightArrowInternal = coverSlotArrow rightSlot,
                crossCoverOverlapSquareInternal = square
              }

emptySitePlans :: SitePlans obj mor
emptySitePlans =
  SitePlans
    { siteCoversByIdInternal = IntMap.empty,
      siteCoverIdsByTargetInternal = IntMap.empty
    }

prepareSitePlans ::
  Site site =>
  ObjectIndex (SiteObject site) ->
  site ->
  Either
    (SitePlanBuildError (SiteObject site) (SiteMorphism site))
    (SitePlans (SiteObject site) (SiteMorphism site))
prepareSitePlans objects site =
  indexCoverPlans
    <$> traverse
      prepareCoverPlan
      (zip (CoverId <$> [0 :: Int ..]) (concatMap (coversAt site) (siteObjects site)))
  where
    note errorValue =
      maybe (Left errorValue) Right

    objectKeyOr errorFor objectValue =
      note (errorFor objectValue) (denseIndexKeyOf objectValue objects)

    prepareCoverPlan (coverId, coverValue) = do
      let target = coverTarget coverValue
      targetKey <-
        objectKeyOr SitePlanUnknownCoverTarget target
      sourceKeys <-
        traverse
          (objectKeyOr (SitePlanUnknownCoverSource coverId))
          (coverSources coverValue)
      effectiveCover <-
        first (SitePlanEffectiveCoverFailed coverId) $
          prepareEffectiveCoverPlan site coverValue
      pure
        CoverPlan
          { coverPlanIdInternal = coverId,
            coverPlanEffectiveCoverInternal = effectiveCover,
            coverPlanTargetKeyInternal = targetKey,
            coverPlanSourceKeysInternal = sourceKeys
          }

indexCoverPlans :: [CoverPlan obj mor] -> SitePlans obj mor
indexCoverPlans coverPlans =
  SitePlans
    { siteCoversByIdInternal =
        IntMap.fromList
          [ (unCoverId (cpCoverId coverPlan), coverPlan)
          | coverPlan <- coverPlans
          ],
      siteCoverIdsByTargetInternal =
        IntMap.fromListWith
          IntSet.union
          [ ( unObjectKey (cpTargetKey coverPlan),
              IntSet.singleton (unCoverId (cpCoverId coverPlan))
            )
          | coverPlan <- coverPlans
          ]
    }

siteCoverPlansAt :: ObjectKey -> SitePlans obj mor -> [CoverPlan obj mor]
siteCoverPlansAt (ObjectKey targetKey) plans =
  foldr lookupCoverPlan [] (IntSet.toAscList coverIds)
  where
    coverIds =
      IntMap.findWithDefault IntSet.empty targetKey (spCoverIdsByTarget plans)

    lookupCoverPlan coverId acc =
      case IntMap.lookup coverId (spCoversById plans) of
        Nothing ->
          acc
        Just coverPlan ->
          coverPlan : acc

siteCoverPlanById :: CoverId -> SitePlans obj mor -> Maybe (CoverPlan obj mor)
siteCoverPlanById (CoverId coverId) =
  IntMap.lookup coverId . spCoversById
{-# INLINE siteCoverPlanById #-}

coverSlots :: CoveringFamily obj mor -> [CoverSlot obj mor]
coverSlots coverValue =
  [ CoverSlot
      { coverSlotKeyInternal = CoverSlotKey slotIndex,
        coverSlotArrowInternal = arrow
      }
  | (slotIndex, arrow) <- zip [0 :: Int ..] (toList (coverArrows coverValue))
  ]

slotPairs :: [CoverSlot obj mor] -> [(CoverSlot obj mor, CoverSlot obj mor)]
slotPairs slots =
  [ (leftSlot, rightSlot)
  | leftSlot : rightSlots <- tails slots,
    rightSlot <- rightSlots
  ]

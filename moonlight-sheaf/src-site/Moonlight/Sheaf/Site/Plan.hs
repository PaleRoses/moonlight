{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Sheaf.Site.Plan
  ( CoverSlotKey (..),
    CoverSlot (..),
    EffectiveCoverPlan,
    EffectiveCoverPlanFailure (..),
    PullbackCoverSlotPlan (..),
    PullbackCoverPlan (..),
    CrossCoverOverlapPlan (..),
    CommonRefinementPlan (..),
    CoverId (..),
    OverlapPlan (..),
    CoverPlan (..),
    SitePlans (..),
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
  { coverSlotKey :: !CoverSlotKey,
    coverSlotArrow :: !(CheckedMorphism obj mor)
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
  deriving stock (Eq, Show)

type PullbackCoverSlotPlan :: Type -> Type -> Type
data PullbackCoverSlotPlan obj mor = PullbackCoverSlotPlan
  { pcspOriginalSlot :: !CoverSlotKey,
    pcspPulledSlot :: !CoverSlotKey,
    pcspOriginalArrow :: !(CheckedMorphism obj mor),
    pcspPulledArrow :: !(CheckedMorphism obj mor),
    pcspRestrictToOriginal :: !(CheckedMorphism obj mor),
    pcspPullbackSquare :: !(PullbackSquare obj mor)
  }
  deriving stock (Eq, Show)

type PullbackCoverPlan :: Type -> Type -> Type
data PullbackCoverPlan obj mor = PullbackCoverPlan
  { pcpAlong :: !(CheckedMorphism obj mor),
    pcpOriginalCover :: !(EffectiveCoverPlan obj mor),
    pcpPulledCover :: !(EffectiveCoverPlan obj mor),
    pcpSlotPlans :: !(IntMap (PullbackCoverSlotPlan obj mor))
  }
  deriving stock (Eq, Show)

type CrossCoverOverlapPlan :: Type -> Type -> Type
data CrossCoverOverlapPlan obj mor = CrossCoverOverlapPlan
  { ccopLeftSlot :: !CoverSlotKey,
    ccopRightSlot :: !CoverSlotKey,
    ccopLeftArrow :: !(CheckedMorphism obj mor),
    ccopRightArrow :: !(CheckedMorphism obj mor),
    ccopPullbackSquare :: !(PullbackSquare obj mor)
  }
  deriving stock (Eq, Show)

type CommonRefinementPlan :: Type -> Type -> Type
data CommonRefinementPlan obj mor = CommonRefinementPlan
  { crpTarget :: !obj,
    crpLeftCover :: !(EffectiveCoverPlan obj mor),
    crpRightCover :: !(EffectiveCoverPlan obj mor),
    crpCrossOverlaps :: ![CrossCoverOverlapPlan obj mor]
  }
  deriving stock (Eq, Show)

type CoverId :: Type
newtype CoverId = CoverId
  { unCoverId :: Int
  }
  deriving stock (Eq, Ord, Show, Read)

type OverlapPlan :: Type -> Type -> Type
data OverlapPlan obj mor = OverlapPlan
  { opLeftSlot :: !CoverSlotKey,
    opRightSlot :: !CoverSlotKey,
    opLeftArrow :: !(CheckedMorphism obj mor),
    opRightArrow :: !(CheckedMorphism obj mor),
    opPullbackSquare :: !(PullbackSquare obj mor)
  }
  deriving stock (Eq, Show)

type CoverPlan :: Type -> Type -> Type
data CoverPlan obj mor = CoverPlan
  { cpCoverId :: !CoverId,
    cpEffectiveCover :: !(EffectiveCoverPlan obj mor),
    cpTargetKey :: !ObjectKey,
    cpSourceKeys :: ![ObjectKey]
  }
  deriving stock (Eq, Show)

type SitePlans :: Type -> Type -> Type
data SitePlans obj mor = SitePlans
  { spCoversById :: !(IntMap (CoverPlan obj mor)),
    spCoverIdsByTarget :: !(IntMap IntSet)
  }
  deriving stock (Eq, Show)

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
              { opLeftSlot = coverSlotKey leftSlot,
                opRightSlot = coverSlotKey rightSlot,
                opLeftArrow = coverSlotArrow leftSlot,
                opRightArrow = coverSlotArrow rightSlot,
                opPullbackSquare = square
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
              { coverSlotKey = CoverSlotKey 0,
                coverSlotArrow = arrow
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
          { pcpAlong = alongMorphism,
            pcpOriginalCover = originalPlan,
            pcpPulledCover = pulledEffectivePlan,
            pcpSlotPlans =
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
                  { pcspOriginalSlot = coverSlotKey originalSlot,
                    pcspPulledSlot = coverSlotKey originalSlot,
                    pcspOriginalArrow = coverSlotArrow originalSlot,
                    pcspPulledArrow = psToRight square,
                    pcspRestrictToOriginal = psToLeft square,
                    pcspPullbackSquare = square
                  }

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
          { crpTarget = coverTarget (ecpCover leftPlan),
            crpLeftCover = leftPlan,
            crpRightCover = rightPlan,
            crpCrossOverlaps = crossOverlaps
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
              { ccopLeftSlot = coverSlotKey leftSlot,
                ccopRightSlot = coverSlotKey rightSlot,
                ccopLeftArrow = coverSlotArrow leftSlot,
                ccopRightArrow = coverSlotArrow rightSlot,
                ccopPullbackSquare = square
              }

emptySitePlans :: SitePlans obj mor
emptySitePlans =
  SitePlans
    { spCoversById = IntMap.empty,
      spCoverIdsByTarget = IntMap.empty
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
          { cpCoverId = coverId,
            cpEffectiveCover = effectiveCover,
            cpTargetKey = targetKey,
            cpSourceKeys = sourceKeys
          }

indexCoverPlans :: [CoverPlan obj mor] -> SitePlans obj mor
indexCoverPlans coverPlans =
  SitePlans
    { spCoversById =
        IntMap.fromList
          [ (unCoverId (cpCoverId coverPlan), coverPlan)
          | coverPlan <- coverPlans
          ],
      spCoverIdsByTarget =
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
      { coverSlotKey = CoverSlotKey slotIndex,
        coverSlotArrow = arrow
      }
  | (slotIndex, arrow) <- zip [0 :: Int ..] (toList (coverArrows coverValue))
  ]

slotPairs :: [CoverSlot obj mor] -> [(CoverSlot obj mor, CoverSlot obj mor)]
slotPairs slots =
  [ (leftSlot, rightSlot)
  | leftSlot : rightSlots <- tails slots,
    rightSlot <- rightSlots
  ]

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Sheaf.Site.CoverBasis.Finite
  ( FiniteCoverBasis,
    FiniteCoverBasisFailure (..),
    mkFiniteCoverBasis,
    finiteCoverBasisSite,
    finiteAllCoverPlans,
    finiteCoversAt,
    finiteCoverPlanForCover,
    finiteCanonicalCoverPlan,
    finiteIdentityCoverAt,
    finitePullbackCoverPlan,
    finiteCommonRefinementPlan,
  )
where

import Data.Bifunctor (first)
import Data.Kind (Type)
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism (..),
    CoveringFamily,
    Site (..),
    SiteConstructionError,
    coverTarget,
    identityCover,
    pullbackCoverAlong,
    siteMorphismUniverse,
  )
import Moonlight.Sheaf.Site.Plan
  ( CommonRefinementPlan,
    EffectiveCoverPlan,
    EffectiveCoverPlanFailure (..),
    PullbackCoverPlan,
    canonicalizePulledCoverPlan,
    pcpPulledCover,
    effectiveCoverFamily,
    identityEffectiveCoverPlan,
    prepareCommonRefinementPlan,
    prepareEffectiveCoverPlan,
    preparePullbackCoverPlan,
  )

type FiniteCoverBasis :: Type -> Type
data FiniteCoverBasis site = FiniteCoverBasis
  { finiteCoverBasisSiteInternal :: !site,
    finiteCoversByTargetInternal ::
      !(Map (SiteObject site) [EffectiveCoverPlan (SiteObject site) (SiteMorphism site)]),
    finiteIdentityCoversByTargetInternal ::
      !(Map (SiteObject site) (EffectiveCoverPlan (SiteObject site) (SiteMorphism site))),
    finitePullbackPlansByTargetInternal ::
      !( Map
           (SiteObject site)
           ( Map
               ( CheckedMorphism (SiteObject site) (SiteMorphism site),
                 CoveringFamily (SiteObject site) (SiteMorphism site)
               )
               (PullbackCoverPlan (SiteObject site) (SiteMorphism site))
           )
       ),
    finiteCommonRefinementsByTargetInternal ::
      !( Map
           (SiteObject site)
           ( Map
               ( CoveringFamily (SiteObject site) (SiteMorphism site),
                 CoveringFamily (SiteObject site) (SiteMorphism site)
               )
               (CommonRefinementPlan (SiteObject site) (SiteMorphism site))
           )
       )
  }

type FiniteCoverBasisFailure :: Type -> Type -> Type
data FiniteCoverBasisFailure obj mor
  = FiniteCoverBasisEffectivePlanFailed
      !(CoveringFamily obj mor)
      !(EffectiveCoverPlanFailure obj mor)
  | FiniteCoverBasisPullbackFailed
      !(CheckedMorphism obj mor)
      !(CoveringFamily obj mor)
      !(SiteConstructionError obj mor)
  | FiniteCoverBasisPullbackPlanFailed
      !(CheckedMorphism obj mor)
      !(EffectiveCoverPlan obj mor)
      !(EffectiveCoverPlanFailure obj mor)
  | FiniteCoverBasisCoverUnavailable
      !(CoveringFamily obj mor)
  | FiniteCoverBasisIdentityMissing !obj
  | FiniteCoverBasisPullbackPlanMissing
      !(CheckedMorphism obj mor)
      !(EffectiveCoverPlan obj mor)
  | FiniteCoverBasisCommonRefinementFailed
      !(EffectiveCoverPlan obj mor)
      !(EffectiveCoverPlan obj mor)
      !(EffectiveCoverPlanFailure obj mor)
  deriving stock (Eq, Show)

mkFiniteCoverBasis ::
  (Site site, Ord (SiteMorphism site)) =>
  site ->
  Either
    (FiniteCoverBasisFailure (SiteObject site) (SiteMorphism site))
    (FiniteCoverBasis site)
mkFiniteCoverBasis site = do
  let initialCovers =
        concat
          [ identityCover site objectValue : coversAt site objectValue
          | objectValue <- siteObjects site
          ]
  closedCovers <-
    closeCoversUnderPullback site Set.empty initialCovers
  effectivePlans <-
    traverse
      ( \coverValue ->
          first
            (FiniteCoverBasisEffectivePlanFailed coverValue)
            (prepareEffectiveCoverPlan site coverValue)
      )
      closedCovers
  let coversByTarget =
        Map.fromListWith
          (<>)
          [ (coverTarget coverValue, [planValue])
          | (coverValue, planValue) <- zip closedCovers effectivePlans
          ]
  commonRefinementsByTarget <-
    Map.fromList
      <$> traverse
        (commonRefinementEntry site)
        (Map.toList coversByTarget)
  pullbacksByTarget <-
    Map.fromList
      <$> traverse
        (pullbackEntry site coversByTarget)
        (Map.toList coversByTarget)
  let identities =
        Map.fromList
          [ (objectValue, identityEffectiveCoverPlan site objectValue)
          | objectValue <- siteObjects site
          ]
  pure
    FiniteCoverBasis
      { finiteCoverBasisSiteInternal = site,
        finiteCoversByTargetInternal = coversByTarget,
        finiteIdentityCoversByTargetInternal = identities,
        finitePullbackPlansByTargetInternal = pullbacksByTarget,
        finiteCommonRefinementsByTargetInternal = commonRefinementsByTarget
      }

finiteCoverBasisSite :: FiniteCoverBasis site -> site
finiteCoverBasisSite = finiteCoverBasisSiteInternal

finiteAllCoverPlans ::
  Site site =>
  FiniteCoverBasis site ->
  [EffectiveCoverPlan (SiteObject site) (SiteMorphism site)]
finiteAllCoverPlans = concat . Map.elems . finiteCoversByTargetInternal

finiteCoversAt ::
  Site site =>
  FiniteCoverBasis site ->
  SiteObject site ->
  [EffectiveCoverPlan (SiteObject site) (SiteMorphism site)]
finiteCoversAt basis objectValue =
  Map.findWithDefault [] objectValue (finiteCoversByTargetInternal basis)

finiteCoverPlanForCover ::
  (Site site, Eq (SiteMorphism site)) =>
  FiniteCoverBasis site ->
  CoveringFamily (SiteObject site) (SiteMorphism site) ->
  Either
    (FiniteCoverBasisFailure (SiteObject site) (SiteMorphism site))
    (EffectiveCoverPlan (SiteObject site) (SiteMorphism site))
finiteCoverPlanForCover basis coverValue =
  maybe
    (Left (FiniteCoverBasisCoverUnavailable coverValue))
    Right
    (find ((== coverValue) . effectiveCoverFamily) (finiteCoversAt basis (coverTarget coverValue)))

finiteCanonicalCoverPlan ::
  (Site site, Eq (SiteMorphism site)) =>
  FiniteCoverBasis site ->
  EffectiveCoverPlan (SiteObject site) (SiteMorphism site) ->
  Either
    (FiniteCoverBasisFailure (SiteObject site) (SiteMorphism site))
    (EffectiveCoverPlan (SiteObject site) (SiteMorphism site))
finiteCanonicalCoverPlan basis =
  finiteCoverPlanForCover basis . effectiveCoverFamily

finiteIdentityCoverAt ::
  Site site =>
  FiniteCoverBasis site ->
  SiteObject site ->
  Either
    (FiniteCoverBasisFailure (SiteObject site) (SiteMorphism site))
    (EffectiveCoverPlan (SiteObject site) (SiteMorphism site))
finiteIdentityCoverAt basis objectValue =
  maybe
    (Left (FiniteCoverBasisIdentityMissing objectValue))
    Right
    (Map.lookup objectValue (finiteIdentityCoversByTargetInternal basis))

finitePullbackCoverPlan ::
  (Site site, Ord (SiteMorphism site)) =>
  FiniteCoverBasis site ->
  CheckedMorphism (SiteObject site) (SiteMorphism site) ->
  EffectiveCoverPlan (SiteObject site) (SiteMorphism site) ->
  Either
    (FiniteCoverBasisFailure (SiteObject site) (SiteMorphism site))
    (PullbackCoverPlan (SiteObject site) (SiteMorphism site))
finitePullbackCoverPlan basis morphismValue coverPlan =
  maybe
    (Left (FiniteCoverBasisPullbackPlanMissing morphismValue coverPlan))
    Right
    ( do
        targetPullbacks <- Map.lookup (cmTarget morphismValue) (finitePullbackPlansByTargetInternal basis)
        Map.lookup (morphismValue, effectiveCoverFamily coverPlan) targetPullbacks
    )

finiteCommonRefinementPlan ::
  (Site site, Ord (SiteMorphism site)) =>
  FiniteCoverBasis site ->
  EffectiveCoverPlan (SiteObject site) (SiteMorphism site) ->
  EffectiveCoverPlan (SiteObject site) (SiteMorphism site) ->
  Either
    (EffectiveCoverPlanFailure (SiteObject site) (SiteMorphism site))
    (CommonRefinementPlan (SiteObject site) (SiteMorphism site))
finiteCommonRefinementPlan basis leftCover rightCover =
  maybe
    (Left (EffectiveCoverPlanCommonRefinementMissing (effectiveCoverFamily leftCover) (effectiveCoverFamily rightCover)))
    Right
    ( do
        targetRefinements <- Map.lookup (coverTarget (effectiveCoverFamily leftCover)) (finiteCommonRefinementsByTargetInternal basis)
        Map.lookup (effectiveCoverFamily leftCover, effectiveCoverFamily rightCover) targetRefinements
    )

closeCoversUnderPullback ::
  (Site site, Ord (SiteMorphism site)) =>
  site ->
  Set (CoveringFamily (SiteObject site) (SiteMorphism site)) ->
  [CoveringFamily (SiteObject site) (SiteMorphism site)] ->
  Either
    (FiniteCoverBasisFailure (SiteObject site) (SiteMorphism site))
    [CoveringFamily (SiteObject site) (SiteMorphism site)]
closeCoversUnderPullback site =
  close []
  where
    close accepted seen queue =
      case queue of
        [] ->
          Right (reverse accepted)
        coverValue : remainingQueue
          | Set.member coverValue seen ->
              close accepted seen remainingQueue
          | otherwise -> do
              pulledBackCovers <-
                traverse
                  (pullbackCoverFor coverValue)
                  [ morphismValue
                  | morphismValue <- siteMorphisms site,
                    cmTarget morphismValue == coverTarget coverValue
                  ]
              close
                (coverValue : accepted)
                (Set.insert coverValue seen)
                (remainingQueue <> pulledBackCovers)

    pullbackCoverFor coverValue morphismValue =
      first
        (FiniteCoverBasisPullbackFailed morphismValue coverValue)
        (pullbackCoverAlong site morphismValue coverValue)

commonRefinementEntry ::
  (Site site, Ord (SiteMorphism site)) =>
  site ->
  (SiteObject site, [EffectiveCoverPlan (SiteObject site) (SiteMorphism site)]) ->
  Either
    (FiniteCoverBasisFailure (SiteObject site) (SiteMorphism site))
    ( SiteObject site,
      Map
        ( CoveringFamily (SiteObject site) (SiteMorphism site),
          CoveringFamily (SiteObject site) (SiteMorphism site)
        )
        (CommonRefinementPlan (SiteObject site) (SiteMorphism site))
    )
commonRefinementEntry site (targetObject, coverPlans) =
  fmap
    (\refinements -> (targetObject, Map.fromList refinements))
    ( traverse
        refinementForPair
        [ (leftPlan, rightPlan)
        | leftPlan <- coverPlans,
          rightPlan <- coverPlans
        ]
    )
  where
    refinementForPair (leftPlan, rightPlan) =
      fmap
        (\refinement -> ((effectiveCoverFamily leftPlan, effectiveCoverFamily rightPlan), refinement))
        ( first
            (FiniteCoverBasisCommonRefinementFailed leftPlan rightPlan)
            (prepareCommonRefinementPlan site leftPlan rightPlan)
        )

pullbackEntry ::
  (Site site, Ord (SiteMorphism site)) =>
  site ->
  Map (SiteObject site) [EffectiveCoverPlan (SiteObject site) (SiteMorphism site)] ->
  (SiteObject site, [EffectiveCoverPlan (SiteObject site) (SiteMorphism site)]) ->
  Either
    (FiniteCoverBasisFailure (SiteObject site) (SiteMorphism site))
    ( SiteObject site,
      Map
        ( CheckedMorphism (SiteObject site) (SiteMorphism site),
          CoveringFamily (SiteObject site) (SiteMorphism site)
        )
        (PullbackCoverPlan (SiteObject site) (SiteMorphism site))
    )
pullbackEntry site coversByTarget (targetObject, coverPlans) =
  fmap
    (\pullbacks -> (targetObject, Map.fromList pullbacks))
    ( traverse
        pullbackForPair
        [ (morphismValue, coverPlan)
        | morphismValue <- siteMorphismUniverse site,
          cmTarget morphismValue == targetObject,
          coverPlan <- coverPlans
        ]
    )
  where
    pullbackForPair (morphismValue, coverPlan) = do
      pullbackPlan <-
        first
          (FiniteCoverBasisPullbackPlanFailed morphismValue coverPlan)
          (preparePullbackCoverPlan site morphismValue coverPlan)
      canonicalPulledCover <-
        maybe
          (Left (FiniteCoverBasisCoverUnavailable (effectiveCoverFamily (pcpPulledCover pullbackPlan))))
          Right
          (canonicalCoverPlanFrom coversByTarget (effectiveCoverFamily (pcpPulledCover pullbackPlan)))
      canonicalPullbackPlan <-
        first
          (FiniteCoverBasisPullbackPlanFailed morphismValue coverPlan)
          (canonicalizePulledCoverPlan canonicalPulledCover pullbackPlan)
      pure
        ( (morphismValue, effectiveCoverFamily coverPlan),
          canonicalPullbackPlan
        )

canonicalCoverPlanFrom ::
  (Ord obj, Eq mor) =>
  Map obj [EffectiveCoverPlan obj mor] ->
  CoveringFamily obj mor ->
  Maybe (EffectiveCoverPlan obj mor)
canonicalCoverPlanFrom coversByTarget coverValue =
  find
    ((== coverValue) . effectiveCoverFamily)
    (Map.findWithDefault [] (coverTarget coverValue) coversByTarget)

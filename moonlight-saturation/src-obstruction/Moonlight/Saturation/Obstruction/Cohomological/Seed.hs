{-# LANGUAGE RankNTypes #-}

module Moonlight.Saturation.Obstruction.Cohomological.Seed
  ( CandidateRegionSeedKey (..),
    candidateRegionSeedKey,
    candidateRegionSeedFromKey,
    SeedFrontierPlan,
    seedFrontierPlanFromList,
    seedFrontierPlanSeeds,
    seedFrontierPlanCount,
    SeedInterpreter (..),
  )
where

import Data.IntSet (IntSet)
import Data.Kind (Type)
import Data.Set (Set)
import Numeric.Natural (Natural)
import Moonlight.Core (RegionNodeId)
import Moonlight.Sheaf.Obstruction
  ( CandidateRegion,
    CandidateRegionSeed (..),
    CandidateRegionSeedKey (..),
    CarrierPlan,
    candidateRegionSeedFromKey,
    candidateRegionSeedKey,
    carrierPlanCount,
    carrierPlanItems,
    representedCarrierPlanFromList,
  )
import Moonlight.Sheaf.Footprint
  ( FootprintMeasureBasis (..),
    FootprintMeasureUnit (..),
  )

type SeedFrontierPlan :: Type -> Type
type SeedFrontierPlan root =
  CarrierPlan (CandidateRegionSeed root)

seedFrontierPlanFromList ::
  [CandidateRegionSeed root] ->
  SeedFrontierPlan root
seedFrontierPlanFromList =
  representedCarrierPlanFromList CandidateSeedUnit RepresentedCandidateCarrier

seedFrontierPlanSeeds ::
  SeedFrontierPlan root ->
  [CandidateRegionSeed root]
seedFrontierPlanSeeds =
  carrierPlanItems

seedFrontierPlanCount ::
  SeedFrontierPlan root ->
  Natural
seedFrontierPlanCount =
  carrierPlanCount

type SeedInterpreter :: (Type -> Type) -> Type -> Type -> Type -> Type
data SeedInterpreter request seedPattern frontier root = SeedInterpreter
  { siSeedPlan ::
      forall runtime.
      request runtime ->
      seedPattern ->
      SeedFrontierPlan root,
    siFrontierSeedPlan ::
      forall runtime.
      frontier ->
      request runtime ->
      seedPattern ->
      SeedFrontierPlan root,
    siRefineSeedPlan ::
      forall runtime.
      request runtime ->
      seedPattern ->
      CandidateRegion root ->
      SeedFrontierPlan root,
    siMaterializeSeed ::
      forall runtime.
      request runtime ->
      seedPattern ->
      CandidateRegionSeed root ->
      Maybe (CandidateRegion root),
    siSeedKey ::
      CandidateRegionSeed root ->
      CandidateRegionSeedKey root,
    siSeedsForRootsPlan ::
      forall runtime.
      IntSet ->
      request runtime ->
      seedPattern ->
      SeedFrontierPlan root,
    siSeedsForNodesPlan ::
      forall runtime.
      Set RegionNodeId ->
      request runtime ->
      seedPattern ->
      SeedFrontierPlan root,
    siSeedsForKeysPlan ::
      forall runtime.
      Set (CandidateRegionSeedKey root) ->
      request runtime ->
      seedPattern ->
      SeedFrontierPlan root
  }

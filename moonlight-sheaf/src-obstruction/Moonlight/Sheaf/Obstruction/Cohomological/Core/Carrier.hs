{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Sheaf.Obstruction.Cohomological.Core.Carrier
  ( CarrierPlan (..),
    RegionCarrierPlan,
    carrierPlanFromList,
    representedCarrierPlanFromList,
    regionCarrierPlanFromList,
    carrierPlanItems,
    carrierPlanCount,
  )
where

import Data.Kind (Type)
import Data.Monoid (Sum (..))
import Numeric.Natural (Natural)
import Moonlight.Sheaf.Footprint
  ( FootprintMeasure,
    FootprintMeasureBasis (..),
    FootprintMeasureUnit (..),
    exactRepresentedFootprintMeasure,
  )

type CarrierPlan :: Type -> Type
data CarrierPlan item = CarrierPlan
  { cpHasAny :: !Bool,
    cpMeasures :: ![FootprintMeasure Natural],
    cpFoldMap :: forall summary. Monoid summary => (item -> summary) -> summary
  }

type RegionCarrierPlan :: Type -> Type
type RegionCarrierPlan region =
  CarrierPlan region

carrierPlanFromList ::
  [FootprintMeasure Natural] ->
  [item] ->
  CarrierPlan item
carrierPlanFromList measures items =
  CarrierPlan
    { cpHasAny = not (null items),
      cpMeasures = measures,
      cpFoldMap = \summarize -> foldMap summarize items
    }

representedCarrierPlanFromList ::
  FootprintMeasureUnit ->
  FootprintMeasureBasis ->
  [item] ->
  CarrierPlan item
representedCarrierPlanFromList unitValue basisValue items =
  carrierPlanFromList
    [ exactRepresentedFootprintMeasure
        unitValue
        basisValue
        (fromIntegral (length items))
    ]
    items

regionCarrierPlanFromList ::
  [region] ->
  RegionCarrierPlan region
regionCarrierPlanFromList =
  representedCarrierPlanFromList CandidateRegionUnit RepresentedCandidateCarrier

carrierPlanItems ::
  CarrierPlan item ->
  [item]
carrierPlanItems plan =
  cpFoldMap plan (: [])

carrierPlanCount ::
  CarrierPlan item ->
  Natural
carrierPlanCount plan =
  getSum (cpFoldMap plan (const (Sum 1)))

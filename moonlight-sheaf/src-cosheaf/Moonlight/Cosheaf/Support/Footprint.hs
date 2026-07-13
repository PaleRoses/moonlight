module Moonlight.Cosheaf.Support.Footprint
  ( supportFootprintMeasure,
  )
where

import Moonlight.Sheaf.Footprint
  ( FootprintMeasure (..),
    FootprintMeasureBasis (..),
    FootprintMeasureExactness (..),
    FootprintMeasureUnit,
  )
import Numeric.Natural (Natural)

supportFootprintMeasure :: FootprintMeasureUnit -> Natural -> Natural -> FootprintMeasure Natural
supportFootprintMeasure unitValue totalValue retainedValue =
  FootprintMeasure
    { fmUnit = unitValue,
      fmExactness = FootprintExactCertified,
      fmTotal = Just totalValue,
      fmRetained = Just retainedValue,
      fmPruned = Just (totalValue `minusNaturalFloor` retainedValue),
      fmBasis = RestrictedLocalSiteCarrier
    }

minusNaturalFloor :: Natural -> Natural -> Natural
minusNaturalFloor leftValue rightValue =
  if leftValue >= rightValue
    then leftValue - rightValue
    else 0

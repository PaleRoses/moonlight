{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Sheaf.Footprint
  ( FootprintMeasureExactness (..),
    FootprintMeasureUnit (..),
    FootprintMeasureBasis (..),
    FootprintMeasure (..),
    exactRepresentedFootprintMeasure,
  )
where

import Data.Kind (Type)
import Numeric.Natural (Natural)

type FootprintMeasureExactness :: Type
data FootprintMeasureExactness
  = FootprintExactRepresented
  | FootprintExactCertified
  | FootprintUpperBound
  deriving stock (Eq, Ord, Show, Read)

type FootprintMeasureUnit :: Type
data FootprintMeasureUnit
  = CandidateSeedUnit
  | CandidateRegionUnit
  | RegionNodeUnit
  | CoboundaryRestrictionUnit
  | SparseEntryUnit
  | ContextOrdinalUnit
  | SupportCellUnit
  deriving stock (Eq, Ord, Show, Read)

type FootprintMeasureBasis :: Type
data FootprintMeasureBasis
  = RepresentedCandidateCarrier
  | NormalizedIntSetCarrier
  | NormalizedSetCarrier
  | RestrictedLocalSiteCarrier
  deriving stock (Eq, Ord, Show, Read)

type FootprintMeasure :: Type -> Type
data FootprintMeasure n = FootprintMeasure
  { fmUnit :: !FootprintMeasureUnit,
    fmExactness :: !FootprintMeasureExactness,
    fmTotal :: !(Maybe n),
    fmRetained :: !(Maybe n),
    fmPruned :: !(Maybe n),
    fmBasis :: !FootprintMeasureBasis
  }
  deriving stock (Eq, Ord, Show, Read)

exactRepresentedFootprintMeasure ::
  FootprintMeasureUnit ->
  FootprintMeasureBasis ->
  Natural ->
  FootprintMeasure Natural
exactRepresentedFootprintMeasure unitValue basisValue retainedValue =
  FootprintMeasure
    { fmUnit = unitValue,
      fmExactness = FootprintExactRepresented,
      fmTotal = Just retainedValue,
      fmRetained = Just retainedValue,
      fmPruned = Nothing,
      fmBasis = basisValue
    }

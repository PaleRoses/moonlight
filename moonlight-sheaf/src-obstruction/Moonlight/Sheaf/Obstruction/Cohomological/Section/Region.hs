module Moonlight.Sheaf.Obstruction.Cohomological.Section.Region
  ( RegionExactness (..),
    RegionAnalysisOutcome (..),
    RegionExactCoverage,
    recExactness,
    recCoverage,
    mkRegionExactCoverage,
    RegionTraversalSummary (..),
    skippedRegionCoverage,
    regionCoverageFromSectionCoverage,
  )
where

import Data.Kind (Type)
import Numeric.Natural (Natural)

import Moonlight.Sheaf.Footprint
  ( FootprintMeasure,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Section
  ( SectionCoverage,
    SectionFeasibilityFailure,
    sectionCoverageFeasibility,
  )

type RegionExactness :: Type -> Type
data RegionExactness gap
  = ExactCoverageSkipped
  | ExactCoverageFeasible
  | ExactCoverageInfeasible !(SectionFeasibilityFailure gap)
  deriving stock (Eq, Show, Read)

type RegionAnalysisOutcome :: Type -> Type -> Type -> Type
data RegionAnalysisOutcome gap witness pruning
  = RegionAnalysisPruned !pruning
  | RegionAnalysisObstructed !witness
  | RegionAnalysisTruncated !witness
  | RegionAnalysisExact !(RegionExactness gap)
  deriving stock (Eq, Show, Read)

type RegionExactCoverage :: Type -> Type -> Type
data RegionExactCoverage match gap = RegionExactCoverage
  { recExactness :: !(RegionExactness gap),
    recCoverage :: !(SectionCoverage match gap)
  }
  deriving stock (Eq, Show, Read)

type RegionTraversalSummary :: Type -> Type -> Type -> Type -> Type -> Type
data RegionTraversalSummary region match gap witness pruning = RegionTraversalSummary
  { rtsRegion :: !region,
    rtsOutcome :: !(RegionAnalysisOutcome gap witness pruning),
    rtsCoverage :: !(RegionExactCoverage match gap),
    rtsMeasures :: ![FootprintMeasure Natural]
  }
  deriving stock (Eq, Show, Read)

skippedRegionCoverage :: RegionExactCoverage match gap
skippedRegionCoverage =
  RegionExactCoverage ExactCoverageSkipped mempty

mkRegionExactCoverage ::
  RegionExactness gap ->
  SectionCoverage match gap ->
  RegionExactCoverage match gap
mkRegionExactCoverage exactness coverage =
  RegionExactCoverage
    { recExactness = exactness,
      recCoverage = coverage
    }

regionCoverageFromSectionCoverage ::
  SectionCoverage match gap ->
  RegionExactCoverage match gap
regionCoverageFromSectionCoverage coverage =
  mkRegionExactCoverage
    ( either
        ExactCoverageInfeasible
        (const ExactCoverageFeasible)
        (sectionCoverageFeasibility coverage)
    )
    coverage

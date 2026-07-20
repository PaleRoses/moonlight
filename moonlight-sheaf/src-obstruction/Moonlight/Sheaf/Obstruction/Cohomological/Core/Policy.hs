module Moonlight.Sheaf.Obstruction.Cohomological.Core.Policy
  ( LaplacianGapThreshold,
    mkLaplacianGapThreshold,
    laplacianGapThresholdValue,
    LaplacianPruning (..),
    profileLaplacianPruning,
    ExactCoverageBudget (..),
    defaultExactCoverageBudget,
    profileExactCoverageBudget,
    CohomologicalPolicy (..),
    CohomologicalProfile (..),
    profilePolicy,
  )
where

import Data.Kind (Type)
import Moonlight.Homology
  ( HomologicalDegree (..),
  )
import Numeric.Natural (Natural)

type LaplacianGapThreshold :: Type
newtype LaplacianGapThreshold = LaplacianGapThreshold
  { laplacianGapThresholdValueInternal :: Double
  }
  deriving stock (Eq, Ord, Show, Read)

mkLaplacianGapThreshold :: Double -> Maybe LaplacianGapThreshold
mkLaplacianGapThreshold thresholdValue
  | thresholdValue > 0.0 = Just (LaplacianGapThreshold thresholdValue)
  | otherwise = Nothing

laplacianGapThresholdValue :: LaplacianGapThreshold -> Double
laplacianGapThresholdValue = laplacianGapThresholdValueInternal

type LaplacianPruning :: Type
data LaplacianPruning = LaplacianPruning
  { lpDegree :: !HomologicalDegree,
    lpGapThreshold :: !LaplacianGapThreshold
  }
  deriving stock (Eq, Ord, Show, Read)

type ExactCoverageBudget :: Type
newtype ExactCoverageBudget = ExactCoverageBudget
  { ecbMaxAssignments :: Natural
  }
  deriving stock (Eq, Ord, Show, Read)

defaultExactCoverageBudget :: ExactCoverageBudget
defaultExactCoverageBudget =
  ExactCoverageBudget 65536

type CohomologicalPolicy :: Type
data CohomologicalPolicy = CohomologicalPolicy
  { cpUseHierarchicalPruning :: !Bool,
    cpMaxCoarseDepth :: !Int,
    cpShortCircuitRankGap :: !Bool,
    cpRequireFactSensitiveCache :: !Bool,
    cpPreferExactWitnessOnFailure :: !Bool,
    cpMinCycleLength :: !Int,
    cpLaplacianPruning :: !(Maybe LaplacianPruning),
    cpExactCoverageBudget :: !(Maybe ExactCoverageBudget)
  }
  deriving stock (Eq, Show, Read)

type CohomologicalProfile :: Type
data CohomologicalProfile
  = ConservativeProfile
  | AggressivePruningProfile
  | BalancedPruningProfile
  | ExactWitnessProfile
  deriving stock (Eq, Ord, Show, Read)

laplacianPruningAt :: HomologicalDegree -> Double -> LaplacianPruning
laplacianPruningAt degreeValue thresholdValue =
  LaplacianPruning
    { lpDegree = degreeValue
    , lpGapThreshold = LaplacianGapThreshold thresholdValue
    }

profileLaplacianThreshold :: CohomologicalProfile -> Maybe Double
profileLaplacianThreshold profile =
  case profile of
    ConservativeProfile -> Just 0.05
    AggressivePruningProfile -> Just 0.12
    BalancedPruningProfile -> Just 0.08
    ExactWitnessProfile -> Nothing

profileLaplacianPruning :: CohomologicalProfile -> Maybe LaplacianPruning
profileLaplacianPruning profile =
  fmap
    (laplacianPruningAt (HomologicalDegree 1))
    (profileLaplacianThreshold profile)

profileExactCoverageBudget :: CohomologicalProfile -> Maybe ExactCoverageBudget
profileExactCoverageBudget ExactWitnessProfile =
  Nothing
profileExactCoverageBudget _ =
  Just defaultExactCoverageBudget

profilePolicy :: CohomologicalProfile -> CohomologicalPolicy
profilePolicy profile =
  case profile of
    ConservativeProfile ->
      policy True 2 True False
    AggressivePruningProfile ->
      policy True 1 True True
    BalancedPruningProfile ->
      policy True 2 True True
    ExactWitnessProfile ->
      policy False 0 False True
  where
    policy useHierarchicalPruning maxCoarseDepth shortCircuitRankGap preferExactWitnessOnFailure =
      CohomologicalPolicy
        { cpUseHierarchicalPruning = useHierarchicalPruning
        , cpMaxCoarseDepth = maxCoarseDepth
        , cpShortCircuitRankGap = shortCircuitRankGap
        , cpRequireFactSensitiveCache = True
        , cpPreferExactWitnessOnFailure = preferExactWitnessOnFailure
        , cpMinCycleLength = 2
        , cpLaplacianPruning = profileLaplacianPruning profile
        , cpExactCoverageBudget = profileExactCoverageBudget profile
        }

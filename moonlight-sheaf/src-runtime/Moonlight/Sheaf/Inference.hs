-- | Weighted-factor inference over sheaf domains: factor specs compiled into
-- inference programs.
module Moonlight.Sheaf.Inference
  ( DomainIndex
  , FactorRow
  , WeightedFactor
  , FactorSpec(..)
  , FactorCompileError(..)
  , LogWeightError(..)
  , BlueprintError(..)
  , InferenceExecutionError(..)
  , WeightedBlueprint
  , EliminationHeuristic(..)
  , InferenceConfig(..)
  , MapSolution(..)
  , SectionPosterior(..)
  , TopKCount
  , TopKCountError(..)
  , defaultInferenceConfig
  , buildWeightedBlueprint
  , selectEliminationOrder
  , inferLogZExact
  , inferMapExact
  , inferMarginalsExact
  , inferPosteriorExact
  , mkTopKCount
  , topKDomains
  ) where

import Moonlight.Sheaf.Inference.Types
import Moonlight.Sheaf.Inference.Bootstrap
import Moonlight.Sheaf.Inference.Query
import Moonlight.Sheaf.Inference.Algebra ()

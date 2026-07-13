module Moonlight.Saturation.Obstruction.Cohomological.Prepared
  ( PreparedRequestCacheKey (..),
    mkPreparedRequestCacheKey,
    InitialRegionStage (..),
    PreparedInitialRegionBatch (..),
    emptyPreparedInitialRegionBatch,
  )
where

import Data.Kind (Type)
import Moonlight.Saturation.Obstruction.Cohomological.Metrics.Pipeline
  ( PipelineMetrics,
    emptyPipelineMetrics,
  )
import Moonlight.Sheaf.Obstruction
  ( CandidateRegion,
  )

type PreparedRequestCacheKey :: Type -> Type
data PreparedRequestCacheKey purpose = PreparedRequestCacheKey
  { prckQueryFingerprint :: !Int,
    prckPurpose :: !purpose,
    prckEnvironmentFingerprint :: !(Maybe Int)
  }
  deriving stock (Eq, Ord, Show, Read)

mkPreparedRequestCacheKey ::
  Int ->
  purpose ->
  Maybe Int ->
  PreparedRequestCacheKey purpose
mkPreparedRequestCacheKey =
  PreparedRequestCacheKey

type InitialRegionStage :: Type
data InitialRegionStage
  = InitialRegionSeeds
  | InitialRegionMaterializedRegions
  | InitialRegionAfterPruningGates
  | InitialRegionAfterFrontierFilter
  | InitialRegionAfterMaterialization
  | InitialRegionAfterMicrosupport
  | InitialRegionAfterContext
  | InitialRegionAfterSpectral
  | InitialRegionAfterLaplacian
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

type PreparedInitialRegionBatch :: Type -> Type
data PreparedInitialRegionBatch root = PreparedInitialRegionBatch
  { pirbMetrics :: !(PipelineMetrics InitialRegionStage),
    pirbRegions :: ![CandidateRegion root]
  }
  deriving stock (Eq, Show)

emptyPreparedInitialRegionBatch :: PreparedInitialRegionBatch root
emptyPreparedInitialRegionBatch =
  PreparedInitialRegionBatch
    { pirbMetrics = emptyPipelineMetrics,
      pirbRegions = []
    }

{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Runtime.Factor.Reuse
  ( FactorReuseKind (..),
    FactorReuseMiss (..),
    FactorReuseAction (..),
    FactorRepairAction (..),
    FactorRepairActionSummary (..),
    FactorRepairReport (..),
    factorRepairActionSummary,
  )
where

import Moonlight.Core
  ( QueryId,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
  )
import Moonlight.Flow.Carrier.Core.Delta
  ( RelationalCarrierDelta,
  )
import Moonlight.Flow.Runtime.Carrier.Reuse.CoverMaterialization
  ( CoverMaterializationPlan,
  )
import Moonlight.Flow.Runtime.Factor.Internal.Exact
  ( ExactFactorRepairResult,
  )
import Moonlight.Flow.Runtime.Factor.Internal.Reuse.Result
  ( FactorReuseMaterialization,
  )
import Moonlight.Flow.Runtime.Factor.Request
  ( FactorRepairCause,
  )
import Moonlight.Flow.Runtime.Core.RepairStats
  ( RuntimeRepairStats,
  )

data FactorReuseKind
  = FactorReuseExactEquivalent
  | FactorReuseExactByCover
  | FactorReuseLowerBound
  deriving stock (Eq, Ord, Show, Read)

data FactorReuseMiss
  = FactorReuseMissNoCandidate
  | FactorReuseMissPolicyExactOnly
  | FactorReuseMissPolicyExactOrCover
  deriving stock (Eq, Ord, Show, Read)

data FactorReuseAction ctx prop boundary evidence = FactorReuseAction
  { fruaKind :: !FactorReuseKind,
    fruaMaterializations :: ![FactorReuseMaterialization ctx prop boundary evidence],
    fruaCoverPlans :: ![CoverMaterializationPlan ctx prop evidence],
    fruaSnapshots :: ![RelationalCarrierDelta ctx Carrier prop boundary evidence],
    fruaDeltas :: ![RelationalCarrierDelta ctx Carrier prop boundary evidence]
  }
  deriving stock (Eq, Show)

data FactorRepairAction ctx prop boundary evidence joinState joinErr
  = FactorActionReuse !(FactorReuseAction ctx prop boundary evidence)
  | FactorActionExact !(ExactFactorRepairResult ctx prop boundary evidence joinState joinErr)

data FactorRepairActionSummary
  = FactorRepairUsedExactEquivalent
  | FactorRepairUsedExactByCover
  | FactorRepairUsedLowerBound
  | FactorRepairRanExact
  deriving stock (Eq, Ord, Show, Read)

data FactorRepairReport ctx prop boundary evidence = FactorRepairReport
  { frrpQueryId :: !QueryId,
    frrpCause :: !FactorRepairCause,
    frrpAction :: !FactorRepairActionSummary,
    frrpEmittedDeltaCount :: {-# UNPACK #-} !Int,
    frrpRegisteredNodeCount :: {-# UNPACK #-} !Int,
    frrpStats :: !RuntimeRepairStats
  }
  deriving stock (Eq, Show)

factorRepairActionSummary ::
  FactorRepairAction ctx prop boundary evidence joinState joinErr ->
  FactorRepairActionSummary
factorRepairActionSummary action =
  case action of
    FactorActionReuse reuseAction ->
      case fruaKind reuseAction of
        FactorReuseExactEquivalent ->
          FactorRepairUsedExactEquivalent
        FactorReuseExactByCover ->
          FactorRepairUsedExactByCover
        FactorReuseLowerBound ->
          FactorRepairUsedLowerBound
    FactorActionExact _ ->
      FactorRepairRanExact
{-# INLINE factorRepairActionSummary #-}

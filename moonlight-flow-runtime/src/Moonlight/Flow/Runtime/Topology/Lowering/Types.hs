{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Runtime.Topology.Lowering.Types
  ( RuntimeRepairRoute (..),
    RuntimeRepairRouting (..),
  )
where

import Moonlight.Core
  ( QueryId,
  )
import Moonlight.Flow.Runtime.Spec.FactorProgram
  ( RepairProgramKey,
  )

data RuntimeRepairRoute = RuntimeRepairRoute
  { rrtRepairKey :: !RepairProgramKey,
    rrtRepresentativeQueryId :: !QueryId
  }
  deriving stock (Eq, Ord, Show)

data RuntimeRepairRouting = RuntimeRepairRouting
  { rrRepairRouteOfQuery :: QueryId -> Maybe RuntimeRepairRoute,
    rrRepairIsCold :: RepairProgramKey -> Bool
  }

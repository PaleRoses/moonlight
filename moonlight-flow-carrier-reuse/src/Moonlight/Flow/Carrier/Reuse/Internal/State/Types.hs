{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Carrier.Reuse.Internal.State.Types
  ( PlanReuseState (..),
    emptyPlanReuseState,
    planReuseStats,
    mapPlanReuseStats,
  )
where

import Moonlight.Flow.Carrier.Reuse.Internal.Index.Materialization
  ( ReuseMaterializationIndex,
    emptyReuseMaterializationIndex,
  )
import Moonlight.Flow.Carrier.Reuse.Internal.Index.Reuse
  ( CarrierReuseRegistry,
    emptyCarrierReuseRegistry,
  )
import Moonlight.Flow.Carrier.Reuse.Internal.Index.Shape
  ( SubsumptionIndex,
    emptySubsumptionIndex,
  )
import Moonlight.Flow.Carrier.Reuse.Internal.Stats
  ( PlanReuseStats,
    emptyPlanReuseStats,
  )
import Moonlight.Flow.Plan.Rewrite
  ( PlanSaturationState,
    emptyPlanSaturationState,
  )

data PlanReuseState ctx prop = PlanReuseState
  { prsPlanSaturationState :: !PlanSaturationState,
    prsSubsumptionIndex :: !(SubsumptionIndex ctx prop),
    prsReuseRegistry :: !(CarrierReuseRegistry ctx prop),
    prsMaterializations :: !(ReuseMaterializationIndex ctx prop),
    prsStats :: !PlanReuseStats
  }
  deriving stock (Eq, Show)

emptyPlanReuseState ::
  (Ord ctx, Ord prop) =>
  PlanReuseState ctx prop
emptyPlanReuseState =
  PlanReuseState
    { prsPlanSaturationState = emptyPlanSaturationState,
      prsSubsumptionIndex = emptySubsumptionIndex,
      prsReuseRegistry = emptyCarrierReuseRegistry,
      prsMaterializations = emptyReuseMaterializationIndex,
      prsStats = emptyPlanReuseStats
    }

planReuseStats :: PlanReuseState ctx prop -> PlanReuseStats
planReuseStats =
  prsStats

mapPlanReuseStats ::
  (PlanReuseStats -> PlanReuseStats) ->
  PlanReuseState ctx prop ->
  PlanReuseState ctx prop
mapPlanReuseStats updateStats state =
  state {prsStats = updateStats (prsStats state)}

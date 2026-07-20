module Moonlight.Flow.Runtime.Inspect
  ( runtimeDiagnostics,
  )
where

import Moonlight.Flow.Carrier.Reuse
  ( planReuseDiagnostics,
    planReuseStats,
  )
import Moonlight.Flow.Runtime.Carrier.Reuse.State
  ( runtimePlanReuseState,
  )
import Moonlight.Flow.Runtime.Factor.State
  ( runtimeRepairStats,
  )
import Moonlight.Flow.Runtime.Types
  ( Runtime (..),
    RuntimeDiagnostics (..),
    runtimeReuseDiagnosticsFromPlanReuseDiagnostics,
    runtimeReuseStatsFromPlanReuseStats,
  )

runtimeDiagnostics :: Runtime ctx prop -> RuntimeDiagnostics
runtimeDiagnostics (Runtime kernel) =
  let reuse =
        runtimePlanReuseState kernel
   in RuntimeDiagnostics
        { rdReuseStats =
            runtimeReuseStatsFromPlanReuseStats
              (planReuseStats reuse),
          rdReuseDiagnostics =
            runtimeReuseDiagnosticsFromPlanReuseDiagnostics
              (planReuseDiagnostics reuse),
          rdRepairStats =
            runtimeRepairStats kernel
        }
{-# INLINE runtimeDiagnostics #-}

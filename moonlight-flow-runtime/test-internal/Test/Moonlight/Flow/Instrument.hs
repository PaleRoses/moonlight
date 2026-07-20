{-# LANGUAGE DerivingStrategies #-}

module Test.Moonlight.Flow.Instrument
  ( CacheBoundConfig (..),
    InstrumentSample (..),
    cacheTouchBoundHolds,
    clearRuntimeEphemeralCaches,
    clearRuntimeFactorCacheState,
    clearRuntimeVisibleCache,
    RtsTelemetryError (..),
    requireRtsTelemetry,
  )
where

import GHC.Stats
  ( RTSStats,
    getRTSStats,
    getRTSStatsEnabled,
  )
import Moonlight.Flow.Carrier.View.Cache
  ( VisibleSectionCache (..),
    emptyVisibleSectionCache,
  )
import Moonlight.Flow.Runtime.Carrier.State
  ( runtimeVisibleCache,
    setRuntimeVisibleCache,
  )
import Moonlight.Flow.Runtime.Core.State qualified as Core
import Moonlight.Flow.Runtime.Factor.Internal.Cache
  ( clearFactorCacheState,
  )
import Moonlight.Flow.Runtime.Factor.Program
  ( factorProgramCacheState,
    factorProgramWithCacheState,
  )
import Moonlight.Flow.Runtime.Factor.State
  ( RuntimeFactorState (..),
  )
import Moonlight.Flow.Runtime.Kernel
  ( RelDiffRuntime,
    RuntimeEnvelope (..),
  )

data CacheBoundConfig = CacheBoundConfig
  { cbcMaxTouchFactor :: !Int,
    cbcSeparationRequired :: !Bool,
    cbcCountEvictions :: !Bool
  }
  deriving stock (Eq, Ord, Show, Read)

data InstrumentSample = InstrumentSample
  { isCacheTouches :: !Int,
    isDistinctProjectedBaseRows :: !Int,
    isFactDeltaRows :: !Int,
    isQueryCount :: !Int,
    isEvictions :: !Int
  }
  deriving stock (Eq, Ord, Show, Read)

data RtsTelemetryError
  = RtsTelemetryDisabled
  deriving stock (Eq, Ord, Show, Read)

cacheTouchBoundHolds :: CacheBoundConfig -> InstrumentSample -> Bool
cacheTouchBoundHolds config sample =
  measuredTouches <= cbcMaxTouchFactor config * (isDistinctProjectedBaseRows sample + isFactDeltaRows sample + isQueryCount sample)
  where
    measuredTouches =
      isCacheTouches sample + if cbcCountEvictions config then isEvictions sample else 0

clearRuntimeVisibleCache ::
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr
clearRuntimeVisibleCache runtime =
  runtime
    { rdrState =
        setRuntimeVisibleCache
          (emptyVisibleSectionCache (vscBudgetBytes (runtimeVisibleCache state0)))
          state0
    }
  where
    state0 =
      rdrState runtime
{-# INLINE clearRuntimeVisibleCache #-}

clearRuntimeFactorCacheState ::
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr
clearRuntimeFactorCacheState runtime =
  runtime
    { rdrState =
        Core.mapRuntimeFactorSection
          ( \factorState ->
              factorState
                { rfsPrograms =
                    fmap clearProgram (rfsPrograms factorState)
                }
          )
          (rdrState runtime)
    }
  where
    clearProgram program =
      factorProgramWithCacheState
        (clearFactorCacheState (factorProgramCacheState program))
        program
{-# INLINE clearRuntimeFactorCacheState #-}

clearRuntimeEphemeralCaches ::
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr
clearRuntimeEphemeralCaches =
  clearRuntimeFactorCacheState . clearRuntimeVisibleCache
{-# INLINE clearRuntimeEphemeralCaches #-}

requireRtsTelemetry :: IO (Either RtsTelemetryError RTSStats)
requireRtsTelemetry = do
  enabled <- getRTSStatsEnabled
  if enabled
    then Right <$> getRTSStats
    else pure (Left RtsTelemetryDisabled)

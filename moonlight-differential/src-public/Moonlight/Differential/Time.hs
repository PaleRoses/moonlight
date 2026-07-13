{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Differential.Time
  ( FrontierStamp,
    frontierStamp,
    frontierStampWord,
    initialFrontierStamp,
    nextFrontierStamp,
    RuntimeScope,
    runtimeScopePath,
    emptyRuntimeScope,
    enterRuntimeScope,
    leaveRuntimeScope,
    isDescendantOf,
    RuntimeEpoch,
    runtimeEpoch,
    reQuotient,
    reLive,
    RuntimeTime,
    runtimeTime,
    rtContext,
    rtScope,
    rtEpoch,
    rtPhase,
    rtFrontier,
    runtimeTimeSameContext,
    runtimeTimeSameScope,
    runtimeTimeSameScopeLeq,
    runtimeTimeSameScopeLt,
    recontextRuntimeTime,
    retimeRuntimePhase,
    rescopeRuntimeTime,
    enterRuntimeTimeScope,
    leaveRuntimeTimeScope,
    delayRuntimeTimeFeedback,
  )
where

import Data.Kind
  ( Type,
  )
import Data.List
  ( isSuffixOf,
  )
import Data.Word
  ( Word64,
  )
import Moonlight.Core
  ( PartialOrder (..),
  )

type FrontierStamp :: Type
newtype FrontierStamp = FrontierStamp
  { unFrontierStamp :: Word64
  }
  deriving stock (Eq, Ord, Show)

frontierStamp :: Word64 -> FrontierStamp
frontierStamp =
  FrontierStamp
{-# INLINE frontierStamp #-}

frontierStampWord :: FrontierStamp -> Word64
frontierStampWord =
  unFrontierStamp
{-# INLINE frontierStampWord #-}

initialFrontierStamp :: FrontierStamp
initialFrontierStamp =
  FrontierStamp 0
{-# INLINE initialFrontierStamp #-}

nextFrontierStamp :: FrontierStamp -> Maybe FrontierStamp
nextFrontierStamp (FrontierStamp stamp)
  | stamp == maxBound =
      Nothing
  | otherwise =
      Just (FrontierStamp (stamp + 1))
{-# INLINE nextFrontierStamp #-}

instance PartialOrder FrontierStamp where
  leq leftStamp rightStamp =
    frontierStampWord leftStamp <= frontierStampWord rightStamp
  {-# INLINE leq #-}

type RuntimeScope :: Type
newtype RuntimeScope = RuntimeScope
  { runtimeScopePathRaw :: [Int]
  }
  deriving stock (Eq, Ord, Show)

runtimeScopePath :: RuntimeScope -> [Int]
runtimeScopePath =
  runtimeScopePathRaw
{-# INLINE runtimeScopePath #-}

emptyRuntimeScope :: RuntimeScope
emptyRuntimeScope =
  RuntimeScope []
{-# INLINE emptyRuntimeScope #-}

enterRuntimeScope :: Int -> RuntimeScope -> RuntimeScope
enterRuntimeScope scopeKey (RuntimeScope path) =
  RuntimeScope (scopeKey : path)
{-# INLINE enterRuntimeScope #-}

leaveRuntimeScope :: RuntimeScope -> Maybe RuntimeScope
leaveRuntimeScope (RuntimeScope path) =
  case path of
    [] ->
      Nothing
    _scopeKey : parent ->
      Just (RuntimeScope parent)
{-# INLINE leaveRuntimeScope #-}

isDescendantOf :: RuntimeScope -> RuntimeScope -> Bool
isDescendantOf (RuntimeScope outerPath) (RuntimeScope innerPath) =
  outerPath `isSuffixOf` innerPath
{-# INLINE isDescendantOf #-}

type RuntimeEpoch :: Type -> Type -> Type
data RuntimeEpoch quotient live = RuntimeEpoch
  { runtimeEpochQuotientRaw :: !quotient,
    runtimeEpochLiveRaw :: !live
  }
  deriving stock (Eq, Ord, Show)

runtimeEpoch :: quotient -> live -> RuntimeEpoch quotient live
runtimeEpoch quotientValue liveValue =
  RuntimeEpoch
    { runtimeEpochQuotientRaw = quotientValue,
      runtimeEpochLiveRaw = liveValue
    }
{-# INLINE runtimeEpoch #-}

reQuotient :: RuntimeEpoch quotient live -> quotient
reQuotient =
  runtimeEpochQuotientRaw
{-# INLINE reQuotient #-}

reLive :: RuntimeEpoch quotient live -> live
reLive =
  runtimeEpochLiveRaw
{-# INLINE reLive #-}

instance (PartialOrder quotient, PartialOrder live) => PartialOrder (RuntimeEpoch quotient live) where
  leq leftEpoch rightEpoch =
    reQuotient leftEpoch `leq` reQuotient rightEpoch
      && reLive leftEpoch `leq` reLive rightEpoch
  {-# INLINE leq #-}

type RuntimeTime :: Type -> Type -> Type -> Type
data RuntimeTime ctx epoch phase = RuntimeTime
  { runtimeTimeContextRaw :: !ctx,
    runtimeTimeScopeRaw :: !RuntimeScope,
    runtimeTimeEpochRaw :: !epoch,
    runtimeTimePhaseRaw :: !phase,
    runtimeTimeFrontierRaw :: !FrontierStamp
  }
  deriving stock (Eq, Ord, Show)

runtimeTime ::
  ctx ->
  RuntimeScope ->
  epoch ->
  phase ->
  FrontierStamp ->
  RuntimeTime ctx epoch phase
runtimeTime contextValue scopeValue epochValue phaseValue frontierValue =
  RuntimeTime
    { runtimeTimeContextRaw = contextValue,
      runtimeTimeScopeRaw = scopeValue,
      runtimeTimeEpochRaw = epochValue,
      runtimeTimePhaseRaw = phaseValue,
      runtimeTimeFrontierRaw = frontierValue
    }
{-# INLINE runtimeTime #-}

rtContext :: RuntimeTime ctx epoch phase -> ctx
rtContext =
  runtimeTimeContextRaw
{-# INLINE rtContext #-}

rtScope :: RuntimeTime ctx epoch phase -> RuntimeScope
rtScope =
  runtimeTimeScopeRaw
{-# INLINE rtScope #-}

rtEpoch :: RuntimeTime ctx epoch phase -> epoch
rtEpoch =
  runtimeTimeEpochRaw
{-# INLINE rtEpoch #-}

rtPhase :: RuntimeTime ctx epoch phase -> phase
rtPhase =
  runtimeTimePhaseRaw
{-# INLINE rtPhase #-}

rtFrontier :: RuntimeTime ctx epoch phase -> FrontierStamp
rtFrontier =
  runtimeTimeFrontierRaw
{-# INLINE rtFrontier #-}

runtimeTimeSameContext ::
  Eq ctx =>
  RuntimeTime ctx epoch phase ->
  RuntimeTime ctx epoch phase ->
  Bool
runtimeTimeSameContext leftTime rightTime =
  rtContext leftTime == rtContext rightTime
{-# INLINE runtimeTimeSameContext #-}

runtimeTimeSameScope ::
  Eq ctx =>
  RuntimeTime ctx epoch phase ->
  RuntimeTime ctx epoch phase ->
  Bool
runtimeTimeSameScope leftTime rightTime =
  runtimeTimeSameContext leftTime rightTime
    && rtScope leftTime == rtScope rightTime
{-# INLINE runtimeTimeSameScope #-}

runtimeTimeSameScopeLeq ::
  (Eq ctx, PartialOrder epoch, PartialOrder phase) =>
  RuntimeTime ctx epoch phase ->
  RuntimeTime ctx epoch phase ->
  Bool
runtimeTimeSameScopeLeq leftTime rightTime =
  runtimeTimeSameScope leftTime rightTime
    && rtEpoch leftTime `leq` rtEpoch rightTime
    && rtPhase leftTime `leq` rtPhase rightTime
    && rtFrontier leftTime `leq` rtFrontier rightTime
{-# INLINE runtimeTimeSameScopeLeq #-}

runtimeTimeSameScopeLt ::
  (Eq ctx, PartialOrder epoch, PartialOrder phase) =>
  RuntimeTime ctx epoch phase ->
  RuntimeTime ctx epoch phase ->
  Bool
runtimeTimeSameScopeLt leftTime rightTime =
  runtimeTimeSameScopeLeq leftTime rightTime && leftTime /= rightTime
{-# INLINE runtimeTimeSameScopeLt #-}

instance (Eq ctx, PartialOrder epoch, PartialOrder phase) => PartialOrder (RuntimeTime ctx epoch phase) where
  leq =
    runtimeTimeSameScopeLeq
  {-# INLINE leq #-}

recontextRuntimeTime ::
  nextCtx ->
  RuntimeTime ctx epoch phase ->
  RuntimeTime nextCtx epoch phase
recontextRuntimeTime contextValue timeValue =
  RuntimeTime
    { runtimeTimeContextRaw = contextValue,
      runtimeTimeScopeRaw = rtScope timeValue,
      runtimeTimeEpochRaw = rtEpoch timeValue,
      runtimeTimePhaseRaw = rtPhase timeValue,
      runtimeTimeFrontierRaw = rtFrontier timeValue
    }
{-# INLINE recontextRuntimeTime #-}

retimeRuntimePhase ::
  phase ->
  RuntimeTime ctx epoch phase ->
  RuntimeTime ctx epoch phase
retimeRuntimePhase phaseValue timeValue =
  timeValue {runtimeTimePhaseRaw = phaseValue}
{-# INLINE retimeRuntimePhase #-}

rescopeRuntimeTime ::
  RuntimeScope ->
  RuntimeTime ctx epoch phase ->
  RuntimeTime ctx epoch phase
rescopeRuntimeTime scopeValue timeValue =
  timeValue {runtimeTimeScopeRaw = scopeValue}
{-# INLINE rescopeRuntimeTime #-}

enterRuntimeTimeScope ::
  Int ->
  RuntimeTime ctx epoch phase ->
  RuntimeTime ctx epoch phase
enterRuntimeTimeScope scopeKey timeValue =
  timeValue {runtimeTimeScopeRaw = enterRuntimeScope scopeKey (rtScope timeValue)}
{-# INLINE enterRuntimeTimeScope #-}

leaveRuntimeTimeScope ::
  RuntimeTime ctx epoch phase ->
  Maybe (RuntimeTime ctx epoch phase)
leaveRuntimeTimeScope timeValue = do
  scopeValue <- leaveRuntimeScope (rtScope timeValue)
  Just timeValue {runtimeTimeScopeRaw = scopeValue}
{-# INLINE leaveRuntimeTimeScope #-}

delayRuntimeTimeFeedback ::
  RuntimeTime ctx epoch phase ->
  Maybe (RuntimeTime ctx epoch phase)
delayRuntimeTimeFeedback timeValue = do
  delayedFrontier <- nextFrontierStamp (rtFrontier timeValue)
  Just timeValue {runtimeTimeFrontierRaw = delayedFrontier}
{-# INLINE delayRuntimeTimeFeedback #-}

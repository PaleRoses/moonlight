module Moonlight.Flow.Carrier.Core.Time
  ( RelationalRuntimeEpoch,
    RelationalCarrierTime,
    mkRelationalCarrierTime,
    mkScopedRelationalCarrierTime,
    recontextRelationalCarrierTime,
    retimeRelationalCarrierPhase,
    rescopeRelationalCarrierTime,
    enterRelationalCarrierTimeScope,
    leaveRelationalCarrierTimeScope,
    delayRelationalCarrierFeedback,
    relationalTimeScope,
    relationalTimeQuotientEpoch,
    relationalTimeLiveEpoch,
    relationalTimeFrontierStamp,
  )
where

import Moonlight.Core
  ( LiveEpoch,
    QuotientEpoch,
  )
import Moonlight.Differential.Time
  ( FrontierStamp,
    RuntimeScope,
    RuntimeEpoch,
    RuntimeTime,
    delayRuntimeTimeFeedback,
    emptyRuntimeScope,
    enterRuntimeTimeScope,
    leaveRuntimeTimeScope,
    reLive,
    reQuotient,
    recontextRuntimeTime,
    rescopeRuntimeTime,
    retimeRuntimePhase,
    rtEpoch,
    rtFrontier,
    rtScope,
    runtimeEpoch,
    runtimeTime,
  )
import Moonlight.Flow.Model.Phase
  ( RelationalPhase,
  )

type RelationalRuntimeEpoch =
  RuntimeEpoch QuotientEpoch LiveEpoch

type RelationalCarrierTime ctx =
  RuntimeTime ctx RelationalRuntimeEpoch RelationalPhase

mkRelationalCarrierTime ::
  ctx ->
  QuotientEpoch ->
  LiveEpoch ->
  RelationalPhase ->
  FrontierStamp ->
  RelationalCarrierTime ctx
mkRelationalCarrierTime contextValue quotientEpoch liveEpoch phaseValue frontierStamp =
  mkScopedRelationalCarrierTime
    contextValue
    emptyRuntimeScope
    quotientEpoch
    liveEpoch
    phaseValue
    frontierStamp
{-# INLINE mkRelationalCarrierTime #-}

mkScopedRelationalCarrierTime ::
  ctx ->
  RuntimeScope ->
  QuotientEpoch ->
  LiveEpoch ->
  RelationalPhase ->
  FrontierStamp ->
  RelationalCarrierTime ctx
mkScopedRelationalCarrierTime contextValue scopeValue quotientEpoch liveEpoch phaseValue frontierStamp =
  runtimeTime
    contextValue
    scopeValue
    (runtimeEpoch quotientEpoch liveEpoch)
    phaseValue
    frontierStamp
{-# INLINE mkScopedRelationalCarrierTime #-}

retimeRelationalCarrierPhase ::
  RelationalPhase ->
  RelationalCarrierTime ctx ->
  RelationalCarrierTime ctx
retimeRelationalCarrierPhase phaseValue timeValue =
  retimeRuntimePhase phaseValue timeValue
{-# INLINE retimeRelationalCarrierPhase #-}

recontextRelationalCarrierTime ::
  nextCtx ->
  RelationalCarrierTime ctx ->
  RelationalCarrierTime nextCtx
recontextRelationalCarrierTime =
  recontextRuntimeTime
{-# INLINE recontextRelationalCarrierTime #-}

rescopeRelationalCarrierTime ::
  RuntimeScope ->
  RelationalCarrierTime ctx ->
  RelationalCarrierTime ctx
rescopeRelationalCarrierTime =
  rescopeRuntimeTime
{-# INLINE rescopeRelationalCarrierTime #-}

enterRelationalCarrierTimeScope ::
  Int ->
  RelationalCarrierTime ctx ->
  RelationalCarrierTime ctx
enterRelationalCarrierTimeScope =
  enterRuntimeTimeScope
{-# INLINE enterRelationalCarrierTimeScope #-}

leaveRelationalCarrierTimeScope ::
  RelationalCarrierTime ctx ->
  Maybe (RelationalCarrierTime ctx)
leaveRelationalCarrierTimeScope =
  leaveRuntimeTimeScope
{-# INLINE leaveRelationalCarrierTimeScope #-}

delayRelationalCarrierFeedback ::
  RelationalCarrierTime ctx ->
  Maybe (RelationalCarrierTime ctx)
delayRelationalCarrierFeedback =
  delayRuntimeTimeFeedback
{-# INLINE delayRelationalCarrierFeedback #-}

relationalTimeScope ::
  RelationalCarrierTime ctx ->
  RuntimeScope
relationalTimeScope =
  rtScope
{-# INLINE relationalTimeScope #-}

relationalTimeQuotientEpoch ::
  RelationalCarrierTime ctx ->
  QuotientEpoch
relationalTimeQuotientEpoch =
  reQuotient . rtEpoch
{-# INLINE relationalTimeQuotientEpoch #-}

relationalTimeLiveEpoch ::
  RelationalCarrierTime ctx ->
  LiveEpoch
relationalTimeLiveEpoch =
  reLive . rtEpoch
{-# INLINE relationalTimeLiveEpoch #-}

relationalTimeFrontierStamp ::
  RelationalCarrierTime ctx ->
  FrontierStamp
relationalTimeFrontierStamp =
  rtFrontier
{-# INLINE relationalTimeFrontierStamp #-}

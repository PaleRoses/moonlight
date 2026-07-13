{-# LANGUAGE BangPatterns #-}

module Moonlight.Flow.Execution.Observe.Provenance.Value
  ( pvZero,
    pvOne,
    pvAtom,
    pvAtomWithTelemetry,
    pvPlus,
    pvPlusWithTelemetry,
    pvTimes,
    pvTimesWithTelemetry,
  )
where

import Moonlight.Flow.Execution.Observe.Provenance.Args
  ( ProvArgs,
    ProvArgsMerge (..),
    emptyProvArgs,
    provArgsIndex,
    provArgsLength,
    provArgsMergeChoice,
    provArgsSingleton,
  )
import Moonlight.Flow.Execution.Observe.Provenance.Arena
  ( internProv,
    internProvWithTelemetry,
    nodeAt,
  )
import Moonlight.Flow.Execution.Observe.Provenance.Types
  ( ProvArena,
    ProvNode (..),
    ProvVal (..),
    ProvenanceObstruction,
  )
import Moonlight.Flow.Execution.Observe.RepairTelemetry
  ( RepairTelemetry,
    RepairTelemetryConfig,
    emptyRepairTelemetry,
    recordPvAtomCall,
    recordPvPlusCall,
    recordPvTimesCall,
    repairTelemetryDetailed,
  )
import Moonlight.Differential.Row.Tuple (RowTupleKey)
import Moonlight.Flow.Plan.Query.Core (AtomId)

pvZero :: ProvVal
pvZero = PVZero
{-# INLINE pvZero #-}

pvOne :: ProvVal
pvOne = PVOne
{-# INLINE pvOne #-}

flattenSumArgs :: ProvArena -> ProvVal -> Either ProvenanceObstruction ProvArgs
flattenSumArgs _ PVZero = Right emptyProvArgs
flattenSumArgs _ PVOne = Right emptyProvArgs
flattenSumArgs _ (PVObstructed obstruction) = Left obstruction
flattenSumArgs arena (PVRef pid) =
  case nodeAt arena pid of
    Right (PNSum args) -> Right args
    Right _ -> Right (provArgsSingleton pid)
    Left obstruction -> Left obstruction
{-# INLINE flattenSumArgs #-}

flattenProdArgs :: ProvArena -> ProvVal -> Either ProvenanceObstruction ProvArgs
flattenProdArgs _ PVZero = Right emptyProvArgs
flattenProdArgs _ PVOne = Right emptyProvArgs
flattenProdArgs _ (PVObstructed obstruction) = Left obstruction
flattenProdArgs arena (PVRef pid) =
  case nodeAt arena pid of
    Right (PNProd args) -> Right args
    Right _ -> Right (provArgsSingleton pid)
    Left obstruction -> Left obstruction
{-# INLINE flattenProdArgs #-}

provArgsValue ::
  ProvVal ->
  (ProvArgs -> ProvNode) ->
  ProvArgs ->
  ProvArena ->
  (ProvArena, ProvVal)
provArgsValue emptyValue mkNode args arena =
  case provArgsLength args of
    0 ->
      (arena, emptyValue)
    1 ->
      case provArgsIndex args 0 of
        Nothing ->
          (arena, emptyValue)
        Just pid ->
          (arena, PVRef pid)
    _ ->
      let (!arena1, !pid) =
            internProv (mkNode args) arena
       in (arena1, PVRef pid)
{-# INLINE provArgsValue #-}

provArgsValueWithTelemetry ::
  ProvVal ->
  RepairTelemetryConfig ->
  (ProvArgs -> ProvNode) ->
  ProvArgs ->
  ProvArena ->
  (ProvArena, ProvVal, RepairTelemetry)
provArgsValueWithTelemetry emptyValue config mkNode args arena =
  case provArgsLength args of
    0 ->
      (arena, emptyValue, emptyRepairTelemetry)
    1 ->
      case provArgsIndex args 0 of
        Nothing ->
          (arena, emptyValue, emptyRepairTelemetry)
        Just pid ->
          (arena, PVRef pid, emptyRepairTelemetry)
    _ ->
      let (!arena1, !pid, !telemetry) =
            internProvWithTelemetry config (mkNode args) arena
       in (arena1, PVRef pid, telemetry)
{-# INLINE provArgsValueWithTelemetry #-}

sumArgsValue ::
  ProvArgs ->
  ProvArena ->
  (ProvArena, ProvVal)
sumArgsValue =
  provArgsValue PVZero PNSum
{-# INLINE sumArgsValue #-}

sumArgsValueWithTelemetry ::
  RepairTelemetryConfig ->
  ProvArgs ->
  ProvArena ->
  (ProvArena, ProvVal, RepairTelemetry)
sumArgsValueWithTelemetry config =
  provArgsValueWithTelemetry PVZero config PNSum
{-# INLINE sumArgsValueWithTelemetry #-}

prodArgsValue ::
  ProvArgs ->
  ProvArena ->
  (ProvArena, ProvVal)
prodArgsValue =
  provArgsValue PVOne PNProd
{-# INLINE prodArgsValue #-}

prodArgsValueWithTelemetry ::
  RepairTelemetryConfig ->
  ProvArgs ->
  ProvArena ->
  (ProvArena, ProvVal, RepairTelemetry)
prodArgsValueWithTelemetry config =
  provArgsValueWithTelemetry PVOne config PNProd
{-# INLINE prodArgsValueWithTelemetry #-}

provArgsMergedWith ::
  (ProvArgs -> ProvArena -> result) ->
  (ProvArena -> ProvVal -> result) ->
  ProvVal ->
  ProvArgs ->
  ProvVal ->
  ProvArgs ->
  ProvArena ->
  result
provArgsMergedWith finish done leftValue leftArgs rightValue rightArgs arena =
  case provArgsMergeChoice leftArgs rightArgs of
    ProvArgsUseLeft ->
      done arena leftValue
    ProvArgsUseRight ->
      done arena rightValue
    ProvArgsUseMerged merged ->
      finish merged arena
{-# INLINE provArgsMergedWith #-}

pvAtom :: AtomId -> RowTupleKey -> ProvArena -> (ProvArena, ProvVal)
pvAtom atomId row arena0 =
  let (arena1, pid) = internProv (PNAtom atomId row) arena0
   in (arena1, PVRef pid)
{-# INLINE pvAtom #-}

pvAtomWithTelemetry ::
  RepairTelemetryConfig ->
  AtomId ->
  RowTupleKey ->
  ProvArena ->
  (ProvArena, ProvVal, RepairTelemetry)
pvAtomWithTelemetry config atomId row arena0
  | not (repairTelemetryDetailed config) =
      let (!arena1, !value) =
            pvAtom atomId row arena0
       in (arena1, value, emptyRepairTelemetry)
  | otherwise =
      let (!arena1, !pid, !internTelemetry) =
            internProvWithTelemetry config (PNAtom atomId row) arena0
       in ( arena1,
            PVRef pid,
            recordPvAtomCall config internTelemetry
          )
{-# INLINE pvAtomWithTelemetry #-}

plusShortcut :: ProvVal -> ProvVal -> Maybe ProvVal
plusShortcut (PVObstructed obstruction) _ = Just (PVObstructed obstruction)
plusShortcut _ (PVObstructed obstruction) = Just (PVObstructed obstruction)
plusShortcut PVZero b = Just b
plusShortcut a PVZero = Just a
plusShortcut PVOne b = Just b
plusShortcut a PVOne = Just a
plusShortcut _ _ = Nothing
{-# INLINE plusShortcut #-}

timesShortcut :: ProvVal -> ProvVal -> Maybe ProvVal
timesShortcut (PVObstructed obstruction) _ = Just (PVObstructed obstruction)
timesShortcut _ (PVObstructed obstruction) = Just (PVObstructed obstruction)
timesShortcut PVZero _ = Just PVZero
timesShortcut _ PVZero = Just PVZero
timesShortcut PVOne b = Just b
timesShortcut a PVOne = Just a
timesShortcut _ _ = Nothing
{-# INLINE timesShortcut #-}

pvCombineWith ::
  (ProvVal -> ProvVal -> Maybe ProvVal) ->
  (ProvArena -> ProvVal -> Either ProvenanceObstruction ProvArgs) ->
  (ProvArgs -> ProvArena -> result) ->
  (ProvArena -> ProvVal -> result) ->
  (ProvArena -> ProvenanceObstruction -> result) ->
  ProvVal ->
  ProvVal ->
  ProvArena ->
  result
pvCombineWith shortcut flatten finish done obstruct a b arena0 =
  case shortcut a b of
    Just value ->
      done arena0 value
    Nothing ->
      case (flatten arena0 a, flatten arena0 b) of
        (Right leftArgs, Right rightArgs) ->
          provArgsMergedWith
            finish
            done
            a
            leftArgs
            b
            rightArgs
            arena0
        (Left obstruction, _) ->
          obstruct arena0 obstruction
        (_, Left obstruction) ->
          obstruct arena0 obstruction
{-# INLINE pvCombineWith #-}

plainResult :: ProvArena -> ProvVal -> (ProvArena, ProvVal)
plainResult arena value =
  (arena, value)
{-# INLINE plainResult #-}

plainObstruction ::
  ProvArena ->
  ProvenanceObstruction ->
  (ProvArena, ProvVal)
plainObstruction arena obstruction =
  (arena, PVObstructed obstruction)
{-# INLINE plainObstruction #-}

pvPlus :: ProvVal -> ProvVal -> ProvArena -> (ProvArena, ProvVal)
pvPlus =
  pvCombineWith
    plusShortcut
    flattenSumArgs
    sumArgsValue
    plainResult
    plainObstruction
{-# INLINE pvPlus #-}

pvPlusWithTelemetry ::
  RepairTelemetryConfig ->
  ProvVal ->
  ProvVal ->
  ProvArena ->
  (ProvArena, ProvVal, RepairTelemetry)
pvPlusWithTelemetry config a b arena0
  | not (repairTelemetryDetailed config) =
      let (!arena1, !value) =
            pvPlus a b arena0
       in (arena1, value, emptyRepairTelemetry)
  | otherwise =
      let !callTelemetry =
            recordPvPlusCall config emptyRepairTelemetry
          done arena value =
            (arena, value, callTelemetry)
          obstruct arena obstruction =
            (arena, PVObstructed obstruction, callTelemetry)
          finish args arena =
            let (!arena1, !value, !internTelemetry) =
                  sumArgsValueWithTelemetry config args arena
             in (arena1, value, callTelemetry <> internTelemetry)
       in pvCombineWith
            plusShortcut
            flattenSumArgs
            finish
            done
            obstruct
            a
            b
            arena0
{-# INLINE pvPlusWithTelemetry #-}

pvTimes :: ProvVal -> ProvVal -> ProvArena -> (ProvArena, ProvVal)
pvTimes =
  pvCombineWith
    timesShortcut
    flattenProdArgs
    prodArgsValue
    plainResult
    plainObstruction
{-# INLINE pvTimes #-}

pvTimesWithTelemetry ::
  RepairTelemetryConfig ->
  ProvVal ->
  ProvVal ->
  ProvArena ->
  (ProvArena, ProvVal, RepairTelemetry)
pvTimesWithTelemetry config a b arena0
  | not (repairTelemetryDetailed config) =
      let (!arena1, !value) =
            pvTimes a b arena0
       in (arena1, value, emptyRepairTelemetry)
  | otherwise =
      let !callTelemetry =
            recordPvTimesCall config emptyRepairTelemetry
          done arena value =
            (arena, value, callTelemetry)
          obstruct arena obstruction =
            (arena, PVObstructed obstruction, callTelemetry)
          finish args arena =
            let (!arena1, !value, !internTelemetry) =
                  prodArgsValueWithTelemetry config args arena
             in (arena1, value, callTelemetry <> internTelemetry)
       in pvCombineWith
            timesShortcut
            flattenProdArgs
            finish
            done
            obstruct
            a
            b
            arena0
{-# INLINE pvTimesWithTelemetry #-}

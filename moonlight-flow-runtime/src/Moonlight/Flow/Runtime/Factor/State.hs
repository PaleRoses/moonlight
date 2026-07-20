module Moonlight.Flow.Runtime.Factor.State
  ( RuntimeFactorState (..),
    RuntimeQueryBinding (..),
    emptyRuntimeFactorState,
    runtimeFactorPrograms,
    runtimeQueryBindings,
    runtimeRepairStats,
    lookupQueryBinding,
    lookupFactorProgramByKey,
    lookupFactorProgram,
    factorQueryIds,
    factorQueryIdsRuntime,
    factorQueryRepairKey,
    factorQueryRepresentativeQueryId,
    factorRepairKeyIsCold,
    factorQueryIsCold,
    factorRepairSubscribers,
    factorProgramsHeldCarrierReads,
    installFactorQueryBindings,
    installFactorProgram,
    installFactorPrograms,
    appendFactorRepairStats,
    clearFactorProgramCacheForCause,
  )
where

import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Set
  ( Set,
  )
import Moonlight.Core
  ( QueryId,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
  )
import Moonlight.Flow.Carrier.Store
  ( CarrierHeldReads,
  )
import Moonlight.Flow.Runtime.Carrier.Emit
  ( AtomCarrierEmitSpec,
  )
import Moonlight.Flow.Runtime.Core.Env
  ( RuntimeEnvelope (..),
  )
import Moonlight.Flow.Runtime.Core.RepairStats
  ( RuntimeRepairStats,
    appendRuntimeRepairStats,
  )
import Moonlight.Flow.Runtime.Core.State qualified as Core
import Moonlight.Flow.Runtime.Factor.Input
  ( heldCarrierReadsForFactorProgramsRuntime,
  )
import Moonlight.Flow.Runtime.Factor.Internal.Cache
  ( clearFactorCacheState,
  )
import Moonlight.Flow.Runtime.Factor.Program
  ( FactorProgram (..),
    factorProgramCacheCold,
    factorProgramQueryId,
  )
import Moonlight.Flow.Runtime.Spec.FactorProgram
  ( RepairProgramKey,
  )
import Moonlight.Flow.Runtime.Factor.Request
  ( FactorRepairCause,
    repairCauseDropsCache,
  )
import Moonlight.Flow.Runtime.Factor.State.Types
  ( RuntimeFactorState (..),
    RuntimeQueryBinding (..),
    emptyRuntimeFactorState,
  )

type FactorRuntime topology engine carrier env =
  RuntimeEnvelope
    (Core.RuntimeState topology engine carrier RuntimeFactorState)
    env

runtimeFactorPrograms ::
  FactorRuntime topology engine carrier env ->
  Map RepairProgramKey FactorProgram
runtimeFactorPrograms =
  rfsPrograms . Core.rsFactor . rdrState
{-# INLINE runtimeFactorPrograms #-}

runtimeQueryBindings ::
  FactorRuntime topology engine carrier env ->
  Map QueryId RuntimeQueryBinding
runtimeQueryBindings =
  rfsQueryBindings . Core.rsFactor . rdrState
{-# INLINE runtimeQueryBindings #-}

runtimeRepairStats ::
  FactorRuntime topology engine carrier env ->
  RuntimeRepairStats
runtimeRepairStats =
  rfsRepairStats . Core.rsFactor . rdrState
{-# INLINE runtimeRepairStats #-}

overRuntimeFactorState ::
  (RuntimeFactorState -> RuntimeFactorState) ->
  FactorRuntime topology engine carrier env ->
  FactorRuntime topology engine carrier env
overRuntimeFactorState update runtime =
  runtime
    { rdrState =
        Core.mapRuntimeFactorSection update (rdrState runtime)
    }
{-# INLINE overRuntimeFactorState #-}

lookupQueryBinding ::
  QueryId ->
  FactorRuntime topology engine carrier env ->
  Maybe RuntimeQueryBinding
lookupQueryBinding queryId runtime =
  Map.lookup queryId (runtimeQueryBindings runtime)
{-# INLINE lookupQueryBinding #-}

lookupFactorProgramByKey ::
  RepairProgramKey ->
  FactorRuntime topology engine carrier env ->
  Maybe FactorProgram
lookupFactorProgramByKey repairKey runtime =
  Map.lookup repairKey (runtimeFactorPrograms runtime)
{-# INLINE lookupFactorProgramByKey #-}

factorQueryRepairKey ::
  FactorRuntime topology engine carrier env ->
  QueryId ->
  Maybe RepairProgramKey
factorQueryRepairKey runtime queryId =
  rqbRepairKey <$> lookupQueryBinding queryId runtime
{-# INLINE factorQueryRepairKey #-}

factorQueryRepresentativeQueryId ::
  FactorRuntime topology engine carrier env ->
  QueryId ->
  Maybe QueryId
factorQueryRepresentativeQueryId runtime queryId =
  factorProgramQueryId <$> lookupFactorProgram queryId runtime
{-# INLINE factorQueryRepresentativeQueryId #-}

lookupFactorProgram ::
  QueryId ->
  FactorRuntime topology engine carrier env ->
  Maybe FactorProgram
lookupFactorProgram queryId runtime = do
  repairKey <- factorQueryRepairKey runtime queryId
  lookupFactorProgramByKey repairKey runtime
{-# INLINE lookupFactorProgram #-}

factorQueryIds ::
  Core.RuntimeState topology engine carrier RuntimeFactorState ->
  Set QueryId
factorQueryIds =
  Map.keysSet . rfsQueryBindings . Core.rsFactor
{-# INLINE factorQueryIds #-}

factorQueryIdsRuntime ::
  FactorRuntime topology engine carrier env ->
  Set QueryId
factorQueryIdsRuntime =
  factorQueryIds . rdrState
{-# INLINE factorQueryIdsRuntime #-}

factorRepairKeyIsCold ::
  FactorRuntime topology engine carrier env ->
  RepairProgramKey ->
  Bool
factorRepairKeyIsCold runtime repairKey =
  maybe True factorProgramCacheCold (lookupFactorProgramByKey repairKey runtime)
{-# INLINE factorRepairKeyIsCold #-}

factorQueryIsCold ::
  FactorRuntime topology engine carrier env ->
  QueryId ->
  Bool
factorQueryIsCold runtime queryId =
  maybe True (factorRepairKeyIsCold runtime) (factorQueryRepairKey runtime queryId)
{-# INLINE factorQueryIsCold #-}

factorRepairSubscribers ::
  RepairProgramKey ->
  FactorRuntime topology engine carrier env ->
  [QueryId]
factorRepairSubscribers repairKey runtime =
  [ queryId
  | (queryId, binding) <- Map.toAscList (runtimeQueryBindings runtime),
    rqbRepairKey binding == repairKey
  ]
{-# INLINE factorRepairSubscribers #-}

factorProgramsHeldCarrierReads ::
  (Ord ctx, Ord prop) =>
  AtomCarrierEmitSpec ctx prop boundary evidence ->
  FactorRuntime topology engine carrier env ->
  CarrierHeldReads ctx Carrier prop
factorProgramsHeldCarrierReads atomSpec runtime =
  heldCarrierReadsForFactorProgramsRuntime
    atomSpec
    (runtimeFactorPrograms runtime)
    (runtimeQueryBindings runtime)
{-# INLINE factorProgramsHeldCarrierReads #-}

installFactorQueryBindings ::
  Map QueryId RuntimeQueryBinding ->
  FactorRuntime topology engine carrier env ->
  FactorRuntime topology engine carrier env
installFactorQueryBindings bindings =
  overRuntimeFactorState
    ( \factorState ->
        factorState
          { rfsQueryBindings =
              Map.union bindings (rfsQueryBindings factorState)
          }
    )
{-# INLINE installFactorQueryBindings #-}

installFactorProgram ::
  RepairProgramKey ->
  FactorProgram ->
  FactorRuntime topology engine carrier env ->
  FactorRuntime topology engine carrier env
installFactorProgram repairKey program =
  installFactorPrograms (Map.singleton repairKey program)
{-# INLINE installFactorProgram #-}

installFactorPrograms ::
  Map RepairProgramKey FactorProgram ->
  FactorRuntime topology engine carrier env ->
  FactorRuntime topology engine carrier env
installFactorPrograms programs =
  overRuntimeFactorState
    ( \factorState ->
        factorState
          { rfsPrograms =
              Map.union programs (rfsPrograms factorState)
          }
    )
{-# INLINE installFactorPrograms #-}

appendFactorRepairStats ::
  RuntimeRepairStats ->
  FactorRuntime topology engine carrier env ->
  FactorRuntime topology engine carrier env
appendFactorRepairStats stats =
  overRuntimeFactorState
    ( \factorState ->
        factorState
          { rfsRepairStats =
              appendRuntimeRepairStats stats (rfsRepairStats factorState)
          }
    )
{-# INLINE appendFactorRepairStats #-}

clearFactorProgramCacheForCause ::
  RepairProgramKey ->
  FactorRepairCause ->
  FactorRuntime topology engine carrier env ->
  FactorRuntime topology engine carrier env
clearFactorProgramCacheForCause repairKey cause runtime
  | not (repairCauseDropsCache cause) =
      runtime
  | otherwise =
      case lookupFactorProgramByKey repairKey runtime of
        Nothing ->
          runtime
        Just program ->
          installFactorProgram
            repairKey
            program
              { fpCacheState =
                  clearFactorCacheState (fpCacheState program)
              }
            runtime
{-# INLINE clearFactorProgramCacheForCause #-}

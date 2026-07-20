module Moonlight.Flow.Runtime.Engine.Patch.Schedule.Retraction
  ( staleCarrierArtifactAddrs,
    retractStaleCarrierArtifactsWithFanout,
  )
where

import Control.Monad
  ( foldM,
  )
import Data.Maybe
  ( maybeToList,
  )
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
  )
import Moonlight.Flow.Carrier.Morphism.Subsumption
  ( CarrierReuseId,
  )
import Moonlight.Flow.Carrier.Reuse
  ( InstalledReuseMaterialization,
    StaleCarrierReuse,
    irmTarget,
    scrExpectedTarget,
    scrSource,
  )
import Moonlight.Flow.Model.Phase
  ( RelationalPhase (PhaseSubsumption),
  )
import Moonlight.Flow.Model.Schema.Boundary
  ( RuntimeBoundary,
  )
import Moonlight.Flow.Runtime.Carrier.Reuse
  ( StaleCarrierReuseRetraction,
    prepareStaleCarrierReuseRetraction,
    prepareStaleInstalledReuseMaterializationRetraction,
    retractStaleCarrierReuseAt,
    staleCarrierReuseRetractionContext,
  )
import Moonlight.Flow.Runtime.Engine.Schedule.Time
  ( allocateExecutionTime,
  )
import Moonlight.Flow.Runtime.Engine.Touch
  ( scheduleCarrierCommitTraceFanout,
  )
import Moonlight.Flow.Runtime.Execution.Failure
  ( RelationalRuntimeError,
  )
import Moonlight.Flow.Runtime.Kernel
  ( RelDiffRuntime,
  )

staleCarrierArtifactAddrs ::
  Ord ctx =>
  Ord prop =>
  [StaleCarrierReuse ctx prop] ->
  [(CarrierReuseId ctx prop, InstalledReuseMaterialization ctx prop)] ->
  Set (CarrierAddr ctx Carrier prop)
staleCarrierArtifactAddrs staleReuses staleInstalled =
  Set.fromList
    ( foldMap staleReuseAddrs staleReuses
        <> fmap (irmTarget . snd) staleInstalled
    )

staleReuseAddrs ::
  StaleCarrierReuse ctx prop ->
  [CarrierAddr ctx Carrier prop]
staleReuseAddrs stale =
  scrSource stale : maybeToList (scrExpectedTarget stale)

retractStaleCarrierArtifactsWithFanout ::
  (boundary ~ RuntimeBoundary, Ord ctx, Ord prop) =>
  [StaleCarrierReuse ctx prop] ->
  [(CarrierReuseId ctx prop, InstalledReuseMaterialization ctx prop)] ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr)
retractStaleCarrierArtifactsWithFanout staleReuses staleInstalled runtime0 = do
  runtimeAfterReuses <-
    foldM retractReuse runtime0 staleReuses
  foldM retractInstalled runtimeAfterReuses staleInstalled
  where
    retractReuse ::
      (Ord ctxValue, Ord propValue) =>
      RelDiffRuntime ctxValue propValue boundaryValue evidenceValue joinStateValue joinErrValue ->
      StaleCarrierReuse ctxValue propValue ->
      Either
        (RelationalRuntimeError ctxValue propValue boundaryValue evidenceValue)
        (RelDiffRuntime ctxValue propValue boundaryValue evidenceValue joinStateValue joinErrValue)
    retractReuse runtime stale = do
      (runtimePrepared, retraction) <-
        prepareStaleCarrierReuseRetraction stale runtime
      retractPrepared runtimePrepared retraction

    retractInstalled ::
      (Ord ctxValue, Ord propValue) =>
      RelDiffRuntime ctxValue propValue boundaryValue evidenceValue joinStateValue joinErrValue ->
      (CarrierReuseId ctxValue propValue, InstalledReuseMaterialization ctxValue propValue) ->
      Either
        (RelationalRuntimeError ctxValue propValue boundaryValue evidenceValue)
        (RelDiffRuntime ctxValue propValue boundaryValue evidenceValue joinStateValue joinErrValue)
    retractInstalled runtime installed = do
      (runtimePrepared, retraction) <-
        prepareStaleInstalledReuseMaterializationRetraction installed runtime
      retractPrepared runtimePrepared retraction

    retractPrepared ::
      (Ord ctxValue, Ord propValue) =>
      RelDiffRuntime ctxValue propValue boundaryValue evidenceValue joinStateValue joinErrValue ->
      StaleCarrierReuseRetraction ctxValue propValue ->
      Either
        (RelationalRuntimeError ctxValue propValue boundaryValue evidenceValue)
        (RelDiffRuntime ctxValue propValue boundaryValue evidenceValue joinStateValue joinErrValue)
    retractPrepared runtimePrepared retraction =
      case staleCarrierReuseRetractionContext retraction of
        Nothing ->
          Right runtimePrepared
        Just contextValue -> do
          (runtimeTimed, eventTime) <-
            allocateExecutionTime PhaseSubsumption contextValue runtimePrepared
          (runtimeRetracted, commitTrace) <-
            retractStaleCarrierReuseAt eventTime retraction runtimeTimed
          scheduleCarrierCommitTraceFanout commitTrace runtimeRetracted

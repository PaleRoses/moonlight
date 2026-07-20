{-# LANGUAGE ScopedTypeVariables #-}

module Moonlight.Flow.Runtime.Engine.GeneratedSite.Materialize
  ( materializeCarrierMoves,
  )
where

import Control.Applicative
  ( (<|>),
  )
import Control.Monad
  ( foldM,
  )
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier (..),
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
    caContext,
  )
import Moonlight.Flow.Carrier.Core.Delta
  ( RelationalCarrierDeltaP (..),
  )
import Moonlight.Flow.Carrier.Core.Time
  ( RelationalCarrierTime,
  )
import Moonlight.Differential.Row.Patch
  ( negatePlainRowPatch,
  )
import Moonlight.Flow.Model.Phase
  ( RelationalPhase (PhaseAmalgamate),
  )
import Moonlight.Flow.Runtime.Carrier.Store.Internal
  ( currentCarrierMaybeAtRouting,
  )
import Moonlight.Flow.Runtime.Carrier.Store.Write
  ( commitCarrierDeltaAtRouting,
  )
import Moonlight.Flow.Runtime.Engine.Schedule.Time
  ( allocateExecutionTimeForContract,
  )
import Moonlight.Flow.Runtime.Engine.Touch
  ( scheduleCarrierCommitTraceFanout,
  )
import Moonlight.Flow.Runtime.Execution.Failure
  ( RelationalRuntimeError,
  )
import Moonlight.Flow.Runtime.Execution.IR
  ( mkRuntimeDataflowContract,
  )
import Moonlight.Flow.Runtime.Kernel
  ( RelDiffRuntime,
  )
import Moonlight.Flow.Runtime.Topology.Routing
  ( RuntimeRouting,
  )
import Moonlight.Flow.Runtime.Topology.Site.Routing
  ( RoutingDelta (..),
  )
import Moonlight.Flow.Runtime.Topology.Site.Types

materializeCarrierMoves ::
  (Ord ctx, Ord prop) =>
  RoutingDelta ctx prop ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr)
materializeCarrierMoves routingDelta runtime0 =
  case carrierMoveContext (rdCarrierMoves routingDelta) of
    Nothing ->
      Right runtime0
    Just contextValue -> do
      (runtimeTimed, eventTime) <-
        allocateExecutionTimeForContract
          (mkRuntimeDataflowContract PhaseAmalgamate Set.empty Set.empty)
          contextValue
          runtime0
      runtimeMoved <-
        foldM
          (moveCarrier (rdBefore routingDelta) (rdAfter routingDelta) eventTime)
          runtimeTimed
          (carrierMovesRetargetPairs (rdCarrierMoves routingDelta))
      foldM
        (evictCarrier (rdBefore routingDelta) eventTime)
        runtimeMoved
        (Set.toAscList (cmEvict (rdCarrierMoves routingDelta)))
{-# INLINE materializeCarrierMoves #-}

carrierMoveContext ::
  CarrierMoves (CarrierAddr ctx Carrier prop) ->
  Maybe ctx
carrierMoveContext moves =
  fmap (caContext . snd) (Map.lookupMin (cmRetarget moves))
    <|> fmap caContext (Set.lookupMin (cmEvict moves))
{-# INLINE carrierMoveContext #-}

commitCarrierDeltaAtRoutingAndScheduleFanout ::
  (Ord ctx, Ord prop) =>
  RuntimeRouting ctx prop ->
  RelationalCarrierDeltaP ctx Carrier prop boundary evidence () ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr)
commitCarrierDeltaAtRoutingAndScheduleFanout routing delta runtime = do
  (runtimeCommitted, commitTrace) <-
    commitCarrierDeltaAtRouting routing delta runtime
  scheduleCarrierCommitTraceFanout commitTrace runtimeCommitted
{-# INLINE commitCarrierDeltaAtRoutingAndScheduleFanout #-}

moveCarrier ::
  (Ord ctx, Ord prop) =>
  RuntimeRouting ctx prop ->
  RuntimeRouting ctx prop ->
  RelationalCarrierTime ctx ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  (CarrierAddr ctx Carrier prop, CarrierAddr ctx Carrier prop) ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr)
moveCarrier beforeRouting afterRouting eventTime runtime (oldAddr, newAddr) = do
  maybeCurrent <- currentCarrierMaybeAtRouting beforeRouting oldAddr runtime
  case maybeCurrent of
    Nothing ->
      Right runtime
    Just current -> do
      let retractOld =
            current
              { deTime = eventTime,
                deRows = negatePlainRowPatch (deRows current)
              }
          insertNew =
            current
              { deAddr = newAddr,
                deTime = eventTime
              }
      runtime1 <-
        commitCarrierDeltaAtRoutingAndScheduleFanout
          beforeRouting
          retractOld
          runtime
      commitCarrierDeltaAtRoutingAndScheduleFanout
        afterRouting
        insertNew
        runtime1
{-# INLINE moveCarrier #-}

evictCarrier ::
  (Ord ctx, Ord prop) =>
  RuntimeRouting ctx prop ->
  RelationalCarrierTime ctx ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  CarrierAddr ctx Carrier prop ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr)
evictCarrier beforeRouting eventTime runtime addr = do
  maybeCurrent <- currentCarrierMaybeAtRouting beforeRouting addr runtime
  case maybeCurrent of
    Nothing ->
      Right runtime
    Just current -> do
      let retract =
            current
              { deTime = eventTime,
                deRows = negatePlainRowPatch (deRows current)
              }
      commitCarrierDeltaAtRoutingAndScheduleFanout
        beforeRouting
        retract
        runtime
{-# INLINE evictCarrier #-}

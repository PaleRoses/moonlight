{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Runtime.Core.Replay.Policy
  ( RuntimeReplayDomain (..),
    RuntimeReplaySelection (..),
    emptyRuntimeReplaySelection,
    mergeRuntimeReplaySelections,
    runtimeReplaySelectionFromCarrierAddrs,
    runtimeReplaySelectionFromDataflowOp,
    runtimeReplaySelectionFromDataflowOps,
    runtimeReplaySelectionPhases,
    runtimeReplaySelectionDomains,
    runtimeReplaySelectionCarriers,
    runtimeReplaySelectionContexts,
    RuntimeReplayValidation (..),
  )
where

import Data.Foldable qualified as Foldable
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
    caContext,
  )
import Moonlight.Flow.Model.Phase
  ( RelationalPhase,
  )
import Moonlight.Flow.Runtime.Execution.IR
  ( RuntimeDataflowOp,
    runtimeDataflowContractPhase,
    runtimeDataflowContractReads,
    runtimeDataflowContractWrites,
    runtimeDataflowOpContext,
    runtimeDataflowOpContract,
  )

data RuntimeReplayDomain ctx prop
  = RuntimeReplayCarrierDomain !(CarrierAddr ctx Carrier prop)
  | RuntimeReplayContextDomain !ctx
  deriving stock (Eq, Ord, Show)

data RuntimeReplaySelection ctx prop = RuntimeReplaySelection
  { rrsPhases :: !(Set RelationalPhase),
    rrsDomains :: !(Set (RuntimeReplayDomain ctx prop))
  }
  deriving stock (Eq, Ord, Show)

emptyRuntimeReplaySelection :: RuntimeReplaySelection ctx prop
emptyRuntimeReplaySelection =
  RuntimeReplaySelection
    { rrsPhases = Set.empty,
      rrsDomains = Set.empty
    }
{-# INLINE emptyRuntimeReplaySelection #-}

mergeRuntimeReplaySelections ::
  Ord ctx =>
  Ord prop =>
  RuntimeReplaySelection ctx prop ->
  RuntimeReplaySelection ctx prop ->
  RuntimeReplaySelection ctx prop
mergeRuntimeReplaySelections newer older =
  RuntimeReplaySelection
    { rrsPhases = Set.union (rrsPhases newer) (rrsPhases older),
      rrsDomains = Set.union (rrsDomains newer) (rrsDomains older)
    }
{-# INLINE mergeRuntimeReplaySelections #-}

runtimeReplaySelectionFromCarrierAddrs ::
  Ord ctx =>
  Ord prop =>
  RelationalPhase ->
  Set (CarrierAddr ctx Carrier prop) ->
  RuntimeReplaySelection ctx prop
runtimeReplaySelectionFromCarrierAddrs phaseValue addrs
  | Set.null addrs =
      emptyRuntimeReplaySelection
  | otherwise =
      RuntimeReplaySelection
        { rrsPhases = Set.singleton phaseValue,
          rrsDomains =
            Set.map RuntimeReplayCarrierDomain addrs
              <> Set.map (RuntimeReplayContextDomain . caContext) addrs
        }
{-# INLINE runtimeReplaySelectionFromCarrierAddrs #-}

runtimeReplaySelectionFromDataflowOp ::
  Ord ctx =>
  Ord prop =>
  RuntimeDataflowOp ctx prop boundary evidence ->
  RuntimeReplaySelection ctx prop
runtimeReplaySelectionFromDataflowOp op =
  let contract =
        runtimeDataflowOpContract op
      carrierAddrs =
        runtimeDataflowContractReads contract <> runtimeDataflowContractWrites contract
   in mergeRuntimeReplaySelections
        (runtimeReplaySelectionFromCarrierAddrs (runtimeDataflowContractPhase contract) carrierAddrs)
        emptyRuntimeReplaySelection
          { rrsDomains = Set.singleton (RuntimeReplayContextDomain (runtimeDataflowOpContext op))
          }
{-# INLINE runtimeReplaySelectionFromDataflowOp #-}

runtimeReplaySelectionFromDataflowOps ::
  Ord ctx =>
  Ord prop =>
  [RuntimeDataflowOp ctx prop boundary evidence] ->
  RuntimeReplaySelection ctx prop
runtimeReplaySelectionFromDataflowOps =
  Foldable.foldl'
    ( \selection op ->
        mergeRuntimeReplaySelections
          (runtimeReplaySelectionFromDataflowOp op)
          selection
    )
    emptyRuntimeReplaySelection
{-# INLINE runtimeReplaySelectionFromDataflowOps #-}

runtimeReplaySelectionPhases ::
  RuntimeReplaySelection ctx prop ->
  Set RelationalPhase
runtimeReplaySelectionPhases =
  rrsPhases
{-# INLINE runtimeReplaySelectionPhases #-}

runtimeReplaySelectionDomains ::
  RuntimeReplaySelection ctx prop ->
  Set (RuntimeReplayDomain ctx prop)
runtimeReplaySelectionDomains =
  rrsDomains
{-# INLINE runtimeReplaySelectionDomains #-}

runtimeReplaySelectionCarriers ::
  Ord ctx =>
  Ord prop =>
  RuntimeReplaySelection ctx prop ->
  Set (CarrierAddr ctx Carrier prop)
runtimeReplaySelectionCarriers selection =
  Set.fromList
    [ addr
    | RuntimeReplayCarrierDomain addr <- Set.toAscList (rrsDomains selection)
    ]
{-# INLINE runtimeReplaySelectionCarriers #-}

runtimeReplaySelectionContexts ::
  Ord ctx =>
  RuntimeReplaySelection ctx prop ->
  Set ctx
runtimeReplaySelectionContexts selection =
  Set.fromList
    [ contextValue
    | domain <- Set.toAscList (rrsDomains selection),
      contextValue <-
        case domain of
          RuntimeReplayCarrierDomain addr ->
            [caContext addr]
          RuntimeReplayContextDomain contextValue ->
            [contextValue]
    ]
{-# INLINE runtimeReplaySelectionContexts #-}

data RuntimeReplayValidation
  = RuntimeReplayValidationDisabled
  | RuntimeReplayValidationEnabled
  deriving stock (Eq, Ord, Show, Read)

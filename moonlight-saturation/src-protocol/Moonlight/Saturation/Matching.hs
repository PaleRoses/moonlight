{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Saturation.Matching
  ( SaturationPurpose (..),
    QueryFingerprint (..),
    MatchSite (..),
    MatchWorld (..),
    QueryRequest (..),
    MatchingQuery (..),
    MatchingState (..),
    AdvanceContext (..),
    Scope,
    Scoped (..),
    MatchingAlgebra (..),
    MatchingReplayDiagnostics,
    MatchingReplayDiagnosticsValidationError,
    diffMatchingReplayDiagnostics,
    mapMatchingQueryScope,
    prepareSingleQuery,
    runSingleQuery,
    runPreparedQueries,
    previewSingleQuery,
    prepareUnitSingleQuery,
    runUnitSingleQuery,
    runUnitPreparedQueries,
    previewUnitSingleQuery,
  )
where

import Data.Kind (Type)
import Moonlight.Delta.Scope
  ( Scope,
    Scoped (..),
    cleanScope,
  )
import Moonlight.Pale.Diagnostic.Section.Replay
  ( ReplayDiagnostics,
    ReplayDiagnosticsValidationError,
    diffReplayDiagnostics,
  )

type QueryFingerprint :: Type
newtype QueryFingerprint = QueryFingerprint
  { queryFingerprintKey :: Int
  }
  deriving stock (Eq, Ord, Show, Read)

type SaturationPurpose :: Type -> Type -> Type
data SaturationPurpose rewriteRule factRule
  = RawMatchPurpose
  | RewritePurpose !rewriteRule
  | FactRulePurpose !factRule
  deriving stock (Eq, Ord, Show, Read)

type MatchSite :: Type -> Type
data MatchSite c
  = BaseSite
  | ContextSite !c
  deriving stock (Eq, Ord, Show)

type MatchWorld :: Type -> Type -> Type -> Type -> Type -> Type
data MatchWorld graph facts derivs capabilities proof = MatchWorld
  { mwGraph :: !graph,
    mwFacts :: !facts,
    mwFactDerivations :: !derivs,
    mwCapabilities :: !capabilities,
    mwProofContext :: !proof,
    mwIteration :: !Int
  }
  deriving stock (Eq, Show)

type QueryRequest :: Type -> Type -> Type -> Type -> Type -> Type
data QueryRequest site snapshot query purpose host = QueryRequest
  { qrSite :: !site,
    qrSnapshot :: !snapshot,
    qrQuery :: !query,
    qrPurpose :: !purpose
  }
  deriving stock (Eq, Ord, Show, Read)

type AdvanceContext :: Type -> Type -> Type
data AdvanceContext host repair = AdvanceContext
  { acHostState :: !host,
    acRepairPayload :: !repair
  }

type MatchingQuery :: Type -> (Type -> Type) -> Type -> Type
data MatchingQuery s request host = MatchingQuery
  { mqScope :: !(Scope s),
    mqRequest :: !(request host)
  }

type MatchingState :: Type -> Type -> Type
data MatchingState base overlay = MatchingState
  { msInner :: !base,
    msOverlay :: !overlay
  }

type MatchingAlgebra :: Type -> Type -> Type -> Type -> Type -> (Type -> Type) -> (Type -> Type) -> Type -> Type -> Type
data MatchingAlgebra environment state s payload world request advance obstruction match = MatchingAlgebra
  { maInitialState :: !state,
    maEnvironment :: !environment,
    maPrepareQueries ::
      forall host.
      state ->
      Scoped s payload ->
      world ->
      [request host] ->
      (state, [MatchingQuery s request host]),
    maRunQueries ::
      forall host.
      state ->
      world ->
      [MatchingQuery s request host] ->
      (state, Either obstruction [[match]]),
    maPreviewQuery ::
      forall host.
      state ->
      world ->
      MatchingQuery s request host ->
      Maybe (state, MatchingReplayDiagnostics),
    maAdvanceState ::
      forall host.
      Scoped s payload ->
      advance host ->
      state ->
      state,
    maReplayDiagnostics ::
      state ->
      Maybe MatchingReplayDiagnostics
  }

type MatchingReplayDiagnostics :: Type
type MatchingReplayDiagnostics = ReplayDiagnostics

type MatchingReplayDiagnosticsValidationError :: Type
type MatchingReplayDiagnosticsValidationError = ReplayDiagnosticsValidationError

diffMatchingReplayDiagnostics ::
  MatchingReplayDiagnostics ->
  MatchingReplayDiagnostics ->
  Either MatchingReplayDiagnosticsValidationError MatchingReplayDiagnostics
diffMatchingReplayDiagnostics = diffReplayDiagnostics

mapMatchingQueryScope ::
  (Scope s -> Scope s) ->
  MatchingQuery s request host ->
  MatchingQuery s request host
mapMatchingQueryScope updateScope matchingQuery =
  matchingQuery
    { mqScope = updateScope (mqScope matchingQuery)
    }
{-# INLINE mapMatchingQueryScope #-}

prepareSingleQuery ::
  MatchingAlgebra environment state s payload world request advance obstruction match ->
  state ->
  Scoped s payload ->
  world ->
  request host ->
  (state, Scope s)
prepareSingleQuery matchingAlgebra state matchingDelta world request =
  let (nextState, preparedQueries) =
        maPrepareQueries matchingAlgebra state matchingDelta world [request]
   in ( nextState,
        case preparedQueries of
          [] -> cleanScope
          preparedQuery : _ -> mqScope preparedQuery
      )
{-# INLINE prepareSingleQuery #-}

runSingleQuery ::
  MatchingAlgebra environment state s payload world request advance obstruction match ->
  state ->
  world ->
  Scope s ->
  request host ->
  (state, Either obstruction [match])
runSingleQuery matchingAlgebra state world matchingFrontier request =
  let (nextState, matchesResult) =
        maRunQueries
          matchingAlgebra
          state
          world
          [MatchingQuery matchingFrontier request]
   in ( nextState,
        case matchesResult of
          Left obstruction -> Left obstruction
          Right [] -> Right []
          Right (matches : _) -> Right matches
      )
{-# INLINE runSingleQuery #-}

runPreparedQueries ::
  MatchingAlgebra environment state s payload world request advance obstruction match ->
  state ->
  world ->
  [(Scope s, request host)] ->
  (state, Either obstruction [[match]])
runPreparedQueries matchingAlgebra state world frontierRequests =
  maRunQueries
    matchingAlgebra
    state
    world
    (fmap (uncurry MatchingQuery) frontierRequests)
{-# INLINE runPreparedQueries #-}

previewSingleQuery ::
  MatchingAlgebra environment state s payload world request advance obstruction match ->
  state ->
  world ->
  Scope s ->
  request host ->
  Maybe (state, MatchingReplayDiagnostics)
previewSingleQuery matchingAlgebra state world matchingFrontier request =
  maPreviewQuery
    matchingAlgebra
    state
    world
    (MatchingQuery matchingFrontier request)
{-# INLINE previewSingleQuery #-}

prepareUnitSingleQuery ::
  MatchingAlgebra environment state s payload () request advance obstruction match ->
  state ->
  Scoped s payload ->
  request host ->
  (state, Scope s)
prepareUnitSingleQuery matchingAlgebra state matchingDelta =
  prepareSingleQuery matchingAlgebra state matchingDelta ()
{-# INLINE prepareUnitSingleQuery #-}

runUnitSingleQuery ::
  MatchingAlgebra environment state s payload () request advance obstruction match ->
  state ->
  Scope s ->
  request host ->
  (state, Either obstruction [match])
runUnitSingleQuery matchingAlgebra state =
  runSingleQuery matchingAlgebra state ()
{-# INLINE runUnitSingleQuery #-}

runUnitPreparedQueries ::
  MatchingAlgebra environment state s payload () request advance obstruction match ->
  state ->
  [(Scope s, request host)] ->
  (state, Either obstruction [[match]])
runUnitPreparedQueries matchingAlgebra state =
  runPreparedQueries matchingAlgebra state ()
{-# INLINE runUnitPreparedQueries #-}

previewUnitSingleQuery ::
  MatchingAlgebra environment state s payload () request advance obstruction match ->
  state ->
  Scope s ->
  request host ->
  Maybe (state, MatchingReplayDiagnostics)
previewUnitSingleQuery matchingAlgebra state =
  previewSingleQuery matchingAlgebra state ()
{-# INLINE previewUnitSingleQuery #-}

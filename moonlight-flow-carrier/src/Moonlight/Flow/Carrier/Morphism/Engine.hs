module Moonlight.Flow.Carrier.Morphism.Engine
  ( stepCarrierMorphism,
    runCarrierMorphismBatch,
    flushCarrierMorphism,
    CarrierReuseOps (..),
    checkedReuseSupportProject,
    runCarrierReuseMorphism,
    runCarrierAmalgamation,
    carrierMorphismOp,
  )
where

import Control.Monad
  ( foldM,
  )
import Data.List.NonEmpty
  ( NonEmpty,
  )
import Moonlight.Delta.Operator
  ( OpResult (..),
    Operator (..),
    noOutput,
  )
import Moonlight.Delta.Time
  ( Timed (..),
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
  )
import Moonlight.Flow.Carrier.Core.Delta
  ( RelationalCarrierDelta,
    RelationalCarrierDeltaP (..),
  )
import Moonlight.Flow.Carrier.Core.Time
  ( RelationalCarrierTime,
  )
import Moonlight.Differential.Carrier.Topology
  ( CarrierCover,
  )
import Moonlight.Flow.Carrier.Morphism.Amalgamation
  ( AmalgamationError,
    AmalgamationResult,
  )
import Moonlight.Flow.Carrier.Morphism.Internal.Glue
  ( amalgamateCarrierFamily,
  )
import Moonlight.Flow.Carrier.Morphism.Core.Program
  ( CarrierMorphismProgram,
    CarrierMorphismRuntime (..),
    applyCarrierMorphismProgram,
    carrierMorphismProgramsFrom,
  )
import Moonlight.Flow.Carrier.Morphism.Internal.Reuse
  ( CarrierReuseOps (..),
    checkedReuseSupportProject,
    projectCarrierReuse,
  )
import Moonlight.Flow.Carrier.Morphism.Result
  ( CarrierMorphismDiagnostic,
    CarrierMorphismError,
    CarrierMorphismOutput (..),
    emptyCarrierMorphismOutput,
  )
import Moonlight.Differential.Row.Patch
  ( plainRowPatchNull,
  )
import Moonlight.Flow.Carrier.Morphism.Subsumption
  ( CarrierReuse,
    CarrierReuseError,
  )
import Moonlight.Flow.Model.Schema.Boundary
  ( RuntimeBoundary,
  )

stepCarrierMorphism ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CarrierMorphismRuntime ctx carrier prop boundary evidence ->
  Timed
    (RelationalCarrierTime ctx)
    (RelationalCarrierDelta ctx carrier prop boundary evidence) ->
  Either
    (CarrierMorphismError ctx carrier prop boundary evidence)
    ( CarrierMorphismOutput ctx carrier prop boundary evidence,
      CarrierMorphismRuntime ctx carrier prop boundary evidence
    )
stepCarrierMorphism runtime timedDelta = do
  emitted <-
    traverse
      (applyProgram (timedAt timedDelta) (timedValue timedDelta))
      ( carrierMorphismProgramsFrom
          (deAddr (timedValue timedDelta))
          (cmrContext runtime)
      )
  pure
    ( CarrierMorphismOutput
        { cmoEmitted = filterNonEmptyDeltas emitted,
          cmoDiagnostics = []
        },
      runtime
    )
{-# INLINE stepCarrierMorphism #-}

applyProgram ::
  (Ord ctx, Ord carrier, Ord prop) =>
  RelationalCarrierTime ctx ->
  RelationalCarrierDelta ctx carrier prop boundary evidence ->
  CarrierMorphismProgram ctx carrier prop boundary evidence ->
  Either
    (CarrierMorphismError ctx carrier prop boundary evidence)
    (RelationalCarrierDelta ctx carrier prop boundary evidence)
applyProgram eventTime sourceDelta program =
  applyCarrierMorphismProgram eventTime sourceDelta program
{-# INLINE applyProgram #-}

runCarrierMorphismBatch ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CarrierMorphismRuntime ctx carrier prop boundary evidence ->
  [Timed
      (RelationalCarrierTime ctx)
      (RelationalCarrierDelta ctx carrier prop boundary evidence)
  ] ->
  Either
    (CarrierMorphismError ctx carrier prop boundary evidence)
    ( CarrierMorphismOutput ctx carrier prop boundary evidence,
      CarrierMorphismRuntime ctx carrier prop boundary evidence
    )
runCarrierMorphismBatch runtime0 timedDeltas =
  finishBatch <$> foldM stepBatch (BatchOutput id id runtime0) timedDeltas
{-# INLINE runCarrierMorphismBatch #-}

data BatchOutput ctx carrier prop boundary evidence = BatchOutput
  { boEmitted ::
      [RelationalCarrierDelta ctx carrier prop boundary evidence] ->
      [RelationalCarrierDelta ctx carrier prop boundary evidence],
    boDiagnostics ::
      [CarrierMorphismDiagnostic ctx carrier prop boundary evidence] ->
      [CarrierMorphismDiagnostic ctx carrier prop boundary evidence],
    boRuntime :: !(CarrierMorphismRuntime ctx carrier prop boundary evidence)
  }

stepBatch ::
  (Ord ctx, Ord carrier, Ord prop) =>
  BatchOutput ctx carrier prop boundary evidence ->
  Timed
    (RelationalCarrierTime ctx)
    (RelationalCarrierDelta ctx carrier prop boundary evidence) ->
  Either
    (CarrierMorphismError ctx carrier prop boundary evidence)
    (BatchOutput ctx carrier prop boundary evidence)
stepBatch batch timedDelta = do
  (output, runtimeNext) <-
    stepCarrierMorphism (boRuntime batch) timedDelta
  pure
    BatchOutput
      { boEmitted = boEmitted batch . (cmoEmitted output <>),
        boDiagnostics = boDiagnostics batch . (cmoDiagnostics output <>),
        boRuntime = runtimeNext
      }
{-# INLINE stepBatch #-}

finishBatch ::
  BatchOutput ctx carrier prop boundary evidence ->
  ( CarrierMorphismOutput ctx carrier prop boundary evidence,
    CarrierMorphismRuntime ctx carrier prop boundary evidence
  )
finishBatch batch =
  ( CarrierMorphismOutput
      { cmoEmitted = boEmitted batch [],
        cmoDiagnostics = boDiagnostics batch []
      },
    boRuntime batch
  )
{-# INLINE finishBatch #-}

flushCarrierMorphism ::
  CarrierMorphismRuntime ctx carrier prop boundary evidence ->
  ( CarrierMorphismOutput ctx carrier prop boundary evidence,
    CarrierMorphismRuntime ctx carrier prop boundary evidence
  )
flushCarrierMorphism runtime =
  (emptyCarrierMorphismOutput, runtime)
{-# INLINE flushCarrierMorphism #-}

runCarrierReuseMorphism ::
  (Ord ctx, Ord prop) =>
  CarrierReuseOps ctx prop evidence ->
  CarrierReuse ctx prop ->
  RelationalCarrierDelta ctx Carrier prop RuntimeBoundary evidence ->
  Either
    (CarrierReuseError ctx prop evidence)
    (RelationalCarrierDelta ctx Carrier prop RuntimeBoundary evidence)
runCarrierReuseMorphism =
  projectCarrierReuse
{-# INLINE runCarrierReuseMorphism #-}

runCarrierAmalgamation ::
  (Ord ctx, Ord carrier, Ord prop, Semigroup evidence) =>
  CarrierCover ctx ->
  NonEmpty (RelationalCarrierDelta ctx carrier prop RuntimeBoundary evidence) ->
  Either
    (AmalgamationError ctx carrier prop RuntimeBoundary evidence)
    (AmalgamationResult ctx carrier prop RuntimeBoundary evidence)
runCarrierAmalgamation =
  amalgamateCarrierFamily
{-# INLINE runCarrierAmalgamation #-}

carrierMorphismOp ::
  (Ord ctx, Ord carrier, Ord prop) =>
  Operator
    (RelationalCarrierTime ctx)
    (CarrierMorphismRuntime ctx carrier prop boundary evidence)
    (RelationalCarrierDelta ctx carrier prop boundary evidence)
    (RelationalCarrierDelta ctx carrier prop boundary evidence)
    (CarrierMorphismError ctx carrier prop boundary evidence)
carrierMorphismOp =
  Operator
    { opStep =
        \runtime timedDelta -> do
          (output, runtimeNext) <-
            stepCarrierMorphism runtime timedDelta
          Right
            OpResult
              { orState = runtimeNext,
                orEmit = fmap timedCarrierDelta (cmoEmitted output)
              },
      opFlush =
        \runtime ->
          Right (noOutput runtime)
    }
{-# INLINE carrierMorphismOp #-}

timedCarrierDelta ::
  RelationalCarrierDelta ctx carrier prop boundary evidence ->
  Timed
    (RelationalCarrierTime ctx)
    (RelationalCarrierDelta ctx carrier prop boundary evidence)
timedCarrierDelta deltaValue =
  Timed (deTime deltaValue) deltaValue
{-# INLINE timedCarrierDelta #-}

filterNonEmptyDeltas ::
  [RelationalCarrierDelta ctx carrier prop boundary evidence] ->
  [RelationalCarrierDelta ctx carrier prop boundary evidence]
filterNonEmptyDeltas =
  filter (not . plainRowPatchNull . deRows)
{-# INLINE filterNonEmptyDeltas #-}

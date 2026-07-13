{-# LANGUAGE ScopedTypeVariables #-}

-- | The circuit denotation seal, phrased as a reusable predicate: integrated
-- advance-replay over a batch sequence equals the eager denotation of the
-- integrated input, read at a chosen output node.  This is the integral form of
-- @advance = incrementalize denotation@: integrating both sides over the prefix
-- and using @denotation empty = empty@ (every relational query maps the empty
-- collection to itself) collapses to
-- @integrate (advance deltas) = denotation (integrate deltas)@, which is what
-- the predicate checks.  A downstream interpreter instantiates it with any
-- sealed circuit and output handle.
module Moonlight.Differential.Effect.Harness.Circuit
  ( advanceReplayIntegratesToDenotation,
    advanceAgreesWithIncrementalizeOfDenotation,
    circuitEagerAgreesWithCollection,
    replayAdvance,
  )
where

import Data.Map.Strict qualified as Map
import Numeric.Natural
  ( Natural,
  )

import Moonlight.Core (AdditiveMonoid (..))
import Moonlight.Differential.Algebra.ZSet
  ( ZSet,
    zsetToAscList,
    zsetUnions,
  )
import Moonlight.Differential.Circuit
  ( Circuit,
    CircuitAdvanceError,
    CircuitBatch,
    CircuitOutputs,
    InputPort,
    Node,
    advanceCircuit,
    emptyCircuitBatch,
    evaluateCircuit,
    feedInput,
    outputDelta,
  )
import Moonlight.Differential.Stream
  ( Stream,
    incrementalize,
    stream,
    streamAt,
  )
import Test.Tasty.QuickCheck qualified as QC

-- | Thread a circuit through a batch sequence, collecting the per-batch outputs
-- in arrival order.  Pure: a refused advance short-circuits to the typed fault
-- and the prior circuit is never observed (persistence makes replay total).
replayAdvance ::
  forall s fault.
  Circuit s fault Int ->
  [CircuitBatch s Int] ->
  Either (CircuitAdvanceError fault) [CircuitOutputs s Int]
replayAdvance = go []
  where
    go ::
      [CircuitOutputs s Int] ->
      Circuit s fault Int ->
      [CircuitBatch s Int] ->
      Either (CircuitAdvanceError fault) [CircuitOutputs s Int]
    go acc _ [] =
      Right (reverse acc)
    go acc circuit (batch : rest) =
      case advanceCircuit batch circuit of
        Left refusal ->
          Left refusal
        Right (outputs, advanced) ->
          go (outputs : acc) advanced rest

-- | The flagship law body.  @batches@ is the delta sequence (per-port feeds
-- already assembled); @wholeBatch@ is the same input integrated into one batch.
-- Replaying advance and integrating the per-batch output deltas must equal the
-- eager evaluation of @wholeBatch@ at @output@.
advanceReplayIntegratesToDenotation ::
  (Ord value, Show value, Show fault) =>
  Circuit s fault Int ->
  Node s value ->
  [CircuitBatch s Int] ->
  CircuitBatch s Int ->
  QC.Property
advanceReplayIntegratesToDenotation circuit output batches wholeBatch =
  case replayAdvance circuit batches of
    Left refusal ->
      QC.counterexample ("advance replay refused: " <> show refusal) False
    Right outputsList ->
      case evaluateCircuit wholeBatch circuit of
        Left refusal ->
          QC.counterexample ("eager evaluation refused: " <> show refusal) False
        Right eager ->
          case (traverse (outputDelta output) outputsList, outputDelta output eager) of
            (Left obstruction, _) ->
              QC.counterexample ("replay output missing: " <> show obstruction) False
            (_, Left obstruction) ->
              QC.counterexample ("eager output missing: " <> show obstruction) False
            (Right replayed, Right eagerOutput) ->
              zsetToAscList (zsetUnions replayed)
                QC.=== zsetToAscList eagerOutput

-- | The literal @incrementalize@ witness, in differential form: the per-batch
-- advance deltas at @output@ equal the named Stream-calculus transform —
-- @incrementalize@ from "Moonlight.Differential.Stream" — applied to the
-- pipeline's pointwise denotation over the input delta stream.  The flagship
-- above is the integral of this statement; this one binds the seal to
-- @incrementalize@ by name.
advanceAgreesWithIncrementalizeOfDenotation ::
  (Ord input, Ord output, Show output, Show fault) =>
  Circuit s fault Int ->
  Node s output ->
  InputPort s input ->
  (ZSet input Int -> ZSet output Int) ->
  [ZSet input Int] ->
  QC.Property
advanceAgreesWithIncrementalizeOfDenotation circuit output port denotation deltas =
  case replayAdvance circuit [feedInput port delta emptyCircuitBatch | delta <- deltas] of
    Left refusal ->
      QC.counterexample ("advance replay refused: " <> show refusal) False
    Right outputsList ->
      case traverse (outputDelta output) outputsList of
        Left obstruction ->
          QC.counterexample ("replay output missing: " <> show obstruction) False
        Right advanced ->
          let incrementalized =
                incrementalize
                  (\collections -> stream (denotation . streamAt collections))
                  (deltaStream deltas)
              expected =
                [streamAt incrementalized time | time <- take (length deltas) [0 ..]]
           in fmap zsetToAscList expected QC.=== fmap zsetToAscList advanced

-- | The batch script as a stream over 'Natural' time: the step delta inside
-- the script window, zero beyond it.
deltaStream :: Ord value => [ZSet value Int] -> Stream Natural (ZSet value Int)
deltaStream deltas =
  stream (\time -> Map.findWithDefault zero time indexed)
  where
    indexed = Map.fromList (zip [0 ..] deltas)

-- | Spec/value agreement: the circuit's own eager denotation at @output@ over
-- the integrated input equals the reference Collection value algebra evaluated
-- over the same input.  Where the flagship pins advance to @evaluateCircuit@,
-- this pins @evaluateCircuit@ to an independently-written relational algebra —
-- the circuit builder's combinators denote what their Collection namesakes do,
-- so the graph structure (handles, slots, topological eval order, fixpoint
-- span) never corrupts the denotation.  The reference arrives already reduced
-- to an ascending assoc list, so the harness itself stays Collection-free.
circuitEagerAgreesWithCollection ::
  (Ord value, Show value, Show fault) =>
  Circuit s fault Int ->
  Node s value ->
  CircuitBatch s Int ->
  [(value, Int)] ->
  QC.Property
circuitEagerAgreesWithCollection circuit output wholeBatch collectionDenotation =
  case evaluateCircuit wholeBatch circuit of
    Left refusal ->
      QC.counterexample ("eager evaluation refused: " <> show refusal) False
    Right eager ->
      case outputDelta output eager of
        Left obstruction ->
          QC.counterexample ("eager output missing: " <> show obstruction) False
        Right eagerOutput ->
          zsetToAscList eagerOutput QC.=== collectionDenotation

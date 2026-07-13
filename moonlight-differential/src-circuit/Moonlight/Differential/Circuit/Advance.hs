-- | Circuit execution: feed a batch of input deltas, advance every kernel
-- once in topological order, read the output deltas; a failed advance leaves
-- the prior circuit value standing untouched.
module Moonlight.Differential.Circuit.Advance
  ( CircuitBatch,
    emptyCircuitBatch,
    feedInput,
    CircuitOutputs,
    outputDelta,
    indexedOutputDelta,
    advanceCircuit,
  )
where

import Control.Monad
  ( foldM,
  )
import Data.IntMap.Strict qualified as IntMap
import Moonlight.Core
  ( AdditiveGroup,
  )
import Moonlight.Differential.Algebra.ZSet
  ( IndexedZSet,
    ZSet,
  )
import Moonlight.Differential.Circuit.Carrier
  ( Circuit (..),
    CircuitBatch (..),
    CircuitOutputs (..),
    Kernel (..),
  )
import Moonlight.Differential.Circuit.Slot
  ( mkSlotValue,
    unsafeReadSlot,
  )
import Moonlight.Differential.Circuit.Types
  ( CircuitAdvanceError,
    CircuitOutputError (..),
    IndexedNode,
    InputPort,
    Node,
    indexedNodeId,
    inputPortId,
    nodeId,
  )

emptyCircuitBatch :: CircuitBatch s weight
emptyCircuitBatch =
  CircuitBatch IntMap.empty
{-# INLINE emptyCircuitBatch #-}

feedInput ::
  forall value s weight.
  (Ord value, Eq weight, AdditiveGroup weight) =>
  InputPort s value ->
  ZSet value weight ->
  CircuitBatch s weight ->
  CircuitBatch s weight
feedInput port delta (CircuitBatch feeds) =
  CircuitBatch
    (IntMap.insertWith merge (inputPortId port) (mkSlotValue delta) feeds)
  where
    merge newSlot oldSlot =
      mkSlotValue
        ((unsafeReadSlot oldSlot :: ZSet value weight) <> unsafeReadSlot newSlot)
{-# INLINE feedInput #-}

outputDelta ::
  Node s value ->
  CircuitOutputs s weight ->
  Either CircuitOutputError (ZSet value weight)
outputDelta node (CircuitOutputs slots) =
  maybe
    (Left (CircuitOutputMissing (nodeId node)))
    (Right . unsafeReadSlot)
    (IntMap.lookup (nodeId node) slots)
{-# INLINE outputDelta #-}

indexedOutputDelta ::
  IndexedNode s key value ->
  CircuitOutputs s weight ->
  Either CircuitOutputError (IndexedZSet key value weight)
indexedOutputDelta node (CircuitOutputs slots) =
  maybe
    (Left (CircuitOutputMissing (indexedNodeId node)))
    (Right . unsafeReadSlot)
    (IntMap.lookup (indexedNodeId node) slots)
{-# INLINE indexedOutputDelta #-}

advanceCircuit ::
  CircuitBatch s weight ->
  Circuit s fault weight ->
  Either (CircuitAdvanceError fault) (CircuitOutputs s weight, Circuit s fault weight)
advanceCircuit (CircuitBatch feeds) circuit = do
  (slots, reversedProgram) <-
    foldM step (feeds, []) (circuitProgram circuit)
  pure
    ( CircuitOutputs slots,
      circuit {circuitProgram = reverse reversedProgram}
    )
  where
    step (slots, reversedProgram) (selfId, kernel) = do
      (outSlot, advancedKernel) <- runKernel kernel slots
      pure
        ( IntMap.insert selfId outSlot slots,
          (selfId, advancedKernel) : reversedProgram
        )

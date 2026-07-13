-- | Circuit calculus facade: build a region-typed operator graph, seal it to
-- compiled kernels, advance batches differentially, evaluate eagerly for the
-- denotation the advance must agree with.
module Moonlight.Differential.Circuit
  ( -- * Handles and vocabulary
    Node,
    IndexedNode,
    InputPort,
    nodeId,
    indexedNodeId,
    inputPortId,
    NodeKind (..),
    NodeShape (..),

    -- * Building and sealing
    CircuitBuilder,
    inputNode,
    mapNode,
    filterNode,
    flatMapNode,
    concatNodes,
    negateNode,
    differenceNodes,
    indexByNode,
    deindexNode,
    joinNodes,
    countByNode,
    aggregateNode,
    distinctNode,
    fixpointNode,
    foreignNode,
    foreignNode2,
    SealedCircuit (..),
    buildCircuit,
    withSealedCircuit,
    CircuitBuildError (..),

    -- * The sealed circuit
    Circuit,
    circuitShapeOf,
    circuitInputIds,

    -- * Advancing
    CircuitBatch,
    emptyCircuitBatch,
    feedInput,
    advanceCircuit,
    CircuitOutputs,
    outputDelta,
    indexedOutputDelta,
    CircuitAdvanceError (..),
    CircuitOutputError (..),

    -- * Denotation
    evaluateCircuit,

    -- * Foreign nodes
    ForeignKernel (..),
    ForeignKernel2 (..),
  )
where

import Moonlight.Differential.Circuit.Advance
  ( CircuitBatch,
    CircuitOutputs,
    advanceCircuit,
    emptyCircuitBatch,
    feedInput,
    indexedOutputDelta,
    outputDelta,
  )
import Moonlight.Differential.Circuit.Build
  ( CircuitBuilder,
    SealedCircuit (..),
    aggregateNode,
    buildCircuit,
    concatNodes,
    countByNode,
    deindexNode,
    differenceNodes,
    distinctNode,
    filterNode,
    fixpointNode,
    flatMapNode,
    foreignNode,
    foreignNode2,
    indexByNode,
    inputNode,
    joinNodes,
    mapNode,
    negateNode,
    withSealedCircuit,
  )
import Moonlight.Differential.Circuit.Carrier
  ( Circuit,
    circuitInputIds,
    circuitShapeOf,
  )
import Moonlight.Differential.Circuit.Denotation
  ( evaluateCircuit,
  )
import Moonlight.Differential.Circuit.Foreign
  ( ForeignKernel (..),
    ForeignKernel2 (..),
  )
import Moonlight.Differential.Circuit.Types
  ( CircuitAdvanceError (..),
    CircuitBuildError (..),
    CircuitOutputError (..),
    IndexedNode,
    InputPort,
    Node,
    NodeKind (..),
    NodeShape (..),
    indexedNodeId,
    inputPortId,
    nodeId,
  )

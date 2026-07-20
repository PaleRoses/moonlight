-- | Circuit denotation: eager whole-collection evaluation of the sealed
-- graph, the value semantics native kernels must agree with. Foreign kernels
-- participate only under their caller-owned step/denotation coherence law.
module Moonlight.Differential.Circuit.Denotation
  ( evaluateCircuit,
  )
where

import Moonlight.Algebra
  ( Semiring,
  )
import Moonlight.Core
  ( AdditiveGroup,
  )
import Moonlight.Differential.Circuit.Carrier
  ( Circuit (..),
    CircuitBatch (..),
    CircuitOutputs (..),
  )
import Moonlight.Differential.Circuit.Eval
  ( evalIncluded,
  )
import Moonlight.Differential.Circuit.Types
  ( CircuitAdvanceError,
  )

evaluateCircuit ::
  (Ord weight, AdditiveGroup weight, Semiring weight) =>
  CircuitBatch s weight ->
  Circuit s fault weight ->
  Either (CircuitAdvanceError fault) (CircuitOutputs s weight)
evaluateCircuit (CircuitBatch feeds) circuit =
  CircuitOutputs
    <$> evalIncluded (circuitNodes circuit) (circuitScopes circuit) feeds

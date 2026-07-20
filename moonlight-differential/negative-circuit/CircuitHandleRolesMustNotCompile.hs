{-# LANGUAGE ExplicitForAll #-}

-- This fixture must fail to compile.  Circuit handles are nominal in both
-- their region and payload indices; representational coercion would let a
-- client read another circuit's slots or reinterpret an erased payload.
module CircuitHandleRolesMustNotCompile where

import Data.Coerce
  ( coerce,
  )
import Moonlight.Differential.Circuit.Handle
  ( IndexedNode,
    InputPort,
    Node,
  )

coerceCircuitRegion :: forall source target value. Node source value -> Node target value
coerceCircuitRegion =
  coerce

coerceCircuitPayload :: forall region. Node region Int -> Node region Bool
coerceCircuitPayload =
  coerce

coerceIndexedKey :: forall region source target value. IndexedNode region source value -> IndexedNode region target value
coerceIndexedKey =
  coerce

coerceInputPayload :: forall region. InputPort region Int -> InputPort region Bool
coerceInputPayload =
  coerce

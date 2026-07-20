{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.EGraph.Saturation.Context.State
  ( ContextSaturationState,
    cssQueryRegistry,
    SaturatingContextEGraph,
    sceContextGraph,
    sceSaturationState,
    SaturatingProofEGraph,
    emptyContextSaturationState,
    emptySaturatingContextEGraph,
    emptySaturatingProofEGraph,
    emptySaturatingProofEGraphWithRetention,
    mapSaturatingContextGraph,
  )
where

import Data.Kind (Type)
import Moonlight.EGraph.Pure.Context
  ( ContextEGraph,
  )
import Moonlight.EGraph.Pure.Context.Proof
  ( ProofGraph,
    emptyProofGraph,
    emptyProofGraphWithRetention,
  )
import Moonlight.EGraph.Pure.Relational
  ( QueryPlan,
  )
import Moonlight.Saturation.Context.Match.State.Registry qualified as SaturationMatch
import Moonlight.Rewrite.ProofContext (ProofRetention)

type ContextSaturationState :: Type -> (Type -> Type) -> Type
newtype ContextSaturationState capability f = ContextSaturationState
  { cssQueryRegistry :: SaturationMatch.QueryRegistry (QueryPlan capability f)
  }

type SaturatingContextEGraph :: Type -> Type -> (Type -> Type) -> Type -> Type -> Type
data SaturatingContextEGraph owner capability f a c = SaturatingContextEGraph
  { sceContextGraph :: !(ContextEGraph owner f a c),
    sceSaturationState :: !(ContextSaturationState capability f)
  }

type SaturatingProofEGraph :: Type -> Type -> (Type -> Type) -> Type -> Type -> Type -> Type
type SaturatingProofEGraph owner capability f a c p =
  ProofGraph (SaturatingContextEGraph owner capability f a c) f c p

emptyContextSaturationState ::
  ContextSaturationState capability f
emptyContextSaturationState =
  ContextSaturationState
    { cssQueryRegistry = SaturationMatch.emptyQueryRegistry
    }
{-# INLINE emptyContextSaturationState #-}

emptySaturatingContextEGraph ::
  ContextEGraph owner f a c ->
  SaturatingContextEGraph owner capability f a c
emptySaturatingContextEGraph contextGraph =
  SaturatingContextEGraph
    { sceContextGraph = contextGraph,
      sceSaturationState = emptyContextSaturationState
    }
{-# INLINE emptySaturatingContextEGraph #-}

emptySaturatingProofEGraph ::
  ContextEGraph owner f a c ->
  SaturatingProofEGraph owner capability f a c p
emptySaturatingProofEGraph =
  emptyProofGraph . emptySaturatingContextEGraph
{-# INLINE emptySaturatingProofEGraph #-}

emptySaturatingProofEGraphWithRetention ::
  ProofRetention ->
  ContextEGraph owner f a c ->
  SaturatingProofEGraph owner capability f a c p
emptySaturatingProofEGraphWithRetention retention =
  emptyProofGraphWithRetention retention . emptySaturatingContextEGraph
{-# INLINE emptySaturatingProofEGraphWithRetention #-}

mapSaturatingContextGraph ::
  (ContextEGraph owner f a c -> ContextEGraph owner f a c) ->
  SaturatingContextEGraph owner capability f a c ->
  SaturatingContextEGraph owner capability f a c
mapSaturatingContextGraph updateGraph saturatingGraph =
  saturatingGraph
    { sceContextGraph = updateGraph (sceContextGraph saturatingGraph)
    }
{-# INLINE mapSaturatingContextGraph #-}

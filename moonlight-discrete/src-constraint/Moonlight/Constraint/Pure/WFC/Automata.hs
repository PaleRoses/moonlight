{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module Moonlight.Constraint.Pure.WFC.Automata
  ( EdgeF (..),
    edgeAutomaton,
    transitionCompatible,
    automatonAdjacencyPolicy,
    automatonAdjacencyPolicyWith,
    automatonAdjacencyRule,
    automatonAdjacencyRuleWith,
  )
where

import Data.Kind (Type)
import Moonlight.Constraint.Pure.WFC.Types
  ( AdjacencyPolicy (..),
    AdjacencyRule (..),
    SlotId,
  )
import Moonlight.Automata.Pure.Core
  ( TopDownTA (..),
  )

type EdgeF :: Type -> Type
newtype EdgeF a = EdgeF
  { unEdgeF :: a
  }
  deriving stock (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

edgeAutomaton :: (state -> state) -> TopDownTA EdgeF state
edgeAutomaton transition =
  TopDownTA
    ( \sourceState (EdgeF child) ->
        EdgeF (transition sourceState, child)
    )

transitionCompatible :: Eq state => TopDownTA EdgeF state -> state -> state -> Bool
transitionCompatible (TopDownTA transition) sourceState targetState =
  case transition sourceState (EdgeF ()) of
    EdgeF (expectedTargetState, ()) ->
      expectedTargetState == targetState

automatonAdjacencyPolicy ::
  Eq state =>
  TopDownTA EdgeF state ->
  (SlotId slot -> value -> state) ->
  AdjacencyPolicy slot value
automatonAdjacencyPolicy automaton =
  automatonAdjacencyPolicyWith (\_ _ -> automaton)

automatonAdjacencyPolicyWith ::
  Eq state =>
  (SlotId slot -> SlotId slot -> TopDownTA EdgeF state) ->
  (SlotId slot -> value -> state) ->
  AdjacencyPolicy slot value
automatonAdjacencyPolicyWith automatonFor classify =
  AdjacencyPolicy
    ( \sourceSlot targetSlot sourceValue targetValue ->
        transitionCompatible
          (automatonFor sourceSlot targetSlot)
          (classify sourceSlot sourceValue)
          (classify targetSlot targetValue)
    )

automatonAdjacencyRule ::
  Eq state =>
  SlotId slot ->
  SlotId slot ->
  TopDownTA EdgeF state ->
  (SlotId slot -> value -> state) ->
  AdjacencyRule slot value
automatonAdjacencyRule sourceSlot targetSlot automaton =
  automatonAdjacencyRuleWith sourceSlot targetSlot (\_ _ -> automaton)

automatonAdjacencyRuleWith ::
  Eq state =>
  SlotId slot ->
  SlotId slot ->
  (SlotId slot -> SlotId slot -> TopDownTA EdgeF state) ->
  (SlotId slot -> value -> state) ->
  AdjacencyRule slot value
automatonAdjacencyRuleWith sourceSlot targetSlot automatonFor classify =
  AdjacencyRule
    { adjacencyRuleSource = sourceSlot,
      adjacencyRuleTarget = targetSlot,
      adjacencyRuleCompatible =
        \sourceValue targetValue ->
          transitionCompatible
            (automatonFor sourceSlot targetSlot)
            (classify sourceSlot sourceValue)
            (classify targetSlot targetValue)
    }

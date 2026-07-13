{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Sheaf.Twist.FactClosure
  ( CompiledSupportedFactRule (..),
    SupportFactClosure (..),
    emptySupportFactClosure,
    LiftedSupportClosure (..),
    compiledSupportedFactActivationIndex,
    deriveSupportedFactClosureWith,
    deriveSupportedFactClosureWithState,
  )
where

import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

type CompiledSupportedFactRule :: Type -> Type -> Type
data CompiledSupportedFactRule support rule = CompiledSupportedFactRule
  { csfrSupport :: !support,
    csfrRule :: !rule
  }
  deriving stock (Eq, Ord, Show)

type SupportFactClosure :: Type -> Type -> Type
data SupportFactClosure facts derivations = SupportFactClosure
  { sfcFacts :: !facts,
    sfcDerivations :: !derivations
  }
  deriving stock (Eq, Ord, Show)

emptySupportFactClosure ::
  (Monoid facts, Monoid derivations) =>
  SupportFactClosure facts derivations
emptySupportFactClosure =
  SupportFactClosure mempty mempty

type LiftedSupportClosure :: Type -> Type -> Type -> Type -> Type -> Type
data LiftedSupportClosure support facts derivations liftedFacts liftedDerivations =
  LiftedSupportClosure
    { lscEmptyFacts :: !liftedFacts,
      lscEmptyDerivations :: !liftedDerivations,
      lscLiftFacts :: !(support -> facts -> liftedFacts),
      lscLiftDerivations :: !(support -> derivations -> liftedDerivations),
      lscMergeFacts :: !(liftedFacts -> liftedFacts -> liftedFacts),
      lscMergeDerivations :: !(liftedDerivations -> liftedDerivations -> liftedDerivations)
    }

deriveSupportedFactClosureWith ::
  (Foldable contexts, Ord ctx) =>
  LiftedSupportClosure support facts derivations liftedFacts liftedDerivations ->
  contexts ctx ->
  (support -> ctx -> Bool) ->
  (ctx -> support) ->
  (ctx -> local) ->
  ([rule] -> local -> SupportFactClosure facts derivations) ->
  [CompiledSupportedFactRule support rule] ->
  SupportFactClosure liftedFacts liftedDerivations
deriveSupportedFactClosureWith liftedClosure contexts supportContains principalSupport localize deriveLocal compiledRules =
  snd
    ( deriveSupportedFactClosureWithState
        liftedClosure
        contexts
        supportContains
        principalSupport
        localize
        ()
        (\stateValue rules localValue -> (stateValue, deriveLocal rules localValue))
        compiledRules
    )

deriveSupportedFactClosureWithState ::
  (Foldable contexts, Ord ctx) =>
  LiftedSupportClosure support facts derivations liftedFacts liftedDerivations ->
  contexts ctx ->
  (support -> ctx -> Bool) ->
  (ctx -> support) ->
  (ctx -> local) ->
  state ->
  (state -> [rule] -> local -> (state, SupportFactClosure facts derivations)) ->
  [CompiledSupportedFactRule support rule] ->
  (state, SupportFactClosure liftedFacts liftedDerivations)
deriveSupportedFactClosureWithState liftedClosure contexts supportContains principalSupport localize initialState deriveLocal compiledRules =
  let activationIndex =
        compiledSupportedFactActivationIndex contexts supportContains compiledRules
      step (currentState, accumulatedClosure) contextValue =
        let applicableRules =
              Map.findWithDefault [] contextValue activationIndex
            (nextState, localClosure) =
              deriveLocal currentState applicableRules (localize contextValue)
            supportValue = principalSupport contextValue
            liftedFacts =
              lscLiftFacts liftedClosure supportValue (sfcFacts localClosure)
            liftedDerivations =
              lscLiftDerivations liftedClosure supportValue (sfcDerivations localClosure)
         in ( nextState,
              SupportFactClosure
                { sfcFacts =
                    lscMergeFacts liftedClosure (sfcFacts accumulatedClosure) liftedFacts,
                  sfcDerivations =
                    lscMergeDerivations liftedClosure (sfcDerivations accumulatedClosure) liftedDerivations
                }
            )
   in foldl'
        step
        ( initialState,
          SupportFactClosure
            { sfcFacts = lscEmptyFacts liftedClosure,
              sfcDerivations = lscEmptyDerivations liftedClosure
            }
        )
        contexts

compiledSupportedFactActivationIndex ::
  (Foldable contexts, Ord ctx) =>
  contexts ctx ->
  (support -> ctx -> Bool) ->
  [CompiledSupportedFactRule support rule] ->
  Map ctx [rule]
compiledSupportedFactActivationIndex contexts supportContains compiledRules =
  Map.fromList
    [ ( contextValue,
        [ csfrRule compiledRule
        | compiledRule <- compiledRules,
          supportContains (csfrSupport compiledRule) contextValue
        ]
      )
    | contextValue <- foldr (:) [] contexts
    ]

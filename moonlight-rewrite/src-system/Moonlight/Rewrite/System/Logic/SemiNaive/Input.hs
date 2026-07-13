{-# LANGUAGE DerivingStrategies #-}

-- | Round input snapshot for semi-naive closure.
-- Owns the separation between total facts/derivations and current deltas,
-- plus the monotone round index.
-- Contract: initial input treats seeds as the first delta, and advance
-- replaces total and delta sections explicitly.
module Moonlight.Rewrite.System.Logic.SemiNaive.Input
  ( SemiNaiveInput (..),
    initialSemiNaiveInput,
    advanceSemiNaiveInput,
  )
where

import Data.Kind (Type)

type SemiNaiveInput :: Type -> Type -> Type
data SemiNaiveInput facts derivations = SemiNaiveInput
  { sniAllFacts :: !facts,
    sniDeltaFacts :: !facts,
    sniAllDerivations :: !derivations,
    sniDeltaDerivations :: !derivations,
    sniRoundIndex :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Show, Read)

initialSemiNaiveInput ::
  facts ->
  derivations ->
  SemiNaiveInput facts derivations
initialSemiNaiveInput initialFacts initialDerivations =
  SemiNaiveInput
    { sniAllFacts = initialFacts,
      sniDeltaFacts = initialFacts,
      sniAllDerivations = initialDerivations,
      sniDeltaDerivations = initialDerivations,
      sniRoundIndex = 0
    }

advanceSemiNaiveInput ::
  facts ->
  facts ->
  derivations ->
  derivations ->
  SemiNaiveInput facts derivations ->
  SemiNaiveInput facts derivations
advanceSemiNaiveInput nextFacts deltaFacts nextDerivations deltaDerivations input =
  SemiNaiveInput
    { sniAllFacts = nextFacts,
      sniDeltaFacts = deltaFacts,
      sniAllDerivations = nextDerivations,
      sniDeltaDerivations = deltaDerivations,
      sniRoundIndex = sniRoundIndex input + 1
    }

{-# LANGUAGE BangPatterns #-}

module Moonlight.Derived.Pure.Functor.ExceptionalPushforward
  ( exceptionalPushforward
  ) where

import Moonlight.Core (MoonlightError)
import Moonlight.Derived.Pure.Failure (DerivedFailure (..), derivedFailureToMoonlightError)
import Moonlight.Derived.Pure.Functor.Presentation.Internal
  ( prepareVerdierSite
  , pushforwardPresentation
  , verdierDualPresentationPrepared
  )
import Moonlight.Derived.Pure.Gluing.Peeling (minimizeComplex)
import Moonlight.Derived.Pure.Site.InjectiveComplex
  ( Derived (..)
  , mkNormalizedDerivedTrusted
  , trustLawfulInjectiveComplex
  )
import Moonlight.Derived.Pure.Site.Poset
  ( applyDerivedPosetFunctor
  , DerivedPosetFunctor
  , derivedPosetFunctorSource
  , derivedPosetFunctorTarget
  , FinObjectId
  )
import Moonlight.LinAlg (GF2)

exceptionalPushforward ::
  DerivedPosetFunctor ->
  Derived GF2 ->
  Either MoonlightError (Derived GF2)
exceptionalPushforward !functorValue derivedValue@Derived {getDerived = sourceComplex} = do
  let sourcePoset = derivedPosetFunctorSource functorValue
      targetPoset = derivedPosetFunctorTarget functorValue
  if derivedPoset derivedValue /= sourcePoset
    then Left (derivedFailureToMoonlightError DerivedFunctorSiteMismatch)
    else do
      let preparedSource = prepareVerdierSite sourcePoset
          preparedTarget = prepareVerdierSite targetPoset
          dualSource = verdierDualPresentationPrepared preparedSource sourceComplex
      pushedDual <- pushforwardPresentation targetPoset (firstDerived . applyDerivedPosetFunctor functorValue) dualSource
      let exceptionalPresentation = verdierDualPresentationPrepared preparedTarget pushedDual
      minimizedExceptional <- minimizeComplex exceptionalPresentation
      pure
        ( mkNormalizedDerivedTrusted
            targetPoset
            (trustLawfulInjectiveComplex minimizedExceptional)
        )
  where
    firstDerived :: Either DerivedFailure FinObjectId -> Either MoonlightError FinObjectId
    firstDerived = either (Left . derivedFailureToMoonlightError) Right
{-# INLINE exceptionalPushforward #-}

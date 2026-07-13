module Moonlight.Derived.Pure.Functor.Pushforward
  ( pushforward
  ) where

import Moonlight.Core (Field, MoonlightError)
import Moonlight.Algebra (IntegralDomain)
import Moonlight.Derived.Pure.Functor.Presentation.Internal (pushforwardPresentation)
import Moonlight.Derived.Pure.Failure (DerivedFailure (..), derivedFailureToMoonlightError)
import Moonlight.Derived.Pure.Site.Poset
  ( applyDerivedPosetFunctor
  , DerivedPosetFunctor
  , derivedPosetFunctorSource
  , derivedPosetFunctorTarget
  )
import Moonlight.Derived.Pure.Site.InjectiveComplex
  ( Derived (..)
  , mkNormalizedDerivedTrusted
  , trustLawfulInjectiveComplex
  )
import Moonlight.Derived.Pure.Gluing.Peeling (minimizeComplex)

pushforward ::
  (Field a, IntegralDomain a, Num a) =>
  DerivedPosetFunctor -> Derived a -> Either MoonlightError (Derived a)
pushforward functorValue derivedValue@Derived {getDerived = injectiveComplex} = do
  let sourcePoset = derivedPosetFunctorSource functorValue
      targetPoset = derivedPosetFunctorTarget functorValue
  if derivedPoset derivedValue /= sourcePoset
    then Left (derivedFailureToMoonlightError DerivedFunctorSiteMismatch)
    else do
      relabeledComplex <- pushforwardPresentation targetPoset (firstDerived . applyDerivedPosetFunctor functorValue) injectiveComplex
      minimizedComplex <- minimizeComplex relabeledComplex
      pure
        ( mkNormalizedDerivedTrusted
            targetPoset
            (trustLawfulInjectiveComplex minimizedComplex)
        )
  where
    firstDerived = either (Left . derivedFailureToMoonlightError) Right

module Moonlight.Derived.Pure.Functor.ExceptionalPullback
  ( exceptionalPullback
  ) where

import Moonlight.Core (MoonlightError)
import Moonlight.Derived.Pure.Functor.Pullback (pullback)
import Moonlight.Derived.Pure.Functor.VerdierDual (verdierDualComplex)
import Moonlight.Derived.Pure.Site.InjectiveComplex (Derived)
import Moonlight.Derived.Pure.Site.Poset (DerivedPosetFunctor)
import Moonlight.LinAlg (GF2)

exceptionalPullback ::
  DerivedPosetFunctor ->
  Derived GF2 ->
  Either MoonlightError (Derived GF2)
exceptionalPullback functorValue targetComplex = do
  dualTarget <- verdierDualComplex targetComplex
  pulledDual <- pullback functorValue dualTarget
  verdierDualComplex pulledDual

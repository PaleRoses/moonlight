{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Derived.Pure.Pruning.VerdierGate
  ( PreparedVerdierPruning
  , preparedVerdierPrimal
  , preparedVerdierDual
  , VerdierPreparation (..)
  , prepareVerdierPruning
  , verdierGate
  , verdierLocalClosedGate
  ) where

import Data.IntSet qualified as IS
import Data.Bifunctor (first)
import Data.Kind (Type)
import Moonlight.Core (MoonlightError)
import Moonlight.Derived.Pure.Functor.ProperPullback
  ( prepareProperPullback
  , properPullback
  )
import Moonlight.Derived.Pure.Functor.VerdierDual (verdierDualComplex)
import Moonlight.Derived.Pure.Morse.Hypercohomology (hypercohomologyVanishes)
import Moonlight.Derived.Pure.Site.Gorenstein (isGorensteinStar)
import Moonlight.Derived.Pure.Site.InjectiveComplex
  ( Derived
  , derivedPoset
  )
import Moonlight.Derived.Pure.Site.Microsupport
  ( LocalClosed
  , localClosedNodes
  , localClosedPoset
  )
import Moonlight.Derived.Pure.Failure
  ( DerivedFailure (DerivedFunctorSiteMismatch)
  , derivedFailureToMoonlightError
  )
import Moonlight.LinAlg (GF2)

type PreparedVerdierPruning :: Type
data PreparedVerdierPruning = PreparedVerdierPruning
  { vgpPrimalComplex :: Derived GF2
  , vgpDualComplex :: Derived GF2
  }
  deriving stock (Eq, Show)

preparedVerdierPrimal :: PreparedVerdierPruning -> Derived GF2
preparedVerdierPrimal = vgpPrimalComplex

preparedVerdierDual :: PreparedVerdierPruning -> Derived GF2
preparedVerdierDual = vgpDualComplex

type VerdierPreparation :: Type
data VerdierPreparation
  = VerdierNotApplicable
  | VerdierPrepared !PreparedVerdierPruning
  deriving stock (Eq, Show)

prepareVerdierPruning :: Derived GF2 -> Either MoonlightError VerdierPreparation
prepareVerdierPruning primalComplex = do
  gorensteinValue <- first derivedFailureToMoonlightError (isGorensteinStar posetValue)
  if gorensteinValue
    then do
      dualComplex <- verdierDualComplex primalComplex
      pure
        ( VerdierPrepared
            PreparedVerdierPruning
              { vgpPrimalComplex = primalComplex
              , vgpDualComplex = dualComplex
              }
        )
    else Right VerdierNotApplicable
  where
    posetValue = derivedPoset primalComplex

verdierLocalClosedGate :: VerdierPreparation -> LocalClosed -> Either MoonlightError Bool
verdierLocalClosedGate VerdierNotApplicable _ = Right True
verdierLocalClosedGate (VerdierPrepared preparedPruning) regionValue
  | not
      ( localClosedPoset regionValue
          == derivedPoset (vgpPrimalComplex preparedPruning)
      ) =
      Left (derivedFailureToMoonlightError DerivedFunctorSiteMismatch)
  | IS.null (localClosedNodes regionValue) = Right True
  | otherwise =
      do
        preparedPrimal <-
          first derivedFailureToMoonlightError
            (prepareProperPullback regionValue (vgpPrimalComplex preparedPruning))
        preparedDual <-
          first derivedFailureToMoonlightError
            (prepareProperPullback regionValue (vgpDualComplex preparedPruning))
        let primalRestricted = properPullback preparedPrimal
            dualRestricted = properPullback preparedDual
        primalNontrivial <-
          fmap not
            (hypercohomologyVanishes primalRestricted)
        dualNontrivial <-
          fmap not
            (hypercohomologyVanishes dualRestricted)
        Right (primalNontrivial && dualNontrivial)

verdierGate ::
  (seed -> LocalClosed) ->
  VerdierPreparation ->
  seed ->
  Either MoonlightError Bool
verdierGate projectRegion preparedPruning =
  verdierLocalClosedGate preparedPruning . projectRegion

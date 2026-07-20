{-# LANGUAGE NamedFieldPuns #-}

module Moonlight.Derived.Pure.Functor.VerdierDual
  ( verdierDualComplex
  , dualizingComplex
  ) where

import Data.Vector qualified as V
import Moonlight.Core (MoonlightError)
import Moonlight.Derived.Pure.Functor.Presentation.Internal
  ( PreparedVerdierSite (..)
  , prepareVerdierSite
  , verdierDualPresentationPrepared
  )
import Moonlight.Derived.Pure.Site.InjectiveComplex
  ( Derived (..)
  , InjectiveComplex (..)
  , mkNormalizedDerivedTrusted
  , trustLawfulInjectiveComplex
  )
import Moonlight.Derived.Pure.Site.LabeledMatrix
  ( emptyAxis
  , fromLabels
  , zeroBlocked
  )
import Moonlight.Derived.Pure.Site.Poset
  ( DerivedPoset (..)
  )
import Moonlight.LinAlg (GF2)

-- | Dualize against the direct sum of all indecomposable injectives placed in
-- the site's single topological degree. This is the Verdier model for finite
-- pure Gorenstein-star posets; arbitrary finite posets require a site-specific
-- dualizing resolution.
verdierDualComplex :: Derived GF2 -> Either MoonlightError (Derived GF2)
verdierDualComplex derivedValue@Derived {getDerived = injectiveComplex} =
  let preparedSite = prepareVerdierSite posetValue
      rawDual = verdierDualPresentationPrepared preparedSite injectiveComplex
   in Right
        ( mkNormalizedDerivedTrusted
            posetValue
            (trustLawfulInjectiveComplex rawDual)
        )
  where
    posetValue = derivedPoset derivedValue

-- | The direct sum of one injective at every site object, concentrated in
-- 'pvsTopologicalDimension'. This is the Verdier dualizing complex for finite
-- pure Gorenstein-star posets, not for arbitrary finite posets.
dualizingComplex :: DerivedPoset -> Either MoonlightError (Derived GF2)
dualizingComplex posetValue =
  let preparedSite = prepareVerdierSite posetValue
      axis = fromLabels (derivedPosetTopoAsc posetValue)
      dualComplex =
        InjectiveComplex
          { icStart = pvsTopologicalDimension preparedSite
          , icDiffs = V.singleton (zeroBlocked emptyAxis axis)
          }
   in Right
        ( mkNormalizedDerivedTrusted
            posetValue
            (trustLawfulInjectiveComplex dualComplex)
        )

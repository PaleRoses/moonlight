module Moonlight.Derived.Morse
  ( -- | Resolve, minimize, normalize, and compute diagonal microsupport for one differential.
    microsupportOfDifferential
  , hypercohomologyDimsWith
  , hypercohomologyDims
  , hypercohomologyVanishesWith
  , hypercohomologyVanishes
  , hypercohomologyReducedVanishesWith
  , hypercohomologyReducedVanishes
  , fiberSubsetAt
  , fiberSubsets
  , microSupportBangOnWith
  , microSupportBangOn
  , MicrosupportResult (..)
  , PreparedMicrosupport
  , prepareMicrosupport
  , preparedMicrosupportPullbacks
  , computeMicrosupportWith
  , computeMicrosupport
  , PreparedPosetCechResolution
  , preparePosetCechResolution
  , preparedPosetCechComplex
  , preparedPosetSheafCohomology
  , preparedPosetSheafCohomologyDims
  , posetCechComplex
  , posetSheafCohomology
  , posetSheafCohomologyDims
  ) where

import Data.Bifunctor (first)
import Data.Vector qualified as V
import Moonlight.Algebra (IntegralDomain)
import Moonlight.Core (Field, MoonlightError)
import Moonlight.Derived.Failure (derivedFailureToMoonlightError)
import Moonlight.Derived.Pure.Gluing.Peeling (minimizeComposableComplex)
import Moonlight.Derived.Pure.Gluing.Resolution (resolutionStep)
import Moonlight.Derived.Pure.Site.InjectiveComplex
  ( mkComposableInjectiveComplex
  , mkNormalizedDerivedFromComposableChecked
  )
import Moonlight.Derived.Pure.Site.LabeledMatrix (BlockedMat)
import Moonlight.Derived.Pure.Site.Poset
  ( DerivedPoset
  , identityDerivedPosetFunctor
  )
import Moonlight.Derived.Pure.Cohomology.Poset
import Moonlight.Derived.Pure.Morse.Hypercohomology
import Moonlight.Derived.Pure.Morse.Support
import Moonlight.Derived.Pure.Pipeline
import Moonlight.LinAlg.Dense.Field (DenseRankBackend)

microsupportOfDifferential ::
  (Eq a, Field a, Num a, IntegralDomain a, DenseRankBackend a) =>
  DerivedPoset ->
  BlockedMat a ->
  Either MoonlightError MicrosupportResult
microsupportOfDifferential posetValue initialDifferential = do
  resolvedStep <- resolutionStep posetValue initialDifferential
  composableComplex <-
    first derivedFailureToMoonlightError
      (mkComposableInjectiveComplex 0 (V.fromList [initialDifferential, resolvedStep]))
  resolvedComplex <- minimizeComposableComplex composableComplex
  derivedValue <-
    first derivedFailureToMoonlightError
      (mkNormalizedDerivedFromComposableChecked posetValue resolvedComplex)
  preparedMicrosupport <-
    first derivedFailureToMoonlightError
      (prepareMicrosupport (identityDerivedPosetFunctor posetValue) derivedValue)
  computeMicrosupport preparedMicrosupport

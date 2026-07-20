module Moonlight.Derived.Complex
  ( Degree
  , InjectiveComplex
  , Derived
  , derivedPoset
  , injectiveComplexStart
  , injectiveComplexDiffs
  , derivedInjectiveComplex
  , isMinimal
  , firstNonMinimal
  , allDiagLabels
  , initialObjectAxis
  , complexObjectAxes
  , composesToZero
  , hasCompatibleObjectAxes
  , adjacentDifferentials
  , normalizeBoundaryPresentation
  , canonicalizeComplexAxes
  , normalizeComplexPresentation
  , ComposableInjectiveComplex
  , mkComposableInjectiveComplex
  , minimizeComposableComplex
  , mkNormalizedDerivedFromComposableChecked
  ) where

import Moonlight.Derived.Pure.Gluing.Peeling (minimizeComposableComplex)
import Moonlight.Derived.Pure.Site.InjectiveComplex

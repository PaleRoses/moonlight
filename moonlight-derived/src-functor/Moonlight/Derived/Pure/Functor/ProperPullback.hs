-- | Restriction to locally closed subposets.
--
-- The fast path accepts only a 'LocallyRestrictableDerived'.  Its differential
-- blocks have one uniform order variance, while 'LocalClosed' supplies a convex
-- node set.  Hence every intermediate node of a nonzero adjacent composite is
-- retained whenever both endpoints are retained, so restriction preserves
-- @d ² = 0@ without re-running the matrix law.  Arbitrary formal complexes do
-- not have that proof; they must use 'restrictFormalDerivedChecked'.
module Moonlight.Derived.Pure.Functor.ProperPullback
  ( PreparedProperPullback
  , prepareProperPullback
  , preparedProperPullbackSupport
  , ProperPullbackResult (..)
  , properPullback
  , properPullbackFamily
  ) where

import qualified Data.Vector as V
import Data.Kind (Type)
import Moonlight.Derived.Pure.Site.LabeledMatrix (restrictBlocked)
import Moonlight.Derived.Pure.Site.InjectiveComplex
  ( Derived (..)
  , InjectiveComplex (..)
  , mkDerivedTrusted
  , normalizeBoundaryPresentation
  , trustLawfulInjectiveComplex
  )
import Moonlight.Derived.Pure.Site.Microsupport
  ( LocalClosed
  , localClosedNodes
  , localClosedPoset
  )
import Moonlight.Derived.Pure.Failure (DerivedFailure (..))

type PreparedProperPullback :: Type -> Type
data PreparedProperPullback a = PreparedProperPullback
  { pppSupport :: !LocalClosed
  , pppSource :: !(Derived a)
  }
  deriving stock (Show)

prepareProperPullback ::
  LocalClosed ->
  Derived a ->
  Either DerivedFailure (PreparedProperPullback a)
prepareProperPullback supportValue sourceValue
  | localClosedPoset supportValue == derivedPoset sourceValue =
      Right (PreparedProperPullback supportValue sourceValue)
  | otherwise =
      Left DerivedFunctorSiteMismatch

preparedProperPullbackSupport :: PreparedProperPullback a -> LocalClosed
preparedProperPullbackSupport = pppSupport
{-# INLINE preparedProperPullbackSupport #-}

type ProperPullbackResult :: Type -> Type
data ProperPullbackResult a = ProperPullbackResult
  { pprSupport :: !LocalClosed
  , pprDerived :: !(Derived a)
  }

properPullback :: PreparedProperPullback a -> Derived a
properPullback PreparedProperPullback
  { pppSupport
  , pppSource = derivedValue
  } =
    mkDerivedTrusted
      (derivedPoset derivedValue)
      (trustLawfulInjectiveComplex (restrictedComplex pppSupport derivedValue))
{-# INLINE properPullback #-}

properPullbackFamily :: [PreparedProperPullback a] -> [ProperPullbackResult a]
properPullbackFamily =
  fmap
    ( \preparedValue@PreparedProperPullback {pppSupport} ->
        ProperPullbackResult
          { pprSupport = pppSupport
          , pprDerived = properPullback preparedValue
          }
    )

restrictedComplex :: LocalClosed -> Derived a -> InjectiveComplex a
restrictedComplex supportValue Derived {getDerived = complexValue@InjectiveComplex {icDiffs = differentialValues}} =
  normalizeBoundaryPresentation
    ( complexValue
        { icDiffs =
            V.map (restrictBlocked (localClosedNodes supportValue)) differentialValues
        }
    )
{-# INLINE restrictedComplex #-}

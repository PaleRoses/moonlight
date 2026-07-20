{-# LANGUAGE BangPatterns #-}

module Moonlight.Derived.Pure.Functor.ProperPushforward
  ( properPushforward
  ) where

import qualified Data.IntSet as IS
import qualified Data.Vector as V
import Moonlight.Core (Field, MoonlightError)
import Moonlight.Algebra (IntegralDomain)
import Moonlight.Derived.Pure.Failure
  ( DerivedFailure (DerivedFunctorSiteMismatch)
  , derivedFailureToMoonlightError
  )
import Moonlight.Derived.Pure.Site.Poset
import Moonlight.Derived.Pure.Site.LabeledMatrix
import Moonlight.Derived.Pure.Site.InjectiveComplex
import Moonlight.Derived.Pure.Gluing.Resolution (resolveLoop)
import Moonlight.Derived.Pure.Gluing.Peeling (minimizeComplex)
import Moonlight.Derived.Pure.Functor.ClosedSupport.Geometry
  ( ClosedSupport
  , closedSupportNodes
  , closedSupportPoset
  )

properPushforward ::
  (Field a, IntegralDomain a, Num a) =>
  ClosedSupport -> Derived a -> Either MoonlightError (Derived a)
properPushforward supportValue derivedValue@(Derived ambient injectiveComplex@InjectiveComplex{icStart, icDiffs}) =
  if closedSupportPoset supportValue /= ambient
    then Left (derivedFailureToMoonlightError DerivedFunctorSiteMismatch)
    else
      maybe
        (Right derivedValue)
        ( \initialAxis ->
            let extendSet = closureOf ambient nodeSet `IS.difference` nodeSet
             in if IS.null extendSet && isMinimal injectiveComplex
                  then
                    Right
                      ( mkNormalizedDerivedTrusted
                          ambient
                          (trustLawfulInjectiveComplex injectiveComplex)
                      )
                  else do
                    let extendDesc = [ objectValue | objectValue <- V.toList (derivedPosetTopoDesc ambient), IS.member (unFinObjectId objectValue) extendSet ]
                    outputDiffs <-
                      resolveLoop
                        ambient
                        extendDesc
                        (\cols mNext -> copyRowsInto cols mNext)
                        id
                        initialAxis
                        icDiffs
                    minimizedComplex <- minimizeComplex (InjectiveComplex icStart outputDiffs)
                    pure
                      ( mkNormalizedDerivedTrusted
                          ambient
                          (trustLawfulInjectiveComplex minimizedComplex)
                      )
        )
        (initialObjectAxis injectiveComplex)
  where
    nodeSet = closedSupportNodes supportValue

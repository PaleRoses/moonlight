{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Derived.Pure.Pipeline
  ( MicrosupportResult (..)
  , PreparedMicrosupport
  , prepareMicrosupport
  , preparedMicrosupportPullbacks
  , computeMicrosupportWith
  , computeMicrosupport
  ) where

import Data.Kind (Type)
import Data.Maybe (catMaybes)
import qualified Data.IntSet as IS
import qualified Data.Set as Set
import qualified Data.Vector as V
import Moonlight.Core (Field, MoonlightError)
import Moonlight.LinAlg.Dense.Field (DenseRankBackend)
import Moonlight.Derived.Pure.LinAlg.Interpreter (fieldRankBackend)
import Moonlight.Derived.Pure.LinAlg.Rank (RankBackend)
import Moonlight.Derived.Pure.Site.Poset
  ( DerivedPoset (..)
  , DerivedPosetFunctor
  , FinObjectId
  , derivedPosetFunctorSource
  , derivedPosetFunctorTarget
  )
import Moonlight.Derived.Pure.Site.Microsupport
  ( LocalClosed
  , Criticality (..)
  , localClosedNodes
  )
import Moonlight.Derived.Pure.Site.InjectiveComplex
  ( Derived (..)
  )
import Moonlight.Derived.Pure.Functor.ProperPullback
  ( PreparedProperPullback
  , prepareProperPullback
  , preparedProperPullbackSupport
  )
import Moonlight.Derived.Pure.Morse.Support
  ( fiberSubsetAt
  , microSupportBangOnWith
  )
import Moonlight.Derived.Pure.Failure (DerivedFailure (..))

type MicrosupportResult :: Type
data MicrosupportResult = MicrosupportResult
  { mrMicrosupport     :: [LocalClosed]
  , mrCriticalFibers   :: [(FinObjectId, Criticality)]
  , mrCriticalCount    :: Int
  , mrNoncriticalCount :: Int
  } deriving stock (Eq, Show)

type PreparedMicrosupport :: Type -> Type
-- | Target-indexed fibers paired with their already proved local restrictions;
-- 'Nothing' retains an empty target fiber for global classification.
newtype PreparedMicrosupport a = PreparedMicrosupport
  [(FinObjectId, Maybe (PreparedProperPullback a))]
  deriving stock (Show)

prepareMicrosupport ::
  DerivedPosetFunctor ->
  Derived a ->
  Either DerivedFailure (PreparedMicrosupport a)
prepareMicrosupport functorValue sourceValue = do
  if src /= derivedPoset sourceValue
    then Left DerivedFunctorSiteMismatch
    else
      fmap PreparedMicrosupport
        (traverse prepareFiber (V.toList (derivedPosetNodes tgt)))
  where
    prepareFiber targetNode = do
      supportValue <- fiberSubsetAt functorValue targetNode
      preparedValue <-
        if IS.null (localClosedNodes supportValue)
          then Right Nothing
          else fmap Just (prepareProperPullback supportValue sourceValue)
      Right (targetNode, preparedValue)
    src = derivedPosetFunctorSource functorValue
    tgt = derivedPosetFunctorTarget functorValue
preparedMicrosupportPullbacks :: PreparedMicrosupport a -> [PreparedProperPullback a]
preparedMicrosupportPullbacks (PreparedMicrosupport fibers) =
  catMaybes (fmap snd fibers)

computeMicrosupport ::
  (Eq a, Field a, Num a, DenseRankBackend a) =>
  PreparedMicrosupport a ->
  Either MoonlightError MicrosupportResult
computeMicrosupport =
  computeMicrosupportWith fieldRankBackend

computeMicrosupportWith ::
  (Eq a, Num a) =>
  RankBackend a ->
  PreparedMicrosupport a ->
  Either MoonlightError MicrosupportResult
computeMicrosupportWith rankBackend preparedValue@(PreparedMicrosupport fibers) = do
  microsupport <- microSupportBangOnWith rankBackend (preparedMicrosupportPullbacks preparedValue)
  let criticalSupports = Set.fromList (fmap localClosedNodes microsupport)
      classified =
        fmap
          ( \(targetNode, preparedFiber) ->
              ( targetNode
              , case preparedFiber of
                  Just pullback
                    | Set.member
                        (localClosedNodes (preparedProperPullbackSupport pullback))
                        criticalSupports -> Critical
                  _ -> NonCritical
              )
          )
          fibers
  let criticalCount = length [ () | (_, criticalityValue) <- classified, criticalityValue == Critical ]
  pure
    MicrosupportResult
      { mrMicrosupport = microsupport
      , mrCriticalFibers = classified
      , mrCriticalCount = criticalCount
      , mrNoncriticalCount = length classified - criticalCount
      }

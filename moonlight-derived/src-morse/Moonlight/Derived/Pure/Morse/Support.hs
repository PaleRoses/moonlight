module Moonlight.Derived.Pure.Morse.Support
  ( fiberSubsetAt
  , fiberSubsets
  , microSupportBangOnWith
  , microSupportBangOn
  ) where

import Data.Maybe (catMaybes)
import qualified Data.IntSet as IS
import qualified Data.Vector as V
import Moonlight.Core (Field, MoonlightError)
import Moonlight.LinAlg.Dense.Field (DenseRankBackend)
import Moonlight.Derived.Pure.LinAlg.Interpreter (fieldRankBackend)
import Moonlight.Derived.Pure.LinAlg.Rank
  ( RankBackend
  , precomputeStableSparseRankCache
  , stableSparseDigestRankBackend
  )
import Moonlight.Derived.Pure.Site.Poset (DerivedPoset (..), FinObjectId (..))
import Moonlight.Derived.Pure.Site.Microsupport
  ( LocalClosed
  , localClosedNodes
  , mkLocalClosed
  )
import Moonlight.Derived.Pure.Failure
  ( DerivedFailure
  )
import Moonlight.Derived.Pure.Site.InjectiveComplex
  ( Derived (..)
  , InjectiveComplex (..)
  )
import Moonlight.Derived.Pure.Site.LabeledMatrix (SparseMat, blockedToSparseMat)
import Moonlight.Derived.Pure.Functor.ProperPullback
  ( PreparedProperPullback
  , ProperPullbackResult (..)
  , properPullbackFamily
  )
import Moonlight.Derived.Pure.Morse.Hypercohomology (hypercohomologyVanishesWith)
import Moonlight.Derived.Pure.Site.Poset
  ( applyDerivedPosetFunctor
  , DerivedPosetFunctor
  , derivedPosetFunctorSource
  , derivedPosetFunctorTarget
  )

fiberSubsetAt :: DerivedPosetFunctor -> FinObjectId -> Either DerivedFailure LocalClosed
fiberSubsetAt functorValue nodeValue = do
  mappedNodes <- traverse (applyDerivedPosetFunctor functorValue) (V.toList (derivedPosetNodes src))
  mkLocalClosed
    src
    ( IS.fromList
        [ unFinObjectId sourceNode
        | (sourceNode, mappedNode) <- zip (V.toList (derivedPosetNodes src)) mappedNodes
        , mappedNode == nodeValue
        ]
    )
  where
    src = derivedPosetFunctorSource functorValue

fiberSubsets :: DerivedPosetFunctor -> Either DerivedFailure [LocalClosed]
fiberSubsets functorValue = do
  fmap
    (filter (not . IS.null . localClosedNodes))
    (traverse (fiberSubsetAt functorValue) (V.toList (derivedPosetNodes tgt)))
  where
    tgt = derivedPosetFunctorTarget functorValue

microSupportBangOn ::
  (Eq a, Field a, Num a, DenseRankBackend a) =>
  [PreparedProperPullback a] -> Either MoonlightError [LocalClosed]
microSupportBangOn =
  microSupportBangOnWith fieldRankBackend

microSupportBangOnWith ::
  (Eq a, Num a) =>
  RankBackend a ->
  [PreparedProperPullback a] ->
  Either MoonlightError [LocalClosed]
microSupportBangOnWith _ [] = Right []
microSupportBangOnWith rankBackend preparedPullbacks = do
  let pullbackFamily = properPullbackFamily preparedPullbacks
      sparseDifferentials =
        concatMap restrictedSparseDifferentials pullbackFamily
  rankCache <-
    precomputeStableSparseRankCache
      rankBackend
      sparseDifferentials
  let cachedRankBackend =
        stableSparseDigestRankBackend rankCache rankBackend
  fmap catMaybes
    ( traverse
        ( \ProperPullbackResult {pprSupport, pprDerived} ->
            fmap
              (\vanishes -> if vanishes then Nothing else Just pprSupport)
              ( hypercohomologyVanishesWith
                  cachedRankBackend
                  pprDerived
              )
        )
        pullbackFamily
    )
  where
    restrictedSparseDifferentials :: (Eq a, Num a) => ProperPullbackResult a -> [SparseMat a]
    restrictedSparseDifferentials ProperPullbackResult
      { pprDerived = Derived
          { getDerived = InjectiveComplex {icDiffs}
          }
      } =
      fmap blockedToSparseMat (V.toList icDiffs)

module Moonlight.Derived.Pure.Morse.Hypercohomology
  ( hypercohomologyDimsWith
  , hypercohomologyDims
  , hypercohomologyVanishesWith
  , hypercohomologyVanishes
  , hypercohomologyReducedVanishesWith
  , hypercohomologyReducedVanishes
  ) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector as V
import Moonlight.Core (Field, MoonlightError)
import Moonlight.LinAlg.Dense.Field (DenseRankBackend)
import Moonlight.Derived.Pure.Dimension (gradedKernelImageDims)
import Moonlight.Derived.Pure.Site.LabeledMatrix
  ( axisSize
  , blockedToSparseMat
  )
import Moonlight.Derived.Pure.Site.InjectiveComplex
  ( InjectiveComplex (..), Derived (..), complexObjectAxes
  )
import Moonlight.Derived.Pure.LinAlg.Interpreter (fieldRankBackend)
import Moonlight.Derived.Pure.LinAlg.Rank
  ( RankBackend
  , rankSparseWith
  )

hypercohomologyDims ::
  (Eq a, Field a, Num a, DenseRankBackend a) =>
  Derived a -> Either MoonlightError (IntMap Int)
hypercohomologyDims =
  hypercohomologyDimsWith fieldRankBackend

hypercohomologyDimsWith ::
  (Eq a, Num a) =>
  RankBackend a ->
  Derived a ->
  Either MoonlightError (IntMap Int)
hypercohomologyDimsWith rankBackend Derived {getDerived = injectiveComplex@InjectiveComplex{icStart, icDiffs}} = do
  let axes = complexObjectAxes injectiveComplex
      sparseDifferentials = map blockedToSparseMat (V.toList icDiffs)
      objectDimensions = fmap axisSize axes
  differentialRanks <- traverse (rankSparseWith rankBackend) sparseDifferentials
  pure (gradedKernelImageDims icStart objectDimensions differentialRanks)

hypercohomologyVanishes ::
  (Eq a, Field a, Num a, DenseRankBackend a) =>
  Derived a -> Either MoonlightError Bool
hypercohomologyVanishes =
  hypercohomologyVanishesWith fieldRankBackend

hypercohomologyVanishesWith ::
  (Eq a, Num a) =>
  RankBackend a ->
  Derived a ->
  Either MoonlightError Bool
hypercohomologyVanishesWith rankBackend =
  fmap (all (== 0) . IM.elems) . hypercohomologyDimsWith rankBackend

hypercohomologyReducedVanishes ::
  (Eq a, Field a, Num a, DenseRankBackend a) =>
  Derived a -> Either MoonlightError Bool
hypercohomologyReducedVanishes =
  hypercohomologyReducedVanishesWith fieldRankBackend

hypercohomologyReducedVanishesWith ::
  (Eq a, Num a) =>
  RankBackend a ->
  Derived a ->
  Either MoonlightError Bool
hypercohomologyReducedVanishesWith rankBackend =
  fmap reducedVanishing . hypercohomologyDimsWith rankBackend
  where
    reducedVanishing :: IntMap Int -> Bool
    reducedVanishing dims =
      IM.findWithDefault 0 0 dims <= 1
        && all (== 0) [v | (k, v) <- IM.toList dims, k /= 0]

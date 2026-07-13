{-# LANGUAGE TypeApplications #-}

module Moonlight.Probability.DenseSimplexSpec (tests) where

import Data.Kind (Type)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Probability.Distribution.DenseSimplex
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Test.Tasty.QuickCheck (Gen, choose, counterexample, elements, forAll, listOf1, testProperty, (===))
import Prelude

type TestKey :: Type
data TestKey
  = KA
  | KB
  | KC
  | KD
  | KE
  deriving stock (Eq, Ord, Show, Enum, Bounded)

eps :: Double
eps = 1.0e-9

approxEqual :: Double -> Double -> Bool
approxEqual a b = abs (a - b) <= 1.0e-6

tests :: TestTree
tests =
  testGroup
    "DenseSimplex"
    [ testCase "cardinality matches enum domain" $
        denseSimplexCardinality @TestKey @?= 5,
      testCase "pure singles a key" $ do
        let ds = pureDenseSimplex KC
        denseSimplexAt ds KC @?= 1.0
        denseSimplexAt ds KA @?= 0.0
        dominantDenseKey ds @?= Just KC
        denseSimplexSupportSize ds @?= 1,
      testCase "uniform gives 1/n everywhere" $ do
        let ds = uniformDenseSimplex @TestKey
            expected = 1.0 / 5.0
        mapM_
          (\k -> assertBool ("uniform " <> show k) (approxEqual (denseSimplexAt ds k) expected))
          [KA, KB, KC, KD, KE]
        assertBool "uniform entropy" (approxEqual (denseSimplexShannonEntropy ds) (log 5.0)),
      testCase "self-interference is 1" $ do
        let ds = denseSimplexFromWeights KA (Map.fromList [(KA, 0.3), (KB, 0.5), (KC, 0.2)])
        assertBool "self-interference" (approxEqual (denseSimplexInterference ds ds) 1.0),
      testProperty "fromWeights produces sum-to-1" $
        forAll genWeightMap $ \wm ->
          let ds = denseSimplexFromWeights KA wm
              total = sum (fmap snd (denseSimplexToList ds))
           in counterexample ("total=" <> show total) (approxEqual total 1.0),
      testProperty "blend(0, l, r) = r" $
        forAll genSimplex $ \l ->
          forAll genSimplex $ \r ->
            denseSimplexToList (denseSimplexBlend 0.0 l r) === denseSimplexToList r,
      testProperty "blend(1, l, r) = l" $
        forAll genSimplex $ \l ->
          forAll genSimplex $ \r ->
            denseSimplexToList (denseSimplexBlend 1.0 l r) === denseSimplexToList l,
      testProperty "blend preserves sum-to-1" $
        forAll (choose (0.0, 1.0)) $ \alpha ->
          forAll genSimplex $ \l ->
            forAll genSimplex $ \r ->
              let blended = denseSimplexBlend alpha l r
                  total = sum (fmap snd (denseSimplexToList blended))
               in counterexample ("total=" <> show total) (approxEqual total 1.0),
      testProperty "blendDenseMixtures preserves sum-to-1" $
        forAll (listOf1 genSimplex) $ \xs ->
          let mixed = blendDenseMixtures (NonEmpty.fromList xs)
              total = sum (fmap snd (denseSimplexToList mixed))
           in counterexample ("total=" <> show total) (approxEqual total 1.0),
      testProperty "mkDenseSimplex rejects non-normalized" $
        forAll (choose (1.5, 10.0)) $ \scaleFactor ->
          let weights = Map.fromList [(KA, 0.3 * scaleFactor), (KB, 0.4 * scaleFactor), (KC, 0.3 * scaleFactor), (KD, 0.0), (KE, 0.0)]
           in case mkDenseSimplex @TestKey weights of
                Left (DenseSimplexNotNormalized _) -> True
                _ -> False,
      testProperty "mkDenseSimplex accepts normalized" $
        forAll genNormalizedWeights $ \wm ->
          case mkDenseSimplex @TestKey wm of
            Right _ -> True
            Left _ -> False,
      testProperty "fromWeights handles NaN gracefully" $
        forAll (elements [KA, KB, KC]) $ \bp ->
          let wm = Map.fromList [(KA, 0 / 0), (KB, 1 / 0), (KC, 0.5)]
              ds = denseSimplexFromWeights bp wm
              total = sum (fmap snd (denseSimplexToList ds))
           in counterexample ("total=" <> show total) (approxEqual total 1.0),
      testCase "dominance of pure is 1" $ do
        let ds = pureDenseSimplex KB
        assertBool "dominance" (approxEqual (denseSimplexDominance ds) 1.0),
      testProperty "interference symmetric" $
        forAll genSimplex $ \l ->
          forAll genSimplex $ \r ->
            let lr = denseSimplexInterference l r
                rl = denseSimplexInterference r l
             in counterexample (show (lr, rl)) (approxEqual lr rl)
    ]

genWeightMap :: Gen (Map TestKey Double)
genWeightMap = do
  let keys = [KA, KB, KC, KD, KE]
  values <- mapM (const (choose (0.0, 10.0))) keys
  pure (Map.fromList (zip keys values))

genNormalizedWeights :: Gen (Map TestKey Double)
genNormalizedWeights = do
  raw <- mapM (const (choose (eps, 10.0))) [KA, KB, KC, KD, KE]
  let total = sum raw
      normalized = fmap (/ total) raw
  pure (Map.fromList (zip [KA, KB, KC, KD, KE] normalized))

genSimplex :: Gen (DenseSimplex TestKey)
genSimplex = do
  wm <- genWeightMap
  bp <- elements [KA, KB, KC, KD, KE]
  pure (denseSimplexFromWeights bp wm)

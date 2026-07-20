{-# LANGUAGE LambdaCase #-}

module Common
  ( benchSizes,
    caseLabel,
    benchCost,
    benchFixTerm,
    benchVariantFixTerm,
    expectBench,
    expectMaybeBench,
    boolWeight,
    eitherWeight,
  )
where

import Data.Fix (Fix (..))
import Fixture (BenchSig (..))
import Moonlight.Rewrite (Cost (..), K (..))
import Moonlight.Rewrite.DSL (Node (..))

benchSizes :: [Int]
benchSizes =
  [4, 16, 48]

caseLabel :: String -> Int -> String
caseLabel label size =
  label <> "/" <> show size

benchCost :: Cost BenchSig Int
benchCost =
  Cost $ \case
    Leaf _ ->
      1
    Wrap (K childCost) ->
      childCost + 1
    Pair (K leftCost) (K rightCost) ->
      leftCost + rightCost + 1
    Flag _ ->
      1

benchFixTerm :: Int -> Fix (Node BenchSig)
benchFixTerm size =
  foldr
    (\key accumulated -> fixPair (fixWrap accumulated) (fixLeaf key))
    (fixLeaf 0)
    [1 .. max 0 size]

benchVariantFixTerm :: Int -> Fix (Node BenchSig)
benchVariantFixTerm size =
  foldr
    (\key accumulated -> fixPair (fixWrap accumulated) (fixLeaf (key + 1)))
    (fixLeaf 1)
    [1 .. max 0 size]

fixLeaf :: Int -> Fix (Node BenchSig)
fixLeaf key =
  Fix (Node (Leaf key))

fixWrap :: Fix (Node BenchSig) -> Fix (Node BenchSig)
fixWrap child =
  Fix (Node (Wrap (K child)))

fixPair :: Fix (Node BenchSig) -> Fix (Node BenchSig) -> Fix (Node BenchSig)
fixPair leftChild rightChild =
  Fix (Node (Pair (K leftChild) (K rightChild)))

expectBench :: Show errorValue => String -> Either errorValue value -> IO value
expectBench label =
  either (\errorValue -> fail (label <> " failed: " <> show errorValue)) pure

expectMaybeBench :: String -> Maybe value -> IO value
expectMaybeBench label =
  maybe (fail (label <> " failed")) pure

boolWeight :: Bool -> Maybe Int
boolWeight observed =
  if observed then Just 1 else Nothing

eitherWeight :: (value -> Int) -> Either errorValue value -> Maybe Int
eitherWeight weigh =
  either (const Nothing) (Just . weigh)

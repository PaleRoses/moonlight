{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Covering
  ( coveringBenchmarks,
  )
where

import Data.Function ((&))
import Data.Kind (Type)
import Data.Monoid (Sum (..))
import Data.Type.Equality ((:~:) (Refl))
import Moonlight.Category.Pure.CoveringFamily (CoveringFamily (..), Exists (..))
import Moonlight.Category.Pure.CoveringProduct
  ( CoveringProduct,
    adjustCoveringProduct,
    foldMapCoveringProductWithWitness,
    indexCoveringProduct,
    mapCoveringProductWithWitness,
    replaceCoveringProduct,
    restrictCoveringProduct,
    tabulateCoveringProduct,
  )
import Test.Tasty.Bench (Benchmark, bench, bgroup, nf)

type DemoField :: Type
data DemoField
  = AlphaField
  | BetaField
  | GammaField

type DemoFieldWitness :: DemoField -> Type
data DemoFieldWitness (field :: DemoField) where
  AlphaFieldWitness :: DemoFieldWitness 'AlphaField
  BetaFieldWitness :: DemoFieldWitness 'BetaField
  GammaFieldWitness :: DemoFieldWitness 'GammaField

type DemoSubsetWitness :: DemoField -> Type
data DemoSubsetWitness (field :: DemoField) where
  AlphaSubsetWitness :: DemoSubsetWitness 'AlphaField
  GammaSubsetWitness :: DemoSubsetWitness 'GammaField

type DemoValue :: DemoField -> Type
newtype DemoValue (field :: DemoField) = DemoValue
  { unDemoValue :: Int
  }
  deriving stock (Eq, Show)

instance CoveringFamily DemoFieldWitness where
  allMembers =
    [ Exists AlphaFieldWitness,
      Exists BetaFieldWitness,
      Exists GammaFieldWitness
    ]

instance CoveringFamily DemoSubsetWitness where
  allMembers =
    [ Exists AlphaSubsetWitness,
      Exists GammaSubsetWitness
    ]

coveringBenchmarks :: Benchmark
coveringBenchmarks =
  bgroup
    "CoveringProduct / CoveringFamily"
    [ bench "index full product batch x512" (nf fullProductIndexBatchWeight sampleBatch),
      bench "restrict subset then index batch x512" (nf restrictSubsetBatchWeight sampleBatch),
      bench "adjust one witness then fold batch x512" (nf adjustProductBatchWeight sampleBatch),
      bench "replace one witness then fold batch x512" (nf replaceProductBatchWeight sampleBatch),
      bench "map with witness then fold batch x512" (nf mapWithWitnessBatchWeight sampleBatch),
      bench "foldMap with existential witnesses batch x512" (nf coveringProductBatchWeight sampleBatch)
    ]


sampleBatch :: [Int]
sampleBatch = [0 .. 511]

sameDemoFieldWitness ::
  DemoFieldWitness left ->
  DemoFieldWitness right ->
  Maybe (left :~: right)
sameDemoFieldWitness leftWitness rightWitness =
  case (leftWitness, rightWitness) of
    (AlphaFieldWitness, AlphaFieldWitness) -> Just Refl
    (BetaFieldWitness, BetaFieldWitness) -> Just Refl
    (GammaFieldWitness, GammaFieldWitness) -> Just Refl
    _ -> Nothing

embedDemoSubsetWitness ::
  DemoSubsetWitness field ->
  DemoFieldWitness field
embedDemoSubsetWitness subsetWitness =
  case subsetWitness of
    AlphaSubsetWitness -> AlphaFieldWitness
    GammaSubsetWitness -> GammaFieldWitness

demoProduct :: CoveringProduct DemoFieldWitness DemoValue
demoProduct =
  tabulateCoveringProduct
    ( \witness ->
        case witness of
          AlphaFieldWitness -> DemoValue 11
          BetaFieldWitness -> DemoValue 17
          GammaFieldWitness -> DemoValue 23
    )

fullProductIndexBatchWeight :: [Int] -> Int
fullProductIndexBatchWeight =
  sum . fmap (\seed -> seed + fullProductIndexWeight demoProduct)

restrictSubsetBatchWeight :: [Int] -> Int
restrictSubsetBatchWeight =
  sum . fmap (\seed -> seed + restrictSubsetWeight demoProduct)

adjustProductBatchWeight :: [Int] -> Int
adjustProductBatchWeight =
  sum . fmap (\seed -> adjustProductWeight seed demoProduct)

replaceProductBatchWeight :: [Int] -> Int
replaceProductBatchWeight =
  sum . fmap (\seed -> replaceProductWeight seed demoProduct)

mapWithWitnessBatchWeight :: [Int] -> Int
mapWithWitnessBatchWeight =
  sum . fmap (\seed -> seed + mapWithWitnessWeight demoProduct)

coveringProductBatchWeight :: [Int] -> Int
coveringProductBatchWeight =
  sum . fmap (\seed -> seed + coveringProductWeight demoProduct)

fullProductIndexWeight :: CoveringProduct DemoFieldWitness DemoValue -> Int
fullProductIndexWeight productValue =
  demoValueWeight (indexCoveringProduct productValue AlphaFieldWitness)
    + demoValueWeight (indexCoveringProduct productValue BetaFieldWitness)
    + demoValueWeight (indexCoveringProduct productValue GammaFieldWitness)

restrictSubsetWeight :: CoveringProduct DemoFieldWitness DemoValue -> Int
restrictSubsetWeight productValue =
  let restrictedProduct = restrictCoveringProduct embedDemoSubsetWitness productValue
   in demoValueWeight (indexCoveringProduct restrictedProduct AlphaSubsetWitness)
        + demoValueWeight (indexCoveringProduct restrictedProduct GammaSubsetWitness)

adjustProductWeight :: Int -> CoveringProduct DemoFieldWitness DemoValue -> Int
adjustProductWeight seed productValue =
  adjustCoveringProduct sameDemoFieldWitness BetaFieldWitness (\(DemoValue value) -> DemoValue (value + seed)) productValue
    & coveringProductWeight

replaceProductWeight :: Int -> CoveringProduct DemoFieldWitness DemoValue -> Int
replaceProductWeight seed productValue =
  replaceCoveringProduct sameDemoFieldWitness GammaFieldWitness (DemoValue seed) productValue
    & coveringProductWeight

mapWithWitnessWeight :: CoveringProduct DemoFieldWitness DemoValue -> Int
mapWithWitnessWeight productValue =
  mapCoveringProductWithWitness (\witness (DemoValue value) -> DemoValue (value + demoWitnessWeight witness)) productValue
    & coveringProductWeight

coveringProductWeight :: CoveringProduct DemoFieldWitness DemoValue -> Int
coveringProductWeight productValue =
  getSum
    ( foldMapCoveringProductWithWitness
        (\witness value -> Sum (demoWitnessWeight witness + demoValueWeight value))
        productValue
    )

demoWitnessWeight :: DemoFieldWitness field -> Int
demoWitnessWeight witness =
  case witness of
    AlphaFieldWitness -> 1
    BetaFieldWitness -> 2
    GammaFieldWitness -> 3

demoValueWeight :: DemoValue field -> Int
demoValueWeight (DemoValue value) = value

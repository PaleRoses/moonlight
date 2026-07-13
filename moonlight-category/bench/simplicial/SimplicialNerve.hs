module SimplicialNerve
  ( nerveBenchmarks,
  )
where

import Control.DeepSeq (NFData (..))
import Data.Bifunctor (first)
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Category
  ( ComposableChain,
    FinCat,
    FinCatValidationError,
    FinGeneratorId (..),
    FinMorphismId (..),
    FinObjectId (..),
    allMorphisms,
    allObjects,
    chainMorphisms,
    finMorId,
    mkFinCat,
  )
import Moonlight.Category.Simplicial
  ( NerveSimplex,
    nerve,
    nerveSimplexChain,
    nerveSimplexDimension,
  )
import Moonlight.Category.Simplicial
  ( TruncatedNormalizedSSet,
    simplicesAtDimension,
    truncationBound,
  )
import Numeric.Natural (Natural)
import SimplicialWeight (naturalWeight)
import Test.Tasty.Bench (Benchmark, bench, bgroup, env, nf)

nerveBenchmarks :: Benchmark
nerveBenchmarks =
  bgroup
    "nerve API"
    (nerveCases & fmap nerveBenchmark)

nerveBenchmark :: NerveCase -> Benchmark
nerveBenchmark nerveCase =
  env (prepareNerveCategory nerveCase) $ \categoryValue ->
    bench (nerveCaseLabel nerveCase) (nf preparedNerveWeight categoryValue)

data NerveCase = NerveCase
  { nerveCaseObjectCount :: !Int,
    nerveCaseTruncationBound :: !Natural
  }
  deriving stock (Eq, Ord, Show)

nerveCases :: [NerveCase]
nerveCases =
  [ NerveCase 4 2,
    NerveCase 5 2,
    NerveCase 5 3,
    NerveCase 6 3
  ]

data PreparedNerveCategory = PreparedNerveCategory
  { preparedNerveTruncationBound :: !Natural,
    preparedNerveCategory :: !FinCat
  }

instance NFData PreparedNerveCategory where
  rnf prepared =
    preparedNerveCategoryWeight prepared `seq` ()

nerveCaseLabel :: NerveCase -> String
nerveCaseLabel nerveCase =
  "nerve FinCat thin-total-order objects="
    <> show (nerveCaseObjectCount nerveCase)
    <> " bound="
    <> show (nerveCaseTruncationBound nerveCase)

prepareNerveCategory :: NerveCase -> IO PreparedNerveCategory
prepareNerveCategory nerveCase =
  case first NonEmpty.toList (thinTotalOrderCategory (nerveCaseObjectCount nerveCase)) of
    Left errors -> fail ("invalid nerve benchmark category: " <> show errors)
    Right categoryValue ->
      pure
        PreparedNerveCategory
          { preparedNerveTruncationBound = nerveCaseTruncationBound nerveCase,
            preparedNerveCategory = categoryValue
          }

thinTotalOrderCategory :: Int -> Either (NonEmpty FinCatValidationError) FinCat
thinTotalOrderCategory objectCount =
  mkFinCat
    (Set.fromAscList (FinObjectId <$> objectKeys objectCount))
    (Map.fromList (morphismBuckets objectCount))
    (Map.fromList (compositionEntries objectCount))

objectKeys :: Int -> [Int]
objectKeys objectCount =
  [0 .. objectCount - 1]

strictObjectPairs :: Int -> [(Int, Int)]
strictObjectPairs objectCount =
  objectKeys objectCount
    >>= (\sourceKey -> fmap (\targetKey -> (sourceKey, targetKey)) [sourceKey + 1 .. objectCount - 1])

morphismBuckets :: Int -> [((FinObjectId, FinObjectId), [FinMorphismId])]
morphismBuckets objectCount =
  strictObjectPairs objectCount
    & fmap
      ( \(sourceKey, targetKey) ->
          ( (FinObjectId sourceKey, FinObjectId targetKey),
            [thinMorphismId sourceKey targetKey]
          )
      )

compositionEntries :: Int -> [((FinMorphismId, FinMorphismId), FinMorphismId)]
compositionEntries objectCount =
  objectKeys objectCount
    >>= (\sourceKey -> [sourceKey + 1 .. objectCount - 1] >>= middleEntries sourceKey)
  where
    middleEntries sourceKey middleKey =
      [middleKey + 1 .. objectCount - 1]
        & fmap
          ( \targetKey ->
              ( (thinMorphismId middleKey targetKey, thinMorphismId sourceKey middleKey),
                thinMorphismId sourceKey targetKey
              )
          )

thinMorphismId :: Int -> Int -> FinMorphismId
thinMorphismId sourceKey targetKey =
  FinGeneratorMorphismId (FinGeneratorId (sourceKey * 1024 + targetKey))

nerveWeight :: Natural -> FinCat -> Int
nerveWeight upperBound categoryValue =
  nerve categoryValue upperBound
    & nerveSSetWeight

preparedNerveWeight :: PreparedNerveCategory -> Int
preparedNerveWeight prepared =
  nerveWeight
    (preparedNerveTruncationBound prepared)
    (preparedNerveCategory prepared)

preparedNerveCategoryWeight :: PreparedNerveCategory -> Int
preparedNerveCategoryWeight prepared =
  length (allObjects (preparedNerveCategory prepared))
    + length (allMorphisms (preparedNerveCategory prepared))
    + naturalWeight (preparedNerveTruncationBound prepared)

nerveSSetWeight :: TruncatedNormalizedSSet (NerveSimplex FinCat) -> Int
nerveSSetWeight simplicialSet =
  [0 .. truncationBound simplicialSet]
    & fmap (nerveSimplicesWeight . simplicesAtDimension simplicialSet)
    & sum

nerveSimplicesWeight :: [NerveSimplex FinCat] -> Int
nerveSimplicesWeight =
  sum . fmap nerveSimplexWeight

nerveSimplexWeight :: NerveSimplex FinCat -> Int
nerveSimplexWeight simplexValue =
  naturalWeight (nerveSimplexDimension simplexValue)
    + composableChainWeight (nerveSimplexChain simplexValue)

composableChainWeight :: ComposableChain FinCat -> Int
composableChainWeight chainValue =
  chainMorphisms chainValue
    & fmap (finMorphismWeight . finMorId)
    & sum

finMorphismWeight :: FinMorphismId -> Int
finMorphismWeight morphismId =
  case morphismId of
    FinIdentityId (FinObjectId objectKey) -> objectKey
    FinGeneratorMorphismId (FinGeneratorId generatorKey) -> generatorKey

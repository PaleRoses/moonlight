module Moonlight.Probability.TestSupport.Generators
  ( PositiveWeightSample,
    CategoricalWeightSample,
    supportFromPositiveWeights,
    PerturbedCategoricalPair (..),
    defaultPerturbationMagnitudes,
    categoricalFromPositiveWeights,
    finiteDistributionFromPositiveWeights,
    withCategoricalFromPositiveWeights,
    withFiniteDistributionFromPositiveWeights,
    withFiniteDistributionPairFromPositiveWeights,
    withDisjointFiniteDistributionPairFromPositiveWeights,
    withCategoricalPairFromPositiveWeights,
    withOverlappingCategoricalPairFromPositiveWeights,
    withDisjointCategoricalPairFromPositiveWeights,
    withNearIdenticalCategoricalPairFromPositiveWeights,
    withNearIdenticalCategoricalPairsAtScales,
  )
where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Moonlight.Probability.Distribution.Categorical (Categorical, mkCategorical)
import Moonlight.Probability.Distribution.Finite (FiniteDistribution, mkFiniteDistribution)
import Test.Tasty.QuickCheck
  ( Positive (..),
    Property,
    NonEmptyList (..),
    counterexample,
  )

type PositiveWeightSample :: Type
type PositiveWeightSample = NonEmptyList (Positive Int)

type CategoricalWeightSample :: Type
type CategoricalWeightSample = PositiveWeightSample

supportFromPositiveWeights :: PositiveWeightSample -> NonEmpty Int
supportFromPositiveWeights weightSample =
  let cardinality = supportCardinality weightSample
   in 0 :| [1 .. cardinality - 1]

type PerturbedCategoricalPair :: Type -> Type
data PerturbedCategoricalPair a = PerturbedCategoricalPair
  { perturbedMagnitude :: Double,
    perturbedReferenceCategorical :: Categorical a,
    perturbedCandidateCategorical :: Categorical a
  }

defaultPerturbationMagnitudes :: NonEmpty Double
defaultPerturbationMagnitudes = 1000.0 :| [100.0, 10.0, 1.0]

categoricalFromPositiveWeights :: CategoricalWeightSample -> Either String (Categorical Int)
categoricalFromPositiveWeights =
  categoricalFromPositiveWeightsAt 0

finiteDistributionFromPositiveWeights :: PositiveWeightSample -> Either String (FiniteDistribution Int)
finiteDistributionFromPositiveWeights (NonEmpty positiveWeights) =
  case mkFiniteDistribution (positiveWeightMap 0 positiveWeights) of
    Left err -> Left ("unexpected finite distribution construction failure: " <> show err)
    Right distribution -> Right distribution

withFiniteDistributionFromPositiveWeights ::
  PositiveWeightSample ->
  (FiniteDistribution Int -> Property) ->
  Property
withFiniteDistributionFromPositiveWeights weightSample continuation =
  case finiteDistributionFromPositiveWeights weightSample of
    Left message -> counterexample message False
    Right distribution -> continuation distribution

withFiniteDistributionPairFromPositiveWeights ::
  PositiveWeightSample ->
  PositiveWeightSample ->
  (FiniteDistribution Int -> FiniteDistribution Int -> Property) ->
  Property
withFiniteDistributionPairFromPositiveWeights leftWeights rightWeights continuation =
  withFiniteDistributionPairAtOffsets leftWeights rightWeights 0 0 continuation

withDisjointFiniteDistributionPairFromPositiveWeights ::
  PositiveWeightSample ->
  PositiveWeightSample ->
  (FiniteDistribution Int -> FiniteDistribution Int -> Property) ->
  Property
withDisjointFiniteDistributionPairFromPositiveWeights leftWeights rightWeights continuation =
  withFiniteDistributionPairAtOffsets leftWeights rightWeights 0 (supportCardinality leftWeights) continuation

withFiniteDistributionPairAtOffsets ::
  PositiveWeightSample ->
  PositiveWeightSample ->
  Int ->
  Int ->
  (FiniteDistribution Int -> FiniteDistribution Int -> Property) ->
  Property
withFiniteDistributionPairAtOffsets leftWeights rightWeights leftOffset rightOffset continuation =
  withFiniteDistributionFromOffset leftOffset leftWeights
    (\leftDistribution ->
       withFiniteDistributionFromOffset rightOffset rightWeights
         (\rightDistribution -> continuation leftDistribution rightDistribution)
    )

withFiniteDistributionFromOffset ::
  Int ->
  PositiveWeightSample ->
  (FiniteDistribution Int -> Property) ->
  Property
withFiniteDistributionFromOffset offset weightSample continuation =
  case finiteDistributionFromPositiveWeightsAt offset weightSample of
    Left message -> counterexample message False
    Right distribution -> continuation distribution

finiteDistributionFromPositiveWeightsAt :: Int -> PositiveWeightSample -> Either String (FiniteDistribution Int)
finiteDistributionFromPositiveWeightsAt offset (NonEmpty positiveWeights) =
  case mkFiniteDistribution (positiveWeightMap offset positiveWeights) of
    Left err -> Left ("unexpected finite distribution construction failure: " <> show err)
    Right distribution -> Right distribution

withCategoricalFromPositiveWeights ::
  CategoricalWeightSample ->
  (Categorical Int -> Property) ->
  Property
withCategoricalFromPositiveWeights weightSample continuation =
  case categoricalFromPositiveWeights weightSample of
    Left message -> counterexample message False
    Right categorical -> continuation categorical

withCategoricalPairFromPositiveWeights ::
  CategoricalWeightSample ->
  CategoricalWeightSample ->
  (Categorical Int -> Categorical Int -> Property) ->
  Property
withCategoricalPairFromPositiveWeights leftWeights rightWeights continuation =
  withCategoricalPairAtOffsets leftWeights rightWeights 0 0 continuation

withOverlappingCategoricalPairFromPositiveWeights ::
  CategoricalWeightSample ->
  CategoricalWeightSample ->
  (Categorical Int -> Categorical Int -> Property) ->
  Property
withOverlappingCategoricalPairFromPositiveWeights leftWeights rightWeights continuation =
  withCategoricalPairAtOffsets leftWeights rightWeights 0 0 continuation

withDisjointCategoricalPairFromPositiveWeights ::
  CategoricalWeightSample ->
  CategoricalWeightSample ->
  (Categorical Int -> Categorical Int -> Property) ->
  Property
withDisjointCategoricalPairFromPositiveWeights leftWeights rightWeights continuation =
  withCategoricalPairAtOffsets leftWeights rightWeights 0 (supportCardinality leftWeights) continuation

withNearIdenticalCategoricalPairFromPositiveWeights ::
  CategoricalWeightSample ->
  (Categorical Int -> Categorical Int -> Property) ->
  Property
withNearIdenticalCategoricalPairFromPositiveWeights weightSample continuation =
  withNearIdenticalCategoricalPairsAtScales
    (perturbationMagnitude :| [])
    weightSample
    (\perturbedPairs ->
       let perturbedPair = NonEmpty.head perturbedPairs
        in continuation
             (perturbedReferenceCategorical perturbedPair)
             (perturbedCandidateCategorical perturbedPair)
    )

withNearIdenticalCategoricalPairsAtScales ::
  NonEmpty Double ->
  CategoricalWeightSample ->
  (NonEmpty (PerturbedCategoricalPair Int) -> Property) ->
  Property
withNearIdenticalCategoricalPairsAtScales perturbationMagnitudes weightSample continuation =
  case nearIdenticalCategoricalPairsAtScales perturbationMagnitudes weightSample of
    Left message -> counterexample message False
    Right perturbedPairs -> continuation perturbedPairs

withCategoricalPairAtOffsets ::
  CategoricalWeightSample ->
  CategoricalWeightSample ->
  Int ->
  Int ->
  (Categorical Int -> Categorical Int -> Property) ->
  Property
withCategoricalPairAtOffsets leftWeights rightWeights leftOffset rightOffset continuation =
  withCategoricalFromOffset leftOffset leftWeights
    (\leftCategorical ->
       withCategoricalFromOffset rightOffset rightWeights
         (\rightCategorical -> continuation leftCategorical rightCategorical)
    )

withCategoricalFromOffset ::
  Int ->
  CategoricalWeightSample ->
  (Categorical Int -> Property) ->
  Property
withCategoricalFromOffset offset weightSample continuation =
  case categoricalFromPositiveWeightsAt offset weightSample of
    Left message -> counterexample message False
    Right categorical -> continuation categorical

categoricalFromPositiveWeightsAt :: Int -> CategoricalWeightSample -> Either String (Categorical Int)
categoricalFromPositiveWeightsAt offset (NonEmpty positiveWeights) =
  case mkCategorical (positiveWeightMap offset positiveWeights) of
    Left err -> Left ("unexpected categorical construction failure: " <> show err)
    Right categorical -> Right categorical

positiveWeightMap :: Int -> [Positive Int] -> Map.Map Int Double
positiveWeightMap offset positiveWeights =
  Map.fromAscList
    ( zip
        [offset ..]
        (fmap (fromIntegral . getPositive) positiveWeights)
    )

supportCardinality :: CategoricalWeightSample -> Int
supportCardinality (NonEmpty positiveWeights) = length positiveWeights

nearIdenticalCategoricalPairsAtScales ::
  NonEmpty Double ->
  CategoricalWeightSample ->
  Either String (NonEmpty (PerturbedCategoricalPair Int))
nearIdenticalCategoricalPairsAtScales perturbationMagnitudes (NonEmpty positiveWeights) = do
  let stabilizedWeights =
        fmap
          ((+ stabilityBaseline) . fromIntegral . getPositive)
          positiveWeights
  referenceCategorical <- categoricalFromWeightValuesAt 0 stabilizedWeights
  traverse
    (mkPerturbedCategoricalPair referenceCategorical stabilizedWeights)
    perturbationMagnitudes

categoricalFromWeightValuesAt :: Int -> [Double] -> Either String (Categorical Int)
categoricalFromWeightValuesAt offset weights =
  case mkCategorical (Map.fromAscList (zip [offset ..] weights)) of
    Left err -> Left ("unexpected categorical construction failure: " <> show err)
    Right categorical -> Right categorical

mkPerturbedCategoricalPair ::
  Categorical Int ->
  [Double] ->
  Double ->
  Either String (PerturbedCategoricalPair Int)
mkPerturbedCategoricalPair referenceCategorical stabilizedWeights magnitude = do
  candidateCategorical <-
    categoricalFromWeightValuesAt 0 (perturbWeightValues magnitude stabilizedWeights)
  pure
    PerturbedCategoricalPair
      { perturbedMagnitude = magnitude,
        perturbedReferenceCategorical = referenceCategorical,
        perturbedCandidateCategorical = candidateCategorical
      }

perturbWeightValues :: Double -> [Double] -> [Double]
perturbWeightValues magnitude weights =
  case weights of
    [] -> []
    [weight] -> [weight + magnitude]
    firstWeight : secondWeight : remainingWeights ->
      (firstWeight + magnitude)
        : (secondWeight - magnitude)
        : remainingWeights

stabilityBaseline :: Double
stabilityBaseline = 1.0e6

perturbationMagnitude :: Double
perturbationMagnitude = 1.0

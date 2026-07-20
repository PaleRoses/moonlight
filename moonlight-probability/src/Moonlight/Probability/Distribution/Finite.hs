module Moonlight.Probability.Distribution.Finite
  ( FiniteDistribution,
    FiniteDistributionError (..),
    mkFiniteDistribution,
    certainFiniteDistribution,
    uniformFiniteDistribution,
    blendFiniteDistribution,
    finiteFoldMap,
    finiteFoldMap1,
    finiteTraverse,
    finiteWeightedOutcomes,
    finiteSupport,
    finiteEntropyValue,
    finiteLookup,
    finiteRestrict,
    sampleAt,
  )
where

import Data.Kind (Type)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Semigroup (sconcat)
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Core (mkFiniteDouble)
import Moonlight.Probability.Core
  ( PositiveProb,
    Prob,
    mkPositiveProb,
    positiveProbOne,
    positiveProbToProb,
    positiveProbValue,
    probValue,
  )
import Moonlight.Probability.Core.Internal (PositiveProb (..), Prob (..))
import Prelude

type FiniteDistribution :: Type -> Type
data FiniteDistribution a = FiniteDistribution
  { finiteWeightedSupport :: NonEmpty (a, PositiveProb),
    finiteProbabilityMap :: Map a Prob,
    finiteEntropyCache :: Double
  }
  deriving stock (Eq, Show)

type FiniteDistributionError :: Type
data FiniteDistributionError
  = EmptyFiniteDistributionSupport
  | InvalidFiniteDistributionWeight Double
  | NonPositiveFiniteDistributionWeight Double
  deriving stock (Eq, Show)

mkFiniteDistribution :: Map a Double -> Either FiniteDistributionError (FiniteDistribution a)
mkFiniteDistribution weights = do
  positiveWeights <- traverse validateWeight weights
  if Map.null positiveWeights
    then Left EmptyFiniteDistributionSupport
    else do
      let totalWeight = sum (Map.elems positiveWeights)
          normalizedWeights = fmap (/ totalWeight) positiveWeights
      (supportWeights, probabilityMap) <- mkWeightedFiniteDistributionData normalizedWeights
      pure (buildFiniteDistribution supportWeights probabilityMap)

certainFiniteDistribution :: a -> FiniteDistribution a
certainFiniteDistribution outcome =
  buildFiniteDistribution ((outcome, positiveProbOne) :| []) (Map.singleton outcome (positiveProbToProb positiveProbOne))

uniformFiniteDistribution :: Ord a => NonEmpty a -> FiniteDistribution a
uniformFiniteDistribution outcomes@(firstOutcome :| remainingInputOutcomes) =
  let representativeOutcome = List.foldl' min firstOutcome remainingInputOutcomes
      remainingOutcomes =
        Set.toAscList
          (Set.delete representativeOutcome (Set.fromList (NonEmpty.toList outcomes)))
   in buildNormalizedFiniteDistribution
        ((representativeOutcome, 1.0) :| fmap (,1.0) remainingOutcomes)

blendFiniteDistribution :: Ord a => NonEmpty (PositiveProb, FiniteDistribution a) -> FiniteDistribution a
blendFiniteDistribution = buildNormalizedFiniteDistribution . sconcat . fmap scaleWeightedDistribution

finiteFoldMap :: Monoid m => ((a, PositiveProb) -> m) -> FiniteDistribution a -> m
finiteFoldMap transform = foldMap transform . finiteWeightedSupport

finiteFoldMap1 :: Semigroup m => ((a, PositiveProb) -> m) -> FiniteDistribution a -> m
finiteFoldMap1 transform = sconcat . fmap transform . finiteWeightedSupport

finiteTraverse ::
  (Applicative f, Ord b) =>
  ((a, PositiveProb) -> f b) ->
  FiniteDistribution a ->
  f (FiniteDistribution b)
finiteTraverse transform =
  fmap (buildNormalizedFiniteDistribution . fmap (\(outcome, probability) -> (outcome, positiveProbValue probability)))
    . traverse (\weightedOutcome@(_, probability) -> (, probability) <$> transform weightedOutcome)
    . finiteWeightedSupport

finiteWeightedOutcomes :: FiniteDistribution a -> NonEmpty (a, PositiveProb)
finiteWeightedOutcomes = finiteWeightedSupport

finiteSupport :: FiniteDistribution a -> Set a
finiteSupport = Map.keysSet . finiteProbabilityMap

finiteEntropyValue :: FiniteDistribution a -> Double
finiteEntropyValue = finiteEntropyCache

finiteLookup :: Ord a => a -> FiniteDistribution a -> Maybe Prob
finiteLookup outcome = Map.lookup outcome . finiteProbabilityMap

finiteRestrict :: Ord a => Set a -> FiniteDistribution a -> Maybe (FiniteDistribution a)
finiteRestrict allowed distribution =
  fmap
    buildNormalizedFiniteDistribution
    ( NonEmpty.nonEmpty
        [ (outcome, positiveProbValue probability)
        | (outcome, probability) <- NonEmpty.toList (finiteWeightedSupport distribution),
          Set.member outcome allowed
        ]
    )

sampleAt :: Prob -> FiniteDistribution a -> a
sampleAt threshold = collapseWith (probValue threshold) . finiteWeightedSupport

validateWeight :: Double -> Either FiniteDistributionError Double
validateWeight weight =
  case mkFiniteDouble "finite distribution weight" weight of
    Left _ -> Left (InvalidFiniteDistributionWeight weight)
    Right finiteWeight ->
      if finiteWeight <= 0.0
        then Left (NonPositiveFiniteDistributionWeight weight)
        else Right finiteWeight

shannonEntropyValue :: Map a Prob -> Double
shannonEntropyValue =
  negate . sum . fmap (\probability -> let value = probValue probability in if value == 0.0 then 0.0 else value * log value) . Map.elems

mkWeightedFiniteDistributionData ::
  Map a Double ->
  Either FiniteDistributionError (NonEmpty (a, PositiveProb), Map a Prob)
mkWeightedFiniteDistributionData normalizedWeights = do
  positiveProbabilityMap <- traverse toPositiveProbability normalizedWeights
  supportWeights <- toNonEmptySupport (Map.toAscList positiveProbabilityMap)
  pure (supportWeights, fmap positiveProbToProb positiveProbabilityMap)

toPositiveProbability :: Double -> Either FiniteDistributionError PositiveProb
toPositiveProbability weight =
  case mkPositiveProb weight of
    Left _ -> Left (InvalidFiniteDistributionWeight weight)
    Right probability -> Right probability

toNonEmptySupport :: [(a, PositiveProb)] -> Either FiniteDistributionError (NonEmpty (a, PositiveProb))
toNonEmptySupport =
  maybe (Left EmptyFiniteDistributionSupport) Right . NonEmpty.nonEmpty

buildFiniteDistribution :: NonEmpty (a, PositiveProb) -> Map a Prob -> FiniteDistribution a
buildFiniteDistribution supportWeights probabilityMap =
  FiniteDistribution {finiteWeightedSupport = supportWeights, finiteProbabilityMap = probabilityMap, finiteEntropyCache = shannonEntropyValue probabilityMap}

buildNormalizedFiniteDistribution :: Ord a => NonEmpty (a, Double) -> FiniteDistribution a
buildNormalizedFiniteDistribution ((representativeOutcome, representativeMass) :| remainingMasses) =
  let (collapsedRepresentativeMass, collapsedRemainingMasses) =
        foldr
          (accumulateOutcome representativeOutcome)
          (representativeMass, Map.empty)
          remainingMasses
      totalMass = collapsedRepresentativeMass + sum (Map.elems collapsedRemainingMasses)
      normalizedRepresentative =
        normalizedPositiveProb (collapsedRepresentativeMass / totalMass)
      normalizedRemaining =
        fmap
          (\(outcome, mass) -> (outcome, normalizedPositiveProb (mass / totalMass)))
          (Map.toAscList collapsedRemainingMasses)
      normalizedRemainingMap =
        fmap (normalizedProb . (/ totalMass)) collapsedRemainingMasses
      probabilityMap =
        Map.insert
          representativeOutcome
          (positiveProbToProb normalizedRepresentative)
          normalizedRemainingMap
   in buildFiniteDistribution
        ((representativeOutcome, normalizedRepresentative) :| normalizedRemaining)
        probabilityMap

scaleFiniteDistribution :: PositiveProb -> FiniteDistribution a -> NonEmpty (a, Double)
scaleFiniteDistribution weight distribution =
  fmap
    (\(outcome, probability) -> (outcome, positiveProbValue weight * positiveProbValue probability))
    (finiteWeightedSupport distribution)

scaleWeightedDistribution :: (PositiveProb, FiniteDistribution a) -> NonEmpty (a, Double)
scaleWeightedDistribution = uncurry scaleFiniteDistribution

accumulateOutcome :: Ord a => a -> (a, Double) -> (Double, Map a Double) -> (Double, Map a Double)
accumulateOutcome representativeOutcome (outcome, mass) (representativeMass, remainingMasses)
  | outcome == representativeOutcome = (representativeMass + mass, remainingMasses)
  | otherwise = (representativeMass, Map.insertWith (+) outcome mass remainingMasses)

collapseWith :: Double -> NonEmpty (a, PositiveProb) -> a
collapseWith _ ((outcome, _) :| []) = outcome
collapseWith threshold ((outcome, probability) :| (nextOutcome : remaining))
  | threshold <= positiveProbValue probability = outcome
  | otherwise = collapseWith (threshold - positiveProbValue probability) (nextOutcome :| remaining)

normalizedProb :: Double -> Prob
normalizedProb = Prob

normalizedPositiveProb :: Double -> PositiveProb
normalizedPositiveProb = PositiveProb . normalizedProb

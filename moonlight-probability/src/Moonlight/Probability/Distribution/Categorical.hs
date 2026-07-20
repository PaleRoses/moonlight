module Moonlight.Probability.Distribution.Categorical
  ( Categorical,
    CategoricalError (..),
    mkCategorical,
    certainCategorical,
    uniformCategorical,
    blendCategorical,
    categoricalFoldMap,
    categoricalFoldMap1,
    categoricalTraverse,
    categoricalWeightedOutcomes,
    categoricalSupport,
    categoricalEntropyValue,
    categoricalLookup,
    categoricalRestrict,
    categoricalCollapseAt,
  )
where

import Data.Bifunctor (first)
import Data.Coerce (coerce)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Moonlight.Probability.Core (PositiveProb, Prob)
import Moonlight.Probability.Distribution.Finite
  ( FiniteDistribution,
    FiniteDistributionError (..),
    blendFiniteDistribution,
    certainFiniteDistribution,
    finiteEntropyValue,
    finiteFoldMap,
    finiteFoldMap1,
    finiteLookup,
    finiteRestrict,
    finiteSupport,
    finiteTraverse,
    finiteWeightedOutcomes,
    mkFiniteDistribution,
    sampleAt,
    uniformFiniteDistribution,
  )
import Prelude

type Categorical :: Type -> Type
newtype Categorical a = Categorical (FiniteDistribution a)
  deriving stock (Eq, Show)

type CategoricalError :: Type
data CategoricalError
  = EmptyCategoricalSupport
  | InvalidCategoricalWeight Double
  | NonPositiveCategoricalWeight Double
  deriving stock (Eq, Show)

mkCategorical :: Map a Double -> Either CategoricalError (Categorical a)
mkCategorical = fmap Categorical . first mapFiniteDistributionError . mkFiniteDistribution

certainCategorical :: a -> Categorical a
certainCategorical = coerce certainFiniteDistribution

uniformCategorical :: Ord a => NonEmpty a -> Categorical a
uniformCategorical = Categorical . uniformFiniteDistribution

blendCategorical :: Ord a => NonEmpty (PositiveProb, Categorical a) -> Categorical a
blendCategorical =
  Categorical . blendFiniteDistribution . coerce

categoricalFoldMap :: Monoid m => ((a, PositiveProb) -> m) -> Categorical a -> m
categoricalFoldMap transform = coerce (finiteFoldMap transform)

categoricalFoldMap1 :: Semigroup m => ((a, PositiveProb) -> m) -> Categorical a -> m
categoricalFoldMap1 transform = coerce (finiteFoldMap1 transform)

categoricalTraverse ::
  (Applicative f, Ord b) =>
  ((a, PositiveProb) -> f b) ->
  Categorical a ->
  f (Categorical b)
categoricalTraverse transform (Categorical distribution) =
  Categorical <$> finiteTraverse transform distribution

categoricalWeightedOutcomes :: Categorical a -> NonEmpty (a, PositiveProb)
categoricalWeightedOutcomes = coerce finiteWeightedOutcomes

categoricalSupport :: Categorical a -> Set a
categoricalSupport = coerce finiteSupport

categoricalEntropyValue :: Categorical a -> Double
categoricalEntropyValue = coerce finiteEntropyValue

categoricalLookup :: Ord a => a -> Categorical a -> Maybe Prob
categoricalLookup outcome = coerce (finiteLookup outcome)

categoricalRestrict :: Ord a => Set a -> Categorical a -> Maybe (Categorical a)
categoricalRestrict allowed (Categorical distribution) =
  fmap Categorical (finiteRestrict allowed distribution)

categoricalCollapseAt :: Prob -> Categorical a -> a
categoricalCollapseAt threshold = coerce (sampleAt threshold)

mapFiniteDistributionError :: FiniteDistributionError -> CategoricalError
mapFiniteDistributionError finiteDistributionError =
  case finiteDistributionError of
    EmptyFiniteDistributionSupport -> EmptyCategoricalSupport
    InvalidFiniteDistributionWeight weight -> InvalidCategoricalWeight weight; NonPositiveFiniteDistributionWeight weight -> NonPositiveCategoricalWeight weight

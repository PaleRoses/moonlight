module Moonlight.EGraph.Fuzzy.Rank
  ( RankMode (..),
    weightedComponent,
    totalOf,
    paretoDominatesBy,
    compareRankBy,
  )
where

import Data.Kind (Type)
import Data.Monoid (All (..), Any (..))

type RankMode :: Type -> Type
data RankMode dimension
  = CompareByTotal
  | CompareLexicographic [dimension]
  | ComparePareto
  deriving stock (Eq, Show)

weightedComponent :: Double -> Double -> Double
weightedComponent weight value = weight * value

totalOf :: Foldable f => f Double -> Double
totalOf = foldr (+) 0.0

paretoDominatesBy :: Foldable f => f dimension -> (dimension -> rank -> Double) -> rank -> rank -> Bool
paretoDominatesBy dimensions componentOf leftRank rightRank =
  let All componentwiseLeq =
        foldMap
          (\dimension -> All (componentOf dimension leftRank <= componentOf dimension rightRank))
          dimensions
      Any componentwiseLt =
        foldMap
          (\dimension -> Any (componentOf dimension leftRank < componentOf dimension rightRank))
          dimensions
   in componentwiseLeq && componentwiseLt

compareRankBy ::
  Foldable f =>
  RankMode dimension ->
  f dimension ->
  (dimension -> rank -> Double) ->
  (rank -> Double) ->
  rank ->
  rank ->
  Ordering
compareRankBy rankMode allDimensions componentOf totalOfRank leftRank rightRank =
  case rankMode of
    CompareByTotal ->
      compare (totalOfRank leftRank) (totalOfRank rightRank)
    CompareLexicographic rankDimensions ->
      foldMap
        (\dimension -> compare (componentOf dimension leftRank) (componentOf dimension rightRank))
        rankDimensions
    ComparePareto ->
      case
        ( paretoDominatesBy allDimensions componentOf leftRank rightRank,
          paretoDominatesBy allDimensions componentOf rightRank leftRank
        )
        of
        (True, False) -> LT
        (False, True) -> GT
        _ -> compare (totalOfRank leftRank) (totalOfRank rightRank)

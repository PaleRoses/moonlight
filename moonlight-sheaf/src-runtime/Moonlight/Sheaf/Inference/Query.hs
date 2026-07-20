{-# LANGUAGE RecordWildCards #-}

module Moonlight.Sheaf.Inference.Query where

import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (Down(..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector qualified as VB
import Moonlight.Sheaf.Inference.Algebra
import Moonlight.Sheaf.Inference.Types

selectEliminationOrder
  :: InferenceConfig
  -> WeightedBlueprint pid obj
  -> [Int]
selectEliminationOrder InferenceConfig{..} WeightedBlueprint{..} =
  chooseEliminationOrder
    icEliminationHeuristic
    (VB.length (diVars wbIndex))
    (VB.toList wbFactors)

inferLogZExact
  :: InferenceConfig
  -> WeightedBlueprint pid obj
  -> Either InferenceExecutionError Double
inferLogZExact cfg blueprint@WeightedBlueprint{..} =
  inferLogZWithOrder
    (selectEliminationOrder cfg blueprint)
    wbIndex
    (VB.toList wbFactors)

inferMapExact
  :: InferenceConfig
  -> WeightedBlueprint pid obj
  -> Either InferenceExecutionError (MapSolution pid obj)
inferMapExact cfg blueprint@WeightedBlueprint{..} =
  inferMapWithOrder
    (selectEliminationOrder cfg blueprint)
    wbIndex
    (VB.toList wbFactors)

inferMarginalsExact
  :: InferenceConfig
  -> WeightedBlueprint pid obj
  -> Either InferenceExecutionError (Map pid (Map obj Double))
inferMarginalsExact cfg blueprint@WeightedBlueprint{..} =
  marginalMapsFromWeights wbIndex . snd
    <$> inferLogZAndMarginalsWithOrder order wbIndex factorBase
  where
    order = selectEliminationOrder cfg blueprint
    factorBase = VB.toList wbFactors

inferPosteriorExact
  :: InferenceConfig
  -> WeightedBlueprint pid obj
  -> Either InferenceExecutionError (SectionPosterior pid obj)
inferPosteriorExact cfg blueprint@WeightedBlueprint{..} = do
  let order = selectEliminationOrder cfg blueprint
      factorBase = VB.toList wbFactors
  (logPartition, marginalWeights) <-
    inferLogZAndMarginalsWithOrder order wbIndex factorBase
  mapSolution <- inferMapWithOrder order wbIndex factorBase
  let marginals = marginalMapsFromWeights wbIndex marginalWeights
  pure
    SectionPosterior
      { spLogPartition = logPartition
      , spMarginals    = marginals
      , spMap          = mapSolution
      }

marginalMapsFromWeights
  :: DomainIndex pid obj
  -> VB.Vector [Double]
  -> Map pid (Map obj Double)
marginalMapsFromWeights index marginalWeights =
  Map.fromDistinctAscList
    ( VB.toList
        ( VB.zipWith3
            (\pid domain probabilities ->
              (pid, Map.fromDistinctAscList (zip (VB.toList domain) probabilities))
            )
            (diVars index)
            (diDomains index)
            marginalWeights
        )
    )

topKDomains
  :: Ord obj
  => TopKCount
  -> Map pid (Map obj Double)
  -> Map pid (Set obj)
topKDomains (TopKCount count) =
  fmap
    ( Set.fromList
    . map fst
    . take count
    . sortOn (Down . snd)
    . Map.toList
    )

mkTopKCount :: Int -> Either TopKCountError TopKCount
mkTopKCount count
  | count < 0 = Left (TopKCountNegative count)
  | otherwise = Right (TopKCount count)

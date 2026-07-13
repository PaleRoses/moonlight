{-# LANGUAGE RecordWildCards #-}

module Moonlight.Sheaf.Inference.Query where

import Data.List (sortOn)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
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
  :: Ord pid
  => InferenceConfig
  -> WeightedBlueprint pid obj
  -> Either InferenceExecutionError (MapSolution pid obj)
inferMapExact cfg blueprint@WeightedBlueprint{..} =
  inferMapWithOrder
    (selectEliminationOrder cfg blueprint)
    wbIndex
    (VB.toList wbFactors)

inferMarginalsExact
  :: (Ord pid, Ord obj)
  => InferenceConfig
  -> WeightedBlueprint pid obj
  -> Either InferenceExecutionError (Map pid (Map obj Double))
inferMarginalsExact cfg blueprint@WeightedBlueprint{..} =
  fmap
    (marginalMapsFromWeights wbIndex)
    (snd <$> inferLogZAndMarginalsWithOrder order wbIndex factorBase)
  where
    order = selectEliminationOrder cfg blueprint
    factorBase = VB.toList wbFactors

inferPosteriorExact
  :: (Ord pid, Ord obj)
  => InferenceConfig
  -> WeightedBlueprint pid obj
  -> Either InferenceExecutionError (SectionPosterior pid obj)
inferPosteriorExact cfg blueprint@WeightedBlueprint{..} = do
  let order = selectEliminationOrder cfg blueprint
      factorBase = VB.toList wbFactors
  (logPartition, marginalWeights) <-
    inferLogZAndMarginalsWithOrder order wbIndex factorBase
  mapSolution <- inferMapWithOrder order wbIndex factorBase
  let marginals =
        marginalMapsFromWeights wbIndex marginalWeights
  pure
    SectionPosterior
      { spLogPartition = logPartition
      , spMarginals    = marginals
      , spMap          = mapSolution
      }

marginalMapsFromWeights
  :: (Ord pid, Ord obj)
  => DomainIndex pid obj
  -> IntMap [Double]
  -> Map pid (Map obj Double)
marginalMapsFromWeights index marginalWeights =
  Map.fromList
    [ ( pid,
        Map.fromList
          ( zip
              (VB.toList domainV)
              (IntMap.findWithDefault (replicate (VB.length domainV) 0.0) varIdx marginalWeights)
          )
      )
    | varIdx <- [0 .. VB.length (diVars index) - 1],
      let pid = diVars index VB.! varIdx,
      let domainV = diDomains index VB.! varIdx
    ]

topKDomains
  :: Ord obj
  => Int
  -> Map pid (Map obj Double)
  -> Map pid (Set obj)
topKDomains k =
  fmap
    ( Set.fromList
    . map fst
    . take (max 1 k)
    . sortOn (Down . snd)
    . Map.toList
    )

module Moonlight.Derived.Pure.Dimension
  ( gradedKernelImageDims,
  )
where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Moonlight.Core (scanMap)

gradedKernelImageDims ::
  Int ->
  [Int] ->
  [Int] ->
  IntMap Int
gradedKernelImageDims startDegree objectDimensions differentialRanks =
  IntMap.fromAscList
    dimensionEntries
  where
    differentialRankByIndex =
      IntMap.fromAscList (zip [0 :: Int ..] differentialRanks)
    indexedDimensions =
      zip [0 :: Int ..] objectDimensions
    (_, dimensionEntries) = scanMap accumulateDimension 0 indexedDimensions
    accumulateDimension incomingRank (offset, objectDimension) =
      let outgoingRank =
            IntMap.findWithDefault 0 offset differentialRankByIndex
          cohomologyDimension =
            objectDimension - outgoingRank - incomingRank
       in ((outgoingRank), (startDegree + offset, cohomologyDimension))

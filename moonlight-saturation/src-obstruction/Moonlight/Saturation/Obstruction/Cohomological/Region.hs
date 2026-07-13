{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Saturation.Obstruction.Cohomological.Region
  ( RegionFoldAlgebra (..),
    regionFoldForRequest,
    regionFoldWith,
  )
where

import Control.Foldl qualified as Foldl
import Data.Kind (Type)

type RegionFoldAlgebra :: Type -> Type -> Type -> Type -> Type -> Type
data RegionFoldAlgebra cache request region summary aggregate = RegionFoldAlgebra
  { rfaAcceptRegion :: !(request -> region -> Bool),
    rfaAnalyzeRegion :: !(cache -> request -> region -> (cache, summary)),
    rfaInsertSummary :: !(request -> summary -> aggregate -> aggregate),
    rfaInitialAggregate :: !(request -> aggregate)
  }

regionFoldForRequest ::
  RegionFoldAlgebra cache request region summary aggregate ->
  cache ->
  request ->
  Foldl.Fold region (cache, aggregate)
regionFoldForRequest foldValue initialCache request =
  Foldl.Fold step (initialCache, rfaInitialAggregate foldValue request) id
  where
    step (cacheValue, aggregateValue) regionValue
      | rfaAcceptRegion foldValue request regionValue =
          let (updatedCache, regionSummary) =
                rfaAnalyzeRegion foldValue cacheValue request regionValue
              updatedAggregate =
                rfaInsertSummary foldValue request regionSummary aggregateValue
           in (updatedCache, updatedAggregate)
      | otherwise =
          (cacheValue, aggregateValue)
{-# INLINE regionFoldForRequest #-}

regionFoldWith ::
  (request -> region -> Bool) ->
  (cache -> request -> region -> (cache, summary)) ->
  (request -> summary -> aggregate -> aggregate) ->
  (request -> aggregate) ->
  cache ->
  request ->
  Foldl.Fold region (cache, aggregate)
regionFoldWith acceptRegion analyzeRegion insertSummary initialAggregate =
  regionFoldForRequest
    RegionFoldAlgebra
      { rfaAcceptRegion = acceptRegion,
        rfaAnalyzeRegion = analyzeRegion,
        rfaInsertSummary = insertSummary,
        rfaInitialAggregate = initialAggregate
      }
{-# INLINE regionFoldWith #-}

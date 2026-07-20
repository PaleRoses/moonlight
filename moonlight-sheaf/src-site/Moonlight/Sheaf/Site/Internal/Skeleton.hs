{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Sheaf.Site.Internal.Skeleton
  ( TruncatedSiteSkeleton (..),
    buildTruncatedSiteSkeletonWithPlan,
  )
where

import Data.Kind (Type)
import Data.List (mapAccumL)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Sheaf.Site.Internal.Face (faceMorphismsForCellWith)
import Moonlight.Sheaf.Site.Skeleton.RowSource
  ( NerveRowSource,
    SkeletonRowPlan,
    nerveFaceAt,
    nerveRowsAtDimension,
    skeletonRowPlanCellDimensions,
    skeletonRowPlanDepth,
    skeletonRowPlanFaceSourceDimensions,
    skeletonRowPlanSource,
  )
import Moonlight.Category.Simplicial
  ( NerveSimplex,
    nerveSimplexDimension,
  )
import Numeric.Natural (Natural)

type TruncatedSiteSkeleton :: Type -> Type -> Type -> Type
data TruncatedSiteSkeleton key cell face = TruncatedSiteSkeleton
  { tssCells :: [cell],
    tssCellsByDimension :: Map Natural [cell],
    tssCellsBySimplexKey :: Map key cell,
    tssFaceMorphisms :: [face]
  }
  deriving stock (Eq, Show)

buildTruncatedSiteSkeletonWithPlan ::
  Ord key =>
  SkeletonRowPlan category ->
  (Natural -> Int -> NerveSimplex category -> cell) ->
  (cell -> NerveSimplex category) ->
  (NerveSimplex category -> key) ->
  (Natural -> Natural -> faceKind) ->
  (cell -> cell -> faceKind -> Natural -> Int -> face) ->
  TruncatedSiteSkeleton key cell face
buildTruncatedSiteSkeletonWithPlan rowPlan mkCell cellSimplex simplexKey faceKindAt mkFace =
  let rowSource = skeletonRowPlanSource rowPlan
      depthValue = skeletonRowPlanDepth rowPlan
      cellDimensions = skeletonRowPlanCellDimensions rowPlan
      faceSourceDimensions = skeletonRowPlanFaceSourceDimensions rowPlan
      dimensionRows = fmap (simplicesAtDimensionWith rowSource) [0 .. depthValue]
      requiredCellDimensions =
        Set.unions
          [ cellDimensions,
            faceSourceDimensions,
            faceTargetDimensions faceSourceDimensions
          ]
      (_, cellsByDimensionList) =
        mapAccumL
          (cellsAtDimensionWindow requiredCellDimensions mkCell)
          0
          dimensionRows
      cellsByDimensionValue = Map.fromList cellsByDimensionList
      cellsValue = concatMap snd cellsByDimensionList
      cellsBySimplexKeyValue = Map.fromList (fmap cellBySimplexKey cellsValue)
      faceMorphismsValue =
        foldMap
          (faceMorphismsForCellWith (nerveFaceAt rowSource) cellSimplex simplexKey faceKindAt mkFace cellsBySimplexKeyValue)
          (filter (cellDimensionMember faceSourceDimensions) cellsValue)
   in TruncatedSiteSkeleton
        { tssCells = cellsValue,
          tssCellsByDimension = cellsByDimensionValue,
          tssCellsBySimplexKey = cellsBySimplexKeyValue,
          tssFaceMorphisms = faceMorphismsValue
        }
  where
    cellBySimplexKey cellValue = (simplexKey (cellSimplex cellValue), cellValue)
    cellDimensionMember dimensionsValue cellValue =
      Set.member (nerveSimplexDimension (cellSimplex cellValue)) dimensionsValue

faceTargetDimensions :: Set Natural -> Set Natural
faceTargetDimensions =
  Set.mapMonotonic pred . Set.delete 0

simplicesAtDimensionWith ::
  NerveRowSource category ->
  Natural ->
  (Natural, [NerveSimplex category])
simplicesAtDimensionWith rowSource dimensionValue =
  (dimensionValue, nerveRowsAtDimension rowSource dimensionValue)

cellsAtDimension ::
  (Natural -> Int -> simplex -> cell) ->
  Int ->
  (Natural, [simplex]) ->
  (Int, (Natural, [cell]))
cellsAtDimension mkCell nextOrdinalValue (dimensionValue, simplexValues) =
  let (nextOrdinalValue', cellValues) =
        mapAccumL
          (\ordinalValue simplexValue ->
              (ordinalValue + 1, mkCell dimensionValue ordinalValue simplexValue)
          )
          nextOrdinalValue
          simplexValues
   in (nextOrdinalValue', (dimensionValue, cellValues))

cellsAtDimensionWindow ::
  Set Natural ->
  (Natural -> Int -> simplex -> cell) ->
  Int ->
  (Natural, [simplex]) ->
  (Int, (Natural, [cell]))
cellsAtDimensionWindow requiredCellDimensions mkCell nextOrdinalValue (dimensionValue, simplexValues)
  | Set.member dimensionValue requiredCellDimensions =
      cellsAtDimension mkCell nextOrdinalValue (dimensionValue, simplexValues)
  | otherwise =
      (nextOrdinalValue + length simplexValues, (dimensionValue, []))

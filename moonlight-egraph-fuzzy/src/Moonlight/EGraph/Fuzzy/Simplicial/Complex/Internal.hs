{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Moonlight.EGraph.Fuzzy.Simplicial.Complex.Internal
  ( SimplexId (..),
    ParallelTagFingerprint (..),
    EdgeKind (..),
    EdgeData (..),
    FaceData (..),
    PatternCell (..),
    EGraphCell (..),
    ComplexBuilder (..),
    TruncatedFaceComplex,
    orderedPair,
    pairwise,
    adjacentPairs,
    safeIndex,
    maxDimensionOf,
    emptyComplexBuilder,
    freshCoreSimplex,
    recordCoreFace,
    coreToFaceComplex,
    faceComplexSimplicesAtDimension,
    faceComplexFaceAtDimension,
    patternDegenerate,
    egraphDegenerate,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Function ((&))
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Core (adjacentPairs, pairwise, safeIndex)
import Numeric.Natural (Natural)
import Prelude

type SimplexId :: Type
newtype SimplexId = SimplexId
  { simplexIdKey :: Int
  }
  deriving stock (Eq, Ord, Show, Read)

type ParallelTagFingerprint :: Type
newtype ParallelTagFingerprint = ParallelTagFingerprint
  { parallelTagFingerprintKey :: Int
  }
  deriving stock (Eq, Ord, Show, Read)

type EdgeKind :: Type -> Type
data EdgeKind tag
  = ChildEdge !tag !Int
  | ParallelEdge !tag !Int !Int
  deriving stock (Eq, Ord, Show)

type EdgeData :: Type -> Type
data EdgeData tag = EdgeData
  { edSource :: !SimplexId,
    edTarget :: !SimplexId,
    edKind :: !(EdgeKind tag)
  }

type FaceData :: Type
data FaceData = FaceData
  { fdV0 :: !SimplexId,
    fdV1 :: !SimplexId,
    fdV2 :: !SimplexId,
    fdTagFingerprint :: !ParallelTagFingerprint,
    fdLeftSlot :: !Int,
    fdRightSlot :: !Int
  }

type PatternCell :: Type -> Type
data PatternCell tag
  = PatternVertexCell
  | PatternEdgeCell !(EdgeData tag)
  | PatternFaceCell !FaceData

type EGraphCell :: Type -> Type
data EGraphCell tag
  = EGraphVertexCell
  | EGraphEdgeCell !(EdgeData tag)
  | EGraphFaceCell !FaceData

type ComplexBuilder :: Type -> Type
data ComplexBuilder cell = ComplexBuilder
  { cbNextId :: !Int,
    cbRows :: !(Map Natural [SimplexId]),
    cbFaces :: !(IntMap (IntMap SimplexId)),
    cbCells :: !(IntMap cell)
  }

type TruncatedFaceComplex :: Type
data TruncatedFaceComplex = TruncatedFaceComplex
  { faceComplexUpperBound :: !Natural,
    faceComplexRows :: !(Map Natural [SimplexId]),
    faceComplexFaces :: !(IntMap (IntMap SimplexId))
  }

orderedPair :: Ord a => a -> a -> (a, a)
orderedPair leftValue rightValue =
  if leftValue <= rightValue
    then (leftValue, rightValue)
    else (rightValue, leftValue)

maxDimensionOf :: Map Natural [SimplexId] -> Natural
maxDimensionOf rows =
  if Map.null rows
    then 0
    else fst (Map.findMax rows)

emptyComplexBuilder :: ComplexBuilder cell
emptyComplexBuilder =
  ComplexBuilder
    { cbNextId = 0,
      cbRows = Map.empty,
      cbFaces = IntMap.empty,
      cbCells = IntMap.empty
    }

freshCoreSimplex :: Natural -> cell -> ComplexBuilder cell -> (SimplexId, ComplexBuilder cell)
freshCoreSimplex dimensionValue cellValue builder =
  let nextId = cbNextId builder
      simplexId = SimplexId nextId
   in ( simplexId,
        builder
          { cbNextId = nextId + 1,
            cbRows = Map.insertWith (<>) dimensionValue [simplexId] (cbRows builder),
            cbCells = IntMap.insert nextId cellValue (cbCells builder)
          }
      )

recordCoreFace :: SimplexId -> Int -> SimplexId -> ComplexBuilder cell -> ComplexBuilder cell
recordCoreFace simplexId faceIndex boundaryId builder =
  builder
    { cbFaces =
        IntMap.insertWith
          IntMap.union
          (simplexIdKey simplexId)
          (IntMap.singleton faceIndex boundaryId)
          (cbFaces builder)
    }

coreToFaceComplex ::
  Natural ->
  ComplexBuilder cell ->
  (SimplexId -> Bool) ->
  TruncatedFaceComplex
coreToFaceComplex upperBound builder isDegenerate =
  TruncatedFaceComplex
    { faceComplexUpperBound = max upperBound (maxDimensionOf (cbRows builder)),
      faceComplexRows =
        cbRows builder
          & Map.map (filter (not . isDegenerate))
          & Map.filter (not . null),
      faceComplexFaces = cbFaces builder
    }

faceComplexSimplicesAtDimension :: TruncatedFaceComplex -> Natural -> [SimplexId]
faceComplexSimplicesAtDimension complex dimensionValue =
  Map.findWithDefault [] dimensionValue (faceComplexRows complex)

faceComplexFaceAtDimension :: TruncatedFaceComplex -> Natural -> Natural -> SimplexId -> Maybe SimplexId
faceComplexFaceAtDimension complex simplexDimension faceIndex simplexId
  | simplexDimension == 0 = Nothing
  | simplexDimension > faceComplexUpperBound complex = Nothing
  | faceIndex > simplexDimension = Nothing
  | otherwise =
      IntMap.lookup (simplexIdKey simplexId) (faceComplexFaces complex)
        >>= IntMap.lookup (fromIntegral faceIndex)

patternDegenerate :: IntMap (PatternCell tag) -> SimplexId -> Bool
patternDegenerate cells simplexId =
  case IntMap.lookup (simplexIdKey simplexId) cells of
    Just PatternVertexCell ->
      False
    Just (PatternEdgeCell EdgeData {..}) ->
      edSource == edTarget
    Just (PatternFaceCell FaceData {..}) ->
      fdV0 == fdV1 || fdV1 == fdV2 || fdV0 == fdV2
    Nothing ->
      False

egraphDegenerate :: IntMap (EGraphCell tag) -> SimplexId -> Bool
egraphDegenerate cells simplexId =
  case IntMap.lookup (simplexIdKey simplexId) cells of
    Just EGraphVertexCell ->
      False
    Just (EGraphEdgeCell EdgeData {..}) ->
      edSource == edTarget
    Just (EGraphFaceCell FaceData {..}) ->
      fdV0 == fdV1 || fdV1 == fdV2 || fdV0 == fdV2
    Nothing ->
      False

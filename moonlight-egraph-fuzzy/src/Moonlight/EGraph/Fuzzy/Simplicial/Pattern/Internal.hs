{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Moonlight.EGraph.Fuzzy.Simplicial.Pattern.Internal
  ( PatternLiftAlgebra (..),
    PatternFrame (..),
    ChildFrame (..),
    patternFrameVertex,
    SimplicialPattern (..),
    liftPatternSimplicially,
    patternTriangleWellFormed,
  )
where

import Control.Monad.Trans.State.Strict (State, gets, modify', runState)
import Data.Foldable (toList)
import Data.Kind (Type)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Moonlight.EGraph.Fuzzy.Simplicial.Complex.Internal
  ( ComplexBuilder (..),
    EdgeData (..),
    EdgeKind (..),
    FaceData (..),
    ParallelTagFingerprint,
    PatternCell (..),
    SimplexId,
    TruncatedFaceComplex,
    adjacentPairs,
    coreToFaceComplex,
    emptyComplexBuilder,
    faceComplexFaceAtDimension,
    freshCoreSimplex,
    maxDimensionOf,
    patternDegenerate,
    recordCoreFace,
  )
import Moonlight.EGraph.Fuzzy.Simplicial.Shape
  ( ParallelRequirement (..),
    TriangleRequirement (..),
  )
import Moonlight.Core (HasConstructorTag (..))
import Moonlight.Core
  ( Pattern (..)
  )
import Moonlight.Core qualified as EGraph
import Numeric.Natural (Natural)
import Prelude

type PatternFrame :: (Type -> Type) -> Type
data PatternFrame f
  = PatternVarFrame !SimplexId !EGraph.PatternVar
  | PatternNodeFrame !SimplexId !(ConstructorTag f) !(f (Pattern f)) !(IntMap (ChildFrame f)) ![ParallelRequirement]

patternFrameVertex :: PatternFrame f -> SimplexId
patternFrameVertex frame =
  case frame of
    PatternVarFrame simplexId _ -> simplexId
    PatternNodeFrame simplexId _ _ _ _ -> simplexId

type ChildFrame :: (Type -> Type) -> Type
data ChildFrame f = ChildFrame
  { childFrameEdge :: !SimplexId,
    childFrameSubpattern :: !(PatternFrame f)
  }

type SimplicialPattern :: (Type -> Type) -> Type
data SimplicialPattern f = SimplicialPattern
  { spRootFrame :: !(PatternFrame f),
    spNondegenerate :: !TruncatedFaceComplex,
    spRequiredDimension :: !Natural,
    spFaceCount :: !Int
  }

type PatternLiftAlgebra :: (Type -> Type) -> Type
data PatternLiftAlgebra f = PatternLiftAlgebra
  { plaUpperBound :: !Natural,
    plaParallelBlocksFor :: ConstructorTag f -> Int -> [IntSet],
    plaCanonicalPatternBlockOrder :: f (Pattern f) -> [Int] -> [Int],
    plaTagFingerprint :: ConstructorTag f -> ParallelTagFingerprint
  }

type PatternBuilder :: Type -> Type
data PatternBuilder tag = PatternBuilder
  { pbCore :: !(ComplexBuilder (PatternCell tag)),
    pbVarVertices :: !(Map EGraph.PatternVar SimplexId)
  }

liftPatternSimplicially ::
  forall f.
  HasConstructorTag f =>
  PatternLiftAlgebra f ->
  Pattern f ->
  SimplicialPattern f
liftPatternSimplicially patternLiftAlgebra patternValue =
  let initial :: PatternBuilder (ConstructorTag f)
      initial =
        PatternBuilder
          { pbCore = emptyComplexBuilder,
            pbVarVertices = Map.empty
          }
      (rootFrame, finalState) = runState (buildPatternFrame patternLiftAlgebra patternValue) initial
      upperBound = max 2 (plaUpperBound patternLiftAlgebra)
      simplicialSet =
        coreToFaceComplex
          upperBound
          (pbCore finalState)
          (patternDegenerate (cbCells (pbCore finalState)))
   in SimplicialPattern
        { spRootFrame = rootFrame,
          spNondegenerate = simplicialSet,
          spRequiredDimension = maxDimensionOf (cbRows (pbCore finalState)),
          spFaceCount = length (Map.findWithDefault [] 2 (cbRows (pbCore finalState)))
        }

buildPatternFrame ::
  forall f.
  HasConstructorTag f =>
  PatternLiftAlgebra f ->
  Pattern f ->
  State (PatternBuilder (ConstructorTag f)) (PatternFrame f)
buildPatternFrame patternLiftAlgebra patternValue =
  case patternValue of
    PatternVar patternVar -> do
      vertex <- ensurePatternVarVertex patternVar
      pure (PatternVarFrame vertex patternVar)
    PatternNode patternNode -> do
      parentVertex <- freshPatternSimplex 0 PatternVertexCell
      let tag = constructorTag patternNode
      builtChildren <-
        traverse
          ( \(slot, childPattern) -> do
              childFrame <- buildPatternFrame patternLiftAlgebra childPattern
              edgeId <-
                freshPatternSimplex
                  1
                  ( PatternEdgeCell
                      EdgeData
                        { edSource = parentVertex,
                          edTarget = patternFrameVertex childFrame,
                          edKind = ChildEdge tag slot
                        }
                  )
              registerPatternFace edgeId 0 (patternFrameVertex childFrame)
              registerPatternFace edgeId 1 parentVertex
              pure (slot, ChildFrame edgeId childFrame)
          )
          (zip [0 ..] (toList patternNode))
      let childMap = IntMap.fromList builtChildren
      parallelRequirements <-
        traverse
          (mkPatternParallelRequirement patternLiftAlgebra patternNode parentVertex childMap tag)
          (plaParallelBlocksFor patternLiftAlgebra tag (length builtChildren))
      pure (PatternNodeFrame parentVertex tag patternNode childMap parallelRequirements)

mkPatternParallelRequirement ::
  PatternLiftAlgebra f ->
  f (Pattern f) ->
  SimplexId ->
  IntMap (ChildFrame f) ->
  ConstructorTag f ->
  IntSet ->
  State (PatternBuilder (ConstructorTag f)) ParallelRequirement
mkPatternParallelRequirement patternLiftAlgebra patternNode parentVertex childMap tag rawSlotSet = do
  let rawSlots = IntSet.toAscList rawSlotSet
      canonicalSlots = plaCanonicalPatternBlockOrder patternLiftAlgebra patternNode rawSlots
      trianglePairs = adjacentPairs canonicalSlots
  triangles <- fmap catMaybes (traverse buildTriangle trianglePairs)
  pure
    ParallelRequirement
      { prRawSlots = rawSlots,
        prCanonicalPatternSlots = canonicalSlots,
        prTriangles = triangles
      }
  where
    buildTriangle (leftSlot, rightSlot) =
      case (IntMap.lookup leftSlot childMap, IntMap.lookup rightSlot childMap) of
        (Just leftChild, Just rightChild) ->
          let leftVertex = patternFrameVertex (childFrameSubpattern leftChild)
              rightVertex = patternFrameVertex (childFrameSubpattern rightChild)
           in if leftVertex == rightVertex
                then pure Nothing
                else do
                  parallelEdgeId <-
                    freshPatternSimplex
                      1
                      ( PatternEdgeCell
                          EdgeData
                            { edSource = leftVertex,
                              edTarget = rightVertex,
                              edKind = ParallelEdge tag leftSlot rightSlot
                            }
                      )
                  registerPatternFace parallelEdgeId 0 rightVertex
                  registerPatternFace parallelEdgeId 1 leftVertex
                  faceId <-
                    freshPatternSimplex
                      2
                      ( PatternFaceCell
                          FaceData
                            { fdV0 = parentVertex,
                              fdV1 = leftVertex,
                              fdV2 = rightVertex,
                              fdTagFingerprint = plaTagFingerprint patternLiftAlgebra tag,
                              fdLeftSlot = leftSlot,
                              fdRightSlot = rightSlot
                            }
                      )
                  registerPatternFace faceId 0 parallelEdgeId
                  registerPatternFace faceId 1 (childFrameEdge rightChild)
                  registerPatternFace faceId 2 (childFrameEdge leftChild)
                  pure
                    ( Just
                        TriangleRequirement
                          { trLeftSlot = leftSlot,
                            trRightSlot = rightSlot,
                            trFaceId = faceId,
                            trBoundary0 = parallelEdgeId,
                            trBoundary1 = childFrameEdge rightChild,
                            trBoundary2 = childFrameEdge leftChild
                          }
                    )
        _ ->
          pure Nothing

patternTriangleWellFormed :: SimplicialPattern f -> TriangleRequirement -> Bool
patternTriangleWellFormed simplicialPattern TriangleRequirement {..} =
  faceComplexFaceAtDimension (spNondegenerate simplicialPattern) 2 0 trFaceId == Just trBoundary0
    && faceComplexFaceAtDimension (spNondegenerate simplicialPattern) 2 1 trFaceId == Just trBoundary1
    && faceComplexFaceAtDimension (spNondegenerate simplicialPattern) 2 2 trFaceId == Just trBoundary2

freshPatternSimplex :: Natural -> PatternCell tag -> State (PatternBuilder tag) SimplexId
freshPatternSimplex dimensionValue cellValue = do
  builder <- gets pbCore
  let (simplexId, builder') = freshCoreSimplex dimensionValue cellValue builder
  modify' (\state -> state {pbCore = builder'})
  pure simplexId

registerPatternFace :: SimplexId -> Int -> SimplexId -> State (PatternBuilder tag) ()
registerPatternFace simplexId faceIndex boundaryId =
  modify' (\state -> state {pbCore = recordCoreFace simplexId faceIndex boundaryId (pbCore state)})

ensurePatternVarVertex :: EGraph.PatternVar -> State (PatternBuilder tag) SimplexId
ensurePatternVarVertex patternVar = do
  existing <- gets (Map.lookup patternVar . pbVarVertices)
  case existing of
    Just simplexId ->
      pure simplexId
    Nothing -> do
      simplexId <- freshPatternSimplex 0 PatternVertexCell
      modify' (\state -> state {pbVarVertices = Map.insert patternVar simplexId (pbVarVertices state)})
      pure simplexId

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Moonlight.EGraph.Fuzzy.Simplicial.Graph.Internal
  ( EGraphLiftAlgebra (..),
    LiftedEGraph (..),
    liftEGraphSimplicially,
    canonicalClassIds,
  )
where

import Control.Monad.Trans.State.Strict (State, gets, modify', runState)
import Data.Function ((&))
import Data.Foldable (toList, traverse_)
import Data.Kind (Type)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.EGraph.Fuzzy.Simplicial.Complex.Internal
  ( ComplexBuilder (..),
    EGraphCell (..),
    EdgeData (..),
    EdgeKind (..),
    FaceData (..),
    ParallelTagFingerprint,
    SimplexId,
    TruncatedFaceComplex,
    adjacentPairs,
    coreToFaceComplex,
    egraphDegenerate,
    emptyComplexBuilder,
    freshCoreSimplex,
    recordCoreFace,
    safeIndex,
  )
import Moonlight.EGraph.Fuzzy.Simplicial.Shape
  ( ParallelFaceKey,
  )
import Moonlight.Core (HasConstructorTag (..), Language)
import Moonlight.EGraph.Pure.Types
  ( ClassId,
    EClass (..),
    EGraph,
    ENode (..),
    canonicalizeClassId,
    materializeEGraphClasses,
  )
import Numeric.Natural (Natural)
import Prelude

type LiftedEGraph :: (Type -> Type) -> Type -> Type
data LiftedEGraph f a = LiftedEGraph
  { leBaseGraph :: !(EGraph f a),
    leNondegenerate :: !TruncatedFaceComplex,
    leRootsByTag :: !(Map (ConstructorTag f) [ClassId]),
    leEagerFaces :: !(Map ParallelFaceKey SimplexId),
    leDeferredFaces :: !Bool
  }

type EGraphLiftAlgebra :: (Type -> Type) -> Type
data EGraphLiftAlgebra f = EGraphLiftAlgebra
  { glaUpperBound :: !Natural,
    glaDeferredFaces :: !Bool,
    glaParallelBlocksFor :: ConstructorTag f -> Int -> [IntSet],
    glaCanonicalClassBlockOrder :: f ClassId -> [Int] -> [Int],
    glaParallelFaceKey :: ClassId -> ConstructorTag f -> Int -> Int -> ClassId -> ClassId -> ParallelFaceKey,
    glaTagFingerprint :: ConstructorTag f -> ParallelTagFingerprint
  }

type EGraphBuilder :: Type -> Type
data EGraphBuilder tag = EGraphBuilder
  { ebCore :: !(ComplexBuilder (EGraphCell tag)),
    ebVertices :: !(Map ClassId SimplexId),
    ebEdges :: !(Map (ClassId, ClassId, EdgeKind tag) SimplexId),
    ebRootsByTag :: !(Map tag (Set ClassId)),
    ebEagerFaces :: !(Map ParallelFaceKey SimplexId)
  }

liftEGraphSimplicially ::
  forall f a.
  HasConstructorTag f =>
  EGraphLiftAlgebra f ->
  EGraph f a ->
  LiftedEGraph f a
liftEGraphSimplicially egraphLiftAlgebra graph =
  let initial :: EGraphBuilder (ConstructorTag f)
      initial =
        EGraphBuilder
          { ebCore = emptyComplexBuilder,
            ebVertices = Map.empty,
            ebEdges = Map.empty,
            ebRootsByTag = Map.empty,
            ebEagerFaces = Map.empty
          }
      finalState =
        canonicalClassIds graph
          & foldl'
            (\builder classId -> snd (runState (ensureGraphVertex classId) builder))
            initial
          & \builder ->
            IntMap.foldl'
              ( \acc eClass ->
                  let parentClass = canonicalizeClassId graph (eClassId eClass)
                   in Set.toAscList (eClassNodes eClass)
                        & foldl'
                          (\acc' enode -> snd (runState (ingestENode egraphLiftAlgebra graph parentClass enode) acc'))
                          acc
              )
              builder
              (materializeEGraphClasses graph)
      upperBound = max 2 (glaUpperBound egraphLiftAlgebra)
      simplicialSet =
        coreToFaceComplex
          upperBound
          (ebCore finalState)
          (egraphDegenerate (cbCells (ebCore finalState)))
   in LiftedEGraph
        { leBaseGraph = graph,
          leNondegenerate = simplicialSet,
          leRootsByTag = fmap Set.toAscList (ebRootsByTag finalState),
          leEagerFaces = ebEagerFaces finalState,
          leDeferredFaces = glaDeferredFaces egraphLiftAlgebra
        }

ingestENode ::
  forall f a.
  HasConstructorTag f =>
  EGraphLiftAlgebra f ->
  EGraph f a ->
  ClassId ->
  ENode f ->
  State (EGraphBuilder (ConstructorTag f)) ()
ingestENode egraphLiftAlgebra graph parentClassId (ENode nodeChildren) = do
  let canonicalParent = canonicalizeClassId graph parentClassId
      tag = constructorTag nodeChildren
      canonicalChildren = fmap (canonicalizeClassId graph) (toList nodeChildren)
  modify'
    ( \builder ->
        builder
          { ebRootsByTag =
              Map.insertWith Set.union tag (Set.singleton canonicalParent) (ebRootsByTag builder)
          }
    )
  parentVertex <- ensureGraphVertex canonicalParent
  childEdges <-
    traverse
      ( \(slot, childClassId) -> do
          childVertex <- ensureGraphVertex childClassId
          ensureGraphEdge canonicalParent childClassId parentVertex childVertex (ChildEdge tag slot)
      )
      (zip [0 ..] canonicalChildren)
  if glaDeferredFaces egraphLiftAlgebra
    then pure ()
    else
      traverse_
        (\rawSlots -> traverse_ (ingestParallelPair canonicalParent rawSlots childEdges canonicalChildren parentVertex tag) (adjacentPairs rawSlots))
        (fmap (glaCanonicalClassBlockOrder egraphLiftAlgebra nodeChildren . IntSet.toAscList) (glaParallelBlocksFor egraphLiftAlgebra tag (length canonicalChildren)))
  where
    ingestParallelPair canonicalParent _ childEdges canonicalChildren parentVertex tag (leftSlot, rightSlot) =
      case (safeIndex leftSlot canonicalChildren, safeIndex rightSlot canonicalChildren) of
        (Just leftChild, Just rightChild)
          | leftChild == rightChild ->
              pure ()
          | otherwise -> do
              leftVertex <- ensureGraphVertex leftChild
              rightVertex <- ensureGraphVertex rightChild
              parallelEdge <- ensureGraphEdge leftChild rightChild leftVertex rightVertex (ParallelEdge tag leftSlot rightSlot)
              let faceKey = glaParallelFaceKey egraphLiftAlgebra canonicalParent tag leftSlot rightSlot leftChild rightChild
              existing <- gets (Map.lookup faceKey . ebEagerFaces)
              case existing of
                Just _ ->
                  pure ()
                Nothing -> do
                  faceId <-
                    freshGraphSimplex
                      2
                      ( EGraphFaceCell
                          FaceData
                            { fdV0 = parentVertex,
                              fdV1 = leftVertex,
                              fdV2 = rightVertex,
                              fdTagFingerprint = glaTagFingerprint egraphLiftAlgebra tag,
                              fdLeftSlot = leftSlot,
                              fdRightSlot = rightSlot
                            }
                      )
                  registerGraphFace faceId 0 parallelEdge
                  registerGraphFace faceId 1 (maybe parallelEdge id (safeIndex leftSlot childEdges))
                  registerGraphFace faceId 2 (maybe parallelEdge id (safeIndex rightSlot childEdges))
                  modify' (\builder -> builder {ebEagerFaces = Map.insert faceKey faceId (ebEagerFaces builder)})
        _ ->
          pure ()

canonicalClassIds :: Language f => EGraph f a -> [ClassId]
canonicalClassIds graph =
  IntMap.elems (materializeEGraphClasses graph)
    & fmap (canonicalizeClassId graph . eClassId)
    & Set.fromList
    & Set.toAscList

freshGraphSimplex :: Natural -> EGraphCell tag -> State (EGraphBuilder tag) SimplexId
freshGraphSimplex dimensionValue cellValue = do
  builder <- gets ebCore
  let (simplexId, builder') = freshCoreSimplex dimensionValue cellValue builder
  modify' (\state -> state {ebCore = builder'})
  pure simplexId

registerGraphFace :: SimplexId -> Int -> SimplexId -> State (EGraphBuilder tag) ()
registerGraphFace simplexId faceIndex boundaryId =
  modify' (\state -> state {ebCore = recordCoreFace simplexId faceIndex boundaryId (ebCore state)})

ensureGraphVertex :: ClassId -> State (EGraphBuilder tag) SimplexId
ensureGraphVertex classId = do
  existing <- gets (Map.lookup classId . ebVertices)
  case existing of
    Just simplexId ->
      pure simplexId
    Nothing -> do
      simplexId <- freshGraphSimplex 0 EGraphVertexCell
      modify' (\state -> state {ebVertices = Map.insert classId simplexId (ebVertices state)})
      pure simplexId

ensureGraphEdge ::
  Ord tag =>
  ClassId ->
  ClassId ->
  SimplexId ->
  SimplexId ->
  EdgeKind tag ->
  State (EGraphBuilder tag) SimplexId
ensureGraphEdge sourceClass targetClass sourceVertex targetVertex edgeKind = do
  existing <- gets (Map.lookup (sourceClass, targetClass, edgeKind) . ebEdges)
  case existing of
    Just simplexId ->
      pure simplexId
    Nothing -> do
      simplexId <-
        freshGraphSimplex
          1
          ( EGraphEdgeCell
              EdgeData
                { edSource = sourceVertex,
                  edTarget = targetVertex,
                  edKind = edgeKind
                }
          )
      registerGraphFace simplexId 0 targetVertex
      registerGraphFace simplexId 1 sourceVertex
      modify' (\state -> state {ebEdges = Map.insert (sourceClass, targetClass, edgeKind) simplexId (ebEdges state)})
      pure simplexId

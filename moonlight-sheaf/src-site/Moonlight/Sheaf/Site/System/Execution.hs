{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Moonlight.Sheaf.Site.System.Execution
  ( ExecutionTransition (..),
    ExecutionVertex (..),
    ExecutionComplex (..),
    ExecutionComplexError (..),
    executionComplex,
    executionTransitionCount,
    executionVertexCount,
  )
where

import Data.Containers.ListUtils (nubOrd)
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Homology.Topology (Graph1Skeleton (..), GraphEdge (..))
import Moonlight.Sheaf.Site.System (AnalyzableSystem (..))

type ExecutionVertex :: Type -> Type
data ExecutionVertex system = ExecutionVertex
  { evContext :: SystemCtx system,
    evObject :: SystemOb system
  }

type ExecutionTransition :: Type -> Type
data ExecutionTransition system = ExecutionTransition
  { etContext :: SystemCtx system,
    etMorphism :: SystemMor system,
    etSource :: ExecutionVertex system,
    etTarget :: ExecutionVertex system
  }

type ExecutionComplex :: Type -> Type
data ExecutionComplex system = ExecutionComplex
  { recVertices :: [ExecutionVertex system],
    recTransitions :: [ExecutionTransition system],
    recUndirectedSupportSkeleton :: Graph1Skeleton
  }

type ExecutionComplexError :: Type
data ExecutionComplexError
  = ExecutionTransitionEndpointUnindexed
  deriving stock (Eq, Ord, Show)

deriving stock instance (Eq (SystemCtx system), Eq (SystemOb system)) => Eq (ExecutionVertex system)
deriving stock instance (Ord (SystemCtx system), Ord (SystemOb system)) => Ord (ExecutionVertex system)
deriving stock instance (Show (SystemCtx system), Show (SystemOb system)) => Show (ExecutionVertex system)

instance (Eq (SystemCtx system), Eq (SystemOb system), Eq (SystemMor system)) => Eq (ExecutionTransition system) where
  leftTransition == rightTransition =
    ( etContext leftTransition,
      etMorphism leftTransition,
      etSource leftTransition,
      etTarget leftTransition
    )
      ==
    ( etContext rightTransition,
      etMorphism rightTransition,
      etSource rightTransition,
      etTarget rightTransition
    )

instance (Ord (SystemCtx system), Ord (SystemOb system), Ord (SystemMor system)) => Ord (ExecutionTransition system) where
  compare leftTransition rightTransition =
    compare
      ( etContext leftTransition,
        etMorphism leftTransition,
        etSource leftTransition,
        etTarget leftTransition
      )
      ( etContext rightTransition,
        etMorphism rightTransition,
        etSource rightTransition,
        etTarget rightTransition
      )

instance (Show (SystemCtx system), Show (SystemOb system), Show (SystemMor system)) => Show (ExecutionTransition system) where
  show transitionValue =
    show
      ( etContext transitionValue,
        etMorphism transitionValue,
        etSource transitionValue,
        etTarget transitionValue
      )

executionComplex ::
  AnalyzableSystem system =>
  system ->
  Either ExecutionComplexError (ExecutionComplex system)
executionComplex systemValue = do
  let transitions = executionTransitions systemValue
      vertices =
        nubOrd
          ( executionVertices systemValue
              <> foldMap transitionEndpoints transitions
          )
      vertexIndices =
        Map.fromList
          (zip vertices [0 :: Int ..])
  edgeSupports <- transitionEdgeSupports vertexIndices transitions
  pure
    ExecutionComplex
      { recVertices = vertices,
        recTransitions = transitions,
        recUndirectedSupportSkeleton = executionSkeleton (length vertices) edgeSupports
      }

executionVertexCount :: ExecutionComplex system -> Int
executionVertexCount =
  length . recVertices

executionTransitionCount :: ExecutionComplex system -> Int
executionTransitionCount =
  length . recTransitions

executionVertices :: AnalyzableSystem system => system -> [ExecutionVertex system]
executionVertices systemValue =
  nubOrd
    [ ExecutionVertex contextValue objectValue
    | contextValue <- allContexts systemValue,
      objectValue <- systemObjectsInContext systemValue contextValue
    ]

executionTransitions :: AnalyzableSystem system => system -> [ExecutionTransition system]
executionTransitions systemValue =
  nubOrd
    [ transitionAt contextValue morphismValue
    | contextValue <- allContexts systemValue,
      morphismValue <- systemMorphismsInContext systemValue contextValue
    ]
  where
    transitionAt contextValue morphismValue =
      let normalizedMorphism = normalizeMorphism systemValue contextValue morphismValue
       in ExecutionTransition
            { etContext = contextValue,
              etMorphism = normalizedMorphism,
              etSource = ExecutionVertex contextValue (morphismSource systemValue normalizedMorphism),
              etTarget = ExecutionVertex contextValue (morphismTarget systemValue normalizedMorphism)
            }

transitionEndpoints :: ExecutionTransition system -> [ExecutionVertex system]
transitionEndpoints transitionValue =
  [etSource transitionValue, etTarget transitionValue]

transitionEdgeSupports ::
  Ord (ExecutionVertex system) =>
  Map.Map (ExecutionVertex system) Int ->
  [ExecutionTransition system] ->
  Either ExecutionComplexError [(Int, Int)]
transitionEdgeSupports vertexIndices =
  fmap (Set.toAscList . Set.fromList)
    . traverse (transitionEdgeSupport vertexIndices)

transitionEdgeSupport ::
  Ord (ExecutionVertex system) =>
  Map.Map (ExecutionVertex system) Int ->
  ExecutionTransition system ->
  Either ExecutionComplexError (Int, Int)
transitionEdgeSupport vertexIndices transitionValue =
  case (Map.lookup (etSource transitionValue) vertexIndices, Map.lookup (etTarget transitionValue) vertexIndices) of
    (Just sourceIndex, Just targetIndex) ->
      Right (sourceIndex, targetIndex)
    _ ->
      Left ExecutionTransitionEndpointUnindexed

executionSkeleton :: Int -> [(Int, Int)] -> Graph1Skeleton
executionSkeleton vertexCount edgeSupports =
  let edges =
        fmap
          ( \(edgeIndexValue, (sourceVertex, targetVertex)) ->
              GraphEdge
                { graphEdgeIndex = edgeIndexValue,
                  graphEdgeSource = sourceVertex,
                  graphEdgeTarget = targetVertex
                }
          )
          (zip [0 :: Int ..] edgeSupports)
      edgeAdjacency =
        foldr
          ( \edgeValue ->
              Map.insertWith (<>) (graphEdgeSource edgeValue) [edgeValue]
                . Map.insertWith (<>) (graphEdgeTarget edgeValue) [edgeValue]
          )
          (Map.fromList (fmap (\vertex -> (vertex, [])) [0 :: Int .. vertexCount - 1]))
          edges
   in Graph1Skeleton
        { graphVertexCount = vertexCount,
          graphEdges = edges,
          graphEdgeAdjacency = edgeAdjacency
        }

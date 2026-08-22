{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}

-- | The singleton segment verbs: glue one segment or a polyline, and retire a
-- constraint, each inside one sealed transaction.
module Moonlight.Triangulation.Internal.Cdt.Segment
  ( addConstraintEdge
  , addConstraintEdges
  , removeConstraintEdge
  , retireConstraintEdge
  ) where

import Control.Monad.ST (ST)
import Data.Bifunctor (first)
import qualified Data.Vector as V
import Data.Primitive.PrimArray (indexPrimArray, sizeofPrimArray)
import Moonlight.Triangulation.BulkLoad (insertMany)
import qualified Moonlight.Triangulation.Dcel as Dcel
import Moonlight.Triangulation.Handles.HandleDefs
import Moonlight.Triangulation.Internal.Cdt.Admission
  ( ConstraintAdmission (..)
  , constraintAdmission
  )
import Moonlight.Triangulation.Internal.Cdt.Batch (recoverConstraints)
import Moonlight.Triangulation.Internal.Cdt.Combinators
  ( asConstraintStep
  , bindMutable
  )
import Moonlight.Triangulation.Internal.Cdt.Recovery (applyMutableConstraint)
import Moonlight.Triangulation.Internal.Cdt.Site
  ( lookupExistingConstraintEndpoint
  , placeConstraintEndpoint
  )
import Moonlight.Triangulation.Internal.Cdt.Types
import Moonlight.Triangulation.Internal.DcelOperations.Legalize (legalizeEdges)
import Moonlight.Triangulation.Internal.Growable
  ( GrowableWord32
  , newGrowableWord32
  )
import Moonlight.Triangulation.Internal.Mutable
import Moonlight.Triangulation.Internal.OperationState (OperationState)
import Moonlight.Triangulation.Internal.Paged (TransactionShape (LocalTransaction))
import Moonlight.Triangulation.Internal.Representation
import Moonlight.Triangulation.Internal.Transaction (runUnmeasuredTransaction)
import Moonlight.Triangulation.Internal.Types
import Moonlight.Triangulation.Math

-- | Site both endpoints and glue the segment between them, in one transaction.
-- A refused request publishes nothing, so the caller's triangulation still
-- stands and the endpoints it would have sited are not among its vertices.
addConstraintEdge
  :: HasPosition vertex
  => Triangulation 'Constrained vertex directed undirected face
  -> vertex
  -> vertex
  -> Either (CdtError) (ConstraintResult vertex directed undirected face)
addConstraintEdge triangulation fromVertex toVertex = do
  _ <- first CdtBuildError (validatePoint Nothing fromPoint)
  _ <- first CdtBuildError (validatePoint Nothing toPoint)
  result <-
    case
        ( lookupExistingConstraintEndpoint triangulation fromPoint
        , lookupExistingConstraintEndpoint triangulation toPoint
        ) of
      (Just from, Just to) ->
        case constraintAdmission triangulation from to of
          ConstraintBlocked blocking -> Left (ConstraintIntersection blocking)
          ConstraintAdmitted ->
            applyConstraintToExistingEndpoints
              triangulation
              from
              fromVertex
              to
              toVertex
      _ -> addConstraintWithEndpointPlacement
  pure result
 where
  !fromPoint = position fromVertex
  !toPoint = position toVertex

  addConstraintWithEndpointPlacement = do
    (request, frozen) <-
      runUnmeasuredTransaction CdtBuildError LocalTransaction triangulation 2 $ \mutable operation -> do
        programWords <- newGrowableWord32 256
        asConstraintStep (placeConstraintEndpoint mutable operation Nothing fromPoint fromVertex)
          `bindMutable` \from ->
            asConstraintStep (placeConstraintEndpoint mutable operation Nothing toPoint toVertex)
              `bindMutable` \to ->
                recoverConstraintRequest programWords mutable operation from to
    pure (publishConstraintResult frozen request)

-- | The common singleton case already owns both sites. Resolve them before
-- opening topology, then retain the original payload semantics in the
-- transaction. The corridor worker is unchanged; only two redundant point
-- locations and two unused vertex-capacity reservations disappear.
applyConstraintToExistingEndpoints
  :: Triangulation 'Constrained vertex directed undirected face
  -> VertexId
  -> vertex
  -> VertexId
  -> vertex
  -> Either (CdtError) (ConstraintResult vertex directed undirected face)
applyConstraintToExistingEndpoints triangulation from@(VertexId rawFrom) fromPayload to@(VertexId rawTo) toPayload =
  do
    (request, frozen) <-
      runUnmeasuredTransaction CdtBuildError LocalTransaction triangulation 0 $ \mutable operation -> do
        programWords <- newGrowableWord32 256
        writeVertexData mutable (fromIntegral rawFrom) fromPayload
        writeVertexData mutable (fromIntegral rawTo) toPayload
        recoverConstraintRequest programWords mutable operation from to
    pure (publishConstraintResult frozen request)

recoverConstraintRequest
  :: GrowableWord32 s
  -> MutableDcel s vertex directed undirected face
  -> OperationState s
  -> VertexId
  -> VertexId
  -> ST s (Either CdtError ConstraintRequestAccumulator)
recoverConstraintRequest programWords mutable operation from to =
  applyMutableConstraint programWords mutable operation from to
    `bindMutable` \applied ->
      case applied of
        MutableConstraintRejected blocking ->
          pure (Left (ConstraintIntersection blocking))
        MutableConstraintAccepted request -> pure (Right request)
{-# INLINE recoverConstraintRequest #-}

-- | Attach one accepted local result to the mesh published by the transaction.
publishConstraintResult
  :: Triangulation 'Constrained vertex directed undirected face
  -> ConstraintRequestAccumulator
  -> ConstraintResult vertex directed undirected face
publishConstraintResult frozen request =
  ConstraintRecoveryResult
    { constraintRecoveryTriangulation = frozen
    , constraintRecoveryPathReceipt = V.fromList (reverse (accumulatedRequestPath request))
    , constraintRecoveryAddedEdges = accumulatedRequestAddedEdges request
    }

-- | Insert a polyline's vertices and recover each adjacent segment as a
-- constraint, optionally closing the final segment back to the first.
addConstraintEdges
  :: HasPosition vertex
  => Triangulation 'Constrained vertex directed undirected face
  -> V.Vector vertex
  -> Bool
  -> Either (CdtError) (Triangulation 'Constrained vertex directed undirected face)
addConstraintEdges triangulation polylineVertices closed
  | V.null polylineVertices = Right triangulation
  | otherwise = do
      (withVertices, handles) <- insertPolylineVertices triangulation polylineVertices
      let adjacent = V.zip handles (V.drop 1 handles)
          closing =
            if closed && V.length handles > 1
              then
                case (handles V.!? (V.length handles - 1), handles V.!? 0) of
                  (Just finalVertex, Just firstVertex) ->
                    V.singleton (finalVertex, firstVertex)
                  _ -> V.empty
              else V.empty
      batch <- recoverConstraints withVertices (adjacent V.++ closing)
      case V.foldl' firstBlocking Nothing (constraintBatchOutcomes batch) of
        Nothing -> Right (constraintBatchTriangulation batch)
        Just blocking -> Left (ConstraintIntersection blocking)
 where
  firstBlocking found@(Just _) _ = found
  firstBlocking Nothing outcome =
    case outcome of
      ConstraintAccepted _ _ -> Nothing
      ConstraintRejected blocking -> Just blocking

insertPolylineVertices
  :: HasPosition vertex
  => Triangulation 'Constrained vertex directed undirected face
  -> V.Vector vertex
  -> Either (CdtError) (Triangulation 'Constrained vertex directed undirected face, V.Vector VertexId)
insertPolylineVertices triangulation points = do
  result <- first CdtBuildError (insertMany triangulation points)
  let !mapping = buildInputVertices result
  pure (buildTriangulation result, V.generate (sizeofPrimArray mapping) (VertexId . indexPrimArray mapping))

-- | Retire one constraint edge and restore local Delaunay legality.
removeConstraintEdge
  :: Triangulation 'Constrained vertex directed undirected face
  -> UndirectedEdgeId
  -> Either (CdtError) (Triangulation 'Constrained vertex directed undirected face)
removeConstraintEdge triangulation edge@(UndirectedEdgeId raw)
  | fromIntegral raw >= edgeCount =
      Left (ConstraintEdgeIndexOutOfRange edge edgeCount)
  -- An edge carrying no constraint has nothing to retire, and answering that
  -- without thawing is the difference between O(1) and a republished mesh.
  | not (Dcel.isConstraintEdge triangulation edge) = Right triangulation
  | otherwise = do
      (_, frozen) <-
        runUnmeasuredTransaction CdtBuildError LocalTransaction triangulation 0 $ \mutable operation ->
          retireConstraintEdge mutable operation edge
      pure frozen
 where
  edgeCount = Dcel.numUndirectedEdges triangulation

-- | Retire one constraint inside the open transaction and restore the Delaunay
-- property across the edge it protected.
retireConstraintEdge
  :: MutableDcel s vertex directed undirected face
  -> OperationState s
  -> UndirectedEdgeId
  -> ST s (Either (CdtError) ())
retireConstraintEdge mutable operation edge@(UndirectedEdgeId raw) = do
  halfEdges <- directedEdgeCount mutable
  let !directed = fromIntegral raw * 2
  if directed >= halfEdges
    then pure (Left (ConstraintEdgeIndexOutOfRange edge (halfEdges `quot` 2)))
    else do
      constrained <- readConstraint mutable directed
      if not constrained
        then pure (Right ())
        else do
          _ <- clearConstraint mutable directed
          legalizeEdges mutable operation [directed]
          pure (Right ())

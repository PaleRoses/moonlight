{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | The degenerate dimensions: a point, a segment chain, and its promotion.
module Moonlight.Triangulation.Internal.DcelOperations.Chain
  ( setupFirstVertex
  , setupSecondVertex
  , splitLineEdge
  , extendLine
  , collectLineChain
  , lineToArea
  ) where

import Control.Monad (forM_, when)
import Control.Monad.ST (ST)
import Data.STRef (writeSTRef)
import Moonlight.Triangulation.Handles.HandleDefs (VertexId (..))
import Moonlight.Triangulation.Internal.DcelOperations.Legalize (legalizeScratch)
import Moonlight.Triangulation.Internal.DcelOperations.Twin (reverseIndex)
import Moonlight.Triangulation.Internal.Mutable
  ( MutableDcel (..)
  , addEdge
  , addEdgeBlock
  , addFaceBlock
  , directedEdgeCount
  , edgeOriginPoint
  , ensureCellCapacity
  , isConnected
  , linkEdges
  , markConnected
  , pointAt
  , pointCount
  , readConstraint
  , readNext
  , readOrigin
  , readPrevious
  , readVertexOut
  , resetEdgeData
  , setConstraint
  , setCycle3
  , writeFace
  , writeFaceEdge
  , writeOrigin
  , writeVertexOut
  )
import Moonlight.Triangulation.Internal.OperationState
  ( Counter (..)
  , OperationState
  , addCounter
  , readScratch
  , writeScratch
  )
import Moonlight.Triangulation.Internal.Probe (KnownProbe)
import Moonlight.Triangulation.Internal.Types (BuildError (..))
import Moonlight.Triangulation.Math (orient2d)

setupFirstVertex :: MutableDcel s vertex directed undirected face -> Int -> ST s ()
setupFirstVertex mutable vertex = markConnected mutable vertex (-1)

setupSecondVertex :: MutableDcel s vertex directed undirected face -> Int -> ST s (Either BuildError ())
setupSecondVertex mutable vertex = do
  vertices <- pointCount mutable
  connected <- findConnected mutable vertices 0
  case connected of
    Left obstruction -> pure (Left obstruction)
    Right first -> do
      capacity <- ensureCellCapacity mutable 1 0
      case capacity of
        Left obstruction -> pure (Left obstruction)
        Right () -> do
          (edge, reverseEdgeEdge) <- addEdge mutable first vertex
          linkEdges mutable edge reverseEdgeEdge
          linkEdges mutable reverseEdgeEdge edge
          writeFace mutable edge 0
          writeFace mutable reverseEdgeEdge 0
          writeFaceEdge mutable 0 edge
          writeVertexOut mutable first edge
          markConnected mutable vertex reverseEdgeEdge
          pure (Right ())

splitLineEdge :: MutableDcel s vertex directed undirected face -> OperationState s -> Int -> Int -> ST s (Either BuildError ())
splitLineEdge mutable operation edge vertex = do
  capacity <- ensureCellCapacity mutable 1 0
  case capacity of
    Left obstruction -> pure (Left obstruction)
    Right () -> do
      protected <- readConstraint mutable edge
      let !reverseEdgeEdge = reverseIndex edge
      destinationVertex <- readOrigin mutable reverseEdgeEdge
      oldNextEdge <- readNext mutable edge
      oldPreviousTwin <- readPrevious mutable reverseEdgeEdge
      oldNextTwin <- readNext mutable reverseEdgeEdge
      writeOrigin mutable reverseEdgeEdge vertex
      (newEdge, newTwin) <- addEdge mutable vertex destinationVertex
      writeFace mutable newEdge 0
      writeFace mutable newTwin 0
      if oldNextEdge == reverseEdgeEdge
        then do
          -- The split segment reaches a line endpoint. Its forward edge and twin
          -- are adjacent, so the replacement is one contiguous four-edge run.
          -- This also covers the initial two-vertex topology.
          linkEdges mutable edge newEdge
          linkEdges mutable newEdge newTwin
          linkEdges mutable newTwin reverseEdgeEdge
          linkEdges mutable reverseEdgeEdge oldNextTwin
        else do
          -- Replace the two occurrences independently: [edge] becomes
          -- [edge,newEdge], while [twin] becomes [newTwin,twin].
          linkEdges mutable edge newEdge
          linkEdges mutable newEdge oldNextEdge
          linkEdges mutable oldPreviousTwin newTwin
          linkEdges mutable newTwin reverseEdgeEdge
      writeVertexOut mutable destinationVertex newTwin
      markConnected mutable vertex newEdge
      -- AB became AV. The new half is a fresh slot and already carries the
      -- default; the truncated half is a different edge in an old slot.
      resetEdgeData mutable (edge `quot` 2)
      when protected $ do
        _ <- setConstraint mutable newEdge
        pure ()
      addCounter operation CounterLineSplits 1
      pure (Right ())

extendLine :: MutableDcel s vertex directed undirected face -> OperationState s -> Int -> Int -> ST s (Either BuildError ())
extendLine mutable operation endpoint vertex = do
  outgoing <- readVertexOut mutable endpoint
  if outgoing < 0
    then pure (Left (DegenerateLineEndpointMissingOutgoing (VertexId (fromIntegral endpoint))))
    else do
      capacity <- ensureCellCapacity mutable 1 0
      case capacity of
        Left obstruction -> pure (Left obstruction)
        Right () -> do
          let !incoming = reverseIndex outgoing
          (newEdge, newTwin) <- addEdge mutable endpoint vertex
          writeFace mutable newEdge 0
          writeFace mutable newTwin 0
          linkEdges mutable incoming newEdge
          linkEdges mutable newEdge newTwin
          linkEdges mutable newTwin outgoing
          writeVertexOut mutable endpoint newEdge
          markConnected mutable vertex newTwin
          addCounter operation CounterLineExtensions 1
          pure (Right ())

collectLineChain :: MutableDcel s vertex directed undirected face -> OperationState s -> ST s (Either BuildError Int)
collectLineChain mutable operation = do
  halfEdges <- directedEdgeCount mutable
  turn <- findTurn halfEdges 0
  case turn of
    Left obstruction -> pure (Left obstruction)
    Right incoming -> do
      let !first = reverseIndex incoming
          go !count !edge = do
            writeScratch operation count edge
            edgeNext <- readNext mutable edge
            if edgeNext == reverseIndex edge
              then pure (Right (count + 1))
              else go (count + 1) edgeNext
      go 0 first
 where
  findTurn halfEdges !edge
    | edge >= halfEdges = pure (Left (DegenerateLineEndpointTurnMissing halfEdges))
    | otherwise = do
        edgeNext <- readNext mutable edge
        if edgeNext == reverseIndex edge then pure (Right edge) else findTurn halfEdges (edge + 1)

lineToArea :: forall p s vertex directed undirected face. KnownProbe p => MutableDcel s vertex directed undirected face -> OperationState s -> Int -> ST s (Either BuildError ())
lineToArea mutable operation vertex = do
  collected <- collectLineChain mutable operation
  case collected of
    Left obstruction -> pure (Left obstruction)
    Right segmentCount -> lineToAreaCollected @p mutable operation vertex segmentCount

lineToAreaCollected :: forall p s vertex directed undirected face. KnownProbe p => MutableDcel s vertex directed undirected face -> OperationState s -> Int -> Int -> ST s (Either BuildError ())
lineToAreaCollected mutable@MutableDcel{mdLastFace} operation vertex segmentCount = do
  capacity <- ensureCellCapacity mutable (segmentCount + 1) segmentCount
  case capacity of
    Left obstruction -> pure (Left obstruction)
    Right () -> lineToAreaWithCapacity
 where
  lineToAreaWithCapacity = do
   firstSegment <- readScratch operation 0
   lastSegment <- readScratch operation (segmentCount - 1)
   firstPoint <- edgeOriginPoint mutable firstSegment
   lastPoint <- edgeOriginPoint mutable (reverseIndex lastSegment)
   insertedPoint <- pointAt mutable vertex
   when (orient2d firstPoint lastPoint insertedPoint == LT) $ reverseScratchDirections operation segmentCount
   spokeBase <- addEdgeBlock mutable (segmentCount + 1)
   faceBase <- addFaceBlock mutable segmentCount
   forM_ [0 .. segmentCount] $ \index -> do
    chainVertex <-
      if index < segmentCount
        then readScratch operation index >>= readOrigin mutable
        else readScratch operation (segmentCount - 1) >>= readOrigin mutable . reverseIndex
    let !forward = spokeBase + 2 * index
        !backward = forward + 1
    writeOrigin mutable forward chainVertex
    writeOrigin mutable backward vertex
   forM_ [0 .. segmentCount - 1] $ \index -> do
    segment <- readScratch operation index
    let !face = faceBase + index
        !nextSpoke = spokeBase + 2 * (index + 1)
        !previousSpoke = spokeBase + 2 * index + 1
    setCycle3 mutable face segment nextSpoke previousSpoke
   lastInnerSegment <- readScratch operation (segmentCount - 1)
   let !outerStart = reverseIndex lastInnerSegment
       !firstOuterSpoke = spokeBase
       !lastOuterSpoke = spokeBase + 2 * segmentCount + 1
   linkOuterTwins mutable operation segmentCount
   firstInnerSegment <- readScratch operation 0
   linkEdges mutable (reverseIndex firstInnerSegment) firstOuterSpoke
   linkEdges mutable firstOuterSpoke lastOuterSpoke
   linkEdges mutable lastOuterSpoke outerStart
   writeFace mutable firstOuterSpoke 0
   writeFace mutable lastOuterSpoke 0
   writeFaceEdge mutable 0 outerStart
   forM_ [0 .. segmentCount - 1] $ \index -> do
    segment <- readScratch operation index
    chainVertex <- readOrigin mutable segment
    writeVertexOut mutable chainVertex segment
   finalVertex <- readOrigin mutable (reverseIndex lastInnerSegment)
   writeVertexOut mutable finalVertex (spokeBase + 2 * segmentCount)
   markConnected mutable vertex (spokeBase + 1)
   writeSTRef mdLastFace faceBase
   addCounter operation CounterLineToAreaTransitions 1
   legalizeScratch @p mutable operation vertex segmentCount
   pure (Right ())

  linkOuterTwins :: MutableDcel s vertex directed undirected face -> OperationState s -> Int -> ST s ()
  linkOuterTwins target ops count =
    let go !index
          | index <= 0 = pure ()
          | otherwise = do
              right <- readScratch ops index
              left <- readScratch ops (index - 1)
              linkEdges target (reverseIndex right) (reverseIndex left)
              writeFace target (reverseIndex right) 0
              go (index - 1)
     in do
          go (count - 1)
          first <- readScratch ops 0
          writeFace target (reverseIndex first) 0

reverseScratchDirections :: OperationState s -> Int -> ST s ()
reverseScratchDirections operation count = do
  forM_ [0 .. count `quot` 2 - 1] $ \left -> do
    let !right = count - 1 - left
    leftEdge <- readScratch operation left
    rightEdge <- readScratch operation right
    writeScratch operation left (reverseIndex rightEdge)
    writeScratch operation right (reverseIndex leftEdge)
  when (odd count) $ do
    let !middle = count `quot` 2
    edge <- readScratch operation middle
    writeScratch operation middle (reverseIndex edge)

findConnected :: MutableDcel s vertex directed undirected face -> Int -> Int -> ST s (Either BuildError Int)
findConnected mutable limit !vertex
  | vertex >= limit = pure (Left (DegenerateLineConnectedVertexMissing limit))
  | otherwise = do
      connected <- isConnected mutable vertex
      if connected then pure (Right vertex) else findConnected mutable limit (vertex + 1)

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Linear seam construction for separated Delaunay triangulations. Admission
-- returns an opaque proof carrying the exact source order and tangents; the
-- executor therefore has no untyped precondition and owns no fallback.
module Moonlight.Triangulation.Internal.Join.Seam
  ( SeamPlan
  , planSeam
  , executeSeam
  , ConstrainedSeamExecution
  , seamExecutionTriangulation
  , seamExecutionBuildStats
  , executeConstrainedSeam
  ) where

import Control.Monad.ST (ST, runST)
import Data.Bits (xor)
import Data.Foldable (traverse_)
import qualified Data.Vector.Unboxed as U
import Moonlight.Triangulation.Dcel
  ( adjacentEdge
  , faceDirectedEdges
  , numDirectedEdges
  , numFaces
  , numInnerFaces
  , numVertices
  , outerFace
  , vertexOutEdge
  , vertexData
  )
import Moonlight.Triangulation.Handles.HandleDefs
  ( DirectedEdgeId (..)
  , FaceId (..)
  , UndirectedEdgeId (..)
  , VertexId (..)
  )
import Moonlight.Triangulation.Internal.DcelOperations.Hull (closeOuterTurn)
import Moonlight.Triangulation.Internal.DcelOperations.Legalize (legalizeEdges)
import Moonlight.Triangulation.Internal.Cdt.Query (constraintEdges)
import Moonlight.Triangulation.Internal.Mutable
import Moonlight.Triangulation.Internal.OperationState
  ( OperationState
  , freezeBuildStats
  , newOperationState
  )
import Moonlight.Triangulation.Internal.Paged (pagedUnsafeIndex)
import Moonlight.Triangulation.Internal.Representation (Triangulation (..))
import Moonlight.Triangulation.Internal.Types
  ( BuildError
  , BuildStats
  , ConstraintMode (Constrained, Unconstrained)
  , unitElementDefaults
  )
import Moonlight.Triangulation.Scalar (inCircleCoordinates, orient2dCoordinates)

-- | Proof that a particular pair can be copied and stitched by the seam
-- kernel. Constructors stay private so an unseparated pair cannot be handed to
-- the executor by accident.
data SeamPlan
  = SeamLeftBeforeRight !SeamTangents
  | SeamRightBeforeLeft !SeamTangents

planSeam
  :: Triangulation mode vertex () () ()
  -> Triangulation mode' vertex () () ()
  -> Maybe SeamPlan
planSeam left right =
  case separatedOrder left right of
    Just LeftBeforeRight ->
      Just (SeamLeftBeforeRight (seamTangents left right))
    Just RightBeforeLeft ->
      Just (SeamRightBeforeLeft (seamTangents right left))
    Nothing -> Nothing

-- | Execute a proved seam schedule. Numbering follows the schedule;
-- 'Moonlight.Triangulation.Dcel.canonicalize' remains the explicit
-- construction-independent observation.
executeSeam
  :: forall vertex
   . SeamPlan
  -> Triangulation 'Unconstrained vertex () () ()
  -> Triangulation 'Unconstrained vertex () () ()
  -> Either BuildError (Triangulation 'Unconstrained vertex () () ())
executeSeam plan left right =
  case plan of
    SeamLeftBeforeRight tangents ->
      fmap fst (mergeSeparated [] left right tangents)
    SeamRightBeforeLeft tangents ->
      fmap fst (mergeSeparated [] right left tangents)

-- | Result of the constrained seam kernel. Constraint flags are copied before
-- legalization, so source contour edges are immutable barriers while the
-- zipper constructs only the missing corridor.
data ConstrainedSeamExecution vertex = ConstrainedSeamExecution
  { seamExecutionTriangulation
      :: !(Triangulation 'Constrained vertex () () ())
  , seamExecutionBuildStats :: !BuildStats
  }

-- | Execute a proved seam while transporting both source constraint planes.
-- This is distinct from promoting the unconstrained result afterward: source
-- hull constraints must already be visible to seam legalization or the
-- legalization schedule could erase a solved source face before recovery had
-- a chance to mark it.
executeConstrainedSeam
  :: forall vertex
   . SeamPlan
  -> Triangulation 'Constrained vertex () () ()
  -> Triangulation 'Constrained vertex () () ()
  -> Either BuildError (ConstrainedSeamExecution vertex)
executeConstrainedSeam plan left right =
  fmap
    (uncurry ConstrainedSeamExecution)
    (case plan of
      SeamLeftBeforeRight tangents ->
        mergeSeparated
          [ (0, left)
          , (numDirectedEdges left, right)
          ]
          left
          right
          tangents
      SeamRightBeforeLeft tangents ->
        mergeSeparated
          [ (0, right)
          , (numDirectedEdges right, left)
          ]
          right
          left
          tangents
    )

data SeparatedOrder
  = LeftBeforeRight
  | RightBeforeLeft

separatedOrder
  :: Triangulation mode vertex directed undirected face
  -> Triangulation mode' vertex' directed' undirected' face'
  -> Maybe SeparatedOrder
separatedOrder left right
  | numInnerFaces left <= 0 || numInnerFaces right <= 0 = Nothing
  | otherwise = do
      (leftMinimum, leftMaximum) <- xBounds left
      (rightMinimum, rightMaximum) <- xBounds right
      if leftMaximum < rightMinimum
        then Just LeftBeforeRight
        else
          if rightMaximum < leftMinimum
            then Just RightBeforeLeft
            else Nothing

xBounds
  :: Triangulation mode vertex directed undirected face
  -> Maybe (Double, Double)
xBounds triangulation
  | total <= 0 = Nothing
  | otherwise = Just (go 1 first first)
 where
  !total = numVertices triangulation
  !coordinates = triPointX triangulation
  !first = coordinates `pagedUnsafeIndex` 0
  go !index !minimumX !maximumX
    | index >= total = (minimumX, maximumX)
    | otherwise =
        let !x = coordinates `pagedUnsafeIndex` index
         in go (index + 1) (min minimumX x) (max maximumX x)

-- The opposite-sign branch cannot overflow in its sum. The same-sign branch
-- cannot overflow in its difference.
data SeamTangents = SeamTangents
  {-# UNPACK #-} !Int
  {-# UNPACK #-} !Int
  {-# UNPACK #-} !Int
  {-# UNPACK #-} !Int

seamTangents
  :: Triangulation mode vertex directed undirected face
  -> Triangulation mode' vertex' directed' undirected' face'
  -> SeamTangents
seamTangents left right =
  let (lowerLeft, lowerRight) = lowerTangent left right leftHull rightHull
      (upperLeft, upperRight) = upperTangent left right leftHull rightHull
   in SeamTangents lowerLeft lowerRight upperLeft upperRight
 where
  !leftHull = outerEdges left
  !rightHull = outerEdges right

lowerTangent
  :: Triangulation mode vertex directed undirected face
  -> Triangulation mode' vertex' directed' undirected' face'
  -> U.Vector Int
  -> U.Vector Int
  -> (Int, Int)
-- The walk carries both endpoints' coordinates: a step replaces exactly one
-- endpoint, and the replacement is the neighbour whose coordinates the step's
-- own test already read.
lowerTangent left right leftHull rightHull =
  walk leftStart leftStartX leftStartY rightStart rightStartX rightStartY
 where
  !leftStart = extremeHullIndex preferRightmost left leftHull
  !rightStart = extremeHullIndex preferLeftmost right rightHull
  (!leftStartX, !leftStartY) = hullPoint left leftHull leftStart
  (!rightStartX, !rightStartY) = hullPoint right rightHull rightStart

  walk !leftIndex !leftX !leftY !rightIndex !rightX !rightY
    | leftBelow = walk nextLeft nextLeftX nextLeftY rightIndex rightX rightY
    | rightBelow = walk leftIndex leftX leftY previousRight previousRightX previousRightY
    | otherwise = (leftHull `U.unsafeIndex` leftIndex, rightHull `U.unsafeIndex` rightIndex)
   where
    !nextLeft = nextIndex (U.length leftHull) leftIndex
    (!nextLeftX, !nextLeftY) = hullPoint left leftHull nextLeft
    !leftBelow =
      orient2dCoordinates leftX leftY rightX rightY nextLeftX nextLeftY == LT
    previousRight = previousIndex (U.length rightHull) rightIndex
    (previousRightX, previousRightY) = hullPoint right rightHull previousRight
    rightBelow =
      orient2dCoordinates leftX leftY rightX rightY previousRightX previousRightY == LT

upperTangent
  :: Triangulation mode vertex directed undirected face
  -> Triangulation mode' vertex' directed' undirected' face'
  -> U.Vector Int
  -> U.Vector Int
  -> (Int, Int)
upperTangent left right leftHull rightHull =
  walk leftStart leftStartX leftStartY rightStart rightStartX rightStartY
 where
  !leftStart = extremeHullIndex preferRightmostUpper left leftHull
  !rightStart = extremeHullIndex preferLeftmostUpper right rightHull
  (!leftStartX, !leftStartY) = hullPoint left leftHull leftStart
  (!rightStartX, !rightStartY) = hullPoint right rightHull rightStart

  walk !leftIndex !leftX !leftY !rightIndex !rightX !rightY
    | leftAbove = walk previousLeft previousLeftX previousLeftY rightIndex rightX rightY
    | rightAbove = walk leftIndex leftX leftY nextRight nextRightX nextRightY
    | otherwise = (leftHull `U.unsafeIndex` leftIndex, rightHull `U.unsafeIndex` rightIndex)
   where
    !previousLeft = previousIndex (U.length leftHull) leftIndex
    (!previousLeftX, !previousLeftY) = hullPoint left leftHull previousLeft
    !leftAbove =
      orient2dCoordinates leftX leftY rightX rightY previousLeftX previousLeftY == GT
    nextRight = nextIndex (U.length rightHull) rightIndex
    (nextRightX, nextRightY) = hullPoint right rightHull nextRight
    rightAbove =
      orient2dCoordinates leftX leftY rightX rightY nextRightX nextRightY == GT

type ExtremePreference = Double -> Double -> Double -> Double -> Bool

preferRightmost :: ExtremePreference
preferRightmost bestX bestY candidateX candidateY =
  candidateX > bestX || (candidateX == bestX && candidateY < bestY)
{-# INLINE preferRightmost #-}

preferLeftmost :: ExtremePreference
preferLeftmost bestX bestY candidateX candidateY =
  candidateX < bestX || (candidateX == bestX && candidateY < bestY)
{-# INLINE preferLeftmost #-}

preferRightmostUpper :: ExtremePreference
preferRightmostUpper bestX bestY candidateX candidateY =
  candidateX > bestX || (candidateX == bestX && candidateY > bestY)
{-# INLINE preferRightmostUpper #-}

preferLeftmostUpper :: ExtremePreference
preferLeftmostUpper bestX bestY candidateX candidateY =
  candidateX < bestX || (candidateX == bestX && candidateY > bestY)
{-# INLINE preferLeftmostUpper #-}

extremeHullIndex
  :: ExtremePreference
  -> Triangulation mode vertex directed undirected face
  -> U.Vector Int
  -> Int
-- The running best carries its own coordinates. Re-reading them per candidate
-- read the same immutable slots @size@ times over instead of once.
extremeHullIndex prefer triangulation hull
  | size <= 0 = 0
  | otherwise =
      let (!firstX, !firstY) = hullPoint triangulation hull 0
       in go 1 0 firstX firstY
 where
  !size = U.length hull
  go !index !best !bestX !bestY
    | index >= size = best
    | otherwise =
        let (!candidateX, !candidateY) = hullPoint triangulation hull index
         in if prefer bestX bestY candidateX candidateY
              then go (index + 1) index candidateX candidateY
              else go (index + 1) best bestX bestY

outerEdges
  :: Triangulation mode vertex directed undirected face
  -> U.Vector Int
outerEdges =
  U.fromList
    . fmap (\(DirectedEdgeId edge) -> fromIntegral edge)
    . (`faceDirectedEdges` outerFace)

hullPoint
  :: Triangulation mode vertex directed undirected face
  -> U.Vector Int
  -> Int
  -> (Double, Double)
hullPoint triangulation hull index =
  let !edge = hull `U.unsafeIndex` index
      !vertex = topologyAt triangulation (4 * edge)
   in ( triPointX triangulation `pagedUnsafeIndex` vertex
      , triPointY triangulation `pagedUnsafeIndex` vertex
      )
{-# INLINE hullPoint #-}

nextIndex :: Int -> Int -> Int
nextIndex size index
  | index + 1 == size = 0
  | otherwise = index + 1
{-# INLINE nextIndex #-}

previousIndex :: Int -> Int -> Int
previousIndex size index
  | index == 0 = size - 1
  | otherwise = index - 1
{-# INLINE previousIndex #-}

mergeSeparated
  :: forall outputMode leftMode rightMode vertex
   . [(Int, Triangulation 'Constrained vertex () () ())]
  -> Triangulation leftMode vertex () () ()
  -> Triangulation rightMode vertex () () ()
  -> SeamTangents
  -> Either
      BuildError
      (Triangulation outputMode vertex () () (), BuildStats)
mergeSeparated constraintSections left right (SeamTangents lowerLeft lowerRight upperLeft upperRight) = runST $ do
  mutable <- newMutableDcel unitElementDefaults (planarDcelCapacity totalVertices)
  pointCapacityOutcome <- ensurePointCapacity mutable totalVertices
  cellCapacityOutcome <-
    ensureCellCapacity
      mutable
      ((leftDirected + rightDirected) `quot` 2 + 1)
      (leftFaces + rightFaces - 2)
  case (pointCapacityOutcome, cellCapacityOutcome) of
    (Left obstruction, _) -> pure (Left obstruction)
    (_, Left obstruction) -> pure (Left obstruction)
    (Right (), Right ()) -> do
      appendSourceVertices mutable left
      appendSourceVertices mutable right
      _ <- addEdgeBlock mutable ((leftDirected + rightDirected) `quot` 2)
      _ <- addFaceBlock mutable (leftFaces + rightFaces - 2)
      copySource mutable left 0 0 0
      copySource mutable right leftVertices leftDirected (leftFaces - 1)
      traverse_ (uncurry (protectSourceEdges mutable)) constraintSections
      base <- spliceLowerTangent mutable leftDirected lowerLeft lowerRight
      operation <- newOperationState (halfEdgeCapacity mutable)
      stitched <-
        stitchSeam
          mutable
          operation
          base
          (topologyAt left (4 * upperLeft))
          (leftVertices + topologyAt right (4 * upperRight))
          []
      case stitched of
        Left obstruction -> pure (Left obstruction)
        Right () -> do
          traverse_ (uncurry (restoreSourceConstraints mutable)) constraintSections
          statistics <- freezeBuildStats operation
          fmap (\triangulation -> (triangulation, statistics))
            <$> freezeTriangulation mutable
 where
  !leftVertices = numVertices left
  !rightVertices = numVertices right
  !totalVertices = leftVertices + rightVertices
  !leftDirected = numDirectedEdges left
  !rightDirected = numDirectedEdges right
  !leftFaces = numFaces left
  !rightFaces = numFaces right

stitchSeam
  :: MutableDcel s vertex directed undirected face
  -> OperationState s
  -> Int
  -> Int
  -> Int
  -> [Int]
  -> ST s (Either BuildError ())
stitchSeam mutable operation base upperLeft upperRight seeds = do
  leftVertex <- readOrigin mutable base
  rightVertex <- readOrigin mutable (base `xor` 1)
  if leftVertex == upperLeft && rightVertex == upperRight
    then legalizeEdges mutable operation seeds >> pure (Right ())
    else do
      leftEdge <- readPrevious mutable base
      rightEdge <- readNext mutable base
      nextLeft <- readOrigin mutable leftEdge
      nextRight <- readOrigin mutable (rightEdge `xor` 1)
      leftTurn <- vertexOrientation mutable nextLeft leftVertex rightVertex
      rightTurn <- vertexOrientation mutable leftVertex rightVertex nextRight
      chooseRight <-
        if rightVertex == upperRight
          then pure False
          else
            if leftVertex == upperLeft
              then pure True
              else
                case (leftTurn == GT, rightTurn == GT) of
                  (True, True) ->
                    (== GT) <$> vertexInCircle mutable nextLeft leftVertex rightVertex nextRight
                  (False, True) -> pure True
                  _ -> pure False
      if chooseRight
        then do
          closed <- closeOuterTurn mutable base
          case closed of
            Left obstruction -> pure (Left obstruction)
            Right nextBase ->
              stitchSeam mutable operation nextBase upperLeft upperRight (base : rightEdge : seeds)
        else do
          closed <- closeOuterTurn mutable leftEdge
          case closed of
            Left obstruction -> pure (Left obstruction)
            Right nextBase ->
              stitchSeam mutable operation nextBase upperLeft upperRight (leftEdge : base : seeds)

vertexOrientation
  :: MutableDcel s vertex directed undirected face
  -> Int
  -> Int
  -> Int
  -> ST s Ordering
vertexOrientation mutable a b c = do
  ax <- readPointX mutable a
  ay <- readPointY mutable a
  bx <- readPointX mutable b
  by <- readPointY mutable b
  cx <- readPointX mutable c
  cy <- readPointY mutable c
  pure $! orient2dCoordinates ax ay bx by cx cy
{-# INLINE vertexOrientation #-}

vertexInCircle
  :: MutableDcel s vertex directed undirected face
  -> Int
  -> Int
  -> Int
  -> Int
  -> ST s Ordering
vertexInCircle mutable a b c d = do
  ax <- readPointX mutable a
  ay <- readPointY mutable a
  bx <- readPointX mutable b
  by <- readPointY mutable b
  cx <- readPointX mutable c
  cy <- readPointY mutable c
  dx <- readPointX mutable d
  dy <- readPointY mutable d
  pure $! inCircleCoordinates ax ay bx by cx cy dx dy
{-# INLINE vertexInCircle #-}

appendSourceVertices
  :: MutableDcel s vertex () () ()
  -> Triangulation mode vertex () () ()
  -> ST s ()
appendSourceVertices mutable source =
  forRange 0 (numVertices source) $ \vertex -> do
    _ <-
      appendVertexCoordinates
        mutable
        (triPointX source `pagedUnsafeIndex` vertex)
        (triPointY source `pagedUnsafeIndex` vertex)
        (vertexData source (VertexId (fromIntegral vertex)))
    pure ()

copySource
  :: MutableDcel s vertex () () ()
  -> Triangulation mode vertex () () ()
  -> Int
  -> Int
  -> Int
  -> ST s ()
copySource mutable source vertexOffset edgeOffset faceOffset = do
  forRange 0 (numDirectedEdges source) $ \edge -> do
    let !target = edgeOffset + edge
        !sourceBase = 4 * edge
        !sourceFace = topologyAt source (sourceBase + 3)
        !targetFace = if sourceFace == 0 then 0 else faceOffset + sourceFace
    writeOrigin mutable target (vertexOffset + topologyAt source sourceBase)
    writeNext mutable target (edgeOffset + topologyAt source (sourceBase + 1))
    writePrevious mutable target (edgeOffset + topologyAt source (sourceBase + 2))
    writeFace mutable target targetFace
  forRange 0 (numVertices source) $ \vertex ->
    case vertexOutEdge source (VertexId (fromIntegral vertex)) of
      Nothing -> markConnected mutable (vertexOffset + vertex) (-1)
      Just (DirectedEdgeId edge) ->
        markConnected mutable (vertexOffset + vertex) (edgeOffset + fromIntegral edge)
  forRange 1 (numFaces source) $ \face ->
    case adjacentEdge source (FaceId (fromIntegral face)) of
      Nothing -> writeFaceEdge mutable (faceOffset + face) (-1)
      Just (DirectedEdgeId edge) ->
        writeFaceEdge mutable (faceOffset + face) (edgeOffset + fromIntegral edge)

copySourceConstraints
  :: MutableDcel s vertex () () ()
  -> Int
  -> Triangulation 'Constrained vertex () () ()
  -> ST s ()
copySourceConstraints mutable directedEdgeOffset source =
  traverse_
    (\(UndirectedEdgeId edge) ->
       ()
         <$ setConstraint
           mutable
           (directedEdgeOffset + 2 * fromIntegral edge)
    )
    (constraintEdges source)

-- Source faces are immutable sections of a separated join.  Protect every
-- copied source edge while the seam is legalized, then restore the original
-- constraint plane before publication.  The temporary flags prevent the
-- corridor legalization from escaping the overlap and flipping a solved
-- source face.
protectSourceEdges
  :: MutableDcel s vertex () () ()
  -> Int
  -> Triangulation 'Constrained vertex () () ()
  -> ST s ()
protectSourceEdges mutable directedEdgeOffset source =
  forRange 0 (numDirectedEdges source `quot` 2) $ \edge -> do
    _ <- setConstraint mutable (directedEdgeOffset + 2 * edge)
    pure ()

restoreSourceConstraints
  :: MutableDcel s vertex () () ()
  -> Int
  -> Triangulation 'Constrained vertex () () ()
  -> ST s ()
restoreSourceConstraints mutable directedEdgeOffset source = do
  forRange 0 (numDirectedEdges source `quot` 2) $ \edge -> do
    _ <- clearConstraint mutable (directedEdgeOffset + 2 * edge)
    pure ()
  copySourceConstraints mutable directedEdgeOffset source

spliceLowerTangent
  :: MutableDcel s vertex () () ()
  -> Int
  -> Int
  -> Int
  -> ST s Int
spliceLowerTangent mutable rightEdgeOffset leftOuter rightOuterSource = do
  let !rightOuter = rightEdgeOffset + rightOuterSource
  leftVertex <- readOrigin mutable leftOuter
  rightVertex <- readOrigin mutable rightOuter
  leftPrevious <- readPrevious mutable leftOuter
  rightPrevious <- readPrevious mutable rightOuter
  (forward, backward) <- addEdge mutable leftVertex rightVertex
  writeFace mutable forward 0
  writeFace mutable backward 0
  linkEdges mutable leftPrevious forward
  linkEdges mutable forward rightOuter
  linkEdges mutable rightPrevious backward
  linkEdges mutable backward leftOuter
  writeFaceEdge mutable 0 forward
  writeVertexOut mutable leftVertex forward
  writeVertexOut mutable rightVertex backward
  pure forward
{-# INLINE spliceLowerTangent #-}

topologyAt
  :: Triangulation mode vertex directed undirected face
  -> Int
  -> Int
topologyAt triangulation slot =
  fromIntegral (triHalfTopology triangulation `pagedUnsafeIndex` slot)
{-# INLINE topologyAt #-}


forRange :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
forRange from to action = go from
 where
  go !index
    | index >= to = pure ()
    | otherwise = action index >> go (index + 1)
{-# INLINE forRange #-}

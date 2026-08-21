{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -O3 -fllvm -optlo-O3 -optlc-O3 #-}

-- | Generation. @delaunay@ builds a mesh from a whole site set by circle sweep;
-- the insertion verbs extend an existing mesh one site at a time.
module Moonlight.Triangulation.BulkLoad
  ( empty
  , clear
  , delaunay
  , delaunayGeometry
  , DuplicatePayloadPolicy (..)
  , delaunayFromCoordinates
  , insert
  , insertAt
  , insertMany
  ) where

import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import qualified Data.IntSet as IntSet
import Data.Primitive.PrimArray
  ( MutablePrimArray
  , newPrimArray
  , readPrimArray
  , unsafeFreezePrimArray
  , writePrimArray
  )
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MUV
import Data.Word (Word32)
import Moonlight.Triangulation.Dcel (numInnerFaces, numVertices)
import Moonlight.Triangulation.Handles.HandleDefs (DirectedEdgeId (..), FaceId (..), VertexId (..))
import Moonlight.Triangulation.Internal.BoxedPaged (boxedFromVector, boxedUpdate, emptyBoxedPaged)
import Moonlight.Triangulation.Internal.Capacity (ensureCapacity)
import Moonlight.Triangulation.Insertion (insertExistingVertexAtLocation, insertVertexAtPoint)
import Moonlight.Triangulation.Internal.Location (MutableLocation (..))
import Moonlight.Triangulation.Internal.Mutable
import Moonlight.Triangulation.Internal.OperationState
  ( Counter (..)
  , OperationState
  , addCounter
  , freezeBuildStats
  , newOperationState
  , setCounter
  )
import Moonlight.Triangulation.Internal.CircleSweep (circleSweepInsert, radiallyOrderArena)
import Moonlight.Triangulation.Internal.PointIndex
  ( MutablePointIndex
  , emptyPointIndex
  , newMutablePointIndex
  , resolveMutablePoint
  , seedMutablePointIndex
  )
import Moonlight.Triangulation.Math (canonicalCoordinate, validatePoint)
import Moonlight.Triangulation.PointLocation (locatePointWithHint)
import Moonlight.Triangulation.Internal.Probe (Probe (..))
import Moonlight.Triangulation.Internal.Representation (Triangulation (..))
import Moonlight.Triangulation.Internal.PackedIndex (noIndex)
import Moonlight.Triangulation.Internal.Paged (TransactionShape (DenseTransaction, LocalTransaction), emptyPaged, fromVector)
import Moonlight.Triangulation.Internal.Transaction (runTransaction)
import Moonlight.Triangulation.Types

-- | The vertexless triangulation: the outer face and nothing else. This is the
-- canonical origin of the type — bulk loading, incremental insertion and
-- refinement all agree with growing this value.
empty
  :: ElementDefaults directed undirected face
  -> Triangulation mode vertex directed undirected face
empty defaults@ElementDefaults{defaultDirectedEdgeData, defaultUndirectedEdgeData, defaultFaceData} =
  Triangulation
    { triPointX = emptyPaged
    , triPointY = emptyPaged
    , triPointIndex = emptyPointIndex
    , triVertexOut = emptyPaged
    , triVertexData = emptyBoxedPaged Nothing
    , triHalfTopology = emptyPaged
    , triDirectedData = emptyBoxedPaged (Just defaultDirectedEdgeData)
    , triUndirectedData = emptyBoxedPaged (Just defaultUndirectedEdgeData)
    , triFaceEdge = fromVector noIndex (U.singleton noIndex)
    , triFaceData = boxedFromVector (Just defaultFaceData) (V.singleton defaultFaceData)
    , triConstraint = emptyPaged
    , triConstraintCount = 0
    , triConstraintEdges = IntSet.empty
    , triElementDefaults = defaults
    }

-- | Discard every vertex while retaining the element defaults the
-- triangulation was built with.
clear
  :: Triangulation mode vertex directed undirected face
  -> Triangulation mode vertex directed undirected face
clear = empty . triElementDefaults

-- | How a canonical bulk source combines payloads whose exact coordinates
-- coincide. Geometry identity is settled independently by the point index.
data DuplicatePayloadPolicy vertex
  = KeepFirstPayload
  | CombineDuplicatePayload !(vertex -> vertex -> vertex)

-- | The local identity verdict for one input position. A fresh verdict carries
-- the canonical coordinates already admitted to the mutable DCEL, so ingress
-- never rereads its own writes merely to accumulate the sweep centre.
data PositionClaim
  = ResidentPosition !Int
  | FreshPosition !Int !Double !Double

-- | Build a finite Delaunay DCEL while preserving the first input payload at
-- every duplicate position. The returned mapping relates every input slot to
-- the canonical stored vertex.
delaunay
  :: forall vertex directed undirected face
   . HasPosition vertex
  => ElementDefaults directed undirected face
  -> V.Vector vertex
  -> Either BuildError (BuildResult 'Unconstrained vertex directed undirected face)
delaunay defaults input =
  buildDelaunayFromSource
    defaults
    (V.length input)
    (position . (input V.!))
    (input V.!)
    KeepFirstPayload

-- | Build the geometry-only Delaunay triangulation of a coordinate vector.
-- Exact duplicate positions collapse to one site. Use 'delaunay' or
-- 'delaunayFromCoordinates' when vertex annotations or the input-to-vertex
-- mapping are part of the result.
delaunayGeometry
  :: V.Vector Point
  -> Either BuildError (DelaunayTriangulation ())
delaunayGeometry coordinates
  | inputCount == 0 = Right (empty unitElementDefaults)
  | otherwise = do
    V.iforM_ coordinates (\index point -> () <$ validatePoint (Just index) point)
    ensureCapacity inputCount
    let !canonicalArena =
          U.generate inputCount $ \index ->
            case coordinates V.! index of
              Point rawX rawY ->
                ( 0
                , canonicalCoordinate rawX
                , canonicalCoordinate rawY
                , fromIntegral index
                )
        (!inputSumX, !inputSumY) =
          U.foldl'
            (\(!sumX, !sumY) (_, x, y, _) -> (sumX + x, sumY + y))
            (0, 0)
            canonicalArena
    runST $ do
      inputArena <- U.unsafeThaw canonicalArena
      let !inputScale = recip (fromIntegral inputCount)
          !inputCenterX = inputSumX * inputScale
          !inputCenterY = inputSumY * inputScale
      assignGeometryRadialDistances inputArena inputCenterX inputCenterY
      orderedInputArena <- radiallyOrderArena inputArena
      defaultedMutable <-
        newMutableDcelWithVertexDefault
          ()
          unitElementDefaults
          (planarDcelCapacity inputCount)
      let !mutable = defaultedVertexDcel defaultedMutable
      operation <- newOperationState (halfEdgeCapacity mutable)
      unique <- appendSortedGeometry defaultedMutable inputArena
      orderedArena <-
        if unique == inputCount
          then pure orderedInputArena
          else do
            let !scale = recip (fromIntegral unique)
                !uniqueArena = MUV.unsafeSlice 0 unique inputArena
            (sumX, sumY) <- sumGeometryArena uniqueArena
            recenterGeometryArena
              (sumX * scale)
              (sumY * scale)
              uniqueArena
            radiallyOrderArena uniqueArena
      inserted <- circleSweepInsert mutable operation orderedArena
      case inserted of
        Left failure -> pure (Left failure)
        Right _ -> freezeTriangulation mutable
 where
  !inputCount = V.length coordinates

  assignGeometryRadialDistances
    :: forall s
     . MUV.MVector s (Double, Double, Double, Word32)
    -> Double
    -> Double
    -> ST s ()
  assignGeometryRadialDistances arena centerX centerY =
    MUV.imapM_
      (\index (_, x, y, input) -> do
        let !deltaX = centerX - x
            !deltaY = centerY - y
        MUV.unsafeWrite arena index (deltaX * deltaX + deltaY * deltaY, x, y, input)
      )
      arena

  appendSortedGeometry
    :: forall s
     . DefaultedVertexDcel s () () () ()
    -> MUV.MVector s (Double, Double, Double, Word32)
    -> ST s Int
  appendSortedGeometry defaultedMutable arena
    | inputCount == 0 = pure 0
    | otherwise = do
        (distance, x, y, _) <- MUV.unsafeRead arena 0
        first <- appendDefaultVertexCoordinates defaultedMutable x y
        MUV.unsafeWrite arena 0 (distance, x, y, fromIntegral first)
        (_, _, unique) <-
          MUV.foldM'
            (\(!previousX, !previousY, !uniqueCount) (nextDistance, nextX, nextY, _) ->
               if nextX == previousX && nextY == previousY
                 then pure (previousX, previousY, uniqueCount)
                 else do
                   vertex <- appendDefaultVertexCoordinates defaultedMutable nextX nextY
                   MUV.unsafeWrite arena uniqueCount (nextDistance, nextX, nextY, fromIntegral vertex)
                   pure (nextX, nextY, uniqueCount + 1)
            )
            (x, y, 1)
            (MUV.unsafeSlice 1 (inputCount - 1) arena)
        pure unique

  sumGeometryArena
    :: forall s
     . MUV.MVector s (Double, Double, Double, Word32)
    -> ST s (Double, Double)
  sumGeometryArena arena =
    MUV.foldM'
      (\(!sumX, !sumY) (_, x, y, _) -> pure (sumX + x, sumY + y))
      (0, 0)
      arena

  recenterGeometryArena
    :: forall s
     . Double
    -> Double
    -> MUV.MVector s (Double, Double, Double, Word32)
    -> ST s ()
  recenterGeometryArena centerX centerY arena =
    MUV.imapM_
      (\index (_, x, y, vertex) -> do
        let !deltaX = centerX - x
            !deltaY = centerY - y
        MUV.unsafeWrite arena index (deltaX * deltaX + deltaY * deltaY, x, y, vertex)
      )
      arena

-- | Canonical construction from separate geometry and annotation sources.
-- The coordinate vector remains the only geometry in ingress; payloads never
-- acquire a fabricated 'HasPosition' instance merely to reach the loader.
delaunayFromCoordinates
  :: forall vertex directed undirected face
   . ElementDefaults directed undirected face
  -> V.Vector (Point)
  -> V.Vector vertex
  -> DuplicatePayloadPolicy vertex
  -> Either BuildError (BuildResult 'Unconstrained vertex directed undirected face)
delaunayFromCoordinates defaults coordinates payloads duplicatePolicy
  | coordinateCount /= payloadCount =
      Left (CoordinatePayloadCountMismatch coordinateCount payloadCount)
  | otherwise =
      buildDelaunayFromSource
        defaults
        coordinateCount
        (coordinates V.!)
        (payloads V.!)
        duplicatePolicy
 where
  !coordinateCount = V.length coordinates
  !payloadCount = V.length payloads

buildDelaunayFromSource
  :: forall vertex directed undirected face
   . ElementDefaults directed undirected face
  -> Int
  -> (Int -> Point)
  -> (Int -> vertex)
  -> DuplicatePayloadPolicy vertex
  -> Either BuildError (BuildResult 'Unconstrained vertex directed undirected face)
buildDelaunayFromSource defaults inputCount pointAtInput payloadAtInput duplicatePolicy = do
  ensureCapacity inputCount
  runST $ do
    inputArena <- MUV.new inputCount
    admitted <- initializeInputArena inputArena 0 0 0
    case admitted of
      Left failure -> pure (Left failure)
      Right (inputSumX, inputSumY) -> do
        let !inputScale = if inputCount == 0 then 0 else recip (fromIntegral inputCount)
            !inputCenterX = inputSumX * inputScale
            !inputCenterY = inputSumY * inputScale
        assignRadialDistances inputArena inputCenterX inputCenterY 0
        orderedInputArena <- radiallyOrderArena inputArena
        mutable <- newMutableDcel defaults (planarDcelCapacity inputCount)
        operation <- newOperationState (halfEdgeCapacity mutable)
        mapping <- newPrimArray inputCount
        unique <- classifySortedInputs mapping inputArena
        (sumX, sumY) <- appendClassifiedInputs mutable mapping 0 0 0
        setCounter operation CounterInputPoints inputCount
        setCounter operation CounterUniquePoints unique
        setCounter operation CounterDuplicatePoints (inputCount - unique)
        inserted <-
          if unique == 0
            then pure (Right 0)
            else do
              orderedArena <-
                if unique == inputCount
                  then pure orderedInputArena
                  else do
                    let !scale = recip (fromIntegral unique)
                        !uniqueArena = MUV.unsafeSlice 0 unique inputArena
                    rewriteCompactedArena
                      mapping
                      (sumX * scale)
                      (sumY * scale)
                      uniqueArena
                      0
                    radiallyOrderArena uniqueArena
              circleSweepInsert mutable operation orderedArena
        case inserted of
          Left failure -> pure (Left failure)
          Right seedCount -> do
            setCounter operation CounterSpatialSeedPoints seedCount
            frozenOutcome <- freezeTriangulation mutable
            case frozenOutcome of
              Left failure -> pure (Left failure)
              Right frozen -> do
                mapped <- unsafeFreezePrimArray mapping
                stats <- freezeBuildStats operation
                pure
                  ( Right
                      BuildResult
                        { buildTriangulation = frozen
                        , buildInputVertices = mapped
                        , buildStats = stats
                        }
                  )
 where
  -- Validate, canonicalize, and materialize each source position in one local
  -- section. The former validation pass and centre fold both traversed the
  -- boxed source before arena generation traversed it a third time; this
  -- section returns their only global invariant, the coordinate sum.
  initializeInputArena
    :: forall s
     . MUV.MVector s (Double, Double, Double, Word32)
    -> Int
    -> Double
    -> Double
    -> ST s (Either BuildError (Double, Double))
  initializeInputArena arena !index !sumX !sumY
    | index >= inputCount = pure (Right (sumX, sumY))
    | otherwise =
        case validatePoint (Just index) (pointAtInput index) of
          Left failure -> pure (Left failure)
          Right admitted ->
            case queryPointValue admitted of
              Point x y -> do
                MUV.unsafeWrite arena index (0, x, y, fromIntegral index)
                initializeInputArena arena (index + 1) (sumX + x) (sumY + y)

  -- The all-input centre seeds the first radial ordering. Duplicate
  -- equivalence classes become adjacent in that total order; after descent,
  -- the compacted section is recentered over unique sites and ordered once
  -- more only when required.
  assignRadialDistances
    :: forall s
     . MUV.MVector s (Double, Double, Double, Word32)
    -> Double
    -> Double
    -> Int
    -> ST s ()
  assignRadialDistances arena centerX centerY !index
    | index >= inputCount = pure ()
    | otherwise = do
        (_, x, y, input) <- MUV.unsafeRead arena index
        let !deltaX = centerX - x
            !deltaY = centerY - y
        MUV.unsafeWrite arena index (deltaX * deltaX + deltaY * deltaY, x, y, input)
        assignRadialDistances arena centerX centerY (index + 1)

  -- Classify the sorted local sections by exact canonical position. The
  -- mapping first names each class by its earliest input slot. Compacting the
  -- unique radial representatives in place cannot overwrite an unread slot.
  classifySortedInputs
    :: forall s
     . MutablePrimArray s Word32
    -> MUV.MVector s (Double, Double, Double, Word32)
    -> ST s Int
  classifySortedInputs mapping arena
    | inputCount == 0 = pure 0
    | otherwise = do
        first@(_, firstX, firstY, firstInput) <- MUV.unsafeRead arena 0
        writePrimArray mapping (fromIntegral firstInput) firstInput
        classifyFrom first firstX firstY firstInput 1 1
   where
    classifyFrom !_ !previousX !previousY !classInput !readIndex !uniqueCount
      | readIndex >= inputCount = pure uniqueCount
      | otherwise = do
          record@(_, x, y, input) <- MUV.unsafeRead arena readIndex
          if x == previousX && y == previousY
            then do
              writePrimArray mapping (fromIntegral input) classInput
              classifyFrom record previousX previousY classInput (readIndex + 1) uniqueCount
            else do
              writePrimArray mapping (fromIntegral input) input
              if uniqueCount == readIndex
                then pure ()
                else MUV.unsafeWrite arena uniqueCount record
              classifyFrom record x y input (readIndex + 1) (uniqueCount + 1)

  -- Materialize in original order, preserving the established handle
  -- assignment and duplicate-payload law. The equivalence mapping for a
  -- duplicate always points backward to an already materialized class owner.
  appendClassifiedInputs
    :: forall s
     . MutableDcel s vertex directed undirected face
    -> MutablePrimArray s Word32
    -> Int
    -> Double
    -> Double
    -> ST s (Double, Double)
  appendClassifiedInputs mutable mapping !index !sumX !sumY
    | index >= inputCount = pure (sumX, sumY)
    | otherwise = do
        classInput <- readPrimArray mapping index
        let !vertexData = payloadAtInput index
        if fromIntegral classInput == index
          then case pointAtInput index of
            Point x y -> do
              let !canonicalX = canonicalCoordinate x
                  !canonicalY = canonicalCoordinate y
              vertex <- appendVertexCoordinates mutable canonicalX canonicalY vertexData
              writePrimArray mapping index (fromIntegral vertex)
              appendClassifiedInputs
                mutable
                mapping
                (index + 1)
                (sumX + canonicalX)
                (sumY + canonicalY)
          else do
            resident <- fromIntegral <$> readPrimArray mapping (fromIntegral classInput)
            writePrimArray mapping index (fromIntegral resident)
            case duplicatePolicy of
              KeepFirstPayload -> pure ()
              CombineDuplicatePayload combine -> do
                residentData <- vertexDataAt mutable resident
                writeVertexData mutable resident (combine residentData vertexData)
            appendClassifiedInputs mutable mapping (index + 1) sumX sumY

  -- Once duplicates have shortened the vertex arena, translate each compacted
  -- representative from its input-class name to its authoritative vertex and
  -- restate its radial key around the exact unique-site centre.
  rewriteCompactedArena
    :: forall s
     . MutablePrimArray s Word32
    -> Double
    -> Double
    -> MUV.MVector s (Double, Double, Double, Word32)
    -> Int
    -> ST s ()
  rewriteCompactedArena mapping centerX centerY arena !index
    | index >= MUV.length arena = pure ()
    | otherwise = do
        (_, x, y, classInput) <- MUV.unsafeRead arena index
        vertex <- readPrimArray mapping (fromIntegral classInput)
        let !deltaX = centerX - x
            !deltaY = centerY - y
        MUV.unsafeWrite arena index (deltaX * deltaX + deltaY * deltaY, x, y, vertex)
        rewriteCompactedArena mapping centerX centerY arena (index + 1)

-- | Insert or replace a vertex payload. A payload at an existing position is
-- overwritten without changing topology.
--
-- The published mesh is independent of the one passed in. A singleton below
-- ten thousand resident sites copies densely; larger bases publish through
-- copy-on-write pages. A caller inserting a sequence wants one
-- 'Moonlight.Triangulation.Session.withSession' over
-- 'Moonlight.Triangulation.Session.insertVertex' instead — see 'insertAt'.
insert
  :: HasPosition vertex
  => Triangulation mode vertex directed undirected face
  -> vertex
  -> Either BuildError (InsertionResult mode vertex directed undirected face)
insert triangulation vertexData = insertAt triangulation (position vertexData) vertexData

-- | Insert at a stated point. 'insert' is this with the point read out of the
-- payload, which is what a caller holding only a payload wants; a caller that
-- computed the point — a constraint split, a Steiner refinement — wants to say
-- so rather than build a payload and hope the round trip through 'HasPosition'
-- returns what it started with.
--
-- This is one shaped transaction over a single insertion. Replacing a fold of
-- it with one session is sound because the two agree on every mesh and differ
-- only in how many intermediate meshes they publish. A fold publishes @k@
-- meshes and pays a thaw for each, so it runs in Θ(n·k); the session pays one
-- thaw and runs in O(k·log n) expected.
insertAt
  :: Triangulation mode vertex directed undirected face
  -> Point
  -> vertex
  -> Either BuildError (InsertionResult mode vertex directed undirected face)
insertAt triangulation rawPoint vertexData = do
  queryPoint <- validatePoint Nothing rawPoint
  case locatePointWithHint triangulation Nothing queryPoint of
    (OnVertex resident, walked) ->
      Right (replaceResidentPayload triangulation resident walked vertexData)
    (located, walked) -> do
      let transactionShape =
            if numVertices triangulation < 10_000
              then DenseTransaction
              else LocalTransaction
      ((vertex, disposition), frozen, stats) <-
        runTransaction
          id
          transactionShape
          triangulation
          1
          (\mutable operation -> do
             addCounter operation CounterInputPoints 1
             inserted <-
               case located of
                 -- A frozen degenerate-line location points at an arbitrary visible
                 -- segment, while the line extension interpreter requires a terminal
                 -- edge. The frozen section carries no terminal witness, so retain
                 -- the existing mutable line locator for this one non-lawful case.
                 OutsideConvexHull (Just _)
                   | numInnerFaces triangulation == 0 ->
                       insertVertexAtPoint @'ProbeOff mutable operation Nothing (queryPointValue queryPoint) vertexData
                 _ -> do
                   capacityOutcome <- ensurePointCapacity mutable 1
                   case capacityOutcome of
                     Left failure -> pure (Left failure)
                     Right () -> do
                       vertex <- appendVertex mutable (queryPointValue queryPoint) vertexData
                       let thawedLocation =
                             case located of
                               EmptyTriangulation -> MutableEmpty
                               OnEdge (DirectedEdgeId raw) -> MutableOnEdge (fromIntegral raw)
                               InFace (FaceId raw) -> MutableInFace (fromIntegral raw)
                               -- The frozen locator emits no edge only for a
                               -- singleton mesh. Its mutable interpreter ignores
                               -- this sentinel while constructing the second vertex.
                               OutsideConvexHull Nothing -> MutableOutsideHull 0
                               OutsideConvexHull (Just (DirectedEdgeId raw)) -> MutableOutsideHull (fromIntegral raw)
                       ((vertex, Inserted) <$) <$> insertExistingVertexAtLocation @'ProbeOff mutable operation vertex thawedLocation
             case inserted of
               Left failure -> pure (Left failure)
               Right (vertex, disposition) -> do
                 case disposition of
                   Inserted -> addCounter operation CounterUniquePoints 1
                   AlreadyPresent -> do
                     writeVertexData mutable vertex vertexData
                     addCounter operation CounterExistingPoints 1
                     addCounter operation CounterDuplicatePoints 1
                 pure (Right (vertex, disposition))
          )
      pure
        InsertionResult
          { insertionTriangulation = frozen
          , insertionVertex = VertexId (fromIntegral vertex)
          , insertionDisposition = disposition
          , insertionStats = withFrozenLocationStats walked stats
          }

-- | Publish a payload replacement without opening a transaction.
--
-- A position already resident changes exactly one thing: the payload slot the
-- vertex already occupies. No coordinate, no half-edge, no constraint flag and
-- no face record differs, so the five unboxed planes are the ones the argument
-- already holds rather than copies taken out of it — which is what a thaw costs
-- and what this exists to refuse. They are immutable values; nothing reached
-- from here is a mutable buffer, and 'boxedUpdate' materializes a fresh page
-- for the one it rewrites, leaving the argument's own directory intact.
--
-- The location counters are the frozen walk's, not a thawed walk's. They
-- describe the walk that actually ran.
replaceResidentPayload
  :: Triangulation mode vertex directed undirected face
  -> VertexId
  -> LocationStats
  -> vertex
  -> InsertionResult mode vertex directed undirected face
replaceResidentPayload triangulation resident@(VertexId raw) walked vertexData =
  InsertionResult
    { insertionTriangulation =
        triangulation
          { triVertexData =
              boxedUpdate (fromIntegral raw) vertexData (triVertexData triangulation)
          }
    , insertionVertex = resident
    , insertionDisposition = AlreadyPresent
    , insertionStats =
        withFrozenLocationStats
          walked
          emptyBuildStats
            { statInputPoints = 1
            , statExistingPoints = 1
            , statDuplicatePoints = 1
            }
    }

-- | Add the frozen locator's observation to the local topology interpreter's
-- operation-owned counters. Direct frozen-site insertion contributes no mutable
-- walk; the degenerate fallback contributes its real mutable walk rather than
-- having it erased from the published result.
withFrozenLocationStats :: LocationStats -> BuildStats -> BuildStats
withFrozenLocationStats walked stats =
  stats
    { statLocationWalkSteps = locationWalkSteps walked + statLocationWalkSteps stats
    , statLocationMaxWalk = max (locationWalkSteps walked) (statLocationMaxWalk stats)
    , statLocationFallbacks = (if locationUsedFallback walked then 1 else 0) + statLocationFallbacks stats
    }

-- | Apply a batch in one page transaction. Duplicate positions are processed
-- in input order, so their last payload wins exactly as repeated 'insert'
-- calls would, while topology is inserted only once per new position.
insertMany
  :: forall mode vertex directed undirected face
   . HasPosition vertex
  => Triangulation mode vertex directed undirected face
  -> V.Vector vertex
  -> Either BuildError (BuildResult mode vertex directed undirected face)
insertMany triangulation input = do
  validateVertices input
  (mapped, frozen, stats) <-
    runTransaction id DenseTransaction triangulation (V.length input) $ \mutable operation -> do
      table <- newMutablePointIndex (pointCapacity mutable)
      seeded <- seedPointTable mutable table
      case seeded of
        Left failure -> pure (Left failure)
        Right () -> do
          mapping <- newPrimArray (V.length input)
          freshBuffer <- MUV.new (V.length input)
          filled <- fill mutable operation table mapping freshBuffer 0 0 0 0
          case filled of
            Left failure -> pure (Left failure)
            Right (sumX, sumY, freshCount) -> do
              let !existingCount = V.length input - freshCount
              setCounter operation CounterInputPoints (V.length input)
              setCounter operation CounterUniquePoints freshCount
              setCounter operation CounterExistingPoints existingCount
              setCounter operation CounterDuplicatePoints existingCount
              inserted <-
                if freshCount == 0
                  then pure (Right 0)
                  else do
                    let !scale = recip (fromIntegral freshCount)
                    arena <-
                      fillRadialArena
                        mutable
                        (sumX * scale)
                        (sumY * scale)
                        (MUV.unsafeRead freshBuffer)
                        freshCount
                    orderedArena <- radiallyOrderArena arena
                    circleSweepInsert mutable operation orderedArena
              case inserted of
                Left failure -> pure (Left failure)
                Right seedCount -> do
                  setCounter operation CounterSpatialSeedPoints seedCount
                  Right <$> unsafeFreezePrimArray mapping
  pure
    BuildResult
      { buildTriangulation = frozen
      , buildInputVertices = mapped
      , buildStats = stats
      }
 where
  fill
    :: forall s
     . MutableDcel s vertex directed undirected face
    -> OperationState s
    -> MutablePointIndex s
    -> MutablePrimArray s Word32
    -> MUV.MVector s Word32
    -> Int
    -> Double
    -> Double
    -> Int
    -> ST s (Either BuildError (Double, Double, Int))
  fill mutable operation table mapping freshBuffer !index !sumX !sumY !freshCount
    | index >= V.length input = pure (Right (sumX, sumY, freshCount))
    | otherwise = do
        let !vertexData = input V.! index
        claimed <- claimPosition mutable table (position vertexData) vertexData
        case claimed of
          Left failure -> pure (Left failure)
          Right (FreshPosition vertex canonicalX canonicalY) -> do
            writePrimArray mapping index (fromIntegral vertex)
            MUV.unsafeWrite freshBuffer freshCount (fromIntegral vertex)
            fill
              mutable
              operation
              table
              mapping
              freshBuffer
              (index + 1)
              (sumX + canonicalX)
              (sumY + canonicalY)
              (freshCount + 1)
          Right (ResidentPosition vertex) -> do
            writePrimArray mapping index (fromIntegral vertex)
            writeVertexData mutable vertex vertexData
            fill mutable operation table mapping freshBuffer (index + 1) sumX sumY freshCount

-- | One packed radial record per swept vertex — the derived sort fields and
-- the vertex handle, nothing else — filled straight from the coordinate
-- arenas and consumed in place by the sweep. The squared distance is stated
-- against the ingress-accumulated centre, in the widened comparison format.
fillRadialArena
  :: MutableDcel s vertex directed undirected face
  -> Double
  -> Double
  -> (Int -> ST s Word32)
  -> Int
  -> ST s (MUV.MVector s (Double, Double, Double, Word32))
fillRadialArena mutable centerX centerY lookupId count = do
  arena <- MUV.new count
  forM_ [0 .. count - 1] $ \index -> do
    raw <- lookupId index
    x <- readPointX mutable (fromIntegral raw)
    y <- readPointY mutable (fromIntegral raw)
    let !wideX = x
        !wideY = y
        !deltaX = centerX - wideX
        !deltaY = centerY - wideY
    MUV.unsafeWrite arena index (deltaX * deltaX + deltaY * deltaY, wideX, wideY, raw)
  pure arena

-- | Claim a position for the vertex the arena would append next, or answer the
-- vertex already holding it. The claim is written before the append, so the two
-- must stay adjacent: nothing may consume a vertex slot in between.
claimPosition
  :: MutableDcel s vertex directed undirected face
  -> MutablePointIndex s
  -> Point
  -> vertex
  -> ST s (Either BuildError PositionClaim)
claimPosition mutable table rawPoint vertexData =
  case rawPoint of
    Point x y -> do
      let !canonicalX = canonicalCoordinate x
          !canonicalY = canonicalCoordinate y
      slot <- nextVertexSlot mutable
      let !candidate = nextVertexSlotIndex slot
      owner <-
        resolveMutablePoint
          table
          (readPointX mutable)
          (readPointY mutable)
          canonicalX
          canonicalY
          candidate
      case owner of
        Left failure -> pure (Left failure)
        Right (Just existing) -> pure (Right (ResidentPosition existing))
        Right Nothing -> do
          vertex <- appendVertexCoordinatesAtSlot mutable slot canonicalX canonicalY vertexData
          pure (Right (FreshPosition vertex canonicalX canonicalY))
-- | Enter the positions a batch inherits from the triangulation it extends, so
-- that an input repeating one of them maps to the vertex already there.
seedPointTable
  :: MutableDcel s vertex directed undirected face
  -> MutablePointIndex s
  -> ST s (Either BuildError ())
seedPointTable mutable table = do
  existing <- pointCount mutable
  seedMutablePointIndex
    table
    existing
    (readPointX mutable)
    (readPointY mutable)

validateVertices :: HasPosition vertex => V.Vector vertex -> Either BuildError ()
validateVertices vertices =
  V.iforM_ vertices (\index vertexData -> validatePoint (Just index) (position vertexData))

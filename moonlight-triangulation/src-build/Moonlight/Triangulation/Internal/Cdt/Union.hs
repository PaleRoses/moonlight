{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Canonical constraint segments and the atomic partial union of two
-- constrained meshes, retaining or combining their site annotations.
module Moonlight.Triangulation.Internal.Cdt.Union
  ( canonicalSegment
  , constraintSegments
  , unionConstrainedWith
  , unionConstrained
  , joinSeparatedConstrained
  , extendConstrainedWith
  , segmentRequest
  , firstRejected
  , completeConstraintConflicts
  , orderedConflict
  , crossingIsRepresented
  ) where

import Control.Monad (foldM)
import Control.Monad.ST (ST)
import qualified Data.Bifunctor as Bifunctor
import Data.Either (isRight)
import Data.List (sort)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Moonlight.Triangulation.Dcel as Dcel
import Moonlight.Triangulation.Handles.HandleDefs
import Moonlight.Triangulation.IntersectionIterator (foldCorridorBetweenPoints)
import Moonlight.Triangulation.Internal.Canonical (canonicalize)
import Moonlight.Triangulation.Insertion (insertPointCombining)
import Moonlight.Triangulation.Internal.Cdt.Batch
  ( finalizeConstraintBatch
  , interpretConstraintRequests
  , recoverConstraints
  )
import Moonlight.Triangulation.Internal.Cdt.Build (fromDelaunay)
import Moonlight.Triangulation.Internal.Cdt.Combinators (foldWhileM)
import Moonlight.Triangulation.Internal.Cdt.Query (constraintEdges)
import Moonlight.Triangulation.Internal.Cdt.Types
import Moonlight.Triangulation.Internal.Join.Rebuild (rebuildCanonicalSiteSet)
import Moonlight.Triangulation.Internal.Join.Seam
  ( executeConstrainedSeam
  , planSeam
  , seamExecutionBuildStats
  , seamExecutionTriangulation
  )
import Moonlight.Triangulation.Internal.Join.SiteSet
  ( SiteSet
  , siteSetAssocs
  , siteSetFromTriangulation
  , siteSetPoints
  , siteSetSize
  , siteSetUnionWith
  )
import Moonlight.Triangulation.Internal.Paged (TransactionShape (DenseTransaction, LocalTransaction))
import Moonlight.Triangulation.Internal.Representation
import Moonlight.Triangulation.Internal.Mutable (MutableDcel)
import Moonlight.Triangulation.Internal.OperationState (OperationState)
import Moonlight.Triangulation.Internal.Transaction (runTransaction)
import Moonlight.Triangulation.Internal.Types
import Moonlight.Triangulation.Math

canonicalSegment :: Point -> Point -> CanonicalSegment
canonicalSegment from to
  | from <= to = CanonicalSegment from to
  | otherwise = CanonicalSegment to from
{-# INLINE canonicalSegment #-}

-- | Geometry of the constraint section, deduplicated and canonically ordered.
constraintSegments
  :: Triangulation 'Constrained vertex directed undirected face
  -> V.Vector (CanonicalSegment)
constraintSegments triangulation =
  V.fromList
    ( Set.toAscList
        ( Set.fromList
            [ canonicalSegment
                (Dcel.vertexPoint triangulation from)
                (Dcel.vertexPoint triangulation to)
            | edge <- constraintEdges triangulation
            , let (from, to) = Dcel.undirectedEndpoints triangulation edge
            ]
        )
    )

-- | Atomic partial union of constrained meshes. Coincident sites combine
-- their annotations before construction. Complete canonical conflict
-- witnesses descend first; a successful branch then reaches the existing
-- batch corridor interpreter exactly once.
unionConstrainedWith
  :: (annotation -> annotation -> annotation)
  -> Triangulation 'Constrained annotation () () ()
  -> Triangulation 'Constrained annotation () () ()
  -> Either
      (ConstrainedUnionError)
      (Triangulation 'Constrained annotation () () ())
unionConstrainedWith combine left right =
  case NonEmpty.nonEmpty (Set.toAscList conflicts) of
    Just witnesses -> Left (ConstraintUnionConflicts witnesses)
    Nothing -> do
      unconstrained <-
        Bifunctor.first
          (ConstraintUnionConstructionFailed . CdtBuildError)
          (rebuildCanonicalSiteSet unionSites)
      requests <- traverse (segmentRequest unconstrained) (V.toList segments)
      recovered <-
        Bifunctor.first ConstraintUnionConstructionFailed
          (recoverConstraints (fromDelaunay unconstrained) (V.fromList requests))
      case firstRejected (constraintBatchOutcomes recovered) of
        Just blocking ->
          Left
            ( ConstraintUnionConstructionFailed
                (ConstraintIntersection blocking)
            )
        Nothing ->
          Bifunctor.first
            (ConstraintUnionConstructionFailed . CdtBuildError)
            (canonicalize (constraintBatchTriangulation recovered))
 where
  leftSites = siteSetFromTriangulation left
  rightSites = siteSetFromTriangulation right
  unionSites = siteSetUnionWith combine leftSites rightSites
  segments =
    V.fromList
      ( Set.toAscList
          ( Set.union
              (Set.fromList (V.toList (constraintSegments left)))
              (Set.fromList (V.toList (constraintSegments right)))
          )
      )
  conflicts = completeConstraintConflicts unionSites segments
{-# INLINE unionConstrainedWith #-}

-- | Atomic partial union specialized to geometry-only constrained meshes.
unionConstrained
  :: Triangulation 'Constrained () () () ()
  -> Triangulation 'Constrained () () () ()
  -> Either
      (ConstrainedUnionError)
      (Triangulation 'Constrained () () () ())
unionConstrained = unionConstrainedWith (\_ _ -> ())
{-# INLINE unionConstrained #-}

-- | Join two strictly separated constrained triangulations by copying both
-- source meshes and zippering only their common tangent corridor. The source
-- constraint planes are present during legalization, so contour edges are
-- immutable barriers rather than edges recovery tries to resurrect after a
-- solved face has already disappeared.
--
-- Strict x-separation proves that the two site sets have no coincident point,
-- so annotations are copied from their source mesh and never combined.
joinSeparatedConstrained
  :: Triangulation 'Constrained annotation () () ()
  -> Triangulation 'Constrained annotation () () ()
  -> Either
      (ConstrainedUnionError)
      (ConstrainedSeamResult annotation)
joinSeparatedConstrained left right = do
  seamPlan <- maybe (Left ConstraintUnionNotSeparated) Right (planSeam left right)
  seamExecution <-
    Bifunctor.first
      (ConstraintUnionConstructionFailed . CdtBuildError)
      (executeConstrainedSeam seamPlan left right)
  let copied = seamExecutionTriangulation seamExecution
      leftSegments = constraintSegments left
      rightSegments = constraintSegments right
      copiedSegments = Set.fromList (V.toList (constraintSegments copied))
      missingSegments =
        V.filter (`Set.notMember` copiedSegments) (leftSegments V.++ rightSegments)
  requests <- traverse (segmentRequest copied) (V.toList missingSegments)
  recovered <-
    Bifunctor.first ConstraintUnionConstructionFailed
      (recoverConstraints copied (V.fromList requests))
  case firstRejected (constraintBatchOutcomes recovered) of
    Just blocking ->
      Left
        ( ConstraintUnionConstructionFailed
            (ConstraintIntersection blocking)
        )
    Nothing -> do
      published <-
        Bifunctor.first
          (ConstraintUnionConstructionFailed . CdtBuildError)
          (canonicalize (constraintBatchTriangulation recovered))
      targetFaces <- targetFaceIndex published
      leftFaces <-
        sourceFaceEvidence
          ConstrainedSeamLeftSource
          left
          targetFaces
      rightFaces <-
        sourceFaceEvidence
          ConstrainedSeamRightSource
          right
          targetFaces
      let preservedTargets =
            Set.fromList
              ( fmap constrainedSeamTargetFace (V.toList leftFaces)
                  <> fmap constrainedSeamTargetFace (V.toList rightFaces)
              )
          newFaces =
            V.fromList
              ( filter
                  (`Set.notMember` preservedTargets)
                  (innerFaceIds published)
              )
          recoveredOutcomes =
            Map.fromList
              (V.toList (V.zip missingSegments (constraintBatchOutcomes recovered)))
          finalSegments = Set.fromList (V.toList (constraintSegments published))
      leftConstraintEvidence <-
        sourceConstraintEvidence
          ConstrainedSeamLeftSource
          finalSegments
          recoveredOutcomes
          leftSegments
      rightConstraintEvidence <-
        sourceConstraintEvidence
          ConstrainedSeamRightSource
          finalSegments
          recoveredOutcomes
          rightSegments
      pure
        ConstrainedSeamResult
          { constrainedSeamResultTriangulation = published
          , constrainedSeamLeftFaceEvidence = leftFaces
          , constrainedSeamRightFaceEvidence = rightFaces
          , constrainedSeamNewFaces = newFaces
          , constrainedSeamLeftConstraintEvidence = leftConstraintEvidence
          , constrainedSeamRightConstraintEvidence = rightConstraintEvidence
          , constrainedSeamConstraintStats = constraintBatchStats recovered
          , constrainedSeamBuildStats = seamExecutionBuildStats seamExecution
          }

data CanonicalFaceKey = CanonicalFaceKey
  !(Point)
  !(Point)
  !(Point)
  deriving stock (Eq, Ord)

targetFaceIndex
  :: Triangulation mode vertex directed undirected face
  -> Either
      (ConstrainedUnionError)
      (Map.Map CanonicalFaceKey FaceId)
targetFaceIndex triangulation =
  foldM insertTarget Map.empty (innerFaceIds triangulation)
 where
  insertTarget
    :: Map.Map CanonicalFaceKey FaceId
    -> FaceId
    -> Either (ConstrainedUnionError) (Map.Map CanonicalFaceKey FaceId)
  insertTarget index face = do
    key <- targetFaceKey triangulation face
    case Map.lookup key index of
      Just existing -> Left (ConstraintUnionTargetFaceAmbiguous existing face)
      Nothing -> Right (Map.insert key face index)

sourceFaceEvidence
  :: ConstrainedSeamSource
  -> Triangulation mode vertex directed undirected face
  -> Map.Map CanonicalFaceKey FaceId
  -> Either
      (ConstrainedUnionError)
      (V.Vector (ConstrainedSeamFaceEvidence))
sourceFaceEvidence source triangulation targetFaces =
  V.fromList <$> traverse evidenceFor (innerFaceIds triangulation)
 where
  evidenceFor sourceFace = do
    key@(CanonicalFaceKey first second third) <-
      sourceFaceKey source triangulation sourceFace
    targetFace <-
      maybe
        (Left (ConstraintUnionSourceFaceNotPreserved source sourceFace))
        Right
        (Map.lookup key targetFaces)
    pure
      ConstrainedSeamFaceEvidence
        { constrainedSeamSourceFace = sourceFace
        , constrainedSeamTargetFace = targetFace
        , constrainedSeamFaceFirstPoint = first
        , constrainedSeamFaceSecondPoint = second
        , constrainedSeamFaceThirdPoint = third
        }

sourceConstraintEvidence
  :: ConstrainedSeamSource
  -> Set.Set (CanonicalSegment)
  -> Map.Map (CanonicalSegment) ConstraintOutcome
  -> V.Vector (CanonicalSegment)
  -> Either
      (ConstrainedUnionError)
      (V.Vector (ConstrainedSeamConstraintEvidence))
sourceConstraintEvidence source finalSegments recovered =
  traverse
    (\segment ->
       if Set.member segment finalSegments
         then
           Right
             ConstrainedSeamConstraintEvidence
               { constrainedSeamConstraintSegment = segment
               , constrainedSeamConstraintRecovery = Map.lookup segment recovered
               }
         else Left (ConstraintUnionSourceConstraintNotPreserved source segment)
    )

sourceFaceKey
  :: ConstrainedSeamSource
  -> Triangulation mode vertex directed undirected face
  -> FaceId
  -> Either (ConstrainedUnionError) CanonicalFaceKey
sourceFaceKey source triangulation face =
  case sort (fmap (Dcel.vertexPoint triangulation) (Dcel.faceVertices triangulation face)) of
    [first, second, third] -> Right (CanonicalFaceKey first second third)
    points ->
      Left
        ( ConstraintUnionSourceFaceNotTriangular
            source
            face
            (length points)
        )

targetFaceKey
  :: Triangulation mode vertex directed undirected face
  -> FaceId
  -> Either (ConstrainedUnionError) CanonicalFaceKey
targetFaceKey triangulation face =
  case sort (fmap (Dcel.vertexPoint triangulation) (Dcel.faceVertices triangulation face)) of
    [first, second, third] -> Right (CanonicalFaceKey first second third)
    points -> Left (ConstraintUnionTargetFaceNotTriangular face (length points))

innerFaceIds
  :: Triangulation mode vertex directed undirected face
  -> [FaceId]
innerFaceIds triangulation =
  fmap (FaceId . fromIntegral) [1 .. Dcel.numFaces triangulation - 1]

-- | Extend one already-resident constrained triangulation with one new
-- constrained section. This is intentionally asymmetric: the base mesh is
-- thawed once, extension sites are inserted into it, and only the extension's
-- constraint section is replayed. Unlike 'unionConstrainedWith', it neither
-- rebuilds a canonical site set nor replays base constraints, because both
-- would erase the physical distinction between solved base and new work.
--
-- Incoming constraint recovery is itself the spatial conflict authority. It
-- walks only the incoming corridors against the resident base and returns a
-- typed intersection obstruction. Re-running the canonical all-pairs union
-- preflight here would make a tiny extension quadratic in the base. Any
-- structural or recovery obstruction abandons the transaction before a
-- partially extended mesh can be published.
extendConstrainedWith
  :: (annotation -> annotation -> annotation)
  -> Triangulation 'Constrained annotation () () ()
  -> Triangulation 'Constrained annotation () () ()
  -> Either
      (ConstrainedUnionError)
      (ConstrainedExtensionResult annotation () () ())
extendConstrainedWith combine base extension = do
    (completed, extended, buildStats) <-
      runTransaction
        (ConstraintUnionConstructionFailed . CdtBuildError)
        transactionShape
        base
        (siteSetSize extensionSites)
        (insertAndRecoverExtension combine extensionSites extensionSegments)
    pure
      ConstrainedExtensionResult
        { constrainedExtensionConstraintBatch = finalizeConstraintBatch extended completed
        , constrainedExtensionBuildStats = buildStats
        }
 where
  extensionSites = siteSetFromTriangulation extension
  extensionSegments = constraintSegments extension
  transactionShape =
    case V.uncons extensionSegments of
      Just (segment, remaining)
        | Dcel.numVertices base >= 200000
        , siteSetSize extensionSites <= 128
        , V.null remaining
        , residentCorridorIsEmpty segment -> LocalTransaction
      _ -> DenseTransaction
  residentCorridorIsEmpty segment =
    case (mkQueryPoint (segmentStart segment), mkQueryPoint (segmentEnd segment)) of
      (Right from, Right to) ->
        foldCorridorBetweenPoints base from to (\_ _ -> Left ()) () == Just (Right ())
      _ -> False
{-# INLINE extendConstrainedWith #-}

insertAndRecoverExtension
  :: (annotation -> annotation -> annotation)
  -> SiteSet annotation
  -> V.Vector (CanonicalSegment)
  -> MutableDcel s annotation () () ()
  -> OperationState s
  -> ST s (Either (ConstrainedUnionError) ConstraintBatchAccumulator)
insertAndRecoverExtension combine extensionSites extensionSegments mutable operation = do
  placed <- insertExtensionSites combine extensionSites mutable operation
  case placed of
    Left obstruction -> pure (Left obstruction)
    Right handles ->
      case traverse (segmentRequestFromHandles handles) (V.toList extensionSegments) of
        Left obstruction -> pure (Left obstruction)
        Right requests -> do
          interpreted <-
            fmap
              (Bifunctor.first ConstraintUnionConstructionFailed)
              (interpretConstraintRequests (V.fromList requests) mutable operation)
          case interpreted of
            Left obstruction -> pure (Left obstruction)
            Right completed ->
              case firstRejected (accumulatorOutcomes completed) of
                Just blocking ->
                  pure
                    ( Left
                        ( ConstraintUnionConstructionFailed
                            (ConstraintIntersection blocking)
                        )
                    )
                Nothing -> pure (Right completed)

insertExtensionSites
  :: forall s annotation
   . (annotation -> annotation -> annotation)
  -> SiteSet annotation
  -> MutableDcel s annotation () () ()
  -> OperationState s
  -> ST s (Either (ConstrainedUnionError) (Map.Map (Point) VertexId))
insertExtensionSites combine extensionSites mutable operation =
  fmap
    (fmap (Map.fromDistinctAscList . reverse))
    ( foldWhileM
        isRight
        insertOne
        (Right [])
        (siteSetAssocs extensionSites)
    )
 where
  insertOne
    :: Either (ConstrainedUnionError) [(Point, VertexId)]
    -> (Point, annotation)
    -> ST s (Either (ConstrainedUnionError) [(Point, VertexId)])
  insertOne rejected@(Left _) _ = pure rejected
  insertOne (Right accumulated) (point, annotation) =
    fmap
      ( Bifunctor.first (ConstraintUnionConstructionFailed . CdtBuildError)
          . fmap
            (\(vertex, _) -> (point, VertexId (fromIntegral vertex)) : accumulated)
      )
      (insertPointCombining combine Nothing mutable operation point annotation)

accumulatorOutcomes :: ConstraintBatchAccumulator -> V.Vector ConstraintOutcome
accumulatorOutcomes = V.fromList . reverse . accumulatedConstraintOutcomes
{-# INLINE accumulatorOutcomes #-}

segmentRequest
  :: Triangulation mode annotation () () ()
  -> CanonicalSegment
  -> Either (ConstrainedUnionError) (VertexId, VertexId)
segmentRequest triangulation segment =
  segmentRequestFromHandles handles segment
 where
  handles =
    Map.fromList
      [ ( Dcel.vertexPoint triangulation vertex
        , vertex
        )
      | raw <- [0 .. Dcel.numVertices triangulation - 1]
      , let vertex = VertexId (fromIntegral raw)
      ]

segmentRequestFromHandles
  :: Map.Map (Point) VertexId
  -> CanonicalSegment
  -> Either (ConstrainedUnionError) (VertexId, VertexId)
segmentRequestFromHandles handles segment =
  case
      ( Map.lookup (segmentStart segment) handles
      , Map.lookup (segmentEnd segment) handles
      ) of
    (Just from, Just to) -> Right (from, to)
    (Nothing, _) -> Left (ConstraintUnionSiteMissing (segmentStart segment))
    (_, Nothing) -> Left (ConstraintUnionSiteMissing (segmentEnd segment))

firstRejected :: V.Vector ConstraintOutcome -> Maybe UndirectedEdgeId
firstRejected =
  V.foldr
    (\outcome later ->
       case outcome of
         ConstraintAccepted _ _ -> later
         ConstraintRejected blocking -> Just blocking
    )
    Nothing

completeConstraintConflicts
  :: SiteSet annotation
  -> V.Vector (CanonicalSegment)
  -> Set.Set (ConstraintConflict)
completeConstraintConflicts unionSites segments =
  Set.fromList
    [ orderedConflict leftSegment rightSegment
    | leftIndex <- [0 .. V.length segments - 1]
    , rightIndex <- [leftIndex + 1 .. V.length segments - 1]
    , let leftSegment = segments V.! leftIndex
    , let rightSegment = segments V.! rightIndex
    , segmentsProperlyCross
        (segmentStart leftSegment)
        (segmentEnd leftSegment)
        (segmentStart rightSegment)
        (segmentEnd rightSegment)
    , not (crossingIsRepresented unionSites leftSegment rightSegment)
    ]

orderedConflict
  :: CanonicalSegment
  -> CanonicalSegment
  -> ConstraintConflict
orderedConflict left right
  | left <= right = ConstraintConflict left right
  | otherwise = ConstraintConflict right left

crossingIsRepresented
  :: SiteSet annotation
  -> CanonicalSegment
  -> CanonicalSegment
  -> Bool
crossingIsRepresented sites first second =
  V.any
    (\point ->
       onClosedSegment (segmentStart first) (segmentEnd first) point
         && onClosedSegment (segmentStart second) (segmentEnd second) point
    )
    (siteSetPoints sites)

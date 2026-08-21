{-# LANGUAGE NamedFieldPuns #-}

-- | Ruppert refinement: Steiner insertion composed after a built mesh, never a
-- second kind of mesh. Parameters are reached through checked verbs rather than
-- a raw record, so an unrealizable quality bar is a refusal.
module Moonlight.Triangulation.Refinement
  ( refine
  , refineWithinDomain
  , validateRefinementParameters
  , withMinimumAngle
  , radiusEdgeRatioForAngle
  ) where

import Control.Monad.ST (ST)
import Data.Foldable (traverse_)
import qualified Data.IntSet as IntSet
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Vector as V
import Moonlight.Triangulation.Dcel
  ( faceVertices
  , incidentFace
  , isConstraintEdge
  , numFaces
  , numUndirectedEdges
  , numVertices
  , undirectedEndpoints
  , vertexPoint
  )
import Moonlight.Triangulation.FloodFillIterator (facesAtEvenBarrierDepth)
import Moonlight.Triangulation.Handles.HandleDefs
  ( FaceId (..)
  , UndirectedEdgeId (..)
  , directedPair
  )
import Moonlight.Triangulation.Internal.Mutable
import Moonlight.Triangulation.Internal.Paged (TransactionShape (DenseTransaction))
import Moonlight.Triangulation.Internal.Refinement
import Moonlight.Triangulation.Internal.Transaction (runTransaction)
import Moonlight.Triangulation.Types
import Moonlight.Triangulation.Validation (validateTopology)

data RefinementExecution mode vertex directed undirected face = RefinementExecution
  { refinementExecutionResult :: !(RefinementResult mode vertex directed undirected face)
  , refinementExecutionVisitedFaces :: ![Int]
  , refinementExecutionInterfaceBoundaryReads :: {-# UNPACK #-} !Int
  , refinementExecutionBoundaryCrossingAttempts :: {-# UNPACK #-} !Int
  }

-- | The radius-edge ratio admitting a minimum angle, in degrees.
radiusEdgeRatioForAngle :: Double -> Either BuildError (Maybe Double)
radiusEdgeRatioForAngle degrees =
  case classifyNonFinite degrees of
    Just nonFinite ->
      Left (RefinementMinimumAngleNotFinite nonFinite)
    Nothing
      | degrees < 0 || degrees > 60 ->
          Left (RefinementMinimumAngleOutOfRange degrees)
      | degrees == 0 -> Right Nothing
      | otherwise ->
          let ratio = 0.5 / sin (degrees * pi / 180)
           in case classifyNonFinite ratio of
                Just nonFinite ->
                  Left (RefinementMinimumAngleDerivedRatioNotFinite nonFinite)
                Nothing -> Right (Just ratio)

-- | Set the minimum angle, in degrees.
withMinimumAngle :: Double -> RefinementParameters -> Either BuildError (RefinementParameters)
withMinimumAngle degrees parameters = do
  ratio <- radiusEdgeRatioForAngle degrees
  pure parameters{refineMaxRadiusEdgeRatio = ratio}

-- | Refine an unconstrained or constrained triangulation in the same finite
-- DCEL. The constructor supplies application payloads for Steiner vertices;
-- payloads annotate the requested geometric point and cannot reauthor it.
refine
  :: (Point -> vertex)
  -> RefinementParameters
  -> Triangulation mode vertex directed undirected face
  -> Either BuildError (RefinementResult mode vertex directed undirected face)
refine makeVertex parameters =
  fmap refinementExecutionResult
    . refineWithInitialSeed RefineEveryFace Nothing makeVertex parameters

-- | Prove that a set of active faces is separated from every protected inner
-- face by exactly the supplied immutable edge section. The witness is tied to
-- the input arena cardinalities and is revalidated before interpretation.
mkRefinementDomain
  :: Set.Set FaceId
  -> Set.Set UndirectedEdgeId
  -> Triangulation mode vertex directed undirected face
  -> Either BuildError RefinementDomain
mkRefinementDomain permittedFaces interfaceEdges triangulation = do
  permitted <- validateSeedFaces triangulation permittedFaces
  interface <- IntSet.fromList <$> traverse validateInterfaceEdge (Set.toAscList interfaceEdges)
  let expected = expectedInterface permitted
  case IntSet.lookupMin (expected IntSet.\\ interface) of
    Just pair -> Left (RefinementDomainInterfaceMissing (UndirectedEdgeId (fromIntegral pair)))
    Nothing ->
      case IntSet.lookupMin (interface IntSet.\\ expected) of
        Just pair -> Left (RefinementDomainInterfaceExtraneous (UndirectedEdgeId (fromIntegral pair)))
        Nothing ->
          Right
            RefinementDomain
              { refinementDomainPermittedFaces = permitted
              , refinementDomainInterfacePairs = interface
              , refinementDomainInputFaces =
                  Map.fromAscList
                    [ (face, faceSignature triangulation face)
                    | raw <- [1 .. numFaces triangulation - 1]
                    , let face = FaceId (fromIntegral raw)
                    ]
              , refinementDomainInputFaceCount = numFaces triangulation
              , refinementDomainInputEdgeCount = numUndirectedEdges triangulation
              }
 where
  totalEdges = numUndirectedEdges triangulation
  validateInterfaceEdge edge@(UndirectedEdgeId raw)
    | toInteger raw >= toInteger totalEdges =
        Left (RefinementDomainInterfaceEdgeNotActive edge totalEdges)
    | otherwise = Right (fromIntegral raw)

  expectedInterface permitted =
    IntSet.fromList
      [ pair
      | pair <- [0 .. totalEdges - 1]
      , let edge = UndirectedEdgeId (fromIntegral pair)
            (forward, backward) = directedPair edge
            FaceId forwardFace = incidentFace triangulation forward
            FaceId backwardFace = incidentFace triangulation backward
            forwardInner = forwardFace /= 0
            backwardInner = backwardFace /= 0
            forwardPermitted = IntSet.member (fromIntegral forwardFace) permitted
            backwardPermitted = IntSet.member (fromIntegral backwardFace) permitted
      , forwardInner && backwardInner && forwardPermitted /= backwardPermitted
      ]

-- | Refine exactly one checked local section. Interface edges are installed as
-- transaction-local legalization barriers and removed before publication;
-- every protected face signature is then compared with the input witness.
refineWithinDomain
  :: (Point -> vertex)
  -> RefinementParameters
  -> Set.Set FaceId
  -> Set.Set UndirectedEdgeId
  -> Triangulation mode vertex directed undirected face
  -> Either BuildError (RefinementDomainResult mode vertex directed undirected face)
refineWithinDomain makeVertex parameters permittedFaces interfaceEdges triangulation = do
  validateDomainParameters parameters
  domain <- mkRefinementDomain permittedFaces interfaceEdges triangulation
  execution <-
    refineWithInitialSeed
      (RefineSeededFaces (refinementDomainPermittedFaces domain))
      (Just domain)
      makeVertex
      parameters
      triangulation
  let result = refinementExecutionResult execution
      receipt =
        buildRefinementReceipt
          domain
          triangulation
          (refinedTriangulation result)
          (refinementExecutionVisitedFaces execution)
          (refinementExecutionInterfaceBoundaryReads execution)
          (refinementExecutionBoundaryCrossingAttempts execution)
  validateProtectedFaces domain triangulation (refinedTriangulation result)
  case V.toList (refinementVisitedProtectedFaces receipt) of
    protected : _ -> Left (RefinementDomainWouldRewriteProtectedFace protected)
    [] ->
      Right
        RefinementDomainResult
          { refinementDomainResult = result
          , refinementDomainReceipt = receipt
          }

validateDomainParameters :: RefinementParameters -> Either BuildError ()
validateDomainParameters parameters
  | not (refinePreserveConvexHull parameters) =
      Left RefinementDomainRequiresConvexHullPreservation
  | not (refineKeepConstraintEdges parameters) =
      Left RefinementDomainRequiresConstraintPreservation
  | refineExcludeOuterFaces parameters =
      Left RefinementDomainForbidsOuterFaceExclusion
  | otherwise = Right ()

refineWithInitialSeed
  :: RefinementInitialSeed
  -> Maybe RefinementDomain
  -> (Point -> vertex)
  -> RefinementParameters
  -> Triangulation mode vertex directed undirected face
  -> Either BuildError (RefinementExecution mode vertex directed undirected face)
refineWithInitialSeed initialSeed domain makeVertex parameters triangulation = do
  validateRefinementParameters parameters
  case validateTopology triangulation of
    violation : _ -> Left (RefinementInputTopologyInvalid violation)
    [] -> pure ()
  let originalCount = numVertices triangulation
      budget = max 0 (fromMaybe (10 * max 1 originalCount) (refineMaxAdditionalVertices parameters))
      initialExcludedFaces =
        if refineExcludeOuterFaces parameters
          then
            IntSet.fromList
              [ fromIntegral raw
              | FaceId raw <-
                  facesAtEvenBarrierDepth
                    triangulation
                    (isConstraintEdge triangulation)
              ]
          else IntSet.empty
  ( outcome
    , frozen
    , stats
    ) <-
    runTransaction id DenseTransaction triangulation budget $ \mutable operation -> do
      installInterfaceBarriers mutable domain
      refinement <-
        refineMutable
          makeVertex
          mutable
          operation
          parameters
          originalCount
          initialExcludedFaces
          initialSeed
          domain
      removeInterfaceBarriers mutable triangulation domain
      pure refinement
  let (complete, added, excluded, visited, interfaceBoundaryReads, boundaryCrossingAttempts) = outcome
  pure
    RefinementExecution
      { refinementExecutionResult =
          RefinementResult
            { refinedTriangulation = frozen
            , refinementStats = stats
            , refinementAddedVertices = added
            , refinementComplete = complete
            , refinementExcludedFaces = V.fromList (map (FaceId . fromIntegral) excluded)
            }
      , refinementExecutionVisitedFaces = visited
      , refinementExecutionInterfaceBoundaryReads = interfaceBoundaryReads
      , refinementExecutionBoundaryCrossingAttempts = boundaryCrossingAttempts
      }

installInterfaceBarriers
  :: MutableDcel s vertex directed undirected face
  -> Maybe RefinementDomain
  -> ST s ()
installInterfaceBarriers mutable =
  traverse_
    (\pair -> setConstraint mutable (2 * pair) >> pure ())
    . maybe [] (IntSet.toAscList . refinementDomainInterfacePairs)

removeInterfaceBarriers
  :: MutableDcel s vertex directed undirected face
  -> Triangulation mode vertex directed undirected face
  -> Maybe RefinementDomain
  -> ST s ()
removeInterfaceBarriers mutable input =
  traverse_
    (\pair ->
       let edge = UndirectedEdgeId (fromIntegral pair)
        in if isConstraintEdge input edge
             then pure ()
             else clearConstraint mutable (2 * pair) >> pure ()
    )
    . maybe [] (IntSet.toAscList . refinementDomainInterfacePairs)

validateProtectedFaces
  :: RefinementDomain
  -> Triangulation mode vertex directed undirected face
  -> Triangulation mode vertex directed undirected face
  -> Either BuildError ()
validateProtectedFaces domain before after =
  traverse_ validateProtected protectedFaces
 where
  protectedFaces =
    [ FaceId (fromIntegral face)
    | face <- [1 .. refinementDomainInputFaceCount domain - 1]
    , IntSet.notMember face (refinementDomainPermittedFaces domain)
    ]
  validateProtected face
    | Map.lookup face (refinementDomainInputFaces domain)
        == Just (faceSignature before face)
        && Map.lookup face (refinementDomainInputFaces domain)
          == Just (faceSignature after face) = Right ()
    | otherwise = Left (RefinementDomainProtectedFaceChanged face)

faceSignature
  :: Triangulation mode vertex directed undirected face
  -> FaceId
  -> [Point]
faceSignature triangulation =
  sort . fmap (vertexPoint triangulation) . faceVertices triangulation

type EdgeSignature = (Point, Point, [FaceId])

edgeSignature
  :: Triangulation mode vertex directed undirected face
  -> UndirectedEdgeId
  -> EdgeSignature
edgeSignature triangulation edge =
  (min fromPoint toPoint, max fromPoint toPoint, sort [forwardFace, backwardFace])
 where
  (fromVertex, toVertex) = undirectedEndpoints triangulation edge
  fromPoint = vertexPoint triangulation fromVertex
  toPoint = vertexPoint triangulation toVertex
  (forward, backward) = directedPair edge
  forwardFace = incidentFace triangulation forward
  backwardFace = incidentFace triangulation backward

buildRefinementReceipt
  :: RefinementDomain
  -> Triangulation mode vertex directed undirected face
  -> Triangulation mode vertex directed undirected face
  -> [Int]
  -> Int
  -> Int
  -> RefinementReceipt
buildRefinementReceipt domain before after visited interfaceBoundaryReads boundaryCrossingAttempts =
  RefinementReceipt
    { refinementVisitedJoinFaces = V.fromList (fmap toFace visitedJoin)
    , refinementVisitedProtectedFaces = V.fromList (fmap toFace visitedProtected)
    , refinementCreatedFaces =
        V.fromList
          [ targetFace
          | face <- [1 .. numFaces after - 1]
          , let targetFace = toFace face
          , face >= numFaces before
              || faceSignature before targetFace /= faceSignature after targetFace
          ]
    , refinementTouchedEdges = V.fromList touched
    , refinementRemovedEdges = V.fromList removed
    , refinementInterfaceBoundaryReads = interfaceBoundaryReads
    , refinementAttemptedBoundaryCrossings = boundaryCrossingAttempts
    }
 where
  permitted = refinementDomainPermittedFaces domain
  (visitedProtected, visitedJoin) =
    foldr
      (\face (protected, join) ->
         if IntSet.member face permitted || face >= refinementDomainInputFaceCount domain
           then (protected, face : join)
           else (face : protected, join)
      )
      ([], [])
      visited
  toFace = FaceId . fromIntegral
  beforeEdges = numUndirectedEdges before
  afterEdges = numUndirectedEdges after
  touched =
    [ edge
    | raw <- [0 .. afterEdges - 1]
    , let edge = UndirectedEdgeId (fromIntegral raw)
    , raw >= beforeEdges || edgeSignature before edge /= edgeSignature after edge
    ]
  removed =
    [ edge
    | raw <- [0 .. beforeEdges - 1]
    , let edge = UndirectedEdgeId (fromIntegral raw)
    , raw >= afterEdges || edgeSignature before edge /= edgeSignature after edge
    ]

-- | Refuse outer or absent faces rather than silently treating an invalid
-- topology witness as an empty repair. Face zero is the outer face and has no
-- active refinement equation.
validateSeedFaces
  :: Triangulation mode vertex directed undirected face
  -> Set.Set FaceId
  -> Either BuildError IntSet.IntSet
validateSeedFaces triangulation requested =
  IntSet.fromList <$> traverse validateFace (Set.toAscList requested)
 where
  totalFaces = numFaces triangulation

  validateFace face@(FaceId raw)
    | raw == 0 || toInteger raw >= toInteger totalFaces =
        Left (RefinementSeedFaceNotActive face totalFaces)
    | otherwise = Right (fromIntegral raw)

-- | Refuse parameters no mesh can satisfy.
validateRefinementParameters :: RefinementParameters -> Either BuildError ()
validateRefinementParameters RefinementParameters{refineMaxAdditionalVertices, refineMinArea, refineMaxArea, refineMaxRadiusEdgeRatio} = do
  case refineMaxAdditionalVertices of
    Just value
      | value < 0 ->
          Left (RefinementMaximumAdditionalVerticesNegative value)
    _ -> Right ()
  validateOptionalRefinementParameter
    RefinementMinimumAreaNotFinite
    RefinementMinimumAreaNegative
    (>= 0)
    refineMinArea
  validateOptionalRefinementParameter
    RefinementMaximumAreaNotFinite
    RefinementMaximumAreaNotPositive
    (> 0)
    refineMaxArea
  validateOptionalRefinementParameter
    RefinementMaximumRadiusEdgeRatioNotFinite
    RefinementMaximumRadiusEdgeRatioNotPositive
    (> 0)
    refineMaxRadiusEdgeRatio
  case (refineMinArea, refineMaxArea) of
    (Just minimumArea, Just maximumArea)
      | minimumArea > maximumArea ->
          Left
            ( RefinementMinimumAreaExceedsMaximum
                minimumArea
                maximumArea
            )
    _ -> Right ()

validateOptionalRefinementParameter
  :: (NonFiniteValue -> BuildError)
  -> (Double -> BuildError)
  -> (Double -> Bool)
  -> Maybe Double
  -> Either BuildError ()
validateOptionalRefinementParameter nonFinite outsideRange predicate value =
  case value of
    Nothing -> Right ()
    Just number ->
      case classifyNonFinite number of
        Just nonFiniteValue ->
          Left (nonFinite nonFiniteValue)
        Nothing
          | predicate number -> Right ()
          | otherwise ->
              Left (outsideRange number)

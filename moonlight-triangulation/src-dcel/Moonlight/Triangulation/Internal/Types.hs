{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The vocabulary: the types the surface names, none of which mentions the
-- stored representation.
module Moonlight.Triangulation.Internal.Types
  ( Point (..)
  , SiteRelation (..)
  , QueryPoint (..)
  , PointValidationError (..)
  , HasPosition (..)
  , ElementDefaults (..)
  , unitElementDefaults
  , ConstraintMode (..)
  , KnownConstraintMode (..)
  , InsertionDisposition (..)
  , BuildStats (..)
  , emptyBuildStats
  , CoordinateError (..)
  , NonFiniteValue (..)
  , classifyNonFinite
  , BuildError (..)
  , Location (..)
  , LocationHint (..)
  , LocationStats (..)
  , emptyLocationStats
  , NearestStats (..)
  , RefinementParameters (..)
  , defaultRefinementParameters
  , InvariantViolation (..)
  ) where

import Control.DeepSeq (NFData)
import Moonlight.Triangulation.Handles.HandleDefs
  ( DirectedEdgeId
  , FaceId
  , UndirectedEdgeId
  , VertexId
  )
import Data.Word (Word8)
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable (..), peekElemOff, pokeElemOff)
import GHC.Generics (Generic)
import Moonlight.Triangulation.Internal.BoxedPaged (BoxedStorageError)

-- | Type-level witness for whether constraint flags may be present.
data ConstraintMode = Unconstrained | Constrained
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Recover a type-level constraint mode as a value.
class KnownConstraintMode (mode :: ConstraintMode) where
  constraintModeValue :: proxy mode -> ConstraintMode

instance KnownConstraintMode 'Unconstrained where
  constraintModeValue _ = Unconstrained

instance KnownConstraintMode 'Constrained where
  constraintModeValue _ = Constrained

-- | Cartesian binary64 point.
data Point = Point
  { pointX :: !Double
  , pointY :: !Double
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Exact geometric relation between two finite coordinate supports. The
-- overlap count is strictly positive in 'PartialOverlap'; equality, subset and
-- disjointness have already been excluded before that constructor is chosen.
data SiteRelation
  = -- | Both supports contain exactly the same coordinates.
    EqualSites
  | -- | Every left coordinate occurs on the right, which has at least one more.
    LeftProperSubset
  | -- | Every right coordinate occurs on the left, which has at least one more.
    RightProperSubset
  | -- | The supports share no coordinate.
    DisjointSites
  | -- | Neither support contains the other; the field is the positive number
    -- of coordinates they share.
    PartialOverlap {-# UNPACK #-} !Int
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | A coordinate pair admitted to the exact-predicate domain and normalized
-- at its construction boundary. Query algorithms consume this phase rather
-- than each inventing a fallback for invalid floating-point input.
newtype QueryPoint = QueryPoint
  { -- | The admitted, canonically normalized point.
    queryPointValue :: Point
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Coordinate axis and reason that kept a point outside the query domain.
data PointValidationError
  = InvalidPointX !CoordinateError
  | InvalidPointY !CoordinateError
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

instance Storable (Point) where
  sizeOf _ = 2 * sizeOf (0 :: Double)
  alignment _ = alignment (0 :: Double)
  peek pointer = do
    x <- peekElemOff (castPtr pointer) 0
    y <- peekElemOff (castPtr pointer) 1
    pure (Point x y)
  poke pointer (Point x y) = do
    pokeElemOff (castPtr pointer) 0 x
    pokeElemOff (castPtr pointer) 1 y

-- | Extract a vertex's position once, at the construction boundary.
class HasPosition vertex where
  position :: vertex -> Point

instance HasPosition (Point) where
  position = id

-- | Payloads inherited by topology elements created after initial loading.
data ElementDefaults directed undirected face = ElementDefaults
  { defaultDirectedEdgeData :: !directed
  , defaultUndirectedEdgeData :: !undirected
  , defaultFaceData :: !face
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Payload defaults for geometry-only triangulations.
unitElementDefaults :: ElementDefaults () () ()
unitElementDefaults = ElementDefaults () () ()

-- | Construction, location, legalization, and refinement work counters.
data BuildStats = BuildStats
  { statInputPoints :: {-# UNPACK #-} !Int
  , statUniquePoints :: {-# UNPACK #-} !Int
  , statExistingPoints :: {-# UNPACK #-} !Int
  , statDuplicatePoints :: {-# UNPACK #-} !Int
  , statSpatialSeedPoints :: {-# UNPACK #-} !Int
  , statFaceSplits :: {-# UNPACK #-} !Int
  , statInteriorEdgeSplits :: {-# UNPACK #-} !Int
  , statBoundaryEdgeSplits :: {-# UNPACK #-} !Int
  , statHullInsertions :: {-# UNPACK #-} !Int
  , statLineSplits :: {-# UNPACK #-} !Int
  , statLineExtensions :: {-# UNPACK #-} !Int
  , statLineToAreaTransitions :: {-# UNPACK #-} !Int
  , statEdgeFlips :: {-# UNPACK #-} !Int
  , statLocationWalkSteps :: {-# UNPACK #-} !Int
  , statLocationFallbacks :: {-# UNPACK #-} !Int
  , statLocationMaxWalk :: {-# UNPACK #-} !Int
  , statLegalizationMaxStack :: {-# UNPACK #-} !Int
  , statSteinerPoints :: {-# UNPACK #-} !Int
  , statRefinementFaceChecks :: {-# UNPACK #-} !Int
  , statRefinementQueuePops :: {-# UNPACK #-} !Int
  , statSweepFastPoints :: {-# UNPACK #-} !Int
  , statSweepSkippedPoints :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (NFData)

-- | The additive identity for construction telemetry.
emptyBuildStats :: BuildStats
emptyBuildStats =
  BuildStats
    { statInputPoints = 0
    , statUniquePoints = 0
    , statExistingPoints = 0
    , statDuplicatePoints = 0
    , statSpatialSeedPoints = 0
    , statFaceSplits = 0
    , statInteriorEdgeSplits = 0
    , statBoundaryEdgeSplits = 0
    , statHullInsertions = 0
    , statLineSplits = 0
    , statLineExtensions = 0
    , statLineToAreaTransitions = 0
    , statEdgeFlips = 0
    , statLocationWalkSteps = 0
    , statLocationFallbacks = 0
    , statLocationMaxWalk = 0
    , statLegalizationMaxStack = 0
    , statSteinerPoints = 0
    , statRefinementFaceChecks = 0
    , statRefinementQueuePops = 0
    , statSweepFastPoints = 0
    , statSweepSkippedPoints = 0
    }

-- | Whether an insertion published a new site or selected an existing one.
data InsertionDisposition = Inserted | AlreadyPresent
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Reason a floating-point coordinate cannot enter the exact-predicate domain.
data CoordinateError
  = CoordinateNaN
  | CoordinateInfinite
  | CoordinateTooSmall
  | CoordinateTooLarge
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Classification retained when a numeric parameter is not finite.
data NonFiniteValue
  = ValueNaN
  | ValuePositiveInfinity
  | ValueNegativeInfinity
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Classify NaN and signed infinity, leaving finite values unclassified.
classifyNonFinite :: Double -> Maybe NonFiniteValue
classifyNonFinite value
  | isNaN value = Just ValueNaN
  | isInfinite value && value < 0 = Just ValueNegativeInfinity
  | isInfinite value = Just ValuePositiveInfinity
  | otherwise = Nothing

-- | Total construction and rewrite obstruction surface.
data BuildError
  = InvalidCoordinate !(Maybe Int) {-# UNPACK #-} !Double !CoordinateError
  | PointLocationFailed !(Point)
  | LocationWalkExhausted !(Point) {-# UNPACK #-} !Int
  | RefinementInputTopologyInvalid !InvariantViolation
  | FreshInsertionMatchedExistingVertex !VertexId !VertexId
  | DegenerateLineEndpointMissingOutgoing !VertexId
  | DegenerateLineEndpointTurnMissing {-# UNPACK #-} !Int
  | DegenerateLineConnectedVertexMissing {-# UNPACK #-} !Int
  | HullStartNotVisible !DirectedEdgeId
  | OuterRangeDidNotTerminate !DirectedEdgeId !DirectedEdgeId {-# UNPACK #-} !Int
  | OuterRangeContainsInnerEdge !DirectedEdgeId !FaceId
  | ConstrainedEdgeFlipRefused !UndirectedEdgeId
  | RemovalVertexOutOfRange !VertexId {-# UNPACK #-} !Int
  | RemovalEdgeOutOfRange !UndirectedEdgeId {-# UNPACK #-} !Int
  | RemovalFaceOutOfRange !FaceId {-# UNPACK #-} !Int
  | RemovalFaceCycleDidNotTerminate
      !FaceId
      !DirectedEdgeId
      {-# UNPACK #-} !Int
  | RemovalEmptyTriangulation !VertexId
  | RemovalTwoPointDegreeMismatch !VertexId {-# UNPACK #-} !Int
  | RemovalCollinearDegreeMismatch !VertexId {-# UNPACK #-} !Int
  | RemovalBorderTooShort {-# UNPACK #-} !Int
  | RemovalBorderArityMismatch {-# UNPACK #-} !Int
  | RemovalOutgoingCycleDidNotTerminate
      !VertexId
      !DirectedEdgeId
      {-# UNPACK #-} !Int
  | CircleSweepRequiresDenseStorage
  | CircleSweepHullEmpty
  | OuterCycleDidNotTerminate
      !DirectedEdgeId
      !DirectedEdgeId
      {-# UNPACK #-} !Int
  | HierarchyLevelPopulationMismatch
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Int
  | HierarchyInsertionHandleMismatch !VertexId !VertexId
  | PointIndexCapacityExhausted {-# UNPACK #-} !Int
  | RefinementMinimumAngleNotFinite !NonFiniteValue
  | RefinementMinimumAngleOutOfRange {-# UNPACK #-} !Double
  | RefinementMinimumAngleDerivedRatioNotFinite !NonFiniteValue
  | RefinementMaximumAdditionalVerticesNegative {-# UNPACK #-} !Int
  | RefinementMinimumAreaNotFinite !NonFiniteValue
  | RefinementMinimumAreaNegative {-# UNPACK #-} !Double
  | RefinementMaximumAreaNotFinite !NonFiniteValue
  | RefinementMaximumAreaNotPositive {-# UNPACK #-} !Double
  | RefinementMaximumRadiusEdgeRatioNotFinite !NonFiniteValue
  | RefinementMaximumRadiusEdgeRatioNotPositive {-# UNPACK #-} !Double
  | RefinementMinimumAreaExceedsMaximum
      {-# UNPACK #-} !Double
      {-# UNPACK #-} !Double
  | RefinementSeedFaceNotActive !FaceId {-# UNPACK #-} !Int
  | RefinementDomainInterfaceEdgeNotActive !UndirectedEdgeId {-# UNPACK #-} !Int
  | RefinementDomainInterfaceMissing !UndirectedEdgeId
  | RefinementDomainInterfaceExtraneous !UndirectedEdgeId
  | RefinementDomainTopologyChanged
  | RefinementDomainRequiresConvexHullPreservation
  | RefinementDomainRequiresConstraintPreservation
  | RefinementDomainForbidsOuterFaceExclusion
  | RefinementDomainWouldCrossInterface !UndirectedEdgeId !FaceId
  | RefinementDomainWouldRewriteProtectedFace !FaceId
  | RefinementDomainProtectedFaceChanged !FaceId
  | CapacityExceeded {-# UNPACK #-} !Int
  | HalfEdgeCapacityExceeded {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  | FaceCapacityExceeded {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  | PayloadStorageFailure !BoxedStorageError
  | CoordinatePayloadCountMismatch
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Int
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Exact position of a query relative to the finite triangulation.
data Location
  = EmptyTriangulation
  | OnVertex !VertexId
  | OnEdge !DirectedEdgeId
  | InFace !FaceId
  | OutsideConvexHull !(Maybe DirectedEdgeId)
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Optional starting cell for point-location descent.
data LocationHint
  = VertexHint !VertexId
  | FaceHint !FaceId
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Work performed by point-location descent.
data LocationStats = LocationStats
  { locationWalkSteps :: {-# UNPACK #-} !Int
  , locationUsedFallback :: !Bool
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Location telemetry for a query that required no descent.
emptyLocationStats :: LocationStats
emptyLocationStats = LocationStats 0 False

-- | Work performed by a nearest-neighbor query.
data NearestStats = NearestStats
  { nearestWalkSteps :: {-# UNPACK #-} !Int
  , nearestDistanceTests :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Independent quality bounds and a finite Steiner-vertex budget.
data RefinementParameters = RefinementParameters
  { refineMaxAdditionalVertices :: !(Maybe Int)
  , refineMinArea :: !(Maybe Double)
  , refineMaxArea :: !(Maybe Double)
  , refineMaxRadiusEdgeRatio :: !(Maybe Double)
  , refinePreserveConvexHull :: !Bool
  , refineKeepConstraintEdges :: !Bool
  , refineExcludeOuterFaces :: !Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Conservative refinement defaults with no explicit area bounds.
defaultRefinementParameters :: RefinementParameters
defaultRefinementParameters =
  RefinementParameters
    { refineMaxAdditionalVertices = Nothing
    , refineMinArea = Nothing
    , refineMaxArea = Nothing
    , refineMaxRadiusEdgeRatio = Just 1
    , refinePreserveConvexHull = True
    , refineKeepConstraintEdges = False
    , refineExcludeOuterFaces = False
    }

-- | A concrete witness that an immutable DCEL law does not hold.
data InvariantViolation
  = CoordinatePlaneLengthMismatch {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  | VertexOutgoingLengthMismatch {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  | VertexPayloadLengthMismatch {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  | TopologyArenaLengthMismatch {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  | DirectedPayloadLengthMismatch {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  | UndirectedPayloadLengthMismatch {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  | DirectedEdgeCountOdd {-# UNPACK #-} !Int
  | ConstraintLengthMismatch {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  | NonCanonicalConstraintFlag !UndirectedEdgeId !Word8
  | CachedConstraintCountMismatch {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  | CachedConstraintIndexMismatch
  | MissingOuterFace
  | FacePayloadLengthMismatch {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  | EdgeOriginOutOfRange !DirectedEdgeId !VertexId {-# UNPACK #-} !Int
  | EdgeNextOutOfRange !DirectedEdgeId !DirectedEdgeId {-# UNPACK #-} !Int
  | EdgePreviousOutOfRange !DirectedEdgeId !DirectedEdgeId {-# UNPACK #-} !Int
  | EdgeFaceOutOfRange !DirectedEdgeId !FaceId {-# UNPACK #-} !Int
  | VertexOutgoingOutOfRange !VertexId !DirectedEdgeId {-# UNPACK #-} !Int
  | FaceAdjacentOutOfRange !FaceId !DirectedEdgeId {-# UNPACK #-} !Int
  | EdgeNextPreviousMismatch !DirectedEdgeId !DirectedEdgeId
  | EdgePreviousNextMismatch !DirectedEdgeId !DirectedEdgeId
  | EdgeDoubleReversalMismatch !DirectedEdgeId
  | EdgeSelfLinkedNext !DirectedEdgeId
  | EdgeSelfLinkedPrevious !DirectedEdgeId
  | InnerFaceNotTriangularAtEdge !DirectedEdgeId
  | FaceMissingAdjacentEdge !FaceId
  | FaceRepresentativeMismatch !FaceId !DirectedEdgeId !FaceId
  | InnerFaceVertexCardinalityMismatch !FaceId {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  | ConnectedVertexMissingOutgoing !VertexId
  | VertexOutgoingOriginMismatch !VertexId !DirectedEdgeId !VertexId
  | CollinearEdgeCountMismatch {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  | EulerCharacteristicMismatch {-# UNPACK #-} !Int
  | InnerFaceNotCounterClockwise !FaceId
  | LocallyIllegalDelaunayEdge !UndirectedEdgeId
  | DelaunayIncidentFaceNotTriangular !UndirectedEdgeId
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

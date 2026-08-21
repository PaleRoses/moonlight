{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | The shared vocabulary of the constrained layer: its refusals, its published
-- results, and the transaction-local state its interpreters carry.
module Moonlight.Triangulation.Internal.Cdt.Types
  ( CdtError (..)
  , CanonicalSegment (..)
  , ConstraintConflict (..)
  , ConstrainedUnionError (..)
  , ConstrainedSeamSource (..)
  , ConstrainedSeamFaceEvidence (..)
  , ConstrainedSeamConstraintEvidence (..)
  , ConstrainedSeamResult (..)
  , CorridorObstruction (..)
  , ConstraintRecoveryResult (..)
  , ConstraintResult
  , ConstraintOutcome (..)
  , ConstraintBatchStats (..)
  , ConstraintBatchResult (..)
  , ConstrainedExtensionResult (..)
  , ConstraintSplitBatchResult
  , ConstraintBatchAccumulator (..)
  , ConstraintRequestAccumulator (..)
  , ConstraintWorkspace (..)
  , MutableConstraintProgram (..)
  , MutableConstraintOutcome (..)
  , MutableProgramCursor (..)
  , MutablePlanWalk (..)
  , MutableConstraintScan (..)
  , ConstraintProgramAccumulator (..)
  , CdtBuildResult (..)
  ) where

import Control.DeepSeq (NFData)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Vector as V
import Moonlight.Triangulation.Handles.HandleDefs
  ( DirectedEdgeId
  , FaceId
  , UndirectedEdgeId
  , VertexId
  )
import Moonlight.Triangulation.Internal.Growable (GrowableWord32)
import Moonlight.Triangulation.Internal.Representation (BuildResult, Triangulation)
import Moonlight.Triangulation.Internal.Types
import Moonlight.Triangulation.IntersectionIterator (Intersection)
import GHC.Generics (Generic)

-- | Typed refusal surface for constrained construction and corridor recovery.
data CdtError
  = CdtBuildError !BuildError
  | InvalidConstraintVertex !VertexId
  | ConstraintIntersection !UndirectedEdgeId
  | ConstraintInputConflicts !(V.Vector (Int, Int))
  | ConstraintCorridorObstructed !CorridorObstruction
  | ConstraintBatchCardinalityMismatch
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Int
  | ConstraintEndpointIndexOutOfRange
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Int
  | ConstraintSplitBudgetExhausted
      !VertexId
      !VertexId
      {-# UNPACK #-} !Int
  | ConstraintSplitIntersectionIndeterminate
      !DirectedEdgeId
  | ConstraintEdgeIndexOutOfRange
      !UndirectedEdgeId
      {-# UNPACK #-} !Int
  | ConstraintRecoverySafetyBudgetExhausted
      !VertexId
      !VertexId
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Int
  | ConstraintRecoveryStripExhausted
      !VertexId
      !VertexId
      {-# UNPACK #-} !Int
  | ConstraintRecoveryStripUnflippable
      !VertexId
      !VertexId
      !DirectedEdgeId
      {-# UNPACK #-} !Int
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Geometric identity of one constraint segment. The constructor is private:
-- endpoints are always canonical points in ascending order, so direction and
-- edge numbering cannot leak into union witnesses.
data CanonicalSegment = CanonicalSegment
  { -- | Lesser endpoint under the point ordering.
    segmentStart :: !(Point)
  , -- | Greater endpoint under the point ordering.
    segmentEnd :: !(Point)
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | A pair of properly crossing constraints, ordered independently of operand
-- and traversal order.
data ConstraintConflict = ConstraintConflict
  { -- | Lesser segment under the canonical segment ordering.
    conflictFirstSegment :: !(CanonicalSegment)
  , -- | Greater segment under the canonical segment ordering.
    conflictSecondSegment :: !(CanonicalSegment)
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Complete obstruction surface for atomic constrained union.
data ConstrainedUnionError
  = ConstraintUnionConflicts !(NonEmpty (ConstraintConflict))
  | ConstraintUnionConstructionFailed !(CdtError)
  | ConstraintUnionSiteMissing !(Point)
  | ConstraintUnionNotSeparated
  | ConstraintUnionSourceFaceNotTriangular
      !ConstrainedSeamSource
      !FaceId
      {-# UNPACK #-} !Int
  | ConstraintUnionTargetFaceNotTriangular
      !FaceId
      {-# UNPACK #-} !Int
  | ConstraintUnionTargetFaceAmbiguous !FaceId !FaceId
  | ConstraintUnionSourceFaceNotPreserved
      !ConstrainedSeamSource
      !FaceId
  | ConstraintUnionSourceConstraintNotPreserved
      !ConstrainedSeamSource
      !(CanonicalSegment)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Operand identity in a separated constrained seam. This belongs in the
-- obstruction and receipt vocabulary rather than being encoded as a Boolean;
-- callers must handle both source sections explicitly.
data ConstrainedSeamSource
  = ConstrainedSeamLeftSource
  | ConstrainedSeamRightSource
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Exact proof that one active source face survives as one target face. The
-- three points are stored in ascending order, so the witness is independent
-- of local face rotation and handle numbering.
data ConstrainedSeamFaceEvidence = ConstrainedSeamFaceEvidence
  { constrainedSeamSourceFace :: !FaceId
    -- ^ Face handle in the source operand.
  , constrainedSeamTargetFace :: !FaceId
    -- ^ Corresponding face handle in the published result.
  , constrainedSeamFaceFirstPoint :: !(Point)
    -- ^ Least vertex position under canonical point order.
  , constrainedSeamFaceSecondPoint :: !(Point)
    -- ^ Middle vertex position under canonical point order.
  , constrainedSeamFaceThirdPoint :: !(Point)
    -- ^ Greatest vertex position under canonical point order.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Exact transport of one source constraint segment. 'Nothing' means its
-- copied constraint edge already represented the segment after zippering;
-- 'Just' records the corridor outcome when recovery was required.
data ConstrainedSeamConstraintEvidence = ConstrainedSeamConstraintEvidence
  { constrainedSeamConstraintSegment :: !(CanonicalSegment)
    -- ^ Canonical source segment preserved by the publication.
  , constrainedSeamConstraintRecovery :: !(Maybe ConstraintOutcome)
    -- ^ Recovery work when copying alone did not preserve the segment.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Atomic publication of a source-preserving constrained seam and the proof
-- needed to transport solved face-indexed data. The receipt is derived only
-- after canonical publication and exact source restriction checks succeed.
data ConstrainedSeamResult vertex = ConstrainedSeamResult
  { constrainedSeamResultTriangulation
      :: !(Triangulation 'Constrained vertex () () ())
    -- ^ Published constrained union.
  , constrainedSeamLeftFaceEvidence
      :: !(V.Vector (ConstrainedSeamFaceEvidence))
    -- ^ Transport evidence for every active left-source face.
  , constrainedSeamRightFaceEvidence
      :: !(V.Vector (ConstrainedSeamFaceEvidence))
    -- ^ Transport evidence for every active right-source face.
  , constrainedSeamNewFaces :: !(V.Vector FaceId)
    -- ^ Faces created by the seam rather than transported from an operand.
  , constrainedSeamLeftConstraintEvidence
      :: !(V.Vector (ConstrainedSeamConstraintEvidence))
    -- ^ Preservation evidence for left-source constraints.
  , constrainedSeamRightConstraintEvidence
      :: !(V.Vector (ConstrainedSeamConstraintEvidence))
    -- ^ Preservation evidence for right-source constraints.
  , constrainedSeamConstraintStats :: !ConstraintBatchStats
    -- ^ Constraint recovery work performed by the seam.
  , constrainedSeamBuildStats :: !BuildStats
    -- ^ Topology work performed by the seam.
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

deriving stock instance Eq vertex => Eq (ConstrainedSeamResult vertex)
deriving stock instance Show vertex => Show (ConstrainedSeamResult vertex)

-- | Structural witness that corridor recovery could not complete.
data CorridorObstruction
  = CorridorWalkDidNotTerminate {-# UNPACK #-} !Int
  | CorridorBoundaryMissing !VertexId !VertexId
  | CorridorProgramMalformed {-# UNPACK #-} !Int
  | CorridorTargetMissing !VertexId !VertexId
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | One constrained publication and its path receipt. The path carrier names
-- whether the caller requested one recovered path or an ordered family; the
-- mesh and edge count have one representation in either case.
data ConstraintRecoveryResult pathReceipt vertex directed undirected face = ConstraintRecoveryResult
  { constraintRecoveryTriangulation :: !(Triangulation 'Constrained vertex directed undirected face)
    -- ^ Published constrained triangulation.
  , constraintRecoveryPathReceipt :: !pathReceipt
    -- ^ Recovered path evidence at the operation's requested multiplicity.
  , constraintRecoveryAddedEdges :: {-# UNPACK #-} !Int
    -- ^ Edges created while recovering the path evidence.
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

deriving stock instance
  (Eq pathReceipt, Eq vertex, Eq directed, Eq undirected, Eq face)
  => Eq (ConstraintRecoveryResult pathReceipt vertex directed undirected face)
deriving stock instance
  (Show pathReceipt, Show vertex, Show directed, Show undirected, Show face)
  => Show (ConstraintRecoveryResult pathReceipt vertex directed undirected face)

-- | Atomic result of admitting one constraint segment.
type ConstraintResult vertex directed undirected face =
  ConstraintRecoveryResult (V.Vector DirectedEdgeId) vertex directed undirected face

-- | Per-request outcome in a constraint batch.
data ConstraintOutcome
  = ConstraintAccepted
      !(V.Vector DirectedEdgeId)
      {-# UNPACK #-} !Int
  | ConstraintRejected
      !UndirectedEdgeId
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Aggregate work and disposition counts for a constraint batch.
data ConstraintBatchStats = ConstraintBatchStats
  { constraintBatchRequests :: {-# UNPACK #-} !Int
    -- ^ Requests interpreted.
  , constraintBatchAccepted :: {-# UNPACK #-} !Int
    -- ^ Requests admitted.
  , constraintBatchRejected :: {-# UNPACK #-} !Int
    -- ^ Requests refused by an existing constraint.
  , constraintBatchCorridors :: {-# UNPACK #-} !Int
    -- ^ Recovery corridors opened.
  , constraintBatchReusedFaces :: {-# UNPACK #-} !Int
    -- ^ Existing faces retained while recovering corridors.
  , constraintBatchCrossedEdges :: {-# UNPACK #-} !Int
    -- ^ Edges crossed while tracing corridors.
  }
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (NFData)

-- | Atomic publication of one constraint batch and its receipts.
data ConstraintBatchResult vertex directed undirected face = ConstraintBatchResult
  { constraintBatchTriangulation :: !(Triangulation 'Constrained vertex directed undirected face)
    -- ^ Published constrained triangulation.
  , constraintBatchOutcomes :: !(V.Vector ConstraintOutcome)
    -- ^ Outcomes in request order.
  , constraintBatchStats :: !ConstraintBatchStats
    -- ^ Aggregate work performed by the batch.
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

deriving stock instance
  (Eq vertex, Eq directed, Eq undirected, Eq face)
  => Eq (ConstraintBatchResult vertex directed undirected face)
deriving stock instance
  (Show vertex, Show directed, Show undirected, Show face)
  => Show (ConstraintBatchResult vertex directed undirected face)

-- | Receipt of one asymmetric constrained extension. The triangulation and
-- all telemetry arise from the same sealed transaction: base sites were
-- resident, incoming sites were inserted, and only incoming constraints were
-- interpreted. A later refinement is deliberately a separate operation over
-- this immutable result, not a hidden continuation of this transaction.
data ConstrainedExtensionResult vertex directed undirected face = ConstrainedExtensionResult
  { constrainedExtensionConstraintBatch :: {-# UNPACK #-} !(ConstraintBatchResult vertex directed undirected face)
    -- ^ Published extension and incoming-constraint receipts.
  , constrainedExtensionBuildStats :: !BuildStats
    -- ^ Topology work performed by the extension.
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

deriving stock instance
  (Eq vertex, Eq directed, Eq undirected, Eq face)
  => Eq (ConstrainedExtensionResult vertex directed undirected face)
deriving stock instance
  (Show vertex, Show directed, Show undirected, Show face)
  => Show (ConstrainedExtensionResult vertex directed undirected face)

-- | One published mesh carrying every requested division. The paths are
-- per-request receipts in traversal order; a later request may have rewritten
-- topology an earlier path names, which is the same as-traversed reading the
-- singleton path already carries.
type ConstraintSplitBatchResult vertex directed undirected face =
  ConstraintRecoveryResult (V.Vector (V.Vector DirectedEdgeId)) vertex directed undirected face

data ConstraintBatchAccumulator = ConstraintBatchAccumulator
  { accumulatedConstraintOutcomes :: ![ConstraintOutcome]
  , accumulatedConstraintStats :: !ConstraintBatchStats
  }

data ConstraintRequestAccumulator = ConstraintRequestAccumulator
  { accumulatedRequestPath :: ![DirectedEdgeId]
  , accumulatedRequestAddedEdges :: {-# UNPACK #-} !Int
  , accumulatedRequestCorridors :: {-# UNPACK #-} !Int
  , accumulatedRequestReusedFaces :: {-# UNPACK #-} !Int
  , accumulatedRequestCrossedEdges :: {-# UNPACK #-} !Int
  }

data ConstraintWorkspace s = ConstraintWorkspace
  { constraintProgramWords :: !(GrowableWord32 s)
  , constraintWalkBudget :: {-# UNPACK #-} !Int
  }

data MutableConstraintProgram = MutableConstraintProgram
  { mutableProgramWordCount :: {-# UNPACK #-} !Int
  , mutableProgramPieceCount :: {-# UNPACK #-} !Int
  }

-- | What one request did to the thawed mesh. A rejection is a value: the batch
-- interpreter records it and carries on, the singleton verbs abandon the
-- transaction on it, and both readings are lawful because an accepted request
-- may lawfully obstruct a later one.
data MutableConstraintOutcome
  = MutableConstraintRejected !UndirectedEdgeId
  | MutableConstraintAccepted !ConstraintRequestAccumulator

data MutableProgramCursor = MutableProgramCursor
  { mutableCursorAt :: !VertexId
  , mutableCursorHeader :: {-# UNPACK #-} !Int
  , mutableCursorWriteAt :: {-# UNPACK #-} !Int
  , mutableCursorConflictCount :: {-# UNPACK #-} !Int
  , mutableCursorPieceCount :: {-# UNPACK #-} !Int
  , mutableCursorAfterOverlap :: !Bool
  }

data MutablePlanWalk
  = MutablePlanActive !Intersection !MutableProgramCursor
  | MutablePlanComplete !MutableConstraintProgram
  | MutablePlanBlocked !DirectedEdgeId
  | MutablePlanFailed !CorridorObstruction

data MutableConstraintScan
  = MutableConstraintScanBlocked !DirectedEdgeId
  | MutableConstraintScanAdmitted !MutableConstraintProgram

data ConstraintProgramAccumulator = ConstraintProgramAccumulator
  { accumulatedProgramCursor :: {-# UNPACK #-} !Int
  , accumulatedProgramRequest :: !ConstraintRequestAccumulator
  }

-- | Result of maximal constrained construction, including rejected requests.
data CdtBuildResult vertex directed undirected face = CdtBuildResult
  { cdtAcceptedBuild :: {-# UNPACK #-} !(BuildResult 'Constrained vertex directed undirected face)
    -- ^ Canonical build result containing every admitted constraint.
  , cdtRejectedConstraints :: !(V.Vector (Int, Int))
    -- ^ Input-index pairs that could not be admitted.
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

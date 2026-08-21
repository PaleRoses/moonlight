-- | The public surface for Delaunay triangulations as values of finite
-- coordinate supports. Its exports are grouped by object, generation,
-- annotations, finite-set algebra, constraints, refinement, observations and
-- validation.
--
-- Geometry-only set laws are observed after erasing vertex annotations with
-- @mapVertices (const ())@ and applying 'canonicalize'. Successful construction
-- may retain schedule-dependent resident numbering, so structural 'Eq' compares
-- stored representations rather than silently normalizing them. Annotations may
-- change independently through 'mapVertices' and 'setVertexData'; they never
-- determine geometry.
--
-- The algebra is deliberately partial. Finite arena exhaustion is returned as
-- 'BuildError', while unrealizable constrained compositions return
-- 'ConstrainedUnionError'. These typed obstructions are why the surface exposes
-- explicit operations rather than total 'Semigroup' instances. Refinement then
-- composes over any admitted result without introducing a second mesh type.
module Moonlight.Triangulation
  ( -- * The object — a triangulation is a value of its site set
    Triangulation
  , DelaunayTriangulation
  , ConstrainedDelaunayTriangulation
  , ConstraintMode (..)
  , Point (..)
  , QueryPoint
  , queryPointValue
  , PointValidationError (..)
  , mkQueryPoint
  , HasPosition (..)
  , ElementDefaults (..)
  , unitElementDefaults
  , JoinSemilattice (..)
    -- | An identifier is admitted by the triangulation that issued it. The
    -- observations below index without a second bounds check, so the
    -- constructors are withheld here and the projections are not: an
    -- identifier can be read, compared and carried, and can only be obtained
    -- from a mesh. "Moonlight.Triangulation.Handles.HandleDefs" exports the
    -- constructors for a caller who is willing to own that obligation.
  , VertexId
  , unVertexId
  , FaceId
  , unFaceId
  , DirectedEdgeId
  , unDirectedEdgeId
  , UndirectedEdgeId
  , unUndirectedEdgeId
  , reverseEdge
  , asUndirected
  , normalizedDirected
  , reversedDirected
  , directedPair
  , isNormalized

    -- * Generation — @delaunay@; canonical observation factors through the site set
  , delaunay
  , delaunayGeometry
  , DuplicatePayloadPolicy (..)
  , delaunayFromCoordinates
  , BuildResult
  , buildTriangulation
  , buildInputVertices
  , BuildError (..)
  , CoordinateError (..)
  , NonFiniteValue (..)

    -- * The annotation functor — payloads annotate geometry, never author it;
    -- the mesh is the same mesh before and after
  , mapVertices
  , vertexData
  , setVertexData

    -- * Finite-set algebra and its normal form — support order is observable,
    -- intersection can combine heterogeneous annotations, and each
    -- construction returns its obstruction rather than an instance that lies
  , canonicalize
  , SiteRelation (..)
  , siteRelation
  , union
  , unions
  , intersection
  , intersectionWith
  , difference
  , symmetricDifference

    -- * The constraint layer — 'constrainedDelaunay' is the partial map from
    -- (sites, segments), defined exactly on realizable segment sets and
    -- naming the corridor that blocked it where it is not
  , constrainedDelaunay
  , fromDelaunay
  , constraintEdges
  , isConstraintEdge
  , CdtError (..)
  , CorridorObstruction (..)
  , CanonicalSegment
  , segmentStart
  , segmentEnd
  , ConstraintConflict
  , conflictFirstSegment
  , conflictSecondSegment
  , ConstrainedUnionError (..)
  , ConstraintBatchResult
  , constraintBatchTriangulation
  , constraintBatchOutcomes
  , constraintBatchStats
  , ConstrainedExtensionResult
  , constrainedExtensionConstraintBatch
  , constrainedExtensionBuildStats
  , ConstrainedSeamSource (..)
  , ConstrainedSeamFaceEvidence
  , constrainedSeamSourceFace
  , constrainedSeamTargetFace
  , constrainedSeamFaceFirstPoint
  , constrainedSeamFaceSecondPoint
  , constrainedSeamFaceThirdPoint
  , ConstrainedSeamConstraintEvidence
  , constrainedSeamConstraintSegment
  , constrainedSeamConstraintRecovery
  , ConstrainedSeamResult
  , constrainedSeamResultTriangulation
  , constrainedSeamLeftFaceEvidence
  , constrainedSeamRightFaceEvidence
  , constrainedSeamNewFaces
  , constrainedSeamLeftConstraintEvidence
  , constrainedSeamRightConstraintEvidence
  , constrainedSeamConstraintStats
  , constrainedSeamBuildStats
  , constraintSegments
  , unionConstrainedWith
  , unionConstrained
  , joinSeparatedConstrained
  , extendConstrainedWith

    -- * Refinement — budget-bounded, composed after any operation above rather
    -- than configured into it; 'refinementComplete' reports sufficiency, not
    -- effort
  , refine
  , refineWithinDomain
  , validateRefinementParameters
  , RefinementParameters (..)
  , defaultRefinementParameters
  , RefinementReceipt
  , RefinementDomainResult
  , RefinementResult
  , refinementDomainResult
  , refinementDomainReceipt
  , refinedTriangulation
  , refinementVisitedJoinFaces
  , refinementVisitedProtectedFaces
  , refinementCreatedFaces
  , refinementTouchedEdges
  , refinementRemovedEdges
  , refinementInterfaceBoundaryReads
  , refinementAttemptedBoundaryCrossings
  , refinementAddedVertices
  , refinementExcludedFaces
  , refinementComplete
  , withMinimumAngle
  , radiusEdgeRatioForAngle

    -- * Observations — pure functions of the value: incidence, location,
    -- interpolation and barrier parity, none of which build a second mesh
  , numVertices
  , vertices
  , vertexPoint
  , vertexPoints
  , numFaces
  , innerFaces
  , outerFace
  , faceDirectedEdges
  , faceVertices
  , innerFaceDirectedEdgeTriples
  , innerFaceVertexTriples
  , vertexOutgoingEdges
  , numUndirectedEdges
  , undirectedEdges
  , undirectedEndpoints
  , origin
  , destination
  , incidentFace
  , isBoundaryEdge
  , nearestNeighbor
  , NearestStats (..)
  , interpolateNearest
  , interpolateBarycentric
  , LocationHint (..)
  , facesAtEvenBarrierDepth

    -- ** Face regions and alpha filtration
  , FaceComponent
  , faceComponentFaces
  , BoundaryOrientation (..)
  , BoundaryLoop
  , boundaryLoopOrientation
  , boundaryLoopVertices
  , RegionBoundary
  , regionBoundaryOuterLoop
  , regionBoundaryHoleLoops
  , BoundaryObstruction (..)
  , faceComponents
  , componentBoundaryLoops
  , componentBoundary
  , RadiusSquared
  , RadiusSquaredError (..)
  , mkRadiusSquared
  , alphaShapeContainsFace

    -- * Exact planar regions — authoritative rational geometry, labelled
    -- overlay, closed cell selections, valuations, and polygonal morphology
  , ExactRational
  , ExactArithmeticError (..)
  , exactRational
  , exactRationalNumerator
  , exactRationalDenominator
  , ExactPoint
  , exactPoint
  , exactPointCoordinates
  , ExactSegment
  , ExactGeometryError (..)
  , exactSegment
  , exactSegmentEndpoints
  , exactPointFromPoint
  , exactPointFromQueryPoint
  , exactPointToEmbeddingCandidate
  , exactOrient2d
  , exactOnClosedSegment
  , SegmentRelation (..)
  , exactSegmentRelation
  , ExactIntersectionError (..)
  , exactLineIntersection
  , ExactLoop
  , exactLoop
  , exactLoopPoints
  , PolygonComponent
  , polygonComponent
  , polygonOuterLoop
  , polygonHoleLoops
  , PlanarRegion
  , planarRegion
  , planarRegionComponents
  , emptyPlanarRegion
  , RegionPointLocation (..)
  , regionPointLocation
  , PlanarLayer
  , planarLayerOutsideLabel
  , planarLayerRegions
  , planarLayer
  , planarLayerLabelAt
  , RegionValidationError (..)
  , RegionPublicationError (..)
  , labelledPlanarLayer
  , ExactCellSet
  , CellSelectionError (..)
  , exactCellSet
  , closeFaceCellSet
  , exactCellSetVertexCount
  , exactCellSetEdgeCount
  , exactCellSetFaceCount
  , foldExactCellVertices
  , foldExactCellEdges
  , foldExactCellFaces
  , OverlayResult
  , OverlayReceipt (..)
  , OverlayError (..)
  , OverlaySelectionKind (..)
  , OverlaySelectionError (..)
  , overlayLayers
  , overlayEmbeddedTriangulation
  , overlayReceipt
  , overlayCells
  , overlayArrangementVertices
  , overlayArrangementEdges
  , overlayPlanarLayer
  , overlaySelectedRegion
  , overlayClosedUnion
  , overlayClosedIntersection
  , overlayRegularizedDifference
  , EulerCharacteristic
  , eulerCharacteristicValue
  , ExactArea
  , exactAreaValue
  , ExactLengthTerm
  , lengthCoefficient
  , squaredLength
  , ExactLengthExpression
  , exactLengthTerms
  , CertifiedInterval (..)
  , ExactLengthMeasurement
  , exactLengthExpression
  , exactLengthBounds
  , PlanarValuations
  , valuationEuler
  , valuationArea
  , valuationIntrinsic1
  , ValuationError (..)
  , cellValuations
  , regionValuations
  , planarValuationsPerimeter
  , cellSetPerimeter
  , regionPerimeter
  , ConvexPolygon
  , convexPolygon
  , convexPolygonPoints
  , StructuringElement
  , structuringElement
  , MinkowskiOperation (..)
  , MinkowskiError (..)
  , MinkowskiReceipt (..)
  , convexMinkowskiSum
  , minkowskiSum
  , erodeBy
  , openWith
  , closeWith
  , polygonOffset
  , polygonInset

    -- * Discharge — the invariants the constructors guarantee, checkable on a
    -- value built by any route; every violation is a value carrying its witness
  , validateTriangulation
  , InvariantViolation (..)
  ) where

import Moonlight.Triangulation.BulkLoad
  ( DuplicatePayloadPolicy (..)
  , delaunay
  , delaunayFromCoordinates
  , delaunayGeometry
  )
import Moonlight.Triangulation.CellSet
  ( CellSelectionError (..)
  , ExactCellSet
  , closeFaceCellSet
  , exactCellSet
  , exactCellSetEdgeCount
  , exactCellSetFaceCount
  , exactCellSetVertexCount
  , foldExactCellEdges
  , foldExactCellFaces
  , foldExactCellVertices
  )
import Moonlight.Triangulation.Dcel
  ( destination
  , faceDirectedEdges
  , faceVertices
  , innerFaceDirectedEdgeTriples
  , innerFaceVertexTriples
  , incidentFace
  , isBoundaryEdge
  , isConstraintEdge
  , numFaces
  , numUndirectedEdges
  , numVertices
  , origin
  , outerFace
  , setVertexData
  , undirectedEndpoints
  , vertexData
  , vertexOutgoingEdges
  , vertexPoint
  , vertexPoints
  )
import Moonlight.Triangulation.FloodFillIterator
  ( BoundaryLoop
  , BoundaryObstruction (..)
  , BoundaryOrientation (..)
  , FaceComponent
  , RadiusSquared
  , RegionBoundary
  , RadiusSquaredError (..)
  , alphaShapeContainsFace
  , boundaryLoopOrientation
  , boundaryLoopVertices
  , componentBoundary
  , componentBoundaryLoops
  , faceComponentFaces
  , faceComponents
  , facesAtEvenBarrierDepth
  , mkRadiusSquared
  , regionBoundaryHoleLoops
  , regionBoundaryOuterLoop
  )
import Moonlight.Triangulation.Exact
  ( ExactGeometryError (..)
  , ExactIntersectionError (..)
  , ExactPoint
  , ExactSegment
  , SegmentRelation (..)
  , exactLineIntersection
  , exactOnClosedSegment
  , exactOrient2d
  , exactPoint
  , exactPointCoordinates
  , exactPointFromPoint
  , exactPointFromQueryPoint
  , exactPointToEmbeddingCandidate
  , exactSegment
  , exactSegmentEndpoints
  , exactSegmentRelation
  )
import Moonlight.Triangulation.Handles.HandleDefs
  ( DirectedEdgeId (..)
  , FaceId (..)
  , UndirectedEdgeId (..)
  , VertexId (..)
  , asUndirected
  , directedPair
  , isNormalized
  , normalizedDirected
  , reverseEdge
  , reversedDirected
  )
import Moonlight.Triangulation.Handles.Iterators.FixedIterators
  ( innerFaces
  , undirectedEdges
  , vertices
  )
import Moonlight.Triangulation.Internal.Canonical (canonicalize)
import Moonlight.Triangulation.Internal.Cdt.Build
  ( constrainedDelaunay
  , fromDelaunay
  )
import Moonlight.Triangulation.Internal.Cdt.Query (constraintEdges)
import Moonlight.Triangulation.Internal.Cdt.Types
  ( CanonicalSegment
  , CdtError (..)
  , ConstrainedUnionError (..)
  , ConstrainedExtensionResult
  , ConstrainedSeamConstraintEvidence
  , ConstrainedSeamFaceEvidence
  , ConstrainedSeamResult
  , ConstrainedSeamSource (..)
  , ConstraintBatchResult
  , ConstraintConflict
  , CorridorObstruction (..)
  , constrainedExtensionBuildStats
  , constrainedExtensionConstraintBatch
  , constraintBatchOutcomes
  , constraintBatchStats
  , constraintBatchTriangulation
  , constrainedSeamBuildStats
  , constrainedSeamConstraintRecovery
  , constrainedSeamConstraintSegment
  , constrainedSeamConstraintStats
  , constrainedSeamFaceFirstPoint
  , constrainedSeamFaceSecondPoint
  , constrainedSeamFaceThirdPoint
  , constrainedSeamLeftConstraintEvidence
  , constrainedSeamLeftFaceEvidence
  , constrainedSeamNewFaces
  , constrainedSeamResultTriangulation
  , constrainedSeamRightConstraintEvidence
  , constrainedSeamRightFaceEvidence
  , constrainedSeamSourceFace
  , constrainedSeamTargetFace
  , conflictFirstSegment
  , conflictSecondSegment
  , segmentEnd
  , segmentStart
  )
import Moonlight.Triangulation.Internal.Cdt.Union
  ( constraintSegments
  , extendConstrainedWith
  , joinSeparatedConstrained
  , unionConstrained
  , unionConstrainedWith
  )
import Moonlight.Triangulation.Internal.ExactRational
  ( ExactArithmeticError (..)
  , ExactRational
  , exactRational
  , exactRationalDenominator
  , exactRationalNumerator
  )
import Moonlight.Triangulation.Internal.Representation
  ( BuildResult
  , ConstrainedDelaunayTriangulation
  , DelaunayTriangulation
  , RefinementDomainResult
  , RefinementReceipt
  , RefinementResult
  , Triangulation
  , buildInputVertices
  , buildTriangulation
  , refinementDomainReceipt
  , refinementDomainResult
  , refinedTriangulation
  , refinementInterfaceBoundaryReads
  , refinementAttemptedBoundaryCrossings
  , refinementAddedVertices
  , refinementComplete
  , refinementCreatedFaces
  , refinementExcludedFaces
  , refinementRemovedEdges
  , refinementTouchedEdges
  , refinementVisitedJoinFaces
  , refinementVisitedProtectedFaces
  )
import Moonlight.Triangulation.Internal.Types
  ( BuildError (..)
  , ConstraintMode (..)
  , CoordinateError (..)
  , ElementDefaults (..)
  , HasPosition (..)
  , InvariantViolation (..)
  , LocationHint (..)
  , NearestStats (..)
  , NonFiniteValue (..)
  , Point (..)
  , PointValidationError (..)
  , QueryPoint
  , RefinementParameters (..)
  , SiteRelation (..)
  , defaultRefinementParameters
  , queryPointValue
  , unitElementDefaults
  )
import Moonlight.Triangulation.Interpolation
  ( interpolateBarycentric
  , interpolateNearest
  , nearestNeighbor
  )
import Moonlight.Triangulation.Minkowski
  ( ConvexPolygon
  , MinkowskiError (..)
  , MinkowskiOperation (..)
  , MinkowskiReceipt (..)
  , StructuringElement
  , closeWith
  , convexMinkowskiSum
  , convexPolygon
  , convexPolygonPoints
  , erodeBy
  , minkowskiSum
  , openWith
  , polygonInset
  , polygonOffset
  , structuringElement
  )
import Moonlight.Triangulation.Math (mkQueryPoint)
import Moonlight.Triangulation.Overlay
  ( OverlayError (..)
  , OverlayReceipt (..)
  , OverlayResult
  , OverlaySelectionError (..)
  , OverlaySelectionKind (..)
  , overlayArrangementEdges
  , overlayArrangementVertices
  , overlayCells
  , overlayClosedIntersection
  , overlayClosedUnion
  , overlayEmbeddedTriangulation
  , overlayLayers
  , overlayPlanarLayer
  , overlayReceipt
  , overlayRegularizedDifference
  , overlaySelectedRegion
  )
import Moonlight.Triangulation.Payload (mapVertices)
import Moonlight.Triangulation.JoinSemilattice (JoinSemilattice (..))
import Moonlight.Triangulation.Refinement
  ( radiusEdgeRatioForAngle
  , refine
  , refineWithinDomain
  , validateRefinementParameters
  , withMinimumAngle
  )
import Moonlight.Triangulation.Region
  ( ExactLoop
  , PlanarLayer
  , PlanarRegion
  , PolygonComponent
  , RegionPointLocation (..)
  , RegionPublicationError (..)
  , RegionValidationError (..)
  , emptyPlanarRegion
  , exactLoop
  , exactLoopPoints
  , labelledPlanarLayer
  , planarLayer
  , planarLayerLabelAt
  , planarLayerOutsideLabel
  , planarLayerRegions
  , planarRegion
  , planarRegionComponents
  , polygonComponent
  , polygonHoleLoops
  , polygonOuterLoop
  , regionPointLocation
  )
import Moonlight.Triangulation.SetAlgebra
  ( difference
  , intersection
  , intersectionWith
  , siteRelation
  , symmetricDifference
  , union
  , unions
  )
import Moonlight.Triangulation.Validation (validateTriangulation)
import Moonlight.Triangulation.Valuation
  ( CertifiedInterval (..)
  , EulerCharacteristic
  , ExactArea
  , ExactLengthExpression
  , ExactLengthMeasurement
  , ExactLengthTerm
  , PlanarValuations
  , ValuationError (..)
  , cellSetPerimeter
  , cellValuations
  , eulerCharacteristicValue
  , exactAreaValue
  , exactLengthBounds
  , exactLengthExpression
  , exactLengthTerms
  , lengthCoefficient
  , planarValuationsPerimeter
  , regionPerimeter
  , regionValuations
  , squaredLength
  , valuationArea
  , valuationEuler
  , valuationIntrinsic1
  )

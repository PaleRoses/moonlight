{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Exact labelled common refinement. Source boundaries descend through one
-- exact segment-event plan, glue into canonical atomic constraints, and are
-- admitted only when their binary64 DCEL realization preserves every exact
-- relation.
module Moonlight.Triangulation.Overlay
  ( BoundaryLoopRef (..)
  , OverlayOperand
  , BoundaryRef
  , BoundaryVertexRef
  , BoundaryEdgeRef
  , boundaryRefComponent
  , boundaryRefLoop
  , boundaryRefLocalIndex
  , OverlayVertexOrigin
  , overlayOriginLeftVertices
  , overlayOriginRightVertices
  , overlayOriginLeftEdges
  , overlayOriginRightEdges
  , OverlayEdgeOrigin
  , overlayEdgeLeftSources
  , overlayEdgeRightSources
  , OverlaySupport
  , overlaySupportLabels
  , OverlayCellSupport (..)
  , OverlayCellId (..)
  , OverlayCellGeometry (..)
  , OverlayCell (..)
  , OverlayFace (..)
  , OverlayVertex (..)
  , OverlayEdge (..)
  , OverlayReceipt (..)
  , OverlayArrangementObstruction (..)
  , OverlayCellWitness (..)
  , OverlayError (..)
  , OverlayResult
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
  ) where

import Data.Bifunctor (first)
import Control.Monad (filterM)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import Moonlight.Triangulation.CellSet (ExactCellSet)
import qualified Moonlight.Triangulation.Dcel as Dcel
import Moonlight.Triangulation.Dcel (vertexData)
import Moonlight.Triangulation.Handles.HandleDefs
  ( UndirectedEdgeId
  , VertexId
  )
import Moonlight.Triangulation.Handles.Iterators.FixedIterators
  ( innerFaces
  , undirectedEdges
  , vertices
  )
import Moonlight.Triangulation.Internal.CellSet (closeExactCellSetWith)
import Moonlight.Triangulation.Internal.Overlay.Arrangement
  ( certifyArrangement
  )
import Moonlight.Triangulation.Internal.Overlay.Resident
  ( OverlayDiagonalSchedule (CanonicalOverlayDiagonals)
  , edgeSupport
  , faceCarriesExactArea
  , faceLabels
  , regionFaceLabels
  , residentOverlay
  , vertexSupport
  )
import Moonlight.Triangulation.Internal.Overlay.Types
import Moonlight.Triangulation.Internal.Representation (Triangulation)
import Moonlight.Triangulation.Internal.Types
  ( ConstraintMode (Constrained)
  )
import Moonlight.Triangulation.Region
  ( PlanarLayer
  , PlanarRegion
  , RegionPublicationError (..)
  , emptyPlanarRegion
  , planarLayerOutsideLabel
  , planarLayerRegions
  )
import Moonlight.Triangulation.Internal.Region.Publication
  ( labelledPlanarLayerFromExactCoordinates
  , planarLayerFromAdmittedComponents
  )

-- | Construct the exact common refinement and its one faithful resident DCEL.
overlayLayers
  :: (Ord leftLabel, Ord rightLabel)
  => PlanarLayer leftLabel
  -> PlanarLayer rightLabel
  -> Either
      (OverlayError leftLabel rightLabel)
      (OverlayResult leftLabel rightLabel)
overlayLayers leftLayer rightLayer = do
  certified <- certifyArrangement leftLayer rightLayer
  residentOverlay
    CanonicalOverlayDiagonals
    (planarLayerOutsideLabel leftLayer, planarLayerOutsideLabel rightLayer)
    certified

-- | The binary64 realization used by existing DCEL observations. Exact overlay
-- operations accept 'OverlayResult', never this projection.
overlayEmbeddedTriangulation
  :: OverlayResult leftLabel rightLabel
  -> Triangulation
      'Constrained
      OverlayVertex
      ()
      OverlayEdge
      OverlayFace
overlayEmbeddedTriangulation = overlayResultTriangulation

overlayReceipt :: OverlayResult leftLabel rightLabel -> OverlayReceipt
overlayReceipt = overlayResultReceipt

overlayCells
  :: OverlayResult leftLabel rightLabel
  -> [(OverlayCellId, OverlayCell leftLabel rightLabel)]
overlayCells result =
  V.toList
    (V.imap (\index cell -> (OverlayCellId index, cell)) (overlayResultCells result))

overlayArrangementVertices
  :: OverlayResult leftLabel rightLabel
  -> [(VertexId, OverlayVertex)]
overlayArrangementVertices result =
  let triangulation = overlayResultTriangulation result
   in [(vertex, vertexData triangulation vertex) | vertex <- vertices triangulation]

overlayArrangementEdges
  :: OverlayResult leftLabel rightLabel
  -> [(UndirectedEdgeId, OverlayEdgeOrigin)]
overlayArrangementEdges result =
  let triangulation = overlayResultTriangulation result
   in [ (edge, origin)
      | edge <- undirectedEdges triangulation
      , OverlayBoundary origin <- [Dcel.undirectedEdgeData triangulation edge]
      ]

-- | Publish the already-admitted bounded cell geometry. The resident DCEL is
-- a realization of these exact cells, not a second authoring source.
overlayPlanarLayer
  :: (Ord leftLabel, Ord rightLabel)
  => OverlayResult leftLabel rightLabel
  -> PlanarLayer (leftLabel, rightLabel)
overlayPlanarLayer result =
  planarLayerFromAdmittedComponents
    (overlayResultOutsideLabels result)
    [ ( (overlayCellLeft cell, overlayCellRight cell)
      , component
      )
    | cell <- V.toList (overlayResultCells result)
    , (overlayCellLeft cell, overlayCellRight cell)
        /= overlayResultOutsideLabels result
    , BoundedOverlayCell component <- [overlayCellGeometry cell]
    ]

-- | Publish the selected two-dimensional cells. Internal arrangement edges
-- between differently labelled but jointly selected cells dissolve because
-- selection precedes component descent.
overlaySelectedRegion
  :: ((leftLabel, rightLabel) -> Bool)
  -> OverlayResult leftLabel rightLabel
  -> Either RegionPublicationError PlanarRegion
overlaySelectedRegion selected result
  | selected (overlayResultOutsideLabels result) = Left RegionUnboundedSelection
  | otherwise = do
      published <-
        labelledPlanarLayerFromExactCoordinates
          False
          triangulation
          exactPointAt
          labelFace
      pure (Map.findWithDefault emptyPlanarRegion True (planarLayerRegions published))
 where
  triangulation = overlayResultTriangulation result
  exactPointAt vertex = Right (overlayExactPoint (vertexData triangulation vertex))
  labelFace face
    | faceCarriesExactArea result face =
        selected <$> regionFaceLabels result face
    | otherwise = Right False

overlayClosedUnion
  :: (Ord leftLabel, Ord rightLabel)
  => (leftLabel -> Bool)
  -> (rightLabel -> Bool)
  -> OverlayResult leftLabel rightLabel
  -> Either OverlaySelectionError ExactCellSet
overlayClosedUnion selectLeft selectRight =
  selectClosedCells
    ClosedUnionSelection
    (\support -> supportAny selectLeft (overlaySupportLeft support) || supportAny selectRight (overlaySupportRight support))
    (\leftLabel rightLabel -> selectLeft leftLabel || selectRight rightLabel)

overlayClosedIntersection
  :: (Ord leftLabel, Ord rightLabel)
  => (leftLabel -> Bool)
  -> (rightLabel -> Bool)
  -> OverlayResult leftLabel rightLabel
  -> Either OverlaySelectionError ExactCellSet
overlayClosedIntersection selectLeft selectRight =
  selectClosedCells
    ClosedIntersectionSelection
    (\support -> supportAny selectLeft (overlaySupportLeft support) && supportAny selectRight (overlaySupportRight support))
    (\leftLabel rightLabel -> selectLeft leftLabel && selectRight rightLabel)

overlayRegularizedDifference
  :: (leftLabel -> Bool)
  -> (rightLabel -> Bool)
  -> OverlayResult leftLabel rightLabel
  -> Either OverlaySelectionError ExactCellSet
overlayRegularizedDifference selectLeft selectRight result =
  let outsidePair = overlayResultOutsideLabels result
      selectFace leftLabel rightLabel = selectLeft leftLabel && not (selectRight rightLabel)
   in if uncurry selectFace outsidePair
        then Left (OverlaySelectionContainsUnboundedCell RegularizedDifferenceSelection)
        else closeSelectedCells [] [] selectFace result

selectClosedCells
  :: (Ord leftLabel, Ord rightLabel)
  => OverlaySelectionKind
  -> (OverlayCellSupport leftLabel rightLabel -> Bool)
  -> (leftLabel -> rightLabel -> Bool)
  -> OverlayResult leftLabel rightLabel
  -> Either OverlaySelectionError ExactCellSet
selectClosedCells selectionKind selectSupport selectFace result =
  let outsidePair = overlayResultOutsideLabels result
      triangulation = overlayResultTriangulation result
   in if uncurry selectFace outsidePair
        then Left (OverlaySelectionContainsUnboundedCell selectionKind)
        else do
          selectedVertices <-
            filterM
              ( fmap selectSupport
                  . first OverlaySelectionProvenance
                  . vertexSupport result
              )
              (vertices triangulation)
          selectedEdges <-
            filterM
              (\edge ->
                 case Dcel.undirectedEdgeData triangulation edge of
                   OverlayDiagonal -> Right False
                   OverlayBoundary _ ->
                     selectSupport
                       <$> first OverlaySelectionProvenance (edgeSupport result edge))
              (undirectedEdges triangulation)
          closeSelectedCells selectedVertices selectedEdges selectFace result

closeSelectedCells
  :: [VertexId]
  -> [UndirectedEdgeId]
  -> (leftLabel -> rightLabel -> Bool)
  -> OverlayResult leftLabel rightLabel
  -> Either OverlaySelectionError ExactCellSet
closeSelectedCells selectedVertices selectedEdges selectFace result =
  let triangulation = overlayResultTriangulation result
      exactPointAt vertex = Right (overlayExactPoint (vertexData triangulation vertex))
   in do
        selectedFaces <-
          filterM
            (\face ->
               if faceCarriesExactArea result face
                 then
                   fmap (uncurry selectFace)
                     ( first OverlaySelectionProvenance
                         (faceLabels result face)
                     )
                 else Right False)
            (innerFaces triangulation)
        first OverlaySelectionInvalid
          ( closeExactCellSetWith
              exactPointAt
              triangulation
              selectedVertices
              selectedEdges
              selectedFaces
          )

supportAny :: (label -> Bool) -> OverlaySupport label -> Bool
supportAny predicate = any predicate . overlaySupportLabels

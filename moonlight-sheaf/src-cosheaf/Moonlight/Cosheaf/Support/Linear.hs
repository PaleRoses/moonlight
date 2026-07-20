{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Cosheaf.Support.Linear
  ( LinearCosheafSupportCertificate (..),
    LinearCosheafSupportPlan,
    lcspCells,
    lcspFaces,
    lcspCoordinates,
    LinearCosheafSupportFailure (..),
    linearCosheafSupportPlanFromLists,
    validateLinearCosheafSupportPlan,
    fullLinearCosheafSupportPlan,
  )
where

import Data.Foldable (traverse_)
import Data.Kind (Type)
import Data.Set qualified as Set
import Moonlight.Cosheaf.Support.Carrier
  ( SupportCarrier,
    scContains,
    supportCarrierFromList,
    supportCarrierItems,
  )
import Moonlight.Cosheaf.Support.Footprint
  ( supportFootprintMeasure,
  )
import Moonlight.Sheaf.Footprint
  ( FootprintMeasure,
    FootprintMeasureUnit (..),
  )
import Moonlight.Sheaf.Site.Analysis.Scaffold
  ( SiteBoundaryAlgebra (..),
  )
import Numeric.Natural (Natural)

type LinearCosheafSupportCertificate :: Type
data LinearCosheafSupportCertificate
  = FullLinearCosheafSupport
  | LinearBoundaryClosedSupport
  deriving stock (Eq, Ord, Show, Read)

type LinearCosheafSupportPlan :: Type -> Type -> Type
data LinearCosheafSupportPlan cell face = LinearCosheafSupportPlan
  { lcspCells :: !(SupportCarrier cell),
    lcspFaces :: !(SupportCarrier face),
    lcspCoordinates :: !(SupportCarrier (cell, Int)),
    lcspFootprintMeasures :: ![FootprintMeasure Natural],
    lcspCertificate :: !LinearCosheafSupportCertificate
  }

type LinearCosheafSupportFailure :: Type -> Type -> Type
data LinearCosheafSupportFailure cell face
  = LinearCosheafSupportCellUnknown !cell
  | LinearCosheafSupportFaceUnknown !face
  | LinearCosheafSupportFaceEndpointPruned !face !cell !cell
  | LinearCosheafSupportCoordinateCellPruned !cell !Int
  | LinearCosheafSupportCoordinateUnknown !cell !Int
  | LinearCosheafSupportBoundaryExits !face !cell !Int !cell !Int
  | LinearCosheafSupportNegativeCostalkDimension !cell !Int
  deriving stock (Eq, Show)

linearCosheafSupportPlanFromLists ::
  (Ord cell, Ord face) =>
  [cell] ->
  [face] ->
  [(cell, Int)] ->
  LinearCosheafSupportPlan cell face
linearCosheafSupportPlanFromLists retainedCells retainedFaces retainedCoordinates =
  LinearCosheafSupportPlan
    { lcspCells = supportCarrierFromList retainedCells,
      lcspFaces = supportCarrierFromList retainedFaces,
      lcspCoordinates = supportCarrierFromList retainedCoordinates,
      lcspFootprintMeasures = [],
      lcspCertificate = LinearBoundaryClosedSupport
    }

validateLinearCosheafSupportPlan ::
  (Ord cell, Ord face) =>
  site ->
  SiteBoundaryAlgebra site cell face ->
  (cell -> Int) ->
  LinearCosheafSupportPlan cell face ->
  Either (LinearCosheafSupportFailure cell face) ()
validateLinearCosheafSupportPlan siteValue boundaryAlgebra dimensionOf supportPlan =
  traverse_ retainedCellDimensionValid retainedCells
    *> traverse_ retainedCellKnown retainedCells
    *> traverse_ retainedFaceKnown retainedFaces
    *> traverse_ retainedCoordinateKnown retainedCoordinates
  where
    maxDegreeInt =
      fromIntegral (sbaDepth boundaryAlgebra siteValue)

    retainedCells =
      supportCarrierItems (lcspCells supportPlan)

    retainedFaces =
      supportCarrierItems (lcspFaces supportPlan)

    retainedCoordinates =
      supportCarrierItems (lcspCoordinates supportPlan)

    allCells =
      Set.fromList (foldMap (sbaCellsAtDimension boundaryAlgebra siteValue) [0 .. maxDegreeInt])

    allFaces =
      Set.fromList (sbaFaceMorphisms boundaryAlgebra siteValue)

    retainedCell cell =
      scContains (lcspCells supportPlan) cell

    retainedCellDimensionValid cell =
      let dimensionValue =
            dimensionOf cell
       in if dimensionValue < 0
            then Left (LinearCosheafSupportNegativeCostalkDimension cell dimensionValue)
            else Right ()

    retainedCellKnown cell =
      if Set.member cell allCells
        then Right ()
        else Left (LinearCosheafSupportCellUnknown cell)

    retainedFaceKnown face
      | not (Set.member face allFaces) =
          Left (LinearCosheafSupportFaceUnknown face)
      | retainedCell sourceCell && retainedCell targetCell =
          Right ()
      | otherwise =
          Left (LinearCosheafSupportFaceEndpointPruned face sourceCell targetCell)
      where
        sourceCell =
          sbaFaceSource boundaryAlgebra face

        targetCell =
          sbaFaceTarget boundaryAlgebra face

    retainedCoordinateKnown (cell, coordinateIndex)
      | not (retainedCell cell) =
          Left (LinearCosheafSupportCoordinateCellPruned cell coordinateIndex)
      | coordinateIndex >= 0 && coordinateIndex < dimensionOf cell =
          Right ()
      | otherwise =
          Left (LinearCosheafSupportCoordinateUnknown cell coordinateIndex)

fullLinearCosheafSupportPlan ::
  (Ord cell, Ord face) =>
  site ->
  SiteBoundaryAlgebra site cell face ->
  (cell -> Int) ->
  Either (LinearCosheafSupportFailure cell face) (LinearCosheafSupportPlan cell face)
fullLinearCosheafSupportPlan siteValue boundaryAlgebra dimensionOf = do
  cellDimensions <-
    traverse costalkDimensionForCell cellsValue
  let coordinatesValue =
        cellDimensions >>= \(cellValue, dimensionValue) ->
          fmap (cellValue,) ([0 .. dimensionValue - 1] :: [Int])
      totalCoordinateCount =
        fromIntegral (length coordinatesValue)
  Right
    LinearCosheafSupportPlan
      { lcspCells = supportCarrierFromList cellsValue,
        lcspFaces = supportCarrierFromList facesValue,
        lcspCoordinates = supportCarrierFromList coordinatesValue,
        lcspFootprintMeasures =
          [ supportFootprintMeasure SupportCellUnit totalCellCount totalCellCount,
            supportFootprintMeasure CoboundaryRestrictionUnit totalFaceCount totalFaceCount,
            supportFootprintMeasure SparseEntryUnit totalCoordinateCount totalCoordinateCount
          ],
        lcspCertificate = FullLinearCosheafSupport
      }
  where
    maxDegreeInt =
      fromIntegral (sbaDepth boundaryAlgebra siteValue)

    cellsValue =
      foldMap (sbaCellsAtDimension boundaryAlgebra siteValue) [0 .. maxDegreeInt]

    facesValue =
      sbaFaceMorphisms boundaryAlgebra siteValue

    costalkDimensionForCell cellValue =
      let dimensionValue =
            dimensionOf cellValue
       in if dimensionValue < 0
            then Left (LinearCosheafSupportNegativeCostalkDimension cellValue dimensionValue)
            else Right (cellValue, dimensionValue)

    totalCellCount =
      fromIntegral (length cellsValue)

    totalFaceCount =
      fromIntegral (length facesValue)

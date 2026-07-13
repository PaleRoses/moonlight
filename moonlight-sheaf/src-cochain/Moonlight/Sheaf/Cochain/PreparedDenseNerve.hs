{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Moonlight.Sheaf.Cochain.PreparedDenseNerve
  ( PreparedDenseNerveCochainPlan,
    PreparedDenseNerveRankOneCoboundaryPlan,
    DenseNerveCellRef,
    DenseNerveFaceRef,
    PreparedDenseNerveCochainError (..),
    denseNerveCellRefDimension,
    denseNerveCellRefOrdinal,
    denseNerveFaceRefSource,
    denseNerveFaceRefTarget,
    denseNerveFaceRefFaceIndex,
    denseNerveFaceRefKind,
    denseNerveFaceRefOrientation,
    preparedDenseNerveCellsAtDimension,
    preparedDenseNerveFacesAtDimension,
    prepareDenseNerveCochainPlan,
    preparedDenseNerveComplexScaffold,
    materializePreparedDenseNerveCoboundaryComplex,
    materializePreparedDenseNerveRankOneCoboundaryComplexWith,
    preparePreparedDenseNerveRankOneCoboundaryPlanWith,
    applyPreparedDenseNerveRankOneCoboundaryPlanDense,
    applyPreparedDenseNerveRankOneCoboundaryDense,
    projectPreparedDenseNerveSite,
  )
where

import Data.Bifunctor (first, second)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Data.Vector.Unboxed qualified as Unboxed
import Moonlight.Category
  ( Category (..),
    FiniteComposableCategory,
  )
import Moonlight.Homology
  ( BoundaryEntry,
    BoundaryIncidence,
    BoundaryIncidenceShapeError (..),
    FiniteChainComplex,
    HomologicalDegree (..),
    HomologyFailure,
    boundaryCoefficient,
    boundaryEntries,
    emptyBoundaryIncidenceOf,
    mkBoundaryEntry,
    mkBoundaryIncidenceFromOrderedEntries,
    mkFiniteChainComplexChecked,
    sourceIndex,
    targetIndex,
    transposeBoundaryIncidence,
  )
import Moonlight.LinAlg.Sparse
  ( PackedSparseOperator,
  )
import Moonlight.Sheaf.Kernel.Basis
  ( SheafBasis,
    mkSheafBasis,
  )
import Moonlight.Sheaf.Operator.BuildError
  ( OperatorBasisRole (..),
    SheafOperatorBuildError (..),
  )
import Moonlight.Sheaf.Operator.GradedComplex
import Moonlight.Sheaf.Operator.LinearBasis
  ( LinearBasis,
    linearBasisCardinality,
    linearBasisCellSlotOrError,
    mkLinearBasis,
  )
import Moonlight.Sheaf.Operator.Sparse
  ( applyPackedSparseOperatorDenseAsSheafOperator,
    packedSparseOperatorFromBoundary,
    validateBoundaryBlockShape,
  )
import Moonlight.Sheaf.Section.Linearize
  ( StalkLinearization (..),
  )
import Moonlight.Sheaf.Site.Analysis.Scaffold
  ( SiteComplexScaffold,
    mkSiteComplexScaffoldFromCells,
  )
import Moonlight.Sheaf.Site.Construction.Nerve
  ( FaceKind (..),
    NerveCategory,
    NerveMorphism,
    NerveSite,
    NerveSiteAlgebra (..),
    NerveSource,
    faceKindFor,
    mkNerveSiteFromRowPlan,
  )
import Moonlight.Sheaf.Site.Skeleton.RowSource
  ( DenseNerveArrangement,
    DenseNerveArrangementError,
    NerveRowSource,
    denseArrangementCategory,
    denseOrdinalSkeletonRowPlanWithDepth,
    nerveRowsAtDimension,
    nerveSimplexFace,
    prepareDenseNerveArrangement,
    rowSourceToTruncatedNerve,
    skeletonRowPlanSource,
  )
import Moonlight.Sheaf.Site.Internal.Face
  ( faceOrientation,
  )
import Moonlight.Sheaf.Site.Stalk.Interface
  ( FaceStalkProjectionError (..),
    InterfaceComposeError,
    InterfaceDomain (..),
    InterfaceMorphism,
    InterfaceObject,
    InterfaceStalk,
    projectInterfaceFaceMorphisms,
    stalkFromSourceAndMorphisms,
  )
import Moonlight.Category.Simplicial
  ( NerveSimplex,
    isNerveSimplexDegenerate,
    nerveSimplexDimension,
  )
import Moonlight.Category.Simplicial (TruncatedSSetObstruction)
import Numeric.Natural (Natural)

type DenseNerveCellRef :: Type -> Type
data DenseNerveCellRef tag = DenseNerveCellRef
  { dncrDimension :: !Int,
    dncrOrdinal :: !Int
  }
  deriving stock (Eq, Ord, Show)

denseNerveCellRefDimension, denseNerveCellRefOrdinal :: DenseNerveCellRef tag -> Int
denseNerveCellRefDimension =
  dncrDimension
denseNerveCellRefOrdinal =
  dncrOrdinal

type DenseNerveFaceRef :: Type -> Type
data DenseNerveFaceRef tag = DenseNerveFaceRef
  { dnfrSource :: !(DenseNerveCellRef tag),
    dnfrTarget :: !(DenseNerveCellRef tag),
    dnfrFaceIndex :: !Natural,
    dnfrKind :: !FaceKind,
    dnfrOrientation :: !Int
  }
  deriving stock (Eq, Ord, Show)

denseNerveFaceRefSource, denseNerveFaceRefTarget :: DenseNerveFaceRef tag -> DenseNerveCellRef tag
denseNerveFaceRefSource =
  dnfrSource
denseNerveFaceRefTarget =
  dnfrTarget

denseNerveFaceRefFaceIndex :: DenseNerveFaceRef tag -> Natural
denseNerveFaceRefFaceIndex =
  dnfrFaceIndex

denseNerveFaceRefKind :: DenseNerveFaceRef tag -> FaceKind
denseNerveFaceRefKind =
  dnfrKind

denseNerveFaceRefOrientation :: DenseNerveFaceRef tag -> Int
denseNerveFaceRefOrientation =
  dnfrOrientation

type PreparedDenseNerveCochainPlan :: Type -> Type
data PreparedDenseNerveCochainPlan tag = PreparedDenseNerveCochainPlan
  { pdnDepth :: !Natural,
    pdnMaxCoboundarySourceDimension :: !Natural,
    pdnArrangement :: !(DenseNerveArrangement (NerveCategory tag)),
    pdnRowsByDimension :: !(Map Int [PackedDenseNerveRow tag]),
    pdnFacesBySourceDimension :: !(Map Int [DenseNerveFaceRef tag]),
    pdnRowsByCell :: !(Map (DenseNerveCellRef tag) (PackedDenseNerveRow tag))
  }

type PreparedDenseNerveRankOneCoboundaryPlan :: Type -> Type
data PreparedDenseNerveRankOneCoboundaryPlan tag = PreparedDenseNerveRankOneCoboundaryPlan
  { pdnrcpCochainPlan :: !(PreparedDenseNerveCochainPlan tag),
    pdnrcpPackedCoboundariesByDegree :: !(Vector.Vector (PackedSparseOperator Int))
  }

instance Show (PreparedDenseNerveCochainPlan tag) where
  show plan =
    "PreparedDenseNerveCochainPlan "
      <> show
        ( pdnDepth plan,
          fmap (second length) (Map.toAscList (pdnRowsByDimension plan)),
          fmap (second length) (Map.toAscList (pdnFacesBySourceDimension plan))
        )

type PackedDenseNerveRow :: Type -> Type
data PackedDenseNerveRow tag = PackedDenseNerveRow
  { pdnrCellRef :: !(DenseNerveCellRef tag),
    pdnrSimplex :: !(NerveSimplex (NerveCategory tag)),
    pdnrKey :: !(DenseNerveRowKey tag),
    pdnrStalk :: !(InterfaceStalk tag)
  }

type DenseNerveRowKey :: Type -> Type
type DenseNerveRowKey tag = (NerveSource tag, [NerveMorphism tag])

type DenseNerveInterface tag =
  ( NerveSiteAlgebra tag,
    InterfaceDomain tag,
    NerveSource tag ~ InterfaceObject tag,
    NerveMorphism tag ~ InterfaceMorphism tag
  )

type DenseNerveFiniteCategory tag =
  ( DenseNerveInterface tag,
    FiniteComposableCategory (NerveCategory tag),
    Ord (Ob (NerveCategory tag)),
    Eq (Mor (NerveCategory tag)),
    Ord (NerveSource tag),
    Ord (NerveMorphism tag)
  )

type DenseNerveFaceCategory tag =
  ( DenseNerveInterface tag,
    Category (NerveCategory tag),
    Eq (Ob (NerveCategory tag)),
    Eq (Mor (NerveCategory tag)),
    Ord (DenseNerveRowKey tag)
  )

type PreparedDenseNerveCochainError :: Type -> Type
data PreparedDenseNerveCochainError tag
  = PreparedDenseNerveArrangementFailed !(DenseNerveArrangementError (NerveCategory tag))
  | PreparedDenseNerveDepthOutOfBounds !Natural
  | PreparedDenseNerveRawFaceCompositionMissing !(DenseNerveCellRef tag) !Natural
  | PreparedDenseNerveInnerFaceIndexOutOfRange !(DenseNerveCellRef tag) !Natural
  | PreparedDenseNerveAdjacentCompositionFailed !(DenseNerveCellRef tag) !Natural !(InterfaceComposeError tag)
  | PreparedDenseNerveFaceTargetAbsent !(DenseNerveCellRef tag) !Natural
  | PreparedDenseNerveTruncatedNerveInvalid !(NonEmpty (TruncatedSSetObstruction (NerveSimplex (NerveCategory tag))))
  | PreparedDenseNerveDegreeOutOfRange !HomologicalDegree
  | PreparedDenseNerveBoundaryShapeFailed !BoundaryIncidenceShapeError
  | PreparedDenseNerveChainComplexFailed !HomologyFailure
  | PreparedDenseNerveOperatorBuildFailed !(SheafOperatorBuildError (DenseNerveCellRef tag))

type PreparedDenseNerveResult tag value = Either (PreparedDenseNerveCochainError tag) value
type PreparedDenseNerveScaffold tag = SiteComplexScaffold (PreparedDenseNerveCochainPlan tag) (DenseNerveCellRef tag)
type PreparedDenseNerveDifferential tag = GradedOperator (DenseNerveCellRef tag) Int
type PreparedDenseNerveComplex tag = GradedComplex (DenseNerveCellRef tag) Int

instance Show (PreparedDenseNerveCochainError tag) where
  show errorValue =
    case errorValue of
      PreparedDenseNerveArrangementFailed _ ->
        "PreparedDenseNerveArrangementFailed"
      PreparedDenseNerveDepthOutOfBounds depthValue ->
        "PreparedDenseNerveDepthOutOfBounds " <> show depthValue
      PreparedDenseNerveRawFaceCompositionMissing cellRef faceIndex ->
        "PreparedDenseNerveRawFaceCompositionMissing " <> show (cellRef, faceIndex)
      PreparedDenseNerveInnerFaceIndexOutOfRange cellRef innerIndex ->
        "PreparedDenseNerveInnerFaceIndexOutOfRange " <> show (cellRef, innerIndex)
      PreparedDenseNerveAdjacentCompositionFailed cellRef innerIndex _ ->
        "PreparedDenseNerveAdjacentCompositionFailed " <> show (cellRef, innerIndex)
      PreparedDenseNerveFaceTargetAbsent cellRef faceIndex ->
        "PreparedDenseNerveFaceTargetAbsent " <> show (cellRef, faceIndex)
      PreparedDenseNerveTruncatedNerveInvalid _ ->
        "PreparedDenseNerveTruncatedNerveInvalid"
      PreparedDenseNerveDegreeOutOfRange degreeValue ->
        "PreparedDenseNerveDegreeOutOfRange " <> show degreeValue
      PreparedDenseNerveBoundaryShapeFailed shapeError ->
        "PreparedDenseNerveBoundaryShapeFailed " <> show shapeError
      PreparedDenseNerveChainComplexFailed failure ->
        "PreparedDenseNerveChainComplexFailed " <> show failure
      PreparedDenseNerveOperatorBuildFailed buildError ->
        "PreparedDenseNerveOperatorBuildFailed " <> show buildError

preparedDenseNerveCellsAtDimension :: PreparedDenseNerveCochainPlan tag -> Int -> [DenseNerveCellRef tag]
preparedDenseNerveCellsAtDimension plan dimensionValue =
  fmap pdnrCellRef (Map.findWithDefault [] dimensionValue (pdnRowsByDimension plan))

cellCountAtDimension :: PreparedDenseNerveCochainPlan tag -> Int -> Int
cellCountAtDimension plan =
  length . preparedDenseNerveCellsAtDimension plan

preparedDenseNerveFacesAtDimension :: PreparedDenseNerveCochainPlan tag -> Int -> [DenseNerveFaceRef tag]
preparedDenseNerveFacesAtDimension plan sourceDimensionValue =
  Map.findWithDefault [] sourceDimensionValue (pdnFacesBySourceDimension plan)

prepareDenseNerveCochainPlan ::
  forall tag.
  DenseNerveFiniteCategory tag =>
  NerveCategory tag ->
  Natural ->
  PreparedDenseNerveResult tag (PreparedDenseNerveCochainPlan tag)
prepareDenseNerveCochainPlan categoryValue maxCoboundarySourceDimension = do
  arrangement <-
    first PreparedDenseNerveArrangementFailed (prepareDenseNerveArrangement categoryValue)
  depthInt <- naturalToBoundedInt (maxCoboundarySourceDimension + 1)
  let denseRowSource =
        skeletonRowPlanSource
          ( denseOrdinalSkeletonRowPlanWithDepth
              arrangement
              (maxCoboundarySourceDimension + 1)
              (Set.fromList [0 .. maxCoboundarySourceDimension + 1])
              Set.empty
          )
  let rowsByDimensionMap =
        Map.fromAscList $
          fmap (preparedRowsAtDimension @tag categoryValue denseRowSource) [0 .. depthInt]
      rowLookup = rowLookupMap rowsByDimensionMap
  facesBySourceDimension <-
    Map.fromAscList
      <$> traverse
        (preparedFacesAtDimension @tag categoryValue rowLookup rowsByDimensionMap)
        [1 .. depthInt]
  pure
    PreparedDenseNerveCochainPlan
      { pdnDepth = maxCoboundarySourceDimension + 1,
        pdnMaxCoboundarySourceDimension = maxCoboundarySourceDimension,
        pdnArrangement = arrangement,
        pdnRowsByDimension = rowsByDimensionMap,
        pdnFacesBySourceDimension = facesBySourceDimension,
        pdnRowsByCell =
          Map.fromList
            [ (pdnrCellRef rowValue, rowValue)
            | rowValue <- concat (Map.elems rowsByDimensionMap)
            ]
      }

preparedDenseNerveFiniteChainComplex :: PreparedDenseNerveCochainPlan tag -> PreparedDenseNerveResult tag (FiniteChainComplex Int)
preparedDenseNerveFiniteChainComplex plan = do
  depthInt <- naturalToBoundedInt (pdnDepth plan)
  incidencesByDimension <-
    Map.fromAscList
      <$> traverse (topologicalBoundaryAtDimension plan) [0 .. depthInt]
  first PreparedDenseNerveChainComplexFailed $
    mkFiniteChainComplexChecked
      (HomologicalDegree depthInt)
      ( \degreeValue ->
          let HomologicalDegree dimensionValue = degreeValue
           in Map.findWithDefault
                (emptyBoundaryIncidenceOf 0 0)
                dimensionValue
                incidencesByDimension
      )

preparedDenseNerveComplexScaffold :: PreparedDenseNerveCochainPlan tag -> PreparedDenseNerveResult tag (PreparedDenseNerveScaffold tag)
preparedDenseNerveComplexScaffold plan = do
  chainComplexValue <- preparedDenseNerveFiniteChainComplex plan
  let cellsByDimensionValue = fmap (fmap pdnrCellRef) (pdnRowsByDimension plan)
  pure (mkSiteComplexScaffoldFromCells plan cellsByDimensionValue chainComplexValue)

materializePreparedDenseNerveCoboundaryComplex :: StalkLinearization (InterfaceStalk tag) Int -> PreparedDenseNerveCochainPlan tag -> PreparedDenseNerveResult tag (PreparedDenseNerveComplex tag)
materializePreparedDenseNerveCoboundaryComplex linearization plan =
  preparedCochainComplexFromDifferentials
    plan
    (explicitDifferentialAtDegree linearization plan)

materializePreparedDenseNerveRankOneCoboundaryComplexWith ::
  (DenseNerveFaceRef tag -> InterfaceStalk tag -> InterfaceStalk tag -> Int) ->
  PreparedDenseNerveCochainPlan tag ->
  PreparedDenseNerveResult tag (PreparedDenseNerveComplex tag)
materializePreparedDenseNerveRankOneCoboundaryComplexWith scalarCoefficient plan =
  preparedCochainComplexFromDifferentials
    plan
    (rankOneDifferentialAtDegree scalarCoefficient plan)

preparedCochainComplexFromDifferentials :: PreparedDenseNerveCochainPlan tag -> (Int -> PreparedDenseNerveResult tag (PreparedDenseNerveDifferential tag)) -> PreparedDenseNerveResult tag (PreparedDenseNerveComplex tag)
preparedCochainComplexFromDifferentials plan differentialAtDegree = do
  maxSourceDimension <- naturalToBoundedInt (pdnMaxCoboundarySourceDimension plan)
  differentials <- traverse differentialAtDegree [0 .. maxSourceDimension]
  first PreparedDenseNerveOperatorBuildFailed
    (mkGradedComplexFromList DegreeIncreasing differentials)

applyPreparedDenseNerveRankOneCoboundaryDense ::
  (DenseNerveFaceRef tag -> InterfaceStalk tag -> InterfaceStalk tag -> Int) ->
  PreparedDenseNerveCochainPlan tag ->
  HomologicalDegree ->
  Unboxed.Vector Int ->
  PreparedDenseNerveResult tag (Unboxed.Vector Int)
applyPreparedDenseNerveRankOneCoboundaryDense scalarCoefficient =
  applyPreparedDenseCoboundaryDenseWith
    (rankOnePackedCoboundaryAtDegree scalarCoefficient)

preparePreparedDenseNerveRankOneCoboundaryPlanWith ::
  (DenseNerveFaceRef tag -> InterfaceStalk tag -> InterfaceStalk tag -> Int) ->
  PreparedDenseNerveCochainPlan tag ->
  PreparedDenseNerveResult tag (PreparedDenseNerveRankOneCoboundaryPlan tag)
preparePreparedDenseNerveRankOneCoboundaryPlanWith scalarCoefficient plan = do
  maxSourceDimension <- naturalToBoundedInt (pdnMaxCoboundarySourceDimension plan)
  packedCoboundaries <-
    Vector.generateM
      (maxSourceDimension + 1)
      (rankOnePackedCoboundaryAtDegree scalarCoefficient plan)
  pure
    PreparedDenseNerveRankOneCoboundaryPlan
      { pdnrcpCochainPlan = plan,
        pdnrcpPackedCoboundariesByDegree = packedCoboundaries
      }

applyPreparedDenseNerveRankOneCoboundaryPlanDense ::
  PreparedDenseNerveRankOneCoboundaryPlan tag ->
  HomologicalDegree ->
  Unboxed.Vector Int ->
  PreparedDenseNerveResult tag (Unboxed.Vector Int)
applyPreparedDenseNerveRankOneCoboundaryPlanDense packedPlan degreeValue sourceVector = do
  let plan =
        pdnrcpCochainPlan packedPlan
  degreeInt <- degreeToInt plan degreeValue
  packedOperator <-
    case pdnrcpPackedCoboundariesByDegree packedPlan Vector.!? degreeInt of
      Just operator ->
        Right operator
      Nothing ->
        Left (PreparedDenseNerveDegreeOutOfRange degreeValue)
  applyPackedPreparedDenseCoboundary packedOperator sourceVector

applyPreparedDenseCoboundaryDenseWith ::
  ( PreparedDenseNerveCochainPlan tag ->
    Int ->
    PreparedDenseNerveResult tag (PackedSparseOperator Int)
  ) ->
  PreparedDenseNerveCochainPlan tag ->
  HomologicalDegree ->
  Unboxed.Vector Int ->
  PreparedDenseNerveResult tag (Unboxed.Vector Int)
applyPreparedDenseCoboundaryDenseWith packedAtDegree plan degreeValue sourceVector = do
  degreeInt <- degreeToInt plan degreeValue
  packedOperator <- packedAtDegree plan degreeInt
  applyPackedPreparedDenseCoboundary packedOperator sourceVector

applyPackedPreparedDenseCoboundary ::
  PackedSparseOperator Int ->
  Unboxed.Vector Int ->
  PreparedDenseNerveResult tag (Unboxed.Vector Int)
applyPackedPreparedDenseCoboundary packedOperator sourceVector =
  first PreparedDenseNerveOperatorBuildFailed
    ( applyPackedSparseOperatorDenseAsSheafOperator
        OperatorSourceBasis
        packedOperator
        sourceVector
    )

projectPreparedDenseNerveSite ::
  forall tag.
  ( NerveSiteAlgebra tag,
    Category (NerveCategory tag),
    Eq (Ob (NerveCategory tag)),
    Eq (Mor (NerveCategory tag)),
    Ord (NerveSource tag),
    Ord (NerveMorphism tag)
  ) =>
  PreparedDenseNerveCochainPlan tag ->
  PreparedDenseNerveResult tag (NerveSite tag)
projectPreparedDenseNerveSite plan =
  let rowPlan =
        denseOrdinalSkeletonRowPlanWithDepth
          (pdnArrangement plan)
          (pdnDepth plan)
          (Set.fromList [0 .. pdnDepth plan])
          (Set.fromList [1 .. pdnDepth plan])
   in do
        sourceNerve <-
          first PreparedDenseNerveTruncatedNerveInvalid (rowSourceToTruncatedNerve rowPlan)
        pure (mkNerveSiteFromRowPlan @tag (denseArrangementCategory (pdnArrangement plan)) sourceNerve rowPlan)

preparedRowsAtDimension ::
  forall tag.
  DenseNerveInterface tag =>
  NerveCategory tag ->
  NerveRowSource (NerveCategory tag) ->
  Int ->
  (Int, [PackedDenseNerveRow tag])
preparedRowsAtDimension categoryValue rowSource dimensionValue =
  ( dimensionValue,
    fmap
      (uncurry (rawRowToPreparedRow @tag categoryValue dimensionValue))
      (zip [0 ..] (nerveRowsAtDimension rowSource (fromIntegral dimensionValue)))
  )

rawRowToPreparedRow ::
  forall tag.
  DenseNerveInterface tag =>
  NerveCategory tag ->
  Int ->
  Int ->
  NerveSimplex (NerveCategory tag) ->
  PackedDenseNerveRow tag
rawRowToPreparedRow categoryValue dimensionValue rowOrdinal simplexValue =
  let sourceValue = simplexSourceValue @tag simplexValue
      morphismValues = simplexMorphismChain @tag simplexValue
      cellRef =
        DenseNerveCellRef
          { dncrDimension = dimensionValue,
            dncrOrdinal = rowOrdinal
          }
   in PackedDenseNerveRow
        { pdnrCellRef = cellRef,
          pdnrSimplex = simplexValue,
          pdnrKey = (sourceValue, morphismValues),
          pdnrStalk = stalkFromSourceAndMorphisms @tag categoryValue sourceValue morphismValues dimensionValue
        }

rowLookupMap ::
  Ord (DenseNerveRowKey tag) =>
  Map Int [PackedDenseNerveRow tag] ->
  Map (Int, DenseNerveRowKey tag) (PackedDenseNerveRow tag)
rowLookupMap rowsByDimension =
  Map.fromList
    [ ((dimensionValue, pdnrKey rowValue), rowValue)
    | (dimensionValue, rows) <- Map.toAscList rowsByDimension,
      rowValue <- rows
    ]

preparedFacesAtDimension ::
  forall tag.
  DenseNerveFaceCategory tag =>
  NerveCategory tag ->
  Map (Int, DenseNerveRowKey tag) (PackedDenseNerveRow tag) ->
  Map Int [PackedDenseNerveRow tag] ->
  Int ->
  PreparedDenseNerveResult tag (Int, [DenseNerveFaceRef tag])
preparedFacesAtDimension categoryValue rowLookup rowsByDimension sourceDimensionValue =
  fmap
    ((,) sourceDimensionValue)
    ( concat
        <$> traverse
          (facesForPreparedRow @tag categoryValue rowLookup)
          (Map.findWithDefault [] sourceDimensionValue rowsByDimension)
    )

facesForPreparedRow ::
  forall tag.
  DenseNerveFaceCategory tag =>
  NerveCategory tag ->
  Map (Int, DenseNerveRowKey tag) (PackedDenseNerveRow tag) ->
  PackedDenseNerveRow tag ->
  PreparedDenseNerveResult tag [DenseNerveFaceRef tag]
facesForPreparedRow categoryValue rowLookup rowValue =
  let sourceDimensionValue = dncrDimension (pdnrCellRef rowValue)
   in catMaybes
        <$> traverse
          (preparedFaceAt @tag categoryValue rowLookup rowValue)
          [0 .. fromIntegral sourceDimensionValue]

preparedFaceAt ::
  forall tag.
  DenseNerveFaceCategory tag =>
  NerveCategory tag ->
  Map (Int, DenseNerveRowKey tag) (PackedDenseNerveRow tag) ->
  PackedDenseNerveRow tag ->
  Natural ->
  PreparedDenseNerveResult tag (Maybe (DenseNerveFaceRef tag))
preparedFaceAt categoryValue rowLookup rowValue faceIndexValue = do
  let sourceCell = pdnrCellRef rowValue
      sourceDimensionValue = dncrDimension sourceCell
  faceSimplex <-
    maybe
      (Left (PreparedDenseNerveRawFaceCompositionMissing sourceCell faceIndexValue))
      Right
      (nerveSimplexFace categoryValue faceIndexValue (pdnrSimplex rowValue))
  _projectedTargetMorphisms <-
    first
      (faceProjectionErrorToPrepared sourceCell)
      (projectInterfaceFaceMorphisms @tag categoryValue faceIndexValue (snd (pdnrKey rowValue)))
  if isNerveSimplexDegenerate categoryValue (nerveSimplexDimension faceSimplex) faceSimplex
    then Right Nothing
    else
      let targetKey =
            (simplexSourceValue @tag faceSimplex, simplexMorphismChain @tag faceSimplex)
       in case Map.lookup (sourceDimensionValue - 1, targetKey) rowLookup of
            Nothing ->
              Left (PreparedDenseNerveFaceTargetAbsent sourceCell faceIndexValue)
            Just targetRow ->
              Right
                ( Just
                    DenseNerveFaceRef
                      { dnfrSource = sourceCell,
                        dnfrTarget = pdnrCellRef targetRow,
                        dnfrFaceIndex = faceIndexValue,
                        dnfrKind = faceKindFor (fromIntegral sourceDimensionValue) faceIndexValue,
                        dnfrOrientation = faceOrientation faceIndexValue
                      }
                )

faceProjectionErrorToPrepared ::
  DenseNerveCellRef tag ->
  FaceStalkProjectionError tag ->
  PreparedDenseNerveCochainError tag
faceProjectionErrorToPrepared sourceCell projectionError =
  case projectionError of
    FaceStalkProjectionInnerFaceIndexOutOfRange innerIndexValue ->
      PreparedDenseNerveInnerFaceIndexOutOfRange sourceCell innerIndexValue
    FaceStalkProjectionAdjacentCompositionFailed innerIndexValue failureValue ->
      PreparedDenseNerveAdjacentCompositionFailed sourceCell innerIndexValue failureValue

topologicalBoundaryAtDimension :: PreparedDenseNerveCochainPlan tag -> Int -> PreparedDenseNerveResult tag (Int, BoundaryIncidence Int)
topologicalBoundaryAtDimension plan dimensionValue = do
  incidenceValue <-
    if dimensionValue <= 0
      then
        Right
          ( emptyBoundaryIncidenceOf
              (fromIntegral (cellCountAtDimension plan 0))
              0
          )
      else
        preparedBoundaryIncidence
          (cellCountAtDimension plan dimensionValue)
          (cellCountAtDimension plan (dimensionValue - 1))
          (fmap topologicalFaceEntry (preparedDenseNerveFacesAtDimension plan dimensionValue))
  Right (dimensionValue, incidenceValue)

topologicalFaceEntry :: DenseNerveFaceRef tag -> BoundaryEntry Int
topologicalFaceEntry faceRef =
  mkBoundaryEntry
    (fromIntegral (dncrOrdinal (dnfrSource faceRef)))
    (fromIntegral (dncrOrdinal (dnfrTarget faceRef)))
    (dnfrOrientation faceRef)

preparedDifferentialAtDegree ::
  ( PreparedDenseNerveCochainPlan tag ->
    Int ->
    PreparedDenseNerveResult tag (LinearBasis (DenseNerveCellRef tag))
  ) ->
  ( PreparedDenseNerveCochainPlan tag ->
    Int ->
    LinearBasis (DenseNerveCellRef tag) ->
    LinearBasis (DenseNerveCellRef tag) ->
    PreparedDenseNerveResult tag (BoundaryIncidence Int)
  ) ->
  PreparedDenseNerveCochainPlan tag ->
  Int ->
  PreparedDenseNerveResult tag (PreparedDenseNerveDifferential tag)
preparedDifferentialAtDegree linearBasisBuilder incidenceAtDegree plan degreeValue = do
  sourceLinearBasis <- linearBasisBuilder plan degreeValue
  targetLinearBasis <- linearBasisBuilder plan (degreeValue + 1)
  incidence <- incidenceAtDegree plan degreeValue sourceLinearBasis targetLinearBasis
  first PreparedDenseNerveOperatorBuildFailed
    ( mkGradedOperator
        (HomologicalDegree degreeValue)
        sourceLinearBasis
        targetLinearBasis
        incidence
    )

preparedBoundaryIncidence :: Int -> Int -> [BoundaryEntry Int] -> PreparedDenseNerveResult tag (BoundaryIncidence Int)
preparedBoundaryIncidence sourceCardinalityValue targetCardinalityValue =
  first PreparedDenseNerveBoundaryShapeFailed
    . mkBoundaryIncidenceFromOrderedEntries
      (fromIntegral sourceCardinalityValue)
      (fromIntegral targetCardinalityValue)

packPreparedBoundaryIncidence :: BoundaryIncidence Int -> PreparedDenseNerveResult tag (PackedSparseOperator Int)
packPreparedBoundaryIncidence =
  first PreparedDenseNerveOperatorBuildFailed
    . packedSparseOperatorFromBoundary

explicitDifferentialAtDegree ::
  StalkLinearization (InterfaceStalk tag) Int ->
  PreparedDenseNerveCochainPlan tag ->
  Int ->
  PreparedDenseNerveResult tag (PreparedDenseNerveDifferential tag)
explicitDifferentialAtDegree linearization =
  preparedDifferentialAtDegree
    (linearBasisAtDimension linearization)
    (explicitCoboundaryIncidenceAtDegree linearization)

explicitCoboundaryIncidenceAtDegree ::
  StalkLinearization (InterfaceStalk tag) Int ->
  PreparedDenseNerveCochainPlan tag ->
  Int ->
  LinearBasis (DenseNerveCellRef tag) ->
  LinearBasis (DenseNerveCellRef tag) ->
  PreparedDenseNerveResult tag (BoundaryIncidence Int)
explicitCoboundaryIncidenceAtDegree linearization plan degreeValue sourceLinearBasis targetLinearBasis = do
  entries <-
    concat
      <$> traverse
        (explicitFaceEntries linearization plan sourceLinearBasis targetLinearBasis)
        (preparedDenseNerveFacesAtDimension plan (degreeValue + 1))
  preparedBoundaryIncidence
    (linearBasisCardinality sourceLinearBasis)
    (linearBasisCardinality targetLinearBasis)
    entries

explicitFaceEntries ::
  StalkLinearization (InterfaceStalk tag) Int ->
  PreparedDenseNerveCochainPlan tag ->
  LinearBasis (DenseNerveCellRef tag) ->
  LinearBasis (DenseNerveCellRef tag) ->
  DenseNerveFaceRef tag ->
  PreparedDenseNerveResult tag [BoundaryEntry Int]
explicitFaceEntries linearization plan sourceBasis targetBasis faceRef = do
  let faceCell = dnfrTarget faceRef
      cofaceCell = dnfrSource faceRef
  faceStalk <- stalkOrError OperatorSourceBasis plan faceCell
  cofaceStalk <- stalkOrError OperatorTargetBasis plan cofaceCell
  (sourceOffsetValue, sourceDimensionValue) <-
    first PreparedDenseNerveOperatorBuildFailed
      (linearBasisCellSlotOrError OperatorSourceBasis sourceBasis faceCell)
  (targetOffsetValue, targetDimensionValue) <-
    first PreparedDenseNerveOperatorBuildFailed
      (linearBasisCellSlotOrError OperatorTargetBasis targetBasis cofaceCell)
  let restrictionIncidence =
        slRestrictionIncidence linearization cofaceStalk faceStalk
      coboundaryBlock =
        transposeBoundaryIncidence restrictionIncidence
  validatePreparedBlockShape sourceDimensionValue targetDimensionValue coboundaryBlock
  pure
    ( fmap
        (shiftBlockEntry (dnfrOrientation faceRef) sourceOffsetValue targetOffsetValue)
        (boundaryEntries coboundaryBlock)
    )

validatePreparedBlockShape ::
  Int ->
  Int ->
  BoundaryIncidence Int ->
  PreparedDenseNerveResult tag ()
validatePreparedBlockShape expectedSource expectedTarget blockValue =
  first
    PreparedDenseNerveOperatorBuildFailed
    (validateBoundaryBlockShape expectedSource expectedTarget blockValue)

shiftBlockEntry ::
  Int ->
  Int ->
  Int ->
  BoundaryEntry Int ->
  BoundaryEntry Int
shiftBlockEntry orientationValue sourceOffsetValue targetOffsetValue entry =
  mkBoundaryEntry
    (fromIntegral (sourceOffsetValue + sourceIndex entry))
    (fromIntegral (targetOffsetValue + targetIndex entry))
    (orientationValue * boundaryCoefficient entry)

rankOneDifferentialAtDegree ::
  (DenseNerveFaceRef tag -> InterfaceStalk tag -> InterfaceStalk tag -> Int) ->
  PreparedDenseNerveCochainPlan tag ->
  Int ->
  PreparedDenseNerveResult tag (PreparedDenseNerveDifferential tag)
rankOneDifferentialAtDegree scalarCoefficient =
  preparedDifferentialAtDegree
    rankOneLinearBasisAtDimension
    (rankOneCoboundaryIncidenceAtDegree scalarCoefficient)

rankOneCoboundaryIncidenceAtDegree ::
  (DenseNerveFaceRef tag -> InterfaceStalk tag -> InterfaceStalk tag -> Int) ->
  PreparedDenseNerveCochainPlan tag ->
  Int ->
  LinearBasis (DenseNerveCellRef tag) ->
  LinearBasis (DenseNerveCellRef tag) ->
  PreparedDenseNerveResult tag (BoundaryIncidence Int)
rankOneCoboundaryIncidenceAtDegree scalarCoefficient plan degreeValue sourceLinearBasis targetLinearBasis = do
  entries <-
    catMaybes
      <$> traverse
        (rankOneFaceEntry scalarCoefficient plan)
        (preparedDenseNerveFacesAtDimension plan (degreeValue + 1))
  preparedBoundaryIncidence
    (linearBasisCardinality sourceLinearBasis)
    (linearBasisCardinality targetLinearBasis)
    entries

rankOnePackedCoboundaryAtDegree ::
  (DenseNerveFaceRef tag -> InterfaceStalk tag -> InterfaceStalk tag -> Int) ->
  PreparedDenseNerveCochainPlan tag ->
  Int ->
  PreparedDenseNerveResult tag (PackedSparseOperator Int)
rankOnePackedCoboundaryAtDegree scalarCoefficient plan degreeValue = do
  sourceLinearBasis <- rankOneLinearBasisAtDimension plan degreeValue
  targetLinearBasis <- rankOneLinearBasisAtDimension plan (degreeValue + 1)
  incidence <-
    rankOneCoboundaryIncidenceAtDegree
      scalarCoefficient
      plan
      degreeValue
      sourceLinearBasis
      targetLinearBasis
  packPreparedBoundaryIncidence incidence

rankOneFaceEntry ::
  (DenseNerveFaceRef tag -> InterfaceStalk tag -> InterfaceStalk tag -> Int) ->
  PreparedDenseNerveCochainPlan tag ->
  DenseNerveFaceRef tag ->
  PreparedDenseNerveResult tag (Maybe (BoundaryEntry Int))
rankOneFaceEntry scalarCoefficient plan faceRef = do
  let faceCell = dnfrTarget faceRef
      cofaceCell = dnfrSource faceRef
  faceStalk <- stalkOrError OperatorSourceBasis plan faceCell
  cofaceStalk <- stalkOrError OperatorTargetBasis plan cofaceCell
  let coefficientValue =
        dnfrOrientation faceRef
          * scalarCoefficient faceRef faceStalk cofaceStalk
  pure
    ( if coefficientValue == 0
        then Nothing
        else
          Just
            ( mkBoundaryEntry
                (fromIntegral (dncrOrdinal faceCell))
                (fromIntegral (dncrOrdinal cofaceCell))
                coefficientValue
            )
    )

linearBasisAtDimension ::
  StalkLinearization (InterfaceStalk tag) Int ->
  PreparedDenseNerveCochainPlan tag ->
  Int ->
  PreparedDenseNerveResult tag (LinearBasis (DenseNerveCellRef tag))
linearBasisAtDimension linearization plan dimensionValue =
  let cellValues = preparedDenseNerveCellsAtDimension plan dimensionValue
   in do
        dimensionsByCell <-
          Map.fromList
            <$> traverse
              ( \cellRef ->
                  fmap
                    (\stalkValue -> (cellRef, slStalkDimension linearization stalkValue))
                    (stalkOrError OperatorDomainBasis plan cellRef)
              )
              cellValues
        first
          PreparedDenseNerveOperatorBuildFailed
          ( mkLinearBasis
              (\cellRef -> Map.findWithDefault 0 cellRef dimensionsByCell)
              (basisAtDimension plan dimensionValue)
          )

rankOneLinearBasisAtDimension ::
  PreparedDenseNerveCochainPlan tag ->
  Int ->
  PreparedDenseNerveResult tag (LinearBasis (DenseNerveCellRef tag))
rankOneLinearBasisAtDimension plan dimensionValue =
  first
    PreparedDenseNerveOperatorBuildFailed
    (mkLinearBasis (const 1) (basisAtDimension plan dimensionValue))

basisAtDimension ::
  PreparedDenseNerveCochainPlan tag ->
  Int ->
  SheafBasis (DenseNerveCellRef tag)
basisAtDimension plan dimensionValue =
  mkSheafBasis (preparedDenseNerveCellsAtDimension plan dimensionValue)

stalkOrError ::
  OperatorBasisRole ->
  PreparedDenseNerveCochainPlan tag ->
  DenseNerveCellRef tag ->
  PreparedDenseNerveResult tag (InterfaceStalk tag)
stalkOrError role plan cellRef =
  maybe
    (Left (PreparedDenseNerveOperatorBuildFailed (OperatorCellAbsentFromBasis role cellRef)))
    Right
    (pdnrStalk <$> Map.lookup cellRef (pdnRowsByCell plan))

degreeToInt ::
  PreparedDenseNerveCochainPlan tag ->
  HomologicalDegree ->
  PreparedDenseNerveResult tag Int
degreeToInt plan degreeValue =
  let HomologicalDegree degreeInt = degreeValue
   in if degreeInt < 0 || fromIntegral degreeInt > pdnMaxCoboundarySourceDimension plan
        then Left (PreparedDenseNerveDegreeOutOfRange degreeValue)
        else Right degreeInt

naturalToBoundedInt ::
  Natural ->
  PreparedDenseNerveResult tag Int
naturalToBoundedInt value =
  if value <= fromIntegral (maxBound :: Int)
    then Right (fromIntegral value)
    else Left (PreparedDenseNerveDepthOutOfBounds value)

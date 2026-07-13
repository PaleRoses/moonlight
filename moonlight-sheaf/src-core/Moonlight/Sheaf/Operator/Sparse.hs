{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Sheaf.Operator.Sparse
  ( BoundaryPairConvention (..),
    mkBoundaryIncidenceFromPairs,
    liftBoundaryShape,
    packedSparseOperatorFromBoundary,
    applyPackedSparseOperatorDenseAsSheafOperator,
    validateBoundaryBlockShape,
    validateLinearIncidenceShape,
  )
where

import Data.Bifunctor (first)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Tuple (swap)
import Data.Vector.Unboxed qualified as Unboxed
import Moonlight.Algebra (Semiring)
import Moonlight.Homology
  ( BoundaryEntry,
    BoundaryIncidence,
    BoundaryIncidenceShapeError (..),
    boundaryEntries,
    boundaryCoefficient,
    mkBoundaryEntry,
    mkBoundaryIncidenceFromOrderedEntries,
    sourceCardinality,
    sourceIndex,
    targetCardinality,
    targetIndex,
  )
import Moonlight.LinAlg.Sparse
  ( PackedSparseApplyError (..),
    PackedSparseEntry,
    PackedSparseOperator,
    PackedSparseOperatorShapeError (..),
    applyPackedSparseOperatorDense,
    mkPackedSparseOperator,
    packedSparseEntry,
  )
import Moonlight.Sheaf.Operator.BuildError
  ( OperatorBasisRole,
    SheafOperatorBuildError (..),
  )
import Moonlight.Sheaf.Operator.LinearBasis
  ( LinearBasis,
    linearBasisCardinality,
  )

data BoundaryPairConvention
  = SourceTargetPairs
  | RowColumnPairs
  deriving stock (Eq, Ord, Show)

boundaryPairCoordinates :: BoundaryPairConvention -> (Int, Int) -> (Int, Int)
boundaryPairCoordinates convention =
  case convention of
    SourceTargetPairs ->
      id
    RowColumnPairs ->
      swap

boundaryEntriesFromPairs ::
  BoundaryPairConvention ->
  Map (Int, Int) coefficient ->
  [BoundaryEntry coefficient]
boundaryEntriesFromPairs convention pairs =
  fmap toBoundaryEntry (Map.toList pairs)
  where
    toBoundaryEntry (pairValue, coefficientValue) =
      let (sourceIndexValue, targetIndexValue) =
            boundaryPairCoordinates convention pairValue
       in mkBoundaryEntry
            (fromIntegral sourceIndexValue)
            (fromIntegral targetIndexValue)
            coefficientValue

mkBoundaryIncidenceFromPairs ::
  (Eq coefficient, Semiring coefficient) =>
  Int ->
  Int ->
  BoundaryPairConvention ->
  Map (Int, Int) coefficient ->
  Either (SheafOperatorBuildError cell) (BoundaryIncidence coefficient)
mkBoundaryIncidenceFromPairs sourceCount targetCount convention pairs =
  liftBoundaryShape
    ( mkBoundaryIncidenceFromOrderedEntries
        (fromIntegral sourceCount)
        (fromIntegral targetCount)
        (boundaryEntriesFromPairs convention pairs)
    )

liftBoundaryShape ::
  Either BoundaryIncidenceShapeError value ->
  Either (SheafOperatorBuildError cell) value
liftBoundaryShape =
  first OperatorBoundaryShapeError

packedSparseOperatorFromBoundary ::
  (Eq coefficient, Num coefficient, Unboxed.Unbox coefficient) =>
  BoundaryIncidence coefficient ->
  Either (SheafOperatorBuildError cell) (PackedSparseOperator coefficient)
packedSparseOperatorFromBoundary incidence =
  first
    packedSparseOperatorShapeToOperatorBuildError
    ( mkPackedSparseOperator
        (fromIntegral (sourceCardinality incidence))
        (fromIntegral (targetCardinality incidence))
        (fmap boundaryEntryToPackedSparseEntry (boundaryEntries incidence))
    )

applyPackedSparseOperatorDenseAsSheafOperator ::
  (Num coefficient, Unboxed.Unbox coefficient) =>
  OperatorBasisRole ->
  PackedSparseOperator coefficient ->
  Unboxed.Vector coefficient ->
  Either (SheafOperatorBuildError cell) (Unboxed.Vector coefficient)
applyPackedSparseOperatorDenseAsSheafOperator sourceRole packedOperator =
  first
    (packedSparseApplyToOperatorBuildError sourceRole)
    . applyPackedSparseOperatorDense packedOperator

boundaryEntryToPackedSparseEntry ::
  BoundaryEntry coefficient ->
  PackedSparseEntry coefficient
boundaryEntryToPackedSparseEntry entry =
  packedSparseEntry
    (sourceIndex entry)
    (targetIndex entry)
    (boundaryCoefficient entry)

packedSparseOperatorShapeToOperatorBuildError :: PackedSparseOperatorShapeError -> SheafOperatorBuildError cell
packedSparseOperatorShapeToOperatorBuildError errorValue =
  case errorValue of
    PackedSparseCardinalityOutOfBounds cardinalityValue ->
      OperatorPackedSparseCardinalityOutOfBounds cardinalityValue
    PackedSparseEntryOutOfBounds sourceOffset targetOffset sourceDimension targetDimension ->
      OperatorBoundaryShapeError (BoundaryIncidenceEntryOutOfBounds sourceOffset targetOffset sourceDimension targetDimension)

packedSparseApplyToOperatorBuildError ::
  OperatorBasisRole ->
  PackedSparseApplyError ->
  SheafOperatorBuildError cell
packedSparseApplyToOperatorBuildError sourceRole errorValue =
  case errorValue of
    PackedSparseInputLengthMismatch expectedLength actualLength ->
      OperatorVectorLengthMismatch sourceRole expectedLength actualLength

validateLinearIncidenceShape ::
  LinearBasis cell ->
  LinearBasis cell ->
  BoundaryIncidence coefficient ->
  Either (SheafOperatorBuildError cell) ()
validateLinearIncidenceShape sourceBasis targetBasis = validateBoundaryBlockShape
    (linearBasisCardinality sourceBasis)
    (linearBasisCardinality targetBasis)

validateBoundaryBlockShape ::
  Int ->
  Int ->
  BoundaryIncidence coefficient ->
  Either (SheafOperatorBuildError cell) ()
validateBoundaryBlockShape expectedSourceDimension expectedTargetDimension incidence =
  if sourceCardinality incidence == expectedSourceDimension
    && targetCardinality incidence == expectedTargetDimension
    then Right ()
    else
      Left
        ( OperatorBoundaryShapeError
            ( BoundaryIncidenceBlockShapeMismatch
                expectedSourceDimension
                expectedTargetDimension
                (sourceCardinality incidence)
                (targetCardinality incidence)
            )
        )

{-# LANGUAGE DerivingStrategies #-}
module Moonlight.Sheaf.Operator.BuildError
  ( OperatorBasisRole (..),
    SheafOperatorBuildError (..),
  )
where

import Moonlight.Homology
  ( BoundaryIncidenceShapeError,
    HomologicalDegree,
  )
import Moonlight.Sheaf.Section.Store.Types
import Numeric.Natural (Natural)

data OperatorBasisRole
  = OperatorSourceBasis
  | OperatorTargetBasis
  | OperatorDomainBasis
  | OperatorCodomainBasis
  deriving stock (Eq, Ord, Show)

data SheafOperatorBuildError cell
  = OperatorBoundaryShapeError BoundaryIncidenceShapeError
  | OperatorPackedSparseCardinalityOutOfBounds Natural
  | OperatorCellAbsentFromBasis OperatorBasisRole cell
  | OperatorExpectedIncidenceRestriction cell cell
  | OperatorZeroIncidenceCoefficient cell cell
  | OperatorVectorLengthMismatch OperatorBasisRole Int Int
  | OperatorSectionLookupFailure cell (SectionLookupError cell)
  | OperatorNegativeStalkDimension cell Int
  | OperatorExpectedScalarCell cell Int
  | OperatorDifferentialDegreeMismatch HomologicalDegree HomologicalDegree
  | OperatorDuplicateDifferentialDegree HomologicalDegree
  | OperatorMissingDifferential HomologicalDegree
  | OperatorIntermediateBasisMismatch HomologicalDegree HomologicalDegree
  | OperatorNonNilpotent HomologicalDegree HomologicalDegree Int Int
  | OperatorStalkCoordinateDimensionMismatch cell Int Int
  deriving stock (Eq, Show)

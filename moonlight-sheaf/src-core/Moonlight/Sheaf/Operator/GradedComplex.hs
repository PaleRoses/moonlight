{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Moonlight.Sheaf.Operator.GradedComplex
  ( GradedDirection (..),
    GradedOperator,
    gradedOperatorDegree,
    gradedOperatorSourceBasis,
    gradedOperatorTargetBasis,
    gradedOperatorIncidence,
    mkGradedOperator,
    GradedComplex,
    emptyGradedComplex,
    gradedComplexDirection,
    gradedOperatorsByDegree,
    mkGradedComplex,
    mkGradedComplexFromList,
    gradedOperatorAt,
  )
where

import Control.Monad (foldM)
import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Moonlight.Algebra (Semiring)
import Moonlight.Homology
  ( BoundaryIncidence,
    BoundaryIncidenceShapeError (..),
    HomologicalDegree,
    boundaryEntries,
    composeBoundaryIncidence,
    decrementDegree,
    incrementDegree,
    sourceCardinality,
    sourceIndex,
    targetCardinality,
    targetIndex,
  )
import Moonlight.Sheaf.Operator.BuildError
  ( SheafOperatorBuildError (..),
  )
import Moonlight.Sheaf.Operator.LinearBasis
  ( LinearBasis,
    linearBasisCardinality,
  )

data GradedDirection
  = DegreeIncreasing
  | DegreeDecreasing
  deriving stock (Eq, Ord, Show)

nextDegreeFor :: GradedDirection -> HomologicalDegree -> HomologicalDegree
nextDegreeFor direction =
  case direction of
    DegreeIncreasing ->
      incrementDegree
    DegreeDecreasing ->
      decrementDegree
{-# INLINE nextDegreeFor #-}

data GradedOperator cell coefficient = GradedOperator
  { gradedOperatorDegreeInternal :: HomologicalDegree,
    gradedOperatorSourceBasisInternal :: LinearBasis cell,
    gradedOperatorTargetBasisInternal :: LinearBasis cell,
    gradedOperatorIncidenceInternal :: BoundaryIncidence coefficient
  }
  deriving stock (Eq, Show)

data GradedComplex cell coefficient = GradedComplex
  { gradedComplexDirectionInternal :: GradedDirection,
    gradedOperatorsByDegreeInternal :: Map HomologicalDegree (GradedOperator cell coefficient)
  }
  deriving stock (Eq, Show)

gradedOperatorDegree :: GradedOperator cell coefficient -> HomologicalDegree
gradedOperatorDegree = gradedOperatorDegreeInternal
{-# INLINE gradedOperatorDegree #-}

gradedOperatorSourceBasis :: GradedOperator cell coefficient -> LinearBasis cell
gradedOperatorSourceBasis = gradedOperatorSourceBasisInternal
{-# INLINE gradedOperatorSourceBasis #-}

gradedOperatorTargetBasis :: GradedOperator cell coefficient -> LinearBasis cell
gradedOperatorTargetBasis = gradedOperatorTargetBasisInternal
{-# INLINE gradedOperatorTargetBasis #-}

gradedOperatorIncidence :: GradedOperator cell coefficient -> BoundaryIncidence coefficient
gradedOperatorIncidence = gradedOperatorIncidenceInternal
{-# INLINE gradedOperatorIncidence #-}

gradedComplexDirection :: GradedComplex cell coefficient -> GradedDirection
gradedComplexDirection = gradedComplexDirectionInternal
{-# INLINE gradedComplexDirection #-}

gradedOperatorsByDegree :: GradedComplex cell coefficient -> Map HomologicalDegree (GradedOperator cell coefficient)
gradedOperatorsByDegree = gradedOperatorsByDegreeInternal
{-# INLINE gradedOperatorsByDegree #-}

emptyGradedComplex :: GradedDirection -> GradedComplex cell coefficient
emptyGradedComplex direction =
  GradedComplex
    { gradedComplexDirectionInternal = direction,
      gradedOperatorsByDegreeInternal = Map.empty
    }

mkGradedOperator ::
  HomologicalDegree ->
  LinearBasis cell ->
  LinearBasis cell ->
  BoundaryIncidence coefficient ->
  Either (SheafOperatorBuildError cell) (GradedOperator cell coefficient)
mkGradedOperator degree sourceBasis targetBasis incidence = do
  validateLinearIncidenceCardinality sourceBasis targetBasis incidence
  Right
    GradedOperator
      { gradedOperatorDegreeInternal = degree,
        gradedOperatorSourceBasisInternal = sourceBasis,
        gradedOperatorTargetBasisInternal = targetBasis,
        gradedOperatorIncidenceInternal = incidence
      }

mkGradedComplexFromList ::
  (Eq cell, Eq coefficient, Num coefficient, Semiring coefficient) =>
  GradedDirection ->
  [GradedOperator cell coefficient] ->
  Either (SheafOperatorBuildError cell) (GradedComplex cell coefficient)
{-# INLINABLE mkGradedComplexFromList #-}
mkGradedComplexFromList direction operators =
  foldM insertOperator Map.empty operators >>= mkGradedComplex direction
  where
    insertOperator ::
      Map HomologicalDegree (GradedOperator cell coefficient) ->
      GradedOperator cell coefficient ->
      Either (SheafOperatorBuildError cell) (Map HomologicalDegree (GradedOperator cell coefficient))
    insertOperator operatorsByDegree operator =
      let degree = gradedOperatorDegree operator
          (existingOperator, updatedOperators) =
            Map.insertLookupWithKey (\_ newOperator _ -> newOperator) degree operator operatorsByDegree
       in maybe
            (Right updatedOperators)
            (const (Left (OperatorDuplicateDifferentialDegree degree)))
            existingOperator

mkGradedComplex ::
  forall cell coefficient.
  (Eq cell, Eq coefficient, Num coefficient, Semiring coefficient) =>
  GradedDirection ->
  Map HomologicalDegree (GradedOperator cell coefficient) ->
  Either (SheafOperatorBuildError cell) (GradedComplex cell coefficient)
{-# INLINABLE mkGradedComplex #-}
mkGradedComplex direction operators = do
  traverse_ validateDegreeKey (Map.toList operators)
  traverse_ validateBasisCompatibilityAt (Map.toList operators)
  traverse_ validateNilpotenceAt (Map.toList operators)
  pure
    GradedComplex
      { gradedComplexDirectionInternal = direction,
        gradedOperatorsByDegreeInternal = operators
      }
  where
    validateDegreeKey ::
      (HomologicalDegree, GradedOperator cell coefficient) ->
      Either (SheafOperatorBuildError cell) ()
    validateDegreeKey (degree, operator) =
      if gradedOperatorDegree operator == degree
        then Right ()
        else
          Left
            (OperatorDifferentialDegreeMismatch degree (gradedOperatorDegree operator))

    validateBasisCompatibilityAt ::
      (HomologicalDegree, GradedOperator cell coefficient) ->
      Either (SheafOperatorBuildError cell) ()
    validateBasisCompatibilityAt (degree, operator) =
      case Map.lookup (nextDegreeFor direction degree) operators of
        Nothing ->
          Right ()
        Just adjacentOperator ->
          if gradedOperatorTargetBasis operator == gradedOperatorSourceBasis adjacentOperator
            then Right ()
            else
              Left
                (OperatorIntermediateBasisMismatch degree (gradedOperatorDegree adjacentOperator))

    validateNilpotenceAt ::
      (HomologicalDegree, GradedOperator cell coefficient) ->
      Either (SheafOperatorBuildError cell) ()
    validateNilpotenceAt (degree, operator) =
      case Map.lookup (nextDegreeFor direction degree) operators of
        Nothing ->
          Right ()
        Just adjacentOperator ->
          case composeAdjacent adjacentOperator operator of
            Left shapeError ->
              Left (OperatorBoundaryShapeError shapeError)
            Right composite ->
              case listToMaybe (boundaryEntries composite) of
                Nothing ->
                  Right ()
                Just witness ->
                  Left
                    ( OperatorNonNilpotent
                        degree
                        (gradedOperatorDegree adjacentOperator)
                        (sourceIndex witness)
                        (targetIndex witness)
                    )

gradedOperatorAt ::
  HomologicalDegree ->
  GradedComplex cell coefficient ->
  Either (SheafOperatorBuildError cell) (GradedOperator cell coefficient)
gradedOperatorAt degree complex =
  case Map.lookup degree (gradedOperatorsByDegree complex) of
    Just operator ->
      Right operator
    Nothing ->
      Left
        (OperatorMissingDifferential degree)

validateLinearIncidenceCardinality ::
  LinearBasis cell ->
  LinearBasis cell ->
  BoundaryIncidence coefficient ->
  Either (SheafOperatorBuildError cell) ()
validateLinearIncidenceCardinality sourceBasis targetBasis incidence =
  if sourceCardinality incidence == linearBasisCardinality sourceBasis
    && targetCardinality incidence == linearBasisCardinality targetBasis
    then Right ()
    else
      Left
        ( OperatorBoundaryShapeError
            ( BoundaryIncidenceBlockShapeMismatch
                (linearBasisCardinality sourceBasis)
                (linearBasisCardinality targetBasis)
                (sourceCardinality incidence)
                (targetCardinality incidence)
            )
        )

composeAdjacent ::
  (Eq coefficient, Num coefficient, Semiring coefficient) =>
  GradedOperator cell coefficient ->
  GradedOperator cell coefficient ->
  Either BoundaryIncidenceShapeError (BoundaryIncidence coefficient)
composeAdjacent adjacentOperator operator =
  composeBoundaryIncidence
    (gradedOperatorIncidence adjacentOperator)
    (gradedOperatorIncidence operator)

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Homology.Pure.Rank.Field
  ( FieldRankBackend (..),
    fieldBettiCapability,
  )
where

import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Kind (Type)
import Data.Maybe (listToMaybe)
import Moonlight.Algebra (Semiring)
import Moonlight.Core (mkCapability)
import Moonlight.Homology.Boundary.Finite
  ( FiniteChainComplex,
    degreeCardinality,
    incidenceMatrixAt,
    maxHomologicalDegree,
    validateFiniteChainComplexShape,
  )
import Moonlight.Homology.Boundary.LinAlg
  ( BoundaryIncidence,
    BoundaryIncidenceShapeError,
    boundaryEntries,
    composeBoundaryIncidence,
  )
import Moonlight.Homology.Pure.Matrix.Reducer
  ( BettiCapability,
    BettiReducer (..),
  )
import Moonlight.Homology.Pure.Degree
  ( HomologicalDegree (..),
  )
import Moonlight.Homology.Pure.Failure
  ( HomologyFailure (..),
    HomologyLaw (..),
  )
import Moonlight.Homology.Pure.Group
  ( HomologyGroup (..),
  )
import Moonlight.Homology.Pure.Phase
  ( RequirePhase2,
  )
import Moonlight.Homology.Pure.Rank.GF2
  ( GF2RankFailure,
    gf2BoundaryRank,
  )
import Moonlight.Homology.Pure.Matrix.SparseLinAlg
  ( SparseRref (..),
    sparseBoundaryMatrixWith,
    sparseRref,
  )
import Moonlight.LinAlg
  ( GF2,
  )

type FieldRankBackend :: Type -> Type
data FieldRankBackend coeff where
  RationalFieldRankBackend :: FieldRankBackend Rational
  GF2FieldRankBackend :: FieldRankBackend GF2

deriving stock instance Eq (FieldRankBackend coeff)

deriving stock instance Show (FieldRankBackend coeff)

type FieldRankFailure :: Type
data FieldRankFailure
  = FieldRankGF2Failed !GF2RankFailure
  deriving stock (Eq, Show)

type FieldHomologyFailure :: Type -> Type
data FieldHomologyFailure coeff
  = FieldHomologyInvalidChainShape !HomologyFailure
  | FieldHomologyBoundaryShapeFailed !HomologicalDegree !BoundaryIncidenceShapeError
  | FieldHomologyNonNilpotent !HomologicalDegree !(BoundaryIncidence coeff)
  | FieldHomologyRankFailed !HomologicalDegree !FieldRankFailure
  | FieldHomologyRankMissing !HomologicalDegree
  | FieldHomologyNegativeDimension !HomologicalDegree !Int !Int !Int
  deriving stock (Eq, Show)

fieldBettiCapability ::
  RequirePhase2 phase =>
  FieldRankBackend coeff ->
  BettiCapability phase coeff
fieldBettiCapability backend =
  mkCapability (fieldBettiReducer backend)
{-# INLINEABLE fieldBettiCapability #-}

fieldBettiReducer ::
  FieldRankBackend coeff ->
  BettiReducer coeff
fieldBettiReducer backend =
  BettiReducer $
    first fieldHomologyFailureToHomologyFailure
      . fieldBettiGroups backend
{-# INLINEABLE fieldBettiReducer #-}

fieldBettiGroups ::
  FieldRankBackend coeff ->
  FiniteChainComplex coeff ->
  Either (FieldHomologyFailure coeff) [HomologyGroup coeff]
fieldBettiGroups backend finite =
  IntMap.elems <$> fieldBettiGroupsByDegree backend finite
{-# INLINEABLE fieldBettiGroups #-}

fieldBettiGroupsByDegree ::
  FieldRankBackend coeff ->
  FiniteChainComplex coeff ->
  Either (FieldHomologyFailure coeff) (IntMap (HomologyGroup coeff))
fieldBettiGroupsByDegree backend finite =
  case backend of
    RationalFieldRankBackend ->
      fieldBettiGroupsWith rationalRank finite
    GF2FieldRankBackend ->
      fieldBettiGroupsWith rankGF2Boundary finite
{-# INLINEABLE fieldBettiGroupsByDegree #-}

fieldBettiGroupsWith ::
  (Eq coeff, Num coeff, Semiring coeff) =>
  (BoundaryIncidence coeff -> Either FieldRankFailure Int) ->
  FiniteChainComplex coeff ->
  Either (FieldHomologyFailure coeff) (IntMap (HomologyGroup coeff))
fieldBettiGroupsWith rankBoundary finite = do
  first FieldHomologyInvalidChainShape $
    validateFiniteChainComplexShape finite
  traverse_ (validateNilpotenceAt finite) (positiveDegrees finite)
  rankByDegree <- rankBoundariesByDegree rankBoundary finite
  IntMap.fromAscList
    <$> traverse (fieldHomologyAt rankByDegree) (degrees finite)
  where
    fieldHomologyAt rankByDegree degreeValue@(HomologicalDegree degreeInt) = do
      let chainDimension =
            degreeCardinality finite degreeValue
      boundaryRank <-
        rankAt rankByDegree degreeValue
      nextBoundaryRank <-
        rankAt rankByDegree (HomologicalDegree (degreeInt + 1))
      let homologyDimension =
            chainDimension - boundaryRank - nextBoundaryRank
      if homologyDimension < 0
        then
          Left
            ( FieldHomologyNegativeDimension
                degreeValue
                chainDimension
                boundaryRank
                nextBoundaryRank
            )
        else
          Right
            ( degreeInt,
              HomologyGroup
                { freeRank = homologyDimension,
                  torsionInvariants = []
                }
            )

rankBoundariesByDegree ::
  (BoundaryIncidence coeff -> Either FieldRankFailure Int) ->
  FiniteChainComplex coeff ->
  Either (FieldHomologyFailure coeff) (IntMap Int)
rankBoundariesByDegree rankBoundary finite =
  IntMap.fromAscList
    <$> traverse rankBoundaryAt (rankDegrees finite)
  where
    rankBoundaryAt degreeValue@(HomologicalDegree degreeInt) =
      fmap
        (\rankValue -> (degreeInt, rankValue))
        ( first (FieldHomologyRankFailed degreeValue) $
            rankBoundary (incidenceMatrixAt finite degreeValue)
        )
{-# INLINEABLE rankBoundariesByDegree #-}

rankAt ::
  IntMap Int ->
  HomologicalDegree ->
  Either (FieldHomologyFailure coeff) Int
rankAt rankByDegree degreeValue@(HomologicalDegree degreeInt) =
  maybe
    (Left (FieldHomologyRankMissing degreeValue))
    Right
    (IntMap.lookup degreeInt rankByDegree)
{-# INLINE rankAt #-}

degrees ::
  FiniteChainComplex coeff ->
  [HomologicalDegree]
degrees finite =
  case maxHomologicalDegree finite of
    HomologicalDegree maxDegreeValue ->
      fmap HomologicalDegree [0 .. maxDegreeValue]
{-# INLINE degrees #-}

rankDegrees ::
  FiniteChainComplex coeff ->
  [HomologicalDegree]
rankDegrees finite =
  case maxHomologicalDegree finite of
    HomologicalDegree maxDegreeValue ->
      fmap HomologicalDegree [0 .. maxDegreeValue + 1]
{-# INLINE rankDegrees #-}

positiveDegrees ::
  FiniteChainComplex coeff ->
  [HomologicalDegree]
positiveDegrees finite =
  case maxHomologicalDegree finite of
    HomologicalDegree maxDegreeValue ->
      fmap HomologicalDegree [1 .. maxDegreeValue]
{-# INLINE positiveDegrees #-}

validateNilpotenceAt ::
  (Eq coeff, Num coeff, Semiring coeff) =>
  FiniteChainComplex coeff ->
  HomologicalDegree ->
  Either (FieldHomologyFailure coeff) ()
validateNilpotenceAt finite degreeValue@(HomologicalDegree degreeInt) =
  case
    composeBoundaryIncidence
      (incidenceMatrixAt finite (HomologicalDegree (degreeInt - 1)))
      (incidenceMatrixAt finite degreeValue)
  of
    Left shapeError ->
      Left (FieldHomologyBoundaryShapeFailed degreeValue shapeError)
    Right composite ->
      case listToMaybe (boundaryEntries composite) of
        Nothing ->
          Right ()
        Just _ ->
          Left (FieldHomologyNonNilpotent degreeValue composite)
{-# INLINEABLE validateNilpotenceAt #-}

rationalRank ::
  BoundaryIncidence Rational ->
  Either FieldRankFailure Int
rationalRank =
  Right
    . length
    . srrefPivots
    . sparseRref
    . sparseBoundaryMatrixWith id
{-# INLINE rationalRank #-}

rankGF2Boundary ::
  BoundaryIncidence GF2 ->
  Either FieldRankFailure Int
rankGF2Boundary =
  first FieldRankGF2Failed . gf2BoundaryRank
{-# INLINEABLE rankGF2Boundary #-}

fieldHomologyFailureToHomologyFailure ::
  FieldHomologyFailure coeff ->
  HomologyFailure
fieldHomologyFailureToHomologyFailure failureValue =
  case failureValue of
    FieldHomologyInvalidChainShape failure ->
      failure
    FieldHomologyBoundaryShapeFailed degreeValue shapeError ->
      InvalidBoundaryIncidence
        ("field homology boundary composition failed at " <> show degreeValue <> ": " <> show shapeError)
    FieldHomologyNonNilpotent _degreeValue _composite ->
      LawViolation ChainNilpotenceLaw
    FieldHomologyRankFailed degreeValue rankFailure ->
      BackendFailure
        ("field homology rank failed at " <> show degreeValue <> ": " <> show rankFailure)
    FieldHomologyRankMissing degreeValue ->
      BackendFailure
        ("field homology rank cache missed degree " <> show degreeValue)
    FieldHomologyNegativeDimension degreeValue chainDimension boundaryRank nextBoundaryRank ->
      InvalidTopologyInput
        ( "field homology dimension went negative at "
            <> show degreeValue
            <> ": chain="
            <> show chainDimension
            <> ", boundaryRank="
            <> show boundaryRank
            <> ", nextBoundaryRank="
            <> show nextBoundaryRank
        )
{-# INLINE fieldHomologyFailureToHomologyFailure #-}

module Moonlight.Sheaf.Obstruction.Cohomological.Types.Witness
  ( WitnessStalk,
    WitnessMismatch (..),
    witnessCoefficients,
    witnessSingleton,
    witnessUnion,
    witnessNegate,
    witnessSubtract,
    witnessIsZero,
    witnessMagnitude,
    witnessStalkAlgebra,
    TupleWitnessBasis (..),
    TupleWitnessStalk,
    TupleWitnessMismatch,
    tupleWitnessFromDescent,
    edgeTupleWitnessFromDescent,
    AssignmentWitnessBasis (..),
    AssignmentWitnessStalk,
    AssignmentWitnessMismatch,
    assignmentWitnessFromDescent,
    edgeAssignmentWitnessFromDescent,
  )
where

import Data.Kind (Type)
import Data.IntMap.Strict qualified as IntMap
import Data.List (elemIndex)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Moonlight.Core (pairwise)
import Moonlight.Sheaf.Descent.Assignment qualified as AssignmentDescent
import Moonlight.Sheaf.Descent.Quotient qualified as QuotientDescent
import Moonlight.Sheaf.Obstruction.Cohomological.Types.Provenance (OrientedNerveEdge (..))
import Moonlight.Sheaf.Section.Stalk (StalkAlgebra (..), StalkRestrictionKernel (..))

normalizeWitnessCoefficients :: Map basis Integer -> Map basis Integer
normalizeWitnessCoefficients =
  Map.filter (/= 0)

witnessSingletonCoefficients :: basis -> Integer -> Map basis Integer
witnessSingletonCoefficients basisValue coefficient
  | coefficient == 0 = Map.empty
  | otherwise = Map.singleton basisValue coefficient

unionWitnessCoefficients ::
  Ord basis =>
  Map basis Integer ->
  Map basis Integer ->
  Map basis Integer
unionWitnessCoefficients leftValues rightValues =
  normalizeWitnessCoefficients
    (Map.unionWith (+) leftValues rightValues)

negateWitnessCoefficients :: Map basis Integer -> Map basis Integer
negateWitnessCoefficients =
  normalizeWitnessCoefficients . fmap negate

witnessMismatches ::
  Ord basis =>
  (basis -> Integer -> Integer -> mismatch) ->
  Map basis Integer ->
  Map basis Integer ->
  [mismatch]
witnessMismatches mkMismatch expectedValues actualValues =
  mapMaybe mismatchForBasis $
    Set.toAscList (Map.keysSet expectedValues `Set.union` Map.keysSet actualValues)
  where
    mismatchForBasis basisValue =
      let expectedCoefficient = Map.findWithDefault 0 basisValue expectedValues
          actualCoefficient = Map.findWithDefault 0 basisValue actualValues
       in if expectedCoefficient == actualCoefficient
            then Nothing
            else Just (mkMismatch basisValue expectedCoefficient actualCoefficient)

witnessCoefficientMagnitude :: Map basis Integer -> Int
witnessCoefficientMagnitude basisValues =
  fromIntegral (sum (fmap abs (Map.elems basisValues)))

type WitnessStalk :: Type -> Type
newtype WitnessStalk basis = WitnessStalk
  { unWitnessStalk :: Map basis Integer
  }
  deriving stock (Eq, Ord, Show)

type WitnessMismatch :: Type -> Type
data WitnessMismatch basis = WitnessMismatch
  { wmBasis :: !basis,
    wmExpected :: !Integer,
    wmActual :: !Integer
  }
  deriving stock (Eq, Ord, Show)

witnessCoefficients :: WitnessStalk basis -> Map basis Integer
witnessCoefficients =
  unWitnessStalk
{-# INLINE witnessCoefficients #-}

witnessSingleton :: basis -> Integer -> WitnessStalk basis
witnessSingleton basisValue coefficient =
  WitnessStalk (witnessSingletonCoefficients basisValue coefficient)

witnessUnion ::
  Ord basis =>
  WitnessStalk basis ->
  WitnessStalk basis ->
  WitnessStalk basis
witnessUnion (WitnessStalk leftValues) (WitnessStalk rightValues) =
  WitnessStalk (unionWitnessCoefficients leftValues rightValues)

witnessNegate :: WitnessStalk basis -> WitnessStalk basis
witnessNegate (WitnessStalk basisValues) =
  WitnessStalk (negateWitnessCoefficients basisValues)

witnessSubtract ::
  Ord basis =>
  WitnessStalk basis ->
  WitnessStalk basis ->
  WitnessStalk basis
witnessSubtract leftValue rightValue =
  witnessUnion leftValue (witnessNegate rightValue)

witnessIsZero :: WitnessStalk basis -> Bool
witnessIsZero =
  Map.null . unWitnessStalk

witnessMagnitude :: WitnessStalk basis -> Int
witnessMagnitude (WitnessStalk basisValues) =
  witnessCoefficientMagnitude basisValues

instance Ord basis => Semigroup (WitnessStalk basis) where
  (<>) = witnessUnion

instance Ord basis => Monoid (WitnessStalk basis) where
  mempty = WitnessStalk Map.empty

witnessStalkAlgebra ::
  Ord basis =>
  StalkAlgebra witness (WitnessStalk basis) (WitnessMismatch basis) ()
witnessStalkAlgebra =
  StalkAlgebra
    { saRestrictionKernel = const StalkRestrictionIdentity,
      saMismatches =
        \(WitnessStalk expectedValues) (WitnessStalk actualValues) ->
          witnessMismatches
            ( \basisValue expectedCoefficient actualCoefficient ->
                WitnessMismatch
                  { wmBasis = basisValue,
                    wmExpected = expectedCoefficient,
                    wmActual = actualCoefficient
                  }
            )
            expectedValues
            actualValues,
      saMerge = \leftValue rightValue -> Right (witnessUnion leftValue rightValue),
      saRepair = const (Left ()),
      saNormalize = id
    }

type TupleWitnessBasis :: Type -> Type -> Type
data TupleWitnessBasis ctx rep = TupleWitnessBasis
  { twbParentContext :: !ctx,
    twbLeftContext :: !ctx,
    twbRightContext :: !ctx,
    twbTupleIndex :: !Int,
    twbLeftRepresentative :: !rep,
    twbRightRepresentative :: !rep
  }
  deriving stock (Eq, Ord, Show)

type TupleWitnessStalk :: Type -> Type -> Type
type TupleWitnessStalk ctx rep = WitnessStalk (TupleWitnessBasis ctx rep)
type TupleWitnessMismatch :: Type -> Type -> Type
type TupleWitnessMismatch ctx rep = WitnessMismatch (TupleWitnessBasis ctx rep)

tupleWitnessFromDescent :: (Ord ctx, Ord rep) => QuotientDescent.QuotientDescentObstruction ctx rep -> TupleWitnessStalk ctx rep
tupleWitnessFromDescent (QuotientDescent.QuotientDescentObstruction doContext doCoverElements doObstructedTuples) =
  foldMap tupleContribution (zip [0 :: Int ..] doObstructedTuples)
  where
    indexedCover =
      zip [0 :: Int ..] doCoverElements

    tupleContribution (tupleIndex, tupleValue) =
      foldMap (pairContribution tupleIndex tupleValue) (pairwise indexedCover)

    pairContribution tupleIndex tupleValue ((leftIndex, leftContext), (rightIndex, rightContext)) =
      case (IntMap.lookup leftIndex tupleValue, IntMap.lookup rightIndex tupleValue) of
        (Just leftRepresentative, Just rightRepresentative)
          | leftRepresentative /= rightRepresentative ->
              let (basisValue, orientation) =
                    canonicalTupleWitnessBasis
                      doContext
                      leftContext
                      rightContext
                      tupleIndex
                      leftRepresentative
                      rightRepresentative
               in witnessSingleton basisValue orientation
        _ ->
          mempty
tupleWitnessFromDescent _lookupObstruction =
  mempty

edgeTupleWitnessFromDescent ::
  (Ord ctx, Ord rep) =>
  OrientedNerveEdge ctx ->
  QuotientDescent.QuotientDescentObstruction ctx rep ->
  TupleWitnessStalk ctx rep
edgeTupleWitnessFromDescent OrientedNerveEdge {oneSourceContext, oneTargetContext} (QuotientDescent.QuotientDescentObstruction doContext doCoverElements doObstructedTuples) =
  case (elemIndex oneSourceContext doCoverElements, elemIndex oneTargetContext doCoverElements) of
    (Just sourceIndex, Just targetIndex) ->
      foldMap
        (\(tupleIndex, tupleValue) -> tupleContribution tupleIndex sourceIndex targetIndex tupleValue)
        (zip [0 :: Int ..] doObstructedTuples)
    _ ->
      mempty
  where
    tupleContribution tupleIndex sourceIndex targetIndex tupleValue =
      case (IntMap.lookup sourceIndex tupleValue, IntMap.lookup targetIndex tupleValue) of
        (Just sourceRepresentative, Just targetRepresentative)
          | sourceRepresentative /= targetRepresentative ->
              let (basisValue, orientation) =
                    canonicalTupleWitnessBasis
                      doContext
                      oneSourceContext
                      oneTargetContext
                      tupleIndex
                      sourceRepresentative
                      targetRepresentative
               in witnessSingleton basisValue orientation
        _ ->
          mempty
edgeTupleWitnessFromDescent _edge _lookupObstruction =
  mempty

type AssignmentWitnessBasis :: Type -> Type -> Type -> Type
data AssignmentWitnessBasis ctx coord value = AssignmentWitnessBasis
  { awbParentContext :: !ctx,
    awbLeftContext :: !ctx,
    awbRightContext :: !ctx,
    awbTupleIndex :: !Int,
    awbCoordinate :: !coord,
    awbLeftValue :: !value,
    awbRightValue :: !value
  }
  deriving stock (Eq, Ord, Show)

type AssignmentWitnessMismatch :: Type -> Type -> Type -> Type
type AssignmentWitnessMismatch ctx coord value = WitnessMismatch (AssignmentWitnessBasis ctx coord value)
type AssignmentWitnessStalk :: Type -> Type -> Type -> Type
type AssignmentWitnessStalk ctx coord value = WitnessStalk (AssignmentWitnessBasis ctx coord value)

assignmentWitnessFromDescent ::
  (Ord ctx, Ord coord, Ord value) =>
  AssignmentDescent.AssignmentDescentObstruction ctx coord value admissibilityWitness admissibilityCost ->
  AssignmentWitnessStalk ctx coord value
assignmentWitnessFromDescent obstructionValue =
  case AssignmentDescent.descentObstructionConflict obstructionValue of
    Just AssignmentDescent.DescentConflict {AssignmentDescent.doContext = doContext, AssignmentDescent.doCoverElements = doCoverElements, AssignmentDescent.doObstructedAssignments = doObstructedAssignments} ->
      foldMap (tupleContribution doContext doCoverElements) (zip [0 :: Int ..] doObstructedAssignments)
    Nothing ->
      mempty
  where
    indexedCover doCoverElements =
      zip [0 :: Int ..] doCoverElements

    tupleContribution doContext doCoverElements (tupleIndex, assignmentValue) =
      foldMap (pairContribution doContext tupleIndex assignmentValue) (pairwise (indexedCover doCoverElements))

    pairContribution doContext tupleIndex assignmentValue ((_, leftContext), (_, rightContext)) =
      let leftAssignment = Map.findWithDefault Map.empty leftContext assignmentValue
          rightAssignment = Map.findWithDefault Map.empty rightContext assignmentValue
       in foldMap
            (coordinateContribution doContext tupleIndex leftContext rightContext)
            (Map.toAscList (Map.intersectionWith (,) leftAssignment rightAssignment))

    coordinateContribution doContext tupleIndex leftContext rightContext (coordinateValue, (leftValue, rightValue))
      | leftValue /= rightValue =
          let (basisValue, orientation) =
                canonicalAssignmentWitnessBasis
                  doContext
                  leftContext
                  rightContext
                  tupleIndex
                  coordinateValue
                  leftValue
                  rightValue
           in witnessSingleton basisValue orientation
      | otherwise = mempty

edgeAssignmentWitnessFromDescent ::
  (Ord ctx, Ord coord, Ord value) =>
  OrientedNerveEdge ctx ->
  AssignmentDescent.AssignmentDescentObstruction ctx coord value admissibilityWitness admissibilityCost ->
  AssignmentWitnessStalk ctx coord value
edgeAssignmentWitnessFromDescent edge@OrientedNerveEdge {oneSourceContext, oneTargetContext} obstructionValue =
  case AssignmentDescent.descentObstructionConflict obstructionValue of
    Just AssignmentDescent.DescentConflict {AssignmentDescent.doContext = doContext, AssignmentDescent.doCoverElements = doCoverElements, AssignmentDescent.doObstructedAssignments = doObstructedAssignments} ->
      case (elemIndex oneSourceContext doCoverElements, elemIndex oneTargetContext doCoverElements) of
        (Just _, Just _) ->
          foldMap
            (uncurry (tupleContribution doContext edge))
            (zip [0 :: Int ..] doObstructedAssignments)
        _ ->
          mempty
    Nothing ->
      mempty
  where
    tupleContribution doContext OrientedNerveEdge {oneSourceContext, oneTargetContext} tupleIndex assignmentValue =
      let sourceAssignment = Map.findWithDefault Map.empty oneSourceContext assignmentValue
          targetAssignment = Map.findWithDefault Map.empty oneTargetContext assignmentValue
       in foldMap
            (coordinateContribution doContext oneSourceContext oneTargetContext tupleIndex)
            (Map.toAscList (Map.intersectionWith (,) sourceAssignment targetAssignment))

    coordinateContribution doContext oneSourceContext oneTargetContext tupleIndex (coordinateValue, (sourceValue, targetValue))
      | sourceValue /= targetValue =
          let (basisValue, orientation) =
                canonicalAssignmentWitnessBasis
                  doContext
                  oneSourceContext
                  oneTargetContext
                  tupleIndex
                  coordinateValue
                  sourceValue
                  targetValue
           in witnessSingleton basisValue orientation
      | otherwise = mempty

canonicalTupleWitnessBasis ::
  Ord ctx =>
  ctx ->
  ctx ->
  ctx ->
  Int ->
  rep ->
  rep ->
  (TupleWitnessBasis ctx rep, Integer)
canonicalTupleWitnessBasis parentContext leftContext rightContext tupleIndex leftRepresentative rightRepresentative
  | leftContext <= rightContext =
      ( TupleWitnessBasis
          { twbParentContext = parentContext,
            twbLeftContext = leftContext,
            twbRightContext = rightContext,
            twbTupleIndex = tupleIndex,
            twbLeftRepresentative = leftRepresentative,
            twbRightRepresentative = rightRepresentative
          },
        1
      )
  | otherwise =
      ( TupleWitnessBasis
          { twbParentContext = parentContext,
            twbLeftContext = rightContext,
            twbRightContext = leftContext,
            twbTupleIndex = tupleIndex,
            twbLeftRepresentative = rightRepresentative,
            twbRightRepresentative = leftRepresentative
          },
        -1
      )

canonicalAssignmentWitnessBasis ::
  (Ord ctx, Ord value) =>
  ctx ->
  ctx ->
  ctx ->
  Int ->
  coord ->
  value ->
  value ->
  (AssignmentWitnessBasis ctx coord value, Integer)
canonicalAssignmentWitnessBasis parentContext leftContext rightContext tupleIndex coordinateValue leftValue rightValue
  | leftContext < rightContext || (leftContext == rightContext && leftValue <= rightValue) =
      ( AssignmentWitnessBasis
          { awbParentContext = parentContext,
            awbLeftContext = leftContext,
            awbRightContext = rightContext,
            awbTupleIndex = tupleIndex,
            awbCoordinate = coordinateValue,
            awbLeftValue = leftValue,
            awbRightValue = rightValue
          },
        1
      )
  | otherwise =
      ( AssignmentWitnessBasis
          { awbParentContext = parentContext,
            awbLeftContext = rightContext,
            awbRightContext = leftContext,
            awbTupleIndex = tupleIndex,
            awbCoordinate = coordinateValue,
            awbLeftValue = rightValue,
            awbRightValue = leftValue
          },
        -1
      )

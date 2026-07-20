module Moonlight.Constraint.Effect.LawNames
  ( ConstraintLawName (..),
    constraintLawName,
    CommonLawName (..),
    IsLawName (..),
    constructorLawNameWithOverrides,
  )
where

import Data.Kind (Type)
import Moonlight.Core (CommonLawName (..), IsLawName (..), constructorLawNameWithOverrides)

type ConstraintLawName :: Type
data ConstraintLawName
  = CommonLaw CommonLawName
  | NormalizeSemanticPreservation
  | DeMorganAnd
  | DeMorganOr
  | DoubleNegationElimination
  | DistributivityAndOverOr
  | DistributivityOrOverAnd
  | BooleanComplement
  | BooleanExcludedMiddle
  | DPLLSoundness
  | DPLLDecisionProcedure
  | ImplicationSoundness
  | EvaluateHomomorphism
  | CNFPreservesSatisfiability
  | CoFiniteTruthLatticeAbsorptionJoin
  | CoFiniteTruthLatticeAbsorptionMeet
  | CoFiniteTruthComplementInvolution
  | EndoPatchMonoidAssoc
  | EndoPatchMonoidLeftId
  | EndoPatchMonoidRightId
  | EndoPatchActionComposition
  | EndoPatchActionIdentity
  | EndoPatchCanonicalAssignments
  deriving stock (Eq, Ord, Show)

constraintLawName :: ConstraintLawName -> String
constraintLawName lawNameValue =
  case lawNameValue of
    CommonLaw commonLawName -> lawNameText commonLawName
    specificLawName -> constructorLawNameWithOverrides [("DeMorganAnd", "demorgan_and"), ("DeMorganOr", "demorgan_or"), ("CoFiniteTruthLatticeAbsorptionJoin", "cofinite_truth_lattice_absorption_join"), ("CoFiniteTruthLatticeAbsorptionMeet", "cofinite_truth_lattice_absorption_meet"), ("CoFiniteTruthComplementInvolution", "cofinite_truth_complement_involution"), ("EndoPatchMonoidAssoc", "endopatch_monoid_assoc"), ("EndoPatchMonoidLeftId", "endopatch_monoid_left_id"), ("EndoPatchMonoidRightId", "endopatch_monoid_right_id"), ("EndoPatchActionComposition", "endopatch_action_composition"), ("EndoPatchActionIdentity", "endopatch_action_identity"), ("EndoPatchCanonicalAssignments", "endopatch_canonical_assignments")] (show specificLawName)

instance IsLawName ConstraintLawName where
  lawNameText = constraintLawName

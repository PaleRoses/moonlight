{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Moonlight.Sheaf.Obstruction.Cohomological.Types.Fact
  ( RelationFlavor (..),
    ExactConstraint (EqualityConstraint, GuardConstraint, RelationConstraint),
    data FactConstraint,
    data ProvenanceConstraint,
    data ProofConstraint,
    data CapabilityConstraint,
    ObstructionCell
      ( RegionRootCell,
        OccurrenceCell,
        EqualityConstraintCell,
        GuardConstraintCell,
        RelationConstraintCell,
        CycleCell
      ),
    data FactConstraintCell,
    data ProvenanceConstraintCell,
    data ProofConstraintCell,
    data CapabilityConstraintCell,
    ExpandedObstructionCell
      ( ExpandedRootCell,
        ExpandedOccurrenceCell,
        ExpandedEqualityConstraintCell,
        ExpandedGuardConstraintCell,
        ExpandedRelationConstraintCell,
        ExpandedCycleCell
      ),
    data ExpandedFactConstraintCell,
    data ExpandedProvenanceConstraintCell,
    data ExpandedProofConstraintCell,
    data ExpandedCapabilityConstraintCell,
    ExpandedStalk (..),
    expandedStalkAlgebra,
    ExpandedMorphism (..),
    expandedRestriction,
    zeroCellForAnchor,
  )
where

import Data.Kind (Type)
import Data.IntSet (IntSet)
import Moonlight.Sheaf.Obstruction.Cohomological.Types.Core
  ( Anchor (..),
    ConstraintId,
    CycleId,
    ExactLabelCode,
    OccurrenceId,
  )
import Moonlight.Sheaf.Section.Stalk (StalkAlgebra (..), StalkRestrictionKernel (..))

type RelationFlavor :: Type
data RelationFlavor
  = FactFlavor
  | ProvenanceFlavor
  | ProofFlavor
  | CapabilityFlavor
  deriving stock (Eq, Ord, Show, Read)

type ExactConstraint :: Type -> Type
data ExactConstraint anchor
  = EqualityConstraint !ConstraintId !anchor !anchor !IntSet
  | GuardConstraint !ConstraintId !anchor !anchor !IntSet
  | RelationConstraint !RelationFlavor !ConstraintId ![anchor] ![[ExactLabelCode]]
  deriving stock (Eq, Show, Read)

type ObstructionCell :: Type
data ObstructionCell
  = RegionRootCell
  | OccurrenceCell !OccurrenceId
  | EqualityConstraintCell !ConstraintId
  | GuardConstraintCell !ConstraintId
  | RelationConstraintCell !RelationFlavor !ConstraintId
  | CycleCell !CycleId
  deriving stock (Eq, Ord, Show, Read)

type ExpandedObstructionCell :: Type
data ExpandedObstructionCell
  = ExpandedRootCell !ExactLabelCode
  | ExpandedOccurrenceCell !OccurrenceId !ExactLabelCode
  | ExpandedEqualityConstraintCell !ConstraintId !ExactLabelCode
  | ExpandedGuardConstraintCell !ConstraintId !ExactLabelCode
  | ExpandedRelationConstraintCell !RelationFlavor !ConstraintId !ExactLabelCode
  | ExpandedCycleCell !CycleId
  deriving stock (Eq, Ord, Show, Read)

type ExpandedStalk :: Type
newtype ExpandedStalk = ExpandedStalk ()
  deriving stock (Eq, Ord, Show, Read)

expandedStalkAlgebra :: StalkAlgebra witness ExpandedStalk () ()
expandedStalkAlgebra =
  StalkAlgebra
    { saRestrictionKernel = const StalkRestrictionIdentity,
      saMismatches = \_ _ -> [],
      saMerge = \_ _ -> Right (ExpandedStalk ()),
      saRepair = const (Left ()),
      saNormalize = id
    }

type ExpandedMorphism :: Type
data ExpandedMorphism = ExpandedMorphism
  { emSource :: !ExpandedObstructionCell,
    emTarget :: !ExpandedObstructionCell
  }
  deriving stock (Eq, Ord, Show, Read)

expandedRestriction :: ExpandedMorphism -> ExpandedStalk -> ExpandedStalk
expandedRestriction _ =
  id

zeroCellForAnchor ::
  ExactLabelCode ->
  Anchor OccurrenceId ->
  ExactLabelCode ->
  ExpandedObstructionCell
zeroCellForAnchor rootCode anchorValue labelCode =
  case anchorValue of
    RootAnchor -> ExpandedRootCell rootCode
    OccurrenceAnchor occurrenceId -> ExpandedOccurrenceCell occurrenceId labelCode

pattern FactConstraint :: ConstraintId -> [anchor] -> [[ExactLabelCode]] -> ExactConstraint anchor
pattern FactConstraint constraintId anchors supportTuples =
  RelationConstraint FactFlavor constraintId anchors supportTuples

pattern ProvenanceConstraint :: ConstraintId -> [anchor] -> [[ExactLabelCode]] -> ExactConstraint anchor
pattern ProvenanceConstraint constraintId anchors supportTuples =
  RelationConstraint ProvenanceFlavor constraintId anchors supportTuples

pattern ProofConstraint :: ConstraintId -> [anchor] -> [[ExactLabelCode]] -> ExactConstraint anchor
pattern ProofConstraint constraintId anchors supportTuples =
  RelationConstraint ProofFlavor constraintId anchors supportTuples

pattern CapabilityConstraint :: ConstraintId -> [anchor] -> [[ExactLabelCode]] -> ExactConstraint anchor
pattern CapabilityConstraint constraintId anchors supportTuples =
  RelationConstraint CapabilityFlavor constraintId anchors supportTuples

pattern FactConstraintCell :: ConstraintId -> ObstructionCell
pattern FactConstraintCell constraintId =
  RelationConstraintCell FactFlavor constraintId

pattern ProvenanceConstraintCell :: ConstraintId -> ObstructionCell
pattern ProvenanceConstraintCell constraintId =
  RelationConstraintCell ProvenanceFlavor constraintId

pattern ProofConstraintCell :: ConstraintId -> ObstructionCell
pattern ProofConstraintCell constraintId =
  RelationConstraintCell ProofFlavor constraintId

pattern CapabilityConstraintCell :: ConstraintId -> ObstructionCell
pattern CapabilityConstraintCell constraintId =
  RelationConstraintCell CapabilityFlavor constraintId

pattern ExpandedFactConstraintCell :: ConstraintId -> ExactLabelCode -> ExpandedObstructionCell
pattern ExpandedFactConstraintCell constraintId labelCode =
  ExpandedRelationConstraintCell FactFlavor constraintId labelCode

pattern ExpandedProvenanceConstraintCell :: ConstraintId -> ExactLabelCode -> ExpandedObstructionCell
pattern ExpandedProvenanceConstraintCell constraintId labelCode =
  ExpandedRelationConstraintCell ProvenanceFlavor constraintId labelCode

pattern ExpandedProofConstraintCell :: ConstraintId -> ExactLabelCode -> ExpandedObstructionCell
pattern ExpandedProofConstraintCell constraintId labelCode =
  ExpandedRelationConstraintCell ProofFlavor constraintId labelCode

pattern ExpandedCapabilityConstraintCell :: ConstraintId -> ExactLabelCode -> ExpandedObstructionCell
pattern ExpandedCapabilityConstraintCell constraintId labelCode =
  ExpandedRelationConstraintCell CapabilityFlavor constraintId labelCode

{-# COMPLETE EqualityConstraint, GuardConstraint, FactConstraint, ProvenanceConstraint, ProofConstraint, CapabilityConstraint #-}
{-# COMPLETE RegionRootCell, OccurrenceCell, EqualityConstraintCell, GuardConstraintCell, FactConstraintCell, ProvenanceConstraintCell, ProofConstraintCell, CapabilityConstraintCell, CycleCell #-}
{-# COMPLETE ExpandedRootCell, ExpandedOccurrenceCell, ExpandedEqualityConstraintCell, ExpandedGuardConstraintCell, ExpandedFactConstraintCell, ExpandedProvenanceConstraintCell, ExpandedProofConstraintCell, ExpandedCapabilityConstraintCell, ExpandedCycleCell #-}

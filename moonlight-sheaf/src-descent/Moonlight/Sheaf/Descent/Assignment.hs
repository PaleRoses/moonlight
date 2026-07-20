-- | Assignment descent: compatibility evidence and admissibility over
-- coordinate assignments.
module Moonlight.Sheaf.Descent.Assignment
  ( CompatibilityEvidence (..),
    compatibleEvidence,
    incompatibleEvidence,
    trivialAdmissibility,
    DescentKernel (..),
    DescentConflict (..),
    AssignmentDescentObstruction (..),
    descentObstructionScope,
    descentObstructionConflict,
    DescentReport (..),
    emptyDescentReport,
    descentAt,
    fullDescentCheck,
  )
where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Sheaf.Descent.Core
  ( DescentReport (..),
    emptyDescentReport,
  )
import Moonlight.Sheaf.Descent.Kernel qualified as CoverKernel
import Moonlight.Sheaf.Verdict
  ( SearchVerdict,
  )

type CompatibilityEvidence :: Type -> Type -> Type
data CompatibilityEvidence witness cost = CompatibilityEvidence
  { ceSatisfied :: !Bool,
    ceWitness :: !witness,
    ceCost :: !cost
  }
  deriving stock (Eq, Show)

instance (Monoid witness, Monoid cost) => Semigroup (CompatibilityEvidence witness cost) where
  (<>) = combineEvidence

instance (Monoid witness, Monoid cost) => Monoid (CompatibilityEvidence witness cost) where
  mempty = compatibleEvidence mempty mempty

type DescentKernel :: Type -> Type -> Type -> Type -> Type -> Type -> Type
data DescentKernel c section coord value witness cost = DescentKernel
  { dkCoverOf :: c -> [c],
    dkMaterializedContexts :: ![c],
    dkSectionAt :: c -> section,
    dkAssignmentOf :: section -> Map coord value,
    dkAdmissibility :: c -> section -> c -> section -> CompatibilityEvidence witness cost
  }

type DescentConflict :: Type -> Type -> Type -> Type -> Type -> Type
data DescentConflict c coord value witness cost = DescentConflict
  { doContext :: !c,
    doCoverElements :: ![c],
    doObstructedAssignments :: ![Map c (Map coord value)],
    doParentAdmissibility :: !(Map c (CompatibilityEvidence witness cost)),
    doPairAdmissibility :: !(Map (c, c) (CompatibilityEvidence witness cost))
  }
  deriving stock (Eq, Show)

type AssignmentDescentObstruction :: Type -> Type -> Type -> Type -> Type -> Type
data AssignmentDescentObstruction c coord value witness cost
  = DescentConflictObstruction !(DescentConflict c coord value witness cost)
  | DescentVacuousObstruction !c ![c] !(NonEmpty c)
  deriving stock (Eq, Show)

descentObstructionScope :: AssignmentDescentObstruction c coord value witness cost -> (c, [c])
descentObstructionScope obstructionValue =
  case obstructionValue of
    DescentConflictObstruction conflict ->
      (doContext conflict, doCoverElements conflict)
    DescentVacuousObstruction contextValue coverElements _ ->
      (contextValue, coverElements)

descentObstructionConflict :: AssignmentDescentObstruction c coord value witness cost -> Maybe (DescentConflict c coord value witness cost)
descentObstructionConflict obstructionValue =
  case obstructionValue of
    DescentConflictObstruction conflict ->
      Just conflict
    DescentVacuousObstruction {} ->
      Nothing

compatibleEvidence :: witness -> cost -> CompatibilityEvidence witness cost
compatibleEvidence witnessValue costValue =
  CompatibilityEvidence True witnessValue costValue

incompatibleEvidence :: witness -> cost -> CompatibilityEvidence witness cost
incompatibleEvidence witnessValue costValue =
  CompatibilityEvidence False witnessValue costValue

trivialAdmissibility :: (Monoid witness, Monoid cost) => c -> section -> c -> section -> CompatibilityEvidence witness cost
trivialAdmissibility _ _ _ _ =
  compatibleEvidence mempty mempty

equalityAdmissibility :: (Eq marker, Monoid witness, Monoid cost) => marker -> marker -> CompatibilityEvidence witness cost
equalityAdmissibility leftValue rightValue =
  if leftValue == rightValue
    then compatibleEvidence mempty mempty
    else incompatibleEvidence mempty mempty

descentAt ::
  (Ord c, Ord coord, Eq value, Monoid witness, Monoid cost) =>
  CoverKernel.CoverSearchBudget ->
  DescentKernel c section coord value witness cost ->
  c ->
  SearchVerdict (CoverKernel.CoverSearchRefusal c) (AssignmentDescentObstruction c coord value witness cost)
descentAt budget kernel contextValue =
  CoverKernel.descentAtCover budget (assignmentCoverKernel kernel) contextValue

assignmentCoverKernel :: (Ord c, Ord coord, Eq value, Monoid witness, Monoid cost) => DescentKernel c section coord value witness cost -> CoverKernel.CoverDescentKernel c c section (AssignmentDescentObstruction c coord value witness cost)
assignmentCoverKernel kernel =
  CoverKernel.CoverDescentKernel
    { CoverKernel.cdkMaterializedContexts = dkMaterializedContexts kernel,
      CoverKernel.cdkCoverOf = dkCoverOf kernel,
      CoverKernel.cdkCoordinates =
        const id,
      CoverKernel.cdkDomainAt =
        \_parentContext _coverContexts -> pure . dkSectionAt kernel,
      CoverKernel.cdkCompatible =
        \_parentContext _coverContexts _leftContext _leftSection _rightContext _rightSection -> True,
      CoverKernel.cdkTupleObstructed =
        assignmentObstructed kernel,
      CoverKernel.cdkObstructions =
        \parentContext coverContexts ->
          CoverKernel.obstructionWhenAssignmentsPresent (assignmentObstruction kernel parentContext coverContexts),
      CoverKernel.cdkVacuousObstruction =
        DescentVacuousObstruction
    }

assignmentObstruction :: (Ord c, Ord coord, Eq value, Monoid witness, Monoid cost) => DescentKernel c section coord value witness cost -> c -> [c] -> [Map c section] -> AssignmentDescentObstruction c coord value witness cost
assignmentObstruction kernel parentContext coverContexts obstructedAssignments =
  let parentSection = dkSectionAt kernel parentContext
      coverSectionValues = coverSections kernel coverContexts
      parentAdmissibility =
        Map.fromList
          [ (contextValue, sectionRelationEvidence kernel parentContext parentSection contextValue sectionValue)
          | (contextValue, sectionValue) <- coverSectionValues
          ]
      pairAdmissibility =
        Map.fromList
          [ ( (leftContext, rightContext),
              sectionRelationEvidence kernel leftContext leftSection rightContext rightSection
            )
          | (leftContext, leftSection) <- coverSectionValues,
            (rightContext, rightSection) <- coverSectionValues,
            leftContext < rightContext
          ]
   in DescentConflictObstruction
        DescentConflict
          { doContext = parentContext,
            doCoverElements = coverContexts,
            doObstructedAssignments =
              fmap (fmap (dkAssignmentOf kernel)) obstructedAssignments,
            doParentAdmissibility = parentAdmissibility,
            doPairAdmissibility = pairAdmissibility
          }

fullDescentCheck ::
  (Ord c, Ord coord, Eq value, Monoid witness, Monoid cost) =>
  CoverKernel.CoverSearchBudget ->
  DescentKernel c section coord value witness cost ->
  DescentReport c (CoverKernel.CoverSearchRefusal c) (AssignmentDescentObstruction c coord value witness cost)
fullDescentCheck budget kernel =
  CoverKernel.fullCoverDescentCheck
    budget
    (assignmentCoverKernel kernel)

assignmentObstructed :: (Ord c, Ord coord, Eq value, Monoid witness, Monoid cost) => DescentKernel c section coord value witness cost -> c -> [c] -> Map c section -> Bool
assignmentObstructed kernel parentContext coverContexts assignment =
  let parentSection = dkSectionAt kernel parentContext
      coverSectionValues = assignedCoverSections assignment coverContexts
      parentEvidence =
        [ sectionRelationEvidence kernel parentContext parentSection coverContext coverSection
        | (coverContext, coverSection) <- coverSectionValues
        ]
      pairEvidence =
        [ sectionRelationEvidence kernel leftContext leftSection rightContext rightSection
        | (leftContext, leftSection) <- coverSectionValues,
          (rightContext, rightSection) <- coverSectionValues,
          leftContext < rightContext
        ]
   in not (all ceSatisfied (parentEvidence <> pairEvidence))

assignedCoverSections :: Ord c => Map c section -> [c] -> [(c, section)]
assignedCoverSections assignment coverContexts =
  [ (contextValue, sectionValue)
  | contextValue <- coverContexts,
    Just sectionValue <- [Map.lookup contextValue assignment]
  ]

coverSections :: DescentKernel c section coord value witness cost -> [c] -> [(c, section)]
coverSections kernel =
  fmap (\contextValue -> (contextValue, dkSectionAt kernel contextValue))

sectionRelationEvidence :: (Ord coord, Eq value, Monoid witness, Monoid cost) => DescentKernel c section coord value witness cost -> c -> section -> c -> section -> CompatibilityEvidence witness cost
sectionRelationEvidence kernel leftContext leftSection rightContext rightSection =
  combineEvidence
    (assignmentRelationEvidence kernel leftSection rightSection)
    (dkAdmissibility kernel leftContext leftSection rightContext rightSection)

assignmentRelationEvidence :: (Ord coord, Eq value, Monoid witness, Monoid cost) => DescentKernel c section coord value witness cost -> section -> section -> CompatibilityEvidence witness cost
assignmentRelationEvidence kernel leftSection rightSection =
  foldMap id $
    Map.intersectionWith
      equalityAdmissibility
      (dkAssignmentOf kernel leftSection)
      (dkAssignmentOf kernel rightSection)

combineEvidence :: (Monoid witness, Monoid cost) => CompatibilityEvidence witness cost -> CompatibilityEvidence witness cost -> CompatibilityEvidence witness cost
combineEvidence leftEvidence rightEvidence =
  CompatibilityEvidence
    { ceSatisfied = ceSatisfied leftEvidence && ceSatisfied rightEvidence,
      ceWitness = ceWitness leftEvidence <> ceWitness rightEvidence,
      ceCost = ceCost leftEvidence <> ceCost rightEvidence
    }

{-# LANGUAGE ScopedTypeVariables #-}

module Moonlight.Constraint.Pure.WFC.Compile
  ( compileWFCProblem,
    compileWFCPolicyProblem,
    compilePolicyDomains,
    compilePolicyAdjacencyRules,
    normalizedPresenceRegion,
    projectCompiledPolicyError,
    projectCompiledPolicySearchResult,
    satisfyingAssignments,
  )
where

import Data.Kind (Type)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Moonlight.Constraint.Pure.CSP
  ( Arc (..),
    BinaryConstraint (..),
    ConstraintSatisfactionProblem (..),
    Domain,
    domainFilter,
    domainFromList,
    domainToAscList,
  )
import Moonlight.Constraint.Pure.WFC.Types
  ( AdjacencyPolicy (..),
    AdjacencyRule (..),
    CompiledPolicySlot (..),
    CompiledPolicyValue (..),
    DomainPolicy (..),
    PresencePolicy (..),
    SlotId (..),
    WFCError (..),
    WFCPolicyProblem (..),
    WFCProblem (..),
    WFCRule (..),
    WFCSearchResult (..),
    WFCTopology (..),
  )

type CompiledSlotId :: Type -> Type
type CompiledSlotId slot = SlotId (CompiledPolicySlot slot)

compileWFCProblem ::
  WFCProblem slot value ->
  ConstraintSatisfactionProblem (SlotId slot) value
compileWFCProblem problem =
  ConstraintSatisfactionProblem
    { cspDomains = wfcProblemDomains problem,
      cspConstraints = fmap compileAdjacencyRule (wfcProblemAdjacencyRules problem)
    }

compileWFCPolicyProblem ::
  (Ord slot, Ord value) =>
  WFCPolicyProblem slot value ->
  WFCProblem (CompiledPolicySlot slot) (CompiledPolicyValue slot value)
compileWFCPolicyProblem problem =
  let baseDomains = compilePolicyDomains problem
      baseRules = compilePolicyAdjacencyRules problem
      presenceArtifacts =
        compilePresenceArtifacts
          baseDomains
          (presencePolicies (wfcPolicyRules problem))
   in WFCProblem
        { wfcProblemDomains =
            liftBaseDomains baseDomains
              <> compiledPresenceDomains presenceArtifacts,
          wfcProblemAdjacencyRules =
            liftBaseRules baseRules
              <> compiledPresenceRules presenceArtifacts
        }

projectCompiledPolicyError :: WFCError (CompiledPolicySlot slot) -> WFCError slot
projectCompiledPolicyError compiledError =
  case compiledError of
    WFCCSPError _ -> WFCProjectionInvariantViolation
    WFCAssignmentMissingSlot compiledSlotId ->
      projectCompiledAssignmentError WFCAssignmentMissingSlot compiledSlotId
    WFCAssignmentValueOutsideDomain compiledSlotId ->
      projectCompiledAssignmentError WFCAssignmentValueOutsideDomain compiledSlotId
    WFCProjectionInvariantViolation -> WFCProjectionInvariantViolation

projectCompiledAssignmentError ::
  (SlotId slot -> WFCError slot) ->
  CompiledSlotId slot ->
  WFCError slot
projectCompiledAssignmentError assignmentError (SlotId compiledSlot) =
  case compiledSlot of
    CompiledBaseSlot slot -> assignmentError (SlotId slot)
    CompiledPresenceWitnessSlot _ -> WFCProjectionInvariantViolation

projectCompiledPolicySearchResult ::
  Ord slot =>
  WFCSearchResult (CompiledPolicySlot slot) (CompiledPolicyValue slot value) ->
  Either (WFCError slot) (WFCSearchResult slot value)
projectCompiledPolicySearchResult searchResult =
  case searchResult of
    WFCSolved assignment ->
      fmap WFCSolved (projectCompiledPolicyAssignment assignment)
    WFCUnsatisfiable -> pure WFCUnsatisfiable
    WFCBacktrackLimitReached -> pure WFCBacktrackLimitReached

projectCompiledPolicyAssignment ::
  Ord slot =>
  Map.Map (CompiledSlotId slot) (CompiledPolicyValue slot value) ->
  Either (WFCError slot) (Map.Map (SlotId slot) value)
projectCompiledPolicyAssignment assignment =
  fmap Map.fromList $
    foldr collect (Right []) (Map.toAscList assignment)
  where
    collect ::
      (CompiledSlotId slot, CompiledPolicyValue slot value) ->
      Either (WFCError slot) [(SlotId slot, value)] ->
      Either (WFCError slot) [(SlotId slot, value)]
    collect entry accumulated =
      case accumulated of
        Left err -> Left err
        Right pairs ->
          case entry of
            (SlotId compiledSlot, compiledValue) ->
              case (compiledSlot, compiledValue) of
                (CompiledBaseSlot slot, CompiledBaseValue value) ->
                  Right ((SlotId slot, value) : pairs)
                (CompiledPresenceWitnessSlot _, CompiledPresenceWitnessValue _) ->
                  Right pairs
                _ ->
                  Left WFCProjectionInvariantViolation

type PresenceArtifacts :: Type -> Type -> Type
data PresenceArtifacts slot value = PresenceArtifacts
  { compiledPresenceDomains :: Map.Map (CompiledSlotId slot) (Domain (CompiledPolicyValue slot value)),
    compiledPresenceRules :: [AdjacencyRule (CompiledPolicySlot slot) (CompiledPolicyValue slot value)]
  }

emptyPresenceArtifacts :: PresenceArtifacts slot value
emptyPresenceArtifacts =
  PresenceArtifacts
    { compiledPresenceDomains = Map.empty,
      compiledPresenceRules = []
    }

compilePresenceArtifacts ::
  (Ord slot, Ord value) =>
  Map.Map (SlotId slot) (Domain value) ->
  [PresencePolicy slot value] ->
  PresenceArtifacts slot value
compilePresenceArtifacts baseDomains =
  foldr accumulate emptyPresenceArtifacts . zip [0 ..]
  where
    accumulate (witnessIndex, policy) accumulated =
      let witnessSlotId = SlotId (CompiledPresenceWitnessSlot witnessIndex)
          witnessDomain =
            domainFromList
              . fmap CompiledPresenceWitnessValue
              . satisfyingAssignments baseDomains
              $ policy
          witnessRules =
            fmap
              (compilePresenceRule witnessSlotId)
              (normalizedPresenceRegion policy)
       in PresenceArtifacts
            { compiledPresenceDomains =
                Map.insert witnessSlotId witnessDomain (compiledPresenceDomains accumulated),
              compiledPresenceRules =
                witnessRules <> compiledPresenceRules accumulated
            }

compileAdjacencyRule ::
  AdjacencyRule slot value ->
  BinaryConstraint (SlotId slot) value
compileAdjacencyRule rule =
  BinaryConstraint
    { binaryConstraintArc =
        Arc
          (adjacencyRuleSource rule)
          (adjacencyRuleTarget rule),
      binaryConstraintSatisfied = adjacencyRuleCompatible rule
    }

liftBaseDomains ::
  forall slot value.
  (Ord slot, Ord value) =>
  Map.Map (SlotId slot) (Domain value) ->
  Map.Map (CompiledSlotId slot) (Domain (CompiledPolicyValue slot value))
liftBaseDomains =
  Map.fromList . fmap liftEntry . Map.toAscList
  where
    liftEntry ::
      (SlotId slot, Domain value) ->
      (CompiledSlotId slot, Domain (CompiledPolicyValue slot value))
    liftEntry (SlotId slot, domainValue) =
      ( SlotId (CompiledBaseSlot slot),
        domainFromList (fmap CompiledBaseValue (domainToAscList domainValue))
      )

liftBaseRules ::
  [AdjacencyRule slot value] ->
  [AdjacencyRule (CompiledPolicySlot slot) (CompiledPolicyValue slot value)]
liftBaseRules =
  fmap liftBaseRule

liftBaseRule ::
  AdjacencyRule slot value ->
  AdjacencyRule (CompiledPolicySlot slot) (CompiledPolicyValue slot value)
liftBaseRule rule =
  AdjacencyRule
    { adjacencyRuleSource =
        SlotId (CompiledBaseSlot (unSlotId (adjacencyRuleSource rule))),
      adjacencyRuleTarget =
        SlotId (CompiledBaseSlot (unSlotId (adjacencyRuleTarget rule))),
      adjacencyRuleCompatible =
        \sourceValue targetValue ->
          case (sourceValue, targetValue) of
            (CompiledBaseValue sourceBaseValue, CompiledBaseValue targetBaseValue) ->
              adjacencyRuleCompatible rule sourceBaseValue targetBaseValue
            _ -> False
    }

compilePolicyDomains ::
  WFCPolicyProblem slot value ->
  Map.Map (SlotId slot) (Domain value)
compilePolicyDomains problem =
  let policies = domainPolicies (wfcPolicyRules problem)
   in Map.mapWithKey (applyPolicies policies) (wfcPolicyDomains problem)

applyPolicies ::
  [DomainPolicy slot value] ->
  SlotId slot ->
  Domain value ->
  Domain value
applyPolicies policies slotId =
  domainFilter (\value -> all (\policy -> applyDomainPolicy policy slotId value) policies)

compilePolicyAdjacencyRules ::
  Ord slot =>
  WFCPolicyProblem slot value ->
  [AdjacencyRule slot value]
compilePolicyAdjacencyRules problem =
  let rules = wfcPolicyRules problem
      policies = adjacencyPolicies rules
   in topologyRules policies (wfcPolicyTopology problem) <> explicitAdjacencyRules rules

topologyRules ::
  Ord slot =>
  [AdjacencyPolicy slot value] ->
  WFCTopology slot ->
  [AdjacencyRule slot value]
topologyRules policies topology =
  if null policies
    then []
    else
      fmap
        (\(source, target) -> compilePolicyAdjacencyRule policies source target)
        (topologyArcs topology)

compilePolicyAdjacencyRule ::
  [AdjacencyPolicy slot value] ->
  SlotId slot ->
  SlotId slot ->
  AdjacencyRule slot value
compilePolicyAdjacencyRule policies source target =
  AdjacencyRule
    { adjacencyRuleSource = source,
      adjacencyRuleTarget = target,
      adjacencyRuleCompatible =
        \sourceValue targetValue ->
          all
            (\policy -> applyAdjacencyPolicy policy source target sourceValue targetValue)
            policies
    }

topologyArcs :: Ord slot => WFCTopology slot -> [(SlotId slot, SlotId slot)]
topologyArcs topology =
  Map.toAscList (wfcTopologyAdjacency topology)
    >>= ( \(source, targets) ->
            fmap (\target -> (source, target))
              (Set.toAscList (Set.fromList targets))
        )

domainPolicies :: [WFCRule slot value] -> [DomainPolicy slot value]
domainPolicies =
  foldr collect []
  where
    collect :: WFCRule slot value -> [DomainPolicy slot value] -> [DomainPolicy slot value]
    collect rule policies =
      case rule of
        DomainPolicyRule policy -> policy : policies
        _ -> policies

adjacencyPolicies :: [WFCRule slot value] -> [AdjacencyPolicy slot value]
adjacencyPolicies =
  foldr collect []
  where
    collect :: WFCRule slot value -> [AdjacencyPolicy slot value] -> [AdjacencyPolicy slot value]
    collect rule policies =
      case rule of
        AdjacencyPolicyRule policy -> policy : policies
        _ -> policies

presencePolicies :: [WFCRule slot value] -> [PresencePolicy slot value]
presencePolicies =
  foldr collect []
  where
    collect :: WFCRule slot value -> [PresencePolicy slot value] -> [PresencePolicy slot value]
    collect rule policies =
      case rule of
        PresencePolicyRule policy -> policy : policies
        _ -> policies

explicitAdjacencyRules :: [WFCRule slot value] -> [AdjacencyRule slot value]
explicitAdjacencyRules =
  foldr collect []
  where
    collect :: WFCRule slot value -> [AdjacencyRule slot value] -> [AdjacencyRule slot value]
    collect rule rules =
      case rule of
        ExplicitAdjacencyRule explicitRule -> explicitRule : rules
        _ -> rules

normalizedPresenceRegion :: Ord slot => PresencePolicy slot value -> [SlotId slot]
normalizedPresenceRegion =
  Set.toAscList . Set.fromList . presencePolicyRegion

satisfyingAssignments ::
  Ord slot =>
  Map.Map (SlotId slot) (Domain value) ->
  PresencePolicy slot value ->
  [Map.Map (SlotId slot) value]
satisfyingAssignments baseDomains policy =
  let region = normalizedPresenceRegion policy
      assignmentChoices =
        traverse
          ( \slotId ->
              fmap
                (\value -> (slotId, value))
                (maybe [] domainToAscList (Map.lookup slotId baseDomains))
          )
          region
   in filter (any (presencePolicyRequired policy) . Map.elems) (fmap Map.fromList assignmentChoices)

compilePresenceRule ::
  (Ord slot, Eq value) =>
  CompiledSlotId slot ->
  SlotId slot ->
  AdjacencyRule (CompiledPolicySlot slot) (CompiledPolicyValue slot value)
compilePresenceRule witnessSlotId slotId =
  AdjacencyRule
    { adjacencyRuleSource = witnessSlotId,
      adjacencyRuleTarget = SlotId (CompiledBaseSlot (unSlotId slotId)),
      adjacencyRuleCompatible =
        \witnessValue baseValue ->
          case (witnessValue, baseValue) of
            (CompiledPresenceWitnessValue assignment, CompiledBaseValue candidateValue) ->
              Map.lookup slotId assignment == Just candidateValue
            _ -> False
    }

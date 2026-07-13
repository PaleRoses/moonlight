module Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Validate
  ( BiomechanicalAnatomicalBlueprintProgram (..),
    defaultBiomechanicalAnatomicalBlueprintProgram,
    validateBiomechanicalAnatomicalBlueprint,
  )
where

import Data.Kind (Type)
import Data.Set qualified as Set
import Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Core
  ( BiomechanicalAnatomicalBlueprint (..),
    BiomechanicalBlueprintInvariantViolation (..),
    BiomechanicalBoneBlueprint (..),
    BiomechanicalBoneName (..),
    BiomechanicalJointBlueprint (..),
    BiomechanicalJointName (..),
    BiomechanicalStructuralBlueprint (..),
    BiomechanicalStructuralName (..),
  )
import Moonlight.Core
  ( PatternVar
  )

type BiomechanicalAnatomicalBlueprintProgram :: Type
data BiomechanicalAnatomicalBlueprintProgram = BiomechanicalAnatomicalBlueprintProgram
  { babpJointNameForVar :: PatternVar -> BiomechanicalJointName,
    babpBoneNameForJoints :: [Int] -> BiomechanicalJointName -> BiomechanicalJointName -> BiomechanicalBoneName,
    babpStructuralNameForPath :: [Int] -> [BiomechanicalJointName] -> BiomechanicalStructuralName,
    babpEffectorJointForNames :: [BiomechanicalJointName] -> Maybe BiomechanicalJointName
  }

defaultBiomechanicalAnatomicalBlueprintProgram :: BiomechanicalAnatomicalBlueprintProgram
defaultBiomechanicalAnatomicalBlueprintProgram =
  BiomechanicalAnatomicalBlueprintProgram
    { babpJointNameForVar = \patternVar -> BiomechanicalJointName patternVar Nothing,
      babpBoneNameForJoints = \path sourceJoint targetJoint -> BiomechanicalBoneName path Nothing sourceJoint targetJoint,
      babpStructuralNameForPath = \path _ -> BiomechanicalStructuralName path Nothing,
      babpEffectorJointForNames = lastMaybe
    }

validateBiomechanicalAnatomicalBlueprint :: BiomechanicalAnatomicalBlueprint -> [BiomechanicalBlueprintInvariantViolation]
validateBiomechanicalAnatomicalBlueprint anatomicalBlueprint =
  let declaredJointNames = fmap bjbName (babJointBlueprints anatomicalBlueprint)
      declaredJointSet = Set.fromList declaredJointNames
   in duplicateViolations DuplicateBiomechanicalJointName declaredJointNames
        <> duplicateViolations DuplicateBiomechanicalBoneName (fmap bbbName (babBoneBlueprints anatomicalBlueprint))
        <> duplicateViolations DuplicateBiomechanicalStructuralName (fmap bsbName (babStructuralBlueprints anatomicalBlueprint))
        <> foldMap (boneJointViolations declaredJointSet) (babBoneBlueprints anatomicalBlueprint)
        <> foldMap (structuralJointViolations declaredJointSet) (babStructuralBlueprints anatomicalBlueprint)
        <> maybe [] (effectorViolations declaredJointSet) (babEffectorJoint anatomicalBlueprint)

duplicateViolations :: Ord name => (name -> violation) -> [name] -> [violation]
duplicateViolations mkViolation =
  fmap mkViolation . duplicateValues

duplicateValues :: Ord value => [value] -> [value]
duplicateValues values =
  let (_, _, duplicateEntries) =
        foldr
          (\value (seenValues, emittedValues, accumulatedDuplicates) ->
              if Set.member value seenValues
                then
                  if Set.member value emittedValues
                    then (seenValues, emittedValues, accumulatedDuplicates)
                    else (seenValues, Set.insert value emittedValues, value : accumulatedDuplicates)
                else (Set.insert value seenValues, emittedValues, accumulatedDuplicates)
          )
          (Set.empty, Set.empty, [])
          values
   in duplicateEntries

boneJointViolations :: Set.Set BiomechanicalJointName -> BiomechanicalBoneBlueprint -> [BiomechanicalBlueprintInvariantViolation]
boneJointViolations declaredJointSet boneBlueprint =
  foldMap
    (\jointName ->
        if Set.member jointName declaredJointSet
          then []
          else [BoneBlueprintReferencesUnknownJoint (bbbName boneBlueprint) jointName]
    )
    [bbbSourceJoint boneBlueprint, bbbTargetJoint boneBlueprint]

structuralJointViolations :: Set.Set BiomechanicalJointName -> BiomechanicalStructuralBlueprint -> [BiomechanicalBlueprintInvariantViolation]
structuralJointViolations declaredJointSet structuralBlueprint =
  foldMap
    (\jointName ->
        if Set.member jointName declaredJointSet
          then []
          else [StructuralBlueprintReferencesUnknownJoint (bsbName structuralBlueprint) jointName]
    )
    (bsbIncidentJoints structuralBlueprint)

effectorViolations :: Set.Set BiomechanicalJointName -> BiomechanicalJointName -> [BiomechanicalBlueprintInvariantViolation]
effectorViolations declaredJointSet jointName =
  if Set.member jointName declaredJointSet
    then []
    else [EffectorJointMissingFromBlueprint jointName]


lastMaybe :: [a] -> Maybe a
lastMaybe values =
  case values of
    [] ->
      Nothing
    currentValue : remainingValues ->
      case remainingValues of
        [] ->
          Just currentValue
        _ ->
          lastMaybe remainingValues

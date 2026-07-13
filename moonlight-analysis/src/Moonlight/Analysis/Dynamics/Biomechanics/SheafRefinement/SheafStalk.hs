module Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.SheafStalk
  ( BiomechanicalStalk (..),
    BiomechanicalMismatch (..),
    biomechanicalStalkOps,
    anchorPositionOf,
    solvedPositionOf,
  )
where

import Data.Kind (Type)
import Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Core
  ( BiomechanicalBoneEndpoint (..),
    BiomechanicalRestriction (..),
  )
import Moonlight.Analysis.SheafRefinement.Tolerance
  ( averageDouble,
    relClose,
    vecApproxEq,
  )
import Moonlight.LinAlg.Geometry (Vec3, averageVec3)
import Moonlight.Sheaf.Section.Stalk (StalkAlgebra (..), StalkRestrictionKernel (..))

type BiomechanicalStalk :: Type
data BiomechanicalStalk
  = JointBiomechanicalStalk
      Vec3
      Vec3
  | BoneEndpointBiomechanicalStalk
      BiomechanicalBoneEndpoint
      Vec3
      Vec3
  | BoneBiomechanicalStalk
      Vec3
      Vec3
      Vec3
      Vec3
      Double
      Double
      Double
      Double
      Double
  deriving stock (Eq, Show)

type BiomechanicalMismatch :: Type
data BiomechanicalMismatch
  = BiomechanicalShapeMismatch
  | BoneEndpointKindMismatch BiomechanicalBoneEndpoint BiomechanicalBoneEndpoint
  | JointAnchorPositionMismatch Vec3 Vec3
  | JointSolvedPositionMismatch Vec3 Vec3
  | BoneEndpointAnchorMismatch BiomechanicalBoneEndpoint Vec3 Vec3
  | BoneEndpointSolvedMismatch BiomechanicalBoneEndpoint Vec3 Vec3
  | BoneRestLengthMismatch Double Double
  | BoneStiffnessMismatch Double Double
  | BoneCurrentLengthMismatch Double Double
  | BoneStrainMismatch Double Double
  | BoneElasticEnergyMismatch Double Double
  deriving stock (Eq, Show)

biomechanicalStalkOps :: StalkAlgebra BiomechanicalRestriction BiomechanicalStalk BiomechanicalMismatch ()
biomechanicalStalkOps =
  StalkAlgebra
    { saRestrictionKernel = StalkRestrictionMap . restrictToEndpoint . bmrEndpoint,
      saMismatches = biomechanicalStalkMismatches,
      saMerge = \left right -> Right (mergeBiomechanicalStalks left right),
      saRepair = const (Left ()),
      saNormalize = id
    }

restrictToEndpoint :: BiomechanicalBoneEndpoint -> BiomechanicalStalk -> BiomechanicalStalk
restrictToEndpoint endpointValue stalk =
  case stalk of
    JointBiomechanicalStalk anchorPosition solvedPosition ->
      BoneEndpointBiomechanicalStalk endpointValue anchorPosition solvedPosition
    BoneEndpointBiomechanicalStalk _ anchorPosition solvedPosition ->
      BoneEndpointBiomechanicalStalk endpointValue anchorPosition solvedPosition
    BoneBiomechanicalStalk sourceAnchorPosition targetAnchorPosition sourceSolvedPosition targetSolvedPosition _ _ _ _ _ ->
      case endpointValue of
        SourceBiomechanicalBoneEndpoint ->
          BoneEndpointBiomechanicalStalk SourceBiomechanicalBoneEndpoint sourceAnchorPosition sourceSolvedPosition
        TargetBiomechanicalBoneEndpoint ->
          BoneEndpointBiomechanicalStalk TargetBiomechanicalBoneEndpoint targetAnchorPosition targetSolvedPosition

biomechanicalStalkMismatches :: BiomechanicalStalk -> BiomechanicalStalk -> [BiomechanicalMismatch]
biomechanicalStalkMismatches left right =
  case (left, right) of
    (JointBiomechanicalStalk leftAnchor leftSolved, JointBiomechanicalStalk rightAnchor rightSolved) ->
      vecMismatch JointAnchorPositionMismatch leftAnchor rightAnchor
        <> vecMismatch JointSolvedPositionMismatch leftSolved rightSolved
    (BoneEndpointBiomechanicalStalk leftEndpoint leftAnchor leftSolved, BoneEndpointBiomechanicalStalk rightEndpoint rightAnchor rightSolved) ->
      endpointKindMismatch leftEndpoint rightEndpoint
        <> endpointMismatch BoneEndpointAnchorMismatch leftEndpoint leftAnchor rightAnchor
        <> endpointMismatch BoneEndpointSolvedMismatch leftEndpoint leftSolved rightSolved
    (BoneEndpointBiomechanicalStalk endpointValue endpointAnchor endpointSolved, otherStalk) ->
      endpointRestrictionMismatch endpointValue endpointAnchor endpointSolved otherStalk
    (otherStalk, BoneEndpointBiomechanicalStalk endpointValue endpointAnchor endpointSolved) ->
      endpointRestrictionMismatch endpointValue endpointAnchor endpointSolved otherStalk
    (BoneBiomechanicalStalk leftSourceAnchor leftTargetAnchor leftSourceSolved leftTargetSolved leftRestLength leftStiffness leftCurrentLength leftStrain leftElasticEnergy, BoneBiomechanicalStalk rightSourceAnchor rightTargetAnchor rightSourceSolved rightTargetSolved rightRestLength rightStiffness rightCurrentLength rightStrain rightElasticEnergy) ->
      endpointMismatch BoneEndpointAnchorMismatch SourceBiomechanicalBoneEndpoint leftSourceAnchor rightSourceAnchor
        <> endpointMismatch BoneEndpointAnchorMismatch TargetBiomechanicalBoneEndpoint leftTargetAnchor rightTargetAnchor
        <> endpointMismatch BoneEndpointSolvedMismatch SourceBiomechanicalBoneEndpoint leftSourceSolved rightSourceSolved
        <> endpointMismatch BoneEndpointSolvedMismatch TargetBiomechanicalBoneEndpoint leftTargetSolved rightTargetSolved
        <> scalarMismatch BoneRestLengthMismatch leftRestLength rightRestLength
        <> scalarMismatch BoneStiffnessMismatch leftStiffness rightStiffness
        <> scalarMismatch BoneCurrentLengthMismatch leftCurrentLength rightCurrentLength
        <> scalarMismatch BoneStrainMismatch leftStrain rightStrain
        <> scalarMismatch BoneElasticEnergyMismatch leftElasticEnergy rightElasticEnergy
    _ ->
      [BiomechanicalShapeMismatch]

mergeBiomechanicalStalks :: BiomechanicalStalk -> BiomechanicalStalk -> BiomechanicalStalk
mergeBiomechanicalStalks left right =
  case (left, right) of
    (JointBiomechanicalStalk leftAnchor leftSolved, JointBiomechanicalStalk rightAnchor rightSolved) ->
      JointBiomechanicalStalk
        (averageVec3 leftAnchor rightAnchor)
        (averageVec3 leftSolved rightSolved)
    (BoneEndpointBiomechanicalStalk leftEndpoint leftAnchor leftSolved, BoneEndpointBiomechanicalStalk _ rightAnchor rightSolved) ->
      BoneEndpointBiomechanicalStalk
        leftEndpoint
        (averageVec3 leftAnchor rightAnchor)
        (averageVec3 leftSolved rightSolved)
    (BoneBiomechanicalStalk leftSourceAnchor leftTargetAnchor leftSourceSolved leftTargetSolved leftRestLength leftStiffness leftCurrentLength leftStrain leftElasticEnergy, BoneBiomechanicalStalk rightSourceAnchor rightTargetAnchor rightSourceSolved rightTargetSolved rightRestLength rightStiffness rightCurrentLength rightStrain rightElasticEnergy) ->
      BoneBiomechanicalStalk
        (averageVec3 leftSourceAnchor rightSourceAnchor)
        (averageVec3 leftTargetAnchor rightTargetAnchor)
        (averageVec3 leftSourceSolved rightSourceSolved)
        (averageVec3 leftTargetSolved rightTargetSolved)
        (averageDouble leftRestLength rightRestLength)
        (averageDouble leftStiffness rightStiffness)
        (averageDouble leftCurrentLength rightCurrentLength)
        (averageDouble leftStrain rightStrain)
        (averageDouble leftElasticEnergy rightElasticEnergy)
    _ ->
      left

anchorPositionOf :: BiomechanicalStalk -> Vec3
anchorPositionOf stalk =
  case stalk of
    JointBiomechanicalStalk anchorPosition _ ->
      anchorPosition
    BoneEndpointBiomechanicalStalk _ anchorPosition _ ->
      anchorPosition
    BoneBiomechanicalStalk sourceAnchorPosition _ _ _ _ _ _ _ _ ->
      sourceAnchorPosition

solvedPositionOf :: BiomechanicalStalk -> Vec3
solvedPositionOf stalk =
  case stalk of
    JointBiomechanicalStalk _ solvedPosition ->
      solvedPosition
    BoneEndpointBiomechanicalStalk _ _ solvedPosition ->
      solvedPosition
    BoneBiomechanicalStalk _ _ sourceSolvedPosition _ _ _ _ _ _ ->
      sourceSolvedPosition

scalarMismatch :: (Double -> Double -> BiomechanicalMismatch) -> Double -> Double -> [BiomechanicalMismatch]
scalarMismatch mismatchConstructor leftValue rightValue =
  if relClose leftValue rightValue
    then []
    else [mismatchConstructor leftValue rightValue]

vecMismatch :: (Vec3 -> Vec3 -> BiomechanicalMismatch) -> Vec3 -> Vec3 -> [BiomechanicalMismatch]
vecMismatch mismatchConstructor leftValue rightValue =
  if vecApproxEq leftValue rightValue
    then []
    else [mismatchConstructor leftValue rightValue]

endpointMismatch :: (BiomechanicalBoneEndpoint -> Vec3 -> Vec3 -> BiomechanicalMismatch) -> BiomechanicalBoneEndpoint -> Vec3 -> Vec3 -> [BiomechanicalMismatch]
endpointMismatch mismatchConstructor endpointValue leftValue rightValue =
  if vecApproxEq leftValue rightValue
    then []
    else [mismatchConstructor endpointValue leftValue rightValue]

endpointKindMismatch :: BiomechanicalBoneEndpoint -> BiomechanicalBoneEndpoint -> [BiomechanicalMismatch]
endpointKindMismatch leftEndpoint rightEndpoint =
  if leftEndpoint == rightEndpoint
    then []
    else [BoneEndpointKindMismatch leftEndpoint rightEndpoint]

endpointRestrictionMismatch ::
  BiomechanicalBoneEndpoint ->
  Vec3 ->
  Vec3 ->
  BiomechanicalStalk ->
  [BiomechanicalMismatch]
endpointRestrictionMismatch endpointValue endpointAnchor endpointSolved otherStalk =
  case projectBoneEndpoint endpointValue otherStalk of
    Just (otherAnchor, otherSolved) ->
      endpointMismatch BoneEndpointAnchorMismatch endpointValue endpointAnchor otherAnchor
        <> endpointMismatch BoneEndpointSolvedMismatch endpointValue endpointSolved otherSolved
    Nothing ->
      [BiomechanicalShapeMismatch]

projectBoneEndpoint :: BiomechanicalBoneEndpoint -> BiomechanicalStalk -> Maybe (Vec3, Vec3)
projectBoneEndpoint endpointValue stalk =
  case stalk of
    BoneEndpointBiomechanicalStalk endpointTag anchorPosition solvedPosition
      | endpointTag == endpointValue ->
          Just (anchorPosition, solvedPosition)
      | otherwise ->
          Nothing
    BoneBiomechanicalStalk sourceAnchorPosition targetAnchorPosition sourceSolvedPosition targetSolvedPosition _ _ _ _ _ ->
      case endpointValue of
        SourceBiomechanicalBoneEndpoint ->
          Just (sourceAnchorPosition, sourceSolvedPosition)
        TargetBiomechanicalBoneEndpoint ->
          Just (targetAnchorPosition, targetSolvedPosition)
    JointBiomechanicalStalk anchorPosition solvedPosition ->
      Just (anchorPosition, solvedPosition)

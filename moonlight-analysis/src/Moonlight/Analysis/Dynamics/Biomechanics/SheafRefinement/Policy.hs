module Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Policy
  ( MinimumBiomechanicalJointCount (..),
    mkMinimumBiomechanicalJointCount,
    defaultMinimumBiomechanicalJointCount,
    BiomechanicalRoundLimit (..),
    mkBiomechanicalRoundLimit,
    defaultBiomechanicalRoundLimit,
    BiomechanicalTolerance (..),
    mkBiomechanicalTolerance,
    defaultBiomechanicalTolerance,
    BiomechanicalSpectralPolicy (..),
    mkBiomechanicalSpectralPolicy,
    mkBiomechanicalSpectralPolicyDetailed,
    mkBiomechanicalSpectralPolicyExtended,
    defaultBiomechanicalSpectralPolicy,
    BiomechanicalScorePolicy (..),
    BiomechanicalRankDimension (..),
    BiomechanicalLexicographicRankOrder (..),
    mkBiomechanicalLexicographicRankOrder,
    defaultBiomechanicalLexicographicRankOrder,
    BiomechanicalRankPolicy (..),
    defaultBiomechanicalRankPolicy,
    BiomechanicalResidualScoreComponent (..),
    mkBiomechanicalResidualScoreComponent,
    BiomechanicalAnchorFidelityScoreComponent (..),
    mkBiomechanicalAnchorFidelityScoreComponent,
    BiomechanicalElasticStrainScoreComponent (..),
    mkBiomechanicalElasticStrainScoreComponent,
    BiomechanicalStructuralCoherenceScoreComponent (..),
    mkBiomechanicalStructuralCoherenceScoreComponent,
    BiomechanicalVolumetricPreservationScoreComponent (..),
    mkBiomechanicalVolumetricPreservationScoreComponent,
    BiomechanicalSpectralDriftScoreComponent (..),
    mkBiomechanicalSpectralDriftScoreComponent,
    mkBiomechanicalScorePolicy,
    defaultBiomechanicalScorePolicy,
    BiomechanicalSolvePolicy (..),
    BiomechanicalAnchorFidelityEnergy (..),
    mkBiomechanicalAnchorFidelityEnergy,
    BiomechanicalElasticStrainEnergy (..),
    mkBiomechanicalElasticStrainEnergy,
    BiomechanicalStructuralCoherenceEnergy (..),
    mkBiomechanicalStructuralCoherenceEnergy,
    BiomechanicalVolumetricPreservationEnergy (..),
    mkBiomechanicalVolumetricPreservationEnergy,
    mkBiomechanicalSolvePolicy,
    defaultBiomechanicalSolvePolicy,
  )
where

import Data.Kind (Type)
import Data.Set qualified as Set
import Moonlight.LinAlg (SparsePreconditionerFamily (..), defaultSparsePreconditionerFamily)

type MinimumBiomechanicalJointCount :: Type
newtype MinimumBiomechanicalJointCount = MinimumBiomechanicalJointCount
  { unMinimumBiomechanicalJointCount :: Int
  }
  deriving stock (Eq, Show)

type BiomechanicalRoundLimit :: Type
newtype BiomechanicalRoundLimit = BiomechanicalRoundLimit
  { unBiomechanicalRoundLimit :: Int
  }
  deriving stock (Eq, Show)

type BiomechanicalTolerance :: Type
newtype BiomechanicalTolerance = BiomechanicalTolerance
  { unBiomechanicalTolerance :: Double
  }
  deriving stock (Eq, Show)

type BiomechanicalSpectralPolicy :: Type
data BiomechanicalSpectralPolicy = BiomechanicalSpectralPolicy
  { bspModeCount :: Int,
    bspMaxEigenvalueDrift :: Double,
    bspMaxGraphGapDrift :: Double,
    bspMaxGraphSupportDrift :: Int,
    bspElasticNearZeroTolerance :: Double,
    bspMaxElasticConditionEstimate :: Double,
    bspMaxElasticNearZeroModes :: Int,
    bspMinElasticEigenvalue :: Double,
    bspMaxElasticLocalization :: Double
  }
  deriving stock (Eq, Show)

type BiomechanicalScorePolicy :: Type
data BiomechanicalScorePolicy = BiomechanicalScorePolicy
  { bscpResidualComponent :: BiomechanicalResidualScoreComponent,
    bscpAnchorFidelityComponent :: BiomechanicalAnchorFidelityScoreComponent,
    bscpElasticStrainComponent :: BiomechanicalElasticStrainScoreComponent,
    bscpStructuralCoherenceComponent :: BiomechanicalStructuralCoherenceScoreComponent,
    bscpVolumetricPreservationComponent :: BiomechanicalVolumetricPreservationScoreComponent,
    bscpSpectralDriftComponent :: BiomechanicalSpectralDriftScoreComponent
  }
  deriving stock (Eq, Show)

type BiomechanicalRankDimension :: Type
data BiomechanicalRankDimension
  = ResidualRankDimension
  | AnchorFidelityRankDimension
  | ElasticStrainRankDimension
  | StructuralCoherenceRankDimension
  | VolumetricPreservationRankDimension
  | SpectralDriftRankDimension
  deriving stock (Eq, Ord, Show, Enum, Bounded)

type BiomechanicalLexicographicRankOrder :: Type
newtype BiomechanicalLexicographicRankOrder = BiomechanicalLexicographicRankOrder
  { unBiomechanicalLexicographicRankOrder :: [BiomechanicalRankDimension]
  }
  deriving stock (Eq, Show)

type BiomechanicalRankPolicy :: Type
data BiomechanicalRankPolicy
  = TotalBiomechanicalRankPolicy
  | LexicographicBiomechanicalRankPolicy BiomechanicalLexicographicRankOrder
  | ParetoBiomechanicalRankPolicy
  deriving stock (Eq, Show)

type BiomechanicalResidualScoreComponent :: Type
newtype BiomechanicalResidualScoreComponent = BiomechanicalResidualScoreComponent
  { brscWeight :: Double
  }
  deriving stock (Eq, Show)

type BiomechanicalAnchorFidelityScoreComponent :: Type
newtype BiomechanicalAnchorFidelityScoreComponent = BiomechanicalAnchorFidelityScoreComponent
  { bafscWeight :: Double
  }
  deriving stock (Eq, Show)

type BiomechanicalElasticStrainScoreComponent :: Type
newtype BiomechanicalElasticStrainScoreComponent = BiomechanicalElasticStrainScoreComponent
  { bescWeight :: Double
  }
  deriving stock (Eq, Show)

type BiomechanicalStructuralCoherenceScoreComponent :: Type
newtype BiomechanicalStructuralCoherenceScoreComponent = BiomechanicalStructuralCoherenceScoreComponent
  { bscscWeight :: Double
  }
  deriving stock (Eq, Show)

type BiomechanicalVolumetricPreservationScoreComponent :: Type
newtype BiomechanicalVolumetricPreservationScoreComponent = BiomechanicalVolumetricPreservationScoreComponent
  { bvpscWeight :: Double
  }
  deriving stock (Eq, Show)

type BiomechanicalSpectralDriftScoreComponent :: Type
newtype BiomechanicalSpectralDriftScoreComponent = BiomechanicalSpectralDriftScoreComponent
  { bsdscWeight :: Double
  }
  deriving stock (Eq, Show)

type BiomechanicalSolvePolicy :: Type
data BiomechanicalSolvePolicy = BiomechanicalSolvePolicy
  { bslpAnchorFidelity :: BiomechanicalAnchorFidelityEnergy,
    bslpElasticStrain :: BiomechanicalElasticStrainEnergy,
    bslpStructuralCoherence :: BiomechanicalStructuralCoherenceEnergy,
    bslpVolumetricPreservation :: BiomechanicalVolumetricPreservationEnergy,
    bslpRegularizationWeight :: Double,
    bslpPreconditionerFamily :: SparsePreconditionerFamily
  }
  deriving stock (Eq, Show)

type BiomechanicalAnchorFidelityEnergy :: Type
data BiomechanicalAnchorFidelityEnergy = BiomechanicalAnchorFidelityEnergy
  { bafeJointAnchorWeight :: Double,
    bafeEffectorTargetWeight :: Double
  }
  deriving stock (Eq, Show)

type BiomechanicalElasticStrainEnergy :: Type
newtype BiomechanicalElasticStrainEnergy = BiomechanicalElasticStrainEnergy
  { beseBoneWeight :: Double
  }
  deriving stock (Eq, Show)

type BiomechanicalStructuralCoherenceEnergy :: Type
newtype BiomechanicalStructuralCoherenceEnergy = BiomechanicalStructuralCoherenceEnergy
  { bsceStructuralWeight :: Double
  }
  deriving stock (Eq, Show)

type BiomechanicalVolumetricPreservationEnergy :: Type
newtype BiomechanicalVolumetricPreservationEnergy = BiomechanicalVolumetricPreservationEnergy
  { bvpeVolumetricWeight :: Double
  }
  deriving stock (Eq, Show)

mkMinimumBiomechanicalJointCount :: Int -> Maybe MinimumBiomechanicalJointCount
mkMinimumBiomechanicalJointCount jointCount
  | jointCount >= 0 =
      Just (MinimumBiomechanicalJointCount jointCount)
  | otherwise =
      Nothing

defaultMinimumBiomechanicalJointCount :: MinimumBiomechanicalJointCount
defaultMinimumBiomechanicalJointCount = MinimumBiomechanicalJointCount 2

mkBiomechanicalRoundLimit :: Int -> Maybe BiomechanicalRoundLimit
mkBiomechanicalRoundLimit roundLimit
  | roundLimit >= 0 =
      Just (BiomechanicalRoundLimit roundLimit)
  | otherwise =
      Nothing

defaultBiomechanicalRoundLimit :: BiomechanicalRoundLimit
defaultBiomechanicalRoundLimit = BiomechanicalRoundLimit 64

mkBiomechanicalTolerance :: Double -> Maybe BiomechanicalTolerance
mkBiomechanicalTolerance tolerance
  | tolerance >= 0.0 =
      Just (BiomechanicalTolerance tolerance)
  | otherwise =
      Nothing

defaultBiomechanicalTolerance :: BiomechanicalTolerance
defaultBiomechanicalTolerance = BiomechanicalTolerance 1.0e-9

mkBiomechanicalSpectralPolicy :: Int -> Double -> Maybe BiomechanicalSpectralPolicy
mkBiomechanicalSpectralPolicy modeCount maxEigenvalueDrift
  =
  mkBiomechanicalSpectralPolicyDetailed modeCount maxEigenvalueDrift 1.0e-12 1.0e12 0 0.0

mkBiomechanicalSpectralPolicyExtended ::
  Int ->
  Double ->
  Double ->
  Double ->
  Int ->
  Double ->
  Double ->
  Int ->
  Double ->
  Maybe BiomechanicalSpectralPolicy
mkBiomechanicalSpectralPolicyExtended modeCount maxEigenvalueDrift elasticNearZeroTolerance maxElasticConditionEstimate maxElasticNearZeroModes minElasticEigenvalue maxGraphGapDrift maxGraphSupportDrift maxElasticLocalization
  | modeCount >= 0 && maxEigenvalueDrift >= 0.0 =
      if
        all
          (>= 0.0)
          [ elasticNearZeroTolerance,
            maxElasticConditionEstimate,
            minElasticEigenvalue,
            maxGraphGapDrift,
            maxElasticLocalization
          ]
          && maxElasticNearZeroModes >= 0
          && maxGraphSupportDrift >= 0
        then
          Just
            BiomechanicalSpectralPolicy
              { bspModeCount = modeCount,
                bspMaxEigenvalueDrift = maxEigenvalueDrift,
                bspMaxGraphGapDrift = maxGraphGapDrift,
                bspMaxGraphSupportDrift = maxGraphSupportDrift,
                bspElasticNearZeroTolerance = elasticNearZeroTolerance,
                bspMaxElasticConditionEstimate = maxElasticConditionEstimate,
                bspMaxElasticNearZeroModes = maxElasticNearZeroModes,
                bspMinElasticEigenvalue = minElasticEigenvalue,
                bspMaxElasticLocalization = maxElasticLocalization
              }
        else
          Nothing
  | otherwise =
      Nothing

mkBiomechanicalSpectralPolicyDetailed :: Int -> Double -> Double -> Double -> Int -> Double -> Maybe BiomechanicalSpectralPolicy
mkBiomechanicalSpectralPolicyDetailed modeCount maxEigenvalueDrift elasticNearZeroTolerance maxElasticConditionEstimate maxElasticNearZeroModes minElasticEigenvalue
  =
  mkBiomechanicalSpectralPolicyExtended
    modeCount
    maxEigenvalueDrift
    elasticNearZeroTolerance
    maxElasticConditionEstimate
    maxElasticNearZeroModes
    minElasticEigenvalue
    maxEigenvalueDrift
    0
    1.0

defaultBiomechanicalSpectralPolicy :: BiomechanicalSpectralPolicy
defaultBiomechanicalSpectralPolicy =
  BiomechanicalSpectralPolicy
    { bspModeCount = 4,
      bspMaxEigenvalueDrift = 1.0e-6,
      bspMaxGraphGapDrift = 1.0e-6,
      bspMaxGraphSupportDrift = 0,
      bspElasticNearZeroTolerance = 1.0e-12,
      bspMaxElasticConditionEstimate = 1.0e12,
      bspMaxElasticNearZeroModes = 0,
      bspMinElasticEigenvalue = 0.0,
      bspMaxElasticLocalization = 1.0
    }

mkBiomechanicalAnchorFidelityEnergy :: Double -> Double -> Maybe BiomechanicalAnchorFidelityEnergy
mkBiomechanicalAnchorFidelityEnergy jointAnchorWeight effectorTargetWeight
  | all (>= 0.0) [jointAnchorWeight, effectorTargetWeight] =
      Just
        BiomechanicalAnchorFidelityEnergy
          { bafeJointAnchorWeight = jointAnchorWeight,
            bafeEffectorTargetWeight = effectorTargetWeight
          }
  | otherwise =
      Nothing

mkBiomechanicalElasticStrainEnergy :: Double -> Maybe BiomechanicalElasticStrainEnergy
mkBiomechanicalElasticStrainEnergy boneWeight
  | boneWeight >= 0.0 =
      Just (BiomechanicalElasticStrainEnergy boneWeight)
  | otherwise =
      Nothing

mkBiomechanicalStructuralCoherenceEnergy :: Double -> Maybe BiomechanicalStructuralCoherenceEnergy
mkBiomechanicalStructuralCoherenceEnergy structuralWeight
  | structuralWeight >= 0.0 =
      Just (BiomechanicalStructuralCoherenceEnergy structuralWeight)
  | otherwise =
      Nothing

mkBiomechanicalVolumetricPreservationEnergy :: Double -> Maybe BiomechanicalVolumetricPreservationEnergy
mkBiomechanicalVolumetricPreservationEnergy volumetricWeight
  | volumetricWeight >= 0.0 =
      Just (BiomechanicalVolumetricPreservationEnergy volumetricWeight)
  | otherwise =
      Nothing

mkBiomechanicalSolvePolicy :: Double -> Double -> Double -> Double -> Double -> Double -> Maybe BiomechanicalSolvePolicy
mkBiomechanicalSolvePolicy anchorWeight effectorWeight boneWeight structuralWeight volumetricWeight regularizationWeight
  | regularizationWeight >= 0.0 =
      BiomechanicalSolvePolicy
        <$> mkBiomechanicalAnchorFidelityEnergy anchorWeight effectorWeight
        <*> mkBiomechanicalElasticStrainEnergy boneWeight
        <*> mkBiomechanicalStructuralCoherenceEnergy structuralWeight
        <*> mkBiomechanicalVolumetricPreservationEnergy volumetricWeight
        <*> Just regularizationWeight
        <*> Just defaultSparsePreconditionerFamily
  | otherwise =
      Nothing

defaultBiomechanicalSolvePolicy :: BiomechanicalSolvePolicy
defaultBiomechanicalSolvePolicy =
  BiomechanicalSolvePolicy
    { bslpAnchorFidelity =
        BiomechanicalAnchorFidelityEnergy
          { bafeJointAnchorWeight = 1.0,
            bafeEffectorTargetWeight = 4.0
          },
      bslpElasticStrain = BiomechanicalElasticStrainEnergy 1.0,
      bslpStructuralCoherence = BiomechanicalStructuralCoherenceEnergy 1.5,
      bslpVolumetricPreservation = BiomechanicalVolumetricPreservationEnergy 3.0,
      bslpRegularizationWeight = 1.0e-9,
      bslpPreconditionerFamily = defaultSparsePreconditionerFamily
    }

mkBiomechanicalResidualScoreComponent :: Double -> Maybe BiomechanicalResidualScoreComponent
mkBiomechanicalResidualScoreComponent weight
  | weight >= 0.0 =
      Just (BiomechanicalResidualScoreComponent weight)
  | otherwise =
      Nothing

mkBiomechanicalAnchorFidelityScoreComponent :: Double -> Maybe BiomechanicalAnchorFidelityScoreComponent
mkBiomechanicalAnchorFidelityScoreComponent weight
  | weight >= 0.0 =
      Just (BiomechanicalAnchorFidelityScoreComponent weight)
  | otherwise =
      Nothing

mkBiomechanicalElasticStrainScoreComponent :: Double -> Maybe BiomechanicalElasticStrainScoreComponent
mkBiomechanicalElasticStrainScoreComponent weight
  | weight >= 0.0 =
      Just (BiomechanicalElasticStrainScoreComponent weight)
  | otherwise =
      Nothing

mkBiomechanicalStructuralCoherenceScoreComponent :: Double -> Maybe BiomechanicalStructuralCoherenceScoreComponent
mkBiomechanicalStructuralCoherenceScoreComponent weight
  | weight >= 0.0 =
      Just (BiomechanicalStructuralCoherenceScoreComponent weight)
  | otherwise =
      Nothing

mkBiomechanicalVolumetricPreservationScoreComponent :: Double -> Maybe BiomechanicalVolumetricPreservationScoreComponent
mkBiomechanicalVolumetricPreservationScoreComponent weight
  | weight >= 0.0 =
      Just (BiomechanicalVolumetricPreservationScoreComponent weight)
  | otherwise =
      Nothing

mkBiomechanicalSpectralDriftScoreComponent :: Double -> Maybe BiomechanicalSpectralDriftScoreComponent
mkBiomechanicalSpectralDriftScoreComponent weight
  | weight >= 0.0 =
      Just (BiomechanicalSpectralDriftScoreComponent weight)
  | otherwise =
      Nothing

mkBiomechanicalScorePolicy ::
  Double ->
  Double ->
  Double ->
  Double ->
  Double ->
  Double ->
  Maybe BiomechanicalScorePolicy
mkBiomechanicalScorePolicy residualWeight anchorFidelityWeight elasticStrainWeight structuralCoherenceWeight volumetricPreservationWeight spectralDriftWeight =
  BiomechanicalScorePolicy
    <$> mkBiomechanicalResidualScoreComponent residualWeight
    <*> mkBiomechanicalAnchorFidelityScoreComponent anchorFidelityWeight
    <*> mkBiomechanicalElasticStrainScoreComponent elasticStrainWeight
    <*> mkBiomechanicalStructuralCoherenceScoreComponent structuralCoherenceWeight
    <*> mkBiomechanicalVolumetricPreservationScoreComponent volumetricPreservationWeight
    <*> mkBiomechanicalSpectralDriftScoreComponent spectralDriftWeight

defaultBiomechanicalScorePolicy :: BiomechanicalScorePolicy
defaultBiomechanicalScorePolicy =
  BiomechanicalScorePolicy
    { bscpResidualComponent = BiomechanicalResidualScoreComponent 1.0,
      bscpAnchorFidelityComponent = BiomechanicalAnchorFidelityScoreComponent 1.0,
      bscpElasticStrainComponent = BiomechanicalElasticStrainScoreComponent 1.0,
      bscpStructuralCoherenceComponent = BiomechanicalStructuralCoherenceScoreComponent 1.0,
      bscpVolumetricPreservationComponent = BiomechanicalVolumetricPreservationScoreComponent 1.0,
      bscpSpectralDriftComponent = BiomechanicalSpectralDriftScoreComponent 1.0
    }

mkBiomechanicalLexicographicRankOrder :: [BiomechanicalRankDimension] -> Maybe BiomechanicalLexicographicRankOrder
mkBiomechanicalLexicographicRankOrder rankDimensions
  | Set.fromList rankDimensions == Set.fromList [minBound .. maxBound] =
      Just (BiomechanicalLexicographicRankOrder rankDimensions)
  | otherwise =
      Nothing

defaultBiomechanicalLexicographicRankOrder :: BiomechanicalLexicographicRankOrder
defaultBiomechanicalLexicographicRankOrder =
  BiomechanicalLexicographicRankOrder [minBound .. maxBound]

defaultBiomechanicalRankPolicy :: BiomechanicalRankPolicy
defaultBiomechanicalRankPolicy =
  TotalBiomechanicalRankPolicy

module Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Core
  ( BiomechanicalJointName (..),
    BiomechanicalBoneName (..),
    BiomechanicalStructuralName (..),
    BiomechanicalStructuralKind (..),
    BiomechanicalJointBlueprint (..),
    BiomechanicalBoneBlueprint (..),
    BiomechanicalStructuralBlueprint (..),
    BiomechanicalAnatomicalBlueprint (..),
    BiomechanicalBlueprintInvariantViolation (..),
    BiomechanicalCandidateMaterializationFailure (..),
    BiomechanicalSolveFailure (..),
    BiomechanicalJointAnchor (..),
    BiomechanicalBoneConstraint (..),
    mkBiomechanicalBoneConstraint,
    BiomechanicalBoneEndpoint (..),
    BiomechanicalSite (..),
    BiomechanicalBlueprint (..),
    BiomechanicalRestriction (..),
    BiomechanicalEvidence (..),
    BiomechanicalGraphSpectralSignature (..),
    BiomechanicalElasticSpectralSignature (..),
    BiomechanicalSpectralSignature (..),
    BiomechanicalRefinementDetail (..),
    BiomechanicalScore (..),
    BiomechanicalRank (..),
    BiomechanicalBoneArtifact (..),
  )
where

import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Policy
  ( BiomechanicalRoundLimit,
    BiomechanicalSolvePolicy,
    BiomechanicalSpectralPolicy,
    BiomechanicalTolerance,
    MinimumBiomechanicalJointCount,
  )
import Moonlight.Core
  ( MoonlightError,
    PatternVar,
  )
import Moonlight.EGraph.Pure.Types (ClassId)
import Moonlight.Homology (Graph1Skeleton (..))
import Moonlight.LinAlg.Geometry (Vec3)
import Moonlight.Sheaf.Section.Morphism (RestrictionKind)

type BiomechanicalJointName :: Type
data BiomechanicalJointName = BiomechanicalJointName
  { biomechanicalJointPatternVar :: PatternVar,
    biomechanicalJointNameLabel :: Maybe Text
  }
  deriving stock (Eq, Ord, Show)

type BiomechanicalBoneName :: Type
data BiomechanicalBoneName = BiomechanicalBoneName
  { biomechanicalBoneNamePath :: [Int],
    biomechanicalBoneNameLabel :: Maybe Text,
    bbnSourceJoint :: BiomechanicalJointName,
    bbnTargetJoint :: BiomechanicalJointName
  }
  deriving stock (Eq, Ord, Show)

type BiomechanicalStructuralName :: Type
data BiomechanicalStructuralName = BiomechanicalStructuralName
  { biomechanicalStructuralPath :: [Int],
    biomechanicalStructuralNameLabel :: Maybe Text
  }
  deriving stock (Eq, Ord, Show)

type BiomechanicalStructuralKind :: Type
data BiomechanicalStructuralKind
  = StructuralBiomechanicalSiteKind
  | VolumetricBiomechanicalSiteKind
  deriving stock (Eq, Ord, Show)

type BiomechanicalJointBlueprint :: Type
data BiomechanicalJointBlueprint = BiomechanicalJointBlueprint
  { bjbName :: BiomechanicalJointName,
    bjbPatternVar :: PatternVar
  }
  deriving stock (Eq, Ord, Show)

type BiomechanicalBoneBlueprint :: Type
data BiomechanicalBoneBlueprint = BiomechanicalBoneBlueprint
  { bbbName :: BiomechanicalBoneName,
    bbbSourceJoint :: BiomechanicalJointName,
    bbbTargetJoint :: BiomechanicalJointName
  }
  deriving stock (Eq, Ord, Show)

type BiomechanicalStructuralBlueprint :: Type
data BiomechanicalStructuralBlueprint = BiomechanicalStructuralBlueprint
  { bsbName :: BiomechanicalStructuralName,
    bsbKind :: BiomechanicalStructuralKind,
    bsbIncidentJoints :: [BiomechanicalJointName]
  }
  deriving stock (Eq, Show)

type BiomechanicalAnatomicalBlueprint :: Type
data BiomechanicalAnatomicalBlueprint = BiomechanicalAnatomicalBlueprint
  { babJointBlueprints :: [BiomechanicalJointBlueprint],
    babBoneBlueprints :: [BiomechanicalBoneBlueprint],
    babStructuralBlueprints :: [BiomechanicalStructuralBlueprint],
    babEffectorJoint :: Maybe BiomechanicalJointName
  }
  deriving stock (Eq, Show)

type BiomechanicalBlueprintInvariantViolation :: Type
data BiomechanicalBlueprintInvariantViolation
  = DuplicateBiomechanicalJointName BiomechanicalJointName
  | DuplicateBiomechanicalBoneName BiomechanicalBoneName
  | DuplicateBiomechanicalStructuralName BiomechanicalStructuralName
  | BoneBlueprintReferencesUnknownJoint BiomechanicalBoneName BiomechanicalJointName
  | StructuralBlueprintReferencesUnknownJoint BiomechanicalStructuralName BiomechanicalJointName
  | EffectorJointMissingFromBlueprint BiomechanicalJointName
  | MissingPatternContext
  deriving stock (Eq, Show)

type BiomechanicalCandidateMaterializationFailure :: Type
data BiomechanicalCandidateMaterializationFailure
  = MissingBiomechanicalJointBinding BiomechanicalJointName PatternVar
  | MissingBiomechanicalJointAnchor BiomechanicalSite ClassId
  | InvalidBiomechanicalBoneEndpointSite BiomechanicalBoneName BiomechanicalSite
  | MissingBiomechanicalBoneEndpointSite BiomechanicalBoneName BiomechanicalJointName
  | MissingBiomechanicalBoneConstraint BiomechanicalBoneName BiomechanicalSite BiomechanicalSite
  | MissingBiomechanicalStructuralMemberSite BiomechanicalStructuralName BiomechanicalJointName
  | MissingBiomechanicalStructuralMemberAnchor BiomechanicalStructuralName BiomechanicalSite
  | MissingBiomechanicalEffectorSelection
  | MissingBiomechanicalEffectorSite BiomechanicalJointName
  deriving stock (Eq, Show)

type BiomechanicalSolveFailure :: Type
data BiomechanicalSolveFailure
  = MissingBiomechanicalAnchorPosition BiomechanicalSite
  | MissingBiomechanicalSolvedSite BiomechanicalSite
  | MissingBiomechanicalBoneEndpointEvidence BiomechanicalSite
  | MissingBiomechanicalBoneConstraintEvidence BiomechanicalSite
  | MissingBiomechanicalBoneJointStalk BiomechanicalSite BiomechanicalSite
  | BiomechanicalElasticSpectralDecompositionFailure String
  | BiomechanicalElasticSystemAssemblyFailure MoonlightError
  | BiomechanicalSolutionDecodeFailure BiomechanicalSite MoonlightError
  | BiomechanicalElasticSpectralPolicyViolation BiomechanicalSpectralSignature
  | BiomechanicalElasticSystemDidNotConverge
  | BiomechanicalSectionConstructionFailure [BiomechanicalSite] [BiomechanicalSite]
  | UnsatisfiedBiomechanicalRestriction BiomechanicalRestriction
  deriving stock (Eq, Show)

type BiomechanicalJointAnchor :: Type
newtype BiomechanicalJointAnchor = BiomechanicalJointAnchor
  { biomechanicalJointAnchorPosition :: Vec3
  }
  deriving stock (Eq, Show)

type BiomechanicalBoneConstraint :: Type
data BiomechanicalBoneConstraint = BiomechanicalBoneConstraint
  { biomechanicalBoneRestLength :: Double,
    biomechanicalBoneStiffness :: Double
  }
  deriving stock (Eq, Show)

type BiomechanicalBoneEndpoint :: Type
data BiomechanicalBoneEndpoint
  = SourceBiomechanicalBoneEndpoint
  | TargetBiomechanicalBoneEndpoint
  deriving stock (Eq, Ord, Show)

type BiomechanicalSite :: Type
data BiomechanicalSite
  = BiomechanicalJointSite PatternVar ClassId
  | BiomechanicalBoneSite PatternVar PatternVar ClassId ClassId
  | BiomechanicalStructuralSite BiomechanicalStructuralName
  deriving stock (Eq, Ord, Show)


type BiomechanicalBlueprint :: Type
data BiomechanicalBlueprint = BiomechanicalBlueprint
  { bmbMinimumJointCount :: MinimumBiomechanicalJointCount,
    bmbRoundLimit :: BiomechanicalRoundLimit,
    bmbTolerance :: BiomechanicalTolerance,
    bmbSolvePolicy :: BiomechanicalSolvePolicy,
    bmbTarget :: Vec3,
    bmbAnatomicalBlueprint :: BiomechanicalAnatomicalBlueprint,
    bmbInvariantViolations :: [BiomechanicalBlueprintInvariantViolation],
    bmbPatternSkeleton :: Graph1Skeleton,
    bmbPatternGraphSignature :: BiomechanicalGraphSpectralSignature,
    bmbSpectralPolicy :: BiomechanicalSpectralPolicy
  }
  deriving stock (Eq, Show)

type BiomechanicalRestriction :: Type
data BiomechanicalRestriction = BiomechanicalRestriction
  { bmrKind :: RestrictionKind,
    bmrSourceSite :: BiomechanicalSite,
    bmrTargetSite :: BiomechanicalSite,
    bmrEndpoint :: BiomechanicalBoneEndpoint
  }
  deriving stock (Eq, Show)

type BiomechanicalEvidence :: Type
data BiomechanicalEvidence = BiomechanicalEvidence
  { bmeOrderedJointSites :: [BiomechanicalSite],
    bmeOrderedBoneSites :: [BiomechanicalSite],
    bmeOrderedStructuralSites :: [BiomechanicalSite],
    bmeEffectorSite :: BiomechanicalSite,
    bmeBoneEndpointsBySite :: Map BiomechanicalSite (BiomechanicalSite, BiomechanicalSite),
    bmeStructuralMembersBySite :: Map BiomechanicalSite [BiomechanicalSite],
    bmeStructuralKindBySite :: Map BiomechanicalSite BiomechanicalStructuralKind,
    bmeJointAnchorBySite :: Map BiomechanicalSite Vec3,
    bmeStructuralAnchorBySite :: Map BiomechanicalSite Vec3,
    bmeBoneConstraintBySite :: Map BiomechanicalSite BiomechanicalBoneConstraint,
    bmeRestrictions :: [BiomechanicalRestriction],
    bmeGraphSkeleton :: Graph1Skeleton,
    bmeGraphSignature :: BiomechanicalGraphSpectralSignature,
    bmeSpectralDrift :: Double
  }
  deriving stock (Eq, Show)

type BiomechanicalGraphSpectralSignature :: Type
data BiomechanicalGraphSpectralSignature = BiomechanicalGraphSpectralSignature
  { bgssEigenvalues :: [Double],
    bgssSpectralGap :: Double,
    bgssPositiveSupportSizes :: [Int],
    bgssNegativeSupportSizes :: [Int],
    bgssSupportCriticalities :: [Double]
  }
  deriving stock (Eq, Show)

type BiomechanicalElasticSpectralSignature :: Type
data BiomechanicalElasticSpectralSignature = BiomechanicalElasticSpectralSignature
  { bessEigenvalues :: [Double],
    bessSmallestEigenvalue :: Double,
    bessLargestEigenvalue :: Double,
    bessSpectralGap :: Double,
    bessConditionEstimate :: Double,
    bessNearZeroModeCount :: Int,
    bessModeLocalizations :: [Double],
    bessMeanLocalization :: Double,
    bessStructuralModePenalty :: Double,
    bessVolumetricModePenalty :: Double
  }
  deriving stock (Eq, Show)

type BiomechanicalSpectralSignature :: Type
data BiomechanicalSpectralSignature = BiomechanicalSpectralSignature
  { bssGraphSignature :: BiomechanicalGraphSpectralSignature,
    bssElasticSignature :: Maybe BiomechanicalElasticSpectralSignature
  }
  deriving stock (Eq, Show)

type BiomechanicalRefinementDetail :: Type
data BiomechanicalRefinementDetail = BiomechanicalRefinementDetail
  { bmdJointCount :: Int,
    bmdBoneCount :: Int,
    bmdStructuralSiteCount :: Int,
    bmdSatisfiedRestrictionCount :: Int,
    bmdSpectralSignature :: BiomechanicalSpectralSignature,
    bmdSpectralDrift :: Double,
    bmdEndEffectorResidual :: Double,
    bmdAnchorFidelityEnergy :: Double,
    bmdStrainEnergy :: Double,
    bmdStructuralCoherenceEnergy :: Double,
    bmdVolumetricPreservationEnergy :: Double,
    bmdTopologicalEnergy :: Double
  }
  deriving stock (Eq, Show)

type BiomechanicalScore :: Type
data BiomechanicalScore = BiomechanicalScore
  { bmsEndEffectorResidual :: Double,
    bmsAnchorFidelityEnergy :: Double,
    bmsStrainEnergy :: Double,
    bmsStructuralCoherenceEnergy :: Double,
    bmsVolumetricPreservationEnergy :: Double,
    bmsTopologicalEnergy :: Double,
    bmsSpectralDrift :: Double
  }
  deriving stock (Eq, Ord, Show)

type BiomechanicalRank :: Type
data BiomechanicalRank = BiomechanicalRank
  { bmrResidualComponent :: Double,
    bmrAnchorFidelityComponent :: Double,
    bmrElasticStrainComponent :: Double,
    bmrStructuralCoherenceComponent :: Double,
    bmrVolumetricPreservationComponent :: Double,
    bmrSpectralDriftComponent :: Double,
    bmrTotal :: Double
  }
  deriving stock (Eq, Ord, Show)

type BiomechanicalBoneArtifact :: Type
data BiomechanicalBoneArtifact = BiomechanicalBoneArtifact
  { biomechanicalArtifactName :: BiomechanicalBoneName,
    biomechanicalArtifactEdge :: (Int, Int),
    biomechanicalArtifactSite :: BiomechanicalSite,
    biomechanicalArtifactConstraint :: BiomechanicalBoneConstraint
  }

mkBiomechanicalBoneConstraint :: Double -> Double -> Maybe BiomechanicalBoneConstraint
mkBiomechanicalBoneConstraint restLength stiffness
  | restLength >= 0.0 && stiffness >= 0.0 =
      Just
        BiomechanicalBoneConstraint
          { biomechanicalBoneRestLength = restLength,
            biomechanicalBoneStiffness = stiffness
          }
  | otherwise =
      Nothing

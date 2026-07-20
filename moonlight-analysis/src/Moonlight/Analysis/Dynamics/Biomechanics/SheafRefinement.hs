{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement
  ( MinimumBiomechanicalJointCount,
    mkMinimumBiomechanicalJointCount,
    defaultMinimumBiomechanicalJointCount,
    BiomechanicalRoundLimit,
    mkBiomechanicalRoundLimit,
    defaultBiomechanicalRoundLimit,
    BiomechanicalTolerance,
    mkBiomechanicalTolerance,
    defaultBiomechanicalTolerance,
    BiomechanicalSpectralPolicy,
    mkBiomechanicalSpectralPolicy,
    mkBiomechanicalSpectralPolicyDetailed,
    mkBiomechanicalSpectralPolicyExtended,
    defaultBiomechanicalSpectralPolicy,
    mkBiomechanicalAnchorFidelityEnergy,
    BiomechanicalElasticStrainEnergy (..),
    mkBiomechanicalElasticStrainEnergy,
    BiomechanicalStructuralCoherenceEnergy (..),
    mkBiomechanicalStructuralCoherenceEnergy,
    BiomechanicalVolumetricPreservationEnergy (..),
    mkBiomechanicalVolumetricPreservationEnergy,
    BiomechanicalSolvePolicy (..),
    mkBiomechanicalSolvePolicy,
    defaultBiomechanicalSolvePolicy,
    BiomechanicalScorePolicy (..),
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
    BiomechanicalRankDimension (..),
    BiomechanicalLexicographicRankOrder (..),
    mkBiomechanicalLexicographicRankOrder,
    defaultBiomechanicalLexicographicRankOrder,
    BiomechanicalRankPolicy (..),
    defaultBiomechanicalRankPolicy,
    BiomechanicalJointName (..),
    BiomechanicalBoneName (..),
    BiomechanicalStructuralName (..),
    BiomechanicalStructuralKind (..),
    BiomechanicalJointBlueprint (..),
    BiomechanicalBoneBlueprint (..),
    BiomechanicalStructuralBlueprint (..),
    BiomechanicalAnatomicalBlueprint (..),
    BiomechanicalAnatomicalBlueprintProgram (..),
    BiomechanicalBlueprintInvariantViolation (..),
    defaultBiomechanicalAnatomicalBlueprintProgram,
    validateBiomechanicalAnatomicalBlueprint,
    BiomechanicalCandidateMaterializationFailure (..),
    BiomechanicalSolveFailure (..),
    BiomechanicalJointAnchor (..),
    BiomechanicalBoneConstraint (..),
    mkBiomechanicalBoneConstraint,
    BiomechanicalBoneEndpoint (..),
    BiomechanicalSite (..),
    BiomechanicalStalk (..),
    BiomechanicalMismatch (..),
    BiomechanicalBlueprint (..),
    BiomechanicalEvidence (..),
    BiomechanicalGraphSpectralSignature (..),
    BiomechanicalElasticSpectralSignature (..),
    BiomechanicalRefinementDetail (..),
    BiomechanicalScore (..),
    BiomechanicalRank (..),
    BiomechanicalSpectralSignature (..),
    graphSpectralDistance,
    BiomechanicalRefinementModel,
    withBiomechanicalSolvePolicy,
    withBiomechanicalPreconditionerFamily,
    withBiomechanicalRankPolicy,
    mkBiomechanicalRefinementModelWithPoliciesAndAnatomy,
    mkBiomechanicalRefinementModelWithPolicies,
    mkBiomechanicalRefinementModelWithAnatomy,
    mkBiomechanicalRefinementModel,
    materializeBiomechanicalCandidate,
    solveBiomechanicalCandidateDetailed,
    biomechanicalRefinerWithPoliciesAndAnatomy,
    biomechanicalRefinerWithPolicies,
    biomechanicalRefinerWithSolvePolicy,
    biomechanicalRefinerWithAnatomy,
    biomechanicalRefiner,
    prepareBiomechanicalModel,
    refineBiomechanicalCompiledWithMatcher,
  )
where

import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Core (Language)
import Data.Maybe (mapMaybe)
import Moonlight.LinAlg (SparsePreconditionerFamily)
import Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Candidate
  ( BiomechanicalCandidateContext (..),
    anchoredJointPositionMapDetailed,
    buildBoneStalkDetailed,
    collectValidations,
    jointStalkEntryForSiteDetailed,
    biomechanicalRestrictionValue,
    restrictionSatisfied,
    seedToCandidate,
  )
import Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Candidate qualified as BiomechanicalCandidate
  ( materializeBiomechanicalCandidate )
import Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Core
  ( BiomechanicalAnatomicalBlueprint (..),
    BiomechanicalBlueprint (..),
    BiomechanicalCandidateMaterializationFailure (..),
    BiomechanicalBlueprintInvariantViolation (..),
    BiomechanicalBoneBlueprint (..),
    BiomechanicalBoneConstraint (..),
    BiomechanicalBoneEndpoint (..),
    BiomechanicalBoneName (..),
    BiomechanicalElasticSpectralSignature (..),
    BiomechanicalEvidence (..),
    BiomechanicalGraphSpectralSignature (..),
    BiomechanicalJointAnchor (..),
    BiomechanicalJointBlueprint (..),
    BiomechanicalJointName (..),
    BiomechanicalRefinementDetail (..),
    BiomechanicalRank (..),
    BiomechanicalRestriction,
    BiomechanicalScore (..),
    BiomechanicalSite (..),
    BiomechanicalSolveFailure (..),
    BiomechanicalSpectralSignature (..),
    BiomechanicalStructuralBlueprint (..),
    BiomechanicalStructuralKind (..),
    BiomechanicalStructuralName (..),
    mkBiomechanicalBoneConstraint
  )
import Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Policy
  ( BiomechanicalAnchorFidelityScoreComponent (..),
    BiomechanicalElasticStrainEnergy (..),
    BiomechanicalElasticStrainScoreComponent (..),
    BiomechanicalLexicographicRankOrder (..),
    BiomechanicalRankDimension (..),
    BiomechanicalRankPolicy (..),
    BiomechanicalResidualScoreComponent (..),
    BiomechanicalRoundLimit (..),
    BiomechanicalScorePolicy (..),
    BiomechanicalSolvePolicy (..),
    BiomechanicalSpectralDriftScoreComponent (..),
    BiomechanicalSpectralPolicy (..),
    BiomechanicalStructuralCoherenceEnergy (..),
    BiomechanicalStructuralCoherenceScoreComponent (..),
    BiomechanicalTolerance (..),
    BiomechanicalVolumetricPreservationEnergy (..),
    BiomechanicalVolumetricPreservationScoreComponent (..),
    MinimumBiomechanicalJointCount (..),
    defaultBiomechanicalLexicographicRankOrder,
    defaultBiomechanicalRankPolicy,
    defaultBiomechanicalRoundLimit,
    defaultBiomechanicalScorePolicy,
    defaultBiomechanicalSolvePolicy,
    defaultBiomechanicalSpectralPolicy,
    defaultBiomechanicalTolerance,
    defaultMinimumBiomechanicalJointCount,
    mkBiomechanicalAnchorFidelityEnergy,
    mkBiomechanicalAnchorFidelityScoreComponent,
    mkBiomechanicalElasticStrainEnergy,
    mkBiomechanicalElasticStrainScoreComponent,
    mkBiomechanicalLexicographicRankOrder,
    mkBiomechanicalResidualScoreComponent,
    mkBiomechanicalRoundLimit,
    mkBiomechanicalScorePolicy,
    mkBiomechanicalSolvePolicy,
    mkBiomechanicalSpectralDriftScoreComponent,
    mkBiomechanicalSpectralPolicy,
    mkBiomechanicalSpectralPolicyDetailed,
    mkBiomechanicalSpectralPolicyExtended,
    mkBiomechanicalStructuralCoherenceEnergy,
    mkBiomechanicalStructuralCoherenceScoreComponent,
    mkBiomechanicalTolerance,
    mkBiomechanicalVolumetricPreservationEnergy,
    mkBiomechanicalVolumetricPreservationScoreComponent,
    mkMinimumBiomechanicalJointCount
  )
import Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.SheafStalk
  ( BiomechanicalMismatch (..),
    BiomechanicalStalk (..),
    biomechanicalStalkOps
  )
import Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Validate
  ( BiomechanicalAnatomicalBlueprintProgram (..),
    defaultBiomechanicalAnatomicalBlueprintProgram,
    validateBiomechanicalAnatomicalBlueprint
  )
import Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Score
  ( compareBiomechanicalRanks,
    rankBiomechanicalScore,
  )
import Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Skeleton
  ( compileBiomechanicalAnatomicalBlueprint,
    graphSpectralCompatible,
    graphSpectralDistance,
    graphSpectralSignature,
    skeletonFromAnatomicalBlueprint,
    skeletonFromEdgeSupports,
  )
import Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Solve
  ( BiomechanicalElasticSolve (..),
    solveBiomechanicalElasticSystemDetailed,
  )
import Moonlight.Analysis.SheafRefinement
  ( SheafEnergy (..),
    SheafRefinementModel (..),
    SheafRefiner (..),
    SheafSolve (..),
    refineSheafCompiledWithMatcher,
  )
import Moonlight.EGraph.Fuzzy.Core
  ( FuzzyMatch,
    FuzzyRank (..),
    RefinementCandidate (..),
  )
import Moonlight.EGraph.Fuzzy.Refiner (CompiledSeedMatcher)
import Moonlight.Rewrite.System (CompiledGuard)
import Moonlight.Rewrite.Algebra (CompiledPatternQuery, cpqPrimaryPattern)
import Moonlight.Core (Substitution)
import Moonlight.EGraph.Pure.Types (ClassId, EGraph)
import Moonlight.LinAlg.Geometry (Vec3, distanceVec3)
import Moonlight.Sheaf.Section.ObjectIndex (SheafModelVersion (..))
import Moonlight.Sheaf.Section.Model (SheafModel, withPreparedSheafModel)
import Moonlight.Sheaf.Section.Morphism
  ( RestrictionParts (..),
    rKind,
    rSource,
    rTarget,
    rWitness,
  )
import Moonlight.Sheaf.Section.ObjectIndex (mkObjectIndex)
import Moonlight.Sheaf.Section.Store.State
  ( evaluateRestrictionInSection,
    mkTotalSectionStore,
  )
import Moonlight.Sheaf.Section.Store.Types
  ( SectionConstructionError (..),
  )

type BiomechanicalRefinementModel :: Type
data BiomechanicalRefinementModel = BiomechanicalRefinementModel
  { brmMinimumJointCount :: MinimumBiomechanicalJointCount,
    brmRoundLimit :: BiomechanicalRoundLimit,
    brmTolerance :: BiomechanicalTolerance,
    brmSolvePolicy :: BiomechanicalSolvePolicy,
    brmScorePolicy :: BiomechanicalScorePolicy,
    brmRankPolicy :: BiomechanicalRankPolicy,
    brmTarget :: Vec3,
    brmSpectralPolicy :: BiomechanicalSpectralPolicy,
    brmAnatomicalBlueprintProgram :: BiomechanicalAnatomicalBlueprintProgram,
    brmPrecompiledBlueprint :: Maybe BiomechanicalBlueprint,
    brmLookupJointAnchor :: ClassId -> Maybe Vec3,
    brmLookupBoneConstraint :: ClassId -> ClassId -> Maybe BiomechanicalBoneConstraint
  }

mkBiomechanicalRefinementModelWithPolicies ::
  MinimumBiomechanicalJointCount ->
  BiomechanicalRoundLimit ->
  BiomechanicalTolerance ->
  BiomechanicalScorePolicy ->
  BiomechanicalSpectralPolicy ->
  Vec3 ->
  (ClassId -> Maybe Vec3) ->
  (ClassId -> ClassId -> Maybe BiomechanicalBoneConstraint) ->
  BiomechanicalRefinementModel
mkBiomechanicalRefinementModelWithPolicies =
  mkBiomechanicalRefinementModelWithPoliciesAndAnatomy defaultBiomechanicalAnatomicalBlueprintProgram

mkBiomechanicalRefinementModelWithPoliciesAndAnatomy ::
  BiomechanicalAnatomicalBlueprintProgram ->
  MinimumBiomechanicalJointCount ->
  BiomechanicalRoundLimit ->
  BiomechanicalTolerance ->
  BiomechanicalScorePolicy ->
  BiomechanicalSpectralPolicy ->
  Vec3 ->
  (ClassId -> Maybe Vec3) ->
  (ClassId -> ClassId -> Maybe BiomechanicalBoneConstraint) ->
  BiomechanicalRefinementModel
mkBiomechanicalRefinementModelWithPoliciesAndAnatomy anatomicalBlueprintProgram minimumJointCount roundLimit tolerance scorePolicy spectralPolicy target lookupJointAnchor lookupBoneConstraint =
  BiomechanicalRefinementModel
    { brmMinimumJointCount = minimumJointCount,
      brmRoundLimit = roundLimit,
      brmTolerance = tolerance,
      brmSolvePolicy = defaultBiomechanicalSolvePolicy,
      brmScorePolicy = scorePolicy,
      brmRankPolicy = defaultBiomechanicalRankPolicy,
      brmTarget = target,
      brmSpectralPolicy = spectralPolicy,
      brmAnatomicalBlueprintProgram = anatomicalBlueprintProgram,
      brmPrecompiledBlueprint = Nothing,
      brmLookupJointAnchor = lookupJointAnchor,
      brmLookupBoneConstraint = lookupBoneConstraint
    }

mkBiomechanicalRefinementModel ::
  MinimumBiomechanicalJointCount ->
  BiomechanicalRoundLimit ->
  BiomechanicalTolerance ->
  Vec3 ->
  (ClassId -> Maybe Vec3) ->
  (ClassId -> ClassId -> Maybe BiomechanicalBoneConstraint) ->
  BiomechanicalRefinementModel
mkBiomechanicalRefinementModel =
  mkBiomechanicalRefinementModelWithAnatomy defaultBiomechanicalAnatomicalBlueprintProgram

mkBiomechanicalRefinementModelWithAnatomy ::
  BiomechanicalAnatomicalBlueprintProgram ->
  MinimumBiomechanicalJointCount ->
  BiomechanicalRoundLimit ->
  BiomechanicalTolerance ->
  Vec3 ->
  (ClassId -> Maybe Vec3) ->
  (ClassId -> ClassId -> Maybe BiomechanicalBoneConstraint) ->
  BiomechanicalRefinementModel
mkBiomechanicalRefinementModelWithAnatomy anatomicalBlueprintProgram minimumJointCount roundLimit tolerance target lookupJointAnchor lookupBoneConstraint =
  mkBiomechanicalRefinementModelWithPoliciesAndAnatomy
    anatomicalBlueprintProgram
    minimumJointCount
    roundLimit
    tolerance
    defaultBiomechanicalScorePolicy
    defaultBiomechanicalSpectralPolicy
    target
    lookupJointAnchor
    lookupBoneConstraint

withBiomechanicalSolvePolicy :: BiomechanicalSolvePolicy -> BiomechanicalRefinementModel -> BiomechanicalRefinementModel
withBiomechanicalSolvePolicy solvePolicy model =
  model {brmSolvePolicy = solvePolicy}

withBiomechanicalPreconditionerFamily :: SparsePreconditionerFamily -> BiomechanicalRefinementModel -> BiomechanicalRefinementModel
withBiomechanicalPreconditionerFamily preconditionerFamily model =
  model
    { brmSolvePolicy =
        (brmSolvePolicy model)
          { bslpPreconditionerFamily = preconditionerFamily
          }
    }

withBiomechanicalRankPolicy :: BiomechanicalRankPolicy -> BiomechanicalRefinementModel -> BiomechanicalRefinementModel
withBiomechanicalRankPolicy rankPolicy model =
  model {brmRankPolicy = rankPolicy}

candidateContext :: BiomechanicalRefinementModel -> BiomechanicalCandidateContext
candidateContext model =
  BiomechanicalCandidateContext
    { bccLookupJointAnchor = brmLookupJointAnchor model,
      bccLookupBoneConstraint = brmLookupBoneConstraint model
    }

materializeBiomechanicalCandidate ::
  BiomechanicalRefinementModel ->
  BiomechanicalBlueprint ->
  (ClassId, Substitution) ->
  Either [BiomechanicalCandidateMaterializationFailure] (RefinementCandidate BiomechanicalSite BiomechanicalJointAnchor BiomechanicalEvidence)
materializeBiomechanicalCandidate model blueprint =
  BiomechanicalCandidate.materializeBiomechanicalCandidate (candidateContext model) blueprint

biomechanicalRefinerWithPolicies ::
  MinimumBiomechanicalJointCount ->
  BiomechanicalRoundLimit ->
  BiomechanicalTolerance ->
  BiomechanicalScorePolicy ->
  BiomechanicalSpectralPolicy ->
  Vec3 ->
  (ClassId -> Maybe Vec3) ->
  (ClassId -> ClassId -> Maybe BiomechanicalBoneConstraint) ->
  SheafRefiner BiomechanicalRefinementModel
biomechanicalRefinerWithPolicies =
  biomechanicalRefinerWithPoliciesAndAnatomy defaultBiomechanicalAnatomicalBlueprintProgram

biomechanicalRefinerWithPoliciesAndAnatomy ::
  BiomechanicalAnatomicalBlueprintProgram ->
  MinimumBiomechanicalJointCount ->
  BiomechanicalRoundLimit ->
  BiomechanicalTolerance ->
  BiomechanicalScorePolicy ->
  BiomechanicalSpectralPolicy ->
  Vec3 ->
  (ClassId -> Maybe Vec3) ->
  (ClassId -> ClassId -> Maybe BiomechanicalBoneConstraint) ->
  SheafRefiner BiomechanicalRefinementModel
biomechanicalRefinerWithPoliciesAndAnatomy anatomicalBlueprintProgram minimumJointCount roundLimit tolerance scorePolicy spectralPolicy target lookupJointAnchor lookupBoneConstraint =
  SheafRefiner
    ( mkBiomechanicalRefinementModelWithPoliciesAndAnatomy
        anatomicalBlueprintProgram
        minimumJointCount
        roundLimit
        tolerance
        scorePolicy
        spectralPolicy
        target
        lookupJointAnchor
        lookupBoneConstraint
    )

biomechanicalRefinerWithSolvePolicy ::
  BiomechanicalSolvePolicy ->
  MinimumBiomechanicalJointCount ->
  BiomechanicalRoundLimit ->
  BiomechanicalTolerance ->
  Vec3 ->
  (ClassId -> Maybe Vec3) ->
  (ClassId -> ClassId -> Maybe BiomechanicalBoneConstraint) ->
  SheafRefiner BiomechanicalRefinementModel
biomechanicalRefinerWithSolvePolicy solvePolicy minimumJointCount roundLimit tolerance target lookupJointAnchor lookupBoneConstraint =
  SheafRefiner
    ( withBiomechanicalSolvePolicy solvePolicy
        ( mkBiomechanicalRefinementModel
            minimumJointCount
            roundLimit
            tolerance
            target
            lookupJointAnchor
            lookupBoneConstraint
        )
    )

biomechanicalRefiner ::
  MinimumBiomechanicalJointCount ->
  BiomechanicalRoundLimit ->
  BiomechanicalTolerance ->
  Vec3 ->
  (ClassId -> Maybe Vec3) ->
  (ClassId -> ClassId -> Maybe BiomechanicalBoneConstraint) ->
  SheafRefiner BiomechanicalRefinementModel
biomechanicalRefiner =
  biomechanicalRefinerWithAnatomy defaultBiomechanicalAnatomicalBlueprintProgram

biomechanicalRefinerWithAnatomy ::
  BiomechanicalAnatomicalBlueprintProgram ->
  MinimumBiomechanicalJointCount ->
  BiomechanicalRoundLimit ->
  BiomechanicalTolerance ->
  Vec3 ->
  (ClassId -> Maybe Vec3) ->
  (ClassId -> ClassId -> Maybe BiomechanicalBoneConstraint) ->
  SheafRefiner BiomechanicalRefinementModel
biomechanicalRefinerWithAnatomy anatomicalBlueprintProgram minimumJointCount roundLimit tolerance target lookupJointAnchor lookupBoneConstraint =
  biomechanicalRefinerWithPoliciesAndAnatomy
    anatomicalBlueprintProgram
    minimumJointCount
    roundLimit
    tolerance
    defaultBiomechanicalScorePolicy
    defaultBiomechanicalSpectralPolicy
    target
    lookupJointAnchor
    lookupBoneConstraint

instance SheafRefinementModel BiomechanicalRefinementModel where
  type SheafSite BiomechanicalRefinementModel = BiomechanicalSite
  type SheafAnchor BiomechanicalRefinementModel = BiomechanicalJointAnchor
  type SheafEvidence BiomechanicalRefinementModel = BiomechanicalEvidence
  type SheafValue BiomechanicalRefinementModel = BiomechanicalStalk
  type SheafDetail BiomechanicalRefinementModel = BiomechanicalRefinementDetail
  type SheafBlueprint BiomechanicalRefinementModel = BiomechanicalBlueprint
  type SheafScore BiomechanicalRefinementModel = BiomechanicalScore
  type SheafRank BiomechanicalRefinementModel = BiomechanicalRank
  type SheafSeed BiomechanicalRefinementModel = (ClassId, Substitution)

  compileSheafBlueprint model =
    case brmPrecompiledBlueprint model of
      Just precompiled -> precompiled
      Nothing ->
        BiomechanicalBlueprint
          { bmbMinimumJointCount = brmMinimumJointCount model,
            bmbRoundLimit = brmRoundLimit model,
            bmbTolerance = brmTolerance model,
            bmbSolvePolicy = brmSolvePolicy model,
            bmbTarget = brmTarget model,
            bmbAnatomicalBlueprint =
              BiomechanicalAnatomicalBlueprint
                { babJointBlueprints = [],
                  babBoneBlueprints = [],
                  babStructuralBlueprints = [],
                  babEffectorJoint = Nothing
                },
            bmbInvariantViolations = [MissingPatternContext],
            bmbPatternSkeleton = skeletonFromEdgeSupports 0 [],
            bmbPatternGraphSignature =
              BiomechanicalGraphSpectralSignature
                { bgssEigenvalues = [],
                  bgssSpectralGap = 0.0,
                  bgssPositiveSupportSizes = [],
                  bgssNegativeSupportSizes = [],
                  bgssSupportCriticalities = []
                },
            bmbSpectralPolicy = brmSpectralPolicy model
          }

  enumerateSheafCandidates model blueprint seedMatches =
    if null (bmbInvariantViolations blueprint)
      then mapMaybe (seedToCandidate (candidateContext model) blueprint) seedMatches
      else []

  acceptSheafCandidate _ blueprint candidate =
    let evidence = rcEvidence candidate
        anatomicalBlueprint = bmbAnatomicalBlueprint blueprint
        requiredBoneCount = length (babBoneBlueprints anatomicalBlueprint)
        requiredStructuralCount = length (babStructuralBlueprints anatomicalBlueprint)
     in null (bmbInvariantViolations blueprint)
          && length (bmeOrderedJointSites evidence) >= unMinimumBiomechanicalJointCount (bmbMinimumJointCount blueprint)
          && all (`Map.member` rcAnchors candidate) (bmeOrderedJointSites evidence)
          && length (bmeOrderedBoneSites evidence) == requiredBoneCount
          && length (bmeOrderedStructuralSites evidence) == requiredStructuralCount
          && graphSpectralCompatible (bmbSpectralPolicy blueprint) (bmbPatternGraphSignature blueprint) (bmeGraphSignature evidence)

  solveSheafCandidate _ blueprint candidate =
    either (const Nothing) Just (solveBiomechanicalCandidateDetailed blueprint candidate)

  interpretSheafSolve _ _ _ SheafSolve {..} =
    SheafEnergy
      BiomechanicalScore
        { bmsEndEffectorResidual = ssResidual,
          bmsAnchorFidelityEnergy = bmdAnchorFidelityEnergy ssDetail,
          bmsStrainEnergy = bmdStrainEnergy ssDetail,
          bmsStructuralCoherenceEnergy = bmdStructuralCoherenceEnergy ssDetail,
          bmsVolumetricPreservationEnergy = bmdVolumetricPreservationEnergy ssDetail,
          bmsTopologicalEnergy = bmdTopologicalEnergy ssDetail,
          bmsSpectralDrift = bmdSpectralDrift ssDetail
        }

  rankSheafEnergy model (SheafEnergy scoreValue) =
    FuzzyRank (rankBiomechanicalScore (brmScorePolicy model) scoreValue)

  compareSheafRanks model (FuzzyRank leftRank) (FuzzyRank rightRank) =
    compareBiomechanicalRanks (brmRankPolicy model) leftRank rightRank

solveBiomechanicalCandidateDetailed ::
  BiomechanicalBlueprint ->
  RefinementCandidate BiomechanicalSite BiomechanicalJointAnchor BiomechanicalEvidence ->
  Either [BiomechanicalSolveFailure] (SheafSolve BiomechanicalSite BiomechanicalStalk BiomechanicalRefinementDetail)
solveBiomechanicalCandidateDetailed blueprint candidate = do
  let evidence = rcEvidence candidate
  anchoredJointPositions <- anchoredJointPositionMapDetailed (bmeOrderedJointSites evidence) (rcAnchors candidate)
  BiomechanicalElasticSolve solvedSitePositions anchorFidelityEnergyValue elasticStrainEnergyValue structuralCoherenceEnergyValue volumetricPreservationEnergyValue spectralSignature objectiveValue <-
    solveBiomechanicalElasticSystemDetailed blueprint evidence anchoredJointPositions
  effectorSolvedPosition <- lookupSolvedSite (bmeEffectorSite evidence) solvedSitePositions
  jointStalkEntries <- collectValidations (fmap (jointStalkEntryForSiteDetailed anchoredJointPositions solvedSitePositions) (bmeOrderedJointSites evidence))
  let jointStalksBySite = Map.fromList jointStalkEntries
      structuralAnchorPositions = bmeStructuralAnchorBySite evidence
      residualValue = distanceVec3 effectorSolvedPosition (bmbTarget blueprint)
  structuralStalkEntries <- collectValidations (fmap (jointStalkEntryForSiteDetailed structuralAnchorPositions solvedSitePositions) (bmeOrderedStructuralSites evidence))
  resolvedBoneStalkEntries <-
    collectValidations
      ( fmap
          (buildBoneStalkDetailed evidence jointStalksBySite)
          (bmeOrderedBoneSites evidence)
      )
  let structuralStalksBySite = Map.fromList structuralStalkEntries
      resolvedBoneStalks = Map.fromList resolvedBoneStalkEntries
      sectionMap = Map.unions [jointStalksBySite, structuralStalksBySite, resolvedBoneStalks]
  case
    withPreparedSheafModel
      (SheafModelVersion 0)
      (mkObjectIndex (Map.keys sectionMap))
      ( \restriction ->
          RestrictionParts
            { partKind = rKind restriction,
              partSource = rSource restriction,
              partTarget = rTarget restriction,
              partWitness = rWitness restriction
            }
      )
      (fmap biomechanicalRestrictionValue (bmeRestrictions evidence))
      (\sectionModel -> validateSolvedSection sectionModel sectionMap evidence residualValue spectralSignature anchorFidelityEnergyValue elasticStrainEnergyValue structuralCoherenceEnergyValue volumetricPreservationEnergyValue objectiveValue blueprint)
    of
    Left _ ->
      Left (fmap UnsatisfiedBiomechanicalRestriction (bmeRestrictions evidence))
    Right solvedSection ->
      solvedSection

validateSolvedSection
  :: SheafModel owner BiomechanicalSite BiomechanicalRestriction
  -> Map.Map BiomechanicalSite BiomechanicalStalk
  -> BiomechanicalEvidence
  -> Double
  -> BiomechanicalSpectralSignature
  -> Double
  -> Double
  -> Double
  -> Double
  -> Double
  -> BiomechanicalBlueprint
  -> Either [BiomechanicalSolveFailure] (SheafSolve BiomechanicalSite BiomechanicalStalk BiomechanicalRefinementDetail)
validateSolvedSection sectionModel sectionMap evidence residualValue spectralSignature anchorFidelityEnergyValue elasticStrainEnergyValue structuralCoherenceEnergyValue volumetricPreservationEnergyValue objectiveValue blueprint = do
  section <-
    case mkTotalSectionStore sectionModel sectionMap of
      Left constructionError ->
        Left [sectionConstructionFailure constructionError]
      Right sectionValue ->
        Right sectionValue
  let unsatisfiedRestrictions =
        foldr
          ( \restrictionValue failures ->
              if restrictionSatisfied (evaluateRestrictionInSection biomechanicalStalkOps sectionModel section (biomechanicalRestrictionValue restrictionValue))
                then failures
                else UnsatisfiedBiomechanicalRestriction restrictionValue : failures
          )
          []
          (bmeRestrictions evidence)
      satisfiedRestrictionCount = length (bmeRestrictions evidence) - length unsatisfiedRestrictions
  if null unsatisfiedRestrictions
    then
      Right
        SheafSolve
          { ssValueBySite = sectionMap,
            ssResidual = residualValue,
            ssDetail =
              BiomechanicalRefinementDetail
                { bmdJointCount = length (bmeOrderedJointSites evidence),
                  bmdBoneCount = length (bmeOrderedBoneSites evidence),
                  bmdStructuralSiteCount = length (bmeOrderedStructuralSites evidence),
                  bmdSatisfiedRestrictionCount = satisfiedRestrictionCount,
                  bmdSpectralSignature = spectralSignature,
                  bmdSpectralDrift = combinedSpectralDrift (bmbSpectralPolicy blueprint) evidence spectralSignature,
                  bmdEndEffectorResidual = residualValue,
                  bmdAnchorFidelityEnergy = anchorFidelityEnergyValue,
                  bmdStrainEnergy = elasticStrainEnergyValue,
                  bmdStructuralCoherenceEnergy = structuralCoherenceEnergyValue,
                  bmdVolumetricPreservationEnergy = volumetricPreservationEnergyValue,
                  bmdTopologicalEnergy = objectiveValue
                }
          }
    else Left unsatisfiedRestrictions

combinedSpectralDrift ::
  BiomechanicalSpectralPolicy ->
  BiomechanicalEvidence ->
  BiomechanicalSpectralSignature ->
  Double
combinedSpectralDrift spectralPolicy evidence spectralSignature =
  bmeSpectralDrift evidence + elasticSpectralPenalty spectralPolicy spectralSignature

elasticSpectralPenalty ::
  BiomechanicalSpectralPolicy ->
  BiomechanicalSpectralSignature ->
  Double
elasticSpectralPenalty spectralPolicy spectralSignature =
  case bssElasticSignature spectralSignature of
    Nothing ->
      0.0
    Just elasticSignature ->
      let localizationScale = max 1.0e-12 (bspMaxElasticLocalization spectralPolicy)
       in bessMeanLocalization elasticSignature / localizationScale
        + bessStructuralModePenalty elasticSignature
        + bessVolumetricModePenalty elasticSignature
        + max 0.0 (abs (bessSpectralGap elasticSignature))


sectionConstructionFailure :: SectionConstructionError BiomechanicalSite -> BiomechanicalSolveFailure
sectionConstructionFailure constructionError =
  BiomechanicalSectionConstructionFailure
    (Set.toAscList (sceMissingCells constructionError))
    (Set.toAscList (sceExtraCells constructionError))

lookupSolvedSite ::
  BiomechanicalSite ->
  Map.Map BiomechanicalSite Vec3 ->
  Either [BiomechanicalSolveFailure] Vec3
lookupSolvedSite site solvedPositions =
  case Map.lookup site solvedPositions of
    Just solvedPosition ->
      Right solvedPosition
    Nothing ->
      Left [MissingBiomechanicalSolvedSite site]

prepareBiomechanicalModel ::
  Language f =>
  CompiledPatternQuery (CompiledGuard capability f) f ->
  BiomechanicalRefinementModel ->
  BiomechanicalRefinementModel
prepareBiomechanicalModel compiledQuery model =
  let anatomicalBlueprint =
        compileBiomechanicalAnatomicalBlueprint
          (brmAnatomicalBlueprintProgram model)
          (cpqPrimaryPattern compiledQuery)
      invariantViolations = validateBiomechanicalAnatomicalBlueprint anatomicalBlueprint
      patternSkeleton = skeletonFromAnatomicalBlueprint anatomicalBlueprint
   in model
        { brmPrecompiledBlueprint =
            Just
              BiomechanicalBlueprint
                { bmbMinimumJointCount = brmMinimumJointCount model,
                  bmbRoundLimit = brmRoundLimit model,
                  bmbTolerance = brmTolerance model,
                  bmbSolvePolicy = brmSolvePolicy model,
                  bmbTarget = brmTarget model,
                  bmbAnatomicalBlueprint = anatomicalBlueprint,
                  bmbInvariantViolations = invariantViolations,
                  bmbPatternSkeleton = patternSkeleton,
                  bmbPatternGraphSignature = graphSpectralSignature (bspModeCount (brmSpectralPolicy model)) patternSkeleton,
                  bmbSpectralPolicy = brmSpectralPolicy model
                }
        }

refineBiomechanicalCompiledWithMatcher ::
  Language f =>
  CompiledSeedMatcher f ->
  SheafRefiner BiomechanicalRefinementModel ->
  CompiledPatternQuery (CompiledGuard capability f) f ->
  EGraph f a ->
  [FuzzyMatch BiomechanicalSite BiomechanicalStalk BiomechanicalRefinementDetail BiomechanicalScore BiomechanicalRank]
refineBiomechanicalCompiledWithMatcher seedMatcher (SheafRefiner model) compiledQuery graph =
  refineSheafCompiledWithMatcher
    seedMatcher
    (SheafRefiner (prepareBiomechanicalModel compiledQuery model))
    compiledQuery
    graph

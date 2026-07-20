{-# LANGUAGE RecordWildCards #-}

module Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Candidate
  ( BiomechanicalCandidateContext (..),
    collectValidations,
    materializeBiomechanicalCandidate,
    seedToCandidate,
    anchoredJointPositionMapDetailed,
    anchoredJointPositionMap,
    jointStalkEntryForSiteDetailed,
    jointStalkEntryForSite,
    buildBoneStalkDetailed,
    buildBoneStalk,
    buildRegistry,
    biomechanicalRestrictionValue,
    restrictionSatisfied,
  )
where

import Data.Kind (Type)
import Control.Applicative ((<|>))
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Core (collectEither)
import Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Core
  ( BiomechanicalAnatomicalBlueprint (..),
    BiomechanicalBlueprint (..),
    BiomechanicalBoneArtifact (..),
    BiomechanicalBoneBlueprint (..),
    BiomechanicalBoneConstraint (..),
    BiomechanicalBoneName,
    BiomechanicalCandidateMaterializationFailure (..),
    BiomechanicalBoneEndpoint (..),
    BiomechanicalEvidence (..),
    BiomechanicalGraphSpectralSignature,
    BiomechanicalJointAnchor (..),
    BiomechanicalJointBlueprint (..),
    BiomechanicalJointName (..),
    BiomechanicalRestriction (..),
    BiomechanicalSite (..),
    BiomechanicalSolveFailure (..),
    BiomechanicalStructuralBlueprint (..),
    BiomechanicalStructuralKind,
  )
import Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Policy
  ( BiomechanicalSpectralPolicy (..)
  )
import Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Score
  ( elasticEnergy,
    normalizedStrain,
  )
import Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.SheafStalk
  ( BiomechanicalStalk (..),
    anchorPositionOf,
    solvedPositionOf,
  )
import Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Skeleton
  ( canonicalVertexEdge,
    graphSpectralDistance,
    graphSpectralSignature,
    skeletonFromEdgeSupports,
  )
import Moonlight.EGraph.Fuzzy.Core
  ( RefinementCandidate (..),
  )
import Moonlight.Core
import Moonlight.Core
  ( Substitution,
    lookupSubst
  )
import Moonlight.EGraph.Pure.Types (ClassId)
import Moonlight.Homology (Graph1Skeleton)
import Moonlight.LinAlg.Geometry
  ( Vec3,
    addVec3,
    distanceVec3,
    scaleVec3,
    vec3Zero,
  )
import Moonlight.Sheaf.Section.Morphism
  ( Restriction (..),
    RestrictionId (..),
    RestrictionParts (..),
    unitIncidenceRestriction,
  )
import Moonlight.Sheaf.Section.ObjectIndex (mkObjectIndex)
import Moonlight.Sheaf.Section.Restriction
  ( RestrictionIndex,
    RestrictionIndexError,
    buildRestrictionIndex,
  )
import Moonlight.Sheaf.Section.Store.Types (SectionRestrictionResult (..))

type BiomechanicalCandidateContext :: Type
data BiomechanicalCandidateContext = BiomechanicalCandidateContext
  { bccLookupJointAnchor :: ClassId -> Maybe Vec3,
    bccLookupBoneConstraint :: ClassId -> ClassId -> Maybe BiomechanicalBoneConstraint
  }

type CandidateMaterializationValidation :: Type -> Type
type CandidateMaterializationValidation a = Either [BiomechanicalCandidateMaterializationFailure] a

materializeBiomechanicalCandidate ::
  BiomechanicalCandidateContext ->
  BiomechanicalBlueprint ->
  (ClassId, Substitution) ->
  CandidateMaterializationValidation (RefinementCandidate BiomechanicalSite BiomechanicalJointAnchor BiomechanicalEvidence)
materializeBiomechanicalCandidate model blueprint (rootClassId, substitution) = do
  let anatomy = bmbAnatomicalBlueprint blueprint
  orderedJointBindings <- collectValidations (fmap (jointBinding substitution) (babJointBlueprints anatomy))
  let jointSites = fmap boundJointSite orderedJointBindings
      jointSiteByName = Map.fromList (fmap (\binding -> (boundJointName binding, boundJointSite binding)) orderedJointBindings)
      jointIndexByName = Map.fromList (zip (fmap boundJointName orderedJointBindings) [0 :: Int ..])
  anchorEntries <- collectValidations (fmap (jointAnchorEntry model) orderedJointBindings)
  boneArtifacts <- collectValidations (fmap (boneArtifact model jointSiteByName jointIndexByName) (babBoneBlueprints anatomy))
  let anchorBySite = Map.fromList anchorEntries
      orderedBoneSites = fmap biomechanicalArtifactSite boneArtifacts
      edgeSupports = fmap biomechanicalArtifactEdge boneArtifacts
  structuralArtifacts <- collectValidations (fmap (structuralArtifact jointSiteByName anchorBySite) (babStructuralBlueprints anatomy))
  let
      orderedStructuralSites = fmap structuralArtifactSite structuralArtifacts
      boneConstraintBySite =
        Map.fromList
          (fmap
            (\artifact -> (biomechanicalArtifactSite artifact, biomechanicalArtifactConstraint artifact))
            boneArtifacts
          )
      boneEndpointsBySite = Map.fromList (fmap boneEndpointEntry orderedBoneSites)
      restrictions = orderedBoneSites >>= siteRestrictions boneEndpointsBySite
      jointAnchorBySite = Map.map biomechanicalJointAnchorPosition anchorBySite
      structuralMembersBySite = Map.fromList (fmap (\artifact -> (structuralArtifactSite artifact, structuralArtifactMembers artifact)) structuralArtifacts)
      structuralKindBySite = Map.fromList (fmap (\artifact -> (structuralArtifactSite artifact, structuralArtifactKind artifact)) structuralArtifacts)
      structuralAnchorBySite = Map.fromList (fmap (\artifact -> (structuralArtifactSite artifact, structuralArtifactAnchor artifact)) structuralArtifacts)
      graphSkeleton = skeletonFromEdgeSupports (length jointSites) edgeSupports
      graphSignature = biomechanicalGraphSignature blueprint graphSkeleton
      varSites =
        IntMap.fromList
          (fmap
            (\binding -> (patternVarKey (boundJointPatternVar binding), boundJointSite binding))
            orderedJointBindings
          )
  effectorSite <- resolveEffectorSite anatomy jointSiteByName
  pure
    RefinementCandidate
      { rcRootClass = rootClassId,
        rcDiscreteSubstitution = substitution,
        rcVarSites = varSites,
        rcSites = jointSites <> orderedBoneSites <> orderedStructuralSites,
        rcAnchors = Map.restrictKeys anchorBySite (Set.fromList jointSites),
        rcEvidence =
          BiomechanicalEvidence
            { bmeOrderedJointSites = jointSites,
              bmeOrderedBoneSites = orderedBoneSites,
              bmeOrderedStructuralSites = orderedStructuralSites,
              bmeEffectorSite = effectorSite,
              bmeBoneEndpointsBySite = boneEndpointsBySite,
              bmeStructuralMembersBySite = structuralMembersBySite,
              bmeStructuralKindBySite = structuralKindBySite,
              bmeJointAnchorBySite = jointAnchorBySite,
              bmeStructuralAnchorBySite = structuralAnchorBySite,
              bmeBoneConstraintBySite = boneConstraintBySite,
              bmeRestrictions = restrictions,
              bmeGraphSkeleton = graphSkeleton,
              bmeGraphSignature = graphSignature,
              bmeSpectralDrift = graphSpectralDistance (bmbPatternGraphSignature blueprint) graphSignature
            }
      }

seedToCandidate ::
  BiomechanicalCandidateContext ->
  BiomechanicalBlueprint ->
  (ClassId, Substitution) ->
  Maybe (RefinementCandidate BiomechanicalSite BiomechanicalJointAnchor BiomechanicalEvidence)
seedToCandidate model blueprint =
  either (const Nothing) Just . materializeBiomechanicalCandidate model blueprint

biomechanicalGraphSignature :: BiomechanicalBlueprint -> Graph1Skeleton -> BiomechanicalGraphSpectralSignature
biomechanicalGraphSignature blueprint =
  graphSpectralSignature (bspModeCount (bmbSpectralPolicy blueprint))

type BoundJoint :: Type
data BoundJoint = BoundJoint
  { boundJointName :: BiomechanicalJointName,
    boundJointPatternVar :: PatternVar,
    boundJointClassId :: ClassId,
    boundJointSite :: BiomechanicalSite
  }

jointBinding :: Substitution -> BiomechanicalJointBlueprint -> CandidateMaterializationValidation BoundJoint
jointBinding substitution BiomechanicalJointBlueprint {..} =
  case lookupSubst bjbPatternVar substitution of
    Just classId ->
      Right
        BoundJoint
          { boundJointName = bjbName,
            boundJointPatternVar = bjbPatternVar,
            boundJointClassId = classId,
            boundJointSite = BiomechanicalJointSite bjbPatternVar classId
          }
    Nothing ->
      Left [MissingBiomechanicalJointBinding bjbName bjbPatternVar]

jointAnchorEntry ::
  BiomechanicalCandidateContext ->
  BoundJoint ->
  CandidateMaterializationValidation (BiomechanicalSite, BiomechanicalJointAnchor)
jointAnchorEntry BiomechanicalCandidateContext {..} boundJoint =
  case bccLookupJointAnchor (boundJointClassId boundJoint) of
    Just anchorPosition ->
      Right (boundJointSite boundJoint, BiomechanicalJointAnchor anchorPosition)
    Nothing ->
      Left [MissingBiomechanicalJointAnchor (boundJointSite boundJoint) (boundJointClassId boundJoint)]

boneArtifact ::
  BiomechanicalCandidateContext ->
  Map BiomechanicalJointName BiomechanicalSite ->
  Map BiomechanicalJointName Int ->
  BiomechanicalBoneBlueprint ->
  CandidateMaterializationValidation BiomechanicalBoneArtifact
boneArtifact BiomechanicalCandidateContext {..} jointSiteByName jointIndexByName boneBlueprint = do
  sourceSite <- lookupBoneEndpointSite boneBlueprint (bbbSourceJoint boneBlueprint) jointSiteByName
  targetSite <- lookupBoneEndpointSite boneBlueprint (bbbTargetJoint boneBlueprint) jointSiteByName
  sourceClassId <- lookupBoneEndpointClassId (bbbName boneBlueprint) sourceSite
  targetClassId <- lookupBoneEndpointClassId (bbbName boneBlueprint) targetSite
  sourcePatternVar <- lookupBoneEndpointPatternVar (bbbName boneBlueprint) sourceSite
  targetPatternVar <- lookupBoneEndpointPatternVar (bbbName boneBlueprint) targetSite
  sourceIndex <- lookupBoneEndpointIndex boneBlueprint (bbbSourceJoint boneBlueprint) jointIndexByName
  targetIndex <- lookupBoneEndpointIndex boneBlueprint (bbbTargetJoint boneBlueprint) jointIndexByName
  case bccLookupBoneConstraint sourceClassId targetClassId <|> bccLookupBoneConstraint targetClassId sourceClassId of
    Just constraintValue ->
      Right
        BiomechanicalBoneArtifact
          { biomechanicalArtifactName = bbbName boneBlueprint,
            biomechanicalArtifactEdge = canonicalVertexEdge sourceIndex targetIndex,
            biomechanicalArtifactSite = BiomechanicalBoneSite sourcePatternVar targetPatternVar sourceClassId targetClassId,
            biomechanicalArtifactConstraint = constraintValue
          }
    Nothing ->
      Left [MissingBiomechanicalBoneConstraint (bbbName boneBlueprint) sourceSite targetSite]

type StructuralArtifact :: Type
data StructuralArtifact = StructuralArtifact
  { structuralArtifactSite :: BiomechanicalSite,
    structuralArtifactMembers :: [BiomechanicalSite],
    structuralArtifactAnchor :: Vec3,
    structuralArtifactKind :: BiomechanicalStructuralKind
  }

structuralArtifact ::
  Map BiomechanicalJointName BiomechanicalSite ->
  Map BiomechanicalSite BiomechanicalJointAnchor ->
  BiomechanicalStructuralBlueprint ->
  CandidateMaterializationValidation StructuralArtifact
structuralArtifact jointSiteByName anchorBySite structuralBlueprint = do
  memberSites <- collectValidations (fmap (lookupStructuralMemberSite structuralBlueprint jointSiteByName) (bsbIncidentJoints structuralBlueprint))
  memberAnchors <- collectValidations (fmap (lookupStructuralMemberAnchor structuralBlueprint anchorBySite) memberSites)
  let anchorPosition = averagePositions memberAnchors
  pure
    StructuralArtifact
      { structuralArtifactSite = BiomechanicalStructuralSite (bsbName structuralBlueprint),
        structuralArtifactMembers = memberSites,
        structuralArtifactAnchor = anchorPosition,
        structuralArtifactKind = bsbKind structuralBlueprint
      }

resolveEffectorSite ::
  BiomechanicalAnatomicalBlueprint ->
  Map BiomechanicalJointName BiomechanicalSite ->
  CandidateMaterializationValidation BiomechanicalSite
resolveEffectorSite anatomy jointSiteByName =
  case babEffectorJoint anatomy of
    Nothing ->
      Left [MissingBiomechanicalEffectorSelection]
    Just effectorJointName ->
      case Map.lookup effectorJointName jointSiteByName of
        Just effectorSite ->
          Right effectorSite
        Nothing ->
          Left [MissingBiomechanicalEffectorSite effectorJointName]

collectValidations :: Semigroup e => [Either e a] -> Either e [a]
collectValidations = collectEither

lookupBoneEndpointSite ::
  BiomechanicalBoneBlueprint ->
  BiomechanicalJointName ->
  Map BiomechanicalJointName BiomechanicalSite ->
  CandidateMaterializationValidation BiomechanicalSite
lookupBoneEndpointSite boneBlueprint jointName jointSiteByName =
  case Map.lookup jointName jointSiteByName of
    Just jointSite ->
      Right jointSite
    Nothing ->
      Left [MissingBiomechanicalBoneEndpointSite (bbbName boneBlueprint) jointName]

lookupBoneEndpointIndex ::
  BiomechanicalBoneBlueprint ->
  BiomechanicalJointName ->
  Map BiomechanicalJointName Int ->
  CandidateMaterializationValidation Int
lookupBoneEndpointIndex boneBlueprint jointName jointIndexByName =
  case Map.lookup jointName jointIndexByName of
    Just jointIndex ->
      Right jointIndex
    Nothing ->
      Left [MissingBiomechanicalBoneEndpointSite (bbbName boneBlueprint) jointName]

lookupBoneEndpointClassId ::
  BiomechanicalBoneName ->
  BiomechanicalSite ->
  CandidateMaterializationValidation ClassId
lookupBoneEndpointClassId boneName site =
  case jointSiteClassId site of
    Just classId ->
      Right classId
    Nothing ->
      Left [InvalidBiomechanicalBoneEndpointSite boneName site]

lookupBoneEndpointPatternVar ::
  BiomechanicalBoneName ->
  BiomechanicalSite ->
  CandidateMaterializationValidation PatternVar
lookupBoneEndpointPatternVar boneName site =
  case jointSitePatternVar site of
    Just patternVar ->
      Right patternVar
    Nothing ->
      Left [InvalidBiomechanicalBoneEndpointSite boneName site]

lookupStructuralMemberSite ::
  BiomechanicalStructuralBlueprint ->
  Map BiomechanicalJointName BiomechanicalSite ->
  BiomechanicalJointName ->
  CandidateMaterializationValidation BiomechanicalSite
lookupStructuralMemberSite structuralBlueprint jointSiteByName jointName =
  case Map.lookup jointName jointSiteByName of
    Just jointSite ->
      Right jointSite
    Nothing ->
      Left [MissingBiomechanicalStructuralMemberSite (bsbName structuralBlueprint) jointName]

lookupStructuralMemberAnchor ::
  BiomechanicalStructuralBlueprint ->
  Map BiomechanicalSite BiomechanicalJointAnchor ->
  BiomechanicalSite ->
  CandidateMaterializationValidation Vec3
lookupStructuralMemberAnchor structuralBlueprint anchorBySite site =
  case Map.lookup site anchorBySite of
    Just jointAnchor ->
      Right (biomechanicalJointAnchorPosition jointAnchor)
    Nothing ->
      Left [MissingBiomechanicalStructuralMemberAnchor (bsbName structuralBlueprint) site]

averagePositions :: [Vec3] -> Vec3
averagePositions positions =
  let positionCount = length positions
      summedPosition = foldr addVec3 vec3Zero positions
   in if positionCount <= 0
        then vec3Zero
        else scaleVec3 (1.0 / fromIntegral positionCount) summedPosition

jointSiteClassId :: BiomechanicalSite -> Maybe ClassId
jointSiteClassId site =
  case site of
    BiomechanicalJointSite _ classId ->
      Just classId
    BiomechanicalBoneSite _ _ _ _ ->
      Nothing
    BiomechanicalStructuralSite _ ->
      Nothing

jointSitePatternVar :: BiomechanicalSite -> Maybe PatternVar
jointSitePatternVar site =
  case site of
    BiomechanicalJointSite patternVar _ ->
      Just patternVar
    BiomechanicalBoneSite _ _ _ _ ->
      Nothing
    BiomechanicalStructuralSite _ ->
      Nothing

boneEndpointEntry :: BiomechanicalSite -> (BiomechanicalSite, (BiomechanicalSite, BiomechanicalSite))
boneEndpointEntry boneSite =
  case boneSite of
    BiomechanicalBoneSite sourcePatternVar targetPatternVar sourceClassId targetClassId ->
      ( boneSite,
        ( BiomechanicalJointSite sourcePatternVar sourceClassId,
          BiomechanicalJointSite targetPatternVar targetClassId
        )
      )
    BiomechanicalJointSite _ _ ->
      (boneSite, (boneSite, boneSite))
    BiomechanicalStructuralSite _ ->
      (boneSite, (boneSite, boneSite))

siteRestrictions ::
  Map BiomechanicalSite (BiomechanicalSite, BiomechanicalSite) ->
  BiomechanicalSite ->
  [BiomechanicalRestriction]
siteRestrictions boneEndpointsBySite boneSite =
  case Map.lookup boneSite boneEndpointsBySite of
    Just (sourceSite, targetSite) ->
      [ BiomechanicalRestriction
          { bmrKind = unitIncidenceRestriction,
            bmrSourceSite = sourceSite,
            bmrTargetSite = boneSite,
            bmrEndpoint = SourceBiomechanicalBoneEndpoint
          },
        BiomechanicalRestriction
          { bmrKind = unitIncidenceRestriction,
            bmrSourceSite = targetSite,
            bmrTargetSite = boneSite,
            bmrEndpoint = TargetBiomechanicalBoneEndpoint
          }
      ]
    Nothing ->
      []

anchoredJointPositionMap ::
  [BiomechanicalSite] ->
  Map BiomechanicalSite BiomechanicalJointAnchor ->
  Maybe (Map BiomechanicalSite Vec3)
anchoredJointPositionMap orderedJointSites anchorsBySite =
  either (const Nothing) Just (anchoredJointPositionMapDetailed orderedJointSites anchorsBySite)

anchoredJointPositionMapDetailed ::
  [BiomechanicalSite] ->
  Map BiomechanicalSite BiomechanicalJointAnchor ->
  Either [BiomechanicalSolveFailure] (Map BiomechanicalSite Vec3)
anchoredJointPositionMapDetailed orderedJointSites anchorsBySite =
  fmap Map.fromList
    ( collectValidations
        ( fmap
            (\site ->
                case Map.lookup site anchorsBySite of
                  Just jointAnchor ->
                    Right (site, biomechanicalJointAnchorPosition jointAnchor)
                  Nothing ->
                    Left [MissingBiomechanicalAnchorPosition site]
            )
            orderedJointSites
        )
    )

jointStalkEntryForSite ::
  Map BiomechanicalSite Vec3 ->
  Map BiomechanicalSite Vec3 ->
  BiomechanicalSite ->
  Maybe (BiomechanicalSite, BiomechanicalStalk)
jointStalkEntryForSite anchoredPositions solvedPositions site =
  either (const Nothing) Just (jointStalkEntryForSiteDetailed anchoredPositions solvedPositions site)

jointStalkEntryForSiteDetailed ::
  Map BiomechanicalSite Vec3 ->
  Map BiomechanicalSite Vec3 ->
  BiomechanicalSite ->
  Either [BiomechanicalSolveFailure] (BiomechanicalSite, BiomechanicalStalk)
jointStalkEntryForSiteDetailed anchoredPositions solvedPositions site =
  case (Map.lookup site anchoredPositions, Map.lookup site solvedPositions) of
    (Just anchorPosition, Just solvedPosition) ->
      Right
        ( site,
          JointBiomechanicalStalk anchorPosition solvedPosition
        )
    (Nothing, Just _) ->
      Left [MissingBiomechanicalAnchorPosition site]
    (Just _, Nothing) ->
      Left [MissingBiomechanicalSolvedSite site]
    (Nothing, Nothing) ->
      Left [MissingBiomechanicalAnchorPosition site, MissingBiomechanicalSolvedSite site]

buildBoneStalk ::
  BiomechanicalEvidence ->
  Map BiomechanicalSite BiomechanicalStalk ->
  BiomechanicalSite ->
  Maybe (BiomechanicalSite, BiomechanicalStalk)
buildBoneStalk evidence jointStalksBySite boneSite =
  either (const Nothing) Just (buildBoneStalkDetailed evidence jointStalksBySite boneSite)

buildBoneStalkDetailed ::
  BiomechanicalEvidence ->
  Map BiomechanicalSite BiomechanicalStalk ->
  BiomechanicalSite ->
  Either [BiomechanicalSolveFailure] (BiomechanicalSite, BiomechanicalStalk)
buildBoneStalkDetailed evidence jointStalksBySite boneSite =
  case
    ( Map.lookup boneSite (bmeBoneEndpointsBySite evidence),
      Map.lookup boneSite (bmeBoneConstraintBySite evidence)
    ) of
    (Just (sourceSite, targetSite), Just constraintValue) ->
      case (Map.lookup sourceSite jointStalksBySite, Map.lookup targetSite jointStalksBySite) of
        (Just sourceJointStalk, Just targetJointStalk) ->
          let sourceAnchorPosition = anchorPositionOf sourceJointStalk
              targetAnchorPosition = anchorPositionOf targetJointStalk
              sourceSolvedPosition = solvedPositionOf sourceJointStalk
              targetSolvedPosition = solvedPositionOf targetJointStalk
              currentLength = distanceVec3 sourceSolvedPosition targetSolvedPosition
              restLength = biomechanicalBoneRestLength constraintValue
              strainValue = normalizedStrain currentLength restLength
           in Right
                ( boneSite,
                  BoneBiomechanicalStalk
                    sourceAnchorPosition
                    targetAnchorPosition
                    sourceSolvedPosition
                    targetSolvedPosition
                    restLength
                    (biomechanicalBoneStiffness constraintValue)
                    currentLength
                    strainValue
                    (elasticEnergy (biomechanicalBoneStiffness constraintValue) currentLength restLength)
                )
        (Nothing, Just _) ->
          Left [MissingBiomechanicalBoneJointStalk boneSite sourceSite]
        (Just _, Nothing) ->
          Left [MissingBiomechanicalBoneJointStalk boneSite targetSite]
        (Nothing, Nothing) ->
          Left
            [ MissingBiomechanicalBoneJointStalk boneSite sourceSite,
              MissingBiomechanicalBoneJointStalk boneSite targetSite
            ]
    (Nothing, Just _) ->
      Left [MissingBiomechanicalBoneEndpointEvidence boneSite]
    (Just _, Nothing) ->
      Left [MissingBiomechanicalBoneConstraintEvidence boneSite]
    (Nothing, Nothing) ->
      Left
        [ MissingBiomechanicalBoneEndpointEvidence boneSite,
          MissingBiomechanicalBoneConstraintEvidence boneSite
        ]

buildRegistry ::
  [BiomechanicalRestriction] ->
  Either
    (RestrictionIndexError BiomechanicalSite)
    (RestrictionIndex BiomechanicalSite BiomechanicalRestriction)
buildRegistry restrictions =
  buildRestrictionIndex
    (mkObjectIndex (foldMap restrictionCells restrictions))
    ( \restrictionValue ->
        RestrictionParts
          { partKind = bmrKind restrictionValue,
            partSource = bmrSourceSite restrictionValue,
            partTarget = bmrTargetSite restrictionValue,
            partWitness = restrictionValue
          }
    )
    restrictions

restrictionCells :: BiomechanicalRestriction -> [BiomechanicalSite]
restrictionCells restrictionValue =
  [ bmrSourceSite restrictionValue,
    bmrTargetSite restrictionValue
  ]

biomechanicalRestrictionValue ::
  BiomechanicalRestriction ->
  Restriction BiomechanicalSite BiomechanicalRestriction
biomechanicalRestrictionValue restrictionValue =
  Restriction
    (RestrictionId 0)
    (bmrKind restrictionValue)
    (bmrSourceSite restrictionValue)
    (bmrTargetSite restrictionValue)
    restrictionValue

restrictionSatisfied :: Either lookupError (SectionRestrictionResult cell stalk mismatch) -> Bool
restrictionSatisfied restrictionResult =
  case restrictionResult of
    Right SectionRestrictionSatisfied ->
      True
    Right (SectionRestrictionMismatch _) ->
      False
    Left _ ->
      False

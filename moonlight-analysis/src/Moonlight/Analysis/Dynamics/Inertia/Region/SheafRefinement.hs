{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Analysis.Dynamics.Inertia.Region.SheafRefinement
  ( MinimumAnchorCount,
    mkMinimumAnchorCount,
    defaultMinimumAnchorCount,
    InertiaRegionSite (..),
    InertiaRegionPathBinding (..),
    InertiaRegionPathBlueprint (..),
    InertiaRegionBlueprint (..),
    InertiaRegionEvidence (..),
    InertiaRegionRefinementDetail (..),
    InertiaRegionRefinementModel,
    mkInertiaRegionRefinementModelWithScorePolicy,
    mkInertiaRegionRefinementModel,
    mkInertiaRegionRefinementModelFromDecompositionWithScorePolicy,
    mkInertiaRegionRefinementModelFromDecomposition,
    mkInertiaRegionRefinementModelFromPatternDecompositionWithScorePolicy,
    mkInertiaRegionRefinementModelFromPatternDecomposition,
    mkInertiaRegionRefinementModelFromPathBlueprintWithScorePolicy,
    mkInertiaRegionRefinementModelFromPathBlueprint,
    inertiaRegionRefinerWithScorePolicy,
    inertiaRegionRefiner,
    inertiaRegionRefinerFromDecompositionWithScorePolicy,
    inertiaRegionRefinerFromDecomposition,
    inertiaRegionRefinerFromPatternDecompositionWithScorePolicy,
    inertiaRegionRefinerFromPatternDecomposition,
    inertiaRegionRefinerFromPathBlueprintWithScorePolicy,
    inertiaRegionRefinerFromPathBlueprint,
    prepareInertiaRegionModel,
  )
where

import Data.Kind (Type)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Moonlight.Analysis.Dynamics.Inertia.Region
  ( MassProperties,
    RegionSubdivisionPath,
    composeMassProperties,
  )
import Moonlight.Analysis.Dynamics.Inertia.Region.Cover
  ( InertiaRegionCoverBlueprint (..),
    InertiaRegionDecomposition,
    coverBlueprintFromDecomposition,
    coverChildrenByParent,
    restrictCoverBlueprint,
  )
import Moonlight.Analysis.Dynamics.Inertia.Region.PathBlueprint
  ( InertiaRegionPathBinding (..),
    InertiaRegionPathBlueprint (..),
    InertiaRegionPathBlueprintProgram (..),
    compileInertiaRegionPathBlueprint,
  )
import Moonlight.Analysis.Dynamics.Inertia.Region.PathBlueprint.Interpret
  ( InertiaRegionPathInterpreter (..),
    relabelDecompositionWithPathBlueprint,
  )
import Moonlight.Analysis.Dynamics.Inertia.Region.Score
  ( InertiaRegionScore,
    InertiaRegionScorePolicy,
    defaultInertiaRegionScorePolicy,
    interpretInertiaRegionScore,
    scoreInertiaRegionSection,
  )
import Moonlight.Analysis.Dynamics.Inertia.Region.SheafStalk
  ( massPropertiesStalkOps,
  )
import Moonlight.Analysis.SheafRefinement
  ( SheafEnergy (..),
    SheafRefinementModel (..),
    SheafRefiner (..),
    SheafSolve (..),
  )
import Moonlight.Core (Language)
import Moonlight.Rewrite.System (CompiledGuard)
import Moonlight.Rewrite.Algebra (CompiledPatternQuery)
import Moonlight.EGraph.Fuzzy.Core
  ( FuzzyRank (..),
    RefinementCandidate (..),
  )
import Moonlight.Core
import Moonlight.Core qualified as EGraph
import Moonlight.Core (Substitution (..))
import Moonlight.EGraph.Pure.Types (ClassId)
import Moonlight.Sheaf.Section.Stalk (stalkApproxEq)
type MinimumAnchorCount :: Type
newtype MinimumAnchorCount = MinimumAnchorCount
  { unMinimumAnchorCount :: Int
  }
  deriving stock (Eq, Show)

type InertiaRegionSite :: Type
data InertiaRegionSite
  = RootInertiaRegionSite ClassId
  | WitnessInertiaRegionSite PatternVar ClassId
  | StructuralInertiaRegionSite RegionSubdivisionPath
  deriving stock (Eq, Ord, Show)

type InertiaRegionBlueprint :: Type
data InertiaRegionBlueprint = InertiaRegionBlueprint
  { irbMinimumAnchorCount :: MinimumAnchorCount,
    irbPathBlueprint :: Maybe InertiaRegionPathBlueprint
  }
  deriving stock (Eq, Show)

type InertiaRegionEvidence :: Type
newtype InertiaRegionEvidence = InertiaRegionEvidence
  { ireSheafSummary :: InertiaRegionSheafSummary
  }
  deriving stock (Eq, Show)

type InertiaRegionSheafSummary :: Type
data InertiaRegionSheafSummary = InertiaRegionSheafSummary
  { irsAnchorMassPropertiesBySite :: Map InertiaRegionSite MassProperties,
    irsMassPropertiesBySite :: Map InertiaRegionSite MassProperties,
    irsCoverBlueprint :: InertiaRegionCoverBlueprint InertiaRegionSite,
    irsScore :: InertiaRegionScore
  }
  deriving stock (Eq, Show)

type InertiaRegionRefinementDetail :: Type
data InertiaRegionRefinementDetail = InertiaRegionRefinementDetail
  { irdAnchorCount :: Int,
    irdSiteCount :: Int,
    irdCoverPairCount :: Int
  }
  deriving stock (Eq, Show)

type InertiaRegionTopology :: Type
data InertiaRegionTopology
  = StaticInertiaRegionTopology (InertiaRegionCoverBlueprint InertiaRegionSite)
  | ProgramIndexedInertiaRegionTopology
      InertiaRegionPathBlueprintProgram
      (InertiaRegionDecomposition RegionSubdivisionPath)

type InertiaRegionRefinementModel :: Type
data InertiaRegionRefinementModel = InertiaRegionRefinementModel
  { irmMinimumAnchorCount :: MinimumAnchorCount,
    irmScorePolicy :: InertiaRegionScorePolicy,
    irmLookupMassProperties :: ClassId -> Maybe MassProperties,
    irmTopology :: InertiaRegionTopology,
    irmPrecompiledPathBlueprint :: Maybe InertiaRegionPathBlueprint
  }

inertiaRegionPathInterpreter :: InertiaRegionPathInterpreter InertiaRegionSite
inertiaRegionPathInterpreter =
  InertiaRegionPathInterpreter
    { irpiRootSite = RootInertiaRegionSite,
      irpiWitnessSite = WitnessInertiaRegionSite,
      irpiStructuralSite = StructuralInertiaRegionSite
    }

mkMinimumAnchorCount :: Int -> Maybe MinimumAnchorCount
mkMinimumAnchorCount anchorCount
  | anchorCount >= 0 =
      Just (MinimumAnchorCount anchorCount)
  | otherwise =
      Nothing

defaultMinimumAnchorCount :: MinimumAnchorCount
defaultMinimumAnchorCount = MinimumAnchorCount 1

requiredAnchorCount :: InertiaRegionBlueprint -> Int
requiredAnchorCount = unMinimumAnchorCount . irbMinimumAnchorCount

siteClassId :: InertiaRegionSite -> Maybe ClassId
siteClassId site =
  case site of
    RootInertiaRegionSite classId ->
      Just classId
    WitnessInertiaRegionSite _ classId ->
      Just classId
    StructuralInertiaRegionSite _ ->
      Nothing

isStructuralSite :: InertiaRegionSite -> Bool
isStructuralSite site =
  case site of
    RootInertiaRegionSite _ ->
      False
    WitnessInertiaRegionSite _ _ ->
      False
    StructuralInertiaRegionSite _ ->
      True

isRootSite :: InertiaRegionSite -> Bool
isRootSite site =
  case site of
    RootInertiaRegionSite _ ->
      True
    WitnessInertiaRegionSite _ _ ->
      False
    StructuralInertiaRegionSite _ ->
      False

siteSupportsChildren :: InertiaRegionSite -> Bool
siteSupportsChildren site =
  case site of
    RootInertiaRegionSite _ ->
      True
    WitnessInertiaRegionSite _ _ ->
      False
    StructuralInertiaRegionSite _ ->
      True

mkInertiaRegionRefinementModelWithScorePolicy ::
  MinimumAnchorCount ->
  InertiaRegionScorePolicy ->
  (ClassId -> Maybe MassProperties) ->
  InertiaRegionCoverBlueprint InertiaRegionSite ->
  InertiaRegionRefinementModel
mkInertiaRegionRefinementModelWithScorePolicy minimumAnchorCount scorePolicy lookupMassProperties coverBlueprint =
  InertiaRegionRefinementModel
    { irmMinimumAnchorCount = minimumAnchorCount,
      irmScorePolicy = scorePolicy,
      irmLookupMassProperties = lookupMassProperties,
      irmTopology = StaticInertiaRegionTopology coverBlueprint,
      irmPrecompiledPathBlueprint = Nothing
    }

mkInertiaRegionRefinementModel ::
  MinimumAnchorCount ->
  (ClassId -> Maybe MassProperties) ->
  InertiaRegionCoverBlueprint InertiaRegionSite ->
  InertiaRegionRefinementModel
mkInertiaRegionRefinementModel minimumAnchorCount lookupMassProperties coverBlueprint =
  mkInertiaRegionRefinementModelWithScorePolicy
    minimumAnchorCount
    defaultInertiaRegionScorePolicy
    lookupMassProperties
    coverBlueprint

mkInertiaRegionRefinementModelFromDecompositionWithScorePolicy ::
  MinimumAnchorCount ->
  InertiaRegionScorePolicy ->
  (ClassId -> Maybe MassProperties) ->
  InertiaRegionDecomposition InertiaRegionSite ->
  InertiaRegionRefinementModel
mkInertiaRegionRefinementModelFromDecompositionWithScorePolicy minimumAnchorCount scorePolicy lookupMassProperties =
  mkInertiaRegionRefinementModelWithScorePolicy minimumAnchorCount scorePolicy lookupMassProperties
    . coverBlueprintFromDecomposition

mkInertiaRegionRefinementModelFromDecomposition ::
  MinimumAnchorCount ->
  (ClassId -> Maybe MassProperties) ->
  InertiaRegionDecomposition InertiaRegionSite ->
  InertiaRegionRefinementModel
mkInertiaRegionRefinementModelFromDecomposition minimumAnchorCount lookupMassProperties =
  mkInertiaRegionRefinementModelFromDecompositionWithScorePolicy
    minimumAnchorCount
    defaultInertiaRegionScorePolicy
    lookupMassProperties

mkInertiaRegionRefinementModelFromPatternDecompositionWithScorePolicy ::
  MinimumAnchorCount ->
  InertiaRegionScorePolicy ->
  (ClassId -> Maybe MassProperties) ->
  InertiaRegionDecomposition RegionSubdivisionPath ->
  InertiaRegionRefinementModel
mkInertiaRegionRefinementModelFromPatternDecompositionWithScorePolicy minimumAnchorCount scorePolicy lookupMassProperties decomposition =
  InertiaRegionRefinementModel
    { irmMinimumAnchorCount = minimumAnchorCount,
      irmScorePolicy = scorePolicy,
      irmLookupMassProperties = lookupMassProperties,
      irmTopology =
        ProgramIndexedInertiaRegionTopology
          PrimaryPatternPathBlueprintProgram
          decomposition,
      irmPrecompiledPathBlueprint = Nothing
    }

mkInertiaRegionRefinementModelFromPatternDecomposition ::
  MinimumAnchorCount ->
  (ClassId -> Maybe MassProperties) ->
  InertiaRegionDecomposition RegionSubdivisionPath ->
  InertiaRegionRefinementModel
mkInertiaRegionRefinementModelFromPatternDecomposition minimumAnchorCount lookupMassProperties decomposition =
  mkInertiaRegionRefinementModelFromPatternDecompositionWithScorePolicy
    minimumAnchorCount
    defaultInertiaRegionScorePolicy
    lookupMassProperties
    decomposition

mkInertiaRegionRefinementModelFromPathBlueprintWithScorePolicy ::
  MinimumAnchorCount ->
  InertiaRegionScorePolicy ->
  (ClassId -> Maybe MassProperties) ->
  InertiaRegionPathBlueprint ->
  InertiaRegionDecomposition RegionSubdivisionPath ->
  InertiaRegionRefinementModel
mkInertiaRegionRefinementModelFromPathBlueprintWithScorePolicy minimumAnchorCount scorePolicy lookupMassProperties pathBlueprint decomposition =
  InertiaRegionRefinementModel
    { irmMinimumAnchorCount = minimumAnchorCount,
      irmScorePolicy = scorePolicy,
      irmLookupMassProperties = lookupMassProperties,
      irmTopology =
        ProgramIndexedInertiaRegionTopology
          (StaticPathBlueprintProgram pathBlueprint)
          decomposition,
      irmPrecompiledPathBlueprint = Just pathBlueprint
    }

mkInertiaRegionRefinementModelFromPathBlueprint ::
  MinimumAnchorCount ->
  (ClassId -> Maybe MassProperties) ->
  InertiaRegionPathBlueprint ->
  InertiaRegionDecomposition RegionSubdivisionPath ->
  InertiaRegionRefinementModel
mkInertiaRegionRefinementModelFromPathBlueprint minimumAnchorCount lookupMassProperties pathBlueprint decomposition =
  mkInertiaRegionRefinementModelFromPathBlueprintWithScorePolicy
    minimumAnchorCount
    defaultInertiaRegionScorePolicy
    lookupMassProperties
    pathBlueprint
    decomposition

inertiaRegionRefinerWithScorePolicy ::
  MinimumAnchorCount ->
  InertiaRegionScorePolicy ->
  (ClassId -> Maybe MassProperties) ->
  InertiaRegionCoverBlueprint InertiaRegionSite ->
  SheafRefiner InertiaRegionRefinementModel
inertiaRegionRefinerWithScorePolicy minimumAnchorCount scorePolicy lookupMassProperties coverBlueprint =
  SheafRefiner
    (mkInertiaRegionRefinementModelWithScorePolicy minimumAnchorCount scorePolicy lookupMassProperties coverBlueprint)

inertiaRegionRefiner ::
  MinimumAnchorCount ->
  (ClassId -> Maybe MassProperties) ->
  InertiaRegionCoverBlueprint InertiaRegionSite ->
  SheafRefiner InertiaRegionRefinementModel
inertiaRegionRefiner minimumAnchorCount lookupMassProperties coverBlueprint =
  inertiaRegionRefinerWithScorePolicy
    minimumAnchorCount
    defaultInertiaRegionScorePolicy
    lookupMassProperties
    coverBlueprint

inertiaRegionRefinerFromDecompositionWithScorePolicy ::
  MinimumAnchorCount ->
  InertiaRegionScorePolicy ->
  (ClassId -> Maybe MassProperties) ->
  InertiaRegionDecomposition InertiaRegionSite ->
  SheafRefiner InertiaRegionRefinementModel
inertiaRegionRefinerFromDecompositionWithScorePolicy minimumAnchorCount scorePolicy lookupMassProperties =
  inertiaRegionRefinerWithScorePolicy minimumAnchorCount scorePolicy lookupMassProperties
    . coverBlueprintFromDecomposition

inertiaRegionRefinerFromDecomposition ::
  MinimumAnchorCount ->
  (ClassId -> Maybe MassProperties) ->
  InertiaRegionDecomposition InertiaRegionSite ->
  SheafRefiner InertiaRegionRefinementModel
inertiaRegionRefinerFromDecomposition minimumAnchorCount lookupMassProperties =
  inertiaRegionRefinerFromDecompositionWithScorePolicy
    minimumAnchorCount
    defaultInertiaRegionScorePolicy
    lookupMassProperties

inertiaRegionRefinerFromPatternDecompositionWithScorePolicy ::
  MinimumAnchorCount ->
  InertiaRegionScorePolicy ->
  (ClassId -> Maybe MassProperties) ->
  InertiaRegionDecomposition RegionSubdivisionPath ->
  SheafRefiner InertiaRegionRefinementModel
inertiaRegionRefinerFromPatternDecompositionWithScorePolicy minimumAnchorCount scorePolicy lookupMassProperties =
  SheafRefiner
    . mkInertiaRegionRefinementModelFromPatternDecompositionWithScorePolicy
        minimumAnchorCount
        scorePolicy
        lookupMassProperties

inertiaRegionRefinerFromPatternDecomposition ::
  MinimumAnchorCount ->
  (ClassId -> Maybe MassProperties) ->
  InertiaRegionDecomposition RegionSubdivisionPath ->
  SheafRefiner InertiaRegionRefinementModel
inertiaRegionRefinerFromPatternDecomposition minimumAnchorCount lookupMassProperties =
  inertiaRegionRefinerFromPatternDecompositionWithScorePolicy
    minimumAnchorCount
    defaultInertiaRegionScorePolicy
    lookupMassProperties

inertiaRegionRefinerFromPathBlueprintWithScorePolicy ::
  MinimumAnchorCount ->
  InertiaRegionScorePolicy ->
  (ClassId -> Maybe MassProperties) ->
  InertiaRegionPathBlueprint ->
  InertiaRegionDecomposition RegionSubdivisionPath ->
  SheafRefiner InertiaRegionRefinementModel
inertiaRegionRefinerFromPathBlueprintWithScorePolicy minimumAnchorCount scorePolicy lookupMassProperties pathBlueprint decomposition =
  SheafRefiner
    ( mkInertiaRegionRefinementModelFromPathBlueprintWithScorePolicy
        minimumAnchorCount
        scorePolicy
        lookupMassProperties
        pathBlueprint
        decomposition
    )

inertiaRegionRefinerFromPathBlueprint ::
  MinimumAnchorCount ->
  (ClassId -> Maybe MassProperties) ->
  InertiaRegionPathBlueprint ->
  InertiaRegionDecomposition RegionSubdivisionPath ->
  SheafRefiner InertiaRegionRefinementModel
inertiaRegionRefinerFromPathBlueprint minimumAnchorCount lookupMassProperties pathBlueprint decomposition =
  inertiaRegionRefinerFromPathBlueprintWithScorePolicy
    minimumAnchorCount
    defaultInertiaRegionScorePolicy
    lookupMassProperties
    pathBlueprint
    decomposition

coverAdmissible ::
  InertiaRegionCoverBlueprint InertiaRegionSite ->
  Bool
coverAdmissible coverBlueprint =
  foldr
    (\coverPair isAdmissible ->
        isAdmissible && admissibleCoverPair coverPair
    )
    True
    (ircbCoverPairs coverBlueprint)

admissibleCoverPair :: (InertiaRegionSite, InertiaRegionSite) -> Bool
admissibleCoverPair (parentSite, childSite) =
  siteSupportsChildren parentSite && not (isRootSite childSite)

coverCompositionConsistent ::
  InertiaRegionCoverBlueprint InertiaRegionSite ->
  Map InertiaRegionSite MassProperties ->
  Bool
coverCompositionConsistent coverBlueprint valueBySite =
  foldr
    (\parentSite isConsistent ->
        isConsistent && siteCompositionConsistent parentSite
    )
    True
    (Map.keys (coverChildrenByParent coverBlueprint))
  where
    childSitesByParent = coverChildrenByParent coverBlueprint

    siteCompositionConsistent parentSite =
      case Map.lookup parentSite childSitesByParent of
        Nothing ->
          True
        Just childSites ->
          case
            ( Map.lookup parentSite valueBySite,
              composeMassProperties =<< traverse (`Map.lookup` valueBySite) childSites
            )
            of
            (Just parentMassProperties, Just composedChildMassProperties) ->
              stalkApproxEq massPropertiesStalkOps parentMassProperties composedChildMassProperties
            _ ->
              False

seedToCandidate ::
  InertiaRegionRefinementModel ->
  Maybe InertiaRegionPathBlueprint ->
  (ClassId, Substitution) ->
  Maybe (RefinementCandidate InertiaRegionSite MassProperties InertiaRegionEvidence)
seedToCandidate model maybePathBlueprint (rootClassId, substitution@(Substitution bindings)) =
  let rootSite = RootInertiaRegionSite rootClassId
      varSites =
        IntMap.mapWithKey
          (\patternKey classId -> WitnessInertiaRegionSite (EGraph.mkPatternVar patternKey) classId)
          bindings
      sites =
        Set.toList
          ( Set.insert rootSite
              (Set.fromList (IntMap.elems varSites))
          )
      massPropertiesBySite = Map.fromList (mapMaybe (siteMassProperties model) sites)
   in fmap
        (\candidateCoverBlueprint ->
            fmap
              (\sheafSummary ->
                  RefinementCandidate
                    { rcRootClass = rootClassId,
                      rcDiscreteSubstitution = substitution,
                      rcVarSites = varSites,
                      rcSites = Map.keys (irsMassPropertiesBySite sheafSummary),
                      rcAnchors = irsAnchorMassPropertiesBySite sheafSummary,
                      rcEvidence = InertiaRegionEvidence sheafSummary
                    }
              )
              (inertiaRegionSheafSummary massPropertiesBySite candidateCoverBlueprint)
        )
        (resolveCandidateCoverBlueprint model maybePathBlueprint rootClassId substitution)
      >>= id

siteMassProperties ::
  InertiaRegionRefinementModel ->
  InertiaRegionSite ->
  Maybe (InertiaRegionSite, MassProperties)
siteMassProperties InertiaRegionRefinementModel {..} site =
  fmap ((,) site) (siteClassId site >>= irmLookupMassProperties)

resolveCandidateCoverBlueprint ::
  InertiaRegionRefinementModel ->
  Maybe InertiaRegionPathBlueprint ->
  ClassId ->
  Substitution ->
  Maybe (InertiaRegionCoverBlueprint InertiaRegionSite)
resolveCandidateCoverBlueprint InertiaRegionRefinementModel {..} maybePathBlueprint rootClassId substitution =
  topologyCoverBlueprint irmTopology maybePathBlueprint rootClassId substitution

topologyCoverBlueprint ::
  InertiaRegionTopology ->
  Maybe InertiaRegionPathBlueprint ->
  ClassId ->
  Substitution ->
  Maybe (InertiaRegionCoverBlueprint InertiaRegionSite)
topologyCoverBlueprint topology maybePathBlueprint rootClassId substitution =
  case topology of
    StaticInertiaRegionTopology coverBlueprint ->
      Just coverBlueprint
    ProgramIndexedInertiaRegionTopology _ decomposition ->
      maybePathBlueprint
        >>= \pathBlueprint ->
          coverBlueprintFromDecomposition
            <$> relabelDecompositionWithPathBlueprint inertiaRegionPathInterpreter pathBlueprint rootClassId substitution decomposition

inertiaRegionSheafSummary ::
  Map InertiaRegionSite MassProperties ->
  InertiaRegionCoverBlueprint InertiaRegionSite ->
  Maybe InertiaRegionSheafSummary
inertiaRegionSheafSummary anchorMassPropertiesBySite coverBlueprint =
  let coverSites = Map.keys (ircbCellsBySite coverBlueprint)
      activeAnchorMassProperties =
        Map.restrictKeys anchorMassPropertiesBySite (Set.fromList coverSites)
   in completeMassPropertiesSection activeAnchorMassProperties coverBlueprint
        >>= \sectionMassPropertiesBySite ->
          let activeSites = Map.keys sectionMassPropertiesBySite
              activeCoverBlueprint = restrictCoverBlueprint activeSites coverBlueprint
           in if coverAdmissible activeCoverBlueprint
                then
                  Just
                    InertiaRegionSheafSummary
                      { irsAnchorMassPropertiesBySite = activeAnchorMassProperties,
                        irsMassPropertiesBySite = sectionMassPropertiesBySite,
                        irsCoverBlueprint = activeCoverBlueprint,
                        irsScore =
                          scoreInertiaRegionSection
                            isStructuralSite
                            activeAnchorMassProperties
                            sectionMassPropertiesBySite
                            activeCoverBlueprint
                      }
                else
                  Nothing

completeMassPropertiesSection ::
  Map InertiaRegionSite MassProperties ->
  InertiaRegionCoverBlueprint InertiaRegionSite ->
  Maybe (Map InertiaRegionSite MassProperties)
completeMassPropertiesSection anchorMassPropertiesBySite coverBlueprint =
  let childSitesByParent = coverChildrenByParent coverBlueprint
      coverSites = Map.keys (ircbCellsBySite coverBlueprint)
   in fmap
        Map.fromList
        (traverse
           (\site -> fmap ((,) site) (sectionMassAtSite childSitesByParent site))
           coverSites
        )
  where
    sectionMassAtSite childSitesByParent site =
      case Map.lookup site anchorMassPropertiesBySite of
        Just exactMassProperties ->
          Just exactMassProperties
        Nothing ->
          case Map.lookup site childSitesByParent of
            Nothing ->
              Nothing
            Just childSites ->
              composeMassProperties =<< traverse (sectionMassAtSite childSitesByParent) childSites

prepareInertiaRegionModel ::
  Language f =>
  CompiledPatternQuery (CompiledGuard capability f) f ->
  InertiaRegionRefinementModel ->
  InertiaRegionRefinementModel
prepareInertiaRegionModel compiledQuery model =
  case irmTopology model of
    StaticInertiaRegionTopology _ ->
      model
    ProgramIndexedInertiaRegionTopology pathBlueprintProgram _ ->
      model
        { irmPrecompiledPathBlueprint =
            Just (compileInertiaRegionPathBlueprint pathBlueprintProgram compiledQuery)
        }

instance SheafRefinementModel InertiaRegionRefinementModel where
  type SheafSite InertiaRegionRefinementModel = InertiaRegionSite
  type SheafAnchor InertiaRegionRefinementModel = MassProperties
  type SheafEvidence InertiaRegionRefinementModel = InertiaRegionEvidence
  type SheafValue InertiaRegionRefinementModel = MassProperties
  type SheafDetail InertiaRegionRefinementModel = InertiaRegionRefinementDetail
  type SheafBlueprint InertiaRegionRefinementModel = InertiaRegionBlueprint
  type SheafScore InertiaRegionRefinementModel = InertiaRegionScore
  type SheafRank InertiaRegionRefinementModel = Double
  type SheafSeed InertiaRegionRefinementModel = (ClassId, Substitution)

  compileSheafBlueprint model =
    InertiaRegionBlueprint
      { irbMinimumAnchorCount = irmMinimumAnchorCount model,
        irbPathBlueprint = irmPrecompiledPathBlueprint model
      }

  enumerateSheafCandidates model blueprint =
    mapMaybe (seedToCandidate model (irbPathBlueprint blueprint))

  acceptSheafCandidate _ blueprint candidate =
    let sheafSummary = ireSheafSummary (rcEvidence candidate)
        anchorBySite = irsAnchorMassPropertiesBySite sheafSummary
        valueBySite = irsMassPropertiesBySite sheafSummary
        coverBlueprint = irsCoverBlueprint sheafSummary
        anchoredCount = Map.size anchorBySite
     in anchoredCount >= requiredAnchorCount blueprint
          && Map.member (RootInertiaRegionSite (rcRootClass candidate)) anchorBySite
          && coverAdmissible coverBlueprint
          && coverCompositionConsistent coverBlueprint valueBySite
          && (Map.size valueBySite <= 1 || not (null (ircbCoverPairs coverBlueprint)))

  solveSheafCandidate _ _ candidate =
    let sheafSummary = ireSheafSummary (rcEvidence candidate)
        anchorBySite = irsAnchorMassPropertiesBySite sheafSummary
        valueBySite = irsMassPropertiesBySite sheafSummary
        coverBlueprint = irsCoverBlueprint sheafSummary
     in if Map.null valueBySite
            || not (coverAdmissible coverBlueprint)
            || not (coverCompositionConsistent coverBlueprint valueBySite)
          then Nothing
          else
            Just
              SheafSolve
                { ssValueBySite = valueBySite,
                  ssResidual = 0.0,
                  ssDetail =
                    InertiaRegionRefinementDetail
                      { irdAnchorCount = Map.size anchorBySite,
                        irdSiteCount = length (rcSites candidate),
                        irdCoverPairCount = length (ircbCoverPairs coverBlueprint)
                      }
                }

  interpretSheafSolve _ _ candidate _ =
    SheafEnergy
      (irsScore (ireSheafSummary (rcEvidence candidate)))

  rankSheafEnergy model (SheafEnergy inertiaRegionScore) =
    FuzzyRank
      (interpretInertiaRegionScore (irmScorePolicy model) inertiaRegionScore)

  compareSheafRanks _ (FuzzyRank leftRank) (FuzzyRank rightRank) =
    compare leftRank rightRank

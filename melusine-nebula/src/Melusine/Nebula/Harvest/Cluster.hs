{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Melusine.Nebula.Harvest.Cluster
  ( ClusterSiteArgs (..),
    AbstractionCluster (..),
    ClusterObstruction (..),
    abstractionClusters,
    clusterPatternVarKeys,
  )
where

import Control.Monad (foldM)
import Data.Graph qualified as Graph
import Data.IntMap.Strict qualified as IntMap
import Data.Kind (Type)
import Data.List (nub, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Set qualified as Set
import Data.Tree (flatten)
import Melusine.Nebula.Discovery.Choose
  ( AbstractionCandidate (..),
    CandidateSite (..),
  )
import Melusine.Nebula.Core (NebulaConfig (..))
import Moonlight.Core (Pattern (..))
import Moonlight.Core qualified as EGraph
import Moonlight.EGraph.Introspection.Core.HsExpr (HsExprF)
import Moonlight.EGraph.Pure.Types (ClassId)
import Moonlight.EGraph.Pure.AntiUnify (BinaryLGGResult (..))

type ClusterSiteArgs :: Type
data ClusterSiteArgs = ClusterSiteArgs
  { csaSite :: !CandidateSite,
    csaArgsByVar :: !(IntMap.IntMap ClassId)
  }
  deriving stock (Eq, Show)

type AbstractionCluster :: Type
data AbstractionCluster = AbstractionCluster
  { aclBody :: !(Pattern HsExprF),
    aclSites :: ![ClusterSiteArgs],
    aclSharedStructure :: !Int
  }
  deriving stock (Eq, Show)

type ClusterObstruction :: Type
data ClusterObstruction
  = ClusterArgumentConflict !String !Int !ClassId !ClassId
  | ClusterMissingVariableBinding !Int !String
  deriving stock (Eq, Show)

type CandidateCluster :: Type
data CandidateCluster = CandidateCluster
  { ccSites :: ![CandidateSite],
    ccCluster :: !AbstractionCluster,
    ccEstimatedWin :: !Int
  }
  deriving stock (Eq, Show)

type ClusterBodyStats :: Type
data ClusterBodyStats = ClusterBodyStats
  { cbsBody :: !(Pattern HsExprF),
    cbsDistinctVarKeys :: ![Int],
    cbsOccurrenceCount :: !Int
  }
  deriving stock (Eq, Show)

type CandidateBodyGroup :: Type
data CandidateBodyGroup = CandidateBodyGroup
  { cbgStats :: !ClusterBodyStats,
    cbgCandidates :: ![AbstractionCandidate]
  }

type CandidateComponent :: Type
data CandidateComponent = CandidateComponent
  { ccoStats :: !ClusterBodyStats,
    ccoCandidates :: ![AbstractionCandidate]
  }

type CandidateAssembly :: Type
data CandidateAssembly = CandidateAssembly
  { caArgsByOrdinal :: !(Map.Map Int ClusterSiteArgs),
    caCandidates :: ![AbstractionCandidate]
  }

abstractionClusters ::
  NebulaConfig ->
  [AbstractionCandidate] ->
  Either ClusterObstruction [AbstractionCluster]
abstractionClusters config candidates =
  fmap (fmap ccCluster . normalizeClusters . concat) $
    traverse (candidateClustersFromComponent config) (candidateComponents candidates)

candidateComponents :: [AbstractionCandidate] -> [CandidateComponent]
candidateComponents =
  foldMap bodyGroupComponents . candidateBodyGroups

candidateBodyGroups :: [AbstractionCandidate] -> [CandidateBodyGroup]
candidateBodyGroups candidates =
  fmap
    (uncurry candidateBodyGroup)
    (Map.toAscList bodyCandidateMap)
  where
    bodyCandidateMap =
      Map.fromListWith
        (<>)
        [ (binaryLggPattern (acResult candidate), [candidate])
        | candidate <- candidates,
          csOrdinal (acLeftSite candidate) /= csOrdinal (acRightSite candidate),
          csClass (acLeftSite candidate) /= csClass (acRightSite candidate)
        ]

candidateBodyGroup :: Pattern HsExprF -> [AbstractionCandidate] -> CandidateBodyGroup
candidateBodyGroup body bodyCandidates =
  CandidateBodyGroup
    { cbgStats = clusterBodyStats body,
      cbgCandidates = bodyCandidates
    }

clusterBodyStats :: Pattern HsExprF -> ClusterBodyStats
clusterBodyStats body =
  ClusterBodyStats
    { cbsBody = body,
      cbsDistinctVarKeys = nub occurrenceKeys,
      cbsOccurrenceCount = length occurrenceKeys
    }
  where
    occurrenceKeys =
      clusterPatternVarKeys body

bodyGroupComponents :: CandidateBodyGroup -> [CandidateComponent]
bodyGroupComponents bodyGroup =
  fmap
    ( \componentCandidates ->
        CandidateComponent
          { ccoStats = cbgStats bodyGroup,
            ccoCandidates = componentCandidates
          }
    )
    (connectedCandidateGroups (cbgCandidates bodyGroup))

connectedCandidateGroups :: [AbstractionCandidate] -> [[AbstractionCandidate]]
connectedCandidateGroups candidates =
  Map.elems $
    Map.fromListWith
      (<>)
      [ (componentIndex, [candidate])
      | candidate <- candidates,
        Just componentIndex <- [candidateComponentIndex siteComponentIndex candidate]
      ]
  where
    siteComponentIndex =
      componentIndexBySite candidates

candidateComponentIndex :: Map.Map Int Int -> AbstractionCandidate -> Maybe Int
candidateComponentIndex siteComponentIndex candidate =
  case (Map.lookup leftOrdinal siteComponentIndex, Map.lookup rightOrdinal siteComponentIndex) of
    (Just leftComponent, Just rightComponent)
      | leftComponent == rightComponent -> Just leftComponent
    _ -> Nothing
  where
    (leftOrdinal, rightOrdinal) =
      candidateSiteOrdinals candidate

componentIndexBySite :: [AbstractionCandidate] -> Map.Map Int Int
componentIndexBySite candidates =
  Map.fromList
    [ (siteOrdinal, componentIndex)
    | (componentIndex, componentSites) <- zip [0 ..] (siteComponents candidates),
      siteOrdinal <- Set.toAscList componentSites
    ]

siteComponents :: [AbstractionCandidate] -> [Set.Set Int]
siteComponents candidates =
  fmap
    ( Set.fromList
        . fmap
          ( \vertex ->
              let (_siteOrdinal, siteKey, _neighbors) = vertexFrom vertex
               in siteKey
          )
        . flatten
    )
    (Graph.components graphValue)
  where
    (graphValue, vertexFrom, _vertexForKey) =
      Graph.graphFromEdges
        [ (siteOrdinal, siteOrdinal, nub neighbors)
        | (siteOrdinal, neighbors) <- Map.toAscList (siteNeighborMap candidates)
        ]

siteNeighborMap :: [AbstractionCandidate] -> Map.Map Int [Int]
siteNeighborMap candidates =
  Map.fromListWith
    (<>)
    [ adjacencyRow
    | candidate <- candidates,
      adjacencyRow <- candidateAdjacencyRows candidate
    ]

candidateAdjacencyRows :: AbstractionCandidate -> [(Int, [Int])]
candidateAdjacencyRows candidate =
  [ (leftOrdinal, [rightOrdinal]),
    (rightOrdinal, [leftOrdinal])
  ]
  where
    (leftOrdinal, rightOrdinal) =
      candidateSiteOrdinals candidate

candidateSiteOrdinals :: AbstractionCandidate -> (Int, Int)
candidateSiteOrdinals candidate =
  (csOrdinal (acLeftSite candidate), csOrdinal (acRightSite candidate))

candidateClustersFromComponent ::
  NebulaConfig ->
  CandidateComponent ->
  Either ClusterObstruction [CandidateCluster]
candidateClustersFromComponent config component =
  fmap catMaybes $
    traverse
      (candidateClusterFromAssembly config (ccoStats component))
      (componentAssemblies component)

componentAssemblies :: CandidateComponent -> [CandidateAssembly]
componentAssemblies component =
  foldr insertCandidateAssembly [] (ccoCandidates component)

candidateAssembly :: AbstractionCandidate -> CandidateAssembly
candidateAssembly candidate =
  CandidateAssembly
    { caArgsByOrdinal = candidateSiteArgs candidate,
      caCandidates = [candidate]
    }

insertCandidateAssembly :: AbstractionCandidate -> [CandidateAssembly] -> [CandidateAssembly]
insertCandidateAssembly candidate assemblies =
  case insertCandidateAssemblyEither candidate assemblies of
    Nothing ->
      [candidateAssembly candidate]
    Just updatedAssemblies ->
      updatedAssemblies

insertCandidateAssemblyEither :: AbstractionCandidate -> [CandidateAssembly] -> Maybe [CandidateAssembly]
insertCandidateAssemblyEither candidate = \case
  [] ->
    Nothing
  assembly : remainingAssemblies ->
    case mergeCandidateAssembly assembly candidate of
      Left _conflict ->
        fmap (assembly :) (insertCandidateAssemblyEither candidate remainingAssemblies)
      Right mergedAssembly ->
        Just (mergedAssembly : remainingAssemblies)

mergeCandidateAssembly ::
  CandidateAssembly ->
  AbstractionCandidate ->
  Either ClusterObstruction CandidateAssembly
mergeCandidateAssembly assembly candidate =
  fmap
    ( \mergedArgs ->
        assembly
          { caArgsByOrdinal = mergedArgs,
            caCandidates = candidate : caCandidates assembly
          }
    )
    (mergeCandidateSiteArgs (caArgsByOrdinal assembly) candidate)

candidateClusterFromAssembly ::
  NebulaConfig ->
  ClusterBodyStats ->
  CandidateAssembly ->
  Either ClusterObstruction (Maybe CandidateCluster)
candidateClusterFromAssembly config stats assembly =
  case caCandidates assembly of
    [] ->
      Right Nothing
    firstCandidate : remainingCandidates -> do
      let sites =
            sortOn (csOrdinal . csaSite) (Map.elems (caArgsByOrdinal assembly))
          cluster =
            AbstractionCluster
              { aclBody = cbsBody stats,
                aclSites = sites,
                aclSharedStructure =
                  foldr
                    (min . binaryLggSharedStructure . acResult)
                    (binaryLggSharedStructure (acResult firstCandidate))
                    remainingCandidates
              }
      estimatedWin <- clusterEstimatedWinWithStats stats cluster
      pure
        ( if length sites >= 2 && aclSharedStructure cluster >= ncDiagnosticMinShared config
            then
              Just
                CandidateCluster
                  { ccSites = fmap csaSite (aclSites cluster),
                    ccCluster = cluster,
                    ccEstimatedWin = estimatedWin
                  }
            else Nothing
        )

clusterSiteArgs :: CandidateSite -> IntMap.IntMap ClassId -> ClusterSiteArgs
clusterSiteArgs site bindings =
  ClusterSiteArgs
    { csaSite = site,
      csaArgsByVar = bindings
    }

candidateSiteArgs :: AbstractionCandidate -> Map.Map Int ClusterSiteArgs
candidateSiteArgs candidate =
  Map.fromList
    [ (csOrdinal (acLeftSite candidate), clusterSiteArgs (acLeftSite candidate) (binaryLggLeftBindings (acResult candidate))),
      (csOrdinal (acRightSite candidate), clusterSiteArgs (acRightSite candidate) (binaryLggRightBindings (acResult candidate)))
    ]

mergeCandidateSiteArgs ::
  Map.Map Int ClusterSiteArgs ->
  AbstractionCandidate ->
  Either ClusterObstruction (Map.Map Int ClusterSiteArgs)
mergeCandidateSiteArgs indexedArgs candidate =
  foldM insertSiteArgs indexedArgs (Map.elems (candidateSiteArgs candidate))
  where
    insertSiteArgs currentArgs nextArgs =
      case Map.lookup (csOrdinal (csaSite nextArgs)) currentArgs of
        Nothing ->
          Right (Map.insert (csOrdinal (csaSite nextArgs)) nextArgs currentArgs)
        Just existingArgs -> do
          mergedArgs <- mergeSiteArgsChecked existingArgs nextArgs
          Right (Map.insert (csOrdinal (csaSite nextArgs)) mergedArgs currentArgs)

mergeSiteArgsChecked :: ClusterSiteArgs -> ClusterSiteArgs -> Either ClusterObstruction ClusterSiteArgs
mergeSiteArgsChecked leftArgs rightArgs =
  case firstArgumentConflict (csaArgsByVar leftArgs) (csaArgsByVar rightArgs) of
    Just (varKey, leftClass, rightClass) ->
      Left (ClusterArgumentConflict (clusterSiteLabel (csaSite leftArgs)) varKey leftClass rightClass)
    Nothing ->
      Right (leftArgs {csaArgsByVar = IntMap.union (csaArgsByVar leftArgs) (csaArgsByVar rightArgs)})

firstArgumentConflict :: IntMap.IntMap ClassId -> IntMap.IntMap ClassId -> Maybe (Int, ClassId, ClassId)
firstArgumentConflict leftArgs rightArgs =
  foldr
    ( \(varKey, (leftClass, rightClass)) conflict ->
        if leftClass == rightClass
          then conflict
          else Just (varKey, leftClass, rightClass)
    )
    Nothing
    (IntMap.toAscList (IntMap.intersectionWith (,) leftArgs rightArgs))

clusterEstimatedWinWithStats :: ClusterBodyStats -> AbstractionCluster -> Either ClusterObstruction Int
clusterEstimatedWinWithStats stats cluster = do
  let siteArgsRows =
        aclSites cluster
  varVectors <-
    traverse
      (\varKey -> traverse (classForClusterVar varKey) siteArgsRows)
      (cbsDistinctVarKeys stats)
  let distinctCount =
        length (nub varVectors)
      occurrenceCount =
        cbsOccurrenceCount stats
      sharedCount =
        aclSharedStructure cluster
      definitionSize =
        distinctCount + sharedCount + occurrenceCount
      perSideWin =
        (sharedCount - distinctCount - 1) + (occurrenceCount - distinctCount)
   in Right (length siteArgsRows * perSideWin - definitionSize)

classForClusterVar :: Int -> ClusterSiteArgs -> Either ClusterObstruction ClassId
classForClusterVar varKey siteArgs =
  maybe
    (Left (ClusterMissingVariableBinding varKey (clusterSiteLabel (csaSite siteArgs))))
    Right
    (IntMap.lookup varKey (csaArgsByVar siteArgs))

clusterSiteLabel :: CandidateSite -> String
clusterSiteLabel site =
  csBindingName site <> "#" <> show (csOrdinal site)

normalizeClusters :: [CandidateCluster] -> [CandidateCluster]
normalizeClusters =
  sortOn clusterMinOrdinal

clusterMinOrdinal :: CandidateCluster -> Int
clusterMinOrdinal =
  maybe 0 id . minimumSiteOrdinal . ccSites

minimumSiteOrdinal :: [CandidateSite] -> Maybe Int
minimumSiteOrdinal =
  foldr
    ( \site currentMinimum ->
        Just
          ( maybe
              (csOrdinal site)
              (min (csOrdinal site))
              currentMinimum
          )
    )
    Nothing

clusterPatternVarKeys :: Pattern HsExprF -> [Int]
clusterPatternVarKeys = \case
  PatternVar patternVar ->
    [EGraph.patternVarKey patternVar]
  PatternNode node ->
    foldMap clusterPatternVarKeys node

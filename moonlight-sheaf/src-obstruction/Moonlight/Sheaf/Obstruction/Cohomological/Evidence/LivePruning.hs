{-# LANGUAGE TupleSections #-}

module Moonlight.Sheaf.Obstruction.Cohomological.Evidence.LivePruning
  ( StaticComponentIndex,
    LiveMicrosupport,
    liveMicrosupportPruningEvidence,
    nonCriticalNodesFromLiveMicrosupport,
    pruningGatesFromLiveMicrosupport,
    recomputeLiveMicrosupport,
    staticComponentIndex,
    updateLiveMicrosupport,
  )
where

import Algebra.Graph.AdjacencyMap qualified as AdjacencyMap
import Algebra.Graph.AdjacencyMap.Algorithm qualified as AdjacencyMapAlgorithm
import Algebra.Graph.NonEmpty.AdjacencyMap qualified as NonEmptyAdjacencyMap
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Kind (Type)
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Moonlight.Core (RegionNodeId)
import Moonlight.Derived.Site (FinObjectId)
import Moonlight.Sheaf.Cochain.Cohomology
  ( SiteCoboundaryRealization (..),
    SiteCochainInput (..),
    buildNerveCochainArtifact,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Evidence.Microsupport
  ( MicrosupportEnrichment (..),
    computeNerveMicrosupportEnrichment,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Evidence.Pruning
  ( CohomologicalPruningGates,
    PruningEvidence (MicrosupportNonCritical),
    buildPruningGates,
  )
import Moonlight.Sheaf.Site.Construction.Nerve
  ( CellKey (..),
    NerveCell,
    NerveMorphism,
    NerveSite,
    NerveSiteAlgebra,
    NerveSource,
    faceMorphismSource,
    faceMorphismTarget,
    nerveCellKey,
    nerveSiteCells,
    restrictNerveSiteToCellKeys,
    siteFaceMorphisms,
  )
import Moonlight.Sheaf.Site.Stalk.Interface
  ( InterfaceComposeError,
    InterfaceDomain,
    InterfaceMorphism,
    InterfaceObject,
  )
import Moonlight.Sheaf.Site.Stalk.Interface.Linearization
  ( interfaceStalkBasisLinearization,
  )

type ComponentMicrosupport :: Type -> Type
data ComponentMicrosupport node = ComponentMicrosupport
  { cmsLiveValid :: !Bool,
    cmsSeedCellKeys :: !(Set.Set CellKey),
    cmsActiveCellKeys :: !(Set.Set CellKey),
    cmsCriticalNodes :: !(Set.Set node)
  }
  deriving stock (Eq, Show)

type StaticComponentIndex :: Type -> Type
data StaticComponentIndex tag = StaticComponentIndex
  { sciSite :: !(NerveSite tag),
    sciParents :: !(Map CellKey (Set.Set CellKey)),
    sciComponents :: !(IntMap (Set.Set CellKey)),
    sciComponentOf :: !(Map CellKey Int)
  }

type LiveMicrosupport :: Type -> Type
data LiveMicrosupport node = LiveMicrosupport
  { lmsComponents :: !(IntMap (ComponentMicrosupport node)),
    lmsNonCriticalNodes :: !(Set.Set node)
  }
  deriving stock (Eq, Show)

cellKey :: NerveCell tag -> CellKey
cellKey = nerveCellKey

emptyCellAdjacency :: NerveSite tag -> Map CellKey (Set.Set CellKey)
emptyCellAdjacency =
  Map.fromSet (const Set.empty) . Set.fromList . fmap cellKey . nerveSiteCells

undirectedAdjacency :: NerveSite tag -> Map CellKey (Set.Set CellKey)
undirectedAdjacency site =
  let addNeighbor :: CellKey -> CellKey -> Map CellKey (Set.Set CellKey) -> Map CellKey (Set.Set CellKey)
      addNeighbor fromK toK =
        Map.insertWith Set.union fromK (Set.singleton toK)
   in List.foldl'
        ( \acc faceMorphism ->
            let sourceKey = cellKey (faceMorphismSource faceMorphism)
                targetKey = cellKey (faceMorphismTarget faceMorphism)
             in addNeighbor targetKey sourceKey (addNeighbor sourceKey targetKey acc)
        )
        (emptyCellAdjacency site)
        (siteFaceMorphisms site)

parentAdjacency :: NerveSite tag -> Map CellKey (Set.Set CellKey)
parentAdjacency site =
  List.foldl'
    ( \acc faceMorphism ->
        let sourceKey = cellKey (faceMorphismSource faceMorphism)
            targetKey = cellKey (faceMorphismTarget faceMorphism)
         in Map.insertWith Set.union targetKey (Set.singleton sourceKey) acc
    )
    (emptyCellAdjacency site)
    (siteFaceMorphisms site)

componentIndex :: NerveSite tag -> (IntMap (Set.Set CellKey), Map CellKey Int)
componentIndex site =
  let indexedComponents =
        zip [0 ..] (weakComponentSetsFromAdjacency (undirectedAdjacency site))
      componentMap =
        IntMap.fromList indexedComponents
      nodeToComponent =
        Map.fromList
          ( indexedComponents
              >>= \(componentId, cellKeys) ->
                fmap (, componentId) (Set.toList cellKeys)
          )
   in (componentMap, nodeToComponent)

staticComponentIndex :: NerveSite tag -> StaticComponentIndex tag
staticComponentIndex site =
  let (componentsValue, componentOfValue) =
        componentIndex site
   in StaticComponentIndex
        { sciSite = site,
          sciParents = parentAdjacency site,
          sciComponents = componentsValue,
          sciComponentOf = componentOfValue
        }

seededComponentCellKeys ::
  StaticComponentIndex tag ->
  Set.Set CellKey ->
  IntMap (Set.Set CellKey)
seededComponentCellKeys staticIndex =
  Set.foldl' insertSeed IntMap.empty
  where
    insertSeed accumulated cellKeyValue =
      case Map.lookup cellKeyValue (sciComponentOf staticIndex) of
        Nothing ->
          accumulated
        Just componentId ->
          IntMap.insertWith
            Set.union
            componentId
            (Set.singleton cellKeyValue)
            accumulated

componentParentAdjacency ::
  Set.Set CellKey ->
  Map CellKey (Set.Set CellKey) ->
  Map CellKey (Set.Set CellKey)
componentParentAdjacency componentKeys =
  Map.map (`Set.intersection` componentKeys)
    . (`Map.restrictKeys` componentKeys)

recomputeLiveMicrosupport ::
  ( NerveSiteAlgebra tag,
    InterfaceDomain tag,
    Show (InterfaceComposeError tag),
    NerveSource tag ~ InterfaceObject tag,
    NerveMorphism tag ~ InterfaceMorphism tag,
    Ord node
  ) =>
  (FinObjectId -> Maybe node) ->
  (CellKey -> Maybe node) ->
  StaticComponentIndex tag ->
  Set.Set CellKey ->
  LiveMicrosupport node
recomputeLiveMicrosupport nodeProjection cellKeyProjection staticIndex seedCellKeys =
  let seedKeysByComponent =
        seededComponentCellKeys staticIndex seedCellKeys

      componentMicrosupport =
        IntMap.mapWithKey
          ( \componentId componentSeedKeys ->
              let componentKeys =
                    IntMap.findWithDefault
                      Set.empty
                      componentId
                      (sciComponents staticIndex)

                  componentSite =
                    restrictNerveSiteToCellKeys componentKeys (sciSite staticIndex)

                  componentParents =
                    componentParentAdjacency componentKeys (sciParents staticIndex)
               in recomputeActiveMicrosupport
                    nodeProjection
                    componentSite
                    componentParents
                    componentSeedKeys
          )
          seedKeysByComponent

      nonCriticalNodes =
        nonCriticalNodesFromComponents cellKeyProjection componentMicrosupport
   in LiveMicrosupport
        { lmsComponents = componentMicrosupport,
          lmsNonCriticalNodes = nonCriticalNodes
        }

nonCriticalNodesFromLiveMicrosupport ::
  LiveMicrosupport node ->
  Set.Set node
nonCriticalNodesFromLiveMicrosupport =
  lmsNonCriticalNodes
{-# INLINE nonCriticalNodesFromLiveMicrosupport #-}

updateLiveMicrosupport ::
  ( NerveSiteAlgebra tag,
    InterfaceDomain tag,
    Show (InterfaceComposeError tag),
    NerveSource tag ~ InterfaceObject tag,
    NerveMorphism tag ~ InterfaceMorphism tag,
    Ord node
  ) =>
  (FinObjectId -> Maybe node) ->
  (CellKey -> Maybe node) ->
  StaticComponentIndex tag ->
  Set.Set CellKey ->
  LiveMicrosupport node ->
  LiveMicrosupport node
updateLiveMicrosupport nodeProjection cellKeyProjection staticIndex seedCellKeys prior =
  let seedKeysByComponent =
        seededComponentCellKeys staticIndex seedCellKeys

      refreshComponent componentId componentSeedKeys =
        let componentKeys =
              IntMap.findWithDefault
                Set.empty
                componentId
                (sciComponents staticIndex)

            componentParents =
              componentParentAdjacency componentKeys (sciParents staticIndex)

            recomputed =
              recomputeActiveMicrosupport
                nodeProjection
                (restrictNerveSiteToCellKeys componentKeys (sciSite staticIndex))
                componentParents
                componentSeedKeys
         in case IntMap.lookup componentId (lmsComponents prior) of
              Just cached
                | cmsSeedCellKeys cached == componentSeedKeys ->
                    cached
                | activeCellKeysFrom componentParents componentSeedKeys
                    == cmsActiveCellKeys cached ->
                    ComponentMicrosupport
                      { cmsLiveValid = cmsLiveValid cached,
                        cmsSeedCellKeys = componentSeedKeys,
                        cmsActiveCellKeys = cmsActiveCellKeys cached,
                        cmsCriticalNodes = cmsCriticalNodes cached
                      }
              _ ->
                recomputed

      componentMicrosupport =
        IntMap.mapWithKey refreshComponent seedKeysByComponent

      nonCriticalNodes =
        nonCriticalNodesFromComponents cellKeyProjection componentMicrosupport
   in LiveMicrosupport
        { lmsComponents = componentMicrosupport,
          lmsNonCriticalNodes = nonCriticalNodes
        }

recomputeActiveMicrosupport ::
  ( NerveSiteAlgebra tag,
    InterfaceDomain tag,
    Show (InterfaceComposeError tag),
    NerveSource tag ~ InterfaceObject tag,
    NerveMorphism tag ~ InterfaceMorphism tag
  ) =>
  Ord node =>
  (FinObjectId -> Maybe node) ->
  NerveSite tag ->
  Map CellKey (Set.Set CellKey) ->
  Set.Set CellKey ->
  ComponentMicrosupport node
recomputeActiveMicrosupport nodeProjection componentSite parentsMap seedCellKeys
  | Set.null seedCellKeys =
      ComponentMicrosupport
        { cmsLiveValid = True,
          cmsSeedCellKeys = Set.empty,
          cmsActiveCellKeys = Set.empty,
          cmsCriticalNodes = Set.empty
        }
  | otherwise =
      let activeCellKeys =
            activeCellKeysFrom parentsMap seedCellKeys
          localSite =
            restrictNerveSiteToCellKeys activeCellKeys componentSite
          invalidComponent =
            ComponentMicrosupport
              { cmsLiveValid = False,
                cmsSeedCellKeys = seedCellKeys,
                cmsActiveCellKeys = activeCellKeys,
                cmsCriticalNodes = Set.empty
              }
       in case buildNerveCochainArtifact (ExplicitSiteCoboundary interfaceStalkBasisLinearization) Right (MaterializedSite localSite) of
            Left _ ->
              invalidComponent
            Right cache ->
              case computeNerveMicrosupportEnrichment nodeProjection localSite cache of
                Left _ ->
                  invalidComponent
                Right enrichment ->
                  ComponentMicrosupport
                    { cmsLiveValid = True,
                      cmsSeedCellKeys = seedCellKeys,
                      cmsActiveCellKeys = activeCellKeys,
                      cmsCriticalNodes = meCriticalNodes enrichment
                    }

activeCellKeysFrom ::
  Map CellKey (Set.Set CellKey) ->
  Set.Set CellKey ->
  Set.Set CellKey
activeCellKeysFrom parentsMap =
  reachableSetFromMany
    (AdjacencyMap.fromAdjacencySets (Map.toAscList parentsMap))

cellKeysToNodesBy :: Ord node => (CellKey -> Maybe node) -> Set.Set CellKey -> Set.Set node
cellKeysToNodesBy nodeProjection =
  Set.fromList . mapMaybe nodeProjection . Set.toAscList

nonCriticalNodesFromComponents ::
  Ord node =>
  (CellKey -> Maybe node) ->
  IntMap (ComponentMicrosupport node) ->
  Set.Set node
nonCriticalNodesFromComponents nodeProjection componentMap =
  let activeSeedNodes =
        foldComponentNodes
          activeSeedNodeSet
          componentMap
      criticalNodes =
        foldComponentNodes cmsCriticalNodes componentMap
   in Set.difference activeSeedNodes criticalNodes
  where
    activeSeedNodeSet componentState
      | cmsLiveValid componentState =
          cellKeysToNodesBy nodeProjection (cmsSeedCellKeys componentState)
      | otherwise =
          Set.empty

foldComponentNodes ::
  Ord node =>
  (ComponentMicrosupport node -> Set.Set node) ->
  IntMap (ComponentMicrosupport node) ->
  Set.Set node
foldComponentNodes project =
  IntMap.foldl' (\acc componentState -> Set.union acc (project componentState)) Set.empty

liveMicrosupportPruningEvidence ::
  LiveMicrosupport RegionNodeId ->
  [PruningEvidence]
liveMicrosupportPruningEvidence liveMicrosupport =
  let nonCriticalNodes =
        lmsNonCriticalNodes liveMicrosupport
   in [ MicrosupportNonCritical nonCriticalNodes
      | not (Set.null nonCriticalNodes)
      ]

pruningGatesFromLiveMicrosupport ::
  LiveMicrosupport RegionNodeId ->
  CohomologicalPruningGates root
pruningGatesFromLiveMicrosupport =
  buildPruningGates . liveMicrosupportPruningEvidence

weakComponentSetsFromAdjacency :: Ord vertex => Map vertex (Set.Set vertex) -> [Set.Set vertex]
weakComponentSetsFromAdjacency =
  strongComponentSets
    . AdjacencyMap.symmetricClosure
    . AdjacencyMap.fromAdjacencySets
    . Map.toAscList

strongComponentSets :: Ord vertex => AdjacencyMap.AdjacencyMap vertex -> [Set.Set vertex]
strongComponentSets graph =
  List.sortOn
    Set.lookupMin
    ( fmap
        (Set.fromList . NonEmpty.toList . NonEmptyAdjacencyMap.vertexList1)
        (AdjacencyMap.vertexList (AdjacencyMapAlgorithm.scc graph))
    )

reachableSetFromMany :: Ord vertex => AdjacencyMap.AdjacencyMap vertex -> Set.Set vertex -> Set.Set vertex
reachableSetFromMany graph seeds =
  Set.union seeds
    . Set.fromList
    . concatMap (AdjacencyMapAlgorithm.reachable graph)
    . Set.toAscList
    $ seeds

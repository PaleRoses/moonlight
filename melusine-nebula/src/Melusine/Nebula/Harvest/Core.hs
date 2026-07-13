{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Melusine.Nebula.Harvest.Core
  ( SupportGroupingError (..),
    renderSupportGroupingError,
    supportBucketIndex,
    supportGroups,
    supportGroupsFromIndex,
    HarvestState (..),
    SiteRow (..),
    buildHarvest,
    buildHarvestFromSections,
    advanceHarvestFromSections,
    siteRow,
    siteRowsByIdentity,
    siteBucketIndex,
    candidateSiteSupportGroups,
    candidateSiteSupportGroupsFromIndex,
    candidateSiteSupportGroupsIncremental,
    harvestIndexDelta,
    harvestDirtyBuckets,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.List (sort, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Graph qualified as Graph
import Data.Tree (flatten)
import Melusine.Nebula.Discovery.Choose
  ( BindingHarvestRow (..),
    CandidateSite (..),
    ChosenBinding (..),
    NebulaSizeExtractionSections,
    ShapeBucket,
    assignCandidateOrdinals,
    bindingHarvestRows,
    candidateSites,
    candidateSitesForBinding,
    chooseBindingRow,
    chooseBindings,
    harvestContexts,
    shapeBuckets,
    sizeExtractionSections,
  )
import Melusine.Nebula.Core (NebulaConfig (..), NebulaError (..))
import Melusine.Nebula.Harvest.Pairs
  ( PairLedger,
    SiteRow (..),
    advancePairLedger,
    buildPairLedger,
    siteRow,
    siteRowsByIdentity,
  )
import Melusine.Nebula.Source.Ingest (IngestedModule)
import Melusine.Nebula.Rewrite.Saturate (SaturatedModule, smContextGraph)
import Moonlight.Differential.Algebra.ZSet
  ( IndexedZSet,
    ZSet,
    indexedZSetEmpty,
    indexedZSetFold,
    indexedZSetInsert,
    zsetFromList,
    zsetToAscList,
  )
import Moonlight.Differential.Operator.Fixpoint
  ( SemiNaiveBudget (..),
    SemiNaiveDivergence (..),
    semiNaiveFixpoint,
  )
import Moonlight.EGraph.Introspection.Core.HsExpr (ScopeCtx)
import Numeric.Natural (Natural)


type SupportGroupingError :: Type
newtype SupportGroupingError = SupportGroupingDiverged Natural
  deriving stock (Eq, Ord, Show)

renderSupportGroupingError :: SupportGroupingError -> String
renderSupportGroupingError = \case
  SupportGroupingDiverged roundsSpent ->
    "shape support grouping diverged: " <> show roundsSpent

supportBucketIndex :: (Ord bucket, Ord row) => (site -> row) -> (site -> [bucket]) -> [site] -> IndexedZSet bucket row Int
supportBucketIndex rowOf bucketsOf =
  foldr indexSite indexedZSetEmpty
  where
    indexSite site indexedRows =
      foldr
        (\bucketValue acc -> indexedZSetInsert bucketValue (rowOf site) 1 acc)
        indexedRows
        (bucketsOf site)

supportGroups :: (Ord bucket, Ord row) => (site -> Int) -> (site -> row) -> (site -> [bucket]) -> [site] -> Either SupportGroupingError [[site]]
supportGroups ordinalOf rowOf bucketsOf sites =
  supportGroupsFromIndex (supportSitesByRow rowOf sites) ordinalOf (supportBucketIndex rowOf bucketsOf sites)

supportGroupsFromIndex :: forall bucket row site. (Ord bucket, Ord row) => Map.Map row [site] -> (site -> Int) -> IndexedZSet bucket row Int -> Either SupportGroupingError [[site]]
supportGroupsFromIndex rowSites ordinalOf bucketIndex =
  Right groupsFromComponents
  where
    rows :: [row]
    rows =
      Map.keys rowSites

    bucketMembers :: Map.Map bucket (Set.Set row)
    bucketMembers =
      indexedZSetFold
        (\acc bucketValue rowWeights -> Map.insert bucketValue (positiveRowsInZSet rowWeights) acc)
        Map.empty
        bucketIndex

    positiveRowsInZSet :: ZSet row Int -> Set.Set row
    positiveRowsInZSet rowWeights =
      Set.fromList
        [ rowValue
        | (rowValue, weight) <- zsetToAscList rowWeights,
          weight > 0
        ]

    rowIds :: Map.Map row Int
    rowIds =
      Map.fromList (zip rows [0 ..])

    rowsById :: IntMap row
    rowsById =
      IntMap.fromList [(rowId, rowValue) | (rowValue, rowId) <- Map.toAscList rowIds]

    rowIdFor :: row -> Maybe Int
    rowIdFor rowValue =
      Map.lookup rowValue rowIds

    adjacency :: IntMap IntSet
    adjacency =
      IntMap.fromListWith
        IntSet.union
        [ (rowId, memberIds)
        | members <- Map.elems bucketMembers,
          let memberIds = IntSet.fromList (mapMaybe rowIdFor (Set.toAscList members)),
          rowId <- IntSet.toList memberIds
        ]

    groupsFromComponents :: [[site]]
    groupsFromComponents =
      sortOn supportGroupMinOrdinal $
        mapMaybe groupFromRowIds componentRowIds
      where
        componentRowIds :: [[Int]]
        componentRowIds
          | null rows =
              []
          | otherwise =
              fmap (sort . flatten) (Graph.components rowGraph)

        rowGraph :: Graph.Graph
        rowGraph =
          Graph.buildG (0, length rows - 1) adjacencyEdges

        adjacencyEdges :: [(Int, Int)]
        adjacencyEdges =
          [ (rowId, neighborId)
          | (rowId, neighbors) <- IntMap.toAscList adjacency,
            neighborId <- IntSet.toList neighbors
          ]

    groupFromRowIds :: [Int] -> Maybe [site]
    groupFromRowIds rowIdValues =
      nonEmptySortedSites
        [ site
        | rowId <- rowIdValues,
          rowValue <- maybe [] pure (IntMap.lookup rowId rowsById),
          site <- Map.findWithDefault [] rowValue rowSites
        ]

    nonEmptySortedSites :: [site] -> Maybe [site]
    nonEmptySortedSites sites =
      case sortOn ordinalOf sites of
        [] -> Nothing
        sortedSites -> Just sortedSites

    supportGroupMinOrdinal :: [site] -> Int
    supportGroupMinOrdinal groupSites =
      maybe maxBound id (minimumMaybe (fmap ordinalOf groupSites))

supportSitesByRow :: Ord row => (site -> row) -> [site] -> Map.Map row [site]
supportSitesByRow rowOf sites =
  Map.fromListWith (<>) [(rowOf site, [site]) | site <- sites]

type HarvestState :: Type
data HarvestState = HarvestState
  { hsSections :: !NebulaSizeExtractionSections,
    hsBindings :: ![ChosenBinding],
    hsSites :: ![CandidateSite],
    hsBucketIndex :: !(IndexedZSet ShapeBucket SiteRow Int),
    hsGroups :: ![[CandidateSite]],
    hsPairs :: !PairLedger
  }

type BindingHarvestSlice :: Type
data BindingHarvestSlice = BindingHarvestSlice
  { bhsBinding :: !ChosenBinding,
    bhsSites :: ![CandidateSite],
    bhsPreviousSites :: ![CandidateSite],
    bhsChanged :: !Bool
  }

buildHarvest ::
  NebulaConfig ->
  IngestedModule ->
  SaturatedModule ->
  Either NebulaError HarvestState
buildHarvest config ingested saturated =
  buildHarvestFromSections config ingested saturated sections
  where
    sections = sizeExtractionSections config (smContextGraph saturated) (harvestContexts ingested)

buildHarvestFromSections ::
  NebulaConfig ->
  IngestedModule ->
  SaturatedModule ->
  NebulaSizeExtractionSections ->
  Either NebulaError HarvestState
buildHarvestFromSections config ingested saturated sections = do
  bindings <- chooseBindings config ingested saturated sections
  sites <- candidateSites config ingested saturated bindings sections
  let bucketIndex = siteBucketIndex sites
  groups <- candidateSiteSupportGroupsFromIndex (siteRowsByIdentity sites) bucketIndex
  pure
    HarvestState
      { hsSections = sections,
        hsBindings = bindings,
        hsSites = sites,
        hsBucketIndex = bucketIndex,
        hsGroups = groups,
        hsPairs = buildPairLedger (ncAntiUnifyMaxPairs config) groups
      }

advanceHarvestFromSections ::
  NebulaConfig ->
  IngestedModule ->
  SaturatedModule ->
  NebulaSizeExtractionSections ->
  Set.Set ScopeCtx ->
  HarvestState ->
  Either NebulaError (IndexedZSet ShapeBucket SiteRow Int, HarvestState)
advanceHarvestFromSections config ingested saturated sections dirtyContexts previousHarvest = do
  let rows =
        bindingHarvestRows ingested
      previousBindings =
        previousBindingsByName previousHarvest
      previousSites =
        previousSitesByName previousHarvest
  slices <-
    traverse
      (advanceBindingHarvestSlice config saturated sections dirtyContexts previousBindings previousSites)
      rows
  let bindings =
        fmap bhsBinding slices
      sites =
        assignCandidateOrdinals (foldMap bhsSites slices)
      changedSlices =
        filter bhsChanged slices
      siteDelta =
        harvestIndexDelta
          (siteBucketIndex (foldMap bhsPreviousSites changedSlices))
          (siteBucketIndex (foldMap bhsSites changedSlices))
      bucketIndex =
        hsBucketIndex previousHarvest <> siteDelta
      changedRows =
        Set.fromList (fmap siteRow (foldMap bhsPreviousSites changedSlices <> foldMap bhsSites changedSlices))
      dirtyBuckets =
        harvestDirtyBuckets siteDelta
  (groups, unaffectedGroups, affectedGroups) <-
    candidateSiteSupportGroupsIncremental
      (siteRowsByIdentity sites)
      (hsGroups previousHarvest)
      (hsBucketIndex previousHarvest)
      bucketIndex
      dirtyBuckets
      changedRows
  pure
    ( siteDelta,
      HarvestState
        { hsSections = sections,
          hsBindings = bindings,
          hsSites = sites,
          hsBucketIndex = bucketIndex,
          hsGroups = groups,
          hsPairs =
            advancePairLedger
              (ncAntiUnifyMaxPairs config)
              unaffectedGroups
              affectedGroups
              (hsPairs previousHarvest)
        }
    )

advanceBindingHarvestSlice ::
  NebulaConfig ->
  SaturatedModule ->
  NebulaSizeExtractionSections ->
  Set.Set ScopeCtx ->
  Map.Map String ChosenBinding ->
  Map.Map String [CandidateSite] ->
  BindingHarvestRow ->
  Either NebulaError BindingHarvestSlice
advanceBindingHarvestSlice config saturated sections dirtyContexts previousBindings previousSites bindingRow =
  case (bindingIsDirty dirtyContexts bindingRow, previousBinding, previousSiteRows) of
    (False, Just binding, Just sites) ->
      Right
        BindingHarvestSlice
          { bhsBinding = binding,
            bhsSites = stripSiteOrdinals sites,
            bhsPreviousSites = stripSiteOrdinals sites,
            bhsChanged = False
          }
    _ -> do
      binding <- chooseBindingRow config saturated sections bindingRow
      sites <-
        candidateSitesForBinding config saturated sections bindingRow binding
      Right
        BindingHarvestSlice
          { bhsBinding = binding,
            bhsSites = sites,
            bhsPreviousSites = foldMap stripSiteOrdinals previousSiteRows,
            bhsChanged = True
          }
  where
    previousBinding =
      Map.lookup (bhrName bindingRow) previousBindings

    previousSiteRows =
      Map.lookup (bhrName bindingRow) previousSites

bindingIsDirty :: Set.Set ScopeCtx -> BindingHarvestRow -> Bool
bindingIsDirty dirtyContexts bindingRow =
  not (Set.null (Set.intersection dirtyContexts (bhrContexts bindingRow)))

previousBindingsByName :: HarvestState -> Map.Map String ChosenBinding
previousBindingsByName =
  Map.fromList . fmap (\binding -> (cbName binding, binding)) . hsBindings

previousSitesByName :: HarvestState -> Map.Map String [CandidateSite]
previousSitesByName =
  Map.map (sortOn csOrdinal)
    . Map.fromListWith (<>)
    . fmap (\site -> (csBindingName site, [site]))
    . hsSites

stripSiteOrdinals :: [CandidateSite] -> [CandidateSite]
stripSiteOrdinals =
  fmap (\site -> site {csOrdinal = -1})

siteBucketIndex :: [CandidateSite] -> IndexedZSet ShapeBucket SiteRow Int
siteBucketIndex =
  foldr indexSite indexedZSetEmpty
  where
    indexSite :: CandidateSite -> IndexedZSet ShapeBucket SiteRow Int -> IndexedZSet ShapeBucket SiteRow Int
    indexSite site indexedRows =
      foldr
        (\bucketValue acc -> indexedZSetInsert bucketValue (siteRow site) 1 acc)
        indexedRows
        (shapeBuckets site)


harvestIndexDelta ::
  IndexedZSet ShapeBucket SiteRow Int ->
  IndexedZSet ShapeBucket SiteRow Int ->
  IndexedZSet ShapeBucket SiteRow Int
harvestIndexDelta beforeIndex afterIndex =
  subtractIndex beforeIndex afterIndex
  where
    subtractIndex :: IndexedZSet ShapeBucket SiteRow Int -> IndexedZSet ShapeBucket SiteRow Int -> IndexedZSet ShapeBucket SiteRow Int
    subtractIndex oldIndex newIndex =
      indexedZSetFold subtractRows newIndex oldIndex

    subtractRows :: IndexedZSet ShapeBucket SiteRow Int -> ShapeBucket -> ZSet SiteRow Int -> IndexedZSet ShapeBucket SiteRow Int
    subtractRows acc bucketValue rowWeights =
      foldr
        (\(rowValue, weight) indexedRows -> indexedZSetInsert bucketValue rowValue (negate weight) indexedRows)
        acc
        (zsetToAscList rowWeights)

harvestDirtyBuckets :: IndexedZSet ShapeBucket SiteRow Int -> Set.Set ShapeBucket
harvestDirtyBuckets =
  indexedZSetFold (\acc bucketValue _rowWeights -> Set.insert bucketValue acc) Set.empty

candidateSiteSupportGroups :: [CandidateSite] -> Either NebulaError [[CandidateSite]]
candidateSiteSupportGroups sites =
  candidateSiteSupportGroupsFromIndex (siteRowsByIdentity sites) (siteBucketIndex sites)

candidateSiteSupportGroupsFromIndex ::
  Map.Map SiteRow [CandidateSite] ->
  IndexedZSet ShapeBucket SiteRow Int ->
  Either NebulaError [[CandidateSite]]
candidateSiteSupportGroupsFromIndex rowSites bucketIndex =
  case supportGroupsFromIndex rowSites csOrdinal bucketIndex of
    Left groupingError ->
      Left (NebulaSynthesisError (renderSupportGroupingError groupingError))
    Right groups ->
      Right groups

candidateSiteSupportGroupsIncremental ::
  Map.Map SiteRow [CandidateSite] ->
  [[CandidateSite]] ->
  IndexedZSet ShapeBucket SiteRow Int ->
  IndexedZSet ShapeBucket SiteRow Int ->
  Set.Set ShapeBucket ->
  Set.Set SiteRow ->
  Either NebulaError ([[CandidateSite]], [[CandidateSite]], [[CandidateSite]])
candidateSiteSupportGroupsIncremental rowSites previousGroups previousIndex nextIndex dirtyBuckets changedRows
  | Set.null dirtyBuckets && Set.null changedRows =
      Right (previousGroups, previousGroups, [])
  | otherwise = do
      affectedRows <- affectedRowsForDelta previousIndex nextIndex dirtyBuckets changedRows
      affectedGroups <-
        candidateSiteSupportGroupsFromIndex
          (restrictSitesByRows affectedRows rowSites)
          (restrictIndexRows affectedRows nextIndex)
      let unaffectedGroups =
            fmap (refreshGroupSites rowSites) $
              filter (not . groupIntersectsRows affectedRows) previousGroups
      pure
        ( sortOn groupMinOrdinal (unaffectedGroups <> affectedGroups),
          unaffectedGroups,
          affectedGroups
        )

refreshGroupSites :: Map.Map SiteRow [CandidateSite] -> [CandidateSite] -> [CandidateSite]
refreshGroupSites rowSites groupSites =
  sortOn csOrdinal $
    foldMap
      (\rowValue -> Map.findWithDefault [] rowValue rowSites)
      (Set.toAscList (Set.fromList (fmap siteRow groupSites)))

affectedRowsForDelta ::
  IndexedZSet ShapeBucket SiteRow Int ->
  IndexedZSet ShapeBucket SiteRow Int ->
  Set.Set ShapeBucket ->
  Set.Set SiteRow ->
  Either NebulaError (Set.Set SiteRow)
affectedRowsForDelta previousIndex nextIndex dirtyBuckets changedRows = do
  let previousBucketMembers =
        bucketMembersFromIndex previousIndex
      nextBucketMembers =
        bucketMembersFromIndex nextIndex
      seedRows =
        changedRows
          <> rowsInBuckets previousBucketMembers dirtyBuckets
          <> rowsInBuckets nextBucketMembers dirtyBuckets
  previousAffected <- connectedRowsFromSeeds previousBucketMembers seedRows
  nextAffected <- connectedRowsFromSeeds nextBucketMembers (seedRows <> previousAffected)
  pure (previousAffected <> nextAffected)

bucketMembersFromIndex :: IndexedZSet ShapeBucket SiteRow Int -> Map.Map ShapeBucket (Set.Set SiteRow)
bucketMembersFromIndex =
  indexedZSetFold
    (\acc bucketValue rowWeights -> Map.insert bucketValue (positiveRows rowWeights) acc)
    Map.empty

positiveRows :: ZSet SiteRow Int -> Set.Set SiteRow
positiveRows rowWeights =
  Set.fromList
    [ rowValue
    | (rowValue, weight) <- zsetToAscList rowWeights,
      weight > 0
    ]

rowsInBuckets :: Map.Map ShapeBucket (Set.Set SiteRow) -> Set.Set ShapeBucket -> Set.Set SiteRow
rowsInBuckets bucketMembers dirtyBuckets =
  Set.unions
    [ Map.findWithDefault Set.empty bucketValue bucketMembers
    | bucketValue <- Set.toAscList dirtyBuckets
    ]

connectedRowsFromSeeds ::
  Map.Map ShapeBucket (Set.Set SiteRow) ->
  Set.Set SiteRow ->
  Either NebulaError (Set.Set SiteRow)
connectedRowsFromSeeds bucketMembers seedRows
  | Set.null seedRows =
      Right Set.empty
  | otherwise =
      either
        (Left . NebulaSynthesisError . ("shape support affected-row closure diverged: " <>) . show . sndRoundsSpent)
        (Right . zsetRows)
        (semiNaiveFixpoint closureBudget closureStep closureSeed)
  where
    rowBuckets =
      rowBucketsFromBucketMembers bucketMembers

    closureBudget =
      SemiNaiveBudget (fromIntegral (max 1 (Map.size rowBuckets)))

    closureSeed =
      zsetFromList [(rowValue, 1) | rowValue <- Set.toAscList seedRows]

    closureStep frontier =
      zsetFromList
        [ (neighborRow, 1)
        | (rowValue, _weight) <- zsetToAscList frontier,
          bucketValue <- Set.toAscList (Map.findWithDefault Set.empty rowValue rowBuckets),
          neighborRow <- Set.toAscList (Map.findWithDefault Set.empty bucketValue bucketMembers)
        ]

rowBucketsFromBucketMembers :: Map.Map ShapeBucket (Set.Set SiteRow) -> Map.Map SiteRow (Set.Set ShapeBucket)
rowBucketsFromBucketMembers bucketMembers =
  Map.fromListWith
    Set.union
    [ (rowValue, Set.singleton bucketValue)
    | (bucketValue, rowValues) <- Map.toAscList bucketMembers,
      rowValue <- Set.toAscList rowValues
    ]

zsetRows :: ZSet SiteRow Int -> Set.Set SiteRow
zsetRows =
  Set.fromList . fmap fst . zsetToAscList

restrictSitesByRows :: Set.Set SiteRow -> Map.Map SiteRow [CandidateSite] -> Map.Map SiteRow [CandidateSite]
restrictSitesByRows rows =
  Map.filterWithKey (\rowValue _sites -> Set.member rowValue rows)

restrictIndexRows ::
  Set.Set SiteRow ->
  IndexedZSet ShapeBucket SiteRow Int ->
  IndexedZSet ShapeBucket SiteRow Int
restrictIndexRows rows =
  indexedZSetFold restrictBucket indexedZSetEmpty
  where
    restrictBucket acc bucketValue rowWeights =
      foldr
        ( \(rowValue, weight) indexedRows ->
            if Set.member rowValue rows && weight > 0
              then indexedZSetInsert bucketValue rowValue weight indexedRows
              else indexedRows
        )
        acc
        (zsetToAscList rowWeights)

groupIntersectsRows :: Set.Set SiteRow -> [CandidateSite] -> Bool
groupIntersectsRows rows =
  any (\site -> Set.member (siteRow site) rows)

groupMinOrdinal :: [CandidateSite] -> Int
groupMinOrdinal groupSites =
  maybe maxBound id (minimumMaybe (fmap csOrdinal groupSites))

minimumMaybe :: Ord a => [a] -> Maybe a
minimumMaybe =
  foldr step Nothing
  where
    step :: Ord a => a -> Maybe a -> Maybe a
    step nextValue maybeBest =
      Just (maybe nextValue (min nextValue) maybeBest)

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- | A persistent union-find whose parent edges hold on exact context regions.
--
-- Different parent regions for one child are disjoint. Every stored parent key
-- is strictly smaller than its child key, so a regional find is total without
-- ranks or a visited set. Where no stored edge is active, the class is its own
-- parent.
module Moonlight.EGraph.Pure.Context.Internal.RegionalUnionFind
  ( RegionalUnionFind,
    RegionalRootPartition,
    RegionalUnionResult,
    RegionalUnionFindMetrics (..),
    RegionalUnionFindObstruction (..),
    emptyRegionalUnionFind,
    regionalFindAt,
    regionalFindPartition,
    regionalRootRegions,
    regionalUnion,
    regionalUnionResultForest,
    regionalUnionResultChangedRegionsByClassKey,
    regionalEquivalentRegion,
    regionalCompress,
    regionalRepresentativeMapAt,
    regionalChangedClassKeys,
    regionalActiveRegion,
    regionalUnionFindMetrics,
    validateRegionalUnionFind,
  )
where

import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.Kind
  ( Type,
  )
import Data.List.NonEmpty
  ( NonEmpty,
  )
import Data.List.NonEmpty qualified as NonEmpty
import Data.IntSet (IntSet)
import Moonlight.EGraph.Pure.Types
  ( ClassId (ClassId),
    classIdKey,
  )
import Moonlight.Sheaf.Context.Region
  ( ContextRegion,
    RegionTable,
    regionCubeCount,
    regionDifference,
    regionEmpty,
    regionEntails,
    regionJoin,
    regionMeet,
    regionMemberKey,
    regionTop,
    regionVoid,
  )
import Moonlight.Sheaf.Context.Site
  ( ContextObjectKey,
  )

type RegionalUnionFind :: Type -> Type
newtype RegionalUnionFind owner = RegionalUnionFind
  { regionalParentRegionsByChildKey :: IntMap (IntMap (ContextRegion owner))
  }
  deriving stock (Eq, Show)

type role RegionalUnionFind nominal

type RegionalRootPartition :: Type -> Type
newtype RegionalRootPartition owner = RegionalRootPartition
  { regionalRootRegions :: IntMap (ContextRegion owner)
  }
  deriving stock (Eq, Show)

type role RegionalRootPartition nominal

type RegionalUnionResult :: Type -> Type
data RegionalUnionResult owner = RegionalUnionResult
  { regionalUnionResultForest :: !(RegionalUnionFind owner),
    -- | Direct parent-section changes only. Callers that need structural
    -- consequences expand these roots through their own dependency index.
    regionalUnionResultChangedRegionsByClassKey :: !(IntMap (ContextRegion owner))
  }
  deriving stock (Eq, Show)

type role RegionalUnionResult nominal

type RegionalUnionFindMetrics :: Type
data RegionalUnionFindMetrics = RegionalUnionFindMetrics
  { regionalUnionFindChildCount :: !Int,
    regionalUnionFindParentEdgeCount :: !Int,
    regionalUnionFindParentRegionCubeCount :: !Int
  }
  deriving stock (Eq, Ord, Show)

type RegionalUnionFindObstruction :: Type -> Type
data RegionalUnionFindObstruction owner
  = RegionalIdentityParentStored !Int
  | RegionalParentDoesNotDescend !Int !Int
  | RegionalParentRegionEmpty !Int !Int
  | RegionalParentRegionsOverlap !Int !Int !Int !(ContextRegion owner)
  | RegionalRootPartitionRegionsOverlap !Int !Int !Int !(ContextRegion owner)
  | RegionalRootPartitionCoverageMismatch !Int !(ContextRegion owner) !(ContextRegion owner)
  deriving stock (Eq, Ord, Show)

type role RegionalUnionFindObstruction nominal

emptyRegionalUnionFind :: RegionalUnionFind owner
emptyRegionalUnionFind =
  RegionalUnionFind IntMap.empty
{-# INLINE emptyRegionalUnionFind #-}

regionalFindAt ::
  ContextObjectKey owner ->
  ClassId ->
  RegionalUnionFind owner ->
  ClassId
regionalFindAt contextObjectKey classId forest =
  ClassId (findRootKeyAt contextObjectKey (classIdKey classId) forest)
{-# INLINE regionalFindAt #-}

findRootKeyAt :: ContextObjectKey owner -> Int -> RegionalUnionFind owner -> Int
findRootKeyAt contextKey classKey forest =
  case activeParentKeyAt contextKey classKey forest of
    Nothing -> classKey
    Just parentKey -> findRootKeyAt contextKey parentKey forest

activeParentKeyAt :: ContextObjectKey owner -> Int -> RegionalUnionFind owner -> Maybe Int
activeParentKeyAt contextKey classKey forest =
  IntMap.foldlWithKey'
    selectActiveParent
    Nothing
    (parentRegionsFor classKey forest)
  where
    selectActiveParent selectedParent parentKey parentRegion =
      case selectedParent of
        Just _ -> selectedParent
        Nothing
          | regionMemberKey parentRegion contextKey -> Just parentKey
          | otherwise -> Nothing

regionalFindPartition ::
  RegionTable owner ->
  ContextRegion owner ->
  ClassId ->
  RegionalUnionFind owner ->
  RegionalRootPartition owner
regionalFindPartition table requestedRegion classId forest =
  RegionalRootPartition
    (findRootRegions table requestedRegion (classIdKey classId) forest)

findRootRegions ::
  RegionTable owner ->
  ContextRegion owner ->
  Int ->
  RegionalUnionFind owner ->
  IntMap (ContextRegion owner)
findRootRegions table requestedRegion classKey forest
  | regionEmpty requestedRegion = IntMap.empty
  | otherwise =
      IntMap.foldlWithKey'
        descendParentRegion
        identityPartition
        activeParentRegions
  where
    activeParentRegions =
      IntMap.mapMaybe
        (nonEmptyRegion . regionMeet requestedRegion)
        (parentRegionsFor classKey forest)

    explicitParentCoverage =
      IntMap.foldl' regionJoin regionVoid activeParentRegions

    identityRegion =
      regionDifference table requestedRegion explicitParentCoverage

    identityPartition
      | regionEmpty identityRegion = IntMap.empty
      | otherwise = IntMap.singleton classKey identityRegion

    descendParentRegion rootRegions parentKey parentRegion =
      IntMap.unionWith
        regionJoin
        rootRegions
        (findRootRegions table parentRegion parentKey forest)

regionalUnion ::
  RegionTable owner ->
  ContextRegion owner ->
  ClassId ->
  ClassId ->
  RegionalUnionFind owner ->
  RegionalUnionResult owner
regionalUnion table requestedRegion leftClassId rightClassId forest
  | regionEmpty requestedRegion = unchangedRegionalUnionResult forest
  | otherwise =
      IntMap.foldlWithKey'
        attachLeftRoot
        (unchangedRegionalUnionResult forest)
        leftRootRegions
  where
    leftRootRegions =
      regionalRootRegions
        (regionalFindPartition table requestedRegion leftClassId forest)

    rightRootRegions =
      regionalRootRegions
        (regionalFindPartition table requestedRegion rightClassId forest)

    attachLeftRoot unionResult leftRootKey leftRootRegion =
      IntMap.foldlWithKey'
        (attachRootOverlap leftRootKey leftRootRegion)
        unionResult
        rightRootRegions

    attachRootOverlap leftRootKey leftRootRegion unionResult rightRootKey rightRootRegion
      | leftRootKey == rightRootKey = unionResult
      | regionEmpty overlapRegion = unionResult
      | otherwise =
          let childKey = max leftRootKey rightRootKey
              parentKey = min leftRootKey rightRootKey
              advancedForest =
                installParentRegion table childKey parentKey overlapRegion
                  (regionalUnionResultForest unionResult)
              advancedChanges =
                IntMap.insertWith
                  regionJoin
                  childKey
                  overlapRegion
                  (regionalUnionResultChangedRegionsByClassKey unionResult)
           in RegionalUnionResult
                { regionalUnionResultForest = advancedForest,
                  regionalUnionResultChangedRegionsByClassKey = advancedChanges
                }
      where
        overlapRegion = regionMeet leftRootRegion rightRootRegion

regionalEquivalentRegion ::
  RegionTable owner ->
  ClassId ->
  ClassId ->
  RegionalUnionFind owner ->
  ContextRegion owner
regionalEquivalentRegion table leftClassId rightClassId forest =
  IntMap.foldlWithKey' collectEqualRootRegion regionVoid leftRootRegions
  where
    wholeRegion = regionTop table
    leftRootRegions =
      regionalRootRegions
        (regionalFindPartition table wholeRegion leftClassId forest)
    rightRootRegions =
      regionalRootRegions
        (regionalFindPartition table wholeRegion rightClassId forest)

    collectEqualRootRegion equivalentRegion rootKey leftRootRegion =
      case IntMap.lookup rootKey rightRootRegions of
        Nothing -> equivalentRegion
        Just rightRootRegion ->
          regionJoin equivalentRegion (regionMeet leftRootRegion rightRootRegion)

regionalCompress :: RegionTable owner -> RegionalUnionFind owner -> RegionalUnionFind owner
regionalCompress table forest =
  RegionalUnionFind
    ( IntMap.mapMaybeWithKey
        compressChild
        (regionalParentRegionsByChildKey forest)
    )
  where
    compressChild childKey parentRegions =
      let explicitCoverage = IntMap.foldl' regionJoin regionVoid parentRegions
          compressedRootRegions =
            regionalRootRegions
              (regionalFindPartition table explicitCoverage (ClassId childKey) forest)
          nonIdentityRoots = IntMap.delete childKey compressedRootRegions
       in nonEmptyParentRegions nonIdentityRoots

regionalRepresentativeMapAt ::
  ContextObjectKey owner ->
  RegionalUnionFind owner ->
  IntMap Int
regionalRepresentativeMapAt contextKey forest =
  IntMap.mapMaybeWithKey representativeWhenChanged
    (regionalParentRegionsByChildKey forest)
  where
    representativeWhenChanged childKey _ =
      let rootKey = classIdKey (regionalFindAt contextKey (ClassId childKey) forest)
       in if rootKey == childKey then Nothing else Just rootKey

-- | Base-canonical coordinates whose representative differs on at least one
-- region.  A structural row can change only when its result or one of its
-- children belongs to this sparse domain.
regionalChangedClassKeys :: RegionalUnionFind owner -> IntSet
regionalChangedClassKeys =
  IntMap.keysSet . regionalParentRegionsByChildKey
{-# INLINE regionalChangedClassKeys #-}

regionalActiveRegion :: RegionalUnionFind owner -> ContextRegion owner
regionalActiveRegion forest =
  IntMap.foldl'
    (\activeRegion -> IntMap.foldl' regionJoin activeRegion)
    regionVoid
    (regionalParentRegionsByChildKey forest)

regionalUnionFindMetrics :: RegionTable owner -> RegionalUnionFind owner -> RegionalUnionFindMetrics
regionalUnionFindMetrics table forest =
  RegionalUnionFindMetrics
    { regionalUnionFindChildCount = IntMap.size parentRegionsByChild,
      regionalUnionFindParentEdgeCount =
        IntMap.foldl' (\edgeCount parentRegions -> edgeCount + IntMap.size parentRegions) 0 parentRegionsByChild,
      regionalUnionFindParentRegionCubeCount =
        IntMap.foldl'
          ( IntMap.foldl'
              (\cubeCount parentRegion -> cubeCount + regionCubeCount table parentRegion)
          )
          0
          parentRegionsByChild
    }
  where
    parentRegionsByChild = regionalParentRegionsByChildKey forest

validateRegionalUnionFind ::
  RegionTable owner ->
  RegionalUnionFind owner ->
  Either (NonEmpty (RegionalUnionFindObstruction owner)) ()
validateRegionalUnionFind table forest =
  case NonEmpty.nonEmpty allObstructions of
    Nothing -> Right ()
    Just obstructions -> Left obstructions
  where
    parentRegionsByChild = regionalParentRegionsByChildKey forest
    localObstructions =
      IntMap.foldlWithKey'
        (\obstructions childKey parentRegions -> obstructions <> validateParentRegions childKey parentRegions)
        []
        parentRegionsByChild
    descentIsValid =
      all
        (\(childKey, parentKey) -> parentKey < childKey)
        ( IntMap.toList parentRegionsByChild
            >>= \(childKey, parentRegions) ->
              fmap ((,) childKey . fst) (IntMap.toList parentRegions)
        )
    partitionObstructions
      | descentIsValid =
          IntMap.foldlWithKey'
            (\obstructions childKey _ -> obstructions <> validateRootPartition table childKey forest)
            []
            parentRegionsByChild
      | otherwise = []
    allObstructions = localObstructions <> partitionObstructions

validateParentRegions :: Int -> IntMap (ContextRegion owner) -> [RegionalUnionFindObstruction owner]
validateParentRegions childKey parentRegions =
  concatMap validateParentRegion parentEntries
    <> foldMap validateParentRegionPair (unorderedPairs parentEntries)
  where
    parentEntries = IntMap.toAscList parentRegions

    validateParentRegion (parentKey, parentRegion) =
      [ RegionalIdentityParentStored childKey
        | parentKey == childKey
      ]
        <> [ RegionalParentDoesNotDescend childKey parentKey
             | parentKey > childKey
           ]
        <> [ RegionalParentRegionEmpty childKey parentKey
             | regionEmpty parentRegion
           ]

    validateParentRegionPair ((leftParentKey, leftRegion), (rightParentKey, rightRegion)) =
      [ RegionalParentRegionsOverlap
          childKey
          leftParentKey
          rightParentKey
          overlapRegion
        | let overlapRegion = regionMeet leftRegion rightRegion,
          not (regionEmpty overlapRegion)
      ]

validateRootPartition ::
  RegionTable owner ->
  Int ->
  RegionalUnionFind owner ->
  [RegionalUnionFindObstruction owner]
validateRootPartition table childKey forest =
  coverageObstructions <> overlapObstructions
  where
    requestedRegion = regionTop table
    rootRegions =
      regionalRootRegions
        (regionalFindPartition table requestedRegion (ClassId childKey) forest)
    actualCoverage = IntMap.foldl' regionJoin regionVoid rootRegions
    coverageObstructions =
      [ RegionalRootPartitionCoverageMismatch childKey requestedRegion actualCoverage
        | not (sameRegion requestedRegion actualCoverage)
      ]
    overlapObstructions =
      foldMap rootOverlapObstruction (unorderedPairs (IntMap.toAscList rootRegions))

    rootOverlapObstruction ((leftRootKey, leftRegion), (rightRootKey, rightRegion)) =
      [ RegionalRootPartitionRegionsOverlap
          childKey
          leftRootKey
          rightRootKey
          overlapRegion
        | let overlapRegion = regionMeet leftRegion rightRegion,
          not (regionEmpty overlapRegion)
      ]

parentRegionsFor :: Int -> RegionalUnionFind owner -> IntMap (ContextRegion owner)
parentRegionsFor classKey forest =
  IntMap.findWithDefault
    IntMap.empty
    classKey
    (regionalParentRegionsByChildKey forest)

installParentRegion ::
  RegionTable owner ->
  Int ->
  Int ->
  ContextRegion owner ->
  RegionalUnionFind owner ->
  RegionalUnionFind owner
installParentRegion table childKey parentKey installedRegion forest =
  RegionalUnionFind
    ( IntMap.alter
        replaceParentRegion
        childKey
        (regionalParentRegionsByChildKey forest)
    )
  where
    replaceParentRegion existingParentRegions =
      let residualParentRegions =
            IntMap.mapMaybe
              (nonEmptyRegion . flip (regionDifference table) installedRegion)
              (maybe IntMap.empty id existingParentRegions)
       in nonEmptyParentRegions
            ( IntMap.insertWith
                regionJoin
                parentKey
                installedRegion
                residualParentRegions
            )

unchangedRegionalUnionResult :: RegionalUnionFind owner -> RegionalUnionResult owner
unchangedRegionalUnionResult forest =
  RegionalUnionResult
    { regionalUnionResultForest = forest,
      regionalUnionResultChangedRegionsByClassKey = IntMap.empty
    }

nonEmptyRegion :: ContextRegion owner -> Maybe (ContextRegion owner)
nonEmptyRegion region
  | regionEmpty region = Nothing
  | otherwise = Just region

nonEmptyParentRegions :: IntMap (ContextRegion owner) -> Maybe (IntMap (ContextRegion owner))
nonEmptyParentRegions parentRegions
  | IntMap.null parentRegions = Nothing
  | otherwise = Just parentRegions

sameRegion :: ContextRegion owner -> ContextRegion owner -> Bool
sameRegion leftRegion rightRegion =
  regionEntails leftRegion rightRegion
    && regionEntails rightRegion leftRegion

unorderedPairs :: [a] -> [(a, a)]
unorderedPairs values =
  case values of
    [] -> []
    value : remainingValues ->
      fmap ((,) value) remainingValues <> unorderedPairs remainingValues

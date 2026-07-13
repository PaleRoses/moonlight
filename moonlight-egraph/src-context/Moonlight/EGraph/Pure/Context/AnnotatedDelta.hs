{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- | The authoritative derived contextual closure.
--
-- Authored fibers remain local sections. Their principal context regions are
-- descended into one region-labelled union-find and glued by one regional
-- congruence fixed point. Point queries are projections of that global
-- section; no context graph or per-context closure is materialized here.
module Moonlight.EGraph.Pure.Context.AnnotatedDelta
  ( AnnotatedDeltaBuckets,
    AnnotatedDeltaFrontier (..),
    AnnotatedDeltaMetrics (..),
    AnnotatedRow (..),
    RegionalClosureObstruction (..),
    RegionalUnionFindObstruction (..),
    emptyAnnotatedDeltaCache,
    freshAnnotatedDeltaCache,
    advanceAnnotatedDeltaCacheAtUnions,
    deriveAnnotatedDeltaBuckets,
    contextAnnotatedDeltaBuckets,
    contextAnnotatedDeltaDirtyFrontier,
    bucketFrontierBetween,
    appendAnnotatedDeltaFrontier,
    emptyAnnotatedDeltaFrontier,
    annotatedRowsAtKey,
    absorbedRowsAtKey,
    annotatedRowsByTagAt,
    absorbedRowsByTagAt,
    annotatedVariantRowsForTag,
    annotatedAbsorbedRowsForTag,
    annotatedRepresentativeKeyAt,
    annotatedRepresentativeMapAt,
    annotatedEquivalentRegion,
    annotatedInhabitedRegion,
    annotatedDeltaMetrics,
    annotatedDeltaFingerprint,
  )
where

import Data.Bifunctor (first)
import Data.Bits (xor)
import Data.Char (ord)
import Data.Foldable (toList)
import Data.Functor (void)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Traversable (mapAccumL)
import Moonlight.Core (ClassId (..), Language, classIdKey)
import Moonlight.EGraph.Pure.Context.Internal.RegionalUnionFind
  ( RegionalUnionFind,
    RegionalUnionFindMetrics (..),
    RegionalUnionFindObstruction (..),
    emptyRegionalUnionFind,
    regionalActiveRegion,
    regionalCompress,
    regionalChangedClassKeys,
    regionalEquivalentRegion,
    regionalFindAt,
    regionalFindPartition,
    regionalRepresentativeMapAt,
    regionalRootRegions,
    regionalUnion,
    regionalUnionFindMetrics,
    regionalUnionResultChangedRegionsByClassKey,
    regionalUnionResultForest,
  )
import Moonlight.EGraph.Pure.Context.Internal.Store
  ( AnnotatedDeltaBuckets,
    AnnotatedDeltaCache (..),
    AnnotatedDeltaFrontier (..),
    AnnotatedRow (..),
    ContextEGraph,
  )
import Moonlight.EGraph.Pure.Context.Internal.Store qualified as Store
import Moonlight.EGraph.Pure.Structural.Store
  ( StructuralLookup (..),
    StructuralStore,
    structuralLookupTupleAll,
    structuralParentKeysOf,
    structuralRowBucketForTag,
    structuralTuplesForResultKey,
  )
import Moonlight.EGraph.Pure.Types (ENode (..), eGraphStore)
import Moonlight.EGraph.Pure.Types qualified as EGraph
import Moonlight.Sheaf.Context.Region
  ( ContextRegion,
    RegionTable,
    regionCubeCount,
    regionEmpty,
    regionGeneratorKeys,
    regionJoin,
    regionMeet,
    regionMemberKey,
    regionSize,
    regionVoid,
  )
import Moonlight.Sheaf.Context.Site
  ( ContextObjectKey (..),
    contextObjectKeyFor,
    preparedRegionAt,
    preparedRegionTable,
  )
import Moonlight.Sheaf.Section.Congruence.Equivalence.Relation
  ( equivalencePairs,
  )
import Numeric.Natural (Natural)

type RegionalClosureObstruction :: Type -> Type
data RegionalClosureObstruction c
  = RegionalClosureAuthorContextMissing !c
  | RegionalClosureActiveContextMissing !c
  | RegionalClosureContextRevisionMismatch !Natural !Natural
  deriving stock (Eq, Ord, Show)

type AnnotatedDeltaMetrics :: Type
data AnnotatedDeltaMetrics = AnnotatedDeltaMetrics
  { annotatedDeltaParentChildCount :: !Int,
    annotatedDeltaParentEdgeCount :: !Int,
    annotatedDeltaParentRegionCubeCount :: !Int,
    annotatedDeltaVariantRowCount :: !Int,
    annotatedDeltaAbsorbedRowCount :: !Int
  }
  deriving stock (Eq, Ord, Show)

emptyAnnotatedDeltaFrontier :: AnnotatedDeltaFrontier f
emptyAnnotatedDeltaFrontier =
  Store.AnnotatedDeltaFrontier
    { Store.adfRepresentativeKeys = IntSet.empty,
      Store.adfVariantRowsByTag = Map.empty,
      Store.adfAbsorbedRowsByTag = Map.empty
    }

emptyAnnotatedDeltaBuckets :: AnnotatedDeltaBuckets f
emptyAnnotatedDeltaBuckets =
  Store.AnnotatedDeltaBuckets
    { Store.adbVariantRowsByTag = Map.empty,
      Store.adbAbsorbedRowsByTag = Map.empty,
      Store.adbRegionalUnionFind = emptyRegionalUnionFind,
      Store.adbInhabitedRegion = regionVoid
    }

emptyAnnotatedDeltaCache ::
  EGraph.EGraph f a ->
  Natural ->
  AnnotatedDeltaCache f
emptyAnnotatedDeltaCache graph contextRevision =
  Store.AnnotatedDeltaCache
    { Store.adcBaseRevision = EGraph.eGraphRevision graph,
      Store.adcContextRevision = contextRevision,
      Store.adcBuckets = emptyAnnotatedDeltaBuckets,
      Store.adcDirtyFrontierByContextKey = IntMap.empty
    }

freshAnnotatedDeltaCache ::
  (Language f, Ord c) =>
  ContextEGraph f a c ->
  Either (RegionalClosureObstruction c) (AnnotatedDeltaCache f)
freshAnnotatedDeltaCache contextGraph = do
  buckets <- deriveAnnotatedDeltaBuckets contextGraph
  pure
    Store.AnnotatedDeltaCache
      { Store.adcBaseRevision = EGraph.eGraphRevision (Store.cegBase contextGraph),
        Store.adcContextRevision = Store.cegContextRevision contextGraph,
        Store.adcBuckets = buckets,
        Store.adcDirtyFrontierByContextKey = IntMap.empty
      }

-- | Every observable contextual graph carries a revision-matching cache.
-- Cache compilation belongs to the typed update boundary, never to a read.
contextAnnotatedDeltaBuckets :: ContextEGraph f a c -> AnnotatedDeltaBuckets f
contextAnnotatedDeltaBuckets =
  Store.adcBuckets . Store.cegAnnotatedDeltaCache
{-# INLINE contextAnnotatedDeltaBuckets #-}

contextAnnotatedDeltaDirtyFrontier ::
  ContextEGraph f a c ->
  IntMap (AnnotatedDeltaFrontier f)
contextAnnotatedDeltaDirtyFrontier =
  Store.adcDirtyFrontierByContextKey . Store.cegAnnotatedDeltaCache
{-# INLINE contextAnnotatedDeltaDirtyFrontier #-}

-- | Compile all authored local sections into one regional congruence section.
deriveAnnotatedDeltaBuckets ::
  (Language f, Ord c) =>
  ContextEGraph f a c ->
  Either (RegionalClosureObstruction c) (AnnotatedDeltaBuckets f)
deriveAnnotatedDeltaBuckets contextGraph = do
  seeds <- authoredRegionalSeeds contextGraph
  let regionTable = preparedRegionTable (Store.cegSite contextGraph)
      structuralStore = eGraphStore (Store.cegBase contextGraph)
      seededForest = foldl' (applyRegionalSeed regionTable) emptyRegionalUnionFind seeds
      inhabitedRegion = foldl' seedInhabitedRegion regionVoid seeds
      closedForest = regionalCongruenceClosure regionTable structuralStore seededForest
      compressedForest = regionalCompress regionTable closedForest
  bucketsFromRegionalForest regionTable structuralStore compressedForest inhabitedRegion

-- | Advance the compiled regional section with a transaction of newly
-- authored equalities. The existing closed forest is the version predecessor;
-- equal pairs are first glued across their author regions, then congruence is
-- restored once for the entire transaction.
-- Authored fibers remain the source of truth for a later base-revision
-- recompile, but staging never replays their complete history.
advanceAnnotatedDeltaCacheAtUnions ::
  (Language f, Ord c) =>
  [(c, [(ClassId, ClassId)])] ->
  ContextEGraph f a c ->
  Either (RegionalClosureObstruction c) (AnnotatedDeltaCache f)
advanceAnnotatedDeltaCacheAtUnions authoredUnionsByContext contextGraph = do
  transactionSeeds <-
    traverse
      ( \(authorContext, classPairs) -> do
          authoredRegion <-
            first
              (const (RegionalClosureAuthorContextMissing authorContext))
              (preparedRegionAt (Store.cegSite contextGraph) authorContext)
          pure
            RegionalSeed
              { rsRegion = authoredRegion,
                rsPairs = classPairs
              }
      )
      authoredUnionsByContext
  activeContextKeys <-
    traverse
      ( \contextValue ->
          first
            (const (RegionalClosureActiveContextMissing contextValue))
            (contextObjectKeyFor (Store.cegSite contextGraph) contextValue)
      )
      (Map.keys (Store.cegContextAnalysisDeltas contextGraph))
  let cache = Store.cegAnnotatedDeltaCache contextGraph
      oldBuckets = Store.adcBuckets cache
      regionTable = preparedRegionTable (Store.cegSite contextGraph)
      structuralStore = eGraphStore (Store.cegBase contextGraph)
      seededForest =
        Map.foldlWithKey'
          ( \forest (leftClass, rightClass) authoredRegion ->
              regionalUnionResultForest
                (regionalUnion regionTable authoredRegion leftClass rightClass forest)
          )
          (Store.adbRegionalUnionFind oldBuckets)
          (combinedSeedRegions transactionSeeds)
      closedForest = regionalCongruenceClosure regionTable structuralStore seededForest
      compressedForest = regionalCompress regionTable closedForest
      inhabitedRegion = foldl' seedInhabitedRegion (Store.adbInhabitedRegion oldBuckets) transactionSeeds
  newBuckets <-
    bucketsFromRegionalForest
      regionTable
      structuralStore
      compressedForest
      inhabitedRegion
  let stepFrontier =
        bucketFrontierBetween
          [contextKey | ContextObjectKey contextKey <- activeContextKeys]
          oldBuckets
          newBuckets
      accumulatedFrontier =
        IntMap.unionWith
          appendAnnotatedDeltaFrontier
          (Store.adcDirtyFrontierByContextKey cache)
          stepFrontier
  pure
    cache
      { Store.adcContextRevision = Store.cegContextRevision contextGraph,
        Store.adcBuckets = newBuckets,
        Store.adcDirtyFrontierByContextKey = accumulatedFrontier
      }

combinedSeedRegions :: [RegionalSeed] -> Map (ClassId, ClassId) ContextRegion
combinedSeedRegions =
  foldl' combineSeed Map.empty
  where
    combineSeed regionsByPair seed =
      foldl'
        ( \currentRegions (leftClass, rightClass) ->
            Map.insertWith
              regionJoin
              (min leftClass rightClass, max leftClass rightClass)
              (rsRegion seed)
              currentRegions
        )
        regionsByPair
        (rsPairs seed)

bucketsFromRegionalForest ::
  Language f =>
  RegionTable ->
  StructuralStore f ->
  RegionalUnionFind ->
  ContextRegion ->
  Either (RegionalClosureObstruction c) (AnnotatedDeltaBuckets f)
bucketsFromRegionalForest regionTable structuralStore forest inhabitedRegion =
  Right
    Store.AnnotatedDeltaBuckets
      { Store.adbVariantRowsByTag =
          groupRegionalRows
            [ (rrTag row, rrCanonicalRoot row, rrCanonicalChildren row, rrRegion row)
              | row <- changedRows,
                not (baseRowExists structuralStore (rrTag row) (rrCanonicalRoot row) (rrCanonicalChildren row))
            ],
        Store.adbAbsorbedRowsByTag =
          groupRegionalRows
            [ (rrTag row, rrBaseRoot row, rrBaseChildren row, rrRegion row)
              | row <- changedRows
            ],
        Store.adbRegionalUnionFind = forest,
        Store.adbInhabitedRegion = inhabitedRegion
      }
  where
    changedRows = regionalChangedRows regionTable structuralStore forest inhabitedRegion

type RegionalSeed :: Type
data RegionalSeed = RegionalSeed
  { rsRegion :: !ContextRegion,
    rsPairs :: ![(ClassId, ClassId)]
  }

authoredRegionalSeeds ::
  Ord c =>
  ContextEGraph f a c ->
  Either (RegionalClosureObstruction c) [RegionalSeed]
authoredRegionalSeeds contextGraph =
  traverse authoredSeed (Map.toAscList (Store.cegContextFibers contextGraph))
  where
    authoredSeed (contextValue, fiberValue) = do
      authoredRegion <-
        first
          (const (RegionalClosureAuthorContextMissing contextValue))
          (preparedRegionAt (Store.cegSite contextGraph) contextValue)
      pure
        RegionalSeed
          { rsRegion = authoredRegion,
            rsPairs = equivalencePairs (Store.cfRelation fiberValue)
          }

applyRegionalSeed :: RegionTable -> RegionalUnionFind -> RegionalSeed -> RegionalUnionFind
applyRegionalSeed regionTable forest seed =
  foldl'
    ( \currentForest (leftClass, rightClass) ->
        regionalUnionResultForest
          (regionalUnion regionTable (rsRegion seed) leftClass rightClass currentForest)
    )
    forest
    (rsPairs seed)

seedInhabitedRegion :: ContextRegion -> RegionalSeed -> ContextRegion
seedInhabitedRegion inhabitedRegion seed
  | null (rsPairs seed) = inhabitedRegion
  | otherwise = regionJoin inhabitedRegion (rsRegion seed)

type RegionalRow :: (Type -> Type) -> Type
data RegionalRow f = RegionalRow
  { rrTag :: !(f ()),
    rrBaseRoot :: !Int,
    rrBaseChildren :: ![Int],
    rrCanonicalRoot :: !Int,
    rrCanonicalChildren :: ![Int],
    rrCanonicalNode :: !(ENode f),
    rrRegion :: !ContextRegion
  }

regionalCongruenceClosure ::
  Language f =>
  RegionTable ->
  StructuralStore f ->
  RegionalUnionFind ->
  RegionalUnionFind
regionalCongruenceClosure regionTable structuralStore =
  refine
  where
    refine forest =
      let activeRegion = regionalForestRegion forest
          changedRows = regionalChangedRows regionTable structuralStore forest activeRegion
          collisionMerges = regionalCollisionMerges changedRows
          baseMerges = foldMap (regionalBaseMerges structuralStore) changedRows
          (advancedForest, changedRegions) =
            foldl'
              applyRegionalMerge
              (forest, IntMap.empty)
              (collisionMerges <> baseMerges)
       in if IntMap.null changedRegions || advancedForest == forest
            then forest
            else refine advancedForest

    applyRegionalMerge (forest, changedRegions) (mergeRegion, leftKey, rightKey) =
      let unionResult =
            regionalUnion
              regionTable
              mergeRegion
              (ClassId leftKey)
              (ClassId rightKey)
              forest
       in ( regionalUnionResultForest unionResult,
            IntMap.unionWith
              regionJoin
              changedRegions
              (regionalUnionResultChangedRegionsByClassKey unionResult)
          )

regionalForestRegion :: RegionalUnionFind -> ContextRegion
regionalForestRegion =
  -- The active edge coverage is precisely where representatives can differ.
  -- Kept local so the closure and final inhabited-region policy remain
  -- separate: redundant authored seeds still inhabit their author region.
  foldRegionalRepresentativeCoverage

foldRegionalRepresentativeCoverage :: RegionalUnionFind -> ContextRegion
foldRegionalRepresentativeCoverage =
  -- `regionalActiveRegion` intentionally hides representation while exposing
  -- this exact derived view.
  regionalActiveRegion

regionalChangedRows ::
  Language f =>
  RegionTable ->
  StructuralStore f ->
  RegionalUnionFind ->
  ContextRegion ->
  [RegionalRow f]
regionalChangedRows regionTable structuralStore forest activeRegion
  | regionEmpty activeRegion = []
  | otherwise =
      concatMap regionalFormsForBaseRow candidateEntries
  where
    changedClassKeys = regionalChangedClassKeys forest
    candidateResultKeys =
      IntSet.union
        changedClassKeys
        (structuralParentKeysOf structuralStore changedClassKeys)
    candidateEntries =
      [ (ClassId resultKey, enode)
        | resultKey <- IntSet.toAscList candidateResultKeys,
          enode <- structuralTuplesForResultKey resultKey structuralStore
      ]

    regionalFormsForBaseRow (ClassId baseRoot, ENode nodeShape) =
      let baseChildren = fmap classIdKey (toList nodeShape)
          rootChoices = representativeChoices baseRoot
          childChoices = fmap representativeChoices baseChildren
       in [ RegionalRow
              { rrTag = void nodeShape,
                rrBaseRoot = baseRoot,
                rrBaseChildren = baseChildren,
                rrCanonicalRoot = canonicalRoot,
                rrCanonicalChildren = canonicalChildren,
                rrCanonicalNode = ENode canonicalNodeShape,
                rrRegion = formRegion
              }
            | (canonicalChildren, childrenRegion) <- combineRegionalChoices activeRegion childChoices,
              (canonicalRoot, rootRegion) <- rootChoices,
              let formRegion = regionMeet childrenRegion rootRegion,
              not (regionEmpty formRegion),
              canonicalRoot /= baseRoot || canonicalChildren /= baseChildren,
              Just canonicalNodeShape <- [replaceNodeChildren nodeShape canonicalChildren]
          ]

    representativeChoices classKey =
      IntMap.toAscList
        ( regionalRootRegions
            ( regionalFindPartition
                regionTable
                activeRegion
                (ClassId classKey)
                forest
            )
        )

combineRegionalChoices ::
  ContextRegion ->
  [[(Int, ContextRegion)]] ->
  [([Int], ContextRegion)]
combineRegionalChoices activeRegion =
  foldr combineOne [([], activeRegion)]
  where
    combineOne :: [(Int, ContextRegion)] -> [([Int], ContextRegion)] -> [([Int], ContextRegion)]
    combineOne choices accumulated =
      [ (choiceKey : remainingKeys, overlapRegion)
        | (choiceKey, choiceRegion) <- choices,
          (remainingKeys, remainingRegion) <- accumulated,
          let overlapRegion = regionMeet choiceRegion remainingRegion,
          not (regionEmpty overlapRegion)
      ]

replaceNodeChildren :: Traversable f => f ClassId -> [Int] -> Maybe (f ClassId)
replaceNodeChildren nodeShape childKeys =
  case mapAccumL replaceChild childKeys nodeShape of
    ([], taggedShape) -> either (const Nothing) Just (sequenceA taggedShape)
    (_ : _, _) -> Nothing
  where
    replaceChild :: [Int] -> ClassId -> ([Int], Either () ClassId)
    replaceChild remainingKeys _ =
      case remainingKeys of
        [] -> ([], Left ())
        nextKey : restKeys -> (restKeys, Right (ClassId nextKey))

type RegionalMerge :: Type
type RegionalMerge = (ContextRegion, Int, Int)

regionalCollisionMerges :: Ord (f ()) => [RegionalRow f] -> [RegionalMerge]
regionalCollisionMerges rows =
  foldMap collisionsForForm (Map.elems rowsByCanonicalForm)
  where
    rowsByCanonicalForm =
      Map.fromListWith
        (<>)
        [ ((rrTag row, rrCanonicalChildren row), [(rrCanonicalRoot row, rrRegion row)])
          | row <- rows
        ]

    collisionsForForm :: [(Int, ContextRegion)] -> [RegionalMerge]
    collisionsForForm rootRegions =
      [ (overlapRegion, leftRoot, rightRoot)
        | ((leftRoot, leftRegion), (rightRoot, rightRegion)) <- unorderedPairs rootRegions,
          leftRoot /= rightRoot,
          let overlapRegion = regionMeet leftRegion rightRegion,
          not (regionEmpty overlapRegion)
      ]

regionalBaseMerges ::
  Language f =>
  StructuralStore f ->
  RegionalRow f ->
  [RegionalMerge]
regionalBaseMerges structuralStore row =
  [ (rrRegion row, rrCanonicalRoot row, classIdKey ownerClass)
    | ownerClass <- structuralOwners (rrCanonicalNode row)
  ]
  where
    structuralOwners canonicalNode =
      case structuralLookupTupleAll canonicalNode structuralStore of
        StructuralMissing -> []
        StructuralUnique ownerClass -> [ownerClass]
        StructuralAmbiguous ownerClasses -> toList ownerClasses

baseRowExists ::
  Language f =>
  StructuralStore f ->
  f () ->
  Int ->
  [Int] ->
  Bool
baseRowExists structuralStore tag rootKey childKeys =
  maybe
    False
    (Set.member childKeys)
    (IntMap.lookup rootKey (structuralRowBucketForTag tag structuralStore))

groupRegionalRows ::
  Ord (f ()) =>
  [(f (), Int, [Int], ContextRegion)] ->
  Map (f ()) [AnnotatedRow]
groupRegionalRows rows =
  fmap
    ( \regionsByForm ->
        [ Store.AnnotatedRow
            { Store.arRootKey = rootKey,
              Store.arChildKeys = childKeys,
              Store.arRegion = rowRegion
            }
          | ((rootKey, childKeys), rowRegion) <- Map.toAscList regionsByForm
        ]
    )
    ( Map.fromListWith
        (Map.unionWith regionJoin)
        [ (tag, Map.singleton (rootKey, childKeys) rowRegion)
          | (tag, rootKey, childKeys, rowRegion) <- rows,
            not (regionEmpty rowRegion)
        ]
    )

bucketFrontierBetween ::
  Ord (f ()) =>
  [Int] ->
  AnnotatedDeltaBuckets f ->
  AnnotatedDeltaBuckets f ->
  IntMap (AnnotatedDeltaFrontier f)
bucketFrontierBetween contextKeys oldBuckets newBuckets =
  IntMap.fromList
    [ (contextKey, frontierValue)
      | contextKey <- contextKeys,
        let frontierValue = bucketFrontierAt contextKey oldBuckets newBuckets,
        not (annotatedDeltaFrontierNull frontierValue)
    ]

bucketFrontierAt ::
  Ord (f ()) =>
  Int ->
  AnnotatedDeltaBuckets f ->
  AnnotatedDeltaBuckets f ->
  AnnotatedDeltaFrontier f
bucketFrontierAt contextKey oldBuckets newBuckets =
  Store.AnnotatedDeltaFrontier
    { Store.adfRepresentativeKeys =
        repKeyDelta
          (annotatedRepresentativeMapAt (ContextObjectKey contextKey) oldBuckets)
          (annotatedRepresentativeMapAt (ContextObjectKey contextKey) newBuckets),
      Store.adfVariantRowsByTag =
        rowsByTagDelta contextKey (Store.adbVariantRowsByTag oldBuckets) (Store.adbVariantRowsByTag newBuckets),
      Store.adfAbsorbedRowsByTag =
        rowsByTagDelta contextKey (Store.adbAbsorbedRowsByTag oldBuckets) (Store.adbAbsorbedRowsByTag newBuckets)
    }

annotatedDeltaFrontierNull :: AnnotatedDeltaFrontier f -> Bool
annotatedDeltaFrontierNull frontierValue =
  IntSet.null (Store.adfRepresentativeKeys frontierValue)
    && Map.null (Store.adfVariantRowsByTag frontierValue)
    && Map.null (Store.adfAbsorbedRowsByTag frontierValue)

appendAnnotatedDeltaFrontier ::
  Ord (f ()) =>
  AnnotatedDeltaFrontier f ->
  AnnotatedDeltaFrontier f ->
  AnnotatedDeltaFrontier f
appendAnnotatedDeltaFrontier leftFrontier rightFrontier =
  Store.AnnotatedDeltaFrontier
    { Store.adfRepresentativeKeys =
        Store.adfRepresentativeKeys leftFrontier
          <> Store.adfRepresentativeKeys rightFrontier,
      Store.adfVariantRowsByTag =
        Map.unionWith
          Set.union
          (Store.adfVariantRowsByTag leftFrontier)
          (Store.adfVariantRowsByTag rightFrontier),
      Store.adfAbsorbedRowsByTag =
        Map.unionWith
          Set.union
          (Store.adfAbsorbedRowsByTag leftFrontier)
          (Store.adfAbsorbedRowsByTag rightFrontier)
    }

repKeyDelta :: IntMap Int -> IntMap Int -> IntSet
repKeyDelta oldReps newReps =
  IntSet.fromList
    [ classKey
      | classKey <- IntSet.toAscList (IntMap.keysSet oldReps <> IntMap.keysSet newReps),
        IntMap.lookup classKey oldReps /= IntMap.lookup classKey newReps
    ]

type RowForm :: Type
type RowForm = (Int, [Int])

rowsByTagDelta ::
  Ord (f ()) =>
  Int ->
  Map (f ()) [AnnotatedRow] ->
  Map (f ()) [AnnotatedRow] ->
  Map (f ()) (Set.Set RowForm)
rowsByTagDelta contextKey oldRowsByTag newRowsByTag =
  Map.filter (not . Set.null) $
    Map.mergeWithKey
      (\_ oldRows newRows -> Just (rowSetSymmetricDifference (rowsAtContext contextKey oldRows) (rowsAtContext contextKey newRows)))
      (fmap (rowSetSymmetricDifference Set.empty . rowsAtContext contextKey))
      (fmap (rowSetSymmetricDifference Set.empty . rowsAtContext contextKey))
      oldRowsByTag
      newRowsByTag

rowsAtContext :: Int -> [AnnotatedRow] -> Set.Set RowForm
rowsAtContext contextKey rows =
  Set.fromList
    [ (Store.arRootKey row, Store.arChildKeys row)
      | row <- rows,
        regionMemberKey (Store.arRegion row) contextKey
    ]

rowSetSymmetricDifference :: Ord row => Set.Set row -> Set.Set row -> Set.Set row
rowSetSymmetricDifference leftRows rightRows =
  Set.union
    (Set.difference leftRows rightRows)
    (Set.difference rightRows leftRows)

annotatedRowsAtKey ::
  Ord (f ()) =>
  f () ->
  ContextObjectKey ->
  AnnotatedDeltaBuckets f ->
  [(Int, [Int])]
annotatedRowsAtKey tag (ContextObjectKey contextKey) buckets =
  rowsAtKey tag contextKey (Store.adbVariantRowsByTag buckets)

absorbedRowsAtKey ::
  Ord (f ()) =>
  f () ->
  ContextObjectKey ->
  AnnotatedDeltaBuckets f ->
  [(Int, [Int])]
absorbedRowsAtKey tag (ContextObjectKey contextKey) buckets =
  rowsAtKey tag contextKey (Store.adbAbsorbedRowsByTag buckets)

annotatedRowsByTagAt ::
  ContextObjectKey ->
  AnnotatedDeltaBuckets f ->
  Map (f ()) [(Int, [Int])]
annotatedRowsByTagAt (ContextObjectKey contextKey) =
  rowsByTagAt contextKey . Store.adbVariantRowsByTag

absorbedRowsByTagAt ::
  ContextObjectKey ->
  AnnotatedDeltaBuckets f ->
  Map (f ()) [(Int, [Int])]
absorbedRowsByTagAt (ContextObjectKey contextKey) =
  rowsByTagAt contextKey . Store.adbAbsorbedRowsByTag

annotatedVariantRowsForTag ::
  Ord (f ()) =>
  f () ->
  AnnotatedDeltaBuckets f ->
  [AnnotatedRow]
annotatedVariantRowsForTag tag =
  Map.findWithDefault [] tag . Store.adbVariantRowsByTag

annotatedAbsorbedRowsForTag ::
  Ord (f ()) =>
  f () ->
  AnnotatedDeltaBuckets f ->
  [AnnotatedRow]
annotatedAbsorbedRowsForTag tag =
  Map.findWithDefault [] tag . Store.adbAbsorbedRowsByTag

rowsByTagAt :: Int -> Map (f ()) [AnnotatedRow] -> Map (f ()) [(Int, [Int])]
rowsByTagAt contextKey =
  Map.mapMaybe nonEmptyRows . fmap rowsInContext
  where
    rowsInContext rows =
      [ (Store.arRootKey row, Store.arChildKeys row)
        | row <- rows,
          regionMemberKey (Store.arRegion row) contextKey
      ]

    nonEmptyRows :: [row] -> Maybe [row]
    nonEmptyRows rows =
      case rows of
        [] -> Nothing
        _ -> Just rows

rowsAtKey ::
  Ord (f ()) =>
  f () ->
  Int ->
  Map (f ()) [AnnotatedRow] ->
  [(Int, [Int])]
rowsAtKey tag contextKey rowsByTag =
  [ (Store.arRootKey row, Store.arChildKeys row)
    | row <- Map.findWithDefault [] tag rowsByTag,
      regionMemberKey (Store.arRegion row) contextKey
  ]

annotatedRepresentativeKeyAt ::
  ContextObjectKey ->
  AnnotatedDeltaBuckets f ->
  Int ->
  Int
annotatedRepresentativeKeyAt contextKey buckets classKey =
  classIdKey
    ( regionalFindAt
        contextKey
        (ClassId classKey)
        (Store.adbRegionalUnionFind buckets)
    )
{-# INLINE annotatedRepresentativeKeyAt #-}

annotatedRepresentativeMapAt ::
  ContextObjectKey ->
  AnnotatedDeltaBuckets f ->
  IntMap Int
annotatedRepresentativeMapAt contextKey =
  regionalRepresentativeMapAt contextKey . Store.adbRegionalUnionFind
{-# INLINE annotatedRepresentativeMapAt #-}

annotatedEquivalentRegion ::
  RegionTable ->
  AnnotatedDeltaBuckets f ->
  Int ->
  Int ->
  ContextRegion
annotatedEquivalentRegion regionTable buckets leftKey rightKey =
  regionalEquivalentRegion
    regionTable
    (ClassId leftKey)
    (ClassId rightKey)
    (Store.adbRegionalUnionFind buckets)

annotatedInhabitedRegion :: AnnotatedDeltaBuckets f -> ContextRegion
annotatedInhabitedRegion = Store.adbInhabitedRegion
{-# INLINE annotatedInhabitedRegion #-}

annotatedDeltaMetrics :: RegionTable -> AnnotatedDeltaBuckets f -> AnnotatedDeltaMetrics
annotatedDeltaMetrics regionTable buckets =
  AnnotatedDeltaMetrics
    { annotatedDeltaParentChildCount = regionalUnionFindChildCount forestMetrics,
      annotatedDeltaParentEdgeCount = regionalUnionFindParentEdgeCount forestMetrics,
      annotatedDeltaParentRegionCubeCount = regionalUnionFindParentRegionCubeCount forestMetrics,
      annotatedDeltaVariantRowCount = foldl' (\count rows -> count + length rows) 0 (Store.adbVariantRowsByTag buckets),
      annotatedDeltaAbsorbedRowCount = foldl' (\count rows -> count + length rows) 0 (Store.adbAbsorbedRowsByTag buckets)
    }
  where
    forestMetrics = regionalUnionFindMetrics regionTable (Store.adbRegionalUnionFind buckets)

-- | Stable forcing fingerprint for diagnostics and benchmark receipts. It is
-- deliberately not semantic equality; behavioral sweeps compare queries.
annotatedDeltaFingerprint ::
  Show (f ()) =>
  RegionTable ->
  AnnotatedDeltaBuckets f ->
  Int
annotatedDeltaFingerprint regionTable buckets =
  foldl' mixFingerprint fingerprintSeed fingerprintWords
  where
    metrics = annotatedDeltaMetrics regionTable buckets
    fingerprintWords =
      [ annotatedDeltaParentChildCount metrics,
        annotatedDeltaParentEdgeCount metrics,
        annotatedDeltaParentRegionCubeCount metrics,
        annotatedDeltaVariantRowCount metrics,
        annotatedDeltaAbsorbedRowCount metrics,
        regionSize (Store.adbInhabitedRegion buckets),
        regionCubeCount regionTable (Store.adbInhabitedRegion buckets)
      ]
        <> foldMap taggedRowsWords (Map.toAscList (Store.adbVariantRowsByTag buckets))
        <> foldMap taggedRowsWords (Map.toAscList (Store.adbAbsorbedRowsByTag buckets))

    taggedRowsWords (tag, rows) =
      fmap ord (show tag) <> foldMap rowWords rows

    rowWords row =
      Store.arRootKey row
        : Store.arChildKeys row
          <> regionGeneratorKeys regionTable (Store.arRegion row)
          <> [ regionSize (Store.arRegion row),
               regionCubeCount regionTable (Store.arRegion row)
             ]

fingerprintSeed :: Int
fingerprintSeed = 2166136261

mixFingerprint :: Int -> Int -> Int
mixFingerprint accumulated wordValue =
  (accumulated * 16777619) `xor` wordValue

unorderedPairs :: [a] -> [(a, a)]
unorderedPairs values =
  case values of
    [] -> []
    value : remainingValues ->
      fmap ((,) value) remainingValues <> unorderedPairs remainingValues

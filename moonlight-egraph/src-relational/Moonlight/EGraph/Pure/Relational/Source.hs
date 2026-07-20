{-# LANGUAGE TypeFamilies #-}

module Moonlight.EGraph.Pure.Relational.Source
  ( dirtyEGraphRowsByAtomFromDatabase,
    dirtyEGraphRowsByAtomFromStructuralStore,
    egraphRowsByAtomFromDatabase,
    egraphRowsByAtomFromDatabaseRows,
    egraphRowsByAtomFromPhysicalRows,
    egraphRowsByAtomFromStructuralStore,
    physicalRowsByTagFromTagRows,
    rowsByResultForAtomSpec,
    projectPhysicalAtomSpecRow,
    structuralRowsByTag,
    structuralRowsByTagForCanonicalResultKeys,
    structuralRowsForOperator,
    structuralRowsForResultKeys,
    structuralRowsFromBucket,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Moonlight.Core
  ( Language,
  )
import Moonlight.Core
  ( Database,
    DatabaseRow,
    Operator (..),
    RowId,
    dirtyRowsForKeys,
    rowChildren,
    rowResult,
    operatorRowsForIds,
    rowsForOperator,
  )
import Moonlight.Differential.Row.Tuple
  ( RepKey (..),
    RowTupleKey,
    tupleKeyFromRepKeys,
    tupleKeyIndex,
  )
import Moonlight.EGraph.Pure.Structural.Store
  ( StructuralStore,
    structuralRowBucketForTag,
  )
import Moonlight.EGraph.Pure.Types
  ( ClassId (..),
    classIdKey,
  )
import Moonlight.Flow.Plan.Query.Core qualified as RelPlan

egraphRowsByAtomFromDatabase ::
  Language f =>
  RelPlan.QueryPlan compiled output guard (f ()) tuple ClassId ->
  (ClassId -> ClassId) ->
  Database f ClassId ->
  IntMap (IntMap [RowTupleKey])
egraphRowsByAtomFromDatabase plan canonicalizeClass db =
  egraphRowsByAtomFromDatabaseRows plan canonicalizeClass
    (`rowsForOperator` db)
{-# INLINE egraphRowsByAtomFromDatabase #-}

egraphRowsByAtomFromStructuralStore ::
  Language f =>
  RelPlan.QueryPlan compiled output guard (f ()) tuple ClassId ->
  (ClassId -> ClassId) ->
  StructuralStore f ->
  IntMap (IntMap [RowTupleKey])
egraphRowsByAtomFromStructuralStore plan canonicalizeClass store =
  egraphRowsByAtomFromTagRowsByResult plan
    ( physicalRowsByTagFromTagRows
        canonicalizeClass
        (structuralRowsByTag (queryPlanTags plan) store)
    )
{-# INLINE egraphRowsByAtomFromStructuralStore #-}

dirtyEGraphRowsByAtomFromDatabase ::
  Language f =>
  RelPlan.QueryPlan compiled output guard (f ()) tuple ClassId ->
  (ClassId -> ClassId) ->
  Database f ClassId ->
  IntSet ->
  IntMap (IntMap [RowTupleKey])
dirtyEGraphRowsByAtomFromDatabase plan canonicalizeClass db dirtyKeys =
  egraphRowsByAtomFromDatabaseRows plan canonicalizeClass
    dirtyRowsForOperator
  where
    dirtyRowsByOperator =
      dirtyRowsForKeys db dirtyKeys

    dirtyRowsForOperator operator =
      maybe
        []
        (\rowIds -> operatorRowsForIds operator rowIds db)
        (Map.lookup operator dirtyRowsByOperator)
{-# INLINE dirtyEGraphRowsByAtomFromDatabase #-}

dirtyEGraphRowsByAtomFromStructuralStore ::
  Language f =>
  RelPlan.QueryPlan compiled output guard (f ()) tuple ClassId ->
  (ClassId -> ClassId) ->
  StructuralStore f ->
  IntSet ->
  IntMap (IntMap [RowTupleKey])
dirtyEGraphRowsByAtomFromStructuralStore plan canonicalizeClass store dirtyKeys =
  egraphRowsByAtomFromTagRowsByResult plan
    ( physicalRowsByTagFromTagRows
        canonicalizeClass
        (structuralRowsByTagForCanonicalResultKeys canonicalizeClass dirtyKeys (queryPlanTags plan) store)
    )
{-# INLINE dirtyEGraphRowsByAtomFromStructuralStore #-}

egraphRowsByAtomFromDatabaseRows ::
  Language f =>
  RelPlan.QueryPlan compiled output guard (f ()) tuple ClassId ->
  (ClassId -> ClassId) ->
  (Operator f -> [(RowId, DatabaseRow)]) ->
  IntMap (IntMap [RowTupleKey])
egraphRowsByAtomFromDatabaseRows plan canonicalizeClass =
  egraphRowsByAtomFromPhysicalRows plan canonicalizeClass
    . fmap (fmap databasePhysicalRow)
{-# INLINE egraphRowsByAtomFromDatabaseRows #-}

databasePhysicalRow :: (RowId, DatabaseRow) -> (Int, [Int])
databasePhysicalRow (_rowId, row) =
  (rowResult row, rowChildren row)
{-# INLINE databasePhysicalRow #-}

egraphRowsByAtomFromPhysicalRows ::
  Language f =>
  RelPlan.QueryPlan compiled output guard (f ()) tuple ClassId ->
  (ClassId -> ClassId) ->
  (Operator f -> [(Int, [Int])]) ->
  IntMap (IntMap [RowTupleKey])
egraphRowsByAtomFromPhysicalRows plan canonicalizeClass =
  egraphRowsByAtomFromTagRowsByResult plan
    . physicalRowsByTagFromRows (queryPlanTags plan) canonicalizeClass
{-# INLINE egraphRowsByAtomFromPhysicalRows #-}

structuralRowsForOperator ::
  Language f =>
  StructuralStore f ->
  Operator f ->
  [(Int, [Int])]
structuralRowsForOperator store (Operator wantedTag) =
  structuralRowsFromBucket (structuralRowBucketForTag wantedTag store)
{-# INLINE structuralRowsForOperator #-}

structuralRowsForResultKeys ::
  Language f =>
  IntSet ->
  StructuralStore f ->
  Operator f ->
  [(Int, [Int])]
structuralRowsForResultKeys resultKeys store (Operator wantedTag) =
  structuralRowsFromBucket
    (IntMap.restrictKeys (structuralRowBucketForTag wantedTag store) resultKeys)
{-# INLINE structuralRowsForResultKeys #-}

structuralRowsFromBucket :: IntMap (Set [Int]) -> [(Int, [Int])]
structuralRowsFromBucket bucket =
  [ (resultKey, childKeys)
    | (resultKey, rows) <- IntMap.toAscList bucket,
      childKeys <- Set.toAscList rows
  ]
{-# INLINE structuralRowsFromBucket #-}

-- | Rows for every wanted tag through the store's tag index, preserving the
-- ascending (result key, child keys) order of the retired filtered scan.
structuralRowsByTag ::
  Language f =>
  Set (f ()) ->
  StructuralStore f ->
  Map (f ()) [(Int, [Int])]
structuralRowsByTag wantedTags store =
  Map.fromDistinctAscList
    [ (tag, rows)
      | tag <- Set.toAscList wantedTags,
        let rows = structuralRowsFromBucket (structuralRowBucketForTag tag store),
        not (null rows)
    ]
{-# INLINE structuralRowsByTag #-}

structuralRowsByTagForCanonicalResultKeys ::
  Language f =>
  (ClassId -> ClassId) ->
  IntSet ->
  Set (f ()) ->
  StructuralStore f ->
  Map (f ()) [(Int, [Int])]
structuralRowsByTagForCanonicalResultKeys canonicalizeClass resultKeys wantedTags store =
  Map.fromDistinctAscList
    [ (tag, rows)
      | tag <- Set.toAscList wantedTags,
        let rows =
              structuralRowsFromBucket
                (IntMap.restrictKeys (structuralRowBucketForTag tag store) indexedResultKeys),
        not (null rows)
    ]
  where
    indexedResultKeys =
      resultKeys <> IntSet.map canonicalResultKey resultKeys

    canonicalResultKey =
      classIdKey . canonicalizeClass . ClassId
{-# INLINE structuralRowsByTagForCanonicalResultKeys #-}

-- | Canonicalize raw structural rows into physical tuple rows grouped by tag and canonical result.
physicalRowsByTagFromTagRows ::
  (ClassId -> ClassId) ->
  Map (f ()) [(Int, [Int])] ->
  Map (f ()) (IntMap [RowTupleKey])
physicalRowsByTagFromTagRows canonicalizeClass =
  Map.map (physicalRowsForOperator canonicalizeClass)
{-# INLINE physicalRowsByTagFromTagRows #-}

queryPlanTags ::
  Ord tag =>
  RelPlan.QueryPlan compiled output guard tag tuple key ->
  Set tag
queryPlanTags =
  Set.fromList . fmap RelPlan.asTag . Vector.toList . RelPlan.qpAtoms
{-# INLINE queryPlanTags #-}

physicalRowsByTagFromRows ::
  Language f =>
  Set (f ()) ->
  (ClassId -> ClassId) ->
  (Operator f -> [(Int, [Int])]) ->
  Map (f ()) (IntMap [RowTupleKey])
physicalRowsByTagFromRows wantedTags canonicalizeClass sourceRowsForOperator =
  Set.foldl' insertTag Map.empty wantedTags
  where
    insertTag sections tag =
      case sourceRowsForOperator (Operator tag) of
        [] ->
          sections
        rows ->
          Map.insertWith
            (IntMap.unionWith (<>))
            tag
            (physicalRowsForOperator canonicalizeClass rows)
            sections
{-# INLINE physicalRowsByTagFromRows #-}

physicalRowsForOperator ::
  (ClassId -> ClassId) ->
  [(Int, [Int])] ->
  IntMap [RowTupleKey]
physicalRowsForOperator canonicalizeClass =
  IntMap.fromListWith (<>)
    . fmap rowEntry
  where
    rowEntry (resultKey, childKeys) =
      ( classIdKey (canonicalizeClass (ClassId resultKey)),
        [physicalRowForTuple canonicalizeClass resultKey childKeys]
      )
{-# INLINE physicalRowsForOperator #-}

physicalRowForTuple ::
  (ClassId -> ClassId) ->
  Int ->
  [Int] ->
  RowTupleKey
physicalRowForTuple canonicalizeClass resultKey childKeys =
  tupleKeyFromRepKeys $
    fmap (RepKey . classIdKey . canonicalizeClass . ClassId) (resultKey : childKeys)
{-# INLINE physicalRowForTuple #-}

egraphRowsByAtomFromTagRowsByResult ::
  Ord tag =>
  RelPlan.QueryPlan compiled output guard tag tuple key ->
  Map tag (IntMap [RowTupleKey]) ->
  IntMap (IntMap [RowTupleKey])
egraphRowsByAtomFromTagRowsByResult plan rowsByTag =
  IntMap.fromList
    [ ( RelPlan.queryAtomKey (RelPlan.asQueryAtomId atomSpec),
        rowsByResultForAtomSpec atomSpec (Map.findWithDefault IntMap.empty (RelPlan.asTag atomSpec) rowsByTag)
      )
      | atomSpec <- Vector.toList (RelPlan.qpAtoms plan)
    ]
{-# INLINE egraphRowsByAtomFromTagRowsByResult #-}

-- | Project canonical physical tag rows through one atom stalk recipe, preserving result buckets.
rowsByResultForAtomSpec ::
  RelPlan.AtomSpec tag tuple key ->
  IntMap [RowTupleKey] ->
  IntMap [RowTupleKey]
rowsByResultForAtomSpec atomSpec =
  IntMap.mapMaybe (nonEmptyRows . mapMaybe (projectPhysicalAtomSpecRow atomSpec))
{-# INLINE rowsByResultForAtomSpec #-}

projectPhysicalAtomSpecRow ::
  RelPlan.AtomSpec tag tuple key ->
  RowTupleKey ->
  Maybe RowTupleKey
projectPhysicalAtomSpecRow atomSpec physicalRow =
  tupleKeyFromRepKeys
    <$> traverse solveColumn (Vector.toList (RelPlan.stalkRecipeColumns (RelPlan.asStalkRecipe atomSpec)))
  where
    solveColumn sources = do
      values <- traverse (sourceRepKey physicalRow) sources
      case values of
        [] ->
          Nothing
        value : rest
          | all (== value) rest ->
              Just value
          | otherwise ->
              Nothing
{-# INLINE projectPhysicalAtomSpecRow #-}

sourceRepKey :: RowTupleKey -> RelPlan.SlotSource -> Maybe RepKey
sourceRepKey row =
  tupleKeyIndex row . physicalSourcePosition
{-# INLINE sourceRepKey #-}

physicalSourcePosition :: RelPlan.SlotSource -> Int
physicalSourcePosition source =
  case source of
    RelPlan.SourceResult ->
      0
    RelPlan.SourceChild childIndex ->
      childIndex + 1
{-# INLINE physicalSourcePosition #-}

nonEmptyRows :: [row] -> Maybe [row]
nonEmptyRows rows =
  case rows of
    [] ->
      Nothing
    _ ->
      Just rows
{-# INLINE nonEmptyRows #-}

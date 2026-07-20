{-# LANGUAGE GHC2024 #-}

-- | Relational execution backend for rewrite host projections.
-- Owns the 'PreparedBackend' instance that maps constructor-tag sections into
-- Flow prepared base/context views and patch deltas.
-- Contracts: the rewrite host is the canonical source; prepared stores and
-- repairs are derived by query-plan atom rows and host revisions.
module Moonlight.Rewrite.Relational.Backend
  ( RewriteRelationalHost,
    RewriteRelationalRepair,
    RewriteRelationalPatch (..),
    RewriteRelationalPreparedObstruction (..),
    RewriteRelationalBackend,
    RewriteBasePrepared,
    rewriteBasePreparedRevision,
    RewriteContextPrepared,
    emptyRewriteRelationalHost,
    replaceRewriteRelationalHost,
    rewriteRelationalHostRevision,
    rewriteRelationalHostSections,
    rewriteRelationalHostPreparedRelationsForPlan,
    rewritePreparedBackend,
  )
where

import Data.Bifunctor (bimap, first)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Vector qualified as Vector
import Moonlight.Flow.Execution.Prepared.Backend
  ( PreparedBackend (..),
    PreparedScopeView (..),
  )
import Moonlight.Flow.Execution.Prepared.Base
  ( BasePreparedDB (..),
    BuildBasePreparedDBError,
    PatchBasePreparedDBError,
    baseStore,
    buildBasePreparedDBFromAtomRows,
    patchBasePreparedDBWithAtomRows,
  )
import Moonlight.Differential.Row.Delta
  ( RowDelta
  )
import Moonlight.Differential.Row.Tuple
  ( RowTupleKey,
  )
import Moonlight.Flow.Plan.Query.Core
  ( QueryPlan,
  )
import Moonlight.Flow.Plan.Query.Core qualified as RelPlan
import Moonlight.Flow.Storage.Relation
  ( Relation,
  )
import Moonlight.Flow.Storage.Store
  ( Store,
    storeFromRelations,
    storeRelations,
  )
import Moonlight.Rewrite.Relational.Output
  ( RelationalRewriteMatch,
  )

-- | Rewrite host facts before they are prepared for a particular relational plan.
--
-- The front owns rewrite-domain nodes and descends them into constructor-tag
-- sections. Flow only receives projected atom rows for the concrete query plan;
-- no generic tuple-projection facade gets to squat in the execution layer.
type RewriteRelationalHost :: Type -> Type
data RewriteRelationalHost tag = RewriteRelationalHost
  { rrhRevision :: {-# UNPACK #-} !Int,
    rrhSections :: !(Map tag (IntMap [RowTupleKey]))
  }
  deriving stock (Eq, Show)

type RewriteRelationalRepair :: Type -> Type
type RewriteRelationalRepair = RewriteRelationalHost

type RewriteRelationalPatch :: Type
data RewriteRelationalPatch = RewriteRelationalPatch
  { rrpDirtyResults :: !IntSet,
    rrpAtomDeltas :: !(IntMap RowDelta)
  }
  deriving stock (Eq, Show)

type RewriteRelationalPreparedObstruction :: Type
data RewriteRelationalPreparedObstruction
  = RewritePreparedBuildObstruction !BuildBasePreparedDBError
  | RewritePreparedPatchObstruction !PatchBasePreparedDBError
  deriving stock (Eq, Show)

type RewriteBasePrepared :: Type -> Type -> Type -> Type -> Type -> Type -> Type
data RewriteBasePrepared compiled output guard tag tuple key = RewriteBasePrepared
  { rbpRevision :: {-# UNPACK #-} !Int,
    rbpPreparedDB :: !(BasePreparedDB compiled output guard tag tuple key)
  }

type RewriteContextPrepared :: Type
newtype RewriteContextPrepared = RewriteContextPrepared
  { rcpStore :: Store
  }

rewriteBasePreparedRevision :: RewriteBasePrepared compiled output guard tag tuple key -> Int
rewriteBasePreparedRevision =
  rbpRevision
{-# INLINE rewriteBasePreparedRevision #-}

type RewriteRelationalBackend :: Type -> Type -> Type -> Type -> Type -> Type -> Type
data RewriteRelationalBackend compiled var key guard tag tuple = RewriteRelationalBackend

instance
  Ord tag =>
  PreparedBackend (RewriteRelationalBackend compiled var key guard tag tuple)
  where
  type PreparedCompiled (RewriteRelationalBackend compiled var key guard tag tuple) =
    compiled

  type PreparedOutput (RewriteRelationalBackend compiled var key guard tag tuple) =
    RelationalRewriteMatch var key

  type PreparedGuard (RewriteRelationalBackend compiled var key guard tag tuple) =
    guard

  type PreparedTag (RewriteRelationalBackend compiled var key guard tag tuple) =
    tag

  type PreparedTuple (RewriteRelationalBackend compiled var key guard tag tuple) =
    tuple

  type PreparedKey (RewriteRelationalBackend compiled var key guard tag tuple) =
    key

  type PreparedHost (RewriteRelationalBackend compiled var key guard tag tuple) =
    RewriteRelationalHost tag

  type PreparedRepair (RewriteRelationalBackend compiled var key guard tag tuple) =
    RewriteRelationalRepair tag

  type PreparedRelation (RewriteRelationalBackend compiled var key guard tag tuple) =
    Relation

  type PreparedBase (RewriteRelationalBackend compiled var key guard tag tuple) =
    RewriteBasePrepared compiled (RelationalRewriteMatch var key) guard tag tuple key

  type PreparedContext (RewriteRelationalBackend compiled var key guard tag tuple) =
    RewriteContextPrepared

  type PreparedFiber (RewriteRelationalBackend compiled var key guard tag tuple) =
    Relation

  type PreparedPatch (RewriteRelationalBackend compiled var key guard tag tuple) =
    RewriteRelationalPatch
  type PreparedObstruction (RewriteRelationalBackend compiled var key guard tag tuple) =
    RewriteRelationalPreparedObstruction

  pbBuildBase _ plan host =
    first
      RewritePreparedBuildObstruction
      ( rewriteBasePreparedFromSections
          (rrhRevision host)
          plan
          (rrhSections host)
      )

  pbPatchBase _ host repair dirtyResults basePrepared =
    bimap
      RewritePreparedPatchObstruction
      ( \(patchedDb, atomDeltas) ->
          ( RewriteBasePrepared
              { rbpRevision = rrhRevision repair,
                rbpPreparedDB = patchedDb
              },
            RewriteRelationalPatch
              { rrpDirtyResults = dirtyResults,
                rrpAtomDeltas = atomDeltas
              }
          )
      )
      ( patchBasePreparedDBWithAtomRows
          ( dirtyRewriteRowsByAtomFromSections
              (bpdPlan (rbpPreparedDB basePrepared))
              dirtyResults
              (rrhSections host)
          )
          ( dirtyRewriteRowsByAtomFromSections
              (bpdPlan (rbpPreparedDB basePrepared))
              dirtyResults
              (rrhSections repair)
          )
          dirtyResults
          (rbpPreparedDB basePrepared)
      )

  pbPrepareContext _ =
    Right . RewriteContextPrepared . storeFromRelations

  pbBaseScopeView _ basePrepared =
    let store =
          baseStore (rbpPreparedDB basePrepared)
     in PreparedScopeView
          { psvFibers = storeRelations store,
            psvStore = store
          }

  pbContextScopeView _ contextPrepared =
    PreparedScopeView
      { psvFibers = storeRelations (rcpStore contextPrepared),
        psvStore = rcpStore contextPrepared
      }

rewriteBasePreparedFromSections ::
  Ord tag =>
  Int ->
  QueryPlan compiled output guard tag tuple key ->
  Map tag (IntMap [RowTupleKey]) ->
  Either BuildBasePreparedDBError (RewriteBasePrepared compiled output guard tag tuple key)
rewriteBasePreparedFromSections revision plan sections =
  fmap
    ( \preparedDb ->
        RewriteBasePrepared
          { rbpRevision = revision,
            rbpPreparedDB = preparedDb
          }
    )
    (buildBasePreparedDBFromAtomRows plan (rewriteRowsByAtomFromSections plan sections))

rewriteRowsByAtomFromSections ::
  Ord tag =>
  QueryPlan compiled output guard tag tuple key ->
  Map tag (IntMap [RowTupleKey]) ->
  IntMap (IntMap [RowTupleKey])
rewriteRowsByAtomFromSections plan sections =
  IntMap.fromList
    [ ( RelPlan.queryAtomKey (RelPlan.asQueryAtomId atomSpec),
        Map.findWithDefault IntMap.empty (RelPlan.asTag atomSpec) sections
      )
      | atomSpec <- Vector.toList (RelPlan.qpAtoms plan)
    ]
{-# INLINE rewriteRowsByAtomFromSections #-}

dirtyRewriteRowsByAtomFromSections ::
  Ord tag =>
  QueryPlan compiled output guard tag tuple key ->
  IntSet ->
  Map tag (IntMap [RowTupleKey]) ->
  IntMap (IntMap [RowTupleKey])
dirtyRewriteRowsByAtomFromSections plan dirtyResults =
  fmap (`IntMap.restrictKeys` dirtyResults) . rewriteRowsByAtomFromSections plan
{-# INLINE dirtyRewriteRowsByAtomFromSections #-}

emptyRewriteRelationalHost :: RewriteRelationalHost tag
emptyRewriteRelationalHost =
  RewriteRelationalHost
    { rrhRevision = 0,
      rrhSections = Map.empty
    }

replaceRewriteRelationalHost ::
  Int ->
  Map tag (IntMap [RowTupleKey]) ->
  RewriteRelationalHost tag
replaceRewriteRelationalHost revision sections =
  RewriteRelationalHost
    { rrhRevision = revision,
      rrhSections = sections
    }

rewriteRelationalHostRevision :: RewriteRelationalHost tag -> Int
rewriteRelationalHostRevision =
  rrhRevision

rewriteRelationalHostSections :: RewriteRelationalHost tag -> Map tag (IntMap [RowTupleKey])
rewriteRelationalHostSections =
  rrhSections

rewriteRelationalHostPreparedRelationsForPlan ::
  Ord tag =>
  QueryPlan compiled output guard tag tuple key ->
  RewriteRelationalHost tag ->
  Either BuildBasePreparedDBError (IntMap Relation)
rewriteRelationalHostPreparedRelationsForPlan plan host =
  storeRelations
    . baseStore
    <$> buildBasePreparedDBFromAtomRows
      plan
      (rewriteRowsByAtomFromSections plan (rrhSections host))

rewritePreparedBackend ::
  RewriteRelationalBackend compiled var key guard tag tuple
rewritePreparedBackend =
  RewriteRelationalBackend

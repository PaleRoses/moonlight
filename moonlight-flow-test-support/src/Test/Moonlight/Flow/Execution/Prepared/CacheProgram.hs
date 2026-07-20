{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Moonlight.Flow.Execution.Prepared.CacheProgram
  ( MatchRow (..),
    SimpleRequest (..),
    TestBackend,
    TestPlan,
    atomRow,
    compilePlan,
    contextRequest,
    countBasePrepared,
    countContextPrepared,
    identityProjection,
    joinDatabase,
    mkSnapshot,
    rowKeys,
    testPreparedBackend,
  )
where

import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Kind
  ( Type,
  )
import Data.Map.Strict qualified as Map
import Data.Vector qualified as Vector
import Moonlight.Core
  ( MatchFootprint,
    QuerySnapshot
      ( QuerySnapshot,
        baseRevision,
        footprint,
        liveEpoch,
        liveRelations,
        projection,
        queryId
      ),
  )
import Moonlight.Flow.Execution.Prepared.Backend
  ( PreparedBackend (..),
    PreparedScopeView (..),
  )
import Moonlight.Flow.Execution.Prepared.Cache
  ( JoinCacheState (jcsPrepared),
    PreparedCacheKey (BasePreparedKey, ContextPreparedKey),
  )
import Moonlight.Differential.Row.Tuple
  ( RowTupleKey,
    tupleKeyFromInts,
    tupleKeyToInts,
  )
import Moonlight.Flow.Plan.Compile.Build qualified as PlanBuild
import Moonlight.Flow.Plan.Query.Core
  ( AtomSpec,
    QueryOutput (..),
    QueryPlan,
    SlotId,
    mkAtomId,
    mkAtomSpec,
    mkQueryAtomId,
    mkQueryId,
    mkSlotId,
    mkSourceAtomId,
    mkStalkRecipe,
    slotIdKey,
  )
import Moonlight.Flow.Storage.Relation
import Moonlight.Flow.Storage.Store
  ( storeFromRelations,
  )
import Moonlight.Differential.Row.Block
import Moonlight.Flow.Model.RowIdentity
  ( rowBlockIdentityForAtom,
  )

type MatchRow :: Type
newtype MatchRow = MatchRow
  { mrRow :: RowTupleKey
  }
  deriving stock (Eq, Ord, Show)

instance QueryOutput MatchRow Int where
  type OutputVar MatchRow Int = ()

  data OutputRecipe MatchRow Int = MatchRowRecipe

  mkOutputRecipe _ =
    MatchRowRecipe

  projectOutputRecipe MatchRowRecipe _root bindingValues =
    Right (MatchRow (tupleKeyFromInts (Vector.toList bindingValues)))

type SimpleRequest :: Type -> Type -> Type
data SimpleRequest c token = SimpleRequest
  { srHost :: !(IntMap (RowBlock 'Canonical)),
    srContext :: !(Maybe (c, QuerySnapshot Int (RowBlock 'Canonical)))
  }

type TestPlan :: Type
type TestPlan = QueryPlan () MatchRow () () () Int

type TestBackend :: Type
data TestBackend = TestBackend

instance PreparedBackend TestBackend where
  type PreparedCompiled TestBackend = ()
  type PreparedOutput TestBackend = MatchRow
  type PreparedGuard TestBackend = ()
  type PreparedTag TestBackend = ()
  type PreparedTuple TestBackend = ()
  type PreparedKey TestBackend = Int
  type PreparedHost TestBackend = IntMap (RowBlock 'Canonical)
  type PreparedRepair TestBackend = ()
  type PreparedRelation TestBackend = RowBlock 'Canonical
  type PreparedBase TestBackend = IntMap Relation
  type PreparedContext TestBackend = IntMap Relation
  type PreparedFiber TestBackend = Relation
  type PreparedPatch TestBackend = ()
  type PreparedObstruction TestBackend = RelationPatchError

  pbBuildBase _ planValue =
    buildPreparedDb planValue

  pbPatchBase _ _ _ _ basePrepared =
    Right (basePrepared, ())

  pbPrepareContext _ =
    prepareContextFibers

  pbBaseScopeView _ basePrepared =
    PreparedScopeView
      { psvFibers = basePrepared,
        psvStore = storeFromRelations basePrepared
      }

  pbContextScopeView _ contextPrepared =
    PreparedScopeView
      { psvFibers = contextPrepared,
        psvStore = storeFromRelations contextPrepared
      }

rootSlot :: SlotId
rootSlot = mkSlotId 0

xSlot :: SlotId
xSlot = mkSlotId 1

ySlot :: SlotId
ySlot = mkSlotId 2

fullSchema :: Vector.Vector SlotId
fullSchema =
  Vector.fromList [rootSlot, xSlot, ySlot]

atomRow :: [Int] -> RowTupleKey
atomRow =
  tupleKeyFromInts

rowKeys :: RowTupleKey -> [Int]
rowKeys =
  tupleKeyToInts

compilePlan :: () -> Either [PlanBuild.QueryPlanError] TestPlan
compilePlan () =
  PlanBuild.mkQueryPlan
    ( PlanBuild.QueryPlanInput
        { PlanBuild.qpiDomain = PlanBuild.StructuralQueryPlan,
          PlanBuild.qpiCompiled = (),
          PlanBuild.qpiDigest = 11,
          PlanBuild.qpiAtoms = atomSpecs,
          PlanBuild.qpiSchemaOrder = Just fullSchema,
          PlanBuild.qpiRootSlot = rootSlot,
          PlanBuild.qpiOutputs = fmap (`PlanBuild.PlanOutputBinding` ()) (Vector.toList fullSchema),
          PlanBuild.qpiResidual = PlanBuild.NoQueryPlanResidual
        }
    )
  where
    atomSpecs =
      Vector.fromList
        [ atomSpec 0 [rootSlot, xSlot],
          atomSpec 1 [rootSlot, ySlot],
          atomSpec 2 [xSlot, ySlot]
        ]

atomSpec :: Int -> [SlotId] -> AtomSpec () () Int
atomSpec atomKey columns =
  let schema = Vector.fromList columns
   in mkAtomSpec
        (mkQueryAtomId atomKey)
        (mkSourceAtomId (mkAtomId atomKey))
        ()
        0
        schema
        (mkStalkRecipe (Vector.replicate (Vector.length schema) []))

atomRelation :: [SlotId] -> [[Int]] -> Either RowBuildError (RowBlock 'Canonical)
atomRelation columns rows =
  atomRowsFromTupleKeys
    (relationIdentityFor columns)
    (Vector.fromList columns)
    (fmap atomRow rows)

relationIdentityFor :: [SlotId] -> RowBlockIdentity
relationIdentityFor columns =
  rowBlockIdentityForAtom
    0
    0
    11
    (mkAtomId (foldl (\acc slotIdValue -> acc * 167 + slotIdKey slotIdValue) 1 columns))
    0

joinDatabase :: [[Int]] -> [[Int]] -> [[Int]] -> Either RowBuildError (IntMap (RowBlock 'Canonical))
joinDatabase rootX rootY xy =
  IntMap.fromList
    <$> sequenceA
      [ fmap ((,) 0) (atomRelation [rootSlot, xSlot] rootX),
        fmap ((,) 1) (atomRelation [rootSlot, ySlot] rootY),
        fmap ((,) 2) (atomRelation [xSlot, ySlot] xy)
      ]

identityProjection :: [[Int]] -> IntMap Int
identityProjection =
  IntMap.fromList
    . fmap (\key -> (key, key))
    . IntSet.toList
    . IntSet.fromList
    . concat

mkSnapshot ::
  Int ->
  Int ->
  Int ->
  MatchFootprint ->
  IntMap (RowBlock 'Canonical) ->
  IntMap Int ->
  QuerySnapshot Int (RowBlock 'Canonical)
mkSnapshot baseRevision queryKey liveEpoch footprint liveRelations projection =
  QuerySnapshot
    { baseRevision = baseRevision,
      queryId = mkQueryId queryKey,
      liveEpoch = liveEpoch,
      liveRelations = liveRelations,
      projection = projection,
      footprint = footprint
    }

contextRequest :: c -> QuerySnapshot Int (RowBlock 'Canonical) -> SimpleRequest c ()
contextRequest ctx snapshot =
  SimpleRequest
    { srHost = IntMap.empty,
      srContext = Just (ctx, snapshot)
    }

buildPreparedDb :: TestPlan -> IntMap (RowBlock 'Canonical) -> Either RelationPatchError (IntMap Relation)
buildPreparedDb _ =
  prepareContextFibers

prepareContextFibers :: IntMap (RowBlock 'Canonical) -> Either RelationPatchError (IntMap Relation)
prepareContextFibers =
  traverse relationFromAtomRows

testPreparedBackend :: TestBackend
testPreparedBackend =
  TestBackend

countBasePrepared :: JoinCacheState c plan baseDb contextPrepared repair -> Int
countBasePrepared =
  Map.size
    . Map.filterWithKey
      ( \key _ ->
          case key of
            BasePreparedKey _ -> True
            _ -> False
      )
    . jcsPrepared

countContextPrepared :: JoinCacheState c plan baseDb contextPrepared repair -> Int
countContextPrepared =
  Map.size
    . Map.filterWithKey
      ( \key _ ->
          case key of
            ContextPreparedKey _ _ _ -> True
            _ -> False
      )
    . jcsPrepared

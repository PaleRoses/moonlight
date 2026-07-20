{-# LANGUAGE DataKinds #-}

module Moonlight.Sheaf.Query.PresheafSpec
  ( tests,
  )
where

import Data.HashSet qualified as HashSet
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.List (sortOn)
import Data.Vector qualified as Vector
import Data.Vector.Unboxed qualified as VU
import Data.Word (Word64)
import Moonlight.Differential.Join.WCOJ
  ( Domain,
    JoinAlgebra (..),
    adaptiveJoin,
    chooseSmallestSlot,
    domainEmpty,
    domainFromList,
    domainSize,
    domainSingleton,
    domainToHashSet,
    existsJoin,
    foldGenericJoin,
  )
import Moonlight.Core (mkSlotId)
import Moonlight.Differential.Row.Block
  ( RowDesc,
    RowBlock,
    RowBlockIdentity (..),
    RowState (Canonical),
    foldRowBlock,
    fromSlotRows,
    rowSlots,
  )
import Moonlight.Sheaf.Pruning (PruningReport (..))
import Moonlight.Sheaf.Query.Restriction
  ( RowPruningObstruction (..),
    RowPruningResult (..),
    pruneRowsWithVerdict,
    rowPruningVerdict,
  )
import Moonlight.Sheaf.Verdict (ObstructionVerdict)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertFailure,
    testCase,
    (@?=),
  )

type JoinCtx :: Type
data JoinCtx
  = OpenCtx
  | NarrowCtx
  deriving stock (Eq, Ord, Show)

type MiniIndexedDb :: Type
data MiniIndexedDb = MiniIndexedDb

restrictMiniArrangementRow :: MiniIndexedDb -> JoinCtx -> JoinCtx -> IntMap Int -> IntMap Int
restrictMiniArrangementRow _ sourceContext targetContext stalkValue =
  case (sourceContext, targetContext) of
    (OpenCtx, NarrowCtx) -> IntMap.map (min 1) stalkValue
    _ -> stalkValue

materializeMiniArrangement :: MiniIndexedDb -> JoinCtx -> IntMap IntSet.IntSet
materializeMiniArrangement _ contextValue =
  case contextValue of
    OpenCtx -> IntMap.fromList [(0, IntSet.fromList [1, 2]), (1, IntSet.fromList [1, 2, 3]), (2, IntSet.singleton 9)]
    NarrowCtx -> IntMap.fromList [(0, IntSet.singleton 1), (1, IntSet.fromList [1, 2]), (2, IntSet.singleton 9)]

invalidateMiniArrangement :: MiniIndexedDb -> JoinCtx -> IntSet.IntSet -> IntSet.IntSet
invalidateMiniArrangement _ contextValue dirtyKeys =
  case contextValue of
    OpenCtx -> dirtyKeys
    NarrowCtx -> IntSet.filter odd dirtyKeys

miniJoinAlgebra :: JoinAlgebra JoinCtx Int
miniJoinAlgebra =
  JoinAlgebra
    { joinCount = \contextValue environment slot -> domainSize (miniJoinDomain MiniIndexedDb contextValue environment slot),
      joinPropose = miniJoinDomain MiniIndexedDb,
      joinValidate = miniJoinWitness MiniIndexedDb
    }

miniJoinDomain :: MiniIndexedDb -> JoinCtx -> IntMap Int -> Int -> Domain Int
miniJoinDomain _ contextValue environment slot =
  case (contextValue, slot, IntMap.lookup 0 environment) of
    (_, 2, _) -> domainSingleton 9
    (OpenCtx, 0, _) -> domainFromList [1, 2]
    (NarrowCtx, 0, _) -> domainSingleton 1
    (_, 1, Nothing) -> domainFromList [1, 2, 3]
    (_, 1, Just seedValue) -> domainFromList [seedValue, seedValue + 1]
    _ -> domainEmpty

miniJoinWitness :: MiniIndexedDb -> JoinCtx -> IntMap Int -> Bool
miniJoinWitness db contextValue environment =
  all slotWitnessed (IntMap.keys environment)
  where
    slotWitnessed slot =
      case IntMap.lookup slot environment of
        Nothing ->
          False
        Just value ->
          HashSet.member value (domainToHashSet (miniJoinDomain db contextValue (IntMap.delete slot environment) slot))

tests :: TestTree
tests =
  testGroup
    "presheaf"
    [ testCase "foldGenericJoin and adaptiveJoin enumerate the same assignments" testIndexedJoinAgreement,
      testCase "restrict, materialize, and invalidate expose the indexed presheaf interface" testIndexedRestriction,
      testCase "row restriction pruning reports removed rows without owning match state" testRowRestrictionPruning
    ]

testIndexedJoinAgreement :: Assertion
testIndexedJoinAgreement =
  let slots = [0, 1, 2]
      genericAssignments = normalizeAssignments (materializedFoldGenericJoin miniJoinAlgebra OpenCtx slots IntMap.empty)
      adaptiveAssignments = normalizeAssignments (adaptiveJoin miniJoinAlgebra OpenCtx slots IntMap.empty)
   in do
        genericAssignments @?= adaptiveAssignments
        existsJoin miniJoinAlgebra OpenCtx slots IntMap.empty @?= True
        chooseSmallestSlot miniJoinAlgebra OpenCtx slots IntMap.empty @?= Just (2, [0, 1])

materializedFoldGenericJoin ::
  JoinAlgebra JoinCtx Int ->
  JoinCtx ->
  [Int] ->
  IntMap Int ->
  [IntMap Int]
materializedFoldGenericJoin algebra contextValue slots environment =
  foldGenericJoin algebra contextValue slots environment (\envs joinedEnv -> joinedEnv : envs) []

testIndexedRestriction :: Assertion
testIndexedRestriction =
  let stalkValue = IntMap.fromList [(0, 2), (1, 3)]
   in do
        restrictMiniArrangementRow MiniIndexedDb OpenCtx NarrowCtx stalkValue @?= IntMap.fromList [(0, 1), (1, 1)]
        materializeMiniArrangement MiniIndexedDb NarrowCtx @?= IntMap.fromList [(0, IntSet.singleton 1), (1, IntSet.fromList [1, 2]), (2, IntSet.singleton 9)]
        invalidateMiniArrangement MiniIndexedDb NarrowCtx (IntSet.fromList [1, 2, 3, 4]) @?= IntSet.fromList [1, 3]

testRowRestrictionPruning :: Assertion
testRowRestrictionPruning =
  case rowsFromWords [[1], [2]] of
    Left err ->
      assertFailure (show err)
    Right rows ->
      let keep :: RowBlock 'Canonical -> RowDesc -> Either () (ObstructionVerdict RowPruningObstruction)
          keep candidateRows desc =
            Right (rowPruningVerdict LocalRowAbsent (rowSlots candidateRows desc == VU.singleton 1))
       in case pruneRowsWithVerdict (const ()) neutralIdentity keep rows of
            Left () -> assertFailure "pure row pruning unexpectedly failed"
            Right result -> do
              rowsToLists (rprRows result) @?= [[1]]
              length (prPruned (rprReport result)) @?= 1

rowsFromWords :: [[Word64]] -> Either String (RowBlock 'Canonical)
rowsFromWords =
  either (Left . show) Right . fromSlotRows neutralIdentity (Vector.singleton (mkSlotId 0)) . fmap VU.fromList

rowsToLists :: RowBlock 'Canonical -> [[Word64]]
rowsToLists rows =
  reverse (foldRowBlock (\acc desc -> VU.toList (rowSlots rows desc) : acc) [] rows)

neutralIdentity :: RowBlockIdentity
neutralIdentity =
  RowBlockIdentity
    { rowBlockBaseRevision = 0,
      rowBlockOverlayEpoch = 0,
      rowBlockPlanFingerprint = 0,
      rowBlockEntityKey = 0,
      rowBlockGeneration = 0
    }

normalizeAssignments :: [IntMap Int] -> [[(Int, Int)]]
normalizeAssignments = sortOn id . fmap IntMap.toAscList

{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Runtime.IndexedJoinSpec
  ( tests,
  )
where

import Data.HashSet qualified as HashSet
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.List (sort)
import Data.Map.Strict qualified as Map
import Moonlight.Differential.Row.Tuple
import Moonlight.Differential.Join.WCOJ
  ( Domain,
    JoinAlgebra (..),
    adaptiveJoin,
    chooseSmallestSlot,
    domainEmpty,
    domainFromList,
    domainSize,
    domainToHashSet,
    existsJoin,
    foldGenericJoin,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

type Scope :: Type
data Scope
  = Coarse
  | Fine
  deriving stock (Eq, Ord, Show)

type TinyDb :: Type
data TinyDb = TinyDb
  { tdPairs :: !(Map.Map Scope [(Int, Int)]),
    tdClasses :: !(Map.Map Scope (IntMap.IntMap Int)),
    tdRestrictions :: !(Map.Map (Scope, Scope) (IntMap.IntMap Int))
  }

restrictTinyArrangementRow :: TinyDb -> Scope -> Scope -> RowTupleKey -> RowTupleKey
restrictTinyArrangementRow db src dst =
  restrictTupleKey (Map.findWithDefault IntMap.empty (src, dst) (tdRestrictions db))

materializeTinyArrangement :: TinyDb -> Scope -> IntMap.IntMap Int
materializeTinyArrangement db ctx =
  Map.findWithDefault IntMap.empty ctx (tdClasses db)

invalidateTinyArrangement :: TinyDb -> Scope -> IntSet.IntSet -> IntSet.IntSet
invalidateTinyArrangement db ctx dirtyKeys =
  IntSet.fromList
    [ localKey
    | (localKey, quotientKey) <- IntMap.toList (materializeTinyArrangement db ctx),
      quotientKey `IntSet.member` dirtyKeys
    ]

tests :: TestTree
tests =
  testGroup
    "indexed-join"
    [ testCase "arrangement cursor respects partial assignments and context-local domains" $ do
        domainToHashSet (joinPropose tinyJoinAlgebra Coarse IntMap.empty 0) @?= HashSet.fromList [1, 2]
        domainToHashSet (joinPropose tinyJoinAlgebra Coarse (IntMap.singleton 0 1) 1) @?= HashSet.fromList [10, 11]
        domainToHashSet (joinPropose tinyJoinAlgebra Fine (IntMap.singleton 0 1) 1) @?= HashSet.fromList [10]

    , testCase "restrict maps stalk rows through the context quotient" $
        restrictTinyArrangementRow tinyDb Coarse Fine (atomRow [1, 11])
          @?= atomRow [1, 10]

    , testCase "materialize exposes the context-local quotient map" $
        materializeTinyArrangement tinyDb Fine
          @?= IntMap.fromList [(1, 1), (2, 2), (10, 10), (11, 10), (20, 20)]

    , testCase "invalidate returns all local keys whose quotient image is dirty" $
        invalidateTinyArrangement tinyDb Fine (IntSet.singleton 10)
          @?= IntSet.fromList [10, 11]

    , testCase "foldGenericJoin enumerates all satisfying assignments" $
        normalizeEnvs (materializedFoldGenericJoin tinyJoinAlgebra Coarse [0, 1] IntMap.empty)
          @?= normalizeEnvs
            [ IntMap.fromList [(0, 1), (1, 10)],
              IntMap.fromList [(0, 1), (1, 11)],
              IntMap.fromList [(0, 2), (1, 20)]
            ]

    , testCase "existsJoin distinguishes inhabited and empty joins" $ do
        existsJoin tinyJoinAlgebra Fine [0, 1] IntMap.empty @?= True
        existsJoin tinyJoinAlgebra Fine [0, 1] (IntMap.singleton 0 2) @?= False

    , testCase "adaptiveJoin returns the same solution set as foldGenericJoin" $
        normalizeEnvs (adaptiveJoin tinyJoinAlgebra Coarse [0, 1] IntMap.empty)
          @?= normalizeEnvs (materializedFoldGenericJoin tinyJoinAlgebra Coarse [0, 1] IntMap.empty)

    , testCase "join terminal condition rejects domain-only false witnesses" $ do
        materializedFoldGenericJoin looseJoinAlgebra Coarse [0, 1] IntMap.empty @?= []
        existsJoin looseJoinAlgebra Coarse [0, 1] IntMap.empty @?= False
        adaptiveJoin looseJoinAlgebra Coarse [0, 1] IntMap.empty @?= []

    , testCase "chooseSmallestSlot prefers the smallest candidate domain and breaks ties by slot id" $ do
        chooseSmallestSlot tinyJoinAlgebra Coarse [0, 1] IntMap.empty @?= Just (0, [1])
        chooseSmallestSlot tinyJoinAlgebra Coarse [0, 1] (IntMap.singleton 0 1) @?= Just (1, [0])
    ]

materializedFoldGenericJoin ::
  JoinAlgebra Scope Int ->
  Scope ->
  [Int] ->
  IntMap.IntMap Int ->
  [IntMap.IntMap Int]
materializedFoldGenericJoin algebra contextValue slots environment =
  foldGenericJoin algebra contextValue slots environment (\envs joinedEnv -> joinedEnv : envs) []

tinyJoinAlgebra :: JoinAlgebra Scope Int
tinyJoinAlgebra =
  JoinAlgebra
    { joinCount = \ctx env slot -> domainSize (tinyJoinDomain tinyDb ctx env slot),
      joinPropose = tinyJoinDomain tinyDb,
      joinValidate = tinyJoinWitness tinyDb
    }

tinyJoinDomain :: TinyDb -> Scope -> IntMap.IntMap Int -> Int -> Domain Int
tinyJoinDomain db ctx env slot =
  let tuples = Map.findWithDefault [] ctx (tdPairs db)
      consistent (leftValue, rightValue) =
        maybe True (== leftValue) (IntMap.lookup 0 env)
          && maybe True (== rightValue) (IntMap.lookup 1 env)
   in case slot of
        0 ->
          domainFromList
            [ leftValue
            | tupleValue@(leftValue, _) <- tuples,
              consistent tupleValue
            ]
        1 ->
          domainFromList
            [ rightValue
            | tupleValue@(_, rightValue) <- tuples,
              consistent tupleValue
            ]
        _ ->
          domainEmpty

tinyJoinWitness :: TinyDb -> Scope -> IntMap.IntMap Int -> Bool
tinyJoinWitness db ctx env =
  any consistent (Map.findWithDefault [] ctx (tdPairs db))
  where
    consistent (leftValue, rightValue) =
      maybe True (== leftValue) (IntMap.lookup 0 env)
        && maybe True (== rightValue) (IntMap.lookup 1 env)

tinyDb :: TinyDb
tinyDb =
  TinyDb
    { tdPairs =
        Map.fromList
          [ (Coarse, [(1, 10), (1, 11), (2, 20)]),
            (Fine, [(1, 10)])
          ],
      tdClasses =
        Map.fromList
          [ (Coarse, IntMap.fromList [(1, 1), (2, 2), (10, 10), (11, 11), (20, 20)]),
            (Fine, IntMap.fromList [(1, 1), (2, 2), (10, 10), (11, 10), (20, 20)])
          ],
      tdRestrictions =
        Map.fromList
          [ ((Coarse, Coarse), IntMap.fromList [(1, 1), (2, 2), (10, 10), (11, 11), (20, 20)]),
            ((Fine, Fine), IntMap.fromList [(1, 1), (2, 2), (10, 10), (11, 10), (20, 20)]),
            ((Coarse, Fine), IntMap.fromList [(1, 1), (2, 2), (10, 10), (11, 10), (20, 20)])
          ]
    }

type LooseDb :: Type
data LooseDb = LooseDb
  { ldSlotDomains :: !(Map.Map Int [Int]),
    ldWitnesses :: ![(Int, Int)]
  }

looseJoinAlgebra :: JoinAlgebra Scope Int
looseJoinAlgebra =
  JoinAlgebra
    { joinCount = \ctx env slot -> domainSize (looseJoinDomain looseDb ctx env slot),
      joinPropose = looseJoinDomain looseDb,
      joinValidate = looseJoinWitness looseDb
    }

looseJoinDomain :: LooseDb -> Scope -> IntMap.IntMap Int -> Int -> Domain Int
looseJoinDomain db _ctx _env slot =
  domainFromList (Map.findWithDefault [] slot (ldSlotDomains db))

looseJoinWitness :: LooseDb -> Scope -> IntMap.IntMap Int -> Bool
looseJoinWitness db _ctx env =
  case (IntMap.lookup 0 env, IntMap.lookup 1 env) of
    (Just leftValue, Just rightValue) ->
      (leftValue, rightValue) `elem` ldWitnesses db
    _ ->
      False

looseDb :: LooseDb
looseDb =
  LooseDb
    { ldSlotDomains =
        Map.fromList [(0, [1]), (1, [20])],
      ldWitnesses =
        [(1, 10)]
    }

atomRow :: [Int] -> RowTupleKey
atomRow =
  tupleKeyFromInts

normalizeEnvs :: [IntMap.IntMap Int] -> [[(Int, Int)]]
normalizeEnvs =
  sort . fmap IntMap.toAscList

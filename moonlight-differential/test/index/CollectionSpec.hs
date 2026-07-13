{-# LANGUAGE DerivingStrategies #-}

module CollectionSpec
  ( tests,
  )
where

import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Moonlight.Differential.Collection
  ( InputAdvance (..),
    RelationPlan (..),
    Update (..),
    advanceInput,
    bootstrapInput,
    collectionFromList,
    collectionToAscList,
    concatIndexedCollections,
    concatCollections,
    concatenateCollections,
    countCollectionByKey,
    deindexCollection,
    differenceCollections,
    differenceIndexedCollections,
    distinctCollection,
    filterCollection,
    flatMapCollection,
    indexCollectionBy,
    indexedCollectionToAscList,
    inputRows,
    iterateCollection,
    joinCollections,
    mapCollection,
    negateCollection,
    negateIndexedCollection,
    validateInput,
  )
import Moonlight.Differential.Index.IndexedRows
  ( IndexedRowBindingError (..),
    IndexedRowFormat,
    indexedRowFormat,
    indexedRowsPayloadMap,
  )
import Moonlight.Differential.Operator.Fixpoint
  ( SemiNaiveBudget (..),
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( assertEqual,
    assertFailure,
    testCase,
  )

tests :: TestTree
tests =
  testGroup
    "collection EDSL laws"
    [ testCase "Collection map/filter/index/count/join compose without raw substrate imports" collectionOperatorsCompose,
      testCase "Collection iterate exposes semi-naive reachability shape" collectionIterateMatchesClosure,
      testCase "InputCollection advances maintained relation views" inputCollectionAdvancesRelationViews
    ]

collectionOperatorsCompose :: IO ()
collectionOperatorsCompose = do
  let source =
        collectionFromList
          [ (1 :: Int, 1 :: Int),
            (2, 2),
            (3, -1),
            (4, 5)
          ]
      transformed =
        filterCollection even (mapCollection (+ 1) source)
      indexed =
        indexCollectionBy (`mod` 4) transformed
      counted =
        countCollectionByKey indexed
      expanded =
        flatMapCollection (\value -> [value, value + 10]) transformed
      rekeyed =
        deindexCollection (,) indexed
      right =
        indexCollectionBy fst (collectionFromList [((2 :: Int, 'a'), 3 :: Int), ((2, 'b'), 7), ((0, 'z'), 11)])
      joined =
        joinCollections (indexCollectionBy id transformed) right
  assertEqual
    "map/filter produce the expected collection"
    [(2, 1), (4, -1)]
    (collectionToAscList transformed)
  assertEqual
    "flatMap preserves each emitted value at the source weight"
    [(2, 1), (4, -1), (12, 1), (14, -1)]
    (collectionToAscList expanded)
  assertEqual
    "concat and difference expose the collection group law"
    (collectionToAscList source)
    (collectionToAscList (differenceCollections (concatCollections source transformed) transformed))
  assertEqual
    "concatenate folds collection addition"
    (collectionToAscList (source <> transformed))
    (collectionToAscList (concatenateCollections [source, transformed]))
  assertEqual
    "negation cancels the collection"
    []
    (collectionToAscList (concatCollections transformed (negateCollection transformed)))
  assertEqual
    "index/count stay in the collection vocabulary"
    [(0, -1), (2, 1)]
    (collectionToAscList counted)
  assertEqual
    "indexed asc list flattens each key bucket"
    [(0, 4, -1), (2, 2, 1)]
    (indexedCollectionToAscList indexed)
  assertEqual
    "deindex returns indexed rows to collection vocabulary"
    [((0, 4), -1), ((2, 2), 1)]
    (collectionToAscList rekeyed)
  assertEqual
    "indexed difference removes identical support"
    []
    (indexedCollectionToAscList (differenceIndexedCollections indexed indexed))
  assertEqual
    "indexed negation cancels with indexed addition"
    []
    (indexedCollectionToAscList (concatIndexedCollections indexed (negateIndexedCollection indexed)))
  assertEqual
    "indexed join multiplies matching weights"
    [((2, 2, (2, 'a')), 3), ((2, 2, (2, 'b')), 7)]
    (collectionToAscList joined)

collectionIterateMatchesClosure :: IO ()
collectionIterateMatchesClosure =
  case iterateCollection (SemiNaiveBudget 8) step seed of
    Left obstruction ->
      assertFailure ("unexpected collection iteration obstruction: " <> show obstruction)
    Right closure ->
      assertEqual
        "semi-naive collection iteration accumulates fresh support"
        [(0, 1), (1, 1), (2, 1), (3, 1)]
        (collectionToAscList (distinctCollection closure))
  where
    seed =
      collectionFromList [(0 :: Int, 1 :: Int)]

    step =
      mapCollection (+ 1) . filterCollection (< 3)

inputCollectionAdvancesRelationViews :: IO ()
inputCollectionAdvancesRelationViews = do
  let initialUpdates =
        [ Update 0 1 10 2,
          Update 0 2 20 5
        ]
      deltaUpdates =
        [ Update 1 1 10 (-2),
          Update 1 2 20 3,
          Update 1 3 30 7
        ]
      plan =
        RelationPlan
          { relationIndexedFormat = queryRowFormat,
            relationLayoutColumnIndex = queryLayoutColumnIndex,
            relationLayout = queryLayout,
            relationProjectCell = queryProjectCell
          }
  case bootstrapInput plan initialUpdates of
    Left obstruction ->
      assertFailure ("unexpected input bootstrap obstruction: " <> show obstruction)
    Right input ->
      case advanceInput deltaUpdates input of
        Left obstruction ->
          assertFailure ("unexpected input advance obstruction: " <> show obstruction)
        Right advanced -> do
          case validateInput (inputAdvanceNext advanced) of
            Left obstruction ->
              assertFailure ("advanced input failed validation: " <> show obstruction)
            Right () ->
              pure ()
          assertEqual
            "input collection keeps row payloads maintained from deltas"
            ( Map.fromList
                [ (QueryRowKey [2, 20], 8),
                  (QueryRowKey [3, 30], 7)
                ]
            )
            (indexedRowsPayloadMap (inputRows (inputAdvanceNext advanced)))

newtype QueryRowKey = QueryRowKey
  { unQueryRowKey :: [Int]
  }
  deriving stock (Eq, Ord, Show)

queryLayout :: [Int]
queryLayout =
  [0, 1]

queryLayoutColumnIndex :: [Int] -> IntMap Int
queryLayoutColumnIndex layout =
  IntMap.fromList (zip layout [0 ..])

queryRowFormat :: IndexedRowFormat [Int] QueryRowKey
queryRowFormat =
  indexedRowFormat (length . unQueryRowKey) length queryFoldBindings

queryFoldBindings ::
  [Int] ->
  QueryRowKey ->
  (Int -> Int -> acc -> acc) ->
  acc ->
  Either (IndexedRowBindingError [Int] QueryRowKey) acc
queryFoldBindings layout (QueryRowKey values) step initial
  | length layout /= length values =
      Left (IndexedRowWidthMismatch (QueryRowKey values) (length layout) (length values))
  | otherwise =
      Right (foldl' foldBinding initial (zip layout values))
  where
    foldBinding acc (slot, value) =
      step slot value acc

queryProjectCell :: Int -> Int -> Int -> Int -> Maybe (QueryRowKey, Int)
queryProjectCell _time key value weight =
  Just (QueryRowKey [key, value], weight)

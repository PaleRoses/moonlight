module Test.Moonlight.Flow.Property.Storage
  ( relationValidatesOnStream,
    separatorIndexValidates,
    bucketDenotationMismatchDetected,
    inverseMismatchDetected,
    liveRefcountMismatchDetected,
    rowWidthMismatchDetected,
    separatorMismatchDetected,
    storageProperties,
  )
where

import Data.Foldable qualified as Foldable
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Vector qualified as Vector
import Moonlight.Core (SlotId, mkSlotId)
import Moonlight.Delta.Signed
  ( Multiplicity (..),
    MultiplicityChange (..)
  )
import Moonlight.Differential.Row.Patch
  ( plainRowPatchFromList,
  )
import Moonlight.Differential.Row.Tuple
  ( RowTupleKey,
    SeparatorTupleKey,
    coerceTupleKey,
    tupleKeyFromInts,
  )
import Moonlight.Differential.Index.IndexedRows
  ( IndexedRows,
    indexedRowsKeyByRowId,
    indexedRowsValueIndex,
  )
import Moonlight.Flow.Storage.Index.Validate
  ( IndexValidationError (..),
    validateRelationIndex,
    validateSeparatorIndexForRelation,
  )
import Moonlight.Flow.Storage.Separator
  ( SeparatorIndex (..),
    buildSeparatorIndex,
  )
import Moonlight.Differential.Row.Block
  ( RowLayout,
  )
import Moonlight.Flow.Storage.Relation
  ( Relation (..),
    RelationPatchError (..),
    RowIdDelta (..),
    applyRelationPatchTracked,
    emptyRowIdDelta,
    relationFromKeyedRows,
  )
import Moonlight.Differential.Index.RowIdSet
  ( rowIdSetFromIntSetCanonical,
  )
import Test.Moonlight.Differential.Index.IndexedRows
  ( indexedRowsWithIdByKeyForValidation,
    indexedRowsWithKeyByRowIdForValidation,
    indexedRowsWithLiveRowsForValidation,
    indexedRowsWithValueIndexForValidation,
  )
import Test.Moonlight.Flow.Gen.Storage
  ( IndexedRowsOp,
    genIndexedRowsOpStream,
  )
import Test.Moonlight.Flow.Oracle.Storage
  ( OracleIndexedRows (..),
    emptyOracleIndexedRows,
    oracleApplyIndexedRowsOp,
  )
import Test.QuickCheck
  ( Property,
    counterexample,
    forAll,
    (===),
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)

relationValidatesOnStream :: Property
relationValidatesOnStream =
  forAll (genIndexedRowsOpStream 3 256) $ \ops ->
    case relationFromOps ops of
      Left err ->
        counterexample ("relationFromOps failed: " <> show err) False
      Right relation ->
        validateRelationIndex relation === Right ()

separatorIndexValidates :: Property
separatorIndexValidates =
  forAll (genIndexedRowsOpStream 3 256) $ \ops ->
    case relationFromOps ops of
      Left err ->
        counterexample ("relationFromOps failed: " <> show err) False
      Right relation ->
        let separator = buildSeparatorIndex relation (Vector.fromList [mkSlotId 0])
         in validateSeparatorIndexForRelation relation separator === Right ()

bucketDenotationMismatchDetected :: Assertion
bucketDenotationMismatchDetected =
  withValidRelation $ \validRelation ->
    assertValidationContains
      "bucket denotation mismatch"
      isBucketDenotationMismatch
      (validateRelationIndex (corruptRelationRows corruptValueBucket validRelation))

inverseMismatchDetected :: Assertion
inverseMismatchDetected =
  withValidRelation $ \validRelation ->
    assertValidationContains
      "inverse mismatch"
      isInverseMismatch
      (validateRelationIndex (corruptRelationRows corruptInverse validRelation))

liveRefcountMismatchDetected :: Assertion
liveRefcountMismatchDetected =
  withValidRelation $ \validRelation ->
    assertValidationContains
      "live-row/refcount mismatch"
      isLiveRefcountMismatch
      (validateRelationIndex (corruptRelationRows corruptLiveRows validRelation))

rowWidthMismatchDetected :: Assertion
rowWidthMismatchDetected =
  withValidRelation $ \validRelation ->
    assertValidationContains
      "row width mismatch"
      isRowWidthMismatch
      (validateRelationIndex (corruptRelationRows corruptRowWidth validRelation))

separatorMismatchDetected :: Assertion
separatorMismatchDetected =
  withValidRelation $ \validRelation ->
    assertValidationContains
      "separator row/key mismatch"
      isSeparatorMismatch
      (validateSeparatorIndexForRelation validRelation (corruptSeparatorIndex validRelation))

missingRelationRowDeleteRejected :: Assertion
missingRelationRowDeleteRejected =
  let rowValue =
        atomRow [1, 10, 100]
   in do
        relation <- expectRelation (relationFromKeyedRows storageSchema [])
        applyRelationPatchTracked (plainRowPatchFromList [(rowValue, MultiplicityChange (-1))]) relation
          @?= Left (RelationPatchMissingRowDelete rowValue (MultiplicityChange (-1)))

relationRowUnderflowRejected :: Assertion
relationRowUnderflowRejected =
  let rowValue =
        atomRow [1, 10, 100]
   in do
        relation <- expectRelation (relationFromKeyedRows storageSchema [(0, rowValue)])
        applyRelationPatchTracked (plainRowPatchFromList [(rowValue, MultiplicityChange (-2))]) relation
          @?= Left (RelationPatchMultiplicityUnderflow rowValue (Multiplicity 1) (MultiplicityChange (-2)))

relationRowZeroDeleteTracked :: Assertion
relationRowZeroDeleteTracked =
  let rowValue =
        atomRow [1, 10, 100]
      expectedDelta =
        emptyRowIdDelta {ridDeleted = IntMap.singleton 0 rowValue}
   in do
        relation <- expectRelation (relationFromKeyedRows storageSchema [(0, rowValue)])
        fmap snd (applyRelationPatchTracked (plainRowPatchFromList [(rowValue, MultiplicityChange (-1))]) relation)
          @?= Right expectedDelta

storageProperties :: TestTree
storageProperties =
  testGroup
    "storage"
    [ testProperty "Relation validates after randomized stream" relationValidatesOnStream,
      testProperty "SeparatorIndex validates after randomized stream" separatorIndexValidates,
      testGroup
        "Relation negative cases"
        [ testCase "bucket denotation mismatch detected" bucketDenotationMismatchDetected,
          testCase "inverse mismatch detected" inverseMismatchDetected,
          testCase "live-row/refcount mismatch detected" liveRefcountMismatchDetected,
          testCase "row width mismatch detected" rowWidthMismatchDetected,
          testCase "separator row/key mismatch detected" separatorMismatchDetected,
          testCase "missing negative row ref is rejected" missingRelationRowDeleteRejected,
          testCase "negative row ref underflow is rejected" relationRowUnderflowRejected,
          testCase "exact zero row ref deletes with row id delta" relationRowZeroDeleteTracked
        ]
    ]

relationFromOps :: [IndexedRowsOp] -> Either RelationPatchError Relation
relationFromOps ops =
  relationFromKeyedRows
    storageSchema
    (IntMap.toAscList (oirRowById oracle))
  where
    oracle =
      Foldable.foldl' (flip oracleApplyIndexedRowsOp) (emptyOracleIndexedRows 3) ops

validRelationEither :: Either RelationPatchError Relation
validRelationEither =
  relationFromKeyedRows
    storageSchema
    [ (0, atomRow [1, 10, 100]),
      (1, atomRow [2, 20, 200]),
      (2, atomRow [3, 30, 300])
        ]

withValidRelation :: (Relation -> Assertion) -> Assertion
withValidRelation useRelation =
  expectRelation validRelationEither >>= useRelation

expectRelation :: Either RelationPatchError Relation -> IO Relation
expectRelation =
  either
    (\err -> assertFailure ("expected valid relation, got: " <> show err))
    pure

corruptRelationRows ::
  (IndexedRows RowLayout RowTupleKey Multiplicity -> IndexedRows RowLayout RowTupleKey Multiplicity) ->
  Relation ->
  Relation
corruptRelationRows edit relation =
  relation {relRows = edit (relRows relation)}

corruptValueBucket :: IndexedRows RowLayout RowTupleKey Multiplicity -> IndexedRows RowLayout RowTupleKey Multiplicity
corruptValueBucket rows =
  indexedRowsWithValueIndexForValidation
    ( IntMap.insert
        0
        (IntMap.singleton 999 (rowIdSetFromIntSetCanonical (IntSet.singleton 0)))
        (indexedRowsValueIndex rows)
    )
    rows

corruptInverse :: IndexedRows RowLayout RowTupleKey Multiplicity -> IndexedRows RowLayout RowTupleKey Multiplicity
corruptInverse rows =
  indexedRowsWithIdByKeyForValidation Map.empty rows

corruptLiveRows :: IndexedRows RowLayout RowTupleKey Multiplicity -> IndexedRows RowLayout RowTupleKey Multiplicity
corruptLiveRows rows =
  indexedRowsWithLiveRowsForValidation IntSet.empty rows

corruptRowWidth :: IndexedRows RowLayout RowTupleKey Multiplicity -> IndexedRows RowLayout RowTupleKey Multiplicity
corruptRowWidth rows =
  indexedRowsWithKeyByRowIdForValidation
    (IntMap.insert 0 (atomRow [42]) (indexedRowsKeyByRowId rows))
    rows

corruptSeparatorIndex :: Relation -> SeparatorIndex
corruptSeparatorIndex validRelation =
  let separator = buildSeparatorIndex validRelation (Vector.fromList [mkSlotId 0])
   in separator {siRowToKey = IntMap.insert 0 bogusSepKey (siRowToKey separator)}

assertValidationContains ::
  String ->
  (IndexValidationError -> Bool) ->
  Either [IndexValidationError] () ->
  Assertion
assertValidationContains label predicate result =
  case result of
    Left errors
      | any predicate errors -> pure ()
      | otherwise -> assertFailure ("expected " <> label <> ", got " <> show errors)
    Right () -> assertFailure ("expected " <> label <> ", validation accepted corrupted index")

isBucketDenotationMismatch :: IndexValidationError -> Bool
isBucketDenotationMismatch = \case
  BucketDenotationMismatch {} -> True
  _ -> False

isInverseMismatch :: IndexValidationError -> Bool
isInverseMismatch = \case
  RelationInverseMissing {} -> True
  RelationReverseInverseMismatch {} -> True
  _ -> False

isLiveRefcountMismatch :: IndexValidationError -> Bool
isLiveRefcountMismatch = \case
  RelationLiveRowsDoNotMatchMultiplicities {} -> True
  _ -> False

isRowWidthMismatch :: IndexValidationError -> Bool
isRowWidthMismatch = \case
  RelationRowWidthMismatch {} -> True
  _ -> False

isSeparatorMismatch :: IndexValidationError -> Bool
isSeparatorMismatch = \case
  SeparatorRowToKeyMismatch {} -> True
  _ -> False

atomRow :: [Int] -> RowTupleKey
atomRow =
  tupleKeyFromInts

bogusSepKey :: SeparatorTupleKey
bogusSepKey =
  coerceTupleKey (atomRow [999])

storageSchema :: Vector.Vector SlotId
storageSchema =
  Vector.fromList (fmap mkSlotId [0, 1, 2])

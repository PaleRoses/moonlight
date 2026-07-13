{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}

module RowSpec
  ( tests,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Data.Vector.Unboxed qualified as VU
import Data.Word (Word64)
import Moonlight.Core (mkSlotId)
import Moonlight.Differential.Row.Block
  ( RowBlock,
    RowBlockIdentity (..),
    RowLayout,
    RowState (Canonical),
    compileRowRestriction,
    containsRestrictedRow,
    containsRow,
    differenceRows,
    foldRowBlock,
    fromSlotRows,
    intersectRows,
    restrictRows,
    rowBlockByteSize,
    rowBlockCount,
    rowBlockIdentity,
    rowBlockLayout,
    rowSlots,
    unionRows,
    withRowBlockIndex,
  )
import Test.QuickCheck
  ( Arbitrary (..),
    Gen,
    Property,
    chooseInt,
    conjoin,
    counterexample,
    forAll,
    listOf,
    suchThatMap,
    vectorOf,
    (===),
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

newtype GeneratedRows = GeneratedRows (RowBlock 'Canonical)
  deriving stock (Show)

instance Arbitrary GeneratedRows where
  arbitrary = GeneratedRows <$> genAtomRows 6 64

testRowIdentity :: RowBlockIdentity
testRowIdentity =
  RowBlockIdentity
    { rowBlockBaseRevision = 0,
      rowBlockOverlayEpoch = 0,
      rowBlockPlanFingerprint = 0,
      rowBlockEntityKey = 0,
      rowBlockGeneration = 0
    }

genSchema :: Int -> Gen RowLayout
genSchema maxWidth = do
  width <- chooseInt (0, max 0 maxWidth)
  pure (Vector.fromList (fmap mkSlotId [0 .. width - 1]))

genSlotRow :: Int -> Gen (VU.Vector Word64)
genSlotRow width =
  VU.fromList . fmap fromIntegral <$> vectorOf (max 0 width) (chooseInt (0, 64))

genAtomRows :: Int -> Int -> Gen (RowBlock 'Canonical)
genAtomRows maxWidth maxRows =
  genSchema maxWidth >>= flip genAtomRowsWithSchema maxRows

genAtomRowsWithSchema :: RowLayout -> Int -> Gen (RowBlock 'Canonical)
genAtomRowsWithSchema schema maxRows =
  suchThatMap genSlotRows $
    either (const Nothing) Just . fromSlotRows testRowIdentity schema
  where
    genSlotRows = do
      rowCount <- chooseInt (0, max 0 maxRows)
      vectorOf rowCount (genSlotRow (Vector.length schema))

genRestrictionMap :: Gen (IntMap Int)
genRestrictionMap =
  IntMap.fromList <$> listOf entry
  where
    entry = do
      source <- chooseInt (0, 64)
      target <- chooseInt (0, 16)
      pure (source, target)

rowSetModel :: RowBlock state -> Set (VU.Vector Word64)
rowSetModel rows =
  foldRowBlock
    (\acc desc -> Set.insert (rowSlots rows desc) acc)
    Set.empty
    rows

restrictRowModel :: IntMap Int -> VU.Vector Word64 -> VU.Vector Word64
restrictRowModel restriction =
  VU.map
    ( \slotWord ->
        let slotKey = fromIntegral slotWord
         in fromIntegral (IntMap.findWithDefault slotKey slotKey restriction)
    )

restrictRowsModel :: IntMap Int -> RowBlock state -> Set (VU.Vector Word64)
restrictRowsModel restriction =
  Set.map (restrictRowModel restriction) . rowSetModel

sealRowsCanonical :: Property
sealRowsCanonical =
  forAll (genAtomRows 6 64) $ \rows ->
    let materialized = Set.toAscList (rowSetModel rows)
     in case fromSlotRows (rowBlockIdentity rows) (rowBlockLayout rows) (materialized <> materialized) of
          Left err -> counterexample (show err) False
          Right resealed -> rowSetModel resealed === rowSetModel rows

rowSetAlgebraOracle :: Property
rowSetAlgebraOracle =
  forAll (genAtomRows 6 64) $ \leftRows ->
    forAll (genAtomRowsWithSchema (rowBlockLayout leftRows) 64) $ \rightRows ->
      let identityValue = rowBlockIdentity leftRows
          leftModel = rowSetModel leftRows
          rightModel = rowSetModel rightRows
       in conjoin
            [ fmap rowSetModel (unionRows identityValue leftRows rightRows) === Right (Set.union leftModel rightModel),
              fmap rowSetModel (differenceRows identityValue leftRows rightRows) === Right (Set.difference leftModel rightModel),
              fmap rowSetModel (intersectRows identityValue leftRows rightRows) === Right (Set.intersection leftModel rightModel)
            ]

containsRowEquivalence :: GeneratedRows -> GeneratedRows -> Property
containsRowEquivalence (GeneratedRows targetRows) (GeneratedRows candidateRows) =
  let indexedTarget = withRowBlockIndex targetRows
   in conjoin
        ( foldRowBlock
            ( \acc desc ->
                (containsRow targetRows candidateRows desc === containsRow indexedTarget candidateRows desc)
                  : acc
            )
            []
            candidateRows
        )

restrictRowsOracle :: GeneratedRows -> Property
restrictRowsOracle (GeneratedRows rows) =
  forAll genRestrictionMap $ \restriction ->
    case compileRowRestriction (rowBlockLayout rows) restriction of
      Left err -> counterexample (show err) False
      Right program ->
        case restrictRows program (rowBlockIdentity rows) rows of
          Left err -> counterexample (show err) False
          Right restricted ->
            rowSetModel restricted === restrictRowsModel restriction rows

containsRestrictedComposition :: GeneratedRows -> Property
containsRestrictedComposition (GeneratedRows rows) =
  forAll genRestrictionMap $ \restriction ->
    case compileRowRestriction (rowBlockLayout rows) restriction of
      Left err -> counterexample (show err) False
      Right program ->
        case restrictRows program (rowBlockIdentity rows) rows of
          Left err -> counterexample (show err) False
          Right restricted ->
            let restrictedModel = rowSetModel restricted
             in conjoin
                  ( foldRowBlock
                      ( \acc desc ->
                          ( containsRestrictedRow program restricted rows desc
                              === Set.member (restrictRowModel restriction (rowSlots rows desc)) restrictedModel
                          )
                            : acc
                      )
                      []
                      rows
                  )

rowByteSizeStats :: GeneratedRows -> Property
rowByteSizeStats (GeneratedRows rows) =
  let foldedCount =
        foldRowBlock
          (\count _desc -> count + 1)
          0
          rows
   in conjoin
        [ foldedCount === rowBlockCount rows,
          counterexample "row byte size cannot be smaller than descriptors" $
            rowBlockByteSize rows >= rowBlockCount rows * 24
        ]

tests :: TestTree
tests =
  testGroup
    "row"
    [ testProperty "sealRows idempotent and canonical" sealRowsCanonical,
      testProperty "union/diff/intersect agree with Set oracle" rowSetAlgebraOracle,
      testProperty "containsRow and indexedContains equivalent" containsRowEquivalence,
      testProperty "restrictRows matches materialized oracle" restrictRowsOracle,
      testProperty "containsRestrictedRow == restrict + contains" containsRestrictedComposition,
      testProperty "byte size stats match payload descriptors" rowByteSizeStats
    ]

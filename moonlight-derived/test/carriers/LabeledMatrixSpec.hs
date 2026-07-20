module LabeledMatrixSpec (tests) where

import Data.List (isInfixOf)
import Moonlight.Core (MoonlightError (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=), assertBool, assertFailure)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Vector as V
import Moonlight.Derived.Pure.Site.Poset (FinObjectId (..), DerivedPoset, mkDerivedPosetFromCovers)
import Moonlight.Derived.Pure.Site.LabeledMatrix

expectPoset :: [FinObjectId] -> [(FinObjectId, FinObjectId)] -> DerivedPoset
expectPoset ns cs = either (error . show) id (mkDerivedPosetFromCovers ns cs)

expectRight :: Show err => Either err a -> IO a
expectRight = either (assertFailure . show) pure

expectSetBlock :: (Eq a, Num a) => FinObjectId -> FinObjectId -> DenseMat a -> BlockedMat a -> IO (BlockedMat a)
expectSetBlock rowLabel columnLabel blockValue blockedMat =
  expectRight (setBlockChecked rowLabel columnLabel blockValue blockedMat)

tests :: TestTree
tests = testGroup "LabeledMatrix"
  [ denseMatTests
  , groupedAxisTests
  , blockedMatTests
  ]

denseMatTests :: TestTree
denseMatTests = testGroup "DenseMat"
  [ testCase "identity * identity = identity" $ do
      let i3 = identMat 3 :: DenseMat Int
      matMul i3 i3 @?= i3

  , testCase "identity * A = A" $ do
      let a = DenseMat 2 3 (V.fromList
                [ V.fromList [1, 2, 3]
                , V.fromList [4, 5, 6] ]) :: DenseMat Int
          i2 = identMat 2
      matMul i2 a @?= a

  , testCase "A * identity = A" $ do
      let a = DenseMat 2 3 (V.fromList
                [ V.fromList [1, 2, 3]
                , V.fromList [4, 5, 6] ]) :: DenseMat Int
          i3 = identMat 3
      matMul a i3 @?= a

  , testCase "2x2 multiplication" $ do
      let a = DenseMat 2 2 (V.fromList
                [ V.fromList [1, 2]
                , V.fromList [3, 4] ]) :: DenseMat Int
          b = DenseMat 2 2 (V.fromList
                [ V.fromList [5, 6]
                , V.fromList [7, 8] ])
          expected = DenseMat 2 2 (V.fromList
                [ V.fromList [19, 22]
                , V.fromList [43, 50] ])
      matMul a b @?= expected

  , testCase "matAddChecked rejects mismatched shapes" $ do
      let leftMat = DenseMat 1 2 (V.fromList [V.fromList [1, 2]]) :: DenseMat Int
          rightMat = DenseMat 1 1 (V.fromList [V.fromList [3]])
      assertLeft (matAddChecked leftMat rightMat)

  , testCase "matAddChecked rejects zero-shape mismatches" $ do
      let leftMat = zeroMat 0 2 :: DenseMat Int
          rightMat = zeroMat 1 2
      assertLeft (matAddChecked leftMat rightMat)

  , testCase "mkDenseMat rejects ragged payloads" $ do
      assertLeft
        ( mkDenseMat
            2
            2
            (V.fromList [V.fromList [1, 2], V.fromList [3]] :: V.Vector (V.Vector Int))
        )

  , testCase "mkDenseMat rejects negative shapes" $ do
      assertLeft
        ( mkDenseMat
            (-1)
            1
            (V.empty :: V.Vector (V.Vector Int))
        )

  , testCase "denseFromEntriesWithChecked rejects out-of-bounds entries" $ do
      assertLeft
        ( denseFromEntriesWithChecked
            1
            1
            [(1 :: Int, 0 :: Int, 7 :: Int)]
            (\(rowIndex, columnIndex, _) -> (rowIndex, columnIndex))
            (\(_, _, value) -> value)
        )

  , testCase "zero matrix" $ do
      let z = zeroMat 2 3 :: DenseMat Int
      assertBool "zero is zero" (isZeroMat z)
      assertBool "identity is not zero" (not (isZeroMat (identMat 2 :: DenseMat Int)))

  , testCase "transpose involution" $ do
      let a = DenseMat 2 3 (V.fromList
                [ V.fromList [1, 2, 3]
                , V.fromList [4, 5, 6] ]) :: DenseMat Int
      transposeMat (transposeMat a) @?= a

  , testCase "transpose dimensions" $ do
      let a = DenseMat 2 3 (V.fromList
                [ V.fromList [1, 2, 3]
                , V.fromList [4, 5, 6] ]) :: DenseMat Int
          t = transposeMat a
      dmRows t @?= 3
      dmCols t @?= 2

  , testCase "addScaledRowMat" $ do
      let a = DenseMat 2 2 (V.fromList
                [ V.fromList [1, 0]
                , V.fromList [0, 1] ]) :: DenseMat Int
          result = addScaledRowMat 0 3 1 a
      matIndex result 0 0 @?= 1
      matIndex result 0 1 @?= 3

  , testCase "deleteRowsMat" $ do
      let a = DenseMat 3 2 (V.fromList
                [ V.fromList [1, 2]
                , V.fromList [3, 4]
                , V.fromList [5, 6] ]) :: DenseMat Int
          result = deleteRowsMat [1] a
      dmRows result @?= 2
      matIndex result 0 0 @?= 1
      matIndex result 1 0 @?= 5

  , testCase "hcat" $ do
      let a = DenseMat 2 1 (V.fromList
                [ V.fromList [1], V.fromList [3] ]) :: DenseMat Int
          b = DenseMat 2 2 (V.fromList
                [ V.fromList [2, 0], V.fromList [4, 0] ])
          result = hcat [a, b]
      dmRows result @?= 2
      dmCols result @?= 3
      matIndex result 0 0 @?= 1
      matIndex result 0 1 @?= 2

  , testCase "vcat" $ do
      let a = DenseMat 1 2 (V.fromList [V.fromList [1, 2]]) :: DenseMat Int
          b = DenseMat 1 2 (V.fromList [V.fromList [3, 4]])
          result = vcat [a, b]
      dmRows result @?= 2
      matIndex result 0 0 @?= 1
      matIndex result 1 0 @?= 3

  , testCase "hcatChecked rejects mismatched row counts" $ do
      let leftMat = DenseMat 1 1 (V.fromList [V.fromList [1]]) :: DenseMat Int
          rightMat = DenseMat 2 1 (V.fromList [V.fromList [2], V.fromList [3]])
      assertLeft (hcatChecked [leftMat, rightMat])
  ]

groupedAxisTests :: TestTree
groupedAxisTests = testGroup "GroupedAxis"
  [ testCase "emptyAxis has size 0" $ do
      axisSize emptyAxis @?= 0

  , testCase "fromLabels with multiplicities" $ do
      let labels = V.fromList [FinObjectId 0, FinObjectId 1, FinObjectId 0, FinObjectId 1, FinObjectId 1]
          ga = fromLabels labels
      axisMultiplicity ga (FinObjectId 0) @?= 2
      axisMultiplicity ga (FinObjectId 1) @?= 3
      axisSize ga @?= 5

  , testCase "fromLabels preserves order" $ do
      let labels = V.fromList [FinObjectId 2, FinObjectId 0, FinObjectId 1, FinObjectId 2]
          ga = fromLabels labels
      V.toList (gaOrder ga) @?= [FinObjectId 2, FinObjectId 0, FinObjectId 1]

  , testCase "appendAxisLabel new label" $ do
      let ga = fromLabels (V.fromList [FinObjectId 0])
          ga' = appendAxisLabel (FinObjectId 1) 2 ga
      axisMultiplicity ga' (FinObjectId 0) @?= 1
      axisMultiplicity ga' (FinObjectId 1) @?= 2
      axisSize ga' @?= 3

  , testCase "appendAxisLabel existing label" $ do
      let ga = fromLabels (V.fromList [FinObjectId 0, FinObjectId 0])
          ga' = appendAxisLabel (FinObjectId 0) 3 ga
      axisMultiplicity ga' (FinObjectId 0) @?= 5
      axisSize ga' @?= 5

  , testCase "restrictAxis" $ do
      let ga = fromLabels (V.fromList [FinObjectId 0, FinObjectId 1, FinObjectId 2])
          ga' = restrictAxis (IS.fromList [0, 2]) ga
      axisMultiplicity ga' (FinObjectId 0) @?= 1
      axisMultiplicity ga' (FinObjectId 1) @?= 0
      axisMultiplicity ga' (FinObjectId 2) @?= 1

  , testCase "axisSlices" $ do
      let ga = fromLabels (V.fromList [FinObjectId 0, FinObjectId 0, FinObjectId 1, FinObjectId 2, FinObjectId 2, FinObjectId 2])
          slices = axisSlices ga
      IM.lookup 0 slices @?= Just (0, 2)
      IM.lookup 1 slices @?= Just (2, 1)
      IM.lookup 2 slices @?= Just (3, 3)

  , testCase "relabelAxis merges labels" $ do
      let ga = fromLabels (V.fromList [FinObjectId 0, FinObjectId 1, FinObjectId 2])
          ga' = relabelAxis (\(FinObjectId n) -> FinObjectId (n `mod` 2)) ga
      axisMultiplicity ga' (FinObjectId 0) @?= 2
      axisMultiplicity ga' (FinObjectId 1) @?= 1

  , testCase "relabelOffsets tracks merged offsets in order" $ do
      let ga = fromLabels (V.fromList [FinObjectId 0, FinObjectId 0, FinObjectId 1, FinObjectId 2])
          (ga', offsets) = relabelOffsets (\(FinObjectId n) -> FinObjectId (n `mod` 2)) ga
      V.toList (gaOrder ga') @?= [FinObjectId 0, FinObjectId 1]
      axisMultiplicity ga' (FinObjectId 0) @?= 3
      axisMultiplicity ga' (FinObjectId 1) @?= 1
      IM.lookup 0 offsets @?= Just (FinObjectId 0, 0)
      IM.lookup 1 offsets @?= Just (FinObjectId 1, 0)
      IM.lookup 2 offsets @?= Just (FinObjectId 0, 2)

  , testCase "removeAxisIndices ignores duplicates and out-of-range indices" $ do
      let ga = fromLabels (V.fromList [FinObjectId 0, FinObjectId 0, FinObjectId 0])
          ga' = removeAxisIndices (FinObjectId 0) [0, 0, 9] ga
      axisMultiplicity ga' (FinObjectId 0) @?= 2
      axisSize ga' @?= 2
  ]

assertLeft :: Either err value -> Assertion
assertLeft result =
  case result of
    Left _ ->
      pure ()
    Right _ ->
      assertFailure "expected Left, got Right"

blockedMatTests :: TestTree
blockedMatTests = testGroup "BlockedMat"
  [ testCase "zero blocked has no blocks" $ do
      let rows = fromLabels (V.fromList [FinObjectId 0, FinObjectId 1])
          cols = fromLabels (V.fromList [FinObjectId 0, FinObjectId 1])
          z = zeroBlocked rows cols :: BlockedMat Int
      assertBool "zero block at (0,0)" (isZeroMat (blockAt (FinObjectId 0) (FinObjectId 0) z))
      IM.null (bmBlocks z) @?= True

  , testCase "setBlock / blockAt roundtrip" $ do
      let rows = fromLabels (V.fromList [FinObjectId 0, FinObjectId 1])
          cols = fromLabels (V.fromList [FinObjectId 0, FinObjectId 1])
          blk = DenseMat 1 1 (V.fromList [V.fromList [42]]) :: DenseMat Int
      bm <- expectSetBlock (FinObjectId 0) (FinObjectId 1) blk (zeroBlocked rows cols)
      blockAt (FinObjectId 0) (FinObjectId 1) bm @?= blk
      assertBool "other block still zero" (isZeroMat (blockAt (FinObjectId 1) (FinObjectId 0) bm))
      storedBlockAt (FinObjectId 0) (FinObjectId 1) bm @?= Just blk
      storedBlockAt (FinObjectId 1) (FinObjectId 0) bm @?= Nothing

  , testCase "setBlock zero elides" $ do
      let rows = fromLabels (V.fromList [FinObjectId 0])
          cols = fromLabels (V.fromList [FinObjectId 0])
          blk = DenseMat 1 1 (V.fromList [V.fromList [42]]) :: DenseMat Int
      bm0 <- expectSetBlock (FinObjectId 0) (FinObjectId 0) blk (zeroBlocked rows cols)
      bm1 <- expectSetBlock (FinObjectId 0) (FinObjectId 0) (zeroMat 1 1) bm0
      IM.null (bmBlocks bm1) @?= True

  , testCase "setBlockChecked rejects wrong block shape" $ do
      let rows = fromLabels (V.fromList [FinObjectId 0])
          cols = fromLabels (V.fromList [FinObjectId 1])
          wrongBlock = DenseMat 2 1 (V.fromList [V.fromList [1], V.fromList [2]]) :: DenseMat Int
      assertLeft (setBlockChecked (FinObjectId 0) (FinObjectId 1) wrongBlock (zeroBlocked rows cols))

  , testCase "composeBlocked identity" $ do
      let ax = fromLabels (V.fromList [FinObjectId 0, FinObjectId 1])
          i0 = DenseMat 1 1 (V.fromList [V.fromList [1]]) :: DenseMat Int
          blk01 = DenseMat 1 1 (V.fromList [V.fromList [7]])
      ident1 <- expectSetBlock (FinObjectId 1) (FinObjectId 1) i0 (zeroBlocked ax ax)
      ident <- expectSetBlock (FinObjectId 0) (FinObjectId 0) i0 ident1
      m <- expectSetBlock (FinObjectId 0) (FinObjectId 1) blk01 (zeroBlocked ax ax)
      composeBlocked ident m @?= m

  , testCase "composeBlocked associative (small)" $ do
      let ax = fromLabels (V.fromList [FinObjectId 0, FinObjectId 1])
      leftMatrix <- expectSetBlock (FinObjectId 0) (FinObjectId 1) (DenseMat 1 1 (V.fromList [V.fromList [2]]) :: DenseMat Int) (zeroBlocked ax ax)
      middleMatrix <- expectSetBlock (FinObjectId 0) (FinObjectId 0) (DenseMat 1 1 (V.fromList [V.fromList [3]]) :: DenseMat Int) (zeroBlocked ax ax)
      rightMatrix <- expectSetBlock (FinObjectId 0) (FinObjectId 1) (DenseMat 1 1 (V.fromList [V.fromList [5]]) :: DenseMat Int) (zeroBlocked ax ax)
      composeBlocked (composeBlocked leftMatrix middleMatrix) rightMatrix @?= composeBlocked leftMatrix (composeBlocked middleMatrix rightMatrix)

  , testCase "expandBlocked / fromExpanded roundtrip" $ do
      let rowLabelsV = V.fromList [FinObjectId 0, FinObjectId 0, FinObjectId 1]
          colLabelsV = V.fromList [FinObjectId 0, FinObjectId 1, FinObjectId 1]
          mat = DenseMat 3 3 (V.fromList
                  [ V.fromList [1, 2, 3]
                  , V.fromList [4, 5, 6]
                  , V.fromList [7, 8, 9] ]) :: DenseMat Int
      bm <- expectRight (fromExpandedChecked rowLabelsV colLabelsV mat)
      let (rowsOut, colsOut, matOut) = expandBlocked bm
      rowsOut @?= rowLabelsV
      colsOut @?= colLabelsV
      matOut @?= mat

  , testCase "fromExpandedChecked rejects label and matrix shape mismatch" $ do
      let rowLabelsV = V.fromList [FinObjectId 0, FinObjectId 1]
          colLabelsV = V.fromList [FinObjectId 0]
          mat = DenseMat 1 1 (V.fromList [V.fromList [1]]) :: DenseMat Int
      assertLeft (fromExpandedChecked rowLabelsV colLabelsV mat)

  , testCase "restrictBlocked preserves subset" $ do
      let ax = fromLabels (V.fromList [FinObjectId 0, FinObjectId 1, FinObjectId 2])
          blk = DenseMat 1 1 (V.fromList [V.fromList [1]]) :: DenseMat Int
          bm = setBlock (FinObjectId 0) (FinObjectId 1) blk
                 (setBlock (FinObjectId 1) (FinObjectId 2) blk
                   (setBlock (FinObjectId 0) (FinObjectId 2) blk (zeroBlocked ax ax)))
          restricted = restrictBlocked (IS.fromList [0, 1]) bm
      blockAt (FinObjectId 0) (FinObjectId 1) restricted @?= blk
      axisMultiplicity (bmRows restricted) (FinObjectId 2) @?= 0
      axisMultiplicity (bmCols restricted) (FinObjectId 2) @?= 0

  , testCase "transposeBlockedMat swaps labels and transposes blocks" $ do
      let rows = fromLabels (V.fromList [FinObjectId 0, FinObjectId 1])
          cols = fromLabels (V.fromList [FinObjectId 2, FinObjectId 3])
          blockValue =
            DenseMat
              1
              1
              (V.fromList [V.fromList [7]])
              :: DenseMat Int
          blockedMatrix =
            setBlock (FinObjectId 0) (FinObjectId 3) blockValue (zeroBlocked rows cols)
          transposedMatrix = transposeBlockedMat blockedMatrix
      bmRows transposedMatrix @?= cols
      bmCols transposedMatrix @?= rows
      blockAt (FinObjectId 3) (FinObjectId 0) transposedMatrix @?= transposeMat blockValue

  , testCase "relabelBlocked merges" $ do
      let ax = fromLabels (V.fromList [FinObjectId 0, FinObjectId 1])
          b00 = DenseMat 1 1 (V.fromList [V.fromList [1]]) :: DenseMat Int
          b11 = DenseMat 1 1 (V.fromList [V.fromList [2]])
          bm = setBlock (FinObjectId 0) (FinObjectId 0) b00
                 (setBlock (FinObjectId 1) (FinObjectId 1) b11 (zeroBlocked ax ax))
      collapsed <- expectRight (relabelBlocked (Right . const (FinObjectId 0)) bm)
      axisMultiplicity (bmRows collapsed) (FinObjectId 0) @?= 2
      let diag = blockAt (FinObjectId 0) (FinObjectId 0) collapsed
      dmRows diag @?= 2
      dmCols diag @?= 2
      matIndex diag 0 0 @?= 1
      matIndex diag 1 1 @?= 2

  , testCase "starView filters by star membership" $ do
      let p = expectPoset
                [FinObjectId 0, FinObjectId 1, FinObjectId 2]
                [(FinObjectId 0, FinObjectId 1), (FinObjectId 1, FinObjectId 2)]
          ax = fromLabels (V.fromList [FinObjectId 0, FinObjectId 1, FinObjectId 2])
          blk = DenseMat 1 1 (V.fromList [V.fromList [1]]) :: DenseMat Int
          bm = setBlock (FinObjectId 0) (FinObjectId 0) blk
                 (setBlock (FinObjectId 1) (FinObjectId 1) blk
                   (setBlock (FinObjectId 2) (FinObjectId 2) blk (zeroBlocked ax ax)))
          sv = starView p (FinObjectId 1) bm
      dmRows sv @?= 2
      dmCols sv @?= 2
      matIndex sv 0 0 @?= 1
      matIndex sv 1 1 @?= 1

  , testCase "appendRowOnLabel" $ do
      let ax = fromLabels (V.fromList [FinObjectId 0, FinObjectId 1])
          bm = zeroBlocked ax ax :: BlockedMat Int
          payload = [(FinObjectId 0, V.fromList [1]), (FinObjectId 1, V.fromList [2])]
          bm' = appendRowOnLabel (FinObjectId 0) payload bm
      axisMultiplicity (bmRows bm') (FinObjectId 0) @?= 2
      let b00 = blockAt (FinObjectId 0) (FinObjectId 0) bm'
      dmRows b00 @?= 2
      matIndex b00 1 0 @?= 1
      let b01 = blockAt (FinObjectId 0) (FinObjectId 1) bm'
      matIndex b01 1 0 @?= 2

  , testCase "removeRowsOnLabel" $ do
      let ax = fromLabels (V.fromList [FinObjectId 0, FinObjectId 0, FinObjectId 1])
          blk = DenseMat 2 1 (V.fromList
                  [V.fromList [3], V.fromList [7]]) :: DenseMat Int
          bm = setBlock (FinObjectId 0) (FinObjectId 1) blk (zeroBlocked (fromLabels (V.fromList [FinObjectId 0, FinObjectId 0])) ax)
          bm' = removeRowsOnLabel (FinObjectId 0) [0] bm
      axisMultiplicity (bmRows bm') (FinObjectId 0) @?= 1
      matIndex (blockAt (FinObjectId 0) (FinObjectId 1) bm') 0 0 @?= 7

  , testCase "copyRowsInto embeds blocks into the new column axis instead of lying about shape" $ do
      let rows = fromLabels (V.fromList [FinObjectId 0])
          sourceCols = fromLabels (V.fromList [FinObjectId 0])
          targetCols = fromLabels (V.fromList [FinObjectId 0, FinObjectId 0, FinObjectId 1])
          blockValue = DenseMat 1 1 (V.fromList [V.fromList [9]]) :: DenseMat Int
      copied <-
        expectRight
          ( copyRowsInto
              targetCols
              (Just (setBlock (FinObjectId 0) (FinObjectId 0) blockValue (zeroBlocked rows sourceCols)))
          )
      blockAt (FinObjectId 0) (FinObjectId 0) copied
        @?= DenseMat 1 2 (V.fromList [V.fromList [9, 0]])
      assertBool "new column label stays zero" (isZeroMat (blockAt (FinObjectId 0) (FinObjectId 1) copied))

  , testCase "rowOp rejects out-of-range indices" $ do
      let axis = fromLabels (V.fromList [FinObjectId 0])
          blockValue = DenseMat 1 1 (V.fromList [V.fromList [1]]) :: DenseMat Int
          blockedMatrix = setBlock (FinObjectId 0) (FinObjectId 0) blockValue (zeroBlocked axis axis)
      case rowOp (FinObjectId 0) 1 1 (FinObjectId 0) 0 blockedMatrix of
        Left (InvariantViolation messageValue) ->
          assertBool "expected rowOp bounds failure" ("out of bounds" `isInfixOf` messageValue)
        Left otherError ->
          assertFailure ("expected InvariantViolation, received " <> show otherError)
        Right _ ->
          assertFailure "expected rowOp to reject an out-of-range target index"

  , testCase "colOp rejects out-of-range indices" $ do
      let axis = fromLabels (V.fromList [FinObjectId 0])
          blockValue = DenseMat 1 1 (V.fromList [V.fromList [1]]) :: DenseMat Int
          blockedMatrix = setBlock (FinObjectId 0) (FinObjectId 0) blockValue (zeroBlocked axis axis)
      case colOp (FinObjectId 0) 0 1 (FinObjectId 0) 1 blockedMatrix of
        Left (InvariantViolation messageValue) ->
          assertBool "expected colOp bounds failure" ("out of bounds" `isInfixOf` messageValue)
        Left otherError ->
          assertFailure ("expected InvariantViolation, received " <> show otherError)
        Right _ ->
          assertFailure "expected colOp to reject an out-of-range source index"
  ]

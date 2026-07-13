module SparseValidationSpec
  ( tests
  ) where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IORef
  ( IORef
  , atomicModifyIORef'
  , newIORef
  , readIORef
  , writeIORef
  )
import Data.Vector qualified as Vector
import Moonlight.Derived.Pure.Site.InjectiveComplex
  ( InjectiveComplex (..)
  , firstNonMinimal
  , isMinimal
  )
import Moonlight.Derived.Pure.Site.LabeledMatrix
  ( BlockedMat (..)
  , DenseMat (..)
  , GroupedAxis
  , appendAxisLabel
  , collapseBlockedDense
  , composeBlocked
  , composeBlockedIsZero
  , emptyAxis
  , isZeroMat
  , zeroBlocked
  )
import Moonlight.Derived.Pure.Site.Poset (FinObjectId (..))
import Moonlight.LinAlg (GF2)
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "SparseValidation"
    [ testCase "missing diagonal blocks do not request synthetic dense zeros" $
        sparseMinimalityTripwire
    , testCase "composeBlockedIsZero agrees with dense collapse on cancellation-shaped products" $
        sparseCompositionTripwire
    ]

data DenseZeroSentinel
  = SyntheticDenseZero
  | StoredDenseValue
  deriving stock (Show)

denseZeroComparisonCounter :: IORef Int
{-# NOINLINE denseZeroComparisonCounter #-}
denseZeroComparisonCounter =
  unsafePerformIO (newIORef 0)

instance Eq DenseZeroSentinel where
  leftValue == rightValue =
    unsafePerformIO
      ( do
          atomicModifyIORef'
            denseZeroComparisonCounter
            (\currentValue -> (currentValue + 1, ()))
          pure
            ( case (leftValue, rightValue) of
                (SyntheticDenseZero, SyntheticDenseZero) ->
                  True
                (StoredDenseValue, StoredDenseValue) ->
                  True
                _ ->
                  False
            )
      )

instance Num DenseZeroSentinel where
  fromInteger integerValue
    | integerValue == 0 =
        SyntheticDenseZero
    | otherwise =
        StoredDenseValue

  leftValue + rightValue =
    case (leftValue, rightValue) of
      (SyntheticDenseZero, SyntheticDenseZero) ->
        SyntheticDenseZero
      _ ->
        StoredDenseValue

  leftValue * rightValue =
    case (leftValue, rightValue) of
      (SyntheticDenseZero, _) ->
        SyntheticDenseZero
      (_, SyntheticDenseZero) ->
        SyntheticDenseZero
      _ ->
        StoredDenseValue

  negate value =
    value

  abs value =
    value

  signum value =
    case value of
      SyntheticDenseZero ->
        SyntheticDenseZero
      StoredDenseValue ->
        StoredDenseValue

sparseMinimalityTripwire :: Assertion
sparseMinimalityTripwire = do
  let nodeValue =
        FinObjectId 0

      axisValue =
        singletonAxis nodeValue 4096

      emptyBlockedMat :: BlockedMat DenseZeroSentinel
      emptyBlockedMat =
        zeroBlocked axisValue axisValue

      complexValue =
        InjectiveComplex
          { icStart = 0
          , icDiffs = Vector.singleton emptyBlockedMat
          }

  resetDenseZeroComparisonCounter

  isMinimal complexValue @?= True
  firstNonMinimal complexValue @?= Nothing

  comparisonCount <- readIORef denseZeroComparisonCounter
  comparisonCount @?= 0

sparseCompositionTripwire :: Assertion
sparseCompositionTripwire = do
  let targetNode =
        FinObjectId 0

      middleLeftNode =
        FinObjectId 10

      middleRightNode =
        FinObjectId 11

      sourceNode =
        FinObjectId 20

      targetAxis =
        axisFromSlices [(targetNode, 1)]

      middleAxis =
        axisFromSlices [(middleLeftNode, 1), (middleRightNode, 1)]

      sourceAxis =
        axisFromSlices [(sourceNode, 1)]

      leftDifferential =
        blockedFromBlocks
          middleAxis
          sourceAxis
          [ (middleLeftNode, sourceNode, dense1x1 1)
          , (middleRightNode, sourceNode, dense1x1 1)
          ]

      cancellingRightDifferential =
        blockedFromBlocks
          targetAxis
          middleAxis
          [ (targetNode, middleLeftNode, dense1x1 1)
          , (targetNode, middleRightNode, dense1x1 1)
          ]

      nonCancellingRightDifferential =
        blockedFromBlocks
          targetAxis
          middleAxis
          [ (targetNode, middleLeftNode, dense1x1 1)
          ]

      cancellingComposition =
        composeBlocked cancellingRightDifferential leftDifferential

      nonCancellingComposition =
        composeBlocked nonCancellingRightDifferential leftDifferential

  composeBlockedIsZero cancellingRightDifferential leftDifferential
    @?= isZeroMat (collapseBlockedDense cancellingComposition)

  composeBlockedIsZero cancellingRightDifferential leftDifferential
    @?= True

  IntMap.null (bmBlocks cancellingComposition)
    @?= True

  composeBlockedIsZero nonCancellingRightDifferential leftDifferential
    @?= isZeroMat (collapseBlockedDense nonCancellingComposition)

  composeBlockedIsZero nonCancellingRightDifferential leftDifferential
    @?= False

resetDenseZeroComparisonCounter :: IO ()
resetDenseZeroComparisonCounter =
  writeIORef denseZeroComparisonCounter 0

singletonAxis :: FinObjectId -> Int -> GroupedAxis
singletonAxis nodeValue multiplicityValue =
  appendAxisLabel nodeValue multiplicityValue emptyAxis

axisFromSlices :: [(FinObjectId, Int)] -> GroupedAxis
axisFromSlices =
  foldl'
    ( \axisValue (nodeValue, multiplicityValue) ->
        appendAxisLabel nodeValue multiplicityValue axisValue
    )
    emptyAxis

dense1x1 :: GF2 -> DenseMat GF2
dense1x1 entryValue =
  DenseMat
    { dmRows = 1
    , dmCols = 1
    , dmData = Vector.singleton (Vector.singleton entryValue)
    }

blockedFromBlocks ::
  GroupedAxis ->
  GroupedAxis ->
  [(FinObjectId, FinObjectId, DenseMat GF2)] ->
  BlockedMat GF2
blockedFromBlocks rowAxis columnAxis blockEntries =
  BlockedMat
    { bmRows = rowAxis
    , bmCols = columnAxis
    , bmBlocks = foldl' insertBlock IntMap.empty blockEntries
    }

insertBlock ::
  IntMap (IntMap (DenseMat GF2)) ->
  (FinObjectId, FinObjectId, DenseMat GF2) ->
  IntMap (IntMap (DenseMat GF2))
insertBlock accumulatedBlocks (FinObjectId rowKey, FinObjectId columnKey, blockValue) =
  IntMap.insertWith
    IntMap.union
    rowKey
    (IntMap.singleton columnKey blockValue)
    accumulatedBlocks

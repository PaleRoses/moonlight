module Common where

import Control.DeepSeq (NFData (..))
import Data.Foldable
  ( traverse_,
  )
import Moonlight.Differential.Arrangement (Arrangement, foldArrangement)
import Moonlight.Differential.Batch (Batch)
import Moonlight.Differential.Index.RowProjection (RowChanges (..))
import Moonlight.Differential.Update (Update (..))

type BenchUpdate = Update Int String Char Int

type BenchBatch = Batch Int String Char Int

type BenchArrangement = Arrangement Int String Char Int

newtype PreparedUpdates = PreparedUpdates
  { preparedUpdates :: [BenchUpdate]
  }

instance NFData PreparedUpdates where
  rnf (PreparedUpdates updates) =
    length updates `seq` ()

rowChangesWeight :: RowChanges rowKey payload -> Int
rowChangesWeight (RowChanges changes) =
  length changes
eitherShow :: Show err => Either err value -> Either String value
eitherShow =
  either (Left . show) Right
fanInCaseLabel :: String -> Int -> Int -> String
fanInCaseLabel prefix size fanIn =
  prefix <> " n=" <> show size <> " fanin=" <> show fanIn

checkedCase :: String -> Either String value -> IO value
checkedCase label =
  either (\obstruction -> fail (label <> ": " <> obstruction)) pure

checkedBenchCase :: String -> (input -> Either String measured) -> Either String input -> IO input
checkedBenchCase label measure input =
  checkedCase label input
    >>= \value ->
      checkedCase label (measure value) *> pure value

checkedBenchCases :: String -> [input -> Either String measured] -> Either String input -> IO input
checkedBenchCases label measures input =
  checkedCase label input
    >>= \value ->
      traverse_ (checkedCase label . ($ value)) measures *> pure value

cancellationUpdateCase :: Int -> PreparedUpdates
cancellationUpdateCase size =
  PreparedUpdates
    ( fmap
        ( \index ->
            (updateAt index)
              { updateWeight = negate (weightAt index)
              }
        )
        [0 .. size - 1]
    )

updateCase :: Int -> PreparedUpdates
updateCase size =
  PreparedUpdates (updateAt <$> [0 .. size - 1])

duplicateHeavyUpdateCase :: Int -> PreparedUpdates
duplicateHeavyUpdateCase size =
  PreparedUpdates (updateAt . (`mod` 64) <$> [0 .. size - 1])

cancellationHeavyUpdateCase :: Int -> PreparedUpdates
cancellationHeavyUpdateCase size =
  PreparedUpdates (cancellationPairUpdateAt <$> [0 .. size - 1])

skewedKeyUpdateCase :: Int -> PreparedUpdates
skewedKeyUpdateCase size =
  PreparedUpdates (skewedKeyUpdateAt <$> [0 .. size - 1])

shiftedUpdateCase :: Int -> PreparedUpdates
shiftedUpdateCase size =
  PreparedUpdates (updateAt . (+ size) <$> [0 .. size - 1])

shiftedSkewedKeyUpdateCase :: Int -> PreparedUpdates
shiftedSkewedKeyUpdateCase size =
  PreparedUpdates (skewedKeyUpdateAt . (+ size) <$> [0 .. size - 1])

updateAt :: Int -> BenchUpdate
updateAt index =
  Update
    { updateTime = index `mod` 96,
      updateKey = keyAt index,
      updateVal = valueAt index,
      updateWeight = weightAt index
    }

monotoneUpdateAt :: Int -> BenchUpdate
monotoneUpdateAt index =
  (updateAt index)
    { updateTime = index
    }

shiftedMonotoneUpdateAt :: Int -> Int -> BenchUpdate
shiftedMonotoneUpdateAt offset index =
  (monotoneUpdateAt (offset + index))
    { updateTime = offset + index
    }

overlappingMonotoneUpdateAt :: Int -> Int -> Int -> BenchUpdate
overlappingMonotoneUpdateAt fanIn section index =
  (monotoneUpdateAt index)
    { updateKey = keyAt (index `mod` max 1 fanIn),
      updateWeight =
        if even section
          then weightAt index
          else negate (weightAt index)
    }

negateUpdateWeight :: Update time key val Int -> Update time key val Int
negateUpdateWeight updateValue =
  updateValue
    { updateWeight = negate (updateWeight updateValue)
    }

cancellationPairUpdateAt :: Int -> BenchUpdate
cancellationPairUpdateAt index =
  (updateAt baseIndex)
    { updateWeight =
        if even index
          then weightAt baseIndex
          else negate (weightAt baseIndex)
    }
  where
    baseIndex =
      index `quot` 2

skewedKeyUpdateAt :: Int -> BenchUpdate
skewedKeyUpdateAt index =
  (updateAt index)
    { updateKey = hotKey
    }

keyAt :: Int -> String
keyAt index =
  "key-" <> show (index `mod` 32)

valueAt :: Int -> Char
valueAt index =
  toEnum (fromEnum 'a' + (index `mod` 8))

weightAt :: Int -> Int
weightAt index =
  case index `mod` 5 of
    0 -> -1
    1 -> 2
    2 -> 1
    3 -> -2
    _ -> 3

traceCutoff :: Int
traceCutoff =
  48

hotKey :: String
hotKey =
  "key-0"

caseLabel :: String -> Int -> String
caseLabel prefix size =
  prefix <> " n=" <> show size

batchSizes :: [Int]
batchSizes =
  [512, 2048]


storageKernelSizes :: [Int]
storageKernelSizes =
  [10000, 100000]

arrangementCellCount :: Arrangement time key val weight -> Int
arrangementCellCount =
  foldArrangement (\count _time _key _value _weight -> count + 1) 0

pairProjection :: String -> Char -> Char -> Maybe (String, (Char, Char))
pairProjection key leftValue rightValue =
  Just (key, (leftValue, rightValue))

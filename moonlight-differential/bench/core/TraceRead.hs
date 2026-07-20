module TraceRead where

import Control.DeepSeq (NFData (..))
import Data.Foldable qualified as Foldable
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Moonlight.Delta.Frontier (singletonFrontier, singletonUpperFrontier)
import Moonlight.Differential.Trace.Description qualified as TraceDescription
import Moonlight.Differential.Trace.ReadIndex

data PreparedTraceDescription = PreparedTraceDescription
  { preparedTraceDescription :: !(TraceDescription.TraceDescription (TimeFrontier Int Int)),
    preparedTraceDescriptionRequests :: ![TimeFrontier Int Int]
  }

instance NFData PreparedTraceDescription where
  rnf preparedCase =
    length (preparedTraceDescriptionRequests preparedCase) `seq` ()

data PreparedTimeIndexRead = PreparedTimeIndexRead
  { preparedTimeIndexKey :: !Int,
    preparedTimeIndexFrontier :: !(TimeFrontier Int Int),
    preparedTimeIndexDescription :: !(TraceDescription.TraceDescription (TimeFrontier Int Int)),
    preparedTimeIndex :: !(TimeIndex Int Int Int)
  }

instance NFData PreparedTimeIndexRead where
  rnf preparedCase =
    preparedTimeIndexKey preparedCase
      `seq` Map.size (timeIndexMapWeight (preparedTimeIndex preparedCase))
      `seq` ()

traceReadSizes :: [Int]
traceReadSizes =
  [512, 2048]

traceDescriptionAdvanceCase :: Int -> PreparedTraceDescription
traceDescriptionAdvanceCase size =
  PreparedTraceDescription
    { preparedTraceDescription =
        TraceDescription.traceDescription
          (singletonFrontier (TimeFrontier 0 0))
          (singletonUpperFrontier (TimeFrontier (size `quot` 64 + 1) size))
          (singletonUpperFrontier (TimeFrontier 0 0)),
      preparedTraceDescriptionRequests =
        fmap (\stamp -> TimeFrontier (stamp `quot` 64) stamp) [0 .. size - 1]
    }

timeIndexReadCase :: Int -> PreparedTimeIndexRead
timeIndexReadCase size =
  PreparedTimeIndexRead
    { preparedTimeIndexKey = hotReadKey,
      preparedTimeIndexFrontier = TimeFrontier (size `quot` 128) (size `quot` 2),
      preparedTimeIndexDescription =
        TraceDescription.traceDescription
          (singletonFrontier (TimeFrontier 0 0))
          (singletonUpperFrontier (TimeFrontier (size `quot` 64 + 1) size))
          (singletonUpperFrontier (TimeFrontier 0 0)),
      preparedTimeIndex =
        Foldable.foldl'
          ( \index entry ->
              insertTimeIndex
                (timeIndexEntryKey entry)
                (timeIndexEntryEpoch entry)
                (timeIndexEntryStamp entry)
                (timeIndexEntryMembers entry)
                index
          )
          emptyTimeIndex
          (timeIndexEntries size)
    }

traceDescriptionAdvanceWeight :: PreparedTraceDescription -> Int
traceDescriptionAdvanceWeight preparedCase =
  Foldable.foldl'
    ( \acc request ->
        acc
          + either (const 0) (const 1) (TraceDescription.traceDescriptionReadAfter request (preparedTraceDescription preparedCase))
          + either (const 0) (const 1) (TraceDescription.traceDescriptionReadAt request (preparedTraceDescription preparedCase))
    )
    0
    (preparedTraceDescriptionRequests preparedCase)

timeIndexRawReadWeight :: PreparedTimeIndexRead -> Int
timeIndexRawReadWeight preparedCase =
  IntSet.size
    ( sliceTimeIndexAfter
        (preparedTimeIndexKey preparedCase)
        (preparedTimeIndexFrontier preparedCase)
        (preparedTimeIndex preparedCase)
    )

timeIndexReadWeight :: PreparedTimeIndexRead -> Either String Int
timeIndexReadWeight preparedCase =
  either
    (Left . show)
    (Right . IntSet.size)
    ( sliceTimeIndexAfterDescription
        (preparedTimeIndexKey preparedCase)
        (preparedTimeIndexFrontier preparedCase)
        (preparedTimeIndexDescription preparedCase)
        (preparedTimeIndex preparedCase)
    )

data TimeIndexEntry = TimeIndexEntry
  { timeIndexEntryKey :: !Int,
    timeIndexEntryEpoch :: !Int,
    timeIndexEntryStamp :: !Int,
    timeIndexEntryMembers :: !IntSet.IntSet
  }

timeIndexEntries :: Int -> [TimeIndexEntry]
timeIndexEntries size =
  fmap
    ( \index ->
        TimeIndexEntry
          { timeIndexEntryKey = index `mod` 32,
            timeIndexEntryEpoch = index `quot` 128,
            timeIndexEntryStamp = index,
            timeIndexEntryMembers = IntSet.fromList [index, index + size]
          }
    )
    [0 .. size - 1]

hotReadKey :: Int
hotReadKey =
  0

timeIndexMapWeight :: TimeIndex key epoch stamp -> Map.Map key (Map.Map epoch (Map.Map stamp IntSet.IntSet))
timeIndexMapWeight =
  timeIndexMap

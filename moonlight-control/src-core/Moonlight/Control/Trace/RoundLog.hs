module Moonlight.Control.Trace.RoundLog
  ( RoundLog,
    emptyRoundLog,
    singletonRoundLog,
    appendRoundLog,
    appendRoundLogsWithPolicy,
    retainRoundLogWithPolicy,
    roundLogRounds,
    roundLogDropWhile,
    roundLogPrefixes,
  )
where

import Data.Foldable qualified as Foldable
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Moonlight.Control.Schedule
  ( TracePolicy,
  )
import Moonlight.Control.Schedule.Round
  ( retainGroupedTraceEntries,
  )

newtype RoundLog round = RoundLog
  { roundLogEntries :: Seq round
  }
  deriving stock (Eq, Show, Read)

instance Semigroup (RoundLog round) where
  RoundLog leftRounds <> RoundLog rightRounds =
    RoundLog (leftRounds Seq.>< rightRounds)

instance Monoid (RoundLog round) where
  mempty =
    emptyRoundLog

emptyRoundLog :: RoundLog round
emptyRoundLog =
  RoundLog Seq.empty

singletonRoundLog :: round -> RoundLog round
singletonRoundLog =
  RoundLog . Seq.singleton

appendRoundLog :: round -> RoundLog round -> RoundLog round
appendRoundLog roundValue (RoundLog rounds) =
  RoundLog (rounds Seq.|> roundValue)

appendRoundLogsWithPolicy ::
  (round -> [traceEntry]) ->
  ([traceEntry] -> round -> round) ->
  TracePolicy ->
  RoundLog round ->
  RoundLog round ->
  RoundLog round
appendRoundLogsWithPolicy traceEntries setTraceEntries tracePolicy previousLog deltaLog =
  retainRoundLogWithPolicy traceEntries setTraceEntries tracePolicy (previousLog <> deltaLog)

retainRoundLogWithPolicy ::
  (round -> [traceEntry]) ->
  ([traceEntry] -> round -> round) ->
  TracePolicy ->
  RoundLog round ->
  RoundLog round
retainRoundLogWithPolicy traceEntries setTraceEntries tracePolicy (RoundLog rounds) =
  RoundLog
    ( retainGroupedTraceEntries
        traceEntries
        setTraceEntries
        tracePolicy
        rounds
    )

roundLogRounds :: RoundLog round -> [round]
roundLogRounds =
  Foldable.toList . roundLogEntries

roundLogDropWhile :: (round -> Bool) -> RoundLog round -> RoundLog round
roundLogDropWhile predicate (RoundLog rounds) =
  RoundLog (Seq.dropWhileL predicate rounds)

roundLogPrefixes :: RoundLog round -> [RoundLog round]
roundLogPrefixes (RoundLog rounds) =
  Foldable.toList (RoundLog <$> Seq.drop 1 (Seq.inits rounds))

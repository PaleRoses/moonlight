{-# LANGUAGE BangPatterns #-}

module Moonlight.Control.Engine.ParallelSpec
  ( tests,
  )
where

import Control.Concurrent
  ( threadDelay,
  )
import Control.Exception
  ( Exception,
    throwIO,
    try,
  )
import Control.Scheduler
  ( Comp (ParN),
  )
import Moonlight.Control.Engine.Parallel
  ( MatchExecution (..),
    ParallelMatchExecution (..),
    applyScheduledMatchDeltas,
    traverseScheduledMatches,
  )
import Moonlight.Control.Engine.Work
  ( ApplyResult,
    applyResult,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( Assertion,
    assertFailure,
    testCase,
    (@?=),
  )
import Test.Tasty.QuickCheck qualified as QC

data ExecutionTestException
  = ExecutionTestException
  deriving stock (Eq, Show)

instance Exception ExecutionTestException

tests :: TestTree
tests =
  testGroup
    "Moonlight.Control.Engine.Parallel"
    [ testCase "sequential and parallel helpers return identical ordered results" $
        sequentialAndParallelReturnOrderedResults,
      testCase "chunked parallel helper preserves scheduled result order" $
        chunkedParallelPreservesScheduledOrder,
      testCase "parallel helper returns first typed failure by scheduled order" $
        parallelReturnsFirstFailureByScheduledOrder,
      testCase "chunked parallel helper returns first typed failure by scheduled order" $
        chunkedParallelReturnsFirstFailureByScheduledOrder,
      testCase "below-threshold parallel config uses sequential short-circuit semantics" $
        belowThresholdUsesSequentialSemantics,
      testCase "empty batch returns Right [] and delegates merge identity" $
        emptyBatchDelegatesMergeIdentity,
      testCase "applyScheduledMatchDeltas matches a hand-written sequential merge" $
        applyScheduledMatchDeltasMatchesSequentialMerge,
      testCase "runtime exceptions are not rebranded as typed domain errors" $
        runtimeExceptionsEscape,
      QC.testProperty "parallel execution agrees with sequential execution for total matches" $
        prop_parallelAgreesWithSequentialSuccess,
      QC.testProperty "parallel execution selects same typed failure as sequential execution" $
        prop_parallelAgreesWithSequentialTypedFailure,
      QC.testProperty "chunked parallel execution agrees with unchunked parallel execution" $
        prop_chunkedParallelAgreesWithUnchunked
    ]

parallelExecution :: MatchExecution
parallelExecution =
  ParallelMatches
    ParallelMatchExecution
      { pmeComp = ParN 4,
        pmeMinBatchSize = 1,
        pmeChunkSize = 1
      }

chunkedParallelExecution :: MatchExecution
chunkedParallelExecution =
  ParallelMatches
    ParallelMatchExecution
      { pmeComp = ParN 4,
        pmeMinBatchSize = 1,
        pmeChunkSize = 3
      }

sequentialAndParallelReturnOrderedResults :: Assertion
sequentialAndParallelReturnOrderedResults = do
  let matches =
        [1 .. 12]
      runMatch :: Int -> IO (Either String Int)
      runMatch match = do
        threadDelay ((13 - match) * 1000)
        pure (Right (match * 10))
      expected =
        Right (fmap (* 10) matches)

  sequentialResult <-
    traverseScheduledMatches SequentialMatches runMatch matches
  parallelResult <-
    traverseScheduledMatches parallelExecution runMatch matches

  sequentialResult @?= expected
  parallelResult @?= expected

chunkedParallelPreservesScheduledOrder :: Assertion
chunkedParallelPreservesScheduledOrder = do
  let matches =
        [1 .. 17]
      runMatch :: Int -> IO (Either String Int)
      runMatch match = do
        threadDelay ((18 - match) * 1000)
        pure (Right (match * 11))
      expected =
        Right (fmap (* 11) matches)

  result <-
    traverseScheduledMatches chunkedParallelExecution runMatch matches

  result @?= expected

parallelReturnsFirstFailureByScheduledOrder :: Assertion
parallelReturnsFirstFailureByScheduledOrder = do
  let matches =
        [0 .. 5]
      runMatch :: Int -> IO (Either String Int)
      runMatch match =
        case match of
          1 -> do
            threadDelay 50000
            pure (Left "scheduled-index-1")
          3 -> do
            threadDelay 1000
            pure (Left "scheduled-index-3")
          _ ->
            pure (Right match)

  result <-
    traverseScheduledMatches parallelExecution runMatch matches

  result @?= Left "scheduled-index-1"

chunkedParallelReturnsFirstFailureByScheduledOrder :: Assertion
chunkedParallelReturnsFirstFailureByScheduledOrder = do
  let matches =
        [0 .. 8]
      runMatch :: Int -> IO (Either String Int)
      runMatch match =
        case match of
          2 -> do
            threadDelay 50000
            pure (Left "scheduled-index-2")
          7 -> do
            threadDelay 1000
            pure (Left "scheduled-index-7")
          _ ->
            pure (Right match)

  result <-
    traverseScheduledMatches chunkedParallelExecution runMatch matches

  result @?= Left "scheduled-index-2"

belowThresholdUsesSequentialSemantics :: Assertion
belowThresholdUsesSequentialSemantics = do
  let execution =
        ParallelMatches
          ParallelMatchExecution
            { pmeComp = ParN 4,
              pmeMinBatchSize = 100,
              pmeChunkSize = 2
            }
      runMatch :: Int -> IO (Either String Int)
      runMatch match =
        case match of
          1 ->
            pure (Right match)
          2 ->
            pure (Left "stop")
          _ ->
            throwIO ExecutionTestException

  result <-
    traverseScheduledMatches execution runMatch [1 .. 5]

  result @?= Left "stop"

emptyBatchDelegatesMergeIdentity :: Assertion
emptyBatchDelegatesMergeIdentity = do
  let runMatch :: Int -> IO (Either String Int)
      runMatch _ =
        assertFailure "empty batch evaluated a match"
          *> pure (Right 0)
      mergeDeltas ::
        Int ->
        [Int] ->
        Either String (ApplyResult Int Int)
      mergeDeltas state deltas =
        case deltas of
          [] ->
            Right (applyResult state 0 0)
          _ ->
            Left "non-empty-deltas"

  traverseResult <-
    traverseScheduledMatches parallelExecution runMatch []
  applyRes <-
    applyScheduledMatchDeltas
      parallelExecution
      runMatch
      mergeDeltas
      []
      42

  traverseResult @?= Right []
  applyRes @?= Right (applyResult 42 0 0)

applyScheduledMatchDeltasMatchesSequentialMerge :: Assertion
applyScheduledMatchDeltasMatchesSequentialMerge = do
  let matches =
        [1 .. 8]
      initialState =
        100
      applyOne :: Int -> IO (Either String Int)
      applyOne match =
        pure (Right (match * 2))
      mergeDeltas ::
        Int ->
        [Int] ->
        Either String (ApplyResult Int Int)
      mergeDeltas state deltas =
        let !nextState =
              foldl' (+) state deltas
            !evidence =
              sum deltas
            !appliedCount =
              length deltas
         in Right (applyResult nextState evidence appliedCount)
      handWrittenSequential :: IO (Either String (ApplyResult Int Int))
      handWrittenSequential = do
        deltaResults <-
          traverse applyOne matches
        pure (sequenceA deltaResults >>= mergeDeltas initialState)

  expected <-
    handWrittenSequential
  actual <-
    applyScheduledMatchDeltas
      parallelExecution
      applyOne
      mergeDeltas
      matches
      initialState

  actual @?= expected

runtimeExceptionsEscape :: Assertion
runtimeExceptionsEscape = do
  let runMatch :: Int -> IO (Either String Int)
      runMatch match =
        if match == 2
          then throwIO ExecutionTestException
          else pure (Right match)

  result <-
    try (traverseScheduledMatches parallelExecution runMatch [1 .. 4])

  case result of
    Left ExecutionTestException ->
      pure ()
    Right typedResult ->
      assertFailure
        ( "expected ExecutionTestException to escape; got typed result "
            <> show typedResult
        )

newtype SmallMatches = SmallMatches
  { smallMatches :: [Int]
  }
  deriving stock (Eq, Show)

newtype FailureCutoff = FailureCutoff
  { failureCutoff :: Int
  }
  deriving stock (Eq, Show)

instance QC.Arbitrary SmallMatches where
  arbitrary =
    SmallMatches <$> do
      matchCount <- QC.chooseInt (0, 24)
      QC.vectorOf matchCount (QC.chooseInt (-50, 50))
  shrink _ =
    []

instance QC.Arbitrary FailureCutoff where
  arbitrary =
    FailureCutoff <$> QC.chooseInt (-50, 50)
  shrink _ =
    []

prop_parallelAgreesWithSequentialSuccess :: SmallMatches -> QC.Property
prop_parallelAgreesWithSequentialSuccess (SmallMatches matches) =
  QC.ioProperty $ do
    sequentialResult <- traverseScheduledMatches SequentialMatches runSuccessfulMatch matches
    parallelResult <- traverseScheduledMatches unchunkedParallelExecution runSuccessfulMatch matches
    pure (parallelResult QC.=== sequentialResult)

prop_parallelAgreesWithSequentialTypedFailure :: FailureCutoff -> SmallMatches -> QC.Property
prop_parallelAgreesWithSequentialTypedFailure (FailureCutoff cutoff) (SmallMatches matches) =
  QC.ioProperty $ do
    sequentialResult <- traverseScheduledMatches SequentialMatches (runFallibleMatch cutoff) matches
    parallelResult <- traverseScheduledMatches unchunkedParallelExecution (runFallibleMatch cutoff) matches
    pure (parallelResult QC.=== sequentialResult)

prop_chunkedParallelAgreesWithUnchunked :: SmallMatches -> QC.Property
prop_chunkedParallelAgreesWithUnchunked (SmallMatches matches) =
  QC.ioProperty $ do
    unchunkedResult <- traverseScheduledMatches unchunkedParallelExecution runSuccessfulMatch matches
    chunkedResult <- traverseScheduledMatches lawsChunkedParallelExecution runSuccessfulMatch matches
    pure (chunkedResult QC.=== unchunkedResult)

runSuccessfulMatch :: Int -> IO (Either String Int)
runSuccessfulMatch match =
  pure (Right (match * 2 + 1))

runFallibleMatch :: Int -> Int -> IO (Either String Int)
runFallibleMatch cutoff match =
  pure
    ( if match >= cutoff
        then Left ("cutoff:" <> show cutoff)
        else Right (match * 3)
    )

unchunkedParallelExecution :: MatchExecution
unchunkedParallelExecution =
  ParallelMatches
    ParallelMatchExecution
      { pmeComp = ParN 4,
        pmeMinBatchSize = 1,
        pmeChunkSize = 1
      }

lawsChunkedParallelExecution :: MatchExecution
lawsChunkedParallelExecution =
  ParallelMatches
    ParallelMatchExecution
      { pmeComp = ParN 4,
        pmeMinBatchSize = 1,
        pmeChunkSize = 5
      }

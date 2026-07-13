{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Control.Scheduler (Comp (ParN))
import Data.Bits (xor)
import Data.Functor.Identity (Identity (..))
import Numeric.Natural (Natural)
import Test.Tasty.Bench
  ( bcompareWithin,
    bench,
    bgroup,
    defaultMain,
    nfIO,
    whnf,
  )

import Moonlight.Control.Candidate
  ( CandidateGroup (..),
    CandidateSpace (..),
    PullResult (..),
    finiteCandidateSpace,
    pullCandidateCursor,
    pullRequest,
  )
import Moonlight.Control.Class
  ( andThen,
    phase,
    skip,
  )
import Moonlight.Control.Engine.Parallel
  ( MatchExecution (..),
    ParallelMatchExecution (..),
    traverseScheduledMatches,
  )
import Moonlight.Control.Gate
  ( GatePullTrace (..),
    MatchSelector (..),
    MatchSelectorResult (..),
    composeSelectors,
    filterSelector,
    gateCandidateSpace,
    noGate,
    Gate (..),
  )
import Moonlight.Control.Machine
  ( Execution (..),
    Progress (..),
    interpret,
    stopVerdict,
  )
import Moonlight.Control.Program
  ( Program,
    ProgramAlgebra (..),
    foldProgram,
    normalize,
    programSize,
  )
import Moonlight.Control.Schedule
  ( ScheduleOrder (..),
    SchedulerConfig (..),
    backoffConfig,
    defaultSchedulerConfig,
  )
import Moonlight.Control.Schedule.Round
  ( ScheduleOutcome (..),
    emptySchedulerState,
    scheduleCandidateSpace,
  )
import Moonlight.Control.Trace (Trace (..))

main :: IO ()
main =
  defaultMain
    [ bgroup
        "construction"
        [ bench "andThen left fold 1k" (whnf builtSize 1000),
          bcompareWithin
            0
            25
            "construction.andThen left fold 1k"
            (bench "andThen left fold 10k" (whnf builtSize 10000))
        ],
      bgroup
        "normalize"
        [ bench "left-nested spine 10k" (whnf normalizedSize 10000)
        ],
      bgroup
        "fold"
        [ bench "foldProgram phase count 10k" (whnf foldedPhaseCount 10000)
        ],
      bgroup
        "machine"
        [ bench "round trip 1k phases @Identity" (whnf machineRoundTrip 1000)
        ],
      bgroup
        "schedule"
        [ bench "deterministic 100 groups" (whnf (scheduledCount deterministicOrder) 100),
          bench "deterministic 1000 groups" (whnf (scheduledCount deterministicOrder) 1000),
          bench "backoff 100 groups" (whnf (scheduledCount backoffOrder) 100),
          bench "backoff 1000 groups" (whnf (scheduledCount backoffOrder) 1000)
        ],
      bgroup
        "gate"
        [ bench "5-selector chain 10k matches" (whnf selectorChainAccepted 10000),
          bench "gated space pull 10k matches" (whnf gatedSpacePull 10000)
        ],
      bgroup
        "parallel"
        [ bench "cheap 16 thresholded" (nfIO (parallelChecksum (+ 1) thresholdedExecution (cheapMatches 16))),
          bench "cpu 64x20000 sequential" (nfIO (parallelChecksum (cpuDelta 20000) SequentialMatches (cpuMatches 64))),
          bench "cpu 64x20000 par4 chunk1" (nfIO (parallelChecksum (cpuDelta 20000) (forcedExecution 4 1) (cpuMatches 64))),
          bench "cpu 64x20000 par4 chunk8" (nfIO (parallelChecksum (cpuDelta 20000) (forcedExecution 4 8) (cpuMatches 64)))
        ]
    ]

buildSpine :: Int -> Program () Int
buildSpine phaseCount =
  foldl' (\acc phasePayload -> andThen acc (phase phasePayload)) skip [1 .. phaseCount]

builtSize :: Int -> Natural
builtSize = programSize . buildSpine

normalizedSize :: Int -> Natural
normalizedSize = programSize . normalize . buildSpine

foldedPhaseCount :: Int -> Natural
foldedPhaseCount =
  foldProgram
    ProgramAlgebra
      { paSkip = 0,
        paPhase = const 1,
        paSeq = (+),
        paOr = (+),
        paUpTo = const id,
        paAttempt = id,
        paScoped = const id
      }
    . buildSpine

machineRoundTrip :: Int -> Int
machineRoundTrip phaseCount =
  either id (seState . snd) (runIdentity (interpret runner (buildSpine phaseCount) 0))
  where
    runner :: () -> Int -> Int -> Identity (Either Int (Int, Execution Int () ()))
    runner _context phasePayload state =
      pure
        ( Right
            ( phasePayload,
              Execution
                { seState = state + phasePayload,
                  seLatestReport = Nothing,
                  seTrace = PhaseTrace (),
                  seVerdict = stopVerdict Progressed
                }
            )
        )

deterministicOrder :: ScheduleOrder
deterministicOrder = ByRuleIdThenSubstitution

backoffOrder :: ScheduleOrder
backoffOrder = BackoffByGroup (backoffConfig 4 2)

scheduledCount :: ScheduleOrder -> Int -> Natural
scheduledCount order groupCount =
  soScheduledCount
    ( runIdentity
        ( scheduleCandidateSpace
            (defaultSchedulerConfig {scOrder = order})
            256
            0
            (benchSpace groupCount)
            emptySchedulerState
        )
    )

benchSpace :: Int -> CandidateSpace Identity Int () Int
benchSpace groupCount =
  finiteCandidateSpace
    [(groupKey, [1 .. 10]) | groupKey <- [1 .. groupCount]]

selectorChain :: MatchSelector Int Int Int ()
selectorChain =
  foldl'
    composeSelectors
    (filterSelector "even" (\_view match -> even match))
    [ filterSelector "positive" (\_view match -> match > 0),
      filterSelector "bounded" (\_view match -> match < 100000),
      filterSelector "nonzero mod 3" (\_view match -> match `mod` 3 /= 0),
      filterSelector "view threshold" (\view match -> match >= view)
    ]

selectorChainAccepted :: Int -> Natural
selectorChainAccepted matchCount =
  let !result = runMatchSelector selectorChain 1 0 [1 .. matchCount]
   in fromIntegral (length (msrAcceptedMatches result)) + msrRejectedCount result

gatedSpacePull :: Int -> Natural
gatedSpacePull matchCount =
  runIdentity $ do
    let gate :: Gate Int Int Int () Int
        gate = noGate {gateSelector = selectorChain}
        sourceSpace :: CandidateSpace Identity Int () Int
        sourceSpace = finiteCandidateSpace [(0, [1 .. matchCount])]
        space = gateCandidateSpace gate 1 sourceSpace
    groupLookup <- csLookupGroup space 0
    case groupLookup of
      Nothing -> pure 0
      Just candidateGroup -> do
        cursor <- cgOpenCursor candidateGroup
        result <- pullCandidateCursor cursor (pullRequest (fromIntegral matchCount))
        pure (prPulledCount result + gptAcceptedCount (prMeta result))

parallelChecksum :: (Int -> Int) -> MatchExecution -> [Int] -> IO Int
parallelChecksum delta execution matches = do
  result <- traverseScheduledMatches execution (pure . Right . delta) matches
  pure (either id (foldl' (+) 0) result)

thresholdedExecution :: MatchExecution
thresholdedExecution =
  ParallelMatches
    ParallelMatchExecution
      { pmeComp = ParN 4,
        pmeMinBatchSize = 128,
        pmeChunkSize = 1
      }

forcedExecution :: Int -> Int -> MatchExecution
forcedExecution workerCount chunkSize =
  ParallelMatches
    ParallelMatchExecution
      { pmeComp = ParN (fromIntegral workerCount),
        pmeMinBatchSize = 1,
        pmeChunkSize = chunkSize
      }

cheapMatches :: Int -> [Int]
cheapMatches matchCount = [1 .. matchCount]

cpuMatches :: Int -> [Int]
cpuMatches matchCount = [1 .. matchCount]

cpuDelta :: Int -> Int -> Int
cpuDelta iterationCount match =
  foldl' step match [1 .. iterationCount]
  where
    step :: Int -> Int -> Int
    step !acc !n =
      (acc * 1664525 + n * 1013904223) `xor` (acc + n)

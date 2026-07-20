{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module ProtocolBench
  ( protocolBenchmarks,
  )
where

import BenchSupport
import Control.DeepSeq (NFData (..))
import Control.Monad (foldM)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import GHC.Generics (Generic)
import Moonlight.Delta.Scope
import Moonlight.Saturation.Matching
import Moonlight.Saturation.Substrate
import Moonlight.Saturation.Test.ContextFixture
import Moonlight.Saturation.Test.ContextWorkload
import Moonlight.Saturation.Test.ProtocolFixture
import Test.Tasty.Bench (Benchmark)

data ProtocolDigest = ProtocolDigest !Int !PopulationDigest !PopulationDigest
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data PrepareSweepInput = PrepareSweepInput !(Scoped IntSet IntSet) ![ProbeRequest ()]

type RunSweepInput = [(Scope IntSet, ProbeRequest ())]

type ScopeTransportInput = [MatchingQuery IntSet ProbeRequest ()]

data ContextGlueInput = ContextGlueInput !(Scoped IntSet IntSet) !ContextMatchingWorkload

data ContextGlueDigest = ContextGlueDigest !Int !PopulationDigest !PopulationDigest
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

protocolBenchmarks :: Either BenchmarkObstruction Benchmark
protocolBenchmarks = do
  _validatedLattice <-
    requireBenchmarkFixture
      ProtocolBenchmarkLane
      "shared context lattice"
      testContextLatticeValidation
  validatedBenchmarkGroup
    "protocol"
    [ validatedBenchmarkFamily "prepare-single-sweep" prepareBenchmark protocolScales,
      validatedBenchmarkFamily "run-single-sweep" runSingleBenchmark protocolScales,
      validatedBenchmarkFamily "run-prepared-batch" runPreparedBenchmark protocolScales,
      validatedBenchmarkFamily "map-scope-batch" transportBenchmark protocolScales,
      validatedBenchmarkFamily "context-glue-disjoint" (contextGlueBenchmark DisjointContextMatches) contextGlueScales,
      validatedBenchmarkFamily "context-glue-overlap" (contextGlueBenchmark OverlappingContextMatches) contextGlueScales
    ]

protocolScales :: [Int]
protocolScales = [256, 4096, 65536]

contextGlueScales :: [Int]
contextGlueScales = [64, 512, 4096]

prepareBenchmark :: Int -> Either BenchmarkObstruction Benchmark
prepareBenchmark size =
  let caseName = benchmarkCaseLabel "requests" size
      input =
        PrepareSweepInput (probeScopedDelta size) (probeRequestBatch size)
      expected = ProtocolDigest size (PopulationDigest size size) mempty
   in validatedPureBenchmark
        ProtocolBenchmarkLane
        caseName
        expected
        forcePrepareSweepInput
        rnf
        prepareSweepDigest
        input

runSingleBenchmark :: Int -> Either BenchmarkObstruction Benchmark
runSingleBenchmark = runSweepBenchmark runSingleSweepDigest

runPreparedBenchmark :: Int -> Either BenchmarkObstruction Benchmark
runPreparedBenchmark = runSweepBenchmark runPreparedDigest

runSweepBenchmark :: (RunSweepInput -> Either ProbeObstruction ProtocolDigest) -> Int -> Either BenchmarkObstruction Benchmark
runSweepBenchmark measure size =
  let caseName = benchmarkCaseLabel "requests" size
      input = runSweepInput size
   in validatedPureBenchmark
        ProtocolBenchmarkLane
        caseName
        (Right (expectedRunDigest size))
        forceRunSweepInput
        (forceEither rnf)
        measure
        input

transportBenchmark :: Int -> Either BenchmarkObstruction Benchmark
transportBenchmark size =
  let caseName = benchmarkCaseLabel "queries" size
      input =
        fmap (MatchingQuery cleanScope) (probeRequestBatch size)
      expected = ProtocolDigest 0 (PopulationDigest size (sumFromZero size + size)) mempty
   in validatedPureBenchmark
        ProtocolBenchmarkLane
        caseName
        expected
        forceScopeTransportInput
        rnf
        transportDigest
        input

contextGlueBenchmark :: ContextMatchProfile -> Int -> Either BenchmarkObstruction Benchmark
contextGlueBenchmark profile size =
  let caseName = benchmarkCaseLabel "roots" size
      input =
        ContextGlueInput (probeScopedDelta size) (contextMatchingWorkload profile size)
      witnessMultiplier =
        case profile of
          DisjointContextMatches -> 1
          OverlappingContextMatches -> 3
      expected =
        Right
          ( ContextGlueDigest
              4
              (PopulationDigest size (sumFromOne size))
              (PopulationDigest (witnessMultiplier * size) (witnessMultiplier * sumFromOne size))
          )
   in validatedPureBenchmark
        ProtocolBenchmarkLane
        caseName
        expected
        forceContextGlueInput
        (forceEither rnf)
        contextGlueDigest
        input

prepareSweepDigest :: PrepareSweepInput -> ProtocolDigest
prepareSweepDigest (PrepareSweepInput delta requests) =
  let step (state, preparedCount, scopeWeight) request =
        let (nextState, matchingScope) =
              prepareSingleQuery
                probeMatchingAlgebra
                state
                delta
                probeWorld
                request
         in ( nextState,
              preparedCount + 1,
              scopeWeight + scopeConstructorWeight matchingScope
            )
      (finalState, batchCount, preparedScopeWeight) =
        foldl'
          step
          (0, 0, 0)
          requests
   in ProtocolDigest finalState (PopulationDigest batchCount preparedScopeWeight) mempty

runSingleSweepDigest :: RunSweepInput -> Either ProbeObstruction ProtocolDigest
runSingleSweepDigest input =
  foldM step (ProtocolDigest 0 mempty mempty) input
  where
    step :: ProtocolDigest -> (Scope IntSet, ProbeRequest ()) -> Either ProbeObstruction ProtocolDigest
    step digest (matchingScope, request) =
      let (nextState, matchesResult) =
            let ProtocolDigest state _ _ = digest
             in runSingleQuery probeMatchingAlgebra state probeWorld matchingScope request
       in fmap (digestMatchBatch nextState digest) matchesResult

runPreparedDigest :: RunSweepInput -> Either ProbeObstruction ProtocolDigest
runPreparedDigest input =
  let (nextState, matchesResult) =
        runPreparedQueries probeMatchingAlgebra 0 probeWorld input
   in fmap (foldl' (digestMatchBatch nextState) (ProtocolDigest nextState mempty mempty)) matchesResult

transportDigest :: ScopeTransportInput -> ProtocolDigest
transportDigest input =
  let transportedQueries =
        fmap
          (mapMatchingQueryScope (const fullScope))
          input
      (batchCount, queryWeight) =
        foldl'
          (\(count, weight) query ->
             ( count + 1,
               weight
                 + probeRequestValue (mqRequest query)
                 + probeScopeWeight (mqScope query)
             )
          )
          (0, 0)
          transportedQueries
   in ProtocolDigest 0 (PopulationDigest batchCount queryWeight) mempty

contextGlueDigest :: ContextGlueInput -> Either String ContextGlueDigest
contextGlueDigest (ContextGlueInput delta workload) =
  fmap
        (\(matchState, supportedMatches) ->
           foldl' accumulateSupportedMatch (ContextGlueDigest (sum (tmsContextCalls matchState)) mempty mempty) supportedMatches
        )
        ( contextSupportedMatchesPreparedViaContexts
            @TestSubstrate
            ()
            ()
            0
            delta
            (contextMatchingGraph workload)
            (contextMatchingInputs workload)
            []
            emptyTestMatchState
        )

accumulateSupportedMatch :: ContextGlueDigest -> TestSupportedMatch -> ContextGlueDigest
accumulateSupportedMatch (ContextGlueDigest calls matches priorWitnesses) supportedMatch =
  let rootClass = tmRootClass (tsmInner supportedMatch)
      witnesses = populationDigest (IntSet.foldl' (+) 0) (supportedMatchWitnesses @TestSubstrate supportedMatch)
   in ContextGlueDigest calls (matches <> PopulationDigest 1 rootClass) (priorWitnesses <> witnesses)

digestMatchBatch :: Int -> ProtocolDigest -> [Int] -> ProtocolDigest
digestMatchBatch state (ProtocolDigest _ batches matches) batch =
  ProtocolDigest state (batches <> PopulationDigest 1 0) (matches <> populationDigest id batch)

runSweepInput :: Int -> RunSweepInput
runSweepInput size =
  fmap ((,) fullScope) (probeRequestBatch size)

expectedRunDigest :: Int -> ProtocolDigest
expectedRunDigest size =
  ProtocolDigest
    size
    (PopulationDigest size 0)
    (PopulationDigest size (sumFromZero size + size * (probeWorld + 1)))

probeWorld :: Int
probeWorld =
  7

probeScopeWeightFromDelta :: Scoped IntSet IntSet -> Int
probeScopeWeightFromDelta =
  probeScopeWeight . scopedDeltaSupport

scopeConstructorWeight :: Scope scope -> Int
scopeConstructorWeight =
  foldScope 0 (const 1) 2

forcePrepareSweepInput :: PrepareSweepInput -> ()
forcePrepareSweepInput (PrepareSweepInput delta requests) =
  rnf (probeScopeWeightFromDelta delta, fmap probeRequestValue requests)

forceRunSweepInput :: RunSweepInput -> ()
forceRunSweepInput =
  rnf . fmap (\(scope, request) -> (probeScopeWeight scope, probeRequestValue request))

forceScopeTransportInput :: ScopeTransportInput -> ()
forceScopeTransportInput =
  rnf . fmap (\query -> (probeScopeWeight (mqScope query), probeRequestValue (mqRequest query)))

forceContextGlueInput :: ContextGlueInput -> ()
forceContextGlueInput (ContextGlueInput delta workload) =
  rnf (probeScopeWeightFromDelta delta, show workload)

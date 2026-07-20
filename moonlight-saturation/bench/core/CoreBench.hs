module CoreBench
  ( coreBenchmarks,
  )
where

import BenchSupport
import Control.DeepSeq (NFData (..))
import Moonlight.Saturation.Core
import Moonlight.Saturation.Test.CoreFixture
import Test.Tasty.Bench (Benchmark)

data CoreWorkload
  = ApplyUntilGoal
  | ApplyUntilStop
  | ApplyUntilConverged
  | AdvanceUntilIterationLimit
  deriving stock (Bounded, Enum, Eq, Ord, Show)

data CoreInput = CoreInput !SaturationBudget !ToyState
  deriving stock (Show)

data CoreDigest = CoreDigest !SaturationTermination !Int !Int
  deriving stock (Eq, Show)

instance NFData CoreDigest where
  rnf (CoreDigest termination iterations facts) = termination `seq` rnf (iterations, facts)

coreBenchmarks :: Either BenchmarkObstruction Benchmark
coreBenchmarks =
  validatedBenchmarkGroup
    "core"
    (fmap workloadBenchmarks coreWorkloads)

coreWorkloads :: [CoreWorkload]
coreWorkloads = [minBound .. maxBound]

coreScales :: [Int]
coreScales = [4096, 65536, 1048576]

workloadBenchmarks :: CoreWorkload -> Either BenchmarkObstruction Benchmark
workloadBenchmarks workload =
  validatedBenchmarkFamily (workloadLabel workload) (coreBenchmark workload) coreScales

coreBenchmark :: CoreWorkload -> Int -> Either BenchmarkObstruction Benchmark
coreBenchmark workload target =
  let caseName = benchmarkCaseLabel "iterations" target
      input = coreInput workload target
   in validatedPureBenchmark
        CoreBenchmarkLane
        caseName
        (expectedCoreDigest workload target)
        (rnf . show)
        (forceEither rnf)
        (runCoreDigest workload)
        input

workloadLabel :: CoreWorkload -> String
workloadLabel workload =
  case workload of
    ApplyUntilGoal -> "apply-goal"
    ApplyUntilStop -> "apply-stop"
    ApplyUntilConverged -> "apply-converged"
    AdvanceUntilIterationLimit -> "advance-iteration-limit"

coreInput :: CoreWorkload -> Int -> CoreInput
coreInput workload target =
  CoreInput
    (SaturationBudget (target + iterationHeadroom workload) (target + 1))
    (initialToy target)

iterationHeadroom :: CoreWorkload -> Int
iterationHeadroom workload =
  case workload of
    ApplyUntilGoal -> 2
    ApplyUntilStop -> 2
    ApplyUntilConverged -> 2
    AdvanceUntilIterationLimit -> 0

expectedCoreDigest :: CoreWorkload -> Int -> Either String CoreDigest
expectedCoreDigest workload target =
  Right
    ( CoreDigest
      (case workload of
         ApplyUntilGoal -> ReachedGoal
         ApplyUntilStop -> ReachedFixedPoint
         ApplyUntilConverged -> ReachedFixedPoint
         AdvanceUntilIterationLimit -> HitIterationLimit)
      target
      ( case workload of
          ApplyUntilGoal -> target
          ApplyUntilStop -> target
          ApplyUntilConverged -> target
          AdvanceUntilIterationLimit -> 0
      )
    )

runCoreDigest :: CoreWorkload -> CoreInput -> Either String CoreDigest
runCoreDigest workload (CoreInput budget state) =
  fmap coreRunDigest
    (runSaturation budget (kernelFor workload) state)

kernelFor :: CoreWorkload -> SaturationKernel ToyState ToyRound Int Int String
kernelFor workload =
  case workload of
    ApplyUntilGoal -> unobservedToyKernel
    ApplyUntilStop -> fixedPointToyKernel
    ApplyUntilConverged -> convergedToyKernel
    AdvanceUntilIterationLimit -> idleKernel

coreRunDigest :: SaturationRun ToyState -> CoreDigest
coreRunDigest run =
  let finalState = srFinalState run
   in CoreDigest (srTermination run) (tsIteration finalState) (tsFacts finalState)

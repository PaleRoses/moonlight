module Proof
  ( proofBenchmarkPreflight,
    proofBenchmarks,
  )
where

import Control.Monad (void)
import Data.Foldable (traverse_)
import Data.IntSet qualified as IntSet
import Data.Proxy (Proxy)
import Moonlight.Core
  ( ClassId (..),
    RewriteRuleId (..),
    emptySubstitution,
  )
import Common
  ( benchSizes,
    boolWeight,
    caseLabel,
    eitherWeight,
    expectBench,
  )
import Moonlight.Rewrite.ProofContext
  ( ProofCompressionSummary (..),
    ProofGraph (..),
    ProofReachability,
    ProofRegistry,
    ProofStep (..),
    defaultProofStepInput,
    emptyProofRegistry,
    proofBetween,
    proofClassesReachableFrom,
    proofGraph,
    proofReachability,
    proofRegistryDroppedStepCount,
    proofRegistryRecordedStepCount,
    proofRegistryRetainedStepCount,
    proofRelated,
    recordProofStepWith,
    serializeProofLog,
    summarizeProofLog,
  )
import Test.Tasty.Bench
  ( Benchmark,
    bench,
    bgroup,
    nf,
  )

type BenchProofRegistry = ProofRegistry Proxy () Int

proofBenchmarks :: Benchmark
proofBenchmarks =
  bgroup
    "proof"
    ( (benchSizes >>= proofBenchmarksForSize)
        <> [ bench
               "proofReachability/contiguous-4096"
               (nf (proofGraphReachabilityWeightFrom (ClassId 0)) contiguousProofRegistry),
             bench
               "proofReachability/sparse-1000000"
               (nf (proofGraphReachabilityWeightFrom (ClassId 1_000_000)) sparseProofRegistry)
           ]
    )

proofBenchmarkPreflight :: IO ()
proofBenchmarkPreflight =
  traverse_ proofSizePreflight benchSizes
    *> sequence_
      [ void (expectBench "proofReachability contiguous-4096" (proofReachability contiguousProofRegistry)),
        void (expectBench "proofReachability sparse-1000000" (proofReachability sparseProofRegistry))
      ]

proofSizePreflight :: Int -> IO ()
proofSizePreflight size =
  sequence_
    [ void (expectBench (caseLabel "proofBetween" size) (proofBetween (ClassId 0) (ClassId 1) registry)),
      void (expectBench (caseLabel "proofGraph" size) (proofGraph registry)),
      void (expectBench (caseLabel "proofReachability" size) (proofReachability registry)),
      expectBench (caseLabel "proofRelated" size) (proofRelated (ClassId 0) (ClassId (boundedProofSize size)) registry)
        >>= \related ->
          if related then pure () else fail (caseLabel "proofRelated returned False" size)
    ]
  where
    registry =
      proofRegistryForSize size

proofBenchmarksForSize :: Int -> [Benchmark]
proofBenchmarksForSize size =
  let registry = proofRegistryForSize size
   in [ bench (caseLabel "recordProofStepWith" size) (nf (proofRegistryWeight . proofRegistryForSize) size),
        bench (caseLabel "proofBetween" size) (nf (eitherWeight proofStepWeight . proofBetween (ClassId 0) (ClassId 1)) registry),
        bench (caseLabel "proofGraph/reachability" size) (nf proofGraphReachabilityWeight registry),
        bench (caseLabel "proofRelated" size) (nf (either (const Nothing) boolWeight . proofRelated (ClassId 0) (ClassId (boundedProofSize size))) registry),
        bench (caseLabel "serializeProofLog" size) (nf (sum . fmap proofStepWeight . serializeProofLog) registry),
        bench (caseLabel "summarizeProofLog" size) (nf (proofSummaryWeight . summarizeProofLog) registry)
      ]

proofRegistryForSize :: Int -> BenchProofRegistry
proofRegistryForSize size =
  foldl' (flip recordProofStep) emptyProofRegistry [0 .. boundedProofSize size - 1]

contiguousProofRegistry :: BenchProofRegistry
contiguousProofRegistry =
  proofRegistryForSize 4095

sparseProofRegistry :: BenchProofRegistry
sparseProofRegistry =
  recordProofStepWith
    ( defaultProofStepInput
        (RewriteRuleId 0)
        (ClassId 1_000_000)
        (ClassId 1_000_000)
        emptySubstitution
        0
    )
    emptyProofRegistry

recordProofStep :: Int -> BenchProofRegistry -> BenchProofRegistry
recordProofStep key =
  recordProofStepWith $
    defaultProofStepInput
      (RewriteRuleId (key `mod` 7))
      (ClassId key)
      (ClassId (key + 1))
      emptySubstitution
      key

proofRegistryWeight :: BenchProofRegistry -> Int
proofRegistryWeight registry =
  proofRegistryRecordedStepCount registry + proofRegistryRetainedStepCount registry + proofRegistryDroppedStepCount registry

proofGraphReachabilityWeight :: BenchProofRegistry -> Maybe Int
proofGraphReachabilityWeight registry =
  proofGraphReachabilityWeightFrom (ClassId 0) registry

proofGraphReachabilityWeightFrom :: ClassId -> BenchProofRegistry -> Maybe Int
proofGraphReachabilityWeightFrom sourceClass registry =
  (+)
    <$> eitherWeight (length . pgEdges) (proofGraph registry)
    <*> eitherWeight (reachabilityWeightFrom sourceClass) (proofReachability registry)

reachabilityWeightFrom :: ClassId -> ProofReachability -> Int
reachabilityWeightFrom sourceClass reachability =
  IntSet.size (proofClassesReachableFrom sourceClass reachability)

proofStepWeight :: ProofStep Proxy () Int -> Int
proofStepWeight proofStep =
  psAnnotation proofStep + 1

proofSummaryWeight :: ProofCompressionSummary -> Int
proofSummaryWeight summary =
  pcsTotalSteps summary
    + pcsUniqueClassPairs summary
    + pcsUniqueRewriteRules summary
    + pcsCompressionSavings summary
    + pcsWitnessedSteps summary
    + pcsGuardedSteps summary
    + pcsContextualSteps summary
    + pcsSupportAwareSteps summary
    + pcsUniqueSupports summary
    + pcsFactfulSteps summary

boundedProofSize :: Int -> Int
boundedProofSize =
  max 1

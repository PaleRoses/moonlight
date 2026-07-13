module OpticsBench
  ( opticsBenchmarks,
  )
where

import BenchSupport (caseLabel, keys, largeSizes)
import Moonlight.Optics
  ( planWrite,
    writeDelta,
    writeOptic,
  )
import Optics.Core
  ( Lens',
    Traversal',
    lens,
    over,
    traversed,
    (%),
  )
import Test.Tasty.Bench (Benchmark, bench, bgroup, nf)

data Sample = Sample
  { sampleFocus :: Int,
    sampleValues :: [Int]
  }
  deriving stock (Eq, Show)

opticsBenchmarks :: Benchmark
opticsBenchmarks =
  bgroup
    "optics"
    [ bgroup "write-plan" (fmap writePlanBenchmark largeSizes),
      bgroup "traversal-over" (fmap traversalBenchmark largeSizes)
    ]

writePlanBenchmark :: Int -> Benchmark
writePlanBenchmark size =
  bench (caseLabel "sample" size) (nf writePlanFocus size)

traversalBenchmark :: Int -> Benchmark
traversalBenchmark size =
  bench (caseLabel "sample" size) (nf traversalSum size)

writePlanFocus :: Int -> Int
writePlanFocus size =
  writeDelta
    (\_ target -> sampleFocus target)
    (planWrite (writeOptic sampleFocusLens) (+ 1) (sample size))

traversalSum :: Int -> Int
traversalSum size =
  sum (sampleValues (over sampleValuesTraversal (+ 1) (sample size)))

sample :: Int -> Sample
sample size =
  Sample
    { sampleFocus = size,
      sampleValues = keys size
    }

sampleFocusLens :: Lens' Sample Int
sampleFocusLens =
  lens sampleFocus (\sampleValue focus -> sampleValue {sampleFocus = focus})

sampleValuesLens :: Lens' Sample [Int]
sampleValuesLens =
  lens sampleValues (\sampleValue values -> sampleValue {sampleValues = values})

sampleValuesTraversal :: Traversal' Sample Int
sampleValuesTraversal =
  sampleValuesLens % traversed

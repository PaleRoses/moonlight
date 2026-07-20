module OpticsBench
  ( opticsBenchmarks,
  )
where

import Control.DeepSeq (NFData (rnf))
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
import Test.Tasty.Bench (Benchmark, bench, bgroup, env, nf)

newtype FocusSample = FocusSample
  { sampleFocus :: Int
  }

instance NFData FocusSample where
  rnf (FocusSample focus) = rnf focus

newtype ValuesSample = ValuesSample
  { sampleValues :: [Int]
  }

instance NFData ValuesSample where
  rnf (ValuesSample values) = rnf values

opticsBenchmarks :: Benchmark
opticsBenchmarks =
  bgroup
    "optics"
    [ bgroup "write-plan" (fmap writePlanBenchmark largeSizes),
      bgroup "traversal-over" (fmap traversalBenchmark largeSizes)
    ]

writePlanBenchmark :: Int -> Benchmark
writePlanBenchmark size =
  env (pure (FocusSample size))
    (\fixture -> bench (caseLabel "sample" size) (nf writePlanResult fixture))

traversalBenchmark :: Int -> Benchmark
traversalBenchmark size =
  env (pure (ValuesSample (keys size)))
    (\fixture -> bench (caseLabel "sample" size) (nf incrementValues fixture))

writePlanResult :: FocusSample -> (FocusSample, FocusSample)
writePlanResult source =
  writeDelta
    (,)
    (planWrite (writeOptic sampleFocusLens) (+ 1) source)

incrementValues :: ValuesSample -> ValuesSample
incrementValues =
  over sampleValuesTraversal (+ 1)

sampleFocusLens :: Lens' FocusSample Int
sampleFocusLens =
  lens sampleFocus (\sampleValue focus -> sampleValue {sampleFocus = focus})

sampleValuesLens :: Lens' ValuesSample [Int]
sampleValuesLens =
  lens sampleValues (\sampleValue values -> sampleValue {sampleValues = values})

sampleValuesTraversal :: Traversal' ValuesSample Int
sampleValuesTraversal =
  sampleValuesLens % traversed

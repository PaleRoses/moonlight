module Runtime
  ( runtimeBenchmarkPreflight,
    runtimeBenchmarks,
  )
where

import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import Data.Foldable (traverse_)
import Data.Fix (Fix (..))
import Data.List.NonEmpty (NonEmpty (..))
import Moonlight.Core
  ( Pattern (..),
    ZipMatch (..),
    zipSameNodeShape,
  )
import Common
  ( benchFixTerm,
    benchSizes,
    benchVariantFixTerm,
    boolWeight,
    caseLabel,
    expectMaybeBench,
  )
import Fixture (BenchSig)
import Moonlight.Rewrite.DSL (Node)
import Moonlight.Core.Pattern.AntiUnify
  ( BinaryLGGResult (..),
    NaryLGGResult (..),
    antiUnifyAllTerms,
    antiUnifyTerms,
  )
import Moonlight.Core.Pattern.Automata
  ( PatternAutomaton,
    compileConjunctivePatternAutomaton,
    compilePatternAutomaton,
    matchesPatternAutomaton,
  )
import Test.Tasty.Bench
  ( Benchmark,
    bench,
    bgroup,
    nf,
  )

runtimeBenchmarks :: Benchmark
runtimeBenchmarks =
  bgroup
    "runtime"
    [ bgroup "anti-unify" (benchSizes >>= antiUnifyBenchmarksForSize),
      bench "anti-unify/nary lgg arity=512 terms=16" (nf wideNaryAntiUnificationWeight 0),
      bgroup "automata" (benchSizes >>= automataBenchmarksForSize)
    ]

runtimeBenchmarkPreflight :: IO ()
runtimeBenchmarkPreflight =
  traverse_ runtimeSizePreflight benchSizes
    *> (() <$ evaluate (force (wideNaryAntiUnificationWeight 0)))

runtimeSizePreflight :: Int -> IO ()
runtimeSizePreflight size =
  sequence_
    [ voidForced (binaryLggSharedStructure (antiUnifyTerms (benchFixTerm size) (benchVariantFixTerm size))),
      voidForced (antiUnifyNaryWeight size),
      expectMaybeBenchForRuntime "compile+match" (automataWeight compilePatternAutomaton size),
      expectMaybeBenchForRuntime "conjunctive compile+match" (automataWeight compileConjunctivePatternAutomaton' size)
    ]
  where
    voidForced :: NFData value => value -> IO ()
    voidForced value =
      () <$ evaluate (force value)

    expectMaybeBenchForRuntime label =
      fmap (const ()) . expectMaybeBench (caseLabel label size)

data WideNode child
  = WideLeaf !Int
  | WideBranch ![child]
  deriving stock (Eq, Ord, Functor, Foldable, Traversable)

instance ZipMatch WideNode where
  zipMatch =
    zipSameNodeShape

wideNaryAntiUnificationWeight :: Int -> Int
wideNaryAntiUnificationWeight sampleSeed =
  let result = antiUnifyAllTerms (wideAntiUnificationTerms sampleSeed)
   in naryLggSharedStructure result
        + sum (fmap length (naryLggBindings result))

wideAntiUnificationTerms :: Int -> NonEmpty (Fix WideNode)
wideAntiUnificationTerms sampleSeed =
  wideTerm sampleSeed 0 :| fmap (wideTerm sampleSeed) [1 .. 15]

wideTerm :: Int -> Int -> Fix WideNode
wideTerm sampleSeed termOffset =
  Fix
    ( WideBranch
        [ Fix (WideLeaf (sampleSeed + termOffset + childIndex))
          | childIndex <- [0 .. 511]
        ]
    )

antiUnifyBenchmarksForSize :: Int -> [Benchmark]
antiUnifyBenchmarksForSize size =
  [ bench (caseLabel "binary lgg" size) (nf (binaryLggSharedStructure . antiUnifyTerms (benchFixTerm size)) (benchVariantFixTerm size)),
    bench (caseLabel "nary lgg" size) (nf antiUnifyNaryWeight size)
  ]

antiUnifyNaryWeight :: Int -> Int
antiUnifyNaryWeight size =
  naryLggSharedStructure $
    antiUnifyAllTerms (benchFixTerm size :| [benchVariantFixTerm size, benchFixTerm (size + 1)])

automataBenchmarksForSize :: Int -> [Benchmark]
automataBenchmarksForSize size =
  [ bench (caseLabel "compile+match" size) (nf (automataWeight compilePatternAutomaton) size),
    bench (caseLabel "conjunctive compile+match" size) (nf (automataWeight compileConjunctivePatternAutomaton') size)
  ]

automataWeight :: (Pattern (Node BenchSig) -> PatternAutomaton (Node BenchSig)) -> Int -> Maybe Int
automataWeight compile size =
  boolWeight (matchesPatternAutomaton (compile (benchPattern size)) (benchFixTerm size))

compileConjunctivePatternAutomaton' :: Pattern (Node BenchSig) -> PatternAutomaton (Node BenchSig)
compileConjunctivePatternAutomaton' patternValue =
  compileConjunctivePatternAutomaton (patternValue :| [patternValue])

benchPattern :: Int -> Pattern (Node BenchSig)
benchPattern =
  fixPattern . benchFixTerm

fixPattern :: Fix (Node BenchSig) -> Pattern (Node BenchSig)
fixPattern (Fix node) =
  PatternNode (fmap fixPattern node)

module AutomataBench
  ( automataBenchmarks,
  )
where

import Data.Fix (Fix (..))
import Data.Functor.Base (ListF (..))
import Moonlight.Automata
  ( Acceptance (..),
    AcceptingDBTA (..),
    DBTA (..),
    acceptsDBTA,
    evalDBTA,
  )
import BenchSupport (caseLabel, keys, largeSizes)
import Test.Tasty.Bench (Benchmark, bench, bgroup, nf)

automataBenchmarks :: Benchmark
automataBenchmarks =
  bgroup
    "automata"
    [ bgroup "dbta-accepts" (fmap acceptsBenchmark largeSizes),
      bgroup "dbta-eval" (fmap evalBenchmark largeSizes)
    ]

acceptsBenchmark :: Int -> Benchmark
acceptsBenchmark size =
  bench (caseLabel "even-length" size) (nf acceptsEvenLength size)

evalBenchmark :: Int -> Benchmark
evalBenchmark size =
  bench (caseLabel "sum-list" size) (nf evalSum size)

acceptsEvenLength :: Int -> Bool
acceptsEvenLength size =
  acceptsDBTA evenLengthAccepting (listFix (keys size))

evalSum :: Int -> Int
evalSum size =
  evalDBTA sumListDBTA (listFix (keys size))

listFix :: [a] -> Fix (ListF a)
listFix =
  foldr (\value rest -> Fix (Cons value rest)) (Fix Nil)

evenLengthAccepting :: AcceptingDBTA (ListF Int) Bool
evenLengthAccepting =
  AcceptingDBTA
    { adbtaAlgebra = evenLengthDBTA,
      adbtaAcceptance = Acceptance id
    }

evenLengthDBTA :: DBTA (ListF Int) Bool
evenLengthDBTA =
  DBTA $ \case
    Nil -> True
    Cons _ rest -> not rest

sumListDBTA :: DBTA (ListF Int) Int
sumListDBTA =
  DBTA $ \case
    Nil -> 0
    Cons value rest -> value + rest

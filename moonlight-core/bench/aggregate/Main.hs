module Main
  ( main,
  )
where

import NumericBench (numericBenchmarks)
import SolverBench (solverBenchmarks)
import BasisBench (basisBenchmarks)
import SyntaxBench (syntaxBenchmarks)
import TermBench (termBenchmarks)
import Test.Tasty.Bench (defaultMain)

main :: IO ()
main =
  defaultMain
    [ basisBenchmarks,
      numericBenchmarks,
      syntaxBenchmarks,
      solverBenchmarks,
      termBenchmarks
    ]

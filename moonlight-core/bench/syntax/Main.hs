module Main
  ( main,
  )
where

import SyntaxBench (syntaxBenchmarks)
import Test.Tasty.Bench (defaultMain)

main :: IO ()
main =
  defaultMain [syntaxBenchmarks]

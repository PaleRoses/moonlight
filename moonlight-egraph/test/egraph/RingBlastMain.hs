module Main (main) where

import qualified Moonlight.EGraph.Diagnostics.RingBlastSpec as RingBlastSpec
import Test.Tasty (defaultMain)

main :: IO ()
main =
  defaultMain RingBlastSpec.tests

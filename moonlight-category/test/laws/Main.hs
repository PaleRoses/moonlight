module Main (main) where

import qualified Moonlight.Category.Effect.Laws as Laws
import Moonlight.Pale.Test.Runner (runTestTree)

main :: IO ()
main =
  runTestTree Laws.tests

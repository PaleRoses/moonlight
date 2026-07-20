module Main (main) where

import Moonlight.Flow.Model.ModelSpec qualified as ModelSpec
import Test.Tasty (defaultMain)

main :: IO ()
main = defaultMain ModelSpec.tests

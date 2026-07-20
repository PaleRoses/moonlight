module Main (main) where

import Moonlight.Flow.Storage.StorageSpec qualified as StorageSpec
import Test.Tasty (defaultMain)

main :: IO ()
main = defaultMain StorageSpec.tests

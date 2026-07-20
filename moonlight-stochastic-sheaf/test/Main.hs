module Main (main) where

import qualified Moonlight.Stochastic.Sheaf.Spec as Spec
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain
    (testGroup "moonlight-stochastic-sheaf" [Spec.tests])

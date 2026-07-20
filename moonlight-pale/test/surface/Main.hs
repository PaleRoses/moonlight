module Main
  ( main,
  )
where

import Moonlight.Pale.Test.Gluing.DisciplineSpec qualified as DisciplineSpec
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain (testGroup "pale-test-surface" [DisciplineSpec.tests])

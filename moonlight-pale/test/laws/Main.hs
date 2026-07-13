{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wmissing-local-signatures #-}

module Main
  ( main,
  )
where

import Moonlight.Pale.Test.Laws.AlgebraicSpec qualified as AlgebraicSpec
import Moonlight.Pale.Test.Laws.LatticeSpec qualified as LatticeSpec
import Moonlight.Pale.Test.Laws.RestrictionSpec qualified as RestrictionSpec
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain (testGroup "pale-test-laws" [AlgebraicSpec.tests, LatticeSpec.tests, RestrictionSpec.tests])

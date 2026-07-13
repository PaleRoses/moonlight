module Moonlight.Saturation.Gen
  ( genFactTarget,
  )
where

import Test.QuickCheck (Gen, chooseInt)

genFactTarget :: Gen Int
genFactTarget =
  chooseInt (0, 64)

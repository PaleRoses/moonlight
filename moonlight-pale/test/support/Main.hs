module Main
  ( main,
  )
where

import Moonlight.Pale.Test.Bridge.RecursionSpec qualified as RecursionSpec
import Moonlight.Pale.Test.Global.EitherSpec qualified as EitherSpec
import Moonlight.Pale.Test.Site.AssertionSpec qualified as AssertionSpec
import Moonlight.Pale.Test.Site.FixtureSpec qualified as FixtureSpec
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "pale-test-support"
      [ AssertionSpec.tests,
        EitherSpec.tests,
        FixtureSpec.tests,
        RecursionSpec.tests
      ]

module Moonlight.Sheaf.TestFixture.Assertions
  ( assertRight,
    expectRight,
    expectJust,
  )
where

import Test.Tasty.HUnit (assertFailure)

assertRight :: Show err => String -> Either err value -> IO value
assertRight label =
  either
    (\err -> assertFailure (label <> ": " <> show err))
    pure

expectRight :: Show failure => Either failure value -> IO value
expectRight =
  either
    (\failure -> assertFailure ("expected Right, received " <> show failure))
    pure

expectJust :: Maybe value -> IO value
expectJust =
  maybe (assertFailure "expected Just") pure

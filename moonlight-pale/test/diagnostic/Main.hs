module Main
  ( main,
  )
where

import Moonlight.Pale.Diagnostic.CohomologySpec qualified as CohomologySpec
import Moonlight.Pale.Diagnostic.OutcomeSpec qualified as OutcomeSpec
import Moonlight.Pale.Diagnostic.RefinementSpec qualified as RefinementSpec
import Moonlight.Pale.Diagnostic.WriterSpec qualified as WriterSpec
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain (testGroup "pale-diagnostic" [WriterSpec.tests, OutcomeSpec.tests, RefinementSpec.tests, CohomologySpec.tests])

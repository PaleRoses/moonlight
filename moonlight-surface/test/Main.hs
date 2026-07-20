module Main (main) where

import Moonlight.Surface.ContextSupportedFactsSpec qualified as ContextSupportedFactsSpec
import Moonlight.Surface.LawAgreementSpec qualified as LawAgreementSpec
import Moonlight.Surface.ReceiptSpec qualified as ReceiptSpec
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain
    ( testGroup
        "moonlight-surface"
        [ LawAgreementSpec.tests,
          ContextSupportedFactsSpec.tests,
          ReceiptSpec.tests
        ]
    )

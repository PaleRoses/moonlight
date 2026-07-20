module Moonlight.Pale.Ghc.ModuleSurfaceSpec
  ( tests,
  )
where

import Moonlight.Pale.Ghc.ModuleSurface (parseHsModule)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "pale.module-surface"
    [ testCase "GHC2024 keeps pattern available as a type-variable name" $
        assertParses
          "PatternTypeVariable.hs"
          [ "{-# LANGUAGE GHC2024 #-}",
            "module PatternTypeVariable where",
            "",
            "foo :: host pattern var -> ()",
            "foo _ = ()"
          ],
      testCase "PatternSynonyms is enabled only when requested by LANGUAGE pragma" $
        assertParses
          "PatternSynonymFixture.hs"
          [ "{-# LANGUAGE PatternSynonyms #-}",
            "module PatternSynonymFixture where",
            "",
            "pattern Unit = ()",
            "value = Unit"
          ]
    ]

assertParses :: FilePath -> [String] -> IO ()
assertParses sourcePath sourceLines =
  case parseHsModule sourcePath (unlines sourceLines) of
    Right _ ->
      pure ()
    Left parserError ->
      assertFailure ("expected parser success for " <> sourcePath <> ":\n" <> parserError)

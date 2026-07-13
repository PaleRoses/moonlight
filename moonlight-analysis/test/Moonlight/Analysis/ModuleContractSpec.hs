module Moonlight.Analysis.ModuleContractSpec
  ( tests,
  )
where

import qualified Data.Set as Set
import Moonlight.Analysis.ModuleContract
  ( ModuleContract (..),
    ModuleLayerTag (..),
    hasCppDirectives,
    parseModuleContract,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), Assertion, testCase)

tests :: TestTree
tests =
  testGroup
    "module-contract"
    [ testCase "extracts imports and exports from parsed module" testExtractsContract,
      testCase "detects cpp directives" testDetectsCppDirectives
    ]

testExtractsContract :: Assertion
testExtractsContract =
  let source =
        unlines
          [ "module Sample.Contract (alpha, Beta(..)) where",
            "import Data.List (nub)",
            "import qualified Melusine.Complex as Complex",
            "alpha :: Int",
            "alpha = 1",
            "data Beta = Beta"
          ]
      expected =
        ModuleContract
          { moduleContractName = Just "Sample.Contract",
            moduleContractImports = Set.fromList ["Data.List", "Melusine.Complex"],
            moduleContractExports = Set.fromList ["alpha", "Beta"],
            moduleContractLayerTags =
              Set.fromList [ModuleLayerTag "Sample", ModuleLayerTag "Contract"]
          }
   in parseModuleContract "inline" source @?= Right expected

testDetectsCppDirectives :: Assertion
testDetectsCppDirectives =
  let source =
        unlines
          [ "{-# LANGUAGE CPP #-}",
            "#if defined(TEST)",
            "module Sample.Cpp where",
            "#endif"
          ]
   in hasCppDirectives source @?= True

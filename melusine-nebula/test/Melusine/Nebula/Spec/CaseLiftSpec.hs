module Melusine.Nebula.Spec.CaseLiftSpec (spec) where

import Bench.CaseLift
  ( CorpusModule (..),
    ModuleBench (..),
    analyseModule,
    benchNebulaConfig,
    loadCorpusModules,
  )
import Data.List qualified as List
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

spec :: TestTree
spec =
  testGroup
    "nebula.case-lift"
    [ testCase "lifts parent-visible equality proven in every exhaustive case alternative" $ do
        benchValue <- requireBenchModule "CaseLift.Either"
        assertEqual "both alternatives produce branch-local structural proofs" 2 (mbAuthoredStructural benchValue)
        assertEqual "one parent equality is glued by case-split descent" 1 (mbLiftCount benchValue)
        assertBool
          "lift witness names the case parent rather than either child alternative"
          (any (List.isInfixOf "CaseLift/Either.hs:eitherKnown @") (mbLiftedFacts benchValue)),
      testCase "does not lift a rewrite whose replacement mentions a branch binder" $ do
        benchValue <- requireBenchModule "CaseLift.Head"
        assertEqual "the branch-local head fact did engage" 1 (mbAuthoredStructural benchValue)
        assertEqual "branch binder y cannot escape to the parent" 0 (mbLiftCount benchValue)
    ]

requireBenchModule :: String -> IO ModuleBench
requireBenchModule moduleName = do
  moduleValue <- requireCorpusModule moduleName
  case analyseModule benchNebulaConfig moduleValue of
    Right benchValue -> pure benchValue
    Left failure -> assertFailure ("case-lift analysis failed: " <> show failure)

requireCorpusModule :: String -> IO CorpusModule
requireCorpusModule moduleName = do
  modules <- loadCorpusModules
  case List.find ((== moduleName) . cmNameText) modules of
    Just moduleValue -> pure moduleValue
    Nothing -> assertFailure ("missing case-lift corpus module: " <> moduleName)

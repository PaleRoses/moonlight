module CompileFailSpec
  ( tests,
  )
where

import Moonlight.Pale.Test.Site.Assertion (expectRight)
import Moonlight.Pale.TestSupport.CompileDiagnostics
  ( GhcPackageSpec (..),
    SnapshotExit (..),
    compileFixture,
    normalizeSnapshot,
    resolveCompilerRoot,
    snapshotDiagnostics,
    snapshotExit,
  )
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "compile-fail fixtures"
    (fmap (\fixtureFile -> testCase fixtureFile (testFixtureFails fixtureFile)) compileFailFixtures)

compileFailFixtures :: [FilePath]
compileFailFixtures =
  [ "ConstrainedSignature.hs",
    "ExistentialSignature.hs",
    "PatternRewriteRecordUpdate.hs",
    "CompiledPatternQueryRecordUpdate.hs",
    "CompiledPatternExtensionConstructor.hs",
    "CompiledPatternExtensionRecordUpdate.hs",
    "CompiledApplicationConditionRecordUpdate.hs",
    "PBPORuleRecordUpdate.hs",
    "TermMorRecordUpdate.hs",
    "CheckedRawRewriteRuleConstructor.hs",
    "CheckedRawRewriteRuleRecordUpdate.hs",
    "CheckedRewriteConstructor.hs",
    "CheckedRewriteRecordUpdate.hs",
    "RulePlanConstructor.hs",
    "RulePlanRecordUpdate.hs",
    "CompiledFactRuleConstructor.hs",
    "CompiledFactRuleRecordUpdate.hs",
    "GuardEvidenceConstructor.hs",
    "FactDerivationIndexConstructor.hs",
    "MatchRecordUpdate.hs",
    "ProofTheoremNameConstructor.hs",
    "ProofTheoremNameRecordUpdate.hs",
    "ProofTheoremManifestRecordUpdate.hs"
  ]

testFixtureFails :: FilePath -> Assertion
testFixtureFails fixtureFile =
  withCompilerRoot $ \compilerRoot -> do
    let relativeFixture =
          "foundation"
            </> "moonlight-rewrite"
            </> "test"
            </> "fixtures"
            </> "CompileFail"
            </> fixtureFile
    result <-
      expectRight
        =<< compileFixture
          [ GhcPackageId "moonlight-rewrite-0.1.0.0-inplace",
            GhcPackageId "moonlight-rewrite-0.1.0.0-inplace-algebra",
            GhcPackageId "moonlight-rewrite-0.1.0.0-inplace-core",
            GhcPackageId "moonlight-rewrite-0.1.0.0-inplace-system",
            GhcPackageId "moonlight-rewrite-0.1.0.0-inplace-proof-context",
            GhcPackageId "moonlight-rewrite-0.1.0.0-inplace-relational-front"
          ]
          compilerRoot
          (compilerRoot </> relativeFixture)
    let snapshot = normalizeSnapshot compilerRoot relativeFixture result
    assertEqual
      (fixtureFile <> " must fail to compile")
      SnapshotFailure
      (snapshotExit snapshot)
    assertBool
      (fixtureFile <> " must emit a structured diagnostic")
      (not (null (snapshotDiagnostics snapshot)))

withCompilerRoot :: (FilePath -> IO ()) -> IO ()
withCompilerRoot action = do
  compilerRootResult <-
    resolveCompilerRoot
      ( "foundation"
          </> "moonlight-rewrite"
          </> "moonlight-rewrite.cabal"
      )
  either assertFailure action compilerRootResult

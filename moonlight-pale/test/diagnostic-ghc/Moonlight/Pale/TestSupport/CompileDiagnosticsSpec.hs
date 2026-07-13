module Moonlight.Pale.TestSupport.CompileDiagnosticsSpec
  ( tests,
  )
where

import Data.Aeson (decode, encode)
import Moonlight.Pale.Test.Site.Assertion (expectRightWithLabel)
import Moonlight.Pale.TestSupport.CompileDiagnostics
  ( CompileFixtureFailure (..),
    DiagnosticSnapshot (..),
    GhcPackageSpec (..),
    SnapshotExit (..),
    UnstructuredCompileFailure (..),
    compileFixture,
    normalizeSnapshot,
    resolveCompilerRoot,
  )
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "Moonlight.Pale.TestSupport.CompileDiagnostics"
    [ testCase "compileFixture captures a round-trippable clean snapshot" compileTrivialFixture,
      testCase "compileFixture preserves unstructured failures" compileUnstructuredFailure
    ]

compileTrivialFixture :: IO ()
compileTrivialFixture = do
  compilerRoot <- expectRightWithLabel "compiler root" =<< resolveCompilerRoot packageMarker
  compileResult <- compileFixture [] compilerRoot compilerRelativeFixturePath
  fixtureResult <- expectRightWithLabel "compile fixture" compileResult
  let snapshot :: DiagnosticSnapshot
      snapshot = normalizeSnapshot compilerRoot compilerRelativeFixturePath fixtureResult
  assertEqual "clean fixture exits successfully" SnapshotSuccess (snapshotExit snapshot)
  assertEqual "diagnostic snapshot JSON round-trips" (pure snapshot) (roundTripDiagnosticSnapshot snapshot)

compileUnstructuredFailure :: IO ()
compileUnstructuredFailure = do
  compilerRoot <- expectRightWithLabel "compiler root" =<< resolveCompilerRoot packageMarker
  compileResult <-
    compileFixture
      [GhcPackageId "pale-definitely-missing-unit-id"]
      compilerRoot
      compilerRelativeFixturePath
  case compileResult of
    Left (CompileFixtureUnstructuredFailure failureValue) ->
      case unstructuredCompileExitCode failureValue of
        ExitFailure _ -> pure ()
        ExitSuccess -> assertFailure "unstructured compiler failure cannot report success"
    Left otherFailure ->
      assertFailure ("expected unstructured compiler failure, got " <> show otherFailure)
    Right fixtureResult ->
      assertFailure ("expected fixture compilation to fail, got " <> show fixtureResult)

packageMarker :: FilePath
packageMarker =
  "foundation/pale/pale.cabal"

packageRelativeFixturePath :: FilePath
packageRelativeFixturePath =
  "test/diagnostic-ghc/fixtures/Trivial.hs"

compilerRelativeFixturePath :: FilePath
compilerRelativeFixturePath =
  takeDirectory packageMarker </> packageRelativeFixturePath

roundTripDiagnosticSnapshot :: DiagnosticSnapshot -> Maybe DiagnosticSnapshot
roundTripDiagnosticSnapshot snapshot =
  decode (encode snapshot)

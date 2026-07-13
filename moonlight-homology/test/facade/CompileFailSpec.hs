{-# LANGUAGE OverloadedStrings #-}

module CompileFailSpec (tests) where

import Data.Kind (Type)
import Moonlight.Pale.TestSupport.CompileDiagnostics
  ( DiagnosticSnapshot (..),
    GhcPackageSpec (..),
    SnapshotExit (..),
    compileFixture,
    normalizeSnapshot,
    readSnapshot,
    renderFixtureFailure,
    resolveCompilerRoot,
    snapshotRefreshEnabled,
    sortSnapshot,
    writeSnapshot,
  )
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

type FixtureCase :: Type
data FixtureCase = FixtureCase
  { fixtureCaseLabel :: !String,
    fixtureRelativePath :: !FilePath,
    fixtureSnapshotFile :: !FilePath,
    fixtureExpectedExit :: !SnapshotExit
  }
  deriving stock (Eq, Show)

tests :: TestTree
tests = testGroup "compile-fixtures" (map buildFixtureCase fixtureCases)

fixtureCases :: [FixtureCase]
fixtureCases =
  [ FixtureCase
      { fixtureCaseLabel = "phase 2 capability compiles in dedicated fixture runner",
        fixtureRelativePath = "CompilePass" </> "Phase2Betti.hs",
        fixtureSnapshotFile = "CompilePass.Phase2Betti.snapshot.json",
        fixtureExpectedExit = SnapshotSuccess
      },
    FixtureCase
      { fixtureCaseLabel = "public API cleanup exposes safe names",
        fixtureRelativePath = "CompilePass" </> "PublicApiCleanup.hs",
        fixtureSnapshotFile = "CompilePass.PublicApiCleanup.snapshot.json",
        fixtureExpectedExit = SnapshotSuccess
      },
    FixtureCase
      { fixtureCaseLabel = "phase 1 betti capability fails at compile-time",
        fixtureRelativePath = "CompileFail" </> "Phase1BettiLeak.hs",
        fixtureSnapshotFile = "CompileFail.Phase1BettiLeak.snapshot.json",
        fixtureExpectedExit = SnapshotFailure
      },
    FixtureCase
      { fixtureCaseLabel = "phase 2 spectral capability fails at compile-time",
        fixtureRelativePath = "CompileFail" </> "Phase2SpectralLeak.hs",
        fixtureSnapshotFile = "CompileFail.Phase2SpectralLeak.snapshot.json",
        fixtureExpectedExit = SnapshotFailure
      }
  ]

buildFixtureCase :: FixtureCase -> TestTree
buildFixtureCase fixtureCase =
  testCase (fixtureCaseLabel fixtureCase) $ withCompilerRoot (assertFixtureSnapshot fixtureCase)

assertFixtureSnapshot :: FixtureCase -> FilePath -> IO ()
assertFixtureSnapshot fixtureCase compilerRoot = do
  let fixturesRoot =
        compilerRoot
          </> "foundation"
          </> "moonlight-homology"
          </> "test"
          </> "fixtures"
      fixturePath = fixturesRoot </> fixtureRelativePath fixtureCase
      snapshotPath = fixturesRoot </> "Snapshots" </> fixtureSnapshotFile fixtureCase

  refreshSnapshots <- snapshotRefreshEnabled
  result <- expectRight =<< compileFixture fixturePackageIds compilerRoot fixturePath

  let actualSnapshot =
        sortSnapshot
          (normalizeSnapshot compilerRoot (fixtureRelativePath fixtureCase) result)

  assertEqual
    "fixture exit mode mismatch"
    (fixtureExpectedExit fixtureCase)
    (snapshotExit actualSnapshot)

  case snapshotExit actualSnapshot of
    SnapshotSuccess ->
      assertBool
        "compile-pass fixture must emit zero structured error diagnostics"
        (null (snapshotDiagnostics actualSnapshot))
    SnapshotFailure ->
      assertBool
        "compile-fail fixture must emit at least one structured error diagnostic"
        (not (null (snapshotDiagnostics actualSnapshot)))

  if refreshSnapshots
    then writeSnapshot snapshotPath actualSnapshot
    else do
      expectedSnapshotResult <- readSnapshot snapshotPath
      expectedSnapshot <-
        case expectedSnapshotResult of
          Left errorMessage -> assertFailure errorMessage
          Right snapshotValue -> pure snapshotValue

      let sortedExpectedSnapshot = sortSnapshot expectedSnapshot

      assertEqual
        ( "snapshot mismatch for fixture: "
            <> fixtureRelativePath fixtureCase
            <> "\n"
            <> renderFixtureFailure result
        )
        sortedExpectedSnapshot
        actualSnapshot

withCompilerRoot :: (FilePath -> IO ()) -> IO ()
withCompilerRoot action = do
  compilerRootResult <-
    resolveCompilerRoot
      ( "foundation"
          </> "moonlight-homology"
          </> "moonlight-homology.cabal"
      )
  case compilerRootResult of
    Left errorMessage -> assertFailure errorMessage
    Right compilerRoot -> action compilerRoot

fixturePackageIds :: [GhcPackageSpec]
fixturePackageIds =
  [ GhcPackageName "moonlight-homology-0.1.0.0",
    GhcPackageName "moonlight-core-0.1.0.0"
  ]

expectRight ::
  Show left =>
  Either left right ->
  IO right
expectRight result =
  case result of
    Left failureValue ->
      assertFailure (show failureValue)
    Right value ->
      pure value

module Moonlight.EGraph.Pure.Session.CompileSpec
  ( tests,
  )
where

import Moonlight.Pale.TestSupport.CompileDiagnostics
  ( DiagnosticSnapshot (..),
    FixtureCompileResult,
    GhcPackageSpec (..),
    NormalizedDiagnostic (..),
    SnapshotExit (..),
    compileFixturesWithDiagnosticsFlag,
    normalizeSnapshot,
    renderFixtureFailure,
    resolveCompilerRoot,
    resolveDiagnosticsFlag,
    sortSnapshot,
  )
import System.FilePath
  ( normalise,
    (</>),
  )
import Test.Tasty
  ( TestTree,
    testGroup,
    withResource,
  )
import Test.Tasty.HUnit
  ( assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )

data FixtureCase = FixtureCase
  { fixtureCaseLabel :: !String,
    fixtureRelativePath :: !FilePath,
    fixtureExpectedExit :: !SnapshotExit
  }
  deriving stock (Eq, Show)

data SessionCompileEnv = SessionCompileEnv
  { sceCompilerRoot :: !FilePath,
    sceBatchResult :: !FixtureCompileResult
  }

tests :: TestTree
tests =
  withResource resolveSessionCompileEnv (const (pure ())) $ \getSessionCompileEnv ->
    testGroup "phase-index compile fixtures" (fmap (buildFixtureCase getSessionCompileEnv) fixtureCases)

fixtureCases :: [FixtureCase]
fixtureCases =
  [ FixtureCase
      { fixtureCaseLabel = "stable rebuild before extraction composes",
        fixtureRelativePath = sessionFixturePath "test/session/fixtures/compile" "PhaseCompositionExample.hs",
        fixtureExpectedExit = SnapshotSuccess
      },
    FixtureCase
      { fixtureCaseLabel = "dirty extraction is rejected by the phase index",
        fixtureRelativePath = sessionFixturePath "test/session/fixtures/compile-fail" "DirtyExtractShouldNotTypecheck.hs",
        fixtureExpectedExit = SnapshotFailure
      }
  ]

buildFixtureCase :: IO SessionCompileEnv -> FixtureCase -> TestTree
buildFixtureCase getSessionCompileEnv fixtureCase =
  testCase (fixtureCaseLabel fixtureCase) $
    getSessionCompileEnv >>= assertFixtureCase fixtureCase

assertFixtureCase :: FixtureCase -> SessionCompileEnv -> IO ()
assertFixtureCase fixtureCase sessionCompileEnv = do
  let actualSnapshot =
        fixtureSnapshotFromBatch fixtureCase sessionCompileEnv

  assertEqual
    "fixture exit mode mismatch"
    (fixtureExpectedExit fixtureCase)
    (snapshotExit actualSnapshot)

  assertStructuredDiagnostics fixtureCase actualSnapshot (sceBatchResult sessionCompileEnv)

fixtureSnapshotFromBatch :: FixtureCase -> SessionCompileEnv -> DiagnosticSnapshot
fixtureSnapshotFromBatch fixtureCase sessionCompileEnv =
  let fixtureRelative =
        normalise (fixtureRelativePath fixtureCase)
      batchSnapshot =
        normalizeSnapshot
          (sceCompilerRoot sessionCompileEnv)
          (fixtureRelativePath fixtureCase)
          (sceBatchResult sessionCompileEnv)
      fixtureDiagnostics =
        filter ((== fixtureRelative) . normalizedFile) (snapshotDiagnostics batchSnapshot)
   in sortSnapshot
        ( batchSnapshot
            { snapshotExit =
                if null fixtureDiagnostics
                  then SnapshotSuccess
                  else SnapshotFailure,
              snapshotDiagnostics = fixtureDiagnostics
            }
        )

assertStructuredDiagnostics ::
  FixtureCase ->
  DiagnosticSnapshot ->
  FixtureCompileResult ->
  IO ()
assertStructuredDiagnostics fixtureCase actualSnapshot result =
  case fixtureExpectedExit fixtureCase of
    SnapshotSuccess ->
      assertBool
        "compile-pass fixture must emit zero structured error diagnostics"
        (null (snapshotDiagnostics actualSnapshot))
    SnapshotFailure ->
      assertBool
        ( "compile-fail fixture must emit at least one structured error diagnostic\n"
            <> renderFixtureFailure result
        )
        (not (null (snapshotDiagnostics actualSnapshot)))

resolveSessionCompileEnv :: IO SessionCompileEnv
resolveSessionCompileEnv = do
  compilerRootResult <- resolveCompilerRoot sessionPackageMarker
  case compilerRootResult of
    Left errorMessage ->
      assertFailure errorMessage
    Right compilerRoot -> do
      diagnosticsFlagResult <- resolveDiagnosticsFlag compilerRoot
      case diagnosticsFlagResult of
        Left flagSelectionFailure ->
          assertFailure ("expected diagnostics flag selection, got " <> show flagSelectionFailure)
        Right diagnosticsFlag -> do
          batchCompileResult <-
            compileFixturesWithDiagnosticsFlag
              diagnosticsFlag
              sessionFixturePackageSpecs
              compilerRoot
              (fmap ((compilerRoot </>) . fixtureRelativePath) fixtureCases)
          case batchCompileResult of
            Left compileFailure ->
              assertFailure ("expected fixture batch compile result, got " <> show compileFailure)
            Right batchResult ->
              pure (SessionCompileEnv compilerRoot batchResult)

sessionPackageMarker :: FilePath
sessionPackageMarker =
  "foundation"
    </> "moonlight-egraph"
    </> "moonlight-egraph.cabal"

sessionFixturePath :: FilePath -> FilePath -> FilePath
sessionFixturePath fixtureRoot fixtureFile =
  "foundation"
    </> "moonlight-egraph"
    </> fixtureRoot
    </> "Moonlight"
    </> "EGraph"
    </> "Pure"
    </> "Session"
    </> fixtureFile

sessionFixturePackageSpecs :: [GhcPackageSpec]
sessionFixturePackageSpecs =
  [ GhcPackageId "moonlight-egraph-core-0.1.0.0-inplace",
    GhcPackageId "moonlight-egraph-0.1.0.0-inplace-session",
    GhcPackageId "moonlight-egraph-0.1.0.0-inplace-extraction"
  ]

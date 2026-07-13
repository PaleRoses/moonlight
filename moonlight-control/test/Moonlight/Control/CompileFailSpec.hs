module Moonlight.Control.CompileFailSpec
  ( tests,
  )
where

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
    [ testCase "invalid symbolic labels have no fallback instance" testInvalidSymbolicLabelFails,
      testCase "validated engine specs cannot be directly constructed" $
        testFixtureFails "EngineSpecValidatedConstructor.hs",
      testCase "validated engine specs cannot be record-updated" $
        testFixtureFails "EngineSpecRecordUpdate.hs",
      testCase "priority profiles cannot be directly constructed" $
        testFixtureFails "PriorityProfileConstructor.hs",
      testCase "programs are opaque outside the algebra and Internal" $
        testFixtureFails "ProgramConstructor.hs"
    ]

testInvalidSymbolicLabelFails :: Assertion
testInvalidSymbolicLabelFails =
  testFixtureFails "InvalidSymbolicLabel.hs"

testFixtureFails :: FilePath -> Assertion
testFixtureFails fixtureFile =
  withCompilerRoot $ \compilerRoot -> do
    result <-
      expectRight
        =<< compileFixture
          [GhcPackageName "moonlight-control"]
          compilerRoot
          ( compilerRoot
              </> "foundation"
              </> "moonlight-control"
              </> "test"
              </> "fixtures"
              </> "CompileFail"
              </> fixtureFile
          )

    let snapshot =
          normalizeSnapshot
            compilerRoot
            ("foundation" </> "moonlight-control" </> "test" </> "fixtures" </> "CompileFail" </> fixtureFile)
            result

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
          </> "moonlight-control"
          </> "moonlight-control.cabal"
      )
  case compilerRootResult of
    Left errorMessage -> assertFailure errorMessage
    Right compilerRoot -> action compilerRoot

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

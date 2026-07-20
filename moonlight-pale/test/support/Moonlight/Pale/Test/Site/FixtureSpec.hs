module Moonlight.Pale.Test.Site.FixtureSpec
  ( tests,
  )
where

import Data.Bifunctor (first)
import Moonlight.Pale.Test.Section.ResourcePath
  ( ResourcePathError (..),
    renderResourcePathError,
    resolvePackageDirectory,
    resolvePackageRoot,
  )
import Moonlight.Pale.Test.Site.Fixture
  ( FixtureM (..),
    fixtureFromEither,
    fixtureFromIO,
    fixtureFromMaybe,
    withFixture,
  )
import System.FilePath (makeRelative, normalise, (</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, (@?=), testCase)

tests :: TestTree
tests =
  testGroup
    "Moonlight.Pale.Test.Site.Fixture"
    [ testCase "constructs successful fixtures from Either" $ do
        result <- unFixtureM successfulFixture
        result @?= Right resourceRelativeDirectory,
      testCase "constructs failed fixtures from Maybe" $ do
        result <- unFixtureM missingFixture
        result @?= Left missingFixtureLabel,
      testCase "resolves a package resource through a fixture" $ do
        packageRoot <- expectResourcePath (resolvePackageRoot palePackageMarker)
        withFixture
          "pale test resource"
          resourceDirectoryFixture
          (assertRelativePath packageRoot resourceRelativeDirectory),
      testCase "reports missing resource directories structurally" $ do
        packageRoot <- expectResourcePath (resolvePackageRoot palePackageMarker)
        missingResult <- resolvePackageDirectory palePackageMarker missingResourceRelativeDirectory
        assertMissingDirectory packageRoot missingResult
    ]

palePackageMarker :: FilePath
palePackageMarker =
  "foundation/pale/pale.cabal"

resourceRelativeDirectory :: FilePath
resourceRelativeDirectory =
  "src-test" </> "Pale" </> "Test"

missingResourceRelativeDirectory :: FilePath
missingResourceRelativeDirectory =
  resourceRelativeDirectory </> "__missing_resource_path__"

missingFixtureLabel :: String
missingFixtureLabel =
  "missing fixture value"

successfulFixture :: FixtureM FilePath
successfulFixture =
  fixtureFromEither (Right resourceRelativeDirectory)

missingFixture :: FixtureM FilePath
missingFixture =
  fixtureFromMaybe missingFixtureLabel Nothing

resourceDirectoryFixture :: FixtureM FilePath
resourceDirectoryFixture =
  fixtureFromIO (first renderResourcePathError <$> resolvePackageDirectory palePackageMarker resourceRelativeDirectory)

expectResourcePath :: IO (Either ResourcePathError FilePath) -> IO FilePath
expectResourcePath action =
  action >>= either (assertFailure . renderResourcePathError) pure

assertRelativePath :: FilePath -> FilePath -> FilePath -> Assertion
assertRelativePath packageRoot expectedRelativePath actualPath =
  makeRelative packageRoot actualPath @?= normalise expectedRelativePath

assertMissingDirectory :: FilePath -> Either ResourcePathError FilePath -> Assertion
assertMissingDirectory packageRoot result =
  case result of
    Left (MissingResourceDirectory missingPath) ->
      makeRelative packageRoot missingPath @?= normalise missingResourceRelativeDirectory
    Left resourcePathError ->
      assertFailure (renderResourcePathError resourcePathError)
    Right resolvedPath ->
      assertFailure ("expected missing resource directory, got: " <> makeRelative packageRoot resolvedPath)

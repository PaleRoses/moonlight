module Moonlight.EGraph.Boundary.RegistryConsistencySpec
  ( tests,
  )
where

import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.List (intercalate, isPrefixOf)
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Moonlight.Pale.Test.Gluing.Registry
  ( ModuleSurface,
    assertRegisteredSetMatches,
    cabalFieldEntries,
    cabalStanzaBody,
    discoverModuleSurfaces,
    moduleSurfaceExportedNames,
    moduleSurfaceIdentity,
    moduleSurfaceImportedNames,
    readModuleSurfaceFile,
  )
import Moonlight.Pale.Test.Section.ResourcePath
  ( renderResourcePathError,
    resolvePackageDirectory,
    resolvePackageFile,
  )
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>), dropExtension, makeRelative, splitDirectories, takeExtension)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

packageMarker :: FilePath
packageMarker =
  "foundation/moonlight-egraph/moonlight-egraph.cabal"

cabalRelativePath :: FilePath
cabalRelativePath =
  "moonlight-egraph.cabal"

testMainRelativePath :: FilePath
testMainRelativePath =
  "test/egraph/Main.hs"

testSuiteRegistryRelativePath :: FilePath
testSuiteRegistryRelativePath =
  "test/egraph/Moonlight/EGraph/Test/Suite.hs"

ringBlastMainRelativePath :: FilePath
ringBlastMainRelativePath =
  "test/egraph/RingBlastMain.hs"

testDirectoryRelativePath :: FilePath
testDirectoryRelativePath =
  "test/egraph"

defaultTestSuiteName :: String
defaultTestSuiteName =
  "test-suite egraph-test"

ringBlastTestSuiteName :: String
ringBlastTestSuiteName =
  "test-suite ring-blast-test"

libraryComponents :: [(FilePath, String, FilePath)]
libraryComponents =
  [ ("core/moonlight-egraph-core.cabal", "library", "core/src"),
    ("moonlight-egraph.cabal", "library relational", "src-relational"),
    ("moonlight-egraph.cabal", "library context", "src-context"),
    ("moonlight-egraph.cabal", "library homology", "src-homology"),
    ("moonlight-egraph.cabal", "library extraction", "src-extraction"),
    ("moonlight-egraph.cabal", "library rewrite", "src-rewrite"),
    ("moonlight-egraph.cabal", "library pure-saturation", "src-pure-saturation"),
    ("moonlight-egraph.cabal", "library session", "src-session"),
    ("moonlight-egraph.cabal", "library test-algebras", "src-test-algebras")
  ]

tests :: TestTree
tests =
  testGroup
    "RegistryConsistency"
    [ testCase "library exposed/other-modules match source module covers" $
        traverse_
          assertLibraryComponentExposure
          libraryComponents,
      testCase "test-suite other-modules are aligned with discovered test modules and suite registries" $ do
        cabalFileResult <- resolvePackageFile packageMarker cabalRelativePath
        testMainFileResult <- resolvePackageFile packageMarker testMainRelativePath
        testSuiteRegistryFileResult <- resolvePackageFile packageMarker testSuiteRegistryRelativePath
        ringBlastMainFileResult <- resolvePackageFile packageMarker ringBlastMainRelativePath
        testDirectoryResult <- resolvePackageDirectory packageMarker testDirectoryRelativePath
        case (cabalFileResult, testMainFileResult, testSuiteRegistryFileResult, ringBlastMainFileResult, testDirectoryResult) of
          (Left pathError, _, _, _, _) -> assertFailure (renderResourcePathError pathError)
          (_, Left pathError, _, _, _) -> assertFailure (renderResourcePathError pathError)
          (_, _, Left pathError, _, _) -> assertFailure (renderResourcePathError pathError)
          (_, _, _, Left pathError, _) -> assertFailure (renderResourcePathError pathError)
          (_, _, _, _, Left pathError) -> assertFailure (renderResourcePathError pathError)
          (Right cabalFile, Right testMainFile, Right testSuiteRegistryFile, Right ringBlastMainFile, Right testDirectory) -> do
            cabalContents <- readFile cabalFile
            discoveredModulesResult <- discoverModuleSurfaces testDirectory
            testMainSurfaceResult <- readModuleSurfaceFile testMainFile
            testSuiteRegistrySurfaceResult <- readModuleSurfaceFile testSuiteRegistryFile
            ringBlastMainSurfaceResult <- readModuleSurfaceFile ringBlastMainFile
            case (discoveredModulesResult, testMainSurfaceResult, testSuiteRegistrySurfaceResult, ringBlastMainSurfaceResult) of
              (Left parseErrors, _, _, _) ->
                assertFailure ("failed to parse test modules for registry consistency:\n" <> intercalate "\n" parseErrors)
              (_, Left parseError, _, _) ->
                assertFailure parseError
              (_, _, Left parseError, _) ->
                assertFailure parseError
              (_, _, _, Left parseError) ->
                assertFailure parseError
              (Right discoveredSurfaces, Right testMainSurface, Right testSuiteRegistrySurface, Right ringBlastMainSurface) ->
                let defaultRegisteredModules =
                      Set.fromList
                        (cabalFieldEntries "other-modules:" (cabalStanzaBody defaultTestSuiteName (lines cabalContents)))
                    ringBlastRegisteredModules =
                      Set.fromList
                        (cabalFieldEntries "other-modules:" (cabalStanzaBody ringBlastTestSuiteName (lines cabalContents)))
                    registeredTestModules =
                      defaultRegisteredModules <> ringBlastRegisteredModules
                    discoveredTestModules =
                      discoveredSurfaces
                        & map moduleIdentityOrFail
                        & sequence
                        & either Left (Right . Set.fromList . filter (`notElem` ["Main", "RingBlastMain"]))
                    runnableTestModules =
                      discoveredSurfaces
                        & mapMaybe exportedTestModuleName
                        & Set.fromList
                    mainImportedTestModules =
                      moduleSurfaceImportedNames testMainSurface
                        & Set.filter ("Moonlight.EGraph." `isPrefixOf`)
                    expectedMainImports =
                      Set.singleton "Moonlight.EGraph.Test.Suite"
                    suiteImportedTestModules =
                      moduleSurfaceImportedNames testSuiteRegistrySurface
                        & Set.filter ("Moonlight.EGraph." `isPrefixOf`)
                    ringBlastImportedTestModules =
                      moduleSurfaceImportedNames ringBlastMainSurface
                        & Set.filter ("Moonlight.EGraph." `isPrefixOf`)
                    defaultRunnableModules =
                      Set.intersection runnableTestModules defaultRegisteredModules
                    ringBlastRunnableModules =
                      Set.intersection runnableTestModules ringBlastRegisteredModules
                 in case discoveredTestModules of
                      Left missingIdentity ->
                        assertFailure missingIdentity
                      Right discoveredModules ->
                        assertRegisteredSetMatches
                          "combined test-suite other-modules must match discovered test modules"
                          discoveredModules
                          registeredTestModules
                          >>
                        assertRegisteredSetMatches
                          "Main imports must name the semantic test suite registry"
                          expectedMainImports
                          mainImportedTestModules
                          >>
                        assertRegisteredSetMatches
                          "semantic test suite registry imports must match default-suite modules that export tests"
                          defaultRunnableModules
                          suiteImportedTestModules
                          >>
                        assertRegisteredSetMatches
                          "RingBlastMain imports must match ring-blast modules that export tests"
                          ringBlastRunnableModules
                          ringBlastImportedTestModules
    ]

assertLibraryComponentExposure :: (FilePath, String, FilePath) -> IO ()
assertLibraryComponentExposure (cabalPath, libraryStanza, sourceDirectoryRelativePath) = do
  cabalFileResult <- resolvePackageFile packageMarker cabalPath
  sourceDirectoryResult <- resolvePackageDirectory packageMarker sourceDirectoryRelativePath
  case (cabalFileResult, sourceDirectoryResult) of
    (Left pathError, _) -> assertFailure (renderResourcePathError pathError)
    (_, Left pathError) -> assertFailure (renderResourcePathError pathError)
    (Right cabalFile, Right sourceDirectory) -> do
      cabalContents <- readFile cabalFile
      discoverModuleNamesFromSourceTree sourceDirectory
        >>= \sourceModules ->
          let registeredExposedModules =
                Set.fromList
                  ( cabalFieldEntries
                      "exposed-modules:"
                      (cabalStanzaBody libraryStanza (lines cabalContents))
                  )
              registeredOtherModules =
                Set.fromList
                  ( cabalFieldEntries
                      "other-modules:"
                      (cabalStanzaBody libraryStanza (lines cabalContents))
                  )
           in assertRegisteredSetMatches
                (cabalPath <> " exposed/other-modules must match source modules under " <> sourceDirectoryRelativePath)
                sourceModules
                (registeredExposedModules <> registeredOtherModules)

discoverModuleNamesFromSourceTree :: FilePath -> IO (Set.Set String)
discoverModuleNamesFromSourceTree sourceDirectory =
  discoverHaskellFiles sourceDirectory
    >>= pure
      . Set.fromList
      . fmap (moduleNameFromSourcePath sourceDirectory)

discoverHaskellFiles :: FilePath -> IO [FilePath]
discoverHaskellFiles directory =
  listDirectory directory
    >>= fmap concat
      . traverse
        ( \entryName ->
            let path = directory </> entryName
             in doesDirectoryExist path
                  >>= \isDirectory ->
                    if isDirectory
                      then discoverHaskellFiles path
                      else
                        pure
                          ( if takeExtension path == ".hs"
                              then [path]
                              else []
                          )
        )

moduleNameFromSourcePath :: FilePath -> FilePath -> String
moduleNameFromSourcePath sourceDirectory sourcePath =
  sourcePath
    & makeRelative sourceDirectory
    & dropExtension
    & splitDirectories
    & intercalate "."

moduleIdentityOrFail :: ModuleSurface -> Either String String
moduleIdentityOrFail moduleSurface =
  case moduleSurfaceIdentity moduleSurface of
    Nothing -> Left "encountered a module without a parseable module identity"
    Just moduleName -> Right moduleName

exportedTestModuleName :: ModuleSurface -> Maybe String
exportedTestModuleName moduleSurface =
  do
    moduleName <- moduleSurfaceIdentity moduleSurface
    if Set.member "tests" (moduleSurfaceExportedNames moduleSurface)
      then Just moduleName
      else Nothing

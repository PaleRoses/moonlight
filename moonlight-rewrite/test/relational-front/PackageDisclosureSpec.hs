module PackageDisclosureSpec
  ( tests,
  )
where

import Data.Char (isSpace)
import Data.Foldable (traverse_)
import Data.List (find, intercalate, isInfixOf, isPrefixOf)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
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
  )
import Moonlight.Pale.Test.Section.ResourcePath
  ( renderResourcePathError,
    resolvePackageDirectory,
    resolvePackageFile,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase)

data RegisteredModule = RegisteredModule
  { registeredModuleName :: !String,
    registeredModuleSurface :: !ModuleSurface
  }

data LibraryComponent = LibraryComponent
  { libraryComponentStanza :: !String,
    libraryComponentFacade :: !String,
    libraryComponentExposedModules :: !(Set String),
    libraryComponentRegisteredModules :: !(Set String),
    libraryComponentSourceModules :: ![RegisteredModule]
  }

data ModuleOwner = ModuleOwner
  { moduleOwnerStanza :: !String,
    moduleOwnerFacade :: !String
  }

tests :: TestTree
tests =
  testGroup
    "package disclosure"
    [ testCase "Cabal glues every source module into exactly one facade-owned component" $ do
        components <- loadLibraryComponents
        traverse_ assertComponentRegistration components
        assertUniqueModuleOwnership components,
      testCase "Internal modules and defining leaves do not escape their facades" $ do
        components <- loadLibraryComponents
        traverse_ assertNoInternalExposure components
        assertCrossComponentImportsUseFacades components,
      testCase "default facade does not export raw relational storage seams" $ do
        components <- loadLibraryComponents
        assertDefaultFacadeOmitsRawStorage components
    ]

packageMarker :: FilePath
packageMarker =
  "foundation/moonlight-rewrite/moonlight-rewrite.cabal"

cabalRelativePath :: FilePath
cabalRelativePath =
  "moonlight-rewrite.cabal"

loadLibraryComponents :: IO [LibraryComponent]
loadLibraryComponents = do
  cabalFileResult <- resolvePackageFile packageMarker cabalRelativePath
  cabalFile <- either (assertFailure . renderResourcePathError) pure cabalFileResult
  cabalContents <- readFile cabalFile
  traverse (loadLibraryComponent (lines cabalContents)) (libraryStanzaHeaders cabalContents)

loadLibraryComponent :: [String] -> String -> IO LibraryComponent
loadLibraryComponent cabalLines stanzaHeader = do
  let stanzaLines = cabalStanzaBody stanzaHeader cabalLines
      sourceDirectories = cabalFieldEntries "hs-source-dirs:" stanzaLines
      exposedModuleSet = Set.fromList (cabalFieldEntries "exposed-modules:" stanzaLines)
      otherModuleSet = Set.fromList (cabalFieldEntries "other-modules:" stanzaLines)
  facade <-
    case Set.toAscList exposedModuleSet of
      [facadeModule] -> pure facadeModule
      exposedModules ->
        assertFailure
          ( stanzaHeader
              <> " must expose exactly one facade, observed "
              <> show exposedModules
          )
  sourceModuleGroups <- traverse discoverSourceDirectoryModules sourceDirectories
  pure
    LibraryComponent
      { libraryComponentStanza = stanzaHeader,
        libraryComponentFacade = facade,
        libraryComponentExposedModules = exposedModuleSet,
        libraryComponentRegisteredModules = exposedModuleSet <> otherModuleSet,
        libraryComponentSourceModules = concat sourceModuleGroups
      }

discoverSourceDirectoryModules :: FilePath -> IO [RegisteredModule]
discoverSourceDirectoryModules relativeDirectory = do
  sourceDirectoryResult <- resolvePackageDirectory packageMarker relativeDirectory
  sourceDirectory <- either (assertFailure . renderResourcePathError) pure sourceDirectoryResult
  moduleSurfacesResult <- discoverModuleSurfaces sourceDirectory
  moduleSurfaces <-
    either
      (assertFailure . ("failed to parse package source modules:\n" <>) . intercalate "\n")
      pure
      moduleSurfacesResult
  traverse registeredModuleFromSurface moduleSurfaces

registeredModuleFromSurface :: ModuleSurface -> IO RegisteredModule
registeredModuleFromSurface moduleSurface =
  case moduleSurfaceIdentity moduleSurface of
    Nothing ->
      assertFailure "encountered a package source module without a parseable identity"
    Just moduleName ->
      pure
        RegisteredModule
          { registeredModuleName = moduleName,
            registeredModuleSurface = moduleSurface
          }

assertComponentRegistration :: LibraryComponent -> Assertion
assertComponentRegistration component =
  assertRegisteredSetMatches
    (libraryComponentStanza component <> " source modules must equal exposed-modules plus other-modules")
    (Set.fromList (fmap registeredModuleName (libraryComponentSourceModules component)))
    (libraryComponentRegisteredModules component)

assertUniqueModuleOwnership :: [LibraryComponent] -> Assertion
assertUniqueModuleOwnership components =
  let ownershipCounts =
        Map.fromListWith (+)
          [ (registeredModuleName sourceModule, 1 :: Int)
            | component <- components,
              sourceModule <- libraryComponentSourceModules component
          ]
      duplicateOwners = Map.keys (Map.filter (/= 1) ownershipCounts)
   in if null duplicateOwners
        then pure ()
        else
          assertFailure
            ( "source modules registered in more than one library component: "
                <> show duplicateOwners
            )

assertNoInternalExposure :: LibraryComponent -> Assertion
assertNoInternalExposure component =
  let internalExposures =
        Set.filter (".Internal" `isInfixOf`) (libraryComponentExposedModules component)
   in if Set.null internalExposures
        then pure ()
        else
          assertFailure
            ( libraryComponentStanza component
                <> " exposes Internal modules: "
                <> show (Set.toAscList internalExposures)
            )

assertCrossComponentImportsUseFacades :: [LibraryComponent] -> Assertion
assertCrossComponentImportsUseFacades components =
  let owners = moduleOwners components
      violations = foldMap (componentImportViolations owners) components
   in if null violations
        then pure ()
        else
          assertFailure
            ( "cross-component imports must name the owning facade:\n"
                <> intercalate "\n" violations
            )

moduleOwners :: [LibraryComponent] -> Map String ModuleOwner
moduleOwners components =
  Map.fromList
    [ ( registeredModuleName sourceModule,
        ModuleOwner
          { moduleOwnerStanza = libraryComponentStanza component,
            moduleOwnerFacade = libraryComponentFacade component
          }
      )
      | component <- components,
        sourceModule <- libraryComponentSourceModules component
    ]

componentImportViolations :: Map String ModuleOwner -> LibraryComponent -> [String]
componentImportViolations owners component =
  foldMap (moduleImportViolations owners component) (libraryComponentSourceModules component)

moduleImportViolations :: Map String ModuleOwner -> LibraryComponent -> RegisteredModule -> [String]
moduleImportViolations owners component sourceModule =
  foldMap importedModuleViolation (Set.toAscList importedModules)
  where
    importedModules = moduleSurfaceImportedNames (registeredModuleSurface sourceModule)

    importedModuleViolation :: String -> [String]
    importedModuleViolation importedModule =
      case Map.lookup importedModule owners of
        Just owner
          | moduleOwnerStanza owner /= libraryComponentStanza component,
            importedModule /= moduleOwnerFacade owner ->
              [ registeredModuleName sourceModule
                  <> " imports "
                  <> importedModule
                  <> " instead of "
                  <> moduleOwnerFacade owner
              ]
        _ -> []

assertDefaultFacadeOmitsRawStorage :: [LibraryComponent] -> Assertion
assertDefaultFacadeOmitsRawStorage components =
  case find ((== defaultFacadeModule) . libraryComponentFacade) components of
    Nothing ->
      assertFailure "default Moonlight.Rewrite facade component is absent"
    Just defaultComponent ->
      case find ((== defaultFacadeModule) . registeredModuleName) (libraryComponentSourceModules defaultComponent) of
        Nothing ->
          assertFailure "default Moonlight.Rewrite facade source is absent"
        Just defaultFacade ->
          let leakedNames =
                Set.intersection
                  rawRelationalStorageNames
                  (moduleSurfaceExportedNames (registeredModuleSurface defaultFacade))
           in if Set.null leakedNames
                then pure ()
                else
                  assertFailure
                    ( "default facade exports raw relational storage names: "
                        <> show (Set.toAscList leakedNames)
                    )

defaultFacadeModule :: String
defaultFacadeModule =
  "Moonlight.Rewrite"

rawRelationalStorageNames :: Set String
rawRelationalStorageNames =
  Set.fromList
    [ "AtomRelation",
      "MatchKey",
      "matchKeyFromInts",
      "hostFromRelations"
    ]

libraryStanzaHeaders :: String -> [String]
libraryStanzaHeaders =
  fmap trim
    . filter isLibraryStanzaHeader
    . lines

isLibraryStanzaHeader :: String -> Bool
isLibraryStanzaHeader sourceLine =
  let strippedLine = trim sourceLine
   in strippedLine == "library"
        || "library " `isPrefixOf` strippedLine

trim :: String -> String
trim =
  reverse . dropWhile isSpace . reverse . dropWhile isSpace

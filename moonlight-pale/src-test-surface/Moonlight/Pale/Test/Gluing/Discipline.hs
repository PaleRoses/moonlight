module Moonlight.Pale.Test.Gluing.Discipline
  ( SheafManifest (..),
    assertSheafDiscipline,
  )
where

import Data.Kind (Type)
import Data.Function ((&))
import Data.List (intercalate, isPrefixOf)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Pale.Test.Gluing.Registry
  ( assertRegisteredSetMatches,
    discoverModuleSurfaces,
    moduleSurfaceIdentity,
    moduleSurfaceImportedNames,
  )
import Moonlight.Pale.Test.Section.ResourcePath
  ( renderResourcePathError,
    resolvePackageDirectory,
  )
import Test.Tasty.HUnit (Assertion, assertFailure)

type SheafManifest :: Type
data SheafManifest = SheafManifest
  { sheafModulePrefix :: String,
    sheafAllowedImports :: Map String (Set String)
  }

assertSheafDiscipline :: FilePath -> FilePath -> SheafManifest -> Assertion
assertSheafDiscipline packageMarker relativeDirectory sheafManifest =
  let modulePrefix = sheafModulePrefix sheafManifest
      allowedImports = sheafAllowedImports sheafManifest
   in
    resolvePackageDirectory packageMarker relativeDirectory
      >>= either
        (assertFailure . renderResourcePathError)
        (\packageDirectory -> do
            discoveredImports <- discoverPrefixedModuleImports packageDirectory modulePrefix
            assertRegisteredSetMatches
              "discovered sheaf modules must match the declared layer registry"
              (Map.keysSet allowedImports)
              (Map.keysSet discoveredImports)
            let violations =
                  Map.toAscList discoveredImports
                    >>= \(moduleName, importedModules) ->
                      let allowedModules = Map.findWithDefault Set.empty moduleName allowedImports
                          forbiddenModules =
                            importedModules
                              & flip Set.difference allowedModules
                              & Set.toAscList
                       in
                        if null forbiddenModules
                          then []
                          else
                            [ moduleName
                                <> " imports forbidden local modules "
                                <> show forbiddenModules
                                <> "; imported local modules = "
                                <> show (Set.toAscList importedModules)
                                <> "; allowed local modules = "
                                <> show (Set.toAscList allowedModules)
                            ]
            if null violations
              then pure ()
              else assertFailure (intercalate "\n" violations)
        )

discoverPrefixedModuleImports :: FilePath -> String -> IO (Map String (Set String))
discoverPrefixedModuleImports packageDirectory modulePrefix =
  discoverModuleSurfaces packageDirectory
    >>= either
      (assertFailure . intercalate "\n")
      ( pure
          . Map.fromList
          . mapMaybe
            ( \moduleSurface ->
                moduleSurfaceIdentity moduleSurface
                  >>= \moduleName ->
                    if isPrefixOf modulePrefix moduleName
                      then
                        Just
                          ( moduleName,
                            moduleSurfaceImportedNames moduleSurface
                              & localModuleImports modulePrefix
                          )
                      else Nothing
            )
      )

localModuleImports :: String -> Set String -> Set String
localModuleImports modulePrefix =
  Set.filter (isPrefixOf modulePrefix)

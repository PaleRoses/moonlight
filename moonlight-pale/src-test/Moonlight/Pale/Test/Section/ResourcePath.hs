{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Pale.Test.Section.ResourcePath
  ( ResourcePathError (..),
    renderResourcePathError,
    resolveCompilerRoot,
    findActiveCabalBuildDirectory,
    resolvePackageRoot,
    resolveCompilerFile,
    resolveCompilerDirectory,
    resolvePackageFile,
    resolvePackageDirectory,
  )
where

import Data.Kind (Type)
import Data.List (find, unfoldr)
import Data.Maybe (catMaybes, listToMaybe)
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory)
import System.Environment (getExecutablePath, lookupEnv)
import System.FilePath ((</>), normalise, takeDirectory)

type ResourcePathError :: Type
data ResourcePathError
  = CompilerRootNotFound FilePath
  | MissingResourceFile FilePath
  | MissingResourceDirectory FilePath
  deriving stock (Eq, Show)

renderResourcePathError :: ResourcePathError -> String
renderResourcePathError resourcePathError =
  case resourcePathError of
    CompilerRootNotFound packageMarker ->
      "unable to locate compiler root with cabal.project and marker: " <> packageMarker
    MissingResourceFile filePath ->
      "missing resource file: " <> filePath
    MissingResourceDirectory directoryPath ->
      "missing resource directory: " <> directoryPath

resolveCompilerRoot :: FilePath -> IO (Either ResourcePathError FilePath)
resolveCompilerRoot packageMarker = do
  currentDirectory <- getCurrentDirectory
  executableDirectory <- takeDirectory <$> getExecutablePath
  maybeCompilerRoot <-
    findAnyCompilerRoot
      packageMarker
      [currentDirectory, executableDirectory]
  pure
    ( case maybeCompilerRoot of
        Nothing -> Left (CompilerRootNotFound packageMarker)
        Just compilerRoot -> Right compilerRoot
    )

resolvePackageRoot :: FilePath -> IO (Either ResourcePathError FilePath)
resolvePackageRoot packageMarker =
  resolveCompilerRoot packageMarker
    >>= pure
      . fmap (\compilerRoot -> normalise (compilerRoot </> takeDirectory packageMarker))

resolveCompilerFile :: FilePath -> FilePath -> IO (Either ResourcePathError FilePath)
resolveCompilerFile =
  resolveExistingPath resolveCompilerRoot doesFileExist MissingResourceFile

resolveCompilerDirectory :: FilePath -> FilePath -> IO (Either ResourcePathError FilePath)
resolveCompilerDirectory =
  resolveExistingPath resolveCompilerRoot doesDirectoryExist MissingResourceDirectory

resolvePackageFile :: FilePath -> FilePath -> IO (Either ResourcePathError FilePath)
resolvePackageFile =
  resolveExistingPath resolvePackageRoot doesFileExist MissingResourceFile

resolvePackageDirectory :: FilePath -> FilePath -> IO (Either ResourcePathError FilePath)
resolvePackageDirectory =
  resolveExistingPath resolvePackageRoot doesDirectoryExist MissingResourceDirectory

resolveExistingPath ::
  (FilePath -> IO (Either ResourcePathError FilePath)) ->
  (FilePath -> IO Bool) ->
  (FilePath -> ResourcePathError) ->
  FilePath ->
  FilePath ->
  IO (Either ResourcePathError FilePath)
resolveExistingPath resolveRoot pathExists toMissingError packageMarker relativePath = do
  rootResult <- resolveRoot packageMarker
  case rootResult of
    Left rootError -> pure (Left rootError)
    Right rootPath -> do
      let resolvedPath = normalise (rootPath </> relativePath)
      pathPresent <- pathExists resolvedPath
      pure
        ( if pathPresent
            then Right resolvedPath
            else Left (toMissingError resolvedPath)
        )

findActiveCabalBuildDirectory :: IO (Maybe FilePath)
findActiveCabalBuildDirectory = do
  maybeComponentBuildDirectory <- lookupEnv "HASKELL_DIST_DIR"
  executableDirectory <- takeDirectory <$> getExecutablePath
  findAnyAncestorDirectory
    hasCabalBuildPlan
    (catMaybes [maybeComponentBuildDirectory, Just executableDirectory])

findAncestorDirectory :: (FilePath -> IO Bool) -> FilePath -> IO (Maybe FilePath)
findAncestorDirectory hasMarker directoryPath = do
  let directories = ancestorDirectories directoryPath
  markerPresence <- traverse hasMarker directories
  pure (fst <$> find snd (zip directories markerPresence))

ancestorDirectories :: FilePath -> [FilePath]
ancestorDirectories directoryPath =
  unfoldr nextDirectory (Just directoryPath)
  where
    nextDirectory :: Maybe FilePath -> Maybe (FilePath, Maybe FilePath)
    nextDirectory maybeDirectory = do
      currentDirectory <- maybeDirectory
      let parentDirectory = takeDirectory currentDirectory
      pure
        ( currentDirectory,
          if parentDirectory == currentDirectory
            then Nothing
            else Just parentDirectory
        )

findAnyCompilerRoot :: FilePath -> [FilePath] -> IO (Maybe FilePath)
findAnyCompilerRoot packageMarker =
  findAnyAncestorDirectory (hasCompilerRootMarkers packageMarker)

findAnyAncestorDirectory :: (FilePath -> IO Bool) -> [FilePath] -> IO (Maybe FilePath)
findAnyAncestorDirectory hasMarker =
  fmap (listToMaybe . catMaybes)
    . traverse (findAncestorDirectory hasMarker)

hasCompilerRootMarkers :: FilePath -> FilePath -> IO Bool
hasCompilerRootMarkers packageMarker directoryPath = do
  hasProject <- doesFileExist (directoryPath </> "cabal.project")
  hasPackage <- doesFileExist (directoryPath </> packageMarker)
  pure (hasProject && hasPackage)

hasCabalBuildPlan :: FilePath -> IO Bool
hasCabalBuildPlan directoryPath =
  doesFileExist (directoryPath </> "cache" </> "plan.json")

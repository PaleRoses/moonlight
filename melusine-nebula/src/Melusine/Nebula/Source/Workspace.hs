{-# LANGUAGE LambdaCase #-}

module Melusine.Nebula.Source.Workspace
  ( haskellSourcePath,
    enumerateModuleWorkloads,
  )
where

import Control.Exception (IOException, try)
import Data.Char (toLower)
import Data.Either (partitionEithers)
import Data.List (isPrefixOf, sort)
import Melusine.Nebula.Core (ModuleWorkload (..), NebulaError (..))
import Moonlight.Pale.Ghc.Hie.Read (HieReadError (..), indexHieRoots)
import Moonlight.Pale.Ghc.Hie.SourceKey (HieOracleIndex, OracleQuery (..), lookupModuleOracle)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, makeAbsolute)
import System.FilePath (normalise, takeExtension, (</>))
import System.IO (readFile')

haskellSourcePath :: FilePath -> Bool
haskellSourcePath =
  (== ".hs") . fmap toLower . takeExtension

enumerateModuleWorkloads :: [FilePath] -> [FilePath] -> IO ([NebulaError], [ModuleWorkload])
enumerateModuleWorkloads roots hieRoots = do
  (hieErrors, oracleIndex) <- indexHieRoots hieRoots
  absoluteRoots <- traverse makeAbsolute roots
  let sourceRoots = roots <> absoluteRoots
  channels <- traverse (workloadsUnderRoot oracleIndex sourceRoots) roots
  let (errorChannels, workloadChannels) = unzip channels
  pure (fmap hieReadError hieErrors <> concat errorChannels, concat workloadChannels)

workloadsUnderRoot :: HieOracleIndex -> [FilePath] -> FilePath -> IO ([NebulaError], [ModuleWorkload])
workloadsUnderRoot oracleIndex sourceRoots root = do
  isDirectory <- doesDirectoryExist root
  isFile <- doesFileExist root
  if isDirectory
    then do
      walked <- try (collectHaskellSources root)
      case walked of
        Left walkFailure ->
          pure ([workspaceError root walkFailure], [])
        Right sourcePaths ->
          partitionEithers <$> traverse (readWorkload oracleIndex sourceRoots) (sort sourcePaths)
    else
      if isFile
        then partitionEithers . pure <$> readWorkload oracleIndex sourceRoots root
        else pure ([NebulaWorkspaceError root "no such file or directory"], [])

collectHaskellSources :: FilePath -> IO [FilePath]
collectHaskellSources directory = do
  entries <- listDirectory directory
  let visiblePaths =
        fmap (directory </>) (filter (not . isPrefixOf ".") entries)
  concat <$> traverse expandEntry visiblePaths

expandEntry :: FilePath -> IO [FilePath]
expandEntry path = do
  isDirectory <- doesDirectoryExist path
  if isDirectory
    then collectHaskellSources path
    else pure [path | haskellSourcePath path]

readWorkload :: HieOracleIndex -> [FilePath] -> FilePath -> IO (Either NebulaError ModuleWorkload)
readWorkload oracleIndex sourceRoots path = do
  readResult <- try (readFile' path)
  absolutePath <- tryMakeAbsolute path
  pure $
    case readResult of
      Left readFailure ->
        Left (workspaceError path readFailure)
      Right sourceText ->
        Right
          ModuleWorkload
            { mwPath = path,
              mwSource = sourceText,
              mwOracleLookup =
                lookupModuleOracle
                  oracleIndex
                  OracleQuery
                    { oqGivenPath = normalise path,
                      oqAbsolutePath = absolutePath,
                      oqSourceRoots = sourceRoots
                    }
            }

tryMakeAbsolute :: FilePath -> IO (Maybe FilePath)
tryMakeAbsolute path = do
  absoluteAttempt <- try (makeAbsolute path)
  pure (either (const Nothing) (Just . normalise) (absoluteAttempt :: Either IOException FilePath))

workspaceError :: FilePath -> IOException -> NebulaError
workspaceError path =
  NebulaWorkspaceError path . show

hieReadError :: HieReadError -> NebulaError
hieReadError = \case
  HieReadError path message ->
    NebulaWorkspaceError path message
  HieRootError path message ->
    NebulaWorkspaceError path message

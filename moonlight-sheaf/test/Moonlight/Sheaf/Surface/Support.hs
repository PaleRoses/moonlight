{-# LANGUAGE ScopedTypeVariables #-}

module Moonlight.Sheaf.Surface.Support
  ( ExportSurfaceLock (..),
    assertExportSurfaceLocked,
    exportEntries,
    showSet,
  )
where

import Control.Exception (IOException, try)
import Data.Char (isSpace)
import Data.List (dropWhileEnd, intercalate, isPrefixOf)
import Data.Set (Set)
import Data.Set qualified as Set
import System.Directory (getCurrentDirectory)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure)

data ExportSurfaceLock = ExportSurfaceLock
  { eslLabel :: String,
    eslBarrelPrefix :: String,
    eslSourceCandidates :: [FilePath],
    eslExpectedExports :: Set String,
    eslForbiddenExport :: String -> Bool
  }

assertExportSurfaceLocked :: ExportSurfaceLock -> Assertion
assertExportSurfaceLocked lock = do
  (_sourcePath, sourceText) <- readSurfaceSource lock
  let exports = exportEntries sourceText
      barrelExports = Set.filter (eslBarrelPrefix lock `isPrefixOf`) exports
      forbiddenExports = Set.filter (eslForbiddenExport lock) exports
  assertBool
    (eslLabel lock <> " must not re-export barrel modules: " <> showSet barrelExports)
    (Set.null barrelExports)
  assertBool
    (eslLabel lock <> " leaked forbidden exports: " <> showSet forbiddenExports)
    (Set.null forbiddenExports)
  assertEqual
    (eslLabel lock <> " export whitelist")
    (eslExpectedExports lock)
    exports

readSurfaceSource :: ExportSurfaceLock -> IO (FilePath, String)
readSurfaceSource lock =
  getCurrentDirectory >>= \cwd ->
    firstReadable (eslSourceCandidates lock)
      >>= maybe
        (assertFailure ("could not locate " <> eslLabel lock <> " source from " <> cwd))
        pure

firstReadable :: [FilePath] -> IO (Maybe (FilePath, String))
firstReadable =
  foldr readCandidate (pure Nothing)
  where
    readCandidate sourcePath fallback =
      try (readFile sourcePath) >>= \result ->
        case result of
          Right sourceText ->
            pure (Just (sourcePath, sourceText))
          Left (_failure :: IOException) ->
            fallback

exportEntries :: String -> Set String
exportEntries =
  Set.fromList
    . mapMaybeExport normalizeExportLine
    . takeWhile ((/= "where") . trim)
    . drop 1
    . dropWhile (not . ("module " `isPrefixOf`) . trim)
    . lines

mapMaybeExport :: (line -> Maybe value) -> [line] -> [value]
mapMaybeExport select =
  foldr
    ( \line selected ->
        case select line of
          Just value -> value : selected
          Nothing -> selected
    )
    []

normalizeExportLine :: String -> Maybe String
normalizeExportLine line =
  case dropTrailingComma (dropLeadingParen (trim line)) of
    "" -> Nothing
    ")" -> Nothing
    exportName -> Just exportName

dropLeadingParen :: String -> String
dropLeadingParen ('(' : rest) = trim rest
dropLeadingParen value = value

dropTrailingComma :: String -> String
dropTrailingComma =
  trim . reverse . dropLeadingComma . reverse
  where
    dropLeadingComma (',' : rest) = rest
    dropLeadingComma value = value

trim :: String -> String
trim =
  dropWhileEnd isSpace . dropWhile isSpace

showSet :: Set String -> String
showSet =
  intercalate ", " . Set.toAscList

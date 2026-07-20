module Moonlight.Pale.Test.Gluing.Registry
  ( ModuleSurface,
    assertRegisteredSetMatches,
    cabalFieldEntries,
    cabalStanzaBody,
    discoverParsedHaskellFiles,
    discoverParsedHaskellFilesWithExcludes,
    discoverModuleSurfaces,
    moduleSurfaceExportedNames,
    moduleSurfaceIdentity,
    moduleSurfaceImportedNames,
    parseModuleSurfaceFile,
    readModuleSurfaceFile,
  )
where

import Data.Char (isSpace)
import Data.Function ((&))
import Data.List (intercalate, isPrefixOf, isSuffixOf)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Pale.Ghc.ModuleSurface
  ( ModuleSurface (..),
    parseHsModule,
    moduleSurfaceFromGhcPs,
    unParsedModuleName,
    unParsedName,
  )
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import Test.Tasty.HUnit (Assertion, assertFailure)

assertRegisteredSetMatches :: String -> Set String -> Set String -> Assertion
assertRegisteredSetMatches label expectedEntries registeredEntries =
  let missingEntries = Set.toAscList (Set.difference expectedEntries registeredEntries)
      unexpectedEntries = Set.toAscList (Set.difference registeredEntries expectedEntries)
   in
    if Set.null (Set.difference expectedEntries registeredEntries)
        && Set.null (Set.difference registeredEntries expectedEntries)
      then pure ()
      else
        assertFailure
          ( intercalate
              "\n"
              [ label,
                "missing: " <> show missingEntries,
                "unexpected: " <> show unexpectedEntries,
                "expected: " <> show (Set.toAscList expectedEntries),
                "registered: " <> show (Set.toAscList registeredEntries)
              ]
          )

cabalStanzaBody :: String -> [String] -> [String]
cabalStanzaBody stanzaHeader cabalLines =
  case dropWhile ((/= stanzaHeader) . trim) cabalLines of
    [] -> []
    (_ : remainingLines) -> takeWhile isStanzaLine remainingLines

cabalFieldEntries :: String -> [String] -> [String]
cabalFieldEntries fieldHeader stanzaLines =
  case dropWhile ((/= fieldHeader) . trimStart) stanzaLines of
    [] -> []
    (headerLine : remainingLines) ->
      let headerIndent = indentation headerLine
       in remainingLines
            & takeWhile ((> headerIndent) . indentation)
            & fmap trim
            & filter (not . null)

discoverModuleSurfaces :: FilePath -> IO (Either [String] [ModuleSurface])
discoverModuleSurfaces rootDirectory =
  discoverParsedHaskellFiles parseHsModule rootDirectory
    >>= pure
      . fmap
        ( fmap
            ( \(_, moduleAst) ->
                moduleSurfaceFromGhcPs moduleAst
            )
        )

parseModuleSurfaceFile :: FilePath -> IO (Either String ModuleSurface)
parseModuleSurfaceFile sourcePath =
  readFile sourcePath
    >>= pure
      . fmap moduleSurfaceFromGhcPs
      . parseHsModule sourcePath

readModuleSurfaceFile :: FilePath -> IO (Either String ModuleSurface)
readModuleSurfaceFile =
  parseModuleSurfaceFile

discoverParsedHaskellFiles :: (FilePath -> String -> Either String value) -> FilePath -> IO (Either [String] [(FilePath, value)])
discoverParsedHaskellFiles parser rootDirectory =
  discoverParsedHaskellFilesWithExcludes [] parser rootDirectory

discoverParsedHaskellFilesWithExcludes :: [FilePath] -> (FilePath -> String -> Either String value) -> FilePath -> IO (Either [String] [(FilePath, value)])
discoverParsedHaskellFilesWithExcludes excludedDirectoryNames parser rootDirectory =
  haskellModuleFilesWithExcludes excludedDirectoryNames rootDirectory
    >>= traverse
      ( \sourcePath ->
          readFile sourcePath
            >>= pure
              . fmap (\parsedValue -> (sourcePath, parsedValue))
              . parser sourcePath
      )
    >>= pure . collectParseResults

moduleSurfaceIdentity :: ModuleSurface -> Maybe String
moduleSurfaceIdentity moduleSurface =
  surfaceModuleName moduleSurface
    >>= pure . unParsedModuleName

moduleSurfaceImportedNames :: ModuleSurface -> Set String
moduleSurfaceImportedNames moduleSurface =
  surfaceImportedModules moduleSurface
    & Set.map unParsedModuleName

moduleSurfaceExportedNames :: ModuleSurface -> Set String
moduleSurfaceExportedNames moduleSurface =
  surfaceExportedNames moduleSurface
    & Set.map unParsedName

isStanzaLine :: String -> Bool
isStanzaLine line =
  null (trim line) || indentation line > 0

indentation :: String -> Int
indentation =
  length . takeWhile isSpace

trimStart :: String -> String
trimStart =
  dropWhile isSpace

trim :: String -> String
trim =
  reverse . dropWhile isSpace . reverse . dropWhile isSpace

haskellModuleFilesWithExcludes :: [FilePath] -> FilePath -> IO [FilePath]
haskellModuleFilesWithExcludes excludedDirectoryNames rootDirectory =
  listDirectory rootDirectory
    >>= traverse
      ( \entryName ->
          let entryPath = rootDirectory </> entryName
           in doesDirectoryExist entryPath
                >>= ( \isDirectory ->
                        if isDirectory
                          then
                            if isExcludedDirectory excludedDirectoryNames entryName
                              then pure []
                              else haskellModuleFilesWithExcludes excludedDirectoryNames entryPath
                          else
                            pure
                              ( if isSuffixOf ".hs" entryPath
                                  then [entryPath]
                                  else []
                              )
                    )
      )
    >>= pure . concat

collectParseResults :: [Either String value] -> Either [String] [value]
collectParseResults parseResults =
  let parseErrors =
        parseResults
          & foldr
            ( \parseResult ->
                case parseResult of
                  Left errorMessage -> (errorMessage :)
                  Right _ -> id
            )
            []
      parsedValues =
        parseResults
          & mapMaybe
            ( \parseResult ->
                case parseResult of
                  Left _ -> Nothing
                  Right parsedValue -> Just parsedValue
            )
   in if null parseErrors
        then Right parsedValues
        else Left parseErrors

isExcludedDirectory :: [FilePath] -> FilePath -> Bool
isExcludedDirectory excludedDirectoryNames entryName =
  any (\excludedDirectoryName -> entryName == excludedDirectoryName || excludedDirectoryName `isPrefixOf` entryName) excludedDirectoryNames

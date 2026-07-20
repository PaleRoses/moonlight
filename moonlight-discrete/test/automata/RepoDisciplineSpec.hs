module RepoDisciplineSpec
  ( tests,
  )
where

import Data.Char (isSpace)
import Data.List (find, intercalate, isPrefixOf)
import Data.Maybe (maybeToList)
import Moonlight.Pale.Test.Gluing.Registry (discoverParsedHaskellFilesWithExcludes)
import Moonlight.Pale.Test.Section.ResourcePath
  ( renderResourcePathError,
    resolveCompilerRoot,
  )
import System.FilePath ((</>), addTrailingPathSeparator, makeRelative, normalise)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "repo-discipline"
    [ testCase "standard recursion schemes are imported, not reimplemented" assertNoLocalRecursionReinvention,
      testCase "compdata is not an automata dependency or import" assertNoCompdataImports
    ]

assertNoLocalRecursionReinvention :: IO ()
assertNoLocalRecursionReinvention =
  withAutomataCompilerRoot checkNoLocalRecursionReinvention

assertNoCompdataImports :: IO ()
assertNoCompdataImports =
  withAutomataCompilerRoot checkNoCompdataImports

withAutomataCompilerRoot :: (FilePath -> IO ()) -> IO ()
withAutomataCompilerRoot check =
  resolveCompilerRoot automataCabalPath
    >>= either (assertFailure . renderResourcePathError) check

withParsedCompilerSources :: FilePath -> ([(FilePath, String)] -> IO ()) -> IO ()
withParsedCompilerSources compilerRoot consumeSources =
  discoverParsedHaskellFilesWithExcludes excludedDirectoryNames (\_ sourceText -> Right sourceText) compilerRoot
    >>= either (assertFailure . intercalate "\n") consumeSources

checkNoLocalRecursionReinvention :: FilePath -> IO ()
checkNoLocalRecursionReinvention compilerRoot =
  withParsedCompilerSources compilerRoot
    (assertNoRenderedViolations "found local recursion reinvention:" compilerRoot . collectRecursionReinventionViolations compilerRoot)

checkNoCompdataImports :: FilePath -> IO ()
checkNoCompdataImports compilerRoot =
  withParsedCompilerSources compilerRoot
    ( \haskellSources ->
        readFile (compilerRoot </> automataCabalPath)
          >>= assertNoRenderedViolations
            "found compdata dependency/import inside moonlight-discrete:automata:"
            compilerRoot
            . (collectCompdataViolations compilerRoot haskellSources <>)
            . extractCabalCompdataViolations (compilerRoot </> automataCabalPath)
      )

assertNoRenderedViolations :: String -> FilePath -> [String] -> IO ()
assertNoRenderedViolations header compilerRoot violations =
  if null violations
    then pure ()
    else
      assertFailure
        ( intercalate
            "\n"
            ( header
                : fmap (renderViolation compilerRoot) violations
            )
        )

collectRecursionReinventionViolations :: FilePath -> [(FilePath, String)] -> [String]
collectRecursionReinventionViolations compilerRoot =
  foldMap
    ( \(sourcePath, sourceText) ->
        if isAutomataPackageSource compilerRoot sourcePath
          then extractViolations sourcePath sourceText
          else []
    )

collectCompdataViolations :: FilePath -> [(FilePath, String)] -> [String]
collectCompdataViolations compilerRoot =
  foldMap
    ( \(sourcePath, sourceText) ->
        if isAutomataPackageFile compilerRoot sourcePath
          then extractCompdataImportViolations sourcePath sourceText
          else []
    )

extractViolations :: FilePath -> String -> [String]
extractViolations =
  extractPrefixedLineViolations forbiddenPrefixes

extractCompdataImportViolations :: FilePath -> String -> [String]
extractCompdataImportViolations =
  extractPrefixedLineViolations compdataImportPrefixes

extractCabalCompdataViolations :: FilePath -> String -> [String]
extractCabalCompdataViolations =
  extractPrefixedLineViolations compdataDependencyPrefixes

extractPrefixedLineViolations :: [String] -> FilePath -> String -> [String]
extractPrefixedLineViolations prefixes sourcePath sourceText =
  zip ([1 ..] :: [Int]) (lines sourceText)
    >>= \(lineNumber, sourceLine) ->
      maybeToList
        ( fmap
            (\forbiddenPrefix -> sourcePath <> ":" <> show lineNumber <> ": " <> forbiddenPrefix)
            (matchingLinePrefix sourceLine prefixes)
        )

matchingLinePrefix :: String -> [String] -> Maybe String
matchingLinePrefix sourceLine =
  find (`isPrefixOf` dropWhile isSpace sourceLine)

renderViolation :: FilePath -> String -> String
renderViolation compilerRoot violation =
  case break (== ':') violation of
    (sourcePath, remainder) ->
      normalise (makeRelative compilerRoot sourcePath) <> remainder

isAutomataPackageSource :: FilePath -> FilePath -> Bool
isAutomataPackageSource compilerRoot =
  isPathUnder
    ( compilerRoot
        </> "foundation/moonlight-discrete/src-automata/Moonlight/Automata"
    )

isAutomataPackageFile :: FilePath -> FilePath -> Bool
isAutomataPackageFile compilerRoot =
  isPathUnder
    ( compilerRoot
        </> "foundation/moonlight-discrete"
    )

isPathUnder :: FilePath -> FilePath -> Bool
isPathUnder ancestorPath sourcePath =
  let ancestor =
        normalise ancestorPath
      descendant =
        normalise sourcePath
   in descendant == ancestor
        || addTrailingPathSeparator ancestor `isPrefixOf` descendant

excludedDirectoryNames :: [FilePath]
excludedDirectoryNames =
  [ ".git",
    "dist",
    "build",
    "target",
    "node_modules",
    "generated"
  ]

automataCabalPath :: FilePath
automataCabalPath =
  "foundation/moonlight-discrete/moonlight-discrete.cabal"

forbiddenPrefixes :: [String]
forbiddenPrefixes =
  [ "newtype Fix",
    "cata ::",
    "cata =",
    "para ::",
    "para =",
    "ana ::",
    "ana =",
    "hylo ::",
    "hylo =",
    "anaM ::",
    "anaM =",
    "cataM ::",
    "cataM =",
    "paraM ::",
    "paraM =",
    "hyloM ::",
    "hyloM ="
  ]

compdataImportPrefixes :: [String]
compdataImportPrefixes =
  [ "import Data.Comp",
    "import qualified Data.Comp"
  ]

compdataDependencyPrefixes :: [String]
compdataDependencyPrefixes =
  [ ", compdata",
    "compdata",
    ", compdata-automata",
    "compdata-automata",
    ", projection",
    "projection"
  ]

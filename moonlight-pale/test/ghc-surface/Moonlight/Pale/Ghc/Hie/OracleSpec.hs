module Moonlight.Pale.Ghc.Hie.OracleSpec (tests) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Pale.Ghc.Hie.Oracle (ModuleNameOracle (..), ResolvedOrigin, mkResolvedOrigin, occResolvesUniquely)
import Moonlight.Pale.Ghc.Hie.Read (indexHieRoots)
import Moonlight.Pale.Ghc.Hie.SourceKey
  ( HieSourceKeyKind (..),
    OracleLookup (..),
    OracleQuery (..),
    TriedKey (..),
    buildHieOracleIndex,
    lookupModuleOracle,
    oracleLookupOracle,
  )
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly)
import System.Exit (ExitCode (..))
import System.FilePath (normalise, (</>))
import System.Process (readProcessWithExitCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "pale.hie.oracle"
    [ testCase "ghc hie resolves map and composition into accepted base origins" $ do
        oracle <- compileAndReadOracle "OracleFixture" oracleFixtureSource
        acceptedMapOrigins <- acceptedOriginsFor "map"
        acceptedComposeOrigins <- acceptedOriginsFor "."
        assertBool "map resolves through the base registry" (occResolvesUniquely oracle "map" acceptedMapOrigins)
        assertBool "composition resolves through the base registry" (occResolvesUniquely oracle "." acceptedComposeOrigins)
        assertBool "hie evidence variables are decoded into span-indexed evidence" (not (Map.null (mnoEvidenceAtSpan oracle)))
        assertBool "hie type table is flattened into span-indexed oracle words" (not (Map.null (mnoTypeAtSpan oracle))),
      testCase "a user-defined composition operator is not accepted as base composition" $ do
        oracle <- compileAndReadOracle "ShadowFixture" shadowFixtureSource
        acceptedComposeOrigins <- acceptedOriginsFor "."
        assertBool "shadowed composition does not satisfy the base registry" (not (occResolvesUniquely oracle "." acceptedComposeOrigins)),
      testCase "source-key lookup uses suffix fallback without guessing through ambiguities" $ do
        let oracle = emptyOracle "src/Foo/Bar.hs"
            oracleIndex = buildHieOracleIndex [oracle]
            lookupResult =
              lookupModuleOracle
                oracleIndex
                OracleQuery
                  { oqGivenPath = "compiler/foundation/demo/src/Foo/Bar.hs",
                    oqAbsolutePath = Nothing,
                    oqSourceRoots = []
                  }
        case lookupResult of
          OracleFound ModuleSuffixKey "src/Foo/Bar.hs" _ ->
            pure ()
          other ->
            assertFailure ("expected module suffix hit, got " <> show other),
      testCase "source-key lookup prefers exact keys over suffix keys" $ do
        let exactOracle = emptyOracle "app/Foo.hs"
            suffixOracle = emptyOracle "src/Foo.hs"
            oracleIndex = buildHieOracleIndex [exactOracle, suffixOracle]
        case lookupModuleOracle oracleIndex (OracleQuery "app/Foo.hs" Nothing []) of
          OracleFound GivenPathKey "app/Foo.hs" _ ->
            pure ()
          other ->
            assertFailure ("expected exact hit before suffix fallback, got " <> show other),
      testCase "source-key lookup attaches root-relative paths before suffix fallback" $ do
        let oracleIndex = buildHieOracleIndex [emptyOracle "src/Foo.hs"]
        case lookupModuleOracle oracleIndex (OracleQuery "/workspace/pkg/src/Foo.hs" Nothing ["/workspace/pkg"]) of
          OracleFound RootRelativeKey "src/Foo.hs" _ ->
            pure ()
          other ->
            assertFailure ("expected root-relative hit, got " <> show other),
      testCase "source-key lookup stops at the longest matching suffix before shorter ambiguities" $ do
        let oracleIndex =
              buildHieOracleIndex
                [ emptyOracle "pkg-a/src/Foo.hs",
                  emptyOracle "pkg-b/src/Foo.hs",
                  emptyOracle "other/Foo.hs"
                ]
        case lookupModuleOracle oracleIndex (OracleQuery "/workspace/pkg-a/src/Foo.hs" Nothing []) of
          OracleFound ModuleSuffixKey "pkg-a/src/Foo.hs" _ ->
            pure ()
          other ->
            assertFailure ("expected longest singleton suffix hit, got " <> show other),
      testCase "source-key lookup reports exact-key ambiguity" $ do
        let oracleIndex = buildHieOracleIndex [emptyOracle "src/Foo.hs", emptyOracle "src/Foo.hs"]
        case lookupModuleOracle oracleIndex (OracleQuery "src/Foo.hs" Nothing []) of
          OracleAmbiguous GivenPathKey "src/Foo.hs" candidates ->
            assertBool "ambiguous exact lookup carries candidates" (length candidates == 2)
          other ->
            assertFailure ("expected exact ambiguity, got " <> show other),
      testCase "source-key lookup records tried keys for misses" $ do
        let oracleIndex = buildHieOracleIndex [emptyOracle "src/Foo.hs"]
        case lookupModuleOracle oracleIndex (OracleQuery "src/Bar.hs" Nothing []) of
          OracleMissing triedKeys ->
            assertBool
              "miss reports exact identity before suffix identities"
              ( take 3 triedKeys
                  == [ TriedKey GivenPathKey "src/Bar.hs",
                       TriedKey ModuleSuffixKey "src/Bar.hs",
                       TriedKey ModuleSuffixKey "Bar.hs"
                     ]
              )
          other ->
            assertFailure ("expected miss, got " <> show other)
    ]

emptyOracle :: FilePath -> ModuleNameOracle
emptyOracle sourcePath =
  ModuleNameOracle
    { mnoSourcePath = normalise sourcePath,
      mnoGlobalUses = Map.empty,
      mnoEvidenceAtSpan = Map.empty,
      mnoTypeAtSpan = Map.empty
    }

oracleFixtureSource :: String
oracleFixtureSource =
  unlines
    [ "module OracleFixture where",
      "composed = (.) id id",
      "mapped xs = map id xs",
      "mappedMaybe = fmap not (Just True)",
      "shown = show (Just True)"
    ]

shadowFixtureSource :: String
shadowFixtureSource =
  unlines
    [ "module ShadowFixture where",
      "import Prelude hiding ((.))",
      "(.) x = x",
      "token = ()",
      "shadow = (.) token"
    ]

acceptedOriginsFor :: String -> IO (Set.Set ResolvedOrigin)
acceptedOriginsFor occText =
  either
    (\failure -> assertFailure ("accepted-origin fixture failed to parse: " <> show failure))
    (pure . Set.fromList)
    ( traverse
        (\(unitText, moduleText) -> mkResolvedOrigin unitText moduleText occText)
        [ ("base", "GHC.Base"),
          ("base", "GHC.Internal.Base"),
          ("ghc-internal", "GHC.Internal.Base")
        ]
    )

compileAndReadOracle :: String -> String -> IO ModuleNameOracle
compileAndReadOracle moduleName sourceText = do
  temporaryDirectory <- getTemporaryDirectory
  let root = temporaryDirectory </> "pale-hie-oracle-spec" </> moduleName
      sourceDirectory = root </> "src"
      hieDirectory = root </> "hie"
      sourcePath = sourceDirectory </> moduleName <> ".hs"
  removePathForcibly root
  createDirectoryIfMissing True sourceDirectory
  createDirectoryIfMissing True hieDirectory
  writeFile sourcePath sourceText
  (exitCode, _stdoutText, stderrText) <-
    readProcessWithExitCode
      "ghc"
      [ "-fno-code",
        "-fforce-recomp",
        "-fwrite-ide-info",
        "-hiedir",
        hieDirectory,
        sourcePath
      ]
      ""
  case exitCode of
    ExitSuccess -> do
      (errors, oracleIndex) <- indexHieRoots [hieDirectory]
      let lookupResult =
            lookupModuleOracle
              oracleIndex
              OracleQuery
                { oqGivenPath = normalise sourcePath,
                  oqAbsolutePath = Just (normalise sourcePath),
                  oqSourceRoots = [sourceDirectory]
                }
      case (errors, oracleLookupOracle lookupResult) of
        ([], Just oracle) ->
          pure oracle
        ([], Nothing) ->
          assertFailure ("oracle missing for " <> sourcePath <> ": " <> show lookupResult)
        (hieErrors, _) ->
          assertFailure ("hie read errors: " <> show hieErrors)
    ExitFailure _ ->
      assertFailure stderrText

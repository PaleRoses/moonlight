module Moonlight.Flow.Runtime.RuntimeAuthorityBoundarySpec
  ( tests,
  )
where

import Data.List
  ( intercalate,
    isInfixOf,
    isPrefixOf,
    isSuffixOf,
  )
import Data.Char
  ( isSpace,
  )
import Moonlight.Pale.Test.Gluing.Registry
  ( discoverParsedHaskellFilesWithExcludes,
  )
import Moonlight.Pale.Test.Section.ResourcePath
  ( renderResourcePathError,
    resolveCompilerDirectory,
    resolveCompilerRoot,
  )
import System.FilePath
  ( (</>),
    makeRelative,
    normalise,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertFailure,
    testCase,
  )

type SourceFile = (FilePath, String)

data SourceViolation = SourceViolation
  { svPath :: !FilePath,
    svMessage :: !String
  }

tests :: TestTree
tests =
  testGroup
    "relational runtime authority boundary"
    [ testCase "does not import derived context-row cache or refresh machinery" derivedContextRowsBoundaryAssertion,
      testCase "runtime internals do not import the public engine facade" runtimeFacadeBoundaryAssertion,
      testCase "keeps runtime internals behind the public/expert surface" runtimeCabalSurfaceAssertion,
      testCase "keeps core state/env/create/hydrate unable to name runtime sections" runtimeCorePureBoundaryAssertion,
      testCase "keeps raw carrier reuse selection out of runtime" runtimeCarrierReuseSelectionBoundaryAssertion,
      testCase "keeps deleted runtime facades dead" runtimeDeletedRuntimeFacadeBoundaryAssertion,
      testCase "keeps runtime section imports pointed forward" runtimeDirectionalBoundaryAssertion
    ]

derivedContextRowsBoundaryAssertion :: Assertion
derivedContextRowsBoundaryAssertion =
  withRuntimeCompilerRoot $ \compilerRoot -> do
    sourceFiles <-
      discoverSourceText semanticSourceRoots
    assertNoSourceViolations
      "derived context-row dependency in runtime authority path:"
      compilerRoot
      (foldMap derivedContextRowViolations (filter semanticFileAllowed sourceFiles))

runtimeFacadeBoundaryAssertion :: Assertion
runtimeFacadeBoundaryAssertion =
  withRuntimeCompilerRoot $ \compilerRoot -> do
    runtimeSources <-
      discoverRuntimeAuthoritySources
    assertNoSourceViolations
      "runtime authority imports public Runtime.Engine facade:"
      compilerRoot
      (foldMap runtimeFacadeImportViolations (filter runtimeFacadeFileAllowed runtimeSources))

runtimeCabalSurfaceAssertion :: Assertion
runtimeCabalSurfaceAssertion =
  withRuntimeCompilerRoot $ \compilerRoot -> do
    cabalSource <-
      readFile (normalise (compilerRoot </> runtimePackageMarker))
    let illegalExports =
          filter
            (`notElem` allowedRuntimeExposedModules)
            (exposedRuntimeModules cabalSource)
    assertBool
      ( "unexpected exposed runtime modules:\n"
          <> intercalate "\n" illegalExports
      )
      (null illegalExports)
    runtimeSources <-
      discoverRuntimeAuthoritySources
    assertNoSourceViolations
      "legacy lattice-first module leaked into runtime authority path:"
      compilerRoot
      (foldMap latticeFirstImportViolations runtimeSources)

runtimeCorePureBoundaryAssertion :: Assertion
runtimeCorePureBoundaryAssertion =
  withRuntimeCompilerRoot $ \compilerRoot -> do
    runtimeSources <-
      discoverRuntimeAuthoritySources
    assertNoSourceViolations
      "core lifecycle module imports concrete runtime section:"
      compilerRoot
      (foldMap runtimeCorePureImportViolations (filter runtimeCorePureFile runtimeSources))

runtimeCarrierReuseSelectionBoundaryAssertion :: Assertion
runtimeCarrierReuseSelectionBoundaryAssertion =
  withRuntimeCompilerRoot $ \compilerRoot -> do
    runtimeSources <-
      discoverRuntimeAuthoritySources
    assertNoSourceViolations
      "runtime imports raw carrier reuse candidate selection:"
      compilerRoot
      (foldMap runtimeCarrierReuseSelectionViolations runtimeSources)

runtimeDeletedRuntimeFacadeBoundaryAssertion :: Assertion
runtimeDeletedRuntimeFacadeBoundaryAssertion =
  withRuntimeCompilerRoot $ \compilerRoot -> do
    cabalSource <-
      readFile (normalise (compilerRoot </> runtimePackageMarker))
    runtimeSources <-
      discoverRuntimeAuthoritySources
    assertNoSourceViolations
      "deleted runtime facade leaked back into runtime source or cabal surface:"
      compilerRoot
      ( deletedRuntimeModuleCabalViolations compilerRoot cabalSource
          <> foldMap deletedRuntimeModuleSourceViolations runtimeSources
      )

runtimeDirectionalBoundaryAssertion :: Assertion
runtimeDirectionalBoundaryAssertion =
  withRuntimeCompilerRoot $ \compilerRoot -> do
    runtimeSources <-
      discoverRuntimeAuthoritySources
    assertNoSourceViolations
      "runtime section imports violate owner direction:"
      compilerRoot
      (foldMap runtimeDirectionalImportViolations runtimeSources)

allowedRuntimeExposedModules :: [String]
allowedRuntimeExposedModules =
  [ "Moonlight.Flow.Runtime",
    "Moonlight.Flow.Runtime.Core",
    "Moonlight.Flow.Runtime.Engine",
    "Moonlight.Flow.Runtime.Topology"
  ]

exposedRuntimeModules :: String -> [String]
exposedRuntimeModules sourceText =
  concatMap runtimeModuleLine exposedLines
  where
    defaultLibraryLines =
      drop 1 (dropWhile ((/= "library") . trimLeft) (lines sourceText))

    exposedLines =
      takeWhile
        (not . otherModulesHeader)
        (drop 1 (dropWhile (not . exposedModulesHeader) defaultLibraryLines))

    exposedModulesHeader line =
      trimLeft line == "exposed-modules:"

    otherModulesHeader line =
      trimLeft line == "other-modules:"

    runtimeModuleLine line =
      let trimmed =
            trimLeft line
       in [ trimmed
          | "Moonlight.Flow.Runtime" `isPrefixOf` trimmed
          ]

trimLeft :: String -> String
trimLeft =
  dropWhile (== ' ')

withRuntimeCompilerRoot :: (FilePath -> Assertion) -> Assertion
withRuntimeCompilerRoot assertion =
  resolveCompilerRoot runtimePackageMarker
    >>= either (assertFailure . renderResourcePathError) assertion

runtimePackageMarker :: FilePath
runtimePackageMarker =
  "foundation/moonlight-flow/runtime/moonlight-flow-runtime.cabal"

semanticSourceRoots :: [FilePath]
semanticSourceRoots =
  [ "foundation/moonlight-flow/runtime/src/Moonlight/Flow/Runtime",
    "foundation/moonlight-flow/carrier/src/Moonlight/Flow/Carrier"
  ]

runtimeAuthorityRoots :: [FilePath]
runtimeAuthorityRoots =
  [ "foundation/moonlight-flow/runtime/src/Moonlight/Flow/Runtime"
  ]

discoverRuntimeAuthoritySources :: IO [SourceFile]
discoverRuntimeAuthoritySources =
  discoverSourceText runtimeAuthorityRoots

discoverSourceText :: [FilePath] -> IO [SourceFile]
discoverSourceText relativeRoots = do
  resolvedRoots <-
    resolveSourceRoots relativeRoots
  sourceResults <-
    traverse discoverSourceRoot resolvedRoots
  case fmap concat (sequenceA sourceResults) of
    Left parseErrors ->
      assertFailure (intercalate "\n" parseErrors)
    Right sourceFiles ->
      pure sourceFiles

resolveSourceRoots :: [FilePath] -> IO [FilePath]
resolveSourceRoots relativeRoots = do
  rootResults <-
    traverse (resolveCompilerDirectory runtimePackageMarker) relativeRoots
  either (assertFailure . renderResourcePathError) pure (sequenceA rootResults)

discoverSourceRoot :: FilePath -> IO (Either [String] [SourceFile])
discoverSourceRoot =
  discoverParsedHaskellFilesWithExcludes excludedDirectoryNames (\_ sourceText -> Right sourceText)

excludedDirectoryNames :: [FilePath]
excludedDirectoryNames =
  [ ".git",
    "dist",
    "build",
    "target",
    "node_modules",
    "generated"
  ]

semanticFileAllowed :: SourceFile -> Bool
semanticFileAllowed (sourcePath, _) =
  not ("Internal/Oracle" `isInfixOf` normalizeSlash sourcePath)
    && not ("Diff/Oracle" `isInfixOf` normalizeSlash sourcePath)

derivedContextRowViolations :: SourceFile -> [SourceViolation]
derivedContextRowViolations (sourcePath, sourceText) =
  [ SourceViolation sourcePath ("contains " <> show needle)
  | needle <- forbiddenNeedles,
    needle `isInfixOf` sourceText
  ]

forbiddenNeedles :: [String]
forbiddenNeedles =
  [ contextRowsCacheModuleNeedle,
    "ContextRowsRuntime",
    "ContextRowsCache",
    "getContextRows",
    "crrChooseRestrictionSource",
    "crrMaterializeRootRows",
    "crrDeriveByRestriction",
    "refreshContextSections",
    "refreshContextSectionsWithBudget"
  ]

contextRowsCacheModuleNeedle :: String
contextRowsCacheModuleNeedle =
  "Moonlight.Sheaf.Context." <> "RowsCache"

runtimeFacadeFileAllowed :: SourceFile -> Bool
runtimeFacadeFileAllowed (sourcePath, _) =
  not ("Runtime/Engine.hs" `isSuffixOf` normalizeSlash sourcePath)

runtimeFacadeImportViolations :: SourceFile -> [SourceViolation]
runtimeFacadeImportViolations (sourcePath, sourceText) =
  [ SourceViolation sourcePath "imports Runtime.Engine"
  | runtimeEngineImportNeedle `isInfixOf` sourceText
  ]

runtimeEngineImportNeedle :: String
runtimeEngineImportNeedle =
  "import Moonlight.Flow.Runtime.Public." <> "Engine"

latticeFirstImportViolations :: SourceFile -> [SourceViolation]
latticeFirstImportViolations (sourcePath, sourceText) =
  [ SourceViolation sourcePath "imports legacy lattice-first module"
  | legacyLatticeFirstImportNeedle `isInfixOf` sourceText
  ]

legacyLatticeFirstImportNeedle :: String
legacyLatticeFirstImportNeedle =
  "import Moonlight.Flow." <> "Lattice.First"

runtimeCorePureFile :: SourceFile -> Bool
runtimeCorePureFile (sourcePath, _) =
  any (`isSuffixOf` normalizeSlash sourcePath) runtimeCorePureModulePaths

runtimeCorePureModulePaths :: [FilePath]
runtimeCorePureModulePaths =
  [ "Moonlight/Flow/Runtime/Core/State.hs",
    "Moonlight/Flow/Runtime/Core/Env.hs",
    "Moonlight/Flow/Runtime/Core/Create.hs",
    "Moonlight/Flow/Runtime/Core/Hydrate.hs",
    "Moonlight/Flow/Runtime/Core/Patch/Validation.hs"
  ]

runtimeCorePureImportViolations :: SourceFile -> [SourceViolation]
runtimeCorePureImportViolations (sourcePath, sourceText) =
  [ SourceViolation sourcePath ("imports concrete runtime section " <> forbiddenModule)
  | forbiddenModule <- runtimeCoreForbiddenImports,
    ("import " <> forbiddenModule) `isInfixOf` sourceText
  ]

runtimeCoreForbiddenImports :: [String]
runtimeCoreForbiddenImports =
  [ "Moonlight.Flow.Runtime.Engine",
    "Moonlight.Flow.Runtime.Topology"
  ]

runtimeCarrierReuseSelectionViolations :: SourceFile -> [SourceViolation]
runtimeCarrierReuseSelectionViolations (sourcePath, sourceText) =
  [ SourceViolation sourcePath "names selectRequestedCarrierReuseCandidates"
  | "selectRequestedCarrierReuseCandidates" `isInfixOf` sourceText
  ]

runtimeDeletedPublicModules :: [String]
runtimeDeletedPublicModules =
  [ "Moonlight.Flow.Runtime.Diagnostics",
    "Moonlight.Flow.Runtime.Diagnostics.Replay",
    "Moonlight.Flow.Runtime.Core.Backend",
    "Moonlight.Flow.Runtime.Core.Config",
    "Moonlight.Flow.Runtime.Core.Types",
    "Moonlight.Flow.Runtime.Core.Error",
    "Moonlight.Flow.Runtime.Core.Input"
  ]

deletedRuntimeModuleCabalViolations :: FilePath -> String -> [SourceViolation]
deletedRuntimeModuleCabalViolations compilerRoot cabalSource =
  [ SourceViolation (compilerRoot </> runtimePackageMarker) ("mentions deleted module " <> moduleName)
  | moduleName <- runtimeDeletedPublicModules,
    moduleName `elem` cabalModuleLines cabalSource
  ]

cabalModuleLines :: String -> [String]
cabalModuleLines =
  fmap trim . lines
{-# INLINE cabalModuleLines #-}

trim :: String -> String
trim =
  reverse . dropWhile isSpace . reverse . dropWhile isSpace
{-# INLINE trim #-}

deletedRuntimeModuleSourceViolations :: SourceFile -> [SourceViolation]
deletedRuntimeModuleSourceViolations (sourcePath, sourceText) =
  [ SourceViolation sourcePath ("mentions deleted module " <> moduleName)
  | moduleName <- runtimeDeletedPublicModules,
    (("module " <> moduleName) `isInfixOf` sourceText)
      || (("import " <> moduleName) `isInfixOf` sourceText)
  ]

runtimeDirectionalImportViolations :: SourceFile -> [SourceViolation]
runtimeDirectionalImportViolations (sourcePath, sourceText) =
  [ SourceViolation sourcePath ("imports forbidden downstream module " <> forbiddenImport)
  | (ownerPath, forbiddenImport) <- runtimeForbiddenDirectionalImports,
    ownerPath `isInfixOf` normalizeSlash sourcePath,
    ("import " <> forbiddenImport) `isInfixOf` sourceText
  ]

runtimeForbiddenDirectionalImports :: [(FilePath, String)]
runtimeForbiddenDirectionalImports =
  [ ("Moonlight/Flow/Runtime/Topology/", "Moonlight.Flow.Runtime.Engine"),
    ("Moonlight/Flow/Runtime/Carrier/", "Moonlight.Flow.Runtime.Factor.Repair"),
    ("Moonlight/Flow/Runtime/Factor/", "Moonlight.Flow.Runtime.Engine.Queue")
  ]

assertNoSourceViolations :: String -> FilePath -> [SourceViolation] -> Assertion
assertNoSourceViolations header compilerRoot violations =
  if null violations
    then pure ()
    else
      assertFailure
        (unlines (header : fmap (renderSourceViolation compilerRoot) violations))

renderSourceViolation :: FilePath -> SourceViolation -> String
renderSourceViolation compilerRoot violation =
  normalise (makeRelative compilerRoot (svPath violation)) <> ": " <> svMessage violation

normalizeSlash :: FilePath -> FilePath
normalizeSlash =
  fmap
    ( \charValue ->
        if charValue == '\\'
          then '/'
          else charValue
    )

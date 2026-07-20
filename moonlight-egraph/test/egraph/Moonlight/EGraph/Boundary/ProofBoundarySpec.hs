module Moonlight.EGraph.Boundary.ProofBoundarySpec
  ( tests,
  )
where

import Data.List (nub, sort)
import Moonlight.Rewrite.ProofContext
  ( checkProofBoundary,
    parseTheoremManifest,
    proofManifestHashPath,
    proofManifestPath,
    proofTheoremManifestIdentifiers,
    requiredProofTheoremManifest,
    requiredRestrictionLeanTheoremManifest,
    requiredRestrictionManifestTheoremManifest,
    requiredRestrictionRuntimeLawIdentifiers,
    requiredRuntimeLawObligationIdentifiers,
    restrictionKernelSchemaHashPath,
    restrictionKernelSchemaPath,
  )
import Moonlight.Sheaf.Context.Schema
  ( renderRestrictionKernelSchemaJson,
    restrictionKernelLeanTheoremIdentifiers,
    restrictionKernelManifestTheoremIdentifiers,
    restrictionKernelRuntimeLawIdentifiers,
    restrictionKernelSchema,
  )
import Moonlight.EGraph.Effect.LawNames (EGraphLawName (..), eGraphLawName)
import Moonlight.EGraph.Test.Case (HUnitCase (..), hunitCases)
import Moonlight.Pale.Test.Section.ResourcePath
  ( renderResourcePathError,
    resolvePackageFile,
  )
import System.Directory (findExecutable)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure)

eGraphPackageMarker :: FilePath
eGraphPackageMarker = "foundation/moonlight-egraph/moonlight-egraph.cabal"

resolveProofFile :: FilePath -> IO FilePath
resolveProofFile relativePath =
  resolvePackageFile eGraphPackageMarker relativePath
    >>= either (assertFailure . renderResourcePathError) pure

normalizeTheoremNames :: [String] -> [String]
normalizeTheoremNames =
  sort . nub

hashToken :: String -> String
hashToken content =
  takeWhile (`notElem` [' ', '\t', '\n', '\r']) content

computeSha256 :: FilePath -> IO (Maybe String)
computeSha256 targetPath = do
  digestFromSha256sum <- computeWith "sha256sum" [targetPath]
  case digestFromSha256sum of
    Just digestValue -> pure (Just digestValue)
    Nothing -> computeWith "shasum" ["-a", "256", targetPath]

computeWith :: String -> [String] -> IO (Maybe String)
computeWith executableName arguments = do
  maybeExecutablePath <- findExecutable executableName
  case maybeExecutablePath of
    Nothing -> pure Nothing
    Just executablePath -> do
      (exitCode, stdoutContent, _) <- readProcessWithExitCode executablePath arguments ""
      case (exitCode, words stdoutContent) of
        (ExitSuccess, digestValue : _) -> pure (Just digestValue)
        _ -> pure Nothing

trimTrailingNewline :: String -> String
trimTrailingNewline content =
  reverse (dropWhile (`elem` ['\n', '\r']) (reverse content))

type HashArtifactCase = (String, FilePath, FilePath, String, String)

tests :: TestTree
tests =
  testGroup "ProofBoundary" . hunitCases $
    [ HUnitCase "extracted theorem manifest matches required registry" $ do
        manifestPath <- resolveProofFile proofManifestPath
        manifest <-
          readFile manifestPath
            >>= expectRight . parseTheoremManifest
        requiredManifest <-
          expectRight requiredProofTheoremManifest
        either
          (assertFailure . show)
          (const (pure ()))
          (checkProofBoundary manifest)
        assertNormalizedEqual
          "manifest theorem set must exactly match required theorem ids"
          (proofTheoremManifestIdentifiers requiredManifest)
          (proofTheoremManifestIdentifiers manifest),
      HUnitCase "restriction kernel schema artifact matches the canonical shared contract" $ do
        schemaPath <- resolveProofFile restrictionKernelSchemaPath
        schemaSource <- readFile schemaPath
        assertEqual
          "restriction kernel schema must match the canonical runtime/proof contract"
          (trimTrailingNewline (renderRestrictionKernelSchemaJson restrictionKernelSchema))
          (trimTrailingNewline schemaSource)
    ]
      <> fmap hashArtifactTest hashArtifactCases
      <> theoremSetCases
      <> [ HUnitCase "restriction kernel manifest subset is contained in the Lean theorem witness surface" $
             do
               leanTheorems <-
                 proofTheoremManifestIdentifiers
                   <$> expectRight requiredRestrictionLeanTheoremManifest
               manifestTheorems <-
                 proofTheoremManifestIdentifiers
                   <$> expectRight requiredRestrictionManifestTheoremManifest
               assertBool
                 "manifest subset must be a subset of the Lean theorem witness set"
                 (all (`elem` normalizeTheoremNames leanTheorems) (normalizeTheoremNames manifestTheorems)),
           HUnitCase "runtime-law obligations have matching egraph law test ids" $
             do
               runtimeLawObligationIdentifiers <-
                 expectRight requiredRuntimeLawObligationIdentifiers
               assertBool
                 "each runtime-law obligation must be present in the egraph runtime law registry"
                 (all (`elem` runtimeLawRegistryNames) runtimeLawObligationIdentifiers)
         ]

hashArtifactCases :: [HashArtifactCase]
hashArtifactCases =
  [ ("manifest hash artifact matches computed digest", proofManifestPath, proofManifestHashPath, "expected sha256sum or shasum to compute manifest digest", "manifest hash must match computed SHA-256 digest"),
    ("restriction kernel schema hash artifact matches computed digest", restrictionKernelSchemaPath, restrictionKernelSchemaHashPath, "expected sha256sum or shasum to compute schema digest", "schema hash must match computed SHA-256 digest")
  ]

hashArtifactTest :: HashArtifactCase -> HUnitCase
hashArtifactTest (caseName, sourceRelativePath, artifactRelativePath, missingToolMessage, mismatchLabel) =
  HUnitCase caseName $ do
    sourcePath <- resolveProofFile sourceRelativePath
    artifactPath <- resolveProofFile artifactRelativePath
    expectedHashContent <- readFile artifactPath
    computedDigest <- computeSha256 sourcePath
    case computedDigest of
      Nothing -> assertFailure missingToolMessage
      Just digestValue ->
        assertEqual mismatchLabel (hashToken expectedHashContent) digestValue

theoremSetCases :: [HUnitCase]
theoremSetCases =
  [ HUnitCase "restriction kernel schema runtime law names align with the effect-law registry" $
      assertNormalizedEqual
        "schema runtime laws must match the runtime law registry"
        restrictionKernelRuntimeLawIdentifiers
        runtimeLawRegistryNames,
    HUnitCase "restriction kernel schema runtime law names drive the proof boundary restriction runtime surface" $
      assertNormalizedEqual
        "required runtime restriction laws must come from the schema"
        restrictionKernelRuntimeLawIdentifiers
        requiredRestrictionRuntimeLawIdentifiers,
    HUnitCase "restriction kernel schema lean theorem names drive the proof boundary restriction surface" $ do
      leanTheorems <-
        proofTheoremManifestIdentifiers
          <$> expectRight requiredRestrictionLeanTheoremManifest
      assertNormalizedEqual
        "required Lean restriction theorems must come from the schema"
        restrictionKernelLeanTheoremIdentifiers
        leanTheorems,
    HUnitCase "restriction kernel schema manifest subset drives the proof boundary manifest subset" $ do
      manifestTheorems <-
        proofTheoremManifestIdentifiers
          <$> expectRight requiredRestrictionManifestTheoremManifest
      assertNormalizedEqual
        "required manifest restriction theorems must come from the schema"
        restrictionKernelManifestTheoremIdentifiers
        manifestTheorems
  ]

assertNormalizedEqual :: String -> [String] -> [String] -> IO ()
assertNormalizedEqual label expected actual =
  assertEqual label (normalizeTheoremNames expected) (normalizeTheoremNames actual)

runtimeLawRegistryNames :: [String]
runtimeLawRegistryNames =
  fmap
    eGraphLawName
    [ContextRestrictionIdentity, ContextRestrictionComposition, ContextMorphismLeftIdentity, ContextMorphismRightIdentity, ContextMorphismAssociative, ContextRestrictionFunctorialAction, ContextGlobalSection, ContextGlobalSectionInvariant]

expectRight :: Show errorValue => Either errorValue value -> IO value
expectRight =
  either (assertFailure . ("unexpected Left: " <>) . show) pure

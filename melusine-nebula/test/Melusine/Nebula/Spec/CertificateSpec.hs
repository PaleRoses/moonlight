module Melusine.Nebula.Spec.CertificateSpec (spec) where

import Data.Foldable (traverse_)
import Data.List (find)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Melusine.Nebula
  ( HunkCertificate (..),
    LawStamp (..),
    ModuleImprovement (..),
    ModulePatch (..),
    ModuleWorkload (..),
    NebulaProvenance (..),
    ProvenanceEntry (..),
    defaultNebulaConfig,
    improveModule,
  )
import Moonlight.EGraph.Introspection.Core.HsExpr
  ( hsExprBetaLawId,
    hsExprCompositionLawId,
    hsExprLetInlineLawId,
    hsExprMapFusionLawId,
  )
import Moonlight.Rewrite.System (LawId)
import Moonlight.Pale.Ghc.Hie.Oracle (ModuleNameOracle (..), PackageUnitParseFailure, mkResolvedOrigin)
import Moonlight.Pale.Ghc.Hie.SourceKey (HieSourceKeyKind (..), OracleLookup (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

spec :: TestTree
spec =
  testGroup
    "nebula.certificate"
    [ testCase "every accepted hunk carries a nonempty provenance set" $ do
        improvement <- requireImproved certificateWorkload
        assertBool "fixture must emit at least one certificate" (not (null (miCertificates improvement)))
        traverse_ assertNonemptyCertificate (miCertificates improvement),
      testCase "the binding-front writeback hunk names the beta and let-inline laws" $ do
        improvement <- requireImproved certificateWorkload
        certificate <- requireCertificate "shadow" improvement
        let laws = certificateLawIds certificate
        assertBool "beta law participates in the hunk certificate" (hsExprBetaLawId `Set.member` laws)
        assertBool "let-inline law participates in the hunk certificate" (hsExprLetInlineLawId `Set.member` laws),
      testCase "certificate digests are deterministic across independent runs" $ do
        firstImprovement <- requireImproved certificateWorkload
        secondImprovement <- requireImproved certificateWorkload
        assertEqual
          "digest sequence is stable"
          (fmap hcDigest (miCertificates firstImprovement))
          (fmap hcDigest (miCertificates secondImprovement)),
      testCase "default certificates do not cite oracle-gated composition" $ do
        improvement <- requireImproved certificateWorkload
        let citedLaws = foldMap certificateLawIds (miCertificates improvement)
        assertBool
          "composition is not admitted without oracle evidence"
          (hsExprCompositionLawId `Set.notMember` citedLaws),
      testCase "oracle-gated map fusion emits a stamped certificate" $ do
        workload <- requireRight "map-fusion workload" mapFusionWorkload
        improvement <- requireImproved workload
        assertBool
          "map fusion fixture must splice the fused binding"
          ("incDouble" `elem` fmap fst (mpSpliced (miPatch improvement)))
        certificate <- requireCertificate "incDouble" improvement
        assertBool
          "map fusion law participates in the hunk certificate"
          (hsExprMapFusionLawId `Set.member` certificateLawIds certificate)
    ]

certificateWorkload :: ModuleWorkload
certificateWorkload =
  ModuleWorkload
    { mwPath = "Melusine/Nebula/CertificateFixture.hs",
      mwSource =
        unlines
          [ "module Melusine.Nebula.CertificateFixture where",
            "",
            "shadow = let g = \\x -> use x x in g alpha",
            "",
            "still = alpha"
          ],
      mwOracleLookup = OracleMissing []
    }

mapFusionWorkload :: Either PackageUnitParseFailure ModuleWorkload
mapFusionWorkload =
  do
    oracle <- mapFusionOracle
    pure
      ModuleWorkload
        { mwPath = "Melusine/Nebula/MapFusionFixture.hs",
          mwSource =
            unlines
              [ "module Melusine.Nebula.MapFusionFixture where",
                "",
                "incDouble xs = map inc (map dbl xs)"
              ],
          mwOracleLookup = attachedOracle oracle
        }

mapFusionOracle :: Either PackageUnitParseFailure ModuleNameOracle
mapFusionOracle =
  do
    compositionOrigin <- mkResolvedOrigin "base" "GHC.Internal.Base" "."
    mapOrigin <- mkResolvedOrigin "base" "GHC.Internal.Base" "map"
    pure
      ModuleNameOracle
        { mnoSourcePath = "Melusine/Nebula/MapFusionFixture.hs",
          mnoGlobalUses =
            Map.fromList
              [ (".", Set.singleton compositionOrigin),
                ("map", Set.singleton mapOrigin)
              ],
          mnoEvidenceAtSpan = Map.empty,
          mnoTypeAtSpan = Map.empty
        }

attachedOracle :: ModuleNameOracle -> OracleLookup
attachedOracle oracle =
  OracleFound GivenPathKey (mnoSourcePath oracle) oracle

requireRight :: Show failure => String -> Either failure value -> IO value
requireRight label =
  either
    (\failure -> assertFailure (label <> " failed: " <> show failure))
    pure

requireImproved :: ModuleWorkload -> IO ModuleImprovement
requireImproved workload =
  either
    (\(modulePath, moduleFailure) -> assertFailure ("improve failed for " <> modulePath <> ": " <> show moduleFailure))
    pure
    (improveModule defaultNebulaConfig workload)

requireCertificate :: String -> ModuleImprovement -> IO HunkCertificate
requireCertificate bindingName improvement =
  maybe
    (assertFailure ("certificate missing for " <> bindingName))
    pure
    (find ((== bindingName) . hcBinding) (miCertificates improvement))

assertNonemptyCertificate :: HunkCertificate -> IO ()
assertNonemptyCertificate certificate =
  assertBool
    ("certificate for " <> hcBinding certificate <> " has no proof entries")
    (not (null (hcEntries certificate)))

certificateLawIds :: HunkCertificate -> Set.Set LawId
certificateLawIds certificate =
  Set.fromList
    [ lsLaw stamp
    | entry <- hcEntries certificate,
      Just stamp <- [npStamp (peProvenance entry)]
    ]

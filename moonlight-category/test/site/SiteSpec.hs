module SiteSpec
  ( tests,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Moonlight.Category.Pure.Site.Compile (thinSiteKernel)
import Moonlight.Category.Pure.Site.Core (SiteFinCatError (..), SiteManifest (..), SiteViolation (..))
import Moonlight.Category.Pure.Site.Graph (importCycles, reachableClosure)
import Moonlight.Category.Pure.Site.Manifest (validateSiteManifest)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Site"
    [ testCase
        "reachableClosure computes the transitive imports of an acyclic DAG"
        testReachableClosureClosesAcyclicDag,
      testCase
        "importCycles reports a singleton self-loop"
        testImportCyclesReportsSingletonSelfLoop,
      testCase
        "importCycles reports disjoint SCCs sorted by least object"
        testImportCyclesReportsDisjointComponentsInLeastObjectOrder,
      testCase
        "validateSiteManifest reports import cycles between declared objects"
        testValidateSiteManifestReportsDeclaredObjectCycle,
      testCase
        "thinSiteKernel rejects cyclic manifests before presentation"
        testThinSiteKernelRejectsDeclaredObjectCycle,
      testCase
        "manifest validation and kernel compilation share diagnostics"
        testManifestValidationAndKernelDiagnosticsAgree,
      testCase
        "validateSiteManifest reports cover sets that are not closed under covered covers"
        testValidateSiteManifestReportsCoverClosureViolation
    ]

testReachableClosureClosesAcyclicDag :: Assertion
testReachableClosureClosesAcyclicDag =
  reachableClosure imports
    @?= Map.fromList
      [ ("api", set ["core"]),
        ("app", set ["api", "core", "ui"]),
        ("core", Set.empty),
        ("ui", set ["core"])
      ]
  where
    imports :: Map String (Set String)
    imports =
      Map.fromList
        [ ("api", set ["core"]),
          ("app", set ["api", "ui"]),
          ("core", Set.empty),
          ("ui", set ["core"])
        ]

testImportCyclesReportsSingletonSelfLoop :: Assertion
testImportCyclesReportsSingletonSelfLoop =
  importCycles manifest @?= ["root" :| []]
  where
    manifest :: SiteManifest String
    manifest =
      SiteManifest
        { siteObjects = set ["root"],
          siteImports = Map.singleton "root" (set ["root"]),
          siteCovers = Map.empty
        }

testImportCyclesReportsDisjointComponentsInLeastObjectOrder :: Assertion
testImportCyclesReportsDisjointComponentsInLeastObjectOrder =
  importCycles manifest @?= ["a" :| ["b"], "c" :| ["d"]]
  where
    manifest :: SiteManifest String
    manifest =
      SiteManifest
        { siteObjects = set ["a", "b", "c", "d", "x"],
          siteImports =
            Map.fromList
              [ ("c", set ["d"]),
                ("x", Set.empty),
                ("a", set ["b"]),
                ("d", set ["c"]),
                ("b", set ["a"])
              ],
          siteCovers = Map.empty
        }

testValidateSiteManifestReportsDeclaredObjectCycle :: Assertion
testValidateSiteManifestReportsDeclaredObjectCycle =
  validateSiteManifest declaredCycleManifest @?= [ImportCycleDetected ("domain" :| ["service"])]

testThinSiteKernelRejectsDeclaredObjectCycle :: Assertion
testThinSiteKernelRejectsDeclaredObjectCycle =
  case thinSiteKernel declaredCycleManifest of
    Left (SiteManifestInvalid violations) ->
      NonEmpty.toList violations @?= validateSiteManifest declaredCycleManifest
    Right _ -> assertFailure "cyclic manifest produced a validated site kernel"

testManifestValidationAndKernelDiagnosticsAgree :: Assertion
testManifestValidationAndKernelDiagnosticsAgree =
  case thinSiteKernel invalidCoverManifest of
    Left (SiteManifestInvalid violations) ->
      NonEmpty.toList violations @?= validateSiteManifest invalidCoverManifest
    Right _ -> assertFailure "invalid cover produced a validated site kernel"

declaredCycleManifest :: SiteManifest String
declaredCycleManifest =
  let objects = set ["domain", "service"]
   in SiteManifest
        { siteObjects = objects,
          siteImports =
            Map.fromList
              [ ("domain", set ["service"]),
                ("service", set ["domain"])
              ],
          siteCovers = Map.fromList [("domain", objects), ("service", objects)]
        }

invalidCoverManifest :: SiteManifest Int
invalidCoverManifest =
  SiteManifest
    { siteObjects = Set.singleton 0,
      siteImports = Map.singleton 0 Set.empty,
      siteCovers = Map.singleton 0 (Set.singleton 1)
    }

testValidateSiteManifestReportsCoverClosureViolation :: Assertion
testValidateSiteManifestReportsCoverClosureViolation =
  validateSiteManifest manifest @?= [CoverNotClosed "root" "leaf" (set ["support"])]
  where
    manifest :: SiteManifest String
    manifest =
      SiteManifest
        { siteObjects = set ["root", "leaf", "support"],
          siteImports =
            Map.fromList
              [ ("root", set ["leaf"]),
                ("leaf", set ["support"]),
                ("support", Set.empty)
              ],
          siteCovers =
            Map.fromList
              [ ("root", set ["leaf"]),
                ("leaf", set ["support"]),
                ("support", Set.empty)
              ]
        }

set :: Ord a => [a] -> Set a
set = Set.fromList

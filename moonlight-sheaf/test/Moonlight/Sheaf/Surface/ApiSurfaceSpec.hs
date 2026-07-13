module Moonlight.Sheaf.Surface.ApiSurfaceSpec
  ( tests,
  )
where

import Data.Char (isSpace)
import Data.List (isInfixOf, isPrefixOf)
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Sheaf.Surface.Support
  ( ExportSurfaceLock (..),
    assertExportSurfaceLocked,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase)

tests :: TestTree
tests =
  testGroup
    "public API surface"
    [ testCase "Moonlight.Sheaf root exports are curated and locked" testRootExportsAreLocked,
      testCase "Moonlight.Sheaf.Presentation exports are curated and locked" testPresentationExportsAreLocked
    ]

testRootExportsAreLocked :: Assertion
testRootExportsAreLocked =
  assertExportSurfaceLocked
    ExportSurfaceLock
      { eslLabel = "Moonlight.Sheaf root",
        eslBarrelPrefix = "module Moonlight.Sheaf.",
        eslSourceCandidates =
          [ "src-public/Moonlight/Sheaf.hs",
            "compiler/foundation/moonlight-sheaf/src-public/Moonlight/Sheaf.hs",
            "/Users/bluerose/Developer/pale-meridian/compiler/foundation/moonlight-sheaf/src-public/Moonlight/Sheaf.hs"
          ],
        eslExpectedExports = expectedRootExports,
        eslForbiddenExport = forbiddenRootExport
      }

testPresentationExportsAreLocked :: Assertion
testPresentationExportsAreLocked =
  assertExportSurfaceLocked
    ExportSurfaceLock
      { eslLabel = "Moonlight.Sheaf.Presentation",
        eslBarrelPrefix = "module Moonlight.Sheaf.",
        eslSourceCandidates =
          [ "src-public/Moonlight/Sheaf/Presentation.hs",
            "compiler/foundation/moonlight-sheaf/src-public/Moonlight/Sheaf/Presentation.hs",
            "/Users/bluerose/Developer/pale-meridian/compiler/foundation/moonlight-sheaf/src-public/Moonlight/Sheaf/Presentation.hs"
          ],
        eslExpectedExports = expectedPresentationExports,
        eslForbiddenExport = forbiddenPresentationExport
      }

forbiddenPresentationExport :: String -> Bool
forbiddenPresentationExport exportEntry =
  "module " `isPrefixOf` exportEntry

expectedPresentationExports :: Set String
expectedPresentationExports =
  Set.fromList
    [ "Presentation",
      "StalkRestrictionKernel (..)",
      "PresentedRestrictionFailure (..)",
      "PresentedComponentFailure (..)",
      "PresentationObstruction (..)",
      "CompiledPresentation",
      "declareCell",
      "declareRefinement",
      "declareCover",
      "declarePresheaf",
      "declareFiber",
      "restricts",
      "declareMorphism",
      "componentAt",
      "declareIdentityMorphism",
      "declareComposition",
      "compilePresentation",
      "presentationSite",
      "presentationPresheafAt",
      "presentationMorphismAt",
      "FinitePresheaf",
      "FinitePresheafFailure (..)",
      "FiniteFiber",
      "finiteFiberAt",
      "finiteFiberValues",
      "restrictPresentedPresheaf",
      "FinitePresheafMorphism",
      "FinitePresheafMorphismFailure (..)",
      "FinitePresheafMorphismCompositionComponentFailure (..)",
      "finitePresheafMorphismComponentAt",
      "finitePresheafMorphismComponents",
      "finitePresheafMorphismSource",
      "finitePresheafMorphismTarget"
    ]

forbiddenRootExport :: String -> Bool
forbiddenRootExport exportEntry =
  let name = exportBaseName exportEntry
   in not (Set.member exportEntry blessedExportExceptions)
        && or
        [ "module Moonlight.Sheaf." `isPrefixOf` exportEntry,
          name == "Scope",
          name == "SectionEpoch",
          "Pruning" `isInfixOf` name,
          "Footprint" `isInfixOf` name,
          any (`isSuffixOfName` name) ["Plan", "Slot", "Report", "Cone", "Audit", "Witness", "Budget"]
        ]

exportBaseName :: String -> String
exportBaseName =
  takeWhile (\char -> not (isSpace char) && char /= '(')

blessedExportExceptions :: Set String
blessedExportExceptions =
  Set.fromList
    [ "CoverSearchBudget (..)",
      "CoverSlot",
      "DescentReport (..)",
      "unboundedCoverSearchBudget"
    ]

isSuffixOfName :: String -> String -> Bool
isSuffixOfName suffix name =
  reverse suffix `isPrefixOf` reverse name

expectedRootExports :: Set String
expectedRootExports =
  Set.fromList
    [ "Amalgamation",
      "AmalgamationLocalityFailure (..)",
      "ChangedObjects (..)",
      "CheckedMorphism (..)",
      "CompileError (..)",
      "CompiledRestriction",
      "CoverConstructionError (..)",
      "CoverGluingFailure (..)",
      "CoverSlot",
      "CoverSlotKey",
      "CoverStalkUniverse",
      "CoveringFamily",
      "CoverSearchBudget (..)",
      "CoverSearchCost (..)",
      "CoverSearchRefusal (..)",
      "DescentOutcome (..)",
      "DescentReport (..)",
      "FiniteMeetMorphism",
      "FiniteMeetSite",
      "FiniteMeetSiteBuildError (..)",
      "FiniteMeetSiteSpec (..)",
      "GhostSection (..)",
      "GluingAlgebra (..)",
      "GluingFailure (..)",
      "GluingObstruction (..)",
      "GlobalSection",
      "IncidenceCoefficient",
      "MatchingFailure (..)",
      "CompatibleMatchingFamily",
      "MatchingFamily",
      "MatchingFamilyConstructionError (..)",
      "PartialSection",
      "PreparedCover",
      "PreparedCoversRefusal (..)",
      "PreparedSite",
      "PullbackSquare (..)",
      "RepairDiagnostics (..)",
      "RepairObstruction (..)",
      "RepairResult (..)",
      "RepairStatus (..)",
      "RestrictionKind (..)",
      "SearchVerdict (..)",
      "Section",
      "SectionCertification (..)",
      "SectionCertificationError (..)",
      "SectionCertificationFailure (..)",
      "SectionConstructionError (..)",
      "SectionLookupError (..)",
      "SectionStoreError (..)",
      "SeparatedCover",
      "SeparatedCoverRefusal (..)",
      "SeparatedEqualityRefusal (..)",
      "SeparatedEqualityVerdict (..)",
      "SeparatedResolutionRefusal (..)",
      "SeparatedUniquenessRefusal (..)",
      "SiteSpec (..)",
      "Site (..)",
      "SiteLawFailure (..)",
      "UniqueAmalgamation",
      "UniverseShapeError (..)",
      "Verdict (..)",
      "amalgamatedStalk",
      "amalgamationMatchingFamily",
      "assign",
      "assignOne",
      "certify",
      "certifyAmalgamation",
      "certifyMatching",
      "certifyUniqueAmalgamation",
      "changedObjects",
      "compile",
      "compatibleMatchingFamilyUnderlying",
      "completeSearchVerdict",
      "coverArrows",
      "coverSize",
      "coverSlotArrow",
      "coverSlotKey",
      "coverSources",
      "coverStalkUniverse",
      "coverTarget",
      "decidedSearchVerdict",
      "entries",
      "finiteMeetMorphism",
      "finiteMeetRefines",
      "finiteMeetSiteCells",
      "finiteMeetSiteCovers",
      "finiteMeetSiteMeet",
      "finiteMeetSiteRefinements",
      "globalSection",
      "globalSectionUnderlying",
      "glue",
      "incidenceCoefficientValue",
      "initialSheafModelVersion",
      "isIdentityMorphism",
      "isSectionCompatible",
      "matching",
      "matchingCover",
      "matchingPreparedCover",
      "matchingSections",
      "matchingTarget",
      "mkCoveringFamily",
      "mkFiniteMeetSite",
      "mkIncidenceCoefficient",
      "mkIncidenceRestriction",
      "negativeUnitIncidenceRestriction",
      "partial",
      "partialEntries",
      "preparedCoverSize",
      "preparedCoverSlots",
      "preparedCoverSources",
      "preparedCoverTarget",
      "preparedCovers",
      "repair",
      "resolveUniqueAmalgamation",
      "restrictionMorphism",
      "searchVerdictDecided",
      "searchVerdictObstructions",
      "searchVerdictRefusals",
      "section",
      "sectionCompatibilityVerdict",
      "sectionEpoch",
      "separatedCover",
      "separatedLocalEqualityAt",
      "siteSpec",
      "siteLawFailures",
      "siteRestrictionMorphisms",
      "stalkAt",
      "tabulateSection",
      "unboundedCoverSearchBudget",
      "uniqueAmalgamationUnderlying",
      "unitIncidenceRestriction"
    ]

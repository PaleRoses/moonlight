{-# LANGUAGE ScopedTypeVariables #-}

module Moonlight.Cosheaf.Surface.RootSpec
  ( tests,
  )
where

import Control.Exception (IOException, try)
import Data.Char (isSpace)
import Data.List (dropWhileEnd, intercalate, isPrefixOf)
import Data.Set (Set)
import Data.Set qualified as Set
import System.Directory (getCurrentDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "cosheaf public API surface"
    [ testCase "Moonlight.Cosheaf root exports are curated and locked" testRootExportsAreLocked
    ]

testRootExportsAreLocked :: Assertion
testRootExportsAreLocked = do
  (_sourcePath, sourceText) <- readCosheafSource
  let exports = exportEntries sourceText
      barrelExports = Set.filter ("module Moonlight.Cosheaf." `isPrefixOf`) exports
  assertBool
    ("root must not re-export barrel modules: " <> showSet barrelExports)
    (Set.null barrelExports)
  assertBool
    "FiniteCosheaf constructor must stay sealed"
    (Set.notMember "FiniteCosheaf (..)" exports)
  assertBool
    "LinearCosheaf constructor must stay sealed"
    (Set.notMember "LinearCosheaf (..)" exports)
  assertEqual
    "cosheaf root export whitelist"
    expectedRootExports
    exports

readCosheafSource :: IO (FilePath, String)
readCosheafSource =
  getCurrentDirectory >>= \cwd ->
    firstReadable
      [ "src-cosheaf/Moonlight/Cosheaf.hs",
        "foundation/moonlight-sheaf/src-cosheaf/Moonlight/Cosheaf.hs",
        "/Users/bluerose/Developer/pale-meridian/compiler/foundation/moonlight-sheaf/src-cosheaf/Moonlight/Cosheaf.hs"
      ]
      >>= maybe
        (assertFailure ("could not locate Moonlight.Cosheaf source from " <> cwd))
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
    . dropWhile (not . ("module Moonlight.Cosheaf" `isPrefixOf`) . trim)
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

expectedRootExports :: Set String
expectedRootExports =
  Set.fromList
    [ "CosheafLawFailure (..)",
      "checkCorestrictionIdentityLawWith",
      "checkCorestrictionCompositionDefined",
      "checkCorestrictionCompositionLawWith",
      "CostalkKey (..)",
      "FiniteCostalk (..)",
      "FiniteCosheafAlgebra (..)",
      "CompiledCorestriction (..)",
      "FiniteCosheaf",
      "fcSite",
      "fcSiteIndex",
      "fcCostalks",
      "fcCorestrictions",
      "FiniteCosheafFailure (..)",
      "finiteCostalkAt",
      "finiteCostalkAtObjectKey",
      "finiteCostalkValues",
      "finiteCostalkKeys",
      "finiteCostalkKeyIntSet",
      "finiteCostalkValueAt",
      "finiteCostalkKeyOf",
      "mkFiniteCosheaf",
      "compiledCorestrictionFor",
      "corestrictCostalkKey",
      "finiteCosheafCorestrictions",
      "CosectionRepKey (..)",
      "CosectionClassKey (..)",
      "CosectionRepresentative (..)",
      "GlobalCosection (..)",
      "cosectionRepKeyInt",
      "cosectionClassKeyInt",
      "cosectionClassOfRepresentativeKey",
      "CosheafColimit (..)",
      "CosheafColimitFailure (..)",
      "CosheafColimitFactor (..)",
      "CosheafColimitFactorFailure (..)",
      "finiteCosheafColimitFromPreparedSupport",
      "finiteCosheafColimitFromSupportPlan",
      "cosheafColimitRepresentatives",
      "cosheafColimitClassOf",
      "cosheafColimitEquivalent",
      "cosheafColimitMembers",
      "cosheafColimitClassKeys",
      "factorCosheafColimit",
      "cosectionRepresentativeKeyOf",
      "cosectionRepresentativeAt",
      "CoefficientOps (..)",
      "PivotOps (..)",
      "intCoefficientOps",
      "integerCoefficientOps",
      "rationalCoefficientOps",
      "gf2CoefficientOps",
      "intUnitPivotOps",
      "integerUnitPivotOps",
      "rationalPivotOps",
      "gf2PivotOps",
      "ProvenanceId (..)",
      "ProvenanceArena (..)",
      "emptyProvenanceArena",
      "appendProvenance",
      "lookupProvenance",
      "CosheafCoordinate (..)",
      "BoundaryTerm (..)",
      "PreparedCosheafBoundary (..)",
      "PreparedCosheafChain (..)",
      "PreparedCosheafChainFailure (..)",
      "buildPreparedCosheafBoundary",
      "mkPreparedCosheafChain",
      "preparedCosheafBoundaryAt",
      "preparedCosheafBasisAt",
      "preparedCosheafBoundaryIncidenceAt",
      "preparedCosheafBoundaryEntryProvenance",
      "CosheafNerveChainKey",
      "CosheafChainBasisKey",
      "CosheafNerveChain (..)",
      "CosheafChainCell (..)",
      "CosheafChainBasisTable (..)",
      "PreparedFiniteCosheafChain (..)",
      "CosheafChainFailure (..)",
      "prepareFiniteCosheafChainFromPreparedSupport",
      "prepareFiniteCosheafChainFromSupportPlan",
      "cosheafChainBasisAtDegree",
      "cosheafChainCellsAtDegree",
      "cosheafChainCellByBasisIndex",
      "cosheafChainBasisIndexOf",
      "cosheafChainBasisKeyAt",
      "cosheafBoundaryIncidenceAt",
      "verifyCosheafBoundaryNilpotence",
      "LinearCosheafChainSpec (..)",
      "CosheafBoundaryProvenance (..)",
      "LinearCosheafBoundaryFailure (..)",
      "LinearCosheafChainFailure (..)",
      "prepareLinearCosheafChainFromSupportPlan",
      "prepareLinearCosheafChainFromLinearCosheafWithSupportPlan",
      "prepareLinearCosheafChainFromLinearCosheaf",
      "CoverIntersectionCell (..)",
      "CoverFace (..)",
      "CoverNervePlan (..)",
      "CoverChainSpec (..)",
      "CoverBoundaryProvenance",
      "CoverChainFailure (..)",
      "coverNervePlanFromEffectiveCoverPlan",
      "prepareCoverCosheafChain",
      "LiftedCosheafChainTerm (..)",
      "CosheafHomologyWitness (..)",
      "CosheafHomologyResult (..)",
      "CosheafHomologyFailure (..)",
      "cosheafIntegralHomology",
      "cosheafIntegralHomologyResult",
      "cosheafIntegralHomologyResultWithRepresentatives",
      "liftCosheafRepresentative",
      "liftCosheafRepresentatives",
      "CosheafH0CellKey (..)",
      "CosheafH0Agreement (..)",
      "CosheafH0ClassAgreement (..)",
      "CosheafH0AgreementReport (..)",
      "CosheafH0Failure (..)",
      "verifyCosheafH0RankAgreement",
      "verifyCosheafH0ClassAgreement",
      "verifyPreparedCosheafH0ClassAgreement",
      "degreeZeroBoundaryEquivalence",
      "LinearCosheafHomologyArtifact (..)",
      "LinearCosheafHomologyFailure (..)",
      "linearCosheafHomology",
      "homologyGroupsByDegree",
      "CoverHomologyArtifact (..)",
      "CoverHomologyFailure (..)",
      "coverHomology",
      "coverHomologyOfPreparedChain",
      "TropicalPDegree (..)",
      "TropicalBidegree (..)",
      "TropicalCellKey (..)",
      "TropicalCell (..)",
      "TropicalTangentBasis (..)",
      "TropicalFace (..)",
      "TropicalCellularComplex (..)",
      "TropicalCoefficientWitness (..)",
      "TropicalBoundaryProvenance",
      "TropicalCoefficientFailure (..)",
      "TropicalHomologyFailure (..)",
      "TropicalHomologyArtifact (..)",
      "tropicalTangentRank",
      "tropicalCellularBoundaryAlgebra",
      "tropicalCoefficientChain",
      "tropicalHomology",
      "tropicalHomologyWithBackend",
      "tropicalHomologyGF2",
      "CoverCosectionRepresentative (..)",
      "CoverCosheafCoequalizer (..)",
      "CoverCosheafFailure (..)",
      "coverCosheafCoequalizer",
      "LocalBasisKey (..)",
      "LinearCostalk (..)",
      "linearCostalkDimension",
      "LinearCorestriction (..)",
      "LinearCosheaf",
      "lcosSite",
      "lcosSiteIndex",
      "lcosCostalks",
      "lcosCorestrictions",
      "LinearCosheafAlgebra (..)",
      "LinearCosheafFailure (..)",
      "mkLinearCosheaf",
      "linearCostalkAt",
      "linearCostalkAtObjectKey",
      "linearCorestrictionFor",
      "linearCosheafCorestrictions",
      "SupportCarrier",
      "scHasAny",
      "scContains",
      "supportCarrierItems",
      "supportCarrierCount",
      "CosheafSupportCertificate (..)",
      "CosheafSupportPlan",
      "cspMaxDegree",
      "cspObjects",
      "cspMorphisms",
      "cspCostalkKeys",
      "cspNerveRows",
      "cspChainCells",
      "cspFootprintMeasures",
      "cspCertificate",
      "CosheafSupportFailure (..)",
      "PreparedCosheafSupport (..)",
      "cosheafSupportPlanFromKeys",
      "prepareCosheafSupport",
      "fullFiniteCosheafChainPreparedSupport",
      "supportedCorestrictions",
      "validateCosheafSupportPlan",
      "fullFiniteCosheafChainSupportPlan",
      "h0SupportPlan",
      "h0PreparedSupport",
      "homologyWindowSupportPlan",
      "homologyWindowPreparedSupport",
      "LinearCosheafSupportCertificate (..)",
      "LinearCosheafSupportPlan",
      "lcspCells",
      "lcspFaces",
      "lcspCoordinates",
      "LinearCosheafSupportFailure (..)",
      "linearCosheafSupportPlanFromLists",
      "validateLinearCosheafSupportPlan",
      "fullLinearCosheafSupportPlan",
      "CosheafMorsePolicy (..)",
      "defaultCosheafMorsePolicy",
      "CosheafMorsePair (..)",
      "CosheafMorseMatching (..)",
      "CosheafMorseHomologyAgreement (..)",
      "MorseProvenance (..)",
      "CosheafMorseReduction (..)",
      "CosheafMorseFailure (..)",
      "morseReduceCosheafChain",
      "CosheafBlockMorsePolicy (..)",
      "defaultWholeCostalkBlockSchurPolicy",
      "CosheafBlockPivotPlan (..)",
      "CosheafResidualBlock (..)",
      "BlockSchurMorseProvenance (..)",
      "CosheafBlockMorseReduction (..)",
      "CosheafBlockMorseFailure (..)",
      "blockSchurReduceCosheafChain",
      "blockSchurReduceCosheafChainWithPlan",
      "MinPlusWeight (..)",
      "TropicalCostParseFailure (..)",
      "minPlusZero",
      "minPlusOne",
      "minPlusFinite",
      "minPlusInfinity",
      "minPlusAdd",
      "minPlusMul",
      "minPlusSum",
      "minPlusProduct",
      "minPlusFromRational",
      "parseMinPlusWeight",
      "TropicalTransition (..)",
      "TropicalWeightedTransition (..)",
      "TropicalCostModel (..)",
      "TropicalCostTable (..)",
      "TropicalClassChoice (..)",
      "TropicalCosectionPlan (..)",
      "TropicalCosectionFailure (..)",
      "compileTropicalCostTableFromSupportPlan",
      "planTropicalCosections",
      "CosheafMorphismKey (..)",
      "IndexedCosheafMorphism (..)",
      "CosheafSiteIndex",
      "CosheafSiteIndexFailure (..)",
      "buildCosheafSiteIndex",
      "cosheafSiteObjectIndex",
      "cosheafIndexedMorphisms",
      "cosheafComposableMorphismPairs",
      "cosheafCompositionValidationBasis",
      "cosheafMorphismKeyOf"
    ]

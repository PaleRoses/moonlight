{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Sheaf.Bench.Operation
  ( operationBenchmarks,
  )
where

import Control.DeepSeq (NFData (..))
import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.IntMap.Strict qualified as IntMap
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.Unboxed qualified as Unboxed
import Moonlight.Homology qualified as Homology
import Moonlight.Sheaf
import Moonlight.Sheaf.Internal.PublicModel (MatchingFamily (..))
import Moonlight.Sheaf.Presentation
  ( CompiledPresentation,
    FinitePresheafMorphism,
    PresentedRestrictionFailure,
    Presentation,
    componentAt,
    compilePresentation,
    declareCell,
    declareComposition,
    declareCover,
    declareFiber,
    declareIdentityMorphism,
    declareMorphism,
    declarePresheaf,
    declareRefinement,
    finitePresheafMorphismComponents,
    presentationMorphismAt,
    presentationPresheafAt,
    presentationSite,
    restrictPresentedPresheaf,
    restricts,
  )
import Moonlight.Sheaf.Stalk
import Moonlight.Sheaf.Cochain.Coboundary
  ( CoboundarySpec (..),
    RankOneCoboundaryPlan,
    applyRankOneCoboundaryPlanDense,
    buildRankOneCoboundaryComplex,
    checkCoboundaryNilpotence,
    prepareRankOneCoboundaryPlan,
  )
import Moonlight.Sheaf.Image.ContextGalois
  ( ContextGaloisMap,
    checkContextImageAdjunction,
    extendFiniteContextPresheaf,
    mkContextGaloisMap,
    restrictFiniteContextPresheaf,
  )
import Moonlight.Sheaf.Image.Adjunction
  ( finiteImageAdjunctionSatisfied,
  )
import Moonlight.Sheaf.Image.Direct
  ( pushforwardFinitePresheaf,
  )
import Moonlight.Sheaf.Image.Restrict
  ( pullbackFinitePresheaf,
  )
import Moonlight.Sheaf.Kernel.Basis (mkSheafBasis)
import Moonlight.Sheaf.Operator.GradedComplex
import Moonlight.Homology
  ( HomologicalDegree (..),
  )
import Moonlight.Sheaf.Presheaf.Enumeration
  ( FiniteEnumerationBudget (..),
  )
import Moonlight.Sheaf.Presheaf.Finite
  ( FinitePresheaf (..),
    finiteFiberAt,
    finiteFiberValues,
    mkFinitePresheaf,
    validateFinitePresheafLaws,
  )
import Moonlight.Sheaf.Presheaf.Morphism
  ( composeAlignedFinitePresheafMorphisms,
    identityFinitePresheafMorphism,
    mkFinitePresheafMorphism,
  )
import Moonlight.Sheaf.Presheaf.Plus
  ( PlusConstruction,
    plusAsFinitePresheaf,
    plusConstruction,
    plusFibers,
    plusFiberRepresentatives,
  )
import Moonlight.Sheaf.Presheaf.Core (CompiledRestriction (..))
import Moonlight.Sheaf.Presheaf.Stalk.Colimit
  ( NeighborhoodFilter (..),
    colimitStalkRepresentatives,
    finiteColimitStalkAt,
  )
import Moonlight.Sheaf.Sheaf.Gluing
  ( certifyMatchingFamilyCompatibilityFirstObstruction,
    pairwiseCompatibilityFailures,
  )
import Moonlight.Sheaf.Kernel.Basis (basisCells)
import Moonlight.Sheaf.Section.ObjectIndex (mkObjectIndex)
import Moonlight.Sheaf.Section.Morphism (RestrictionParts (..))
import Moonlight.Sheaf.Section.Restriction (RestrictionIndex, buildRestrictionIndex)
import Moonlight.Sheaf.Sheafification.Finite
  ( Sheafification (..),
    associatedSheafificationReport,
    sheafConditionReportAccepted,
    sheafificationUnitEvidence,
    sheafifyFinitePresheaf,
  )
import Moonlight.Sheaf.Site.CoverBasis.Finite
  ( FiniteCoverBasis,
    mkFiniteCoverBasis,
  )
import Moonlight.Sheaf.Site.Construction.Map
  ( ContinuousSiteMap,
    FiniteSiteMap,
    mkContinuousSiteMap,
    mkFiniteSiteMap,
  )
import Test.Tasty.Bench (Benchmark, bench, bgroup, env, nf)
import Moonlight.FiniteLattice
  ( ContextLattice,
    compileContextLattice,
    contextOrderDecl
  )

operationBenchmarks :: Benchmark
operationBenchmarks =
  env setupOperationBenchEnv $ \benchEnv ->
    bgroup
      "public"
      [ bgroup
          "finite-sheaf-operations"
          [ benchOperation benchEnv "restriction/stalk-map" runRestriction,
            benchOperation benchEnv "compatibility/overlap-compatible" runCompatibleOverlap,
            benchOperation benchEnv "compatibility/overlap-obstructed" runObstructedOverlap,
            benchOperation benchEnv "compatibility/overlap-all-failures-compatible-multi" runMultiOverlapAllFailuresCompatible,
            benchOperation benchEnv "compatibility/overlap-all-failures-first-obstructed-multi" runMultiOverlapAllFailuresFirstObstructed,
            benchOperation benchEnv "compatibility/overlap-all-failures-last-obstructed-multi" runMultiOverlapAllFailuresLastObstructed,
            benchOperation benchEnv "compatibility/overlap-first-obstruction-compatible-multi" runMultiOverlapFirstObstructionCompatible,
            benchOperation benchEnv "compatibility/overlap-first-obstruction-first-obstructed-multi" runMultiOverlapFirstObstructionFirstObstructed,
            benchOperation benchEnv "compatibility/overlap-first-obstruction-last-obstructed-multi" runMultiOverlapFirstObstructionLastObstructed,
            benchOperation benchEnv "compatibility/overlap-first-obstruction-everywhere-obstructed-multi" runMultiOverlapFirstObstructionEverywhereObstructed,
            benchOperation benchEnv "gluing/descent-compatible-cover" runGluingDescent,
            benchOperation benchEnv "extension/global-section-compatible" runExtensionAccepted,
            benchOperation benchEnv "extension/global-section-obstructed" runExtensionObstructed,
            benchOperation benchEnv "stalk-germ/section-read" runSectionStalkRead,
            benchOperation benchEnv "stalk-germ/finite-colimit" runFiniteColimitStalk,
            benchOperation benchEnv "local-to-global/certification-compatible" runCertificationAccepted,
            benchOperation benchEnv "local-to-global/certification-obstructed" runCertificationObstructed,
            benchOperation benchEnv "local-to-global/section-verdict-compatible" runSectionVerdictCompatible,
            benchOperation benchEnv "local-to-global/section-verdict-obstructed" runSectionVerdictObstructed,
            benchOperation benchEnv "pullback/inverse-image-identity" runPullbackIdentity,
            benchOperation benchEnv "pushforward/direct-image-identity" runPushforwardIdentity,
            bgroup
              "adjunction"
              [ benchOperation benchEnv "context-galois-report" runContextGaloisAdjunctionReport,
                benchOperation benchEnv "context-galois-restrict" runContextGaloisRestrict,
                benchOperation benchEnv "context-galois-extend" runContextGaloisExtend,
                benchOperation benchEnv "sheafification-unit-evidence" runSheafificationUnitEvidence
              ],
            bgroup
              "hom-exact"
              [ benchOperation benchEnv "finite-presheaf-natural-identity" runFinitePresheafNaturalIdentity,
                benchOperation benchEnv "sparse-homology-basis-h0" runSparseHomologyBasisH0,
                benchOperation benchEnv "sparse-cohomology-basis-h0" runSparseCohomologyBasisH0,
                benchOperation benchEnv "integral-homology-groups" runIntegralHomologyGroups
              ],
            bgroup
              "authoring/cold"
              [ benchOperation benchEnv "presentation-compile" runPresentationCompile,
                benchOperation benchEnv "presentation-restriction" runPresentationRestriction,
                benchOperation benchEnv "presentation-checked-restriction" runPresentationCheckedRestriction,
                benchOperation benchEnv "finite-presheaf-construction" runFinitePresheafConstruction,
                benchOperation benchEnv "morphism-composition-aligned" runAlignedMorphismComposition
              ],
            bgroup
              "sheafification/hot"
              [ benchOperation benchEnv "finite-plus-plus" runSheafification,
                benchOperation benchEnv "first-plus" runSheafificationFirstPlus,
                benchOperation benchEnv "first-plus-as-finite-presheaf" runSheafificationFirstPlusAsFinitePresheaf,
                benchOperation benchEnv "second-plus" runSheafificationSecondPlus,
                benchOperation benchEnv "second-plus-as-finite-presheaf" runSheafificationSecondPlusAsFinitePresheaf
              ],
            bgroup
              "sheafification/cold-audit"
              [ benchOperation benchEnv "finite-laws" runSheafificationFiniteLawAudit,
                benchOperation benchEnv "associated-report" runSheafificationAssociatedReport
              ],
            bgroup
              "cech-complex"
              [ benchOperation benchEnv "rank-one-dense-coboundary-prepared" runCechDenseCoboundary,
                benchOperation benchEnv "rank-one-complex-project-prepared" runCechPreparedComplexProjection,
                benchOperation benchEnv "nilpotence-prepared" runCechNilpotenceCertification,
                benchOperation benchEnv "cold-build-rank-one-complex-audit" runCechRankOneComplexBuild
              ]
          ]
      ]

benchOperation :: OperationBenchEnv -> String -> (OperationBenchEnv -> OperationMeasure) -> Benchmark
benchOperation benchEnv label runOperation =
  bench label (nf runOperation benchEnv)

data BenchCell
  = BenchGlobal
  | BenchLeft
  | BenchRight
  | BenchOverlap
  deriving stock (Eq, Ord, Show)

newtype BenchStalk = BenchStalk Int
  deriving stock (Eq, Ord, Show)

data MultiBenchCell
  = MultiBenchGlobal
  | MultiBenchA
  | MultiBenchB
  | MultiBenchC
  | MultiBenchAB
  | MultiBenchAC
  | MultiBenchBC
  | MultiBenchABC
  deriving stock (Eq, Ord, Show)

data MultiBenchStalk = MultiBenchStalk
  { mbsAB :: !Int,
    mbsAC :: !Int,
    mbsBC :: !Int
  }
  deriving stock (Eq, Show)

data MultiBenchMismatch = MultiBenchMismatch !MultiBenchStalk !MultiBenchStalk
  deriving stock (Eq, Show)

data BenchGluingFailure
  = BenchEmptyFamily
  | BenchNonConstantFamily ![BenchStalk]
  deriving stock (Eq, Show)

data BenchVoidRestrictionFailure
  deriving stock (Eq, Show)

type BenchSite = FiniteMeetSite BenchCell

type MultiBenchSite = FiniteMeetSite MultiBenchCell

type BenchMismatch = DiscreteMismatch BenchStalk

type BenchRepair = DiscreteRepairObstruction BenchStalk

type BenchPresheaf = FinitePresheaf BenchSite Int () BenchVoidRestrictionFailure

type BenchPresentedPresheaf =
  FinitePresheaf
    BenchSite
    BenchStalk
    BenchMismatch
    (PresentedRestrictionFailure BenchCell)

type BenchPresheafMorphism =
  FinitePresheafMorphism
    BenchSite
    Int
    Int
    ()
    ()
    BenchVoidRestrictionFailure
    BenchVoidRestrictionFailure

data OperationBenchEnv = OperationBenchEnv
  { obeSite :: !BenchSite,
    obePreparedSite :: !(PreparedSite BenchSite),
    obeCoverPlan :: !(PreparedCover BenchSite),
    obeOverlapToLeft :: !(CheckedMorphism BenchCell (FiniteMeetMorphism BenchCell)),
    obeCompatibleFamily :: !(MatchingFamily BenchSite BenchStalk),
    obeObstructedFamily :: !(MatchingFamily BenchSite BenchStalk),
    obeMultiSite :: !MultiBenchSite,
    obeMultiCompatibleFamily :: !(MatchingFamily MultiBenchSite MultiBenchStalk),
    obeMultiFirstObstructedFamily :: !(MatchingFamily MultiBenchSite MultiBenchStalk),
    obeMultiLastObstructedFamily :: !(MatchingFamily MultiBenchSite MultiBenchStalk),
    obeMultiEverywhereObstructedFamily :: !(MatchingFamily MultiBenchSite MultiBenchStalk),
    obeCompatibleSlots :: !(Vector BenchStalk),
    obeCompatibleSection :: !(Section BenchSite BenchStalk),
    obeObstructedSection :: !(Section BenchSite BenchStalk),
    obePresheaf :: !BenchPresheaf,
    obePresentedPresheaf :: !BenchPresentedPresheaf,
    obeIdentityMorphism :: !BenchPresheafMorphism,
    obeNonSheafPresheaf :: !BenchPresheaf,
    obeSheafification :: !(Sheafification BenchSite Int () BenchVoidRestrictionFailure),
    obeFiniteSiteMap :: !(FiniteSiteMap BenchSite BenchSite),
    obeContinuousSiteMap :: !(ContinuousSiteMap BenchSite BenchSite),
    obeContextGaloisMap :: !(ContextGaloisMap BenchCell BenchCell),
    obeCoverBasis :: !(FiniteCoverBasis BenchSite),
    obeNeighborhoodFilter :: !(NeighborhoodFilter () BenchCell),
    obeCechFixture :: !CechFixture,
    obeHomologyPathComplex :: !(Homology.FiniteChainComplex Integer)
  }

instance NFData OperationBenchEnv where
  rnf benchEnv =
    rnf
      ( length (siteObjects (obeSite benchEnv))
          + length (siteObjects (obeMultiSite benchEnv))
          + either (const 0) length (preparedCovers (obePreparedSite benchEnv) BenchGlobal)
          + finitePresheafCardinality (obePresheaf benchEnv)
          + finitePresheafCardinality (obePresentedPresheaf benchEnv)
          + finitePresheafMorphismCardinality (obeIdentityMorphism benchEnv)
          + finitePresheafCardinality (obeNonSheafPresheaf benchEnv)
          + finitePresheafCardinality (sheafificationAssociated (obeSheafification benchEnv))
          + Homology.sourceCardinality
            (Homology.incidenceMatrixAt (obeHomologyPathComplex benchEnv) (Homology.HomologicalDegree 1))
          + Unboxed.length (cechSourceVector (obeCechFixture benchEnv))
      )

data OperationMeasure
  = OperationMeasured !Int
  | OperationObstructed !String
  deriving stock (Eq, Show)

instance NFData OperationMeasure where
  rnf measure =
    case measure of
      OperationMeasured value -> rnf value
      OperationObstructed message -> rnf message

benchCells :: [BenchCell]
benchCells =
  [BenchGlobal, BenchLeft, BenchRight, BenchOverlap]

benchSiteSpec :: FiniteMeetSiteSpec BenchCell
benchSiteSpec =
  FiniteMeetSiteSpec
    { fmssCells = BenchGlobal :| [BenchLeft, BenchRight, BenchOverlap],
      fmssRefinements =
        Set.fromList
          [ (BenchLeft, BenchGlobal),
            (BenchRight, BenchGlobal),
            (BenchOverlap, BenchLeft),
            (BenchOverlap, BenchRight)
          ],
      fmssCovers =
        Map.singleton BenchGlobal [BenchLeft :| [BenchRight]]
    }

benchAlgebra :: StalkAlgebra (CompiledRestriction BenchSite) BenchStalk BenchMismatch BenchRepair
benchAlgebra =
  discreteStalkAlgebra

multiBenchSiteSpec :: FiniteMeetSiteSpec MultiBenchCell
multiBenchSiteSpec =
  FiniteMeetSiteSpec
    { fmssCells =
        MultiBenchGlobal
          :| [ MultiBenchA,
               MultiBenchB,
               MultiBenchC,
               MultiBenchAB,
               MultiBenchAC,
               MultiBenchBC,
               MultiBenchABC
             ],
      fmssRefinements =
        Set.fromList
          [ (MultiBenchA, MultiBenchGlobal),
            (MultiBenchB, MultiBenchGlobal),
            (MultiBenchC, MultiBenchGlobal),
            (MultiBenchAB, MultiBenchA),
            (MultiBenchAB, MultiBenchB),
            (MultiBenchAC, MultiBenchA),
            (MultiBenchAC, MultiBenchC),
            (MultiBenchBC, MultiBenchB),
            (MultiBenchBC, MultiBenchC),
            (MultiBenchABC, MultiBenchAB),
            (MultiBenchABC, MultiBenchAC),
            (MultiBenchABC, MultiBenchBC)
          ],
      fmssCovers = Map.singleton MultiBenchGlobal [MultiBenchA :| [MultiBenchB, MultiBenchC]]
    }

multiBenchAlgebra :: StalkAlgebra (CompiledRestriction MultiBenchSite) MultiBenchStalk MultiBenchMismatch ()
multiBenchAlgebra =
  StalkAlgebra
    { saRestrictionKernel = multiBenchRestrictionKernel,
      saMismatches = \left right -> [MultiBenchMismatch left right | left /= right],
      saMerge = \left _right -> Right left,
      saRepair = const (Left ()),
      saNormalize = id
    }

multiBenchRestrictionKernel :: CompiledRestriction MultiBenchSite -> StalkRestrictionKernel MultiBenchStalk
multiBenchRestrictionKernel (CompiledRestriction _site morphism) =
  case cmSource morphism of
    MultiBenchAB ->
      StalkRestrictionMap (\stalk -> MultiBenchStalk (mbsAB stalk) 0 0)
    MultiBenchAC ->
      StalkRestrictionMap (\stalk -> MultiBenchStalk 0 (mbsAC stalk) 0)
    MultiBenchBC ->
      StalkRestrictionMap (\stalk -> MultiBenchStalk 0 0 (mbsBC stalk))
    _ ->
      StalkRestrictionIdentity

benchGluing :: GluingAlgebra BenchSite BenchStalk BenchGluingFailure
benchGluing =
  GluingAlgebra
    { gaAmalgamate = \_site compatibleFamily ->
        let matchingFamilyValue =
              compatibleMatchingFamilyUnderlying compatibleFamily
         in case Vector.toList (matchingSections matchingFamilyValue) of
              [] ->
                Left (GluingRejected BenchEmptyFamily)
              firstStalk : remainingStalks
                | all (== firstStalk) remainingStalks ->
                    Right firstStalk
                | otherwise ->
                    Left (GluingRejected (BenchNonConstantFamily (firstStalk : remainingStalks)))
    }

setupOperationBenchEnv :: IO OperationBenchEnv
setupOperationBenchEnv =
  either (ioError . userError) pure operationBenchEnv

operationBenchEnv :: Either String OperationBenchEnv
operationBenchEnv = do
  site <- showSuccess (mkFiniteMeetSite benchSiteSpec)
  preparedSite <- showSuccess (compile (siteSpec site))
  coverPlan <- singleCoverPlan preparedSite BenchGlobal
  overlapToLeft <- note "missing overlap-to-left restriction" (finiteMeetMorphism site BenchOverlap BenchLeft)
  compatibleSectionValue <- totalSectionOf preparedSite (constantEntries 5)
  obstructedSection <-
    totalSectionOf
      preparedSite
      ( Map.fromList
          [ (BenchGlobal, BenchStalk 5),
            (BenchLeft, BenchStalk 5),
            (BenchRight, BenchStalk 6),
            (BenchOverlap, BenchStalk 5)
          ]
      )
  compatibleFamily <-
    showSuccess
      (matching coverPlan (Vector.fromList [BenchStalk 7, BenchStalk 7]))
  obstructedFamily <-
    showSuccess
      (matching coverPlan (Vector.fromList [BenchStalk 7, BenchStalk 8]))
  multiSite <- showSuccess (mkFiniteMeetSite multiBenchSiteSpec)
  multiPreparedSite <- showSuccess (compile (siteSpec multiSite))
  multiCoverPlan <- singleCoverPlan multiPreparedSite MultiBenchGlobal
  multiCompatibleFamily <- showSuccess (matching multiCoverPlan multiBenchCompatibleSections)
  multiFirstObstructedFamily <- showSuccess (matching multiCoverPlan multiBenchFirstObstructedSections)
  multiLastObstructedFamily <- showSuccess (matching multiCoverPlan multiBenchLastObstructedSections)
  multiEverywhereObstructedFamily <- showSuccess (matching multiCoverPlan multiBenchEverywhereObstructedSections)
  coverBasis <- showSuccess (mkFiniteCoverBasis site)
  presheaf <- benchIntPresheaf site (benchCellMap (const [0, 1]))
  (_presentationResult, compiledPresentation) <-
    showSuccess (compilePresentation (benchPresentation 2))
  presentedPresheaf <-
    note
      "compiled presentation omitted its declared presheaf"
      (presentationPresheafAt BenchPresentedPresheaf compiledPresentation)
  let identityMorphism = identityFinitePresheafMorphism presheaf
  nonSheafPresheaf <- benchIntPresheaf site (benchCellMap (\cell -> [7 | cell /= BenchGlobal]))
  sheafificationValue <- showSuccess (sheafifyFinitePresheaf (FiniteEnumerationBudget Nothing) coverBasis nonSheafPresheaf)
  finiteSiteMap <- identityFiniteSiteMap site
  continuousSiteMap <- identityContinuousSiteMap site finiteSiteMap coverBasis
  contextLattice <- benchContextLattice
  contextGaloisMap <- showSuccess (mkContextGaloisMap contextLattice contextLattice site site id id)
  cechFixtureValue <- cechFixture 256
  homologyPathComplex <- pathChainComplex 32
  pure
    OperationBenchEnv
      { obeSite = site,
        obePreparedSite = preparedSite,
        obeCoverPlan = coverPlan,
        obeOverlapToLeft = overlapToLeft,
        obeCompatibleFamily = compatibleFamily,
        obeObstructedFamily = obstructedFamily,
        obeMultiSite = multiSite,
        obeMultiCompatibleFamily = multiCompatibleFamily,
        obeMultiFirstObstructedFamily = multiFirstObstructedFamily,
        obeMultiLastObstructedFamily = multiLastObstructedFamily,
        obeMultiEverywhereObstructedFamily = multiEverywhereObstructedFamily,
        obeCompatibleSlots = compatibleSlots coverPlan,
        obeCompatibleSection = compatibleSectionValue,
        obeObstructedSection = obstructedSection,
        obePresheaf = presheaf,
        obePresentedPresheaf = presentedPresheaf,
        obeIdentityMorphism = identityMorphism,
        obeNonSheafPresheaf = nonSheafPresheaf,
        obeSheafification = sheafificationValue,
        obeFiniteSiteMap = finiteSiteMap,
        obeContinuousSiteMap = continuousSiteMap,
        obeContextGaloisMap = contextGaloisMap,
        obeCoverBasis = coverBasis,
        obeNeighborhoodFilter = allObjectsNeighborhood,
        obeCechFixture = cechFixtureValue,
        obeHomologyPathComplex = homologyPathComplex
      }

benchContextLattice :: Either String (ContextLattice BenchCell)
benchContextLattice =
  showSuccess $
    compileContextLattice
      (Set.fromList benchCells)
      ( contextOrderDecl
          BenchOverlap
          BenchGlobal
          [ (BenchGlobal, BenchLeft),
            (BenchGlobal, BenchRight),
            (BenchLeft, BenchOverlap),
            (BenchRight, BenchOverlap)
          ]
      )

singleCoverPlan ::
  (Site site, Show (SiteObject site)) =>
  PreparedSite site ->
  SiteObject site ->
  Either String (PreparedCover site)
singleCoverPlan preparedSite target =
  case preparedCovers preparedSite target of
    Right [coverPlan] -> Right coverPlan
    Right coverPlans -> Left ("expected one cover plan at " <> show target <> ", received " <> show (length coverPlans))
    Left refusal -> Left ("cover lookup refused at " <> show target <> ": " <> show refusal)

totalSectionOf :: PreparedSite BenchSite -> Map BenchCell BenchStalk -> Either String (Section BenchSite BenchStalk)
totalSectionOf preparedSite =
  showSuccess . section preparedSite

constantEntries :: Int -> Map BenchCell BenchStalk
constantEntries value =
  benchCellMap (const (BenchStalk value))

benchCellMap :: (BenchCell -> value) -> Map BenchCell value
benchCellMap valueAt =
  Map.fromList (fmap (\cell -> (cell, valueAt cell)) benchCells)

benchIntPresheaf :: BenchSite -> Map BenchCell [Int] -> Either String BenchPresheaf
benchIntPresheaf site fibers =
  showSuccess $
    mkFinitePresheaf
      site
      (\_morphism value -> Right value)
      (\_object leftValue rightValue -> [() | leftValue /= rightValue])
      (\_object value -> value)
      fibers

identityFiniteSiteMap :: BenchSite -> Either String (FiniteSiteMap BenchSite BenchSite)
identityFiniteSiteMap site =
  showSuccess $
    mkFiniteSiteMap
      site
      site
      (identityEntries (siteObjects site))
      (identityEntries (morphismUniverse site))

identityContinuousSiteMap :: BenchSite -> FiniteSiteMap BenchSite BenchSite -> FiniteCoverBasis BenchSite -> Either String (ContinuousSiteMap BenchSite BenchSite)
identityContinuousSiteMap _site siteMapValue basis =
  showSuccess (mkContinuousSiteMap basis basis siteMapValue)

identityEntries :: Ord value => [value] -> Map value value
identityEntries =
  Map.fromList . fmap (\value -> (value, value))

morphismUniverse :: (Site site, Ord (SiteMorphism site)) => site -> [CheckedMorphism (SiteObject site) (SiteMorphism site)]
morphismUniverse site =
  Set.toAscList . Set.fromList $ siteMorphisms site <> fmap (identityAt site) (siteObjects site)

multiBenchCompatibleSections :: Vector MultiBenchStalk
multiBenchCompatibleSections =
  Vector.fromList
    [ MultiBenchStalk 1 2 0,
      MultiBenchStalk 1 0 3,
      MultiBenchStalk 0 2 3
    ]

multiBenchFirstObstructedSections :: Vector MultiBenchStalk
multiBenchFirstObstructedSections =
  Vector.fromList
    [ MultiBenchStalk 10 2 0,
      MultiBenchStalk 11 0 3,
      MultiBenchStalk 0 2 3
    ]

multiBenchLastObstructedSections :: Vector MultiBenchStalk
multiBenchLastObstructedSections =
  Vector.fromList
    [ MultiBenchStalk 1 2 0,
      MultiBenchStalk 1 0 30,
      MultiBenchStalk 0 2 31
    ]

multiBenchEverywhereObstructedSections :: Vector MultiBenchStalk
multiBenchEverywhereObstructedSections =
  Vector.fromList
    [ MultiBenchStalk 10 20 0,
      MultiBenchStalk 11 0 30,
      MultiBenchStalk 0 21 31
    ]

compatibleSlots :: PreparedCover BenchSite -> Vector BenchStalk
compatibleSlots _coverPlan =
  Vector.fromList [BenchStalk 11, BenchStalk 11]

allObjectsNeighborhood :: NeighborhoodFilter () BenchCell
allObjectsNeighborhood =
  NeighborhoodFilter
    { neighborhoodPoint = (),
      neighborhoodContains = \() _object -> True
    }

runRestriction :: OperationBenchEnv -> OperationMeasure
runRestriction benchEnv =
  let BenchStalk value = restrictStalk benchAlgebra (CompiledRestriction (obeSite benchEnv) (obeOverlapToLeft benchEnv)) (BenchStalk 42)
   in OperationMeasured value

runCompatibleOverlap :: OperationBenchEnv -> OperationMeasure
runCompatibleOverlap benchEnv =
  OperationMeasured (length (pairwiseCompatibilityFailures benchAlgebra (obeSite benchEnv) (matchingFamilyRawInternal (obeCompatibleFamily benchEnv))))

runObstructedOverlap :: OperationBenchEnv -> OperationMeasure
runObstructedOverlap benchEnv =
  OperationMeasured (length (pairwiseCompatibilityFailures benchAlgebra (obeSite benchEnv) (matchingFamilyRawInternal (obeObstructedFamily benchEnv))))

runMultiOverlapAllFailuresCompatible :: OperationBenchEnv -> OperationMeasure
runMultiOverlapAllFailuresCompatible benchEnv =
  OperationMeasured (multiOverlapFailureCount benchEnv (obeMultiCompatibleFamily benchEnv))

runMultiOverlapAllFailuresFirstObstructed :: OperationBenchEnv -> OperationMeasure
runMultiOverlapAllFailuresFirstObstructed benchEnv =
  OperationMeasured (multiOverlapFailureCount benchEnv (obeMultiFirstObstructedFamily benchEnv))

runMultiOverlapAllFailuresLastObstructed :: OperationBenchEnv -> OperationMeasure
runMultiOverlapAllFailuresLastObstructed benchEnv =
  OperationMeasured (multiOverlapFailureCount benchEnv (obeMultiLastObstructedFamily benchEnv))

runMultiOverlapFirstObstructionCompatible :: OperationBenchEnv -> OperationMeasure
runMultiOverlapFirstObstructionCompatible benchEnv =
  OperationMeasured (multiFirstObstructionWeight benchEnv (obeMultiCompatibleFamily benchEnv))

runMultiOverlapFirstObstructionFirstObstructed :: OperationBenchEnv -> OperationMeasure
runMultiOverlapFirstObstructionFirstObstructed benchEnv =
  OperationMeasured (multiFirstObstructionWeight benchEnv (obeMultiFirstObstructedFamily benchEnv))

runMultiOverlapFirstObstructionLastObstructed :: OperationBenchEnv -> OperationMeasure
runMultiOverlapFirstObstructionLastObstructed benchEnv =
  OperationMeasured (multiFirstObstructionWeight benchEnv (obeMultiLastObstructedFamily benchEnv))

runMultiOverlapFirstObstructionEverywhereObstructed :: OperationBenchEnv -> OperationMeasure
runMultiOverlapFirstObstructionEverywhereObstructed benchEnv =
  OperationMeasured (multiFirstObstructionWeight benchEnv (obeMultiEverywhereObstructedFamily benchEnv))

multiOverlapFailureCount :: OperationBenchEnv -> MatchingFamily MultiBenchSite MultiBenchStalk -> Int
multiOverlapFailureCount benchEnv =
  length . pairwiseCompatibilityFailures multiBenchAlgebra (obeMultiSite benchEnv) . matchingFamilyRawInternal

multiFirstObstructionWeight :: OperationBenchEnv -> MatchingFamily MultiBenchSite MultiBenchStalk -> Int
multiFirstObstructionWeight benchEnv matchingFamilyValue =
  case certifyMatchingFamilyCompatibilityFirstObstruction multiBenchAlgebra (obeMultiSite benchEnv) (matchingFamilyRawInternal matchingFamilyValue) of
    Right _compatibleFamily ->
      0
    Left _failure ->
      1

runGluingDescent :: OperationBenchEnv -> OperationMeasure
runGluingDescent benchEnv =
  outcome $ do
    amalgamation <-
      showSuccess
        ( first CoverMatchingFamilyConstructionFailed (matching (obeCoverPlan benchEnv) (obeCompatibleSlots benchEnv))
            >>= glue benchAlgebra benchGluing
        )
    pure (stalkWeight (amalgamatedStalk amalgamation))

runExtensionAccepted :: OperationBenchEnv -> OperationMeasure
runExtensionAccepted benchEnv =
  outcome $ do
    _global <- showSuccess (globalSection benchAlgebra (obeCompatibleSection benchEnv))
    pure 1

runExtensionObstructed :: OperationBenchEnv -> OperationMeasure
runExtensionObstructed benchEnv =
  case globalSection benchAlgebra (obeObstructedSection benchEnv) of
    Left (SectionCertificationSemanticallyRejected rejections) -> OperationMeasured (Map.size rejections)
    Left (SectionCertificationInfrastructureFailed (SectionCertificationLookupFailed failure)) ->
      OperationObstructed ("certification lookup failed: " <> show failure)
    Left (SectionCertificationInfrastructureFailed (SectionCertificationRestrictionMissing restrictionId)) ->
      OperationObstructed ("certification restriction missing: " <> show restrictionId)
    Left (SectionCertificationInfrastructureFailed (SectionCertificationStoreFailed failure)) ->
      OperationObstructed ("certification store failed: " <> show failure)
    Left (SectionCertificationInfrastructureFailed (SectionCertificationDescentPreparationFailed failure)) ->
      OperationObstructed ("certification descent preparation failed: " <> show failure)
    Right _global -> OperationObstructed "obstructed section extended globally"

runSectionStalkRead :: OperationBenchEnv -> OperationMeasure
runSectionStalkRead benchEnv =
  outcome (stalkWeight <$> showSuccess (stalkAt BenchOverlap (obeCompatibleSection benchEnv)))

runFiniteColimitStalk :: OperationBenchEnv -> OperationMeasure
runFiniteColimitStalk benchEnv =
  outcome $ do
    stalkValue <- showSuccess (finiteColimitStalkAt (obeNeighborhoodFilter benchEnv) (obePresheaf benchEnv))
    pure (length (colimitStalkRepresentatives stalkValue))

runCertificationAccepted :: OperationBenchEnv -> OperationMeasure
runCertificationAccepted benchEnv =
  case certify benchAlgebra (obeCompatibleSection benchEnv) of
    Right SectionCertified -> OperationMeasured 1
    Right (SectionRejected rejections) -> OperationObstructed ("compatible section rejected at " <> show (Map.size rejections) <> " cells")
    Left (SectionCertificationLookupFailed failure) -> OperationObstructed ("certification lookup failed: " <> show failure)
    Left (SectionCertificationRestrictionMissing restrictionId) -> OperationObstructed ("certification restriction missing: " <> show restrictionId)
    Left (SectionCertificationStoreFailed failure) -> OperationObstructed ("certification store failed: " <> show failure)
    Left (SectionCertificationDescentPreparationFailed failure) -> OperationObstructed ("certification descent preparation failed: " <> show failure)

runCertificationObstructed :: OperationBenchEnv -> OperationMeasure
runCertificationObstructed benchEnv =
  case certify benchAlgebra (obeObstructedSection benchEnv) of
    Right SectionCertified -> OperationObstructed "obstructed section certified"
    Right (SectionRejected rejections) -> OperationMeasured (Map.size rejections)
    Left (SectionCertificationLookupFailed failure) -> OperationObstructed ("certification lookup failed: " <> show failure)
    Left (SectionCertificationRestrictionMissing restrictionId) -> OperationObstructed ("certification restriction missing: " <> show restrictionId)
    Left (SectionCertificationStoreFailed failure) -> OperationObstructed ("certification store failed: " <> show failure)
    Left (SectionCertificationDescentPreparationFailed failure) -> OperationObstructed ("certification descent preparation failed: " <> show failure)

runSectionVerdictCompatible :: OperationBenchEnv -> OperationMeasure
runSectionVerdictCompatible benchEnv =
  case sectionCompatibilityVerdict benchAlgebra (obeCompatibleSection benchEnv) of
    Accepted () ->
      OperationMeasured 1
    Rejected rejection ->
      OperationObstructed ("compatible section verdict rejected: " <> show rejection)

runSectionVerdictObstructed :: OperationBenchEnv -> OperationMeasure
runSectionVerdictObstructed benchEnv =
  case sectionCompatibilityVerdict benchAlgebra (obeObstructedSection benchEnv) of
    Accepted () ->
      OperationObstructed "obstructed section verdict accepted"
    Rejected (SectionCertificationSemanticallyRejected rejections) ->
      OperationMeasured (Map.size rejections)
    Rejected (SectionCertificationInfrastructureFailed (SectionCertificationLookupFailed failure)) ->
      OperationObstructed ("certification lookup failed: " <> show failure)
    Rejected (SectionCertificationInfrastructureFailed (SectionCertificationRestrictionMissing restrictionId)) ->
      OperationObstructed ("certification restriction missing: " <> show restrictionId)
    Rejected (SectionCertificationInfrastructureFailed (SectionCertificationStoreFailed failure)) ->
      OperationObstructed ("certification store failed: " <> show failure)
    Rejected (SectionCertificationInfrastructureFailed (SectionCertificationDescentPreparationFailed failure)) ->
      OperationObstructed ("certification descent preparation failed: " <> show failure)

runPullbackIdentity :: OperationBenchEnv -> OperationMeasure
runPullbackIdentity benchEnv =
  outcome $ do
    pulledPresheaf <- showSuccess (pullbackFinitePresheaf (obeFiniteSiteMap benchEnv) (obePresheaf benchEnv))
    pure (finiteIntPresheafWeight pulledPresheaf)

runPushforwardIdentity :: OperationBenchEnv -> OperationMeasure
runPushforwardIdentity benchEnv =
  outcome $ do
    directImage <-
      showSuccess
        (pushforwardFinitePresheaf (FiniteEnumerationBudget Nothing) (obeContinuousSiteMap benchEnv) (obePresheaf benchEnv))
    pure (finitePresheafCardinality directImage)

runContextGaloisAdjunctionReport :: OperationBenchEnv -> OperationMeasure
runContextGaloisAdjunctionReport benchEnv =
  outcome $ do
    adjunction <-
      showSuccess
        (checkContextImageAdjunction (obeContextGaloisMap benchEnv) (obePresheaf benchEnv) (obePresheaf benchEnv))
    pure (if finiteImageAdjunctionSatisfied adjunction then 1 else 0)

runContextGaloisRestrict :: OperationBenchEnv -> OperationMeasure
runContextGaloisRestrict benchEnv =
  outcome $ do
    restrictedPresheaf <-
      showSuccess
        (restrictFiniteContextPresheaf (obeContextGaloisMap benchEnv) (obePresheaf benchEnv))
    pure (finiteIntPresheafWeight restrictedPresheaf)

runContextGaloisExtend :: OperationBenchEnv -> OperationMeasure
runContextGaloisExtend benchEnv =
  outcome $ do
    extendedPresheaf <-
      showSuccess
        (extendFiniteContextPresheaf (obeContextGaloisMap benchEnv) (obePresheaf benchEnv))
    pure (finiteIntPresheafWeight extendedPresheaf)

runFinitePresheafNaturalIdentity :: OperationBenchEnv -> OperationMeasure
runFinitePresheafNaturalIdentity benchEnv =
  outcome $ do
    morphismValue <-
      showSuccess
        ( mkFinitePresheafMorphism
            (obePresheaf benchEnv)
            (obePresheaf benchEnv)
            (\_object value -> Right value :: Either () Int)
        )
    pure (sumBy length (finitePresheafMorphismComponents morphismValue))

data BenchPresheafName = BenchPresentedPresheaf
  deriving stock (Eq, Ord, Show)

data BenchMorphismName
  = BenchPresentationIdentity
  | BenchPresentationComponent
  | BenchPresentationComposite
  deriving stock (Eq, Ord, Show)

benchPresentation ::
  Int ->
  Presentation
    BenchCell
    BenchPresheafName
    BenchMorphismName
    BenchStalk
    BenchMismatch
    ()
benchPresentation seed = do
  traverse_ declareCell [BenchGlobal, BenchLeft, BenchRight, BenchOverlap]
  declareRefinement BenchLeft BenchGlobal
  declareRefinement BenchRight BenchGlobal
  declareRefinement BenchOverlap BenchLeft
  declareRefinement BenchOverlap BenchRight
  declareCover BenchGlobal (BenchLeft :| [BenchRight])
  declarePresheaf
    BenchPresentedPresheaf
      (\_cell leftStalk rightStalk -> [DiscreteMismatch leftStalk rightStalk | leftStalk /= rightStalk])
      (\_cell stalk -> stalk)
  traverse_
    (\cellValue -> declareFiber BenchPresentedPresheaf cellValue [BenchStalk seed, BenchStalk (seed + 1)])
    [BenchGlobal, BenchLeft, BenchRight, BenchOverlap]
  traverse_
    (\(finer, coarser) -> restricts BenchPresentedPresheaf finer coarser StalkRestrictionIdentity)
    [ (BenchLeft, BenchGlobal),
      (BenchRight, BenchGlobal),
      (BenchOverlap, BenchLeft),
      (BenchOverlap, BenchRight),
      (BenchOverlap, BenchGlobal)
    ]
  declareIdentityMorphism BenchPresentationIdentity BenchPresentedPresheaf
  declareMorphism BenchPresentationComponent BenchPresentedPresheaf BenchPresentedPresheaf
  traverse_
    (\cellValue -> componentAt BenchPresentationComponent cellValue id)
    [BenchGlobal, BenchLeft, BenchRight, BenchOverlap]
  declareComposition
    BenchPresentationComposite
    BenchPresentationIdentity
    BenchPresentationComponent

runPresentationCompile :: OperationBenchEnv -> OperationMeasure
runPresentationCompile benchEnv =
  case compilePresentation (benchPresentation (finitePresheafCardinality (obePresheaf benchEnv))) of
    Left obstruction ->
      OperationObstructed (show obstruction)
    Right (_presentationResult, compiled) ->
      OperationMeasured (compiledPresentationWeight compiled)

runPresentationRestriction :: OperationBenchEnv -> OperationMeasure
runPresentationRestriction benchEnv =
  case
      fpRestrict
        (obePresentedPresheaf benchEnv)
        (obeOverlapToLeft benchEnv)
        (BenchStalk 2)
    of
      Left failure ->
        OperationObstructed (show failure)
      Right (BenchStalk restrictedValue) ->
        OperationMeasured restrictedValue

runPresentationCheckedRestriction :: OperationBenchEnv -> OperationMeasure
runPresentationCheckedRestriction benchEnv =
  case
      restrictPresentedPresheaf
        (obeOverlapToLeft benchEnv)
        (BenchStalk 2)
        (obePresentedPresheaf benchEnv)
    of
      Left failure ->
        OperationObstructed (show failure)
      Right (BenchStalk restrictedValue) ->
        OperationMeasured restrictedValue

compiledPresentationWeight ::
  CompiledPresentation
    BenchCell
    BenchPresheafName
    BenchMorphismName
    BenchStalk
    BenchMismatch ->
  Int
compiledPresentationWeight compiled =
  length (siteObjects (presentationSite compiled))
    + maybe 0 finitePresheafCardinality (presentationPresheafAt BenchPresentedPresheaf compiled)
    + maybe 0 finitePresheafMorphismCardinality (presentationMorphismAt BenchPresentationComposite compiled)

runFinitePresheafConstruction :: OperationBenchEnv -> OperationMeasure
runFinitePresheafConstruction benchEnv =
  outcome
    ( finitePresheafCardinality
        <$> benchIntPresheaf
          (obeSite benchEnv)
          (benchCellMap (const [0, 1]))
    )

runAlignedMorphismComposition :: OperationBenchEnv -> OperationMeasure
runAlignedMorphismComposition benchEnv =
  case
      composeAlignedFinitePresheafMorphisms
        (obeIdentityMorphism benchEnv)
        (obeIdentityMorphism benchEnv)
    of
      Left failure ->
        OperationObstructed (show failure)
      Right composed ->
        OperationMeasured (finitePresheafMorphismCardinality composed)

runSparseHomologyBasisH0 :: OperationBenchEnv -> OperationMeasure
runSparseHomologyBasisH0 benchEnv =
  OperationMeasured
    ( length
        ( Homology.sparseHomologyBasisAt
            (obeHomologyPathComplex benchEnv)
            (Homology.HomologicalDegree 0)
        )
    )

runSparseCohomologyBasisH0 :: OperationBenchEnv -> OperationMeasure
runSparseCohomologyBasisH0 benchEnv =
  OperationMeasured
    ( length
        ( Homology.sparseCohomologyBasisAt
            (obeHomologyPathComplex benchEnv)
            (Homology.HomologicalDegree 0)
        )
    )

runIntegralHomologyGroups :: OperationBenchEnv -> OperationMeasure
runIntegralHomologyGroups benchEnv =
  outcome $ do
    homologyGroups <- showSuccess (Homology.integralHomologyGroupsOf (obeHomologyPathComplex benchEnv))
    pure (length homologyGroups)

runSheafification :: OperationBenchEnv -> OperationMeasure
runSheafification benchEnv =
  outcome $ do
    sheafification <-
      showSuccess
        (sheafifyFinitePresheaf (FiniteEnumerationBudget Nothing) (obeCoverBasis benchEnv) (obeNonSheafPresheaf benchEnv))
    pure (finitePresheafCardinality (sheafificationAssociated sheafification))

runSheafificationFirstPlus :: OperationBenchEnv -> OperationMeasure
runSheafificationFirstPlus benchEnv =
  outcome $ do
    firstPlus <-
      showSuccess
        (plusConstruction (FiniteEnumerationBudget Nothing) (obeCoverBasis benchEnv) (obeNonSheafPresheaf benchEnv))
    pure (plusConstructionWeight firstPlus)

runSheafificationFirstPlusAsFinitePresheaf :: OperationBenchEnv -> OperationMeasure
runSheafificationFirstPlusAsFinitePresheaf benchEnv =
  outcome $ do
    separatedPresheaf <-
      showSuccess
        (plusAsFinitePresheaf (sheafificationFirstPlusConstruction (obeSheafification benchEnv)))
    pure (finitePresheafCardinality separatedPresheaf)

runSheafificationSecondPlus :: OperationBenchEnv -> OperationMeasure
runSheafificationSecondPlus benchEnv =
  outcome $ do
    secondPlus <-
      showSuccess
        (plusConstruction (FiniteEnumerationBudget Nothing) (obeCoverBasis benchEnv) (sheafificationSeparated (obeSheafification benchEnv)))
    pure (plusConstructionWeight secondPlus)

runSheafificationSecondPlusAsFinitePresheaf :: OperationBenchEnv -> OperationMeasure
runSheafificationSecondPlusAsFinitePresheaf benchEnv =
  outcome $ do
    associatedPresheaf <-
      showSuccess
        (plusAsFinitePresheaf (sheafificationSecondPlusConstruction (obeSheafification benchEnv)))
    pure (finitePresheafCardinality associatedPresheaf)

runSheafificationFiniteLawAudit :: OperationBenchEnv -> OperationMeasure
runSheafificationFiniteLawAudit benchEnv =
  outcome $ do
    showSuccess (validateFinitePresheafLaws (sheafificationSeparated (obeSheafification benchEnv)))
    showSuccess (validateFinitePresheafLaws (sheafificationAssociated (obeSheafification benchEnv)))
    pure 1

runSheafificationUnitEvidence :: OperationBenchEnv -> OperationMeasure
runSheafificationUnitEvidence benchEnv =
  outcome $ do
    _unitEvidence <-
      showSuccess
        (sheafificationUnitEvidence (obeCoverBasis benchEnv) (obeSheafification benchEnv))
    pure 1

runSheafificationAssociatedReport :: OperationBenchEnv -> OperationMeasure
runSheafificationAssociatedReport benchEnv =
  outcome $ do
    associatedReport <-
      showSuccess
        (associatedSheafificationReport (FiniteEnumerationBudget Nothing) (obeCoverBasis benchEnv) (obeSheafification benchEnv))
    if sheafConditionReportAccepted associatedReport
      then pure 1
      else Left "associated presheaf failed the sheaf condition"

runCechDenseCoboundary :: OperationBenchEnv -> OperationMeasure
runCechDenseCoboundary benchEnv =
  outcome $ do
    targetVector <-
      showSuccess
        ( applyRankOneCoboundaryPlanDense
            (cechRankOnePlan (obeCechFixture benchEnv))
            (cechSourceVector (obeCechFixture benchEnv))
        )
    pure (Unboxed.sum targetVector)

runCechNilpotenceCertification :: OperationBenchEnv -> OperationMeasure
runCechNilpotenceCertification benchEnv =
  OperationMeasured
    ( if checkCoboundaryNilpotence (cechRankOneComplex (obeCechFixture benchEnv))
        then 1
        else 0
    )

runCechPreparedComplexProjection :: OperationBenchEnv -> OperationMeasure
runCechPreparedComplexProjection benchEnv =
  OperationMeasured
    ( Map.size
        (gradedOperatorsByDegree (cechRankOneComplex (obeCechFixture benchEnv)))
    )

runCechRankOneComplexBuild :: OperationBenchEnv -> OperationMeasure
runCechRankOneComplexBuild benchEnv =
  outcome $ do
    complexValue <-
      showSuccess
        ( buildRankOneCoboundaryComplex
            (const ())
            (\_restriction _sourceStalk _targetStalk -> 1 :: Int)
            (cechSpec0 (obeCechFixture benchEnv))
            (cechSpec1 (obeCechFixture benchEnv))
            (cechRestrictionIndex (obeCechFixture benchEnv))
        )
    pure (Map.size (gradedOperatorsByDegree complexValue))

stalkWeight :: BenchStalk -> Int
stalkWeight (BenchStalk value) =
  value

finiteIntPresheafWeight :: Site site => FinitePresheaf site Int mismatch restrictionFailure -> Int
finiteIntPresheafWeight presheaf =
  sumBy fiberWeight (siteObjects (fpSite presheaf))
  where
    fiberWeight object =
      maybe 0 (sum . finiteFiberValues) (finiteFiberAt object presheaf)

finitePresheafCardinality :: Site site => FinitePresheaf site value mismatch restrictionFailure -> Int
finitePresheafCardinality presheaf =
  sumBy fiberCardinality (siteObjects (fpSite presheaf))
  where
    fiberCardinality object =
      maybe 0 (length . finiteFiberValues) (finiteFiberAt object presheaf)

finitePresheafMorphismCardinality ::
  FinitePresheafMorphism site sourceValue targetValue sourceMismatch targetMismatch sourceRestrictionFailure targetRestrictionFailure ->
  Int
finitePresheafMorphismCardinality =
  sumBy length . finitePresheafMorphismComponents

plusConstructionWeight :: PlusConstruction site value mismatch restrictionFailure -> Int
plusConstructionWeight =
  sumBy (IntMap.size . plusFiberRepresentatives) . Map.elems . plusFibers

outcome :: Either String Int -> OperationMeasure
outcome =
  either OperationObstructed OperationMeasured

showSuccess :: Show failure => Either failure value -> Either String value
showSuccess =
  first show

note :: String -> Maybe value -> Either String value
note failureMessage =
  maybe (Left failureMessage) Right

sumBy :: Foldable foldable => (value -> Int) -> foldable value -> Int
sumBy measure =
  foldl' (\total value -> total + measure value) 0

newtype CechCell = CechCell Int
  deriving stock (Eq, Ord, Show)

data CechFixture = CechFixture
  { cechSpec0 :: !(CoboundarySpec CechCell),
    cechSpec1 :: !(CoboundarySpec CechCell),
    cechRestrictionIndex :: !(RestrictionIndex CechCell (CechCell, CechCell, Int)),
    cechRankOnePlan :: !(RankOneCoboundaryPlan CechCell),
    cechRankOneComplex :: !(GradedComplex CechCell Int),
    cechSourceVector :: !(Unboxed.Vector Int)
  }

cechFixture :: Int -> Either String CechFixture
cechFixture cellCount
  | cellCount <= 0 =
      Left ("Cech benchmark cell count must be positive: " <> show cellCount)
  | otherwise = do
      restrictionIndex <- cechRestrictionIndexOf cellCount
      rankOnePlan <-
        showSuccess
          ( prepareRankOneCoboundaryPlan
              (const ())
              (\_restriction _sourceStalk _targetStalk -> 1 :: Int)
              (cechSpec0Of cellCount)
              restrictionIndex
          )
      rankOneComplex <-
        showSuccess
          ( buildRankOneCoboundaryComplex
              (const ())
              (\_restriction _sourceStalk _targetStalk -> 1 :: Int)
              (cechSpec0Of cellCount)
              (cechSpec1Of cellCount)
              restrictionIndex
          )
      pure
        CechFixture
          { cechSpec0 = cechSpec0Of cellCount,
            cechSpec1 = cechSpec1Of cellCount,
            cechRestrictionIndex = restrictionIndex,
            cechRankOnePlan = rankOnePlan,
            cechRankOneComplex = rankOneComplex,
            cechSourceVector = Unboxed.generate cellCount (+ 1)
          }

cechSpec0Of :: Int -> CoboundarySpec CechCell
cechSpec0Of cellCount =
  CoboundarySpec
    { csDimension = (HomologicalDegree 0),
      csSourceBasis = mkSheafBasis (cechZeroCells cellCount),
      csTargetBasis = mkSheafBasis (cechOneCells cellCount)
    }

cechSpec1Of :: Int -> CoboundarySpec CechCell
cechSpec1Of cellCount =
  CoboundarySpec
    { csDimension = (HomologicalDegree 1),
      csSourceBasis = mkSheafBasis (cechOneCells cellCount),
      csTargetBasis = mkSheafBasis []
    }

cechRestrictionIndexOf :: Int -> Either String (RestrictionIndex CechCell (CechCell, CechCell, Int))
cechRestrictionIndexOf cellCount =
  showSuccess
    ( buildRestrictionIndex
        (mkObjectIndex (basisCells (mkSheafBasis (cechZeroCells cellCount <> cechOneCells cellCount))))
        ( \(sourceCell, targetCell, coefficient) ->
            RestrictionParts
              { partKind = unitIncidenceRestriction,
                partSource = sourceCell,
                partTarget = targetCell,
                partWitness = (sourceCell, targetCell, coefficient)
              }
        )
        (zipWith (\sourceCell targetCell -> (sourceCell, targetCell, 1 :: Int)) (cechOneCells cellCount) (cechZeroCells cellCount))
    )

cechZeroCells :: Int -> [CechCell]
cechZeroCells cellCount =
  fmap CechCell [0 .. cellCount - 1]

cechOneCells :: Int -> [CechCell]
cechOneCells cellCount =
  fmap (CechCell . (+ cellCount)) [0 .. cellCount - 1]

pathChainComplex :: Int -> Either String (Homology.FiniteChainComplex Integer)
pathChainComplex edgeCount
  | edgeCount <= 0 =
      Left ("Homology path benchmark edge count must be positive: " <> show edgeCount)
  | otherwise =
      pathBoundary edgeCount
        >>= \pathBoundaryValue ->
          first show $
            Homology.mkFiniteChainComplexChecked (Homology.HomologicalDegree 1) $
              \degreeValue ->
                case degreeValue of
                  Homology.HomologicalDegree 1 ->
                    pathBoundaryValue
                  Homology.HomologicalDegree 0 ->
                    Homology.emptyBoundaryIncidenceOf (fromIntegral (edgeCount + 1)) 0
                  _ ->
                    Homology.emptyBoundaryIncidence

pathBoundary :: Int -> Either String (Homology.BoundaryIncidence Integer)
pathBoundary edgeCount =
  showSuccess $
    Homology.mkBoundaryIncidence
      (fromIntegral edgeCount)
      (fromIntegral (edgeCount + 1))
      (pathBoundaryEntries edgeCount)

pathBoundaryEntries :: Int -> [Homology.BoundaryEntry Integer]
pathBoundaryEntries edgeCount =
  foldMap edgeBoundaryEntries [0 .. edgeCount - 1]

edgeBoundaryEntries :: Int -> [Homology.BoundaryEntry Integer]
edgeBoundaryEntries edgeIndexValue =
  [ pathBoundaryEntry edgeIndexValue edgeIndexValue (-1),
    pathBoundaryEntry edgeIndexValue (edgeIndexValue + 1) 1
  ]

pathBoundaryEntry :: Int -> Int -> coefficient -> Homology.BoundaryEntry coefficient
pathBoundaryEntry sourceIndexValue targetIndexValue =
  Homology.mkBoundaryEntry (fromIntegral sourceIndexValue) (fromIntegral targetIndexValue)

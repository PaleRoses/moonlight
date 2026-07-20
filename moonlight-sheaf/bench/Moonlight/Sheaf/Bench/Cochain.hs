{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}

module Moonlight.Sheaf.Bench.Cochain
  ( cochainBenchmarks,
  )
where

import Control.Exception (evaluate)
import Control.DeepSeq (NFData (..))
import Control.Monad (foldM)
import Test.Tasty.Bench (Benchmark, Benchmarkable, bench, bgroup, env, nf, whnf)
import Data.Bifunctor (first)
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, mapMaybe)
import Data.Monoid (Any (..))
import Data.Set qualified as Set
import Data.Vector.Unboxed qualified as Unboxed
import System.Environment (lookupEnv)
import Moonlight.Category
  ( FinCat,
    FinGeneratorId (..),
    FinMor,
    FinMorphismId (..),
    FinObjectId (..),
    FinObj,
    allMorphisms,
    allObjects,
    chainMorphisms,
    chainStartObject,
    composeMor,
    finMorSourceId,
    finMorTargetId,
    finMorId,
    finObjId,
    mkFinCat,
  )
import Moonlight.Homology
  ( BasisCellRef,
    BoundaryIncidence,
    FiniteChainComplex,
    FormalMap (..),
    HomologicalDegree (..),
    HomologyFailure,
    RationalSpectralPage,
    SpectralEntry (..),
    boundaryEntries,
    computeRationalSpectralPages,
    emptyBoundaryIncidenceOf,
    entryGroupValue,
    formalCodomainBasis,
    formalDomainBasis,
    formalMatrix,
    freeRank,
    filteredRefinedMorseComplex,
    frmcRefinedMorseComplex,
    incidenceMatrixAt,
    maxHomologicalDegree,
    pageDifferentialMap,
    pageEntryMap,
    pageIndex,
    rationalizeFiniteChainComplex,
    rmcReducedComplex,
    sourceCardinality,
    torsionInvariants,
  )
import Moonlight.Sheaf.Cochain.Coboundary
  ( CoboundaryAssemblyPlan,
    CoboundaryBlockKernel (..),
    CoboundaryEntry,
    CoboundaryIncidencePlan,
    CoboundaryMatrix,
    CoboundarySpec (..),
    RankOneCoboundaryPlan,
    applyCoboundaryAssemblyPlanWithKernel,
    applyCoboundaryIncidencePlan,
    applyRankOneCoboundaryPlan,
    applyRankOneCoboundaryPlanDense,
    buildCoboundary,
    buildRankOneCoboundaryComplex,
    materializeCoboundaryAssemblyPlan,
    materializeCoboundaryIncidencePlan,
    materializeCoboundaryIncidence,
    materializeRankOneCoboundaryDifferential,
    materializeRankOneCoboundaryIncidence,
    mkCoboundaryEntry,
    mkCoboundaryMatrix,
    prepareCoboundaryAssemblyPlan,
    prepareCoboundaryIncidencePlan,
    prepareCoboundaryIncidencePlanWithKernel,
    prepareRankOneCoboundaryPlan,
  )
import Moonlight.Sheaf.Cochain.Cohomology
  ( cochainSupportWindow,
  )
import Moonlight.Sheaf.Cochain.Laplacian
  ( LaplacianKind (..),
    PackedSheafLaplacian,
    RestrictionGraphPlanKind (..),
    SheafLaplacian,
    buildPackedTarskiLaplacianFromPlan,
    buildTarskiLaplacianFromPlan,
    laplacianCoordinateVectorWithCellCoordinates,
    laplacianResidualSquaredNormFromCoordinateVector,
    packedLaplacianDenseCoordinateVectorWithCellCoordinates,
    packedLaplacianResidualSquaredNormFromDenseCoordinates,
    prepareRestrictionGraphPlanWithDimensions,
  )
import Moonlight.Sheaf.Cochain.Preparation
  ( spectralReadyIterationValue,
    prepareNerveCochainSpectralWith,
    srscFilteredMorse,
    srscSpectralPages,
  )
import Moonlight.Sheaf.Cochain.PreparedDenseNerve
  ( PreparedDenseNerveCochainPlan,
    applyPreparedDenseNerveRankOneCoboundaryDense,
    materializePreparedDenseNerveCoboundaryComplex,
    materializePreparedDenseNerveRankOneCoboundaryComplexWith,
    prepareDenseNerveCochainPlan,
    preparedDenseNerveCellsAtDimension,
    preparedDenseNerveComplexScaffold,
    preparedDenseNerveFacesAtDimension,
    projectPreparedDenseNerveSite,
  )
import Moonlight.Sheaf.Kernel.Basis (mkSheafBasis)
import Moonlight.Sheaf.Operator.GradedComplex
import Moonlight.Sheaf.Operator.LinearBasis (mkLinearBasis)
import Moonlight.Sheaf.Section.Linearize (identityBoundaryIncidence)
import Moonlight.Sheaf.Section.Morphism
  ( Restriction (..),
    RestrictionId (..),
    RestrictionParts (..),
    unitIncidenceRestriction,
  )
import Moonlight.Sheaf.Section.Model
  ( withPreparedSheafModel,
  )
import Moonlight.Sheaf.Kernel.Basis (basisCells)
import Moonlight.Sheaf.Section.ObjectIndex
  ( SheafModelVersion (..),
    mkObjectIndex,
  )
import Moonlight.Sheaf.Section.Restriction
  ( RestrictionIndex,
    buildRestrictionIndex,
  )
import Moonlight.Sheaf.Section.Store.State
import Moonlight.Sheaf.Site.Interface.Types
  ( InterfaceMeasure (..),
    InterfaceName,
    interfaceNameFromString,
  )
import Moonlight.Sheaf.Site.Construction.Nerve
  ( CellKey (..),
    FaceMorphism,
    NerveCell,
    NerveSite,
    NerveSiteAlgebra (..),
    faceMorphismFaceIndex,
    faceMorphismOrientation,
    faceMorphismSource,
    faceMorphismTarget,
    mkNerveSite,
    mkNerveSiteDenseWindow,
    mkNerveSiteWCOJWindow,
    mkNerveSiteWindow,
    nerveCellKey,
    nerveSiteDepth,
    siteCellsAtDimension,
    siteFaceMorphisms,
  )
import Moonlight.Sheaf.Site.Analysis.Scaffold
  ( SiteComplexScaffold,
    mkNerveComplexScaffold,
    scsChainComplex,
  )
import Moonlight.Sheaf.Site.Stalk.Interface
  ( InterfaceDomain (..),
  )
import Moonlight.Sheaf.Site.Stalk.Interface.Linearization
  ( interfaceStalkBasisLinearization,
  )
import Moonlight.Category.Simplicial (NerveSimplex, nerve, nerveSimplexChain)
import Moonlight.Category.Simplicial (TruncatedNormalizedSSet, simplicesAtDimension)
import Numeric.Natural (Natural)

type BenchCell :: Type
newtype BenchCell = BenchCell Int
  deriving stock (Eq, Ord, Show)

type BenchStalk :: Type
newtype BenchStalk = BenchStalk Int
  deriving stock (Eq, Show)

type BenchRestrictionWitness :: Type
type BenchRestrictionWitness = (BenchCell, BenchCell)

presentBenchRestrictionWitness :: BenchRestrictionWitness -> RestrictionParts BenchCell BenchRestrictionWitness
presentBenchRestrictionWitness witness@(sourceCell, targetCell) =
  RestrictionParts
    { partKind = unitIncidenceRestriction,
      partSource = sourceCell,
      partTarget = targetCell,
      partWitness = witness
    }

type BenchSiteTag :: Type
data BenchSiteTag

type PreparedBenchmarkValue :: Type -> Type
newtype PreparedBenchmarkValue value = PreparedBenchmarkValue value

instance NFData (PreparedBenchmarkValue value) where
  rnf (PreparedBenchmarkValue value) = value `seq` ()

type ForcedNerveSite :: Type
data ForcedNerveSite = ForcedNerveSite !Int (NerveSite BenchSiteTag)

instance NFData ForcedNerveSite where
  rnf (ForcedNerveSite weightValue _) = rnf weightValue

type ForcedPreparedDensePlan :: Type
data ForcedPreparedDensePlan = ForcedPreparedDensePlan !Int (PreparedDenseNerveCochainPlan BenchSiteTag)

instance NFData ForcedPreparedDensePlan where
  rnf (ForcedPreparedDensePlan weightValue _) = rnf weightValue

type PackedResidualBenchData :: Type
data PackedResidualBenchData = PackedResidualBenchData
  !Int
  (SheafLaplacian 'TarskiLaplacian BenchCell)
  (PackedSheafLaplacian 'TarskiLaplacian BenchCell)
  !(Map.Map Int Double)
  !(Unboxed.Vector Double)

instance NFData PackedResidualBenchData where
  rnf (PackedResidualBenchData weightValue _ _ oldMapInput packedDenseInput) =
    rnf weightValue `seq` rnf oldMapInput `seq` rnf packedDenseInput

cochainBenchmarks :: IO Benchmark
cochainBenchmarks = do
  enableMillionScale <- isJust <$> lookupEnv "MOONLIGHT_SHEAF_PACKED_RESIDUAL_BENCH_ENABLE_1M"
  case sheafSpectralBenchmarks of
    Left failureMessage -> fail failureMessage
    Right spectralBenchmarks ->
      pure (bgroup "cochain" (coboundaryBenchmarks <> packedResidualBenchmarks enableMillionScale <> sheafSiteConstructionBenchmarks <> spectralBenchmarks))

coboundaryBenchmarks :: [Benchmark]
coboundaryBenchmarks =
  [ benchSuite "coboundary/small" 256,
    benchSuite "coboundary/medium" 2048,
    benchSuite "coboundary/large" 8192
  ]

packedResidualBenchmarks :: Bool -> [Benchmark]
packedResidualBenchmarks enableMillionScale =
  fmap
    (uncurry packedResidualBenchSuite)
    ( [ ("packed-residual/10k", 10000),
        ("packed-residual/100k", 100000)
      ]
        <> [("packed-residual/1m", 1000000) | enableMillionScale]
    )

packedResidualBenchSuite :: String -> Int -> Benchmark
packedResidualBenchSuite label objectCount =
  env
    (forcePackedResidualBenchData objectCount)
    ( \benchData ->
        bgroup
          label
          [ bench "old-map-residual-norm" (nf packedResidualOldMapNormWeight benchData),
            bench "packed-dense-residual-norm" (nf packedResidualDenseNormWeight benchData)
          ]
    )

sheafSiteConstructionBenchmarks :: [Benchmark]
sheafSiteConstructionBenchmarks =
  [ bgroup
      "sheaf-site-construction/total-order-20"
      [ benchWeight "category" totalOrderCategoryConstructionWeightResult 20,
        benchWeight "nerve" totalOrderNerveConstructionWeightResult 20,
        benchWeight "site" totalOrderSiteConstructionWeightResult 20,
        benchWeight "scaffold-from-category" totalOrderScaffoldFromCategoryWeightResult 20
      ]
  , bgroup
      "sheaf-site-construction/total-order-14-depth3-window"
      [ benchWeight "category" totalOrderCategoryConstructionWeightResult 14,
        benchWeight "nerve-depth3" (totalOrderNerveConstructionDepthWeightResult 3) 14,
        benchWeight "site-depth3" (totalOrderSiteConstructionDepthWeightResult 3) 14,
        benchWeight "site-cochain-window" totalOrderSiteCochainWindowWeightResult 14,
        benchWeight "site-cochain-wcoj-window" totalOrderSiteCochainWCOJWindowWeightResult 14,
        benchWeight "site-cochain-dense-window" totalOrderSiteCochainDenseWindowWeightResult 14,
        benchWeight "scaffold-from-cochain-window" totalOrderScaffoldFromCochainWindowWeightResult 14,
        benchWeight "prepared-dense-plan" totalOrderPreparedDensePlanWeightResult 14,
        benchWeight "prepared-dense-scaffold" totalOrderPreparedDenseScaffoldWeightResult 14,
        benchWeight "prepared-dense-explicit-complex" totalOrderPreparedDenseExplicitComplexWeightResult 14,
        benchWeight "prepared-dense-rank-one-complex" totalOrderPreparedDenseRankOneComplexWeightResult 14,
        env totalOrderPreparedDensePlan14
          ( \ ~(ForcedPreparedDensePlan _ preparedPlan) ->
              benchWeight "prepared-dense-rank-one-apply" preparedDenseRankOneApplyWeightResult preparedPlan
          ),
        env totalOrderPreparedDensePlan14
          ( \ ~(ForcedPreparedDensePlan _ preparedPlan) ->
              benchWeight "project-prepared-dense-site" projectPreparedDenseSiteWeightResult preparedPlan
          )
      ]
  ]

benchSuite :: String -> Int -> Benchmark
benchSuite label entryCount =
  bgroup
    label
    [ benchEither "generic-unit" (whnf materializeGenericUnit) genericUnitMatrixValue,
      bench "canonical-unit" (whnf materializeCanonicalUnitResult canonicalUnitMatrixValue),
      benchEither "generic-heterogeneous" (whnf materializeGenericHeterogeneous) heterogeneousMatrixValue,
      benchEither "assembly-plan-unit-from-scratch" (nf materializeUnitAssemblyPlanFromScratch) genericUnitMatrixValue,
      preparedUnitAssemblyPlanBenchmark,
      benchEither "kernel-unit-from-scratch" (nf materializeUnitKernelFromScratch) genericUnitMatrixValue,
      preparedUnitKernelBenchmark,
      preparedUnitKernelApplyBenchmark,
      rankOneIncidenceFromScratchBenchmark,
      rankOneDifferentialFromScratchBenchmark,
      rankOneComplexFromScratchBenchmark,
      preparedRankOneIncidenceBenchmark,
      preparedRankOneApplyBenchmark,
      preparedRankOneDenseApplyBenchmark,
      benchEither "incidence-plan-unit-from-scratch" (nf materializeUnitIncidencePlanFromScratch) genericUnitMatrixValue,
      preparedUnitIncidencePlanBenchmark,
      preparedUnitIncidencePlanApplyBenchmark,
      bench "cochain-complex-int-from-scratch" (nf cochainConstructorIntWeight entryCount),
      preparedCochainConstructorBenchmark
    ]
  where
    genericUnitMatrixValue = genericUnitMatrix entryCount
    canonicalUnitMatrixValue = canonicalUnitMatrix entryCount
    canonicalUnitIndexValue = canonicalUnitIndex entryCount
    rankOneSpecValue = canonicalUnitSpec entryCount
    rankOneComplexIndexValue = rankOneComplexIndex entryCount
    rankOneComplexSpec0Value = rankOneComplexSpec0 entryCount
    rankOneComplexSpec1Value = rankOneComplexSpec1 entryCount
    heterogeneousMatrixValue = heterogeneousMatrix entryCount
    preparedUnitAssemblyPlanValue = genericUnitMatrixValue >>= prepareUnitAssemblyPlan
    preparedUnitKernelIncidencePlanValue = genericUnitMatrixValue >>= prepareUnitKernelIncidencePlan
    rankOnePlanValue = canonicalUnitIndexValue >>= prepareRankOnePlan rankOneSpecValue
    preparedCochainConstructorBenchmark =
      benchEither
        "cochain-complex-int-prepared-constructor"
        (nf cochainConstructorPreparedWeight)
        (cochainConstructorIntDifferentials entryCount)
    preparedUnitAssemblyPlanBenchmark =
      benchEither
        "assembly-plan-unit-prepared"
        (nf materializePreparedUnitAssemblyPlan)
        preparedUnitAssemblyPlanValue
    preparedUnitIncidencePlanBenchmark =
      benchEither
        "incidence-plan-unit-prepared"
        (nf materializePreparedUnitIncidencePlan)
        (genericUnitMatrixValue >>= prepareUnitIncidencePlan)
    preparedUnitKernelBenchmark =
      benchEither
        "kernel-unit-prepared"
        (nf materializePreparedUnitIncidencePlan)
        preparedUnitKernelIncidencePlanValue
    preparedUnitKernelApplyBenchmark =
      benchEither
        "kernel-unit-apply-prepared"
        (nf (materializePreparedUnitKernelApplication unitVectorValue))
        preparedUnitAssemblyPlanValue
    rankOneIncidenceFromScratchBenchmark =
      benchEither
        "rank-one-incidence-from-scratch"
        (nf (materializeRankOneIncidenceFromScratch rankOneSpecValue))
        canonicalUnitIndexValue
    rankOneDifferentialFromScratchBenchmark =
      benchEither
        "rank-one-differential-from-scratch"
        (nf (materializeRankOneDifferentialFromScratch rankOneSpecValue))
        canonicalUnitIndexValue
    rankOneComplexFromScratchBenchmark =
      benchEither
        "rank-one-complex-from-scratch"
        (nf (materializeRankOneComplexFromScratch rankOneComplexSpec0Value rankOneComplexSpec1Value))
        rankOneComplexIndexValue
    preparedRankOneIncidenceBenchmark =
      benchEither
        "rank-one-incidence-prepared"
        (nf materializePreparedRankOneIncidence)
        rankOnePlanValue
    preparedRankOneApplyBenchmark =
      benchEither
        "rank-one-apply-prepared"
        (nf (materializePreparedRankOneApplication unitVectorValue))
        rankOnePlanValue
    preparedRankOneDenseApplyBenchmark =
      benchEither
        "rank-one-dense-apply-prepared"
        (nf (materializePreparedRankOneDenseApplication unitDenseVectorValue))
        rankOnePlanValue
    preparedUnitIncidencePlanApplyBenchmark =
      benchEither
        "incidence-plan-unit-apply-prepared"
        (nf (materializePreparedUnitIncidenceApplication unitVectorValue))
        preparedUnitKernelIncidencePlanValue
    unitVectorValue =
      unitVector entryCount
    unitDenseVectorValue =
      Unboxed.replicate entryCount 1

benchEither :: String -> (value -> Benchmarkable) -> Either String value -> Benchmark
benchEither label benchmarkValue preparedValue =
  env
    (prepareBenchmarkValue preparedValue)
    (\ ~(PreparedBenchmarkValue value) -> bench label (benchmarkValue value))

benchWeight :: String -> (value -> Either String Int) -> value -> Benchmark
benchWeight label weightValue value =
  env
    (validateBenchmarkWeight weightValue value)
    (\ ~(PreparedBenchmarkValue preparedValue) -> bench label (nf weightValue preparedValue))

prepareBenchmarkValue :: Either String value -> IO (PreparedBenchmarkValue value)
prepareBenchmarkValue =
  either fail (pure . PreparedBenchmarkValue)

validateBenchmarkWeight :: (value -> Either String Int) -> value -> IO (PreparedBenchmarkValue value)
validateBenchmarkWeight weightValue value =
  case weightValue value of
    Left failureMessage -> fail failureMessage
    Right weight -> evaluate weight >> pure (PreparedBenchmarkValue value)

materializeGenericUnit :: CoboundaryMatrix BenchCell witness -> Either String (BoundaryIncidence Int)
materializeGenericUnit =
  materializeUnitMatrix

materializeCanonicalUnitResult :: Either String (CoboundaryMatrix BenchCell witness) -> Either String (BoundaryIncidence Int)
materializeCanonicalUnitResult =
  (>>= materializeUnitMatrix)

materializeGenericHeterogeneous :: CoboundaryMatrix BenchCell () -> Either String (BoundaryIncidence Int)
materializeGenericHeterogeneous =
  showSuccess
    . materializeCoboundaryIncidence
      stalkAtCell
      stalkDimension
      blockForStalks

materializeUnitMatrix :: CoboundaryMatrix BenchCell witness -> Either String (BoundaryIncidence Int)
materializeUnitMatrix =
  showSuccess
    . materializeCoboundaryIncidence
      (const ())
      (const 1)
      (\_ _ -> unitBlock 1)

materializeUnitAssemblyPlanFromScratch :: CoboundaryMatrix BenchCell () -> Either String Int
materializeUnitAssemblyPlanFromScratch matrix =
  prepareUnitAssemblyPlan matrix >>= materializePreparedUnitAssemblyPlan

materializeUnitIncidencePlanFromScratch :: CoboundaryMatrix BenchCell () -> Either String Int
materializeUnitIncidencePlanFromScratch matrix =
  prepareUnitIncidencePlan matrix >>= materializePreparedUnitIncidencePlan

materializeUnitKernelFromScratch :: CoboundaryMatrix BenchCell () -> Either String Int
materializeUnitKernelFromScratch matrix =
  prepareUnitKernelIncidencePlan matrix >>= materializePreparedUnitIncidencePlan

materializeRankOneIncidenceFromScratch ::
  CoboundarySpec BenchCell ->
  RestrictionIndex BenchCell BenchRestrictionWitness ->
  Either String Int
materializeRankOneIncidenceFromScratch spec restrictionIndexValue =
  prepareRankOnePlan spec restrictionIndexValue >>= materializePreparedRankOneIncidence

materializeRankOneDifferentialFromScratch ::
  CoboundarySpec BenchCell ->
  RestrictionIndex BenchCell BenchRestrictionWitness ->
  Either String Int
materializeRankOneDifferentialFromScratch spec restrictionIndexValue =
  prepareRankOnePlan spec restrictionIndexValue >>= materializePreparedRankOneDifferential

materializeRankOneComplexFromScratch ::
  CoboundarySpec BenchCell ->
  CoboundarySpec BenchCell ->
  RestrictionIndex BenchCell BenchRestrictionWitness ->
  Either String Int
materializeRankOneComplexFromScratch spec0 spec1 restrictionIndexValue =
  fmap
    (Map.size . gradedOperatorsByDegree)
    ( showSuccess
        ( buildRankOneCoboundaryComplex
            (const ())
            (\_ _ _ -> 1)
            spec0
            spec1
            restrictionIndexValue
        )
    )

prepareUnitAssemblyPlan :: CoboundaryMatrix BenchCell () -> Either String (CoboundaryAssemblyPlan BenchCell ())
prepareUnitAssemblyPlan =
  showSuccess
    . prepareCoboundaryAssemblyPlan
      (const ())
      (const 1)

prepareUnitIncidencePlan :: CoboundaryMatrix BenchCell () -> Either String (CoboundaryIncidencePlan BenchCell)
prepareUnitIncidencePlan matrix =
  prepareUnitAssemblyPlan matrix
    >>= showSuccess . prepareCoboundaryIncidencePlan (\_ _ -> unitBlock 1)

prepareUnitKernelIncidencePlan :: CoboundaryMatrix BenchCell () -> Either String (CoboundaryIncidencePlan BenchCell)
prepareUnitKernelIncidencePlan matrix =
  prepareUnitAssemblyPlan matrix
    >>= showSuccess . prepareCoboundaryIncidencePlanWithKernel UnitCoboundaryBlock

materializePreparedUnitAssemblyPlan :: CoboundaryAssemblyPlan BenchCell () -> Either String Int
materializePreparedUnitAssemblyPlan =
  fmap (length . boundaryEntries . gradedOperatorIncidence)
    . showSuccess
    . materializeCoboundaryAssemblyPlan (\_ _ -> unitBlock 1)

materializePreparedUnitIncidencePlan :: CoboundaryIncidencePlan BenchCell -> Either String Int
materializePreparedUnitIncidencePlan =
  fmap (length . boundaryEntries . gradedOperatorIncidence)
    . showSuccess
    . materializeCoboundaryIncidencePlan

materializePreparedUnitKernelApplication :: Map.Map Int Int -> CoboundaryAssemblyPlan BenchCell () -> Either String Int
materializePreparedUnitKernelApplication vectorValue =
  fmap Map.size
    . showSuccess
    . (\assemblyPlan -> applyCoboundaryAssemblyPlanWithKernel UnitCoboundaryBlock assemblyPlan vectorValue)

materializePreparedUnitIncidenceApplication :: Map.Map Int Int -> CoboundaryIncidencePlan BenchCell -> Int
materializePreparedUnitIncidenceApplication vectorValue incidencePlan =
  Map.size (applyCoboundaryIncidencePlan incidencePlan vectorValue)

prepareRankOnePlan ::
  CoboundarySpec BenchCell ->
  RestrictionIndex BenchCell BenchRestrictionWitness ->
  Either String (RankOneCoboundaryPlan BenchCell)
prepareRankOnePlan spec =
  showSuccess
    . prepareRankOneCoboundaryPlan
      (const ())
      (\_ _ _ -> 1)
      spec

materializePreparedRankOneIncidence :: RankOneCoboundaryPlan BenchCell -> Either String Int
materializePreparedRankOneIncidence =
  Right . length . boundaryEntries . materializeRankOneCoboundaryIncidence

materializePreparedRankOneDifferential :: RankOneCoboundaryPlan BenchCell -> Either String Int
materializePreparedRankOneDifferential =
  fmap (length . boundaryEntries . gradedOperatorIncidence)
    . showSuccess
    . materializeRankOneCoboundaryDifferential

materializePreparedRankOneApplication :: Map.Map Int Int -> RankOneCoboundaryPlan BenchCell -> Int
materializePreparedRankOneApplication vectorValue rankOnePlan =
  Map.size (applyRankOneCoboundaryPlan rankOnePlan vectorValue)

materializePreparedRankOneDenseApplication :: Unboxed.Vector Int -> RankOneCoboundaryPlan BenchCell -> Either String Int
materializePreparedRankOneDenseApplication sourceVector rankOnePlan =
  fmap
    Unboxed.sum
    (showSuccess (applyRankOneCoboundaryPlanDense rankOnePlan sourceVector))

cochainConstructorIntWeight :: Int -> Either String Int
cochainConstructorIntWeight entryCount =
  cochainConstructorPreparedWeight =<< cochainConstructorIntDifferentials entryCount

cochainConstructorPreparedWeight :: [GradedOperator BenchCell Int] -> Either String Int
cochainConstructorPreparedWeight =
  fmap (Map.size . gradedOperatorsByDegree) . showSuccess . mkGradedComplexFromList DegreeIncreasing

cochainConstructorIntDifferentials :: Int -> Either String [GradedOperator BenchCell Int]
cochainConstructorIntDifferentials entryCount = do
  basisValue <-
    showSuccess
      (mkLinearBasis (const 1) (mkSheafBasis (entryCells entryCount)))
  rightDifferential <-
    showSuccess
      (mkGradedOperator (HomologicalDegree 0) basisValue basisValue emptyIncidenceValue)
  leftDifferential <-
    showSuccess
      (mkGradedOperator (HomologicalDegree 1) basisValue basisValue emptyIncidenceValue)
  pure [rightDifferential, leftDifferential]
  where
    emptyIncidenceValue :: BoundaryIncidence Int
    emptyIncidenceValue =
      emptyBoundaryIncidenceOf
        (fromIntegral entryCount)
        (fromIntegral entryCount)

showSuccess :: Show err => Either err value -> Either String value
showSuccess =
  either (Left . show) Right

genericUnitMatrix :: Int -> Either String (CoboundaryMatrix BenchCell ())
genericUnitMatrix entryCount =
  unitMatrixFromCells (HomologicalDegree 0) (entryCells entryCount)

canonicalUnitMatrix :: Int -> Either String (CoboundaryMatrix BenchCell BenchRestrictionWitness)
canonicalUnitMatrix entryCount =
  canonicalUnitIndex entryCount
    >>= showSuccess . buildCoboundary (canonicalUnitSpec entryCount)

canonicalUnitSpec :: Int -> CoboundarySpec BenchCell
canonicalUnitSpec entryCount =
  CoboundarySpec
    { csDimension = (HomologicalDegree 0),
      csSourceBasis = mkSheafBasis cells,
      csTargetBasis = mkSheafBasis cells
    }
  where
    cells = entryCells entryCount

canonicalUnitIndex :: Int -> Either String (RestrictionIndex BenchCell BenchRestrictionWitness)
canonicalUnitIndex entryCount =
  showSuccess
    ( buildRestrictionIndex
        (mkObjectIndex (basisCells (mkSheafBasis (entryCells entryCount))))
        presentBenchRestrictionWitness
        (fmap (\cellValue -> (cellValue, cellValue)) (entryCells entryCount))
    )

rankOneComplexIndex :: Int -> Either String (RestrictionIndex BenchCell BenchRestrictionWitness)
rankOneComplexIndex entryCount =
  showSuccess
    ( buildRestrictionIndex
        (mkObjectIndex (basisCells (mkSheafBasis (rankOneComplexCells entryCount))))
        presentBenchRestrictionWitness
        (zip (rankOneComplexOneCells entryCount) (rankOneComplexZeroCells entryCount))
    )

rankOneComplexSpec0 :: Int -> CoboundarySpec BenchCell
rankOneComplexSpec0 entryCount =
  CoboundarySpec
    { csDimension = (HomologicalDegree 0),
      csSourceBasis = mkSheafBasis (rankOneComplexZeroCells entryCount),
      csTargetBasis = mkSheafBasis (rankOneComplexOneCells entryCount)
    }

rankOneComplexSpec1 :: Int -> CoboundarySpec BenchCell
rankOneComplexSpec1 entryCount =
  CoboundarySpec
    { csDimension = (HomologicalDegree 1),
      csSourceBasis = mkSheafBasis (rankOneComplexOneCells entryCount),
      csTargetBasis = mkSheafBasis []
    }

rankOneComplexCells :: Int -> [BenchCell]
rankOneComplexCells entryCount =
  rankOneComplexZeroCells entryCount <> rankOneComplexOneCells entryCount

rankOneComplexZeroCells :: Int -> [BenchCell]
rankOneComplexZeroCells =
  entryCells

rankOneComplexOneCells :: Int -> [BenchCell]
rankOneComplexOneCells entryCount =
  fmap (BenchCell . (+ entryCount)) [0 .. entryCount - 1]

heterogeneousMatrix :: Int -> Either String (CoboundaryMatrix BenchCell ())
heterogeneousMatrix entryCount =
  unitMatrixFromCells (HomologicalDegree 0) (entryCells entryCount)

unitMatrixFromCells :: HomologicalDegree -> [BenchCell] -> Either String (CoboundaryMatrix BenchCell ())
unitMatrixFromCells dimensionValue cells = do
  entries <-
    traverse
      (uncurry unitCoboundaryEntry)
      (zip [0 ..] cells)
  showSuccess
    ( mkCoboundaryMatrix
        CoboundarySpec
          { csDimension = dimensionValue,
            csSourceBasis = mkSheafBasis cells,
            csTargetBasis = mkSheafBasis cells
          }
        entries
    )

unitCoboundaryEntry :: Int -> BenchCell -> Either String (CoboundaryEntry BenchCell ())
unitCoboundaryEntry entryId cellValue =
  showSuccess
    ( mkCoboundaryEntry
        Restriction
          { rId = RestrictionId entryId,
            rKind = unitIncidenceRestriction,
            rSource = cellValue,
            rTarget = cellValue,
            rWitness = ()
          }
    )

entryCells :: Int -> [BenchCell]
entryCells entryCount =
  fmap BenchCell [0 .. entryCount - 1]

unitVector :: Int -> Map.Map Int Int
unitVector entryCount =
  Map.fromList (zip [0 .. entryCount - 1] [1 .. entryCount])

forcePackedResidualBenchData :: Int -> IO PackedResidualBenchData
forcePackedResidualBenchData objectCount =
  case packedResidualBenchData objectCount of
    Left failureMessage ->
      fail failureMessage
    Right benchData -> do
      _ <- evaluate (rnf benchData)
      pure benchData

packedResidualBenchData :: Int -> Either String PackedResidualBenchData
packedResidualBenchData objectCount =
  showSuccess
    ( withPreparedSheafModel
          (SheafModelVersion 0)
          (mkObjectIndex (basisCells (mkSheafBasis cells)))
          presentBenchRestrictionWitness
          (packedResidualMorphisms objectCount)
          ( \model -> do
              plan <-
                showSuccess
                  (prepareRestrictionGraphPlanWithDimensions (const 1) TarskiRestrictionGraphPlan model)
              symbolicLaplacian <- showSuccess (buildTarskiLaplacianFromPlan plan)
              packedLaplacian <- showSuccess (buildPackedTarskiLaplacianFromPlan plan)
              sectionValue <-
                showSuccess
                  ( mkTotalSectionStore
                      model
                      (Map.fromList (fmap packedResidualSectionEntry cells))
                  )
              oldMapInput <-
                showSuccess
                  ( laplacianCoordinateVectorWithCellCoordinates
                      packedResidualCoordinates
                      symbolicLaplacian
                      model
                      sectionValue
                  )
              packedDenseInput <-
                showSuccess
                  ( packedLaplacianDenseCoordinateVectorWithCellCoordinates
                      packedResidualCoordinates
                      packedLaplacian
                      model
                      sectionValue
                  )
              oldNorm <- packedResidualOldMapNormWeightFrom symbolicLaplacian oldMapInput
              packedNorm <- packedResidualDenseNormWeightFrom packedLaplacian packedDenseInput
              if oldNorm == packedNorm
                then
                  Right
                    ( PackedResidualBenchData
                        objectCount
                        symbolicLaplacian
                        packedLaplacian
                        oldMapInput
                        packedDenseInput
                    )
                else Left ("packed residual mismatch: old=" <> show oldNorm <> ", packed=" <> show packedNorm)
          )
    )
    >>= id
  where
    cells =
      entryCells objectCount

packedResidualMorphisms :: Int -> [BenchRestrictionWitness]
packedResidualMorphisms objectCount =
  zip cells (drop 1 cells)
  where
    cells =
      entryCells objectCount

packedResidualSectionEntry :: BenchCell -> (BenchCell, BenchStalk)
packedResidualSectionEntry cell@(BenchCell cellValue) =
  (cell, BenchStalk (cellValue + 1))

packedResidualOldMapNormWeight :: PackedResidualBenchData -> Either String Double
packedResidualOldMapNormWeight (PackedResidualBenchData _ symbolicLaplacian _ oldMapInput _) =
  packedResidualOldMapNormWeightFrom symbolicLaplacian oldMapInput

packedResidualDenseNormWeight :: PackedResidualBenchData -> Either String Double
packedResidualDenseNormWeight (PackedResidualBenchData _ _ packedLaplacian _ packedDenseInput) =
  packedResidualDenseNormWeightFrom packedLaplacian packedDenseInput

packedResidualOldMapNormWeightFrom ::
  SheafLaplacian 'TarskiLaplacian BenchCell ->
  Map.Map Int Double ->
  Either String Double
packedResidualOldMapNormWeightFrom symbolicLaplacian oldMapInput =
  Right (laplacianResidualSquaredNormFromCoordinateVector symbolicLaplacian oldMapInput)

packedResidualDenseNormWeightFrom ::
  PackedSheafLaplacian 'TarskiLaplacian BenchCell ->
  Unboxed.Vector Double ->
  Either String Double
packedResidualDenseNormWeightFrom packedLaplacian packedDenseInput =
  showSuccess
    ( packedLaplacianResidualSquaredNormFromDenseCoordinates
        packedLaplacian
        packedDenseInput
    )

packedResidualCoordinates :: BenchCell -> BenchStalk -> [Double]
packedResidualCoordinates _ (BenchStalk value) =
  [fromIntegral value]

stalkAtCell :: BenchCell -> BenchStalk
stalkAtCell (BenchCell cellValue) =
  BenchStalk
    (if even cellValue then 2 else 3)

stalkDimension :: BenchStalk -> Int
stalkDimension (BenchStalk dimensionValue) = dimensionValue

blockForStalks :: BenchStalk -> BenchStalk -> BoundaryIncidence Int
blockForStalks (BenchStalk sourceDimension) (BenchStalk targetDimension) =
  identityBoundaryIncidence (min sourceDimension targetDimension)

unitBlock :: Int -> BoundaryIncidence Int
unitBlock = identityBoundaryIncidence

sheafSpectralBenchmarks :: Either String [Benchmark]
sheafSpectralBenchmarks = do
  comparisonBenchmark <- sheafSpectralComparisonSuite "sheaf-spectral/total-order-8" 8
  reducedOnlyBenchmarks <-
    traverse
      (uncurry sheafSpectralReducedOnlySuite)
      [ ("sheaf-spectral/total-order-10", 10)
      ]
  pure (comparisonBenchmark : reducedOnlyBenchmarks)

sheafSpectralComparisonSuite :: String -> Int -> Either String Benchmark
sheafSpectralComparisonSuite label objectCount =
  env
    (forceTotalOrderNerveSite objectCount)
    ( \ ~(ForcedNerveSite _ siteValue) ->
        bgroup
          label
          [ benchWeight "raw-unreduced-site-spectral" rawSheafSpectralWeightResult siteValue,
            benchWeight "spectral-ready-filtered-morse" sheafSpectralReadyWeightResult siteValue
          ]
      )
    <$ totalOrderNerveSite objectCount

sheafSpectralReducedOnlySuite :: String -> Int -> Either String Benchmark
sheafSpectralReducedOnlySuite label objectCount =
  env
    (forceTotalOrderNerveSite objectCount)
    ( \ ~(ForcedNerveSite _ siteValue) ->
        bgroup
          label
          [ benchWeight "site-scaffold" nerveScaffoldWeightResult siteValue,
            benchWeight "scaffold-plus-filtered-morse" filteredMorseWeightResult siteValue,
            benchWeight "spectral-ready-filtered-morse" sheafSpectralReadyWeightResult siteValue
          ]
      )
    <$ totalOrderNerveSite objectCount

forceTotalOrderNerveSite :: Int -> IO ForcedNerveSite
forceTotalOrderNerveSite objectCount =
  case totalOrderNerveSite objectCount of
    Left failureMessage -> fail failureMessage
    Right siteValue -> do
      weightValue <- evaluate (nerveSiteWeight siteValue)
      pure (ForcedNerveSite weightValue siteValue)

totalOrderCategoryConstructionWeightResult :: Int -> Either String Int
totalOrderCategoryConstructionWeightResult objectCount = do
  categoryValue <- totalOrderCategory objectCount
  pure (length (allObjects categoryValue) + length (allMorphisms categoryValue))

totalOrderNerveConstructionWeightResult :: Int -> Either String Int
totalOrderNerveConstructionWeightResult objectCount = do
  totalOrderNerveConstructionDepthWeightResult 2 objectCount

totalOrderNerveConstructionDepthWeightResult :: Natural -> Int -> Either String Int
totalOrderNerveConstructionDepthWeightResult depthValue objectCount = do
  categoryValue <- totalOrderCategory objectCount
  pure (simplicialSetWeight depthValue (nerve categoryValue depthValue))

totalOrderSiteConstructionWeightResult :: Int -> Either String Int
totalOrderSiteConstructionWeightResult =
  totalOrderSiteConstructionDepthWeightResult 2

totalOrderSiteConstructionDepthWeightResult :: Natural -> Int -> Either String Int
totalOrderSiteConstructionDepthWeightResult depthValue objectCount =
  fmap nerveSiteWeight (totalOrderNerveSiteAtDepth depthValue objectCount)

totalOrderScaffoldFromCategoryWeightResult :: Int -> Either String Int
totalOrderScaffoldFromCategoryWeightResult objectCount = do
  siteValue <- totalOrderNerveSite objectCount
  scaffoldValue <- first show (mkNerveComplexScaffold siteValue)
  pure (nerveSiteWeight siteValue + scaffoldWeight scaffoldValue)

totalOrderSiteCochainWindowWeightResult :: Int -> Either String Int
totalOrderSiteCochainWindowWeightResult objectCount =
  fmap nerveSiteWeight (totalOrderNerveSiteCochainWindow objectCount)

totalOrderSiteCochainWCOJWindowWeightResult :: Int -> Either String Int
totalOrderSiteCochainWCOJWindowWeightResult objectCount =
  fmap nerveSiteWeight (totalOrderNerveSiteCochainWCOJWindow objectCount)

totalOrderSiteCochainDenseWindowWeightResult :: Int -> Either String Int
totalOrderSiteCochainDenseWindowWeightResult objectCount =
  fmap nerveSiteWeight (totalOrderNerveSiteCochainDenseWindow objectCount)

totalOrderScaffoldFromCochainWindowWeightResult :: Int -> Either String Int
totalOrderScaffoldFromCochainWindowWeightResult objectCount = do
  siteValue <- totalOrderNerveSiteCochainWindow objectCount
  scaffoldValue <- first show (mkNerveComplexScaffold siteValue)
  pure (nerveSiteWeight siteValue + scaffoldWeight scaffoldValue)

totalOrderPreparedDensePlanWeightResult :: Int -> Either String Int
totalOrderPreparedDensePlanWeightResult objectCount =
  preparedDensePlanWeight <$> totalOrderPreparedDensePlan objectCount

totalOrderPreparedDenseScaffoldWeightResult :: Int -> Either String Int
totalOrderPreparedDenseScaffoldWeightResult objectCount = do
  preparedPlan <- totalOrderPreparedDensePlan objectCount
  scaffoldValue <- showSuccess (preparedDenseNerveComplexScaffold preparedPlan)
  pure (preparedDensePlanWeight preparedPlan + scaffoldWeight scaffoldValue)

totalOrderPreparedDenseExplicitComplexWeightResult :: Int -> Either String Int
totalOrderPreparedDenseExplicitComplexWeightResult objectCount = do
  preparedPlan <- totalOrderPreparedDensePlan objectCount
  complexValue <-
    showSuccess
      (materializePreparedDenseNerveCoboundaryComplex interfaceStalkBasisLinearization preparedPlan)
  pure (preparedDensePlanWeight preparedPlan + cochainComplexWeight complexValue)

totalOrderPreparedDenseRankOneComplexWeightResult :: Int -> Either String Int
totalOrderPreparedDenseRankOneComplexWeightResult objectCount = do
  preparedPlan <- totalOrderPreparedDensePlan objectCount
  complexValue <-
    showSuccess
      (materializePreparedDenseNerveRankOneCoboundaryComplexWith (\_ _ _ -> 1) preparedPlan)
  pure (preparedDensePlanWeight preparedPlan + cochainComplexWeight complexValue)

preparedDenseRankOneApplyWeightResult :: PreparedDenseNerveCochainPlan BenchSiteTag -> Either String Int
preparedDenseRankOneApplyWeightResult preparedPlan = do
  let sourceVector =
        Unboxed.replicate
          (length (preparedDenseNerveCellsAtDimension preparedPlan 0))
          1
  targetVector <-
    showSuccess
      ( applyPreparedDenseNerveRankOneCoboundaryDense
          (\_ _ _ -> 1)
          preparedPlan
          (HomologicalDegree 0)
          sourceVector
      )
  pure (Unboxed.sum targetVector)

projectPreparedDenseSiteWeightResult :: PreparedDenseNerveCochainPlan BenchSiteTag -> Either String Int
projectPreparedDenseSiteWeightResult =
  fmap nerveSiteWeight . first show . projectPreparedDenseNerveSite

totalOrderPreparedDensePlan14 :: IO ForcedPreparedDensePlan
totalOrderPreparedDensePlan14 =
  either
    fail
    (\preparedPlan -> evaluate (ForcedPreparedDensePlan (preparedDensePlanWeight preparedPlan) preparedPlan))
    (totalOrderPreparedDensePlan 14)

totalOrderPreparedDensePlan :: Int -> Either String (PreparedDenseNerveCochainPlan BenchSiteTag)
totalOrderPreparedDensePlan objectCount = do
  categoryValue <- totalOrderCategory objectCount
  showSuccess (prepareDenseNerveCochainPlan @BenchSiteTag categoryValue 1)

preparedDensePlanWeight :: PreparedDenseNerveCochainPlan BenchSiteTag -> Int
preparedDensePlanWeight preparedPlan =
  sumBy
    ( \dimensionValue ->
        length (preparedDenseNerveCellsAtDimension preparedPlan dimensionValue)
          + length (preparedDenseNerveFacesAtDimension preparedPlan dimensionValue)
    )
    [0, 1, 2]

simplicialSetWeight :: Natural -> TruncatedNormalizedSSet simplex -> Int
simplicialSetWeight maxDimensionValue simplicialSet =
  sumBy
    (length . simplicesAtDimension simplicialSet)
    [0 .. maxDimensionValue]

nerveSiteWeight :: NerveSite BenchSiteTag -> Int
nerveSiteWeight siteValue =
  cellsWeight + faceMorphismsWeight
  where
    cellsWeight =
      sumBy
        (sumBy nerveCellWeight . siteCellsAtDimension siteValue)
        [0 .. nerveSiteDepth siteValue]
    faceMorphismsWeight =
      sumBy faceMorphismWeight (siteFaceMorphisms siteValue)

nerveCellWeight :: NerveCell BenchSiteTag -> Int
nerveCellWeight cellValue =
  let CellKey dimensionValue ordinalValue = nerveCellKey cellValue
   in (2 * fromIntegral dimensionValue) + ordinalValue

faceMorphismWeight :: FaceMorphism BenchSiteTag -> Int
faceMorphismWeight faceValue =
  nerveCellWeight (faceMorphismSource faceValue)
    + nerveCellWeight (faceMorphismTarget faceValue)
    + fromIntegral (faceMorphismFaceIndex faceValue)
    + faceMorphismOrientation faceValue

nerveScaffoldWeightResult :: NerveSite BenchSiteTag -> Either String Int
nerveScaffoldWeightResult =
  fmap scaffoldWeight . first show . mkNerveComplexScaffold

filteredMorseWeightResult :: NerveSite BenchSiteTag -> Either String Int
filteredMorseWeightResult siteValue = do
    scaffoldValue <- first show (mkNerveComplexScaffold siteValue)
    filteredMorseValue <-
      firstHomologyFailure "filtered refined Morse reduction failed" $
        filteredRefinedMorseComplex (scsChainComplex scaffoldValue) trivialFiltration (const 0)
    let reducedComplex =
          rmcReducedComplex
            (frmcRefinedMorseComplex filteredMorseValue)
    pure (scaffoldWeight scaffoldValue + finiteComplexWeight reducedComplex)

scaffoldWeight :: SiteComplexScaffold site cell -> Int
scaffoldWeight scaffoldValue =
  finiteComplexWeight (scsChainComplex scaffoldValue)

cochainComplexWeight :: GradedComplex cell Int -> Int
cochainComplexWeight =
  sumBy
    ( \differentialValue ->
        sourceCardinality (gradedOperatorIncidence differentialValue)
          + length (boundaryEntries (gradedOperatorIncidence differentialValue))
    )
    . Map.elems
    . gradedOperatorsByDegree

rawSheafSpectralWeightResult :: NerveSite BenchSiteTag -> Either String Int
rawSheafSpectralWeightResult siteValue = do
  scaffoldValue <- first show (mkNerveComplexScaffold siteValue)
  firstHomologyFailure "raw sheaf spectral computation failed" $
    fmap spectralPagesWeight
      ( computeRationalSpectralPages
          (rationalizeFiniteChainComplex (scsChainComplex scaffoldValue))
          trivialFiltration
      )

sheafSpectralReadyWeightResult :: NerveSite BenchSiteTag -> Either String Int
sheafSpectralReadyWeightResult siteValue =
  case prepareNerveCochainSpectralWith (\_ _ -> 0) trivialFiltration siteValue of
    Left preparationError ->
      Left ("sheaf spectral-ready preparation failed: " <> show preparationError)
    Right spectralIteration ->
      let spectralValue =
            spectralReadyIterationValue spectralIteration
          reducedComplex =
            rmcReducedComplex
              (frmcRefinedMorseComplex (srscFilteredMorse spectralValue))
       in Right
            ( spectralPagesWeight (srscSpectralPages spectralValue)
                + basisCellCount reducedComplex
            )

totalOrderNerveSite :: Int -> Either String (NerveSite BenchSiteTag)
totalOrderNerveSite =
  totalOrderNerveSiteAtDepth 2

totalOrderNerveSiteAtDepth :: Natural -> Int -> Either String (NerveSite BenchSiteTag)
totalOrderNerveSiteAtDepth depthValue objectCount =
  fmap
    (\categoryValue -> mkNerveSite @BenchSiteTag categoryValue depthValue)
    (totalOrderCategory objectCount)

totalOrderNerveSiteCochainWindow :: Int -> Either String (NerveSite BenchSiteTag)
totalOrderNerveSiteCochainWindow objectCount =
  fmap
    (\categoryValue -> mkNerveSiteWindow @BenchSiteTag categoryValue (cochainSupportWindow 1))
    (totalOrderCategory objectCount)

totalOrderNerveSiteCochainWCOJWindow :: Int -> Either String (NerveSite BenchSiteTag)
totalOrderNerveSiteCochainWCOJWindow objectCount =
  totalOrderCategory objectCount
    >>= \categoryValue ->
      first show (mkNerveSiteWCOJWindow @BenchSiteTag categoryValue (cochainSupportWindow 1))

totalOrderNerveSiteCochainDenseWindow :: Int -> Either String (NerveSite BenchSiteTag)
totalOrderNerveSiteCochainDenseWindow objectCount =
  totalOrderCategory objectCount
    >>= ( \categoryValue ->
            first
              (\constructionError -> "dense nerve site failed: " <> show constructionError)
              (mkNerveSiteDenseWindow @BenchSiteTag categoryValue (cochainSupportWindow 1))
        )

totalOrderCategory :: Int -> Either String FinCat
totalOrderCategory objectCount =
  if objectCount <= 0
    then Left ("total-order object count must be positive: " <> show objectCount)
    else
      first show
        ( mkFinCat
            (Set.fromList (objectIds objectCount))
            (totalOrderMorphismMap objectCount)
            (totalOrderCompositionMap objectCount)
        )

totalOrderCategoryFromMorphisms :: [FinMor] -> Either () FinCat
totalOrderCategoryFromMorphisms morphismValues =
  case totalOrderObjectCountFromMorphisms morphismValues of
    Nothing ->
      Left ()
    Just objectCount ->
      first (const ()) (totalOrderCategory objectCount)

totalOrderObjectCountFromMorphisms :: [FinMor] -> Maybe Int
totalOrderObjectCountFromMorphisms morphismValues =
  case Set.toAscList (Set.fromList (mapMaybe totalOrderObjectCountFromMorphism morphismValues)) of
    [objectCount] -> Just objectCount
    _ -> Nothing

totalOrderObjectCountFromMorphism :: FinMor -> Maybe Int
totalOrderObjectCountFromMorphism morphismValue =
  case finMorId morphismValue of
    FinIdentityId _ ->
      Nothing
    FinGeneratorMorphismId (FinGeneratorId generatorId) ->
      let FinObjectId sourceId = finMorSourceId morphismValue
          FinObjectId targetId = finMorTargetId morphismValue
          rawObjectCount = generatorId - targetId
       in if sourceId > 0 && rawObjectCount >= sourceId && rawObjectCount `mod` sourceId == 0
            then
              let objectCount = rawObjectCount `div` sourceId
               in if objectCount > max sourceId targetId
                    then Just objectCount
                    else Nothing
            else Nothing

totalOrderMorphismMap :: Int -> Map.Map (FinObjectId, FinObjectId) [FinMorphismId]
totalOrderMorphismMap objectCount =
  Map.fromList
    [ ((sourceId, targetId), [totalOrderMorphismId objectCount sourceId targetId])
    | (sourceId, targetId) <- strictOrderPairs objectCount
    ]

totalOrderCompositionMap :: Int -> Map.Map (FinMorphismId, FinMorphismId) FinMorphismId
totalOrderCompositionMap objectCount =
  Map.fromList
    [ ( ( totalOrderMorphismId objectCount middleId targetId,
          totalOrderMorphismId objectCount sourceId middleId
        ),
        totalOrderMorphismId objectCount sourceId targetId
      )
    | sourceId <- objectIds objectCount,
      middleId <- objectIds objectCount,
      sourceId < middleId,
      targetId <- objectIds objectCount,
      middleId < targetId
    ]

strictOrderPairs :: Int -> [(FinObjectId, FinObjectId)]
strictOrderPairs objectCount =
  [ (sourceId, targetId)
  | sourceId <- objectIds objectCount,
    targetId <- objectIds objectCount,
    sourceId < targetId
  ]

objectIds :: Int -> [FinObjectId]
objectIds objectCount =
  FinObjectId <$> [0 .. objectCount - 1]

totalOrderMorphismId :: Int -> FinObjectId -> FinObjectId -> FinMorphismId
totalOrderMorphismId objectCount (FinObjectId sourceId) (FinObjectId targetId) =
  FinGeneratorMorphismId (FinGeneratorId (sourceId * objectCount + targetId))

firstHomologyFailure :: String -> Either HomologyFailure value -> Either String value
firstHomologyFailure contextMessage =
  either (\failureValue -> Left (contextMessage <> ": " <> show failureValue)) Right

sumBy :: Foldable foldable => (value -> Int) -> foldable value -> Int
sumBy measure =
  foldl' (\weightValue value -> weightValue + measure value) 0

trivialFiltration :: BasisCellRef -> Int
trivialFiltration = const 0

spectralPagesWeight :: [RationalSpectralPage] -> Int
spectralPagesWeight =
  sumBy spectralPageWeight

spectralPageWeight :: RationalSpectralPage -> Int
spectralPageWeight pageValue =
  pageIndex pageValue
    + sumBy spectralEntryWeight (Map.elems (pageEntryMap pageValue))
    + sumBy formalMapWeight (Map.elems (pageDifferentialMap pageValue))

spectralEntryWeight :: SpectralEntry Rational -> Int
spectralEntryWeight entryValue =
  let groupValue = entryGroupValue entryValue
   in freeRank groupValue + length (torsionInvariants groupValue)

formalMapWeight :: FormalMap Rational -> Int
formalMapWeight formalMapValue =
  sumMatrixWeight (formalMatrix formalMapValue)
    + length (formalDomainBasis formalMapValue)
    + length (formalCodomainBasis formalMapValue)

sumMatrixWeight :: [[Rational]] -> Int
sumMatrixWeight =
  sumBy rowWeight

rowWeight :: [Rational] -> Int
rowWeight =
  sumBy coefficientWeight

coefficientWeight :: Rational -> Int
coefficientWeight coefficientValue =
  if coefficientValue == 0
    then 0
    else 1

basisCellCount :: FiniteChainComplex r -> Int
basisCellCount finiteComplex =
  let HomologicalDegree maxDegreeValue = maxHomologicalDegree finiteComplex
   in sumBy
        (sourceCardinality . incidenceMatrixAt finiteComplex . HomologicalDegree)
        [0 .. maxDegreeValue]

finiteComplexWeight :: FiniteChainComplex r -> Int
finiteComplexWeight finiteComplex =
  let HomologicalDegree maxDegreeValue = maxHomologicalDegree finiteComplex
   in sumBy
        ( \degreeValue ->
            let incidence = incidenceMatrixAt finiteComplex (HomologicalDegree degreeValue)
             in sourceCardinality incidence + length (boundaryEntries incidence)
        )
        [0 .. maxDegreeValue]

instance NerveSiteAlgebra BenchSiteTag where
  type NerveCategory BenchSiteTag = FinCat
  type NerveSource BenchSiteTag = FinObj
  type NerveMorphism BenchSiteTag = FinMor

  buildSiteNerve :: FinCat -> Natural -> TruncatedNormalizedSSet (NerveSimplex FinCat)
  buildSiteNerve = nerve

  simplexSourceValue =
    chainStartObject . nerveSimplexChain

  simplexMorphismChain =
    chainMorphisms . nerveSimplexChain

benchInterfaceName :: String -> InterfaceName BenchSiteTag
benchInterfaceName =
  interfaceNameFromString

instance InterfaceDomain BenchSiteTag where
  type InterfaceObject BenchSiteTag = FinObj
  type InterfaceMorphism BenchSiteTag = FinMor
  type InterfaceComposeError BenchSiteTag = ()

  measureObject objectValue =
    InterfaceMeasure
      { imBoundNames = Set.singleton (benchInterfaceName ("obj-" <> show (finObjId objectValue))),
        imDeletedNames = Set.empty,
        imCreatedNames = Set.empty,
        imGuarded = Any False
      }

  measureMorphism morphismValue =
    InterfaceMeasure
      { imBoundNames = Set.singleton (benchInterfaceName ("mor-" <> show (finMorId morphismValue))),
        imDeletedNames = Set.empty,
        imCreatedNames = Set.empty,
        imGuarded = Any False
      }

  composeMorphismChain morphismValues =
    case morphismValues of
      [] ->
        Left ()
      firstMorphism : restMorphisms ->
        foldM composeStep firstMorphism restMorphisms
    where
      composeStep :: FinMor -> FinMor -> Either () FinMor
      composeStep accumulatedMorphism nextMorphism =
        case (finMorId nextMorphism, finMorId accumulatedMorphism) of
          (FinIdentityId _, _) ->
            Right accumulatedMorphism
          (_, FinIdentityId _) ->
            Right nextMorphism
          _ ->
            totalOrderCategoryFromMorphisms [accumulatedMorphism, nextMorphism]
              >>= \categoryValue ->
                either
                  (const (Left ()))
                  Right
                  (composeMor categoryValue nextMorphism accumulatedMorphism)

  composeMorphismChainInCategory categoryValue morphismValues =
    case morphismValues of
      [] ->
        Left ()
      firstMorphism : restMorphisms ->
        foldM composeStep firstMorphism restMorphisms
    where
      composeStep :: FinMor -> FinMor -> Either () FinMor
      composeStep accumulatedMorphism nextMorphism =
        either
          (const (Left ()))
          Right
          (composeMor categoryValue nextMorphism accumulatedMorphism)

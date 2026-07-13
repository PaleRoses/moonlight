module Pruning
  ( benchmarks
  , probeCases
  ) where

import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Fixture
  ( BenchmarkChecksum (..)
  , BenchmarkFixture (..)
  , BenchmarkResult
  , ProbeCase
  , ProbeFamily (..)
  , benchmarkSuccess
  , checksumDerivedGF2
  , checksumPoset
  )
import Registry
  ( BenchCase
  , benchCase
  , familyBenchmarks
  , hostileProbeCases
  , preparedBenchCase
  )
import Moonlight.Derived.Complex
  ( Derived
  , derivedPoset
  )
import Moonlight.Derived.Functor
  ( prepareProperPullback
  , properPullback
  )
import Moonlight.Derived.Morse (hypercohomologyDims)
import Moonlight.Derived.Pruning
  ( PreparedVerdierPruning
  , VerdierPreparation (..)
  , SpectralPruningOracle
  , iterativeSpectralPrune
  , laplacianGate
  , mkSpectralPruningOracle
  , prepareVerdierPruning
  , preparedVerdierDual
  , preparedVerdierPrimal
  , spectralPruningGate
  , verdierLocalClosedGate
  )
import Moonlight.Derived.Site
  ( DerivedPoset
  , FinObjectId (..)
  , LocalClosed
  , localClosedNodes
  , mkLocalClosed
  )
import Moonlight.Homology
  ( BasisCellRef (..)
  , Bidegree
  , FormalMap (..)
  , HomologicalDegree (..)
  , HomologyGroup (..)
  , SpectralPage (..)
  , mkBidegree
  )
import Moonlight.LinAlg (GF2)
import Test.Tasty.Bench (Benchmark)

benchmarks :: [BenchmarkFixture] -> Benchmark
benchmarks fixtures =
  familyBenchmarks "pruning" pruningFamilies fixtures

probeCases :: [BenchmarkFixture] -> [ProbeCase]
probeCases =
  hostileProbeCases "pruning" ProbeFamilyMorse pruningFamilies

pruningFamilies :: [BenchCase]
pruningFamilies =
  [ benchCase "prepare-verdier" runPrepareVerdier
  , preparedBenchCase "verdier-query" prepareVerdierBench forcePreparedVerdierBench runPreparedVerdierQuery
  , benchCase "prepare-laplacian" runPrepareLaplacian
  , preparedBenchCase "laplacian-query" prepareLaplacianPruning forcePreparedLaplacian runPreparedLaplacianQuery
  , benchCase "prepare-spectral" runPrepareSpectral
  , preparedBenchCase "spectral-query" prepareSpectralPruning forcePreparedSpectral runPreparedSpectralQuery
  , benchCase "verdier-prepare" runPrepareVerdier
  , preparedBenchCase "verdier-node-set-gate" prepareVerdierBench forcePreparedVerdierBench runPreparedVerdierQuery
  , preparedBenchCase "laplacian-gate" prepareLaplacianPruning forcePreparedLaplacian runPreparedLaplacianQuery
  , preparedBenchCase "spectral-gate" prepareSpectralPruning forcePreparedSpectral runPreparedSpectralQuery
  , preparedBenchCase "spectral-iterative" prepareSpectralPruning forcePreparedSpectral runPreparedSpectralIterative
  ]

data PreparedVerdierBench = PreparedVerdierBench
  { pvbPrepared :: !(Either String VerdierPreparation)
  , pvbRegion :: !LocalClosed
  }

data PreparedLaplacianPruning = PreparedLaplacianPruning
  { plpSeeds :: ![Int]
  , plpDecisionsBySeed :: !(IntMap.IntMap Bool)
  }

data PreparedSpectralPruning = PreparedSpectralPruning
  { pspSeeds :: ![Int]
  , pspRanksBySeed :: !(IntMap.IntMap Int)
  , pspOracle :: !(SpectralPruningOracle Rational)
  }

runPrepareVerdier :: BenchmarkFixture -> BenchmarkResult
runPrepareVerdier fixture =
  benchmarkSuccess
    (checksumPreparedVerdierBench (prepareVerdierBench fixture))

prepareVerdierBench :: BenchmarkFixture -> PreparedVerdierBench
prepareVerdierBench fixture =
  PreparedVerdierBench
    { pvbPrepared =
        firstShow (prepareVerdierPruning (bfSourceDerived fixture))
    , pvbRegion = bfOuterLocalClosed fixture
    }

runPreparedVerdierQuery :: PreparedVerdierBench -> BenchmarkResult
runPreparedVerdierQuery PreparedVerdierBench{pvbPrepared, pvbRegion} =
  case pvbPrepared of
    Left failureMessage -> Left failureMessage
    Right preparedValue ->
      fmap boolChecksum (firstShow (verdierLocalClosedGate preparedValue pvbRegion))
        >>= benchmarkInt

runPrepareLaplacian :: BenchmarkFixture -> BenchmarkResult
runPrepareLaplacian fixture =
  case prepareLaplacianPruning fixture of
    Left failureMessage -> Left failureMessage
    Right preparedValue -> benchmarkSuccess (checksumPreparedLaplacian preparedValue)

prepareLaplacianPruning :: BenchmarkFixture -> Either String PreparedLaplacianPruning
prepareLaplacianPruning fixture = do
  decisionsBySeed <-
    IntMap.fromAscList
      <$> traverse
        ( \seedValue ->
            fmap
              ((,) seedValue)
              ( firstShow
                  ( laplacianGate
                      0.5
                      FinObjectId
                      (HomologicalDegree 0)
                      (bfAmbientPoset fixture)
                      (bfSourceDerived fixture)
                      seedValue
                  )
              )
        )
        seedsValue
  Right PreparedLaplacianPruning
    { plpSeeds = seedsValue
    , plpDecisionsBySeed = decisionsBySeed
    }
  where
    seedsValue =
      supportNodeKeys (localClosedNodes (bfOuterLocalClosed fixture))

runPreparedLaplacianQuery :: Either String PreparedLaplacianPruning -> BenchmarkResult
runPreparedLaplacianQuery preparedResult = do
  preparedValue <- preparedResult
  decisionsValue <-
    maybe
      (Left "laplacian-query: missing prepared seed")
      Right
      (traverse (laplacianPreparedGate preparedValue) (plpSeeds preparedValue))
  benchmarkInt (sum (fmap boolChecksum decisionsValue))

laplacianPreparedGate :: PreparedLaplacianPruning -> Int -> Maybe Bool
laplacianPreparedGate PreparedLaplacianPruning{plpDecisionsBySeed} seedValue =
  IntMap.lookup seedValue plpDecisionsBySeed

runPrepareSpectral :: BenchmarkFixture -> BenchmarkResult
runPrepareSpectral fixture =
  case prepareSpectralPruning fixture of
    Left failureMessage ->
      Left failureMessage
    Right preparedValue ->
      benchmarkSuccess (checksumPreparedSpectral preparedValue)

prepareSpectralPruning :: BenchmarkFixture -> Either String PreparedSpectralPruning
prepareSpectralPruning fixture = do
  ranksBySeed <-
    IntMap.fromAscList
      <$> traverse
        ( \seedValue ->
            fmap
              ((,) seedValue)
              (spectralSeedRank (bfAmbientPoset fixture) (bfSourceDerived fixture) seedValue)
        )
        seedsValue
  pure
    PreparedSpectralPruning
      { pspSeeds = seedsValue
      , pspRanksBySeed = ranksBySeed
      , pspOracle =
          mkSpectralPruningOracle [spectralPageFromRanks ranksBySeed] seedBidegree
      }
  where
    seedsValue =
      supportNodeKeys (localClosedNodes (bfOuterLocalClosed fixture))

spectralSeedRank :: DerivedPoset -> Derived GF2 -> Int -> Either String Int
spectralSeedRank posetValue derivedValue seedValue = do
  supportValue <- firstShow (mkLocalClosed posetValue (IntSet.singleton seedValue))
  preparedPullback <- firstShow (prepareProperPullback supportValue derivedValue)
  let restrictedValue = properPullback preparedPullback
  fmap
    (IntMap.foldl' (+) 0)
    (firstShow (hypercohomologyDims restrictedValue))

runPreparedSpectralQuery :: Either String PreparedSpectralPruning -> BenchmarkResult
runPreparedSpectralQuery preparedResult =
  case preparedResult of
    Left failureMessage ->
      Left failureMessage
    Right preparedValue -> do
      gateResults <-
        firstShow
          ( traverse
              (spectralPruningGate (pspOracle preparedValue) 1 projectSeedCell)
              (pspSeeds preparedValue)
          )
      benchmarkInt (sum (fmap boolChecksum gateResults))

runPreparedSpectralIterative :: Either String PreparedSpectralPruning -> BenchmarkResult
runPreparedSpectralIterative preparedResult =
  case preparedResult of
    Left failureMessage ->
      Left failureMessage
    Right preparedValue ->
      benchmarkInt
        ( sum
            ( fmap
                (\(pageNumber, keptSeeds) -> pageNumber * 97 + length keptSeeds)
                ( iterativeSpectralPrune
                    (pspOracle preparedValue)
                    projectSeedCell
                    (pspSeeds preparedValue)
                )
            )
        )

forcePreparedVerdierBench :: PreparedVerdierBench -> Int
forcePreparedVerdierBench =
  checksumIntValue . checksumPreparedVerdierBench

forcePreparedLaplacian :: Either String PreparedLaplacianPruning -> Int
forcePreparedLaplacian preparedResult =
  case preparedResult of
    Left failureMessage -> length failureMessage
    Right preparedValue -> checksumIntValue (checksumPreparedLaplacian preparedValue)

forcePreparedSpectral :: Either String PreparedSpectralPruning -> Int
forcePreparedSpectral preparedResult =
  case preparedResult of
    Left failureMessage ->
      length failureMessage
    Right preparedValue ->
      checksumIntValue (checksumPreparedSpectral preparedValue)

checksumPreparedVerdierBench :: PreparedVerdierBench -> BenchmarkChecksum
checksumPreparedVerdierBench PreparedVerdierBench{pvbPrepared, pvbRegion} =
  mixChecksumInts
    [ checksumIntValue (checksumPreparedVerdier pvbPrepared)
    , checksumIntSet (localClosedNodes pvbRegion)
    ]

checksumPreparedLaplacian :: PreparedLaplacianPruning -> BenchmarkChecksum
checksumPreparedLaplacian PreparedLaplacianPruning{plpSeeds, plpDecisionsBySeed} =
  mixChecksumInts
    [ checksumIntList plpSeeds
    , IntMap.foldlWithKey'
        ( \checksumValue seedValue decisionValue ->
            checksumValue * 16777619 + seedValue * 31 + boolChecksum decisionValue
        )
        216613626
        plpDecisionsBySeed
    ]

checksumPreparedSpectral :: PreparedSpectralPruning -> BenchmarkChecksum
checksumPreparedSpectral PreparedSpectralPruning{pspSeeds, pspRanksBySeed} =
  mixChecksumInts
    [ checksumIntList pspSeeds
    , IntMap.foldlWithKey'
        ( \checksumValue seedValue rankValue ->
            checksumValue * 16777619 + seedValue * 31 + rankValue
        )
        216613626
        pspRanksBySeed
    ]

checksumIntSet :: IntSet.IntSet -> Int
checksumIntSet =
  checksumIntList . IntSet.toAscList

checksumIntList :: [Int] -> Int
checksumIntList =
  foldl' (\checksum value -> checksum * 16777619 + value) 216613626

firstShow :: Show errorValue => Either errorValue value -> Either String value
firstShow =
  either (Left . show) Right

checksumPreparedVerdier :: Either String VerdierPreparation -> BenchmarkChecksum
checksumPreparedVerdier preparedResult =
  case preparedResult of
    Left failureMessage -> BenchmarkChecksum (length failureMessage)
    Right VerdierNotApplicable -> BenchmarkChecksum 0
    Right (VerdierPrepared preparedValue) ->
      let primalComplex = preparedVerdierPrimal preparedValue
          dualComplex = preparedVerdierDual preparedValue
       in
        mixChecksumInts
          [ checksumIntValue (checksumPoset (derivedPoset primalComplex))
          , checksumIntValue (checksumDerivedGF2 primalComplex)
          , checksumIntValue (checksumDerivedGF2 dualComplex)
          ]

supportNodeKeys :: IntSet.IntSet -> [Int]
supportNodeKeys =
  IntSet.toAscList

spectralPageFromRanks :: IntMap.IntMap Int -> SpectralPage Rational
spectralPageFromRanks ranksBySeed =
  SpectralPage
    { pageIndex = 1
    , groupAt =
        \filtrationDegree complementaryDegree ->
          HomologyGroup
            { freeRank =
                if complementaryDegree == 0
                  then IntMap.findWithDefault 0 filtrationDegree ranksBySeed
                  else 0
            , torsionInvariants = []
            }
    , diffMap =
        \_ _ ->
          FormalMap
            { formalMatrix = []
            , formalDomainBasis = []
            , formalCodomainBasis = []
            }
    , pageEntryMap = Map.empty
    , pageDifferentialMap = Map.empty
    , pageAdvanceSource = Nothing
    , pageAdvanceState = Nothing
    }

projectSeedCell :: Int -> BasisCellRef
projectSeedCell seedValue =
  BasisCellRef
    { cellDegree = HomologicalDegree 0
    , cellIndex = seedValue
    }

seedBidegree :: BasisCellRef -> Bidegree
seedBidegree BasisCellRef{cellIndex} =
  mkBidegree cellIndex 0

benchmarkInt :: Int -> BenchmarkResult
benchmarkInt =
  benchmarkSuccess . BenchmarkChecksum

boolChecksum :: Bool -> Int
boolChecksum flag =
  if flag then 1 else 0

checksumIntValue :: BenchmarkChecksum -> Int
checksumIntValue (BenchmarkChecksum checksumValue) =
  checksumValue

mixChecksumInts :: [Int] -> BenchmarkChecksum
mixChecksumInts =
  BenchmarkChecksum . foldl' (\checksum value -> checksum * 16777619 + value) 216613626

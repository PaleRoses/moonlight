module NativeLapack
  ( nativeLapackBenchmarks,
    nativeLapackOnceBenchmarks,
  )
where

import Control.DeepSeq (NFData (..))
import Data.Bifunctor (first)
import Env (BenchmarkSelection (..))
import Fixtures
  ( benchmarkSeedBlock,
    denseOperator,
    genericBenchmarkTridiagonal,
    pathLaplacianTridiagonal,
    projectedBenchmarkDimension,
    projectedBenchmarkRows,
    projectedBlockBenchmarkCases,
  )
import Types
  ( BenchmarkSetup (..),
    BenchmarkWeight (..),
    OnceBenchmark (..),
    ProjectedBlockBenchmarkCase (..),
    eigenpairsChecksum,
    benchmarkWeightEither,
    prepareBenchmarkSetup,
  )
import Data.Vector.Unboxed qualified as U
import Moonlight.LinAlg.Dense (DynMatrix, mkDynMatrix)
import Moonlight.LinAlg.Krylov
  ( SpectrumEnd (SmallestEigenvalues),
    blockLanczosProjectedBlockTridiagonal,
    blockLanczosSymmetric,
    defaultBlockLanczosConfig,
    mkPositiveCount,
    withBlockLanczosBlockSize,
    withBlockLanczosIterations,
  )
import Moonlight.LinAlg.Native
  ( selectedSymmetricBlockTridiagonalEigenRequestLapack,
    selectedSymmetricTridiagonalEigenRequestLapack,
    symmetricEigenRequestLapack,
  )
import Moonlight.LinAlg.Pure.Structured.BlockTridiagonal (SymmetricBlockTridiagonal)
import Moonlight.LinAlg.Pure.Structured.Tridiagonal (SymmetricTridiagonal)
import Moonlight.LinAlg.Spectral
  ( Eigenpairs,
    EigenRequest (..),
  )
import Test.Tasty.Bench (Benchmark, bench, bgroup, env, nfIO)
import Prelude

data NativeTridiagonalLapackCase = NativeTridiagonalLapackCase
  { nativeTridiagonalLapackKind :: !NativeTridiagonalLapackKind,
    nativeTridiagonalLapackDimension :: !Int,
    nativeTridiagonalLapackModes :: !Int
  }

data NativeTridiagonalLapackKind
  = NativePathLaplacianTridiagonal
  | NativeGenericTridiagonal
  deriving stock (Eq, Show)

data NativeProjectedBandPreparedCase = NativeProjectedBandPreparedCase
  { nativeProjectedBandPreparedCase :: !ProjectedBlockBenchmarkCase,
    nativeProjectedBandOperator :: !SymmetricBlockTridiagonal
  }

instance NFData NativeProjectedBandPreparedCase where
  rnf preparedCase =
    nativeProjectedBandPreparedCase preparedCase
      `seq` nativeProjectedBandOperator preparedCase
      `seq` ()

nativeLapackBenchmarks :: BenchmarkSelection -> Benchmark
nativeLapackBenchmarks benchmarkSelection =
  bgroup
    "native LAPACK symmetric eigensolve"
    ( (nativeLapackBenchmark <$> projectedBlockBenchmarkCases benchmarkSelection)
        <> concatMap nativeDenseSelectedBenchmarkRows (projectedBlockBenchmarkCases benchmarkSelection)
        <> concatMap nativeProjectedBandBenchmarkRows (projectedBlockBenchmarkCases benchmarkSelection)
        <> concatMap nativeTridiagonalLapackBenchmarkRows (nativeTridiagonalLapackCases benchmarkSelection)
    )

nativeLapackOnceBenchmarks :: BenchmarkSelection -> [OnceBenchmark]
nativeLapackOnceBenchmarks benchmarkSelection =
  (nativeLapackOnceBenchmark <$> projectedBlockBenchmarkCases benchmarkSelection)
    <> concatMap nativeDenseSelectedOnceBenchmarkRows (projectedBlockBenchmarkCases benchmarkSelection)
    <> concatMap nativeProjectedBandOnceBenchmarkRows (projectedBlockBenchmarkCases benchmarkSelection)
    <> concatMap nativeTridiagonalLapackOnceBenchmarkRows (nativeTridiagonalLapackCases benchmarkSelection)

nativeLapackBenchmark :: ProjectedBlockBenchmarkCase -> Benchmark
nativeLapackBenchmark benchmarkCase =
  bench (nativeLapackBenchmarkLabel benchmarkCase) (nfIO (nativeLapackWeight benchmarkCase))

nativeLapackOnceBenchmark :: ProjectedBlockBenchmarkCase -> OnceBenchmark
nativeLapackOnceBenchmark benchmarkCase =
  OnceBenchmark
    { onceBenchmarkLabel = "native LAPACK symmetric eigensolve." <> nativeLapackBenchmarkLabel benchmarkCase,
      onceBenchmarkAction = benchmarkWeightEither <$> nativeLapackWeight benchmarkCase
    }

nativeLapackBenchmarkLabel :: ProjectedBlockBenchmarkCase -> String
nativeLapackBenchmarkLabel benchmarkCase =
  projectedBenchmarkLabel benchmarkCase
    <> " profile="
    <> show (projectedBenchmarkSpectrumProfile benchmarkCase)
    <> " n="
    <> show (projectedBenchmarkDimension benchmarkCase)

nativeLapackWeight :: ProjectedBlockBenchmarkCase -> IO BenchmarkWeight
nativeLapackWeight benchmarkCase =
  case mkDynMatrix
    (projectedBenchmarkDimension benchmarkCase)
    (projectedBenchmarkDimension benchmarkCase)
    (concat (projectedBenchmarkRows benchmarkCase)) of
    Left err -> pure (BenchmarkMeasurementFailure (projectedBenchmarkLabel benchmarkCase <> ": " <> show err))
    Right matrixValue ->
      case mkPositiveCount (projectedBenchmarkDimension benchmarkCase) of
        Left err -> pure (BenchmarkMeasurementFailure (projectedBenchmarkLabel benchmarkCase <> ": " <> show err))
        Right requestedCount ->
          symmetricEigenRequestLapack (EigenpairsRequest SmallestEigenvalues requestedCount) matrixValue
            >>= nativeEigenpairsWeight (projectedBenchmarkLabel benchmarkCase)

nativeDenseSelectedBenchmarkRows :: ProjectedBlockBenchmarkCase -> [Benchmark]
nativeDenseSelectedBenchmarkRows benchmarkCase =
  [ bench (nativeDenseSelectedValuesLabel benchmarkCase) (nfIO (nativeDenseSelectedValuesWeight benchmarkCase)),
    bench (nativeDenseSelectedPairsLabel benchmarkCase) (nfIO (nativeDenseSelectedPairsWeight benchmarkCase))
  ]

nativeDenseSelectedOnceBenchmarkRows :: ProjectedBlockBenchmarkCase -> [OnceBenchmark]
nativeDenseSelectedOnceBenchmarkRows benchmarkCase =
  [ OnceBenchmark
      { onceBenchmarkLabel = "native LAPACK symmetric eigensolve." <> nativeDenseSelectedValuesLabel benchmarkCase,
        onceBenchmarkAction = benchmarkWeightEither <$> nativeDenseSelectedValuesWeight benchmarkCase
      },
    OnceBenchmark
      { onceBenchmarkLabel = "native LAPACK symmetric eigensolve." <> nativeDenseSelectedPairsLabel benchmarkCase,
        onceBenchmarkAction = benchmarkWeightEither <$> nativeDenseSelectedPairsWeight benchmarkCase
      }
  ]

nativeDenseSelectedValuesLabel :: ProjectedBlockBenchmarkCase -> String
nativeDenseSelectedValuesLabel =
  nativeDenseSelectedLabel "DSYEVX dense values"

nativeDenseSelectedPairsLabel :: ProjectedBlockBenchmarkCase -> String
nativeDenseSelectedPairsLabel =
  nativeDenseSelectedLabel "DSYEVX dense pairs"

nativeDenseSelectedLabel :: String -> ProjectedBlockBenchmarkCase -> String
nativeDenseSelectedLabel requestLabel benchmarkCase =
  projectedBenchmarkLabel benchmarkCase
    <> " "
    <> requestLabel
    <> " modes="
    <> show (projectedBenchmarkRequestedModes benchmarkCase)
    <> " profile="
    <> show (projectedBenchmarkSpectrumProfile benchmarkCase)
    <> " n="
    <> show (projectedBenchmarkDimension benchmarkCase)

nativeDenseSelectedValuesWeight :: ProjectedBlockBenchmarkCase -> IO BenchmarkWeight
nativeDenseSelectedValuesWeight benchmarkCase =
  case prepareNativeDenseMatrix benchmarkCase of
    Left err -> pure (BenchmarkMeasurementFailure (nativeDenseSelectedValuesLabel benchmarkCase <> ": " <> err))
    Right matrixValue ->
      case mkPositiveCount (projectedBenchmarkRequestedModes benchmarkCase) of
        Left err -> pure (BenchmarkMeasurementFailure (nativeDenseSelectedValuesLabel benchmarkCase <> ": " <> show err))
        Right requestedCount ->
          symmetricEigenRequestLapack (EigenvaluesRequest SmallestEigenvalues requestedCount) matrixValue
            >>= nativeEigenvaluesWeight (nativeDenseSelectedValuesLabel benchmarkCase)

nativeDenseSelectedPairsWeight :: ProjectedBlockBenchmarkCase -> IO BenchmarkWeight
nativeDenseSelectedPairsWeight benchmarkCase =
  case prepareNativeDenseMatrix benchmarkCase of
    Left err -> pure (BenchmarkMeasurementFailure (nativeDenseSelectedPairsLabel benchmarkCase <> ": " <> err))
    Right matrixValue ->
      case mkPositiveCount (projectedBenchmarkRequestedModes benchmarkCase) of
        Left err -> pure (BenchmarkMeasurementFailure (nativeDenseSelectedPairsLabel benchmarkCase <> ": " <> show err))
        Right requestedCount ->
          symmetricEigenRequestLapack (EigenpairsRequest SmallestEigenvalues requestedCount) matrixValue
            >>= nativeEigenpairsWeight (nativeDenseSelectedPairsLabel benchmarkCase)

prepareNativeDenseMatrix :: ProjectedBlockBenchmarkCase -> Either String (DynMatrix Double)
prepareNativeDenseMatrix benchmarkCase =
  first
    show
    ( mkDynMatrix
        (projectedBenchmarkDimension benchmarkCase)
        (projectedBenchmarkDimension benchmarkCase)
        (concat (projectedBenchmarkRows benchmarkCase))
    )

nativeProjectedBandBenchmarkRows :: ProjectedBlockBenchmarkCase -> [Benchmark]
nativeProjectedBandBenchmarkRows benchmarkCase =
  [ nativeProjectedBandBenchmark nativeProjectedBandValuesLabel nativeProjectedBandValuesWeight benchmarkCase,
    nativeProjectedBandBenchmark nativeProjectedBandPairsLabel nativeProjectedBandPairsWeight benchmarkCase
  ]

nativeProjectedBandOnceBenchmarkRows :: ProjectedBlockBenchmarkCase -> [OnceBenchmark]
nativeProjectedBandOnceBenchmarkRows benchmarkCase =
  [ nativeProjectedBandOnceBenchmark nativeProjectedBandValuesLabel nativeProjectedBandValuesWeight benchmarkCase,
    nativeProjectedBandOnceBenchmark nativeProjectedBandPairsLabel nativeProjectedBandPairsWeight benchmarkCase
  ]

nativeProjectedBandBenchmark ::
  (ProjectedBlockBenchmarkCase -> String) ->
  (NativeProjectedBandPreparedCase -> IO BenchmarkWeight) ->
  ProjectedBlockBenchmarkCase ->
  Benchmark
nativeProjectedBandBenchmark benchmarkLabel benchmarkWeight benchmarkCase =
  env (prepareBenchmarkSetup (prepareNativeProjectedBandCase benchmarkCase)) $ \preparedCase ->
    bench (benchmarkLabel benchmarkCase) (nfIO (benchmarkWeight preparedCase))

nativeProjectedBandOnceBenchmark ::
  (ProjectedBlockBenchmarkCase -> String) ->
  (NativeProjectedBandPreparedCase -> IO BenchmarkWeight) ->
  ProjectedBlockBenchmarkCase ->
  OnceBenchmark
nativeProjectedBandOnceBenchmark benchmarkLabel benchmarkWeight benchmarkCase =
  OnceBenchmark
    { onceBenchmarkLabel = "native LAPACK symmetric eigensolve." <> benchmarkLabel benchmarkCase,
      onceBenchmarkAction =
        case runBenchmarkSetup (prepareNativeProjectedBandCase benchmarkCase) of
          Left err -> pure (Left err)
          Right preparedCase -> benchmarkWeightEither <$> benchmarkWeight preparedCase
    }

prepareNativeProjectedBandCase :: ProjectedBlockBenchmarkCase -> BenchmarkSetup NativeProjectedBandPreparedCase
prepareNativeProjectedBandCase benchmarkCase =
  BenchmarkSetup $ do
    iterationCount <-
      first
        (\err -> "invalid native projected-band iteration count for " <> projectedBenchmarkLabel benchmarkCase <> ": " <> show err)
        (mkPositiveCount (projectedBenchmarkIterations benchmarkCase))
    blockSize <-
      first
        (\err -> "invalid native projected-band block size for " <> projectedBenchmarkLabel benchmarkCase <> ": " <> show err)
        (mkPositiveCount (projectedBenchmarkBlockSize benchmarkCase))
    operatorValue <-
      first
        (\err -> "native projected-band operator construction failed for " <> projectedBenchmarkLabel benchmarkCase <> ": " <> err)
        (denseOperator (projectedBenchmarkRows benchmarkCase))
    let operatorDimension = projectedBenchmarkDimension benchmarkCase
        seedBlock = benchmarkSeedBlock operatorDimension (projectedBenchmarkBlockSize benchmarkCase)
        blockConfig =
          withBlockLanczosBlockSize
            blockSize
            (withBlockLanczosIterations iterationCount defaultBlockLanczosConfig)
    decomposition <-
      first
        (\err -> "native projected-band decomposition failed for " <> projectedBenchmarkLabel benchmarkCase <> ": " <> show err)
        (blockLanczosSymmetric blockConfig operatorValue seedBlock)
    pure
      NativeProjectedBandPreparedCase
        { nativeProjectedBandPreparedCase = benchmarkCase,
          nativeProjectedBandOperator = blockLanczosProjectedBlockTridiagonal decomposition
        }

nativeProjectedBandValuesLabel :: ProjectedBlockBenchmarkCase -> String
nativeProjectedBandValuesLabel benchmarkCase =
  nativeProjectedBandLabel "DSBEVX projected block values" benchmarkCase

nativeProjectedBandPairsLabel :: ProjectedBlockBenchmarkCase -> String
nativeProjectedBandPairsLabel benchmarkCase =
  nativeProjectedBandLabel "DSBEVX projected block pairs" benchmarkCase

nativeProjectedBandLabel :: String -> ProjectedBlockBenchmarkCase -> String
nativeProjectedBandLabel requestLabel benchmarkCase =
  projectedBenchmarkLabel benchmarkCase
    <> " "
    <> requestLabel
    <> " modes="
    <> show (projectedBenchmarkRequestedModes benchmarkCase)
    <> " profile="
    <> show (projectedBenchmarkSpectrumProfile benchmarkCase)
    <> " n="
    <> show (projectedBenchmarkDimension benchmarkCase)

nativeProjectedBandValuesWeight :: NativeProjectedBandPreparedCase -> IO BenchmarkWeight
nativeProjectedBandValuesWeight preparedCase =
  case mkPositiveCount (projectedBenchmarkRequestedModes (nativeProjectedBandPreparedCase preparedCase)) of
    Left err -> pure (BenchmarkMeasurementFailure (projectedBenchmarkLabel (nativeProjectedBandPreparedCase preparedCase) <> " DSBEVX values: " <> show err))
    Right requestedCount ->
      selectedSymmetricBlockTridiagonalEigenRequestLapack
        (EigenvaluesRequest SmallestEigenvalues requestedCount)
        (nativeProjectedBandOperator preparedCase)
        >>= nativeEigenvaluesWeight (nativeProjectedBandValuesLabel (nativeProjectedBandPreparedCase preparedCase))

nativeProjectedBandPairsWeight :: NativeProjectedBandPreparedCase -> IO BenchmarkWeight
nativeProjectedBandPairsWeight preparedCase =
  case mkPositiveCount (projectedBenchmarkRequestedModes (nativeProjectedBandPreparedCase preparedCase)) of
    Left err -> pure (BenchmarkMeasurementFailure (projectedBenchmarkLabel (nativeProjectedBandPreparedCase preparedCase) <> " DSBEVX pairs: " <> show err))
    Right requestedCount ->
      selectedSymmetricBlockTridiagonalEigenRequestLapack
        (EigenpairsRequest SmallestEigenvalues requestedCount)
        (nativeProjectedBandOperator preparedCase)
        >>= nativeEigenpairsWeight (nativeProjectedBandPairsLabel (nativeProjectedBandPreparedCase preparedCase))

nativeTridiagonalLapackCases :: BenchmarkSelection -> [NativeTridiagonalLapackCase]
nativeTridiagonalLapackCases benchmarkSelection =
  [ NativeTridiagonalLapackCase NativePathLaplacianTridiagonal 512 4,
    NativeTridiagonalLapackCase NativeGenericTridiagonal 512 4
  ]
    <> [NativeTridiagonalLapackCase NativePathLaplacianTridiagonal 10000 4 | includeNativeLarge benchmarkSelection]

nativeTridiagonalLapackBenchmarkRows :: NativeTridiagonalLapackCase -> [Benchmark]
nativeTridiagonalLapackBenchmarkRows benchmarkCase =
  [ bench (nativeTridiagonalLapackValuesLabel benchmarkCase) (nfIO (nativeTridiagonalLapackValuesWeight benchmarkCase)),
    bench (nativeTridiagonalLapackPairsLabel benchmarkCase) (nfIO (nativeTridiagonalLapackPairsWeight benchmarkCase))
  ]

nativeTridiagonalLapackOnceBenchmarkRows :: NativeTridiagonalLapackCase -> [OnceBenchmark]
nativeTridiagonalLapackOnceBenchmarkRows benchmarkCase =
  [ OnceBenchmark
      { onceBenchmarkLabel = "native LAPACK symmetric eigensolve." <> nativeTridiagonalLapackValuesLabel benchmarkCase,
        onceBenchmarkAction = benchmarkWeightEither <$> nativeTridiagonalLapackValuesWeight benchmarkCase
      },
    OnceBenchmark
      { onceBenchmarkLabel = "native LAPACK symmetric eigensolve." <> nativeTridiagonalLapackPairsLabel benchmarkCase,
        onceBenchmarkAction = benchmarkWeightEither <$> nativeTridiagonalLapackPairsWeight benchmarkCase
      }
  ]

nativeTridiagonalLapackValuesLabel :: NativeTridiagonalLapackCase -> String
nativeTridiagonalLapackValuesLabel benchmarkCase =
  nativeTridiagonalLapackLabelPrefix benchmarkCase
    <> " DSTEMR selected tridiagonal values modes="
    <> show (nativeTridiagonalLapackModes benchmarkCase)

nativeTridiagonalLapackPairsLabel :: NativeTridiagonalLapackCase -> String
nativeTridiagonalLapackPairsLabel benchmarkCase =
  nativeTridiagonalLapackLabelPrefix benchmarkCase
    <> " DSTEMR selected tridiagonal pairs modes="
    <> show (nativeTridiagonalLapackModes benchmarkCase)

nativeTridiagonalLapackLabelPrefix :: NativeTridiagonalLapackCase -> String
nativeTridiagonalLapackLabelPrefix benchmarkCase =
  nativeTridiagonalLapackKindLabel (nativeTridiagonalLapackKind benchmarkCase)
    <> show (nativeTridiagonalLapackDimension benchmarkCase)

nativeTridiagonalLapackKindLabel :: NativeTridiagonalLapackKind -> String
nativeTridiagonalLapackKindLabel benchmarkKind =
  case benchmarkKind of
    NativePathLaplacianTridiagonal -> "path-laplacian-"
    NativeGenericTridiagonal -> "generic-tridiagonal-"

nativeTridiagonalLapackValuesWeight :: NativeTridiagonalLapackCase -> IO BenchmarkWeight
nativeTridiagonalLapackValuesWeight benchmarkCase =
  case mkPositiveCount (nativeTridiagonalLapackModes benchmarkCase) of
    Left err -> pure (BenchmarkMeasurementFailure (nativeTridiagonalLapackValuesLabel benchmarkCase <> ": " <> show err))
    Right requestedCount ->
      case nativeTridiagonalLapackOperator benchmarkCase of
        Left err -> pure (BenchmarkMeasurementFailure (nativeTridiagonalLapackValuesLabel benchmarkCase <> ": " <> err))
        Right tridiagonalValue ->
          selectedSymmetricTridiagonalEigenRequestLapack (EigenvaluesRequest SmallestEigenvalues requestedCount) tridiagonalValue
            >>= nativeEigenvaluesWeight (nativeTridiagonalLapackValuesLabel benchmarkCase)

nativeTridiagonalLapackPairsWeight :: NativeTridiagonalLapackCase -> IO BenchmarkWeight
nativeTridiagonalLapackPairsWeight benchmarkCase =
  case mkPositiveCount (nativeTridiagonalLapackModes benchmarkCase) of
    Left err -> pure (BenchmarkMeasurementFailure (nativeTridiagonalLapackPairsLabel benchmarkCase <> ": " <> show err))
    Right requestedCount ->
      case nativeTridiagonalLapackOperator benchmarkCase of
        Left err -> pure (BenchmarkMeasurementFailure (nativeTridiagonalLapackPairsLabel benchmarkCase <> ": " <> err))
        Right tridiagonalValue ->
          selectedSymmetricTridiagonalEigenRequestLapack (EigenpairsRequest SmallestEigenvalues requestedCount) tridiagonalValue
            >>= nativeEigenpairsWeight (nativeTridiagonalLapackPairsLabel benchmarkCase)

nativeTridiagonalLapackOperator :: NativeTridiagonalLapackCase -> Either String SymmetricTridiagonal
nativeTridiagonalLapackOperator benchmarkCase =
  case nativeTridiagonalLapackKind benchmarkCase of
    NativePathLaplacianTridiagonal ->
      pathLaplacianTridiagonal (nativeTridiagonalLapackDimension benchmarkCase)
    NativeGenericTridiagonal ->
      genericBenchmarkTridiagonal (nativeTridiagonalLapackDimension benchmarkCase)

nativeEigenvaluesWeight :: Show err => String -> Either err (U.Vector Double) -> IO BenchmarkWeight
nativeEigenvaluesWeight label eigenResult =
  pure
    ( case eigenResult of
        Left err -> BenchmarkMeasurementFailure (label <> ": " <> show err)
        Right values -> BenchmarkWeight (U.sum values)
    )

nativeEigenpairsWeight :: Show err => String -> Either err Eigenpairs -> IO BenchmarkWeight
nativeEigenpairsWeight label eigenResult =
  pure
    ( case eigenResult of
        Left err -> BenchmarkMeasurementFailure (label <> ": " <> show err)
        Right pairs -> BenchmarkWeight (eigenpairsChecksum pairs)
    )

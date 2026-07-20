module Main (main) where

import Data.Word (Word64)
import Moonlight.Pale.Bench.Measure
  ( FreshMeasurement (..),
    FreshRtsCounter (..),
    FreshRtsDelta (..),
    FreshRtsDeltaObstruction (..),
    FreshRtsSnapshot (..),
    checkedFreshRtsDelta,
    finalizeFreshMeasurement,
    measureFreshSample,
  )
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), assertFailure, testCase)

main :: IO ()
main =
  defaultMain
    ( testGroup
        "fresh RTS measurement"
        [ testCase "all monotone counters produce their exact differences" monotoneDeltaLaw,
          testCase "equal snapshots produce the zero delta" zeroDeltaLaw,
          testGroup "counter regression obstructions" (fmap counterRegressionLaw allFreshRtsCounters),
          testGroup
            "extreme monotone signed counters"
            (fmap extremeSignedCounterLaw allFreshSignedRtsCounters),
          testCase
            "pure finalization selects post-GC retained and post-action peak"
            finalizationSnapshotLaw,
          testCase "RTS-backed measurement integration succeeds" measurementIntegrationSmokeLaw
        ]
    )

monotoneDeltaLaw :: IO ()
monotoneDeltaLaw =
  checkedFreshRtsDelta monotoneBeforeSnapshot monotoneAfterSnapshot
    @?= Right expectedMonotoneDelta

zeroDeltaLaw :: IO ()
zeroDeltaLaw =
  checkedFreshRtsDelta monotoneBeforeSnapshot monotoneBeforeSnapshot
    @?= Right zeroFreshRtsDelta

counterRegressionLaw :: FreshRtsCounter -> TestTree
counterRegressionLaw counter =
  testCase (show counter) $ do
    let (beforeSnapshot, afterSnapshot) = regressionSnapshots counter
    checkedFreshRtsDelta beforeSnapshot afterSnapshot
      @?= Left (FreshRtsCounterRegression counter 2 1)

data FreshSignedRtsCounter
  = FreshSignedRtsCounterMutatorCpuNanoseconds
  | FreshSignedRtsCounterMutatorElapsedNanoseconds
  | FreshSignedRtsCounterGcCpuNanoseconds
  | FreshSignedRtsCounterGcElapsedNanoseconds
  | FreshSignedRtsCounterCpuNanoseconds
  | FreshSignedRtsCounterElapsedNanoseconds
  deriving stock (Eq, Show)

extremeSignedCounterLaw :: FreshSignedRtsCounter -> TestTree
extremeSignedCounterLaw counter =
  testCase (show counter) $
    case checkedFreshRtsDelta beforeSnapshot afterSnapshot of
      Left obstructionValue ->
        assertFailure ("extreme monotone counter was rejected: " <> show obstructionValue)
      Right deltaValue ->
        signedCounterDelta counter deltaValue @?= maxBound
  where
    (beforeSnapshot, afterSnapshot) = extremeSignedSnapshots counter

finalizationSnapshotLaw :: IO ()
finalizationSnapshotLaw =
  case finalizeFreshMeasurement 17 beforeSnapshot afterActionSnapshot afterPostGcSnapshot "value" 29 of
    Left obstructionValue ->
      assertFailure ("valid finalization was rejected: " <> show obstructionValue)
    Right measurement -> do
      freshMeasurementElapsedNanoseconds measurement @?= 17
      freshRtsDeltaAllocatedBytes (freshMeasurementRtsDelta measurement) @?= 23
      freshMeasurementRetainedLiveBytes measurement @?= 31
      freshMeasurementPeakLiveBytesThroughAction measurement @?= 37
      freshMeasurementValue measurement @?= "value"
      freshMeasurementDigest measurement @?= 29
  where
    beforeSnapshot = zeroFreshRtsSnapshot
    afterActionSnapshot =
      zeroFreshRtsSnapshot
        { freshRtsSnapshotAllocatedBytes = 23
        , freshRtsSnapshotLiveBytes = 41
        , freshRtsSnapshotMaxLiveBytes = 37
        }
    afterPostGcSnapshot =
      afterActionSnapshot
        { freshRtsSnapshotLiveBytes = 31
        , freshRtsSnapshotMaxLiveBytes = 43
        }

measurementIntegrationSmokeLaw :: IO ()
measurementIntegrationSmokeLaw = do
  measurementResult <-
    measureFreshSample
      1
      41
      (\input -> pure (Right (input + 1) :: Either String Int))
      (\value -> value `seq` ())
      id
  case measurementResult of
    Left failure ->
      assertFailure ("RTS-backed measurement failed: " <> show failure)
    Right measurement -> do
      freshMeasurementValue measurement @?= 42
      freshMeasurementDigest measurement @?= 42

allFreshRtsCounters :: [FreshRtsCounter]
allFreshRtsCounters =
  [ FreshRtsCounterGcs,
    FreshRtsCounterMajorGcs,
    FreshRtsCounterAllocatedBytes,
    FreshRtsCounterCopiedBytes,
    FreshRtsCounterMutatorCpuNanoseconds,
    FreshRtsCounterMutatorElapsedNanoseconds,
    FreshRtsCounterGcCpuNanoseconds,
    FreshRtsCounterGcElapsedNanoseconds,
    FreshRtsCounterCpuNanoseconds,
    FreshRtsCounterElapsedNanoseconds
  ]

allFreshSignedRtsCounters :: [FreshSignedRtsCounter]
allFreshSignedRtsCounters =
  [ FreshSignedRtsCounterMutatorCpuNanoseconds
  , FreshSignedRtsCounterMutatorElapsedNanoseconds
  , FreshSignedRtsCounterGcCpuNanoseconds
  , FreshSignedRtsCounterGcElapsedNanoseconds
  , FreshSignedRtsCounterCpuNanoseconds
  , FreshSignedRtsCounterElapsedNanoseconds
  ]

extremeSignedSnapshots :: FreshSignedRtsCounter -> (FreshRtsSnapshot, FreshRtsSnapshot)
extremeSignedSnapshots counter =
  case counter of
    FreshSignedRtsCounterMutatorCpuNanoseconds ->
      ( zeroFreshRtsSnapshot {freshRtsSnapshotMutatorCpuNanoseconds = minBound}
      , zeroFreshRtsSnapshot {freshRtsSnapshotMutatorCpuNanoseconds = maxBound}
      )
    FreshSignedRtsCounterMutatorElapsedNanoseconds ->
      ( zeroFreshRtsSnapshot {freshRtsSnapshotMutatorElapsedNanoseconds = minBound}
      , zeroFreshRtsSnapshot {freshRtsSnapshotMutatorElapsedNanoseconds = maxBound}
      )
    FreshSignedRtsCounterGcCpuNanoseconds ->
      ( zeroFreshRtsSnapshot {freshRtsSnapshotGcCpuNanoseconds = minBound}
      , zeroFreshRtsSnapshot {freshRtsSnapshotGcCpuNanoseconds = maxBound}
      )
    FreshSignedRtsCounterGcElapsedNanoseconds ->
      ( zeroFreshRtsSnapshot {freshRtsSnapshotGcElapsedNanoseconds = minBound}
      , zeroFreshRtsSnapshot {freshRtsSnapshotGcElapsedNanoseconds = maxBound}
      )
    FreshSignedRtsCounterCpuNanoseconds ->
      ( zeroFreshRtsSnapshot {freshRtsSnapshotCpuNanoseconds = minBound}
      , zeroFreshRtsSnapshot {freshRtsSnapshotCpuNanoseconds = maxBound}
      )
    FreshSignedRtsCounterElapsedNanoseconds ->
      ( zeroFreshRtsSnapshot {freshRtsSnapshotElapsedNanoseconds = minBound}
      , zeroFreshRtsSnapshot {freshRtsSnapshotElapsedNanoseconds = maxBound}
      )

signedCounterDelta :: FreshSignedRtsCounter -> FreshRtsDelta -> Word64
signedCounterDelta counter deltaValue =
  case counter of
    FreshSignedRtsCounterMutatorCpuNanoseconds -> freshRtsDeltaMutatorCpuNanoseconds deltaValue
    FreshSignedRtsCounterMutatorElapsedNanoseconds -> freshRtsDeltaMutatorElapsedNanoseconds deltaValue
    FreshSignedRtsCounterGcCpuNanoseconds -> freshRtsDeltaGcCpuNanoseconds deltaValue
    FreshSignedRtsCounterGcElapsedNanoseconds -> freshRtsDeltaGcElapsedNanoseconds deltaValue
    FreshSignedRtsCounterCpuNanoseconds -> freshRtsDeltaCpuNanoseconds deltaValue
    FreshSignedRtsCounterElapsedNanoseconds -> freshRtsDeltaElapsedNanoseconds deltaValue

regressionSnapshots :: FreshRtsCounter -> (FreshRtsSnapshot, FreshRtsSnapshot)
regressionSnapshots = \case
  FreshRtsCounterGcs ->
    ( zeroFreshRtsSnapshot {freshRtsSnapshotGcs = 2},
      zeroFreshRtsSnapshot {freshRtsSnapshotGcs = 1}
    )
  FreshRtsCounterMajorGcs ->
    ( zeroFreshRtsSnapshot {freshRtsSnapshotMajorGcs = 2},
      zeroFreshRtsSnapshot {freshRtsSnapshotMajorGcs = 1}
    )
  FreshRtsCounterAllocatedBytes ->
    ( zeroFreshRtsSnapshot {freshRtsSnapshotAllocatedBytes = 2},
      zeroFreshRtsSnapshot {freshRtsSnapshotAllocatedBytes = 1}
    )
  FreshRtsCounterCopiedBytes ->
    ( zeroFreshRtsSnapshot {freshRtsSnapshotCopiedBytes = 2},
      zeroFreshRtsSnapshot {freshRtsSnapshotCopiedBytes = 1}
    )
  FreshRtsCounterMutatorCpuNanoseconds ->
    ( zeroFreshRtsSnapshot {freshRtsSnapshotMutatorCpuNanoseconds = 2},
      zeroFreshRtsSnapshot {freshRtsSnapshotMutatorCpuNanoseconds = 1}
    )
  FreshRtsCounterMutatorElapsedNanoseconds ->
    ( zeroFreshRtsSnapshot {freshRtsSnapshotMutatorElapsedNanoseconds = 2},
      zeroFreshRtsSnapshot {freshRtsSnapshotMutatorElapsedNanoseconds = 1}
    )
  FreshRtsCounterGcCpuNanoseconds ->
    ( zeroFreshRtsSnapshot {freshRtsSnapshotGcCpuNanoseconds = 2},
      zeroFreshRtsSnapshot {freshRtsSnapshotGcCpuNanoseconds = 1}
    )
  FreshRtsCounterGcElapsedNanoseconds ->
    ( zeroFreshRtsSnapshot {freshRtsSnapshotGcElapsedNanoseconds = 2},
      zeroFreshRtsSnapshot {freshRtsSnapshotGcElapsedNanoseconds = 1}
    )
  FreshRtsCounterCpuNanoseconds ->
    ( zeroFreshRtsSnapshot {freshRtsSnapshotCpuNanoseconds = 2},
      zeroFreshRtsSnapshot {freshRtsSnapshotCpuNanoseconds = 1}
    )
  FreshRtsCounterElapsedNanoseconds ->
    ( zeroFreshRtsSnapshot {freshRtsSnapshotElapsedNanoseconds = 2},
      zeroFreshRtsSnapshot {freshRtsSnapshotElapsedNanoseconds = 1}
    )

zeroFreshRtsSnapshot :: FreshRtsSnapshot
zeroFreshRtsSnapshot =
  FreshRtsSnapshot
    { freshRtsSnapshotGcs = 0,
      freshRtsSnapshotMajorGcs = 0,
      freshRtsSnapshotAllocatedBytes = 0,
      freshRtsSnapshotCopiedBytes = 0,
      freshRtsSnapshotMutatorCpuNanoseconds = 0,
      freshRtsSnapshotMutatorElapsedNanoseconds = 0,
      freshRtsSnapshotGcCpuNanoseconds = 0,
      freshRtsSnapshotGcElapsedNanoseconds = 0,
      freshRtsSnapshotCpuNanoseconds = 0,
      freshRtsSnapshotElapsedNanoseconds = 0,
      freshRtsSnapshotLiveBytes = 0,
      freshRtsSnapshotMaxLiveBytes = 0
    }

monotoneBeforeSnapshot :: FreshRtsSnapshot
monotoneBeforeSnapshot =
  FreshRtsSnapshot
    { freshRtsSnapshotGcs = 2,
      freshRtsSnapshotMajorGcs = 4,
      freshRtsSnapshotAllocatedBytes = 10,
      freshRtsSnapshotCopiedBytes = 40,
      freshRtsSnapshotMutatorCpuNanoseconds = 50,
      freshRtsSnapshotMutatorElapsedNanoseconds = 60,
      freshRtsSnapshotGcCpuNanoseconds = 70,
      freshRtsSnapshotGcElapsedNanoseconds = 80,
      freshRtsSnapshotCpuNanoseconds = 90,
      freshRtsSnapshotElapsedNanoseconds = 100,
      freshRtsSnapshotLiveBytes = 11,
      freshRtsSnapshotMaxLiveBytes = 12
    }

monotoneAfterSnapshot :: FreshRtsSnapshot
monotoneAfterSnapshot =
  FreshRtsSnapshot
    { freshRtsSnapshotGcs = 5,
      freshRtsSnapshotMajorGcs = 7,
      freshRtsSnapshotAllocatedBytes = 110,
      freshRtsSnapshotCopiedBytes = 240,
      freshRtsSnapshotMutatorCpuNanoseconds = 350,
      freshRtsSnapshotMutatorElapsedNanoseconds = 460,
      freshRtsSnapshotGcCpuNanoseconds = 570,
      freshRtsSnapshotGcElapsedNanoseconds = 680,
      freshRtsSnapshotCpuNanoseconds = 790,
      freshRtsSnapshotElapsedNanoseconds = 900,
      freshRtsSnapshotLiveBytes = 13,
      freshRtsSnapshotMaxLiveBytes = 14
    }

expectedMonotoneDelta :: FreshRtsDelta
expectedMonotoneDelta =
  FreshRtsDelta
    { freshRtsDeltaGcs = 3,
      freshRtsDeltaMajorGcs = 3,
      freshRtsDeltaAllocatedBytes = 100,
      freshRtsDeltaCopiedBytes = 200,
      freshRtsDeltaMutatorCpuNanoseconds = 300,
      freshRtsDeltaMutatorElapsedNanoseconds = 400,
      freshRtsDeltaGcCpuNanoseconds = 500,
      freshRtsDeltaGcElapsedNanoseconds = 600,
      freshRtsDeltaCpuNanoseconds = 700,
      freshRtsDeltaElapsedNanoseconds = 800
    }

zeroFreshRtsDelta :: FreshRtsDelta
zeroFreshRtsDelta =
  FreshRtsDelta
    { freshRtsDeltaGcs = 0,
      freshRtsDeltaMajorGcs = 0,
      freshRtsDeltaAllocatedBytes = 0,
      freshRtsDeltaCopiedBytes = 0,
      freshRtsDeltaMutatorCpuNanoseconds = 0,
      freshRtsDeltaMutatorElapsedNanoseconds = 0,
      freshRtsDeltaGcCpuNanoseconds = 0,
      freshRtsDeltaGcElapsedNanoseconds = 0,
      freshRtsDeltaCpuNanoseconds = 0,
      freshRtsDeltaElapsedNanoseconds = 0
    }

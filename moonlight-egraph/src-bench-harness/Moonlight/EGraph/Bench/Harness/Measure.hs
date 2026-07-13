-- | E-graph probe policy and repeated-sample orchestration over the shared Pale sampler.
module Moonlight.EGraph.Bench.Harness.Measure
  ( Probe (..),
    digestOnlyProbe,
    SamplePolicy (..),
    defaultSamplePolicy,
    fixedThreeSamplePolicy,
    Sampled (..),
    sampledMedianNs,
    medianNs,
    samplePoint,
    sampleWithinBudget,
  ) where

import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Moonlight.Pale.Bench.Measure
  ( TimedSample (..),
    timeFreshSample,
  )
import Moonlight.EGraph.Bench.Harness.Run (BenchFailure)
import System.Timeout (timeout)

data Probe input value = Probe
  { probeLabel :: !String,
    probeRun :: input -> Either BenchFailure value,
    probeDigest :: value -> Int
  }

digestOnlyProbe :: Probe input value -> Probe input Int
digestOnlyProbe probe =
  Probe
    { probeLabel = probeLabel probe,
      probeRun = fmap (probeDigest probe) . probeRun probe,
      probeDigest = id
    }

data SamplePolicy = SamplePolicy
  { spBaseSamples :: !Int,
    spSingleSampleThresholdNs :: !Word64
  }

defaultSamplePolicy :: SamplePolicy
defaultSamplePolicy =
  SamplePolicy 3 (30 * 1000 * 1000 * 1000)

fixedThreeSamplePolicy :: SamplePolicy
fixedThreeSamplePolicy =
  defaultSamplePolicy {spSingleSampleThresholdNs = maxBound}

data Sampled value = Sampled
  { sampledFirst :: !(TimedSample value),
    sampledElapsedNanoseconds :: !(NonEmpty Word64),
    sampledDigest :: !Int
  }

sampledMedianNs :: Sampled value -> Word64
sampledMedianNs = medianNs . sampledElapsedNanoseconds

medianNs :: NonEmpty Word64 -> Word64
medianNs samples =
  maybe
    (NonEmpty.last sorted)
    NonEmpty.head
    (NonEmpty.nonEmpty (NonEmpty.drop (NonEmpty.length sorted `div` 2) sorted))
  where
    sorted = NonEmpty.sort samples

samplePoint ::
  SamplePolicy -> Probe input value -> input ->
  IO (Either BenchFailure (Sampled value))
samplePoint policy probe input = do
  runOrdinal probe input 1 >>= \case
    Left sampleError -> pure (Left sampleError)
    Right firstSample ->
      fmap
        (\restResults -> sequence restResults >>= agreedSamples probe firstSample)
        ( traverse
          (runOrdinal probe input)
          [2 .. sampleCountFor policy (timedSampleElapsedNanoseconds firstSample)]
        )

sampleWithinBudget ::
  SamplePolicy -> Word64 -> Word64 -> Probe input value -> input ->
  IO (Either BenchFailure (Maybe (Sampled value)))
sampleWithinBudget policy budgetStart budgetNs probe input =
  runWithin 1 >>= \case
    Left sampleError -> pure (Left sampleError)
    Right Nothing -> pure (Right Nothing)
    Right (Just firstSample) ->
      fmap
        ( \restResults ->
            sequence restResults
              >>= traverse (agreedSamples probe firstSample) . sequence
        )
        ( traverse
          runWithin
          [2 .. sampleCountFor policy (timedSampleElapsedNanoseconds firstSample)]
        )
  where
    runWithin sampleOrdinal = do
      now <- getMonotonicTimeNSec
      let remaining = budgetNs - min budgetNs (now - budgetStart)
      if remaining == 0
        then pure (Right Nothing)
        else
          maybe (Right Nothing) (fmap Just)
            <$> timeout
                (fromIntegral (remaining `div` 1000))
                (runOrdinal probe input sampleOrdinal)

runOrdinal ::
  Probe input value -> input -> Int ->
  IO (Either BenchFailure (TimedSample value))
runOrdinal probe input sampleOrdinal =
  fmap
    (first ((probeLabel probe <> ": ") <>))
    (timeFreshSample sampleOrdinal input (probeRun probe) (probeDigest probe))

agreedSamples ::
  Probe input value -> TimedSample value -> [TimedSample value] ->
  Either BenchFailure (Sampled value)
agreedSamples probe firstSample restSamples =
  case NonEmpty.nub digests of
    agreedDigest :| [] ->
      Right
        Sampled
          { sampledFirst = firstSample,
            sampledElapsedNanoseconds = fmap timedSampleElapsedNanoseconds samples,
            sampledDigest = agreedDigest
          }
    _ ->
      Left
        ( probeLabel probe
            <> ": digest disagreement across samples: "
            <> show (NonEmpty.toList digests)
        )
  where
    samples = firstSample :| restSamples
    digests = fmap timedSampleDigest samples

sampleCountFor :: SamplePolicy -> Word64 -> Int
sampleCountFor policy firstElapsedNs
  | firstElapsedNs > spSingleSampleThresholdNs policy = 1
  | otherwise = spBaseSamples policy

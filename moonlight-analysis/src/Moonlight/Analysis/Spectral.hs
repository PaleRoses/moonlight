module Moonlight.Analysis.Spectral
  ( SpectralGapSample (..),
    SpectralModeSample (..),
    ScalarShadowSeries (..),
    ScalarShadowModeSeries (..),
    ThresholdGapSpread (..),
    ThresholdModeTransport (..),
    DecategorificationSensitivity (..),
    gapFromModes,
    analyzeScalarShadowSensitivity,
    leadingModeTransport,
    weightedGraphSpectralGap,
    weightedGraphSpectralModes,
  )
where

import Data.Function ((&))
import Data.Kind (Type)
import Data.List (find)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Core
  ( averageOf,
    maximumOf,
    minimumOf,
    pairwise,
    spectralGap,
    spreadOf,
  )
import Moonlight.Homology
  ( FiltrationValue,
    GraphSpectralMode (..),
  )
import Moonlight.Homology.Sequence
  ( leadingModeTransport,
    weightedGraphSpectralGap,
    weightedGraphSpectralModes,
  )

type SpectralGapSample :: Type
data SpectralGapSample = SpectralGapSample
  { sgsThreshold :: FiltrationValue,
    sgsGap :: Maybe Double
  }
  deriving stock (Eq, Show)

type SpectralModeSample :: Type
data SpectralModeSample = SpectralModeSample
  { smsThreshold :: FiltrationValue,
    smsModes :: [GraphSpectralMode]
  }
  deriving stock (Eq, Show)

type ScalarShadowSeries :: Type -> Type
data ScalarShadowSeries shadow = ScalarShadowSeries
  { sssShadow :: shadow,
    sssSamples :: [SpectralGapSample]
  }
  deriving stock (Eq, Show)

type ScalarShadowModeSeries :: Type -> Type
data ScalarShadowModeSeries shadow = ScalarShadowModeSeries
  { ssmsShadow :: shadow,
    ssmsSamples :: [SpectralModeSample]
  }
  deriving stock (Eq, Show)

type ThresholdGapSpread :: Type -> Type
data ThresholdGapSpread shadow = ThresholdGapSpread
  { tgsThreshold :: FiltrationValue,
    tgsGapByShadow :: Map.Map shadow Double,
    tgsSpread :: Double
  }
  deriving stock (Eq, Show)

type ThresholdModeTransport :: Type -> Type
data ThresholdModeTransport shadow = ThresholdModeTransport
  { tmtThreshold :: FiltrationValue,
    tmtTransportByShadowPair :: Map.Map (shadow, shadow) Double,
    tmtMinimumTransport :: Maybe Double,
    tmtAverageTransport :: Maybe Double
  }
  deriving stock (Eq, Show)

type DecategorificationSensitivity :: Type -> Type
data DecategorificationSensitivity shadow = DecategorificationSensitivity
  { dcsSeries :: [ScalarShadowSeries shadow],
    dcsModeSeries :: [ScalarShadowModeSeries shadow],
    dcsThresholdSpreads :: [ThresholdGapSpread shadow],
    dcsThresholdModeTransports :: [ThresholdModeTransport shadow],
    dcsMaxGapSpread :: Maybe Double,
    dcsAverageGapSpread :: Maybe Double,
    dcsMinimumModeTransport :: Maybe Double,
    dcsAverageModeTransport :: Maybe Double
  }
  deriving stock (Eq, Show)

gapFromModes :: [GraphSpectralMode] -> Maybe Double
gapFromModes = spectralGap . fmap spectralEigenvalue

analyzeScalarShadowSensitivity ::
  Ord shadow =>
  [ScalarShadowSeries shadow] ->
  [ScalarShadowModeSeries shadow] ->
  DecategorificationSensitivity shadow
analyzeScalarShadowSensitivity seriesValues modeSeriesValues =
  let thresholdSpreads = thresholdSpreadValues seriesValues
      thresholdTransports = thresholdModeTransportValues modeSeriesValues
      spreadValues = fmap tgsSpread thresholdSpreads
      transportValues = thresholdTransports >>= Map.elems . tmtTransportByShadowPair
   in DecategorificationSensitivity
        { dcsSeries = seriesValues,
          dcsModeSeries = modeSeriesValues,
          dcsThresholdSpreads = thresholdSpreads,
          dcsThresholdModeTransports = thresholdTransports,
          dcsMaxGapSpread = maximumOf spreadValues,
          dcsAverageGapSpread = averageOf spreadValues,
          dcsMinimumModeTransport = minimumOf transportValues,
          dcsAverageModeTransport = averageOf transportValues
        }

thresholdSpreadValues :: Ord shadow => [ScalarShadowSeries shadow] -> [ThresholdGapSpread shadow]
thresholdSpreadValues seriesValues =
  allThresholds seriesValues
    & fmap (thresholdSpreadAt seriesValues)

thresholdModeTransportValues ::
  Ord shadow =>
  [ScalarShadowModeSeries shadow] ->
  [ThresholdModeTransport shadow]
thresholdModeTransportValues modeSeriesValues =
  allModeThresholds modeSeriesValues
    & fmap (thresholdModeTransportAt modeSeriesValues)

allThresholds :: [ScalarShadowSeries shadow] -> [FiltrationValue]
allThresholds seriesValues =
  seriesValues
    & foldMap (Set.fromList . fmap sgsThreshold . sssSamples)
    & Set.toAscList

allModeThresholds :: [ScalarShadowModeSeries shadow] -> [FiltrationValue]
allModeThresholds seriesValues =
  seriesValues
    & foldMap (Set.fromList . fmap smsThreshold . ssmsSamples)
    & Set.toAscList

thresholdSpreadAt ::
  Ord shadow =>
  [ScalarShadowSeries shadow] ->
  FiltrationValue ->
  ThresholdGapSpread shadow
thresholdSpreadAt seriesValues thresholdValue =
  let gapValues =
        seriesValues
          & foldr
            (\seriesValue ->
               case gapAt thresholdValue seriesValue of
                 Just entryValue -> Map.insert (fst entryValue) (snd entryValue)
                 Nothing -> id
            )
            Map.empty
   in ThresholdGapSpread
        { tgsThreshold = thresholdValue,
          tgsGapByShadow = gapValues,
          tgsSpread = maybe 0.0 id (spreadOf (Map.elems gapValues))
        }

thresholdModeTransportAt ::
  Ord shadow =>
  [ScalarShadowModeSeries shadow] ->
  FiltrationValue ->
  ThresholdModeTransport shadow
thresholdModeTransportAt seriesValues thresholdValue =
  let modeValues =
        seriesValues
          & foldr
            (\seriesValue ->
               case modeAt thresholdValue seriesValue of
                 Just entryValue -> (entryValue :)
                 Nothing -> id
            )
            []
      transportValues =
        pairwise modeValues
          & foldr
            (\((leftShadow, leftMode), (rightShadow, rightMode)) ->
               case leadingModeTransport [leftMode] [rightMode] of
                 Just transportValue -> Map.insert (leftShadow, rightShadow) transportValue
                 Nothing -> id
            )
            Map.empty
      transportScalars = Map.elems transportValues
   in ThresholdModeTransport
        { tmtThreshold = thresholdValue,
          tmtTransportByShadowPair = transportValues,
          tmtMinimumTransport = minimumOf transportScalars,
          tmtAverageTransport = averageOf transportScalars
        }

gapAt :: FiltrationValue -> ScalarShadowSeries shadow -> Maybe (shadow, Double)
gapAt thresholdValue seriesValue =
  sampleGapAt thresholdValue (sssSamples seriesValue)
    >>= fmap ((,) (sssShadow seriesValue)) . sgsGap

modeAt :: FiltrationValue -> ScalarShadowModeSeries shadow -> Maybe (shadow, GraphSpectralMode)
modeAt thresholdValue seriesValue =
  sampleModeAt thresholdValue (ssmsSamples seriesValue)
    >>= fmap ((,) (ssmsShadow seriesValue)) . preferredMode . smsModes

sampleGapAt :: FiltrationValue -> [SpectralGapSample] -> Maybe SpectralGapSample
sampleGapAt thresholdValue =
  find ((== thresholdValue) . sgsThreshold)

sampleModeAt :: FiltrationValue -> [SpectralModeSample] -> Maybe SpectralModeSample
sampleModeAt thresholdValue =
  find ((== thresholdValue) . smsThreshold)

preferredMode :: [GraphSpectralMode] -> Maybe GraphSpectralMode
preferredMode spectralModes =
  case filter ((> 1.0e-10) . spectralEigenvalue) spectralModes of
    preferredValue : _ -> Just preferredValue
    [] ->
      case spectralModes of
        firstMode : _ -> Just firstMode
        [] -> Nothing

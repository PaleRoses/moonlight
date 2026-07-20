{-# LANGUAGE BangPatterns #-}

module Moonlight.Control.Feedback
  ( AdaptiveScale,
    AdaptiveScaleError (..),
    EwmaAlpha,
    EwmaAlphaError (..),
    Feedback (..),
    FeedbackStep (..),
    TriggerLatch (..),
    Hysteresis (..),
    HystereticGainState,
    AdaptiveTrigger (..),
    ObservationFilter (..),
    FeedbackMode (..),
    FeedbackController (..),
    FeedbackControllerState,
    FeedbackTrace (..),
    mkAdaptiveScale,
    scaleUnclamped,
    scaleAtLeast,
    scaleAtMost,
    scaleWithin,
    scaleAtLeastLiteral,
    scaleAtMostLiteral,
    adaptiveScaleMultiplier,
    adaptiveScaleMinimum,
    adaptiveScaleMaximum,
    mkEwmaAlpha,
    ewmaAlphaFortyPercent,
    ewmaAlphaValue,
    stepEwma,
    initialHystereticGainState,
    hystereticFeedback,
    hystereticGain,
    hystereticLatch,
    initialFeedbackControllerState,
    feedbackControllerObservation,
    feedbackControllerTriggerStates,
    applyFeedback,
    applyFeedbackWithTrace,
    scaleParameter,
    scaleParameterAtGain,
    scaleField,
    lessThan,
    greaterThan,
    outsideRange,
    withinRange,
    belowBand,
    aboveBand,
  )
where

import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Data.Monoid (Endo (..))

data AdaptiveScale = AdaptiveScale
  { adaptiveScaleMultiplierValue :: !Rational,
    adaptiveScaleClamp :: !AdaptiveClamp
  }
  deriving stock (Eq, Ord, Show)

data AdaptiveClamp
  = AdaptiveUnclamped
  | AdaptiveLowerBounded !Rational
  | AdaptiveUpperBounded !Rational
  | AdaptiveBounded !Rational !Rational
  deriving stock (Eq, Ord, Show)

data AdaptiveScaleError
  = AdaptiveScaleMultiplierNotFinite !Double
  | AdaptiveScaleMinimumNotFinite !Double
  | AdaptiveScaleMaximumNotFinite !Double
  | AdaptiveScaleBoundsInverted !Double !Double
  deriving stock (Eq, Ord, Show, Read)

newtype EwmaAlpha = EwmaAlpha
  { ewmaAlphaRational :: Rational
  }
  deriving stock (Eq, Ord, Show)

data EwmaAlphaError
  = EwmaAlphaNotFinite !Double
  | EwmaAlphaNotPositive !Double
  | EwmaAlphaGreaterThanOne !Double
  deriving stock (Eq, Ord, Show, Read)

data FeedbackStep state parameter = FeedbackStep
  { fstepState :: !state,
    fstepAction :: !(Maybe (Endo parameter))
  }

newtype Feedback state observation parameter = Feedback
  { runFeedback :: state -> observation -> FeedbackStep state parameter
  }

data TriggerLatch
  = TriggerInactive
  | TriggerActive
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

data Hysteresis observation = Hysteresis
  { hEnter :: observation -> Bool,
    hExit :: observation -> Bool
  }

data HystereticGainState = HystereticGainState
  { hgsLatch :: !TriggerLatch,
    hgsGain :: !Double
  }
  deriving stock (Eq, Ord, Show)

data AdaptiveTrigger observation parameter = AdaptiveTrigger
  { atInitialState :: !HystereticGainState,
    atFeedback :: !(Feedback HystereticGainState observation parameter)
  }

data ObservationFilter rawObservation observation = ObservationFilter
  { ofSeed :: rawObservation -> observation,
    ofStep :: rawObservation -> observation -> observation
  }

data FeedbackMode
  = ApplyAll
  | ApplyFirst
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

data FeedbackController rawObservation observation parameter = FeedbackController
  { fcMode :: !FeedbackMode,
    fcObservationFilter :: !(ObservationFilter rawObservation observation),
    fcTriggers :: ![AdaptiveTrigger observation parameter]
  }

data FeedbackControllerState observation = FeedbackControllerState
  { fcsObservation :: !(Maybe observation),
    fcsTriggerStates :: ![HystereticGainState]
  }
  deriving stock (Eq, Ord, Show)

data FeedbackTrace parameter = FeedbackTrace
  { ftActiveTriggerIndexes :: ![Int],
    ftAppliedTriggerIndexes :: ![Int],
    ftInitialParameter :: !parameter,
    ftFinalParameter :: !parameter
  }
  deriving stock (Eq, Ord, Show, Read)

data IndexedFeedbackStep parameter = IndexedFeedbackStep
  { ifsIndex :: !Int,
    ifsStep :: !(FeedbackStep HystereticGainState parameter)
  }

mkAdaptiveScale :: Double -> Maybe Double -> Maybe Double -> Either AdaptiveScaleError AdaptiveScale
mkAdaptiveScale multiplierValue minimumValue maximumValue = do
  finiteMultiplier <- validateFinite AdaptiveScaleMultiplierNotFinite multiplierValue
  finiteMinimum <- traverse (validateFinite AdaptiveScaleMinimumNotFinite) minimumValue
  finiteMaximum <- traverse (validateFinite AdaptiveScaleMaximumNotFinite) maximumValue
  clampValue <- adaptiveClamp finiteMinimum finiteMaximum
  pure
    AdaptiveScale
      { adaptiveScaleMultiplierValue = toRational finiteMultiplier,
        adaptiveScaleClamp = clampValue
      }

scaleUnclamped :: Double -> Either AdaptiveScaleError AdaptiveScale
scaleUnclamped multiplierValue =
  mkAdaptiveScale multiplierValue Nothing Nothing

scaleAtLeast :: Double -> Double -> Either AdaptiveScaleError AdaptiveScale
scaleAtLeast multiplierValue minimumValue =
  mkAdaptiveScale multiplierValue (Just minimumValue) Nothing

scaleAtMost :: Double -> Double -> Either AdaptiveScaleError AdaptiveScale
scaleAtMost multiplierValue maximumValue =
  mkAdaptiveScale multiplierValue Nothing (Just maximumValue)

scaleWithin :: Double -> Double -> Double -> Either AdaptiveScaleError AdaptiveScale
scaleWithin multiplierValue minimumValue maximumValue =
  mkAdaptiveScale multiplierValue (Just minimumValue) (Just maximumValue)

scaleAtLeastLiteral :: Rational -> Rational -> AdaptiveScale
scaleAtLeastLiteral multiplierValue minimumValue =
  AdaptiveScale
    { adaptiveScaleMultiplierValue = multiplierValue,
      adaptiveScaleClamp = AdaptiveLowerBounded minimumValue
    }

scaleAtMostLiteral :: Rational -> Rational -> AdaptiveScale
scaleAtMostLiteral multiplierValue maximumValue =
  AdaptiveScale
    { adaptiveScaleMultiplierValue = multiplierValue,
      adaptiveScaleClamp = AdaptiveUpperBounded maximumValue
    }

adaptiveScaleMultiplier :: AdaptiveScale -> Double
adaptiveScaleMultiplier =
  fromRational . adaptiveScaleMultiplierValue

adaptiveScaleMinimum :: AdaptiveScale -> Maybe Double
adaptiveScaleMinimum AdaptiveScale {adaptiveScaleClamp} =
  case adaptiveScaleClamp of
    AdaptiveUnclamped ->
      Nothing
    AdaptiveLowerBounded minimumValue ->
      Just (fromRational minimumValue)
    AdaptiveUpperBounded _maximumValue ->
      Nothing
    AdaptiveBounded minimumValue _maximumValue ->
      Just (fromRational minimumValue)

adaptiveScaleMaximum :: AdaptiveScale -> Maybe Double
adaptiveScaleMaximum AdaptiveScale {adaptiveScaleClamp} =
  case adaptiveScaleClamp of
    AdaptiveUnclamped ->
      Nothing
    AdaptiveLowerBounded _minimumValue ->
      Nothing
    AdaptiveUpperBounded maximumValue ->
      Just (fromRational maximumValue)
    AdaptiveBounded _minimumValue maximumValue ->
      Just (fromRational maximumValue)

mkEwmaAlpha :: Double -> Either EwmaAlphaError EwmaAlpha
mkEwmaAlpha alpha
  | isNaN alpha || isInfinite alpha =
      Left (EwmaAlphaNotFinite alpha)
  | alpha <= 0.0 =
      Left (EwmaAlphaNotPositive alpha)
  | alpha > 1.0 =
      Left (EwmaAlphaGreaterThanOne alpha)
  | otherwise =
      Right (EwmaAlpha (toRational alpha))

ewmaAlphaFortyPercent :: EwmaAlpha
ewmaAlphaFortyPercent = EwmaAlpha (2 / 5)

ewmaAlphaValue :: EwmaAlpha -> Double
ewmaAlphaValue = fromRational . ewmaAlphaRational

stepEwma :: EwmaAlpha -> Double -> Double -> Double
stepEwma alpha !observation !previous =
  let !weight = ewmaAlphaValue alpha
   in weight * observation + (1.0 - weight) * previous

initialHystereticGainState :: HystereticGainState
initialHystereticGainState =
  HystereticGainState
    { hgsLatch = TriggerInactive,
      hgsGain = initialGain
    }

hystereticFeedback ::
  Hysteresis observation ->
  (HystereticGainState -> Endo parameter) ->
  Feedback HystereticGainState observation parameter
hystereticFeedback hysteresis actionAtGain =
  Feedback $ \state observation ->
    let !nextState = advanceHystereticGain hysteresis observation state
        !action =
          case hgsLatch nextState of
            TriggerInactive -> Nothing
            TriggerActive -> Just (actionAtGain nextState)
     in FeedbackStep
          { fstepState = nextState,
            fstepAction = action
          }

hystereticGain :: HystereticGainState -> Double
hystereticGain = hgsGain

hystereticLatch :: HystereticGainState -> TriggerLatch
hystereticLatch = hgsLatch

initialFeedbackControllerState ::
  FeedbackController rawObservation observation parameter ->
  FeedbackControllerState observation
initialFeedbackControllerState controller =
  FeedbackControllerState
    { fcsObservation = Nothing,
      fcsTriggerStates = fmap atInitialState (fcTriggers controller)
    }

feedbackControllerObservation :: FeedbackControllerState observation -> Maybe observation
feedbackControllerObservation = fcsObservation

feedbackControllerTriggerStates :: FeedbackControllerState observation -> [HystereticGainState]
feedbackControllerTriggerStates = fcsTriggerStates

applyFeedback ::
  FeedbackController rawObservation observation parameter ->
  FeedbackControllerState observation ->
  rawObservation ->
  parameter ->
  (FeedbackControllerState observation, parameter)
applyFeedback controller controllerState observation parameter =
  let (!nextControllerState, !trace) =
        applyFeedbackWithTrace controller controllerState observation parameter
   in (nextControllerState, ftFinalParameter trace)

applyFeedbackWithTrace ::
  FeedbackController rawObservation observation parameter ->
  FeedbackControllerState observation ->
  rawObservation ->
  parameter ->
  (FeedbackControllerState observation, FeedbackTrace parameter)
applyFeedbackWithTrace controller controllerState rawObservation parameter =
  let !observation =
        case fcsObservation controllerState of
          Nothing ->
            ofSeed (fcObservationFilter controller) rawObservation
          Just previousObservation ->
            ofStep (fcObservationFilter controller) rawObservation previousObservation
      !steps =
        zipWith3
          (\triggerIndex trigger triggerState ->
             IndexedFeedbackStep
               { ifsIndex = triggerIndex,
                 ifsStep = runFeedback (atFeedback trigger) triggerState observation
               }
          )
          [0 ..]
          (fcTriggers controller)
          (fcsTriggerStates controllerState)
      !activeActions = mapMaybe activeAction steps
      !appliedActions =
        case fcMode controller of
          ApplyAll -> activeActions
          ApplyFirst -> take 1 activeActions
      !finalParameter =
        foldl'
          (\currentParameter (_, action) -> appEndo action currentParameter)
          parameter
          appliedActions
      !nextControllerState =
        FeedbackControllerState
          { fcsObservation = Just observation,
            fcsTriggerStates = fmap (fstepState . ifsStep) steps
          }
      !trace =
        FeedbackTrace
          { ftActiveTriggerIndexes = fmap fst activeActions,
            ftAppliedTriggerIndexes = fmap fst appliedActions,
            ftInitialParameter = parameter,
            ftFinalParameter = finalParameter
          }
   in (nextControllerState, trace)

scaleParameter :: AdaptiveScale -> Double -> Double
scaleParameter AdaptiveScale {adaptiveScaleMultiplierValue, adaptiveScaleClamp} !currentValue =
  let !scaledValue = fromRational adaptiveScaleMultiplierValue * currentValue
   in applyAdaptiveClamp adaptiveScaleClamp scaledValue

scaleParameterAtGain :: HystereticGainState -> AdaptiveScale -> Double -> Double
scaleParameterAtGain gainState AdaptiveScale {adaptiveScaleMultiplierValue, adaptiveScaleClamp} !currentValue =
  let !multiplierValue = fromRational adaptiveScaleMultiplierValue
      !effectiveMultiplier = 1.0 + hgsGain gainState * (multiplierValue - 1.0)
      !scaledValue = effectiveMultiplier * currentValue
   in applyAdaptiveClamp adaptiveScaleClamp scaledValue

scaleField ::
  AdaptiveScale ->
  (parameter -> Double) ->
  (Double -> parameter -> parameter) ->
  parameter ->
  parameter
scaleField scaleValue getter setter parameter =
  setter (scaleParameter scaleValue (getter parameter)) parameter

lessThan :: (Ord value) => (observation -> value) -> value -> observation -> Bool
lessThan projection threshold observation =
  projection observation < threshold

greaterThan :: (Ord value) => (observation -> value) -> value -> observation -> Bool
greaterThan projection threshold observation =
  projection observation > threshold

outsideRange :: (Ord value) => (observation -> value) -> value -> value -> observation -> Bool
outsideRange projection lowerBound upperBound observation =
  let value = projection observation
   in value < lowerBound || value > upperBound

withinRange :: (Ord value) => (observation -> value) -> value -> value -> observation -> Bool
withinRange projection lowerBound upperBound observation =
  let value = projection observation
   in value >= lowerBound && value <= upperBound

belowBand ::
  (Ord value) =>
  (observation -> value) ->
  value ->
  value ->
  Hysteresis observation
belowBand projection enterThreshold exitThreshold =
  Hysteresis
    { hEnter = lessThan projection enterThreshold,
      hExit = \observation -> projection observation >= exitThreshold
    }

aboveBand ::
  (Ord value) =>
  (observation -> value) ->
  value ->
  value ->
  Hysteresis observation
aboveBand projection enterThreshold exitThreshold =
  Hysteresis
    { hEnter = greaterThan projection enterThreshold,
      hExit = \observation -> projection observation <= exitThreshold
    }

validateFinite :: (Double -> AdaptiveScaleError) -> Double -> Either AdaptiveScaleError Double
validateFinite buildError value
  | isNaN value || isInfinite value =
      Left (buildError value)
  | otherwise =
      Right value

adaptiveClamp :: Maybe Double -> Maybe Double -> Either AdaptiveScaleError AdaptiveClamp
adaptiveClamp minimumValue maximumValue =
  case (minimumValue, maximumValue) of
    (Nothing, Nothing) ->
      Right AdaptiveUnclamped
    (Just lowerBound, Nothing) ->
      Right (AdaptiveLowerBounded (toRational lowerBound))
    (Nothing, Just upperBound) ->
      Right (AdaptiveUpperBounded (toRational upperBound))
    (Just lowerBound, Just upperBound)
      | lowerBound <= upperBound ->
          Right (AdaptiveBounded (toRational lowerBound) (toRational upperBound))
      | otherwise ->
          Left (AdaptiveScaleBoundsInverted lowerBound upperBound)

applyAdaptiveClamp :: AdaptiveClamp -> Double -> Double
applyAdaptiveClamp clampValue scaledValue =
  case clampValue of
    AdaptiveUnclamped ->
      scaledValue
    AdaptiveLowerBounded minimumValue ->
      max (fromRational minimumValue) scaledValue
    AdaptiveUpperBounded maximumValue ->
      min (fromRational maximumValue) scaledValue
    AdaptiveBounded minimumValue maximumValue ->
      min (fromRational maximumValue) (max (fromRational minimumValue) scaledValue)

advanceHystereticGain ::
  Hysteresis observation ->
  observation ->
  HystereticGainState ->
  HystereticGainState
advanceHystereticGain hysteresis observation state =
  case hgsLatch state of
    TriggerInactive
      | hEnter hysteresis observation ->
          HystereticGainState
            { hgsLatch = TriggerActive,
              hgsGain = min maximumGain (max initialGain (hgsGain state + additiveIncrease))
            }
      | otherwise ->
          HystereticGainState
            { hgsLatch = TriggerInactive,
              hgsGain = max minimumGain (hgsGain state * multiplicativeDecrease)
            }
    TriggerActive
      | hExit hysteresis observation ->
          HystereticGainState
            { hgsLatch = TriggerInactive,
              hgsGain = max minimumGain (hgsGain state * multiplicativeDecrease)
            }
      | otherwise ->
          HystereticGainState
            { hgsLatch = TriggerActive,
              hgsGain = min maximumGain (hgsGain state + additiveIncrease)
            }

activeAction :: IndexedFeedbackStep parameter -> Maybe (Int, Endo parameter)
activeAction indexedStep =
  fmap ((,) (ifsIndex indexedStep)) (fstepAction (ifsStep indexedStep))

initialGain :: Double
initialGain = 0.5

additiveIncrease :: Double
additiveIncrease = 0.25

multiplicativeDecrease :: Double
multiplicativeDecrease = 0.5

minimumGain :: Double
minimumGain = 0.0

maximumGain :: Double
maximumGain = 1.0

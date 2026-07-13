module Moonlight.Control.FeedbackSpec
  ( tests,
  )
where

import Data.List (mapAccumL)
import Data.Monoid (Endo (..))
import Moonlight.Control.Feedback
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

data NaNRejection
  = MultiplierNaNRejected
  | MinimumNaNRejected
  | MaximumNaNRejected
  | NaNNotRejected
  deriving stock (Eq, Show)

data AlphaRejection
  = AlphaNonFiniteRejected
  | AlphaNonPositiveRejected
  | AlphaAboveOneRejected
  | AlphaNotRejected
  deriving stock (Eq, Show)

data Observation = Observation
  { obsLoad :: !Double,
    obsNoise :: !Double
  }
  deriving stock (Eq, Show)

tests :: TestTree
tests =
  testGroup
    "feedback"
    [ testCase "scaleParameter multiplies and clamps" $
        fmap (`scaleParameter` 4.0) (mkAdaptiveScale 3.0 (Just 2.0) (Just 10.0))
          @?= Right 10.0,
      testCase "mkAdaptiveScale rejects inverted bounds" $
        mkAdaptiveScale 1.0 (Just 2.0) (Just 1.0)
          @?= Left (AdaptiveScaleBoundsInverted 2.0 1.0),
      testCase "mkAdaptiveScale rejects infinite scale inputs" $
        ( mkAdaptiveScale (1.0 / 0.0) Nothing Nothing,
          mkAdaptiveScale ((-1.0) / 0.0) Nothing Nothing,
          mkAdaptiveScale 1.0 (Just (1.0 / 0.0)) Nothing,
          mkAdaptiveScale 1.0 (Just ((-1.0) / 0.0)) Nothing,
          mkAdaptiveScale 1.0 Nothing (Just (1.0 / 0.0)),
          mkAdaptiveScale 1.0 Nothing (Just ((-1.0) / 0.0))
        )
          @?= ( Left (AdaptiveScaleMultiplierNotFinite (1.0 / 0.0)),
                Left (AdaptiveScaleMultiplierNotFinite ((-1.0) / 0.0)),
                Left (AdaptiveScaleMinimumNotFinite (1.0 / 0.0)),
                Left (AdaptiveScaleMinimumNotFinite ((-1.0) / 0.0)),
                Left (AdaptiveScaleMaximumNotFinite (1.0 / 0.0)),
                Left (AdaptiveScaleMaximumNotFinite ((-1.0) / 0.0))
              ),
      testCase "mkAdaptiveScale rejects NaN scale inputs" $
        ( nanRejectionOf (mkAdaptiveScale (0.0 / 0.0) Nothing Nothing),
          nanRejectionOf (mkAdaptiveScale 1.0 (Just (0.0 / 0.0)) Nothing),
          nanRejectionOf (mkAdaptiveScale 1.0 Nothing (Just (0.0 / 0.0)))
        )
          @?= ( MultiplierNaNRejected,
                MinimumNaNRejected,
                MaximumNaNRejected
              ),
      testCase "one-sided literal constructors preserve readable bounds" $
        ( adaptiveScaleMultiplier (scaleAtLeastLiteral 0.5 2.0),
          adaptiveScaleMinimum (scaleAtLeastLiteral 0.5 2.0),
          adaptiveScaleMaximum (scaleAtMostLiteral 2.0 5.0)
        )
          @?= (0.5, Just 2.0, Just 5.0),
      testCase "EWMA alpha validation rejects every invalid class" $
        ( alphaRejectionOf (mkEwmaAlpha (0.0 / 0.0)),
          alphaRejectionOf (mkEwmaAlpha (1.0 / 0.0)),
          alphaRejectionOf (mkEwmaAlpha 0.0),
          alphaRejectionOf (mkEwmaAlpha (-0.1)),
          alphaRejectionOf (mkEwmaAlpha 1.1),
          fmap ewmaAlphaValue (mkEwmaAlpha 0.4)
        )
          @?= ( AlphaNonFiniteRejected,
                AlphaNonFiniteRejected,
                AlphaNonPositiveRejected,
                AlphaNonPositiveRejected,
                AlphaAboveOneRejected,
                Right 0.4
              ),
      testCase "EWMA seeds from the first observation and only then smooths" $
        let controller =
              FeedbackController
                { fcMode = ApplyFirst,
                  fcObservationFilter =
                    ObservationFilter
                      { ofSeed = id,
                        ofStep = stepEwma ewmaAlphaFortyPercent
                      },
                  fcTriggers = []
                }
                :: FeedbackController Double Double ()
            firstState = fst (applyFeedback controller (initialFeedbackControllerState controller) 10.0 ())
            secondState = fst (applyFeedback controller firstState 0.0 ())
         in ( feedbackControllerObservation firstState,
              feedbackControllerObservation secondState
            )
              @?= (Just 10.0, Just 6.0),
      testCase "Schmitt bands retain the active latch throughout the neutral gap" $
        let controller =
              singleTriggerController
                (belowBand id 3.0 4.0)
                (const (Endo (+ (1 :: Int))))
            snapshots = runFeedbackSequence controller 0 [2.0, 3.5, 3.8, 4.0, 3.5, 2.5]
         in ( triggerLatches snapshots,
              triggerGains snapshots,
              fmap (ftAppliedTriggerIndexes . snd) snapshots
            )
              @?= ( [ [TriggerActive],
                       [TriggerActive],
                       [TriggerActive],
                       [TriggerInactive],
                       [TriggerInactive],
                       [TriggerActive]
                     ],
                     [[0.75], [1.0], [1.0], [0.5], [0.25], [0.5]],
                     [[0], [0], [0], [], [], [0]]
                   ),
      testCase "above-band hysteresis also retains its latch across the Schmitt gap" $
        let controller =
              singleTriggerController
                (aboveBand id 7.0 6.0)
                (const (Endo (+ (1 :: Int))))
            snapshots = runFeedbackSequence controller 0 [8.0, 6.5, 6.2, 6.0, 6.5, 7.5]
         in (triggerLatches snapshots, fmap (ftAppliedTriggerIndexes . snd) snapshots)
              @?= ( [ [TriggerActive],
                       [TriggerActive],
                       [TriggerActive],
                       [TriggerInactive],
                       [TriggerInactive],
                       [TriggerActive]
                     ],
                     [[0], [0], [0], [], [], [0]]
                   ),
      testCase "AIMD gain advances on enter, persistence, exit, inactivity, and re-entry" $
        let controller =
              singleTriggerController
                ( Hysteresis
                    { hEnter = id,
                      hExit = not
                    }
                )
                (const (Endo (+ (1 :: Int))))
            snapshots = runFeedbackSequence controller 0 [True, True, False, False, True]
         in (triggerGains snapshots, fmap (ftFinalParameter . snd) snapshots)
              @?= ([[0.75], [1.0], [0.5], [0.25], [0.5]], [1, 2, 2, 2, 3]),
      testCase "gain interpolation applies before preserving AdaptiveScale clamps" $
        ( scaleParameterAtGain initialHystereticGainState (scaleAtLeastLiteral 0.5 2.0) 1.0,
          scaleParameterAtGain initialHystereticGainState (scaleAtMostLiteral 2.0 5.0) 4.0,
          scaleParameterAtGain initialHystereticGainState (scaleAtMostLiteral 2.0 10.0) 4.0
        )
          @?= (2.0, 5.0, 6.0),
      testCase "ApplyAll composes every active endomorphism in order" $
        let controller = orderedController ApplyAll
            (_nextState, trace) =
              applyFeedbackWithTrace
                controller
                (initialFeedbackControllerState controller)
                Observation {obsLoad = 5.0, obsNoise = 0.7}
                (1 :: Int)
         in trace
              @?= FeedbackTrace
                { ftActiveTriggerIndexes = [0, 1],
                  ftAppliedTriggerIndexes = [0, 1],
                  ftInitialParameter = 1,
                  ftFinalParameter = 9
                },
      testCase "ApplyFirst applies one action while advancing every latch" $
        let controller = orderedController ApplyFirst
            (firstState, firstTrace) =
              applyFeedbackWithTrace
                controller
                (initialFeedbackControllerState controller)
                Observation {obsLoad = 5.0, obsNoise = 0.7}
                (1 :: Int)
            (secondState, secondTrace) =
              applyFeedbackWithTrace
                controller
                firstState
                Observation {obsLoad = 5.0, obsNoise = 0.2}
                (ftFinalParameter firstTrace)
         in ( fmap hystereticLatch (feedbackControllerTriggerStates firstState),
              fmap hystereticGain (feedbackControllerTriggerStates firstState),
              firstTrace,
              fmap hystereticLatch (feedbackControllerTriggerStates secondState),
              fmap hystereticGain (feedbackControllerTriggerStates secondState),
              secondTrace
            )
              @?= ( [TriggerActive, TriggerActive],
                     [0.75, 0.75],
                     FeedbackTrace
                       { ftActiveTriggerIndexes = [0, 1],
                         ftAppliedTriggerIndexes = [0],
                         ftInitialParameter = 1,
                         ftFinalParameter = 3
                       },
                     [TriggerActive, TriggerInactive],
                     [1.0, 0.375],
                     FeedbackTrace
                       { ftActiveTriggerIndexes = [0],
                         ftAppliedTriggerIndexes = [0],
                         ftInitialParameter = 3,
                         ftFinalParameter = 5
                       }
                   ),
      testCase "initial controller state derives exactly one state per trigger" $
        let controller = orderedController ApplyFirst
         in length (feedbackControllerTriggerStates (initialFeedbackControllerState controller))
              @?= length (fcTriggers controller),
      testCase "identical observation streams replay deterministically" $
        let controller = orderedController ApplyFirst
            observations =
              [ Observation {obsLoad = 5.0, obsNoise = 0.7},
                Observation {obsLoad = 4.5, obsNoise = 0.4},
                Observation {obsLoad = 3.0, obsNoise = 0.1}
              ]
            replay = runFeedbackSequence controller (1 :: Int) observations
         in replay @?= runFeedbackSequence controller (1 :: Int) observations,
      testCase "range predicates remain reusable observation sections" $
        ( outsideRange obsNoise 0.2 0.8 Observation {obsLoad = 0.0, obsNoise = 0.9},
          withinRange obsLoad 1.0 4.0 Observation {obsLoad = 3.0, obsNoise = 0.0},
          lessThan obsLoad 1.0 Observation {obsLoad = 3.0, obsNoise = 0.0}
        )
          @?= (True, True, False)
    ]

singleTriggerController ::
  Hysteresis observation ->
  (HystereticGainState -> Endo parameter) ->
  FeedbackController observation observation parameter
singleTriggerController hysteresis actionAtGain =
  FeedbackController
    { fcMode = ApplyFirst,
      fcObservationFilter = latestObservationFilter,
      fcTriggers =
        [ AdaptiveTrigger
            { atInitialState = initialHystereticGainState,
              atFeedback = hystereticFeedback hysteresis actionAtGain
            }
        ]
    }

orderedController :: FeedbackMode -> FeedbackController Observation Observation Int
orderedController mode =
  FeedbackController
    { fcMode = mode,
      fcObservationFilter = latestObservationFilter,
      fcTriggers =
        [ AdaptiveTrigger
            { atInitialState = initialHystereticGainState,
              atFeedback =
                hystereticFeedback
                  (aboveBand obsLoad 4.0 3.0)
                  (const (Endo (+ 2)))
            },
          AdaptiveTrigger
            { atInitialState = initialHystereticGainState,
              atFeedback =
                hystereticFeedback
                  (aboveBand obsNoise 0.5 0.3)
                  (const (Endo (* 3)))
            }
        ]
    }

latestObservationFilter :: ObservationFilter observation observation
latestObservationFilter =
  ObservationFilter
    { ofSeed = id,
      ofStep = \observation _previous -> observation
    }

runFeedbackSequence ::
  FeedbackController rawObservation observation parameter ->
  parameter ->
  [rawObservation] ->
  [(FeedbackControllerState observation, FeedbackTrace parameter)]
runFeedbackSequence controller initialParameter observations =
  snd (mapAccumL stepController (initialFeedbackControllerState controller, initialParameter) observations)
  where
    stepController (controllerState, parameter) observation =
      let (nextControllerState, trace) =
            applyFeedbackWithTrace controller controllerState observation parameter
       in ( (nextControllerState, ftFinalParameter trace),
            (nextControllerState, trace)
          )

triggerGains ::
  [(FeedbackControllerState observation, FeedbackTrace parameter)] ->
  [[Double]]
triggerGains =
  fmap (fmap hystereticGain . feedbackControllerTriggerStates . fst)

triggerLatches ::
  [(FeedbackControllerState observation, FeedbackTrace parameter)] ->
  [[TriggerLatch]]
triggerLatches =
  fmap (fmap hystereticLatch . feedbackControllerTriggerStates . fst)

nanRejectionOf :: Either AdaptiveScaleError AdaptiveScale -> NaNRejection
nanRejectionOf result =
  case result of
    Left (AdaptiveScaleMultiplierNotFinite value)
      | isNaN value ->
          MultiplierNaNRejected
    Left (AdaptiveScaleMinimumNotFinite value)
      | isNaN value ->
          MinimumNaNRejected
    Left (AdaptiveScaleMaximumNotFinite value)
      | isNaN value ->
          MaximumNaNRejected
    _ ->
      NaNNotRejected

alphaRejectionOf :: Either EwmaAlphaError EwmaAlpha -> AlphaRejection
alphaRejectionOf result =
  case result of
    Left (EwmaAlphaNotFinite _) ->
      AlphaNonFiniteRejected
    Left (EwmaAlphaNotPositive _) ->
      AlphaNonPositiveRejected
    Left (EwmaAlphaGreaterThanOne _) ->
      AlphaAboveOneRejected
    Right _ ->
      AlphaNotRejected

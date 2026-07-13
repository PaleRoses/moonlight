module Moonlight.Analysis.CPGSpec
  ( tests,
  )
where

import Data.Map.Strict qualified as Map
import Moonlight.Analysis
  ( CPGError (..),
    CPGInterrupt (..),
    CPGStep (..),
    ChainId (..),
    Coupling (..),
    Oscillator (..),
    OscillatorId (..),
    TerrainGate (..),
    applyInterrupt,
    cpgFixedDt,
    cpgState,
    cpgStatePhases,
    cpgStep,
    integrateCPGRK4,
    mkCPGNetwork,
    mkCPGState,
    oscillatorSignal,
    phaseDerivatives,
    phaseDerivativesWithTerrain,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, (@?=), testCase)

closeTo :: Double -> Double -> Double -> Bool
closeTo tolerance expected actual = abs (expected - actual) <= tolerance

closeList :: Double -> [Double] -> [Double] -> Bool
closeList tolerance expected actual =
  case (expected, actual) of
    ([], []) -> True
    (expectedHead : expectedTail, actualHead : actualTail) ->
      closeTo tolerance expectedHead actualHead
        && closeList tolerance expectedTail actualTail
    _ -> False

finalState :: [CPGStep] -> Maybe [Double]
finalState steps =
  case steps of
    [] -> Nothing
    [stepValue] -> Just (cpgStatePhases (cpgState stepValue))
    _ : remainingSteps -> finalState remainingSteps

simpleOscillator :: Double -> Oscillator
simpleOscillator frequencyValue =
  Oscillator
    { oscillatorChainId = ChainId 0,
      oscillatorNaturalFrequency = frequencyValue,
      oscillatorIntrinsicAmplitude = 1.0,
      oscillatorDutyFactor = 0.5,
      oscillatorPhaseBias = 0.0
    }

tests :: TestTree
tests =
  testGroup
    "cpg"
    [ testCase "single oscillator integrates to linear phase drift" $ do
        case mkCPGNetwork [simpleOscillator 1.0] [] of
          Left err ->
            assertFailure ("unexpected network construction failure: " <> show err)
          Right network ->
            case mkCPGState network [0.0] of
              Left err ->
                assertFailure ("unexpected state construction failure: " <> show err)
              Right initialState ->
                case integrateCPGRK4 0.05 0.0 1.0 network initialState of
                  Left err ->
                    assertFailure ("unexpected integration failure: " <> show err)
                  Right steps ->
                    assertBool
                      "single oscillator should drift at its natural frequency"
                      (case finalState steps of
                         Just [phaseValue] -> closeTo 2.0e-2 1.0 phaseValue
                         _ -> False),
      testCase "phase derivatives depend only on relative phase offsets" $ do
        let couplings =
              [ Coupling (OscillatorId 0) (OscillatorId 1) 0.5 0.0,
                Coupling (OscillatorId 1) (OscillatorId 0) 0.5 0.0
              ]
            oscillators = [simpleOscillator 1.0, (simpleOscillator 2.0) {oscillatorChainId = ChainId 1}]
        case mkCPGNetwork oscillators couplings of
          Left err ->
            assertFailure ("unexpected network construction failure: " <> show err)
          Right network ->
            case (mkCPGState network [0.0, pi / 2.0], mkCPGState network [1.3, 1.3 + pi / 2.0]) of
              (Right referenceState, Right shiftedState) ->
                assertBool
                  "global phase shifts should not change the vector field"
                  (closeList 1.0e-9 (phaseDerivatives network referenceState) (phaseDerivatives network shiftedState))
              (Left err, _) ->
                assertFailure ("unexpected reference state failure: " <> show err)
              (_, Left err) ->
                assertFailure ("unexpected shifted state failure: " <> show err),
      testCase "stance gating can fully lock an oscillator in stance" $ do
        let lockedTerrain = Map.singleton (OscillatorId 0) (TerrainGate True False 0.0 0.5)
            lockedOscillator = (simpleOscillator 1.0) {oscillatorDutyFactor = 1.0}
        case mkCPGNetwork [lockedOscillator] [] of
          Left err ->
            assertFailure ("unexpected network construction failure: " <> show err)
          Right network ->
            case mkCPGState network [0.0] of
              Left err ->
                assertFailure ("unexpected state construction failure: " <> show err)
              Right initialState ->
                phaseDerivativesWithTerrain network lockedTerrain initialState @?= [0.0],
      testCase "phase debt discharges after a locked stance opens" $ do
        let lockedTerrain = Map.singleton (OscillatorId 0) (TerrainGate True False 0.0 0.5)
            openTerrain :: Map.Map OscillatorId TerrainGate
            openTerrain = Map.empty
            lockedOscillator = (simpleOscillator 1.0) {oscillatorDutyFactor = 1.0}
        case mkCPGNetwork [lockedOscillator] [] of
          Left err ->
            assertFailure ("unexpected network construction failure: " <> show err)
          Right network ->
            case mkCPGState network [0.0] of
              Left err ->
                assertFailure ("unexpected state construction failure: " <> show err)
              Right initialState ->
                let lockedState = cpgStep network lockedTerrain initialState
                    releasedState = cpgStep network openTerrain lockedState
                 in assertBool
                      "released oscillator should advance by more than one ungated tick due to debt discharge"
                      (case cpgStatePhases releasedState of
                         [phaseValue] -> phaseValue > (1.5 * cpgFixedDt)
                         _ -> False),
      testCase "interrupt resets phases to biases and zeros the output during recovery" $ do
        let oscillators =
              [ (simpleOscillator 1.0) {oscillatorPhaseBias = pi / 3.0},
                ((simpleOscillator 1.0) {oscillatorChainId = ChainId 1, oscillatorPhaseBias = pi})
              ]
        case mkCPGNetwork oscillators [] of
          Left err ->
            assertFailure ("unexpected network construction failure: " <> show err)
          Right network ->
            case mkCPGState network [0.1, 0.2] of
              Left err ->
                assertFailure ("unexpected state construction failure: " <> show err)
              Right initialState ->
                let resetState = applyInterrupt network Stagger initialState
                 in do
                      assertBool
                        "interrupt should reset phases to oscillator biases"
                        (closeList 1.0e-9 [pi / 3.0, pi] (cpgStatePhases resetState))
                      assertBool
                        "recovery starts with zero output amplitude"
                        (case oscillatorSignal network resetState (OscillatorId 0) of
                           Just signalValue -> closeTo 1.0e-9 0.0 signalValue
                           Nothing -> False),
      testCase "state construction rejects phase count mismatches" $ do
        case mkCPGNetwork [simpleOscillator 1.0, (simpleOscillator 2.0) {oscillatorChainId = ChainId 1}] [] of
          Left err ->
            assertFailure ("unexpected network construction failure: " <> show err)
          Right network ->
            mkCPGState network [0.0] @?= Left (PhaseCountMismatch 2 1),
      testCase "network construction rejects invalid duty factors" $
        mkCPGNetwork [(simpleOscillator 1.0) {oscillatorDutyFactor = 1.5}] []
          @?= Left (InvalidDutyFactor (OscillatorId 0) 1.5)
    ]

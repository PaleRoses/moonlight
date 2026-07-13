module Moonlight.Analysis.Dynamics.CPG.Network
  ( OscillatorId (..),
    ChainId (..),
    Oscillator (..),
    Coupling (..),
    TerrainGate (..),
    CPGInterrupt (..),
    CPGError (..),
    CPGNetwork,
    CPGState,
    CPGStep (..),
    cpgTickHz,
    cpgFixedDt,
    mkCPGNetwork,
    mkCPGState,
    oscillatorIds,
    cpgStatePhases,
    wrappedPhase,
    phaseDerivatives,
    phaseDerivativesWithTerrain,
    oscillatorSignal,
    cpgStep,
    applyInterrupt,
    integrateCPGRK4,
    integrateCPGAdaptive,
  )
where

import Data.Kind (Type)
import Data.Fixed (mod')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Analysis.Integrate.ODE
  ( ODEStep (..),
    ODESystem,
    StepSizeControl,
    integrateAdaptive,
    integrateRK4,
    rk4Step,
  )

type OscillatorId :: Type
newtype OscillatorId = OscillatorId
  { unOscillatorId :: Int
  }
  deriving stock (Eq, Ord, Show, Read)

type ChainId :: Type
newtype ChainId = ChainId
  { unChainId :: Int
  }
  deriving stock (Eq, Ord, Show, Read)

type Oscillator :: Type
data Oscillator = Oscillator
  { oscillatorChainId :: ChainId,
    oscillatorNaturalFrequency :: Double,
    oscillatorIntrinsicAmplitude :: Double,
    oscillatorDutyFactor :: Double,
    oscillatorPhaseBias :: Double
  }
  deriving stock (Eq, Show, Read)

type Coupling :: Type
data Coupling = Coupling
  { couplingSource :: OscillatorId,
    couplingTarget :: OscillatorId,
    couplingStrength :: Double,
    couplingPhaseOffset :: Double
  }
  deriving stock (Eq, Show, Read)

type TerrainGate :: Type
data TerrainGate = TerrainGate
  { terrainFootLocked :: Bool,
    terrainSearchingForFoothold :: Bool,
    terrainStanceGate :: Double,
    terrainSearchGate :: Double
  }
  deriving stock (Eq, Show, Read)

type CPGInterrupt :: Type
data CPGInterrupt
  = Stagger
  | Ragdoll
  | ExternalImpulse
  deriving stock (Eq, Show, Read)

type CPGError :: Type
data CPGError
  = UnknownOscillatorId OscillatorId
  | PhaseCountMismatch Int Int
  | InvalidDutyFactor OscillatorId Double
  | NegativeAmplitude OscillatorId Double
  deriving stock (Eq, Show, Read)

type CPGNetwork :: Type
data CPGNetwork = CPGNetwork
  { networkOscillators :: Map OscillatorId Oscillator,
    networkCouplings :: [Coupling],
    networkStanceGateDefault :: Double,
    networkSearchGateDefault :: Double,
    networkMaxPhaseDebt :: Double,
    networkRecoveryFrames :: Int
  }
  deriving stock (Eq, Show)

type OscillatorState :: Type
data OscillatorState = OscillatorState
  { oscillatorPhaseUnwrapped :: Double,
    oscillatorPhaseDebt :: Double,
    oscillatorRecoveryRemaining :: Int
  }
  deriving stock (Eq, Show)

type CPGState :: Type
newtype CPGState = CPGState
  { stateOscillators :: Map OscillatorId OscillatorState
  }
  deriving stock (Eq, Show)

type CPGStep :: Type
data CPGStep = CPGStep
  { cpgTime :: Double,
    cpgState :: CPGState
  }
  deriving stock (Eq, Show)

cpgTickHz :: Double
cpgTickHz = 60.0

cpgFixedDt :: Double
cpgFixedDt = 1.0 / cpgTickHz

mkCPGNetwork :: [Oscillator] -> [Coupling] -> Either CPGError CPGNetwork
mkCPGNetwork oscillators couplings = do
  let oscillatorMap =
        Map.fromList
          (zipWith (\indexValue oscillator -> (OscillatorId indexValue, oscillator)) [0 ..] oscillators)
      knownOscillatorIds = Set.fromList (Map.keys oscillatorMap)
  maybe
    (validateOscillators oscillatorMap)
    Left
    (findUnknownOscillatorId knownOscillatorIds couplings)
  where
    validateOscillators oscillatorMap =
      maybe
        ( Right
            CPGNetwork
              { networkOscillators = oscillatorMap,
                networkCouplings = couplings,
                networkStanceGateDefault = 0.0,
                networkSearchGateDefault = 0.5,
                networkMaxPhaseDebt = pi / 2.0,
                networkRecoveryFrames = 30
              }
        )
        Left
        (findInvalidOscillator oscillatorMap)

findUnknownOscillatorId :: Set.Set OscillatorId -> [Coupling] -> Maybe CPGError
findUnknownOscillatorId knownOscillatorIds couplings =
  case couplings of
    [] -> Nothing
    coupling : remainingCouplings
      | Set.notMember (couplingSource coupling) knownOscillatorIds ->
          Just (UnknownOscillatorId (couplingSource coupling))
      | Set.notMember (couplingTarget coupling) knownOscillatorIds ->
          Just (UnknownOscillatorId (couplingTarget coupling))
      | otherwise ->
          findUnknownOscillatorId knownOscillatorIds remainingCouplings

findInvalidOscillator :: Map OscillatorId Oscillator -> Maybe CPGError
findInvalidOscillator oscillatorMap =
  foldr
    (\(oscillatorId, oscillator) acc ->
        case acc of
          Just err -> Just err
          Nothing
            | oscillatorDutyFactor oscillator <= 0.0 || oscillatorDutyFactor oscillator > 1.0 ->
                Just (InvalidDutyFactor oscillatorId (oscillatorDutyFactor oscillator))
            | oscillatorIntrinsicAmplitude oscillator < 0.0 ->
                Just (NegativeAmplitude oscillatorId (oscillatorIntrinsicAmplitude oscillator))
            | otherwise ->
                Nothing
    )
    Nothing
    (Map.toAscList oscillatorMap)

mkCPGState :: CPGNetwork -> [Double] -> Either CPGError CPGState
mkCPGState network phases
  | length phases == Map.size (networkOscillators network) =
      Right
        ( CPGState
            ( Map.fromAscList
                ( zipWith
                    (\oscillatorId phaseValue -> (oscillatorId, OscillatorState phaseValue 0.0 0))
                    (oscillatorIds network)
                    phases
                )
            )
        )
  | otherwise =
      Left (PhaseCountMismatch (Map.size (networkOscillators network)) (length phases))

oscillatorIds :: CPGNetwork -> [OscillatorId]
oscillatorIds = Map.keys . networkOscillators

cpgStatePhases :: CPGState -> [Double]
cpgStatePhases = fmap (oscillatorPhaseUnwrapped . snd) . Map.toAscList . stateOscillators

wrappedPhase :: Double -> Double
wrappedPhase phaseValue = phaseValue `mod'` (2.0 * pi)

phaseDerivatives :: CPGNetwork -> CPGState -> [Double]
phaseDerivatives network = phaseDerivativesWithTerrain network Map.empty

phaseDerivativesWithTerrain :: CPGNetwork -> Map OscillatorId TerrainGate -> CPGState -> [Double]
phaseDerivativesWithTerrain network terrainContext stateValue =
  fmap
    (\(oscillatorId, oscillator) ->
        let rawDerivative = rawDerivativeAt network stateValue oscillatorId oscillator
            gateValue = phaseGate network terrainContext oscillatorId oscillator stateValue
         in gateValue * rawDerivative
    )
    (Map.toAscList (networkOscillators network))

oscillatorSignal :: CPGNetwork -> CPGState -> OscillatorId -> Maybe Double
oscillatorSignal network stateValue oscillatorId =
  (\oscillator stateSlot ->
        recoveryScale network stateSlot
          * oscillatorIntrinsicAmplitude oscillator
          * sin (wrappedPhase (oscillatorPhaseUnwrapped stateSlot))
    )
    <$> Map.lookup oscillatorId (networkOscillators network)
    <*> Map.lookup oscillatorId (stateOscillators stateValue)

cpgStep :: CPGNetwork -> Map OscillatorId TerrainGate -> CPGState -> CPGState
cpgStep network terrainContext stateValue =
  let gateMap = gateMapAt network terrainContext stateValue
      rawDerivatives = rawDerivativeMap network stateValue
      nextPhases =
        rk4Step
          cpgFixedDt
          (phaseSystemWithGates network gateMap stateValue)
          0.0
          (cpgStatePhases stateValue)
      nextPhaseMap = Map.fromAscList (zip (oscillatorIds network) nextPhases)
   in CPGState
        ( Map.intersectionWithKey
            (advanceOscillator network gateMap rawDerivatives)
            nextPhaseMap
            (stateOscillators stateValue)
        )

applyInterrupt :: CPGNetwork -> CPGInterrupt -> CPGState -> CPGState
applyInterrupt network _interrupt stateValue =
  CPGState
    ( Map.intersectionWith
        (resetOscillatorState network)
        (networkOscillators network)
        (stateOscillators stateValue)
    )

integrateCPGRK4 ::
  Double ->
  Double ->
  Double ->
  CPGNetwork ->
  CPGState ->
  Either CPGError [CPGStep]
integrateCPGRK4 stepSize startTime endTime network initialState =
  traverse
    (decodeStep network)
    (integrateRK4 stepSize startTime endTime (cpgSystem network) (cpgStatePhases initialState))

integrateCPGAdaptive ::
  StepSizeControl ->
  Double ->
  Double ->
  CPGNetwork ->
  CPGState ->
  Either CPGError [CPGStep]
integrateCPGAdaptive control startTime endTime network initialState =
  traverse
    (decodeStep network)
    (integrateAdaptive control startTime endTime (cpgSystem network) (cpgStatePhases initialState))

cpgSystem :: CPGNetwork -> ODESystem
cpgSystem network _timeValue phaseVector =
  case mkCPGState network phaseVector of
    Right stateValue -> phaseDerivatives network stateValue
    Left _ -> []

phaseSystemWithGates :: CPGNetwork -> Map OscillatorId Double -> CPGState -> ODESystem
phaseSystemWithGates network gateMap stateValue _timeValue phaseVector =
  let phaseState = stateValue {stateOscillators = attachPhases phaseVector (stateOscillators stateValue)}
   in fmap
        (\(oscillatorId, oscillator) ->
            Map.findWithDefault 1.0 oscillatorId gateMap
              * rawDerivativeAt network phaseState oscillatorId oscillator
        )
        (Map.toAscList (networkOscillators network))

attachPhases :: [Double] -> Map OscillatorId OscillatorState -> Map OscillatorId OscillatorState
attachPhases phaseVector stateSlots =
  Map.fromAscList
    ( zipWith
        (\(oscillatorId, stateSlot) phaseValue -> (oscillatorId, stateSlot {oscillatorPhaseUnwrapped = phaseValue}))
        (Map.toAscList stateSlots)
        phaseVector
    )

rawDerivativeMap :: CPGNetwork -> CPGState -> Map OscillatorId Double
rawDerivativeMap network stateValue =
  Map.fromAscList
    ( fmap
        (\(oscillatorId, oscillator) -> (oscillatorId, rawDerivativeAt network stateValue oscillatorId oscillator))
        (Map.toAscList (networkOscillators network))
    )

rawDerivativeAt :: CPGNetwork -> CPGState -> OscillatorId -> Oscillator -> Double
rawDerivativeAt network stateValue oscillatorId oscillator =
  oscillatorNaturalFrequency oscillator
    + recoveryScale network (lookupStateSlot oscillatorId stateValue)
      * foldr (incomingContribution stateValue oscillatorId) 0.0 (networkCouplings network)

incomingContribution :: CPGState -> OscillatorId -> Coupling -> Double -> Double
incomingContribution stateValue targetId coupling accumulated
  | couplingTarget coupling /= targetId =
      accumulated
  | otherwise =
      let sourcePhase = oscillatorPhaseUnwrapped (lookupStateSlot (couplingSource coupling) stateValue)
          targetPhase = oscillatorPhaseUnwrapped (lookupStateSlot targetId stateValue)
       in accumulated
            + couplingStrength coupling
              * sin (sourcePhase - targetPhase - couplingPhaseOffset coupling)

phaseGate :: CPGNetwork -> Map OscillatorId TerrainGate -> OscillatorId -> Oscillator -> CPGState -> Double
phaseGate network terrainContext oscillatorId oscillator stateValue =
  let terrainGate = Map.findWithDefault (defaultTerrainGate network) oscillatorId terrainContext
      phaseValue = oscillatorPhaseUnwrapped (lookupStateSlot oscillatorId stateValue)
   in if inStancePhase oscillator phaseValue && terrainFootLocked terrainGate
        then terrainStanceGate terrainGate
        else
          if inSwingPhase oscillator phaseValue && terrainSearchingForFoothold terrainGate
            then terrainSearchGate terrainGate
            else 1.0

gateMapAt :: CPGNetwork -> Map OscillatorId TerrainGate -> CPGState -> Map OscillatorId Double
gateMapAt network terrainContext stateValue =
  Map.fromAscList
    ( fmap
        (\(oscillatorId, oscillator) -> (oscillatorId, phaseGate network terrainContext oscillatorId oscillator stateValue))
        (Map.toAscList (networkOscillators network))
    )

inStancePhase :: Oscillator -> Double -> Bool
inStancePhase oscillator phaseValue =
  wrappedPhase phaseValue <= 2.0 * pi * oscillatorDutyFactor oscillator

inSwingPhase :: Oscillator -> Double -> Bool
inSwingPhase oscillator = not . inStancePhase oscillator

recoveryScale :: CPGNetwork -> OscillatorState -> Double
recoveryScale network stateSlot =
  let totalFrames = networkRecoveryFrames network
      remainingFrames = oscillatorRecoveryRemaining stateSlot
   in if totalFrames <= 0
        then 1.0
        else 1.0 - fromIntegral remainingFrames / fromIntegral totalFrames

advanceOscillator :: CPGNetwork -> Map OscillatorId Double -> Map OscillatorId Double -> OscillatorId -> Double -> OscillatorState -> OscillatorState
advanceOscillator network gateMap rawDerivatives oscillatorId nextPhaseValue stateSlot =
  let gateValue = Map.findWithDefault 1.0 oscillatorId gateMap
      rawDerivative = Map.findWithDefault 0.0 oscillatorId rawDerivatives
      accumulatedDebt =
        if gateValue < 1.0
          then min (networkMaxPhaseDebt network) (oscillatorPhaseDebt stateSlot + cpgFixedDt * (abs rawDerivative - abs (gateValue * rawDerivative)))
          else oscillatorPhaseDebt stateSlot
      debtDischarge =
        if gateValue >= 1.0
          then min accumulatedDebt (cpgFixedDt * abs rawDerivative)
          else 0.0
      recoveryRemaining = max 0 (oscillatorRecoveryRemaining stateSlot - 1)
   in OscillatorState
        { oscillatorPhaseUnwrapped = nextPhaseValue + signum rawDerivative * debtDischarge,
          oscillatorPhaseDebt = accumulatedDebt - debtDischarge,
          oscillatorRecoveryRemaining = recoveryRemaining
        }

resetOscillatorState :: CPGNetwork -> Oscillator -> OscillatorState -> OscillatorState
resetOscillatorState network oscillator _stateSlot =
  OscillatorState
    { oscillatorPhaseUnwrapped = oscillatorPhaseBias oscillator,
      oscillatorPhaseDebt = 0.0,
      oscillatorRecoveryRemaining = networkRecoveryFrames network
    }

lookupStateSlot :: OscillatorId -> CPGState -> OscillatorState
lookupStateSlot oscillatorId stateValue =
  Map.findWithDefault (OscillatorState 0.0 0.0 0) oscillatorId (stateOscillators stateValue)

defaultTerrainGate :: CPGNetwork -> TerrainGate
defaultTerrainGate network =
  TerrainGate
    { terrainFootLocked = False,
      terrainSearchingForFoothold = False,
      terrainStanceGate = networkStanceGateDefault network,
      terrainSearchGate = networkSearchGateDefault network
    }

decodeStep :: CPGNetwork -> ODEStep -> Either CPGError CPGStep
decodeStep network stepValue =
  CPGStep (odeTime stepValue)
    <$> mkCPGState network (odeState stepValue)

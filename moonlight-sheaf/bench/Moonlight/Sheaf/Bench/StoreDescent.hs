{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Sheaf.Bench.StoreDescent
  ( storeDescentBenchmarks,
  )
where

import Control.DeepSeq (NFData (..))
import Control.Monad (foldM)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Vector qualified as Vector
import Moonlight.Delta.Scope (dirtyScope)
import Moonlight.Sheaf.Section.Model
  ( ModelFingerprint,
    SheafModel,
    prepareSheafModel,
    sheafModelFingerprint,
  )
import Moonlight.Sheaf.Section.Morphism
  ( RestrictionKind (..),
    RestrictionParts (..),
  )
import Moonlight.Sheaf.Section.ObjectIndex
  ( ObjectKey (..),
    initialSheafModelVersion,
    mkObjectIndex,
  )
import Moonlight.Sheaf.Section.Stalk
  ( StalkAlgebra (..),
    StalkRestrictionKernel (..),
  )
import Moonlight.Sheaf.Section.Store.Descent.Execute
import Moonlight.Sheaf.Section.Store.Descent.Prepare
import Moonlight.Sheaf.Section.Store.State
import Moonlight.Sheaf.Section.Store.Types
import Test.Tasty.Bench (Benchmark, bench, bgroup, env, nf, whnf)

storeDescentBenchmarks :: Benchmark
storeDescentBenchmarks =
  bgroup
    "core"
    [ env setupStoreBenchEnv $ \benchEnv ->
        bgroup
          "section-store-descent"
          [ bench "prepareSectionDescent/chain-1k" (whnf prepareSectionDescent (sbeChainModel benchEnv)),
            bench "prepareSectionDescent/fanout-1k" (whnf prepareSectionDescent (sbeFanoutModel benchEnv)),
            storeBench "descent/fast-root-final-program-128x-chain-1k" runFastRootFinalProgram128 benchEnv,
            storeBench "descent/fast-root-once-chain-1k" runFastRootOnce benchEnv,
            storeBench "descent/fast-root-final-coalesced-128x-chain-1k" runFastRootFinalCoalesced128 benchEnv,
            storeBench "descent/generic-incident-dirty-scope" runGenericIncidentDirtyScope benchEnv,
            storeBench "descent/mixed-delta-frontier-certification" runMixedDeltaFrontierCertification benchEnv
          ],
      env (setupDeepChainEnv 8192) $ \deepEnv ->
        bgroup
          "section-store-descent-deep"
          [ bench "prepareSectionDescent/chain-8k" (whnf prepareSectionDescent (dceModel deepEnv)),
            bench "descent/fast-root-once-chain-8k" (nf runDeepRootOnce deepEnv),
            bench "descent/generic-incident-dirty-scope-8k" (nf runDeepMiddleDelta deepEnv)
          ],
      env (setupPaddedEventEnv 8192) $ \paddedEnv ->
        bgroup
          "section-store-event-stream"
          [ bench "descent/event-stream-1000x-per-call-padded-8k" (nf (runPaddedEventStreamPerCall 1000) paddedEnv),
            bench "descent/event-stream-1000x-transaction-padded-8k" (nf (runPaddedEventStreamTransaction 1000) paddedEnv)
          ]
    ]

data DeepChainEnv = DeepChainEnv
  { dceObjectCount :: !Int,
    dceModel :: !(SheafModel StoreBenchCell StoreBenchWitness),
    dcePrepared :: !(PreparedSectionDescent StoreBenchCell StoreBenchWitness),
    dceSection :: !(TotalSectionStore StoreBenchCell StoreBenchStalk),
    dceRootOnceProgram :: !(PreparedSectionProgram StoreBenchStalk),
    dceMiddleDelta :: !(KeyedSectionDelta StoreBenchStalk)
  }

instance NFData DeepChainEnv where
  rnf deepEnv =
    dceModel deepEnv
      `seq` dcePrepared deepEnv
      `seq` dceSection deepEnv
      `seq` dceRootOnceProgram deepEnv
      `seq` dceMiddleDelta deepEnv
      `seq` ()

setupDeepChainEnv :: Int -> IO DeepChainEnv
setupDeepChainEnv objectCount =
  case do
    model <- storeBenchModel objectCount (chainMorphismsFor objectCount)
    prepared <- firstShow (prepareSectionDescent model)
    rootOnceProgram <- firstShow (prepareSectionObjectProgram prepared rootKey finalRootValue)
    pure
      DeepChainEnv
        { dceObjectCount = objectCount,
          dceModel = model,
          dcePrepared = prepared,
          dceSection = emptyTotalSectionStoreWith model (const zeroStalk),
          dceRootOnceProgram = rootOnceProgram,
          dceMiddleDelta = singletonDirtyDelta (sheafModelFingerprint model) (objectCount `div` 2) zeroStalk
        }
  of
    Left failure -> fail (show failure)
    Right deepEnv -> pure deepEnv

runDeepRootOnce :: DeepChainEnv -> StoreBenchOutcome
runDeepRootOnce deepEnv =
  sectionOutcome
    (ObjectKey (dceObjectCount deepEnv - 1))
    ( descendPreparedSectionProgram
        (dcePrepared deepEnv)
        storeBenchStalkAlgebra
        (dceRootOnceProgram deepEnv)
        (dceSection deepEnv)
    )

runDeepMiddleDelta :: DeepChainEnv -> StoreBenchOutcome
runDeepMiddleDelta deepEnv =
  sectionOutcome
    (ObjectKey (dceObjectCount deepEnv - 1))
    ( descendPreparedLocalKeyedBatch
        (dcePrepared deepEnv)
        storeBenchStalkAlgebra
        ObserveEachStep
        [dceMiddleDelta deepEnv]
        (dceSection deepEnv)
    )

data PaddedEventEnv = PaddedEventEnv
  { peePrepared :: !(PreparedSectionDescent StoreBenchCell StoreBenchWitness),
    peeSection :: !(TotalSectionStore StoreBenchCell StoreBenchStalk)
  }

instance NFData PaddedEventEnv where
  rnf paddedEnv =
    peePrepared paddedEnv
      `seq` peeSection paddedEnv
      `seq` ()

setupPaddedEventEnv :: Int -> IO PaddedEventEnv
setupPaddedEventEnv objectCount =
  case do
    model <- storeBenchModel objectCount [StoreBenchMorphism (StoreBenchCell 0) (StoreBenchCell 1)]
    prepared <- firstShow (prepareSectionDescent model)
    pure
      PaddedEventEnv
        { peePrepared = prepared,
          peeSection = emptyTotalSectionStoreWith model (const zeroStalk)
        }
  of
    Left failure -> fail (show failure)
    Right paddedEnv -> pure paddedEnv

paddedEventDelta :: PaddedEventEnv -> Int -> KeyedSectionDelta StoreBenchStalk
paddedEventDelta paddedEnv eventValue =
  singletonDirtyDelta (psdModelFingerprint (peePrepared paddedEnv)) 0 (StoreBenchStalk eventValue)

paddedEventOutcome :: Either String (TotalSectionStore StoreBenchCell StoreBenchStalk) -> StoreBenchOutcome
paddedEventOutcome finalOutcome =
  case finalOutcome of
    Left failure ->
      StoreBenchFailed failure
    Right finalSection ->
      case totalStalkAtKey (ObjectKey 1) finalSection of
        Left _ -> StoreBenchFailed "section projection failed"
        Right (StoreBenchStalk value) -> StoreBenchSucceeded value

runPaddedEventStreamPerCall :: Int -> PaddedEventEnv -> StoreBenchOutcome
runPaddedEventStreamPerCall eventCount paddedEnv =
  paddedEventOutcome (foldM applyEvent (peeSection paddedEnv) [1 .. eventCount])
  where
    applyEvent currentSection eventValue =
      either
        (Left . show)
        (Right . sdrSection)
        ( descendPreparedLocalKeyedBatch
            (peePrepared paddedEnv)
            storeBenchStalkAlgebra
            ObserveEachStep
            [paddedEventDelta paddedEnv eventValue]
            currentSection
        )

runPaddedEventStreamTransaction :: Int -> PaddedEventEnv -> StoreBenchOutcome
runPaddedEventStreamTransaction eventCount paddedEnv =
  paddedEventOutcome
    ( case
        runSectionDescentTransaction (peePrepared paddedEnv) storeBenchStalkAlgebra (peeSection paddedEnv) $ \transaction ->
          foldM
            ( \outcome eventValue ->
                case outcome of
                  Left descentError ->
                    pure (Left descentError)
                  Right () ->
                    transactKeyedSectionDelta transaction (paddedEventDelta paddedEnv eventValue)
            )
            (Right ())
            [1 .. eventCount]
        of
        Left failure -> Left (show failure)
        Right ((), descentResult) -> Right (sdrSection descentResult)
    )

storeBench :: String -> (StoreBenchEnv -> StoreBenchOutcome) -> StoreBenchEnv -> Benchmark
storeBench label runBench benchEnv =
  bench label (nf runBench benchEnv)

data StoreBenchEnv = StoreBenchEnv
  { sbeChainModel :: !(SheafModel StoreBenchCell StoreBenchWitness),
    sbeChainPrepared :: !(PreparedSectionDescent StoreBenchCell StoreBenchWitness),
    sbeChainSection :: !(TotalSectionStore StoreBenchCell StoreBenchStalk),
    sbeChainRootFinalProgram :: !(PreparedSectionProgram StoreBenchStalk),
    sbeChainRootOnceProgram :: !(PreparedSectionProgram StoreBenchStalk),
    sbeChainRootDeltas :: ![KeyedSectionDelta StoreBenchStalk],
    sbeChainMiddleDelta :: !(KeyedSectionDelta StoreBenchStalk),
    sbeFanoutModel :: !(SheafModel StoreBenchCell StoreBenchWitness),
    sbeFanoutPrepared :: !(PreparedSectionDescent StoreBenchCell StoreBenchWitness),
    sbeFanoutSection :: !(TotalSectionStore StoreBenchCell StoreBenchStalk),
    sbeFanoutLeafDeltas :: ![KeyedSectionDelta StoreBenchStalk]
  }

instance NFData StoreBenchEnv where
  rnf benchEnv =
    sbeChainModel benchEnv
      `seq` sbeChainPrepared benchEnv
      `seq` sbeChainSection benchEnv
      `seq` sbeChainRootFinalProgram benchEnv
      `seq` sbeChainRootOnceProgram benchEnv
      `seq` length (sbeChainRootDeltas benchEnv)
      `seq` sbeChainMiddleDelta benchEnv
      `seq` sbeFanoutModel benchEnv
      `seq` sbeFanoutPrepared benchEnv
      `seq` sbeFanoutSection benchEnv
      `seq` length (sbeFanoutLeafDeltas benchEnv)
      `seq` ()

data StoreBenchOutcome
  = StoreBenchSucceeded !Int
  | StoreBenchFailed !String
  deriving stock (Eq, Show)

instance NFData StoreBenchOutcome where
  rnf outcome =
    case outcome of
      StoreBenchSucceeded score -> rnf score
      StoreBenchFailed failure -> rnf failure

newtype StoreBenchCell = StoreBenchCell
  { unStoreBenchCell :: Int
  }
  deriving stock (Eq, Ord, Show)

data StoreBenchMorphism = StoreBenchMorphism
  { sbmSource :: !StoreBenchCell,
    sbmTarget :: !StoreBenchCell
  }
  deriving stock (Eq, Show)

data StoreBenchWitness = StoreBenchWitness
  deriving stock (Eq, Show)

newtype StoreBenchStalk = StoreBenchStalk
  { unStoreBenchStalk :: Int
  }
  deriving stock (Eq, Show)

data StoreBenchRepair = StoreBenchRepair
  deriving stock (Eq, Show)

setupStoreBenchEnv :: IO StoreBenchEnv
setupStoreBenchEnv =
  case do
    chainModel <- storeBenchModel 1024 chainMorphisms
    chainPrepared <- firstShow (prepareSectionDescent chainModel)
    let chainSection = emptyTotalSectionStoreWith chainModel (const zeroStalk)
    chainRootFinalProgram <- firstShow (prepareSectionObjectProgram chainPrepared rootKey repeatedRootValues)
    chainRootOnceProgram <- firstShow (prepareSectionObjectProgram chainPrepared rootKey finalRootValue)
    fanoutModel <- storeBenchModel 1024 fanoutMorphisms
    fanoutPrepared <- firstShow (prepareSectionDescent fanoutModel)
    let fanoutSection = emptyTotalSectionStoreWith fanoutModel (const zeroStalk)
    pure
      StoreBenchEnv
        { sbeChainModel = chainModel,
          sbeChainPrepared = chainPrepared,
          sbeChainSection = chainSection,
          sbeChainRootFinalProgram = chainRootFinalProgram,
          sbeChainRootOnceProgram = chainRootOnceProgram,
          sbeChainRootDeltas = repeatedRootDeltas (sheafModelFingerprint chainModel),
          sbeChainMiddleDelta = singletonDirtyDelta (sheafModelFingerprint chainModel) 512 zeroStalk,
          sbeFanoutModel = fanoutModel,
          sbeFanoutPrepared = fanoutPrepared,
          sbeFanoutSection = fanoutSection,
          sbeFanoutLeafDeltas = fmap (\objectOrdinal -> singletonDirtyDelta (sheafModelFingerprint fanoutModel) objectOrdinal zeroStalk) [1 .. 128]
        }
  of
    Left failure -> fail (show failure)
    Right benchEnv -> pure benchEnv

storeBenchModel :: Int -> [StoreBenchMorphism] -> Either String (SheafModel StoreBenchCell StoreBenchWitness)
storeBenchModel objectCount morphisms =
  firstShow
    ( prepareSheafModel
        initialSheafModelVersion
        (mkObjectIndex (storeBenchCells objectCount))
        ( \morphism ->
            RestrictionParts
              { partKind = PortalRestriction,
                partSource = sbmSource morphism,
                partTarget = sbmTarget morphism,
                partWitness = StoreBenchWitness
              }
        )
        morphisms
    )

storeBenchCells :: Int -> [StoreBenchCell]
storeBenchCells objectCount =
  fmap StoreBenchCell [0 .. objectCount - 1]

chainMorphisms :: [StoreBenchMorphism]
chainMorphisms =
  chainMorphismsFor 1024

chainMorphismsFor :: Int -> [StoreBenchMorphism]
chainMorphismsFor objectCount =
  fmap
    (\sourceOrdinal -> StoreBenchMorphism (StoreBenchCell sourceOrdinal) (StoreBenchCell (sourceOrdinal + 1)))
    [0 .. objectCount - 2]

fanoutMorphisms :: [StoreBenchMorphism]
fanoutMorphisms =
  fmap
    (\targetOrdinal -> StoreBenchMorphism (StoreBenchCell 0) (StoreBenchCell targetOrdinal))
    [1 .. 1023]

zeroStalk :: StoreBenchStalk
zeroStalk =
  StoreBenchStalk 0

rootKey :: ObjectKey
rootKey =
  ObjectKey 0

chainTailKey :: ObjectKey
chainTailKey =
  ObjectKey 1023

repeatedRootValues :: Vector.Vector StoreBenchStalk
repeatedRootValues =
  Vector.fromList (fmap StoreBenchStalk [1 .. 128])

finalRootValue :: Vector.Vector StoreBenchStalk
finalRootValue =
  Vector.singleton (StoreBenchStalk 128)

repeatedRootDeltas :: ModelFingerprint -> [KeyedSectionDelta StoreBenchStalk]
repeatedRootDeltas modelFingerprint =
  fmap (singletonDirtyDelta modelFingerprint 0 . StoreBenchStalk) [1 .. 128]

singletonDirtyDelta :: ModelFingerprint -> Int -> StoreBenchStalk -> KeyedSectionDelta StoreBenchStalk
singletonDirtyDelta modelFingerprint objectOrdinal stalk =
  KeyedSectionDelta
    { ksdModelFingerprint = modelFingerprint,
      ksdModelVersion = initialSheafModelVersion,
      ksdExtent = dirtyScope (IntSet.singleton objectOrdinal),
      ksdAssignments = IntMap.singleton objectOrdinal stalk
    }

runFastRootFinalProgram128 :: StoreBenchEnv -> StoreBenchOutcome
runFastRootFinalProgram128 =
  runChainProgram sbeChainRootFinalProgram

runFastRootOnce :: StoreBenchEnv -> StoreBenchOutcome
runFastRootOnce =
  runChainProgram sbeChainRootOnceProgram

runFastRootFinalCoalesced128 :: StoreBenchEnv -> StoreBenchOutcome
runFastRootFinalCoalesced128 =
  runChainBatch ObserveFinalSection sbeChainRootDeltas

runGenericIncidentDirtyScope :: StoreBenchEnv -> StoreBenchOutcome
runGenericIncidentDirtyScope =
  runChainBatch ObserveEachStep ((: []) . sbeChainMiddleDelta)

runMixedDeltaFrontierCertification :: StoreBenchEnv -> StoreBenchOutcome
runMixedDeltaFrontierCertification =
  runFanoutBatch ObserveEachStep sbeFanoutLeafDeltas

runChainProgram ::
  (StoreBenchEnv -> PreparedSectionProgram StoreBenchStalk) ->
  StoreBenchEnv ->
  StoreBenchOutcome
runChainProgram programOf benchEnv =
  sectionOutcome
    chainTailKey
    ( descendPreparedSectionProgram
        (sbeChainPrepared benchEnv)
        storeBenchStalkAlgebra
        (programOf benchEnv)
        (sbeChainSection benchEnv)
    )

runChainBatch ::
  SectionDescentObservation ->
  (StoreBenchEnv -> [KeyedSectionDelta StoreBenchStalk]) ->
  StoreBenchEnv ->
  StoreBenchOutcome
runChainBatch observation deltasOf benchEnv =
  sectionOutcome
    chainTailKey
    ( descendPreparedLocalKeyedBatch
        (sbeChainPrepared benchEnv)
        storeBenchStalkAlgebra
        observation
        (deltasOf benchEnv)
        (sbeChainSection benchEnv)
    )

runFanoutBatch ::
  SectionDescentObservation ->
  (StoreBenchEnv -> [KeyedSectionDelta StoreBenchStalk]) ->
  StoreBenchEnv ->
  StoreBenchOutcome
runFanoutBatch observation deltasOf benchEnv =
  sectionOutcome
    chainTailKey
    ( descendPreparedLocalKeyedBatch
        (sbeFanoutPrepared benchEnv)
        storeBenchStalkAlgebra
        observation
        (deltasOf benchEnv)
        (sbeFanoutSection benchEnv)
    )

sectionOutcome :: ObjectKey -> Either failure (SectionDescentResult StoreBenchCell StoreBenchStalk) -> StoreBenchOutcome
sectionOutcome sampleKey result =
  case result of
    Left _ ->
      StoreBenchFailed "descent failed"
    Right descentResult ->
      case totalStalkAtKey sampleKey (sdrSection descentResult) of
        Left _ -> StoreBenchFailed "section projection failed"
        Right (StoreBenchStalk value) -> StoreBenchSucceeded (sdrObservedSteps descentResult + value)

storeBenchStalkAlgebra :: StalkAlgebra StoreBenchWitness StoreBenchStalk () StoreBenchRepair
storeBenchStalkAlgebra =
  StalkAlgebra
    { saRestrictionKernel = const StalkRestrictionIdentity,
      saMismatches = \left right -> [() | left /= right],
      saMerge = \left _ -> Right left,
      saRepair = const (Left StoreBenchRepair),
      saNormalize = id
    }

firstShow :: Show failure => Either failure value -> Either String value
firstShow =
  either (Left . show) Right

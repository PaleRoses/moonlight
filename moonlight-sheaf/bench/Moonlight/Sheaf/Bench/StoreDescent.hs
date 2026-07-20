{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Sheaf.Bench.StoreDescent
  ( storeDescentBenchmarks,
  )
where

import Control.DeepSeq (NFData (..))
import Control.Monad (foldM)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.Vector qualified as Vector
import Moonlight.Delta.Scope (dirtyScope)
import Moonlight.Sheaf.Section.Model
  ( SheafModel,
    withPreparedSheafModel,
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
          [ bench "prepareSectionDescent/chain-1k" (whnf runSomeChainPreparation benchEnv),
            bench "prepareSectionDescent/fanout-1k" (whnf runSomeFanoutPreparation benchEnv),
            storeBench "descent/fast-root-final-program-128x-chain-1k" runFastRootFinalProgram128 benchEnv,
            storeBench "descent/fast-root-once-chain-1k" runFastRootOnce benchEnv,
            storeBench "descent/fast-root-final-coalesced-128x-chain-1k" runFastRootFinalCoalesced128 benchEnv,
            storeBench "descent/generic-incident-dirty-scope" runGenericIncidentDirtyScope benchEnv,
            storeBench "descent/mixed-delta-frontier-certification" runMixedDeltaFrontierCertification benchEnv
          ],
      env (setupDeepChainEnv 8192) $ \deepEnv ->
        bgroup
          "section-store-descent-deep"
          [ bench "prepareSectionDescent/chain-8k" (whnf runSomeDeepPreparation deepEnv),
            deepChainBench "descent/fast-root-once-chain-8k" runDeepRootOnce deepEnv,
            deepChainBench "descent/generic-incident-dirty-scope-8k" runDeepMiddleDelta deepEnv
          ],
      env (setupPaddedEventEnv 8192) $ \paddedEnv ->
        bgroup
          "section-store-event-stream"
          [ paddedEventBench "descent/event-stream-1000x-per-call-padded-8k" (runPaddedEventStreamPerCall 1000) paddedEnv,
            paddedEventBench "descent/event-stream-1000x-transaction-padded-8k" (runPaddedEventStreamTransaction 1000) paddedEnv
          ]
    ]

runSomeDeepPreparation :: SomeDeepChainEnv -> StoreBenchOutcome
runSomeDeepPreparation (SomeDeepChainEnv deepEnv) =
  preparationOutcome (prepareSectionDescent (dceModel deepEnv))

deepChainBench ::
  String ->
  (forall owner. DeepChainEnv owner -> StoreBenchOutcome) ->
  SomeDeepChainEnv ->
  Benchmark
deepChainBench label runBench deepEnv =
  bench label (nf (runSomeDeepChainBench runBench) deepEnv)

runSomeDeepChainBench ::
  (forall owner. DeepChainEnv owner -> StoreBenchOutcome) ->
  SomeDeepChainEnv ->
  StoreBenchOutcome
runSomeDeepChainBench runBench (SomeDeepChainEnv deepEnv) =
  runBench deepEnv

paddedEventBench ::
  String ->
  (forall owner. PaddedEventEnv owner -> StoreBenchOutcome) ->
  SomePaddedEventEnv ->
  Benchmark
paddedEventBench label runBench paddedEnv =
  bench label (nf (runSomePaddedEventBench runBench) paddedEnv)

runSomePaddedEventBench ::
  (forall owner. PaddedEventEnv owner -> StoreBenchOutcome) ->
  SomePaddedEventEnv ->
  StoreBenchOutcome
runSomePaddedEventBench runBench (SomePaddedEventEnv paddedEnv) =
  runBench paddedEnv

data DeepChainEnv owner = DeepChainEnv
  { dceObjectCount :: !Int,
    dceModel :: !(SheafModel owner StoreBenchCell StoreBenchWitness),
    dcePrepared :: !(PreparedSectionDescent owner StoreBenchCell StoreBenchWitness),
    dceSection :: !(TotalSectionStore owner StoreBenchCell StoreBenchStalk),
    dceRootOnceProgram :: !(PreparedSectionProgram owner StoreBenchStalk),
    dceMiddleDelta :: !(KeyedSectionDelta owner StoreBenchStalk)
  }

type role DeepChainEnv nominal

data SomeDeepChainEnv where
  SomeDeepChainEnv :: !(DeepChainEnv owner) -> SomeDeepChainEnv

instance NFData (DeepChainEnv owner) where
  rnf deepEnv =
    dceModel deepEnv
      `seq` dcePrepared deepEnv
      `seq` dceSection deepEnv
      `seq` dceRootOnceProgram deepEnv
      `seq` dceMiddleDelta deepEnv
      `seq` ()

instance NFData SomeDeepChainEnv where
  rnf (SomeDeepChainEnv deepEnv) = rnf deepEnv

setupDeepChainEnv :: Int -> IO SomeDeepChainEnv
setupDeepChainEnv objectCount =
  case do
    withStoreBenchModel objectCount (chainMorphismsFor objectCount) $ \model -> do
      prepared <- firstShow (prepareSectionDescent model)
      rootOnceProgram <- firstShow (prepareSectionObjectProgram prepared rootKey finalRootValue)
      pure
        ( SomeDeepChainEnv
            DeepChainEnv
              { dceObjectCount = objectCount,
                dceModel = model,
                dcePrepared = prepared,
                dceSection = emptyTotalSectionStoreWith model (const zeroStalk),
                dceRootOnceProgram = rootOnceProgram,
                dceMiddleDelta = singletonDirtyDelta model (objectCount `div` 2) zeroStalk
              }
        )
  of
    Left failure -> fail (show failure)
    Right deepEnv -> pure deepEnv

runDeepRootOnce :: DeepChainEnv owner -> StoreBenchOutcome
runDeepRootOnce deepEnv =
  sectionOutcome
    (ObjectKey (dceObjectCount deepEnv - 1))
    ( descendPreparedSectionProgram
        (dcePrepared deepEnv)
        storeBenchStalkAlgebra
        (dceRootOnceProgram deepEnv)
        (dceSection deepEnv)
    )

runDeepMiddleDelta :: DeepChainEnv owner -> StoreBenchOutcome
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

type PaddedEventEnv :: Type -> Type
data PaddedEventEnv owner = PaddedEventEnv
  { peePrepared :: !(PreparedSectionDescent owner StoreBenchCell StoreBenchWitness),
    peeSection :: !(TotalSectionStore owner StoreBenchCell StoreBenchStalk)
  }

type role PaddedEventEnv nominal

data SomePaddedEventEnv where
  SomePaddedEventEnv :: !(PaddedEventEnv owner) -> SomePaddedEventEnv

instance NFData (PaddedEventEnv owner) where
  rnf paddedEnv =
    peePrepared paddedEnv
      `seq` peeSection paddedEnv
      `seq` ()

instance NFData SomePaddedEventEnv where
  rnf (SomePaddedEventEnv paddedEnv) = rnf paddedEnv

setupPaddedEventEnv :: Int -> IO SomePaddedEventEnv
setupPaddedEventEnv objectCount =
  case do
    withStoreBenchModel objectCount [StoreBenchMorphism (StoreBenchCell 0) (StoreBenchCell 1)] $ \model -> do
      prepared <- firstShow (prepareSectionDescent model)
      pure
        ( SomePaddedEventEnv
            PaddedEventEnv
              { peePrepared = prepared,
                peeSection = emptyTotalSectionStoreWith model (const zeroStalk)
              }
        )
  of
    Left failure -> fail (show failure)
    Right paddedEnv -> pure paddedEnv

paddedEventDelta :: PaddedEventEnv owner -> Int -> KeyedSectionDelta owner StoreBenchStalk
paddedEventDelta paddedEnv eventValue =
  singletonDirtyDeltaFromPrepared (peePrepared paddedEnv) 0 (StoreBenchStalk eventValue)

paddedEventOutcome :: Either String (TotalSectionStore owner StoreBenchCell StoreBenchStalk) -> StoreBenchOutcome
paddedEventOutcome finalOutcome =
  case finalOutcome of
    Left failure ->
      StoreBenchFailed failure
    Right finalSection ->
      case totalStalkAtKey (ObjectKey 1) finalSection of
        Left _ -> StoreBenchFailed "section projection failed"
        Right (StoreBenchStalk value) -> StoreBenchSucceeded value

runPaddedEventStreamPerCall :: Int -> PaddedEventEnv owner -> StoreBenchOutcome
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

runPaddedEventStreamTransaction :: Int -> PaddedEventEnv owner -> StoreBenchOutcome
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

storeBench ::
  String ->
  (forall chainOwner fanoutOwner. StoreBenchEnv chainOwner fanoutOwner -> StoreBenchOutcome) ->
  SomeStoreBenchEnv ->
  Benchmark
storeBench label runBench benchEnv =
  bench label (nf (runSomeStoreBench runBench) benchEnv)

runSomeStoreBench ::
  (forall chainOwner fanoutOwner. StoreBenchEnv chainOwner fanoutOwner -> StoreBenchOutcome) ->
  SomeStoreBenchEnv ->
  StoreBenchOutcome
runSomeStoreBench runBench (SomeStoreBenchEnv benchEnv) =
  runBench benchEnv

runSomeChainPreparation :: SomeStoreBenchEnv -> StoreBenchOutcome
runSomeChainPreparation (SomeStoreBenchEnv benchEnv) =
  preparationOutcome (prepareSectionDescent (sbeChainModel benchEnv))

runSomeFanoutPreparation :: SomeStoreBenchEnv -> StoreBenchOutcome
runSomeFanoutPreparation (SomeStoreBenchEnv benchEnv) =
  preparationOutcome (prepareSectionDescent (sbeFanoutModel benchEnv))

preparationOutcome :: Show failure => Either failure prepared -> StoreBenchOutcome
preparationOutcome preparation =
  case preparation of
    Left failure -> StoreBenchFailed (show failure)
    Right _prepared -> StoreBenchSucceeded 1

data StoreBenchEnv chainOwner fanoutOwner = StoreBenchEnv
  { sbeChainModel :: !(SheafModel chainOwner StoreBenchCell StoreBenchWitness),
    sbeChainPrepared :: !(PreparedSectionDescent chainOwner StoreBenchCell StoreBenchWitness),
    sbeChainSection :: !(TotalSectionStore chainOwner StoreBenchCell StoreBenchStalk),
    sbeChainRootFinalProgram :: !(PreparedSectionProgram chainOwner StoreBenchStalk),
    sbeChainRootOnceProgram :: !(PreparedSectionProgram chainOwner StoreBenchStalk),
    sbeChainRootDeltas :: ![KeyedSectionDelta chainOwner StoreBenchStalk],
    sbeChainMiddleDelta :: !(KeyedSectionDelta chainOwner StoreBenchStalk),
    sbeFanoutModel :: !(SheafModel fanoutOwner StoreBenchCell StoreBenchWitness),
    sbeFanoutPrepared :: !(PreparedSectionDescent fanoutOwner StoreBenchCell StoreBenchWitness),
    sbeFanoutSection :: !(TotalSectionStore fanoutOwner StoreBenchCell StoreBenchStalk),
    sbeFanoutLeafDeltas :: ![KeyedSectionDelta fanoutOwner StoreBenchStalk]
  }

type role StoreBenchEnv nominal nominal

data SomeStoreBenchEnv where
  SomeStoreBenchEnv :: !(StoreBenchEnv chainOwner fanoutOwner) -> SomeStoreBenchEnv

instance NFData (StoreBenchEnv chainOwner fanoutOwner) where
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

instance NFData SomeStoreBenchEnv where
  rnf (SomeStoreBenchEnv benchEnv) = rnf benchEnv

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

setupStoreBenchEnv :: IO SomeStoreBenchEnv
setupStoreBenchEnv =
  case do
    withStoreBenchModel 1024 chainMorphisms $ \chainModel -> do
      chainPrepared <- firstShow (prepareSectionDescent chainModel)
      let chainSection = emptyTotalSectionStoreWith chainModel (const zeroStalk)
      chainRootFinalProgram <- firstShow (prepareSectionObjectProgram chainPrepared rootKey repeatedRootValues)
      chainRootOnceProgram <- firstShow (prepareSectionObjectProgram chainPrepared rootKey finalRootValue)
      withStoreBenchModel 1024 fanoutMorphisms $ \fanoutModel -> do
        fanoutPrepared <- firstShow (prepareSectionDescent fanoutModel)
        let fanoutSection = emptyTotalSectionStoreWith fanoutModel (const zeroStalk)
        pure
          ( SomeStoreBenchEnv
              StoreBenchEnv
                { sbeChainModel = chainModel,
                  sbeChainPrepared = chainPrepared,
                  sbeChainSection = chainSection,
                  sbeChainRootFinalProgram = chainRootFinalProgram,
                  sbeChainRootOnceProgram = chainRootOnceProgram,
                  sbeChainRootDeltas = repeatedRootDeltas chainModel,
                  sbeChainMiddleDelta = singletonDirtyDelta chainModel 512 zeroStalk,
                  sbeFanoutModel = fanoutModel,
                  sbeFanoutPrepared = fanoutPrepared,
                  sbeFanoutSection = fanoutSection,
                  sbeFanoutLeafDeltas = fmap (\objectOrdinal -> singletonDirtyDelta fanoutModel objectOrdinal zeroStalk) [1 .. 128]
                }
          )
  of
    Left failure -> fail (show failure)
    Right benchEnv -> pure benchEnv

withStoreBenchModel ::
  Int ->
  [StoreBenchMorphism] ->
  (forall owner. SheafModel owner StoreBenchCell StoreBenchWitness -> Either String result) ->
  Either String result
withStoreBenchModel objectCount morphisms useModel =
  firstShow
    ( withPreparedSheafModel
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
        useModel
    )
    >>= id

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

repeatedRootDeltas :: SheafModel owner StoreBenchCell StoreBenchWitness -> [KeyedSectionDelta owner StoreBenchStalk]
repeatedRootDeltas model =
  fmap (singletonDirtyDelta model 0 . StoreBenchStalk) [1 .. 128]

singletonDirtyDelta :: SheafModel owner StoreBenchCell StoreBenchWitness -> Int -> StoreBenchStalk -> KeyedSectionDelta owner StoreBenchStalk
singletonDirtyDelta _model objectOrdinal stalk =
  KeyedSectionDelta
    { ksdExtent = dirtyScope (IntSet.singleton objectOrdinal),
      ksdAssignments = IntMap.singleton objectOrdinal stalk
    }

singletonDirtyDeltaFromPrepared ::
  PreparedSectionDescent owner StoreBenchCell StoreBenchWitness ->
  Int ->
  StoreBenchStalk ->
  KeyedSectionDelta owner StoreBenchStalk
singletonDirtyDeltaFromPrepared _prepared objectOrdinal stalk =
  KeyedSectionDelta
    { ksdExtent = dirtyScope (IntSet.singleton objectOrdinal),
      ksdAssignments = IntMap.singleton objectOrdinal stalk
    }

runFastRootFinalProgram128 :: StoreBenchEnv chainOwner fanoutOwner -> StoreBenchOutcome
runFastRootFinalProgram128 =
  runChainProgram sbeChainRootFinalProgram

runFastRootOnce :: StoreBenchEnv chainOwner fanoutOwner -> StoreBenchOutcome
runFastRootOnce =
  runChainProgram sbeChainRootOnceProgram

runFastRootFinalCoalesced128 :: StoreBenchEnv chainOwner fanoutOwner -> StoreBenchOutcome
runFastRootFinalCoalesced128 =
  runChainBatch ObserveFinalSection sbeChainRootDeltas

runGenericIncidentDirtyScope :: StoreBenchEnv chainOwner fanoutOwner -> StoreBenchOutcome
runGenericIncidentDirtyScope =
  runChainBatch ObserveEachStep ((: []) . sbeChainMiddleDelta)

runMixedDeltaFrontierCertification :: StoreBenchEnv chainOwner fanoutOwner -> StoreBenchOutcome
runMixedDeltaFrontierCertification =
  runFanoutBatch ObserveEachStep sbeFanoutLeafDeltas

runChainProgram ::
  (StoreBenchEnv chainOwner fanoutOwner -> PreparedSectionProgram chainOwner StoreBenchStalk) ->
  StoreBenchEnv chainOwner fanoutOwner ->
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
  (StoreBenchEnv chainOwner fanoutOwner -> [KeyedSectionDelta chainOwner StoreBenchStalk]) ->
  StoreBenchEnv chainOwner fanoutOwner ->
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
  (StoreBenchEnv chainOwner fanoutOwner -> [KeyedSectionDelta fanoutOwner StoreBenchStalk]) ->
  StoreBenchEnv chainOwner fanoutOwner ->
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

sectionOutcome :: ObjectKey -> Either failure (SectionDescentResult owner StoreBenchCell StoreBenchStalk) -> StoreBenchOutcome
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

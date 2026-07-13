module Moonlight.Sheaf.Bench.PropagationToy
  ( propagationToyBenchmarks,
  )
where

import Control.DeepSeq (NFData (..))
import Control.Monad (foldM)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Moonlight.Delta.Scope (dirtyScope)
import Moonlight.Sheaf.TestFixture.PropagationToy
import Moonlight.Sheaf.Section.Model (SheafModel, prepareSheafModel, sheafModelFingerprint, sheafModelObjects)
import Moonlight.Sheaf.Section.Morphism (RestrictionParts (..), unitIncidenceRestriction)
import Moonlight.Sheaf.Index.Dense (denseIndexKeyOf)
import Moonlight.Sheaf.Section.ObjectIndex
  ( ObjectKey (..),
    initialSheafModelVersion,
    mkObjectIndex,
    unObjectKey,
  )
import Moonlight.Sheaf.Section.Stalk.Discrete
  ( discreteStalkAlgebra,
  )
import Moonlight.Sheaf.Section.Store.Descent.Execute
import Moonlight.Sheaf.Section.Store.Descent.Prepare
import Moonlight.Sheaf.Section.Store.State
import Moonlight.Sheaf.Section.Store.Types
import Test.Tasty.Bench (Benchmark, bench, bgroup, env, nf)

propagationToyBenchmarks :: Benchmark
propagationToyBenchmarks =
  bgroup
    "propagation-toy"
    [ bgroup
        "toy-flow"
        [ bench "moonlight core prepared object program/final section from 1000 parent values" (nf runMoonlightPreparedObjectProgramPropagationObservedBatch 1000),
          bench "moonlight keyed extent propagation/batch merge/1000 parent patches" (nf runMoonlightKeyedExtentPropagationBatchMerge 1000),
          bench "moonlight keyed extent propagation/coalesced transaction/1000 parent patches" (nf runMoonlightKeyedExtentPropagationCoalesced 1000),
          bench "reflex-style dynamic fanout lower-bound/1000 parent events" (nf runReflexStyleFanoutLowerBound 1000)
        ]
    , env setupToyBenchEnv $ \benchEnv ->
        bgroup
          "prepared-core"
          [ bench "moonlight core prepared object program/final section from 1000 values" (nf runPreparedObjectProgramObservedBatch benchEnv),
            bench "moonlight core prepared edit program/final section from 1000 edits" (nf runPreparedEditProgramObservedBatch benchEnv),
            bench "moonlight keyed extent propagation/batch merge/prebuilt 1000 patches" (nf runPreparedKeyedBatchMerge benchEnv),
            bench "moonlight keyed extent propagation/event-stream 1000x per-call/prebuilt root patches" (nf runPreparedEventStreamPerCall benchEnv),
            bench "moonlight keyed extent propagation/event-stream 1000x transaction/prebuilt root patches" (nf runPreparedEventStreamTransaction benchEnv),
            bench "moonlight keyed extent propagation/coalesced/prebuilt final patch" (nf runPreparedKeyedCoalesced benchEnv)
          ]
    , env setupSyntheticBenchEnv $ \syntheticEnv ->
        bgroup
          "synthetic-frontier"
          [ bench "chain 64/frontier closure" (nf runSyntheticPreparedBatch (sbeChain syntheticEnv)),
            bench "fanout 64/frontier closure" (nf runSyntheticPreparedBatch (sbeFanout syntheticEnv)),
            bench "diamond/frontier closure" (nf runSyntheticPreparedBatch (sbeDiamond syntheticEnv)),
            bench "disconnected padding 4096/immutable copy cost" (nf runSyntheticPreparedBatch (sbeCopy syntheticEnv))
          ]
    ]

data ToyBenchEnv = ToyBenchEnv
  { tbeSheaf :: !ToySheaf,
    tbeSection :: !ToySection,
    tbeObjectProgram :: !(PreparedSectionProgram ToyStalk),
    tbeStalkValues :: !(Vector ToyStalk),
    tbeEditProgram :: !(PreparedSectionProgram ToyStalk),
    tbeDeltas :: ![KeyedSectionDelta ToyStalk],
    tbeFinalDelta :: !(KeyedSectionDelta ToyStalk)
  }

instance NFData ToyBenchEnv where
  rnf benchEnv =
    tbeSheaf benchEnv
      `seq` tbeSection benchEnv
      `seq` tbeObjectProgram benchEnv
      `seq` Vector.length (tbeStalkValues benchEnv)
      `seq` tbeEditProgram benchEnv
      `seq` length (tbeDeltas benchEnv)
      `seq` tbeFinalDelta benchEnv
      `seq` ()

setupToyBenchEnv :: IO ToyBenchEnv
setupToyBenchEnv =
  case do
    sheaf <- toySheaf
    section <- initialToySectionWith sheaf (ToyStalk 0)
    objectProgram <- toyBenchmarkPreparedObjectProgram sheaf 1000
    let stalkValues = toyBenchmarkStalkValues 1000
    editProgram <- toyBenchmarkPreparedEditProgram sheaf 1000
    deltas <- toyBenchmarkKeyedPatches sheaf 1000
    finalDelta <- toyBenchmarkKeyedBatchDelta sheaf 1000
    pure
      ToyBenchEnv
        { tbeSheaf = sheaf,
          tbeSection = section,
          tbeObjectProgram = objectProgram,
          tbeStalkValues = stalkValues,
          tbeEditProgram = editProgram,
          tbeDeltas = deltas,
          tbeFinalDelta = finalDelta
        }
    of
    Left obstruction ->
      fail (show obstruction)
    Right benchEnv ->
      pure benchEnv

runPreparedObjectProgramObservedBatch :: ToyBenchEnv -> BenchmarkOutcome
runPreparedObjectProgramObservedBatch benchEnv =
  outcomeToyResult $ do
    propagatedSection <-
      propagateToyPreparedProgramObservedWith
        (tbeSheaf benchEnv)
        (tbeSection benchEnv)
        (tbeObjectProgram benchEnv)
    toyChildScore (tbeSheaf benchEnv) propagatedSection

runPreparedEditProgramObservedBatch :: ToyBenchEnv -> BenchmarkOutcome
runPreparedEditProgramObservedBatch benchEnv =
  outcomeToyResult $ do
    propagatedSection <- propagateToyPreparedProgramObservedWith (tbeSheaf benchEnv) (tbeSection benchEnv) (tbeEditProgram benchEnv)
    toyChildScore (tbeSheaf benchEnv) propagatedSection

runPreparedKeyedBatchMerge :: ToyBenchEnv -> BenchmarkOutcome
runPreparedKeyedBatchMerge benchEnv =
  outcomeToyResult $ do
    propagatedSection <- propagateToyKeyedDeltasWith (tbeSheaf benchEnv) (tbeSection benchEnv) (tbeDeltas benchEnv)
    toyChildScore (tbeSheaf benchEnv) propagatedSection

runPreparedEventStreamPerCall :: ToyBenchEnv -> BenchmarkOutcome
runPreparedEventStreamPerCall benchEnv =
  outcomeToyResult $ do
    propagatedSection <-
      foldM
        (propagateToyKeyedSectionWith (tbeSheaf benchEnv))
        (tbeSection benchEnv)
        (tbeDeltas benchEnv)
    toyChildScore (tbeSheaf benchEnv) propagatedSection

runPreparedEventStreamTransaction :: ToyBenchEnv -> BenchmarkOutcome
runPreparedEventStreamTransaction benchEnv =
  outcomeToyResult $ do
    propagatedSection <- propagateToyEventStreamWith (tbeSheaf benchEnv) (tbeSection benchEnv) (tbeDeltas benchEnv)
    toyChildScore (tbeSheaf benchEnv) propagatedSection

runPreparedKeyedCoalesced :: ToyBenchEnv -> BenchmarkOutcome
runPreparedKeyedCoalesced benchEnv =
  outcomeToyResult $ do
    propagatedSection <- propagateToyKeyedDeltasWith (tbeSheaf benchEnv) (tbeSection benchEnv) [tbeFinalDelta benchEnv]
    toyChildScore (tbeSheaf benchEnv) propagatedSection

runMoonlightKeyedExtentPropagationBatchMerge :: Int -> BenchmarkOutcome
runMoonlightKeyedExtentPropagationBatchMerge count =
  outcomeToyResult $ do
    sheaf <- toySheaf
    deltas <- toyBenchmarkKeyedPatches sheaf count
    section0 <- initialToySectionWith sheaf (ToyStalk 0)
    propagatedSection <- propagateToyKeyedBatchWith sheaf section0 deltas
    toyChildScore sheaf propagatedSection

runMoonlightPreparedObjectProgramPropagationObservedBatch :: Int -> BenchmarkOutcome
runMoonlightPreparedObjectProgramPropagationObservedBatch count =
  outcomeToyResult $ do
    sheaf <- toySheaf
    objectProgram <- toyBenchmarkPreparedObjectProgram sheaf count
    section0 <- initialToySectionWith sheaf (ToyStalk 0)
    propagatedSection <- propagateToyPreparedProgramObservedWith sheaf section0 objectProgram
    toyChildScore sheaf propagatedSection

runMoonlightKeyedExtentPropagationCoalesced :: Int -> BenchmarkOutcome
runMoonlightKeyedExtentPropagationCoalesced count =
  outcomeToyResult $ do
    sheaf <- toySheaf
    delta <- toyBenchmarkKeyedBatchDelta sheaf count
    section0 <- initialToySectionWith sheaf (ToyStalk 0)
    propagatedSection <- propagateToyKeyedSectionWith sheaf section0 delta
    toyChildScore sheaf propagatedSection

data BenchmarkOutcome
  = BenchmarkSucceeded !Int
  | BenchmarkFailed !String

instance NFData BenchmarkOutcome where
  rnf outcome =
    case outcome of
      BenchmarkSucceeded score -> rnf score
      BenchmarkFailed reason -> rnf reason

outcomeToyResult :: Either ToyPropagationObstruction Int -> BenchmarkOutcome
outcomeToyResult =
  either (BenchmarkFailed . show) BenchmarkSucceeded

toyChildScore :: ToySheaf -> ToySection -> Either ToyPropagationObstruction Int
toyChildScore sheaf sectionValue = do
  stalk <- toyStalkAtWith sheaf ChildCell sectionValue
  Right (unToyStalk stalk)

newtype SyntheticCell = SyntheticCell
  { unSyntheticCell :: Int
  }
  deriving stock (Eq, Ord, Show)

data SyntheticArrow = SyntheticArrow
  { syntheticArrowSource :: !SyntheticCell,
    syntheticArrowTarget :: !SyntheticCell
  }
  deriving stock (Eq, Ord, Show)

newtype SyntheticStalk = SyntheticStalk
  { unSyntheticStalk :: Int
  }
  deriving stock (Eq, Ord, Show)

data SyntheticBenchEnv = SyntheticBenchEnv
  { sbeChain :: !SyntheticBenchCase,
    sbeFanout :: !SyntheticBenchCase,
    sbeDiamond :: !SyntheticBenchCase,
    sbeCopy :: !SyntheticBenchCase
  }

instance NFData SyntheticBenchEnv where
  rnf syntheticEnv =
    sbeChain syntheticEnv
      `seq` sbeFanout syntheticEnv
      `seq` sbeDiamond syntheticEnv
      `seq` sbeCopy syntheticEnv
      `seq` ()

data SyntheticBenchCase = SyntheticBenchCase
  { sbcModel :: !(SheafModel SyntheticCell SyntheticArrow),
    sbcDescent :: !(PreparedSectionDescent SyntheticCell SyntheticArrow),
    sbcSection :: !(TotalSectionStore SyntheticCell SyntheticStalk),
    sbcPatch :: !(KeyedSectionDelta SyntheticStalk),
    sbcTarget :: !SyntheticCell
  }

setupSyntheticBenchEnv :: IO SyntheticBenchEnv
setupSyntheticBenchEnv =
  case do
    chainCase <- mkSyntheticCase 64 (chainEdges 64) (SyntheticCell 0) (SyntheticCell 63)
    fanoutCase <- mkSyntheticCase 64 (fanoutEdges 64) (SyntheticCell 0) (SyntheticCell 63)
    diamondCase <- mkSyntheticCase 4 diamondEdges (SyntheticCell 0) (SyntheticCell 3)
    copyCase <- mkSyntheticCase 4096 [] (SyntheticCell 4095) (SyntheticCell 4095)
    pure
      SyntheticBenchEnv
        { sbeChain = chainCase,
          sbeFanout = fanoutCase,
          sbeDiamond = diamondCase,
          sbeCopy = copyCase
        }
    of
    Left failure ->
      fail failure
    Right syntheticEnv ->
      pure syntheticEnv

mkSyntheticCase ::
  Int ->
  [SyntheticArrow] ->
  SyntheticCell ->
  SyntheticCell ->
  Either String SyntheticBenchCase
mkSyntheticCase cellCount arrows sourceCell targetCell = do
  model <-
    either
      (Left . show)
      Right
      ( prepareSheafModel
          initialSheafModelVersion
          (mkObjectIndex cells)
          ( \arrow ->
              RestrictionParts
                { partKind = unitIncidenceRestriction,
                  partSource = syntheticArrowSource arrow,
                  partTarget = syntheticArrowTarget arrow,
                  partWitness = arrow
                }
          )
          arrows
      )
  preparedDescent <- either (Left . show) Right (prepareSectionDescent model)
  section <- either (Left . show) Right (mkTotalSectionStore model zeroEntries)
  sourceKey <- keyForSyntheticCell model sourceCell
  pure
    SyntheticBenchCase
      { sbcModel = model,
        sbcDescent = preparedDescent,
        sbcSection = section,
        sbcPatch =
          KeyedSectionDelta
            { ksdModelFingerprint = sheafModelFingerprint model,
              ksdModelVersion = initialSheafModelVersion,
              ksdExtent = dirtyScope (IntSet.singleton (unObjectKey sourceKey)),
              ksdAssignments = IntMap.singleton (unObjectKey sourceKey) (SyntheticStalk 7)
            },
        sbcTarget = targetCell
      }
  where
    cells =
      fmap SyntheticCell [0 .. cellCount - 1]

    zeroEntries =
      Map.fromList (fmap (\cell -> (cell, SyntheticStalk 0)) cells)

chainEdges :: Int -> [SyntheticArrow]
chainEdges cellCount =
  fmap
    (\sourceOrdinal -> SyntheticArrow (SyntheticCell sourceOrdinal) (SyntheticCell (sourceOrdinal + 1)))
    [0 .. cellCount - 2]

fanoutEdges :: Int -> [SyntheticArrow]
fanoutEdges cellCount =
  fmap
    (\targetOrdinal -> SyntheticArrow (SyntheticCell 0) (SyntheticCell targetOrdinal))
    [1 .. cellCount - 1]

diamondEdges :: [SyntheticArrow]
diamondEdges =
  [ SyntheticArrow (SyntheticCell 0) (SyntheticCell 1),
    SyntheticArrow (SyntheticCell 0) (SyntheticCell 2),
    SyntheticArrow (SyntheticCell 1) (SyntheticCell 3),
    SyntheticArrow (SyntheticCell 2) (SyntheticCell 3)
  ]

keyForSyntheticCell :: SheafModel SyntheticCell SyntheticArrow -> SyntheticCell -> Either String ObjectKey
keyForSyntheticCell model cell =
  case denseIndexKeyOf cell (sheafModelObjects model) of
    Just key ->
      Right key
    Nothing ->
      Left ("synthetic cell missing from model: " <> show cell)

runSyntheticPreparedBatch :: SyntheticBenchCase -> BenchmarkOutcome
runSyntheticPreparedBatch benchCase =
  outcomeSyntheticResult $ do
    descentResult <-
      either
        (Left . show)
        Right
        ( descendPreparedLocalKeyedBatch
            (sbcDescent benchCase)
            discreteStalkAlgebra
            ObserveFinalSection
            [sbcPatch benchCase]
            (sbcSection benchCase)
        )
    syntheticTargetScore (sbcModel benchCase) (sbcTarget benchCase) (sdrSection descentResult)

syntheticTargetScore ::
  SheafModel SyntheticCell SyntheticArrow ->
  SyntheticCell ->
  TotalSectionStore SyntheticCell SyntheticStalk ->
  Either String Int
syntheticTargetScore model targetCell sectionValue =
  either (Left . show) (Right . unSyntheticStalk) (totalStalkAt model targetCell sectionValue)

outcomeSyntheticResult :: Either String Int -> BenchmarkOutcome
outcomeSyntheticResult =
  either BenchmarkFailed BenchmarkSucceeded

data ReflexStyleSection = ReflexStyleSection
  { rssParent :: !Int,
    rssChild :: !Int
  }

data ReflexStyleEvent
  = ReflexParentUpdated !Int
  | ReflexChildPinned !Int

runReflexStyleFanoutLowerBound :: Int -> BenchmarkOutcome
runReflexStyleFanoutLowerBound count =
  let finalSection =
        List.foldl' applyReflexStyleEvent initialReflexStyleSection (reflexStyleBenchmarkEvents count)
   in rssParent finalSection `seq` BenchmarkSucceeded (rssChild finalSection)
  where
    initialReflexStyleSection =
      ReflexStyleSection
        { rssParent = 0,
          rssChild = 0
        }

reflexStyleBenchmarkEvents :: Int -> [ReflexStyleEvent]
reflexStyleBenchmarkEvents count =
  fmap ReflexParentUpdated [1 .. count]
{-# NOINLINE reflexStyleBenchmarkEvents #-}

applyReflexStyleEvent :: ReflexStyleSection -> ReflexStyleEvent -> ReflexStyleSection
applyReflexStyleEvent _ (ReflexParentUpdated value) =
  ReflexStyleSection
    { rssParent = value,
      rssChild = value
    }
applyReflexStyleEvent sectionValue (ReflexChildPinned value) =
  sectionValue {rssChild = value}

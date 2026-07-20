{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}

module Moonlight.Sheaf.Bench.PropagationAudit
  ( propagationAuditBenchmarks,
  )
where

import Control.DeepSeq (NFData (..))
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Delta.Scope (Scoped, dirtyScope)
import Moonlight.Sheaf.TestFixture.PropagationToy
import Moonlight.Sheaf.TestFixture.PropagationToy.Audit
  ( fullToyCompatibilityAuditAfterPatchWith,
    scopedToyCompatibilityAuditAfterPatchWith,
  )
import Moonlight.Sheaf.Section.Certified
  ( SectionCertification (..),
    certifyPreparedSectionCompatibility,
    certifyPreparedSectionExtentCompatibility,
    certifySectionCompatibility,
  )
import Moonlight.Sheaf.Section.Model (SheafModel, sheafModelObjects, withPreparedSheafModel)
import Moonlight.Sheaf.Section.Morphism (RestrictionParts (..), unitIncidenceRestriction)
import Moonlight.Sheaf.Index.Dense (denseIndexKeyOf)
import Moonlight.Sheaf.Section.ObjectIndex
  ( ObjectKey,
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

propagationAuditBenchmarks :: Benchmark
propagationAuditBenchmarks =
  bgroup
    "propagation-audit"
    [ bgroup
        "compatibility-audit"
        [ bench "toy full compatibility audit/1000 parent patches" (nf runToyFullCompatibilityAuditCached 1000),
          bench "toy scoped compatibility audit/1000 parent patches" (nf runToyScopedCompatibilityAuditCached 1000),
          env setupSyntheticAuditEnv $ \syntheticEnv ->
            syntheticAuditBench "synthetic diamond/full compatibility audit" runSyntheticFullCompatibilityAudit syntheticEnv,
          env setupSyntheticAuditEnv $ \syntheticEnv ->
            syntheticAuditBench "synthetic diamond/prepared compatibility audit" runSyntheticPreparedCompatibilityAudit syntheticEnv,
          env setupSyntheticChainAuditEnv $ \syntheticEnv ->
            syntheticAuditBench "synthetic chain-4096/prepared full audit" runSyntheticPreparedCompatibilityAudit syntheticEnv,
          env setupSyntheticChainAuditEnv $ \syntheticEnv ->
            syntheticAuditBench "synthetic chain-4096/scoped audit dirty-singleton" runSyntheticScopedCompatibilityAudit syntheticEnv
        ]
    ]

syntheticAuditBench ::
  String ->
  (forall owner. SyntheticAuditEnv owner -> BenchmarkOutcome) ->
  SomeSyntheticAuditEnv ->
  Benchmark
syntheticAuditBench label runBench syntheticEnv =
  bench label (nf (runSomeSyntheticAuditBench runBench) syntheticEnv)

runSomeSyntheticAuditBench ::
  (forall owner. SyntheticAuditEnv owner -> BenchmarkOutcome) ->
  SomeSyntheticAuditEnv ->
  BenchmarkOutcome
runSomeSyntheticAuditBench runBench (SomeSyntheticAuditEnv syntheticEnv) =
  runBench syntheticEnv

data BenchmarkOutcome
  = BenchmarkSucceeded !Int
  | BenchmarkFailed !String

instance NFData BenchmarkOutcome where
  rnf outcome =
    case outcome of
      BenchmarkSucceeded score -> rnf score
      BenchmarkFailed reason -> rnf reason

runToyFullCompatibilityAuditCached :: Int -> BenchmarkOutcome
runToyFullCompatibilityAuditCached count =
  outcomeToyResult
    ( withToySheaf $ \sheaf -> do
        section0 <- initialToySectionWith sheaf (ToyStalk 0)
        List.foldl' (auditToyPatch sheaf section0) (Right 0) (toyBenchmarkPatches count)
    )

auditToyPatch ::
  ToySheaf owner ->
  ToySection owner ->
  Either ToyPropagationObstruction Int ->
  Scoped (Set.Set ToyCell) ToyPatch ->
  Either ToyPropagationObstruction Int
auditToyPatch sheaf section eitherScore patchValue = do
  score <- eitherScore
  certification <- fullToyCompatibilityAuditAfterPatchWith sheaf section patchValue
  pure (score + certificationWeight certification)

runToyScopedCompatibilityAuditCached :: Int -> BenchmarkOutcome
runToyScopedCompatibilityAuditCached count =
  outcomeToyResult
    ( withToySheaf $ \sheaf -> do
        section0 <- initialToySectionWith sheaf (ToyStalk 0)
        List.foldl' (auditToyScopedPatch sheaf section0) (Right 0) (toyBenchmarkPatches count)
    )

auditToyScopedPatch ::
  ToySheaf owner ->
  ToySection owner ->
  Either ToyPropagationObstruction Int ->
  Scoped (Set.Set ToyCell) ToyPatch ->
  Either ToyPropagationObstruction Int
auditToyScopedPatch sheaf section eitherScore patchValue = do
  score <- eitherScore
  certification <- scopedToyCompatibilityAuditAfterPatchWith sheaf section patchValue
  pure (score + certificationWeight certification)

certificationWeight :: SectionCertification cell mismatch -> Int
certificationWeight certification =
  case certification of
    SectionCertified -> 1
    SectionRejected mismatches -> Map.size mismatches

outcomeToyResult :: Either ToyPropagationObstruction Int -> BenchmarkOutcome
outcomeToyResult =
  either (BenchmarkFailed . show) BenchmarkSucceeded

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

data SyntheticAuditEnv owner = SyntheticAuditEnv
  { saeModel :: !(SheafModel owner SyntheticCell SyntheticArrow),
    saeDescent :: !(PreparedSectionDescent owner SyntheticCell SyntheticArrow),
    saeSection :: !(TotalSectionStore owner SyntheticCell SyntheticStalk),
    saeSourceKey :: !Int
  }

type role SyntheticAuditEnv nominal

data SomeSyntheticAuditEnv where
  SomeSyntheticAuditEnv :: !(SyntheticAuditEnv owner) -> SomeSyntheticAuditEnv

instance NFData (SyntheticAuditEnv owner) where
  rnf syntheticEnv =
    saeModel syntheticEnv
      `seq` saeDescent syntheticEnv
      `seq` saeSection syntheticEnv
      `seq` saeSourceKey syntheticEnv
      `seq` ()

instance NFData SomeSyntheticAuditEnv where
  rnf (SomeSyntheticAuditEnv syntheticEnv) = rnf syntheticEnv

setupSyntheticAuditEnv :: IO SomeSyntheticAuditEnv
setupSyntheticAuditEnv =
  either fail pure (mkSyntheticAuditEnvFrom 4 diamondEdges (SyntheticCell 0))

setupSyntheticChainAuditEnv :: IO SomeSyntheticAuditEnv
setupSyntheticChainAuditEnv =
  either fail pure (mkSyntheticAuditEnvFrom chainAuditCellCount (chainEdges chainAuditCellCount) (SyntheticCell 0))

chainAuditCellCount :: Int
chainAuditCellCount =
  4096

chainEdges :: Int -> [SyntheticArrow]
chainEdges cellCount =
  fmap
    (\index -> SyntheticArrow (SyntheticCell index) (SyntheticCell (index + 1)))
    [0 .. cellCount - 2]

mkSyntheticAuditEnvFrom :: Int -> [SyntheticArrow] -> SyntheticCell -> Either String SomeSyntheticAuditEnv
mkSyntheticAuditEnvFrom cellCount edges sourceCell =
  withSyntheticCase cellCount edges sourceCell $ \benchCase -> do
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
    sourceKey <- keyForSyntheticCell (sbcModel benchCase) sourceCell
    pure
      ( SomeSyntheticAuditEnv
          SyntheticAuditEnv
            { saeModel = sbcModel benchCase,
              saeDescent = sbcDescent benchCase,
              saeSection = sdrSection descentResult,
              saeSourceKey = unObjectKey sourceKey
            }
      )

data SyntheticBenchCase owner = SyntheticBenchCase
  { sbcModel :: !(SheafModel owner SyntheticCell SyntheticArrow),
    sbcDescent :: !(PreparedSectionDescent owner SyntheticCell SyntheticArrow),
    sbcSection :: !(TotalSectionStore owner SyntheticCell SyntheticStalk),
    sbcPatch :: !(KeyedSectionDelta owner SyntheticStalk)
  }

type role SyntheticBenchCase nominal

withSyntheticCase ::
  Int ->
  [SyntheticArrow] ->
  SyntheticCell ->
  (forall owner. SyntheticBenchCase owner -> Either String result) ->
  Either String result
withSyntheticCase cellCount arrows sourceCell useBenchCase =
  either (Left . show) Right
    ( withPreparedSheafModel
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
          ( \model -> do
              preparedDescent <- either (Left . show) Right (prepareSectionDescent model)
              section <- either (Left . show) Right (mkTotalSectionStore model zeroEntries)
              sourceKey <- keyForSyntheticCell model sourceCell
              useBenchCase
                SyntheticBenchCase
                  { sbcModel = model,
                    sbcDescent = preparedDescent,
                    sbcSection = section,
                    sbcPatch =
                      KeyedSectionDelta
                        { ksdExtent = dirtyScope (IntSet.singleton (unObjectKey sourceKey)),
                          ksdAssignments = IntMap.singleton (unObjectKey sourceKey) (SyntheticStalk 7)
                        }
                  }
          )
    )
    >>= id
  where
    cells =
      fmap SyntheticCell [0 .. cellCount - 1]

    zeroEntries =
      Map.fromList (fmap (\cell -> (cell, SyntheticStalk 0)) cells)

diamondEdges :: [SyntheticArrow]
diamondEdges =
  [ SyntheticArrow (SyntheticCell 0) (SyntheticCell 1),
    SyntheticArrow (SyntheticCell 0) (SyntheticCell 2),
    SyntheticArrow (SyntheticCell 1) (SyntheticCell 3),
    SyntheticArrow (SyntheticCell 2) (SyntheticCell 3)
  ]

keyForSyntheticCell :: SheafModel owner SyntheticCell SyntheticArrow -> SyntheticCell -> Either String ObjectKey
keyForSyntheticCell model cell =
  case denseIndexKeyOf cell (sheafModelObjects model) of
    Just key ->
      Right key
    Nothing ->
      Left ("synthetic cell missing from model: " <> show cell)

runSyntheticFullCompatibilityAudit :: SyntheticAuditEnv owner -> BenchmarkOutcome
runSyntheticFullCompatibilityAudit syntheticEnv =
  certificationOutcome
    ( certifySectionCompatibility
        (saeModel syntheticEnv)
        discreteStalkAlgebra
        (saeSection syntheticEnv)
    )

runSyntheticPreparedCompatibilityAudit :: SyntheticAuditEnv owner -> BenchmarkOutcome
runSyntheticPreparedCompatibilityAudit syntheticEnv =
  certificationOutcome
    ( certifyPreparedSectionCompatibility
        (saeDescent syntheticEnv)
        discreteStalkAlgebra
        (saeSection syntheticEnv)
    )

runSyntheticScopedCompatibilityAudit :: SyntheticAuditEnv owner -> BenchmarkOutcome
runSyntheticScopedCompatibilityAudit syntheticEnv =
  certificationOutcome
    ( certifyPreparedSectionExtentCompatibility
        (saeDescent syntheticEnv)
        discreteStalkAlgebra
        (dirtyScope (IntSet.singleton (saeSourceKey syntheticEnv)))
        (saeSection syntheticEnv)
    )

certificationOutcome :: Show failure => Either failure (SectionCertification cell mismatch) -> BenchmarkOutcome
certificationOutcome =
  either (BenchmarkFailed . show) (BenchmarkSucceeded . certificationWeight)

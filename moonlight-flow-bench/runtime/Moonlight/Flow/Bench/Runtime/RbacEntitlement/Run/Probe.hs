{-# LANGUAGE BangPatterns #-}

module Moonlight.Flow.Bench.Runtime.RbacEntitlement.Run.Probe
  ( runRbacSetupProbe,
  )
where

import Control.Exception
  ( evaluate,
  )
import Data.Bifunctor
  ( first,
  )
import Data.Bits
  ( xor,
  )
import Data.Foldable qualified as Foldable
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word
  ( Word64,
  )
import Moonlight.Flow.Runtime.Create qualified as R
import Moonlight.Differential.Row.Tuple
  ( RowTupleKey,
    tupleKeyToInts,
  )
import Moonlight.Flow.Bench.Runtime.RbacEntitlement.Stats
import Moonlight.Flow.Bench.Runtime.RbacEntitlement.Types
import Moonlight.Flow.Runtime.RbacFixture.Plans
  ( buildRbacModel,
  )
import Moonlight.Flow.Runtime.RbacFixture.Truth
  ( runtimeSpecFromModel,
    seedTruth,
    truthRelation,
  )
import Moonlight.Flow.Runtime.RbacFixture.Types
  ( allAtomNames,
    rbacAtomKey,
  )
import System.IO
  ( hFlush,
    stdout,
  )

runRbacSetupProbe :: RbacWorkloadConfig -> IO ()
runRbacSetupProbe config = do
  probeLine ("config=" <> show config)
  probeLine "phase=model:start"
  (!modelNs, modelResult) <- timed (evaluate (fromRbacFixture buildRbacModel))
  case modelResult of
    Left err ->
      probeLine ("phase=model:error ms=" <> showNsMs modelNs <> " error=" <> show err)
    Right model -> do
      probeLine ("phase=model:done ms=" <> showNsMs modelNs)
      probeLine "phase=seed:start"
      (!seedNs, (!truth0, !_rng0, !truthProbeValue)) <-
        timed $ do
          let (!truthValue, !rngValue) = seedTruth (rwcSize config) (rwcSeedCounts config) (rwcPatchSeed config)
              !probeValue = truthProbe truthValue
          evaluate (forceTruthProbe probeValue)
          pure (truthValue, rngValue, probeValue)
      probeLine ("phase=seed:done ms=" <> showNsMs seedNs <> " truth=" <> show truthProbeValue)
      probeLine "phase=spec:start"
      (!specNs, specResult) <-
        timed (evaluate (fromRbacFixture (runtimeSpecFromModel model truth0)))
      case specResult of
        Left err ->
          probeLine ("phase=spec:error ms=" <> showNsMs specNs <> " error=" <> show err)
        Right specValue -> do
          probeLine ("phase=spec:done ms=" <> showNsMs specNs)
          probeLine "phase=createRuntime:start"
          (!createNs, createResult) <-
            timed $
              case first RbacCreateError (R.createRuntime specValue) of
                Left err ->
                  pure (Left err)
                Right runtime0 -> do
                  let !reuseStats = runtimeDiagnosticsReuseStats runtime0
                      !reuseDiagnostics = runtimeDiagnosticsReuseDiagnostics runtime0
                  _ <- evaluate reuseStats
                  _ <- evaluate reuseDiagnostics
                  pure (Right (runtime0, reuseStats, reuseDiagnostics))
          case createResult of
            Left err ->
              probeLine ("phase=createRuntime:error ms=" <> showNsMs createNs <> " error=" <> show err)
            Right (_runtime0, reuseStats, reuseDiagnostics) -> do
              statsSample <- readRuntimeStatsSample
              probeLine
                ( "phase=createRuntime:done ms="
                    <> showNsMs createNs
                    <> " reuse="
                    <> show reuseStats
                    <> " reuse_diag="
                    <> show reuseDiagnostics
                    <> " rts="
                    <> show statsSample
                )

probeLine :: String -> IO ()
probeLine text = do
  putStrLn text
  hFlush stdout

truthProbe :: RbacTruth -> RbacTruthProbe
truthProbe truth =
  RbacTruthProbe
    { rtpTotalRows = totalRows,
      rtpRelationRows = relationRows,
      rtpChecksum = checksum
    }
  where
    relationSummaries =
      fmap summarizeRelation allAtomNames
    relationRows =
      Map.fromList (fmap (\(!atomName, !rowCount, !_rowChecksum) -> (atomName, rowCount)) relationSummaries)
    totalRows =
      Foldable.foldl' (\acc (!_atomName, !rowCount, !_rowChecksum) -> acc + rowCount) 0 relationSummaries
    checksum =
      Foldable.foldl' (\acc (!atomName, !_rowCount, !rowChecksum) -> acc `xor` (fromIntegral (rbacAtomKey atomName) * 0x9e3779b97f4a7c15) `xor` rowChecksum) 0 relationSummaries
    summarizeRelation atomName =
      let !rowsValue = truthRelation atomName truth
          !rowCount = Set.size rowsValue
          !rowChecksum = Foldable.foldl' checksumRow 0 rowsValue
       in (atomName, rowCount, rowChecksum)
    checksumRow :: Word64 -> RowTupleKey -> Word64
    checksumRow acc rowValue =
      Foldable.foldl'
        (\slotAcc slotValue -> slotAcc * 0x100000001b3 `xor` fromIntegral slotValue)
        (acc `xor` 0xcbf29ce484222325)
        (tupleKeyToInts rowValue)

forceTruthProbe :: RbacTruthProbe -> ()
forceTruthProbe probeValue =
  rtpTotalRows probeValue
    `seq` rtpChecksum probeValue
    `seq` Map.foldl' (\acc rowCount -> rowCount `seq` acc) () (rtpRelationRows probeValue)
{-# INLINE forceTruthProbe #-}

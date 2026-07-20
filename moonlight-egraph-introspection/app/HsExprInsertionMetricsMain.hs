module Main
  ( main,
  )
where

import System.Environment (getArgs)
import System.Exit (die)
import Moonlight.EGraph.Introspection.Core.HsExpr
  ( HsExprInsertionMetrics (..),
    measureHaskellSourceInsertionMetrics,
  )

main :: IO ()
main = do
  args <- getArgs
  case args of
    [sourcePath] -> do
      sourceText <- readFile sourcePath
      case measureHaskellSourceInsertionMetrics sourcePath sourceText of
        Left failureValue ->
          die ("insertion-metrics failed: " <> show failureValue)
        Right metricsValue ->
          putStrLn (renderInsertionMetrics metricsValue)
    _ ->
      die "usage: moonlight-hsexpr-insertion-metrics <haskell-source-file>"

renderInsertionMetrics :: HsExprInsertionMetrics -> String
renderInsertionMetrics metricsValue =
  unlines
    [ "bindings: " <> show (himBindingCount metricsValue),
      "scoped-exprs: " <> show (himScopedExprCount metricsValue),
      "observed-contexts: " <> show (himObservedContextCount metricsValue),
      "total-support-width: " <> show (himTotalSupportContextCount metricsValue),
      "max-support-width: " <> show (himMaxSupportContextCount metricsValue),
      "rebases: " <> show (himRebaseCount metricsValue),
      "rebase-dirty-contexts: " <> show (himRebaseDirtyContextCount metricsValue),
      "final-active-contexts: " <> show (himFinalActiveContextCount metricsValue),
      "final-restrictions: " <> show (himFinalRestrictionCount metricsValue),
      "final-changed-contexts: " <> show (himFinalChangedContextCount metricsValue),
      "final-class-support-entry-count: " <> show (himFinalClassSupportEntryCount metricsValue),
      "final-support-carrier-generator-count: " <> show (himFinalStoredSupportContextCount metricsValue),
      "final-regional-parent-edge-count: " <> show (himFinalRegionalParentEdgeCount metricsValue),
      "final-regional-parent-region-cube-count: " <> show (himFinalRegionalParentRegionCubeCount metricsValue),
      "final-regional-variant-row-count: " <> show (himFinalRegionalVariantRowCount metricsValue),
      "final-regional-absorbed-row-count: " <> show (himFinalRegionalAbsorbedRowCount metricsValue),
      "final-context-analysis-delta-count: " <> show (himFinalContextAnalysisDeltaCount metricsValue),
      "base-nodes: "
        <> show (himBaseNodeCountBefore metricsValue)
        <> " -> "
        <> show (himBaseNodeCountAfter metricsValue),
      "base-classes: "
        <> show (himBaseClassCountBefore metricsValue)
        <> " -> "
        <> show (himBaseClassCountAfter metricsValue)
    ]

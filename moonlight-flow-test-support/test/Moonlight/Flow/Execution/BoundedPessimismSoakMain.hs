module Main
  ( main,
  )
where

import System.Environment
  ( getArgs,
  )
import System.Exit
  ( die,
  )
import Test.Moonlight.Flow.Execution.BoundedPessimism
  ( BoundedPessimismConfig (..),
    BoundedPessimismReport,
    boundedPessimismSoakConfig,
    bprIncrementalWork,
    bprMaxIncrementalWork,
    bprMaxReferenceWork,
    bprOperations,
    bprProperSubsetOperations,
    bprReferenceWork,
    runBoundedPessimismWorkGuarantee,
  )

main :: IO ()
main = do
  args <- getArgs
  config <-
    case configFromArgs args of
      Left message ->
        die message
      Right parsed ->
        pure parsed
  report <- runBoundedPessimismWorkGuarantee config
  putStrLn (renderReport report)

configFromArgs :: [String] -> Either String BoundedPessimismConfig
configFromArgs args =
  case args of
    [] ->
      Right boundedPessimismSoakConfig
    [seconds] -> do
      secondsValue <- readNonNegativeInt "seconds" seconds
      Right boundedPessimismSoakConfig {bpcDurationSeconds = Just secondsValue}
    [seconds, leaves, roots, rowsPerRoot, auditPeriod] -> do
      secondsValue <- readNonNegativeInt "seconds" seconds
      leafValue <- readPositiveInt "leaf-count" leaves
      rootValue <- readPositiveInt "root-count" roots
      rowsPerRootValue <- readPositiveInt "rows-per-root" rowsPerRoot
      auditValue <- readPositiveInt "audit-period" auditPeriod
      Right
        boundedPessimismSoakConfig
          { bpcDurationSeconds = Just secondsValue,
            bpcLeafCount = leafValue,
            bpcRootCount = rootValue,
            bpcRowsPerRoot = rowsPerRootValue,
            bpcAuditPeriod = auditValue
          }
    _ ->
      Left usage

usage :: String
usage =
  unlines
    [ "usage:",
      "  bounded-pessimism-soak",
      "  bounded-pessimism-soak <seconds>",
      "  bounded-pessimism-soak <seconds> <leaf-count> <root-count> <rows-per-root> <audit-period>"
    ]

readPositiveInt :: String -> String -> Either String Int
readPositiveInt label value = do
  parsed <- readInt label value
  if parsed > 0
    then Right parsed
    else Left (label <> " must be positive: " <> value)

readNonNegativeInt :: String -> String -> Either String Int
readNonNegativeInt label value = do
  parsed <- readInt label value
  if parsed >= 0
    then Right parsed
    else Left (label <> " must be non-negative: " <> value)

readInt :: String -> String -> Either String Int
readInt label value =
  case reads value of
    [(parsed, "")] ->
      Right parsed
    _ ->
      Left ("invalid " <> label <> ": " <> value)

renderReport :: BoundedPessimismReport -> String
renderReport report =
  "bounded-pessimism soak ok: operations="
    <> show (bprOperations report)
    <> " proper-subset-operations="
    <> show (bprProperSubsetOperations report)
    <> " reference-work="
    <> show (bprReferenceWork report)
    <> " incremental-work="
    <> show (bprIncrementalWork report)
    <> " saved-work="
    <> show saved
    <> " saved-ratio="
    <> show savedRatio
    <> " max-reference-work="
    <> show (bprMaxReferenceWork report)
    <> " max-incremental-work="
    <> show (bprMaxIncrementalWork report)
  where
    saved =
      bprReferenceWork report - bprIncrementalWork report

    savedRatio :: Double
    savedRatio =
      if bprReferenceWork report == 0
        then 0
        else fromIntegral saved / fromIntegral (bprReferenceWork report)

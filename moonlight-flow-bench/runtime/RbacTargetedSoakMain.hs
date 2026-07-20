module Main
  ( main,
  )
where

import Moonlight.Flow.Bench.Runtime.RbacEntitlement.Print
  ( printRbacTargetedTimingReport,
  )
import Moonlight.Flow.Bench.Runtime.RbacEntitlement.Run.Targeted
  ( runRbacTargetedTimingMatrix,
  )
import System.Environment
  ( getArgs,
  )

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] ->
      runTargeted
    ["--help"] ->
      putStrLn usageText
    _ ->
      putStrLn usageText

runTargeted :: IO ()
runTargeted = do
  result <- runRbacTargetedTimingMatrix
  case result of
    Left err ->
      print err
    Right report ->
      printRbacTargetedTimingReport report

usageText :: String
usageText =
  "usage: rbac-targeted-soak"

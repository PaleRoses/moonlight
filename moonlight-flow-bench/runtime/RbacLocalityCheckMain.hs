module Main
  ( main,
  )
where

import Moonlight.Flow.Bench.Runtime.RbacEntitlement.Print
  ( printRbacLocalityMatrixReport,
  )
import Moonlight.Flow.Bench.Runtime.RbacEntitlement.Run.Locality
  ( runRbacLocalityMatrix,
  )
import System.Environment
  ( getArgs,
  )

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] ->
      runLocality
    ["--help"] ->
      putStrLn usageText
    _ ->
      putStrLn usageText

runLocality :: IO ()
runLocality = do
  result <- runRbacLocalityMatrix
  case result of
    Left err ->
      print err
    Right report ->
      printRbacLocalityMatrixReport report

usageText :: String
usageText =
  "usage: rbac-locality-check"

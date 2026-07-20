module Main
  ( main,
  )
where

import Moonlight.Flow.Bench.Runtime.RbacEntitlement.Print
  ( printResourceScopeReproducerReport,
  )
import Moonlight.Flow.Bench.Runtime.RbacEntitlement.Run.Reproducer
  ( runResourceScopeFrontierReproducer,
  )
import System.Environment
  ( getArgs,
  )

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] ->
      runReproducer
    ["--help"] ->
      putStrLn usageText
    _ ->
      putStrLn usageText

runReproducer :: IO ()
runReproducer = do
  result <- runResourceScopeFrontierReproducer
  case result of
    Left err ->
      print err
    Right report ->
      printResourceScopeReproducerReport report

usageText :: String
usageText =
  "usage: rbac-reproducer"

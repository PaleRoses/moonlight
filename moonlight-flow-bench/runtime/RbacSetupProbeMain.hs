module Main
  ( main,
  )
where

import Moonlight.Flow.Bench.Runtime.RbacEntitlement.Config
  ( hugeRbacWorkloadConfig,
    smokePerfRbacWorkloadConfig,
    smokeRbacWorkloadConfig,
    workstationRbacWorkloadConfig,
  )
import Moonlight.Flow.Bench.Runtime.RbacEntitlement.Run.Probe
  ( runRbacSetupProbe,
  )
import Moonlight.Flow.Bench.Runtime.RbacEntitlement.Types
  ( RbacWorkloadConfig,
  )
import System.Environment
  ( getArgs,
  )

main :: IO ()
main = do
  args <- getArgs
  case parseConfig args of
    Left err ->
      putStrLn err
    Right config ->
      runRbacSetupProbe config

parseConfig :: [String] -> Either String RbacWorkloadConfig
parseConfig args =
  case args of
    [] ->
      Right workstationRbacWorkloadConfig
    ["smoke"] ->
      Right smokeRbacWorkloadConfig
    ["smoke-perf"] ->
      Right smokePerfRbacWorkloadConfig
    ["workstation"] ->
      Right workstationRbacWorkloadConfig
    ["huge"] ->
      Right hugeRbacWorkloadConfig
    ["--help"] ->
      Left usageText
    _ ->
      Left usageText

usageText :: String
usageText =
  "usage: rbac-setup-probe [smoke|smoke-perf|workstation|huge]"

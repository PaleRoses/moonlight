module Main (main) where

import System.Environment
  ( getArgs,
  )
import System.Exit
  ( die,
  )
import Moonlight.Flow.Runtime.RbacDataflowFixture
  ( writeRbacDataflowCBOR,
  )

main :: IO ()
main =
  getArgs >>= either die writeFixtureOrDie . outputPathFromArgs

writeFixtureOrDie :: FilePath -> IO ()
writeFixtureOrDie path =
  writeRbacDataflowCBOR path >>= either (die . show) pure

outputPathFromArgs :: [String] -> Either String FilePath
outputPathFromArgs args =
  case args of
    [path] -> Right path
    _ -> Left "usage: moonlight-rbac-runtime-dataflow-fixture <output-runtime-dataflow.cbor>"

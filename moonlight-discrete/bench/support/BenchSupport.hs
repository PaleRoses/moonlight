module BenchSupport
  ( BenchmarkFixtureFailure (..),
    benchFailure,
    caseLabel,
    smallSizes,
    mediumSizes,
    largeSizes,
    keys,
  )
where

import Control.Exception (Exception, throw)

newtype BenchmarkFixtureFailure = BenchmarkFixtureFailure
  { benchmarkFixtureFailureDetail :: String
  }
  deriving stock (Show)

instance Exception BenchmarkFixtureFailure

benchFailure :: Show err => String -> err -> result
benchFailure label err =
  throw (BenchmarkFixtureFailure (label <> ": " <> show err))

caseLabel :: String -> Int -> String
caseLabel label size =
  label <> "/" <> show size

smallSizes :: [Int]
smallSizes = [8, 16, 32]

mediumSizes :: [Int]
mediumSizes = [32, 128, 512]

largeSizes :: [Int]
largeSizes = [128, 512, 2048]

keys :: Int -> [Int]
keys size = [0 .. size - 1]

module BenchSupport
  ( foundationSizes,
    numericSizes,
    syntaxSizes,
    termSizes,
    unionFindSizes,
    caseLabel,
    keys,
    sampleKeys,
    showLength,
  )
where

import Prelude

foundationSizes :: [Int]
foundationSizes = [128, 512, 2048]

numericSizes :: [Int]
numericSizes = [256, 1024, 4096]

syntaxSizes :: [Int]
syntaxSizes = [16, 32, 64]

termSizes :: [Int]
termSizes = [128, 512, 1024]

unionFindSizes :: [Int]
unionFindSizes = [128, 512, 2048, 8192]

caseLabel :: String -> Int -> String
caseLabel label size =
  label <> " n=" <> show size

keys :: Int -> [Int]
keys size = [0 .. size - 1]

sampleKeys :: Int -> [Int]
sampleKeys size =
  take 64 (keys size)

showLength :: String -> Int
showLength =
  length

{-# LANGUAGE BangPatterns #-}

module Moonlight.Sheaf.Bench.QueryTrianglePowerLaw
  ( trianglePowerLawBenchmarks,
  )
where

import Test.Tasty.Bench
  ( Benchmark,
    bench,
    bgroup,
    env,
    nf,
  )
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Bits
  ( shiftL,
    shiftR,
    xor,
  )
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Maybe (isJust)
import Data.Word (Word64)
import Moonlight.Differential.Join.WCOJ.Dense.Triangle
  ( DenseTriangleTrie,
    TriangleBenchmarkStats (..),
    buildDenseTriangleTrie,
    countTrianglesWCOJ,
    normalizeUndirectedEdge,
    triangleBenchmarkStats,
  )
import System.Environment (lookupEnv)
import System.Exit (die)
import Text.Read (readMaybe)

trianglePowerLawBenchmarks :: IO Benchmark
trianglePowerLawBenchmarks = do
  giantEnabled <- isJust <$> lookupEnv "MOONLIGHT_SHEAF_QUERY_BENCH_ENABLE_GIANT"
  if not giantEnabled
    then do
      putStrLn "giant triangle power-law query benchmark skipped by default. Set MOONLIGHT_SHEAF_QUERY_BENCH_ENABLE_GIANT=1 to opt in."
      pure (bgroup "triangle-power-law" [])
    else do
      config <- triangleBenchConfigFromEnv
      pure
        ( bgroup
            "triangle-power-law"
            [ env (buildGuardedTriangleTrie config) $ \trie ->
                bench (triangleBenchLabel config) $
                  nf countTrianglesWCOJ trie
            ]
        )

buildGuardedTriangleTrie :: TriangleBenchConfig -> IO DenseTriangleTrie
buildGuardedTriangleTrie config = do
  trie <-
    evaluate
      ( force
          ( buildDenseTriangleTrie
              (rmatUndirectedEdges (tbcScale config) (tbcTargetEdges config) (tbcSeed config))
          )
      )
  let stats = triangleBenchmarkStats trie
  putStrLn (renderTriangleStats stats)
  if tbsWorkToAgm stats > 10.0
    then
      die
        ( "dense-trie WCOJ work ratio exceeded 10x AGM proxy: "
            <> show (tbsWorkToAgm stats)
        )
    else pure trie

data TriangleBenchConfig = TriangleBenchConfig
  { tbcScale :: !Int,
    tbcTargetEdges :: !Int,
    tbcSeed :: !Word64
  }

triangleBenchConfigFromEnv :: IO TriangleBenchConfig
triangleBenchConfigFromEnv =
  TriangleBenchConfig
    <$> positiveEnvIntWithDefault "MOONLIGHT_SHEAF_QUERY_BENCH_SCALE" 20
    <*> positiveEnvIntWithDefault "MOONLIGHT_SHEAF_QUERY_BENCH_EDGES" 1_000_000
    <*> pure 0x9e3779b97f4a7c15

positiveEnvIntWithDefault :: String -> Int -> IO Int
positiveEnvIntWithDefault name fallback =
  lookupEnv name >>= maybe (pure fallback) parseEnvValue
  where
    parseEnvValue rawValue =
      case readMaybe rawValue of
        Just parsedValue | parsedValue > 0 ->
          pure parsedValue
        _ ->
          die (name <> " must be a positive integer, received: " <> show rawValue)

triangleBenchLabel :: TriangleBenchConfig -> String
triangleBenchLabel config =
  "triangle/RMAT-2^"
    <> show (tbcScale config)
    <> "/"
    <> renderEdgeCount (tbcTargetEdges config)
    <> "/wcoj-dense-trie"

renderEdgeCount :: Int -> String
renderEdgeCount edgeCount
  | edgeCount == 1_000_000 = "1e6"
  | otherwise = show edgeCount

renderTriangleStats :: TriangleBenchmarkStats -> String
renderTriangleStats stats =
  unlines
    [ "triangle benchmark input:",
      "  vertices:           " <> show (tbsVertices stats),
      "  edges:              " <> show (tbsEdges stats),
      "  triangles:          " <> show (tbsTriangles stats),
      "  intersection steps: " <> show (tbsIntersectionSteps stats),
      "  AGM proxy m^1.5:    " <> show (tbsAgmBound stats),
      "  work / AGM:         " <> show (tbsWorkToAgm stats)
    ]

rmatUndirectedEdges :: Int -> Int -> Word64 -> [(Int, Int)]
rmatUndirectedEdges scale requestedEdges seed0 =
  go (sanitizeSeed seed0) HashSet.empty 0 []
  where
    normalizedScale =
      max 1 scale

    vertexCount =
      1 `shiftL` normalizedScale

    maxEdges =
      vertexCount * (vertexCount - 1) `quot` 2

    targetEdges =
      max 0 (min requestedEdges maxEdges)

    go ::
      Word64 ->
      HashSet (Int, Int) ->
      Int ->
      [(Int, Int)] ->
      [(Int, Int)]
    go !seed !seen !accepted !acc
      | accepted >= targetEdges =
          acc
      | otherwise =
          let (!rawEdge, !seed') =
                rmatEdge normalizedScale seed
           in case normalizeUndirectedEdge rawEdge of
                Nothing ->
                  go seed' seen accepted acc
                Just edgeValue
                  | HashSet.member edgeValue seen ->
                      go seed' seen accepted acc
                  | otherwise ->
                      go
                        seed'
                        (HashSet.insert edgeValue seen)
                        (accepted + 1)
                        (edgeValue : acc)

rmatEdge :: Int -> Word64 -> ((Int, Int), Word64)
rmatEdge scale =
  go scale 0 0
  where
    go :: Int -> Int -> Int -> Word64 -> ((Int, Int), Word64)
    go !remainingBits !leftVertex !rightVertex !seed
      | remainingBits <= 0 =
          ((leftVertex, rightVertex), seed)
      | otherwise =
          let !seed' = nextWord64 seed
              !quadrant = fromIntegral (seed' `rem` 100) :: Int
              !bit = 1 `shiftL` (remainingBits - 1)
              (!leftVertex', !rightVertex')
                | quadrant < 57 = (leftVertex, rightVertex)
                | quadrant < 76 = (leftVertex + bit, rightVertex)
                | quadrant < 95 = (leftVertex, rightVertex + bit)
                | otherwise = (leftVertex + bit, rightVertex + bit)
           in go
                (remainingBits - 1)
                leftVertex'
                rightVertex'
                seed'

sanitizeSeed :: Word64 -> Word64
sanitizeSeed seed
  | seed == 0 =
      0x9e3779b97f4a7c15
  | otherwise =
      seed

nextWord64 :: Word64 -> Word64
nextWord64 seed0 =
  let seed1 = sanitizeSeed seed0
      seed2 = seed1 `xor` (seed1 `shiftR` 12)
      seed3 = seed2 `xor` (seed2 `shiftL` 25)
      seed4 = seed3 `xor` (seed3 `shiftR` 27)
   in seed4 * 2685821657736338717

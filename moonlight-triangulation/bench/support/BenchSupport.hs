{-# LANGUAGE NumericUnderscores #-}

-- | The measurement harness every benchmark slice shares. Deliberately not
-- @tasty-bench@: allocated bytes and the work counters the library reports
-- about itself are the figures a slice is here to expose, and a harness built
-- around a timing distribution cannot express either.
module BenchSupport
  ( timedValue
  , requireRight
  , randomPoints
  , latticePoints
  , latticeFaceBand
  ) where

import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import qualified Data.List as List
import Data.Word (Word64)
import qualified Data.Vector as V
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Conc.Sync (getAllocationCounter)
import GHC.Stats
  ( RTSStats (max_live_bytes)
  , getRTSStats
  , getRTSStatsEnabled
  )
import Moonlight.Triangulation
  ( FaceId
  , Point (Point)
  , Triangulation
  , faceVertices
  , pointX
  , vertexPoint
  )
import System.CPUTime (getCPUTime)

-- | Both clocks, because they answer different questions and neither
-- substitutes for the other. Elapsed is what anything is compared against —
-- the board against spade is wall clock, and a parallel construction that used
-- more cores gets no allowance for having used them. CPU is the work receipt:
-- on one capability the two agree, and where they diverge the ratio is the
-- parallelism actually obtained. Reporting CPU alone, as this did, would let a
-- change that halves elapsed time and doubles total work read as a regression.
-- Allocation comes from the per-thread counter, not from @RTSStats@. The RTS
-- field is only refreshed at a collection, so a lane whose whole allocation
-- fits between two GCs reports a delta of exactly zero — the instrument
-- saturates at the bottom and reads as a perfect result. The thread counter is
-- block-granular and monotone, so a lane that allocates less says so.
--
-- @max_live_bytes@ is left as the RTS reports it and is a whole-process running
-- maximum sampled at major collections, never this lane's residency. Read it
-- as the watermark up to here, and only against a run with the same GC
-- schedule; @-G1@ is what makes two schedules comparable.
timedValue :: NFData value => String -> IO value -> IO value
timedValue label action = do
  statsEnabled <- getRTSStatsEnabled
  allocationStart <- getAllocationCounter
  wallStart <- getMonotonicTimeNSec
  cpuStart <- getCPUTime
  value <- action >>= evaluate . force
  cpuEnd <- getCPUTime
  wallEnd <- getMonotonicTimeNSec
  allocationEnd <- getAllocationCounter
  after <- if statsEnabled then Just <$> getRTSStats else pure Nothing
  putStrLn (label <> "-elapsed: " <> show (fromIntegral (wallEnd - wallStart) / 1.0e9 :: Double) <> "s")
  putStrLn (label <> "-cpu: " <> show (fromIntegral (cpuEnd - cpuStart) / 1.0e12 :: Double) <> "s")
  putStrLn (label <> "-allocated-bytes: " <> show (allocationStart - allocationEnd))
  case after of
    Just right -> putStrLn (label <> "-process-max-live-bytes: " <> show (max_live_bytes right))
    Nothing -> pure ()
  pure value

requireRight :: Show error => Either error value -> IO value
requireRight value = case value of
  Left failure -> fail (show failure)
  Right result -> pure result

randomPoints :: Word64 -> Int -> [Point]
randomPoints seed count = take count (go seed)
 where
  go :: Word64 -> [Point]
  go state =
    let state1 = state * 6364136223846793005 + 1442695040888963407
        state2 = state1 * 6364136223846793005 + 1442695040888963407
        unit :: Word64 -> Double
        unit value = fromIntegral (value `div` 2048) / 9_007_199_254_740_992
     in Point (2 * unit state1 - 1) (2 * unit state2 - 1) : go state2

-- | The canonical large planar-region fixture. Both the DCEL control and the
-- exact publication arm consume this one source rather than quietly drifting
-- into merely similar grids.
latticePoints :: Int -> Int -> V.Vector Point
latticePoints widthInCells heightInCells =
  V.generate
    ((widthInCells + 1) * (heightInCells + 1))
    ( \index ->
        let (row, column) = index `quotRem` (widthInCells + 1)
         in Point (fromIntegral column) (fromIntegral row)
    )

-- | Twenty-column connected face bands used by the frozen 239,360-face
-- publication receipt.
latticeFaceBand
  :: Triangulation mode Point directed undirected face
  -> FaceId
  -> Int
latticeFaceBand triangulation face =
  let xSum =
        List.foldl'
          (\accumulator vertex -> accumulator + pointX (vertexPoint triangulation vertex))
          0
          (faceVertices triangulation face)
   in floor (xSum / 3) `quot` 20

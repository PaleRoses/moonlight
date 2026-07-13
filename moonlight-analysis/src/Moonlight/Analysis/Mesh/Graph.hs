{-# LANGUAGE BangPatterns #-}

module Moonlight.Analysis.Mesh.Graph
  ( Graph(..)
  , edgeRange
  , pairMetricNormalFactor
  ) where

import Data.Kind (Type)
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word8)

type Graph :: Type
data Graph = Graph
  { grFaces          :: !Int
  , grOffsets        :: !(VU.Vector Int)
  , grNbrs           :: !(VU.Vector Int)
  , grEdgeSrc        :: !(VU.Vector Int)
  , grEdgePair       :: !(VU.Vector Int)
  , grPairA          :: !(VU.Vector Int)
  , grPairB          :: !(VU.Vector Int)
  , grPairHasAB      :: !(VU.Vector Word8)
  , grPairHasBA      :: !(VU.Vector Word8)
  , grPairBaseW      :: !(VU.Vector Double)
  , grFaceArea       :: !(VU.Vector Double)
  , grPairEdgeLen    :: !(VU.Vector Double)
  , grPairCenterDist :: !(VU.Vector Double)
  , grPairNx         :: !(VU.Vector Double)
  , grPairNy         :: !(VU.Vector Double)
  , grPairMetric11   :: !(VU.Vector Double)
  , grPairMetric12   :: !(VU.Vector Double)
  , grPairMetric22   :: !(VU.Vector Double)
  , grFaceOutDeg     :: !(VU.Vector Int)
  , grNewToOld       :: !(VU.Vector Int)
  , grOldToNew       :: !(VU.Vector Int)
  }

pairMetricNormalFactor :: Graph -> Int -> Double
pairMetricNormalFactor !gr !p =
  let !nx = VU.unsafeIndex (grPairNx gr) p
      !ny = VU.unsafeIndex (grPairNy gr) p
      !g11 = VU.unsafeIndex (grPairMetric11 gr) p
      !g12 = VU.unsafeIndex (grPairMetric12 gr) p
      !g22 = VU.unsafeIndex (grPairMetric22 gr) p
      !q0 = g11 * nx + g12 * ny
      !q1 = g12 * nx + g22 * ny
  in max 0.0 (nx * q0 + ny * q1)
{-# INLINE pairMetricNormalFactor #-}

edgeRange :: Graph -> Int -> (Int, Int)
edgeRange !gr !i =
  ( VU.unsafeIndex (grOffsets gr) i
  , VU.unsafeIndex (grOffsets gr) (i + 1)
  )
{-# INLINE edgeRange #-}

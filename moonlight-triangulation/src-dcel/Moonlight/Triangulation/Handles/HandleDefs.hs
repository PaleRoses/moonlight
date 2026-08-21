{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The identifier family and the arithmetic on it: a directed edge's twin is
-- its index complement, so orientation is a bit rather than a lookup.
module Moonlight.Triangulation.Handles.HandleDefs
  ( VertexId (..)
  , FaceId (..)
  , DirectedEdgeId (..)
  , UndirectedEdgeId (..)
  , reverseEdge
  , asUndirected
  , normalizedDirected
  , reversedDirected
  , directedPair
  , isNormalized
  ) where

import Control.DeepSeq (NFData)
import Data.Bits (shiftL, shiftR, xor, (.&.))
import Data.Word (Word32)

-- | Index of a vertex in the immutable DCEL.
newtype VertexId = VertexId { unVertexId :: Word32 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, NFData)

-- | Index of a face in the immutable DCEL; zero denotes the outer face.
newtype FaceId = FaceId { unFaceId :: Word32 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, NFData)

-- | Oriented half-edge index. Twin orientations differ only in the low bit.
newtype DirectedEdgeId = DirectedEdgeId { unDirectedEdgeId :: Word32 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, NFData)

-- | Index of a twin pair, with orientation forgotten.
newtype UndirectedEdgeId = UndirectedEdgeId { unUndirectedEdgeId :: Word32 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, NFData)

-- | Select the opposite orientation of the same undirected edge.
reverseEdge :: DirectedEdgeId -> DirectedEdgeId
reverseEdge (DirectedEdgeId edge) = DirectedEdgeId (edge `xor` 1)
{-# INLINE reverseEdge #-}

-- | Forget a directed edge's orientation.
asUndirected :: DirectedEdgeId -> UndirectedEdgeId
asUndirected (DirectedEdgeId edge) = UndirectedEdgeId (edge `shiftR` 1)
{-# INLINE asUndirected #-}

-- | Select the even-indexed orientation of an undirected edge.
normalizedDirected :: UndirectedEdgeId -> DirectedEdgeId
normalizedDirected (UndirectedEdgeId edge) = DirectedEdgeId (edge `shiftL` 1)
{-# INLINE normalizedDirected #-}

-- | Select the odd-indexed orientation of an undirected edge.
reversedDirected :: UndirectedEdgeId -> DirectedEdgeId
reversedDirected edge = reverseEdge (normalizedDirected edge)
{-# INLINE reversedDirected #-}

-- | Both orientations, normalized first and reversed second.
directedPair :: UndirectedEdgeId -> (DirectedEdgeId, DirectedEdgeId)
directedPair edge = (normalizedDirected edge, reversedDirected edge)
{-# INLINE directedPair #-}

-- | Whether a directed edge is the normalized orientation of its pair.
isNormalized :: DirectedEdgeId -> Bool
isNormalized (DirectedEdgeId edge) = edge .&. 1 == 0
{-# INLINE isNormalized #-}

-- | Compressed-sparse-row adjacency for finite @Int@ digraphs: construction
-- from rows, per-vertex target slices, out-degree, and transpose. A pure leaf.
module Moonlight.Core.Fixpoint.Dense.Internal.Csr
  ( CsrRole (..),
    Csr (..),
    GraphCsr,
    RowCsr,
    csrFromRows,
    csrFromBoundedRows,
    csrTargetsForKey,
    csrTargetsSet,
    csrOutDegree,
    csrTranspose,
    inBounds,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.List qualified as List
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as U
import Prelude

data CsrRole
  = SquareGraph
  | RectangularRows

type Csr :: CsrRole -> Type
data Csr shape = Csr
  { csrVertexCount :: !Int,
    csrOffsets :: !(Vector Int),
    csrTargets :: !(Vector Int)
  }
  deriving stock (Eq, Show)

type GraphCsr = Csr 'SquareGraph

type RowCsr = Csr 'RectangularRows

csrFromRows :: Int -> [[Int]] -> RowCsr
csrFromRows vertexCount rows =
  csrFromRowsWith (const True) vertexCount rows
{-# INLINE csrFromRows #-}

csrFromBoundedRows :: Int -> [[Int]] -> GraphCsr
csrFromBoundedRows vertexCount rows =
  csrFromRowsWith (inBounds n) n rows
  where
    n = max 0 vertexCount
{-# INLINE csrFromBoundedRows #-}

csrFromRowsWith :: (Int -> Bool) -> Int -> [[Int]] -> Csr shape
csrFromRowsWith keepTarget vertexCount rows =
  Csr
    { csrVertexCount = n,
      csrOffsets = U.fromList offsets,
      csrTargets = U.fromList targets
    }
  where
    n = max 0 vertexCount
    normalizedRows =
      fmap
        (IntSet.toAscList . IntSet.filter keepTarget . IntSet.fromList)
        (take n (rows <> repeat []))
    offsets = List.scanl' (+) 0 (fmap length normalizedRows)
    targets = foldMap id normalizedRows
{-# INLINE csrFromRowsWith #-}

csrTranspose :: GraphCsr -> GraphCsr
csrTranspose csr =
  csrFromBoundedRows
    (csrVertexCount csr)
    [ IntSet.toAscList (IntMap.findWithDefault IntSet.empty v predecessors)
      | v <- [0 .. csrVertexCount csr - 1]
    ]
  where
    predecessors =
      IntMap.fromListWith
        IntSet.union
        [ (target, IntSet.singleton source)
          | source <- [0 .. csrVertexCount csr - 1],
            target <- U.toList (csrTargetsForKey csr source)
        ]
{-# INLINE csrTranspose #-}

csrTargetsForKey :: Csr shape -> Int -> Vector Int
csrTargetsForKey csr key =
  case (csrOffsets csr U.!? key, csrOffsets csr U.!? (key + 1)) of
    (Just offset, Just nextOffset) -> U.slice offset (nextOffset - offset) (csrTargets csr)
    _ -> U.empty
{-# INLINE csrTargetsForKey #-}

csrTargetsSet :: Csr shape -> Int -> IntSet
csrTargetsSet csr key =
  IntSet.fromDistinctAscList (U.toList (csrTargetsForKey csr key))
{-# INLINE csrTargetsSet #-}

csrOutDegree :: Csr shape -> Int -> Int
csrOutDegree =
  (U.length .) . csrTargetsForKey
{-# INLINE csrOutDegree #-}

inBounds :: Int -> Int -> Bool
inBounds n key = key >= 0 && key < n
{-# INLINE inBounds #-}

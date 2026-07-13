{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Differential.Join.WCOJ.Dense.Triangle
  ( DenseTriangleTrie,
    TriangleCount (..),
    TriangleBenchmarkStats (..),
    normalizeUndirectedEdge,
    buildDenseTriangleTrie,
    countTrianglesWCOJ,
    triangleBenchmarkStats,
    agmTriangleBound,
  )
where

import Control.DeepSeq (NFData)
import Data.Bits ((.&.), (.|.), popCount, shiftL)
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List qualified as List
import Data.Tuple (swap)
import Data.Vector.Unboxed qualified as VU
import Data.Word (Word64)
import GHC.Generics (Generic)

-- | Sealed dense triangle trie for the specialized triangle WCOJ kernel.
--
-- The constructor is intentionally hidden.  The unsafe indexing below is valid
-- only for values produced by 'buildDenseTriangleTrie', which normalizes input
-- into nonnegative loop-free undirected edges, deduplicates them, orients them
-- by degree, sorts adjacency slices, and builds offsets with length
-- @vertexCount + 1@.  Callers get a pure counting API, not a forged hot kernel.
data DenseTriangleTrie = DenseTriangleTrie
  { dttVertexCount :: !Int,
    dttEdgeCount :: !Int,
    dttOffsets :: !(VU.Vector Int),
    dttTargets :: !(VU.Vector Int),
    dttOrientedSources :: !(VU.Vector Int),
    dttOrientedTargets :: !(VU.Vector Int),
    dttBitWordCount :: {-# UNPACK #-} !Int,
    dttBitWordOffsets :: !(VU.Vector Int),
    dttFirstNonzeroBitWords :: !(VU.Vector Int),
    dttLastNonzeroBitWords :: !(VU.Vector Int),
    dttAdjacencyBits :: !(VU.Vector Word64)
  }
  deriving stock (Eq, Show, Generic)

instance NFData DenseTriangleTrie

data TriangleCount = TriangleCount
  { tcTriangles :: {-# UNPACK #-} !Int,
    tcIntersectionSteps :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Show, Generic)

instance NFData TriangleCount

data TriangleBenchmarkStats = TriangleBenchmarkStats
  { tbsEdges :: {-# UNPACK #-} !Int,
    tbsVertices :: {-# UNPACK #-} !Int,
    tbsTriangles :: {-# UNPACK #-} !Int,
    tbsIntersectionSteps :: {-# UNPACK #-} !Int,
    tbsAgmBound :: {-# UNPACK #-} !Double,
    tbsWorkToAgm :: {-# UNPACK #-} !Double
  }
  deriving stock (Eq, Show, Generic)

instance NFData TriangleBenchmarkStats

buildDenseTriangleTrie :: [(Int, Int)] -> DenseTriangleTrie
buildDenseTriangleTrie rawEdges =
  DenseTriangleTrie
    { dttVertexCount = vertexCount,
      dttEdgeCount = length normalizedEdges,
      dttOffsets = offsets,
      dttTargets = targets,
      dttOrientedSources = orientedSources,
      dttOrientedTargets = orientedTargets,
      dttBitWordCount = bitWordCount,
      dttBitWordOffsets = bitWordOffsets,
      dttFirstNonzeroBitWords = firstNonzeroBitWords,
      dttLastNonzeroBitWords = lastNonzeroBitWords,
      dttAdjacencyBits = adjacencyBits
    }
  where
    normalizedEdges =
      normalizeUndirectedEdges rawEdges

    vertexCount =
      vertexCountFromEdges normalizedEdges

    degrees =
      degreeVector vertexCount normalizedEdges

    orientedEdges =
      List.sort (fmap (orientByDegree degrees) normalizedEdges)

    (orientedSources, orientedTargets) =
      orientedEdgeVectors orientedEdges

    bitWordCount =
      wordCountForVertices vertexCount

    bitWordOffsets =
      VU.generate bitWordCount id

    adjacencyBits =
      orientedAdjacencyBitVector vertexCount bitWordCount orientedEdges

    firstNonzeroBitWords =
      adjacencyBitFirstWords vertexCount bitWordCount bitWordOffsets adjacencyBits

    lastNonzeroBitWords =
      adjacencyBitLastWords vertexCount bitWordCount bitWordOffsets adjacencyBits

    adjacency =
      foldl'
        ( \acc (sourceVertex, targetVertex) ->
            IntMap.insertWith
              (<>)
              sourceVertex
              [targetVertex]
              acc
        )
        IntMap.empty
        orientedEdges

    (offsets, targets) =
      buildOffsetTargetVectors vertexCount adjacency

countTrianglesWCOJ :: DenseTriangleTrie -> TriangleCount
countTrianglesWCOJ trie =
  if denseTriangleBitsetWorthwhile trie
    then countTrianglesBitset trie
    else countTrianglesMerge trie

denseTriangleBitsetWorthwhile :: DenseTriangleTrie -> Bool
denseTriangleBitsetWorthwhile trie =
  dttBitWordCount trie > 1
    && dttEdgeCount trie >= dttVertexCount trie * dttBitWordCount trie

countTrianglesMerge :: DenseTriangleTrie -> TriangleCount
countTrianglesMerge trie =
  countEdges 0 0 0
  where
    -- The nested descent keeps the edge and intersection covers glued through
    -- scalar state, so the count-only kernel does not allocate one result
    -- carrier per oriented edge.
    !orientedSources = dttOrientedSources trie
    !orientedTargets = dttOrientedTargets trie
    !targets = dttTargets trie
    !orientedEdgeCount = VU.length orientedSources

    countEdges !edgeIndex !triangleAcc !stepAcc
      | edgeIndex >= orientedEdgeCount =
          TriangleCount
            { tcTriangles = triangleAcc,
              tcIntersectionSteps = stepAcc
            }
      | otherwise =
          let !leftVertex = VU.unsafeIndex orientedSources edgeIndex
              !rightVertex = VU.unsafeIndex orientedTargets edgeIndex
              (!leftStart, !leftEnd) = neighborBounds trie leftVertex
              (!rightStart, !rightEnd) = neighborBounds trie rightVertex
           in countIntersection
                (edgeIndex + 1)
                triangleAcc
                stepAcc
                leftStart
                leftEnd
                rightStart
                rightEnd

    countIntersection !nextEdgeIndex !triangleAcc !stepAcc !leftIndex !leftEnd !rightIndex !rightEnd
      | leftIndex >= leftEnd || rightIndex >= rightEnd =
          countEdges nextEdgeIndex triangleAcc stepAcc
      | otherwise =
          let !leftValue = VU.unsafeIndex targets leftIndex
              !rightValue = VU.unsafeIndex targets rightIndex
              !nextStepAcc = stepAcc + 1
           in case compare leftValue rightValue of
                LT ->
                  countIntersection
                    nextEdgeIndex
                    triangleAcc
                    nextStepAcc
                    (leftIndex + 1)
                    leftEnd
                    rightIndex
                    rightEnd
                EQ ->
                  countIntersection
                    nextEdgeIndex
                    (triangleAcc + 1)
                    nextStepAcc
                    (leftIndex + 1)
                    leftEnd
                    (rightIndex + 1)
                    rightEnd
                GT ->
                  countIntersection
                    nextEdgeIndex
                    triangleAcc
                    nextStepAcc
                    leftIndex
                    leftEnd
                    (rightIndex + 1)
                    rightEnd

data TriangleAccum = TriangleAccum
  { triangleAccumTriangles :: {-# UNPACK #-} !Int,
    triangleAccumSteps :: {-# UNPACK #-} !Int
  }

emptyTriangleAccum :: TriangleAccum
emptyTriangleAccum =
  TriangleAccum
    { triangleAccumTriangles = 0,
      triangleAccumSteps = 0
    }

triangleAccumToCount :: TriangleAccum -> TriangleCount
triangleAccumToCount accum =
  TriangleCount
    { tcTriangles = triangleAccumTriangles accum,
      tcIntersectionSteps = triangleAccumSteps accum
    }

countTrianglesBitset :: DenseTriangleTrie -> TriangleCount
countTrianglesBitset trie =
  triangleAccumToCount
    (VU.ifoldl' countEdge emptyTriangleAccum (dttOrientedSources trie))
  where
    !orientedTargets =
      dttOrientedTargets trie

    countEdge accum edgeIndex leftVertex =
      let !rightVertex = VU.unsafeIndex orientedTargets edgeIndex
       in countBitsetIntersection trie leftVertex rightVertex accum

countBitsetIntersection :: DenseTriangleTrie -> Int -> Int -> TriangleAccum -> TriangleAccum
countBitsetIntersection trie leftVertex rightVertex initial =
  VU.foldl' step initial wordOffsets
  where
    !wordCount =
      dttBitWordCount trie
    !leftBase =
      leftVertex * wordCount
    !rightBase =
      rightVertex * wordCount
    !bits =
      dttAdjacencyBits trie
    !startWord =
      max
        (VU.unsafeIndex (dttFirstNonzeroBitWords trie) leftVertex)
        (VU.unsafeIndex (dttFirstNonzeroBitWords trie) rightVertex)
    !endWord =
      min
        (VU.unsafeIndex (dttLastNonzeroBitWords trie) leftVertex)
        (VU.unsafeIndex (dttLastNonzeroBitWords trie) rightVertex)
    !wordOffsets =
      VU.slice startWord (max 0 (endWord - startWord)) (dttBitWordOffsets trie)

    step accum wordOffset =
      let !intersectionWord =
            VU.unsafeIndex bits (leftBase + wordOffset)
              .&. VU.unsafeIndex bits (rightBase + wordOffset)
       in TriangleAccum
            { triangleAccumTriangles = triangleAccumTriangles accum + popCount intersectionWord,
              triangleAccumSteps = triangleAccumSteps accum + 1
            }

triangleBenchmarkStats :: DenseTriangleTrie -> TriangleBenchmarkStats
triangleBenchmarkStats trie =
  let countValue = countTrianglesWCOJ trie
      boundValue = agmTriangleBound (dttEdgeCount trie)
      workRatio =
        if boundValue <= 0.0
          then 0.0
          else fromIntegral (tcIntersectionSteps countValue) / boundValue
   in TriangleBenchmarkStats
        { tbsEdges = dttEdgeCount trie,
          tbsVertices = dttVertexCount trie,
          tbsTriangles = tcTriangles countValue,
          tbsIntersectionSteps = tcIntersectionSteps countValue,
          tbsAgmBound = boundValue,
          tbsWorkToAgm = workRatio
        }

agmTriangleBound :: Int -> Double
agmTriangleBound edgeCount =
  fromIntegral (max 0 edgeCount) ** (1.5 :: Double)

normalizeUndirectedEdges :: [(Int, Int)] -> [(Int, Int)]
normalizeUndirectedEdges =
  reverse . snd . foldl' step (HashSet.empty, [])
  where
    step ::
      (HashSet (Int, Int), [(Int, Int)]) ->
      (Int, Int) ->
      (HashSet (Int, Int), [(Int, Int)])
    step (!seen, !acc) rawEdge =
      case normalizeUndirectedEdge rawEdge of
        Nothing ->
          (seen, acc)
        Just edgeValue
          | HashSet.member edgeValue seen ->
              (seen, acc)
          | otherwise ->
              (HashSet.insert edgeValue seen, edgeValue : acc)

-- | Canonicalize a raw graph edge for the triangle kernel.
--
-- Negative endpoints and loops are rejected; surviving undirected edges are
-- represented by ascending endpoint order.
normalizeUndirectedEdge :: (Int, Int) -> Maybe (Int, Int)
normalizeUndirectedEdge (leftVertex, rightVertex)
  | leftVertex < 0 || rightVertex < 0 =
      Nothing
  | leftVertex == rightVertex =
      Nothing
  | otherwise =
      Just (min leftVertex rightVertex, max leftVertex rightVertex)

vertexCountFromEdges :: [(Int, Int)] -> Int
vertexCountFromEdges edges =
  let maxVertex =
        foldl'
          ( \currentMax (leftVertex, rightVertex) ->
              max currentMax (max leftVertex rightVertex)
          )
          (-1)
          edges
   in if maxVertex < 0
        then 0
        else maxVertex + 1

degreeVector :: Int -> [(Int, Int)] -> VU.Vector Int
degreeVector vertexCount edges =
  VU.generate vertexCount degreeAt
  where
    counts =
      foldl'
        ( \acc (leftVertex, rightVertex) ->
            IntMap.insertWith (+) leftVertex 1 $
              IntMap.insertWith (+) rightVertex 1 acc
        )
        IntMap.empty
        edges

    degreeAt vertex =
      IntMap.findWithDefault 0 vertex counts

orientByDegree :: VU.Vector Int -> (Int, Int) -> (Int, Int)
orientByDegree degrees edge@(leftVertex, rightVertex) =
  if degreeKey leftVertex <= degreeKey rightVertex
    then edge
    else swap edge
  where
    degreeKey vertex =
      (degreeAt vertex, vertex)

    degreeAt vertex
      | vertex < 0 =
          0
      | vertex >= VU.length degrees =
          0
      | otherwise =
          VU.unsafeIndex degrees vertex

orientedEdgeVectors :: [(Int, Int)] -> (VU.Vector Int, VU.Vector Int)
orientedEdgeVectors orientedEdges =
  ( VU.fromList (fmap fst orientedEdges),
    VU.fromList (fmap snd orientedEdges)
  )

buildOffsetTargetVectors ::
  Int ->
  IntMap [Int] ->
  (VU.Vector Int, VU.Vector Int)
buildOffsetTargetVectors vertexCount adjacency =
  (VU.fromList offsets, VU.fromList targets)
  where
    chunks =
      [ List.sort (IntMap.findWithDefault [] vertex adjacency)
      | vertex <- [0 .. vertexCount - 1]
      ]

    offsets =
      List.scanl' (+) 0 (fmap length chunks)

    targets =
      concat chunks

neighborBounds :: DenseTriangleTrie -> Int -> (Int, Int)
neighborBounds trie vertex
  | vertex < 0 =
      (0, 0)
  | vertex + 1 >= VU.length (dttOffsets trie) =
      (0, 0)
  | otherwise =
      ( VU.unsafeIndex (dttOffsets trie) vertex,
        VU.unsafeIndex (dttOffsets trie) (vertex + 1)
      )

wordCountForVertices :: Int -> Int
wordCountForVertices vertexCount
  | vertexCount <= 0 =
      0
  | otherwise =
      ((vertexCount - 1) `quot` wordBits) + 1

wordBits :: Int
wordBits =
  64

orientedAdjacencyBitVector :: Int -> Int -> [(Int, Int)] -> VU.Vector Word64
orientedAdjacencyBitVector vertexCount wordCount orientedEdges =
  VU.accum
    (.|.)
    (VU.replicate (vertexCount * wordCount) 0)
    (fmap (orientedEdgeBit wordCount) orientedEdges)

orientedEdgeBit :: Int -> (Int, Int) -> (Int, Word64)
orientedEdgeBit wordCount (sourceVertex, targetVertex) =
  let !wordOffset =
        targetVertex `quot` wordBits
      !bitOffset =
        targetVertex - (wordOffset * wordBits)
      !wordIndex =
        (sourceVertex * wordCount) + wordOffset
   in (wordIndex, (1 :: Word64) `shiftL` bitOffset)

adjacencyBitFirstWords :: Int -> Int -> VU.Vector Int -> VU.Vector Word64 -> VU.Vector Int
adjacencyBitFirstWords vertexCount wordCount wordOffsets bits =
  VU.generate vertexCount (firstNonzeroAdjacencyWord wordCount wordOffsets bits)

firstNonzeroAdjacencyWord :: Int -> VU.Vector Int -> VU.Vector Word64 -> Int -> Int
firstNonzeroAdjacencyWord wordCount wordOffsets bits vertex =
  VU.foldl' step wordCount wordOffsets
  where
    !base =
      vertex * wordCount

    step best wordOffset
      | best < wordCount =
          best
      | VU.unsafeIndex bits (base + wordOffset) == 0 =
          best
      | otherwise =
          wordOffset

adjacencyBitLastWords :: Int -> Int -> VU.Vector Int -> VU.Vector Word64 -> VU.Vector Int
adjacencyBitLastWords vertexCount wordCount wordOffsets bits =
  VU.generate vertexCount (lastNonzeroAdjacencyWord wordCount wordOffsets bits)

lastNonzeroAdjacencyWord :: Int -> VU.Vector Int -> VU.Vector Word64 -> Int -> Int
lastNonzeroAdjacencyWord wordCount wordOffsets bits vertex =
  VU.foldl' step 0 wordOffsets
  where
    !base =
      vertex * wordCount

    step lastWord wordOffset
      | VU.unsafeIndex bits (base + wordOffset) == 0 =
          lastWord
      | otherwise =
          wordOffset + 1

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- | Dense-id arrow index; 'arrowIndexVertices' is caller-declared vocabulary,
-- not an enforced closure over edge endpoints — doors whose semantics require
-- endpoint membership validate it themselves.
module Moonlight.Differential.Index.Arrow
  ( ArrowIndex,
    ArrowIndexError (..),
    IndexedArrow (..),
    emptyArrowIndex,
    buildArrowIndex,
    appendArrows,
    buildIndexedArrowIndex,
    appendIndexedArrows,
    replaceIndexedArrows,
    arrowIndexVertices,
    arrowIndexCount,
    arrowIndexIds,
    arrowIndexEntries,
    lookupArrow,
    indexedArrowAt,
    arrowsForIds,
    arrowsFrom,
    arrowsTo,
    arrowsAlong,
    arrowEndpoint,
    arrowEndpointMap,
    arrowIdsBySource,
    arrowIdsByTarget,
    arrowIdsByEndpointPair,
  )
where

import Control.Monad
  ( foldM,
  )
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Vector (Vector)
import Data.Vector qualified as Vector

type ArrowIndexError :: Type
data ArrowIndexError
  = ArrowIndexNonDenseId !Int !Int
  | ArrowIndexDuplicateId !Int
  | ArrowIndexUnknownId !Int
  deriving stock (Eq, Show)

type IndexedArrow :: Type -> Type -> Type
data IndexedArrow endpoint edge = IndexedArrow
  { iaId :: !Int,
    iaSource :: !endpoint,
    iaTarget :: !endpoint,
    iaEdge :: !edge
  }
  deriving stock (Eq, Show)

type ArrowIndex :: Type -> Type -> Type
data ArrowIndex endpoint edge = ArrowIndex
  { aiVertices :: !(Set endpoint),
    aiEdges :: !(Vector edge),
    aiEndpoints :: !(IntMap (endpoint, endpoint)),
    aiBySource :: !(Map endpoint IntSet),
    aiByTarget :: !(Map endpoint IntSet),
    aiByEndpointPair :: !(Map (endpoint, endpoint) IntSet)
  }
  deriving stock (Eq, Show)

emptyArrowIndex :: Set endpoint -> ArrowIndex endpoint edge
emptyArrowIndex vertices =
  ArrowIndex
    { aiVertices = vertices,
      aiEdges = Vector.empty,
      aiEndpoints = IntMap.empty,
      aiBySource = Map.empty,
      aiByTarget = Map.empty,
      aiByEndpointPair = Map.empty
    }

buildArrowIndex :: Ord endpoint => Set endpoint -> (edge -> endpoint) -> (edge -> endpoint) -> [edge] -> ArrowIndex endpoint edge
buildArrowIndex vertices sourceOf targetOf edges =
  buildDenseArrowIndex vertices (zipWith indexedArrow [0 :: Int ..] edges)
  where
    indexedArrow arrowId edge =
      IndexedArrow
        { iaId = arrowId,
          iaSource = sourceOf edge,
          iaTarget = targetOf edge,
          iaEdge = edge
        }

appendArrows :: Ord endpoint => (edge -> endpoint) -> (edge -> endpoint) -> [edge] -> ArrowIndex endpoint edge -> ArrowIndex endpoint edge
appendArrows sourceOf targetOf edges indexValue =
  appendDenseArrows (zipWith indexedArrow [arrowIndexCount indexValue :: Int ..] edges) indexValue
  where
    indexedArrow arrowId edge =
      IndexedArrow
        { iaId = arrowId,
          iaSource = sourceOf edge,
          iaTarget = targetOf edge,
          iaEdge = edge
        }

buildIndexedArrowIndex :: Ord endpoint => Set endpoint -> [IndexedArrow endpoint edge] -> Either ArrowIndexError (ArrowIndex endpoint edge)
buildIndexedArrowIndex vertices indexedArrows = do
  validateSequentialIds 0 indexedArrows
  pure (buildDenseArrowIndex vertices indexedArrows)

appendIndexedArrows :: Ord endpoint => [IndexedArrow endpoint edge] -> ArrowIndex endpoint edge -> Either ArrowIndexError (ArrowIndex endpoint edge)
appendIndexedArrows indexedArrows indexValue = do
  validateSequentialIds (arrowIndexCount indexValue) indexedArrows
  pure (appendDenseArrows indexedArrows indexValue)

replaceIndexedArrows :: Ord endpoint => [IndexedArrow endpoint edge] -> ArrowIndex endpoint edge -> Either ArrowIndexError (ArrowIndex endpoint edge)
replaceIndexedArrows indexedArrows indexValue = do
  validateReplacementIds indexValue indexedArrows
  oldArrows <- traverse (`indexedArrowAtOrError` indexValue) (fmap iaId indexedArrows)
  pure
    ( List.foldl'
        insertIndexedArrow
        ( List.foldl'
            removeIndexedArrow
            indexValue {aiEdges = aiEdges indexValue Vector.// fmap edgeUpdate indexedArrows}
            oldArrows
        )
        indexedArrows
    )
  where
    edgeUpdate :: IndexedArrow endpoint edge -> (Int, edge)
    edgeUpdate indexedArrow =
      (iaId indexedArrow, iaEdge indexedArrow)

arrowIndexVertices :: ArrowIndex endpoint edge -> Set endpoint
arrowIndexVertices =
  aiVertices

arrowIndexCount :: ArrowIndex endpoint edge -> Int
arrowIndexCount =
  Vector.length . aiEdges

arrowIndexIds :: ArrowIndex endpoint edge -> [Int]
arrowIndexIds indexValue =
  [0 .. arrowIndexCount indexValue - 1]

arrowIndexEntries :: ArrowIndex endpoint edge -> [edge]
arrowIndexEntries =
  Vector.toList . aiEdges

lookupArrow :: Int -> ArrowIndex endpoint edge -> Maybe edge
lookupArrow arrowId indexValue =
  if arrowId < 0
    then Nothing
    else aiEdges indexValue Vector.!? arrowId

indexedArrowAt :: Int -> ArrowIndex endpoint edge -> Maybe (IndexedArrow endpoint edge)
indexedArrowAt arrowId indexValue = do
  edge <- lookupArrow arrowId indexValue
  (sourceKey, targetKey) <- IntMap.lookup arrowId (aiEndpoints indexValue)
  pure
    IndexedArrow
      { iaId = arrowId,
        iaSource = sourceKey,
        iaTarget = targetKey,
        iaEdge = edge
      }

arrowsForIds :: IntSet -> ArrowIndex endpoint edge -> [edge]
arrowsForIds ids indexValue =
  mapMaybe (`lookupArrow` indexValue) (IntSet.toAscList ids)

arrowsFrom :: Ord endpoint => endpoint -> ArrowIndex endpoint edge -> [edge]
arrowsFrom sourceKey indexValue =
  arrowsForIds (Map.findWithDefault IntSet.empty sourceKey (aiBySource indexValue)) indexValue

arrowsTo :: Ord endpoint => endpoint -> ArrowIndex endpoint edge -> [edge]
arrowsTo targetKey indexValue =
  arrowsForIds (Map.findWithDefault IntSet.empty targetKey (aiByTarget indexValue)) indexValue

arrowsAlong :: Ord endpoint => endpoint -> endpoint -> ArrowIndex endpoint edge -> [edge]
arrowsAlong sourceKey targetKey indexValue =
  arrowsForIds (Map.findWithDefault IntSet.empty (sourceKey, targetKey) (aiByEndpointPair indexValue)) indexValue

arrowEndpoint :: Int -> ArrowIndex endpoint edge -> Maybe (endpoint, endpoint)
arrowEndpoint arrowId =
  IntMap.lookup arrowId . aiEndpoints

arrowEndpointMap :: ArrowIndex endpoint edge -> IntMap (endpoint, endpoint)
arrowEndpointMap =
  aiEndpoints

arrowIdsBySource :: ArrowIndex endpoint edge -> Map endpoint IntSet
arrowIdsBySource =
  aiBySource

arrowIdsByTarget :: ArrowIndex endpoint edge -> Map endpoint IntSet
arrowIdsByTarget =
  aiByTarget

arrowIdsByEndpointPair :: ArrowIndex endpoint edge -> Map (endpoint, endpoint) IntSet
arrowIdsByEndpointPair =
  aiByEndpointPair

buildDenseArrowIndex :: Ord endpoint => Set endpoint -> [IndexedArrow endpoint edge] -> ArrowIndex endpoint edge
buildDenseArrowIndex vertices indexedArrows =
  List.foldl'
    insertIndexedArrow
    (emptyArrowIndex vertices) {aiEdges = Vector.fromList (fmap iaEdge indexedArrows)}
    indexedArrows

appendDenseArrows :: Ord endpoint => [IndexedArrow endpoint edge] -> ArrowIndex endpoint edge -> ArrowIndex endpoint edge
appendDenseArrows indexedArrows indexValue =
  List.foldl'
    insertIndexedArrow
    indexValue {aiEdges = aiEdges indexValue <> Vector.fromList (fmap iaEdge indexedArrows)}
    indexedArrows

indexedArrowAtOrError :: Int -> ArrowIndex endpoint edge -> Either ArrowIndexError (IndexedArrow endpoint edge)
indexedArrowAtOrError arrowId indexValue =
  case indexedArrowAt arrowId indexValue of
    Just indexedArrow -> Right indexedArrow
    Nothing -> Left (ArrowIndexUnknownId arrowId)

insertIndexedArrow :: Ord endpoint => ArrowIndex endpoint edge -> IndexedArrow endpoint edge -> ArrowIndex endpoint edge
insertIndexedArrow indexValue arrow =
  alterArrowBuckets IntSet.insert arrow $
    indexValue {aiEndpoints = IntMap.insert (iaId arrow) (iaSource arrow, iaTarget arrow) (aiEndpoints indexValue)}

removeIndexedArrow :: Ord endpoint => ArrowIndex endpoint edge -> IndexedArrow endpoint edge -> ArrowIndex endpoint edge
removeIndexedArrow indexValue arrow =
  alterArrowBuckets IntSet.delete arrow $
    indexValue {aiEndpoints = IntMap.delete (iaId arrow) (aiEndpoints indexValue)}

alterArrowBuckets :: Ord endpoint => (Int -> IntSet -> IntSet) -> IndexedArrow endpoint edge -> ArrowIndex endpoint edge -> ArrowIndex endpoint edge
alterArrowBuckets update arrow indexValue =
  indexValue
    { aiBySource = alterBucket update (iaSource arrow) (iaId arrow) (aiBySource indexValue),
      aiByTarget = alterBucket update (iaTarget arrow) (iaId arrow) (aiByTarget indexValue),
      aiByEndpointPair = alterBucket update (iaSource arrow, iaTarget arrow) (iaId arrow) (aiByEndpointPair indexValue)
    }

alterBucket :: Ord key => (Int -> IntSet -> IntSet) -> key -> Int -> Map key IntSet -> Map key IntSet
alterBucket update key arrowId =
  Map.alter (nonEmpty . update arrowId . maybe IntSet.empty id) key

nonEmpty :: IntSet -> Maybe IntSet
nonEmpty ids
  | IntSet.null ids = Nothing
  | otherwise = Just ids

validateSequentialIds :: Int -> [IndexedArrow endpoint edge] -> Either ArrowIndexError ()
validateSequentialIds startId =
  fmap (const ()) . foldM step startId
  where
    step :: Int -> IndexedArrow endpoint edge -> Either ArrowIndexError Int
    step expectedId indexedArrow =
      if iaId indexedArrow == expectedId
        then Right (expectedId + 1)
        else Left (ArrowIndexNonDenseId expectedId (iaId indexedArrow))

validateReplacementIds :: ArrowIndex endpoint edge -> [IndexedArrow endpoint edge] -> Either ArrowIndexError ()
validateReplacementIds indexValue =
  fmap (const ()) . foldM step IntSet.empty
  where
    step seenIds indexedArrow =
      let arrowId = iaId indexedArrow
       in if IntSet.member arrowId seenIds
            then Left (ArrowIndexDuplicateId arrowId)
            else
              if arrowId < 0 || arrowId >= arrowIndexCount indexValue
                then Left (ArrowIndexUnknownId arrowId)
                else Right (IntSet.insert arrowId seenIds)

{-# LANGUAGE DerivingStrategies #-}

-- | Greedy min-degree tree-decomposition planner for cyclic conjunctive
-- queries.
--
-- 'findMinDegreeTreeDecomp' compiles a 'DecompPlan' suitable for factorized
-- marginal evaluation by running min-degree variable elimination on the
-- query's primal graph. Each elimination step contributes one bag: the
-- eliminated slot together with its current neighbourhood. Bag parents are
-- the earliest later bag whose eliminated slot appears in the current bag;
-- separators are set intersections.
--
-- This is a bounded primal-graph heuristic. It is not a fractional-hypertree
-- planner, it is not a fractional-width decision procedure, and it does not
-- estimate AGM or fractional-edge-cover cost. Scientific authority lives in a
-- real hypergraph cost model; this module is merely a cheap candidate that
-- callers may reject and fall back from.
module Moonlight.Flow.Plan.Physical.Planner
  ( findMinDegreeTreeDecomp,
    maxAcceptableBagWidth,
    minDegreeEliminationOrder,
    primalGraph,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Flow.Plan.Query.Core

-- | Maximum acceptable bag size.  Decompositions wider than this are
-- rejected; callers should fall back to 'ExactJoin'.  Empirically, most
-- pattern queries in e-graph rewriting fit within a handful of slots per
-- bag; this threshold keeps us from committing to wildly expensive
-- factorized joins.
maxAcceptableBagWidth :: Int
maxAcceptableBagWidth = 8

findMinDegreeTreeDecomp ::
  IntMap [SlotId] ->
  [SlotId] ->
  Maybe DecompPlan
findMinDegreeTreeDecomp atomSchemas _fullSchema
  | IntMap.null atomSchemas =
      Nothing
  | otherwise =
      let graph = primalGraph atomSchemas
          order = minDegreeEliminationOrder graph
          bags = buildBags order graph
          maxSize =
            case bags of
              [] -> 0
              _ -> maximum (fmap (IntSet.size . snd) bags)
       in if null bags || maxSize > maxAcceptableBagWidth
            then Nothing
            else case assignAtoms atomSchemas bags of
              Nothing -> Nothing
              Just owners -> Just (buildDecompPlan bags owners)

-- ---------------------------------------------------------------------------
-- Primal graph and elimination.
-- ---------------------------------------------------------------------------

-- | Primal graph: vertices are slot ids, edges connect slots that co-occur
-- in some atom.
primalGraph :: IntMap [SlotId] -> IntMap IntSet
primalGraph atomSchemas =
  foldl' addAtomClique IntMap.empty (IntMap.elems atomSchemas)
  where
    addAtomClique :: IntMap IntSet -> [SlotId] -> IntMap IntSet
    addAtomClique graph slots =
      let keys = fmap slotIdKey slots
          graph1 = foldl' ensureVertex graph keys
          edges = [(a, b) | a <- keys, b <- keys, a /= b]
       in foldl' addEdge graph1 edges

    ensureVertex :: IntMap IntSet -> Int -> IntMap IntSet
    ensureVertex g s = IntMap.insertWith IntSet.union s IntSet.empty g

    addEdge :: IntMap IntSet -> (Int, Int) -> IntMap IntSet
    addEdge g (a, b) = IntMap.insertWith IntSet.union a (IntSet.singleton b) g

-- | Greedy min-degree elimination order.  At each step, picks the vertex
-- with the fewest remaining neighbours; ties broken by vertex id.
minDegreeEliminationOrder :: IntMap IntSet -> [Int]
minDegreeEliminationOrder = go
  where
    go :: IntMap IntSet -> [Int]
    go g
      | IntMap.null g = []
      | otherwise =
          let ranked =
                sortOn
                  (\(vertex, nbrs) -> (IntSet.size nbrs, vertex))
                  (IntMap.toList g)
           in case ranked of
                [] -> []
                ((v, _) : _) -> v : go (eliminate v g)

-- | Remove vertex @v@ from the graph, filling in edges between every pair
-- of its current neighbours (the clique induced by elimination).
eliminate :: Int -> IntMap IntSet -> IntMap IntSet
eliminate v g =
  let nbrs = IntMap.findWithDefault IntSet.empty v g

      fillIn =
        IntSet.foldl'
          ( \acc a ->
              IntSet.foldl'
                ( \acc' b ->
                    if a == b
                      then acc'
                      else IntMap.insertWith IntSet.union a (IntSet.singleton b) acc'
                )
                acc
                nbrs
          )
          g
          nbrs

      dropV =
        IntSet.foldl'
          (\acc nbr -> IntMap.adjust (IntSet.delete v) nbr acc)
          fillIn
          nbrs
   in IntMap.delete v dropV

-- | Run elimination, recording (eliminated slot, bag content) per step.
-- The bag at step @i@ is @{s_i} \\cup neighbours(s_i)@ at the time @s_i@ is
-- eliminated.
buildBags :: [Int] -> IntMap IntSet -> [(Int, IntSet)]
buildBags [] _ = []
buildBags (v : rest) g =
  let nbrs = IntMap.findWithDefault IntSet.empty v g
      bag = IntSet.insert v nbrs
   in (v, bag) : buildBags rest (eliminate v g)

-- ---------------------------------------------------------------------------
-- Atom assignment and decomp construction.
-- ---------------------------------------------------------------------------

-- | Assign each atom to the earliest bag index whose eliminated slot is in
-- the atom's schema AND whose slot set covers the schema.  This rule gives
-- non-trivial coverage for cyclic fragments (atoms land near where they
-- first contribute to the elimination) while respecting
-- running-intersection.
assignAtoms ::
  IntMap [SlotId] ->
  [(Int, IntSet)] ->
  Maybe (IntMap Int)
assignAtoms atomSchemas bags =
  IntMap.traverseWithKey (\_ slots -> findOwner slots) atomSchemas
  where
    indexed :: [(Int, (Int, IntSet))]
    indexed = zip [0 ..] bags

    findOwner :: [SlotId] -> Maybe Int
    findOwner slots =
      let slotInts = IntSet.fromList (fmap slotIdKey slots)
          candidates =
            [ idx
              | (idx, (elimSlot, bagSet)) <- indexed,
                IntSet.member elimSlot slotInts,
                slotInts `IntSet.isSubsetOf` bagSet
            ]
       in case candidates of
            (c : _) -> Just c
            [] ->
              -- Fallback: any bag that covers the atom, regardless of which
              -- slot was eliminated there.
              case [ idx
                     | (idx, (_, bagSet)) <- indexed,
                       slotInts `IntSet.isSubsetOf` bagSet
                   ] of
                (c : _) -> Just c
                [] -> Nothing

buildDecompPlan ::
  [(Int, IntSet)] ->
  IntMap Int ->
  DecompPlan
buildDecompPlan bags owners =
  let indexed :: [(Int, (Int, IntSet))]
      indexed = zip [0 ..] bags

      bagMap :: IntMap (Int, IntSet)
      bagMap = IntMap.fromList indexed

      baseBagCount :: Int
      baseBagCount = length bags

      parentOfBaseBag :: Int -> Maybe Int
      parentOfBaseBag i =
        case IntMap.lookup i bagMap of
          Nothing -> Nothing
          Just (_, bag_i) ->
            case
              [ j
                | j <- [i + 1 .. baseBagCount - 1],
                  Just (elimJ, _) <- [IntMap.lookup j bagMap],
                  IntSet.member elimJ bag_i
              ]
            of
              parentKey : _ -> Just parentKey
              [] -> Nothing

      baseParentMap :: IntMap BagId
      baseParentMap =
        IntMap.fromList
          [ (i, BagId parentKey)
            | i <- [0 .. baseBagCount - 1],
              Just parentKey <- [parentOfBaseBag i]
          ]

      baseRoots :: [Int]
      baseRoots =
        [ i
          | i <- [0 .. baseBagCount - 1],
            IntMap.notMember i baseParentMap
        ]

      useSuperRoot :: Bool
      useSuperRoot =
        length baseRoots > 1

      superRootKey :: Int
      superRootKey =
        baseBagCount

      rootBag :: BagId
      rootBag =
        case (useSuperRoot, baseRoots) of
          (True, _) -> BagId superRootKey
          (False, rootKey : _) -> BagId rootKey
          (False, []) -> BagId 0

      parentMap :: IntMap BagId
      parentMap =
        if useSuperRoot
          then
            foldl'
              ( \acc childRoot ->
                  IntMap.insert childRoot (BagId superRootKey) acc
              )
              baseParentMap
              baseRoots
          else baseParentMap

      childrenMap :: IntMap [BagId]
      childrenMap =
        IntMap.foldlWithKey'
          ( \acc childKey (BagId parentKey) ->
              IntMap.insertWith (<>) parentKey [BagId childKey] acc
          )
          IntMap.empty
          parentMap

      baseSeparators :: Map (BagId, BagId) [SlotId]
      baseSeparators =
        Map.fromList
          [ ( (BagId i, BagId parentKey),
              fmap mkSlotId (IntSet.toAscList (IntSet.intersection bag_i bag_parent))
            )
            | i <- [0 .. baseBagCount - 1],
              Just parentKey <- [parentOfBaseBag i],
              Just (_, bag_i) <- [IntMap.lookup i bagMap],
              Just (_, bag_parent) <- [IntMap.lookup parentKey bagMap]
          ]

      superSeparators :: Map (BagId, BagId) [SlotId]
      superSeparators =
        if useSuperRoot
          then
            Map.fromList
              [ ((BagId childRoot, BagId superRootKey), [])
                | childRoot <- baseRoots
              ]
          else Map.empty

      separators :: Map (BagId, BagId) [SlotId]
      separators =
        Map.union baseSeparators superSeparators

      atomsInBag :: Int -> IntSet
      atomsInBag bagIdx =
        IntMap.foldlWithKey'
          ( \acc atomKey owner ->
              if owner == bagIdx
                then IntSet.insert atomKey acc
                else acc
          )
          IntSet.empty
          owners

      baseDecompBags :: IntMap DecompBag
      baseDecompBags =
        IntMap.fromList
          [ ( i,
              mkDecompBag (BagId i) (fmap mkSlotId (IntSet.toAscList bag_i)) (atomsInBag i)
            )
            | (i, (_, bag_i)) <- indexed
          ]

      decompBags :: IntMap DecompBag
      decompBags =
        if useSuperRoot
          then
            IntMap.insert
              superRootKey
              (mkDecompBag (BagId superRootKey) [] IntSet.empty)
              baseDecompBags
          else baseDecompBags

      atomOwnerMap :: IntMap BagId
      atomOwnerMap =
        fmap BagId owners
   in mkDecompPlan
        rootBag
        decompBags
        parentMap
        childrenMap
        separators
        atomOwnerMap

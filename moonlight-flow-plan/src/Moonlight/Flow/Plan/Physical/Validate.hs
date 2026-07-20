{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Plan.Physical.Validate
  ( PlanError (..),
    validateAtomSchemas,
    validateJoinForest,
    validateDecompPlan,
    validateJoinMeta,
  )
where

import Algebra.Graph.AdjacencyIntMap qualified as AdjacencyIntMap
import Algebra.Graph.AdjacencyIntMap.Algorithm qualified as AdjacencyIntMapAlgorithm
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Moonlight.Core
  ( duplicateValuesOn,
  )
import Moonlight.Flow.Plan.Query.Core

data PlanError
  = DuplicateAtomSlot !Int !SlotId
  | JoinForestRootMissing !AtomId
  | JoinForestRootHasParent !AtomId
  | JoinForestParentMissingChild !Int
  | JoinForestParentMissingParent !Int !AtomId
  | JoinForestChildrenMismatch !(IntMap IntSet) !(IntMap IntSet)
  | JoinForestSeparatorMissing !AtomId !AtomId
  | JoinForestSeparatorMismatch !AtomId !AtomId ![SlotId] ![SlotId]
  | JoinForestExtraSeparator !AtomId !AtomId
  | JoinForestUnreachable !IntSet
  | DecompRootMissing !BagId
  | DecompRootHasParent !BagId
  | DecompBagKeyMismatch !Int !BagId
  | DecompDuplicateBagSlot !BagId !SlotId
  | DecompParentMissingChild !Int
  | DecompParentMissingParent !Int !BagId
  | DecompChildrenMismatch !(IntMap IntSet) !(IntMap IntSet)
  | DecompSeparatorMissing !BagId !BagId
  | DecompSeparatorMismatch !BagId !BagId ![SlotId] ![SlotId]
  | DecompExtraSeparator !BagId !BagId
  | DecompUnreachable !IntSet
  | DecompAtomOwnerMissing !Int
  | DecompAtomOwnerUnknownAtom !Int
  | DecompAtomOwnerMissingBag !Int !BagId
  | DecompAtomNotCovered !Int !BagId
  | DecompBagAtomOwnerMismatch !Int !BagId !(Maybe BagId)
  | DecompOwnerNotListedInBag !Int !BagId
  | DecompSlotDisconnected !SlotId !IntSet
  deriving stock (Eq, Ord, Show)

finish :: [PlanError] -> Either [PlanError] ()
finish [] = Right ()
finish errors = Left errors

slotSet :: [SlotId] -> IntSet
slotSet =
  IntSet.fromList . fmap slotIdKey

duplicateSlots :: [SlotId] -> [SlotId]
duplicateSlots =
  fmap mkSlotId
    . IntSet.toAscList
    . IntSet.fromList
    . fmap (slotIdKey . snd)
    . duplicateValuesOn slotIdKey

validateAtomSchemas :: IntMap [SlotId] -> [PlanError]
validateAtomSchemas atomSchemas =
  [ DuplicateAtomSlot atomKey dup
    | (atomKey, slots) <- IntMap.toList atomSchemas,
      dup <- duplicateSlots slots
  ]

canonicalSeparator :: [SlotId] -> [SlotId] -> [SlotId]
canonicalSeparator left right =
  fmap mkSlotId
    . IntSet.toAscList
    $ IntSet.intersection (slotSet left) (slotSet right)

normalizeAtomChildren :: IntMap [AtomId] -> IntMap IntSet
normalizeAtomChildren =
  IntMap.mapMaybe nonEmpty
    . IntMap.map (IntSet.fromList . fmap atomIdKey)
  where
    nonEmpty children
      | IntSet.null children = Nothing
      | otherwise = Just children

normalizeBagChildren :: IntMap [BagId] -> IntMap IntSet
normalizeBagChildren =
  IntMap.mapMaybe nonEmpty
    . IntMap.map (IntSet.fromList . fmap unBagId)
  where
    nonEmpty children
      | IntSet.null children = Nothing
      | otherwise = Just children

expectedChildrenFromParents :: IntMap Int -> IntMap IntSet
expectedChildrenFromParents =
  IntMap.fromListWith IntSet.union
    . fmap
      ( \(childKey, parentKey) ->
          (parentKey, IntSet.singleton childKey)
      )
    . IntMap.toAscList

reachableFromChildren :: Int -> IntMap IntSet -> IntSet
reachableFromChildren root children =
  reachableFromIntAdjacency (IntSet.singleton root) children root

data RootedTreeSpec err = RootedTreeSpec
  { rtsAllKeys :: !IntSet,
    rtsRootKey :: !Int,
    rtsParentMap :: !(IntMap Int),
    rtsChildrenMap :: !(IntMap IntSet),
    rtsRootMissing :: Int -> err,
    rtsRootHasParent :: Int -> err,
    rtsParentMissingChild :: Int -> err,
    rtsParentMissingParent :: Int -> Int -> err,
    rtsChildrenMismatch :: IntMap IntSet -> IntMap IntSet -> err,
    rtsUnreachable :: IntSet -> err
  }

data RootedTreeReport err = RootedTreeReport
  { rtrRootErrors :: ![err],
    rtrParentErrors :: ![err],
    rtrChildrenErrors :: ![err],
    rtrReachabilityErrors :: ![err]
  }

validateRootedTree :: RootedTreeSpec err -> RootedTreeReport err
validateRootedTree spec =
  RootedTreeReport
    { rtrRootErrors =
        [rtsRootMissing spec rootKey | IntSet.notMember rootKey allKeys]
          <> [rtsRootHasParent spec rootKey | IntMap.member rootKey parentMap],
      rtrParentErrors =
        concat
          [ [rtsParentMissingChild spec childKey | IntSet.notMember childKey allKeys]
              <> [rtsParentMissingParent spec childKey parentKey | IntSet.notMember parentKey allKeys]
          | (childKey, parentKey) <- IntMap.toList parentMap
          ],
      rtrChildrenErrors =
        [rtsChildrenMismatch spec expectedChildren actualChildren | expectedChildren /= actualChildren],
      rtrReachabilityErrors =
        [rtsUnreachable spec (IntSet.difference allKeys reachable) | reachable /= allKeys]
    }
  where
    allKeys =
      rtsAllKeys spec

    rootKey =
      rtsRootKey spec

    parentMap =
      rtsParentMap spec

    expectedChildren =
      expectedChildrenFromParents parentMap

    actualChildren =
      rtsChildrenMap spec

    reachable =
      reachableFromChildren rootKey actualChildren

validateJoinForest :: IntMap [SlotId] -> JoinForest -> Either [PlanError] ()
validateJoinForest atomSchemas forest =
  finish $
    validateAtomSchemas atomSchemas
      <> rtrRootErrors treeReport
      <> rtrParentErrors treeReport
      <> rtrChildrenErrors treeReport
      <> separatorErrors
      <> extraSeparatorErrors
      <> rtrReachabilityErrors treeReport
  where
    atomKeys :: IntSet
    atomKeys =
      IntMap.keysSet atomSchemas

    rootKey :: Int
    rootKey =
      atomIdKey (jfRoot forest)

    parentMap :: IntMap Int
    parentMap =
      fmap atomIdKey (jfParent forest)

    actualChildren :: IntMap IntSet
    actualChildren =
      normalizeAtomChildren (jfChildren forest)

    treeReport :: RootedTreeReport PlanError
    treeReport =
      validateRootedTree
        RootedTreeSpec
          { rtsAllKeys = atomKeys,
            rtsRootKey = rootKey,
            rtsParentMap = parentMap,
            rtsChildrenMap = actualChildren,
            rtsRootMissing = const (JoinForestRootMissing (jfRoot forest)),
            rtsRootHasParent = const (JoinForestRootHasParent (jfRoot forest)),
            rtsParentMissingChild = JoinForestParentMissingChild,
            rtsParentMissingParent = \childKey parentKey ->
              JoinForestParentMissingParent childKey (mkAtomId parentKey),
            rtsChildrenMismatch = JoinForestChildrenMismatch,
            rtsUnreachable = JoinForestUnreachable
          }

    separatorErrors :: [PlanError]
    separatorErrors =
      concat
        [ case (IntMap.lookup childKey atomSchemas, IntMap.lookup parentKey atomSchemas, Map.lookup (mkAtomId childKey, mkAtomId parentKey) (jfSeparator forest)) of
            (Just childSlots, Just parentSlots, Just actualSep) ->
              let expectedSep = canonicalSeparator childSlots parentSlots
               in [JoinForestSeparatorMismatch (mkAtomId childKey) (mkAtomId parentKey) expectedSep actualSep | expectedSep /= actualSep]
            (_, _, Nothing) ->
              [JoinForestSeparatorMissing (mkAtomId childKey) (mkAtomId parentKey)]
            _ ->
              []
        | (childKey, parentKey) <- IntMap.toList parentMap
        ]

    extraSeparatorErrors :: [PlanError]
    extraSeparatorErrors =
      [ JoinForestExtraSeparator child parent
        | ((child, parent), _) <- Map.toList (jfSeparator forest),
          IntMap.lookup (atomIdKey child) parentMap /= Just (atomIdKey parent)
      ]

validateDecompPlan :: IntMap [SlotId] -> DecompPlan -> Either [PlanError] ()
validateDecompPlan atomSchemas decomp =
  finish $
    validateAtomSchemas atomSchemas
      <> rtrRootErrors treeReport
      <> bagShapeErrors
      <> rtrParentErrors treeReport
      <> rtrChildrenErrors treeReport
      <> separatorErrors
      <> extraSeparatorErrors
      <> rtrReachabilityErrors treeReport
      <> ownerErrors
      <> bagAtomMirrorErrors
      <> runningIntersectionErrors
  where
    bagKeys :: IntSet
    bagKeys =
      IntMap.keysSet (dpBags decomp)

    atomKeys :: IntSet
    atomKeys =
      IntMap.keysSet atomSchemas

    rootKey :: Int
    rootKey =
      unBagId (dpRoot decomp)

    parentMap :: IntMap Int
    parentMap =
      fmap unBagId (dpParent decomp)

    actualChildren :: IntMap IntSet
    actualChildren =
      normalizeBagChildren (dpChildren decomp)

    bagShapeErrors :: [PlanError]
    bagShapeErrors =
      concat
        [ [DecompBagKeyMismatch bagKey (dbBagId bag) | unBagId (dbBagId bag) /= bagKey]
            <> [DecompDuplicateBagSlot (dbBagId bag) dup | dup <- duplicateSlots (dbSlots bag)]
        | (bagKey, bag) <- IntMap.toList (dpBags decomp)
        ]

    treeReport :: RootedTreeReport PlanError
    treeReport =
      validateRootedTree
        RootedTreeSpec
          { rtsAllKeys = bagKeys,
            rtsRootKey = rootKey,
            rtsParentMap = parentMap,
            rtsChildrenMap = actualChildren,
            rtsRootMissing = const (DecompRootMissing (dpRoot decomp)),
            rtsRootHasParent = const (DecompRootHasParent (dpRoot decomp)),
            rtsParentMissingChild = DecompParentMissingChild,
            rtsParentMissingParent = \childKey parentKey ->
              DecompParentMissingParent childKey (BagId parentKey),
            rtsChildrenMismatch = DecompChildrenMismatch,
            rtsUnreachable = DecompUnreachable
          }

    separatorErrors :: [PlanError]
    separatorErrors =
      concat
        [ case (IntMap.lookup childKey (dpBags decomp), IntMap.lookup parentKey (dpBags decomp), Map.lookup (BagId childKey, BagId parentKey) (dpSeparator decomp)) of
            (Just childBag, Just parentBag, Just actualSep) ->
              let expectedSep = canonicalSeparator (dbSlots childBag) (dbSlots parentBag)
               in [DecompSeparatorMismatch (BagId childKey) (BagId parentKey) expectedSep actualSep | expectedSep /= actualSep]
            (_, _, Nothing) ->
              [DecompSeparatorMissing (BagId childKey) (BagId parentKey)]
            _ ->
              []
        | (childKey, parentKey) <- IntMap.toList parentMap
        ]

    extraSeparatorErrors :: [PlanError]
    extraSeparatorErrors =
      [ DecompExtraSeparator child parent
        | ((child, parent), _) <- Map.toList (dpSeparator decomp),
          IntMap.lookup (unBagId child) parentMap /= Just (unBagId parent)
      ]

    ownerAtomKeys :: IntSet
    ownerAtomKeys =
      IntMap.keysSet (dpAtomOwner decomp)

    ownerErrors :: [PlanError]
    ownerErrors =
      [ DecompAtomOwnerMissing atomKey
        | atomKey <- IntSet.toAscList (IntSet.difference atomKeys ownerAtomKeys)
      ]
        <> [ DecompAtomOwnerUnknownAtom atomKey
             | atomKey <- IntSet.toAscList (IntSet.difference ownerAtomKeys atomKeys)
           ]
        <> concat
          [ case IntMap.lookup (unBagId owner) (dpBags decomp) of
              Nothing ->
                [DecompAtomOwnerMissingBag atomKey owner]
              Just bag ->
                let atomSlots = IntMap.findWithDefault [] atomKey atomSchemas
                 in [DecompAtomNotCovered atomKey owner | not (slotSet atomSlots `IntSet.isSubsetOf` slotSet (dbSlots bag))]
          | (atomKey, owner) <- IntMap.toList (dpAtomOwner decomp)
          ]

    bagAtomMirrorErrors :: [PlanError]
    bagAtomMirrorErrors =
      [ DecompBagAtomOwnerMismatch atomKey (BagId bagKey) (IntMap.lookup atomKey (dpAtomOwner decomp))
        | (bagKey, bag) <- IntMap.toList (dpBags decomp),
          atomKey <- IntSet.toAscList (dbAtoms bag),
          IntMap.lookup atomKey (dpAtomOwner decomp) /= Just (BagId bagKey)
      ]
        <> [ DecompOwnerNotListedInBag atomKey owner
             | (atomKey, owner) <- IntMap.toList (dpAtomOwner decomp),
               Just bag <- [IntMap.lookup (unBagId owner) (dpBags decomp)],
               IntSet.notMember atomKey (dbAtoms bag)
           ]

    bagsBySlot :: IntMap IntSet
    bagsBySlot =
      IntMap.fromListWith IntSet.union
        [ (slotIdKey slotIdValue, IntSet.singleton bagKey)
        | (bagKey, bag) <- IntMap.toAscList (dpBags decomp),
          slotIdValue <- dbSlots bag
        ]

    runningIntersectionErrors :: [PlanError]
    runningIntersectionErrors =
      [ DecompSlotDisconnected (mkSlotId slotKey) containers
        | (slotKey, containers) <- IntMap.toList bagsBySlot,
          not (containersConnected containers)
      ]

    containersConnected :: IntSet -> Bool
    containersConnected containers =
      case IntSet.minView containers of
        Nothing -> True
        Just (start, _) ->
          restrictedReach start containers == containers

    restrictedReach :: Int -> IntSet -> IntSet
    restrictedReach start allowed =
      reachableFromIntAdjacency allowed restrictedAdjacency start
      where
        restrictedAdjacency =
          IntMap.fromAscList
            [ (bagKey, IntSet.intersection allowed (undirectedNeighbors bagKey))
            | bagKey <- IntSet.toAscList allowed
            ]

    undirectedNeighbors :: Int -> IntSet
    undirectedNeighbors bagKey =
      let parent =
            maybe IntSet.empty (IntSet.singleton . unBagId) (IntMap.lookup bagKey (dpParent decomp))
          children =
            IntSet.fromList
              [ unBagId child
                | child <- IntMap.findWithDefault [] bagKey (dpChildren decomp)
              ]
       in IntSet.union parent children

reachableFromIntAdjacency :: IntSet -> IntMap IntSet -> Int -> IntSet
reachableFromIntAdjacency vertices adjacency start =
  let graph =
        AdjacencyIntMap.overlay
          (AdjacencyIntMap.vertices (IntSet.toAscList vertices))
          (AdjacencyIntMap.fromAdjacencyIntSets (IntMap.toAscList adjacency))
   in IntSet.insert start
        . IntSet.fromList
        $ AdjacencyIntMapAlgorithm.reachable graph start

errorsOf :: Either [PlanError] () -> [PlanError]
errorsOf (Right ()) = []
errorsOf (Left errors) = errors

validateJoinMeta :: JoinMeta -> Either [PlanError] ()
validateJoinMeta meta =
  finish $
    validateAtomSchemas (jmAtomSchemas meta)
      <> foldJoinShape
        []
        (errorsOf . validateJoinForest (jmAtomSchemas meta))
        (errorsOf . validateDecompPlan (jmAtomSchemas meta))
        (jmShape meta)

module Moonlight.Flow.Plan.Physical.Meta
  ( buildJoinMeta,
    detectAcyclicJoinForest,
    decompFromJoinForest,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Moonlight.Flow.Plan.Query.Core
import Moonlight.Flow.Plan.Physical.Validate
  ( validateDecompPlan,
    validateJoinForest,
  )
import Moonlight.Flow.Plan.Physical.Planner (findMinDegreeTreeDecomp)

buildJoinMeta :: IntMap [SlotId] -> [SlotId] -> JoinMeta
buildJoinMeta atomSchemas fullSchema =
  let atomsBySlot =
        IntMap.foldlWithKey'
          ( \acc atomKey cols ->
              foldl'
                ( \m sid ->
                    IntMap.insertWith
                      IntSet.union
                      (slotIdKey sid)
                      (IntSet.singleton atomKey)
                      m
                )
                acc
                cols
          )
          IntMap.empty
          atomSchemas
      neighborsBySlot =
        IntMap.foldl'
          ( \acc cols ->
              foldl'
                ( \m sid ->
                    let neigh =
                          IntSet.fromList
                            [ slotIdKey other
                              | other <- cols,
                                other /= sid
                            ]
                     in IntMap.insertWith IntSet.union (slotIdKey sid) neigh m
                )
                acc
                cols
          )
          IntMap.empty
          atomSchemas
      staticRank =
        IntMap.fromList
          [ (slotIdKey sid, ix)
            | (ix, sid) <- zip ([0 :: Int ..]) fullSchema
          ]
      incidence =
        fmap IntSet.size atomsBySlot
      shape =
        case detectAcyclicJoinForest atomSchemas of
          Just forest ->
            case validateJoinForest atomSchemas forest of
              Right () ->
                acyclicJoinShape forest
              Left _ ->
                fallbackShape
          Nothing ->
            fallbackShape

      fallbackShape =
        case findMinDegreeTreeDecomp atomSchemas fullSchema of
          Just decomp ->
            case validateDecompPlan atomSchemas decomp of
              Right () -> factorizedJoinShape decomp
              Left _ -> exactJoinShape
          Nothing ->
            exactJoinShape
   in mkJoinMeta
        atomSchemas
        atomsBySlot
        neighborsBySlot
        staticRank
        incidence
        shape

decompFromJoinForest ::
  JoinForest ->
  IntMap [SlotId] ->
  DecompPlan
decompFromJoinForest forest atomSchemas =
  mkDecompPlan
    rootBag
    bags
    parent
    children
    separator
    owner
  where
    rootBag =
      BagId (atomIdKey (jfRoot forest))

    bags =
      IntMap.mapWithKey
        ( \atomKey slots ->
            mkDecompBag
              (BagId atomKey)
              slots
              (IntSet.singleton atomKey)
        )
        atomSchemas

    parent =
      IntMap.map (BagId . atomIdKey) (jfParent forest)

    children =
      IntMap.map (fmap (BagId . atomIdKey)) (jfChildren forest)

    separator =
      Map.fromList
        [ ( (BagId (atomIdKey child), BagId (atomIdKey parentId)),
            sep
          )
          | ((child, parentId), sep) <- Map.toAscList (jfSeparator forest)
        ]

    owner =
      IntMap.mapWithKey
        (\atomKey _slots -> BagId atomKey)
        atomSchemas
{-# INLINE decompFromJoinForest #-}

detectAcyclicJoinForest :: IntMap [SlotId] -> Maybe JoinForest
detectAcyclicJoinForest atomSchemas0 = do
  let original :: IntMap IntSet
      original =
        fmap (IntSet.fromList . fmap slotIdKey) atomSchemas0
  (rootKey, parentKeys) <- gyoEliminate original
  let children =
        IntMap.fromListWith
          (<>)
          [ (parentKey, [mkAtomId child])
            | (child, parentKey) <- IntMap.toList parentKeys
          ]
      separators =
        Map.fromList
          [ ( (mkAtomId child, mkAtomId parentKey),
              fmap mkSlotId
                . IntSet.toAscList
                $ IntSet.intersection childSlots parentSlots
            )
            | (child, parentKey) <- IntMap.toList parentKeys,
              Just childSlots <- [IntMap.lookup child original],
              Just parentSlots <- [IntMap.lookup parentKey original]
          ]
   in Just
        ( mkJoinForest
            (mkAtomId rootKey)
            (fmap mkAtomId parentKeys)
            children
            separators
        )

gyoEliminate :: IntMap IntSet -> Maybe (Int, IntMap Int)
gyoEliminate =
  go IntMap.empty
  where
    go :: IntMap Int -> IntMap IntSet -> Maybe (Int, IntMap Int)
    go parents edges
      | IntMap.null edges = Nothing
      | IntMap.size edges == 1 = Just (fst (IntMap.findMin edges), parents)
      | otherwise =
          let occurrenceCounts =
                IntMap.foldl'
                  ( \acc slots ->
                      IntSet.foldl'
                        (\m slotKey -> IntMap.insertWith (+) slotKey (1 :: Int) m)
                        acc
                        slots
                  )
                  IntMap.empty
                  edges
              trimmed =
                fmap
                  (IntSet.filter (\slotKey -> IntMap.findWithDefault 0 slotKey occurrenceCounts > 1))
                  edges
              candidates =
                [ (IntSet.size leafSlots, leafKey, parentKey)
                  | (leafKey, leafSlots) <- IntMap.toList trimmed,
                    (parentKey, parentSlots) <- IntMap.toList trimmed,
                    leafKey /= parentKey,
                    leafSlots `IntSet.isSubsetOf` parentSlots
                ]
           in case candidates of
                [] -> Nothing
                _ ->
                  let (_, leafKey, parentKey) =
                        minimum candidates
                   in go
                        (IntMap.insert leafKey parentKey parents)
                        (IntMap.delete leafKey trimmed)

module Moonlight.Flow.Execution.Acyclic
  ( ReducedForestView (..),
    semijoinReduceForestView,
    enumerateReducedForestRows,
    reducedForestSupport,
  )
where

import Control.Monad (foldM)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Maybe
  ( fromMaybe,
    mapMaybe,
    maybeToList,
  )
import Data.Vector qualified as Vector
import Moonlight.Differential.Row.Tuple
import Moonlight.Flow.Plan.Query.Core
import Moonlight.Differential.Index.IndexedRows
  ( indexedRowsLayout,
  )
import Moonlight.Differential.Index.RowId
  ( RowId,
    rowIdInt,
  )
import Moonlight.Flow.Storage.Separator
import Moonlight.Flow.Storage.Relation
import Moonlight.Flow.Storage.Store
import Moonlight.Flow.Storage.View
import Moonlight.Differential.Index.RowIdSet
  ( emptyRowIdSet,
  )
import Moonlight.Differential.Index.RowSet
  ( rowSetFromIntSetCanonical,
    rowSetFoldl',
    rowSetIntersectionWithRowIdSet,
    rowSetIntersectsRowIdSet,
    rowSetToList,
  )

type ReducedForestView :: Type
data ReducedForestView = ReducedForestView
  { rfvStore :: !Store,
    rfvView :: !View,
    rfvSepIx :: !(Map.Map (AtomId, AtomId) SeparatorIndex)
  }

separatorIndexFor ::
  Store ->
  AtomId ->
  Relation ->
  [SlotId] ->
  SeparatorIndex
separatorIndexFor db atomId atomPr sep =
  fromMaybe (buildSeparatorIndex atomPr (Vector.fromList sep)) $
    lookupSeparatorIndex atomId sep db

semijoinActiveLeftByRight ::
  [SlotId] ->
  AtomId ->
  AtomId ->
  Store ->
  View ->
  View
semijoinActiveLeftByRight sep leftAtom rightAtom store view =
  let prepared = storeRelations store
      leftKey = atomIdKey leftAtom
      rightKey = atomIdKey rightAtom
   in case (IntMap.lookup leftKey prepared, IntMap.lookup rightKey prepared) of
        (Just leftPr, Just rightPr) ->
          let activeLeft = viewRows store view leftKey
              activeRight = viewRows store view rightKey
              leftIx = separatorIndexFor store leftAtom leftPr sep
              rightIx = separatorIndexFor store rightAtom rightPr sep
              keep =
                rowSetFromIntSetCanonical $
                  rowSetFoldl'
                    ( \acc rowId ->
                        let !rowKey =
                              rowIdInt rowId
                         in case IntMap.lookup rowKey (siRowToKey leftIx) of
                              Nothing ->
                                acc
                              Just key ->
                                let bucket =
                                      Map.findWithDefault emptyRowIdSet key (siByKey rightIx)
                                 in if rowSetIntersectsRowIdSet bucket activeRight
                                      then IntSet.insert rowKey acc
                                      else acc
                    )
                    IntSet.empty
                    activeLeft
           in setViewRows store leftKey keep view
        _ ->
          view
{-# INLINE semijoinActiveLeftByRight #-}

semijoinReduceForestView :: JoinForest -> Store -> View -> ReducedForestView
semijoinReduceForestView forest store view0 =
  let viewUp = reduceUp (jfRoot forest) view0
      viewDown = reduceDown (jfRoot forest) viewUp
      prepared = storeRelations store
      sepIx =
        Map.fromList
          [ ((parent, child), separatorIndexFor store child childPr sep)
            | (childKey, parentId) <- IntMap.toList (jfParent forest),
              let child = mkAtomId childKey,
              let parent = parentId,
              Just sep <- [Map.lookup (child, parent) (jfSeparator forest)],
              Just childPr <- [IntMap.lookup childKey prepared]
          ]
   in ReducedForestView
        { rfvStore = store,
          rfvView = viewDown,
          rfvSepIx = sepIx
        }
  where
    reduceUp :: AtomId -> View -> View
    reduceUp atom view =
      let children =
            IntMap.findWithDefault [] (atomIdKey atom) (jfChildren forest)
          afterChildren =
            foldl' (flip reduceUp) view children
       in foldl'
            ( \v child ->
                case Map.lookup (child, atom) (jfSeparator forest) of
                  Nothing -> v
                  Just sep -> semijoinActiveLeftByRight sep atom child store v
            )
            afterChildren
            children

    reduceDown :: AtomId -> View -> View
    reduceDown atom view =
      let children =
            IntMap.findWithDefault [] (atomIdKey atom) (jfChildren forest)
          afterSelf =
            foldl'
              ( \v child ->
                  case Map.lookup (child, atom) (jfSeparator forest) of
                    Nothing -> v
                    Just sep -> semijoinActiveLeftByRight sep child atom store v
              )
              view
              children
       in foldl' (flip reduceDown) afterSelf children

extendEnvWithRow :: Relation -> RowId -> JoinEnv -> Maybe JoinEnv
extendEnvWithRow pr rid env = do
  row <- rowForId pr rid
  let vals = tupleKeyToRepKeys row
  foldM
    ( \envAcc (sid, rep) ->
        case IntMap.lookup (slotIdKey sid) envAcc of
          Nothing -> pure (IntMap.insert (slotIdKey sid) rep envAcc)
          Just rep'
            | rep' == rep -> pure envAcc
            | otherwise -> Nothing
    )
    env
    (zip (Vector.toList (indexedRowsLayout (relRows pr))) vals)
{-# INLINE extendEnvWithRow #-}

enumerateReducedForestRows ::
  [SlotId] ->
  JoinForest ->
  ReducedForestView ->
  [RowTupleKey]
enumerateReducedForestRows fullSchema forest reduced =
  mapMaybe (tupleKeyFromSlotEnv fullSchema) (enumerateFromRoot reduced)
  where
    enumerateFromRoot :: ReducedForestView -> [JoinEnv]
    enumerateFromRoot currentReduced =
      let root = jfRoot forest
          rootKey = atomIdKey root
          store = rfvStore currentReduced
          prepared = storeRelations store
       in case IntMap.lookup rootKey prepared of
            Nothing -> []
            Just rootPr -> do
              rowId <- rowSetToList (viewRows store (rfvView currentReduced) rootKey)
              env0 <- maybeToList (extendEnvWithRow rootPr rowId IntMap.empty)
              descend currentReduced root env0

    descend :: ReducedForestView -> AtomId -> JoinEnv -> [JoinEnv]
    descend currentReduced parent env0 =
      foldM
        (\env child -> descendOne currentReduced parent child env)
        env0
        (IntMap.findWithDefault [] (atomIdKey parent) (jfChildren forest))

    descendOne :: ReducedForestView -> AtomId -> AtomId -> JoinEnv -> [JoinEnv]
    descendOne currentReduced parent child env0 =
      case Map.lookup (child, parent) (jfSeparator forest) of
        Nothing ->
          []
        Just sep ->
          let childKey = atomIdKey child
              store = rfvStore currentReduced
              prepared = storeRelations store
              activeChild = viewRows store (rfvView currentReduced) childKey
           in case (Map.lookup (parent, child) (rfvSepIx currentReduced), IntMap.lookup childKey prepared) of
                (Just ix, Just childPr) ->
                  case separatorKeyFromEnv env0 sep of
                    Nothing -> []
                    Just key ->
                      let bucket =
                            Map.findWithDefault emptyRowIdSet key (siByKey ix)
                          candidates =
                            rowSetIntersectionWithRowIdSet bucket activeChild
                       in do
                            rowId <- rowSetToList candidates
                            env1 <- maybeToList (extendEnvWithRow childPr rowId env0)
                            descend currentReduced child env1
                _ -> []
{-# INLINE enumerateReducedForestRows #-}

reducedForestSupport ::
  ReducedForestView ->
  SupportIds
reducedForestSupport reduced =
  materializeViewRows (rfvStore reduced) (rfvView reduced)
{-# INLINE reducedForestSupport #-}

separatorKeyFromEnv :: JoinEnv -> [SlotId] -> Maybe SeparatorTupleKey
separatorKeyFromEnv env sep =
  tupleKeyFromRepKeys <$> traverse (\sid -> IntMap.lookup (slotIdKey sid) env) sep

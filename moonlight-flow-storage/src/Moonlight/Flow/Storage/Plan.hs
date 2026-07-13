{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Storage.Plan
  ( StoragePlan (..),
    CompiledStoragePlan (..),
    StoragePlanError (..),
    emptyStoragePlan,
    storagePlanFromLayouts,
    storagePlanFromRelations,
    storagePlanFromQueryPlan,
    storagePlanAddSeparator,
    compileStoragePlan,
  )
where

import Data.Foldable qualified as Foldable
import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet
  ( IntSet,
  )
import Data.IntSet qualified as IntSet
import Data.Kind
  ( Type,
  )
import Data.Map.Strict qualified as Map
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Moonlight.Core
  ( SlotId,
    atomIdKey,
    slotIdKey,
  )
import Moonlight.Flow.Plan.Query.Core
  ( JoinForest,
    JoinShape,
    QueryPlan,
    asQueryAtomId,
    asColumns,
    foldJoinShape,
    jfSeparator,
    jmShape,
    qpAtoms,
    qpJoinMeta,
    queryAtomKey,
  )
import Moonlight.Differential.Row.Block
  ( RowLayout,
  )
import Moonlight.Flow.Storage.Relation
  ( Relation,
    relationLayout,
  )
import Moonlight.Flow.Storage.Separator
  ( SeparatorSpec (..),
  )

type StoragePlan :: Type
data StoragePlan = StoragePlan
  { spLayouts :: !(IntMap RowLayout),
    spSeparators :: !(Set SeparatorSpec)
  }
  deriving stock (Eq, Show)

type CompiledStoragePlan :: Type
data CompiledStoragePlan = CompiledStoragePlan
  { cspLayouts :: !(IntMap RowLayout),
    cspSeparators :: !(Set SeparatorSpec),
    cspSeparatorsByAtom :: !(IntMap [SeparatorSpec])
  }
  deriving stock (Eq, Show)

type StoragePlanError :: Type
data StoragePlanError
  = StoragePlanNegativeRowLayoutSlot !Int !SlotId
  | StoragePlanDuplicateRowLayoutSlot !Int !SlotId
  | StoragePlanMissingSeparatorAtom !SeparatorSpec
  | StoragePlanNegativeSeparatorSlot !SeparatorSpec !SlotId
  | StoragePlanDuplicateSeparatorSlot !SeparatorSpec !SlotId
  | StoragePlanSeparatorSlotNotInAtom !SeparatorSpec !SlotId
  deriving stock (Eq, Show)

emptyStoragePlan :: StoragePlan
emptyStoragePlan =
  StoragePlan
    { spLayouts = IntMap.empty,
      spSeparators = Set.empty
    }
{-# INLINE emptyStoragePlan #-}

storagePlanFromLayouts :: IntMap RowLayout -> StoragePlan
storagePlanFromLayouts schemas =
  emptyStoragePlan
    { spLayouts = schemas
    }
{-# INLINE storagePlanFromLayouts #-}

storagePlanFromRelations :: IntMap Relation -> StoragePlan
storagePlanFromRelations =
  storagePlanFromLayouts . IntMap.map relationLayout
{-# INLINE storagePlanFromRelations #-}

storagePlanFromQueryPlan ::
  QueryPlan compiled output guard tag tuple key ->
  StoragePlan
storagePlanFromQueryPlan plan =
  addJoinShapeSeparators
    (jmShape (qpJoinMeta plan))
    (storagePlanFromLayouts atomRowLayouts)
  where
    !atomRowLayouts =
      Vector.foldl'
        ( \schemas atomSpec ->
            IntMap.insert
              (queryAtomKey (asQueryAtomId atomSpec))
              (asColumns atomSpec)
              schemas
        )
        IntMap.empty
        (qpAtoms plan)
{-# INLINE storagePlanFromQueryPlan #-}

storagePlanAddSeparator :: SeparatorSpec -> StoragePlan -> StoragePlan
storagePlanAddSeparator separator plan =
  plan
    { spSeparators =
        Set.insert separator (spSeparators plan)
    }
{-# INLINE storagePlanAddSeparator #-}

compileStoragePlan ::
  StoragePlan ->
  Either StoragePlanError CompiledStoragePlan
compileStoragePlan plan = do
  IntMap.foldlWithKey'
    ( \validated atomKey schema ->
        validated *> validateRowLayout atomKey schema
    )
    (Right ())
    (spLayouts plan)

  Foldable.traverse_
    (validateSeparator (spLayouts plan))
    (Set.toAscList (spSeparators plan))

  pure
    CompiledStoragePlan
      { cspLayouts = spLayouts plan,
        cspSeparators = spSeparators plan,
        cspSeparatorsByAtom =
          Set.foldl'
            ( \byAtom separator ->
                IntMap.insertWith
                  (<>)
                  (atomIdKey (ssAtom separator))
                  [separator]
                  byAtom
            )
            IntMap.empty
            (spSeparators plan)
      }
{-# INLINE compileStoragePlan #-}

addJoinShapeSeparators ::
  JoinShape ->
  StoragePlan ->
  StoragePlan
addJoinShapeSeparators shape =
  foldJoinShape
    id
    addForestSeparators
    (const id)
    shape
{-# INLINE addJoinShapeSeparators #-}

addForestSeparators ::
  JoinForest ->
  StoragePlan ->
  StoragePlan
addForestSeparators forest plan =
  Foldable.foldl'
    (flip storagePlanAddSeparator)
    plan
    [ SeparatorSpec atomId (Vector.fromList sep)
      | ((child, parent), sep) <- Map.toAscList (jfSeparator forest),
        atomId <- [child, parent]
    ]
{-# INLINE addForestSeparators #-}

validateRowLayout :: Int -> RowLayout -> Either StoragePlanError ()
validateRowLayout atomKey schema =
  case firstNegativeSlot schema of
    Just slot ->
      Left (StoragePlanNegativeRowLayoutSlot atomKey slot)
    Nothing ->
      case firstDuplicateSlot schema of
        Just slot ->
          Left (StoragePlanDuplicateRowLayoutSlot atomKey slot)
        Nothing ->
          Right ()
{-# INLINE validateRowLayout #-}

validateSeparator ::
  IntMap RowLayout ->
  SeparatorSpec ->
  Either StoragePlanError ()
validateSeparator schemas separator =
  case IntMap.lookup atomKey schemas of
    Nothing ->
      Left (StoragePlanMissingSeparatorAtom separator)
    Just schema ->
      case firstNegativeSlot (ssSlots separator) of
        Just slot ->
          Left (StoragePlanNegativeSeparatorSlot separator slot)
        Nothing ->
          case firstDuplicateSlot (ssSlots separator) of
            Just slot ->
              Left (StoragePlanDuplicateSeparatorSlot separator slot)
            Nothing ->
              case firstMissingSlot schema (ssSlots separator) of
                Just slot ->
                  Left (StoragePlanSeparatorSlotNotInAtom separator slot)
                Nothing ->
                  Right ()
  where
    !atomKey =
      atomIdKey (ssAtom separator)
{-# INLINE validateSeparator #-}

firstNegativeSlot :: RowLayout -> Maybe SlotId
firstNegativeSlot =
  Vector.find ((< 0) . slotIdKey)
{-# INLINE firstNegativeSlot #-}

firstDuplicateSlot :: RowLayout -> Maybe SlotId
firstDuplicateSlot schema =
  snd
    ( Vector.foldl'
        step
        (IntSet.empty, Nothing)
        schema
    )
  where
    step result@(_, Just _) _slot =
      result
    step (!seen, Nothing) slot =
      let !slotKey =
            slotIdKey slot
       in if IntSet.member slotKey seen
            then (seen, Just slot)
            else (IntSet.insert slotKey seen, Nothing)
{-# INLINE firstDuplicateSlot #-}

firstMissingSlot :: RowLayout -> RowLayout -> Maybe SlotId
firstMissingSlot available wanted =
  Vector.find
    (not . (`IntSet.member` availableKeys) . slotIdKey)
    wanted
  where
    !availableKeys =
      layoutSlotKeys available
{-# INLINE firstMissingSlot #-}

layoutSlotKeys :: RowLayout -> IntSet
layoutSlotKeys =
  Vector.foldl'
    (\keys slot -> IntSet.insert (slotIdKey slot) keys)
    IntSet.empty
{-# INLINE layoutSlotKeys #-}

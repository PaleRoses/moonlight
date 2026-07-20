{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- | Support-counted view maintenance.  An output @key@ holds a bag of
-- contributions, each a @value@ tagged with the set of source @cell@s that
-- witness it; the key's materialized value is the commutative-monoid fold over
-- its bag.  An inverted index @cell -> keys@ makes invalidation surgical: a
-- dirty source cell touches only the keys whose bag references it, and a
-- per-row @cell -> count@ tally decides edge existence without rescanning.
-- Contributions are content-keyed, so identical witnesses coalesce for free —
-- there is no interning table and no contribution-id allocation.  The law is
-- @advance = rebuild@: an incremental advance agrees with rebuilding the view
-- from the surviving contributions plus the fresh ones.
module Moonlight.Differential.Operator.SupportedView
  ( Contribution (..),
    SupportedView,
    SupportedRow,
    supportedViewRows,
    supportedRowContributions,
    supportedRowValue,
    ViewChange (..),
    viewChangeBefore,
    viewChangeAfter,
    emptySupportedView,
    buildSupportedView,
    supportedViewKeys,
    supportedViewValueAt,
    supportedViewKeysForCells,
    supportedViewAdvance,
  )
where

import Data.Kind
  ( Type,
  )
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Maybe
  ( isJust,
  )
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set

type Contribution :: Type -> Type -> Type
data Contribution cell value = Contribution
  { contributionValue :: !value,
    contributionSupport :: !(Set cell)
  }
  deriving stock (Eq, Ord, Show)

type SupportedRow :: Type -> Type -> Type
data SupportedRow cell value = SupportedRow
  { supportedRowContributions :: !(Map (Contribution cell value) Int),
    supportedRowSupportCounts :: !(Map cell Int),
    supportedRowValue :: !value
  }
  deriving stock (Eq, Show)

type SupportedView :: Type -> Type -> Type -> Type
data SupportedView cell key value = SupportedView
  { supportedViewRows :: !(Map key (SupportedRow cell value)),
    supportedViewBySupport :: !(Map cell (SupportedKeys key))
  }
  deriving stock (Eq, Show)

data SupportedKeys key
  = OneSupportedKey !key
  | ManySupportedKeys !(Set key)
  deriving stock (Eq, Show)

type ViewChange :: Type -> Type
data ViewChange value
  = ViewInserted !value
  | ViewRemoved !value
  | ViewUpdated !value !value
  deriving stock (Eq, Show)

viewChangeBefore :: ViewChange value -> Maybe value
viewChangeBefore change =
  case change of
    ViewInserted _ ->
      Nothing
    ViewRemoved before ->
      Just before
    ViewUpdated before _ ->
      Just before
{-# INLINE viewChangeBefore #-}

viewChangeAfter :: ViewChange value -> Maybe value
viewChangeAfter change =
  case change of
    ViewInserted after ->
      Just after
    ViewRemoved _ ->
      Nothing
    ViewUpdated _ after ->
      Just after
{-# INLINE viewChangeAfter #-}

emptySupportedView :: SupportedView cell key value
emptySupportedView =
  SupportedView
    { supportedViewRows = Map.empty,
      supportedViewBySupport = Map.empty
    }
{-# INLINE emptySupportedView #-}

buildSupportedView ::
  (Ord cell, Ord key, Ord value, Monoid value) =>
  Map key [Contribution cell value] ->
  SupportedView cell key value
buildSupportedView contributions =
  let rows =
        Map.mapMaybe rowFromContributions contributions
   in SupportedView
        { supportedViewRows = rows,
          supportedViewBySupport = invertedFromRows rows
        }

supportedViewKeys :: SupportedView cell key value -> Set key
supportedViewKeys =
  Map.keysSet . supportedViewRows
{-# INLINE supportedViewKeys #-}

supportedViewValueAt ::
  (Ord key, Monoid value) =>
  key ->
  SupportedView cell key value ->
  value
supportedViewValueAt key view =
  maybe mempty supportedRowValue (Map.lookup key (supportedViewRows view))
{-# INLINE supportedViewValueAt #-}

supportedViewKeysForCells ::
  (Ord cell, Ord key) =>
  Set cell ->
  SupportedView cell key value ->
  Set key
supportedViewKeysForCells cells view =
  Set.foldl'
    ( \acc cell ->
        maybe acc (`insertSupportedKeysInto` acc) (Map.lookup cell (supportedViewBySupport view))
    )
    Set.empty
    cells

insertSupportedKeysInto :: Ord key => SupportedKeys key -> Set key -> Set key
insertSupportedKeysInto supportedKeys keys =
  case supportedKeys of
    OneSupportedKey key ->
      Set.insert key keys
    ManySupportedKeys supportedKeySet ->
      Set.union supportedKeySet keys
{-# INLINE insertSupportedKeysInto #-}

supportedViewAdvance ::
  (Ord cell, Ord key, Ord value, Monoid value) =>
  Set cell ->
  Map key [Contribution cell value] ->
  SupportedView cell key value ->
  (SupportedView cell key value, Map key (ViewChange value))
supportedViewAdvance dirtyCells freshContributions view =
  let !affectedKeys =
        supportedViewKeysForCells dirtyCells view
      !workKeys =
        Set.union affectedKeys (Map.keysSet freshContributions)
      (!rows1, !inverted1, !changes) =
        Set.foldl'
          advanceKey
          (supportedViewRows view, supportedViewBySupport view, Map.empty)
          workKeys
   in ( SupportedView
          { supportedViewRows = rows1,
            supportedViewBySupport = inverted1
          },
        changes
      )
  where
    advanceKey (!rows, !inverted, !changes) key =
      let !oldRow =
            Map.lookup key rows
          !oldValue =
            maybe mempty supportedRowValue oldRow
          !oldCells =
            maybe Set.empty (Map.keysSet . supportedRowSupportCounts) oldRow
          !survivingBag =
            maybe
              Map.empty
              ( Map.filterWithKey
                  (\contribution _ -> Set.disjoint (contributionSupport contribution) dirtyCells)
                  . supportedRowContributions
              )
              oldRow
          !freshBag =
            bagFromList (Map.findWithDefault [] key freshContributions)
          !newBag =
            Map.filter (> 0) (Map.unionWith (+) survivingBag freshBag)
          !maybeNewRow =
            if Map.null newBag
              then Nothing
              else Just (rowFromBag newBag)
          !newValue =
            maybe mempty supportedRowValue maybeNewRow
          !newCells =
            maybe Set.empty (Map.keysSet . supportedRowSupportCounts) maybeNewRow
          !rows1 =
            maybe (Map.delete key rows) (\row -> Map.insert key row rows) maybeNewRow
          !inverted1 =
            reindexKey key oldCells newCells inverted
          !changes1 =
            case classifyChange (isJust oldRow) oldValue (isJust maybeNewRow) newValue of
              Nothing ->
                changes
              Just change ->
                Map.insert key change changes
       in (rows1, inverted1, changes1)

classifyChange :: Eq value => Bool -> value -> Bool -> value -> Maybe (ViewChange value)
classifyChange presentBefore before presentAfter after =
  case (presentBefore, presentAfter) of
    (False, False) ->
      Nothing
    (False, True) ->
      Just (ViewInserted after)
    (True, False) ->
      Just (ViewRemoved before)
    (True, True) ->
      if before == after
        then Nothing
        else Just (ViewUpdated before after)
{-# INLINE classifyChange #-}

reindexKey ::
  (Ord cell, Ord key) =>
  key ->
  Set cell ->
  Set cell ->
  Map cell (SupportedKeys key) ->
  Map cell (SupportedKeys key)
reindexKey key oldCells newCells inverted =
  let !removed =
        Set.difference oldCells newCells
      !added =
        Set.difference newCells oldCells
      !invertedWithoutRemoved =
        Set.foldl' (dropEdge key) inverted removed
   in Set.foldl' (addEdge key) invertedWithoutRemoved added

dropEdge :: (Ord cell, Ord key) => key -> Map cell (SupportedKeys key) -> cell -> Map cell (SupportedKeys key)
dropEdge key inverted cell =
  Map.update
    (deleteSupportedKey key)
    cell
    inverted
{-# INLINE dropEdge #-}

deleteSupportedKey :: Ord key => key -> SupportedKeys key -> Maybe (SupportedKeys key)
deleteSupportedKey key supportedKeys =
  case supportedKeys of
    OneSupportedKey existingKey
      | key == existingKey ->
          Nothing
      | otherwise ->
          Just supportedKeys
    ManySupportedKeys existingKeys ->
      case Set.delete key existingKeys of
        remainingKeys
          | Set.null remainingKeys ->
              Nothing
          | Set.size remainingKeys == 1 ->
              OneSupportedKey <$> Set.lookupMin remainingKeys
          | otherwise ->
              Just (ManySupportedKeys remainingKeys)
{-# INLINE deleteSupportedKey #-}

addEdge :: (Ord cell, Ord key) => key -> Map cell (SupportedKeys key) -> cell -> Map cell (SupportedKeys key)
addEdge key inverted cell =
  Map.alter
    (Just . maybe (OneSupportedKey key) (insertSupportedKey key))
    cell
    inverted
{-# INLINE addEdge #-}

insertSupportedKey :: Ord key => key -> SupportedKeys key -> SupportedKeys key
insertSupportedKey key supportedKeys =
  case supportedKeys of
    OneSupportedKey existingKey
      | key == existingKey ->
          supportedKeys
      | otherwise ->
          ManySupportedKeys (Set.fromList [existingKey, key])
    ManySupportedKeys existingKeys ->
      ManySupportedKeys (Set.insert key existingKeys)
{-# INLINE insertSupportedKey #-}

rowFromContributions ::
  (Ord cell, Ord value, Monoid value) =>
  [Contribution cell value] ->
  Maybe (SupportedRow cell value)
rowFromContributions contributions =
  let !bag =
        bagFromList contributions
   in if Map.null bag
        then Nothing
        else Just (rowFromBag bag)
{-# INLINE rowFromContributions #-}

bagFromList :: (Ord cell, Ord value) => [Contribution cell value] -> Map (Contribution cell value) Int
bagFromList =
  Map.fromListWith (+) . fmap (\contribution -> (contribution, 1))
{-# INLINE bagFromList #-}

rowFromBag ::
  (Ord cell, Monoid value) =>
  Map (Contribution cell value) Int ->
  SupportedRow cell value
rowFromBag bag =
  SupportedRow
    { supportedRowContributions = bag,
      supportedRowSupportCounts = supportCountsFromBag bag,
      supportedRowValue = valueFromBag bag
    }
{-# INLINE rowFromBag #-}

supportCountsFromBag ::
  Ord cell =>
  Map (Contribution cell value) Int ->
  Map cell Int
supportCountsFromBag =
  Map.foldlWithKey'
    ( \acc contribution count ->
        Set.foldl'
          (\counts cell -> Map.insertWith (+) cell count counts)
          acc
          (contributionSupport contribution)
    )
    Map.empty
{-# INLINE supportCountsFromBag #-}

valueFromBag ::
  Monoid value =>
  Map (Contribution cell value) Int ->
  value
valueFromBag =
  Map.foldlWithKey'
    (\acc contribution count -> acc <> mtimesMonoid count (contributionValue contribution))
    mempty
{-# INLINE valueFromBag #-}

mtimesMonoid :: Monoid value => Int -> value -> value
mtimesMonoid count value
  | count <= 0 =
      mempty
  | count == 1 =
      value
  | otherwise =
      mconcat (replicate count value)
{-# INLINE mtimesMonoid #-}

invertedFromRows ::
  (Ord cell, Ord key) =>
  Map key (SupportedRow cell value) ->
  Map cell (SupportedKeys key)
invertedFromRows rows =
  Map.foldlWithKey'
    ( \acc key row ->
        Map.foldlWithKey'
          (\inverted cell _ -> addEdge key inverted cell)
          acc
          (supportedRowSupportCounts row)
    )
    Map.empty
    rows
{-# INLINE invertedFromRows #-}

module Moonlight.Sheaf.Relational.Carrier.Visible
  ( visibleSheafSection,
    visibleSheafPartialAssignment,
  )
where

import Data.Map.Strict qualified as Map
import Data.Set
  ( Set,
  )
import Moonlight.Sheaf.Section.Model
  ( SheafModel,
  )
import Moonlight.Sheaf.Section.Store.State qualified as Store
import Moonlight.Sheaf.Section.Store.Types
  ( PartialSectionStore,
    SectionStoreError,
    TotalSectionStore,
  )

visibleSheafSection ::
  SheafModel owner ctx witness ->
  (store -> Set ctx) ->
  (ctx -> store -> section) ->
  store ->
  TotalSectionStore owner ctx section
visibleSheafSection sheafModel _activeContexts buildSection store =
  Store.emptyTotalSectionStoreWith sheafModel (`buildSection` store)
{-# INLINE visibleSheafSection #-}

visibleSheafPartialAssignment ::
  Ord ctx =>
  SheafModel owner ctx witness ->
  (store -> Set ctx) ->
  (ctx -> store -> section) ->
  store ->
  Either
    (SectionStoreError ctx)
    (PartialSectionStore owner ctx section)
visibleSheafPartialAssignment sheafModel activeContexts buildSection store =
  Store.mkPartialSectionStore
    sheafModel
    (Map.fromSet (`buildSection` store) (activeContexts store))
{-# INLINE visibleSheafPartialAssignment #-}

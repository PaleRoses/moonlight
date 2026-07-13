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
  SheafModel ctx witness ->
  (store -> Set ctx) ->
  (ctx -> store -> section) ->
  store ->
  TotalSectionStore ctx section
visibleSheafSection sheafModel _activeContexts buildSection store =
  Store.emptyTotalSectionStoreWith sheafModel (`buildSection` store)
{-# INLINE visibleSheafSection #-}

visibleSheafPartialAssignment ::
  Ord ctx =>
  SheafModel ctx witness ->
  (store -> Set ctx) ->
  (ctx -> store -> section) ->
  store ->
  Either
    (SectionStoreError ctx)
    (PartialSectionStore ctx section)
visibleSheafPartialAssignment sheafModel activeContexts buildSection store =
  Store.mkPartialSectionStore
    sheafModel
    (Map.fromSet (`buildSection` store) (activeContexts store))
{-# INLINE visibleSheafPartialAssignment #-}

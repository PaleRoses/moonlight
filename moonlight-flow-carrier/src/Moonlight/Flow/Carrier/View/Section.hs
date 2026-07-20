{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Carrier.View.Section
  ( RelationalSection (..),
    RelationalGlobalSection (..),
    emptyVisibleGlobalSection,
    setVisibleCarrierRows,
    deleteVisibleCarrierRows,
    unionVisibleSections,
    unionVisibleGlobalSections,
  )
where

import Data.Kind
  ( Type,
  )
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
  )
import Moonlight.Differential.Row.Delta
  ( RowDelta
  )
import Moonlight.Differential.Row.Delta
  ( rowDeltaNull
  )
import Moonlight.Differential.Row.Patch
  ( composePlainRowPatch,
  )

type RelationalSection :: Type -> Type -> Type -> Type
newtype RelationalSection ctx carrier prop = RelationalSection
  { rsCarriers :: Map (CarrierAddr ctx carrier prop) RowDelta
  }
  deriving stock (Eq, Show)

type RelationalGlobalSection :: Type -> Type -> Type -> Type
newtype RelationalGlobalSection ctx carrier prop = RelationalGlobalSection
  { rgsContexts :: Map ctx (RelationalSection ctx carrier prop)
  }
  deriving stock (Eq, Show)

emptyVisibleGlobalSection ::
  RelationalGlobalSection ctx carrier prop
emptyVisibleGlobalSection =
  RelationalGlobalSection
    { rgsContexts = Map.empty
    }
{-# INLINE emptyVisibleGlobalSection #-}

setVisibleCarrierRows ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CarrierAddr ctx carrier prop ->
  RowDelta ->
  RelationalSection ctx carrier prop ->
  RelationalSection ctx carrier prop
setVisibleCarrierRows addr rows section
  | rowDeltaNull rows =
      deleteVisibleCarrierRows addr section
  | otherwise =
      section
        { rsCarriers = Map.insert addr rows (rsCarriers section)
        }
{-# INLINE setVisibleCarrierRows #-}

deleteVisibleCarrierRows ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CarrierAddr ctx carrier prop ->
  RelationalSection ctx carrier prop ->
  RelationalSection ctx carrier prop
deleteVisibleCarrierRows addr section =
  section
    { rsCarriers = Map.delete addr (rsCarriers section)
    }
{-# INLINE deleteVisibleCarrierRows #-}

unionVisibleGlobalSections ::
  (Ord ctx, Ord carrier, Ord prop) =>
  RelationalGlobalSection ctx carrier prop ->
  RelationalGlobalSection ctx carrier prop ->
  RelationalGlobalSection ctx carrier prop
unionVisibleGlobalSections leftGlobal rightGlobal =
  RelationalGlobalSection
    { rgsContexts =
        Map.unionWith
          unionVisibleSections
          (rgsContexts leftGlobal)
          (rgsContexts rightGlobal)
    }
{-# INLINE unionVisibleGlobalSections #-}

unionVisibleSections ::
  (Ord ctx, Ord carrier, Ord prop) =>
  RelationalSection ctx carrier prop ->
  RelationalSection ctx carrier prop ->
  RelationalSection ctx carrier prop
unionVisibleSections leftSection rightSection =
  RelationalSection
    { rsCarriers =
        Map.unionWith
          composePlainRowPatch
          (rsCarriers leftSection)
          (rsCarriers rightSection)
    }
{-# INLINE unionVisibleSections #-}

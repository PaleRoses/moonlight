{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Sheaf.Relational.Carrier.Fact
  ( relationalFactTotalSection,
    relationalFactPartialAssignment,
  )
where

import Moonlight.Core
  ( BoundaryOps,
  )
import Moonlight.Flow.Carrier.Fact
  ( CarrierFactSection,
    carrierFactContextsNow,
    carrierFactSectionNow,
  )
import Moonlight.Flow.Carrier.Store.Core.State
  ( CarrierStore,
  )
import Moonlight.Sheaf.Section.Model
  ( SheafModel,
  )
import Moonlight.Sheaf.Section.Store.Types
  ( PartialSectionStore,
    SectionStoreError,
    TotalSectionStore,
  )
import Moonlight.Sheaf.Relational.Carrier.Visible
  ( visibleSheafPartialAssignment,
    visibleSheafSection,
  )

relationalFactTotalSection ::
  (Ord ctx, Ord prop, BoundaryOps boundary) =>
  SheafModel ctx witness ->
  CarrierStore ctx carrier prop boundary evidence ->
  TotalSectionStore ctx (CarrierFactSection ctx carrier prop boundary evidence)
relationalFactTotalSection sheafModel =
  visibleSheafSection sheafModel carrierFactContextsNow carrierFactSectionNow
{-# INLINE relationalFactTotalSection #-}

relationalFactPartialAssignment ::
  (Ord ctx, Ord prop, BoundaryOps boundary) =>
  SheafModel ctx witness ->
  CarrierStore ctx carrier prop boundary evidence ->
  Either
    (SectionStoreError ctx)
    (PartialSectionStore ctx (CarrierFactSection ctx carrier prop boundary evidence))
relationalFactPartialAssignment sheafModel =
  visibleSheafPartialAssignment sheafModel carrierFactContextsNow carrierFactSectionNow
{-# INLINE relationalFactPartialAssignment #-}

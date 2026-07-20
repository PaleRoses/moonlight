module Moonlight.Flow.Carrier.Store.Projection.Current
  ( CarrierCurrentRows (..),
    CarrierSnapshot (..),
    CarrierCurrentIndex (..),
    CarrierCurrentProjection (..),
    emptyCarrierCurrentRows,
    emptyCarrierCurrentIndex,
    emptyCarrierCurrentProjection,
    carrierCurrentRowsPlain,
    applyCarrierCurrentRows,
    applyCarrierCurrentProjection,
    putCarrierCurrentSnapshotProjection,
    spliceCarrierCurrentProjection,
    carrierCurrentRowsPresent,
    updateCarrierCurrentIndexForSnapshot,
  )
where

import Data.Map.Strict qualified as Map
import Moonlight.Differential.Index.IntSet
  ( deleteSetIndex,
    insertSetIndex,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
    caContext,
    caProp,
    caCarrier,
  )
import Moonlight.Flow.Carrier.Core.Delta
  ( RelationalCarrierDelta,
    RelationalCarrierDeltaP (..),
  )
import Moonlight.Flow.Carrier.Store.Core.Error
  ( CarrierStoreError (..),
  )
import Moonlight.Flow.Carrier.Store.Core.State
import Moonlight.Differential.Row.Delta
  ( RowDelta
  )
import Moonlight.Differential.Row.Patch
  ( applyPlainRowPatchWith,
    emptyPlainRowPatch,
    plainRowPatchFromMultiplicityMap,
    plainRowPatchNull,
    positivePlainRowPatchRows,
  )

emptyCarrierCurrentRows :: CarrierCurrentRows
emptyCarrierCurrentRows =
  CarrierCurrentRows
    { ccrRows = emptyPlainRowPatch
    }
{-# INLINE emptyCarrierCurrentRows #-}

emptyCarrierCurrentIndex :: CarrierCurrentIndex ctx carrier prop
emptyCarrierCurrentIndex =
  CarrierCurrentIndex
    { ciCurrentByContext = Map.empty,
      ciCurrentByCarrier = Map.empty,
      ciCurrentByProp = Map.empty
    }
{-# INLINE emptyCarrierCurrentIndex #-}

emptyCarrierCurrentProjection :: CarrierCurrentProjection ctx carrier prop boundary evidence
emptyCarrierCurrentProjection =
  CarrierCurrentProjection
    { ccpSnapshots = Map.empty,
      ccpIndexes = emptyCarrierCurrentIndex
    }
{-# INLINE emptyCarrierCurrentProjection #-}

carrierCurrentRowsPlain ::
  CarrierCurrentRows ->
  RowDelta
carrierCurrentRowsPlain =
  ccrRows
{-# INLINE carrierCurrentRowsPlain #-}

applyCarrierCurrentRows ::
  CarrierAddr ctx carrier prop ->
  RowDelta ->
  Maybe CarrierCurrentRows ->
  Either
    (CarrierStoreError ctx carrier prop boundary evidence)
    (RowDelta, CarrierCurrentRows)
applyCarrierCurrentRows addr rowsDelta maybeCurrentRows = do
  let currentRows =
        maybe emptyCarrierCurrentRows id maybeCurrentRows
      mkRowsUnderflow rowValue oldMultiplicity deltaMultiplicity =
        CarrierStoreRowMultiplicityUnderflow
          addr
          rowValue
          oldMultiplicity
          deltaMultiplicity

  nextPlainRows <-
    applyPlainRowPatchWith
      mkRowsUnderflow
      rowsDelta
      (positivePlainRowPatchRows (carrierCurrentRowsPlain currentRows))

  let nextRows =
        plainRowPatchFromMultiplicityMap nextPlainRows

  pure
    ( nextRows,
      CarrierCurrentRows
        { ccrRows = nextRows
        }
    )
{-# INLINE applyCarrierCurrentRows #-}

applyCarrierCurrentProjection ::
  (Ord ctx, Ord carrier, Ord prop) =>
  TraceId ->
  RelationalCarrierDelta ctx carrier prop boundary evidence ->
  CarrierCurrentProjection ctx carrier prop boundary evidence ->
  Either
    (CarrierStoreError ctx carrier prop boundary evidence)
    (CarrierCurrentProjection ctx carrier prop boundary evidence)
applyCarrierCurrentProjection traceId delta projection = do
  let addr =
        deAddr delta
      oldSnapshot =
        Map.lookup addr (ccpSnapshots projection)
      oldPresent =
        maybe False (carrierCurrentRowsPresent . csCurrentRows) oldSnapshot

  (_plainDelta, nextRows) <-
    applyCarrierCurrentRows addr (deRows delta) (csCurrentRows <$> oldSnapshot)

  let nextPresent =
        carrierCurrentRowsPresent nextRows
      nextSnapshots =
        if nextPresent
          then
            Map.insert
              addr
              CarrierSnapshot
                { csCurrentRows = nextRows,
                  csLatestTrace = traceId
                }
              (ccpSnapshots projection)
          else Map.delete addr (ccpSnapshots projection)
      nextIndexes =
        updateCarrierCurrentIndexForSnapshot
          addr
          oldPresent
          nextPresent
          (ccpIndexes projection)
  pure
    projection
      { ccpSnapshots = nextSnapshots,
        ccpIndexes = nextIndexes
      }
{-# INLINE applyCarrierCurrentProjection #-}

putCarrierCurrentSnapshotProjection ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CarrierAddr ctx carrier prop ->
  CarrierSnapshot ctx carrier prop boundary evidence ->
  CarrierCurrentProjection ctx carrier prop boundary evidence ->
  CarrierCurrentProjection ctx carrier prop boundary evidence
putCarrierCurrentSnapshotProjection addr snapshot projection =
  let oldPresent =
        Map.member addr (ccpSnapshots projection)
      nextSnapshots =
        Map.insert addr snapshot (ccpSnapshots projection)
      nextIndexes =
        updateCarrierCurrentIndexForSnapshot addr oldPresent True (ccpIndexes projection)
   in projection
        { ccpSnapshots = nextSnapshots,
          ccpIndexes = nextIndexes
        }
{-# INLINE putCarrierCurrentSnapshotProjection #-}

spliceCarrierCurrentProjection ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CarrierAddr ctx carrier prop ->
  CarrierCurrentProjection ctx carrier prop boundary evidence ->
  CarrierCurrentProjection ctx carrier prop boundary evidence ->
  CarrierCurrentProjection ctx carrier prop boundary evidence
spliceCarrierCurrentProjection addr localProjection baseProjection =
  let maybeSnapshot =
        Map.lookup addr (ccpSnapshots localProjection)
      oldPresent =
        Map.member addr (ccpSnapshots baseProjection)
      newPresent =
        maybe False (const True) maybeSnapshot
      nextSnapshots =
        case maybeSnapshot of
          Nothing ->
            Map.delete addr (ccpSnapshots baseProjection)
          Just snapshot ->
            Map.insert addr snapshot (ccpSnapshots baseProjection)
      nextIndexes =
        updateCarrierCurrentIndexForSnapshot addr oldPresent newPresent (ccpIndexes baseProjection)
   in baseProjection
        { ccpSnapshots = nextSnapshots,
          ccpIndexes = nextIndexes
        }
{-# INLINE spliceCarrierCurrentProjection #-}

carrierCurrentRowsPresent :: CarrierCurrentRows -> Bool
carrierCurrentRowsPresent =
  not . plainRowPatchNull . carrierCurrentRowsPlain
{-# INLINE carrierCurrentRowsPresent #-}

updateCarrierCurrentIndexForSnapshot ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CarrierAddr ctx carrier prop ->
  Bool ->
  Bool ->
  CarrierCurrentIndex ctx carrier prop ->
  CarrierCurrentIndex ctx carrier prop
updateCarrierCurrentIndexForSnapshot addr wasPresent isPresent indexes =
  case (wasPresent, isPresent) of
    (False, False) ->
      indexes
    (True, True) ->
      indexes
    (False, True) ->
      indexes
        { ciCurrentByContext =
            insertSetIndex (caContext addr) addr (ciCurrentByContext indexes),
          ciCurrentByCarrier =
            insertSetIndex (caCarrier addr) addr (ciCurrentByCarrier indexes),
          ciCurrentByProp =
            insertSetIndex (caProp addr) addr (ciCurrentByProp indexes)
        }
    (True, False) ->
      indexes
        { ciCurrentByContext =
            deleteSetIndex (caContext addr) addr (ciCurrentByContext indexes),
          ciCurrentByCarrier =
            deleteSetIndex (caCarrier addr) addr (ciCurrentByCarrier indexes),
          ciCurrentByProp =
            deleteSetIndex (caProp addr) addr (ciCurrentByProp indexes)
        }
{-# INLINE updateCarrierCurrentIndexForSnapshot #-}

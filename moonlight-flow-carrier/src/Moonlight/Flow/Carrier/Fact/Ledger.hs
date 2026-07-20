{-# LANGUAGE BangPatterns #-}

-- | Live fact ledger for carrier stores.
--
-- The ledger keeps durable trace seeds and their current live row projection.
-- Higher reasoning layers consume these readouts as fact evidence instead of
-- rescanning or reinterpreting the raw carrier trace.
module Moonlight.Flow.Carrier.Fact.Ledger
  ( CarrierCurrentFactEvidence (..),
    CarrierFactCell,
    CarrierFactSeed (..),
    CarrierFactSeedRowKey,
    CarrierFactLedger,
    CarrierFactLedgerError,
    emptyCarrierFactLedger,
    carrierFactCellAt,
    carrierFactCellRows,
    carrierFactCellFacts,
    carrierFactRowsAt,
    carrierFactFactsAt,
    carrierFactAddressesNow,
    carrierFactContextsNow,
    carrierLiveTraceEntriesAt,
    carrierLiveEvidenceAt,
    applyCarrierFactTrace,
    deleteCarrierFactAddress,
  )
where

import Data.Foldable qualified as Foldable
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet
  ( IntSet,
  )
import Data.IntSet qualified as IntSet
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Moonlight.Core
  ( BoundaryOps,
  )
import Moonlight.Differential.Trace.Indexed
  ( indexedTraceEntriesForKeys,
  )
import Moonlight.Differential.Fact.Local
  ( FactAntichain,
    LocalFact,
    emptyFactAntichain,
    mkLocalAddress,
    mkLocalFact,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
    caContext,
    caProp,
    caCarrier,
  )
import Moonlight.Flow.Carrier.Core.Delta
  ( RelationalCarrierDeltaP (..),
  )
import Moonlight.Flow.Carrier.Fact.Internal.LedgerIndex
  ( CarrierFactCell (..),
    CarrierFactCurrentCell (..),
    carrierFactCurrentCellReadout,
  )
import Moonlight.Flow.Carrier.Store.Core.Error
  ( CarrierStoreError (..),
  )
import Moonlight.Flow.Carrier.Store.Core.State
import Moonlight.Differential.Index.IntSet
  ( alterIntMapNull,
    deleteIntMapKeys,
    deleteMapIntSetIndex,
    insertMapIndex,
  )
import Moonlight.Differential.Row.Delta
  ( PositiveMultiplicity,
    RowDelta,
    positiveMultiplicityValue,
    rowDeltaNegativePart,
    rowDeltaPositivePart
  )
import Moonlight.Delta.Signed
  ( Multiplicity (..),
    MultiplicityChange,
    multiplicityValue,
    multiplicityAsChange,
    negateMultiplicityChange,
    subtractMultiplicity,
    zeroMultiplicity
  )
import Moonlight.Differential.Row.Patch
  ( applyPlainRowPatchWith,
    emptyPlainRowPatch,
    plainRowPatchFromMultiplicityMap,
    plainRowPatchNull,
    positivePlainRowPatchRows
  )
import Moonlight.Differential.Row.Tuple
  ( RowTupleKey,
  )
import Numeric.Natural
  ( Natural,
  )
import Moonlight.FiniteLattice
  ( ContextLattice
  )

type CarrierFactLedgerError ctx carrier prop boundary evidence =
  CarrierStoreError ctx carrier prop boundary evidence

emptyCarrierFactLedger :: CarrierFactLedger ctx carrier prop boundary evidence
emptyCarrierFactLedger =
  CarrierFactLedger
    { cflSeeds = IntMap.empty,
      cflLiveByAddr = Map.empty,
      cflLiveByRow = Map.empty,
      cflLiveRowsBySeed = IntMap.empty,
      cflCurrent = Map.empty
    }
{-# INLINE emptyCarrierFactLedger #-}


carrierLiveTraceEntriesAt ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CarrierAddr ctx carrier prop ->
  CarrierStore ctx carrier prop boundary evidence ->
  [CarrierTraceEntry ctx carrier prop boundary evidence]
carrierLiveTraceEntriesAt addr indexState =
  indexedTraceEntriesForKeys liveTraceIds (cstTrace indexState)
  where
    factsIndex =
      cvFacts (cstViews indexState)
    liveSeedIds =
      Map.findWithDefault IntSet.empty addr (cflLiveByAddr factsIndex)
    liveTraceIds =
      IntSet.fromList
        [ traceIdKey (cfsTraceId seed)
        | seedKey <- IntSet.toAscList liveSeedIds,
          IntMap.member seedKey (cflLiveRowsBySeed factsIndex),
          Just seed <- [IntMap.lookup seedKey (cflSeeds factsIndex)]
        ]
{-# INLINE carrierLiveTraceEntriesAt #-}


carrierLiveEvidenceAt ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CarrierAddr ctx carrier prop ->
  CarrierStore ctx carrier prop boundary evidence ->
  [evidence]
carrierLiveEvidenceAt addr indexState =
  let factsIndex =
        cvFacts (cstViews indexState)
      seedIds =
        Map.findWithDefault IntSet.empty addr (cflLiveByAddr factsIndex)
   in [ cfsEvidence seed
      | seedKey <- IntSet.toAscList seedIds,
        IntMap.member seedKey (cflLiveRowsBySeed factsIndex),
        Just seed <- [IntMap.lookup seedKey (cflSeeds factsIndex)]
      ]
{-# INLINE carrierLiveEvidenceAt #-}


carrierFactCellAt ::
  (Ord ctx, Ord carrier, Ord prop, BoundaryOps boundary) =>
  CarrierAddr ctx carrier prop ->
  CarrierStore ctx carrier prop boundary evidence ->
  Maybe (CarrierFactCell ctx carrier prop boundary evidence)
carrierFactCellAt addr store =
  case Map.lookup addr (cflCurrent (cvFacts (cstViews store))) of
    Nothing ->
      Nothing
    Just currentCell ->
      carrierFactCurrentCellReadout currentCell
{-# INLINE carrierFactCellAt #-}

carrierFactCellRows ::
  CarrierFactCell ctx carrier prop boundary evidence ->
  RowDelta
carrierFactCellRows =
  cfcRows
{-# INLINE carrierFactCellRows #-}

carrierFactCellFacts ::
  CarrierFactCell ctx carrier prop boundary evidence ->
  FactAntichain ctx prop (CarrierCurrentFactEvidence carrier evidence) boundary
carrierFactCellFacts =
  cfcFacts
{-# INLINE carrierFactCellFacts #-}

carrierFactFactsAt ::
  (Ord ctx, Ord carrier, Ord prop, BoundaryOps boundary) =>
  CarrierAddr ctx carrier prop ->
  CarrierStore ctx carrier prop boundary evidence ->
  FactAntichain ctx prop (CarrierCurrentFactEvidence carrier evidence) boundary
carrierFactFactsAt addr =
  maybe emptyFactAntichain cfcFacts . carrierFactCellAt addr
{-# INLINE carrierFactFactsAt #-}

carrierFactRowsAt ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CarrierAddr ctx carrier prop ->
  CarrierStore ctx carrier prop boundary evidence ->
  RowDelta
carrierFactRowsAt addr store =
  maybe
    emptyPlainRowPatch
    cfccRows
    (Map.lookup addr (cflCurrent (cvFacts (cstViews store))))
{-# INLINE carrierFactRowsAt #-}

carrierFactAddressesNow ::
  CarrierStore ctx carrier prop boundary evidence ->
  Set (CarrierAddr ctx carrier prop)
carrierFactAddressesNow =
  Map.keysSet . cflCurrent . cvFacts . cstViews
{-# INLINE carrierFactAddressesNow #-}

carrierFactContextsNow ::
  Ord ctx =>
  CarrierStore ctx carrier prop boundary evidence ->
  Set ctx
carrierFactContextsNow =
  Set.map caContext . carrierFactAddressesNow
{-# INLINE carrierFactContextsNow #-}


applyCarrierFactTrace ::
  (Ord ctx, Ord carrier, Ord prop) =>
  ContextLattice ctx ->
  CarrierTraceEntry ctx carrier prop boundary evidence ->
  Map (CarrierAddr ctx carrier prop) (CarrierSnapshot ctx carrier prop boundary evidence) ->
  CarrierFactLedger ctx carrier prop boundary evidence ->
  Either
    (CarrierStoreError ctx carrier prop boundary evidence)
    (CarrierFactLedger ctx carrier prop boundary evidence)
applyCarrierFactTrace latticeValue traceEntry currentSnapshots factsIndex0 = do
  let delta =
        cteDelta traceEntry
      addr =
        deAddr delta
      negativeRows =
        rowDeltaNegativePart (deRows delta)

  (factsWithoutRetractedContributions, retractedSeedIds) <-
    consumeNegativeFactRows addr negativeRows factsIndex0

  let (factsWithInsertedContributions, insertedSeedIds) =
        insertFactSeed traceEntry factsWithoutRetractedContributions
      touchedSeedIds =
        IntSet.union retractedSeedIds insertedSeedIds

  applyCarrierFactCurrentDelta
    latticeValue
    addr
    currentSnapshots
    (deRows delta)
    touchedSeedIds
    factsWithInsertedContributions
{-# INLINE applyCarrierFactTrace #-}

applyCarrierFactCurrentDelta ::
  (Ord ctx, Ord carrier, Ord prop) =>
  ContextLattice ctx ->
  CarrierAddr ctx carrier prop ->
  Map (CarrierAddr ctx carrier prop) (CarrierSnapshot ctx carrier prop boundary evidence) ->
  RowDelta ->
  IntSet ->
  CarrierFactLedger ctx carrier prop boundary evidence ->
  Either
    (CarrierStoreError ctx carrier prop boundary evidence)
    (CarrierFactLedger ctx carrier prop boundary evidence)
applyCarrierFactCurrentDelta latticeValue addr currentSnapshots rowsDelta touchedSeedIds factsIndex
  | Map.notMember addr currentSnapshots =
      Right
        factsIndex
          { cflCurrent = Map.delete addr (cflCurrent factsIndex)
          }
  | otherwise = do
      let oldCurrent =
            Map.findWithDefault
              emptyCarrierFactCurrentCell
              addr
              (cflCurrent factsIndex)

      nextRows <-
        applyPlainRowPatchWith
          (factCurrentUnderflow addr)
          rowsDelta
          (positivePlainRowPatchRows (cfccRows oldCurrent))

      let nextCurrentRows =
            plainRowPatchFromMultiplicityMap nextRows

      nextFactsBySeed <-
        Foldable.foldlM
          (refreshCurrentFact latticeValue factsIndex)
          (cfccFactsBySeed oldCurrent)
          (IntSet.toAscList touchedSeedIds)

      let nextCell =
            CarrierFactCurrentCell
              { cfccRows = nextCurrentRows,
                cfccFactsBySeed = nextFactsBySeed
              }
          nextCurrent =
            if plainRowPatchNull nextCurrentRows
              then Map.delete addr (cflCurrent factsIndex)
              else Map.insert addr nextCell (cflCurrent factsIndex)

      Right
        factsIndex
          { cflCurrent = nextCurrent
          }
{-# INLINE applyCarrierFactCurrentDelta #-}

emptyCarrierFactCurrentCell ::
  CarrierFactCurrentCell ctx carrier prop boundary evidence
emptyCarrierFactCurrentCell =
  CarrierFactCurrentCell
    { cfccRows = emptyPlainRowPatch,
      cfccFactsBySeed = IntMap.empty
    }
{-# INLINE emptyCarrierFactCurrentCell #-}

factCurrentUnderflow ::
  CarrierAddr ctx carrier prop ->
  RowTupleKey ->
  Multiplicity ->
  MultiplicityChange ->
  CarrierStoreError ctx carrier prop boundary evidence
factCurrentUnderflow addr rowValue oldMultiplicity deltaMultiplicity =
  CarrierStoreFactContributionUnderflow
    addr
    rowValue
    oldMultiplicity
    deltaMultiplicity
{-# INLINE factCurrentUnderflow #-}

refreshCurrentFact ::
  Ord ctx =>
  ContextLattice ctx ->
  CarrierFactLedger ctx carrier prop boundary evidence ->
  IntMap.IntMap (LocalFact ctx prop (CarrierCurrentFactEvidence carrier evidence) boundary) ->
  Int ->
  Either
    (CarrierStoreError ctx carrier prop boundary evidence)
    (IntMap.IntMap (LocalFact ctx prop (CarrierCurrentFactEvidence carrier evidence) boundary))
refreshCurrentFact latticeValue factsIndex factsBySeed seedKey = do
  maybeFact <-
    factForSeed latticeValue seedKey factsIndex
  pure $
    case maybeFact of
      Nothing ->
        IntMap.delete seedKey factsBySeed
      Just factValue ->
        IntMap.insert seedKey factValue factsBySeed
{-# INLINE refreshCurrentFact #-}

insertFactSeed ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CarrierTraceEntry ctx carrier prop boundary evidence ->
  CarrierFactLedger ctx carrier prop boundary evidence ->
  (CarrierFactLedger ctx carrier prop boundary evidence, IntSet)
insertFactSeed traceEntry factsIndex =
  let delta =
        cteDelta traceEntry
      addr =
        deAddr delta
      positiveRowsMap =
        Map.map
          positiveMultiplicityValue
          (rowDeltaPositivePart (deRows delta))
      positiveRows =
        plainRowPatchFromMultiplicityMap positiveRowsMap
      seedKey =
        traceIdKey (cteId traceEntry)
      seedSingleton =
        IntSet.singleton seedKey
      seed =
        CarrierFactSeed
          { cfsAddr = addr,
            cfsTraceId = cteId traceEntry,
            cfsSupport = deSupport delta,
            cfsBoundary = deBoundary delta,
            cfsPositiveRows = positiveRows,
            cfsEvidence = deEvidence delta
          }
   in if plainRowPatchNull positiveRows
        then (factsIndex, IntSet.empty)
        else
          ( factsIndex
              { cflSeeds =
                  IntMap.insert seedKey seed (cflSeeds factsIndex),
                cflLiveByAddr =
                  insertMapIndex
                    addr
                    seedSingleton
                    (cflLiveByAddr factsIndex),
                cflLiveByRow =
                  insertFactSeedRows
                    addr
                    seedKey
                    positiveRowsMap
                    (cflLiveByRow factsIndex),
                cflLiveRowsBySeed =
                  IntMap.insert seedKey positiveRows (cflLiveRowsBySeed factsIndex)
              },
            seedSingleton
          )
{-# INLINE insertFactSeed #-}

insertFactSeedRows ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CarrierAddr ctx carrier prop ->
  Int ->
  Map RowTupleKey Multiplicity ->
  Map (CarrierFactSeedRowKey ctx carrier prop) IntSet ->
  Map (CarrierFactSeedRowKey ctx carrier prop) IntSet
insertFactSeedRows addr seedKey positiveRows index =
  Map.foldlWithKey'
    ( \currentIndex rowValue _multiplicity ->
        Map.insertWith
          IntSet.union
          (addr, rowValue)
          (IntSet.singleton seedKey)
          currentIndex
    )
    index
    positiveRows
{-# INLINE insertFactSeedRows #-}

consumeNegativeFactRows ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CarrierAddr ctx carrier prop ->
  Map RowTupleKey PositiveMultiplicity ->
  CarrierFactLedger ctx carrier prop boundary evidence ->
  Either
    (CarrierStoreError ctx carrier prop boundary evidence)
    (CarrierFactLedger ctx carrier prop boundary evidence, IntSet)
consumeNegativeFactRows addr negativeRows factsIndex =
  Map.foldlWithKey'
    consumeOne
    (Right (factsIndex, IntSet.empty))
    negativeRows
  where
    consumeOne eitherState rowValue removedMultiplicity = do
      (factsAccum, touchedAccum) <- eitherState
      (factsNext, touchedNext) <-
        consumeNegativeFactRow
          addr
          rowValue
          (positiveMultiplicityValue removedMultiplicity)
          factsAccum
      Right (factsNext, IntSet.union touchedAccum touchedNext)
{-# INLINE consumeNegativeFactRows #-}

consumeNegativeFactRow ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CarrierAddr ctx carrier prop ->
  RowTupleKey ->
  Multiplicity ->
  CarrierFactLedger ctx carrier prop boundary evidence ->
  Either
    (CarrierStoreError ctx carrier prop boundary evidence)
    (CarrierFactLedger ctx carrier prop boundary evidence, IntSet)
consumeNegativeFactRow addr rowValue requestedMultiplicity factsIndex =
  let requestedCount =
        multiplicityValue requestedMultiplicity
      availableMultiplicity =
        availableFactRowMultiplicity addr rowValue factsIndex
   in if requestedCount <= 0
        then Right (factsIndex, IntSet.empty)
        else
          if multiplicityValue availableMultiplicity < requestedCount
            then
              Left
                ( CarrierStoreFactContributionUnderflow
                    addr
                    rowValue
                    availableMultiplicity
                    (negateMultiplicityChange (multiplicityAsChange requestedMultiplicity))
                )
            else
              Right
                ( consumeLiveFactRow
                    addr
                    rowValue
                    requestedCount
                    factsIndex
                )
{-# INLINE consumeNegativeFactRow #-}

consumeLiveFactRow ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CarrierAddr ctx carrier prop ->
  RowTupleKey ->
  Natural ->
  CarrierFactLedger ctx carrier prop boundary evidence ->
  (CarrierFactLedger ctx carrier prop boundary evidence, IntSet)
consumeLiveFactRow addr rowValue requestedCount factsIndex =
  let (_remaining, factsNext, touchedSeeds) =
        IntSet.foldl'
          consumeFromSeed
          (requestedCount, factsIndex, IntSet.empty)
          (liveSeedIdsForRow addr rowValue factsIndex)
   in (factsNext, touchedSeeds)
  where
    consumeFromSeed (!remaining, !factsAccum, !touchedAccum) seedKey
      | remaining <= 0 =
          (0, factsAccum, touchedAccum)
      | otherwise =
          let availableAtSeed =
                seedRowMultiplicity seedKey rowValue factsAccum
              availableCount =
                multiplicityValue availableAtSeed
              consumedCount =
                min remaining availableCount
           in if consumedCount <= 0
                then (remaining, factsAccum, touchedAccum)
                else
                  ( remaining - consumedCount,
                    decrementLiveSeedRow
                      addr
                      rowValue
                      seedKey
                      (Multiplicity consumedCount)
                      factsAccum,
                    IntSet.insert seedKey touchedAccum
                  )
{-# INLINE consumeLiveFactRow #-}

availableFactRowMultiplicity ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CarrierAddr ctx carrier prop ->
  RowTupleKey ->
  CarrierFactLedger ctx carrier prop boundary evidence ->
  Multiplicity
availableFactRowMultiplicity addr rowValue factsIndex =
  Multiplicity
    ( IntSet.foldl'
        ( \acc seedKey ->
            acc
              + multiplicityValue
                (seedRowMultiplicity seedKey rowValue factsIndex)
        )
        0
        (liveSeedIdsForRow addr rowValue factsIndex)
    )
{-# INLINE availableFactRowMultiplicity #-}

liveSeedIdsForRow ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CarrierAddr ctx carrier prop ->
  RowTupleKey ->
  CarrierFactLedger ctx carrier prop boundary evidence ->
  IntSet
liveSeedIdsForRow addr rowValue factsIndex =
  Map.findWithDefault
    IntSet.empty
    (addr, rowValue)
    (cflLiveByRow factsIndex)
{-# INLINE liveSeedIdsForRow #-}

seedRowMultiplicity ::
  Int ->
  RowTupleKey ->
  CarrierFactLedger ctx carrier prop boundary evidence ->
  Multiplicity
seedRowMultiplicity seedKey rowValue factsIndex =
  case IntMap.lookup seedKey (cflLiveRowsBySeed factsIndex) of
    Nothing ->
      zeroMultiplicity
    Just seedRows ->
      Map.findWithDefault
        zeroMultiplicity
        rowValue
        (positivePlainRowPatchRows seedRows)
{-# INLINE seedRowMultiplicity #-}

decrementLiveSeedRow ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CarrierAddr ctx carrier prop ->
  RowTupleKey ->
  Int ->
  Multiplicity ->
  CarrierFactLedger ctx carrier prop boundary evidence ->
  CarrierFactLedger ctx carrier prop boundary evidence
decrementLiveSeedRow addr rowValue seedKey removedMultiplicity factsIndex =
  case IntMap.lookup seedKey (cflLiveRowsBySeed factsIndex) of
    Nothing ->
      factsIndex
    Just seedRows0 ->
      let seedRowsMap0 =
            positivePlainRowPatchRows seedRows0
          oldMultiplicity =
            Map.findWithDefault zeroMultiplicity rowValue seedRowsMap0
          seedRowsMap1 =
            case subtractMultiplicity oldMultiplicity removedMultiplicity of
              Just nextMultiplicity
                | multiplicityValue nextMultiplicity > 0 ->
                    Map.insert rowValue nextMultiplicity seedRowsMap0
              _ ->
                Map.delete rowValue seedRowsMap0
          seedRows1 =
            plainRowPatchFromMultiplicityMap seedRowsMap1
          rowStillLive =
            Map.member rowValue seedRowsMap1
          seedStillLive =
            not (plainRowPatchNull seedRows1)
          factsWithSeedRows =
            factsIndex
              { cflLiveRowsBySeed =
                  alterIntMapNull
                    plainRowPatchNull
                    seedKey
                    seedRows1
                    (cflLiveRowsBySeed factsIndex)
              }
          factsWithRowIndex =
            if rowStillLive
              then factsWithSeedRows
              else
                factsWithSeedRows
                  { cflLiveByRow =
                      deleteMapIntSetIndex
                        (addr, rowValue)
                        seedKey
                        (cflLiveByRow factsWithSeedRows)
                  }
          factsWithAddrIndex =
            if seedStillLive
              then factsWithRowIndex
              else
                factsWithRowIndex
                  { cflLiveByAddr =
                      deleteMapIntSetIndex
                        addr
                        seedKey
                        (cflLiveByAddr factsWithRowIndex)
                  }
       in factsWithAddrIndex
{-# INLINE decrementLiveSeedRow #-}

deleteCarrierFactAddress ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CarrierAddr ctx carrier prop ->
  CarrierFactLedger ctx carrier prop boundary evidence ->
  CarrierFactLedger ctx carrier prop boundary evidence
deleteCarrierFactAddress addr factsIndex =
  let seedKeys =
        IntMap.keysSet (IntMap.filter ((== addr) . cfsAddr) (cflSeeds factsIndex))
   in factsIndex
        { cflSeeds = deleteIntMapKeys seedKeys (cflSeeds factsIndex),
          cflLiveByAddr = Map.delete addr (cflLiveByAddr factsIndex),
          cflLiveByRow = Map.filterWithKey (\(seedAddr, _rowValue) _ids -> seedAddr /= addr) (cflLiveByRow factsIndex),
          cflLiveRowsBySeed = deleteIntMapKeys seedKeys (cflLiveRowsBySeed factsIndex),
          cflCurrent = Map.delete addr (cflCurrent factsIndex)
        }
{-# INLINE deleteCarrierFactAddress #-}

factForSeed ::
  Ord ctx =>
  ContextLattice ctx ->
  Int ->
  CarrierFactLedger ctx carrier prop boundary evidence ->
  Either
    (CarrierStoreError ctx carrier prop boundary evidence)
    (Maybe (LocalFact ctx prop (CarrierCurrentFactEvidence carrier evidence) boundary))
factForSeed latticeValue seedKey factsIndex =
  case ( IntMap.lookup seedKey (cflSeeds factsIndex),
         IntMap.lookup seedKey (cflLiveRowsBySeed factsIndex)
       ) of
    (Just seed, Just currentRows) -> do
      let addr =
            cfsAddr seed
      address <-
        case mkLocalAddress latticeValue (caProp addr) (cfsSupport seed) of
          Right localAddress ->
            Right localAddress
          Left lookupError ->
            Left (CarrierStoreLatticeLookupFailed lookupError)
      pure
        ( Just
            ( mkLocalFact
                address
                (cfsBoundary seed)
                CarrierCurrentFactEvidence
                  { ccfeCarrier = caCarrier addr,
                    ccfeTraceId = cfsTraceId seed,
                    ccfeRows = positivePlainRowPatchRows currentRows,
                    ccfeEvidence = cfsEvidence seed
                  }
            )
        )
    _ ->
      Right Nothing
{-# INLINE factForSeed #-}

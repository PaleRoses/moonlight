{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Carrier.Fact.Internal.LedgerIndex
  ( CarrierCurrentFactEvidence (..),
    CarrierFactSeed (..),
    CarrierFactSeedRowKey,
    CarrierFactRuntime (..),
    CarrierFactCell (..),
    CarrierFactCurrentCell (..),
    CarrierFactLedger (..),
    CarrierFactSection (..),
    RestrictedCarrierFactSection (..),
    mkCarrierFactCell,
    carrierFactCurrentCellReadout,
    mkCarrierFactSection,
    insertCarrierFactCell,
    carrierFactSectionAddressSet,
    checkedSpliceCarrierFactAddress,
  )
where

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
import Moonlight.Differential.Context.Restriction
  ( ContextRestrictionEdge,
  )
import Moonlight.Differential.Fact.Local
  ( FactAntichain,
    LocalFact,
    antichainFromFacts,
    membersAntichain,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
    caContext,
  )
import Moonlight.Flow.Carrier.Morphism.Core.Program
  ( CarrierMorphismContext,
  )
import Moonlight.Differential.Trace.Id
  ( TraceId,
  )
import Moonlight.Differential.Row.Delta
  ( RowDelta
  )
import Moonlight.Delta.Signed
  ( Multiplicity,
    multiplicityValue,
    zeroMultiplicity
  )
import Moonlight.Differential.Row.Patch
  ( composePlainRowPatch,
    emptyPlainRowPatch,
    plainRowPatchNull,
    positivePlainRowPatchRows
  )
import Moonlight.Differential.Row.Tuple
  ( RowTupleKey,
  )
import Moonlight.FiniteLattice
  ( SupportBasis
  )
import Moonlight.FiniteLattice
  ( ContextLattice
  )


type CarrierCurrentFactEvidence :: Type -> Type -> Type
data CarrierCurrentFactEvidence carrier evidence = CarrierCurrentFactEvidence
  { ccfeCarrier :: !carrier,
    ccfeTraceId :: !TraceId,
    ccfeRows :: !(Map RowTupleKey Multiplicity),
    ccfeEvidence :: !evidence
  }
  deriving stock (Eq, Show)

type CarrierFactSeed :: Type -> Type -> Type -> Type -> Type -> Type
data CarrierFactSeed ctx carrier prop boundary evidence = CarrierFactSeed
  { cfsAddr :: !(CarrierAddr ctx carrier prop),
    cfsTraceId :: !TraceId,
    cfsSupport :: !(SupportBasis ctx),
    cfsBoundary :: !boundary,
    cfsPositiveRows :: !RowDelta,
    cfsEvidence :: !evidence
  }
  deriving stock (Eq, Show)

type CarrierFactSeedRowKey :: Type -> Type -> Type -> Type
type CarrierFactSeedRowKey ctx carrier prop =
  (CarrierAddr ctx carrier prop, RowTupleKey)

type CarrierFactRuntime :: Type -> Type -> Type -> Type -> Type
data CarrierFactRuntime ctx carrier prop boundary = CarrierFactRuntime
  { cfrLattice :: !(ContextLattice ctx),
    cfrMorphism :: !(CarrierMorphismContext ctx carrier prop boundary ())
  }

type CarrierFactCell :: Type -> Type -> Type -> Type -> Type -> Type
data CarrierFactCell ctx carrier prop boundary evidence = CarrierFactCell
  { cfcRows :: !RowDelta,
    cfcFacts :: !(FactAntichain ctx prop (CarrierCurrentFactEvidence carrier evidence) boundary)
  }
  deriving stock (Eq, Show)

type CarrierFactCurrentCell :: Type -> Type -> Type -> Type -> Type -> Type
data CarrierFactCurrentCell ctx carrier prop boundary evidence = CarrierFactCurrentCell
  { cfccRows :: !RowDelta,
    cfccFactsBySeed ::
      !(IntMap (LocalFact ctx prop (CarrierCurrentFactEvidence carrier evidence) boundary))
  }
  deriving stock (Eq, Show)

type CarrierFactLedger :: Type -> Type -> Type -> Type -> Type -> Type
data CarrierFactLedger ctx carrier prop boundary evidence = CarrierFactLedger
  { cflSeeds :: !(IntMap (CarrierFactSeed ctx carrier prop boundary evidence)),
    cflLiveByAddr :: !(Map (CarrierAddr ctx carrier prop) IntSet),
    cflLiveByRow :: !(Map (CarrierFactSeedRowKey ctx carrier prop) IntSet),
    cflLiveRowsBySeed :: !(IntMap RowDelta),
    cflCurrent :: !(Map (CarrierAddr ctx carrier prop) (CarrierFactCurrentCell ctx carrier prop boundary evidence))
  }
  deriving stock (Eq, Show)

type CarrierFactSection :: Type -> Type -> Type -> Type -> Type -> Type
data CarrierFactSection ctx carrier prop boundary evidence = CarrierFactSection
  { cfsContext :: !ctx,
    cfsCells :: !(Map (CarrierAddr ctx carrier prop) (CarrierFactCell ctx carrier prop boundary evidence))
  }
  deriving stock (Eq, Show)

type RestrictedCarrierFactSection :: Type -> Type -> Type -> Type -> Type -> Type
data RestrictedCarrierFactSection ctx carrier prop boundary evidence = RestrictedCarrierFactSection
  { rcfsEdge :: !(ContextRestrictionEdge ctx),
    rcfsProvenAddresses :: !(Set (CarrierAddr ctx carrier prop)),
    rcfsSection :: !(CarrierFactSection ctx carrier prop boundary evidence)
  }
  deriving stock (Eq, Show)

mkCarrierFactCell ::
  (Ord ctx, Ord prop, BoundaryOps boundary) =>
  RowDelta ->
  FactAntichain ctx prop (CarrierCurrentFactEvidence carrier evidence) boundary ->
  Maybe (CarrierFactCell ctx carrier prop boundary evidence)
mkCarrierFactCell rows facts =
  let normalizedFacts = antichainFromFacts (membersAntichain facts)
   in if plainRowPatchNull rows
        then Nothing
        else
          Just
            CarrierFactCell
              { cfcRows = rows,
                cfcFacts = normalizedFacts
              }
{-# INLINE mkCarrierFactCell #-}

carrierFactCurrentCellReadout ::
  (Ord ctx, Ord prop, BoundaryOps boundary) =>
  CarrierFactCurrentCell ctx carrier prop boundary evidence ->
  Maybe (CarrierFactCell ctx carrier prop boundary evidence)
carrierFactCurrentCellReadout currentCell =
  mkCarrierFactCell
    (cfccRows currentCell)
    (antichainFromFacts (IntMap.elems (cfccFactsBySeed currentCell)))
{-# INLINE carrierFactCurrentCellReadout #-}

mkCarrierFactSection ::
  (Ord ctx, Ord prop, BoundaryOps boundary) =>
  ctx ->
  Map (CarrierAddr ctx carrier prop) (CarrierFactCell ctx carrier prop boundary evidence) ->
  CarrierFactSection ctx carrier prop boundary evidence
mkCarrierFactSection contextValue cells =
  CarrierFactSection
    { cfsContext = contextValue,
      cfsCells = Map.filterWithKey (\addr _cell -> caContext addr == contextValue) (Map.mapMaybe normalizeCell cells)
    }
  where
    normalizeCell ::
      (Ord ctx, Ord prop, BoundaryOps boundary) =>
      CarrierFactCell ctx carrier prop boundary evidence ->
      Maybe (CarrierFactCell ctx carrier prop boundary evidence)
    normalizeCell cell = mkCarrierFactCell (cfcRows cell) (cfcFacts cell)
{-# INLINE mkCarrierFactSection #-}

insertCarrierFactCell ::
  (Ord ctx, Ord carrier, Ord prop, BoundaryOps boundary) =>
  CarrierAddr ctx carrier prop ->
  CarrierFactCell ctx carrier prop boundary evidence ->
  CarrierFactSection ctx carrier prop boundary evidence ->
  CarrierFactSection ctx carrier prop boundary evidence
insertCarrierFactCell addr cell section
  | caContext addr /= cfsContext section = section
  | otherwise =
      section
        { cfsCells =
            case mkCarrierFactCell (cfcRows cell) (cfcFacts cell) of
              Nothing -> Map.delete addr (cfsCells section)
              Just normalized -> Map.insert addr normalized (cfsCells section)
        }
{-# INLINE insertCarrierFactCell #-}

carrierFactSectionAddressSet ::
  CarrierFactSection ctx carrier prop boundary evidence ->
  Set (CarrierAddr ctx carrier prop)
carrierFactSectionAddressSet =
  Map.keysSet . cfsCells
{-# INLINE carrierFactSectionAddressSet #-}

checkedSpliceCarrierFactAddress ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CarrierAddr ctx carrier prop ->
  CarrierFactLedger ctx carrier prop boundary evidence ->
  CarrierFactLedger ctx carrier prop boundary evidence ->
  Maybe (CarrierFactLedger ctx carrier prop boundary evidence)
checkedSpliceCarrierFactAddress addr localLedger baseLedger =
  if localLedgerScopedToAddress addr localLedger && ledgersSeedDisjoint localLedger baseLedger
    then Just (mergeCarrierFactLedger localLedger baseLedger)
    else Nothing
{-# INLINE checkedSpliceCarrierFactAddress #-}

localLedgerScopedToAddress ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CarrierAddr ctx carrier prop ->
  CarrierFactLedger ctx carrier prop boundary evidence ->
  Bool
localLedgerScopedToAddress addr ledger =
  all ((== addr) . cfsAddr) (IntMap.elems (cflSeeds ledger))
    && Set.isSubsetOf (Map.keysSet (cflLiveByAddr ledger)) (Set.singleton addr)
    && all ((== addr) . fst) (Map.keys (cflLiveByRow ledger))
    && Set.isSubsetOf (Map.keysSet (cflCurrent ledger)) (Set.singleton addr)
    && liveSeedIndexesValid ledger
    && liveByAddrEntriesValid ledger
    && liveByRowEntriesValid ledger
    && liveRowsCoveredByIndexes ledger
    && currentRowsConsistent addr ledger
    && currentFactSeedsConsistent addr ledger
{-# INLINE localLedgerScopedToAddress #-}

liveSeedIndexesValid ::
  CarrierFactLedger ctx carrier prop boundary evidence ->
  Bool
liveSeedIndexesValid ledger =
  let seedKeys = IntMap.keysSet (cflSeeds ledger)
      rowSeedKeys = IntMap.keysSet (cflLiveRowsBySeed ledger)
      liveByAddrKeys = IntSet.unions (Map.elems (cflLiveByAddr ledger))
      liveByRowKeys = IntSet.unions (Map.elems (cflLiveByRow ledger))
   in rowSeedKeys `IntSet.isSubsetOf` seedKeys
        && liveByAddrKeys `IntSet.isSubsetOf` rowSeedKeys
        && liveByRowKeys `IntSet.isSubsetOf` rowSeedKeys
{-# INLINE liveSeedIndexesValid #-}

liveByAddrEntriesValid ::
  (Eq ctx, Eq carrier, Eq prop) =>
  CarrierFactLedger ctx carrier prop boundary evidence ->
  Bool
liveByAddrEntriesValid ledger =
  Map.foldlWithKey'
    ( \allValid addr seedIds ->
        allValid
          && IntSet.foldl'
            ( \seedsValid seedKey ->
                seedsValid
                  && case IntMap.lookup seedKey (cflSeeds ledger) of
                    Just seed ->
                      cfsAddr seed == addr
                        && IntMap.member seedKey (cflLiveRowsBySeed ledger)
                    Nothing ->
                      False
            )
            True
            seedIds
    )
    True
    (cflLiveByAddr ledger)
{-# INLINE liveByAddrEntriesValid #-}

liveByRowEntriesValid ::
  (Eq ctx, Eq carrier, Eq prop) =>
  CarrierFactLedger ctx carrier prop boundary evidence ->
  Bool
liveByRowEntriesValid ledger =
  Map.foldlWithKey'
    ( \allValid (addr, rowValue) seedIds ->
        allValid
          && IntSet.foldl'
            ( \seedsValid seedKey ->
                seedsValid
                  && case IntMap.lookup seedKey (cflSeeds ledger) of
                    Just seed ->
                      cfsAddr seed == addr
                        && positiveSeedRowMultiplicity seedKey rowValue ledger
                    Nothing ->
                      False
            )
            True
            seedIds
    )
    True
    (cflLiveByRow ledger)
{-# INLINE liveByRowEntriesValid #-}

liveRowsCoveredByIndexes ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CarrierFactLedger ctx carrier prop boundary evidence ->
  Bool
liveRowsCoveredByIndexes ledger =
  IntMap.foldlWithKey'
    ( \allValid seedKey rows ->
        allValid
          && case IntMap.lookup seedKey (cflSeeds ledger) of
            Nothing ->
              False
            Just seed ->
              let addr = cfsAddr seed
               in IntSet.member seedKey (Map.findWithDefault IntSet.empty addr (cflLiveByAddr ledger))
                    && Map.foldlWithKey'
                      ( \rowsValid rowValue multiplicity ->
                          rowsValid
                            && multiplicityValue multiplicity > 0
                            && IntSet.member
                              seedKey
                              (Map.findWithDefault IntSet.empty (addr, rowValue) (cflLiveByRow ledger))
                      )
                      True
                      (positivePlainRowPatchRows rows)
    )
    True
    (cflLiveRowsBySeed ledger)
{-# INLINE liveRowsCoveredByIndexes #-}

positiveSeedRowMultiplicity ::
  Int ->
  RowTupleKey ->
  CarrierFactLedger ctx carrier prop boundary evidence ->
  Bool
positiveSeedRowMultiplicity seedKey rowValue ledger =
  case IntMap.lookup seedKey (cflLiveRowsBySeed ledger) of
    Nothing ->
      False
    Just rows ->
      multiplicityValue (Map.findWithDefault zeroMultiplicity rowValue (positivePlainRowPatchRows rows)) > 0
{-# INLINE positiveSeedRowMultiplicity #-}

currentRowsConsistent ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CarrierAddr ctx carrier prop ->
  CarrierFactLedger ctx carrier prop boundary evidence ->
  Bool
currentRowsConsistent addr ledger =
  let rows = foldedLiveRowsAt addr ledger
   in case (plainRowPatchNull rows, Map.lookup addr (cflCurrent ledger)) of
        (True, Nothing) ->
          True
        (False, Just cell) ->
          cfccRows cell == rows
        _ ->
          False
{-# INLINE currentRowsConsistent #-}

currentFactSeedsConsistent ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CarrierAddr ctx carrier prop ->
  CarrierFactLedger ctx carrier prop boundary evidence ->
  Bool
currentFactSeedsConsistent addr ledger =
  case Map.lookup addr (cflCurrent ledger) of
    Nothing ->
      True
    Just cell ->
      IntMap.keysSet (cfccFactsBySeed cell)
        == Map.findWithDefault IntSet.empty addr (cflLiveByAddr ledger)
{-# INLINE currentFactSeedsConsistent #-}

foldedLiveRowsAt ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CarrierAddr ctx carrier prop ->
  CarrierFactLedger ctx carrier prop boundary evidence ->
  RowDelta
foldedLiveRowsAt addr ledger =
  IntSet.foldl'
    ( \rows seedKey ->
        case IntMap.lookup seedKey (cflLiveRowsBySeed ledger) of
          Nothing -> rows
          Just seedRows -> composePlainRowPatch seedRows rows
    )
    emptyPlainRowPatch
    (Map.findWithDefault IntSet.empty addr (cflLiveByAddr ledger))
{-# INLINE foldedLiveRowsAt #-}

ledgersSeedDisjoint ::
  CarrierFactLedger ctx carrier prop boundary evidence ->
  CarrierFactLedger ctx carrier prop boundary evidence ->
  Bool
ledgersSeedDisjoint left right =
  IntSet.null (IntSet.intersection (IntMap.keysSet (cflSeeds left)) (IntMap.keysSet (cflSeeds right)))
{-# INLINE ledgersSeedDisjoint #-}

mergeCarrierFactLedger ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CarrierFactLedger ctx carrier prop boundary evidence ->
  CarrierFactLedger ctx carrier prop boundary evidence ->
  CarrierFactLedger ctx carrier prop boundary evidence
mergeCarrierFactLedger localLedger baseLedger =
  baseLedger
    { cflSeeds = IntMap.union (cflSeeds localLedger) (cflSeeds baseLedger),
      cflLiveByAddr = Map.unionWith IntSet.union (cflLiveByAddr localLedger) (cflLiveByAddr baseLedger),
      cflLiveByRow = Map.unionWith IntSet.union (cflLiveByRow localLedger) (cflLiveByRow baseLedger),
      cflLiveRowsBySeed = IntMap.union (cflLiveRowsBySeed localLedger) (cflLiveRowsBySeed baseLedger),
      cflCurrent = Map.union (cflCurrent localLedger) (cflCurrent baseLedger)
    }
{-# INLINE mergeCarrierFactLedger #-}

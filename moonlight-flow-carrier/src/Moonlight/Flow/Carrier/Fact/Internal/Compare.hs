{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TupleSections #-}

module Moonlight.Flow.Carrier.Fact.Internal.Compare
  ( CarrierFactComparison (..),
    compareCarrierFactSections,
    carrierFactComparisonEmpty,
    comparisonConflictsOutside,
    carrierFactCellObstructions,
  )
where

import Data.Kind
  ( Type,
  )
import Data.List.NonEmpty
  ( NonEmpty (..),
  )
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Maybe
  ( isNothing,
    mapMaybe,
  )
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Moonlight.Core
  ( BoundaryOps,
  )
import Moonlight.Differential.Fact.Local
  ( LocalFact,
    LocalFactObstruction,
    compatibleFacts,
    membersAntichain,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
  )
import Moonlight.Flow.Carrier.Fact.Internal.LedgerIndex
  ( CarrierFactCell (..),
    CarrierFactSection (..),
  )
import Moonlight.Differential.Row.Delta
  ( RowDelta
  )

type CarrierFactComparison :: Type -> Type -> Type -> Type -> Type
data CarrierFactComparison ctx carrier prop boundary = CarrierFactComparison
  { cfcContextMismatch :: !(Maybe (ctx, ctx)),
    cfcLeftOnly :: !(Set (CarrierAddr ctx carrier prop)),
    cfcRightOnly :: !(Set (CarrierAddr ctx carrier prop)),
    cfcRowConflicts :: !(Map (CarrierAddr ctx carrier prop) (RowDelta, RowDelta)),
    cfcFactConflicts :: !(Map (CarrierAddr ctx carrier prop) (NonEmpty (LocalFactObstruction ctx boundary)))
  }
  deriving stock (Eq, Show)

compareCarrierFactSections ::
  (Ord ctx, Ord carrier, Ord prop, BoundaryOps boundary) =>
  CarrierFactSection ctx carrier prop boundary evidence ->
  CarrierFactSection ctx carrier prop boundary evidence ->
  CarrierFactComparison ctx carrier prop boundary
compareCarrierFactSections leftSection rightSection =
  CarrierFactComparison
    { cfcContextMismatch =
        if cfsContext leftSection == cfsContext rightSection
          then Nothing
          else Just (cfsContext leftSection, cfsContext rightSection),
      cfcLeftOnly = Set.difference leftAddresses rightAddresses,
      cfcRightOnly = Set.difference rightAddresses leftAddresses,
      cfcRowConflicts = Map.fromList (mapMaybe rowConflictAt commonAddresses),
      cfcFactConflicts = Map.fromList (mapMaybe factConflictAt commonAddresses)
    }
  where
    leftCells = cfsCells leftSection
    rightCells = cfsCells rightSection
    leftAddresses = Map.keysSet leftCells
    rightAddresses = Map.keysSet rightCells
    commonAddresses = Set.toAscList (Set.intersection leftAddresses rightAddresses)
    rowConflictAt address = do
      leftCell <- Map.lookup address leftCells
      rightCell <- Map.lookup address rightCells
      if cfcRows leftCell == cfcRows rightCell
        then Nothing
        else Just (address, (cfcRows leftCell, cfcRows rightCell))
    factConflictAt address = do
      leftCell <- Map.lookup address leftCells
      rightCell <- Map.lookup address rightCells
      fmap (address,) (NonEmpty.nonEmpty (carrierFactCellObstructions leftCell rightCell))
{-# INLINE compareCarrierFactSections #-}

carrierFactComparisonEmpty ::
  CarrierFactComparison ctx carrier prop boundary ->
  Bool
carrierFactComparisonEmpty comparison =
  isNothing (cfcContextMismatch comparison)
    && Set.null (cfcLeftOnly comparison)
    && Set.null (cfcRightOnly comparison)
    && Map.null (cfcRowConflicts comparison)
    && Map.null (cfcFactConflicts comparison)
{-# INLINE carrierFactComparisonEmpty #-}

comparisonConflictsOutside ::
  Ord (CarrierAddr ctx carrier prop) =>
  Set (CarrierAddr ctx carrier prop) ->
  CarrierFactComparison ctx carrier prop boundary ->
  CarrierFactComparison ctx carrier prop boundary
comparisonConflictsOutside proven comparison =
  comparison
    { cfcLeftOnly = Set.empty,
      cfcRightOnly = Set.empty,
      cfcRowConflicts = Map.withoutKeys (cfcRowConflicts comparison) proven,
      cfcFactConflicts = Map.withoutKeys (cfcFactConflicts comparison) proven
    }
{-# INLINE comparisonConflictsOutside #-}

carrierFactCellObstructions ::
  (Ord ctx, BoundaryOps boundary) =>
  CarrierFactCell ctx carrier prop boundary evidence ->
  CarrierFactCell ctx carrier prop boundary evidence ->
  [LocalFactObstruction ctx boundary]
carrierFactCellObstructions leftCell rightCell =
  concatMap
    (\leftFact -> mapMaybe (factObstruction leftFact) (membersAntichain (cfcFacts rightCell)))
    (membersAntichain (cfcFacts leftCell))
{-# INLINE carrierFactCellObstructions #-}

factObstruction ::
  (Ord ctx, BoundaryOps boundary) =>
  LocalFact ctx prop evidence boundary ->
  LocalFact ctx prop evidence boundary ->
  Maybe (LocalFactObstruction ctx boundary)
factObstruction leftFact rightFact =
  case compatibleFacts leftFact rightFact of
    Left obstruction -> Just obstruction
    Right _compatibility -> Nothing
{-# INLINE factObstruction #-}

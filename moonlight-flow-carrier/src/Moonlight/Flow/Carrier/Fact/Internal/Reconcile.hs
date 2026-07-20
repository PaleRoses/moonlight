{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Carrier.Fact.Internal.Reconcile
  ( CarrierFactMergeError (..),
    CarrierFactCommonError (..),
    CarrierFactReconcileError (..),
    mergeCarrierFactSections,
    commonCarrierFactSection,
    reconcileCarrierFactRestriction,
    reconcileCarrierFactSectionWithProof,
  )
where

import Control.Monad
  ( foldM,
  )
import Data.Kind
  ( Type,
  )
import Data.List qualified as List
import Data.List.NonEmpty
  ( NonEmpty (..),
  )
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Maybe
  ( mapMaybe,
  )
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Moonlight.Core
  ( BoundaryOps,
  )
import Moonlight.Differential.Context.Restriction
  ( ContextRestrictionEdge (..),
  )
import Moonlight.Differential.Fact.Local
  ( LocalFactObstruction,
    mergeAntichains,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
  )
import Moonlight.Flow.Carrier.Fact.Internal.Compare
  ( CarrierFactComparison (..),
    carrierFactCellObstructions,
    carrierFactComparisonEmpty,
    compareCarrierFactSections,
    comparisonConflictsOutside,
  )
import Moonlight.Flow.Carrier.Fact.Internal.LedgerIndex
  ( CarrierFactCell (..),
    CarrierFactRuntime,
    CarrierFactSection (..),
    RestrictedCarrierFactSection (..),
    mkCarrierFactCell,
    mkCarrierFactSection,
  )
import Moonlight.Flow.Carrier.Fact.Internal.Restrict
  ( CarrierFactRestrictionError,
  )
import Moonlight.Differential.Row.Delta
  ( RowDelta
  )

type CarrierFactMergeError :: Type -> Type -> Type -> Type -> Type
data CarrierFactMergeError ctx carrier prop boundary
  = CarrierFactMergeContextMismatch !ctx !ctx
  | CarrierFactMergeRowConflict
      !(CarrierAddr ctx carrier prop)
      !RowDelta
      !RowDelta
  | CarrierFactMergeFactConflict
      !(CarrierAddr ctx carrier prop)
      !(NonEmpty (LocalFactObstruction ctx boundary))
  deriving stock (Eq, Show)

type CarrierFactCommonError :: Type -> Type -> Type -> Type -> Type
data CarrierFactCommonError ctx carrier prop boundary
  = CarrierFactCommonContextMismatch !ctx !ctx
  | CarrierFactNoCommonAddresses
  | CarrierFactNoCommonRows !(CarrierAddr ctx carrier prop)
  | CarrierFactCommonFactConflict
      !(CarrierAddr ctx carrier prop)
      !(NonEmpty (LocalFactObstruction ctx boundary))
  deriving stock (Eq, Show)

type CarrierFactReconcileError :: Type -> Type -> Type -> Type -> Type
data CarrierFactReconcileError ctx carrier prop boundary
  = CarrierFactReconcileRestrictionFailed
      !(NonEmpty (CarrierFactRestrictionError ctx carrier prop boundary))
  | CarrierFactReconcileContextMismatch !ctx !ctx
  | CarrierFactReconcileUnresolvedComparison
      !(CarrierFactComparison ctx carrier prop boundary)
  | CarrierFactReconcileMissingRestrictionProof
      !(NonEmpty (CarrierAddr ctx carrier prop))
  deriving stock (Eq, Show)

mergeCarrierFactSections ::
  (Ord ctx, Ord carrier, Ord prop, BoundaryOps boundary) =>
  NonEmpty (CarrierFactSection ctx carrier prop boundary evidence) ->
  Either
    (NonEmpty (CarrierFactMergeError ctx carrier prop boundary))
    (CarrierFactSection ctx carrier prop boundary evidence)
mergeCarrierFactSections (firstSection :| restSections) =
  foldM mergeCarrierFactSection firstSection restSections
{-# INLINE mergeCarrierFactSections #-}

mergeCarrierFactSection ::
  (Ord ctx, Ord carrier, Ord prop, BoundaryOps boundary) =>
  CarrierFactSection ctx carrier prop boundary evidence ->
  CarrierFactSection ctx carrier prop boundary evidence ->
  Either
    (NonEmpty (CarrierFactMergeError ctx carrier prop boundary))
    (CarrierFactSection ctx carrier prop boundary evidence)
mergeCarrierFactSection leftSection rightSection
  | cfsContext leftSection /= cfsContext rightSection =
      Left (CarrierFactMergeContextMismatch (cfsContext leftSection) (cfsContext rightSection) :| [])
  | otherwise =
      case Map.foldlWithKey' mergeSectionCell (Right (cfsCells leftSection)) (cfsCells rightSection) of
        Left errors -> Left errors
        Right mergedCells -> Right (mkCarrierFactSection (cfsContext leftSection) mergedCells)
{-# INLINE mergeCarrierFactSection #-}

mergeSectionCell ::
  (Ord ctx, Ord carrier, Ord prop, BoundaryOps boundary) =>
  Either
    (NonEmpty (CarrierFactMergeError ctx carrier prop boundary))
    (Map (CarrierAddr ctx carrier prop) (CarrierFactCell ctx carrier prop boundary evidence)) ->
  CarrierAddr ctx carrier prop ->
  CarrierFactCell ctx carrier prop boundary evidence ->
  Either
    (NonEmpty (CarrierFactMergeError ctx carrier prop boundary))
    (Map (CarrierAddr ctx carrier prop) (CarrierFactCell ctx carrier prop boundary evidence))
mergeSectionCell eitherCells address rightCell = do
  cells <- eitherCells
  case Map.lookup address cells of
    Nothing -> Right (Map.insert address rightCell cells)
    Just leftCell -> do
      mergedCell <- mergeCarrierFactCells address leftCell rightCell
      Right (Map.insert address mergedCell cells)
{-# INLINE mergeSectionCell #-}

mergeCarrierFactCells ::
  (Ord ctx, Ord prop, BoundaryOps boundary) =>
  CarrierAddr ctx carrier prop ->
  CarrierFactCell ctx carrier prop boundary evidence ->
  CarrierFactCell ctx carrier prop boundary evidence ->
  Either
    (NonEmpty (CarrierFactMergeError ctx carrier prop boundary))
    (CarrierFactCell ctx carrier prop boundary evidence)
mergeCarrierFactCells address leftCell rightCell
  | cfcRows leftCell /= cfcRows rightCell =
      Left (CarrierFactMergeRowConflict address (cfcRows leftCell) (cfcRows rightCell) :| [])
  | otherwise =
      case NonEmpty.nonEmpty (carrierFactCellObstructions leftCell rightCell) of
        Just obstructions -> Left (CarrierFactMergeFactConflict address obstructions :| [])
        Nothing ->
          case mkCarrierFactCell (cfcRows leftCell) (mergeAntichains (cfcFacts leftCell) (cfcFacts rightCell)) of
            Just mergedCell -> Right mergedCell
            Nothing -> Left (CarrierFactMergeRowConflict address (cfcRows leftCell) (cfcRows rightCell) :| [])
{-# INLINE mergeCarrierFactCells #-}

commonCarrierFactSection ::
  (Ord ctx, Ord carrier, Ord prop, BoundaryOps boundary) =>
  NonEmpty (CarrierFactSection ctx carrier prop boundary evidence) ->
  Either
    (CarrierFactCommonError ctx carrier prop boundary)
    (CarrierFactSection ctx carrier prop boundary evidence)
commonCarrierFactSection sections@(firstSection :| restSections) = do
  ensureCommonContexts firstSection restSections
  case Set.toAscList (commonAddresses sections) of
    [] -> Left CarrierFactNoCommonAddresses
    firstAddress : restAddresses -> do
      retainedCells <- traverse (commonCellAt sections) (firstAddress : restAddresses)
      let retainedMap = Map.fromList (mapMaybe id retainedCells)
      if Map.null retainedMap
        then Left (CarrierFactNoCommonRows firstAddress)
        else Right (mkCarrierFactSection (cfsContext firstSection) retainedMap)
{-# INLINE commonCarrierFactSection #-}

ensureCommonContexts ::
  Eq ctx =>
  CarrierFactSection ctx carrier prop boundary evidence ->
  [CarrierFactSection ctx carrier prop boundary evidence] ->
  Either (CarrierFactCommonError ctx carrier prop boundary) ()
ensureCommonContexts firstSection =
  foldM
    (\() section ->
      if cfsContext firstSection == cfsContext section
        then Right ()
        else Left (CarrierFactCommonContextMismatch (cfsContext firstSection) (cfsContext section))
    )
    ()
{-# INLINE ensureCommonContexts #-}

commonAddresses ::
  Ord (CarrierAddr ctx carrier prop) =>
  NonEmpty (CarrierFactSection ctx carrier prop boundary evidence) ->
  Set (CarrierAddr ctx carrier prop)
commonAddresses (firstSection :| restSections) =
  List.foldl' Set.intersection (Map.keysSet (cfsCells firstSection)) (fmap (Map.keysSet . cfsCells) restSections)
{-# INLINE commonAddresses #-}

commonCellAt ::
  (Ord ctx, Ord carrier, Ord prop, BoundaryOps boundary) =>
  NonEmpty (CarrierFactSection ctx carrier prop boundary evidence) ->
  CarrierAddr ctx carrier prop ->
  Either
    (CarrierFactCommonError ctx carrier prop boundary)
    (Maybe (CarrierAddr ctx carrier prop, CarrierFactCell ctx carrier prop boundary evidence))
commonCellAt sections address =
  let cells = mapMaybe (Map.lookup address . cfsCells) (NonEmpty.toList sections)
   in case cells of
        firstCell : restCells ->
          if all ((== cfcRows firstCell) . cfcRows) restCells
            then do
              mergedCell <- foldM (mergeCommonCell address) firstCell restCells
              Right (Just (address, mergedCell))
            else Right Nothing
        [] -> Right Nothing
{-# INLINE commonCellAt #-}

mergeCommonCell ::
  (Ord ctx, Ord prop, BoundaryOps boundary) =>
  CarrierAddr ctx carrier prop ->
  CarrierFactCell ctx carrier prop boundary evidence ->
  CarrierFactCell ctx carrier prop boundary evidence ->
  Either
    (CarrierFactCommonError ctx carrier prop boundary)
    (CarrierFactCell ctx carrier prop boundary evidence)
mergeCommonCell address leftCell rightCell =
  case NonEmpty.nonEmpty (carrierFactCellObstructions leftCell rightCell) of
    Just obstructions -> Left (CarrierFactCommonFactConflict address obstructions)
    Nothing ->
      case mkCarrierFactCell (cfcRows leftCell) (mergeAntichains (cfcFacts leftCell) (cfcFacts rightCell)) of
        Just mergedCell -> Right mergedCell
        Nothing -> Right leftCell
{-# INLINE mergeCommonCell #-}

reconcileCarrierFactRestriction ::
  (Ord ctx, Ord carrier, Ord prop, BoundaryOps boundary) =>
  CarrierFactRuntime ctx carrier prop boundary ->
  RestrictedCarrierFactSection ctx carrier prop boundary evidence ->
  CarrierFactSection ctx carrier prop boundary evidence ->
  Either
    (CarrierFactReconcileError ctx carrier prop boundary)
    (CarrierFactSection ctx carrier prop boundary evidence)
reconcileCarrierFactRestriction _runtime restricted targetSection
  | cfsContext restrictedSection /= creTargetContext (rcfsEdge restricted) =
      Left (CarrierFactReconcileContextMismatch (creTargetContext (rcfsEdge restricted)) (cfsContext restrictedSection))
  | cfsContext restrictedSection /= cfsContext targetSection =
      Left (CarrierFactReconcileContextMismatch (cfsContext restrictedSection) (cfsContext targetSection))
  | otherwise = do
      let proofSet = rcfsProvenAddresses restricted
          comparison = compareCarrierFactSections restrictedSection targetSection
          missingProof = Set.toAscList (Set.difference (Map.keysSet (cfsCells restrictedSection)) proofSet)
          unresolved = comparisonConflictsOutside proofSet comparison
      case missingProof of
        proofAddress : proofTail -> Left (CarrierFactReconcileMissingRestrictionProof (proofAddress :| proofTail))
        [] ->
          if carrierFactComparisonEmpty unresolved
            then
              Right
                ( mkCarrierFactSection
                    (cfsContext targetSection)
                    (Map.union (cfsCells restrictedSection) (Map.withoutKeys (cfsCells targetSection) proofSet))
                )
            else Left (CarrierFactReconcileUnresolvedComparison unresolved)
  where
    restrictedSection = rcfsSection restricted
{-# INLINE reconcileCarrierFactRestriction #-}

reconcileCarrierFactSectionWithProof ::
  (Ord ctx, Ord carrier, Ord prop, BoundaryOps boundary) =>
  CarrierFactRuntime ctx carrier prop boundary ->
  ContextRestrictionEdge ctx ->
  Set (CarrierAddr ctx carrier prop) ->
  CarrierFactSection ctx carrier prop boundary evidence ->
  CarrierFactSection ctx carrier prop boundary evidence ->
  Either
    (CarrierFactReconcileError ctx carrier prop boundary)
    (CarrierFactSection ctx carrier prop boundary evidence)
reconcileCarrierFactSectionWithProof runtime edge proofSet restrictedSection =
  reconcileCarrierFactRestriction
    runtime
    RestrictedCarrierFactSection
      { rcfsEdge = edge,
        rcfsProvenAddresses = proofSet,
        rcfsSection = restrictedSection
      }
{-# INLINE reconcileCarrierFactSectionWithProof #-}

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Carrier.Fact.Internal.Restrict
  ( CarrierFactRestrictionError (..),
    restrictCarrierFactSection,
  )
where

import Data.IntMap.Strict
  ( IntMap,
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
  ( LocalFact,
    lfBoundary,
    lfEvidence,
    mkLocalFact,
    emptyFactAntichain,
    insertAntichain,
    membersAntichain,
    mkLocalAddress,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
    caContext,
    caProp,
    caCarrier,
    rkTarget,
  )
import Moonlight.Flow.Carrier.Fact.Internal.LedgerIndex
  ( CarrierCurrentFactEvidence (..),
    CarrierFactCell (..),
    CarrierFactRuntime (..),
    CarrierFactSection (..),
    RestrictedCarrierFactSection (..),
    mkCarrierFactCell,
    mkCarrierFactSection,
  )
import Moonlight.Flow.Carrier.Morphism.Core.Program
  ( CarrierMorphismContext,
    carrierMorphismRestrictionsBetweenFrom,
  )
import Moonlight.Flow.Carrier.Morphism.Restriction
  ( CompiledCarrierRestriction (..),
    RestrictionDeltaError,
  )
import Moonlight.Differential.Row.Delta
  ( RowDelta
  )
import Moonlight.Differential.Row.Patch
  ( composePlainRowPatch,
    emptyPlainRowPatch,
    mapPlainRowPatchRows,
    plainRowPatchFromMultiplicityMap,
    plainRowPatchNull,
    positivePlainRowPatchRows,
  )
import Moonlight.Differential.Row.Tuple
import Moonlight.FiniteLattice
  ( ContextLattice,
    ContextLatticeLookupError
  )
import Moonlight.FiniteLattice
  ( principalSupport
  )

type CarrierFactRestrictionError :: Type -> Type -> Type -> Type -> Type
data CarrierFactRestrictionError ctx carrier prop boundary
  = CarrierFactRestrictionContextMismatch
      !(ContextRestrictionEdge ctx)
      !ctx
  | CarrierFactMissingRestrictionProgram
      !(ContextRestrictionEdge ctx)
      !(CarrierAddr ctx carrier prop)
  | CarrierFactBoundaryRestrictionFailed
      !(ContextRestrictionEdge ctx)
      !(CarrierAddr ctx carrier prop)
      !RestrictionDeltaError
  | CarrierFactLatticeLookupFailed
      !(ContextRestrictionEdge ctx)
      !(CarrierAddr ctx carrier prop)
      !(ContextLatticeLookupError ctx)
  deriving stock (Eq, Show)

data RestrictAccum ctx carrier prop boundary evidence = RestrictAccum
  { raCells :: !(Map (CarrierAddr ctx carrier prop) (CarrierFactCell ctx carrier prop boundary evidence)),
    raProof :: !(Set (CarrierAddr ctx carrier prop)),
    raErrors :: ![CarrierFactRestrictionError ctx carrier prop boundary]
  }

restrictCarrierFactSection ::
  (Ord ctx, Ord carrier, Ord prop, BoundaryOps boundary) =>
  CarrierFactRuntime ctx carrier prop boundary ->
  ContextRestrictionEdge ctx ->
  CarrierFactSection ctx carrier prop boundary evidence ->
  Either
    (NonEmpty (CarrierFactRestrictionError ctx carrier prop boundary))
    (RestrictedCarrierFactSection ctx carrier prop boundary evidence)
restrictCarrierFactSection runtime edge section
  | cfsContext section /= creSourceContext edge =
      Left (CarrierFactRestrictionContextMismatch edge (cfsContext section) :| [])
  | creSourceContext edge == creTargetContext edge =
      Right
        RestrictedCarrierFactSection
          { rcfsEdge = edge,
            rcfsProvenAddresses = Map.keysSet (cfsCells section),
            rcfsSection = section
          }
  | otherwise =
      restrictNonIdentity (cfrLattice runtime) (cfrMorphism runtime) edge section
{-# INLINE restrictCarrierFactSection #-}

restrictNonIdentity ::
  (Ord ctx, Ord carrier, Ord prop, BoundaryOps boundary) =>
  ContextLattice ctx ->
  CarrierMorphismContext ctx carrier prop boundary () ->
  ContextRestrictionEdge ctx ->
  CarrierFactSection ctx carrier prop boundary evidence ->
  Either
    (NonEmpty (CarrierFactRestrictionError ctx carrier prop boundary))
    (RestrictedCarrierFactSection ctx carrier prop boundary evidence)
restrictNonIdentity latticeValue restrictionGraph edge section =
  let accum =
        Map.foldlWithKey'
          (restrictAddress latticeValue restrictionGraph edge)
          emptyRestrictAccum
          (cfsCells section)
   in case NonEmpty.nonEmpty (reverse (raErrors accum)) of
        Just errors -> Left errors
        Nothing ->
          Right
            RestrictedCarrierFactSection
              { rcfsEdge = edge,
                rcfsProvenAddresses = raProof accum,
                rcfsSection = mkCarrierFactSection (creTargetContext edge) (raCells accum)
              }
{-# INLINE restrictNonIdentity #-}

emptyRestrictAccum :: RestrictAccum ctx carrier prop boundary evidence
emptyRestrictAccum =
  RestrictAccum
    { raCells = Map.empty,
      raProof = Set.empty,
      raErrors = []
    }
{-# INLINE emptyRestrictAccum #-}

restrictAddress ::
  (Ord ctx, Ord carrier, Ord prop, BoundaryOps boundary) =>
  ContextLattice ctx ->
  CarrierMorphismContext ctx carrier prop boundary () ->
  ContextRestrictionEdge ctx ->
  RestrictAccum ctx carrier prop boundary evidence ->
  CarrierAddr ctx carrier prop ->
  CarrierFactCell ctx carrier prop boundary evidence ->
  RestrictAccum ctx carrier prop boundary evidence
restrictAddress latticeValue restrictionGraph edge accum sourceAddress sourceCell =
  case carrierMorphismRestrictionsBetweenFrom (creSourceContext edge) (creTargetContext edge) sourceAddress restrictionGraph of
    [] -> addRestrictionError (CarrierFactMissingRestrictionProgram edge sourceAddress) accum
    programs -> List.foldl' (applyRestrictionProgram latticeValue edge sourceAddress sourceCell) accum programs
{-# INLINE restrictAddress #-}

applyRestrictionProgram ::
  (Ord ctx, Ord carrier, Ord prop, BoundaryOps boundary) =>
  ContextLattice ctx ->
  ContextRestrictionEdge ctx ->
  CarrierAddr ctx carrier prop ->
  CarrierFactCell ctx carrier prop boundary evidence ->
  RestrictAccum ctx carrier prop boundary evidence ->
  CompiledCarrierRestriction ctx carrier prop boundary ->
  RestrictAccum ctx carrier prop boundary evidence
applyRestrictionProgram latticeValue edge sourceAddress sourceCell accum program =
  let targetAddress = rkTarget (ccrKey program)
      targetRows = restrictRowDelta (ccrTargetClasses program) (cfcRows sourceCell)
      accumWithProof = accum {raProof = Set.insert targetAddress (raProof accum)}
      accumWithRows = insertRowsInAccum targetAddress targetRows accumWithProof
   in if plainRowPatchNull targetRows
        then accumWithRows
        else
          List.foldl'
            (restrictFactWithProgram latticeValue edge sourceAddress program targetRows)
            accumWithRows
            (membersAntichain (cfcFacts sourceCell))
{-# INLINE applyRestrictionProgram #-}

restrictFactWithProgram ::
  (Ord ctx, Ord carrier, Ord prop, BoundaryOps boundary) =>
  ContextLattice ctx ->
  ContextRestrictionEdge ctx ->
  CarrierAddr ctx carrier prop ->
  CompiledCarrierRestriction ctx carrier prop boundary ->
  RowDelta ->
  RestrictAccum ctx carrier prop boundary evidence ->
  LocalFact ctx prop (CarrierCurrentFactEvidence carrier evidence) boundary ->
  RestrictAccum ctx carrier prop boundary evidence
restrictFactWithProgram latticeValue edge sourceAddress program targetRows accum localFact =
  case ccrBoundaryMap program (lfBoundary localFact) of
    Left restrictionError ->
      addRestrictionError (CarrierFactBoundaryRestrictionFailed edge sourceAddress restrictionError) accum
    Right targetBoundary ->
      let targetAddress = rkTarget (ccrKey program)
          evidence = lfEvidence localFact
          restrictedRows = restrictRowDelta (ccrTargetClasses program) (plainRowPatchFromMultiplicityMap (ccfeRows evidence))
       in if plainRowPatchNull restrictedRows
            then accum
            else
              case mkLocalAddress latticeValue (caProp targetAddress) (principalSupport (caContext targetAddress)) of
                Left lookupError ->
                  addRestrictionError (CarrierFactLatticeLookupFailed edge sourceAddress lookupError) accum
                Right targetLocalAddress ->
                  let restrictedFact =
                        mkLocalFact
                          targetLocalAddress
                          targetBoundary
                          evidence
                            { ccfeCarrier = caCarrier targetAddress,
                              ccfeRows = positivePlainRowPatchRows restrictedRows
                            }
                   in insertFactInAccum targetAddress targetRows restrictedFact accum
{-# INLINE restrictFactWithProgram #-}

addRestrictionError ::
  CarrierFactRestrictionError ctx carrier prop boundary ->
  RestrictAccum ctx carrier prop boundary evidence ->
  RestrictAccum ctx carrier prop boundary evidence
addRestrictionError restrictionError accum =
  accum {raErrors = restrictionError : raErrors accum}
{-# INLINE addRestrictionError #-}

insertRowsInAccum ::
  (Ord ctx, Ord carrier, Ord prop, BoundaryOps boundary) =>
  CarrierAddr ctx carrier prop ->
  RowDelta ->
  RestrictAccum ctx carrier prop boundary evidence ->
  RestrictAccum ctx carrier prop boundary evidence
insertRowsInAccum targetAddress rows accum =
  if plainRowPatchNull rows
    then accum
    else accum {raCells = Map.alter (mergeRowsCell rows) targetAddress (raCells accum)}
{-# INLINE insertRowsInAccum #-}

mergeRowsCell ::
  (Ord ctx, Ord prop, BoundaryOps boundary) =>
  RowDelta ->
  Maybe (CarrierFactCell ctx carrier prop boundary evidence) ->
  Maybe (CarrierFactCell ctx carrier prop boundary evidence)
mergeRowsCell rows maybeCell =
  let oldRows = maybe emptyPlainRowPatch cfcRows maybeCell
      oldFacts = maybe emptyFactAntichain cfcFacts maybeCell
   in mkCarrierFactCell (composePlainRowPatch rows oldRows) oldFacts
{-# INLINE mergeRowsCell #-}

insertFactInAccum ::
  (Ord ctx, Ord carrier, Ord prop, BoundaryOps boundary) =>
  CarrierAddr ctx carrier prop ->
  RowDelta ->
  LocalFact ctx prop (CarrierCurrentFactEvidence carrier evidence) boundary ->
  RestrictAccum ctx carrier prop boundary evidence ->
  RestrictAccum ctx carrier prop boundary evidence
insertFactInAccum targetAddress rows localFact accum =
  accum {raCells = Map.alter (mergeFactCell rows localFact) targetAddress (raCells accum)}
{-# INLINE insertFactInAccum #-}

mergeFactCell ::
  (Ord ctx, Ord prop, BoundaryOps boundary) =>
  RowDelta ->
  LocalFact ctx prop (CarrierCurrentFactEvidence carrier evidence) boundary ->
  Maybe (CarrierFactCell ctx carrier prop boundary evidence) ->
  Maybe (CarrierFactCell ctx carrier prop boundary evidence)
mergeFactCell rows localFact maybeCell =
  let oldFacts = maybe emptyFactAntichain cfcFacts maybeCell
      nextRows = maybe rows cfcRows maybeCell
      nextFacts = insertAntichain localFact oldFacts
   in mkCarrierFactCell nextRows nextFacts
{-# INLINE mergeFactCell #-}

restrictRowDelta ::
  IntMap RepKey ->
  RowDelta ->
  RowDelta
restrictRowDelta targetClasses =
  mapPlainRowPatchRows (restrictTupleKey targetClasses)
{-# INLINE restrictRowDelta #-}

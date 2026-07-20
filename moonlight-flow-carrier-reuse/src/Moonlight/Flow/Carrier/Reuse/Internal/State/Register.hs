{-# LANGUAGE DataKinds #-}

module Moonlight.Flow.Carrier.Reuse.Internal.State.Register
  ( registerSubsumptionEntry,
    registerSubsumptionEntryChecked,
    insertEntryState,
    registerFactorCarrierShapes,
    registerReusableCarriers,
  )
where

import Control.Monad
  ( foldM,
  )
import Data.Bifunctor
  ( first,
  )
import Data.IntSet qualified as IntSet
import Moonlight.Core
  ( QueryId,
  )
import Moonlight.Differential.Proposition
  ( PropositionKey (..),
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier (..),
    QueryCarrierNode (..),
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
    caCarrier,
    carrierAddr,
  )
import Moonlight.Flow.Carrier.Core.Coverage
  ( CoverageFact (..),
  )
import Moonlight.Flow.Carrier.Reuse.Internal.Index.Shape
  ( insertEntryIndex,
    lookupSubsumptionEntryByCarrier,
  )
import Moonlight.Flow.Carrier.Reuse.Internal.Shape
  ( SubsumptionEntry (..),
    SubsumptionRegistrationError (..),
  )
import Moonlight.Flow.Carrier.Reuse.Internal.State.Normalize
  ( normalizeFactorShapeForReuse,
  )
import Moonlight.Flow.Carrier.Reuse.Internal.State.Types
  ( PlanReuseState (..),
    mapPlanReuseStats,
  )
import Moonlight.Flow.Carrier.Reuse.Internal.Stats
  ( recordRegisteredNew,
  )
import Moonlight.Flow.Carrier.Reuse.Internal.Validity
  ( ReuseValidity,
    reuseValidityFromRegistration,
  )
import Moonlight.Flow.Carrier.Reuse.Types
  ( PlanReuseError (..),
    PlanReuseRegistration (..),
    PlanReuseRegistrationEntry (..),
  )
import Moonlight.Flow.Execution.Subsumption.FactorShape
  ( FactorShapeManifest,
    factorShapeFromManifestBoundary,
    lookupFactorShapeManifestNode,
  )
import Moonlight.Flow.Model.Schema.Boundary
  ( RuntimeBoundary,
  )
import Moonlight.Flow.Model.Scope
  ( scopeDeps,
    scopeTopo,
  )
import Moonlight.Flow.Model.Schema.Digest
  ( StableDigest128,
  )
import Moonlight.Flow.Plan.Rewrite
  ( fsnKey,
  )
import Moonlight.Flow.Plan.Shape
  ( CanonicalizationResult,
    factorShapeResidual,
  )
import Moonlight.Flow.Plan.Shape.Term
  ( PlanShape,
    PlanStage (..),
  )

registerSubsumptionEntry ::
  Ord ctx =>
  Ord prop =>
  PlanShape 'FactorShape ->
  CarrierAddr ctx Carrier prop ->
  ReuseValidity ->
  RuntimeBoundary ->
  CoverageFact ->
  IntSet.IntSet ->
  IntSet.IntSet ->
  PlanReuseState ctx prop ->
  Either SubsumptionRegistrationError (PlanReuseState ctx prop)
registerSubsumptionEntry rawShape carrier validity boundary coverageHint deps topo state0 =
  case caCarrier carrier of
    DerivedCarrier {} ->
      Left (SubsumptionRegistrationDerivedCarrierRejected (caCarrier carrier))
    QueryCarrier _ (QueryAtom {}) ->
      Left SubsumptionRegistrationAtomCarrierRejected
    QueryCarrier _ (QueryFactor {}) -> do
      (state1, normalization) <-
        normalizeFactorShapeForReuse rawShape state0
      let entry =
            SubsumptionEntry
              { seShape = rawShape,
                seShapeKey = fsnKey normalization,
                seShapeNormalization = normalization,
                seCarrier = carrier,
                seValidity = validity,
                seBoundary = boundary,
                seCoverageHint = coverageHint,
                seDeps = deps,
                seTopo = topo
              }
      registerSubsumptionEntryChecked entry state1

registerSubsumptionEntryChecked ::
  Ord ctx =>
  Ord prop =>
  SubsumptionEntry ctx prop ->
  PlanReuseState ctx prop ->
  Either SubsumptionRegistrationError (PlanReuseState ctx prop)
registerSubsumptionEntryChecked entry state =
  case lookupSubsumptionEntryByCarrier (seCarrier entry) (prsSubsumptionIndex state) of
    Nothing ->
      Right
        ( insertEntryState
            entry
            (mapPlanReuseStats (recordRegisteredNew 1) state)
        )
    Just existing
      | seShapeKey existing /= seShapeKey entry ->
          Left
            ( SubsumptionRegistrationDuplicateCarrierDifferentShape
                (eraseCarrierAddr (seCarrier entry))
                (seShapeKey existing)
                (seShapeKey entry)
            )
      | existing == entry ->
          Right state
      | otherwise ->
          Right (insertEntryState entry state)

insertEntryState ::
  Ord ctx =>
  Ord prop =>
  SubsumptionEntry ctx prop ->
  PlanReuseState ctx prop ->
  PlanReuseState ctx prop
insertEntryState entry state =
  state {prsSubsumptionIndex = insertEntryIndex entry (prsSubsumptionIndex state)}

registerFactorCarrierShapes ::
  (Ord ctx, Ord prop) =>
  QueryId ->
  CanonicalizationResult ->
  FactorShapeManifest ->
  StableDigest128 ->
  [PlanReuseRegistrationEntry ctx prop] ->
  PlanReuseState ctx prop ->
  Either SubsumptionRegistrationError (PlanReuseState ctx prop)
registerFactorCarrierShapes queryId planShape manifest inputDigest entries state =
  case
    registerReusableCarriers
      PlanReuseRegistration
        { prrQueryId = queryId,
          prrCanonicalPlan = planShape,
          prrFactorManifest = manifest,
          prrInputDigest = inputDigest,
          prrEntries = entries
        }
      state
  of
    Left (ReuseRegisterFailed err) ->
      Left err
    Left (ReuseNormalizeFailed err) ->
      Left err
    Left _unexpected ->
      Left (SubsumptionRegistrationNormalizationUnstable 0)
    Right state' ->
      Right state'

registerReusableCarriers ::
  (Ord ctx, Ord prop) =>
  PlanReuseRegistration ctx prop ->
  PlanReuseState ctx prop ->
  Either (PlanReuseError ctx prop) (PlanReuseState ctx prop)
registerReusableCarriers registration state0 =
  first ReuseRegisterFailed $
    foldM registerEntry state0 (prrEntries registration)
  where
    queryId =
      prrQueryId registration

    planShape =
      prrCanonicalPlan registration

    manifest =
      prrFactorManifest registration

    inputDigest =
      prrInputDigest registration

    registerEntry state entry =
      case caCarrier (prreAddr entry) of
        QueryCarrier _ (QueryAtom {}) ->
          Left SubsumptionRegistrationAtomCarrierRejected
        DerivedCarrier {} ->
          Left (SubsumptionRegistrationDerivedCarrierRejected (caCarrier (prreAddr entry)))
        QueryCarrier carrierQueryId (QueryFactor node)
          | carrierQueryId /= queryId ->
              Left (SubsumptionRegistrationQueryMismatch queryId carrierQueryId)
          | otherwise -> do
              nodeManifest <-
                case lookupFactorShapeManifestNode node manifest of
                  Nothing ->
                    Left (SubsumptionRegistrationMissingManifestNode (caCarrier (prreAddr entry)))
                  Just manifestValue ->
                    Right manifestValue
              shapeKey <-
                first SubsumptionRegistrationFactorShapeError $
                  factorShapeFromManifestBoundary
                    planShape
                    nodeManifest
                    (prreBoundary entry)
              registerSubsumptionEntry
                shapeKey
                (prreAddr entry)
                ( reuseValidityFromRegistration
                    (Just inputDigest)
                    (factorShapeResidual shapeKey)
                    (prreTime entry)
                    (prreScope entry)
                )
                (prreBoundary entry)
                ExactLocal
                (scopeDeps (prreScope entry))
                (scopeTopo (prreScope entry))
                state

eraseCarrierAddr ::
  CarrierAddr ctx Carrier prop ->
  CarrierAddr () Carrier ()
eraseCarrierAddr addr =
  carrierAddr () (PropositionKey ()) (caCarrier addr)

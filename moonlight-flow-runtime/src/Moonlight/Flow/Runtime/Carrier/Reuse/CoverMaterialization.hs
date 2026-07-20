{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Flow.Runtime.Carrier.Reuse.CoverMaterialization
  ( CoverMaterializationPlan (..),
    CoverMaterializationError (..),
    CurrentCarrierLookupE (..),
    mkCoverMaterializationPlan,
  )
where

import Control.Monad
  ( unless,
  )
import Data.Bifunctor
  ( first,
  )
import Data.Functor.Identity
  ( Identity (..),
  )
import Data.IntSet
  ( IntSet,
  )
import Data.IntSet qualified as IntSet
import Data.List.NonEmpty
  ( NonEmpty (..),
  )
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
    caContext,
    caProp,
    caCarrier,
  )
import Moonlight.Flow.Carrier.Core.Coverage
  ( CoverageFact (..),
  )
import Moonlight.Flow.Carrier.Core.Delta
  ( RelationalCarrierDelta,
    RelationalCarrierDeltaP (..),
    rowDeltaDigest,
  )
import Moonlight.Flow.Carrier.Core.Reuse
  ( CarrierReuseId,
  )
import Moonlight.Flow.Carrier.Core.Time
  ( RelationalCarrierTime,
  )
import Moonlight.Differential.Carrier.Topology
  ( CarrierFamily,
    carrierCoverComplete,
    carrierCoverMembers,
    carrierCoverTarget,
    carrierFamilyCover,
    carrierFamilyMembers,
    carrierFamilyProp,
  )
import Moonlight.Flow.Carrier.Morphism.Amalgamation
  ( mergeCarrierBoundaries,
  )
import Moonlight.Flow.Carrier.Morphism.Engine
  ( CarrierReuseOps (..),
    checkedReuseSupportProject,
    runCarrierReuseMorphism,
  )
import Moonlight.Flow.Carrier.Morphism.Subsumption
  ( CarrierReuse (..),
    CarrierReuseError,
    CoverageProjectionRule (ExactByCover),
    ReuseWitness (..),
    carrierReuseFromWitness,
    carrierReuseId,
  )
import Moonlight.Flow.Carrier.Reuse
  ( InstalledReuseMaterialization (..),
    PlanReuseRegistrationEntry (..),
    PlanReuseState,
    RequestedFactorShape (..),
    ReuseValidityRequest (..),
    SubsumptionEntry (..),
    lookupReusableCarrierEntry,
    reuseExactValidityMatchesRequest,
  )
import Moonlight.Differential.Row.Patch
  ( emptyPlainRowPatch,
    subtractPlainRowPatch,
  )
import Moonlight.Flow.Model.Schema.Boundary
  ( RuntimeBoundary,
    boundaryDigest,
  )
import Moonlight.Flow.Model.Schema.Digest
  ( StableDigest128,
  )
import Moonlight.Flow.Plan.Shape.Term
  ( PlanShape,
    PlanStage (..),
  )
import Moonlight.Flow.Runtime.Topology.Site.Types
  ( GeneratedCover (..),
    GeneratedSiteState (..),
  )

data CoverMaterializationF f ctx prop evidence = CoverMaterializationF
  { cmfSourceCarrier :: f (CarrierAddr ctx Carrier prop),
    cmfTargetCarrier :: f (CarrierAddr ctx Carrier prop),
    cmfFamily :: f (CarrierFamily ctx Carrier prop),
    cmfSourceEntry :: f (SubsumptionEntry ctx prop),
    cmfSourceSnapshot :: f (RelationalCarrierDelta ctx Carrier prop RuntimeBoundary evidence),
    cmfMemberSnapshots ::
      f
        ( Map
            (CarrierAddr ctx Carrier prop)
            (RelationalCarrierDelta ctx Carrier prop RuntimeBoundary evidence)
        ),
    cmfMemberEntries ::
      f
        ( Map
            (CarrierAddr ctx Carrier prop)
            (SubsumptionEntry ctx prop)
        ),
    cmfTargetShape :: f (PlanShape 'FactorShape),
    cmfTargetBoundary :: f RuntimeBoundary,
    cmfMergedBoundary :: f RuntimeBoundary,
    cmfDeps :: f IntSet,
    cmfTopo :: f IntSet,
    cmfViewDigest :: f (Maybe StableDigest128),
    cmfSiteDigest :: f StableDigest128
  }

type VerifiedCover ctx prop evidence =
  CoverMaterializationF Identity ctx prop evidence

newtype CurrentCarrierLookupE ctx prop evidence = CurrentCarrierLookupE
  { runCurrentCarrierLookupE ::
      CarrierAddr ctx Carrier prop ->
      Either
        (CoverMaterializationError ctx prop evidence)
        (Maybe (RelationalCarrierDelta ctx Carrier prop RuntimeBoundary evidence))
  }

data CoverMaterializationPlan ctx prop evidence = CoverMaterializationPlan
  { cmpReuseId :: !(CarrierReuseId ctx prop),
    cmpTarget :: !(CarrierAddr ctx Carrier prop),
    cmpSourceSnapshot :: !(RelationalCarrierDelta ctx Carrier prop RuntimeBoundary evidence),
    cmpProjectedSnapshot :: !(RelationalCarrierDelta ctx Carrier prop RuntimeBoundary evidence),
    cmpProjectedDelta :: !(RelationalCarrierDelta ctx Carrier prop RuntimeBoundary evidence),
    cmpInstalled :: !(InstalledReuseMaterialization ctx prop),
    cmpRegistration :: !(PlanReuseRegistrationEntry ctx prop),
    cmpRegistrationDeps :: !IntSet,
    cmpRegistrationTopo :: !IntSet
  }
  deriving stock (Eq, Show)

data CoverMaterializationError ctx prop evidence
  = CoverCandidateSourceMismatch !(CarrierAddr ctx Carrier prop) !(CarrierAddr ctx Carrier prop)
  | CoverCandidateTargetMismatch !(CarrierAddr ctx Carrier prop) !(CarrierAddr ctx Carrier prop)
  | CoverCandidateShapeMismatch
  | CoverCandidateBoundaryMismatch !RuntimeBoundary !RuntimeBoundary
  | CoverViewDigestMismatch !(Maybe StableDigest128) !(Maybe StableDigest128)
  | CoverRuntimeLookupFailed !(CarrierAddr ctx Carrier prop)
  | CoverSourceMissing !(CarrierAddr ctx Carrier prop)
  | CoverMemberMissing !(CarrierAddr ctx Carrier prop)
  | CoverFamilyMissing !(CarrierAddr ctx Carrier prop)
  | CoverFamilyAmbiguous !(NonEmpty (CarrierFamily ctx Carrier prop))
  | CoverFamilyIncomplete !(CarrierAddr ctx Carrier prop)
  | CoverFamilyMemberMismatch !(CarrierAddr ctx Carrier prop)
  | CoverSourceEntryNotExact !(CarrierAddr ctx Carrier prop)
  | CoverSourceEntryShapeMismatch !(CarrierAddr ctx Carrier prop)
  | CoverSourceBoundaryMismatch !RuntimeBoundary !RuntimeBoundary
  | CoverSourceEntryBoundaryMismatch !(CarrierAddr ctx Carrier prop) !RuntimeBoundary !RuntimeBoundary
  | CoverSourceEntryValidityMismatch !(CarrierAddr ctx Carrier prop)
  | CoverMemberEntryMissing !(CarrierAddr ctx Carrier prop)
  | CoverMemberEntryAmbiguous !(CarrierAddr ctx Carrier prop)
  | CoverMemberEntryNotExact !(CarrierAddr ctx Carrier prop)
  | CoverMemberEntryShapeMismatch !(CarrierAddr ctx Carrier prop)
  | CoverMemberBoundaryMismatch !(CarrierAddr ctx Carrier prop) !RuntimeBoundary !RuntimeBoundary
  | CoverMemberEntryBoundaryMismatch !(CarrierAddr ctx Carrier prop) !RuntimeBoundary !RuntimeBoundary
  | CoverMemberEntryValidityMismatch !(CarrierAddr ctx Carrier prop)
  | CoverMergedBoundaryFailed
  | CoverBoundaryMismatch !RuntimeBoundary !RuntimeBoundary
  | CoverReuseProjectionRejected !(CarrierReuseError ctx prop evidence)
  | CoverProjectedTargetMismatch !(CarrierAddr ctx Carrier prop) !(CarrierAddr ctx Carrier prop)
  | CoverTargetCurrentBoundaryMismatch !(CarrierAddr ctx Carrier prop) !RuntimeBoundary !RuntimeBoundary
  deriving stock (Eq, Show)

mkCoverMaterializationPlan ::
  (Ord ctx, Ord prop) =>
  GeneratedSiteState ctx prop ->
  PlanReuseState ctx prop ->
  CurrentCarrierLookupE ctx prop evidence ->
  RelationalCarrierTime ctx ->
  RequestedFactorShape ctx prop ->
  SubsumptionEntry ctx prop ->
  CarrierReuse ctx prop ->
  Either
    (NonEmpty (CoverMaterializationError ctx prop evidence))
    (CoverMaterializationPlan ctx prop evidence)
mkCoverMaterializationPlan site planReuse currentLookup eventTime request sourceEntry candidateReuse = do
  let !witness =
        cruWitness candidateReuse
      !sourceCarrier =
        rwSourceCarrier witness
      !targetCarrier =
        rfsTargetCarrier request
      !expectedViewDigest =
        rvrViewDigest (rfsValidity request)

  require
    (sourceCarrier == seCarrier sourceEntry)
    (CoverCandidateSourceMismatch sourceCarrier (seCarrier sourceEntry))
  require
    (rwTargetCarrier witness == targetCarrier)
    (CoverCandidateTargetMismatch targetCarrier (rwTargetCarrier witness))
  require
    (rwTargetShape witness == rfsShape request)
    CoverCandidateShapeMismatch
  require
    (cruTargetBoundary candidateReuse == rfsBoundary request)
    (CoverCandidateBoundaryMismatch (rfsBoundary request) (cruTargetBoundary candidateReuse))
  require
    (cruTargetViewDigest candidateReuse == expectedViewDigest)
    (CoverViewDigestMismatch (cruTargetViewDigest candidateReuse) expectedViewDigest)
  require
    (seShape sourceEntry == rwSourceShape witness)
    (CoverSourceEntryShapeMismatch sourceCarrier)
  require
    (reuseExactValidityMatchesRequest (rfsValidity request) (seValidity sourceEntry))
    (CoverSourceEntryValidityMismatch sourceCarrier)
  require
    (entryCanSupplyExact sourceEntry)
    (CoverSourceEntryNotExact sourceCarrier)

  sourceSnapshot <-
    requireCurrent CoverSourceMissing currentLookup sourceCarrier

  require
    (deBoundary sourceSnapshot == seBoundary sourceEntry)
    (CoverSourceEntryBoundaryMismatch sourceCarrier (seBoundary sourceEntry) (deBoundary sourceSnapshot))

  (family, generatedCover) <-
    selectedCoverFamily site sourceCarrier

  let !memberAddrs =
        Set.toAscList (carrierFamilyMembers family)

  memberSnapshotPairs <-
    traverse
      (memberSnapshotPair currentLookup)
      memberAddrs

  let !memberSnapshots =
        Map.fromDistinctAscList memberSnapshotPairs

  memberEntryPairs <-
    traverse
      (memberEntryPair planReuse (rfsValidity request) (seShape sourceEntry))
      memberSnapshotPairs

  let !memberEntries =
        Map.fromDistinctAscList memberEntryPairs
      !memberSet =
        Map.keysSet memberEntries

  require
    (memberSet == carrierFamilyMembers family)
    (CoverFamilyMemberMismatch sourceCarrier)

  mergedBoundary <-
    mergedMemberBoundary (Map.elems memberSnapshots)

  require
    (mergedBoundary == deBoundary sourceSnapshot)
    (CoverBoundaryMismatch (deBoundary sourceSnapshot) mergedBoundary)

  let !deps =
        foldIntSets seDeps (sourceEntry : Map.elems memberEntries)
      !topo =
        IntSet.union
          (gcDirtyTopo generatedCover)
          (foldIntSets seTopo (sourceEntry : Map.elems memberEntries))
      !verified =
        CoverMaterializationF
          { cmfSourceCarrier = Identity sourceCarrier,
            cmfTargetCarrier = Identity targetCarrier,
            cmfFamily = Identity family,
            cmfSourceEntry = Identity sourceEntry,
            cmfSourceSnapshot = Identity sourceSnapshot,
            cmfMemberSnapshots = Identity memberSnapshots,
            cmfMemberEntries = Identity memberEntries,
            cmfTargetShape = Identity (rfsShape request),
            cmfTargetBoundary = Identity (rfsBoundary request),
            cmfMergedBoundary = Identity mergedBoundary,
            cmfDeps = Identity deps,
            cmfTopo = Identity topo,
            cmfViewDigest = Identity expectedViewDigest,
            cmfSiteDigest = Identity (gssDigest site)
          }

  materializationPlanFromVerified
    currentLookup
    eventTime
    candidateReuse
    verified
{-# INLINE mkCoverMaterializationPlan #-}

materializationPlanFromVerified ::
  (Ord ctx, Ord prop) =>
  CurrentCarrierLookupE ctx prop evidence ->
  RelationalCarrierTime ctx ->
  CarrierReuse ctx prop ->
  VerifiedCover ctx prop evidence ->
  Either
    (NonEmpty (CoverMaterializationError ctx prop evidence))
    (CoverMaterializationPlan ctx prop evidence)
materializationPlanFromVerified currentLookup eventTime candidateReuse verified = do
  let !sourceSnapshot =
        runIdentity (cmfSourceSnapshot verified)
      !targetCarrier =
        runIdentity (cmfTargetCarrier verified)
      !targetBoundary =
        runIdentity (cmfTargetBoundary verified)
      !viewDigest =
        runIdentity (cmfViewDigest verified)
      !deps =
        runIdentity (cmfDeps verified)
      !topo =
        runIdentity (cmfTopo verified)
      !exactReuse =
        carrierReuseFromWitness
          ExactByCover
          targetBoundary
          viewDigest
          deps
          topo
          (cruWitness candidateReuse)
      !reuseId =
        carrierReuseId exactReuse

  projectedSnapshot <-
    first
      (\projectionError -> CoverReuseProjectionRejected projectionError :| [])
      ( runCarrierReuseMorphism
          (coverMaterializationReuseOps eventTime)
          exactReuse
          sourceSnapshot
      )

  require
    (deAddr projectedSnapshot == targetCarrier)
    (CoverProjectedTargetMismatch targetCarrier (deAddr projectedSnapshot))

  maybeCurrentTarget <-
    lookupCurrent currentLookup targetCarrier

  case maybeCurrentTarget of
    Nothing ->
      Right ()
    Just currentTarget ->
      require
        (deBoundary currentTarget == deBoundary projectedSnapshot)
        (CoverTargetCurrentBoundaryMismatch targetCarrier (deBoundary currentTarget) (deBoundary projectedSnapshot))

  let !currentRows =
        maybe emptyPlainRowPatch deRows maybeCurrentTarget
      !projectedDelta =
        projectedSnapshot
          { deRows =
              subtractPlainRowPatch
                (deRows projectedSnapshot)
                currentRows
          }
      !installed =
        InstalledReuseMaterialization
          { irmReuseId = reuseId,
            irmTarget = targetCarrier,
            irmRows = deRows projectedSnapshot,
            irmBoundaryDigest = boundaryDigest (deBoundary projectedSnapshot),
            irmSourceCurrentDigest = rowDeltaDigest (deRows sourceSnapshot),
            irmDeps = deps,
            irmTopo = topo
          }
      !registration =
        PlanReuseRegistrationEntry
          { prreAddr = targetCarrier,
            prreTime = deTime projectedSnapshot,
            prreBoundary = deBoundary projectedSnapshot,
            prreScope = deScope projectedSnapshot
          }

  pure
    CoverMaterializationPlan
      { cmpReuseId = reuseId,
        cmpTarget = targetCarrier,
        cmpSourceSnapshot = sourceSnapshot,
        cmpProjectedSnapshot = projectedSnapshot,
        cmpProjectedDelta = projectedDelta,
        cmpInstalled = installed,
        cmpRegistration = registration,
        cmpRegistrationDeps = deps,
        cmpRegistrationTopo = topo
      }
{-# INLINE materializationPlanFromVerified #-}

coverMaterializationReuseOps ::
  RelationalCarrierTime ctx ->
  CarrierReuseOps ctx prop evidence
coverMaterializationReuseOps eventTime =
  CarrierReuseOps
    { croEventTime = eventTime,
      croEvidenceOf = \_witness _rule _boundary evidence -> Right evidence,
      croSupportProject = checkedReuseSupportProject
    }
{-# INLINE coverMaterializationReuseOps #-}

selectedCoverFamily ::
  (Ord ctx, Ord prop) =>
  GeneratedSiteState ctx prop ->
  CarrierAddr ctx Carrier prop ->
  Either
    (NonEmpty (CoverMaterializationError ctx prop evidence))
    (CarrierFamily ctx Carrier prop, GeneratedCover ctx prop)
selectedCoverFamily site sourceCarrier =
  case NonEmpty.nonEmpty (sourceFamilyCandidates site sourceCarrier) of
    Nothing ->
      single (CoverFamilyMissing sourceCarrier)
    Just (candidate :| []) ->
      validateCoverFamily sourceCarrier candidate
    Just candidates ->
      single (CoverFamilyAmbiguous (fmap fst candidates))
{-# INLINE selectedCoverFamily #-}

sourceFamilyCandidates ::
  (Ord ctx, Ord prop) =>
  GeneratedSiteState ctx prop ->
  CarrierAddr ctx Carrier prop ->
  [(CarrierFamily ctx Carrier prop, GeneratedCover ctx prop)]
sourceFamilyCandidates site sourceCarrier =
  [ pair
  | pair@(family, _generatedCover) <- Map.toAscList (gssCovers site),
    carrierFamilyProp family == caProp sourceCarrier,
    carrierCoverTarget (carrierFamilyCover family) == caContext sourceCarrier
  ]
{-# INLINE sourceFamilyCandidates #-}

validateCoverFamily ::
  (Ord ctx, Ord prop) =>
  CarrierAddr ctx Carrier prop ->
  (CarrierFamily ctx Carrier prop, GeneratedCover ctx prop) ->
  Either
    (NonEmpty (CoverMaterializationError ctx prop evidence))
    (CarrierFamily ctx Carrier prop, GeneratedCover ctx prop)
validateCoverFamily sourceCarrier pair@(family, _generatedCover) = do
  require
    (carrierCoverComplete (carrierFamilyCover family))
    (CoverFamilyIncomplete sourceCarrier)
  require
    (not (Set.null (carrierFamilyMembers family)))
    (CoverFamilyIncomplete sourceCarrier)
  require
    (carrierCoverMembers (carrierFamilyCover family) == Set.map caContext (carrierFamilyMembers family))
    (CoverFamilyMemberMismatch sourceCarrier)
  require
    (all memberMatchesSource (Set.toAscList (carrierFamilyMembers family)))
    (CoverFamilyMemberMismatch sourceCarrier)
  pure pair
  where
    memberMatchesSource member =
      caProp member == caProp sourceCarrier
        && caCarrier member == caCarrier sourceCarrier
{-# INLINE validateCoverFamily #-}

memberSnapshotPair ::
  CurrentCarrierLookupE ctx prop evidence ->
  CarrierAddr ctx Carrier prop ->
  Either
    (NonEmpty (CoverMaterializationError ctx prop evidence))
    (CarrierAddr ctx Carrier prop, RelationalCarrierDelta ctx Carrier prop RuntimeBoundary evidence)
memberSnapshotPair currentLookup addr = do
  snapshot <-
    requireCurrent CoverMemberMissing currentLookup addr
  pure (addr, snapshot)
{-# INLINE memberSnapshotPair #-}

memberEntryPair ::
  (Ord ctx, Ord prop) =>
  PlanReuseState ctx prop ->
  ReuseValidityRequest ->
  PlanShape 'FactorShape ->
  (CarrierAddr ctx Carrier prop, RelationalCarrierDelta ctx Carrier prop RuntimeBoundary evidence) ->
  Either
    (NonEmpty (CoverMaterializationError ctx prop evidence))
    (CarrierAddr ctx Carrier prop, SubsumptionEntry ctx prop)
memberEntryPair planReuse validity expectedShape (member, snapshot) =
  case lookupReusableCarrierEntry member planReuse of
    Nothing ->
      single (CoverMemberEntryMissing member)
    Just entry -> do
      require
        (reuseExactValidityMatchesRequest validity (seValidity entry))
        (CoverMemberEntryValidityMismatch member)
      require
        (entryCanSupplyExact entry)
        (CoverMemberEntryNotExact member)
      require
        (seShape entry == expectedShape)
        (CoverMemberEntryShapeMismatch member)
      require
        (seBoundary entry == deBoundary snapshot)
        (CoverMemberEntryBoundaryMismatch member (seBoundary entry) (deBoundary snapshot))
      pure (member, entry)
{-# INLINE memberEntryPair #-}

entryCanSupplyExact :: SubsumptionEntry ctx prop -> Bool
entryCanSupplyExact entry =
  case seCoverageHint entry of
    ExactLocal ->
      True
    ExactRestricted ->
      True
    ExactAmalgamated ->
      True
    LowerBound ->
      False
    Obstructed {} ->
      False
{-# INLINE entryCanSupplyExact #-}

mergedMemberBoundary ::
  [RelationalCarrierDelta ctx Carrier prop RuntimeBoundary evidence] ->
  Either
    (NonEmpty (CoverMaterializationError ctx prop evidence))
    RuntimeBoundary
mergedMemberBoundary snapshots =
  case NonEmpty.nonEmpty (fmap deBoundary snapshots) of
    Nothing ->
      single CoverMergedBoundaryFailed
    Just boundaries ->
      case mergeCarrierBoundaries boundaries of
        Left _err ->
          single CoverMergedBoundaryFailed
        Right boundary ->
          Right boundary
{-# INLINE mergedMemberBoundary #-}

lookupCurrent ::
  CurrentCarrierLookupE ctx prop evidence ->
  CarrierAddr ctx Carrier prop ->
  Either
    (NonEmpty (CoverMaterializationError ctx prop evidence))
    (Maybe (RelationalCarrierDelta ctx Carrier prop RuntimeBoundary evidence))
lookupCurrent currentLookup addr =
  first (:| []) (runCurrentCarrierLookupE currentLookup addr)
{-# INLINE lookupCurrent #-}

requireCurrent ::
  (CarrierAddr ctx Carrier prop -> CoverMaterializationError ctx prop evidence) ->
  CurrentCarrierLookupE ctx prop evidence ->
  CarrierAddr ctx Carrier prop ->
  Either
    (NonEmpty (CoverMaterializationError ctx prop evidence))
    (RelationalCarrierDelta ctx Carrier prop RuntimeBoundary evidence)
requireCurrent missing currentLookup addr = do
  maybeSnapshot <-
    lookupCurrent currentLookup addr
  case maybeSnapshot of
    Nothing ->
      single (missing addr)
    Just snapshot ->
      Right snapshot
{-# INLINE requireCurrent #-}

foldIntSets ::
  (value -> IntSet) ->
  [value] ->
  IntSet
foldIntSets project =
  foldMap project
{-# INLINE foldIntSets #-}

require ::
  Bool ->
  CoverMaterializationError ctx prop evidence ->
  Either (NonEmpty (CoverMaterializationError ctx prop evidence)) ()
require condition err =
  unless condition (single err)
{-# INLINE require #-}

single ::
  CoverMaterializationError ctx prop evidence ->
  Either (NonEmpty (CoverMaterializationError ctx prop evidence)) value
single err =
  Left (err :| [])
{-# INLINE single #-}

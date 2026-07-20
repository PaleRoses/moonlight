{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Flow.Runtime.Factor.Input
  ( factorAtomCarrierAddr,
    FactorInputFrame (..),
    factorInputFrameRuntime,
    atomBoundaryDigestsFromReadouts,
    factorInputSignatureFromAtomReadouts,
    factorInputSignatureRuntime,
    heldCarrierReadsForFactorProgramsRuntime,
  )
where

import Data.Foldable qualified as Foldable
import Data.Bifunctor
  ( first,
  )
import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Maybe
  ( fromMaybe,
  )
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Data.Word
  ( Word64,
  )
import Moonlight.Core
  ( AtomId,
    QueryId,
    SlotId,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier (..),
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
  )
import Moonlight.Flow.Carrier.Core.Delta
  ( RelationalCarrierDeltaP (..),
    rowDeltaDigest,
  )
import Moonlight.Flow.Carrier.Core.Delta.Emit
  ( CarrierEmitSpec (..),
  )
import Moonlight.Flow.Carrier.Store
  ( CarrierHeldReads,
    CarrierStoreError (..),
    carrierSnapshotRows,
    emptyCarrierHeldReads,
    insertCarrierHeldRead,
    lookupCarrierSnapshot,
  )
import Moonlight.Flow.Carrier.View.Query
  ( carrierBoundaryLatestTraceNow,
  )
import Moonlight.Flow.Execution.Factor.Types
  ( FactorCache,
    FactorInput (..),
    emptyFactorCache,
  )
import Moonlight.Flow.Model.Delta
  ( AtomEvent (..)
  )
import Moonlight.Differential.Row.Delta
  ( RowDelta
  )
import Moonlight.Flow.Internal.Digest
  ( wordOfInt,
  )
import Moonlight.Differential.Row.Patch
  ( emptyPlainRowPatch,
    plainRowPatchFromMultiplicityMap,
    plainRowPatchNull,
    subtractPlainRowPatch,
  )
import Moonlight.Flow.Model.Schema.Boundary
  ( BoundaryShape (..),
    RuntimeBoundary,
    boundaryDigest,
    boundaryShape,
  )
import Moonlight.Flow.Model.Schema.Digest
  ( StableDigest128,
    stableDigest128,
    stableDigestWords,
  )
import Moonlight.Flow.Model.Scope
  ( RelationalScope,
  )
import Moonlight.Flow.Plan.Query.Core
  ( FactorNode,
    QueryAtomId,
    mkQueryAtomId,
    queryAtomAsAtomId,
  )
import Moonlight.Flow.Runtime.Factor.Internal.Cache
  ( FactorAtomReadStamp (..),
    FactorCacheReadiness (..),
    FactorCacheState,
    FactorPreparedInputCache (..),
    factorCacheReadiness,
    factorCacheStateToTransientCache,
    fcsAtomReads,
    fcsNodes,
    fcsPreparedInput,
  )
import Moonlight.Flow.Runtime.Factor.Program
  ( FactorProgram (..),
    factorProgramAtomKeys,
    factorProgramQueryId,
    factorProgramMaintenanceNodes,
  )
import Moonlight.Flow.Runtime.Spec.FactorProgram
  ( RepairProgramKey,
  )
import Moonlight.Flow.Runtime.Factor.State.Types
  ( RuntimeQueryBinding (..),
  )
import Moonlight.Flow.Runtime.Factor.Request
  ( FactorRepairRequest (..),
    repairCauseAtomDeltas,
    repairCauseRelationalScope,
    repairCauseIsFull,
  )
import Moonlight.Flow.Runtime.Carrier.Store
  ( currentCarrierMaybe,
  )
import Moonlight.Flow.Runtime.Carrier.Store.Internal
  ( carrierStoreAtRouting,
  )
import Moonlight.Flow.Runtime.Carrier.Emit
  ( AtomCarrierEmitSpec,
    AtomCarrierPayload (..),
  )
import Moonlight.Flow.Runtime.Kernel
  ( RelDiffRuntime,
    RuntimeEnvelope (..),
    rsRouting,
  )
import Moonlight.Flow.Runtime.Error
  ( RuntimeError (..),
  )
import Moonlight.Flow.Runtime.Execution.Failure
  ( RelationalRuntimeError,
    RelationalRuntimeOpFailure (..),
  )
import Moonlight.Flow.Runtime.Core.RepairStats
  ( RuntimeRepairInputStats (..),
    emptyRuntimeRepairInputStats,
  )
import Moonlight.Flow.Storage.Relation
  ( Relation,
    applyRelationPatch,
    emptyRelation,
    relationLayout,
    relationRows,
  )
import Moonlight.Flow.Storage.Plan
  ( compileStoragePlan,
    storagePlanFromRelations,
  )
import Moonlight.Flow.Storage.Store
  ( Store,
    StoragePatch (..),
    applyStoragePatch,
    sprStore,
    storeFromRelationsWithPlan,
  )
import Moonlight.Flow.Storage.View
  ( unrestrictedView,
    viewSignature,
  )

data FactorInputFrame ctx prop boundary evidence joinState joinErr = FactorInputFrame
  { fifInput :: !FactorInput,
    fifCache :: !FactorCache,
    fifRuntime :: !(RelDiffRuntime ctx prop boundary evidence joinState joinErr),
    fifAtomReadouts :: !(IntMap (RuntimeBoundary, RowDelta)),
    fifPreparedInput :: !FactorPreparedInputCache,
    fifInputStats :: !RuntimeRepairInputStats
  }

data PreparedAtomRelations ctx prop boundary evidence joinState joinErr = PreparedAtomRelations
  { parRelations :: !(IntMap Relation),
    parAtomReadouts :: !(IntMap (RuntimeBoundary, RowDelta)),
    parRuntime :: !(RelDiffRuntime ctx prop boundary evidence joinState joinErr)
  }

factorAtomCarrierAddr ::
  AtomCarrierEmitSpec ctx prop boundary evidence ->
  QueryId ->
  QueryAtomId ->
  CarrierAddr ctx Carrier prop
factorAtomCarrierAddr spec queryId queryAtomId =
  cesAddrOf spec (emptyAtomPayload queryId (queryAtomAsAtomId queryAtomId))
{-# INLINE factorAtomCarrierAddr #-}

factorInputFrameRuntime ::
  (boundary ~ RuntimeBoundary, Ord ctx, Ord prop) =>
  AtomCarrierEmitSpec ctx prop boundary evidence ->
  QueryId ->
  FactorRepairRequest ctx prop ->
  FactorProgram ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (FactorInputFrame ctx prop boundary evidence joinState joinErr)
factorInputFrameRuntime atomSpec queryId request program runtime0 =
  let !atomDeltas =
        repairCauseAtomDeltas (frrCause request)
      rebuildFrame =
        rebuildInputFrameRuntime atomSpec queryId program runtime0
      patchFrame =
        patchInputFrameFromPreparedCache
          atomSpec
          queryId
          request
          program
          runtime0
          atomDeltas
   in if repairCauseIsFull (frrCause request)
        then rebuildFrame
        else patchFrameOrRebuild rebuildFrame patchFrame
{-# INLINE factorInputFrameRuntime #-}

patchFrameOrRebuild ::
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (FactorInputFrame ctx prop boundary evidence joinState joinErr) ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (FactorInputFrame ctx prop boundary evidence joinState joinErr) ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (FactorInputFrame ctx prop boundary evidence joinState joinErr)
patchFrameOrRebuild rebuildFrame patchFrame =
  case patchFrame of
    Right frame ->
      Right frame
    Left obstruction
      | patchFrameObstructionAllowsRebuild obstruction ->
          rebuildFrame
      | otherwise ->
          Left obstruction
{-# INLINE patchFrameOrRebuild #-}

patchFrameObstructionAllowsRebuild ::
  RelationalRuntimeError ctx prop boundary evidence ->
  Bool
patchFrameObstructionAllowsRebuild obstruction =
  case obstruction of
    RuntimeFactorCacheCold {} ->
      True
    RuntimeFactorCacheIncoherent {} ->
      True
    RuntimeOpFailure (RelationalRuntimeFactorPreparedRelationPatchFailed {}) ->
      True
    RuntimeOpFailure (RelationalRuntimeFactorStoragePatchFailed {}) ->
      True
    _ ->
      False
{-# INLINE patchFrameObstructionAllowsRebuild #-}

rebuildInputFrameRuntime ::
  (boundary ~ RuntimeBoundary, Ord ctx, Ord prop) =>
  AtomCarrierEmitSpec ctx prop boundary evidence ->
  QueryId ->
  FactorProgram ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (FactorInputFrame ctx prop boundary evidence joinState joinErr)
rebuildInputFrameRuntime atomSpec queryId program runtime0 = do
  prepared <-
    preparedAtomRelationsRuntime atomSpec queryId program runtime0
  store <-
    factorStoreFromRelationsRuntime queryId (parRelations prepared)
  let !input0 =
        FactorInput
          { fiStore = store,
            fiView = unrestrictedView,
            fiAtomDeltas = IntMap.empty
          }
      !preparedInput =
        preparedInputCacheFromPrepared prepared input0
      !inputStats =
        emptyRuntimeRepairInputStats
          { rrisPreparedInputRebuilds = 1,
            rrisPreparedRelationRows = preparedRelationRowCount (parRelations prepared),
            rrisStoreRebuilds = 1
          }
  pure
    FactorInputFrame
      { fifInput = input0,
        fifCache = emptyFactorCache,
        fifRuntime = parRuntime prepared,
        fifAtomReadouts = parAtomReadouts prepared,
        fifPreparedInput = preparedInput,
        fifInputStats = inputStats
      }
{-# INLINE rebuildInputFrameRuntime #-}

preparedRelationRowCount :: IntMap Relation -> Int
preparedRelationRowCount =
  sum . fmap (Map.size . relationRows)
{-# INLINE preparedRelationRowCount #-}

factorStoreFromRelationsRuntime ::
  QueryId ->
  IntMap Relation ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    Store
factorStoreFromRelationsRuntime queryId relations = do
  compiledStoragePlan <-
    first
      (RuntimeOpFailure . RelationalRuntimeFactorStoragePlanFailed queryId)
      (compileStoragePlan (storagePlanFromRelations relations))
  first
    (RuntimeOpFailure . RelationalRuntimeFactorStorageBuildFailed queryId)
    (storeFromRelationsWithPlan compiledStoragePlan relations)
{-# INLINE factorStoreFromRelationsRuntime #-}

preparedInputCacheFromPrepared ::
  PreparedAtomRelations ctx prop boundary evidence joinState joinErr ->
  FactorInput ->
  FactorPreparedInputCache
preparedInputCacheFromPrepared prepared input0 =
  FactorPreparedInputCache
    { fpicRelations = parRelations prepared,
      fpicStore = fiStore input0,
      fpicBoundaryDigests = atomBoundaryDigestsFromReadouts (parAtomReadouts prepared),
      fpicViewSignature = viewSignature (fiStore input0) (fiView input0)
    }
{-# INLINE preparedInputCacheFromPrepared #-}

patchInputFrameFromPreparedCache ::
  (boundary ~ RuntimeBoundary, Ord ctx, Ord prop) =>
  AtomCarrierEmitSpec ctx prop boundary evidence ->
  QueryId ->
  FactorRepairRequest ctx prop ->
  FactorProgram ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  IntMap RowDelta ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (FactorInputFrame ctx prop boundary evidence joinState joinErr)
patchInputFrameFromPreparedCache atomSpec queryId request program runtime0 atomDeltas =
  case factorCacheReadiness atomKeys (fpCacheState program) of
    FactorCacheCold ->
      Left (RuntimeFactorCacheCold queryId)
    FactorCacheIncoherent ->
      Left (RuntimeFactorCacheIncoherent queryId)
    FactorCacheReady _heldFrontier
      | not (factorCacheStateHasNodes (factorProgramMaintenanceNodes program) (fpCacheState program)) ->
          Left (RuntimeFactorCacheCold queryId)
      | otherwise -> do
          currentAtomReadouts <-
            factorAtomReadoutsRuntime atomSpec queryId program runtime0
          let !currentBoundaryDigests =
                atomBoundaryDigestsFromReadouts currentAtomReadouts
          preparedInput0 <-
            case fcsPreparedInput (fpCacheState program) of
              Nothing ->
                Left (RuntimeFactorCacheCold queryId)
              Just preparedInput ->
                Right preparedInput
          if fpicBoundaryDigests preparedInput0 /= currentBoundaryDigests
            then Left (RuntimeFactorCacheIncoherent queryId)
            else do
              (preparedInput1, refreshedRelationRows) <-
                patchPreparedInputCache
                  atomSpec
                  queryId
                  runtime0
                  (repairCauseRelationalScope (frrCause request))
                  atomDeltas
                  preparedInput0
              pure
                FactorInputFrame
                  { fifInput =
                      FactorInput
                        { fiStore = fpicStore preparedInput1,
                          fiView = unrestrictedView,
                          fiAtomDeltas = atomDeltas
                    },
                    fifCache = factorCacheStateToTransientCache (fpCacheState program),
                    fifRuntime = runtime0,
                    fifAtomReadouts = currentAtomReadouts,
                    fifPreparedInput = preparedInput1,
                    fifInputStats =
                      emptyRuntimeRepairInputStats
                        { rrisPreparedInputPatchHits = 1
                        , rrisPreparedRelationRows = refreshedRelationRows
                        }
                  }
  where
    !atomKeys =
      factorProgramAtomKeys program
{-# INLINE patchInputFrameFromPreparedCache #-}

patchPreparedInputCache ::
  (boundary ~ RuntimeBoundary, Ord ctx, Ord prop) =>
  AtomCarrierEmitSpec ctx prop boundary evidence ->
  QueryId ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  RelationalScope ->
  IntMap RowDelta ->
  FactorPreparedInputCache ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (FactorPreparedInputCache, Int)
patchPreparedInputCache atomSpec queryId runtime0 scopeValue atomDeltas prepared0 = do
  preparedPatch <-
    IntMap.foldlWithKey'
      (patchPreparedRelationDelta atomSpec queryId runtime0)
      ( Right
          PreparedInputPatch
            { pipRelations = fpicRelations prepared0,
              pipStoreDeltas = IntMap.empty,
              pipPreparedRelationRows = 0
            }
      )
      atomDeltas
  store1 <-
    first
      (RuntimeOpFailure . RelationalRuntimeFactorStoragePatchFailed queryId)
      ( sprStore
          <$> applyStoragePatch
            StoragePatch
              { spScope = scopeValue,
                spRowsByAtom = pipStoreDeltas preparedPatch
              }
            (fpicStore prepared0)
      )
  pure
    ( prepared0
        { fpicRelations = pipRelations preparedPatch,
          fpicStore = store1,
          fpicViewSignature = viewSignature store1 unrestrictedView
        },
      pipPreparedRelationRows preparedPatch
    )
{-# INLINE patchPreparedInputCache #-}

data PreparedInputPatch = PreparedInputPatch
  { pipRelations :: !(IntMap Relation),
    pipStoreDeltas :: !(IntMap RowDelta),
    pipPreparedRelationRows :: {-# UNPACK #-} !Int
  }

patchPreparedRelationDelta ::
  (boundary ~ RuntimeBoundary, Ord ctx, Ord prop) =>
  AtomCarrierEmitSpec ctx prop boundary evidence ->
  QueryId ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    PreparedInputPatch ->
  Int ->
  RowDelta ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    PreparedInputPatch
patchPreparedRelationDelta atomSpec queryId runtime0 eitherPatch atomKey delta = do
  patch0 <- eitherPatch
  case IntMap.lookup atomKey (pipRelations patch0) of
    Nothing ->
      refreshPreparedRelation atomSpec queryId runtime0 (Right patch0) atomKey delta
    Just relation0 -> do
      let !queryAtomId =
            mkQueryAtomId atomKey
          !atomId =
            queryAtomAsAtomId queryAtomId
      relation1 <-
        first
          (RuntimeOpFailure . RelationalRuntimeFactorPreparedRelationPatchFailed queryId atomId)
          (applyRelationPatch delta relation0)
      let !storeDeltas1 =
            if plainRowPatchNull delta
              then pipStoreDeltas patch0
              else IntMap.insert atomKey delta (pipStoreDeltas patch0)
      pure
        patch0
          { pipRelations = IntMap.insert atomKey relation1 (pipRelations patch0),
            pipStoreDeltas = storeDeltas1
          }
{-# INLINE patchPreparedRelationDelta #-}

refreshPreparedRelation ::
  (boundary ~ RuntimeBoundary, Ord ctx, Ord prop) =>
  AtomCarrierEmitSpec ctx prop boundary evidence ->
  QueryId ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    PreparedInputPatch ->
  Int ->
  RowDelta ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    PreparedInputPatch
refreshPreparedRelation atomSpec queryId runtime0 eitherPatch atomKey _delta = do
  patch0 <- eitherPatch
  (_boundaryValue, _rows, relation1, _runtime1) <-
    ensurePreparedRelationForAtomRuntime
      atomSpec
      queryId
      runtime0
      atomKey
  let !relation0 =
        IntMap.findWithDefault
          (emptyRelation (relationLayout relation1))
          atomKey
          (pipRelations patch0)
      !storeDelta =
        relationDelta relation1 relation0
      !storeDeltas1 =
        if plainRowPatchNull storeDelta
          then pipStoreDeltas patch0
          else IntMap.insert atomKey storeDelta (pipStoreDeltas patch0)
      !preparedRelationRows1 =
        pipPreparedRelationRows patch0 + Map.size (relationRows relation1)
  pure
    patch0
      { pipRelations = IntMap.insert atomKey relation1 (pipRelations patch0),
        pipStoreDeltas = storeDeltas1,
        pipPreparedRelationRows = preparedRelationRows1
      }
{-# INLINE refreshPreparedRelation #-}

relationDelta :: Relation -> Relation -> RowDelta
relationDelta newer older =
  subtractPlainRowPatch
    (plainRowPatchFromMultiplicityMap (relationRows newer))
    (plainRowPatchFromMultiplicityMap (relationRows older))
{-# INLINE relationDelta #-}

factorCacheStateHasNodes ::
  Set.Set FactorNode ->
  FactorCacheState ->
  Bool
factorCacheStateHasNodes requiredNodes state =
  requiredNodes `Set.isSubsetOf` Map.keysSet (fcsNodes state)
{-# INLINE factorCacheStateHasNodes #-}

factorAtomReadoutsRuntime ::
  (boundary ~ RuntimeBoundary, Ord ctx, Ord prop) =>
  AtomCarrierEmitSpec ctx prop boundary evidence ->
  QueryId ->
  FactorProgram ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (IntMap (RuntimeBoundary, RowDelta))
factorAtomReadoutsRuntime atomSpec queryId program runtime0 =
  Foldable.foldlM
    ensureOne
    IntMap.empty
    (IntSet.toAscList (factorProgramAtomKeys program))
  where
    ensureOne readouts atomKey = do
      readout <-
        currentFactorAtomReadout
          atomSpec
          queryId
          runtime0
          atomKey
      pure (IntMap.insert atomKey readout readouts)
{-# INLINE factorAtomReadoutsRuntime #-}

atomBoundaryDigestsFromReadouts ::
  IntMap (RuntimeBoundary, RowDelta) ->
  IntMap StableDigest128
atomBoundaryDigestsFromReadouts =
  IntMap.map (boundaryDigest . fst)
{-# INLINE atomBoundaryDigestsFromReadouts #-}

preparedAtomRelationsRuntime ::
  (boundary ~ RuntimeBoundary, Ord ctx, Ord prop) =>
  AtomCarrierEmitSpec ctx prop boundary evidence ->
  QueryId ->
  FactorProgram ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (PreparedAtomRelations ctx prop boundary evidence joinState joinErr)
preparedAtomRelationsRuntime atomSpec queryId program runtime0 =
  Foldable.foldlM
    ensureOne
    PreparedAtomRelations
      { parRelations = IntMap.empty,
        parAtomReadouts = IntMap.empty,
        parRuntime = runtime0
      }
    (IntSet.toAscList (factorProgramAtomKeys program))
  where
    ensureOne prepared atomKey = do
      (boundaryValue, rows, relation, runtime1) <-
        ensurePreparedRelationForAtomRuntime
          atomSpec
          queryId
          (parRuntime prepared)
          atomKey
      pure
        prepared
          { parRelations =
              IntMap.insert atomKey relation (parRelations prepared),
            parAtomReadouts =
              IntMap.insert
                atomKey
                (boundaryValue, rows)
                (parAtomReadouts prepared),
            parRuntime = runtime1
          }
{-# INLINE preparedAtomRelationsRuntime #-}

ensurePreparedRelationForAtomRuntime ::
  (boundary ~ RuntimeBoundary, Ord ctx, Ord prop) =>
  AtomCarrierEmitSpec ctx prop boundary evidence ->
  QueryId ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Int ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    ( RuntimeBoundary,
      RowDelta,
      Relation,
      RelDiffRuntime ctx prop boundary evidence joinState joinErr
    )
ensurePreparedRelationForAtomRuntime atomSpec queryId runtime atomKey = do
  let queryAtomId =
        mkQueryAtomId atomKey
      atomId =
        queryAtomAsAtomId queryAtomId
      addr =
        factorAtomCarrierAddr atomSpec queryId queryAtomId
  (boundaryValue, rows) <-
    currentFactorAtomReadout
      atomSpec
      queryId
      runtime
      atomKey
  let boundaryDigestValue =
        boundaryDigest boundaryValue
  relation <-
    first
      (RuntimeOpFailure . RelationalRuntimeFactorCarrierRelationProjectionFailed queryId atomId)
      ( carrierRelationProjectionFromRows
          addr
          boundaryDigestValue
          boundaryValue
          rows
      )
  pure
    ( boundaryValue,
      rows,
      relation,
      runtime
    )
{-# INLINE ensurePreparedRelationForAtomRuntime #-}

currentFactorAtomReadout ::
  (boundary ~ RuntimeBoundary, Ord ctx, Ord prop) =>
  AtomCarrierEmitSpec ctx prop boundary evidence ->
  QueryId ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Int ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RuntimeBoundary, RowDelta)
currentFactorAtomReadout atomSpec queryId runtime atomKey = do
  let queryAtomId =
        mkQueryAtomId atomKey
      atomId =
        queryAtomAsAtomId queryAtomId
      addr =
        factorAtomCarrierAddr atomSpec queryId queryAtomId
      defaultBoundary =
        cesBoundaryOf atomSpec (emptyAtomPayload queryId atomId)
  (_shard, store0) <-
    carrierStoreAtRouting
      (rsRouting (rdrState runtime))
      addr
      runtime
  let !boundaryValue =
        fromMaybe
          defaultBoundary
          (carrierBoundaryLatestTraceNow addr store0)
      !rows =
        maybe
          emptyPlainRowPatch
          carrierSnapshotRows
          (lookupCarrierSnapshot addr store0)
  pure (boundaryValue, rows)
{-# INLINE currentFactorAtomReadout #-}

carrierRelationProjectionFromRows ::
  CarrierAddr ctx Carrier prop ->
  StableDigest128 ->
  RuntimeBoundary ->
  RowDelta ->
  Either (CarrierStoreError ctx Carrier prop boundary evidence) Relation
carrierRelationProjectionFromRows addr boundaryDigestValue boundaryValue rows =
  first
    (CarrierStoreRelationProjectionBuildFailed addr boundaryDigestValue)
    ( applyRelationPatch
        rows
        (emptyRelation (Vector.fromList (runtimeBoundarySchema boundaryValue)))
    )
{-# INLINE carrierRelationProjectionFromRows #-}

factorInputSignatureRuntime ::
  (boundary ~ RuntimeBoundary, Ord ctx, Ord prop) =>
  AtomCarrierEmitSpec ctx prop boundary evidence ->
  QueryId ->
  FactorProgram ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    StableDigest128
factorInputSignatureRuntime atomSpec queryId program runtime = do
  atomReadouts <-
    traverse
      atomSignatureReadout
      (IntSet.toAscList (factorProgramAtomKeys program))
  pure (factorInputSignatureFromAtomReadouts (IntMap.fromAscList atomReadouts))
  where
    atomSignatureReadout atomKey = do
      let queryAtomId =
            mkQueryAtomId atomKey
          atomId =
            queryAtomAsAtomId queryAtomId
          addr =
            factorAtomCarrierAddr atomSpec queryId queryAtomId
      maybeSnapshot <-
        currentCarrierMaybe addr runtime
      let boundaryValue =
            maybe
              (cesBoundaryOf atomSpec (emptyAtomPayload queryId atomId))
              deBoundary
              maybeSnapshot
          rows =
            maybe
              emptyPlainRowPatch
              deRows
              maybeSnapshot
      pure (atomKey, (boundaryValue, rows))
{-# INLINE factorInputSignatureRuntime #-}

factorInputSignatureFromAtomReadouts ::
  IntMap (RuntimeBoundary, RowDelta) ->
  StableDigest128
factorInputSignatureFromAtomReadouts atomReadouts =
  stableDigest128
    ( [0x666163746f72496e, wordOfInt (IntMap.size atomReadouts)]
        <> foldMap atomSignatureWords (IntMap.toAscList atomReadouts)
    )
  where
    atomSignatureWords ::
      (Int, (RuntimeBoundary, RowDelta)) ->
      [Word64]
    atomSignatureWords (atomKey, (boundaryValue, rows)) =
      [0x61746f6d00000000, wordOfInt atomKey]
        <> stableDigestWords (boundaryDigest boundaryValue)
        <> stableDigestWords (rowDeltaDigest rows)
{-# INLINE factorInputSignatureFromAtomReadouts #-}

heldCarrierReadsForFactorProgramsRuntime ::
  (Ord ctx, Ord prop) =>
  AtomCarrierEmitSpec ctx prop boundary evidence ->
  Map RepairProgramKey FactorProgram ->
  Map QueryId RuntimeQueryBinding ->
  CarrierHeldReads ctx Carrier prop
heldCarrierReadsForFactorProgramsRuntime atomSpec programs =
  Map.foldlWithKey'
    collectBinding
    emptyCarrierHeldReads
  where
    collectBinding heldReads _queryId binding =
      case Map.lookup (rqbRepairKey binding) programs of
        Nothing ->
          heldReads
        Just program ->
          IntMap.foldlWithKey'
            (collectAtomRead (factorProgramQueryId program))
            heldReads
            (fcsAtomReads (fpCacheState program))

    collectAtomRead queryId heldReads atomKey atomRead =
      let queryAtomId =
            mkQueryAtomId atomKey
          addr =
            factorAtomCarrierAddr atomSpec queryId queryAtomId
       in insertCarrierHeldRead addr (farsFrontier atomRead) heldReads
{-# INLINE heldCarrierReadsForFactorProgramsRuntime #-}

emptyAtomPayload ::
  QueryId ->
  AtomId ->
  AtomCarrierPayload
emptyAtomPayload queryId atomId =
  AtomCarrierPayload
    { acpScope = mempty,
      acpEvent =
        AtomEvent
          { aeQueryId = queryId,
            aeAtomId = atomId,
            aeRows = emptyPlainRowPatch
          }
    }
{-# INLINE emptyAtomPayload #-}

runtimeBoundarySchema :: RuntimeBoundary -> [SlotId]
runtimeBoundarySchema =
  bsSchema . boundaryShape
{-# INLINE runtimeBoundarySchema #-}

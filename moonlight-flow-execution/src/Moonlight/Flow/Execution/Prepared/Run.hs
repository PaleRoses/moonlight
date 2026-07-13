{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Flow.Execution.Prepared.Run
  ( PreparedOp (..),
    SupportView (..),
    supportIds,
    supportRelations,
    supportExists,

    PreparedRunKind (..),
    PreparedRunMode (..),
    PreparedRunSpec (..),
    PreparedResult (..),
    PreparedProvenanceError (..),
    PreparedProvenanceRow (..),
    PreparedProvenanceRows (..),

    runPrepared,
    runPreparedMeasuredWithDecomp,
    runPreparedValueWithDecomp,
    runPreparedValueWithStructuralSourcesWithDecomp,
    preparedOpRestriction,
    structuralDecompFromPlan,

    materializeSupportRelations,
    supportRowCount,
    boolAsInt,
  )
where

import Control.Monad (foldM)
import Data.HashSet qualified as HashSet
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Kind (Type)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Vector qualified as Vector
import Data.Vector.Unboxed qualified as VU
import Data.Word
  ( Word64,
  )
import Moonlight.Delta.Signed
  ( multiplicityChangeValue,
  )
import Moonlight.Flow.Execution.Factor.Enumerate
  ( attachPreparedRowProvenance,
    enumerateBagRowsBounded,
  )
import Moonlight.Delta.Patch qualified as CorePatch
import Moonlight.Flow.Plan.Physical.Meta
  ( decompFromJoinForest,
  )
import Moonlight.Flow.Execution.Factor.Run
  ( factorRunTelemetry,
    runFactor,
  )
import Moonlight.Flow.Execution.Factor.Types
  ( FactorCache (..),
    FactorEntry (..),
    FactorDemand (..),
    FactorRunResult (..),
    FactorRunSpec (..),
    emptyFactorCache,
    factorCacheLookup,
    factorCacheFactorAt,
    factorInputFromStoreView,
  )
import Moonlight.Flow.Execution.Dense.Plan
  ( DenseArrangement,
    denseRestrictArrangementByPinnedRows,
    denseRestrictArrangementBySlotValues,
    mkDenseJoinPlan,
  )
import Moonlight.Flow.Execution.Dense.WCOJ
  ( denseJoinDeltaRows,
    denseJoinRows,
  )
import Moonlight.Differential.Row.Delta
  ( RowDelta
  )
import Moonlight.Differential.Row.Patch
  ( plainRowPatchChangeMap,
    plainRowPatchFromChangeMap,
    plainRowPatchNull,
    ShapedPatch (..),
  )
import Moonlight.Flow.Execution.Observe.Provenance.Types
  ( ProvenanceObstruction (..),
    ProvVal (..),
    emptyProvArena,
  )
import Moonlight.Flow.Execution.Observe.Provenance.GC
  ( defaultProvGCConfig,
  )
import Moonlight.Flow.Execution.Observe.Telemetry
  ( defaultRepairTelemetryConfig,
    DispatchBranch (..),
    DispatchTelemetry,
    FactorCacheTelemetry,
    mkDispatchTelemetry,
  )
import Moonlight.Flow.Execution.Prepared.Contract
  ( PreparedOp (..),
    PreparedProvenanceError (..),
    PreparedProvenanceRow (..),
    PreparedProvenanceRows (..),
    SupportView (..),
    mkSupportView,
    supportExists,
    supportIds,
  )
import Moonlight.Differential.Index.RowSet
  ( rowSetMember,
    rowSetSize,
    rowSetToList,
  )
import Moonlight.Differential.Index.IndexedRows
  ( indexedRowsPayloadMap,
  )
import Moonlight.Differential.Row.Tuple
import Moonlight.Flow.Plan.Query.Core
import Moonlight.Differential.Row.Block
import Moonlight.Flow.Model.RowIdentity
  ( rowBlockIdentityForAtom,
  )
import Moonlight.Flow.Storage.Relation
import Moonlight.Flow.Storage.Restriction
  ( Restriction,
    applyRestriction,
    emptyRestriction,
    restrictionSlotValues,
    restrictionPinnedRowsByAtom,
    restrictionSlotValueSets,
    restrictPinnedRow,
  )
import Moonlight.Flow.Storage.Store
  ( Store,
    storeFromRelations,
    storeRelations,
  )
import Moonlight.Flow.Storage.View
  ( SupportIds,
    View,
    unrestrictedView,
    viewRows,
  )

supportRelations ::
  QueryPlan compiled output guard tag tuple key ->
  Store ->
  SupportView ->
  Either RowBuildError (IntMap (RowBlock 'Canonical))
supportRelations plan store support =
  materializeSupportRelations
    plan
    store
    (supportIds support)
{-# INLINE supportRelations #-}

type PreparedRunKind :: Type
data PreparedRunKind
  = PreparedValueRun
  | PreparedMeasuredRun

type family PreparedResultTelemetry mode where
  PreparedResultTelemetry 'PreparedValueRun = ()
  PreparedResultTelemetry 'PreparedMeasuredRun = DispatchTelemetry

type family PreparedResultFactorCache mode where
  PreparedResultFactorCache 'PreparedValueRun = ()
  PreparedResultFactorCache 'PreparedMeasuredRun = Maybe FactorCache

type PreparedRunMode :: PreparedRunKind -> Type
data PreparedRunMode mode where
  PreparedValueOnly :: PreparedRunMode 'PreparedValueRun
  PreparedMeasuredFresh :: PreparedRunMode 'PreparedMeasuredRun
  PreparedMeasuredCached :: !FactorCache -> PreparedRunMode 'PreparedMeasuredRun

type PreparedRunSpec ::
  PreparedRunKind ->
  Type ->
  Type ->
  Type ->
  Type ->
  Type ->
  Type ->
  Type ->
  Type
data PreparedRunSpec mode compiled output guard tag tuple key a = PreparedRunSpec
  { prsPlan :: !(QueryPlan compiled output guard tag tuple key),
    prsRestriction :: !Restriction,
    prsStore :: !Store,
    prsView :: !View,
    prsAtomDeltas :: !(IntMap RowDelta),
    prsStructuralSources :: !(Maybe [DenseArrangement]),
    prsOp :: !(PreparedOp a),
    prsMode :: !(PreparedRunMode mode)
  }

type PreparedResult :: PreparedRunKind -> Type -> Type
data PreparedResult mode a = PreparedResult
  { prValue :: !a,
    prTelemetry :: !(PreparedResultTelemetry mode),
    prFactorCache :: !(PreparedResultFactorCache mode)
  }

type PreparedOpMeasurement :: Type
data PreparedOpMeasurement = PreparedOpMeasurement
  { pomRowsEmitted :: {-# UNPACK #-} !Int,
    pomSupportAtoms :: !(Maybe Int),
    pomSupportRows :: !(Maybe Int)
  }

type PreparedInternalMode :: Type
data PreparedInternalMode
  = PreparedInternalValueOnly
  | PreparedInternalMeasured !(Maybe FactorCache)

type PreparedExecution :: Type -> Type
data PreparedExecution a = PreparedExecution
  { pexValue :: !a,
    pexBranch :: !DispatchBranch,
    pexFactorCache :: !(Maybe FactorCache),
    pexFactorTelemetry :: !(Maybe FactorCacheTelemetry)
  }

runPrepared ::
  PreparedRunSpec mode compiled output guard tag tuple key a ->
  Either ProvenanceObstruction (PreparedResult mode a)
runPrepared spec =
  case prsMode spec of
    PreparedValueOnly -> do
      execution <-
        runPreparedExecution
          PreparedInternalValueOnly
          spec
          Nothing
      pure
        PreparedResult
          { prValue = pexValue execution,
            prTelemetry = (),
            prFactorCache = ()
          }
    PreparedMeasuredFresh ->
      runPreparedMeasured Nothing spec
    PreparedMeasuredCached cache0 ->
      runPreparedMeasured (Just cache0) spec

runPreparedMeasured ::
  Maybe FactorCache ->
  PreparedRunSpec 'PreparedMeasuredRun compiled output guard tag tuple key a ->
  Either ProvenanceObstruction (PreparedResult 'PreparedMeasuredRun a)
runPreparedMeasured maybeCache spec = do
  let plan =
        prsPlan spec
      sourceStore =
        prsStore spec
      sourceView =
        prsView spec
      maybeRestrictedView =
        structuralRestrictedView plan (prsRestriction spec) (prsOp spec) sourceStore sourceView

  execution <-
    runPreparedExecution
      (PreparedInternalMeasured maybeCache)
      spec
      maybeRestrictedView

  let measurement =
        preparedOpMeasurement (prsOp spec) (pexValue execution)
      dispatchTelemetry =
        mkDispatchTelemetry
          (pexBranch execution)
          Nothing
          Nothing
          (pomRowsEmitted measurement)
          (pomSupportAtoms measurement)
          (pomSupportRows measurement)
          Nothing
          (pexFactorTelemetry execution)

  pure
    PreparedResult
      { prValue = pexValue execution,
        prTelemetry = dispatchTelemetry,
        prFactorCache = pexFactorCache execution
      }

runPreparedMeasuredWithDecomp ::
  QueryPlan compiled output guard tag tuple key ->
  Restriction ->
  Store ->
  View ->
  IntMap RowDelta ->
  DecompPlan ->
  PreparedOp a ->
  FactorCache ->
  Either ProvenanceObstruction (PreparedResult 'PreparedMeasuredRun a)
runPreparedMeasuredWithDecomp plan restriction sourceStore sourceView atomDeltas decomp op cache0 = do
  execution <-
    case qpDomain plan of
      RootDomainQueryPlan ->
        runRootDomainExecution plan restriction op
      StructuralQueryPlan ->
        runFactorizedPreparedRequest
          (PreparedInternalMeasured (Just cache0))
          (Vector.toList (qpFullSchema plan))
          plan
          Nothing
          sourceStore
          restrictedView
          restrictedAtomDeltas
          decomp
          op
  let measurement =
        preparedOpMeasurement op (pexValue execution)
      dispatchTelemetry =
        mkDispatchTelemetry
          (pexBranch execution)
          Nothing
          Nothing
          (pomRowsEmitted measurement)
          (pomSupportAtoms measurement)
          (pomSupportRows measurement)
          Nothing
          (pexFactorTelemetry execution)
  pure
    PreparedResult
      { prValue = pexValue execution,
        prTelemetry = dispatchTelemetry,
        prFactorCache = pexFactorCache execution
      }
  where
    restrictedView =
      applyRestriction (restriction <> preparedOpRestriction op) sourceStore sourceView

    restrictedAtomDeltas =
      restrictAtomDeltasToView sourceStore restrictedView atomDeltas
{-# INLINE runPreparedMeasuredWithDecomp #-}

runPreparedExecution ::
  PreparedInternalMode ->
  PreparedRunSpec mode compiled output guard tag tuple key a ->
  Maybe View ->
  Either ProvenanceObstruction (PreparedExecution a)
runPreparedExecution internalMode spec maybeRestrictedView =
  case qpDomain plan of
    RootDomainQueryPlan ->
      runRootDomainExecution plan restriction op
    StructuralQueryPlan ->
      runFactorizedPreparedRequest
        internalMode
        fullSchema
        plan
        restrictedStructuralSources
        sourceStore
        restrictedView
        restrictedAtomDeltas
        (structuralDecompFromPlan plan)
        op
  where
    plan =
      prsPlan spec

    restriction =
      prsRestriction spec

    sourceStore =
      prsStore spec

    sourceView =
      prsView spec

    op =
      prsOp spec

    fullSchema =
      Vector.toList (qpFullSchema plan)

    restrictedView =
      fromMaybe
        (applyRestriction (restriction <> preparedOpRestriction op) sourceStore sourceView)
        maybeRestrictedView

    restrictedAtomDeltas =
      restrictAtomDeltasToView sourceStore restrictedView (prsAtomDeltas spec)

    restrictedStructuralSources =
      restrictStructuralSources
        (restriction <> preparedOpRestriction op)
        <$> prsStructuralSources spec

runRootDomainExecution ::
  QueryPlan compiled output guard tag tuple key ->
  Restriction ->
  PreparedOp a ->
  Either ProvenanceObstruction (PreparedExecution a)
runRootDomainExecution plan restriction op =
  Right
    ( case op of
        PreparedRows maybeLimit ->
          staticExecution DispatchAdaptive (limitRows maybeLimit (rootDomainRows plan restriction))
        PreparedDeltaRows _maybeLimit ->
          staticExecution DispatchAdaptive []
        PreparedRowsWithProvenance maybeLimit ->
          staticExecution DispatchAdaptive $
            Right
              PreparedProvenanceRows
                { pprsArena = emptyProvArena,
                  pprsRows =
                    fmap
                      (\rowValue -> PreparedProvenanceRow rowValue [PVOne])
                      (limitRows maybeLimit (rootDomainRows plan restriction))
                }
        PreparedSupport ->
          staticExecution DispatchAdaptive (mkSupportView IntMap.empty)
        PreparedExists ->
          staticExecution DispatchAdaptive (not (null (rootDomainRows plan restriction)))
        PreparedExistsPinned _atomId _pinnedRow ->
          staticExecution DispatchAdaptive False
    )

runFactorizedPreparedRequest ::
  PreparedInternalMode ->
  [SlotId] ->
  QueryPlan compiled output guard tag tuple key ->
  Maybe [DenseArrangement] ->
  Store ->
  View ->
  IntMap RowDelta ->
  DecompPlan ->
  PreparedOp a ->
  Either ProvenanceObstruction (PreparedExecution a)
runFactorizedPreparedRequest internalMode fullSchema plan maybeStructuralSources sourceStore restrictedView atomDeltas decomp op =
  case op of
    PreparedRows maybeLimit ->
      case maybeStructuralSources of
        Just structuralSources ->
          Right
            ( staticExecution
                DispatchAdaptive
                (limitRows maybeLimit (denseStructuralRowsFromSources decomp plan structuralSources))
            )
        Nothing -> do
          result <- runDemand FactorDemandRows
          let !value =
                enumerateBagRowsBounded maybeLimit fullSchema decomp (frrPreSealCache result)
          pure (finish result value)

    PreparedDeltaRows maybeLimit ->
      case maybeStructuralSources of
        Just structuralSources ->
          Right
            ( staticExecution
                DispatchAdaptive
                (limitRows maybeLimit (denseStructuralDeltaRowsFromSources decomp plan structuralSources))
            )
        Nothing -> do
          result <- runDemand FactorDemandRows
          let !value =
                limitRows maybeLimit (factorRootDeltaRows result)
          pure (finish result value)

    PreparedRowsWithProvenance maybeLimit ->
      case maybeStructuralSources of
        Just _ ->
          Right
            ( staticExecution
                DispatchAdaptive
                (Left PreparedProvenanceRequiresFactorCache)
            )
        Nothing -> do
          result <- runDemand FactorDemandRows
          let cache = frrCache result
              rows = enumerateBagRowsBounded maybeLimit fullSchema decomp cache
              value =
                fmap
                  (PreparedProvenanceRows (fcArena cache))
                  (attachPreparedRowProvenance fullSchema decomp cache rows)
          pure (finish result value)

    PreparedSupport -> do
      result <- runDemand FactorDemandSupport
      let !value =
            mkSupportView (frrSupport result)
      pure (finish result value)

    PreparedExists -> do
      result <- runDemand FactorDemandMaintenance
      value <- rootFactorExists (frrPreSealCache result)
      pure (finish result value)

    PreparedExistsPinned _atomId _pinnedRow -> do
      result <- runDemand FactorDemandMaintenance
      value <- rootFactorExists (frrPreSealCache result)
      pure (finish result value)
  where
    runDemand ::
      FactorDemand support ->
      Either ProvenanceObstruction (FactorRunResult support)
    runDemand demand =
      runFactor
        FactorRunSpec
          { frsDecomp = decomp,
            frsInput =
              factorInputFromStoreView sourceStore restrictedView atomDeltas,
            frsCache = preparedInternalFactorCache internalMode,
            frsGc = defaultProvGCConfig,
            frsRepairTelemetry = defaultRepairTelemetryConfig,
            frsDemand = demand
          }

    finish ::
      FactorRunResult support ->
      b ->
      PreparedExecution b
    finish result value =
      PreparedExecution
        { pexValue = value,
          pexBranch = DispatchFactorized,
          pexFactorCache = retainedFactorCache internalMode (frrCache result),
          pexFactorTelemetry = retainedFactorTelemetry internalMode result
        }

staticExecution :: DispatchBranch -> a -> PreparedExecution a
staticExecution branch value =
  PreparedExecution
    { pexValue = value,
      pexBranch = branch,
      pexFactorCache = Nothing,
      pexFactorTelemetry = Nothing
    }
{-# INLINE staticExecution #-}

preparedInternalFactorCache :: PreparedInternalMode -> FactorCache
preparedInternalFactorCache internalMode =
  case internalMode of
    PreparedInternalValueOnly ->
      emptyFactorCache
    PreparedInternalMeasured Nothing ->
      emptyFactorCache
    PreparedInternalMeasured (Just cache0) ->
      cache0
{-# INLINE preparedInternalFactorCache #-}

retainedFactorTelemetry ::
  PreparedInternalMode ->
  FactorRunResult support ->
  Maybe FactorCacheTelemetry
retainedFactorTelemetry internalMode result =
  case internalMode of
    PreparedInternalMeasured {} ->
      Just (factorRunTelemetry result)
    PreparedInternalValueOnly ->
      Nothing
{-# INLINE retainedFactorTelemetry #-}

retainedFactorCache :: PreparedInternalMode -> FactorCache -> Maybe FactorCache
retainedFactorCache internalMode cache1 =
  case internalMode of
    PreparedInternalMeasured (Just _) ->
      Just cache1
    _ ->
      Nothing
{-# INLINE retainedFactorCache #-}

rootFactorExists :: FactorCache -> Either ProvenanceObstruction Bool
rootFactorExists cache =
  case factorCacheFactorAt FactorNodeRoot cache of
    Nothing ->
      Right False
    Just rootFactor ->
      case Map.findWithDefault PVZero emptyTupleKey (indexedRowsPayloadMap rootFactor) of
        PVZero ->
          Right False
        PVOne ->
          Right True
        PVRef _ ->
          Right True
        PVObstructed obstruction ->
          Left obstruction
{-# INLINE rootFactorExists #-}

runPreparedValueWithDecomp ::
  QueryPlan compiled output guard tag tuple key ->
  Restriction ->
  Store ->
  View ->
  DecompPlan ->
  PreparedOp a ->
  Either ProvenanceObstruction a
runPreparedValueWithDecomp plan restriction sourceStore sourceView decomp op =
  pexValue
    <$> case qpDomain plan of
      RootDomainQueryPlan ->
        runRootDomainExecution plan restriction op
      StructuralQueryPlan ->
        runFactorizedPreparedRequest
          PreparedInternalValueOnly
          (Vector.toList (qpFullSchema plan))
          plan
          Nothing
          sourceStore
          (applyRestriction (restriction <> preparedOpRestriction op) sourceStore sourceView)
          IntMap.empty
          decomp
          op
{-# INLINE runPreparedValueWithDecomp #-}

runPreparedValueWithStructuralSourcesWithDecomp ::
  QueryPlan compiled output guard tag tuple key ->
  Restriction ->
  DecompPlan ->
  [DenseArrangement] ->
  PreparedOp a ->
  Either ProvenanceObstruction a
runPreparedValueWithStructuralSourcesWithDecomp plan restriction decomp sources op =
  pexValue
    <$> runFactorizedPreparedRequest
      PreparedInternalValueOnly
      (Vector.toList (qpFullSchema plan))
      plan
      (Just (restrictStructuralSources (restriction <> preparedOpRestriction op) sources))
      store
      view
      IntMap.empty
      decomp
      op
  where
    store =
      storeFromRelations IntMap.empty

    view =
      unrestrictedView
{-# INLINE runPreparedValueWithStructuralSourcesWithDecomp #-}

structuralDecompFromPlan ::
  QueryPlan compiled output guard tag tuple key ->
  DecompPlan
structuralDecompFromPlan plan =
  foldJoinShape
    (denseDecompFromPlan plan)
    (\forest -> decompFromJoinForest forest (jmAtomSchemas (qpJoinMeta plan)))
    id
    (jmShape (qpJoinMeta plan))

denseDecompFromPlan ::
  QueryPlan compiled output guard tag tuple key ->
  DecompPlan
denseDecompFromPlan plan =
  mkDecompPlan
    rootBag
    (IntMap.singleton rootBagKey rootBagPlan)
    IntMap.empty
    IntMap.empty
    mempty
    (IntMap.map (const rootBag) atomSchemas)
  where
    !rootBagKey =
      0

    !rootBag =
      BagId rootBagKey

    !atomSchemas =
      jmAtomSchemas (qpJoinMeta plan)

    !rootBagPlan =
      mkDecompBag
        rootBag
        (Vector.toList (qpFullSchema plan))
        (IntMap.keysSet atomSchemas)

structuralRestrictedView ::
  QueryPlan compiled output guard tag tuple key ->
  Restriction ->
  PreparedOp a ->
  Store ->
  View ->
  Maybe View
structuralRestrictedView plan restriction op store view =
  case qpDomain plan of
    RootDomainQueryPlan ->
      Nothing
    StructuralQueryPlan ->
      Just (applyRestriction (restriction <> preparedOpRestriction op) store view)

denseStructuralRowsFromSources ::
  DecompPlan ->
  QueryPlan compiled output guard tag tuple key ->
  [DenseArrangement] ->
  [RowTupleKey]
denseStructuralRowsFromSources _decomp plan =
  List.sortOn (canonicalRowBlockOrderKey fullSchema) . denseJoinRows . mkDenseJoinPlan fullSchema fullSchema
  where
    !fullSchema =
      Vector.toList (qpFullSchema plan)
{-# INLINE denseStructuralRowsFromSources #-}

denseStructuralDeltaRowsFromSources ::
  DecompPlan ->
  QueryPlan compiled output guard tag tuple key ->
  [DenseArrangement] ->
  [RowTupleKey]
denseStructuralDeltaRowsFromSources _decomp plan =
  List.sortOn (canonicalRowBlockOrderKey fullSchema) . denseJoinDeltaRows . mkDenseJoinPlan fullSchema fullSchema
  where
    !fullSchema =
      Vector.toList (qpFullSchema plan)
{-# INLINE denseStructuralDeltaRowsFromSources #-}

canonicalRowBlockOrderKey :: [SlotId] -> RowTupleKey -> (Word64, Int, [Word64])
canonicalRowBlockOrderKey schema row =
  case tupleKeyToWord64Vector row of
    Left _negative ->
      (maxBound, 0, [])
    Right slots ->
      let !rowHash =
            hashRowFromSlots (layoutHash (Vector.fromList schema)) (VU.length slots) (VU.unsafeIndex slots)
          !slotWords =
            VU.toList slots
          !width =
            length slotWords
       in (rowHash, width, slotWords)
{-# INLINE canonicalRowBlockOrderKey #-}

factorRootDeltaRows ::
  FactorRunResult support ->
  [RowTupleKey]
factorRootDeltaRows result =
  maybe [] (factorDeltaRows . feDelta) (factorCacheLookup FactorNodeRoot (frrPreSealCache result))
{-# INLINE factorRootDeltaRows #-}

factorDeltaRows ::
  ShapedPatch schema AssignmentTupleKey ProvVal ->
  [RowTupleKey]
factorDeltaRows delta =
  [ coerceTupleKey key
    | (key, change) <- CorePatch.toAscList (spdDelta delta),
      Just value <- [CorePatch.cellAfter change],
      value /= PVZero
  ]
{-# INLINE factorDeltaRows #-}

restrictStructuralSources ::
  Restriction ->
  [DenseArrangement] ->
  [DenseArrangement]
restrictStructuralSources restriction =
  fmap
    ( denseRestrictArrangementByPinnedRows pinnedRows
        . denseRestrictArrangementBySlotValues slotValues
    )
  where
    !slotValues =
      restrictionSlotValueSets restriction

    !pinnedRows =
      restrictionPinnedRowsByAtom restriction
{-# INLINE restrictStructuralSources #-}

restrictAtomDeltasToView ::
  Store ->
  View ->
  IntMap RowDelta ->
  IntMap RowDelta
restrictAtomDeltasToView store restrictedView =
  IntMap.mapMaybeWithKey restrictOne
  where
    restrictOne atomKey delta =
      case IntMap.lookup atomKey (storeRelations store) of
        Nothing ->
          Nothing
        Just relation ->
          let visibleRows =
                viewRows store restrictedView atomKey
              filtered =
                plainRowPatchFromChangeMap $
                  Map.fromAscList
                    [ (row, multiplicity)
                      | (row, multiplicity) <- Map.toAscList (plainRowPatchChangeMap delta),
                      multiplicityChangeValue multiplicity < 0
                        || maybe
                          False
                          (`rowSetMember` visibleRows)
                          (rowIdForRow relation row)
                    ]
           in if plainRowPatchNull filtered
                then Nothing
                else Just filtered
{-# INLINE restrictAtomDeltasToView #-}

preparedOpRestriction :: PreparedOp a -> Restriction
preparedOpRestriction op =
  case op of
    PreparedExistsPinned atomId pinnedRow ->
      restrictPinnedRow atomId pinnedRow
    PreparedDeltaRows _ ->
      emptyRestriction
    PreparedRows _ ->
      emptyRestriction
    PreparedRowsWithProvenance _ ->
      emptyRestriction
    PreparedSupport ->
      emptyRestriction
    PreparedExists ->
      emptyRestriction

rootDomainRows ::
  QueryPlan compiled output guard tag tuple key ->
  Restriction ->
  [RowTupleKey]
rootDomainRows plan restriction =
  maybe
    []
    (fmap rootDomainRow . List.sort . HashSet.toList)
    (restrictionSlotValues restriction (qpRootSlot plan))
  where
    rootDomainRow (RepKey rootKey) =
      tupleKeyFromInts (replicate (Vector.length (qpFullSchema plan)) rootKey)

preparedOpMeasurement :: PreparedOp a -> a -> PreparedOpMeasurement
preparedOpMeasurement op value =
  case op of
    PreparedDeltaRows _ ->
      rowsMeasurement value
    PreparedRows _ ->
      rowsMeasurement value
    PreparedRowsWithProvenance _ ->
      provenanceRowsMeasurement value
    PreparedSupport ->
      supportMeasurement (supportIds value)
    PreparedExists ->
      boolMeasurement value
    PreparedExistsPinned _atomId _pinnedRow ->
      boolMeasurement value
{-# INLINE preparedOpMeasurement #-}

provenanceRowsMeasurement ::
  Either PreparedProvenanceError PreparedProvenanceRows ->
  PreparedOpMeasurement
provenanceRowsMeasurement =
  either
    (const (rowsMeasurement []))
    (rowsMeasurement . fmap pprTuple . pprsRows)
{-# INLINE provenanceRowsMeasurement #-}

limitRows :: Maybe Int -> [row] -> [row]
limitRows maybeLimit rows =
  case maybeLimit of
    Nothing ->
      rows

    Just limit ->
      take (max 0 limit) rows
{-# INLINE limitRows #-}

rowsMeasurement :: [RowTupleKey] -> PreparedOpMeasurement
rowsMeasurement rows =
  let !rowCount = length rows
   in PreparedOpMeasurement
        { pomRowsEmitted = rowCount,
          pomSupportAtoms = Nothing,
          pomSupportRows = Nothing
        }
{-# INLINE rowsMeasurement #-}

supportMeasurement :: SupportIds -> PreparedOpMeasurement
supportMeasurement support =
  let !supportAtoms = IntMap.size support
      !supportRows = supportRowCount support
   in PreparedOpMeasurement
        { pomRowsEmitted = supportRows,
          pomSupportAtoms = Just supportAtoms,
          pomSupportRows = Just supportRows
        }
{-# INLINE supportMeasurement #-}

boolMeasurement :: Bool -> PreparedOpMeasurement
boolMeasurement existsResult =
  PreparedOpMeasurement
    { pomRowsEmitted = boolAsInt existsResult,
      pomSupportAtoms = Nothing,
      pomSupportRows = Nothing
    }
{-# INLINE boolMeasurement #-}

materializeSupportRelations ::
  QueryPlan compiled output guard tag tuple key ->
  Store ->
  SupportIds ->
  Either RowBuildError (IntMap (RowBlock 'Canonical))
materializeSupportRelations plan store supportIds0 =
  let prepared =
        storeRelations store
      materialize supportRelationsAcc (atomKey, rowIds) =
        case IntMap.lookup atomKey prepared of
          Nothing ->
            Right supportRelationsAcc
          Just preparedRelation ->
            let rows =
                  mapMaybe
                    (rowForId preparedRelation)
                    (rowSetToList rowIds)
                identityValue =
                  rowBlockIdentityForAtom 0 0 (qpFingerprint plan) (mkAtomId atomKey) 0
                schemaValue =
                  relationLayout preparedRelation
             in if null rows
                  then Right supportRelationsAcc
                  else do
                    relation <-
                      atomRowsFromTupleKeys
                        identityValue
                        schemaValue
                        rows
                    pure (IntMap.insert atomKey relation supportRelationsAcc)
   in foldM materialize IntMap.empty (IntMap.toAscList supportIds0)

supportRowCount :: SupportIds -> Int
supportRowCount =
  sum . fmap rowSetSize . IntMap.elems
{-# INLINE supportRowCount #-}

boolAsInt :: Bool -> Int
boolAsInt =
  fromEnum
{-# INLINE boolAsInt #-}

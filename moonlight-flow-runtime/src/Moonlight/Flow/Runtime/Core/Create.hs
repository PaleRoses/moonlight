{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Moonlight.Flow.Runtime.Core.Create
  ( CompiledRuntimeSpec (..),
    compileRuntimeSpec,
    deferInitialData,
  )
where

import Control.Monad
  ( unless,
  )
import Data.Foldable qualified as Foldable
import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Core
  ( AtomId,
    atomIdKey,
    mkAtomId,
    queryIdKey,
  )
import Moonlight.Differential.Proposition
  ( PropositionKey,
  )
import Moonlight.Flow.Model.Delta
  ( AtomPatch,
    atomPatchRows
  )
import Moonlight.Differential.Row.Patch
  ( plainRowPatchChangeMap,
  )
import Moonlight.Differential.Row.Tuple
  ( tupleKeyWidth,
  )
import Moonlight.Flow.Runtime.Core.Env
  ( RuntimeEnvelope (..),
  )
import Moonlight.Flow.Runtime.Core.Patch.Internal
  ( Patch (..),
  )
import Moonlight.Flow.Runtime.Core.State qualified as Core
import Moonlight.Flow.Runtime.Spec.Schema
  ( RuntimeAtomSchema (..),
    RuntimeContextSchema (..),
    RuntimeInitialData (..),
    RuntimePlan (..),
    RuntimeSchema (..),
    RuntimeSpec (..),
    RuntimeSpecError (..),
    runtimePlanQueryId,
    runtimePlanSourceAtomKeys,
  )

data CompiledRuntimeSpec ctx prop =
  CompiledRuntimeSpec
    { crsRuntimeSpec :: !(RuntimeSpec ctx prop),
      crsAtomSchemas :: !(IntMap RuntimeAtomSchema),
      crsQueryContexts :: !(IntMap ctx),
      crsQueryProps :: !(IntMap (PropositionKey prop)),
      crsDefaultContext :: !ctx,
      crsDefaultProp :: !(PropositionKey prop),
      crsInitialData :: !(RuntimeInitialData ctx prop)
    }

compileRuntimeSpec ::
  forall ctx prop.
  (Ord ctx, Ord prop) =>
  RuntimeSpec ctx prop ->
  Either (RuntimeSpecError ctx prop) (CompiledRuntimeSpec ctx prop)
compileRuntimeSpec spec@RuntimeSpec {rsSchema, rsPlans, rsInitialData} = do
  defaultContext <-
    firstContext
  defaultProp <-
    firstRuntimeProp rsSchema rsPlans
  atomSchemas <-
    compileRuntimeAtomSchemas rsSchema
  validatePlans rsSchema rsPlans
  validateInitialData atomSchemas rsInitialData
  queryRoutes <-
    compileQueryRoutes rsPlans
  pure
    CompiledRuntimeSpec
      { crsRuntimeSpec = spec,
        crsAtomSchemas = atomSchemas,
        crsQueryContexts = fmap fst queryRoutes,
        crsQueryProps = fmap snd queryRoutes,
        crsDefaultContext = defaultContext,
        crsDefaultProp = defaultProp,
        crsInitialData = rsInitialData
      }
  where
    firstContext =
      case Map.lookupMin (rscContexts rsSchema) of
        Nothing ->
          Left RuntimeSpecEmptyContexts
        Just (contextValue, _shape) ->
          Right contextValue
{-# INLINE compileRuntimeSpec #-}

deferInitialData ::
  CompiledRuntimeSpec ctx prop ->
  RuntimeEnvelope (Core.RuntimeState topology engine carrier factor) env ->
  RuntimeEnvelope (Core.RuntimeState topology engine carrier factor) env
deferInitialData compiled runtime =
  case crsInitialData compiled of
    RuntimeInitialData patch ->
      runtime
        { rdrState =
            Core.setRuntimeSeedState
              (Core.runtimeSeedStateFromPatch patch)
              (rdrState runtime)
        }
{-# INLINE deferInitialData #-}

validatePlans ::
  forall ctx prop.
  (Ord ctx, Ord prop) =>
  RuntimeSchema ctx prop ->
  [RuntimePlan ctx prop] ->
  Either (RuntimeSpecError ctx prop) ()
validatePlans schema plans =
  Foldable.traverse_ validatePlan plans
  where
    validatePlan plan@RuntimePlan {rpContext, rpProp} = do
      contextSchema <-
        case Map.lookup rpContext (rscContexts schema) of
          Nothing ->
            Left (RuntimeSpecPlanContextMissing rpContext)
          Just value ->
            Right value
      unless (Set.member rpProp (rcsPropositions contextSchema)) $
        Left (RuntimeSpecPlanPropositionMissing rpContext rpProp)
      Foldable.traverse_
        (validatePlanAtom contextSchema plan)
        (IntSet.toAscList (runtimePlanSourceAtomKeys plan))

    validatePlanAtom ::
      RuntimeContextSchema prop ->
      RuntimePlan ctx prop ->
      Int ->
      Either (RuntimeSpecError ctx prop) ()
    validatePlanAtom contextSchema plan atomKey =
      let atomId =
            mkAtomId atomKey
       in unless
            (Map.member atomId (rcsAtoms contextSchema))
            ( Left
                ( RuntimeSpecPlanAtomUndeclared
                    (rpContext plan)
                    (runtimePlanQueryId plan)
                    atomId
                )
            )
{-# INLINE validatePlans #-}

compileRuntimeAtomSchemas ::
  forall ctx prop.
  RuntimeSchema ctx prop ->
  Either (RuntimeSpecError ctx prop) (IntMap RuntimeAtomSchema)
compileRuntimeAtomSchemas schema =
  Map.foldlWithKey'
    ( \eitherAtoms _contextValue contextSchema -> do
        atoms <- eitherAtoms
        Map.foldlWithKey'
          insertAtom
          (Right atoms)
          (rcsAtoms contextSchema)
    )
    (Right IntMap.empty)
    (rscContexts schema)
  where
    insertAtom ::
      Either (RuntimeSpecError ctx prop) (IntMap RuntimeAtomSchema) ->
      AtomId ->
      RuntimeAtomSchema ->
      Either (RuntimeSpecError ctx prop) (IntMap RuntimeAtomSchema)
    insertAtom eitherAtoms atomId atomSchema = do
      atoms <- eitherAtoms
      case IntMap.lookup (atomIdKey atomId) atoms of
        Nothing ->
          Right (IntMap.insert (atomIdKey atomId) atomSchema atoms)
        Just existing ->
          case mergeAtomSchema existing atomSchema of
            Just merged ->
              Right (IntMap.insert (atomIdKey atomId) merged atoms)
            Nothing ->
              Left
                ( RuntimeSpecAtomSchemaConflict
                    atomId
                    existing
                    atomSchema
                )
{-# INLINE compileRuntimeAtomSchemas #-}


mergeAtomSchema ::
  RuntimeAtomSchema ->
  RuntimeAtomSchema ->
  Maybe RuntimeAtomSchema
mergeAtomSchema left right
  | rasColumns left /= rasColumns right =
      Nothing
  | rasBoundarySensitiveSlots left /= rasBoundarySensitiveSlots right =
      Nothing
  | rasBoundarySlotKeys left /= rasBoundarySlotKeys right =
      Nothing
  | otherwise =
      Just
        left
          { rasTouchDeps =
              IntSet.union (rasTouchDeps left) (rasTouchDeps right),
            rasTouchTopo =
              IntSet.union (rasTouchTopo left) (rasTouchTopo right)
          }
{-# INLINE mergeAtomSchema #-}

validateInitialData ::
  forall ctx prop.
  IntMap RuntimeAtomSchema ->
  RuntimeInitialData ctx prop ->
  Either (RuntimeSpecError ctx prop) ()
validateInitialData atomSchemas (RuntimeInitialData patch) =
  IntMap.foldlWithKey'
    validateAtomPatch
    (Right ())
    (patchEvents patch)
  where
    validateAtomPatch eitherUnit atomKey atomPatch = do
      eitherUnit
      atomSchema <-
        case IntMap.lookup atomKey atomSchemas of
          Nothing ->
            Left (RuntimeSpecInitialAtomUndeclared (mkAtomId atomKey))
          Just value ->
            Right value
      validateRows atomKey atomSchema atomPatch

    validateRows ::
      Int ->
      RuntimeAtomSchema ->
      AtomPatch ->
      Either (RuntimeSpecError ctx prop) ()
    validateRows atomKey atomSchema atomPatch =
      Map.foldlWithKey'
        ( \eitherUnit rowValue _multiplicity -> do
            eitherUnit
            let expected =
                  length (rasColumns atomSchema)
                actual =
                  tupleKeyWidth rowValue
            unless (actual == expected) $
              Left
                ( RuntimeSpecInitialRowWidthMismatch
                    (mkAtomId atomKey)
                    expected
                    rowValue
                )
        )
        (Right ())
        (plainRowPatchChangeMap (atomPatchRows atomPatch))
{-# INLINE validateInitialData #-}

compileQueryRoutes ::
  forall ctx prop.
  [RuntimePlan ctx prop] ->
  Either (RuntimeSpecError ctx prop) (IntMap (ctx, PropositionKey prop))
compileQueryRoutes plans =
  Foldable.foldlM insertPlan IntMap.empty plans
  where
    insertPlan ::
      IntMap (ctx, PropositionKey prop) ->
      RuntimePlan ctx prop ->
      Either (RuntimeSpecError ctx prop) (IntMap (ctx, PropositionKey prop))
    insertPlan routes plan =
      let queryId =
            runtimePlanQueryId plan
          key =
            queryIdKey queryId
          value =
            (rpContext plan, rpProp plan)
       in case IntMap.lookup key routes of
            Nothing ->
              Right (IntMap.insert key value routes)
            Just _old ->
              Left (RuntimeSpecDuplicateQuery queryId)
{-# INLINE compileQueryRoutes #-}

firstRuntimeProp ::
  RuntimeSchema ctx prop ->
  [RuntimePlan ctx prop] ->
  Either (RuntimeSpecError ctx prop) (PropositionKey prop)
firstRuntimeProp schema plans =
  case fmap rpProp plans <> schemaProps of
    propKey : _ ->
      Right propKey
    [] ->
      Left RuntimeSpecEmptyPropositions
  where
    schemaProps =
      concatMap
        (Set.toAscList . rcsPropositions)
        (Map.elems (rscContexts schema))
{-# INLINE firstRuntimeProp #-}

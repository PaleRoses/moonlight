{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.EGraph.Pure.Session.Interpret
  ( EGraphScriptError (..),
    PhaseWitness (..),
    runEGraphScript,
  )
where

import Data.Kind (Type)
import Data.IntMap.Strict qualified as IntMap
import Data.Set (Set)
import Moonlight.Core
  ( ClassId,
    Language,
    UnionFindAllocationError,
    classIdKey,
  )
import Moonlight.EGraph.Pure.Analysis
  ( asMake,
  )
import Moonlight.EGraph.Pure.Change
  ( EGraphMutationResult (..),
    EGraphRebuildTrace,
    GraphPhase (..),
    ObservedClassUnions,
    appendEGraphMutationTrace,
    emptyEGraphMutationTrace,
    eGraphPhase,
    observedClassUnionsFromEditDelta,
  )
import Moonlight.EGraph.Pure.Extraction qualified as Extraction
import Moonlight.EGraph.Pure.Kernel.HashCons
  ( insertENodeTracked,
    insertTermTracked,
  )
import Moonlight.EGraph.Pure.Rebuild
  ( equateClassesTracked,
    rebuildTracked,
  )
import Moonlight.EGraph.Pure.Session.Internal
  ( EGraphScript (..),
    StableEGraphQuery (..),
    classRef,
    classRefClassId,
  )
import Moonlight.EGraph.Pure.Types
  ( EGraph,
    ENode (..),
    canonicalizeClassId,
    eGraphAnalysis,
    eGraphAnalysisSpec,
    eGraphClassNodes,
    eGraphPendingDelta,
  )

type EGraphScriptError :: (Type -> Type) -> Type -> Type
data EGraphScriptError f analysis
  = EGraphScriptExpectedStableGraph !ObservedClassUnions
  | EGraphScriptMissingEClass !ClassId
  | EGraphScriptClassIdAllocationFailed !UnionFindAllocationError
  deriving stock (Eq, Show)

type PhaseWitness :: GraphPhase -> Type
data PhaseWitness phase where
  StableWitness :: PhaseWitness 'Stable
  DirtyWitness :: PhaseWitness 'Dirty

runEGraphScript ::
  Language f =>
  PhaseWitness from ->
  EGraphScript f analysis from to result ->
  EGraph f analysis ->
  Either
    (EGraphScriptError f analysis)
    (EGraphMutationResult f analysis result)
runEGraphScript phaseWitness script graph =
  requireWitness phaseWitness graph
    *> interpretEGraphScript script graph

requireWitness ::
  PhaseWitness phase ->
  EGraph f analysis ->
  Either (EGraphScriptError f analysis) ()
requireWitness phaseWitness graph =
  case phaseWitness of
    StableWitness ->
      requireStableGraph graph
    DirtyWitness ->
      Right ()

interpretEGraphScript ::
  Language f =>
  EGraphScript f analysis from to result ->
  EGraph f analysis ->
  Either (EGraphScriptError f analysis) (EGraphMutationResult f analysis result)
interpretEGraphScript script graph =
  case script of
    ScriptPure resultValue ->
      Right (unchangedResult resultValue graph)

    ScriptBind firstScript continue -> do
      firstResult <-
        interpretEGraphScript firstScript graph
      secondResult <-
        interpretEGraphScript (continue (emrResult firstResult)) (emrGraph firstResult)
      Right
        secondResult
          { emrTrace = appendEGraphMutationTrace (emrTrace firstResult) (emrTrace secondResult)
          }

    InsertTerm term ->
      case insertTermTracked term graph of
        Left allocationError ->
          Left (EGraphScriptClassIdAllocationFailed allocationError)
        Right mutationResult ->
          Right (classRef <$> mutationResult)

    InsertENode node -> do
      let canonicalNode =
            fmap (canonicalizeClassId graph . classRefClassId) node
      childAnalyses <- traverse (resolveClassAnalysis graph) canonicalNode
      let nodeAnalysis =
            asMake (eGraphAnalysisSpec graph) childAnalyses
      case insertENodeTracked (ENode canonicalNode) nodeAnalysis graph of
        Left allocationError ->
          Left (EGraphScriptClassIdAllocationFailed allocationError)
        Right mutationResult ->
          Right (classRef <$> mutationResult)

    MergeClasses leftRef rightRef -> do
      leftClass <- resolveCanonicalClassId graph (classRefClassId leftRef)
      rightClass <- resolveCanonicalClassId graph (classRefClassId rightRef)
      Right (classRef <$> equateClassesTracked leftClass rightClass graph)

    RebuildGraph ->
      Right (rebuildGraphSession graph)

    StableQuery queryValue ->
      requireStableGraph graph
        *> fmap
          (`unchangedResult` graph)
          (interpretStableEGraphQuery queryValue graph)

requireStableGraph ::
  EGraph f analysis ->
  Either (EGraphScriptError f analysis) ()
requireStableGraph graph =
  if eGraphPhase graph == Stable
    then Right ()
    else Left (EGraphScriptExpectedStableGraph (observedClassUnionsFromEditDelta (eGraphPendingDelta graph)))

rebuildGraphSession ::
  Language f =>
  EGraph f analysis ->
  EGraphMutationResult f analysis (Maybe (EGraphRebuildTrace f))
rebuildGraphSession graph =
  if eGraphPhase graph == Stable
    then unchangedResult Nothing graph
    else
      Just <$> rebuildTracked Nothing graph

interpretStableEGraphQuery ::
  Language f =>
  StableEGraphQuery f analysis result ->
  EGraph f analysis ->
  Either (EGraphScriptError f analysis) result
interpretStableEGraphQuery queryValue graph =
  case queryValue of
    QueryCanonicalClass classId -> do
      canonicalClassId <- resolveCanonicalClassId graph (classRefClassId classId)
      Right (classRef canonicalClassId)

    QueryClassAnalysis classId -> do
      resolveClassAnalysis graph (classRefClassId classId)

    QueryClassNodes classId -> do
      resolveClassNodes graph (classRefClassId classId)

    QueryExtractClass costAlgebra classId -> do
      canonicalClassId <- resolveCanonicalClassId graph (classRefClassId classId)
      Right (Extraction.stableExtractionSnapshotFromEGraph graph >>= Extraction.extractWithAnalysis costAlgebra canonicalClassId)

unchangedResult ::
  result ->
  EGraph f analysis ->
  EGraphMutationResult f analysis result
unchangedResult resultValue graph =
  EGraphMutationResult
    { emrResult = resultValue,
      emrTrace = emptyEGraphMutationTrace graph,
      emrGraph = graph
    }

resolveCanonicalClassId ::
  EGraph f analysis ->
  ClassId ->
  Either (EGraphScriptError f analysis) ClassId
resolveCanonicalClassId graph classId =
  fst <$> resolveCanonicalClassAnalysis graph classId

resolveClassAnalysis ::
  EGraph f analysis ->
  ClassId ->
  Either (EGraphScriptError f analysis) analysis
resolveClassAnalysis graph classId =
  snd <$> resolveCanonicalClassAnalysis graph classId

resolveCanonicalClassAnalysis ::
  EGraph f analysis ->
  ClassId ->
  Either (EGraphScriptError f analysis) (ClassId, analysis)
resolveCanonicalClassAnalysis graph classId =
  maybe
    (Left (EGraphScriptMissingEClass classId))
    (\analysisValue -> Right (canonicalClassId, analysisValue))
    (IntMap.lookup (classIdKey canonicalClassId) (eGraphAnalysis graph))
  where
    canonicalClassId =
      canonicalizeClassId graph classId

resolveClassNodes ::
  Language f =>
  EGraph f analysis ->
  ClassId ->
  Either (EGraphScriptError f analysis) (Set (ENode f))
resolveClassNodes graph classId = do
  canonicalClassId <- resolveCanonicalClassId graph classId
  Right (eGraphClassNodes graph canonicalClassId)

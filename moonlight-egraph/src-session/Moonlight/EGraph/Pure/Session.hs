{-# LANGUAGE DataKinds #-}

module Moonlight.EGraph.Pure.Session
  ( EGraphMutationResult (..),
    EGraphMutationTrace (..),
    EGraphRebuildTrace (..),
    ObservedClassUnions,
    observedClassUnionKeys,
    observedClassUnionPairs,
    observedClassUnions,
    observedClassUnionsFromEditDelta,
    observedClassUnionsNull,
    appendEGraphMutationTrace,
    eGraphMutationTraceEffect,
    emptyEGraphMutationTrace,
    EGraphScriptError (..),
    GraphPhase (..),
    PhaseWitness (..),
    runEGraphScript,
    ClassRef (..),
    classRef,
    classRefClassId,
    EGraphScript,
    StableEGraphQuery,
    scriptPure,
    (>>>=),
    (>>>),
    insertTerm,
    insertENode,
    mergeClassRefs,
    rebuildGraph,
    canonicalClass,
    classAnalysis,
    classNodes,
    extractClass,
  )
where

import Data.Set (Set)
import Moonlight.EGraph.Pure.Change
  ( EGraphMutationResult (..),
    EGraphMutationTrace (..),
    EGraphRebuildTrace (..),
    ObservedClassUnions,
    appendEGraphMutationTrace,
    eGraphMutationTraceEffect,
    emptyEGraphMutationTrace,
    GraphPhase (..),
    observedClassUnionKeys,
    observedClassUnionPairs,
    observedClassUnions,
    observedClassUnionsFromEditDelta,
    observedClassUnionsNull,
  )
import Moonlight.EGraph.Pure.Extraction
  ( AnalysisCostAlgebra,
    ExtractionResult,
  )
import Moonlight.EGraph.Pure.Session.Interpret
  ( EGraphScriptError (..),
    PhaseWitness (..),
    runEGraphScript,
  )
import Moonlight.EGraph.Pure.Session.Internal
  ( ClassRef (..),
    EGraphScript,
    StableEGraphQuery,
    classRef,
    classRefClassId,
  )
import Moonlight.EGraph.Pure.Session.Internal qualified as Internal
import Moonlight.EGraph.Pure.Types
  ( ENode,
  )
import Data.Fix
  ( Fix,
  )

scriptPure :: result -> EGraphScript f analysis phase phase result
scriptPure =
  Internal.ScriptPure
{-# INLINE scriptPure #-}

(>>>=) ::
  EGraphScript f analysis from mid intermediate ->
  (intermediate -> EGraphScript f analysis mid to result) ->
  EGraphScript f analysis from to result
(>>>=) =
  Internal.ScriptBind
{-# INLINE (>>>=) #-}

infixl 1 >>>=

(>>>) ::
  EGraphScript f analysis from mid ignored ->
  EGraphScript f analysis mid to result ->
  EGraphScript f analysis from to result
leftScript >>> rightScript =
  leftScript >>>= const rightScript
{-# INLINE (>>>) #-}

infixl 1 >>>

insertTerm :: Fix f -> EGraphScript f analysis phase phase ClassRef
insertTerm =
  Internal.InsertTerm
{-# INLINE insertTerm #-}

insertENode :: f ClassRef -> EGraphScript f analysis phase phase ClassRef
insertENode =
  Internal.InsertENode
{-# INLINE insertENode #-}

mergeClassRefs :: ClassRef -> ClassRef -> EGraphScript f analysis phase 'Dirty ClassRef
mergeClassRefs =
  Internal.MergeClasses
{-# INLINE mergeClassRefs #-}

rebuildGraph :: EGraphScript f analysis phase 'Stable (Maybe (EGraphRebuildTrace f))
rebuildGraph =
  Internal.RebuildGraph
{-# INLINE rebuildGraph #-}

canonicalClass :: ClassRef -> EGraphScript f analysis 'Stable 'Stable ClassRef
canonicalClass =
  Internal.StableQuery . Internal.QueryCanonicalClass
{-# INLINE canonicalClass #-}

classAnalysis :: ClassRef -> EGraphScript f analysis 'Stable 'Stable analysis
classAnalysis =
  Internal.StableQuery . Internal.QueryClassAnalysis
{-# INLINE classAnalysis #-}

classNodes :: ClassRef -> EGraphScript f analysis 'Stable 'Stable (Set (ENode f))
classNodes =
  Internal.StableQuery . Internal.QueryClassNodes
{-# INLINE classNodes #-}

extractClass ::
  Ord cost =>
  AnalysisCostAlgebra f analysis cost ->
  ClassRef ->
  EGraphScript f analysis 'Stable 'Stable (Maybe (ExtractionResult f cost))
extractClass costAlgebra =
  Internal.StableQuery . Internal.QueryExtractClass costAlgebra
{-# INLINE extractClass #-}

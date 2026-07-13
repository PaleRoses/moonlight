{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.EGraph.Pure.Session.Internal
  ( ClassRef (..),
    classRef,
    classRefClassId,
    EGraphScript (..),
    StableEGraphQuery (..),
  )
where

import Data.Kind (Type)
import Data.Set (Set)
import Moonlight.Core
  ( ClassId,
  )
import Moonlight.EGraph.Pure.Change
  ( EGraphRebuildTrace,
    GraphPhase (..),
  )
import Moonlight.EGraph.Pure.Extraction
  ( AnalysisCostAlgebra,
    ExtractionResult,
  )
import Moonlight.EGraph.Pure.Types
  ( ENode,
  )
import Data.Fix
  ( Fix,
  )

type ClassRef :: Type
newtype ClassRef = ClassRef
  { unClassRef :: ClassId
  }
  deriving stock (Eq, Ord, Show)

classRef :: ClassId -> ClassRef
classRef =
  ClassRef
{-# INLINE classRef #-}

classRefClassId :: ClassRef -> ClassId
classRefClassId =
  unClassRef
{-# INLINE classRefClassId #-}

type EGraphScript :: (Type -> Type) -> Type -> GraphPhase -> GraphPhase -> Type -> Type
data EGraphScript f analysis from to result where
  ScriptPure ::
    result ->
    EGraphScript f analysis phase phase result
  ScriptBind ::
    EGraphScript f analysis from mid intermediate ->
    (intermediate -> EGraphScript f analysis mid to result) ->
    EGraphScript f analysis from to result
  InsertTerm ::
    Fix f ->
    EGraphScript f analysis phase phase ClassRef
  InsertENode ::
    f ClassRef ->
    EGraphScript f analysis phase phase ClassRef
  MergeClasses ::
    ClassRef ->
    ClassRef ->
    EGraphScript f analysis phase 'Dirty ClassRef
  RebuildGraph ::
    EGraphScript f analysis phase 'Stable (Maybe (EGraphRebuildTrace f))
  StableQuery ::
    StableEGraphQuery f analysis result ->
    EGraphScript f analysis 'Stable 'Stable result

type StableEGraphQuery :: (Type -> Type) -> Type -> Type -> Type
data StableEGraphQuery f analysis result where
  QueryCanonicalClass ::
    ClassRef ->
    StableEGraphQuery f analysis ClassRef
  QueryClassAnalysis ::
    ClassRef ->
    StableEGraphQuery f analysis analysis
  QueryClassNodes ::
    ClassRef ->
    StableEGraphQuery f analysis (Set (ENode f))
  QueryExtractClass ::
    Ord cost =>
    AnalysisCostAlgebra f analysis cost ->
    ClassRef ->
    StableEGraphQuery f analysis (Maybe (ExtractionResult f cost))

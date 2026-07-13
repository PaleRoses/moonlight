{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RankNTypes #-}

module Moonlight.Core.EGraph.Program
  ( EGraphProgramOp (..),
    EGraphProgram,
    EGraphProgramEffect,
    emptyEGraphProgramEffect,
    insertedFreshNodeEffect,
    requiredClassMergeEffect,
    eGraphProgramEffectCount,
    eGraphProgramChanged,
    insertedFreshNode,
    eGraphProgramRequiredClassMerge,
    foldEGraphProgram,
    abortProgram,
    canonicalizeClass,
    canonicalizeClasses,
    addNode,
    addCanonicalNode,
    mergeClasses,
    mergeCanonicalClasses,
  )
where

import Control.Monad.Free (Free (..), foldFree)
import Data.Kind (Type)
import Data.Monoid (Any (..), Sum (..))
import GHC.Generics (Generic, Generically (..))
import Moonlight.Core.Identifier.EGraph (ClassId)
import Prelude
  ( Bool (..),
    Eq,
    Functor,
    Int,
    Monad,
    Monoid,
    Ord,
    Semigroup,
    Show,
    Traversable,
    mempty,
    traverse,
    (>>=),
    (>),
    (.),
  )

-- | Host-neutral e-graph program instruction.
--
-- This is the tiny algebra every equality-saturation backend must implement:
-- canonicalize class identifiers, insert e-nodes, stage class merges for host
-- rebuild/repair, or abort with a typed obstruction. Rewrite compilers emit
-- this language; e-graph hosts interpret it.
type EGraphProgramOp :: Type -> Type -> Type -> Type
data EGraphProgramOp programError node next
  = CanonicalizeClass !ClassId (ClassId -> next)
  | AddNode !node (ClassId -> next)
  | MergeClasses !ClassId !ClassId (ClassId -> next)
  | AbortProgram !programError
  deriving stock (Functor)

type EGraphProgram :: Type -> Type -> Type -> Type
type EGraphProgram programError node resultValue =
  Free (EGraphProgramOp programError node) resultValue

type EGraphProgramEffect :: Type
data EGraphProgramEffect = EGraphProgramEffect
  { egpeEffectiveApplications :: !(Sum Int),
    insertedFreshNode :: !Any,
    egpeRequiredClassMerge :: !Any
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Semigroup, Monoid) via (Generically EGraphProgramEffect)

emptyEGraphProgramEffect :: EGraphProgramEffect
emptyEGraphProgramEffect =
  mempty

insertedFreshNodeEffect :: EGraphProgramEffect
insertedFreshNodeEffect =
  EGraphProgramEffect
    { egpeEffectiveApplications = Sum 1,
      insertedFreshNode = Any True,
      egpeRequiredClassMerge = mempty
    }

requiredClassMergeEffect :: EGraphProgramEffect
requiredClassMergeEffect =
  EGraphProgramEffect
    { egpeEffectiveApplications = Sum 1,
      insertedFreshNode = mempty,
      egpeRequiredClassMerge = Any True
    }

eGraphProgramEffectCount :: EGraphProgramEffect -> Int
eGraphProgramEffectCount EGraphProgramEffect {egpeEffectiveApplications = count} =
  getSum count

eGraphProgramChanged :: EGraphProgramEffect -> Bool
eGraphProgramChanged =
  (> 0) . eGraphProgramEffectCount

insertedFreshNode :: EGraphProgramEffect -> Bool
insertedFreshNode EGraphProgramEffect {insertedFreshNode = inserted} =
  getAny inserted

eGraphProgramRequiredClassMerge :: EGraphProgramEffect -> Bool
eGraphProgramRequiredClassMerge EGraphProgramEffect {egpeRequiredClassMerge = required} =
  getAny required

foldEGraphProgram ::
  Monad m =>
  (forall next. EGraphProgramOp programError node next -> m next) ->
  EGraphProgram programError node resultValue ->
  m resultValue
foldEGraphProgram =
  foldFree

abortProgram ::
  programError ->
  EGraphProgram programError node resultValue
abortProgram =
  Free . AbortProgram

canonicalizeClass ::
  ClassId ->
  EGraphProgram programError node ClassId
canonicalizeClass classId =
  Free (CanonicalizeClass classId Pure)

canonicalizeClasses ::
  Traversable t =>
  t ClassId ->
  EGraphProgram programError node (t ClassId)
canonicalizeClasses =
  traverse canonicalizeClass

addNode ::
  node ->
  EGraphProgram programError node ClassId
addNode node =
  Free (AddNode node Pure)

addCanonicalNode ::
  Traversable f =>
  f ClassId ->
  EGraphProgram programError (f ClassId) ClassId
addCanonicalNode node = do
  canonicalNode <- canonicalizeClasses node
  addNode canonicalNode >>= canonicalizeClass

mergeClasses ::
  ClassId ->
  ClassId ->
  EGraphProgram programError node ClassId
mergeClasses leftClassId rightClassId =
  Free (MergeClasses leftClassId rightClassId Pure)

mergeCanonicalClasses ::
  ClassId ->
  ClassId ->
  EGraphProgram programError node ClassId
mergeCanonicalClasses leftClassId rightClassId = do
  canonicalLeftClassId <- canonicalizeClass leftClassId
  canonicalRightClassId <- canonicalizeClass rightClassId
  mergeClasses canonicalLeftClassId canonicalRightClassId

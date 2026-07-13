{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.EGraph.Test.Context.SimpleArith
  ( ArithF (..),
    ArithTag (..),
    Depth (..),
    depthSpec,
    lit,
    plus,
    baseFixture,
    extendFixture,
  )
where

import Moonlight.Core (ZipMatch (..))
import Data.Bifunctor (first)
import Data.Kind (Type)
import Data.Set qualified as Set
import Moonlight.Algebra (JoinSemilattice (join))
import Moonlight.FiniteLattice
  ( ContextLattice (..)
  )

import Moonlight.Core (HasConstructorTag (..), ConstructorTag, zipSameNodeShape)
import Moonlight.EGraph.Pure.Analysis (AnalysisSpec, semilatticeAnalysis)
import Moonlight.EGraph.Pure.Context (ContextDeltaError (..), ContextEGraph, contextCachedObjectsForExecution, emptyContextEGraph, rebaseContextGraphAtContexts)
import Moonlight.EGraph.Pure.Context.Core (cegBase)
import Moonlight.EGraph.Pure.Kernel.HashCons (addTerm)
import Moonlight.EGraph.Pure.Types (ClassId (..), EGraph, emptyEGraph)
import Data.Fix (Fix (Fix))
type ArithF :: Type -> Type
data ArithF a
  = Lit Int
  | Plus a a
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

type ArithTag :: Type
data ArithTag
  = LitTag Int
  | PlusTag
  deriving stock (Eq, Ord, Show)

instance HasConstructorTag ArithF where
  type ConstructorTag ArithF = ArithTag

  constructorTag arithNode =
    case arithNode of
      Lit value -> LitTag value
      Plus _ _ -> PlusTag

instance ZipMatch ArithF where
  zipMatch = zipSameNodeShape

type Depth :: Type
newtype Depth = Depth Int
  deriving stock (Eq, Ord, Show)

instance JoinSemilattice Depth where
  join (Depth leftDepth) (Depth rightDepth) =
    Depth (max leftDepth rightDepth)

depthSpec :: AnalysisSpec ArithF Depth
depthSpec =
  semilatticeAnalysis $ \case
    Lit _ -> Depth 0
    Plus (Depth leftDepth) (Depth rightDepth) ->
      Depth (max leftDepth rightDepth + 1)

lit :: Int -> Fix ArithF
lit value =
  Fix (Lit value)

plus :: Fix ArithF -> Fix ArithF -> Fix ArithF
plus leftTerm rightTerm =
  Fix (Plus leftTerm rightTerm)

baseFixture :: Ord c => ContextLattice c -> Either (ContextDeltaError ArithF c) (ClassId, ClassId, ContextEGraph ArithF Depth c)
baseFixture contextLatticeValue = do
  let graph0 = emptyEGraph depthSpec
  (classA, graph1) <- first ContextClassIdAllocationFailed (addTerm (lit 1) graph0)
  (classB, graph2) <- first ContextClassIdAllocationFailed (addTerm (plus (lit 2) (lit 3)) graph1)
  pure (classA, classB, emptyContextEGraph contextLatticeValue graph2)

diagnosticFullContextRebase :: Ord c => ContextEGraph ArithF Depth c -> EGraph ArithF Depth -> Either (ContextDeltaError ArithF c) (ContextEGraph ArithF Depth c)
diagnosticFullContextRebase contextGraph baseGraph =
  rebaseContextGraphAtContexts
    (Set.fromList (contextCachedObjectsForExecution contextGraph))
    baseGraph
    contextGraph

extendFixture :: Ord c => ContextEGraph ArithF Depth c -> Either (ContextDeltaError ArithF c) (ClassId, ClassId, ContextEGraph ArithF Depth c)
extendFixture contextGraph = do
  let baseGraph = cegBase contextGraph
  (classC, graph1) <- first ContextClassIdAllocationFailed (addTerm (lit 7) baseGraph)
  (classD, graph2) <- first ContextClassIdAllocationFailed (addTerm (plus (lit 8) (lit 9)) graph1)
  rebasedGraph <- diagnosticFullContextRebase contextGraph graph2
  pure (classC, classD, rebasedGraph)

{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Sheaf.Site.Context.Presentation
  ( ContextPresentation (..),
    ContextPresentationSystem (..),
    contextPresentation,
    contextPresentationWith,
    contextPresentationPairs,
    contextPresentationReflexivePairs,
  )
where

import Data.Kind (Constraint, Type)
import Moonlight.Sheaf.Site.Context.Pairs
  ( ContextPairStrategy (..),
    downwardPairsByStrategy,
    reflexiveDownwardPairsByStrategy,
  )
import Moonlight.Sheaf.Site.System
  ( AnalyzableSystem (..),
    LatticeAnalyzableSystem,
    SystemCtx,
  )

type ContextPresentation :: Type -> Type
data ContextPresentation system = ContextPresentation
  { cpSystem :: !system,
    cpContexts :: ![SystemCtx system],
    cpPairStrategy :: !(ContextPairStrategy (SystemCtx system))
  }

contextPresentationWith ::
  system ->
  [SystemCtx system] ->
  ContextPairStrategy (SystemCtx system) ->
  ContextPresentation system
contextPresentationWith systemValue contextValues pairStrategy =
  ContextPresentation
    { cpSystem = systemValue,
      cpContexts = contextValues,
      cpPairStrategy = pairStrategy
    }

contextPresentation ::
  AnalyzableSystem system =>
  system ->
  ContextPresentation system
contextPresentation systemValue =
  contextPresentationWith
    systemValue
    (allContexts systemValue)
    ExhaustivePairs

contextPresentationPairs ::
  LatticeAnalyzableSystem system =>
  ContextPresentation system ->
  [(SystemCtx system, SystemCtx system)]
contextPresentationPairs presentationValue =
  downwardPairsByStrategy
    (cpPairStrategy presentationValue)
    (cpSystem presentationValue)
    (cpContexts presentationValue)

contextPresentationReflexivePairs ::
  LatticeAnalyzableSystem system =>
  ContextPresentation system ->
  [(SystemCtx system, SystemCtx system)]
contextPresentationReflexivePairs presentationValue =
  reflexiveDownwardPairsByStrategy
    (cpPairStrategy presentationValue)
    (cpSystem presentationValue)
    (cpContexts presentationValue)

type ContextPresentationSystem :: Type -> Constraint
class AnalyzableSystem system => ContextPresentationSystem system where
  systemContextPresentation ::
    system ->
    ContextPresentation system

  systemContextPresentation =
    contextPresentation

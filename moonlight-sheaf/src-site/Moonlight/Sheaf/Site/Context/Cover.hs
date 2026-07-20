{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Sheaf.Site.Context.Cover
  ( ContextArrow (..),
    ContextCover,
    ContextCoverError (..),
    mkContextArrow,
    contextCoverFromSources,
  )
where

import Data.Bifunctor (first)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism (..),
    CoverConstructionError,
    CoveringFamily,
    mkCoveringFamily,
  )
import Moonlight.Sheaf.Site.System
  ( AnalyzableSystem (..),
    SystemCtx,
  )

type ContextArrow :: Type -> Type
data ContextArrow ctx = ContextArrow
  deriving stock (Eq, Ord, Show)

type ContextCover :: Type -> Type
type ContextCover system =
  CoveringFamily
    (SystemCtx system)
    (ContextArrow (SystemCtx system))

type ContextCoverError :: Type -> Type
data ContextCoverError ctx
  = ContextSourceDoesNotRefineTarget !ctx !ctx
  | ContextCoverMalformed !(CoverConstructionError ctx)
  deriving stock (Eq, Ord, Show)

mkContextArrow ::
  AnalyzableSystem system =>
  system ->
  SystemCtx system ->
  SystemCtx system ->
  Either
    (ContextCoverError (SystemCtx system))
    (CheckedMorphism (SystemCtx system) (ContextArrow (SystemCtx system)))
mkContextArrow systemValue sourceContext targetContext =
  if contextLeq systemValue sourceContext targetContext
    then
      Right
        CheckedMorphism
          { cmSource = sourceContext,
            cmTarget = targetContext,
            cmWitness = ContextArrow
          }
    else
      Left
        (ContextSourceDoesNotRefineTarget sourceContext targetContext)

contextCoverFromSources ::
  AnalyzableSystem system =>
  system ->
  SystemCtx system ->
  NonEmpty (SystemCtx system) ->
  Either
    (ContextCoverError (SystemCtx system))
    (ContextCover system)
contextCoverFromSources systemValue targetContext sourceContexts = do
  coverArrows <-
    traverse
      (\sourceContext -> mkContextArrow systemValue sourceContext targetContext)
      sourceContexts
  first ContextCoverMalformed
    (mkCoveringFamily targetContext coverArrows)

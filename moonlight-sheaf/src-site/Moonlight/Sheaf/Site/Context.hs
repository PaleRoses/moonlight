{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Sheaf.Site.Context
  ( ContextCoverBasis (..),
    ContextArrow (..),
    ContextCover,
    ContextCoverError (..),
    contextCoverFromSources,
    allContextMorphisms,
    identityContextArrow,
    composeContextArrows,
    pullbackContextSquare,
  )
where

import Data.Kind (Constraint, Type)
import Moonlight.Algebra (Lattice, meet)
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism (..),
    PullbackSquare (..),
  )
import Moonlight.Sheaf.Site.Context.Cover
  ( ContextArrow (..),
    ContextCover,
    ContextCoverError (..),
    contextCoverFromSources,
    mkContextArrow,
  )
import Moonlight.Sheaf.Site.System
  ( AnalyzableSystem (..),
    SystemCtx,
  )

type ContextCoverBasis :: Type -> Constraint
class
  ( AnalyzableSystem system,
    Lattice (SystemCtx system)
  ) =>
  ContextCoverBasis system
  where
  contextCoversAt ::
    system ->
    SystemCtx system ->
    [ContextCover system]

allContextMorphisms ::
  AnalyzableSystem system =>
  system ->
  [CheckedMorphism (SystemCtx system) (ContextArrow (SystemCtx system))]
allContextMorphisms systemValue =
  let contextValues = allContexts systemValue
   in [ morphismValue
        | sourceContext <- contextValues,
          targetContext <- contextValues,
          Just morphismValue <- [contextArrowMaybe systemValue sourceContext targetContext]
      ]

identityContextArrow ::
  ctx ->
  CheckedMorphism ctx (ContextArrow ctx)
identityContextArrow contextValue =
  CheckedMorphism
    { cmSource = contextValue,
      cmTarget = contextValue,
      cmWitness = ContextArrow
    }

composeContextArrows ::
  Eq ctx =>
  CheckedMorphism ctx (ContextArrow ctx) ->
  CheckedMorphism ctx (ContextArrow ctx) ->
  Maybe (CheckedMorphism ctx (ContextArrow ctx))
composeContextArrows outerArrow innerArrow
  | cmSource outerArrow == cmTarget innerArrow =
      Just
        CheckedMorphism
          { cmSource = cmSource innerArrow,
            cmTarget = cmTarget outerArrow,
            cmWitness = ContextArrow
          }
  | otherwise =
      Nothing

pullbackContextSquare ::
  ( AnalyzableSystem system,
    Lattice (SystemCtx system)
  ) =>
  system ->
  CheckedMorphism (SystemCtx system) (ContextArrow (SystemCtx system)) ->
  CheckedMorphism (SystemCtx system) (ContextArrow (SystemCtx system)) ->
  Maybe
    (PullbackSquare (SystemCtx system) (ContextArrow (SystemCtx system)))
pullbackContextSquare systemValue leftArrow rightArrow
  | cmTarget leftArrow /= cmTarget rightArrow =
      Nothing
  | otherwise =
      let apexContext = meet (cmSource leftArrow) (cmSource rightArrow)
       in do
            leftLeg <- contextArrowMaybe systemValue apexContext (cmSource leftArrow)
            rightLeg <- contextArrowMaybe systemValue apexContext (cmSource rightArrow)
            pure
              PullbackSquare
                { psLeftBase = leftArrow,
                  psRightBase = rightArrow,
                  psApex = apexContext,
                  psToLeft = leftLeg,
                  psToRight = rightLeg
                }

contextArrowMaybe ::
  AnalyzableSystem system =>
  system ->
  SystemCtx system ->
  SystemCtx system ->
  Maybe (CheckedMorphism (SystemCtx system) (ContextArrow (SystemCtx system)))
contextArrowMaybe systemValue sourceContext targetContext =
  case mkContextArrow systemValue sourceContext targetContext of
    Left _ ->
      Nothing
    Right morphismValue ->
      Just morphismValue

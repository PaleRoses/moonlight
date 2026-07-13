{-# LANGUAGE TypeFamilies #-}

-- | Composition of decorated cospans: the 'StructuredCompositionAlgebra', composition
-- results carrying obligations, and obligation reconciliation.
module Moonlight.Category.Pure.DecoratedComposition
  ( CompositionResult (..),
    DecoratedCompositionError (..),
    StructuredCompositionAlgebra (..),
    composeDecorated,
    composeDecoratedStructured,
    composeStructuredDecoratedCospan,
    reconcileCompositionObligations,
  )
where

import Data.Bifunctor (first)
import Data.Kind (Type)
import Data.List (genericDrop)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Numeric.Natural (Natural)
import Moonlight.Category.Pure.Category (Ob)
import Moonlight.Category.Pure.Limits (HasPushouts)
import Moonlight.Category.Pure.StructuredCospan (StructuredCospan, StructuredCospanError, composeStructuredCospan, structuredDecoration)

type CompositionResult :: Type -> Type -> Type -> Type
data CompositionResult ir obligation decoration = CompositionResult
  { composedIR :: ir,
    composedObligations :: [obligation],
    composedDecoration :: decoration
  }
  deriving stock (Eq, Show)

type DecoratedCompositionError :: Type -> Type
data DecoratedCompositionError category
  = DecoratedCompositionLeftBoundaryMissing
  | DecoratedCompositionRightBoundaryMissing
  | DecoratedCompositionStructuredError (StructuredCospanError category)

type StructuredCompositionAlgebra ::
  Type -> Type -> Type -> Type -> Type -> Type
data StructuredCompositionAlgebra boundary category ir decoration obligation = StructuredCompositionAlgebra
  { toStructuredBoundary ::
      boundary ->
      (ir, decoration) ->
      Maybe (StructuredCospan category decoration),
    fromStructuredComposition ::
      boundary ->
      (ir, decoration) ->
      (ir, decoration) ->
      StructuredCospan category decoration ->
      (ir, [obligation])
  }

composeDecorated ::
  (decoration -> decoration -> decoration) ->
  (boundary -> (ir, decoration) -> (ir, decoration) -> (ir, [obligation])) ->
  boundary ->
  (ir, decoration) ->
  (ir, decoration) ->
  CompositionResult ir obligation decoration
composeDecorated combineDecorations glue boundaryValue leftValue rightValue =
  let (composedValue, obligations) = glue boundaryValue leftValue rightValue
   in CompositionResult
        { composedIR = composedValue,
          composedObligations = obligations,
          composedDecoration = combineDecorations (snd leftValue) (snd rightValue)
        }
{-# INLINE composeDecorated #-}

composeDecoratedStructured ::
  (HasPushouts category, Eq (Ob category)) =>
  category ->
  StructuredCompositionAlgebra boundary category ir decoration obligation ->
  (decoration -> decoration -> decoration) ->
  boundary ->
  (ir, decoration) ->
  (ir, decoration) ->
  Either (DecoratedCompositionError category) (CompositionResult ir obligation decoration)
composeDecoratedStructured categoryValue compositionAlgebra combineDecorations boundaryValue leftValue rightValue = do
  leftBoundaryValue <-
    maybe
      (Left DecoratedCompositionLeftBoundaryMissing)
      Right
      (toStructuredBoundary compositionAlgebra boundaryValue leftValue)
  rightBoundaryValue <-
    maybe
      (Left DecoratedCompositionRightBoundaryMissing)
      Right
      (toStructuredBoundary compositionAlgebra boundaryValue rightValue)
  composedBoundary <-
    first
      DecoratedCompositionStructuredError
      ( composeStructuredDecoratedCospan
          categoryValue
          combineDecorations
          leftBoundaryValue
          rightBoundaryValue
      )
  let (composedValue, obligations) =
        fromStructuredComposition compositionAlgebra boundaryValue leftValue rightValue composedBoundary
   in Right
        CompositionResult
          { composedIR = composedValue,
            composedObligations = obligations,
            composedDecoration = structuredDecoration composedBoundary
          }
{-# INLINE composeDecoratedStructured #-}

composeStructuredDecoratedCospan ::
  (HasPushouts category, Eq (Ob category)) =>
  category ->
  (leftDecoration -> rightDecoration -> combinedDecoration) ->
  StructuredCospan category leftDecoration ->
  StructuredCospan category rightDecoration ->
  Either (StructuredCospanError category) (StructuredCospan category combinedDecoration)
composeStructuredDecoratedCospan =
  composeStructuredCospan
{-# INLINE composeStructuredDecoratedCospan #-}

reconcileCompositionObligations ::
  [obligation] ->
  Natural ->
  Either (NonEmpty obligation) ()
reconcileCompositionObligations obligations budget =
  case NonEmpty.nonEmpty obligations of
    Nothing -> Right ()
    Just nonEmptyObligations ->
      if null (genericDrop budget obligations)
        then Right ()
        else Left nonEmptyObligations

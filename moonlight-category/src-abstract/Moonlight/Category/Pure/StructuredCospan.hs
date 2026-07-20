{-# LANGUAGE TypeFamilies #-}

-- | Structured cospans: two legs into a shared apex carrying a decoration, with
-- boundary projections and pushout composition.
module Moonlight.Category.Pure.StructuredCospan
  ( StructuredCospan,
    structuredLeftLeg,
    structuredRightLeg,
    structuredApex,
    structuredDecoration,
    mkStructuredCospan,
    leftBoundary,
    rightBoundary,
    composeStructuredCospan,
    StructuredCospanError (..),
  )
where

import Data.Bifunctor (first)
import Data.Kind (Type)
import Moonlight.Category.Pure.Category (Category (..), composeMor)
import Moonlight.Category.Pure.Limits (HasPushouts (..))

type StructuredCospan :: Type -> Type -> Type
data StructuredCospan category decoration = StructuredCospan
  { structuredLeftBoundary :: Ob category,
    structuredRightBoundary :: Ob category,
    structuredLeftLeg :: Mor category,
    structuredRightLeg :: Mor category,
    structuredApex :: Ob category,
    structuredDecoration :: decoration
  }

type StructuredCospanError :: Type -> Type
data StructuredCospanError category
  = StructuredCospanCategoryError (CategoryError category)
  | StructuredCospanBoundaryMismatch (Ob category) (Ob category)
  | StructuredCospanPushoutMissing (Mor category) (Mor category)

mkStructuredCospan :: (Category category, Eq (Ob category)) => category -> Mor category -> Mor category -> decoration -> Either (StructuredCospanError category) (StructuredCospan category decoration)
mkStructuredCospan categoryValue leftLeg rightLeg decoration = do
  leftSource <- first StructuredCospanCategoryError (source categoryValue leftLeg)
  rightSource <- first StructuredCospanCategoryError (source categoryValue rightLeg)
  leftTarget <- first StructuredCospanCategoryError (target categoryValue leftLeg)
  rightTarget <- first StructuredCospanCategoryError (target categoryValue rightLeg)
  if leftTarget == rightTarget
    then Right (StructuredCospan leftSource rightSource leftLeg rightLeg leftTarget decoration)
    else Left (StructuredCospanBoundaryMismatch leftTarget rightTarget)
{-# INLINE mkStructuredCospan #-}

leftBoundary :: category -> StructuredCospan category decoration -> Either (CategoryError category) (Ob category)
leftBoundary _ =
  Right . structuredLeftBoundary
{-# INLINE leftBoundary #-}

rightBoundary :: category -> StructuredCospan category decoration -> Either (CategoryError category) (Ob category)
rightBoundary _ =
  Right . structuredRightBoundary
{-# INLINE rightBoundary #-}

composeStructuredCospan ::
  (HasPushouts category, Eq (Ob category)) =>
  category ->
  (leftDecoration -> rightDecoration -> combinedDecoration) ->
  StructuredCospan category leftDecoration ->
  StructuredCospan category rightDecoration ->
  Either (StructuredCospanError category) (StructuredCospan category combinedDecoration)
composeStructuredCospan categoryValue combineDecorations leftCospan rightCospan = do
  if structuredRightBoundary leftCospan == structuredLeftBoundary rightCospan
    then Right ()
    else Left (StructuredCospanBoundaryMismatch (structuredRightBoundary leftCospan) (structuredLeftBoundary rightCospan))
  (pushoutObject, pushoutLeft, pushoutRight) <-
    maybe
      (Left (StructuredCospanPushoutMissing (structuredRightLeg leftCospan) (structuredLeftLeg rightCospan)))
      Right
      (pushout categoryValue (structuredRightLeg leftCospan) (structuredLeftLeg rightCospan))
  composedLeft <- first StructuredCospanCategoryError (composeMor categoryValue pushoutLeft (structuredLeftLeg leftCospan))
  composedRight <- first StructuredCospanCategoryError (composeMor categoryValue pushoutRight (structuredRightLeg rightCospan))
  pure
    ( StructuredCospan
        (structuredLeftBoundary leftCospan)
        (structuredRightBoundary rightCospan)
        composedLeft
        composedRight
        pushoutObject
        (combineDecorations (structuredDecoration leftCospan) (structuredDecoration rightCospan))
    )
{-# INLINE composeStructuredCospan #-}


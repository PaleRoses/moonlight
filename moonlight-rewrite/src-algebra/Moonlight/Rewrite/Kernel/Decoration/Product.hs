{-# LANGUAGE TypeFamilies #-}

module Moonlight.Rewrite.Kernel.Decoration.Product
  ( ProductDecoration (..),
    ProductDecorationObstruction (..),
    leftDecoration,
    rightDecoration,
  )
where

import Data.Bifunctor (first)
import Data.Kind
  ( Type,
  )
import Moonlight.Rewrite.Kernel.Decoration
  ( RewriteDecoration (..),
    mapDecorationError,
  )

type ProductDecoration ::
  ((Type -> Type) -> Type) ->
  ((Type -> Type) -> Type) ->
  (Type -> Type) ->
  Type
data ProductDecoration left right f = ProductDecoration
  { pdLeft :: !(left f),
    pdRight :: !(right f)
  }

deriving stock instance
  (Eq (left f), Eq (right f)) =>
  Eq (ProductDecoration left right f)

deriving stock instance
  (Ord (left f), Ord (right f)) =>
  Ord (ProductDecoration left right f)

deriving stock instance
  (Show (left f), Show (right f)) =>
  Show (ProductDecoration left right f)

type ProductDecorationObstruction :: Type -> Type -> Type
data ProductDecorationObstruction left right
  = ProductLeftDecorationObstruction !left
  | ProductRightDecorationObstruction !right
  deriving stock (Eq, Ord, Show)

leftDecoration ::
  ProductDecoration left right f ->
  left f
leftDecoration =
  pdLeft

rightDecoration ::
  ProductDecoration left right f ->
  right f
rightDecoration =
  pdRight

instance
  (RewriteDecoration left, RewriteDecoration right) =>
  RewriteDecoration (ProductDecoration left right)
  where
  type DecorationConstraint (ProductDecoration left right) f =
    (DecorationConstraint left f, DecorationConstraint right f)
  type DecorationObstruction (ProductDecoration left right) f =
    ProductDecorationObstruction (DecorationObstruction left f) (DecorationObstruction right f)

  emptyDecoration =
    ProductDecoration
      { pdLeft = emptyDecoration,
        pdRight = emptyDecoration
      }

  decorationVariables decoration =
    decorationVariables (pdLeft decoration)
      <> decorationVariables (pdRight decoration)

  renameDecoration renaming decoration =
    ProductDecoration
      { pdLeft = renameDecoration renaming (pdLeft decoration),
        pdRight = renameDecoration renaming (pdRight decoration)
      }

  projectDecoration projection decoration =
    ProductDecoration
      <$> first (mapDecorationError ProductLeftDecorationObstruction) (projectDecoration projection (pdLeft decoration))
      <*> first (mapDecorationError ProductRightDecorationObstruction) (projectDecoration projection (pdRight decoration))

  composeDecoration left right =
    ProductDecoration
      <$> first (mapDecorationError ProductLeftDecorationObstruction) (composeDecoration (pdLeft left) (pdLeft right))
      <*> first (mapDecorationError ProductRightDecorationObstruction) (composeDecoration (pdRight left) (pdRight right))

  validateDecoration boundVariables decoration =
    first (mapDecorationError ProductLeftDecorationObstruction) (validateDecoration boundVariables (pdLeft decoration))
      *> first (mapDecorationError ProductRightDecorationObstruction) (validateDecoration boundVariables (pdRight decoration))

{-# LANGUAGE TypeFamilies #-}

-- | Universal-property classes for (co)limits: products, coproducts, pullbacks,
-- pushouts, equalizers and coequalizers over a 'Category'.
module Moonlight.Category.Pure.Limits
  ( HasProducts (..),
    HasCoproducts (..),
    HasPullbacks (..),
    HasPushouts (..),
    HasEqualizers (..),
    HasCoequalizers (..),
  )
where

import Data.Kind (Constraint, Type)
import Moonlight.Category.Pure.Category (Category (..))

type HasProducts :: Type -> Constraint
class Category c => HasProducts c where
  type ProductOb c :: Type
  productProj1 :: c -> ProductOb c -> Mor c
  productProj2 :: c -> ProductOb c -> Mor c
  productUniversal :: c -> Mor c -> Mor c -> Mor c

type HasCoproducts :: Type -> Constraint
class Category c => HasCoproducts c where
  type CoproductOb c :: Type
  coproductInj1 :: c -> CoproductOb c -> Mor c
  coproductInj2 :: c -> CoproductOb c -> Mor c
  coproductUniversal :: c -> Mor c -> Mor c -> Mor c

type HasPullbacks :: Type -> Constraint
class Category c => HasPullbacks c where
  pullback :: c -> Mor c -> Mor c -> Maybe (Ob c, Mor c, Mor c)
  pullbackMediator :: c -> Mor c -> Mor c -> Mor c -> Mor c -> Maybe (Mor c)

type HasPushouts :: Type -> Constraint
class Category c => HasPushouts c where
  pushout :: c -> Mor c -> Mor c -> Maybe (Ob c, Mor c, Mor c)

type HasEqualizers :: Type -> Constraint
class Category c => HasEqualizers c where
  equalizer :: c -> Mor c -> Mor c -> Maybe (Ob c, Mor c)

type HasCoequalizers :: Type -> Constraint
class Category c => HasCoequalizers c where
  coequalizer :: c -> Mor c -> Mor c -> Maybe (Ob c, Mor c)

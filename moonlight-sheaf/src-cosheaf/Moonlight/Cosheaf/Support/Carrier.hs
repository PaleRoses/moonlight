{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Cosheaf.Support.Carrier
  ( SupportCarrier (..),
    supportCarrierFromList,
    supportCarrierItems,
    supportCarrierCount,
  )
where

import Data.Kind (Type)
import Data.Monoid (Sum (..))
import Data.Set qualified as Set
import Numeric.Natural (Natural)

type SupportCarrier :: Type -> Type
data SupportCarrier item = SupportCarrier
  { scHasAny :: !Bool,
    scContains :: item -> Bool,
    scFoldMap :: forall summary. Monoid summary => (item -> summary) -> summary
  }

supportCarrierFromList :: Ord item => [item] -> SupportCarrier item
supportCarrierFromList rawItems =
  SupportCarrier
    { scHasAny = not (Set.null itemSet),
      scContains = (`Set.member` itemSet),
      scFoldMap = \summarize -> foldMap summarize itemSet
    }
  where
    itemSet =
      Set.fromList rawItems

supportCarrierItems :: SupportCarrier item -> [item]
supportCarrierItems carrier =
  scFoldMap carrier (: [])
{-# INLINE supportCarrierItems #-}

supportCarrierCount :: SupportCarrier item -> Natural
supportCarrierCount carrier =
  getSum (scFoldMap carrier (const (Sum 1)))
{-# INLINE supportCarrierCount #-}

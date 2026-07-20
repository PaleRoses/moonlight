module Moonlight.Derived.Test.Fixture
  ( mkTestFunctor
  ) where

import Data.Map.Strict qualified as Map
import Data.Vector qualified as Vector
import Moonlight.Category (FinObjectId (..))
import Moonlight.Derived.Pure.Site.Poset
  ( DerivedPoset (..)
  , DerivedPosetFunctor
  , mkDerivedPosetFunctor
  )

mkTestFunctor :: DerivedPoset -> DerivedPoset -> (FinObjectId -> FinObjectId) -> Either String DerivedPosetFunctor
mkTestFunctor sourcePoset targetPoset mapNode =
  either (Left . show) Right
    ( mkDerivedPosetFunctor
        sourcePoset
        targetPoset
        ( Map.fromList
            [ (FinObjectId sourceKey, FinObjectId targetKey)
            | sourceNode@(FinObjectId sourceKey) <- Vector.toList (derivedPosetNodes sourcePoset)
            , let FinObjectId targetKey = mapNode sourceNode
            ]
        )
    )

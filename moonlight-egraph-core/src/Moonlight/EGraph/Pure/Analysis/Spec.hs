module Moonlight.EGraph.Pure.Analysis.Spec
  ( AnalysisSpec (..),
  )
where

import Data.Kind (Type)

type AnalysisSpec :: (Type -> Type) -> Type -> Type
data AnalysisSpec f a = AnalysisSpec
  { asMake :: f a -> a,
    asJoin :: a -> a -> a,
    asJoinChanged :: a -> a -> (a, Bool)
  }

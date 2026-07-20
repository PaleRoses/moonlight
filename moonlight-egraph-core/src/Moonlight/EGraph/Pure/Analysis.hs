module Moonlight.EGraph.Pure.Analysis
  ( AnalysisSpec (..),
    semilatticeAnalysis,
  )
where

import Moonlight.Algebra (JoinSemilattice (join))
import Moonlight.EGraph.Pure.Analysis.Spec (AnalysisSpec (..))

semilatticeAnalysis :: (JoinSemilattice a, Eq a) => (f a -> a) -> AnalysisSpec f a
semilatticeAnalysis make =
  AnalysisSpec
    { asMake = make,
      asJoin = join,
      asJoinChanged = \left right ->
        let joined = join left right
         in (joined, joined /= left)
    }

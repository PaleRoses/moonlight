module Moonlight.EGraph.Pure.Types.Internal
  ( EGraph (..),
    bumpEGraphRevision,
  )
where

import Data.Kind (Type)
import Data.IntMap.Strict (IntMap)
import Moonlight.Core (TheorySpec)
import Moonlight.EGraph.Pure.Analysis.Spec (AnalysisSpec)
import Moonlight.EGraph.Pure.Delta (EGraphEditDelta)
import Moonlight.Core (UnionFind)
import Moonlight.EGraph.Pure.Structural.Store (StructuralStore)
import Moonlight.EGraph.Pure.Types.Core (EGraphRevision, nextEGraphRevision)

type EGraph :: (Type -> Type) -> Type -> Type
data EGraph f a = EGraph
  { egUnionFind :: !UnionFind,
    egStore :: !(StructuralStore f),
    egAnalysis :: !(IntMap a),
    egPendingDelta :: !EGraphEditDelta,
    egAnalysisSpec :: AnalysisSpec f a,
    egTheorySpec :: TheorySpec f,
    egRevision :: !EGraphRevision
  }

bumpEGraphRevision :: EGraph f a -> EGraph f a
bumpEGraphRevision graph =
  graph {egRevision = nextEGraphRevision (egRevision graph)}

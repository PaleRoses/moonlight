module Moonlight.Sheaf.Obstruction.Cohomological.Types.Provenance
  ( NerveEdge (..),
    OrientedNerveEdge (..),
    CoverNerve (..),
  )
where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Set (Set)

type NerveEdge :: Type -> Type
data NerveEdge ctx = NerveEdge
  { neLeftContext :: !ctx,
    neRightContext :: !ctx
  }
  deriving stock (Eq, Ord, Show)

type OrientedNerveEdge :: Type -> Type
data OrientedNerveEdge ctx = OrientedNerveEdge
  { oneSourceContext :: !ctx,
    oneTargetContext :: !ctx
  }
  deriving stock (Eq, Ord, Show)

type CoverNerve :: Type -> Type
data CoverNerve ctx = CoverNerve
  { cnVertices :: ![ctx],
    cnEdges :: !(Set (NerveEdge ctx)),
    cnAdjacency :: !(Map ctx (Set ctx)),
    cnFundamentalCycles :: ![NonEmpty ctx]
  }
  deriving stock (Eq, Show)

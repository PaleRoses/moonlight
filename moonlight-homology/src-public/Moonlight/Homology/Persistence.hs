module Moonlight.Homology.Persistence
  ( BiFilteredCell (..),
    BiPersistencePair (..),
    FiltrationValue (..),
    FilteredFiniteChainComplex (..),
    mkFilteredFiniteChainComplex,
    mod2PersistentPairs,
    mod2PersistenceTopologyWitness,
  )
where

import Data.Kind (Type)
import Moonlight.Homology.Pure.Chain (HomologicalDegree)
import Moonlight.Homology.Topology (BasisCellRef)
import Moonlight.Homology.Pure.Filtration (FiltrationValue (..))
import Moonlight.Homology.Pure.Topology.Core (FilteredFiniteChainComplex (..))
import Moonlight.Homology.Pure.Topology.Persistence
  ( mkFilteredFiniteChainComplex,
    mod2PersistentPairs,
    mod2PersistenceTopologyWitness,
  )

-- | A cell with two independent filtration parameters. Forward-looking
-- vocabulary for two-parameter persistence: no function in this package
-- produces or consumes these values yet — they fix the types downstream
-- multi-parameter code will speak.
type BiFilteredCell :: Type
data BiFilteredCell = BiFilteredCell
  { bfcCell :: BasisCellRef,
    bfcBirth1 :: FiltrationValue,
    bfcBirth2 :: FiltrationValue
  }
  deriving stock (Eq, Show)

-- | A two-parameter birth/death pair. Forward-looking vocabulary; see
-- 'BiFilteredCell'.
type BiPersistencePair :: Type
data BiPersistencePair = BiPersistencePair
  { bppDegree :: HomologicalDegree,
    bppBirth :: (FiltrationValue, FiltrationValue),
    bppDeath :: Maybe (FiltrationValue, FiltrationValue)
  }
  deriving stock (Eq, Show)

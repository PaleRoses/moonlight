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

type BiFilteredCell :: Type
data BiFilteredCell = BiFilteredCell
  { bfcCell :: BasisCellRef,
    bfcBirth1 :: FiltrationValue,
    bfcBirth2 :: FiltrationValue
  }
  deriving stock (Eq, Show)

type BiPersistencePair :: Type
data BiPersistencePair = BiPersistencePair
  { bppDegree :: HomologicalDegree,
    bppBirth :: (FiltrationValue, FiltrationValue),
    bppDeath :: Maybe (FiltrationValue, FiltrationValue)
  }
  deriving stock (Eq, Show)

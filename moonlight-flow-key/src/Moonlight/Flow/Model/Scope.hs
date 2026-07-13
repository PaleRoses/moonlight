{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Flow.Model.Scope
  ( DepsWitness,
    TopoWitness,
    RootsWitness,
    ResultsWitness,
    ImpactedWitness,
    WitnessReverseIndex (..),
    DepsDelta (..),
    TopoDelta (..),
    RootsDelta (..),
    ResultsDelta (..),
    ImpactedDelta (..),
    RelationalScope (..),
    relationalScopeFromSets,
    relationalScopeNull,
    relationalScopeScope,
    relationalScopeDelta,
    scopeDeps,
    scopeTopo,
    scopeRoots,
    scopeResults,
    scopeImpacted,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import GHC.Generics
  ( Generic,
    Generically (..),
  )
import Moonlight.Delta.Scope
  ( ScopeCarrier (..),
    Scope,
    Scoped,
    dirtyScope,
    scopedDelta,
  )

type DepsWitness :: Type
data DepsWitness

type TopoWitness :: Type
data TopoWitness

type RootsWitness :: Type
data RootsWitness

type ResultsWitness :: Type
data ResultsWitness

type ImpactedWitness :: Type
data ImpactedWitness

type WitnessReverseIndex :: Type -> Type
newtype WitnessReverseIndex witness = WitnessReverseIndex
  { unWitnessReverseIndex :: IntMap IntSet
  }
  deriving stock (Eq, Show)

type DepsDelta :: Type
newtype DepsDelta = DepsDelta
  { unDepsDelta :: IntSet
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Semigroup, Monoid)

type TopoDelta :: Type
newtype TopoDelta = TopoDelta
  { unTopoDelta :: IntSet
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Semigroup, Monoid)

type RootsDelta :: Type
newtype RootsDelta = RootsDelta
  { unRootsDelta :: IntSet
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Semigroup, Monoid)

type ResultsDelta :: Type
newtype ResultsDelta = ResultsDelta
  { unResultsDelta :: IntSet
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Semigroup, Monoid)

type ImpactedDelta :: Type
newtype ImpactedDelta = ImpactedDelta
  { unImpactedDelta :: IntSet
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Semigroup, Monoid)

type RelationalScope :: Type
data RelationalScope = RelationalScope
  { rsDeps :: !DepsDelta,
    rsTopo :: !TopoDelta,
    rsRoots :: !RootsDelta,
    rsResults :: !ResultsDelta,
    rsImpacted :: !ImpactedDelta
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Semigroup, Monoid) via (Generically RelationalScope)

relationalScopeFromSets :: IntSet -> IntSet -> IntSet -> IntSet -> IntSet -> RelationalScope
relationalScopeFromSets deps topo roots results impacted =
  RelationalScope
    { rsDeps = DepsDelta deps,
      rsTopo = TopoDelta topo,
      rsRoots = RootsDelta roots,
      rsResults = ResultsDelta results,
      rsImpacted = ImpactedDelta impacted
    }
{-# INLINE relationalScopeFromSets #-}

scopeDeps :: RelationalScope -> IntSet
scopeDeps =
  unDepsDelta . rsDeps
{-# INLINE scopeDeps #-}

scopeTopo :: RelationalScope -> IntSet
scopeTopo =
  unTopoDelta . rsTopo
{-# INLINE scopeTopo #-}

scopeRoots :: RelationalScope -> IntSet
scopeRoots =
  unRootsDelta . rsRoots
{-# INLINE scopeRoots #-}

scopeResults :: RelationalScope -> IntSet
scopeResults =
  unResultsDelta . rsResults
{-# INLINE scopeResults #-}

scopeImpacted :: RelationalScope -> IntSet
scopeImpacted =
  unImpactedDelta . rsImpacted
{-# INLINE scopeImpacted #-}

relationalScopeNull :: RelationalScope -> Bool
relationalScopeNull scope =
  IntSet.null (scopeDeps scope)
    && IntSet.null (scopeTopo scope)
    && IntSet.null (scopeRoots scope)
    && IntSet.null (scopeResults scope)
    && IntSet.null (scopeImpacted scope)
{-# INLINE relationalScopeNull #-}

instance ScopeCarrier RelationalScope where
  scopeCarrierNull =
    relationalScopeNull

relationalScopeScope ::
  RelationalScope ->
  Scope RelationalScope
relationalScopeScope =
  dirtyScope
{-# INLINE relationalScopeScope #-}

relationalScopeDelta ::
  RelationalScope ->
  Maybe payload ->
  Scoped RelationalScope payload
relationalScopeDelta scope payload =
  scopedDelta (dirtyScope scope) payload
{-# INLINE relationalScopeDelta #-}

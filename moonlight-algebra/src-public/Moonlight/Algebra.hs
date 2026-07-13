{-|
Convenience re-export of the whole "Moonlight.Algebra.Pure" tower as a single
import. For finer-grained control, import the individual @Moonlight.Algebra.Pure.*@
modules directly.
-}
module Moonlight.Algebra
  ( -- * Standard semigroups, monoids and groups
    module Group,
    -- * Free structures
    module FreeMonoid,
    module FreeAbelianGroup,
    -- * Actions
    module Action,
    -- * Lattices and orientation
    module Lattice,
    module Orientation,
    -- * Rings, modular arithmetic and number theory
    module Ring,
    module Zn,
    module NumberTheory,
    module GCD,
    -- * Modules, vector spaces and magnitude
    module Module,
    module Magnitude,
    -- * Polynomials
    module Polynomial,
    -- * Power sets, products and quotients
    module PowerSet,
    module Product,
    module Quotient,
    -- * Sparse vectors
    module SparseVec,
    -- * Selected re-exports from "Moonlight.Core"
    AdditiveGroup,
    AdditiveMonoid (..),
    Field (..),
    MultiplicativeMonoid (..),
    Ring,
  )
where

import Moonlight.Algebra.Pure.Action as Action
import Moonlight.Algebra.Pure.FreeAbelianGroup as FreeAbelianGroup
import Moonlight.Algebra.Pure.FreeMonoid as FreeMonoid
import Moonlight.Algebra.Pure.GCD as GCD
import Moonlight.Algebra.Pure.Group as Group
import Moonlight.Algebra.Pure.Lattice as Lattice
import Moonlight.Algebra.Pure.Magnitude as Magnitude
import Moonlight.Algebra.Pure.Module as Module
import Moonlight.Algebra.Pure.NumberTheory as NumberTheory
import Moonlight.Algebra.Pure.Orientation as Orientation
import Moonlight.Algebra.Pure.Polynomial as Polynomial
import Moonlight.Algebra.Pure.PowerSet as PowerSet
import Moonlight.Algebra.Pure.Product as Product
import Moonlight.Algebra.Pure.Quotient as Quotient
import Moonlight.Algebra.Pure.Ring as Ring
import Moonlight.Algebra.Pure.SparseVec as SparseVec
import Moonlight.Algebra.Pure.Zn as Zn
import Moonlight.Core
  ( AdditiveGroup,
    AdditiveMonoid (..),
    Field (..),
    MultiplicativeMonoid (..),
    Ring,
  )

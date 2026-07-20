module Moonlight.EGraph.Test.Context.ThreeLevel
  ( Scope (..),
  )
where

import Moonlight.Algebra
  ( BoundedJoinSemilattice (..),
    BoundedMeetSemilattice (..),
    JoinSemilattice (..),
    Lattice,
    MeetSemilattice (..),
  )
import qualified Test.Tasty.QuickCheck as QC
import Data.Kind (Type)

type Scope :: Type
data Scope
  = GlobalCtx
  | ModuleCtx
  | LocalCtx
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance JoinSemilattice Scope where
  join = max

instance BoundedJoinSemilattice Scope where
  bottom = GlobalCtx

instance MeetSemilattice Scope where
  meet = min

instance BoundedMeetSemilattice Scope where
  top = LocalCtx

instance Lattice Scope

instance QC.Arbitrary Scope where
  arbitrary = QC.elements [GlobalCtx, ModuleCtx, LocalCtx]

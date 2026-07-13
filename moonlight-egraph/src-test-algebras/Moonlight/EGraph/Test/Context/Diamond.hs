module Moonlight.EGraph.Test.Context.Diamond
  ( DiamondCtx (..),
  )
where

import Moonlight.Algebra
  ( BoundedJoinSemilattice (..),
    BoundedMeetSemilattice (..),
    JoinSemilattice (..),
    Lattice,
    MeetSemilattice (..),
  )
import Data.Kind (Type)

type DiamondCtx :: Type
data DiamondCtx
  = DBottom
  | DLeft
  | DRight
  | DTop
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance JoinSemilattice DiamondCtx where
  join DBottom b = b
  join a DBottom = a
  join DTop _ = DTop
  join _ DTop = DTop
  join DLeft DLeft = DLeft
  join DRight DRight = DRight
  join DLeft DRight = DTop
  join DRight DLeft = DTop

instance BoundedJoinSemilattice DiamondCtx where
  bottom = DBottom

instance MeetSemilattice DiamondCtx where
  meet DTop b = b
  meet a DTop = a
  meet DBottom _ = DBottom
  meet _ DBottom = DBottom
  meet DLeft DLeft = DLeft
  meet DRight DRight = DRight
  meet DLeft DRight = DBottom
  meet DRight DLeft = DBottom

instance BoundedMeetSemilattice DiamondCtx where
  top = DTop

instance Lattice DiamondCtx

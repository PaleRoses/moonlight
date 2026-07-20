{-# OPTIONS_GHC -Wno-orphans #-}

module Moonlight.Sketch.Pure.Instances () where

import Moonlight.Algebra
  ( BoundedJoinSemilattice (..),
    BoundedMeetSemilattice (..),
    JoinSemilattice (..),
    Lattice,
    MeetSemilattice (..),
  )
import Moonlight.Sketch.Pure.Normalize (normalize)
import Moonlight.Sketch.Pure.Subtype (isSubtype)
import Moonlight.Sketch.Pure.Types (SchemaNode (..))

instance JoinSemilattice SchemaNode where
  join left right =
    case (left, right) of
      (SVoid, other) -> other
      (other, SVoid) -> other
      (SUnknown, _) -> SUnknown
      (_, SUnknown) -> SUnknown
      (l, r)
        | l == r -> l
        | isSubtype l r -> r
        | isSubtype r l -> l
        | otherwise -> normalize (SUnion [l, r])

instance BoundedJoinSemilattice SchemaNode where
  bottom = SVoid

instance MeetSemilattice SchemaNode where
  meet left right =
    case (left, right) of
      (SUnknown, other) -> other
      (other, SUnknown) -> other
      (SVoid, _) -> SVoid
      (_, SVoid) -> SVoid
      (l, r)
        | l == r -> l
        | isSubtype l r -> l
        | isSubtype r l -> r
        | otherwise -> SVoid

instance BoundedMeetSemilattice SchemaNode where
  top = SUnknown

instance Lattice SchemaNode


-- | Type-indexed covering families: the 'Exists', 'Dict' and
-- 'CoveringFamily'/'CoveringConstraints' machinery for enumerating and constraining
-- the members of a kind.
module Moonlight.Category.Pure.CoveringFamily
  ( CoveringFamily (..),
    Exists (..),
    Dict (..),
    CoveringConstraints (..),
    withMember,
    traverseMembers,
    traverseMembers_,
  )
where

import Data.Kind (Constraint, Type)

type Dict :: Constraint -> Type
data Dict (c :: Constraint) where
  Dict :: c => Dict c

type Exists :: forall k. (k -> Type) -> Type
data Exists (w :: k -> Type) where
  Exists :: w member -> Exists w

type CoveringFamily :: forall k. (k -> Type) -> Constraint
class CoveringFamily (w :: k -> Type) where
  allMembers :: [Exists w]

type CoveringConstraints :: forall k. (k -> Type) -> (k -> Constraint) -> Constraint
class CoveringFamily w => CoveringConstraints (w :: k -> Type) (c :: k -> Constraint) where
  constraintDict :: w member -> Dict (c member)

withMember ::
  forall k (w :: k -> Type) (c :: k -> Constraint) r.
  CoveringConstraints w c =>
  Exists w ->
  (forall (member :: k). c member => w member -> r) ->
  r
withMember (Exists witness) continuation =
  case constraintDict @k @w @c witness of
    Dict -> continuation witness

traverseMembers ::
  forall k (w :: k -> Type) (c :: k -> Constraint) r.
  CoveringConstraints w c =>
  (forall (member :: k). c member => w member -> r) ->
  [r]
traverseMembers continuation =
  fmap (\existential -> withMember @k @w @c existential continuation) (allMembers @k @w)

traverseMembers_ ::
  forall k (w :: k -> Type) (c :: k -> Constraint) m.
  (CoveringConstraints w c, Applicative m) =>
  (forall (member :: k). c member => w member -> m ()) ->
  m ()
traverseMembers_ continuation =
  sequenceAll (fmap (\existential -> withMember @k @w @c existential continuation) (allMembers @k @w))
  where
    sequenceAll :: Applicative f => [f ()] -> f ()
    sequenceAll [] = pure ()
    sequenceAll (x : xs) = x *> sequenceAll xs

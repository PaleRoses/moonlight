module Moonlight.EGraph.Test.Context.Anatomy
  ( AnatomyRegion (..),
    anatomyAncestors,
    anatomyLeq,
    coarseAnatomyLattice,
    preciseAnatomyLattice,
  )
where

import Data.Kind (Type)
import Moonlight.Algebra
  ( BoundedJoinSemilattice (..),
    BoundedMeetSemilattice (..),
    JoinSemilattice (..),
    Lattice,
    MeetSemilattice (..)
  )
import Moonlight.FiniteLattice
  ( ContextLattice,
    latticeContext
  )

type AnatomyRegion :: Type
data AnatomyRegion
  = Whole
  | Upper
  | Lower
  | Head
  | Torso
  | ArmLeft
  | ArmRight
  | LegLeft
  | LegRight
  | Local
  deriving stock (Eq, Ord, Show, Enum, Bounded)

anatomyAncestors :: AnatomyRegion -> [AnatomyRegion]
anatomyAncestors anatomyRegion =
  case anatomyRegion of
    Whole -> []
    Upper -> [Whole]
    Lower -> [Whole]
    Head -> [Upper, Whole]
    Torso -> [Upper, Whole]
    ArmLeft -> [Upper, Whole]
    ArmRight -> [Upper, Whole]
    LegLeft -> [Lower, Whole]
    LegRight -> [Lower, Whole]
    Local -> [minBound .. LegRight]

anatomyLeq :: AnatomyRegion -> AnatomyRegion -> Bool
anatomyLeq leftRegion rightRegion
  | leftRegion == rightRegion = True
  | rightRegion == Local = True
  | leftRegion == Whole = True
  | otherwise = leftRegion `elem` anatomyAncestors rightRegion

coarseAnatomyLattice :: ContextLattice AnatomyRegion
coarseAnatomyLattice =
  checkedAnatomyLattice

preciseAnatomyLattice :: ContextLattice AnatomyRegion
preciseAnatomyLattice =
  checkedAnatomyLattice

checkedAnatomyLattice :: ContextLattice AnatomyRegion
checkedAnatomyLattice =
  case latticeContext of
    Right latticeValue -> latticeValue
    Left compileError ->
      error ("invalid AnatomyRegion lattice fixture: " <> show compileError)

instance JoinSemilattice AnatomyRegion where
  join =
    anatomyLeastUpperBound

instance MeetSemilattice AnatomyRegion where
  meet =
    anatomyGreatestLowerBound

instance BoundedJoinSemilattice AnatomyRegion where
  bottom =
    Whole

instance BoundedMeetSemilattice AnatomyRegion where
  top =
    Local

instance Lattice AnatomyRegion

allAnatomyRegions :: [AnatomyRegion]
allAnatomyRegions =
  [minBound .. maxBound]

coarseAnatomyJoin :: AnatomyRegion -> AnatomyRegion -> AnatomyRegion
coarseAnatomyJoin leftRegion rightRegion
  | anatomyLeq leftRegion rightRegion = rightRegion
  | anatomyLeq rightRegion leftRegion = leftRegion
  | otherwise = Local

coarseAnatomyMeet :: AnatomyRegion -> AnatomyRegion -> AnatomyRegion
coarseAnatomyMeet leftRegion rightRegion
  | anatomyLeq leftRegion rightRegion = leftRegion
  | anatomyLeq rightRegion leftRegion = rightRegion
  | otherwise = Whole

anatomyLeastUpperBound :: AnatomyRegion -> AnatomyRegion -> AnatomyRegion
anatomyLeastUpperBound leftRegion rightRegion =
  let leftUpperBounds = filter (anatomyLeq leftRegion) allAnatomyRegions
      rightUpperBounds = filter (anatomyLeq rightRegion) allAnatomyRegions
      commonUpperBounds = filter (`elem` rightUpperBounds) leftUpperBounds
      isLeastUpperBound candidate =
        not (any (\otherCandidate -> otherCandidate /= candidate && anatomyLeq otherCandidate candidate) commonUpperBounds)
   in case filter isLeastUpperBound commonUpperBounds of
        firstUpperBound : _ -> firstUpperBound
        [] -> Local

anatomyGreatestLowerBound :: AnatomyRegion -> AnatomyRegion -> AnatomyRegion
anatomyGreatestLowerBound leftRegion rightRegion =
  let leftLowerBounds = filter (\candidate -> anatomyLeq candidate leftRegion) allAnatomyRegions
      rightLowerBounds = filter (\candidate -> anatomyLeq candidate rightRegion) allAnatomyRegions
      commonLowerBounds = filter (`elem` rightLowerBounds) leftLowerBounds
      isGreatestLowerBound candidate =
        not (any (\otherCandidate -> otherCandidate /= candidate && anatomyLeq candidate otherCandidate) commonLowerBounds)
   in case filter isGreatestLowerBound commonLowerBounds of
        firstLowerBound : _ -> firstLowerBound
        [] -> Whole

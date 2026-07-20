module Moonlight.Constraint.Pure.CoFiniteTruth
  ( CoFiniteTruth,
    coFiniteTruth,
    coFiniteDefault,
    coFiniteOverrides,
    lookupCoFiniteTruth,
    setCoFiniteTruth,
    coFiniteTrueOverrides,
    combineCoFiniteTruth,
    applyEndoPatch,
  )
where

import Data.Kind (Type)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Moonlight.Algebra
  ( Action (..),
    BooleanAlgebra (..),
    BoundedJoinSemilattice (..),
    BoundedMeetSemilattice (..),
    DistributiveLattice,
    HeytingAlgebra (..),
    JoinSemilattice (..),
    Lattice,
    MeetSemilattice (..),
    EndoPatch,
    endoPatchAssignments,
  )

type CoFiniteTruth :: Type -> Type
data CoFiniteTruth k = CoFiniteTruth
  { coFiniteDefault :: Bool,
    coFiniteOverrides :: Map.Map k Bool
  }
  deriving stock (Eq, Show)

coFiniteTruth :: Bool -> Map.Map k Bool -> CoFiniteTruth k
coFiniteTruth defaultValue overrides =
  CoFiniteTruth defaultValue (Map.filter (/= defaultValue) overrides)

lookupCoFiniteTruth :: Ord k => k -> CoFiniteTruth k -> Bool
lookupCoFiniteTruth key (CoFiniteTruth defaultValue overrides) =
  Map.findWithDefault defaultValue key overrides

setCoFiniteTruth :: Ord k => k -> Bool -> CoFiniteTruth k -> CoFiniteTruth k
setCoFiniteTruth key value (CoFiniteTruth defaultValue overrides) =
  CoFiniteTruth defaultValue
    ( if value == defaultValue
        then Map.delete key overrides
        else Map.insert key value overrides
    )

coFiniteTrueOverrides :: CoFiniteTruth k -> Set.Set k
coFiniteTrueOverrides (CoFiniteTruth _ overrides) =
  Map.keysSet (Map.filter id overrides)

combineCoFiniteTruth ::
  Ord k =>
  (Bool -> Bool -> Bool) ->
  CoFiniteTruth k ->
  CoFiniteTruth k ->
  CoFiniteTruth k
combineCoFiniteTruth combine left right =
  CoFiniteTruth
    combinedDefault
    (Map.mapMaybeWithKey combineOverride allOverrideKeys)
  where
    combinedDefault = combine (coFiniteDefault left) (coFiniteDefault right)
    allOverrideKeys =
      Map.union (coFiniteOverrides left) (coFiniteOverrides right)
    combineOverride key _ =
      let combinedValue = combine (lookupCoFiniteTruth key left) (lookupCoFiniteTruth key right)
       in if combinedValue == combinedDefault
            then Nothing
            else Just combinedValue

instance Ord k => JoinSemilattice (CoFiniteTruth k) where
  join = combineCoFiniteTruth (||)

instance Ord k => BoundedJoinSemilattice (CoFiniteTruth k) where
  bottom = coFiniteTruth False Map.empty

instance Ord k => MeetSemilattice (CoFiniteTruth k) where
  meet = combineCoFiniteTruth (&&)

instance Ord k => BoundedMeetSemilattice (CoFiniteTruth k) where
  top = coFiniteTruth True Map.empty

instance Ord k => Lattice (CoFiniteTruth k)

instance Ord k => DistributiveLattice (CoFiniteTruth k)

instance Ord k => HeytingAlgebra (CoFiniteTruth k) where
  implies = combineCoFiniteTruth (\left right -> not left || right)

instance Ord k => BooleanAlgebra (CoFiniteTruth k) where
  complement (CoFiniteTruth defaultValue overrides) =
    CoFiniteTruth (not defaultValue) (Map.map not overrides)

instance Ord k => Action (EndoPatch k) (CoFiniteTruth k) where
  act = applyEndoPatch

applyEndoPatch :: Ord k => EndoPatch k -> CoFiniteTruth k -> CoFiniteTruth k
applyEndoPatch patch truthValue =
  Map.foldrWithKey setCoFiniteTruth truthValue (endoPatchAssignments patch)

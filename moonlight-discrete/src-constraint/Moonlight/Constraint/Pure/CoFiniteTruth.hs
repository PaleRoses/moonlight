module Moonlight.Constraint.Pure.CoFiniteTruth
  ( CoFiniteTruth,
    coFiniteTruth,
    coFiniteDefault,
    coFiniteOverrides,
    normalizeCoFiniteTruth,
    lookupCoFiniteTruth,
    setCoFiniteTruth,
    coFiniteTrueOverrides,
    combineCoFiniteTruth,
    EndoPatch,
    endoPatch,
    normalizeEndoPatch,
    endoPatchAdds,
    endoPatchRemoves,
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
  )

type CoFiniteTruth :: Type -> Type
data CoFiniteTruth k = CoFiniteTruth
  { coFiniteDefault :: Bool,
    coFiniteOverrides :: Map.Map k Bool
  }
  deriving stock (Eq, Show)

coFiniteTruth :: Bool -> Map.Map k Bool -> CoFiniteTruth k
coFiniteTruth defaultValue overrides =
  normalizeCoFiniteTruth (CoFiniteTruth defaultValue overrides)

normalizeCoFiniteTruth :: CoFiniteTruth k -> CoFiniteTruth k
normalizeCoFiniteTruth (CoFiniteTruth defaultValue overrides) =
  CoFiniteTruth defaultValue (Map.filter (/= defaultValue) overrides)

lookupCoFiniteTruth :: Ord k => k -> CoFiniteTruth k -> Bool
lookupCoFiniteTruth key (CoFiniteTruth defaultValue overrides) =
  Map.findWithDefault defaultValue key overrides

setCoFiniteTruth :: Ord k => k -> Bool -> CoFiniteTruth k -> CoFiniteTruth k
setCoFiniteTruth key value (CoFiniteTruth defaultValue overrides) =
  normalizeCoFiniteTruth (CoFiniteTruth defaultValue (Map.insert key value overrides))

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
  normalizeCoFiniteTruth
    ( CoFiniteTruth
        combinedDefault
        (Map.fromSet combineAtKey allKeys)
    )
  where
    combinedDefault = combine (coFiniteDefault left) (coFiniteDefault right)
    allKeys =
      Set.union
        (Map.keysSet (coFiniteOverrides left))
        (Map.keysSet (coFiniteOverrides right))
    combineAtKey key = combine (lookupCoFiniteTruth key left) (lookupCoFiniteTruth key right)

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
    normalizeCoFiniteTruth (CoFiniteTruth (not defaultValue) (Map.map not overrides))

type EndoPatch :: Type -> Type
data EndoPatch k = EndoPatch
  { endoPatchAdds :: Set.Set k,
    endoPatchRemoves :: Set.Set k
  }
  deriving stock (Eq, Show, Read)

endoPatch :: Set.Set k -> Set.Set k -> EndoPatch k
endoPatch = EndoPatch

normalizeEndoPatch :: Ord k => EndoPatch k -> EndoPatch k
normalizeEndoPatch (EndoPatch adds removes) =
  EndoPatch adds (Set.difference removes adds)

instance Ord k => Semigroup (EndoPatch k) where
  (<>) (EndoPatch addLeft removeLeft) (EndoPatch addRight removeRight) =
    EndoPatch
      (Set.union (Set.difference addLeft removeRight) addRight)
      (Set.union (Set.difference removeLeft addRight) removeRight)


instance Ord k => Monoid (EndoPatch k) where
  mempty = EndoPatch Set.empty Set.empty

instance Ord k => Action (EndoPatch k) (CoFiniteTruth k) where
  act = applyEndoPatch

applyEndoPatch :: Ord k => EndoPatch k -> CoFiniteTruth k -> CoFiniteTruth k
applyEndoPatch patch =
  applyAssignments keysToAdd True . applyAssignments keysToRemove False
  where
    EndoPatch keysToAdd keysToRemove = normalizeEndoPatch patch

    applyAssignments :: Ord k => Set.Set k -> Bool -> CoFiniteTruth k -> CoFiniteTruth k
    applyAssignments keys value =
      Set.foldr
        (\key continue -> setCoFiniteTruth key value . continue)
        id
        keys

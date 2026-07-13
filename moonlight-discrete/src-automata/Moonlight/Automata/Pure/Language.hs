module Moonlight.Automata.Pure.Language
  ( Language (..),
    treeLanguageFromAcceptingDBTA,
    treeLanguageFromAcceptingNBTA,
  )
where

import Data.Functor.Foldable (Base, Recursive)
import Data.Kind (Type)
import Moonlight.Algebra
  ( BooleanAlgebra (complement),
    BoundedJoinSemilattice (bottom),
    BoundedMeetSemilattice (top),
    DistributiveLattice,
    HeytingAlgebra (implies),
    JoinSemilattice (join),
    Lattice,
    MeetSemilattice (meet),
  )
import Moonlight.Automata.Pure.Algebra
  ( acceptsDBTA,
    acceptsNBTA,
  )
import Moonlight.Automata.Pure.Core
  ( AcceptingDBTA,
    AcceptingNBTA,
  )
type Language :: Type -> Type
newtype Language carrier = Language
  { runLanguage :: carrier -> Bool
  }

treeLanguageFromAcceptingDBTA :: (Recursive tree, Base tree ~ f) => AcceptingDBTA f state -> Language tree
treeLanguageFromAcceptingDBTA automaton =
  Language (acceptsDBTA automaton)

treeLanguageFromAcceptingNBTA :: (Recursive tree, Base tree ~ f) => AcceptingNBTA f state -> Language tree
treeLanguageFromAcceptingNBTA automaton =
  Language (acceptsNBTA automaton)

instance JoinSemilattice (Language carrier) where
  join (Language leftLanguage) (Language rightLanguage) =
    Language (liftBinaryLanguage (||) leftLanguage rightLanguage)

instance BoundedJoinSemilattice (Language carrier) where
  bottom = Language (const False)

instance MeetSemilattice (Language carrier) where
  meet (Language leftLanguage) (Language rightLanguage) =
    Language (liftBinaryLanguage (&&) leftLanguage rightLanguage)

instance BoundedMeetSemilattice (Language carrier) where
  top = Language (const True)

instance Lattice (Language carrier)

instance DistributiveLattice (Language carrier)

instance HeytingAlgebra (Language carrier) where
  implies (Language leftLanguage) (Language rightLanguage) =
    Language (liftBinaryLanguage (\left right -> not left || right) leftLanguage rightLanguage)

instance BooleanAlgebra (Language carrier) where
  complement (Language language) =
    Language (liftUnaryLanguage not language)

liftUnaryLanguage :: (Bool -> Bool) -> (carrier -> Bool) -> carrier -> Bool
liftUnaryLanguage morphism language =
  morphism . language

liftBinaryLanguage :: (Bool -> Bool -> Bool) -> (carrier -> Bool) -> (carrier -> Bool) -> carrier -> Bool
liftBinaryLanguage morphism leftLanguage rightLanguage carrier =
  morphism (leftLanguage carrier) (rightLanguage carrier)

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Moonlight.Automata.Effect.Laws
  ( Rose (..),
    listFix,
    unlistFix,
    treeFix,
    untreeFix,
    dependentDBTAIsZygoLaw,
    productDBTALaw,
    intersectionAcceptanceLaw,
    unionAcceptanceLaw,
    complementAcceptanceLaw,
    denotationalUnionHomomorphismLaw,
    denotationalIntersectionHomomorphismLaw,
    denotationalComplementHomomorphismLaw,
    topDownFoldLaw,
    topDownAnnotationLaw,
    topDownAnnotatedAttributeLaw,
    topDownStateProjectionLaw,
    topDownAttributeProjectionLaw,
    rootAttributeProjectionLaw,
  )
where

import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree (..))
import Data.Fix (Fix (..))
import Data.Functor.Base (ListF (..), TreeF (..))
import Data.Functor.Foldable
  ( Base,
    Recursive (project),
    cata,
    zygo,
  )
import Data.Kind (Type)
import Moonlight.Algebra
  ( BooleanAlgebra (complement),
    JoinSemilattice (join),
    MeetSemilattice (meet),
  )
import Moonlight.Automata.Pure.Algebra
  ( acceptsDBTA,
    complementAcceptance,
    dependentDBTA,
    intersectionDBTA,
    productDBTA,
    unionDBTA,
  )
import Moonlight.Automata.Pure.Coalgebra
  ( annotateTopDown,
    annotateTopDownWithAttribute,
    inheritedAttribute,
    projectAttributeAnnotation,
    rootAttribute,
    topDownFold,
  )
import Moonlight.Automata.Pure.Core
  ( Acceptance,
    AcceptingDBTA (..),
    DBTA (..),
    TopDownTA (..),
    accepts,
  )
import Moonlight.Automata.Pure.Language
  ( runLanguage,
    treeLanguageFromAcceptingDBTA,
  )

type Rose :: Type -> Type
data Rose a = Rose a [Rose a]
  deriving stock (Eq, Show)

listFix :: [a] -> Fix (ListF a)
listFix = foldr (\value rest -> Fix (Cons value rest)) (Fix Nil)

unlistFix :: Fix (ListF a) -> [a]
unlistFix = cata toList
  where
    toList :: ListF a [a] -> [a]
    toList Nil = []
    toList (Cons value rest) = value : rest

treeFix :: Rose a -> Fix (TreeF a)
treeFix (Rose value children) = Fix (NodeF value (fmap treeFix children))

untreeFix :: Fix (TreeF a) -> Rose a
untreeFix = cata toRose
  where
    toRose :: TreeF a (Rose a) -> Rose a
    toRose (NodeF value children) = Rose value children

dependentDBTAIsZygoLaw :: (Recursive t, Base t ~ f, Functor f, Eq leftState, Eq rightState) => DBTA f leftState -> (f (leftState, rightState) -> rightState) -> t -> Bool
dependentDBTAIsZygoLaw leftAutomaton rightAlgebra value =
  cata (runDBTA (dependentDBTA leftAutomaton rightAlgebra)) value
    == ( cata (runDBTA leftAutomaton) value,
         zygo (runDBTA leftAutomaton) rightAlgebra value
       )

productDBTALaw :: (Recursive t, Base t ~ f, Functor f, Eq leftState, Eq rightState) => DBTA f leftState -> DBTA f rightState -> t -> Bool
productDBTALaw leftAutomaton rightAutomaton value =
  cata (runDBTA (productDBTA leftAutomaton rightAutomaton)) value
    == (cata (runDBTA leftAutomaton) value, cata (runDBTA rightAutomaton) value)

intersectionAcceptanceLaw :: (Recursive t, Base t ~ f, Functor f) => AcceptingDBTA f leftState -> AcceptingDBTA f rightState -> t -> Bool
intersectionAcceptanceLaw leftAutomaton rightAutomaton value =
  acceptsDBTA (intersectionDBTA leftAutomaton rightAutomaton) value
    == (acceptsDBTA leftAutomaton value && acceptsDBTA rightAutomaton value)

unionAcceptanceLaw :: (Recursive t, Base t ~ f, Functor f) => AcceptingDBTA f leftState -> AcceptingDBTA f rightState -> t -> Bool
unionAcceptanceLaw leftAutomaton rightAutomaton value =
  acceptsDBTA (unionDBTA leftAutomaton rightAutomaton) value
    == (acceptsDBTA leftAutomaton value || acceptsDBTA rightAutomaton value)

complementAcceptanceLaw :: Acceptance state -> state -> Bool
complementAcceptanceLaw acceptance value =
  accepts (complementAcceptance acceptance) value
    == not (accepts acceptance value)

denotationalUnionHomomorphismLaw :: (Recursive t, Base t ~ f, Functor f) => AcceptingDBTA f leftState -> AcceptingDBTA f rightState -> t -> Bool
denotationalUnionHomomorphismLaw leftAutomaton rightAutomaton value =
  runLanguage
    (join (treeLanguageFromAcceptingDBTA leftAutomaton) (treeLanguageFromAcceptingDBTA rightAutomaton))
    value
    == runLanguage
      (treeLanguageFromAcceptingDBTA (unionDBTA leftAutomaton rightAutomaton))
      value

denotationalIntersectionHomomorphismLaw :: (Recursive t, Base t ~ f, Functor f) => AcceptingDBTA f leftState -> AcceptingDBTA f rightState -> t -> Bool
denotationalIntersectionHomomorphismLaw leftAutomaton rightAutomaton value =
  runLanguage
    (meet (treeLanguageFromAcceptingDBTA leftAutomaton) (treeLanguageFromAcceptingDBTA rightAutomaton))
    value
    == runLanguage
      (treeLanguageFromAcceptingDBTA (intersectionDBTA leftAutomaton rightAutomaton))
      value

denotationalComplementHomomorphismLaw :: (Recursive t, Base t ~ f) => AcceptingDBTA f state -> t -> Bool
denotationalComplementHomomorphismLaw automaton value =
  runLanguage
    (complement (treeLanguageFromAcceptingDBTA automaton))
    value
    == runLanguage
      ( treeLanguageFromAcceptingDBTA
          AcceptingDBTA
            { adbtaAlgebra = adbtaAlgebra automaton,
              adbtaAcceptance = complementAcceptance (adbtaAcceptance automaton)
            }
      )
      value

topDownFoldLaw :: (Recursive t, Base t ~ f, Functor f, Eq attribute) => TopDownTA f state -> (state -> f attribute -> attribute) -> state -> t -> Bool
topDownFoldLaw automaton algebra initialState value =
  topDownFold automaton algebra initialState value
    == algebra
      initialState
      (fmap (uncurry (topDownFold automaton algebra)) (runTopDownTA automaton initialState (project value)))

topDownAnnotationLaw :: (Recursive t, Base t ~ f, Functor f, Eq state, Eq (f state)) => TopDownTA f state -> state -> t -> Bool
topDownAnnotationLaw automaton initialState value =
  case annotateTopDown automaton initialState value of
    annotatedState :< annotatedChildren ->
      annotatedState == initialState
        && fmap extract annotatedChildren == fmap fst (runTopDownTA automaton initialState (project value))

topDownAnnotatedAttributeLaw :: (Recursive t, Base t ~ f, Functor f, Eq state, Eq attribute, Eq (f state)) => TopDownTA f state -> (state -> f attribute -> attribute) -> state -> t -> Bool
topDownAnnotatedAttributeLaw automaton algebra initialState value =
  case annotateTopDownWithAttribute automaton algebra initialState value of
    (annotatedState, attribute) :< annotatedChildren ->
      annotatedState == initialState
        && attribute == inheritedAttribute automaton algebra initialState value
        && fmap (fst . extract) annotatedChildren == fmap fst (runTopDownTA automaton initialState (project value))

topDownStateProjectionLaw :: (Recursive t, Base t ~ f, Functor f, Eq state, Eq (f state)) => TopDownTA f state -> (state -> f attribute -> attribute) -> state -> t -> Bool
topDownStateProjectionLaw automaton algebra initialState value =
  case
    ( annotateTopDown automaton initialState value,
      annotateTopDownWithAttribute automaton algebra initialState value
    ) of
    (annotatedState :< annotatedChildren, (combinedState, _) :< combinedChildren) ->
      annotatedState == combinedState
        && fmap extract annotatedChildren == fmap (fst . extract) combinedChildren

topDownAttributeProjectionLaw :: (Recursive t, Base t ~ f, Functor f, Eq attribute, Eq (f attribute)) => TopDownTA f state -> (state -> f attribute -> attribute) -> state -> t -> Bool
topDownAttributeProjectionLaw automaton algebra initialState value =
  case
    ( projectAttributeAnnotation (annotateTopDownWithAttribute automaton algebra initialState value),
      annotateTopDownWithAttribute automaton algebra initialState value
    ) of
    (attribute :< annotatedChildren, (_, combinedAttribute) :< combinedChildren) ->
      attribute == combinedAttribute
        && attribute == inheritedAttribute automaton algebra initialState value
        && fmap extract annotatedChildren == fmap (snd . extract) combinedChildren

rootAttributeProjectionLaw :: (Recursive t, Base t ~ f, Functor f, Eq attribute) => TopDownTA f state -> (state -> f attribute -> attribute) -> state -> t -> Bool
rootAttributeProjectionLaw automaton algebra initialState value =
  rootAttribute (annotateTopDownWithAttribute automaton algebra initialState value)
    == inheritedAttribute automaton algebra initialState value

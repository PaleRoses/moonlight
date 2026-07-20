module Moonlight.Constraint.Pure.Types
  ( ConstraintExpr (..),
    ConstraintExprF (..),
    Literal (..),
    negateLiteral,
    literalVariable,
    literalPolarity,
    Clause,
    CNF,
    normalize,
    isLocallyIrreducible,
  )
where

import Data.Kind (Type)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Functor.Foldable (Base)
import Data.Functor.Foldable (Corecursive (..), Recursive (..), cata)

type ConstraintExpr :: Type -> Type
data ConstraintExpr a
  = Atom a
  | And [ConstraintExpr a]
  | Or [ConstraintExpr a]
  | Not (ConstraintExpr a)
  deriving stock (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

type ConstraintExprF :: Type -> Type -> Type
data ConstraintExprF a r
  = AtomF a
  | AndF [r]
  | OrF [r]
  | NotF r
  deriving stock (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

type instance Base (ConstraintExpr a) = ConstraintExprF a

instance Recursive (ConstraintExpr a) where
  project expression =
    case expression of
      Atom variable -> AtomF variable
      And children -> AndF children
      Or children -> OrF children
      Not child -> NotF child

instance Corecursive (ConstraintExpr a) where
  embed expressionLayer =
    case expressionLayer of
      AtomF variable -> Atom variable
      AndF children -> And children
      OrF children -> Or children
      NotF child -> Not child

type Literal :: Type -> Type
data Literal a
  = Pos a
  | Neg a
  deriving stock (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

negateLiteral :: Literal a -> Literal a
negateLiteral literal =
  case literal of
    Pos variable -> Neg variable
    Neg variable -> Pos variable

literalVariable :: Literal a -> a
literalVariable literal =
  case literal of
    Pos variable -> variable
    Neg variable -> variable

literalPolarity :: Literal a -> Bool
literalPolarity literal =
  case literal of
    Pos _ -> True
    Neg _ -> False

type Clause :: Type -> Type
type Clause a = Set (Literal a)

type CNF :: Type -> Type
type CNF a = [Clause a]

normalize :: Ord a => ConstraintExpr a -> ConstraintExpr a
normalize = cata normalizeLayer

isLocallyIrreducible :: Ord a => ConstraintExpr a -> Bool
isLocallyIrreducible = snd . cata irreducibilityLayer

irreducibilityLayer ::
  Ord a =>
  ConstraintExprF a (ConstraintExpr a, Bool) ->
  (ConstraintExpr a, Bool)
irreducibilityLayer expressionLayer =
  case expressionLayer of
    AtomF variable -> (Atom variable, True)
    AndF children ->
      irreducibleVariadic And canonicalAnd children
    OrF children ->
      irreducibleVariadic Or canonicalOr children
    NotF (child, childIrreducible) ->
      let expression = Not child
       in (expression, childIrreducible && complementNormalized child == expression)

irreducibleVariadic ::
  Ord a =>
  ([ConstraintExpr a] -> ConstraintExpr a) ->
  ([ConstraintExpr a] -> ConstraintExpr a) ->
  [(ConstraintExpr a, Bool)] ->
  (ConstraintExpr a, Bool)
irreducibleVariadic constructor canonicalize children =
  let childExpressions = fmap fst children
      expression = constructor childExpressions
   in (expression, all snd children && canonicalize childExpressions == expression)

normalizeLayer :: Ord a => ConstraintExprF a (ConstraintExpr a) -> ConstraintExpr a
normalizeLayer expressionLayer =
  case expressionLayer of
    AtomF variable -> Atom variable
    AndF children -> canonicalAnd children
    OrF children -> canonicalOr children
    NotF child -> complementNormalized child

canonicalAnd :: Ord a => [ConstraintExpr a] -> ConstraintExpr a
canonicalAnd normalizedChildren =
  let distinctChildren = Set.fromList (foldMap conjuncts normalizedChildren)
      materialChildren = Set.delete (And []) distinctChildren
      preFlattenComplement =
        any isConjunction normalizedChildren
          && hasComplementaryPair (Set.fromList normalizedChildren)
   in if preFlattenComplement
        || Set.member (Or []) distinctChildren
        || hasComplementaryPair materialChildren
        then Or []
        else collapseVariadic And (Set.filter (not . absorbedConjunct materialChildren) materialChildren)

canonicalOr :: Ord a => [ConstraintExpr a] -> ConstraintExpr a
canonicalOr normalizedChildren =
  let distinctChildren = Set.fromList (foldMap disjuncts normalizedChildren)
      materialChildren = Set.delete (Or []) distinctChildren
      preFlattenComplement =
        any isDisjunction normalizedChildren
          && hasComplementaryPair (Set.fromList normalizedChildren)
   in if preFlattenComplement
        || Set.member (And []) distinctChildren
        || hasComplementaryPair materialChildren
        then And []
        else collapseVariadic Or (Set.filter (not . absorbedDisjunct materialChildren) materialChildren)

conjuncts :: ConstraintExpr a -> [ConstraintExpr a]
conjuncts expression =
  case expression of
    And children -> children
    other -> [other]

disjuncts :: ConstraintExpr a -> [ConstraintExpr a]
disjuncts expression =
  case expression of
    Or children -> children
    other -> [other]

absorbedConjunct :: Ord a => Set (ConstraintExpr a) -> ConstraintExpr a -> Bool
absorbedConjunct siblings expression =
  case expression of
    Or children -> not (Set.disjoint siblings (Set.fromList children))
    _ -> False

absorbedDisjunct :: Ord a => Set (ConstraintExpr a) -> ConstraintExpr a -> Bool
absorbedDisjunct siblings expression =
  case expression of
    And children -> not (Set.disjoint siblings (Set.fromList children))
    _ -> False

hasComplementaryPair :: Ord a => Set (ConstraintExpr a) -> Bool
hasComplementaryPair expressions =
  let expressionList = Set.toAscList expressions
      positiveAtoms = Set.fromList [variable | Atom variable <- expressionList]
      negativeAtoms = Set.fromList [variable | Not (Atom variable) <- expressionList]
      conjunctions = Set.fromList [expression | expression@(And _) <- expressionList]
      disjunctions = Set.fromList [expression | expression@(Or _) <- expressionList]
      hasLiteralComplement = not (Set.disjoint positiveAtoms negativeAtoms)
      hasCompoundComplement
        | Set.null conjunctions || Set.null disjunctions = False
        | Set.size conjunctions <= Set.size disjunctions =
            not (Set.disjoint disjunctions (Set.map complementNormalized conjunctions))
        | otherwise =
            not (Set.disjoint conjunctions (Set.map complementNormalized disjunctions))
   in hasLiteralComplement || hasCompoundComplement

isConjunction :: ConstraintExpr a -> Bool
isConjunction expression =
  case expression of
    And _ -> True
    _ -> False

isDisjunction :: ConstraintExpr a -> Bool
isDisjunction expression =
  case expression of
    Or _ -> True
    _ -> False

complementNormalized :: Ord a => ConstraintExpr a -> ConstraintExpr a
complementNormalized expression =
  case expression of
    Atom variable -> Not (Atom variable)
    Not inner -> inner
    And children -> canonicalOr (fmap complementNormalized children)
    Or children -> canonicalAnd (fmap complementNormalized children)

collapseVariadic :: ([ConstraintExpr a] -> ConstraintExpr a) -> Set (ConstraintExpr a) -> ConstraintExpr a
collapseVariadic constructor expressions =
  case Set.toAscList expressions of
    [] -> constructor []
    [single] -> single
    children -> constructor children

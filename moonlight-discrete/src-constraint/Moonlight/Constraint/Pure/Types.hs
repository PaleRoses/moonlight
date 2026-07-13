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
    flatten,
    dedup,
    absorb,
    eliminateDoubleNegation,
    eliminateTrivial,
  )
where

import Data.Kind (Type)
import qualified Data.Set as Set
import Data.Set (Set)
import Moonlight.Algebra
  ( BooleanAlgebra (..),
    BoundedJoinSemilattice (..),
    BoundedMeetSemilattice (..),
    DistributiveLattice,
    HeytingAlgebra (..),
    JoinSemilattice (..),
    Lattice,
    MeetSemilattice (..),
  )
import Data.Functor.Foldable (Base)
import Data.Functor.Foldable (Corecursive (..), Recursive (..))

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
normalize expression =
  let normalized = normalizeStep expression
   in if normalized == expression
        then expression
        else normalize normalized

normalizeStep :: Ord a => ConstraintExpr a -> ConstraintExpr a
normalizeStep =
  eliminateTrivial
    . simplifyBooleanIdentities
    . absorb
    . dedup
    . flatten
    . eliminateDoubleNegation
    . normalizeChildren

normalizeChildren :: ConstraintExpr a -> ConstraintExpr a
normalizeChildren expression =
  case expression of
    Atom variable -> Atom variable
    Not inner -> Not (normalizeChildren inner)
    And children -> And (map normalizeChildren children)
    Or children -> Or (map normalizeChildren children)

flatten :: ConstraintExpr a -> ConstraintExpr a
flatten expression =
  case expression of
    Atom variable -> Atom variable
    Not inner -> Not (flatten inner)
    And children -> And (concatMap (flattenAnd . flatten) children)
    Or children -> Or (concatMap (flattenOr . flatten) children)

flattenAnd :: ConstraintExpr a -> [ConstraintExpr a]
flattenAnd expression =
  case expression of
    And nested -> nested
    other -> [other]

flattenOr :: ConstraintExpr a -> [ConstraintExpr a]
flattenOr expression =
  case expression of
    Or nested -> nested
    other -> [other]

dedup :: Ord a => ConstraintExpr a -> ConstraintExpr a
dedup expression =
  case expression of
    And children -> And (Set.toAscList (Set.fromList children))
    Or children -> Or (Set.toAscList (Set.fromList children))
    other -> other

absorb :: Ord a => ConstraintExpr a -> ConstraintExpr a
absorb expression =
  case expression of
    And children ->
      And (filter (not . isAbsorbedByAnd children) children)
    Or children ->
      Or (filter (not . isAbsorbedByOr children) children)
    other -> other

isAbsorbedByAnd :: Eq a => [ConstraintExpr a] -> ConstraintExpr a -> Bool
isAbsorbedByAnd siblings child =
  case child of
    Or orChildren -> any (`elem` siblings) orChildren
    _ -> False

isAbsorbedByOr :: Eq a => [ConstraintExpr a] -> ConstraintExpr a -> Bool
isAbsorbedByOr siblings child =
  case child of
    And andChildren -> any (`elem` siblings) andChildren
    _ -> False

eliminateDoubleNegation :: ConstraintExpr a -> ConstraintExpr a
eliminateDoubleNegation expression =
  case expression of
    Not (Not inner) -> eliminateDoubleNegation inner
    Not (And children) -> Or (map (eliminateDoubleNegation . Not) children)
    Not (Or children) -> And (map (eliminateDoubleNegation . Not) children)
    Not inner -> Not (eliminateDoubleNegation inner)
    And children -> And (map eliminateDoubleNegation children)
    Or children -> Or (map eliminateDoubleNegation children)
    Atom variable -> Atom variable

simplifyBooleanIdentities :: Eq a => ConstraintExpr a -> ConstraintExpr a
simplifyBooleanIdentities expression =
  case expression of
    And children
      | any isBottom children -> Or []
      | hasComplementaryPair children -> Or []
      | otherwise -> And (filter (not . isTop) children)
    Or children
      | any isTop children -> And []
      | hasComplementaryPair children -> And []
      | otherwise -> Or (filter (not . isBottom) children)
    other -> other

hasComplementaryPair :: Eq a => [ConstraintExpr a] -> Bool
hasComplementaryPair children =
  any (\child -> negateExpr child `elem` children) children

negateExpr :: ConstraintExpr a -> ConstraintExpr a
negateExpr expression =
  case expression of
    Not inner -> inner
    other -> Not other

isTop :: Eq a => ConstraintExpr a -> Bool
isTop expression = expression == And []

isBottom :: Eq a => ConstraintExpr a -> Bool
isBottom expression = expression == Or []

eliminateTrivial :: ConstraintExpr a -> ConstraintExpr a
eliminateTrivial expression =
  case expression of
    And [] -> And []
    And [single] -> single
    Or [] -> Or []
    Or [single] -> single
    other -> other

instance Ord a => JoinSemilattice (ConstraintExpr a) where
  join left right = normalize (Or [left, right])

instance Ord a => BoundedJoinSemilattice (ConstraintExpr a) where
  bottom = Or []

instance Ord a => MeetSemilattice (ConstraintExpr a) where
  meet left right = normalize (And [left, right])

instance Ord a => BoundedMeetSemilattice (ConstraintExpr a) where
  top = And []

instance Ord a => Lattice (ConstraintExpr a)

instance Ord a => DistributiveLattice (ConstraintExpr a)

instance Ord a => HeytingAlgebra (ConstraintExpr a) where
  implies left right = normalize (Or [Not left, right])

instance Ord a => BooleanAlgebra (ConstraintExpr a) where
  complement expression = normalize (Not expression)

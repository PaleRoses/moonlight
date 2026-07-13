module Moonlight.Constraint.Pure.CNF
  ( NNFExpr (..),
    toNNF,
    toCNF,
    exprToClauses,
  )
where

import Data.Kind (Type)
import qualified Data.Set as Set
import Moonlight.Constraint.Pure.Recursion
  ( ConstraintExprF (..),
    cataConstraintExpr,
  )
import Moonlight.Constraint.Pure.Types
  ( CNF,
    ConstraintExpr,
    Literal (..),
  )

type NNFExpr :: Type -> Type
data NNFExpr a
  = NAtom a
  | NAnd [NNFExpr a]
  | NOr [NNFExpr a]
  | NNegAtom a
  deriving stock (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

toNNF :: ConstraintExpr a -> NNFExpr a
toNNF = cataConstraintExpr algebra
  where
    algebra :: ConstraintExprF a (NNFExpr a) -> NNFExpr a
    algebra expressionLayer =
      case expressionLayer of
        AtomF variable -> NAtom variable
        AndF children -> NAnd children
        OrF children -> NOr children
        NotF inner -> negateNNF inner

negateNNF :: NNFExpr a -> NNFExpr a
negateNNF expression =
  case expression of
    NAtom variable -> NNegAtom variable
    NNegAtom variable -> NAtom variable
    NAnd children -> NOr (map negateNNF children)
    NOr children -> NAnd (map negateNNF children)

toCNF :: Ord a => ConstraintExpr a -> CNF a
toCNF = exprToClauses . toNNF

exprToClauses :: Ord a => NNFExpr a -> CNF a
exprToClauses expression =
  case expression of
    NAtom variable -> [Set.singleton (Pos variable)]
    NNegAtom variable -> [Set.singleton (Neg variable)]
    NAnd children -> concatMap exprToClauses children
    NOr children -> distributeOr (map exprToClauses children)

distributeOr :: Ord a => [CNF a] -> CNF a
distributeOr cnfGroups =
  case cnfGroups of
    [] -> [Set.empty]
    [single] -> single
    first : rest ->
      let distributed = distributeOr rest
       in [Set.union left right | left <- first, right <- distributed]

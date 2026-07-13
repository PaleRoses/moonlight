{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Type-indexed signature model for rewrite terms.
-- Owns sort witnesses, higher-kinded traversal over signatures, the erased
-- 'Node' wrapper, constructor tags, and node ordering.
-- Contracts: node comparison orders by result sort before tag and children,
-- and traversal preserves the signature-declared child sorts.
module Moonlight.Rewrite.DSL.Signature
  ( K (..),
    SortWitness (..),
    sortWitness,
    sortWitnessName,
    sortWitnessSortName,
    SomeSortWitness (..),
    someSortWitnessName,
    someSortWitnessSortName,
    sameSortWitness,
    Node (..),
    nodeSort,
    nodeChildren,
    HTraversable (..),
    hmap,
    RewriteSignature (..),
  )
where

import Control.Applicative (Const (..))
import Data.Type.Equality ((:~:) (..))
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Word (Word64)
import GHC.TypeLits (KnownSymbol, Symbol, sameSymbol, symbolVal)
import Moonlight.Core (HasConstructorTag (..))
import Moonlight.Rewrite.DSL.Term
  ( SortName,
    sortName,
    symbolToken,
  )

newtype K a (sort :: Symbol) = K
  { unK :: a
  }

data SortWitness (sort :: Symbol) where
  SortWitness :: KnownSymbol sort => SortWitness sort

sortWitness :: KnownSymbol sort => SortWitness sort
sortWitness =
  SortWitness

sortWitnessName :: forall sort. SortWitness sort -> String
sortWitnessName SortWitness =
  symbolVal (Proxy @sort)

sortWitnessSortName :: forall sort. SortWitness sort -> SortName
sortWitnessSortName SortWitness =
  sortName (symbolToken @sort)

data SomeSortWitness where
  SomeSortWitness :: !(SortWitness sort) -> SomeSortWitness

someSortWitnessName :: SomeSortWitness -> String
someSortWitnessName (SomeSortWitness witness) =
  sortWitnessName witness

someSortWitnessSortName :: SomeSortWitness -> SortName
someSortWitnessSortName (SomeSortWitness witness) =
  sortWitnessSortName witness

instance Eq SomeSortWitness where
  left == right =
    someSortWitnessName left == someSortWitnessName right

instance Ord SomeSortWitness where
  compare left right =
    compare (someSortWitnessName left) (someSortWitnessName right)

instance Show SomeSortWitness where
  showsPrec precedence witness =
    showParen (precedence > 10) $
      showString "SomeSortWitness " . showsPrec 11 (someSortWitnessName witness)

sameSortWitness :: forall left right. SortWitness left -> SortWitness right -> Maybe (left :~: right)
sameSortWitness SortWitness SortWitness =
  sameSymbol (Proxy @left) (Proxy @right)

class HTraversable sig where
  htraverseWithSort ::
    Applicative m =>
    (forall sort. SortWitness sort -> r sort -> m (q sort)) ->
    sig result r ->
    m (sig result q)

  htraverse ::
    Applicative m =>
    (forall sort. r sort -> m (q sort)) ->
    sig result r ->
    m (sig result q)
  htraverse transform =
    htraverseWithSort (\_ -> transform)

  hfoldMap ::
    Monoid m =>
    (forall sort. r sort -> m) ->
    sig result r ->
    m
  hfoldMap transform =
    getConst . htraverseWithSort (\_ value -> Const (transform value))

  {-# MINIMAL htraverseWithSort #-}

hmap :: HTraversable sig => (forall sort. r sort -> q sort) -> sig result r -> sig result q
hmap transform =
  runIdentity . htraverse (Identity . transform)

class HTraversable sig => RewriteSignature sig where
  type NodeTag sig :: Type

  nodeTag :: sig result r -> NodeTag sig

  nodeTagDigest :: Proxy sig -> NodeTag sig -> Word64

  nodeResultSort :: sig result r -> SortWitness result

data Node sig a where
  Node :: !(sig sort (K a)) -> Node sig a

nodeSort :: RewriteSignature sig => Node sig a -> SomeSortWitness
nodeSort (Node sigNode) =
  SomeSortWitness (nodeResultSort sigNode)

instance RewriteSignature sig => Functor (Node sig) where
  fmap transform (Node sigNode) =
    Node
      ( runIdentity
          ( htraverse
              (\(K value) -> Identity (K (transform value)))
              sigNode
          )
      )

instance RewriteSignature sig => Foldable (Node sig) where
  foldMap transform (Node sigNode) =
    hfoldMap (\(K value) -> transform value) sigNode

instance RewriteSignature sig => Traversable (Node sig) where
  traverse transform (Node sigNode) =
    Node
      <$> htraverse
        (\(K value) -> K <$> transform value)
        sigNode

instance (RewriteSignature sig, Ord (NodeTag sig), Ord a) => Eq (Node sig a) where
  leftNode == rightNode =
    compare leftNode rightNode == EQ

instance (RewriteSignature sig, Ord (NodeTag sig), Ord a) => Ord (Node sig a) where
  compare (Node leftNode) (Node rightNode) =
    compare
      (SomeSortWitness (nodeResultSort leftNode))
      (SomeSortWitness (nodeResultSort rightNode))
      <> compare (nodeTag leftNode) (nodeTag rightNode)
      <> compare (nodeChildren leftNode) (nodeChildren rightNode)

instance (RewriteSignature sig, Show (NodeTag sig), Show a) => Show (Node sig a) where
  showsPrec precedence (Node sigNode) =
    showParen (precedence > 10) $
      showString "Node "
        . showsPrec 11 (nodeTag sigNode)
        . showChar ' '
        . showsPrec 11 (nodeChildren sigNode)

instance (RewriteSignature sig, Ord (NodeTag sig)) => HasConstructorTag (Node sig) where
  type ConstructorTag (Node sig) = NodeTag sig

  constructorTag (Node sigNode) =
    nodeTag sigNode

nodeChildren :: HTraversable sig => sig result (K a) -> [a]
nodeChildren =
  getConst . htraverse (\(K value) -> Const [value])

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.EGraph.Test.Front.Mono
  ( MonoSig,
    monoNode,
    monoFix,
    monoExtractTerm,
    monoAnalysisSpec,
    monoCostAlgebra,
    monoAnalysisCostAlgebra,
    monoTheorySpec,
  )
where

import Moonlight.Core (ZipMatch (..))
import Data.Hashable (hash)
import Data.Kind (Type)
import GHC.TypeLits (Symbol)
import Moonlight.Core (StructuralLaw (..), TheorySpec (..))
import Moonlight.EGraph.Pure.Analysis (AnalysisSpec (..))
import Moonlight.EGraph.Pure.Extraction (AnalysisCostAlgebra (..), CostAlgebra (..))
import Moonlight.EGraph.Pure.Saturation.Front (Term, node)
import Data.Fix (Fix (..))
import Moonlight.Rewrite.DSL
  ( HTraversable (..),
    K (..),
    Node (..),
    RewriteSignature (..),
    SortWitness (..),
  )

-- | One-sort adapter for legacy test functors.  The Front remains the authoring
-- owner; this only prevents each fixture from rebuilding the same sorted GADT by
-- hand like a penitent carving spoons in a dungeon.
type MonoSig :: (Type -> Type) -> Symbol -> (Symbol -> Type) -> Type
data MonoSig f result r where
  MonoNode :: f (r "Expr") -> MonoSig f "Expr" r

instance Traversable f => HTraversable (MonoSig f) where
  htraverseWithSort transform =
    \case
      MonoNode layer ->
        MonoNode <$> traverse (transform SortWitness) layer

instance (Traversable f, Show (f ())) => RewriteSignature (MonoSig f) where
  type NodeTag (MonoSig f) = f ()

  nodeTag =
    \case
      MonoNode layer -> () <$ layer

  nodeTagDigest _ =
    fromIntegral . hash . show

  nodeResultSort =
    \case
      MonoNode {} -> SortWitness

instance (ZipMatch f, Show (f ())) => ZipMatch (Node (MonoSig f)) where
  zipMatch leftNode rightNode =
    case (leftNode, rightNode) of
      (Node (MonoNode leftLayer), Node (MonoNode rightLayer)) ->
        Node . MonoNode . fmap adaptMonoZipChild <$> zipMatch leftLayer rightLayer

monoNode :: f (Term (MonoSig f) "Expr") -> Term (MonoSig f) "Expr"
monoNode =
  node . MonoNode

monoFix :: Functor f => Fix f -> Term (MonoSig f) "Expr"
monoFix (Fix layer) =
  monoNode (fmap monoFix layer)

monoExtractTerm :: Functor f => Fix (Node (MonoSig f)) -> Fix f
monoExtractTerm (Fix (Node (MonoNode layer))) =
  Fix (fmap (monoExtractTerm . unK) layer)

monoAnalysisSpec :: Functor f => AnalysisSpec f analysis -> AnalysisSpec (Node (MonoSig f)) analysis
monoAnalysisSpec source =
  AnalysisSpec
    { asMake =
        \case
          Node (MonoNode layer) -> asMake source (fmap unK layer),
      asJoin = asJoin source,
      asJoinChanged = asJoinChanged source
    }

monoCostAlgebra :: Functor f => CostAlgebra f cost -> AnalysisCostAlgebra (Node (MonoSig f)) analysis cost
monoCostAlgebra (CostAlgebra computeCost) =
  AnalysisCostAlgebra $ \_analysis ->
    \case
      Node (MonoNode layer) -> computeCost (fmap (snd . unK) layer)

monoAnalysisCostAlgebra :: Functor f => AnalysisCostAlgebra f analysis cost -> AnalysisCostAlgebra (Node (MonoSig f)) analysis cost
monoAnalysisCostAlgebra (AnalysisCostAlgebra computeCost) =
  AnalysisCostAlgebra $ \analysis ->
    \case
      Node (MonoNode layer) -> computeCost analysis (fmap unK layer)

monoTheorySpec :: Functor f => TheorySpec f -> TheorySpec (Node (MonoSig f))
monoTheorySpec source =
  TheorySpec
    { tsClassify =
        \case
          Node (MonoNode layer) ->
            case tsClassify source (fmap unK layer) of
              Ordinary -> Ordinary
              CommutativeBinary law ->
                CommutativeBinary $ \left right ->
                  Node (MonoNode (fmap K (law left right)))
    }

adaptMonoZipChild :: (K left sort, K right sort) -> K (left, right) sort
adaptMonoZipChild (leftChild, rightChild) =
  K (unK leftChild, unK rightChild)

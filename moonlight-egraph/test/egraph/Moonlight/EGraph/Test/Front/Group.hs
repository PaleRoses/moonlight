{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.EGraph.Test.Front.Group
  ( GroupSig,
    gI,
    gA,
    gB,
    gMul,
    gInv,
    assertGroupEquivalent,
  )
where

import Moonlight.EGraph.Effect.CoveringSurface (SurfaceKind)
import Moonlight.Core (ZipMatch (..))
import GHC.TypeLits (Symbol)
import Moonlight.Algebra (JoinSemilattice (..))
import Moonlight.FiniteLattice (singletonContextLattice)
import Moonlight.Core (emptyTheorySpec)
import Moonlight.EGraph.Pure.Analysis (AnalysisSpec, semilatticeAnalysis)
import Moonlight.EGraph.Pure.Context (withEmptyContextEGraph)
import Moonlight.EGraph.Pure.Saturation.Front
import Moonlight.EGraph.Pure.Saturation.Front.PackedNode
  ( PackedNode,
    packAnalysisSpec,
  )
import Moonlight.EGraph.Pure.Types (emptyEGraphWithTheory)
import Moonlight.EGraph.Saturation.Context.State
  ( SaturatingContextEGraph,
    emptySaturatingContextEGraph,
  )
import Moonlight.Rewrite.DSL
  ( HTraversable (..),
    K (..),
    Node (..),
    RewriteSignature (..),
    SortWitness (..),
  )
import Test.Tasty.HUnit (Assertion, assertFailure, (@?=))

assertGroupEquivalent :: Term GroupSig "Group" -> Term GroupSig "Group" -> Assertion
assertGroupEquivalent left right =
  withEmptyGroupGraph $ \emptyGroupGraph -> do
    report <- expectFront (runEGraphFront (equivalenceProgram left right) emptyGroupGraph)
    efrResult report @?= True

equivalenceProgram :: Term GroupSig "Group" -> Term GroupSig "Group" -> EGraphFront 'Authored owner GroupSig GroupDepth GroupContext Bool
equivalenceProgram left right =
  egraph $ do
    group <- ruleset @"group" groupRules
    lhs <- def @"lhs" left

    run $
      runUntil (lhs === right) $
        runFor groupBudget group

    check @"equivalent" (lhs === right)

groupBudget :: SaturationBudget
groupBudget =
  SaturationBudget
    { sbMaxIterations = 10000,
      sbMaxNodes = 100000
    }

groupRules :: RulesetM GroupSig ()
groupRules = do
  birewrite @"assoc" (gMul (gMul #x #y) #z) (gMul #x (gMul #y #z))
  rewrite @"id-left" $
    gMul gI #x ==> #x
  rewrite @"id-right" $
    gMul #x gI ==> #x
  rewrite @"inv-left" $
    gMul (gInv #x) #x ==> gI
  rewrite @"inv-right" $
    gMul #x (gInv #x) ==> gI
  rewrite @"a-cyclic-4" $
    gMul gA (gMul gA (gMul gA gA)) ==> gI

data GroupContext = GroupContext
  deriving stock (Eq, Ord, Show)

data GroupTag
  = GIdentityTag
  | GATag
  | GBTag
  | GMulTag
  | GInvTag
  deriving stock (Eq, Ord, Show)

data GroupSig (result :: Symbol) r where
  GIdentityNode :: GroupSig "Group" r
  GANode :: GroupSig "Group" r
  GBNode :: GroupSig "Group" r
  GMulNode :: r "Group" -> r "Group" -> GroupSig "Group" r
  GInvNode :: r "Group" -> GroupSig "Group" r

instance HTraversable GroupSig where
  htraverseWithSort transform =
    \case
      GIdentityNode ->
        pure GIdentityNode
      GANode ->
        pure GANode
      GBNode ->
        pure GBNode
      GMulNode left right ->
        GMulNode
          <$> transform SortWitness left
          <*> transform SortWitness right
      GInvNode term ->
        GInvNode <$> transform SortWitness term

instance RewriteSignature GroupSig where
  type NodeTag GroupSig = GroupTag

  nodeTag =
    \case
      GIdentityNode -> GIdentityTag
      GANode -> GATag
      GBNode -> GBTag
      GMulNode {} -> GMulTag
      GInvNode {} -> GInvTag

  nodeTagDigest _ =
    \case
      GIdentityTag -> 0
      GATag -> 1
      GBTag -> 2
      GMulTag -> 3
      GInvTag -> 4

  nodeResultSort =
    \case
      GIdentityNode -> SortWitness
      GANode -> SortWitness
      GBNode -> SortWitness
      GMulNode {} -> SortWitness
      GInvNode {} -> SortWitness

instance ZipMatch (Node GroupSig) where
  zipMatch leftNode rightNode =
    case (leftNode, rightNode) of
      (Node GIdentityNode, Node GIdentityNode) ->
        Just (Node GIdentityNode)
      (Node GANode, Node GANode) ->
        Just (Node GANode)
      (Node GBNode, Node GBNode) ->
        Just (Node GBNode)
      (Node (GMulNode leftA rightA), Node (GMulNode leftB rightB)) ->
        Just (Node (GMulNode (zipChild leftA leftB) (zipChild rightA rightB)))
      (Node (GInvNode termA), Node (GInvNode termB)) ->
        Just (Node (GInvNode (zipChild termA termB)))
      _ ->
        Nothing
    where
      zipChild :: K left sort -> K right sort -> K (left, right) sort
      zipChild leftChild rightChild =
        K (unK leftChild, unK rightChild)

newtype GroupDepth = GroupDepth Int
  deriving stock (Eq, Ord, Show)

instance JoinSemilattice GroupDepth where
  join (GroupDepth left) (GroupDepth right) =
    GroupDepth (max left right)

withEmptyGroupGraph ::
  (forall owner. SaturatingContextEGraph owner SurfaceKind (PackedNode GroupSig) GroupDepth GroupContext -> result) ->
  result
withEmptyGroupGraph useGraph =
  withEmptyContextEGraph
    (singletonContextLattice GroupContext)
    (emptyEGraphWithTheory (packAnalysisSpec groupAnalysis) emptyTheorySpec)
    (useGraph . emptySaturatingContextEGraph)

groupAnalysis :: AnalysisSpec (Node GroupSig) GroupDepth
groupAnalysis =
  semilatticeAnalysis $
    \case
      Node GIdentityNode -> GroupDepth 0
      Node GANode -> GroupDepth 0
      Node GBNode -> GroupDepth 0
      Node (GMulNode (K (GroupDepth left)) (K (GroupDepth right))) ->
        GroupDepth (max left right + 1)
      Node (GInvNode (K (GroupDepth term))) ->
        GroupDepth (term + 1)

gI :: Term GroupSig "Group"
gI =
  node GIdentityNode

gA :: Term GroupSig "Group"
gA =
  node GANode

gB :: Term GroupSig "Group"
gB =
  node GBNode

gMul :: Term GroupSig "Group" -> Term GroupSig "Group" -> Term GroupSig "Group"
gMul left right =
  node (GMulNode left right)

gInv :: Term GroupSig "Group" -> Term GroupSig "Group"
gInv term =
  node (GInvNode term)

expectFront :: Either (EGraphFrontError owner GroupSig GroupDepth GroupContext) value -> IO value
expectFront =
  \case
    Right value -> pure value
    Left err -> assertFailure (frontErrorMessage err)

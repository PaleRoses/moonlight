{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.EGraph.Test.Front.Tiny
  ( FrontTinyContext (..),
    FrontTinySig,
    FrontTinyView (..),
    NodeCount (..),
    defaultBudget,
    emptyFrontGraph,
    contextualFrontGraph,
    termSize,
    viewFrontTinyTerm,
    expectFront,
    expectCompiled,
    simpleArithmeticRules,
    num,
    zero,
    one,
    sym,
    add,
    mul,
  )
where

import Moonlight.EGraph.Effect.CoveringSurface (SurfaceKind)
import Moonlight.Core (ZipMatch (..))
import GHC.TypeLits (Symbol)
import Moonlight.Algebra
  ( BoundedJoinSemilattice (..),
    BoundedMeetSemilattice (..),
    JoinSemilattice (..),
    Lattice,
    MeetSemilattice (..)
  )
import Moonlight.Core (emptyTheorySpec)
import Moonlight.EGraph.Pure.Analysis (AnalysisSpec, semilatticeAnalysis)
import Moonlight.EGraph.Pure.Context (emptyContextEGraph)
import Moonlight.EGraph.Pure.Extraction (AnalysisCostAlgebra (..))
import Moonlight.EGraph.Pure.Saturation.Front
  ( EGraphFront,
    EGraphFrontError,
    FrontPhase (Authored),
    RulesetM,
    SaturationBudget (..),
    Term,
    compileEGraphFront,
    frontErrorMessage,
    node,
    rewrite,
    (==>),
  )
import Moonlight.EGraph.Pure.Saturation.Front.PackedNode
  ( PackedNode,
    packAnalysisSpec,
  )
import Moonlight.EGraph.Pure.Types (emptyEGraphWithTheory)
import Moonlight.EGraph.Saturation.Context.State
  ( SaturatingContextEGraph,
    emptySaturatingContextEGraph,
  )
import Data.Fix (Fix (..))
import Moonlight.Rewrite.DSL
  ( HTraversable (..),
    K (..),
    Node (..),
    RewriteSignature (..),
    SortWitness (..),
  )
import Test.Tasty.HUnit
  ( Assertion,
    assertFailure,
  )
import Moonlight.FiniteLattice
  ( ContextLattice,
    latticeContext,
    singletonContextLattice
  )

data FrontTinyContext
  = BaseOnly
  | Rain
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance JoinSemilattice FrontTinyContext where
  join BaseOnly contextValue =
    contextValue
  join Rain _ =
    Rain

instance MeetSemilattice FrontTinyContext where
  meet Rain contextValue =
    contextValue
  meet BaseOnly _ =
    BaseOnly

instance Lattice FrontTinyContext

instance BoundedJoinSemilattice FrontTinyContext where
  bottom =
    BaseOnly

instance BoundedMeetSemilattice FrontTinyContext where
  top =
    Rain

data FrontTinyTag
  = NumTag Int
  | SymTag String
  | AddTag
  | MulTag
  deriving stock (Eq, Ord, Show)

data FrontTinySig (result :: Symbol) r where
  NumNode :: Int -> FrontTinySig "Expr" r
  SymNode :: String -> FrontTinySig "Expr" r
  AddNode :: r "Expr" -> r "Expr" -> FrontTinySig "Expr" r
  MulNode :: r "Expr" -> r "Expr" -> FrontTinySig "Expr" r

instance HTraversable FrontTinySig where
  htraverseWithSort transform =
    \case
      NumNode value ->
        pure (NumNode value)
      SymNode name ->
        pure (SymNode name)
      AddNode left right ->
        AddNode
          <$> transform SortWitness left
          <*> transform SortWitness right
      MulNode left right ->
        MulNode
          <$> transform SortWitness left
          <*> transform SortWitness right

instance RewriteSignature FrontTinySig where
  type NodeTag FrontTinySig = FrontTinyTag

  nodeTag =
    \case
      NumNode value -> NumTag value
      SymNode name -> SymTag name
      AddNode {} -> AddTag
      MulNode {} -> MulTag

  nodeTagDigest _ =
    \case
      NumTag value -> fromIntegral (1000 + value)
      SymTag _ -> 3
      AddTag -> 4
      MulTag -> 5

  nodeResultSort =
    \case
      NumNode {} -> SortWitness
      SymNode {} -> SortWitness
      AddNode {} -> SortWitness
      MulNode {} -> SortWitness

instance ZipMatch (Node FrontTinySig) where
  zipMatch leftNode rightNode =
    case (leftNode, rightNode) of
      (Node (NumNode leftValue), Node (NumNode rightValue))
        | leftValue == rightValue ->
            Just (Node (NumNode leftValue))
      (Node (SymNode leftName), Node (SymNode rightName))
        | leftName == rightName ->
            Just (Node (SymNode leftName))
      (Node (AddNode leftA rightA), Node (AddNode leftB rightB)) ->
        Just (Node (AddNode (zipChild leftA leftB) (zipChild rightA rightB)))
      (Node (MulNode leftA rightA), Node (MulNode leftB rightB)) ->
        Just (Node (MulNode (zipChild leftA leftB) (zipChild rightA rightB)))
      _ ->
        Nothing
    where
      zipChild :: K child sortLeft -> K child sortRight -> K (child, child) sortResult
      zipChild leftChild rightChild =
        K (unK leftChild, unK rightChild)

data FrontTinyView
  = NumView Int
  | SymView String
  | AddView FrontTinyView FrontTinyView
  | MulView FrontTinyView FrontTinyView
  deriving stock (Eq, Ord, Show)

newtype NodeCount = NodeCount Int
  deriving stock (Eq, Ord, Show)

instance JoinSemilattice NodeCount where
  join (NodeCount left) (NodeCount right) =
    NodeCount (max left right)

defaultBudget :: SaturationBudget
defaultBudget =
  SaturationBudget
    { sbMaxIterations = 20,
      sbMaxNodes = 10000
    }

emptyFrontGraph :: SaturatingContextEGraph SurfaceKind (PackedNode FrontTinySig) NodeCount FrontTinyContext
emptyFrontGraph =
  emptySaturatingContextEGraph $
    emptyContextEGraph (singletonContextLattice BaseOnly) $
      emptyEGraphWithTheory (packAnalysisSpec frontTinyAnalysis) emptyTheorySpec

contextualFrontGraph :: SaturatingContextEGraph SurfaceKind (PackedNode FrontTinySig) NodeCount FrontTinyContext
contextualFrontGraph =
  emptySaturatingContextEGraph $
    emptyContextEGraph frontTinyContextLattice $
      emptyEGraphWithTheory (packAnalysisSpec frontTinyAnalysis) emptyTheorySpec

frontTinyContextLattice :: ContextLattice FrontTinyContext
frontTinyContextLattice =
  case latticeContext of
    Right latticeValue -> latticeValue
    Left compileError ->
      error ("invalid FrontTinyContext lattice fixture: " <> show compileError)

frontTinyAnalysis :: AnalysisSpec (Node FrontTinySig) NodeCount
frontTinyAnalysis =
  semilatticeAnalysis $
    \case
      Node NumNode {} -> NodeCount 1
      Node SymNode {} -> NodeCount 1
      Node (AddNode (K (NodeCount left)) (K (NodeCount right))) ->
        NodeCount (left + right + 1)
      Node (MulNode (K (NodeCount left)) (K (NodeCount right))) ->
        NodeCount (left + right + 1)

termSize :: AnalysisCostAlgebra (Node FrontTinySig) NodeCount Int
termSize =
  AnalysisCostAlgebra $
    \_analysis ->
      \case
        Node NumNode {} -> 1
        Node SymNode {} -> 1
        Node (AddNode (K (_, left)) (K (_, right))) -> left + right + 1
        Node (MulNode (K (_, left)) (K (_, right))) -> left + right + 1

num :: Int -> Term FrontTinySig "Expr"
num value =
  node (NumNode value)

zero :: Term FrontTinySig "Expr"
zero =
  num 0

one :: Term FrontTinySig "Expr"
one =
  num 1

sym :: String -> Term FrontTinySig "Expr"
sym name =
  node (SymNode name)

add :: Term FrontTinySig "Expr" -> Term FrontTinySig "Expr" -> Term FrontTinySig "Expr"
add left right =
  node (AddNode left right)

mul :: Term FrontTinySig "Expr" -> Term FrontTinySig "Expr" -> Term FrontTinySig "Expr"
mul left right =
  node (MulNode left right)

simpleArithmeticRules :: RulesetM FrontTinySig ()
simpleArithmeticRules = do
  rewrite @"add-zero-right" $
    add #x zero ==> #x
  rewrite @"add-zero-left" $
    add zero #x ==> #x
  rewrite @"mul-zero-right" $
    mul #x zero ==> zero
  rewrite @"mul-zero-left" $
    mul zero #x ==> zero
  rewrite @"mul-one-right" $
    mul #x one ==> #x
  rewrite @"mul-one-left" $
    mul one #x ==> #x

viewFrontTinyTerm :: Fix (Node FrontTinySig) -> FrontTinyView
viewFrontTinyTerm (Fix (Node sigNode)) =
  case sigNode of
    NumNode value -> NumView value
    SymNode name -> SymView name
    AddNode (K left) (K right) ->
      AddView (viewFrontTinyTerm left) (viewFrontTinyTerm right)
    MulNode (K left) (K right) ->
      MulView (viewFrontTinyTerm left) (viewFrontTinyTerm right)

expectFront :: Either (EGraphFrontError FrontTinySig NodeCount FrontTinyContext) value -> IO value
expectFront =
  \case
    Right value -> pure value
    Left err -> assertFailure (frontErrorMessage err)

expectCompiled :: EGraphFront 'Authored FrontTinySig NodeCount FrontTinyContext result -> Assertion
expectCompiled frontValue =
  case compileEGraphFront frontValue of
    Right _ -> pure ()
    Left err -> assertFailure (frontErrorMessage err)

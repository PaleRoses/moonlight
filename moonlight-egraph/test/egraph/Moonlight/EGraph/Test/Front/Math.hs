{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.EGraph.Test.Front.Math
  ( MathSig,
    MathDepth (..),
    emptyMathGraph,
    mathBudget,
    mathCost,
    mathRules,
    assertMathEquivalent,
    mAdd,
    mMul,
    mSub,
    mDiv,
    mPow,
    mDiff,
    mInteg,
    mLn,
    mSin,
    mCos,
    mConst,
    mSym,
  )
where

import Moonlight.EGraph.Effect.CoveringSurface (SurfaceKind)
import Moonlight.Core (ZipMatch (..))
import Moonlight.Algebra (JoinSemilattice (..))
import Moonlight.FiniteLattice (singletonContextLattice)
import Moonlight.Core (ConstructorTag, HasConstructorTag (..), zipSameNodeShape)
import Moonlight.Core (StructuralLaw (..), TheorySpec (..), commutativeBinary)
import Moonlight.EGraph.Pure.Analysis (AnalysisSpec, semilatticeAnalysis)
import Moonlight.EGraph.Pure.Context (emptyContextEGraph)
import Moonlight.EGraph.Pure.Extraction (AnalysisCostAlgebra, CostAlgebra (..))
import Moonlight.EGraph.Pure.Saturation.Front
import Moonlight.EGraph.Pure.Saturation.Front.PackedNode
  ( PackedNode,
    packAnalysisSpec,
    packTheorySpec,
  )
import Moonlight.EGraph.Pure.Types (emptyEGraphWithTheory)
import Moonlight.EGraph.Saturation.Context.State
  ( SaturatingContextEGraph,
    emptySaturatingContextEGraph,
  )
import Moonlight.Rewrite.DSL (Node)
import Moonlight.EGraph.Test.Front.Mono
import Test.Tasty.HUnit (Assertion, assertFailure, (@?=))

type MathSig = MonoSig MathF

data MathF a
  = MDiff a a
  | MIntegral a a
  | MAdd a a
  | MSub a a
  | MMul a a
  | MDiv a a
  | MPow a a
  | MLn a
  | MSqrt a
  | MSin a
  | MCos a
  | MConst Int
  | MSymbol String
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

data MathTag
  = MDiffTag
  | MIntegralTag
  | MAddTag
  | MSubTag
  | MMulTag
  | MDivTag
  | MPowTag
  | MLnTag
  | MSqrtTag
  | MSinTag
  | MCosTag
  | MConstTag Int
  | MSymbolTag String
  deriving stock (Eq, Ord, Show)

instance HasConstructorTag MathF where
  type ConstructorTag MathF = MathTag

  constructorTag =
    \case
      MDiff {} -> MDiffTag
      MIntegral {} -> MIntegralTag
      MAdd {} -> MAddTag
      MSub {} -> MSubTag
      MMul {} -> MMulTag
      MDiv {} -> MDivTag
      MPow {} -> MPowTag
      MLn {} -> MLnTag
      MSqrt {} -> MSqrtTag
      MSin {} -> MSinTag
      MCos {} -> MCosTag
      MConst value -> MConstTag value
      MSymbol name -> MSymbolTag name

instance ZipMatch MathF where
  zipMatch =
    zipSameNodeShape

newtype MathDepth = MathDepth Int
  deriving stock (Eq, Ord, Show)

instance JoinSemilattice MathDepth where
  join (MathDepth left) (MathDepth right) =
    MathDepth (max left right)

mathAnalysisSpec :: AnalysisSpec MathF MathDepth
mathAnalysisSpec =
  semilatticeAnalysis $ \case
    MConst _ -> MathDepth 0
    MSymbol _ -> MathDepth 0
    MLn (MathDepth value) -> MathDepth (value + 1)
    MSqrt (MathDepth value) -> MathDepth (value + 1)
    MSin (MathDepth value) -> MathDepth (value + 1)
    MCos (MathDepth value) -> MathDepth (value + 1)
    MAdd (MathDepth left) (MathDepth right) -> MathDepth (max left right + 1)
    MSub (MathDepth left) (MathDepth right) -> MathDepth (max left right + 1)
    MMul (MathDepth left) (MathDepth right) -> MathDepth (max left right + 1)
    MDiv (MathDepth left) (MathDepth right) -> MathDepth (max left right + 1)
    MPow (MathDepth left) (MathDepth right) -> MathDepth (max left right + 1)
    MDiff (MathDepth left) (MathDepth right) -> MathDepth (max left right + 100)
    MIntegral (MathDepth left) (MathDepth right) -> MathDepth (max left right + 100)

mathCostSource :: CostAlgebra MathF Int
mathCostSource =
  CostAlgebra $ \case
    MConst _ -> 1
    MSymbol _ -> 1
    MLn value -> value + 1
    MSqrt value -> value + 1
    MSin value -> value + 1
    MCos value -> value + 1
    MAdd left right -> left + right + 1
    MSub left right -> left + right + 1
    MMul left right -> left + right + 1
    MDiv left right -> left + right + 1
    MPow left right -> left + right + 1
    MDiff left right -> left + right + 100
    MIntegral left right -> left + right + 100

mathCost :: AnalysisCostAlgebra (Node MathSig) MathDepth Int
mathCost =
  monoCostAlgebra mathCostSource

mathTheorySpec :: TheorySpec MathF
mathTheorySpec =
  TheorySpec
    { tsClassify = \case
        MAdd _ _ -> commutativeBinary MAdd
        MMul _ _ -> commutativeBinary MMul
        _ -> Ordinary
    }

emptyMathGraph :: SaturatingContextEGraph SurfaceKind (PackedNode MathSig) MathDepth ()
emptyMathGraph =
  emptySaturatingContextEGraph $
    emptyContextEGraph (singletonContextLattice ()) $
      emptyEGraphWithTheory (packAnalysisSpec (monoAnalysisSpec mathAnalysisSpec)) (packTheorySpec (monoTheorySpec mathTheorySpec))

mathBudget :: SaturationBudget
mathBudget =
  SaturationBudget
    { sbMaxIterations = 10000,
      sbMaxNodes = 100000
    }

mAdd :: Term MathSig "Expr" -> Term MathSig "Expr" -> Term MathSig "Expr"
mAdd left right =
  monoNode (MAdd left right)

mMul :: Term MathSig "Expr" -> Term MathSig "Expr" -> Term MathSig "Expr"
mMul left right =
  monoNode (MMul left right)

mSub :: Term MathSig "Expr" -> Term MathSig "Expr" -> Term MathSig "Expr"
mSub left right =
  monoNode (MSub left right)

mDiv :: Term MathSig "Expr" -> Term MathSig "Expr" -> Term MathSig "Expr"
mDiv left right =
  monoNode (MDiv left right)

mPow :: Term MathSig "Expr" -> Term MathSig "Expr" -> Term MathSig "Expr"
mPow left right =
  monoNode (MPow left right)

mDiff :: Term MathSig "Expr" -> Term MathSig "Expr" -> Term MathSig "Expr"
mDiff left right =
  monoNode (MDiff left right)

mInteg :: Term MathSig "Expr" -> Term MathSig "Expr" -> Term MathSig "Expr"
mInteg left right =
  monoNode (MIntegral left right)

mLn :: Term MathSig "Expr" -> Term MathSig "Expr"
mLn term =
  monoNode (MLn term)

mSin :: Term MathSig "Expr" -> Term MathSig "Expr"
mSin term =
  monoNode (MSin term)

mCos :: Term MathSig "Expr" -> Term MathSig "Expr"
mCos term =
  monoNode (MCos term)

mConst :: Int -> Term MathSig "Expr"
mConst value =
  monoNode (MConst value)

mSym :: String -> Term MathSig "Expr"
mSym name =
  monoNode (MSymbol name)

mathRules :: RulesetM MathSig ()
mathRules = do
  rewrite @"assoc-add" $
    mAdd #a (mAdd #b #c) ==> mAdd (mAdd #a #b) #c
  rewrite @"assoc-mul" $
    mMul #a (mMul #b #c) ==> mMul (mMul #a #b) #c
  rewrite @"sub-canon" $
    mSub #a #b ==> mAdd #a (mMul (mConst (-1)) #b)
  rewrite @"zero-add" $
    mAdd #a (mConst 0) ==> #a
  rewrite @"zero-mul" $
    mMul #a (mConst 0) ==> mConst 0
  rewrite @"one-mul" $
    mMul #a (mConst 1) ==> #a
  rewrite @"cancel-sub" $
    mSub #a #a ==> mConst 0
  rewrite @"distribute" $
    mMul #a (mAdd #b #c) ==> mAdd (mMul #a #b) (mMul #a #c)
  rewrite @"factor" $
    mAdd (mMul #a #b) (mMul #a #c) ==> mMul #a (mAdd #b #c)
  rewrite @"pow-mul" $
    mMul (mPow #a #b) (mPow #a #c) ==> mPow #a (mAdd #b #c)
  rewrite @"pow1" $
    mPow #x (mConst 1) ==> #x
  rewrite @"pow2" $
    mPow #x (mConst 2) ==> mMul #x #x
  rewrite @"d-variable" $
    mDiff #x #x ==> mConst 1
  rewrite @"d-add" $
    mDiff #x (mAdd #a #b) ==> mAdd (mDiff #x #a) (mDiff #x #b)
  rewrite @"d-mul" $
    mDiff #x (mMul #a #b) ==> mAdd (mMul #a (mDiff #x #b)) (mMul #b (mDiff #x #a))
  rewrite @"d-sin" $
    mDiff #x (mSin #x) ==> mCos #x
  rewrite @"d-cos" $
    mDiff #x (mCos #x) ==> mMul (mConst (-1)) (mSin #x)
  rewrite @"d-ln" $
    mDiff #x (mLn #x) ==> mDiv (mConst 1) #x
  rewrite @"i-one" $
    mInteg (mConst 1) #x ==> #x
  rewrite @"i-cos" $
    mInteg (mCos #x) #x ==> mSin #x
  rewrite @"i-sin" $
    mInteg (mSin #x) #x ==> mMul (mConst (-1)) (mCos #x)
  rewrite @"i-sum" $
    mInteg (mAdd #a #b) #x ==> mAdd (mInteg #a #x) (mInteg #b #x)
  rewrite @"i-dif" $
    mInteg (mSub #a #b) #x ==> mSub (mInteg #a #x) (mInteg #b #x)
  rewrite @"i-parts" $
    mInteg (mMul #a #b) #x ==> mSub (mMul #a (mInteg #b #x)) (mInteg (mMul (mDiff #x #a) (mInteg #b #x)) #x)

assertMathEquivalent :: Term MathSig "Expr" -> Term MathSig "Expr" -> Assertion
assertMathEquivalent left right = do
  report <- expectFront (runEGraphFront (equivalenceProgram left right) emptyMathGraph)
  efrResult report @?= True

equivalenceProgram :: Term MathSig "Expr" -> Term MathSig "Expr" -> EGraphFront 'Authored MathSig MathDepth () Bool
equivalenceProgram left right =
  egraph $ do
    math <- ruleset @"math" mathRules
    lhs <- def @"lhs" left

    run $
      runUntil (lhs === right) $
        runFor mathBudget math

    check @"equivalent" (lhs === right)

expectFront :: Either (EGraphFrontError MathSig MathDepth ()) value -> IO value
expectFront =
  \case
    Right value -> pure value
    Left err -> assertFailure (frontErrorMessage err)

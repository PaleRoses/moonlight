{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.EGraph.Introspection.Arbitrary
  ( ArithF (..),
    GeneratedContextPair (..),
    GeneratedRewriteSystem (..),
  )
where

import Data.Kind (Type)
import Data.Set qualified as Set
import Moonlight.Core (ZipMatch (..), HasConstructorTag (..), Pattern (..), patternVariables, zipSameNodeShape)
import Moonlight.Core qualified as EGraph
import Moonlight.EGraph.Introspection.Core.Rewrite (RewriteContext, RewriteMorphism, RewriteSystem, mkRewriteContext, mkRewriteSystem, rewriteMorphismLeft, rewriteMorphismName, rewriteMorphismRight, rewriteMorphismWithInterface)
import Test.Tasty.QuickCheck qualified as QC

type ArithF :: Type -> Type
data ArithF a
  = Num Int
  | Add a a
  | Var Int
  | Mul a a
  | Neg a
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

type ArithTag :: Type
data ArithTag
  = NumTag Int
  | AddTag
  | VarTag Int
  | MulTag
  | NegTag
  deriving stock (Eq, Ord, Show)

instance HasConstructorTag ArithF where
  type ConstructorTag ArithF = ArithTag

  constructorTag arithNode =
    case arithNode of
      Num value -> NumTag value
      Add {} -> AddTag
      Var v -> VarTag v
      Mul {} -> MulTag
      Neg {} -> NegTag

instance ZipMatch ArithF where
  zipMatch = zipSameNodeShape

type GeneratedRewriteSystem :: Type
data GeneratedRewriteSystem = GeneratedRewriteSystem
  { grsSpans :: [RewriteMorphism ArithF],
    grsSystem :: RewriteSystem ArithF
  }

type GeneratedContextPair :: Type
data GeneratedContextPair = GeneratedContextPair
  { gcpLeftObjects :: [Pattern ArithF],
    gcpRightObjects :: [Pattern ArithF],
    gcpLeftContext :: RewriteContext ArithF,
    gcpRightContext :: RewriteContext ArithF
  }

instance Show GeneratedRewriteSystem where
  show = show . grsSpans

instance Show GeneratedContextPair where
  show contextPair =
    show (gcpLeftObjects contextPair, gcpRightObjects contextPair)

instance QC.Arbitrary (Pattern ArithF) where
  arbitrary = QC.sized arbitraryPatternSized
  shrink = shrinkPattern

instance QC.Arbitrary (RewriteMorphism ArithF) where
  arbitrary = arbitrarySpan
  shrink = shrinkSpan

instance QC.Arbitrary GeneratedRewriteSystem where
  arbitrary =
    mkGeneratedRewriteSystem <$> generatedSystemSpans
  shrink generatedRewriteSystem =
    grsSpans generatedRewriteSystem
      & QC.shrinkList QC.shrink
      & filter (not . null)
      & filter (all validGeneratedSpan)
      & fmap mkGeneratedRewriteSystem

instance QC.Arbitrary GeneratedContextPair where
  arbitrary =
    generatedContextPair <$> arbitraryContextObjects <*> arbitraryContextObjects
  shrink contextPair =
    let shrinkLeft =
          QC.shrinkList shrinkPattern (gcpLeftObjects contextPair)
            & fmap (`generatedContextPair` gcpRightObjects contextPair)
        shrinkRight =
          QC.shrinkList shrinkPattern (gcpRightObjects contextPair)
            & fmap (generatedContextPair (gcpLeftObjects contextPair))
     in shrinkLeft <> shrinkRight

arbitraryPatternSized :: Int -> QC.Gen (Pattern ArithF)
arbitraryPatternSized size =
  if size <= 0
    then QC.oneof [arbitraryPatternVar, arbitraryLiteral]
    else
      QC.frequency
        [ (1, arbitraryPatternVar),
          (2, arbitraryLiteral),
          (3, PatternNode <$> (Add <$> nextPattern <*> nextPattern))
        ]
  where
    nextPattern =
      QC.resize (size `div` 2) (arbitraryPatternSized (size `div` 2))

shrinkPattern :: Pattern ArithF -> [Pattern ArithF]
shrinkPattern patternValue =
  case patternValue of
    PatternVar patternVar ->
      QC.shrink (EGraph.patternVarKey patternVar)
        & filter (>= 0)
        & fmap (PatternVar . EGraph.mkPatternVar)
    PatternNode (Num value) ->
      QC.shrink value
        & fmap (PatternNode . Num)
    PatternNode (Var value) ->
      QC.shrink value
        & fmap (PatternNode . Var)
    PatternNode (Add leftPattern rightPattern) ->
      [leftPattern, rightPattern]
        <> fmap (\leftValue -> PatternNode (Add leftValue rightPattern)) (shrinkPattern leftPattern)
        <> fmap (PatternNode . Add leftPattern) (shrinkPattern rightPattern)
    PatternNode (Mul leftPattern rightPattern) ->
      [leftPattern, rightPattern]
        <> fmap (\leftValue -> PatternNode (Mul leftValue rightPattern)) (shrinkPattern leftPattern)
        <> fmap (PatternNode . Mul leftPattern) (shrinkPattern rightPattern)
    PatternNode (Neg childPattern) ->
      childPattern
        : fmap (PatternNode . Neg) (shrinkPattern childPattern)

arbitraryPatternVar :: QC.Gen (Pattern ArithF)
arbitraryPatternVar =
  PatternVar . EGraph.mkPatternVar <$> QC.chooseInt (0, 2)

arbitraryLiteral :: QC.Gen (Pattern ArithF)
arbitraryLiteral =
  PatternNode . Num <$> QC.chooseInt (0, 3)

arbitrarySpan :: QC.Gen (RewriteMorphism ArithF)
arbitrarySpan = do
  leftPattern <- QC.resize 4 QC.arbitrary
  rightPattern <- QC.resize 4 QC.arbitrary
  spanOrdinal <- QC.chooseInt (0, 32)
  pure (mkGeneratedSpan ("span-" <> show spanOrdinal) leftPattern rightPattern)

shrinkSpan :: RewriteMorphism ArithF -> [RewriteMorphism ArithF]
shrinkSpan spanValue =
  let shrinkLeft =
        fmap
          (\leftPattern -> mkGeneratedSpan (rewriteMorphismName spanValue) leftPattern (rewriteMorphismRight spanValue))
          (shrinkPattern (rewriteMorphismLeft spanValue))
      shrinkRight =
        fmap
          (mkGeneratedSpan (rewriteMorphismName spanValue) (rewriteMorphismLeft spanValue))
          (shrinkPattern (rewriteMorphismRight spanValue))
   in shrinkLeft <> shrinkRight

mkGeneratedSpan :: String -> Pattern ArithF -> Pattern ArithF -> RewriteMorphism ArithF
mkGeneratedSpan spanName leftPattern rightPattern =
  expectGeneratedSpan $
    rewriteMorphismWithInterface
      spanName
      leftPattern
      (Set.intersection (patternVariables leftPattern) (patternVariables rightPattern))
      rightPattern
      Nothing
      Nothing

expectGeneratedSpan :: Show error => Either error value -> value
expectGeneratedSpan =
  either
    (\failure -> error ("generated rewrite span rejected: " <> show failure))
    id

mkGeneratedRewriteSystem :: [RewriteMorphism ArithF] -> GeneratedRewriteSystem
mkGeneratedRewriteSystem spanValues =
  GeneratedRewriteSystem
    { grsSpans = spanValues,
      grsSystem = mkRewriteSystem spanValues
    }

generatedSystemSpans :: QC.Gen [RewriteMorphism ArithF]
generatedSystemSpans =
  QC.frequency
    [ (3, generatedSingleSpan),
      (3, generatedReversibleSpans),
      (3, generatedChainSpans),
      (3, generatedDisjointSpans)
    ]

generatedReversibleSpans :: QC.Gen [RewriteMorphism ArithF]
generatedReversibleSpans = do
  literalValue <- QC.chooseInt (0, 3)
  let variablePattern :: Pattern ArithF
      variablePattern = PatternVar (EGraph.mkPatternVar 0)
      expandedPattern = PatternNode (Add variablePattern (PatternNode (Num literalValue)))
  pure
    [ mkGeneratedSpan "expand" variablePattern expandedPattern,
      mkGeneratedSpan "shrink" expandedPattern variablePattern
    ]

generatedChainSpans :: QC.Gen [RewriteMorphism ArithF]
generatedChainSpans = do
  firstValue <- QC.chooseInt (0, 3)
  secondValue <- distinctTarget firstValue
  thirdValue <- distinctTarget secondValue
  pure
    [ generatedConstantSpan "chain-left" firstValue secondValue,
      generatedConstantSpan "chain-right" secondValue thirdValue
    ]

generatedDisjointSpans :: QC.Gen [RewriteMorphism ArithF]
generatedDisjointSpans = do
  leftSource <- QC.chooseInt (0, 3)
  leftTarget <- distinctTarget leftSource
  rightSource <- QC.chooseInt (0, 3)
  rightTarget <- distinctTarget rightSource
  pure
    [ generatedConstantSpan "left" leftSource leftTarget,
      generatedConstantSpan "right" rightSource rightTarget
    ]

generatedSingleSpan :: QC.Gen [RewriteMorphism ArithF]
generatedSingleSpan = do
  sourceValue <- QC.chooseInt (0, 3)
  targetValue <- distinctTarget sourceValue
  pure [generatedConstantSpan "single" sourceValue targetValue]


validGeneratedSpan :: RewriteMorphism ArithF -> Bool
validGeneratedSpan spanValue =
  rewriteMorphismLeft spanValue /= rewriteMorphismRight spanValue

distinctTarget :: Int -> QC.Gen Int
distinctTarget sourceValue =
  QC.suchThat (QC.chooseInt (0, 3)) (/= sourceValue)

generatedConstantSpan :: String -> Int -> Int -> RewriteMorphism ArithF
generatedConstantSpan spanName sourceValue targetValue =
  mkGeneratedSpan spanName (PatternNode (Num sourceValue)) (PatternNode (Num targetValue))

arbitraryContextObjects :: QC.Gen [Pattern ArithF]
arbitraryContextObjects = do
  contextSize <- QC.chooseInt (0, 3)
  QC.vectorOf contextSize (QC.resize 3 QC.arbitrary)

generatedContextPair :: [Pattern ArithF] -> [Pattern ArithF] -> GeneratedContextPair
generatedContextPair leftObjects rightObjects =
  GeneratedContextPair
    { gcpLeftObjects = leftObjects,
      gcpRightObjects = rightObjects,
      gcpLeftContext = mkRewriteContext 0 leftObjects,
      gcpRightContext = mkRewriteContext 1 rightObjects
    }

(&) :: a -> (a -> b) -> b
value & continuation = continuation value

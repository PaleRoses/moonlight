{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.EGraph.Test.SDF.Core
  ( SDFF (..),
    SDFTag (..),
    Depth (..),
    depthAnalysis,
    sdfCost,
    sphere,
    capsule,
    box,
    sdfUnion,
    sdfIntersect,
    sdfComplement,
    smoothUnion,
    sdfEmpty,
    sdfFull,
    SDFLawVariable (..),
    SDFLawTerm,
    foldSDFLawTerm,
    SDFLawRequirement (..),
    SDFLaw (..),
    SDFFactLaw (..),
    sdfLatticeLaws,
    sdfComplementLaws,
    sdfCommutativityLaws,
    sdfSmoothBlendLaws,
    sdfGlobalLaws,
    sdfCoarseApproximationLaw,
    sdfLawBook,
    nonDegenerateRadiusFactId,
    nonDegenerateRadiusFactLaw,
    sdfRawRewriteRule,
    sdfRawFactRule,
    sdfGlobalRules,
    sdfCoarseApproximationRule,
    sdfRuleBook,
    nonDegenerateRadiusFactRule,
    genPositiveDouble,
    genLeaf,
    genSDFTerm,
    seededSDFTerms,
  )
where

import Moonlight.Algebra (JoinSemilattice (join))
import Moonlight.Core
  ( HasConstructorTag (..),
    Pattern (..),
    PatternVar,
    ZipMatch (..),
    zipSameNodeShape,
  )
import Moonlight.Core qualified as EGraph
import Data.Fix (Fix (..))
import Data.Kind (Type)
import Moonlight.EGraph.Pure.Analysis (AnalysisSpec, semilatticeAnalysis)
import Moonlight.EGraph.Pure.Extraction (CostAlgebra (..))
import Moonlight.EGraph.Pure.Types (RewriteRuleId (..))
import Moonlight.Rewrite.System
  ( RewriteCondition (..),
    data GuardRoot,
    guardHasFact,
  )
import Moonlight.Rewrite.System
  ( FactRule,
    FactRuleId (..),
    RawFactRule (..),
  )
import Moonlight.Rewrite.System (FactId (..))
import Moonlight.Rewrite.System (RawRewriteRule (..))
import Test.Tasty.QuickCheck
  ( Arbitrary (..),
    Gen,
    chooseInt,
    oneof,
    sized,
  )
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (mkQCGen)

type SDFF :: Type -> Type
data SDFF a
  = Sphere Double
  | Capsule Double Double
  | Box Double Double Double
  | SDFUnion a a
  | SDFIntersect a a
  | SDFSubtract a a
  | SmoothUnion Double a a
  | Complement a
  | SDFEmpty
  | SDFFull
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

type SDFTag :: Type
data SDFTag
  = SphereTag
  | CapsuleTag
  | BoxTag
  | UnionTag
  | IntersectTag
  | SubtractTag
  | SmoothUnionTag
  | ComplementTag
  | EmptyTag
  | FullTag
  deriving stock (Eq, Ord, Show)

instance HasConstructorTag SDFF where
  type ConstructorTag SDFF = SDFTag
  constructorTag = \case
    Sphere {} -> SphereTag
    Capsule {} -> CapsuleTag
    Box {} -> BoxTag
    SDFUnion {} -> UnionTag
    SDFIntersect {} -> IntersectTag
    SDFSubtract {} -> SubtractTag
    SmoothUnion {} -> SmoothUnionTag
    Complement {} -> ComplementTag
    SDFEmpty -> EmptyTag
    SDFFull -> FullTag

instance ZipMatch SDFF where
  zipMatch = zipSameNodeShape

type Depth :: Type
newtype Depth = Depth Int
  deriving stock (Eq, Ord, Show)

instance JoinSemilattice Depth where
  join (Depth leftDepth) (Depth rightDepth) =
    Depth (max leftDepth rightDepth)

depthAnalysis :: AnalysisSpec SDFF Depth
depthAnalysis =
  semilatticeAnalysis $ \case
    Sphere {} -> Depth 0
    Capsule {} -> Depth 0
    Box {} -> Depth 0
    SDFEmpty -> Depth 0
    SDFFull -> Depth 0
    SDFUnion (Depth leftDepth) (Depth rightDepth) -> Depth (max leftDepth rightDepth + 1)
    SDFIntersect (Depth leftDepth) (Depth rightDepth) -> Depth (max leftDepth rightDepth + 1)
    SDFSubtract (Depth leftDepth) (Depth rightDepth) -> Depth (max leftDepth rightDepth + 1)
    SmoothUnion _ (Depth leftDepth) (Depth rightDepth) -> Depth (max leftDepth rightDepth + 1)
    Complement (Depth childDepth) -> Depth (childDepth + 1)

sdfCost :: CostAlgebra SDFF Int
sdfCost =
  CostAlgebra $ \case
    Sphere {} -> 1
    Capsule {} -> 2
    Box {} -> 3
    SDFEmpty -> 0
    SDFFull -> 0
    SDFUnion leftCost rightCost -> leftCost + rightCost + 1
    SDFIntersect leftCost rightCost -> leftCost + rightCost + 1
    SDFSubtract leftCost rightCost -> leftCost + rightCost + 1
    SmoothUnion _ leftCost rightCost -> leftCost + rightCost + 2
    Complement childCost -> childCost + 1

sphere :: Double -> Fix SDFF
sphere radius =
  Fix (Sphere radius)

capsule :: Double -> Double -> Fix SDFF
capsule radius height =
  Fix (Capsule radius height)

box :: Double -> Double -> Double -> Fix SDFF
box width height depth =
  Fix (Box width height depth)

sdfUnion :: Fix SDFF -> Fix SDFF -> Fix SDFF
sdfUnion leftChild rightChild =
  Fix (SDFUnion leftChild rightChild)

sdfIntersect :: Fix SDFF -> Fix SDFF -> Fix SDFF
sdfIntersect leftChild rightChild =
  Fix (SDFIntersect leftChild rightChild)

sdfComplement :: Fix SDFF -> Fix SDFF
sdfComplement child =
  Fix (Complement child)

smoothUnion :: Double -> Fix SDFF -> Fix SDFF -> Fix SDFF
smoothUnion blendRadius leftChild rightChild =
  Fix (SmoothUnion blendRadius leftChild rightChild)

sdfEmpty :: Fix SDFF
sdfEmpty =
  Fix SDFEmpty

sdfFull :: Fix SDFF
sdfFull =
  Fix SDFFull

type SDFLawVariable :: Type
data SDFLawVariable
  = SDFLawX
  | SDFLawY
  deriving stock (Eq, Ord, Show)

type SDFLawTerm :: Type
data SDFLawTerm
  = SDFLawVariable SDFLawVariable
  | SDFLawNode (SDFF SDFLawTerm)
  deriving stock (Eq, Ord, Show)

foldSDFLawTerm ::
  (SDFLawVariable -> value) ->
  (SDFF value -> value) ->
  SDFLawTerm ->
  value
foldSDFLawTerm interpretVariable interpretNode =
  foldTerm
  where
    foldTerm = \case
      SDFLawVariable variable ->
        interpretVariable variable
      SDFLawNode layer ->
        interpretNode (fmap foldTerm layer)

type SDFLawRequirement :: Type
data SDFLawRequirement
  = UnconditionalSDFLaw
  | RequiresNonDegenerateRadius
  deriving stock (Eq, Ord, Show)

type SDFLaw :: Type
data SDFLaw = SDFLaw
  { sdfLawId :: RewriteRuleId,
    sdfLawName :: String,
    sdfLawLhs :: SDFLawTerm,
    sdfLawRhs :: SDFLawTerm,
    sdfLawRequirement :: SDFLawRequirement
  }
  deriving stock (Eq, Ord, Show)

type SDFFactLaw :: Type
data SDFFactLaw = SDFFactLaw
  { sdfFactLawId :: FactRuleId,
    sdfFactLawName :: String,
    sdfFactLawTerm :: SDFLawTerm,
    sdfFactLawFactId :: FactId
  }
  deriving stock (Eq, Ord, Show)

sdfLatticeLaws :: [SDFLaw]
sdfLatticeLaws =
  [ unconditionalLaw 0 "union-empty-right" (lawUnion lawX lawEmpty) lawX,
    unconditionalLaw 1 "union-empty-left" (lawUnion lawEmpty lawX) lawX,
    unconditionalLaw 2 "intersect-full-right" (lawIntersect lawX lawFull) lawX,
    unconditionalLaw 3 "intersect-full-left" (lawIntersect lawFull lawX) lawX,
    unconditionalLaw 4 "union-full-right" (lawUnion lawX lawFull) lawFull,
    unconditionalLaw 5 "union-full-left" (lawUnion lawFull lawX) lawFull,
    unconditionalLaw 6 "intersect-empty-right" (lawIntersect lawX lawEmpty) lawEmpty,
    unconditionalLaw 7 "intersect-empty-left" (lawIntersect lawEmpty lawX) lawEmpty,
    unconditionalLaw 8 "union-idempotent" (lawUnion lawX lawX) lawX
  ]

sdfComplementLaws :: [SDFLaw]
sdfComplementLaws =
  [ unconditionalLaw 20 "double-complement" (lawComplement (lawComplement lawX)) lawX,
    unconditionalLaw 21 "complement-empty" (lawComplement lawEmpty) lawFull,
    unconditionalLaw 22 "complement-full" (lawComplement lawFull) lawEmpty
  ]

sdfCommutativityLaws :: [SDFLaw]
sdfCommutativityLaws =
  [ unconditionalLaw 30 "union-commute" (lawUnion lawX lawY) (lawUnion lawY lawX),
    unconditionalLaw 31 "intersect-commute" (lawIntersect lawX lawY) (lawIntersect lawY lawX),
    unconditionalLaw 32 "smooth-union-commute" (lawSmoothUnion 0.5 lawX lawY) (lawSmoothUnion 0.5 lawY lawX)
  ]

sdfSmoothBlendLaws :: [SDFLaw]
sdfSmoothBlendLaws =
  [unconditionalLaw 40 "smooth-union-zero-degenerate" (lawSmoothUnion 0.0 lawX lawY) (lawUnion lawX lawY)]

sdfGlobalLaws :: [SDFLaw]
sdfGlobalLaws =
  sdfLatticeLaws
    <> sdfComplementLaws
    <> sdfCommutativityLaws
    <> sdfSmoothBlendLaws

sdfCoarseApproximationLaw :: SDFLaw
sdfCoarseApproximationLaw =
  SDFLaw
    { sdfLawId = RewriteRuleId 41,
      sdfLawName = "smooth-union-coarse-collapse",
      sdfLawLhs = lawSmoothUnion nonDegenerateRadiusWitness lawX lawY,
      sdfLawRhs = lawUnion lawX lawY,
      sdfLawRequirement = RequiresNonDegenerateRadius
    }

sdfLawBook :: [SDFLaw]
sdfLawBook =
  sdfGlobalLaws <> [sdfCoarseApproximationLaw]

nonDegenerateRadiusFactId :: FactId
nonDegenerateRadiusFactId =
  FactId 0

nonDegenerateRadiusFactLaw :: SDFFactLaw
nonDegenerateRadiusFactLaw =
  SDFFactLaw
    { sdfFactLawId = FactRuleId 0,
      sdfFactLawName = "derive-nondegenerate-positive-radius",
      sdfFactLawTerm = sdfLawLhs sdfCoarseApproximationLaw,
      sdfFactLawFactId = nonDegenerateRadiusFactId
    }

sdfRawRewriteRule :: SDFLaw -> RawRewriteRule (RewriteCondition capability SDFF) SDFF
sdfRawRewriteRule law =
  RawRewriteRule
    { rrId = sdfLawId law,
      rrLhs = rawPattern (sdfLawLhs law),
      rrRhs = rawPattern (sdfLawRhs law),
      rrCondition = rawRequirement (sdfLawRequirement law),
      rrApplicationCondition = Nothing,
      rrPostSubst = Nothing
    }

sdfRawFactRule :: SDFFactLaw -> FactRule capability SDFF
sdfRawFactRule law =
  FactRule
    { frId = sdfFactLawId law,
      frName = sdfFactLawName law,
      frPattern = rawPattern (sdfFactLawTerm law),
      frProjection = [GuardRoot],
      frFactId = sdfFactLawFactId law,
      frCondition = Nothing
    }

sdfGlobalRules :: [RawRewriteRule (RewriteCondition capability SDFF) SDFF]
sdfGlobalRules =
  fmap sdfRawRewriteRule sdfGlobalLaws

sdfCoarseApproximationRule :: RawRewriteRule (RewriteCondition capability SDFF) SDFF
sdfCoarseApproximationRule =
  sdfRawRewriteRule sdfCoarseApproximationLaw

sdfRuleBook :: [RawRewriteRule (RewriteCondition capability SDFF) SDFF]
sdfRuleBook =
  fmap sdfRawRewriteRule sdfLawBook

nonDegenerateRadiusFactRule :: FactRule capability SDFF
nonDegenerateRadiusFactRule =
  sdfRawFactRule nonDegenerateRadiusFactLaw

unconditionalLaw :: Int -> String -> SDFLawTerm -> SDFLawTerm -> SDFLaw
unconditionalLaw ruleId ruleName lhs rhs =
  SDFLaw
    { sdfLawId = RewriteRuleId ruleId,
      sdfLawName = ruleName,
      sdfLawLhs = lhs,
      sdfLawRhs = rhs,
      sdfLawRequirement = UnconditionalSDFLaw
    }

lawX :: SDFLawTerm
lawX =
  SDFLawVariable SDFLawX

lawY :: SDFLawTerm
lawY =
  SDFLawVariable SDFLawY

lawUnion :: SDFLawTerm -> SDFLawTerm -> SDFLawTerm
lawUnion left right =
  SDFLawNode (SDFUnion left right)

lawIntersect :: SDFLawTerm -> SDFLawTerm -> SDFLawTerm
lawIntersect left right =
  SDFLawNode (SDFIntersect left right)

lawComplement :: SDFLawTerm -> SDFLawTerm
lawComplement term =
  SDFLawNode (Complement term)

lawSmoothUnion :: Double -> SDFLawTerm -> SDFLawTerm -> SDFLawTerm
lawSmoothUnion blend left right =
  SDFLawNode (SmoothUnion blend left right)

lawEmpty :: SDFLawTerm
lawEmpty =
  SDFLawNode SDFEmpty

lawFull :: SDFLawTerm
lawFull =
  SDFLawNode SDFFull

rawPattern :: SDFLawTerm -> Pattern SDFF
rawPattern =
  foldSDFLawTerm (PatternVar . rawPatternVariable) PatternNode

rawPatternVariable :: SDFLawVariable -> PatternVar
rawPatternVariable =
  \case
    SDFLawX -> EGraph.mkPatternVar 0
    SDFLawY -> EGraph.mkPatternVar 1

rawRequirement :: SDFLawRequirement -> Maybe (RewriteCondition capability SDFF)
rawRequirement =
  \case
    UnconditionalSDFLaw ->
      Nothing
    RequiresNonDegenerateRadius ->
      Just (RewriteCondition (guardHasFact nonDegenerateRadiusFactId [GuardRoot]))

nonDegenerateRadiusWitness :: Double
nonDegenerateRadiusWitness =
  0.5

genPositiveDouble :: Gen Double
genPositiveDouble =
  fmap (\n -> fromIntegral (n :: Int) * 0.5 + 0.5) (chooseInt (0, 19))

genLeaf :: Gen (Fix SDFF)
genLeaf =
  oneof
    [ fmap (Fix . Sphere) genPositiveDouble,
      fmap (\(radius, height) -> Fix (Capsule radius height)) genLeafPair,
      fmap (\(width, height, depth) -> Fix (Box width height depth)) genLeafTriple,
      pure (Fix SDFEmpty),
      pure (Fix SDFFull)
    ]
  where
    genLeafPair = (,) <$> genPositiveDouble <*> genPositiveDouble
    genLeafTriple = (,,) <$> genPositiveDouble <*> genPositiveDouble <*> genPositiveDouble

genSDFTerm :: Int -> Gen (Fix SDFF)
genSDFTerm remainingDepth
  | remainingDepth <= 0 = genLeaf
  | otherwise =
      let childGen = genSDFTerm (remainingDepth - 1)
       in oneof
            [ genLeaf,
              fmap (\(leftChild, rightChild) -> Fix (SDFUnion leftChild rightChild)) (genBinaryChildren childGen),
              fmap (\(leftChild, rightChild) -> Fix (SDFIntersect leftChild rightChild)) (genBinaryChildren childGen),
              fmap (\(leftChild, rightChild) -> Fix (SDFSubtract leftChild rightChild)) (genBinaryChildren childGen),
              fmap
                (\(blendRadius, leftChild, rightChild) -> Fix (SmoothUnion blendRadius leftChild rightChild))
                (genSmoothChildren childGen),
              fmap (Fix . Complement) childGen
            ]
  where
    genBinaryChildren :: Applicative f => f a -> f (a, a)
    genBinaryChildren childGenerator =
      (,) <$> childGenerator <*> childGenerator
    genSmoothChildren :: Gen a -> Gen (Double, a, a)
    genSmoothChildren childGenerator =
      (,,) <$> genPositiveDouble <*> childGenerator <*> childGenerator

seededSDFTerms :: Int -> Int -> Int -> [Fix SDFF]
seededSDFTerms seedValue termDepth termCount =
  fmap
    ( \termIndex ->
        unGen
          (genSDFTerm termDepth)
          (mkQCGen (seedValue + termIndex))
          termDepth
    )
    [0 .. termCount - 1]

instance Arbitrary (Fix SDFF) where
  arbitrary =
    sized (\generatorSize -> genSDFTerm (min generatorSize 4))

  shrink (Fix layer) =
    case layer of
      Sphere {} -> []
      Capsule {} -> [Fix SDFEmpty]
      Box {} -> [Fix SDFEmpty]
      SDFEmpty -> []
      SDFFull -> []
      SDFUnion leftChild rightChild ->
        [leftChild, rightChild]
          <> fmap (\shrunkLeft -> Fix (SDFUnion shrunkLeft rightChild)) (shrink leftChild)
          <> fmap (\shrunkRight -> Fix (SDFUnion leftChild shrunkRight)) (shrink rightChild)
      SDFIntersect leftChild rightChild ->
        [leftChild, rightChild]
          <> fmap (\shrunkLeft -> Fix (SDFIntersect shrunkLeft rightChild)) (shrink leftChild)
          <> fmap (\shrunkRight -> Fix (SDFIntersect leftChild shrunkRight)) (shrink rightChild)
      SDFSubtract leftChild rightChild ->
        [leftChild, rightChild]
          <> fmap (\shrunkLeft -> Fix (SDFSubtract shrunkLeft rightChild)) (shrink leftChild)
          <> fmap (\shrunkRight -> Fix (SDFSubtract leftChild shrunkRight)) (shrink rightChild)
      SmoothUnion blendRadius leftChild rightChild ->
        [leftChild, rightChild, Fix (SDFUnion leftChild rightChild)]
          <> fmap (\shrunkLeft -> Fix (SmoothUnion blendRadius shrunkLeft rightChild)) (shrink leftChild)
          <> fmap (\shrunkRight -> Fix (SmoothUnion blendRadius leftChild shrunkRight)) (shrink rightChild)
      Complement child ->
        [child]
          <> fmap (Fix . Complement) (shrink child)

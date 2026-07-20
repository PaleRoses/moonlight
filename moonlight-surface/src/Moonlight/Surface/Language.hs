{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Surface.Language
  ( SurfaceF (..),
    SurfaceTag (..),
    SurfaceView (..),
    SurfaceNodeCount (..),
    SurfaceLiteralValue (..),
    SurfaceValue (..),
    SurfaceAnalysis (..),
    SurfaceCapability (..),
    surfaceAnalysis,
    surfaceAnalysisValueOf,
    surfaceKnownZero,
    surfaceKnownOne,
    surfaceGuardCapabilityResolver,
    surfaceCost,
    surfaceReify,
    surfaceLiteralTerm,
    lit,
    vec,
    vadd,
    vmul,
    sphere,
    cube,
    cylinder,
    translate,
    rotate,
    scale,
    union,
    inter,
    diff,
    viewSurface,
    surfaceFromView,
  )
where

import Data.Fix (Fix (..))
import Data.IntMap.Strict qualified as IntMap
import Data.Kind (Type)
import Moonlight.Algebra (JoinSemilattice (join))
import Moonlight.Core (ClassId, ConstructorTag, HasConstructorTag (..), ZipMatch (..), classIdKey, zipSameNodeShape)
import Moonlight.EGraph.Pure.Analysis (AnalysisSpec, semilatticeAnalysis)
import Moonlight.EGraph.Pure.Extraction (CostAlgebra (..))
import Moonlight.EGraph.Pure.Types (EGraph, canonicalizeClassId, eGraphAnalysis)
import Moonlight.Rewrite.System (GuardCapabilityResolver (..))

type SurfaceF :: Type -> Type
data SurfaceF a
  = SurfaceLit !Double
  | SurfaceVec !a !a !a
  | SurfaceVAdd !a !a
  | SurfaceVMul !a !a
  | SurfaceSphere !a
  | SurfaceCube !a
  | SurfaceCylinder !a !a
  | SurfaceTranslate !a !a
  | SurfaceRotate !a !a
  | SurfaceScale !a !a
  | SurfaceUnion !a !a
  | SurfaceInter !a !a
  | SurfaceDiff !a !a
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

type SurfaceTag :: Type
data SurfaceTag
  = SurfaceLitTag !Double
  | SurfaceVecTag
  | SurfaceVAddTag
  | SurfaceVMulTag
  | SurfaceSphereTag
  | SurfaceCubeTag
  | SurfaceCylinderTag
  | SurfaceTranslateTag
  | SurfaceRotateTag
  | SurfaceScaleTag
  | SurfaceUnionTag
  | SurfaceInterTag
  | SurfaceDiffTag
  deriving stock (Eq, Ord, Show)

instance HasConstructorTag SurfaceF where
  type ConstructorTag SurfaceF = SurfaceTag

  constructorTag =
    \case
      SurfaceLit value -> SurfaceLitTag value
      SurfaceVec {} -> SurfaceVecTag
      SurfaceVAdd {} -> SurfaceVAddTag
      SurfaceVMul {} -> SurfaceVMulTag
      SurfaceSphere {} -> SurfaceSphereTag
      SurfaceCube {} -> SurfaceCubeTag
      SurfaceCylinder {} -> SurfaceCylinderTag
      SurfaceTranslate {} -> SurfaceTranslateTag
      SurfaceRotate {} -> SurfaceRotateTag
      SurfaceScale {} -> SurfaceScaleTag
      SurfaceUnion {} -> SurfaceUnionTag
      SurfaceInter {} -> SurfaceInterTag
      SurfaceDiff {} -> SurfaceDiffTag

instance ZipMatch SurfaceF where
  zipMatch = zipSameNodeShape

type SurfaceView :: Type
data SurfaceView
  = SurfaceLitView !Double
  | SurfaceVecView !SurfaceView !SurfaceView !SurfaceView
  | SurfaceVAddView !SurfaceView !SurfaceView
  | SurfaceVMulView !SurfaceView !SurfaceView
  | SurfaceSphereView !SurfaceView
  | SurfaceCubeView !SurfaceView
  | SurfaceCylinderView !SurfaceView !SurfaceView
  | SurfaceTranslateView !SurfaceView !SurfaceView
  | SurfaceRotateView !SurfaceView !SurfaceView
  | SurfaceScaleView !SurfaceView !SurfaceView
  | SurfaceUnionView !SurfaceView !SurfaceView
  | SurfaceInterView !SurfaceView !SurfaceView
  | SurfaceDiffView !SurfaceView !SurfaceView
  deriving stock (Eq, Ord, Show)

type SurfaceNodeCount :: Type
newtype SurfaceNodeCount = SurfaceNodeCount Int
  deriving stock (Eq, Ord, Show)

type SurfaceLiteralValue :: Type
data SurfaceLiteralValue
  = SurfaceScalar !Double
  | SurfaceVector !Double !Double !Double
  deriving stock (Eq, Ord, Show)

type SurfaceValue :: Type
data SurfaceValue
  = SurfaceOpaque
  | SurfaceKnown !SurfaceLiteralValue
  | SurfaceConflict
  deriving stock (Eq, Ord, Show)

type SurfaceAnalysis :: Type
data SurfaceAnalysis = SurfaceAnalysis
  { surfaceAnalysisNodeCount :: !SurfaceNodeCount,
    surfaceAnalysisValue :: !SurfaceValue
  }
  deriving stock (Eq, Ord, Show)

type SurfaceCapability :: Type
data SurfaceCapability
  = SurfaceKnownZero
  | SurfaceKnownOne
  deriving stock (Eq, Ord, Show)

instance JoinSemilattice SurfaceNodeCount where
  join (SurfaceNodeCount leftCount) (SurfaceNodeCount rightCount) =
    SurfaceNodeCount (max leftCount rightCount)

instance JoinSemilattice SurfaceValue where
  join leftValue rightValue =
    case (leftValue, rightValue) of
      (SurfaceConflict, _) -> SurfaceConflict
      (_, SurfaceConflict) -> SurfaceConflict
      (SurfaceOpaque, value) -> value
      (value, SurfaceOpaque) -> value
      (SurfaceKnown leftKnown, SurfaceKnown rightKnown)
        | leftKnown == rightKnown -> SurfaceKnown leftKnown
        | otherwise -> SurfaceConflict

instance JoinSemilattice SurfaceAnalysis where
  join leftAnalysis rightAnalysis =
    SurfaceAnalysis
      { surfaceAnalysisNodeCount = join (surfaceAnalysisNodeCount leftAnalysis) (surfaceAnalysisNodeCount rightAnalysis),
        surfaceAnalysisValue = join (surfaceAnalysisValue leftAnalysis) (surfaceAnalysisValue rightAnalysis)
      }

surfaceAnalysis :: AnalysisSpec SurfaceF SurfaceAnalysis
surfaceAnalysis =
  semilatticeAnalysis surfaceLayerAnalysis

surfaceAnalysisValueOf :: EGraph SurfaceF SurfaceAnalysis -> ClassId -> SurfaceValue
surfaceAnalysisValueOf graph classId =
  maybe
    SurfaceOpaque
    surfaceAnalysisValue
    (IntMap.lookup (classIdKey (canonicalizeClassId graph classId)) (eGraphAnalysis graph))

surfaceKnownZero :: SurfaceValue -> Bool
surfaceKnownZero =
  \case
    SurfaceKnown (SurfaceScalar 0) -> True
    SurfaceKnown (SurfaceVector 0 0 0) -> True
    _ -> False

surfaceKnownOne :: SurfaceValue -> Bool
surfaceKnownOne =
  \case
    SurfaceKnown (SurfaceScalar 1) -> True
    SurfaceKnown (SurfaceVector 1 1 1) -> True
    _ -> False

surfaceGuardCapabilityResolver :: EGraph SurfaceF SurfaceAnalysis -> GuardCapabilityResolver SurfaceCapability
surfaceGuardCapabilityResolver graph =
  GuardCapabilityResolver
    ( \capability classIds ->
        all (surfaceCapabilityHolds capability . surfaceAnalysisValueOf graph) classIds
    )

surfaceCapabilityHolds :: SurfaceCapability -> SurfaceValue -> Bool
surfaceCapabilityHolds capability value =
  case capability of
    SurfaceKnownZero -> surfaceKnownZero value
    SurfaceKnownOne -> surfaceKnownOne value

surfaceLayerAnalysis :: SurfaceF SurfaceAnalysis -> SurfaceAnalysis
surfaceLayerAnalysis layer =
  SurfaceAnalysis
    { surfaceAnalysisNodeCount = surfaceNodeCount (fmap surfaceAnalysisNodeCount layer),
      surfaceAnalysisValue = surfaceLayerValue (fmap surfaceAnalysisValue layer)
    }

surfaceNodeCount :: SurfaceF SurfaceNodeCount -> SurfaceNodeCount
surfaceNodeCount =
  \case
    SurfaceLit _ ->
      SurfaceNodeCount 1
    SurfaceVec (SurfaceNodeCount xCount) (SurfaceNodeCount yCount) (SurfaceNodeCount zCount) ->
      SurfaceNodeCount (xCount + yCount + zCount + 1)
    SurfaceVAdd (SurfaceNodeCount leftCount) (SurfaceNodeCount rightCount) ->
      SurfaceNodeCount (leftCount + rightCount + 1)
    SurfaceVMul (SurfaceNodeCount leftCount) (SurfaceNodeCount rightCount) ->
      SurfaceNodeCount (leftCount + rightCount + 1)
    SurfaceSphere (SurfaceNodeCount radiusCount) ->
      SurfaceNodeCount (radiusCount + 1)
    SurfaceCube (SurfaceNodeCount sizeCount) ->
      SurfaceNodeCount (sizeCount + 1)
    SurfaceCylinder (SurfaceNodeCount radiusCount) (SurfaceNodeCount heightCount) ->
      SurfaceNodeCount (radiusCount + heightCount + 1)
    SurfaceTranslate (SurfaceNodeCount vectorCount) (SurfaceNodeCount bodyCount) ->
      SurfaceNodeCount (vectorCount + bodyCount + 1)
    SurfaceRotate (SurfaceNodeCount vectorCount) (SurfaceNodeCount bodyCount) ->
      SurfaceNodeCount (vectorCount + bodyCount + 1)
    SurfaceScale (SurfaceNodeCount vectorCount) (SurfaceNodeCount bodyCount) ->
      SurfaceNodeCount (vectorCount + bodyCount + 1)
    SurfaceUnion (SurfaceNodeCount leftCount) (SurfaceNodeCount rightCount) ->
      SurfaceNodeCount (leftCount + rightCount + 1)
    SurfaceInter (SurfaceNodeCount leftCount) (SurfaceNodeCount rightCount) ->
      SurfaceNodeCount (leftCount + rightCount + 1)
    SurfaceDiff (SurfaceNodeCount leftCount) (SurfaceNodeCount rightCount) ->
      SurfaceNodeCount (leftCount + rightCount + 1)

surfaceLayerValue :: SurfaceF SurfaceValue -> SurfaceValue
surfaceLayerValue =
  \case
    SurfaceLit value ->
      SurfaceKnown (SurfaceScalar value)
    SurfaceVec xValue yValue zValue ->
      surfaceVectorValue xValue yValue zValue
    SurfaceVAdd leftValue rightValue ->
      surfaceBinaryValue (+) (+) leftValue rightValue
    SurfaceVMul leftValue rightValue ->
      surfaceBinaryValue (*) (*) leftValue rightValue
    _ ->
      SurfaceOpaque

surfaceVectorValue :: SurfaceValue -> SurfaceValue -> SurfaceValue -> SurfaceValue
surfaceVectorValue xValue yValue zValue =
  case (xValue, yValue, zValue) of
    (SurfaceConflict, _, _) -> SurfaceConflict
    (_, SurfaceConflict, _) -> SurfaceConflict
    (_, _, SurfaceConflict) -> SurfaceConflict
    (SurfaceKnown (SurfaceScalar x), SurfaceKnown (SurfaceScalar y), SurfaceKnown (SurfaceScalar z)) ->
      SurfaceKnown (SurfaceVector x y z)
    (SurfaceOpaque, _, _) -> SurfaceOpaque
    (_, SurfaceOpaque, _) -> SurfaceOpaque
    (_, _, SurfaceOpaque) -> SurfaceOpaque
    _ -> SurfaceConflict

surfaceBinaryValue :: (Double -> Double -> Double) -> (Double -> Double -> Double) -> SurfaceValue -> SurfaceValue -> SurfaceValue
surfaceBinaryValue scalarOp vectorOp leftValue rightValue =
  case (leftValue, rightValue) of
    (SurfaceConflict, _) -> SurfaceConflict
    (_, SurfaceConflict) -> SurfaceConflict
    (SurfaceOpaque, _) -> SurfaceOpaque
    (_, SurfaceOpaque) -> SurfaceOpaque
    (SurfaceKnown (SurfaceScalar left), SurfaceKnown (SurfaceScalar right)) ->
      SurfaceKnown (SurfaceScalar (scalarOp left right))
    (SurfaceKnown (SurfaceVector leftX leftY leftZ), SurfaceKnown (SurfaceVector rightX rightY rightZ)) ->
      SurfaceKnown (SurfaceVector (vectorOp leftX rightX) (vectorOp leftY rightY) (vectorOp leftZ rightZ))
    _ -> SurfaceConflict

surfaceCost :: CostAlgebra SurfaceF Int
surfaceCost =
  CostAlgebra $
    \case
      SurfaceLit _ -> 1
      SurfaceVec xCost yCost zCost -> xCost + yCost + zCost + 1
      SurfaceVAdd leftCost rightCost -> leftCost + rightCost
      SurfaceVMul leftCost rightCost -> leftCost + rightCost
      SurfaceSphere radiusCost -> radiusCost + 1
      SurfaceCube sizeCost -> sizeCost + 1
      SurfaceCylinder radiusCost heightCost -> radiusCost + heightCost + 1
      SurfaceTranslate vectorCost bodyCost -> vectorCost + bodyCost + 1
      SurfaceRotate vectorCost bodyCost -> vectorCost + bodyCost + 1
      SurfaceScale vectorCost bodyCost -> vectorCost + bodyCost + 1
      SurfaceUnion leftCost rightCost -> leftCost + rightCost + 1
      SurfaceInter leftCost rightCost -> leftCost + rightCost + 1
      SurfaceDiff leftCost rightCost -> leftCost + rightCost + 1

surfaceReify :: Fix SurfaceF -> Fix SurfaceF
surfaceReify (Fix surfaceNode) =
  case fmap surfaceReify surfaceNode of
    SurfaceVAdd leftValue rightValue ->
      surfaceReifyArithmetic SurfaceVAdd leftValue rightValue
    SurfaceVMul leftValue rightValue ->
      surfaceReifyArithmetic SurfaceVMul leftValue rightValue
    reifiedNode ->
      Fix reifiedNode

surfaceReifyArithmetic :: (Fix SurfaceF -> Fix SurfaceF -> SurfaceF (Fix SurfaceF)) -> Fix SurfaceF -> Fix SurfaceF -> Fix SurfaceF
surfaceReifyArithmetic node leftTerm rightTerm =
  maybe
    (Fix (node leftTerm rightTerm))
    surfaceLiteralTerm
    (surfaceEvaluateBinary node leftTerm rightTerm)

surfaceEvaluateBinary :: (Fix SurfaceF -> Fix SurfaceF -> SurfaceF (Fix SurfaceF)) -> Fix SurfaceF -> Fix SurfaceF -> Maybe SurfaceLiteralValue
surfaceEvaluateBinary node leftTerm rightTerm =
  case node leftTerm rightTerm of
    SurfaceVAdd {} -> surfaceLiteralBinary (+) leftTerm rightTerm
    SurfaceVMul {} -> surfaceLiteralBinary (*) leftTerm rightTerm
    _ -> Nothing

surfaceLiteralBinary :: (Double -> Double -> Double) -> Fix SurfaceF -> Fix SurfaceF -> Maybe SurfaceLiteralValue
surfaceLiteralBinary op leftTerm rightTerm =
  case (surfaceTermLiteralValue leftTerm, surfaceTermLiteralValue rightTerm) of
    (Just (SurfaceScalar left), Just (SurfaceScalar right)) ->
      Just (SurfaceScalar (op left right))
    (Just (SurfaceVector leftX leftY leftZ), Just (SurfaceVector rightX rightY rightZ)) ->
      Just (SurfaceVector (op leftX rightX) (op leftY rightY) (op leftZ rightZ))
    _ -> Nothing

surfaceTermLiteralValue :: Fix SurfaceF -> Maybe SurfaceLiteralValue
surfaceTermLiteralValue =
  \case
    Fix (SurfaceLit value) ->
      Just (SurfaceScalar value)
    Fix (SurfaceVec (Fix (SurfaceLit xValue)) (Fix (SurfaceLit yValue)) (Fix (SurfaceLit zValue))) ->
      Just (SurfaceVector xValue yValue zValue)
    _ -> Nothing

surfaceLiteralTerm :: SurfaceLiteralValue -> Fix SurfaceF
surfaceLiteralTerm =
  \case
    SurfaceScalar value ->
      lit value
    SurfaceVector xValue yValue zValue ->
      vec (lit xValue) (lit yValue) (lit zValue)

lit :: Double -> Fix SurfaceF
lit =
  Fix . SurfaceLit

vec :: Fix SurfaceF -> Fix SurfaceF -> Fix SurfaceF -> Fix SurfaceF
vec xValue yValue zValue =
  Fix (SurfaceVec xValue yValue zValue)

vadd :: Fix SurfaceF -> Fix SurfaceF -> Fix SurfaceF
vadd left right =
  Fix (SurfaceVAdd left right)

vmul :: Fix SurfaceF -> Fix SurfaceF -> Fix SurfaceF
vmul left right =
  Fix (SurfaceVMul left right)

sphere :: Fix SurfaceF -> Fix SurfaceF
sphere =
  Fix . SurfaceSphere

cube :: Fix SurfaceF -> Fix SurfaceF
cube =
  Fix . SurfaceCube

cylinder :: Fix SurfaceF -> Fix SurfaceF -> Fix SurfaceF
cylinder radius height =
  Fix (SurfaceCylinder radius height)

translate :: Fix SurfaceF -> Fix SurfaceF -> Fix SurfaceF
translate vectorValue body =
  Fix (SurfaceTranslate vectorValue body)

rotate :: Fix SurfaceF -> Fix SurfaceF -> Fix SurfaceF
rotate vectorValue body =
  Fix (SurfaceRotate vectorValue body)

scale :: Fix SurfaceF -> Fix SurfaceF -> Fix SurfaceF
scale vectorValue body =
  Fix (SurfaceScale vectorValue body)

union :: Fix SurfaceF -> Fix SurfaceF -> Fix SurfaceF
union left right =
  Fix (SurfaceUnion left right)

inter :: Fix SurfaceF -> Fix SurfaceF -> Fix SurfaceF
inter left right =
  Fix (SurfaceInter left right)

diff :: Fix SurfaceF -> Fix SurfaceF -> Fix SurfaceF
diff left right =
  Fix (SurfaceDiff left right)

viewSurface :: Fix SurfaceF -> SurfaceView
viewSurface (Fix surfaceNode) =
  case surfaceNode of
    SurfaceLit value -> SurfaceLitView value
    SurfaceVec xValue yValue zValue -> SurfaceVecView (viewSurface xValue) (viewSurface yValue) (viewSurface zValue)
    SurfaceVAdd leftValue rightValue -> SurfaceVAddView (viewSurface leftValue) (viewSurface rightValue)
    SurfaceVMul leftValue rightValue -> SurfaceVMulView (viewSurface leftValue) (viewSurface rightValue)
    SurfaceSphere radius -> SurfaceSphereView (viewSurface radius)
    SurfaceCube size -> SurfaceCubeView (viewSurface size)
    SurfaceCylinder radius height -> SurfaceCylinderView (viewSurface radius) (viewSurface height)
    SurfaceTranslate vectorValue body -> SurfaceTranslateView (viewSurface vectorValue) (viewSurface body)
    SurfaceRotate vectorValue body -> SurfaceRotateView (viewSurface vectorValue) (viewSurface body)
    SurfaceScale vectorValue body -> SurfaceScaleView (viewSurface vectorValue) (viewSurface body)
    SurfaceUnion left right -> SurfaceUnionView (viewSurface left) (viewSurface right)
    SurfaceInter left right -> SurfaceInterView (viewSurface left) (viewSurface right)
    SurfaceDiff left right -> SurfaceDiffView (viewSurface left) (viewSurface right)

surfaceFromView :: SurfaceView -> Fix SurfaceF
surfaceFromView =
  \case
    SurfaceLitView value -> lit value
    SurfaceVecView xValue yValue zValue -> vec (surfaceFromView xValue) (surfaceFromView yValue) (surfaceFromView zValue)
    SurfaceVAddView leftValue rightValue -> vadd (surfaceFromView leftValue) (surfaceFromView rightValue)
    SurfaceVMulView leftValue rightValue -> vmul (surfaceFromView leftValue) (surfaceFromView rightValue)
    SurfaceSphereView radius -> sphere (surfaceFromView radius)
    SurfaceCubeView size -> cube (surfaceFromView size)
    SurfaceCylinderView radius height -> cylinder (surfaceFromView radius) (surfaceFromView height)
    SurfaceTranslateView vectorValue body -> translate (surfaceFromView vectorValue) (surfaceFromView body)
    SurfaceRotateView vectorValue body -> rotate (surfaceFromView vectorValue) (surfaceFromView body)
    SurfaceScaleView vectorValue body -> scale (surfaceFromView vectorValue) (surfaceFromView body)
    SurfaceUnionView left right -> union (surfaceFromView left) (surfaceFromView right)
    SurfaceInterView left right -> inter (surfaceFromView left) (surfaceFromView right)
    SurfaceDiffView left right -> diff (surfaceFromView left) (surfaceFromView right)

module Moonlight.Geometry.Site.Token
  ( SDFTokenF (..),
    SDFConstructorTag (..),
    tokenInterfaceOperator,
    tokenOrderingKey,
  )
where

import Data.Kind (Type)
import Moonlight.Core (HasConstructorTag (..), ZipMatch (..), zipSameNodeShape)
import Moonlight.Geometry.Site.Parameters
import Moonlight.Geometry.Site.Primitive
import Moonlight.Geometry.Site.Semantics (InterfaceOperator (..))
import Moonlight.LinAlg.Geometry (OrthonormalFrame, orthonormalFrameColumns)
import Moonlight.LinAlg.Geometry (AffineTransform (..))
import Moonlight.LinAlg.Geometry (Vec3, vec3ToList)

type SDFTokenF :: Type -> Type
data SDFTokenF a
  = Prim SDFPrimitive
  | SmoothUnion BlendRadius a a
  | SmoothSubtract BlendRadius a a
  | SmoothIntersect BlendRadius a a
  | HardUnion a a
  | HardSubtract a a
  | HardIntersect a a
  | Chamfer Double a a
  | Round Double a
  | NoisePerturbation NoiseParams a
  | DomainWarp NoiseParams a
  | Twist Double a
  | Bend Double a
  | Onion Double a
  | Transform AffineTransform a
  | Scale Vec3 a
  | Repeat RepeatParams a
  | SDFEmpty
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

type SDFConstructorTag :: Type
data SDFConstructorTag
  = TagPrim
  | TagSmoothUnion
  | TagSmoothSubtract
  | TagSmoothIntersect
  | TagHardUnion
  | TagHardSubtract
  | TagHardIntersect
  | TagChamfer
  | TagRound
  | TagNoisePerturbation
  | TagDomainWarp
  | TagTwist
  | TagBend
  | TagOnion
  | TagTransform
  | TagScale
  | TagRepeat
  | TagSDFEmpty
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

instance Ord a => Ord (SDFTokenF a) where
  compare leftToken rightToken =
    compare (tokenOrderingKey leftToken) (tokenOrderingKey rightToken)

instance HasConstructorTag SDFTokenF where
  type ConstructorTag SDFTokenF = SDFConstructorTag

  constructorTag = \case
    Prim _ -> TagPrim
    SmoothUnion {} -> TagSmoothUnion
    SmoothSubtract {} -> TagSmoothSubtract
    SmoothIntersect {} -> TagSmoothIntersect
    HardUnion {} -> TagHardUnion
    HardSubtract {} -> TagHardSubtract
    HardIntersect {} -> TagHardIntersect
    Chamfer {} -> TagChamfer
    Round {} -> TagRound
    NoisePerturbation {} -> TagNoisePerturbation
    DomainWarp {} -> TagDomainWarp
    Twist {} -> TagTwist
    Bend {} -> TagBend
    Onion {} -> TagOnion
    Transform {} -> TagTransform
    Scale {} -> TagScale
    Repeat {} -> TagRepeat
    SDFEmpty -> TagSDFEmpty

instance ZipMatch SDFTokenF where
  zipMatch =
    zipSameNodeShape

tokenInterfaceOperator :: SDFTokenF a -> Maybe InterfaceOperator
tokenInterfaceOperator = \case
  SmoothUnion {} -> Just InterfaceSmoothUnion
  SmoothSubtract {} -> Just InterfaceSmoothSubtract
  SmoothIntersect {} -> Just InterfaceSmoothIntersect
  HardUnion {} -> Just InterfaceHardUnion
  HardSubtract {} -> Just InterfaceHardSubtract
  HardIntersect {} -> Just InterfaceHardIntersect
  Chamfer {} -> Just InterfaceChamfer
  Round {} -> Just InterfaceRound
  _ -> Nothing

tokenOrderingKey :: SDFTokenF a -> (SDFConstructorTag, [Double], [Int], [a])
tokenOrderingKey = \case
  Prim primitive -> (TagPrim, primitiveDoublePayload primitive, primitiveIntPayload primitive, [])
  SmoothUnion blendRadius leftTerm rightTerm -> (TagSmoothUnion, [blendRadius], [], [leftTerm, rightTerm])
  SmoothSubtract blendRadius leftTerm rightTerm -> (TagSmoothSubtract, [blendRadius], [], [leftTerm, rightTerm])
  SmoothIntersect blendRadius leftTerm rightTerm -> (TagSmoothIntersect, [blendRadius], [], [leftTerm, rightTerm])
  HardUnion leftTerm rightTerm -> (TagHardUnion, [], [], [leftTerm, rightTerm])
  HardSubtract leftTerm rightTerm -> (TagHardSubtract, [], [], [leftTerm, rightTerm])
  HardIntersect leftTerm rightTerm -> (TagHardIntersect, [], [], [leftTerm, rightTerm])
  Chamfer radius leftTerm rightTerm -> (TagChamfer, [radius], [], [leftTerm, rightTerm])
  Round radius childTerm -> (TagRound, [radius], [], [childTerm])
  NoisePerturbation noiseParams childTerm -> (TagNoisePerturbation, noiseParamPayload noiseParams, noiseParamIntPayload noiseParams, [childTerm])
  DomainWarp noiseParams childTerm -> (TagDomainWarp, noiseParamPayload noiseParams, noiseParamIntPayload noiseParams, [childTerm])
  Twist rate childTerm -> (TagTwist, [rate], [], [childTerm])
  Bend curvature childTerm -> (TagBend, [curvature], [], [childTerm])
  Onion thickness childTerm -> (TagOnion, [thickness], [], [childTerm])
  Transform affineTransform childTerm -> (TagTransform, affineTransformPayload affineTransform, [], [childTerm])
  Scale scaleVector childTerm -> (TagScale, vec3ToList scaleVector, [], [childTerm])
  Repeat repeatParams childTerm -> (TagRepeat, repeatParamPayload repeatParams, repeatParamIntPayload repeatParams, [childTerm])
  SDFEmpty -> (TagSDFEmpty, [], [], [])

primitiveDoublePayload :: SDFPrimitive -> [Double]
primitiveDoublePayload primitive =
  let (_, doublePayload, _) = primitiveOrderingKey primitive
   in doublePayload

primitiveIntPayload :: SDFPrimitive -> [Int]
primitiveIntPayload primitive =
  let (_, _, intPayload) = primitiveOrderingKey primitive
   in intPayload

noiseParamPayload :: NoiseParams -> [Double]
noiseParamPayload noiseParams = [npFrequency noiseParams, npAmplitude noiseParams]

noiseParamIntPayload :: NoiseParams -> [Int]
noiseParamIntPayload noiseParams = [fromEnum (npKernel noiseParams), npOctaves noiseParams, npSeed noiseParams]

affineTransformPayload :: AffineTransform -> [Double]
affineTransformPayload affineTransform =
  vec3ToList (atTranslation affineTransform)
    <> framePayload (atRotationFrame affineTransform)
    <> vec3ToList (atScale affineTransform)

framePayload :: OrthonormalFrame -> [Double]
framePayload frameValue =
  case orthonormalFrameColumns frameValue of
    (axis1, axis2, axis3) ->
      vec3ToList axis1 <> vec3ToList axis2 <> vec3ToList axis3

repeatParamPayload :: RepeatParams -> [Double]
repeatParamPayload repeatParams = vec3ToList (rpStride repeatParams)

repeatParamIntPayload :: RepeatParams -> [Int]
repeatParamIntPayload repeatParams = maybe [] pure (rpCount repeatParams)

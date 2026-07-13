module Moonlight.Geometry.Site.Parameters
  ( BlendRadius,
    NoiseKernel (..),
    allNoiseKernels,
    RegularityClass (..),
    NoiseParams (..),
    RepeatParams (..),
    RepeatExtent (..),
    neutralNoiseParams,
    isNeutralNoiseParams,
    noiseDisplacementRadius,
    repeatExtent,
    noiseKernelRegularity,
  )
where

import Data.Kind (Type)
import Moonlight.LinAlg.Geometry (Vec3 (..), vec3ToTuple)

type BlendRadius :: Type
type BlendRadius = Double

type NoiseKernel :: Type
data NoiseKernel
  = GradientNoise
  | ValueNoise
  | SimplexNoise
  | WorleyNoise
  | FbmNoise
  | RidgedFbmNoise
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

allNoiseKernels :: [NoiseKernel]
allNoiseKernels = [minBound .. maxBound]

type RegularityClass :: Type
data RegularityClass
  = MerelyContinuous
  | LipschitzRegular
  | C1Regular
  | C2Regular
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

type NoiseParams :: Type
data NoiseParams = NoiseParams
  { npKernel :: !NoiseKernel,
    npFrequency :: !Double,
    npAmplitude :: !Double,
    npOctaves :: !Int,
    npSeed :: !Int
  }
  deriving stock (Eq, Ord, Show, Read)

type RepeatParams :: Type
data RepeatParams = RepeatParams
  { rpStride :: !Vec3,
    rpCount :: !(Maybe Int)
  }
  deriving stock (Eq, Show, Read)

type RepeatExtent :: Type
data RepeatExtent
  = RepeatEmpty
  | RepeatSingleton
  | RepeatFinite !Int
  | RepeatInfinite
  deriving stock (Eq, Ord, Show, Read)

instance Ord RepeatParams where
  compare leftParams rightParams =
    compare
      (vec3ToTuple (rpStride leftParams), rpCount leftParams)
      (vec3ToTuple (rpStride rightParams), rpCount rightParams)

neutralNoiseParams :: NoiseKernel -> NoiseParams
neutralNoiseParams kernel =
  NoiseParams
    { npKernel = kernel,
      npFrequency = 0.0,
      npAmplitude = 0.0,
      npOctaves = 1,
      npSeed = 0
    }

isNeutralNoiseParams :: NoiseParams -> Bool
isNeutralNoiseParams = (== 0.0) . noiseDisplacementRadius

noiseDisplacementRadius :: NoiseParams -> Double
noiseDisplacementRadius noiseParams = abs (npAmplitude noiseParams)

repeatExtent :: RepeatParams -> RepeatExtent
repeatExtent repeatParams =
  case rpCount repeatParams of
    Nothing -> RepeatInfinite
    Just countValue
      | countValue <= 0 -> RepeatEmpty
      | countValue == 1 -> RepeatSingleton
      | otherwise -> RepeatFinite countValue

noiseKernelRegularity :: NoiseKernel -> RegularityClass
noiseKernelRegularity = \case
  GradientNoise -> C1Regular
  ValueNoise -> MerelyContinuous
  SimplexNoise -> C1Regular
  WorleyNoise -> LipschitzRegular
  FbmNoise -> MerelyContinuous
  RidgedFbmNoise -> MerelyContinuous

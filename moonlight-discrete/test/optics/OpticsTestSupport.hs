{-# LANGUAGE OverloadedStrings #-}

module OpticsTestSupport
  ( AnyMReal (..),
    Sample,
    sampleFocusLens,
    sampleManyTraversal,
    sampleFocusGetter,
    sampleFocusFold,
    channelLensProperty,
    deltaAccumulationLaw,
    canonicalAddressingLaw,
    readOpticGetterCoherence,
    restrictionFunctorialProperty,
    restrictionCompatibilityProperty,
    outerIndexedGetter,
    innerIndexedGetter,
    indexedCompositionCoherence,
    indexedHedgehog,
  )
where

import Control.Monad.Writer.Strict (execWriter)
import Data.Kind (Type)
import Data.Text (Text)
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Test.Tasty.QuickCheck as QC
import Veil.Computation.TestSupport.QuickCheck
  ( arbitraryBoundedDoubleWrap,
    shrinkBoundedDoubleWrap,
  )
import Moonlight.Optics (iview, ito, lens, to, traversed, (%), (<%>), (^.), (^..))
import qualified Moonlight.Optics as VO
import Moonlight.Optics.Boundary (emitDelta)
import Moonlight.Optics.Effect.Laws
  ( restrictionCompatibilityLaw,
    restrictionFunctorialLaw,
  )
import Moonlight.Optics.Pure.Delta (DeltaIR (..), mkDeltaOptic)
import Moonlight.Optics.Pure.Path
  ( HasStalk (stalk),
    PathCell (..),
    PathComplex (..),
    PathEdge (..),
    PathFace (..),
    PathVertex (..),
    PathWorldTopology (..),
    complex,
    face,
  )
import Moonlight.Optics.Pure.Restriction (Restriction (..))
import Moonlight.Optics.Pure.Write (planWrite, writeDelta, writeOptic)

type FaceId :: Type
newtype FaceId = FaceId {unFaceId :: Int}
  deriving stock (Eq, Ord, Show)

type MReal :: Type
newtype MReal = MReal {unMReal :: Double}
  deriving stock (Eq, Ord, Show)

type AnyMReal :: Type
newtype AnyMReal = AnyMReal {unwrapMReal :: MReal}
  deriving stock (Eq, Show)

instance QC.Arbitrary AnyMReal where
  arbitrary = AnyMReal <$> arbitraryBoundedDoubleWrap (-1000000) 1000000 MReal
  shrink (AnyMReal value) = fmap AnyMReal (shrinkBoundedDoubleWrap unMReal MReal value)

type Sample :: Type
data Sample = Sample
  { sampleFocus :: Int,
    sampleMany :: [Int],
    sampleVariant :: Maybe Int
  }
  deriving stock (Eq, Show)

instance QC.Arbitrary Sample where
  arbitrary =
    Sample
      <$> QC.arbitrary
      <*> QC.arbitrary
      <*> QC.arbitrary
  shrink (Sample focus many variant) =
    fmap
      (\(nextFocus, nextMany, nextVariant) -> Sample nextFocus nextMany nextVariant)
      (QC.shrink (focus, many, variant))

sampleFocusLens :: VO.Lens' Sample Int
sampleFocusLens =
  lens
    sampleFocus
    (\sampleValue focus -> sampleValue {sampleFocus = focus})

sampleManyLens :: VO.Lens' Sample [Int]
sampleManyLens =
  lens
    sampleMany
    (\sampleValue many -> sampleValue {sampleMany = many})

sampleManyTraversal :: VO.Traversal' Sample Int
sampleManyTraversal = sampleManyLens % traversed

sampleFocusGetter :: VO.Getter Sample Int
sampleFocusGetter = to sampleFocus

sampleFocusFold :: VO.Fold Sample Int
sampleFocusFold = sampleFocusLens % to (\value -> [value]) % VO.folded

type SampleClimate :: Type
data SampleClimate = SampleClimate
  { sampleTemperature :: MReal
  }
  deriving stock (Eq, Show)

type SampleStalk :: Type
data SampleStalk = SampleStalk
  { sampleClimate :: SampleClimate,
    sampleTags :: [Text],
    sampleInfluence :: [MReal],
    samplePopulation :: Integer,
    sampleConstraints :: [Text],
    sampleLore :: [Text]
  }
  deriving stock (Eq, Show)

climate :: VO.Getter SampleStalk SampleClimate
climate = to sampleClimate

temperature :: VO.Getter SampleClimate MReal
temperature = to sampleTemperature

climateCh :: VO.Lens' SampleStalk SampleClimate
climateCh =
  lens
    sampleClimate
    (\stalkValue climateValue -> stalkValue {sampleClimate = climateValue})

channelLensProperty :: AnyMReal -> Bool
channelLensProperty (AnyMReal temperatureValue) =
  let update = SampleClimate temperatureValue
      writeResult = planWrite (writeOptic climateCh) (const update) sampleStalk
   in writeDelta (\_ targetStalk -> targetStalk ^. climate == update) writeResult

readOpticGetterCoherence :: Sample -> Bool
readOpticGetterCoherence sampleValue =
  let optic = VO.readOptic sampleFocusGetter
   in VO.viewRead optic sampleValue == VO.view sampleFocusGetter sampleValue
        && VO.previewRead optic sampleValue == VO.preview sampleFocusGetter sampleValue
        && VO.toListOfRead optic sampleValue == sampleValue ^.. sampleFocusGetter

type FaceLayer :: Type
data FaceLayer = FaceLayer
  { edgeAt :: Int -> EdgeLayer
  }

type EdgeLayer :: Type
data EdgeLayer = EdgeLayer
  { vertexAt :: Int -> VertexLayer
  }

type VertexLayer :: Type
newtype VertexLayer = VertexLayer
  { vertexPayload :: Int
  }
  deriving stock (Eq, Show)

faceRestriction :: Restriction FaceLayer EdgeLayer Int
faceRestriction = Restriction (\edgeKey faceLayer -> edgeAt faceLayer edgeKey)

edgeRestriction :: Restriction EdgeLayer VertexLayer Int
edgeRestriction = Restriction (\vertexKey edgeLayer -> vertexAt edgeLayer vertexKey)

combinedRestriction :: Restriction FaceLayer VertexLayer (Int, Int)
combinedRestriction =
  Restriction
    (\(edgeId, vertexId) faceLayer -> vertexAt (edgeAt faceLayer edgeId) vertexId)

directRestricted :: FaceLayer -> VertexLayer
directRestricted faceLayer =
  vertexAt (edgeAt faceLayer 3) 7

sampleFaceLayer :: FaceLayer
sampleFaceLayer =
  FaceLayer
    { edgeAt =
        \edgeId ->
          EdgeLayer
            { vertexAt = \vertexId -> VertexLayer (edgeId * 100 + vertexId)
            }
    }

restrictionFunctorialProperty :: Bool
restrictionFunctorialProperty =
  restrictionFunctorialLaw faceRestriction edgeRestriction combinedRestriction (,) (3 :: Int) (7 :: Int) sampleFaceLayer

restrictionCompatibilityProperty :: Bool
restrictionCompatibilityProperty =
  restrictionCompatibilityLaw combinedRestriction directRestricted (3, 7) sampleFaceLayer

type SampleCell :: Type
type SampleCell = PathCell SampleStalk

type SampleFace :: Type
type SampleFace = PathFace SampleCell

type SampleEdge :: Type
type SampleEdge = PathEdge SampleCell

type SampleVertex :: Type
type SampleVertex = PathVertex SampleCell

type SampleComplex :: Type
type SampleComplex = PathComplex FaceId Int Int SampleFace SampleEdge SampleVertex

type SampleWorld :: Type
type SampleWorld = PathWorldTopology String SampleComplex

sampleWorld :: SampleWorld
sampleWorld =
  PathWorldTopology
    { worldComplexAt =
        \_ ->
          PathComplex
            { complexFaceAt = \_ -> PathFace (PathCell sampleStalk),
              complexEdgeAt = \_ -> PathEdge (PathCell sampleStalk),
              complexVertexAt = \_ -> PathVertex (PathCell sampleStalk)
            }
    }

sampleStalk :: SampleStalk
sampleStalk =
  SampleStalk
    { sampleClimate = SampleClimate (MReal 273.15),
      sampleTags = ["weather", "stable"],
      sampleInfluence = [MReal 1.0, MReal 2.0],
      samplePopulation = 42,
      sampleConstraints = ["frozen"],
      sampleLore = ["glacial"]
    }

canonicalAddressingLaw :: Bool
canonicalAddressingLaw =
  sampleWorld ^. complex "godrealm-3" % face (FaceId 47) % stalk % climate % temperature
    == MReal 273.15

outerIndexedGetter :: VO.IxGetter Int (Int, (Bool, Char)) (Bool, Char)
outerIndexedGetter = ito (\(outerIndex, innerPair) -> (outerIndex, innerPair))

innerIndexedGetter :: VO.IxGetter Bool (Bool, Char) Char
innerIndexedGetter = ito (\(innerIndex, target) -> (innerIndex, target))

indexedCompositionCoherence :: (Int, (Bool, Char)) -> Bool
indexedCompositionCoherence source =
  iview (outerIndexedGetter <%> innerIndexedGetter) source
    ==
    let (outerIndex, middleValue) = iview outerIndexedGetter source
        (innerIndex, targetValue) = iview innerIndexedGetter middleValue
     in ((outerIndex, innerIndex), targetValue)

type FocusDelta :: Type
data FocusDelta = FocusDelta
  { oldFocus :: Int,
    newFocus :: Int
  }
  deriving stock (Eq, Show)

focusDelta :: Sample -> Sample -> FocusDelta
focusDelta before after =
  FocusDelta
    { oldFocus = sampleFocus before,
      newFocus = sampleFocus after
    }

deltaAccumulationLaw :: Sample -> Bool
deltaAccumulationLaw sampleValue =
  let optic = mkDeltaOptic focusDelta (writeOptic sampleFocusLens)
      deltas =
        execWriter $ do
          emitDelta optic (+ 1) sampleValue
          emitDelta optic (+ 2) sampleValue
   in length (unDeltaIR deltas) == 2

indexedHedgehog :: HH.Property
indexedHedgehog = HH.property $ do
  outerIndex <- HH.forAll (Gen.int (Range.linear 0 100))
  innerIndex <- HH.forAll Gen.bool
  target <- HH.forAll Gen.alphaNum
  let source = (outerIndex, (innerIndex, target))
      expected = ((outerIndex, innerIndex), target)
      received = iview (outerIndexedGetter <%> innerIndexedGetter) source
  HH.assert (expected == received)

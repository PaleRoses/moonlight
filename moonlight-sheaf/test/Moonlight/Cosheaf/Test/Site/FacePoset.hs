{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Cosheaf.Test.Site.FacePoset
  ( Face (..),
    FaceInclusion (..),
    FacePosetSite,
    FacePosetSiteFailure (..),
    facePosetBoundarySite,
    faceCardinality,
    faceVertex,
    faceEdge,
    faceTriangle,
    faceFromVertices,
    faceInclusionMorphism,
  )
where

import Control.Monad (guard)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism (..),
    PullbackSquare (..),
    Site (..),
  )

type Face :: Type
newtype Face = Face
  { faceVertices :: IntSet
  }
  deriving stock (Eq, Ord, Show, Read)

type FaceInclusion :: Type
data FaceInclusion = FaceInclusion
  deriving stock (Eq, Ord, Show, Read)

type FacePosetSite :: Type
data FacePosetSite = FacePosetSite
  { fpsVertexCount :: !Int,
    fpsObjects :: ![Face],
    fpsObjectSet :: !(Set Face)
  }
  deriving stock (Eq, Ord, Show, Read)

type FacePosetSiteFailure :: Type
data FacePosetSiteFailure
  = FacePosetVertexCountTooSmall !Int
  deriving stock (Eq, Ord, Show, Read)

facePosetBoundarySite :: Int -> Either FacePosetSiteFailure FacePosetSite
facePosetBoundarySite vertexCountValue
  | vertexCountValue < 2 =
      Left (FacePosetVertexCountTooSmall vertexCountValue)
  | otherwise =
      let objectsValue =
            nonemptyProperFaces vertexCountValue
       in Right
            FacePosetSite
              { fpsVertexCount = vertexCountValue,
                fpsObjects = objectsValue,
                fpsObjectSet = Set.fromList objectsValue
              }

faceCardinality :: Face -> Int
faceCardinality =
  IntSet.size . faceVertices
{-# INLINE faceCardinality #-}

faceVertex :: Int -> Face
faceVertex =
  faceFromVertices . (: [])
{-# INLINE faceVertex #-}

faceEdge :: Int -> Int -> Face
faceEdge leftVertex rightVertex =
  faceFromVertices [leftVertex, rightVertex]
{-# INLINE faceEdge #-}

faceTriangle :: Int -> Int -> Int -> Face
faceTriangle leftVertex middleVertex rightVertex =
  faceFromVertices [leftVertex, middleVertex, rightVertex]
{-# INLINE faceTriangle #-}

faceFromVertices :: [Int] -> Face
faceFromVertices =
  Face . IntSet.fromList
{-# INLINE faceFromVertices #-}

faceInclusionMorphism ::
  FacePosetSite ->
  Face ->
  Face ->
  Maybe (CheckedMorphism Face FaceInclusion)
faceInclusionMorphism site sourceFace targetFace = do
  guard (faceBelongs site sourceFace)
  guard (faceBelongs site targetFace)
  guard (faceSubsetOf sourceFace targetFace)
  pure (checkedFaceInclusion sourceFace targetFace)

instance Site FacePosetSite where
  type SiteObject FacePosetSite = Face
  type SiteMorphism FacePosetSite = FaceInclusion

  siteObjects =
    fpsObjects

  siteMorphisms site =
    [ checkedFaceInclusion sourceFace targetFace
    | sourceFace <- fpsObjects site,
      targetFace <- fpsObjects site,
      sourceFace /= targetFace,
      faceSubsetOf sourceFace targetFace
    ]

  identityAt _site faceValue =
    checkedFaceInclusion faceValue faceValue

  coversAt _site _objectValue =
    []

  composeChecked site outerMorphism innerMorphism
    | cmSource outerMorphism /= cmTarget innerMorphism =
        Nothing
    | not (faceMorphismBelongs site outerMorphism) =
        Nothing
    | not (faceMorphismBelongs site innerMorphism) =
        Nothing
    | otherwise =
        faceInclusionMorphism site (cmSource innerMorphism) (cmTarget outerMorphism)

  pullbackPair site leftMorphism rightMorphism
    | cmTarget leftMorphism /= cmTarget rightMorphism =
        Nothing
    | not (faceMorphismBelongs site leftMorphism) =
        Nothing
    | not (faceMorphismBelongs site rightMorphism) =
        Nothing
    | otherwise = do
        let apexFace =
              Face
                ( IntSet.intersection
                    (faceVertices (cmSource leftMorphism))
                    (faceVertices (cmSource rightMorphism))
                )
        guard (faceBelongs site apexFace)
        leftLeg <- faceInclusionMorphism site apexFace (cmSource leftMorphism)
        rightLeg <- faceInclusionMorphism site apexFace (cmSource rightMorphism)
        pure
          PullbackSquare
            { psLeftBase = leftMorphism,
              psRightBase = rightMorphism,
              psApex = apexFace,
              psToLeft = leftLeg,
              psToRight = rightLeg
            }

checkedFaceInclusion :: Face -> Face -> CheckedMorphism Face FaceInclusion
checkedFaceInclusion sourceFace targetFace =
  CheckedMorphism
    { cmSource = sourceFace,
      cmTarget = targetFace,
      cmWitness = FaceInclusion
    }
{-# INLINE checkedFaceInclusion #-}

faceSubsetOf :: Face -> Face -> Bool
faceSubsetOf sourceFace targetFace =
  faceVertices sourceFace `IntSet.isSubsetOf` faceVertices targetFace
{-# INLINE faceSubsetOf #-}

faceBelongs :: FacePosetSite -> Face -> Bool
faceBelongs site faceValue =
  Set.member faceValue (fpsObjectSet site)
{-# INLINE faceBelongs #-}

faceMorphismBelongs ::
  FacePosetSite ->
  CheckedMorphism Face FaceInclusion ->
  Bool
faceMorphismBelongs site morphismValue =
  faceBelongs site (cmSource morphismValue)
    && faceBelongs site (cmTarget morphismValue)
    && faceSubsetOf (cmSource morphismValue) (cmTarget morphismValue)
{-# INLINE faceMorphismBelongs #-}

nonemptyProperFaces :: Int -> [Face]
nonemptyProperFaces vertexCountValue =
  Set.toAscList . Set.fromList $
    [ Face subset
    | subset <- subsetsOfIntSet universeVertices,
      not (IntSet.null subset),
      subset /= universeVertices
    ]
  where
    universeVertices =
      IntSet.fromAscList [0 .. vertexCountValue - 1]

subsetsOfIntSet :: IntSet -> [IntSet]
subsetsOfIntSet =
  foldl' extendSubsets [IntSet.empty] . IntSet.toAscList
  where
    extendSubsets :: [IntSet] -> Int -> [IntSet]
    extendSubsets subsets vertexValue =
      subsets <> fmap (IntSet.insert vertexValue) subsets

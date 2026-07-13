{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Derived.Pure.Functor.Tensor.Layout
  ( ExpandedDiff (..)
  , ExpandedComplex (..)
  , PairKey
  , PairInstance (..)
  , SummandInstance (..)
  , SummandKey (..)
  , DegreeLayout (..)
  , RestrictionKey (..)
  , RestrictionCache
  , TensorLayoutInput (..)
  , tensorLayoutInput
  , tensorPairInput
  , expandedComplex
  , expandedDegreeCount
  , expandedBasisCellCount
  , supportPresentationCache
  , maxTotalDegree
  , summandsAtDegree
  , summandsForPair
  , buildDegreeLayout
  , summandKey
  , incrementSupportDegree
  , axisAtDegree
  , lookupAxisNode
  , lookupSummandOffset
  , lookupVector
  , labelsAtDegree
  , nonZeroColumnEntriesAt
  , nonZeroColumnEntries
  , lookupSupportPresentation
  , lookupRestrictionSparse
  , lookupRestrictionDense
  , sumVector
  ) where

import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Vector (Vector)
import Data.Vector qualified as V
import Moonlight.Core (MoonlightError (..))
import Moonlight.Derived.Pure.Functor.ClosedSupport.Geometry
  ( supportIntersection
  )
import Moonlight.Derived.Pure.Functor.Tensor.SupportPresentation
  ( TensorSupportPresentation (..)
  , tensorSupportPresentation
  , tensorSupportRestrictionSparse
  )
import Moonlight.Derived.Pure.Site.InjectiveComplex
  ( InjectiveComplex (..)
  , complexObjectAxes
  )
import Moonlight.Derived.Pure.Site.LabeledMatrix
  ( BlockedMat
  , DenseMat (..)
  , SparseMat
  , axisLabelsExpanded
  , blockedToSparseMat
  , gaOrder
  , matIndex
  , sparseMatColumnEntries
  , sparseMatToDense
  )
import Moonlight.Derived.Pure.Site.Poset
  ( DerivedPoset
  , FinObjectId
  )

type ExpandedDiff :: Type -> Type
data ExpandedDiff a = ExpandedDiff
  { edColumns :: !(Vector [(Int, a)])
  }

type ExpandedComplex :: Type -> Type
data ExpandedComplex a = ExpandedComplex
  { ecAxes :: !(Vector (Vector FinObjectId))
  , ecDiffs :: !(Vector (ExpandedDiff a))
  }

type PairKey :: Type
type PairKey = (FinObjectId, FinObjectId)

type PairInstance :: Type -> Type
data PairInstance a = PairInstance
  { piLeftDegree :: !Int
  , piLeftBasis :: !Int
  , piLeftNode :: !FinObjectId
  , piRightDegree :: !Int
  , piRightBasis :: !Int
  , piRightNode :: !FinObjectId
  , piSupportKey :: !PairKey
  , piSupport :: !(TensorSupportPresentation a)
  }

type SummandInstance :: Type -> Type
data SummandInstance a = SummandInstance
  { siPair :: !(PairInstance a)
  , siSupportDegree :: !Int
  , siAxisLabels :: !(Vector FinObjectId)
  }

type SummandKey :: Type
data SummandKey = SummandKey
  { skLeftDegree :: !Int
  , skLeftBasis :: !Int
  , skRightDegree :: !Int
  , skRightBasis :: !Int
  , skSupportDegree :: !Int
  } deriving stock (Eq, Ord, Show)

type DegreeLayout :: Type
data DegreeLayout = DegreeLayout
  { dlLabels :: !(Vector FinObjectId)
  , dlOffsets :: !(Map SummandKey Int)
  }

type RestrictionKey :: Type
data RestrictionKey = RestrictionKey
  { rkSourcePair :: !PairKey
  , rkTargetPair :: !PairKey
  , rkSupportDegree :: !Int
  } deriving stock (Eq, Ord, Show)

type RestrictionCache :: Type -> Type
type RestrictionCache a = Map RestrictionKey (SparseMat a)

type TensorLayoutInput :: Type -> Type
data TensorLayoutInput a = TensorLayoutInput
  { tliSupportCache :: !(Map PairKey (TensorSupportPresentation a))
  , tliLeftExpanded :: !(ExpandedComplex a)
  , tliRightExpanded :: !(ExpandedComplex a)
  , tliPairInstances :: ![PairInstance a]
  , tliSummandsByDegree :: !(Vector [SummandInstance a])
  , tliLayouts :: !(Vector DegreeLayout)
  , tliStartDegree :: !Int
  }

tensorLayoutInput ::
  (Eq a, Num a) =>
  DerivedPoset ->
  InjectiveComplex a ->
  InjectiveComplex a ->
  Either MoonlightError (TensorLayoutInput a)
tensorLayoutInput posetValue leftComplex rightComplex = do
  leftExpanded <- pure (expandedComplex leftComplex)
  rightExpanded <- pure (expandedComplex rightComplex)
  pairInput <- tensorPairInput posetValue leftComplex rightComplex leftExpanded rightExpanded
  let maxDegreeValue = maxTotalDegree (tliPairInstances pairInput)
      summandsByDegree =
        V.generate
          (maxDegreeValue + 1)
          (summandsAtDegree (tliPairInstances pairInput))
      layouts = V.map buildDegreeLayout summandsByDegree
  pure
    pairInput
      { tliSummandsByDegree = summandsByDegree
      , tliLayouts = layouts
      , tliStartDegree = icStart leftComplex + icStart rightComplex
      }

tensorPairInput ::
  Num a =>
  DerivedPoset ->
  InjectiveComplex a ->
  InjectiveComplex a ->
  ExpandedComplex a ->
  ExpandedComplex a ->
  Either MoonlightError (TensorLayoutInput a)
tensorPairInput posetValue leftComplex rightComplex leftExpanded rightExpanded = do
  supportCache <- supportPresentationCache posetValue leftComplex rightComplex
  pairInstances <- allPairInstances supportCache leftExpanded rightExpanded
  pure
    TensorLayoutInput
      { tliSupportCache = supportCache
      , tliLeftExpanded = leftExpanded
      , tliRightExpanded = rightExpanded
      , tliPairInstances = pairInstances
      , tliSummandsByDegree = V.empty
      , tliLayouts = V.empty
      , tliStartDegree = icStart leftComplex + icStart rightComplex
      }


expandedComplex :: (Eq a, Num a) => InjectiveComplex a -> ExpandedComplex a
expandedComplex injectiveComplex =
  ExpandedComplex
    { ecAxes = V.fromList (fmap axisLabelsExpanded (complexObjectAxes injectiveComplex))
    , ecDiffs =
        V.map
          expandedDiffFromBlocked
          (icDiffs injectiveComplex)
    }

expandedDiffFromBlocked :: (Eq a, Num a) => BlockedMat a -> ExpandedDiff a
expandedDiffFromBlocked blockedMatrix =
  ExpandedDiff
    { edColumns =
        sparseMatColumnEntries
          (blockedToSparseMat blockedMatrix)
    }

supportPresentationCache ::
  Num a =>
  DerivedPoset ->
  InjectiveComplex a ->
  InjectiveComplex a ->
  Either MoonlightError (Map PairKey (TensorSupportPresentation a))
supportPresentationCache posetValue leftComplex rightComplex =
  fmap Map.fromList
    ( traverse
        ( \pairKey@(leftNode, rightNode) ->
            fmap
              ((,) pairKey)
              (supportPresentationForPair posetValue leftNode rightNode)
        )
        uniquePairs
    )
  where
    leftNodes = concatMap (V.toList . gaOrder) (complexObjectAxes leftComplex)
    rightNodes = concatMap (V.toList . gaOrder) (complexObjectAxes rightComplex)
    uniquePairs = Map.keys (Map.fromList [((leftNode, rightNode), ()) | leftNode <- leftNodes, rightNode <- rightNodes])

supportPresentationForPair ::
  Num a =>
  DerivedPoset ->
  FinObjectId ->
  FinObjectId ->
  Either MoonlightError (TensorSupportPresentation a)
supportPresentationForPair posetValue leftNode rightNode =
  supportIntersection posetValue leftNode rightNode
    >>= tensorSupportPresentation posetValue

allPairInstances ::
  Map PairKey (TensorSupportPresentation a) ->
  ExpandedComplex a ->
  ExpandedComplex a ->
  Either MoonlightError [PairInstance a]
allPairInstances supportCache leftExpanded rightExpanded =
  fmap concat
    ( traverse
        leftPairsAtDegree
        (zip [0 :: Int ..] (V.toList (ecAxes leftExpanded)))
    )
  where
    leftPairsAtDegree (leftDegreeValue, leftAxis) =
      fmap concat
        ( traverse
            (uncurry (rightPairsAtDegree leftDegreeValue))
            (zip [0 :: Int ..] (V.toList leftAxis))
        )

    rightPairsAtDegree leftDegreeValue leftBasisValue leftNode =
      fmap concat
        ( traverse
            (uncurry (mkPairsForRightAxis leftDegreeValue leftBasisValue leftNode))
            (zip [0 :: Int ..] (V.toList (ecAxes rightExpanded)))
        )

    mkPairsForRightAxis leftDegreeValue leftBasisValue leftNode rightDegreeValue rightAxis =
      traverse
        (uncurry (mkPairInstance leftDegreeValue leftBasisValue leftNode rightDegreeValue))
        (zip [0 :: Int ..] (V.toList rightAxis))

    mkPairInstance leftDegreeValue leftBasisValue leftNode rightDegreeValue rightBasisValue rightNode = do
      supportPresentation <- lookupSupportPresentation supportCache leftNode rightNode
      let supportKey = (leftNode, rightNode)
      pure
        PairInstance
          { piLeftDegree = leftDegreeValue
          , piLeftBasis = leftBasisValue
          , piLeftNode = leftNode
          , piRightDegree = rightDegreeValue
          , piRightBasis = rightBasisValue
          , piRightNode = rightNode
          , piSupportKey = supportKey
          , piSupport = supportPresentation
          }

lookupSupportPresentation ::
  Map PairKey (TensorSupportPresentation a) ->
  FinObjectId ->
  FinObjectId ->
  Either MoonlightError (TensorSupportPresentation a)
lookupSupportPresentation supportCache leftNode rightNode =
  case Map.lookup (leftNode, rightNode) supportCache of
    Just presentation -> Right presentation
    Nothing ->
      Left
        ( InvariantViolation
            ( "tensorProduct: missing support presentation cache entry for "
                <> show (leftNode, rightNode)
            )
        )

lookupRestrictionSparse ::
  Num a =>
  PairKey ->
  TensorSupportPresentation a ->
  PairKey ->
  TensorSupportPresentation a ->
  Int ->
  RestrictionCache a ->
  Either MoonlightError (SparseMat a, RestrictionCache a)
lookupRestrictionSparse sourceKey sourcePresentation targetKey targetPresentation degreeValue restrictionCache =
  let cacheKey =
        RestrictionKey
          { rkSourcePair = sourceKey
          , rkTargetPair = targetKey
          , rkSupportDegree = degreeValue
          }
   in case Map.lookup cacheKey restrictionCache of
        Just denseValue ->
          Right (denseValue, restrictionCache)
        Nothing -> do
          sparseValue <-
            tensorSupportRestrictionSparse
              sourcePresentation
              targetPresentation
              degreeValue
          pure (sparseValue, Map.insert cacheKey sparseValue restrictionCache)

lookupRestrictionDense ::
  Num a =>
  PairKey ->
  TensorSupportPresentation a ->
  PairKey ->
  TensorSupportPresentation a ->
  Int ->
  RestrictionCache a ->
  Either MoonlightError (DenseMat a, RestrictionCache a)
lookupRestrictionDense sourceKey sourcePresentation targetKey targetPresentation degreeValue restrictionCache = do
  (sparseValue, restrictionCache') <-
    lookupRestrictionSparse
      sourceKey
      sourcePresentation
      targetKey
      targetPresentation
      degreeValue
      restrictionCache
  pure (sparseMatToDense sparseValue, restrictionCache')

maxTotalDegree :: [PairInstance a] -> Int
maxTotalDegree pairInstances =
  maximum
    ( 0
        : fmap
          ( \PairInstance {piLeftDegree, piRightDegree, piSupport = TensorSupportPresentation {tspAxes}} ->
              piLeftDegree + piRightDegree + max 0 (V.length tspAxes - 1)
          )
          pairInstances
    )

summandsAtDegree :: [PairInstance a] -> Int -> [SummandInstance a]
summandsAtDegree pairInstances degreeValue =
  concatMap (summandsForPair degreeValue) pairInstances

summandsForPair :: Int -> PairInstance a -> [SummandInstance a]
summandsForPair degreeValue pairInstance@PairInstance {piLeftDegree, piRightDegree, piSupport = TensorSupportPresentation {tspAxes}} =
  fmap
    ( \(supportDegreeValue, axisLabels) ->
        SummandInstance
          { siPair = pairInstance
          , siSupportDegree = supportDegreeValue
          , siAxisLabels = axisLabels
          }
    )
    ( filter
        (\(supportDegreeValue, axisLabels) -> not (V.null axisLabels) && piLeftDegree + piRightDegree + supportDegreeValue == degreeValue)
        (zip [0 :: Int ..] (V.toList tspAxes))
    )

buildDegreeLayout :: [SummandInstance a] -> DegreeLayout
buildDegreeLayout summands =
  let appendSummand ::
        ([Vector FinObjectId], Map SummandKey Int, Int) ->
        SummandInstance a ->
        ([Vector FinObjectId], Map SummandKey Int, Int)
      appendSummand (labelChunks, offsetMap, currentOffset) summandInstance =
        let summandKeyValue = summandKey summandInstance
            axisLabels = siAxisLabels summandInstance
         in ( axisLabels : labelChunks
            , Map.insert summandKeyValue currentOffset offsetMap
            , currentOffset + V.length axisLabels
            )
      (reversedChunks, finalOffsets, _) =
        foldl' appendSummand ([], Map.empty, 0) summands
   in DegreeLayout
        { dlLabels = V.concat (reverse reversedChunks)
        , dlOffsets = finalOffsets
        }

summandKey :: SummandInstance a -> SummandKey
summandKey SummandInstance {siPair = PairInstance {piLeftDegree, piLeftBasis, piRightDegree, piRightBasis}, siSupportDegree} =
  SummandKey
    { skLeftDegree = piLeftDegree
    , skLeftBasis = piLeftBasis
    , skRightDegree = piRightDegree
    , skRightBasis = piRightBasis
    , skSupportDegree = siSupportDegree
    }


incrementSupportDegree :: SummandInstance a -> SummandKey
incrementSupportDegree summandInstance@SummandInstance {siSupportDegree} =
  (summandKey summandInstance) {skSupportDegree = siSupportDegree + 1}

axisAtDegree :: Int -> TensorSupportPresentation a -> Vector FinObjectId
axisAtDegree degreeValue TensorSupportPresentation {tspAxes} =
  case tspAxes V.!? degreeValue of
    Just axisLabels -> axisLabels
    Nothing -> V.empty

lookupAxisNode :: ExpandedComplex a -> Int -> Int -> Either MoonlightError FinObjectId
lookupAxisNode ExpandedComplex {ecAxes} degreeValue basisValue = do
  axisLabels <- lookupVector "tensorProduct: missing expanded axis" degreeValue ecAxes
  lookupVector "tensorProduct: missing expanded basis label" basisValue axisLabels

lookupSummandOffset :: String -> DegreeLayout -> SummandKey -> Either MoonlightError Int
lookupSummandOffset context DegreeLayout {dlOffsets} keyValue =
  case Map.lookup keyValue dlOffsets of
    Just offsetValue -> Right offsetValue
    Nothing -> Left (InvariantViolation (context <> ": " <> show keyValue))

lookupVector :: String -> Int -> Vector a -> Either MoonlightError a
lookupVector context indexValue vectorValue =
  case vectorValue V.!? indexValue of
    Just value -> Right value
    Nothing -> Left (InvariantViolation (context <> ": index " <> show indexValue <> " is out of bounds"))

labelsAtDegree :: Int -> Vector DegreeLayout -> Vector FinObjectId
labelsAtDegree degreeValue layouts =
  case layouts V.!? degreeValue of
    Just DegreeLayout {dlLabels} -> dlLabels
    Nothing -> V.empty

nonZeroColumnEntriesAt :: Int -> ExpandedComplex a -> Int -> [(Int, a)]
nonZeroColumnEntriesAt columnIndexValue expandedValue degreeValue =
  case ecDiffs expandedValue V.!? degreeValue of
    Nothing -> []
    Just ExpandedDiff {edColumns} ->
      case edColumns V.!? columnIndexValue of
        Nothing -> []
        Just entries -> entries

nonZeroColumnEntries :: (Eq a, Num a) => Int -> DenseMat a -> [(Int, a)]
nonZeroColumnEntries columnIndexValue denseMatrix =
  filter ((/= 0) . snd)
    [ (rowIndexValue, matIndex denseMatrix rowIndexValue columnIndexValue)
    | rowIndexValue <- [0 .. dmRows denseMatrix - 1]
    ]

expandedDegreeCount :: ExpandedComplex a -> Int
expandedDegreeCount =
  V.length . ecAxes

expandedBasisCellCount :: ExpandedComplex a -> Int
expandedBasisCellCount =
  sumVector . V.map V.length . ecAxes

sumVector :: Num a => Vector a -> a
sumVector =
  V.foldl' (+) 0

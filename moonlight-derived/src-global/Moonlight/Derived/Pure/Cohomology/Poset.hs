module Moonlight.Derived.Pure.Cohomology.Poset
  ( PreparedPosetCechResolution
  , preparePosetCechResolution
  , preparedPosetCechComplex
  , preparedPosetSheafCohomology
  , preparedPosetSheafCohomologyDims
  , posetCechComplex
  , posetSheafCohomology
  , posetSheafCohomologyDims
  ) where

import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Vector qualified as V
import Moonlight.Algebra (Semiring)
import Moonlight.Core (scanMap)
import Moonlight.Derived.Pure.Site.Poset (DerivedPoset, FinObjectId)
import Moonlight.Derived.Pure.Site.Poset.OrderComplex
  ( PosetChain
  , PreparedOrderComplex
  , facesOfChain
  , prepareOrderComplex
  , preparedOrderComplexChainsByDegree
  )
import Moonlight.Homology
  ( FiniteChainComplex
  , HomologicalDegree (..)
  , HomologyFailure (..)
  , RepresentativeCocycle
  , cohomologyBasisAt
  , identityBoundaryIncidenceOf
  , mkBoundaryIncidence
  , mkFiniteChainComplexChecked
  )
import Moonlight.Homology
  ( BoundaryEntry, boundaryCoefficient, sourceIndex, targetIndex
  , BoundaryIncidence, boundaryEntries
  , emptyBoundaryIncidence
  , emptyBoundaryIncidenceOf
  , mkBoundaryEntry
  , transposeBoundaryIncidence
  )
import Numeric.Natural (Natural)

type PreparedPosetCechResolution :: Type -> Type
data PreparedPosetCechResolution r = PreparedPosetCechResolution
  { pcrOrderComplex :: !PreparedOrderComplex
  , pcrComplex :: !(FiniteChainComplex r)
  }

preparePosetCechResolution ::
  (Eq r, Num r, Semiring r) =>
  DerivedPoset ->
  (FinObjectId -> Int) ->
  ((FinObjectId, FinObjectId) -> BoundaryIncidence r) ->
  Either HomologyFailure (PreparedPosetCechResolution r)
preparePosetCechResolution posetValue stalkDimension restrictionMap =
  let orderComplexValue = prepareOrderComplex posetValue
      chainsByDegreeValue = preparedOrderComplexChainsByDegree orderComplexValue
      topDegreeValue = max 0 (V.length chainsByDegreeValue - 1)
      boundaryDegrees = [0 .. V.length chainsByDegreeValue]
   in do
        incidences <-
          traverse
            ( \degreeIndex -> do
                incidenceValue <- incidenceAt chainsByDegreeValue degreeIndex
                pure (degreeIndex, incidenceValue)
            )
            boundaryDegrees
        let incidenceByDegree = Map.fromList incidences
        chainComplexValue <-
          mkFiniteChainComplexChecked
            (HomologicalDegree (topDegreeValue + 1))
            (\(HomologicalDegree degreeIndex) -> Map.findWithDefault emptyBoundaryIncidence degreeIndex incidenceByDegree)
        pure
          PreparedPosetCechResolution
            { pcrOrderComplex = orderComplexValue
            , pcrComplex = chainComplexValue
            }
  where
    incidenceAt chainsByDegreeValue degreeIndex
      | degreeIndex <= 0 =
          pure
            ( emptyBoundaryIncidenceOf
                (fromIntegral (degreeCardinality stalkDimension (chainsAt chainsByDegreeValue 0)))
                0
            )
      | degreeIndex <= V.length chainsByDegreeValue =
          fmap
            transposeBoundaryIncidence
            ( cechDifferential
                stalkDimension
                restrictionMap
                (chainsAt chainsByDegreeValue (degreeIndex - 1))
                (chainsAt chainsByDegreeValue degreeIndex)
            )
      | otherwise =
          pure emptyBoundaryIncidence

preparedPosetCechComplex :: PreparedPosetCechResolution r -> FiniteChainComplex r
preparedPosetCechComplex =
  pcrComplex

preparedPosetSheafCohomology ::
  Integral r =>
  PreparedPosetCechResolution r ->
  HomologicalDegree ->
  [RepresentativeCocycle Rational Int]
preparedPosetSheafCohomology preparedResolution degreeValue =
  cohomologyBasisAt (pcrComplex preparedResolution) degreeValue

preparedPosetSheafCohomologyDims ::
  Integral r =>
  PreparedPosetCechResolution r ->
  [Int]
preparedPosetSheafCohomologyDims preparedResolution =
  fmap
    ( \degreeIndex ->
        length
          ( preparedPosetSheafCohomology
              preparedResolution
              (HomologicalDegree degreeIndex)
          )
    )
    [0 .. topDegreeValue]
  where
    topDegreeValue =
      max 0 (V.length (preparedOrderComplexChainsByDegree (pcrOrderComplex preparedResolution)) - 1)

posetCechComplex ::
  (Eq r, Num r, Semiring r) =>
  DerivedPoset ->
  (FinObjectId -> Int) ->
  ((FinObjectId, FinObjectId) -> BoundaryIncidence r) ->
  Either HomologyFailure (FiniteChainComplex r)
posetCechComplex posetValue stalkDimension restrictionMap =
  fmap
    preparedPosetCechComplex
    (preparePosetCechResolution posetValue stalkDimension restrictionMap)

posetSheafCohomology ::
  (Integral r, Semiring r) =>
  DerivedPoset ->
  (FinObjectId -> Int) ->
  ((FinObjectId, FinObjectId) -> BoundaryIncidence r) ->
  HomologicalDegree ->
  Either HomologyFailure [RepresentativeCocycle Rational Int]
posetSheafCohomology posetValue stalkDimension restrictionMap degreeValue =
  fmap
    (`preparedPosetSheafCohomology` degreeValue)
    (preparePosetCechResolution posetValue stalkDimension restrictionMap)

posetSheafCohomologyDims ::
  (Integral r, Semiring r) =>
  DerivedPoset ->
  (FinObjectId -> Int) ->
  ((FinObjectId, FinObjectId) -> BoundaryIncidence r) ->
  Either HomologyFailure [Int]
posetSheafCohomologyDims posetValue stalkDimension restrictionMap =
  fmap
    preparedPosetSheafCohomologyDims
    (preparePosetCechResolution posetValue stalkDimension restrictionMap)

chainsAt :: V.Vector [PosetChain] -> Int -> [PosetChain]
chainsAt chainsByDegreeValue degreeIndex =
  case chainsByDegreeValue V.!? degreeIndex of
    Just chainsValue -> chainsValue
    Nothing -> []

cechDifferential ::
  (Eq r, Num r, Semiring r) =>
  (FinObjectId -> Int) ->
  ((FinObjectId, FinObjectId) -> BoundaryIncidence r) ->
  [PosetChain] ->
  [PosetChain] ->
  Either HomologyFailure (BoundaryIncidence r)
cechDifferential stalkDimension restrictionMap sourceChains targetChains = do
  sourceDimension <-
    naturalDimension
      "Moonlight.Derived.Global.Cohomology.Poset.cechDifferential"
      (degreeCardinality stalkDimension sourceChains)
  targetDimension <-
    naturalDimension
      "Moonlight.Derived.Global.Cohomology.Poset.cechDifferential"
      (degreeCardinality stalkDimension targetChains)
  mergedEntries <-
    fmap
      (fmap toBoundaryEntry . Map.toList . Map.fromListWith (+) . concat)
      (traverse targetChainTerms targetChains)
  firstShapeError (mkBoundaryIncidence sourceDimension targetDimension mergedEntries)
  where
    sourceOffsets = blockOffsets stalkDimension sourceChains
    targetOffsets = blockOffsets stalkDimension targetChains

    targetChainTerms targetChain =
      fmap concat
        ( traverse
            (uncurry (faceTerms targetChain))
            (zip [0 :: Int ..] (facesOfChain targetChain))
        )

    faceTerms targetChain faceIndex sourceChain =
      do
        localIncidence <- faceIncidence targetChain faceIndex
        pure $
          case (Map.lookup sourceChain sourceOffsets, Map.lookup targetChain targetOffsets) of
            (Just sourceOffsetValue, Just targetOffsetValue) ->
              fmap
                (shiftEntry (faceCoefficient faceIndex) sourceOffsetValue targetOffsetValue)
                (boundaryEntries localIncidence)
            _ -> []

    faceIncidence targetChain faceIndex =
      case (faceIndex, targetChain) of
        (0, firstNode : secondNode : _) ->
          boundedIncidence
            (chainCardinality stalkDimension (drop 1 targetChain))
            (chainCardinality stalkDimension targetChain)
            (boundaryEntries (restrictionMap (secondNode, firstNode)))
        (_, firstNode : _) ->
          pure (identityIncidence (stalkDimension firstNode))
        _ ->
          pure emptyBoundaryIncidence

    faceCoefficient :: Num r => Int -> r
    faceCoefficient faceIndex =
      if even faceIndex
        then 1
        else -1

    shiftEntry :: Num r => r -> Int -> Int -> BoundaryEntry r -> ((Int, Int), r)
    shiftEntry coefficientValue sourceOffsetValue targetOffsetValue entryValue =
      ( ( sourceOffsetValue + sourceIndex entryValue
        , targetOffsetValue + targetIndex entryValue
        )
      , coefficientValue * boundaryCoefficient entryValue
      )

    toBoundaryEntry :: ((Int, Int), r) -> BoundaryEntry r
    toBoundaryEntry ((sourceIndexValue, targetIndexValue), coefficientValue) =
      mkBoundaryEntry
        (fromIntegral sourceIndexValue)
        (fromIntegral targetIndexValue)
        coefficientValue

identityIncidence :: Num r => Int -> BoundaryIncidence r
identityIncidence dimensionValue
  | dimensionValue <= 0 = emptyBoundaryIncidence
  | otherwise = identityBoundaryIncidenceOf (fromIntegral dimensionValue)

boundedIncidence :: (Eq r, Semiring r) => Int -> Int -> [BoundaryEntry r] -> Either HomologyFailure (BoundaryIncidence r)
boundedIncidence sourceCardinalityValue targetCardinalityValue entriesValue =
  do
    sourceDimension <-
      naturalDimension
        "Moonlight.Derived.Global.Cohomology.Poset.boundedIncidence"
        sourceCardinalityValue
    targetDimension <-
      naturalDimension
        "Moonlight.Derived.Global.Cohomology.Poset.boundedIncidence"
        targetCardinalityValue
    firstShapeError (mkBoundaryIncidence sourceDimension targetDimension entriesValue)

firstShapeError :: Show errorValue => Either errorValue boundary -> Either HomologyFailure boundary
firstShapeError =
  either (Left . InvalidBoundaryIncidence . show) Right

naturalDimension :: String -> Int -> Either HomologyFailure Natural
naturalDimension context dimensionValue
  | dimensionValue < 0 =
      Left
        ( InvalidBoundaryIncidence
            (context <> ": negative dimension " <> show dimensionValue)
        )
  | otherwise = Right (fromIntegral dimensionValue)

blockOffsets :: (FinObjectId -> Int) -> [PosetChain] -> Map PosetChain Int
blockOffsets stalkDimension chainsValue =
  Map.fromList offsetEntries
  where
    (_, offsetEntries) = scanMap step 0 chainsValue
    step offsetValue chainValue =
      let nextOffsetValue = offsetValue + chainCardinality stalkDimension chainValue
       in (nextOffsetValue, (chainValue, offsetValue))

degreeCardinality :: (FinObjectId -> Int) -> [PosetChain] -> Int
degreeCardinality stalkDimension =
  sum . fmap (chainCardinality stalkDimension)

chainCardinality :: (FinObjectId -> Int) -> PosetChain -> Int
chainCardinality stalkDimension chainValue =
  case chainValue of
    nodeValue : _ -> stalkDimension nodeValue
    [] -> 0

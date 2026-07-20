module Moonlight.Probability.Distribution.Simplex
  ( SimplexWeights,
    SimplexError (..),
    mkSimplexWeights,
    singletonSimplexWeights,
    simplexWeightsToMap,
    emitThresholded,
    blendMixtures,
    pureSimplex,
    simplexFromWeights,
    simplexBlend,
    simplexShannonEntropy,
    simplexSupportSize,
    simplexDominance,
    dominantSimplexKey,
    dominantSimplexEntry,
    simplexTopEntries,
    simplexInterference,
  )
where

import Data.Kind (Type)
import Data.List (sortBy)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..), comparing)
import Moonlight.Core (MoonlightError)
import Moonlight.Core (clampUnitInterval)
import Moonlight.Probability.Core (Prob, mkProb, probOne, probValue, probZero)
import Moonlight.Probability.Core.Internal (Prob (..))
import Prelude

simplexEpsilon :: Double
simplexEpsilon = 1.0e-9

type SimplexError :: Type
data SimplexError
  = SimplexWeightInvalid MoonlightError
  | SimplexWeightsNotNormalized Double
  | SimplexSelectedKeyAbsent
  deriving stock (Eq, Show)

type SimplexWeights :: Type -> Type
newtype SimplexWeights key = SimplexWeights
  { unSimplexWeights :: Map key Prob
  }
  deriving stock (Eq, Show)

mkSimplexWeights :: Map key Double -> Either SimplexError (SimplexWeights key)
mkSimplexWeights weightMap = do
  probabilityMap <- traverse validateWeight weightMap
  let totalWeight = Map.foldl' (\acc p -> acc + probValue p) 0.0 probabilityMap
  if abs (totalWeight - 1.0) > simplexEpsilon
    then Left (SimplexWeightsNotNormalized totalWeight)
    else Right (SimplexWeights probabilityMap)
  where
    validateWeight weightValue =
      case mkProb weightValue of
        Left errorValue -> Left (SimplexWeightInvalid errorValue)
        Right probability -> Right probability

singletonSimplexWeights :: Ord key => NonEmpty key -> key -> Either SimplexError (SimplexWeights key)
singletonSimplexWeights candidates selectedKey =
  if any (== selectedKey) candidates
    then
      Right
        ( SimplexWeights
            ( Map.fromList
                ( fmap
                    (\candidateKey -> (candidateKey, if candidateKey == selectedKey then probOne else probZero))
                    (NonEmpty.toList candidates)
                )
            )
        )
    else Left SimplexSelectedKeyAbsent

simplexWeightsToMap :: SimplexWeights key -> Map key Prob
simplexWeightsToMap = unSimplexWeights

emitThresholded :: Prob -> (key -> value) -> SimplexWeights key -> [value]
emitThresholded threshold project (SimplexWeights m) =
  Map.foldrWithKey'
    (\k v acc -> if v >= threshold then project k : acc else acc)
    []
    m

blendMixtures :: Ord key => NonEmpty (SimplexWeights key) -> SimplexWeights key
blendMixtures simplexValues =
  let totalCount = fromIntegral (NonEmpty.length simplexValues)
      combined =
        Map.unionsWith
          (+)
          (fmap (Map.map probValue . unSimplexWeights) (NonEmpty.toList simplexValues))
   in SimplexWeights (Map.map (\v -> Prob (v / totalCount)) combined)

pureSimplex :: key -> SimplexWeights key
pureSimplex selectedKey = SimplexWeights (Map.singleton selectedKey probOne)

simplexFromWeights :: Ord key => key -> Map key Double -> SimplexWeights key
simplexFromWeights basepoint weights =
  let withBase = Map.insertWith (+) basepoint simplexEpsilon weights
      totalWeight = Map.foldl' (\acc v -> acc + max 0.0 v) 0.0 withBase
  in if totalWeight <= simplexEpsilon || isNaN totalWeight || isInfinite totalWeight
       then pureSimplex basepoint
       else
         let result = Map.mapMaybe (normalizeEntry totalWeight) withBase
          in if Map.null result
               then pureSimplex basepoint
               else SimplexWeights result
  where
    normalizeEntry total v =
      let nv = max 0.0 v / total
       in if nv > simplexEpsilon then Just (Prob nv) else Nothing

simplexBlend :: Ord key => Double -> SimplexWeights key -> SimplexWeights key -> SimplexWeights key
simplexBlend alpha leftWeights rightWeights =
  let blendFactor = clampUnitInterval alpha
      leftMap = unSimplexWeights leftWeights
      rightMap = unSimplexWeights rightWeights
      basepoint = fromMaybe (basepointFallback leftWeights rightWeights) (dominantSimplexKey leftWeights)
      blended = Map.mergeWithKey
        (\_ lv rv -> Just (blendFactor * probValue lv + (1.0 - blendFactor) * probValue rv))
        (Map.map (\lv -> blendFactor * probValue lv))
        (Map.map (\rv -> (1.0 - blendFactor) * probValue rv))
        leftMap rightMap
  in simplexFromWeights basepoint blended

simplexShannonEntropy :: SimplexWeights key -> Double
simplexShannonEntropy (SimplexWeights m) =
  negate
    ( Map.foldl'
        ( \acc v ->
            let p = probValue v
             in if p > simplexEpsilon then acc + p * log p else acc
        )
        0.0
        m
    )

simplexSupportSize :: SimplexWeights key -> Int
simplexSupportSize (SimplexWeights m) =
  Map.foldl' (\acc v -> if probValue v > simplexEpsilon then acc + 1 else acc) 0 m

simplexDominance :: SimplexWeights key -> Double
simplexDominance simplexValue =
  maybe 0.0 snd (dominantSimplexEntry simplexValue)

dominantSimplexKey :: SimplexWeights key -> Maybe key
dominantSimplexKey =
  fmap fst . dominantSimplexEntry

dominantSimplexEntry :: SimplexWeights key -> Maybe (key, Double)
dominantSimplexEntry (SimplexWeights m) =
  Map.foldlWithKey'
    ( \best k v ->
        let val = probValue v
         in case best of
              Nothing -> Just (k, val)
              Just (_, bestVal) -> if val > bestVal then Just (k, val) else best
    )
    Nothing
    m

simplexTopEntries :: SimplexWeights key -> [(key, Double)]
simplexTopEntries (SimplexWeights m) =
  sortBy (comparing (Down . snd))
    (Map.foldrWithKey' (\k v acc -> (k, probValue v) : acc) [] m)

simplexInterference :: Ord key => SimplexWeights key -> SimplexWeights key -> Double
simplexInterference leftWeights rightWeights =
  let leftMap = unSimplexWeights leftWeights
      rightMap = unSimplexWeights rightWeights
      overlap =
        Map.foldlWithKey'
          ( \acc k lv -> case Map.lookup k rightMap of
              Just rv -> acc + min (probValue lv) (probValue rv)
              Nothing -> acc
          )
          0.0
          leftMap
      dominanceGap =
        case dominantSimplexEntry leftWeights of
          Nothing -> 1.0
          Just (leftKey, leftValue) ->
            case dominantSimplexEntry rightWeights of
              Just (rightKey, rightValue)
                | leftKey == rightKey -> abs (leftValue - rightValue)
                | otherwise -> 1.0 - overlap
              Nothing -> 1.0 - overlap
   in clampUnitInterval (0.5 * overlap + 0.5 * (1.0 - dominanceGap))

basepointFallback :: SimplexWeights key -> SimplexWeights key -> key
basepointFallback leftWeights rightWeights =
  case dominantSimplexKey leftWeights of
    Just keyValue -> keyValue
    Nothing ->
      case dominantSimplexKey rightWeights of
        Just keyValue -> keyValue
        Nothing ->
          case Map.lookupMin (simplexWeightsToMap leftWeights) of
            Just (keyValue, _) -> keyValue
            Nothing ->
              case Map.lookupMin (simplexWeightsToMap rightWeights) of
                Just (keyValue, _) -> keyValue
                Nothing -> error "simplexBlend: both simplexes empty"

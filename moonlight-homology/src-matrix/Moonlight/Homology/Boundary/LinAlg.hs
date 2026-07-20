module Moonlight.Homology.Boundary.LinAlg
  ( BoundaryScope (..),
    ScopedBoundary (..),
    BoundaryIncidenceShapeError (..),
    BoundaryEntry,
    sourceIndex,
    targetIndex,
    boundaryCoefficient,
    mkBoundaryEntry,
    mkBoundaryEntryFromInts,
    BoundaryIncidence,
    sourceCardinality,
    targetCardinality,
    boundaryEntries,
    mkBoundaryIncidence,
    mkBoundaryIncidenceFromOrderedEntries,
    overlapBoundaryIncidence,
    emptyBoundaryIncidence,
    emptyBoundaryIncidenceOf,
    identityBoundaryIncidenceOf,
    directSumBoundaryIncidence,
    reindexBoundaryIncidenceWith,
    boundaryIncidenceApply,
    transposeBoundaryIncidence,
    composeBoundaryIncidence,
    boundaryIncidenceDiagonal,
    addBoundaryIncidence,
    mapBoundaryCoefficients,
    BlockBoundaryEntry,
    blockSourceIndex,
    blockTargetIndex,
    blockSubmatrix,
    mkBlockBoundaryEntry,
    BlockBoundaryIncidence,
    blockSourceDimensions,
    blockTargetDimensions,
    blockEntries,
    mkBlockBoundaryIncidence,
    flattenBlockIncidence,
    scaleBoundaryIncidence,
    materializeIncidenceBoundary,
    materializeBoundary,
  )
where

import Data.Function ((&))
import Data.Kind (Type)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Moonlight.Core (AdditiveMonoid (..), Semiring)
import Moonlight.Homology.Pure.Failure (HomologyFailure (..), HomologyLaw (..))
import Numeric.Natural (Natural)
import Moonlight.Pale.Diagnostic.Site.Boundary (BoundaryIncidenceShapeError (..))

type BoundaryScope :: Type
data BoundaryScope
  = IncidenceScope
  | PortalScope
  deriving stock (Eq, Ord, Show)

type ScopedBoundary :: Type -> Type
data ScopedBoundary boundary = ScopedBoundary
  { boundaryScope :: BoundaryScope,
    boundaryValue :: boundary
  }
  deriving stock (Eq, Ord, Show)

type BoundaryEntry :: Type -> Type
data BoundaryEntry r = BoundaryEntry
  { sourceIndex :: Int,
    targetIndex :: Int,
    boundaryCoefficient :: r
  }
  deriving stock (Eq, Show)

mkBoundaryEntry :: Natural -> Natural -> r -> BoundaryEntry r
mkBoundaryEntry sourceIndexValue targetIndexValue coefficientValue =
  BoundaryEntry
    { sourceIndex = fromIntegral sourceIndexValue,
      targetIndex = fromIntegral targetIndexValue,
      boundaryCoefficient = coefficientValue
    }

mkBoundaryEntryFromInts :: Int -> Int -> r -> BoundaryEntry r
mkBoundaryEntryFromInts sourceIndexValue targetIndexValue coefficientValue =
  BoundaryEntry
    { sourceIndex = sourceIndexValue,
      targetIndex = targetIndexValue,
      boundaryCoefficient = coefficientValue
    }

type BoundaryIncidence :: Type -> Type
data BoundaryIncidence r = BoundaryIncidence
  { sourceCardinality :: Int,
    targetCardinality :: Int,
    boundaryEntries :: [BoundaryEntry r]
  }
  deriving stock (Eq, Show)

mkBoundaryIncidence :: (Eq r, Semiring r) => Natural -> Natural -> [BoundaryEntry r] -> Either BoundaryIncidenceShapeError (BoundaryIncidence r)
mkBoundaryIncidence sourceCardinalityValue targetCardinalityValue entries =
  let sourceDimension = fromIntegral sourceCardinalityValue
      targetDimension = fromIntegral targetCardinalityValue
   in case firstOutOfBoundsEntry sourceDimension targetDimension entries of
        Just entryValue -> Left (entryOutOfBoundsError sourceDimension targetDimension entryValue)
        Nothing -> Right (uncheckedBoundaryIncidence sourceDimension targetDimension (canonicalizeEntries entries))

-- | Canonicalize already ordered sparse entries without constructing a map.
-- If the entries are not ordered by @(sourceIndex, targetIndex)@, this falls
-- back to the fully general constructor, preserving the exact semantics of
-- 'mkBoundaryIncidence' while giving inspected/preallocated callers a linear
-- hot path.
mkBoundaryIncidenceFromOrderedEntries :: (Eq r, Semiring r) => Natural -> Natural -> [BoundaryEntry r] -> Either BoundaryIncidenceShapeError (BoundaryIncidence r)
mkBoundaryIncidenceFromOrderedEntries sourceCardinalityValue targetCardinalityValue entries =
  let sourceDimension = fromIntegral sourceCardinalityValue
      targetDimension = fromIntegral targetCardinalityValue
   in case firstOutOfBoundsEntry sourceDimension targetDimension entries of
        Just entryValue -> Left (entryOutOfBoundsError sourceDimension targetDimension entryValue)
        Nothing ->
          case canonicalizeOrderedEntries entries of
            Nothing ->
              mkBoundaryIncidence sourceCardinalityValue targetCardinalityValue entries
            Just canonicalEntries ->
              Right (uncheckedBoundaryIncidence sourceDimension targetDimension canonicalEntries)

overlapBoundaryIncidence :: (Ord atom, Eq r, Semiring r) => r -> [atom] -> [atom] -> BoundaryIncidence r
overlapBoundaryIncidence coefficient sourceBasis targetBasis =
  uncheckedBoundaryIncidence (length sourceBasis) (length targetBasis)
    . canonicalizeEntries
    . Map.elems
    $ Map.intersectionWith
      (\sourceIdx targetIdx -> mkBoundaryEntry sourceIdx targetIdx coefficient)
      (basisIndex sourceBasis)
      (basisIndex targetBasis)

basisIndex :: Ord atom => [atom] -> Map.Map atom Natural
basisIndex =
  Map.fromList . flip zip [0 ..]

canonicalizeEntries :: (Eq r, Semiring r) => [BoundaryEntry r] -> [BoundaryEntry r]
canonicalizeEntries entries =
  entries
    & fmap (\entry -> ((sourceIndex entry, targetIndex entry), boundaryCoefficient entry))
    & Map.fromListWith add
    & Map.toAscList
    & mapMaybe
      ( \((sourceValue, targetValue), coefficientValue) ->
          if coefficientValue == zero
            then Nothing
            else
              Just
                ( mkBoundaryEntry
                    (fromIntegral sourceValue)
                    (fromIntegral targetValue)
                    coefficientValue
                )
      )

data OrderedCanonicalization r
  = OrderedEntriesOutOfOrder
  | OrderedCanonicalization !(Maybe (BoundaryEntry r)) ![BoundaryEntry r]

canonicalizeOrderedEntries :: (Eq r, Semiring r) => [BoundaryEntry r] -> Maybe [BoundaryEntry r]
canonicalizeOrderedEntries entries =
  finalizeOrderedCanonicalization
    ( foldl'
        appendOrderedEntry
        (OrderedCanonicalization Nothing [])
        entries
    )

appendOrderedEntry :: (Eq r, Semiring r) => OrderedCanonicalization r -> BoundaryEntry r -> OrderedCanonicalization r
appendOrderedEntry OrderedEntriesOutOfOrder _ =
  OrderedEntriesOutOfOrder
appendOrderedEntry (OrderedCanonicalization Nothing reversedCanonicalEntries) entry =
  OrderedCanonicalization (Just entry) reversedCanonicalEntries
appendOrderedEntry (OrderedCanonicalization (Just pendingEntry) reversedCanonicalEntries) entry =
  case compareBoundaryEntryCoordinate pendingEntry entry of
    GT ->
      OrderedEntriesOutOfOrder
    EQ ->
      -- Force the summed coefficient: a run of equal-coordinate entries on
      -- this advertised linear hot path must not chain 'add' thunks.
      let !summedCoefficient =
            add
              (boundaryCoefficient pendingEntry)
              (boundaryCoefficient entry)
       in OrderedCanonicalization
            (Just (pendingEntry {boundaryCoefficient = summedCoefficient}))
            reversedCanonicalEntries
    LT ->
      OrderedCanonicalization
        (Just entry)
        (prependNonZeroBoundaryEntry pendingEntry reversedCanonicalEntries)

finalizeOrderedCanonicalization :: (Eq r, Semiring r) => OrderedCanonicalization r -> Maybe [BoundaryEntry r]
finalizeOrderedCanonicalization OrderedEntriesOutOfOrder =
  Nothing
finalizeOrderedCanonicalization (OrderedCanonicalization pendingEntry reversedCanonicalEntries) =
  Just
    ( reverse
        ( maybe
            reversedCanonicalEntries
            (`prependNonZeroBoundaryEntry` reversedCanonicalEntries)
            pendingEntry
        )
    )

prependNonZeroBoundaryEntry :: (Eq r, Semiring r) => BoundaryEntry r -> [BoundaryEntry r] -> [BoundaryEntry r]
prependNonZeroBoundaryEntry entry entries =
  if boundaryCoefficient entry == zero
    then entries
    else entry : entries

compareBoundaryEntryCoordinate :: BoundaryEntry r -> BoundaryEntry r -> Ordering
compareBoundaryEntryCoordinate left right =
  compare
    (sourceIndex left, targetIndex left)
    (sourceIndex right, targetIndex right)


emptyBoundaryIncidence :: BoundaryIncidence r
emptyBoundaryIncidence =
  uncheckedBoundaryIncidence 0 0 []

emptyBoundaryIncidenceOf :: Natural -> Natural -> BoundaryIncidence r
emptyBoundaryIncidenceOf sourceCardinalityValue targetCardinalityValue =
  uncheckedBoundaryIncidence
    (fromIntegral sourceCardinalityValue)
    (fromIntegral targetCardinalityValue)
    []

identityBoundaryIncidenceOf :: Num r => Natural -> BoundaryIncidence r
identityBoundaryIncidenceOf dimensionValue =
  uncheckedBoundaryIncidence dimension dimension entries
  where
    dimension = fromIntegral dimensionValue
    entries =
      fmap
        (\index -> mkBoundaryEntry (fromIntegral index) (fromIntegral index) 1)
        (take dimension [0 :: Int ..])

directSumBoundaryIncidence :: BoundaryIncidence r -> BoundaryIncidence r -> BoundaryIncidence r
directSumBoundaryIncidence left right =
  uncheckedBoundaryIncidence
    (sourceCardinality left + sourceCardinality right)
    (targetCardinality left + targetCardinality right)
    ( boundaryEntries left
        <> fmap
          ( \entry ->
              mkBoundaryEntry
                (fromIntegral (sourceCardinality left + sourceIndex entry))
                (fromIntegral (targetCardinality left + targetIndex entry))
                (boundaryCoefficient entry)
          )
          (boundaryEntries right)
    )

reindexBoundaryIncidenceWith ::
  (Int -> Maybe Natural) ->
  (Int -> Maybe Natural) ->
  (BoundaryEntry a -> Maybe b) ->
  BoundaryIncidence a ->
  BoundaryIncidence b
reindexBoundaryIncidenceWith sourceReindex targetReindex coefficientAt incidence =
  uncheckedBoundaryIncidence
    (mappingDimension sourceReindex (sourceCardinality incidence))
    (mappingDimension targetReindex (targetCardinality incidence))
    ( mapMaybe
        ( \entry ->
            mkBoundaryEntry
              <$> sourceReindex (sourceIndex entry)
              <*> targetReindex (targetIndex entry)
              <*> coefficientAt entry
        )
        (boundaryEntries incidence)
    )

boundaryIncidenceApply :: Num r => BoundaryIncidence r -> Map.Map Int r -> Map.Map Int r
boundaryIncidenceApply incidence vectorValues =
  boundaryEntries incidence
    & fmap
      ( \entry ->
          ( targetIndex entry,
            boundaryCoefficient entry * Map.findWithDefault 0 (sourceIndex entry) vectorValues
          )
      )
    & Map.fromListWith (+)

transposeBoundaryIncidence :: BoundaryIncidence r -> BoundaryIncidence r
transposeBoundaryIncidence incidence =
  uncheckedBoundaryIncidence
    (targetCardinality incidence)
    (sourceCardinality incidence)
    ( boundaryEntries incidence
        & fmap
          ( \entry ->
              mkBoundaryEntry
                (fromIntegral (targetIndex entry))
                (fromIntegral (sourceIndex entry))
                (boundaryCoefficient entry)
          )
    )

composeBoundaryIncidence :: (Eq r, Num r, Semiring r) => BoundaryIncidence r -> BoundaryIncidence r -> Either BoundaryIncidenceShapeError (BoundaryIncidence r)
composeBoundaryIncidence left right =
  if targetCardinality right /= sourceCardinality left
    then
      Left
        ( BoundaryIncidenceShapeMismatch
            (sourceCardinality left)
            (targetCardinality left)
            (sourceCardinality right)
            (targetCardinality right)
        )
    else
      if null (boundaryEntries left) || null (boundaryEntries right)
        then
          Right
            ( emptyBoundaryIncidenceOf
                (fromIntegral (sourceCardinality right))
                (fromIntegral (targetCardinality left))
            )
        else
          let rightByTarget =
                boundaryEntries right
                  & fmap (\entry -> (targetIndex entry, [entry]))
                  & Map.fromListWith (<>)
              leftBySource =
                boundaryEntries left
                  & fmap (\entry -> (sourceIndex entry, [entry]))
                  & Map.fromListWith (<>)
              productTerms =
                Map.intersectionWith (,) rightByTarget leftBySource
                  & Map.elems
                  >>= ( \(rightBucket, leftBucket) ->
                          rightBucket
                            >>= ( \rightEntry ->
                                    leftBucket
                                      & fmap
                                        ( \leftEntry ->
                                            ( (sourceIndex rightEntry, targetIndex leftEntry),
                                              boundaryCoefficient leftEntry * boundaryCoefficient rightEntry
                                            )
                                        )
                                )
                      )
              composedEntries =
                productTerms
                  & Map.fromListWith (+)
                  & Map.toList
                  & fmap
                    ( \((sourceValue, targetValue), coefficientValue) ->
                        mkBoundaryEntry
                          (fromIntegral sourceValue)
                          (fromIntegral targetValue)
                          coefficientValue
                    )
           in mkBoundaryIncidence
                (fromIntegral (sourceCardinality right))
                (fromIntegral (targetCardinality left))
                composedEntries

boundaryIncidenceDiagonal :: Num r => BoundaryIncidence r -> Map.Map Int r
boundaryIncidenceDiagonal incidence =
  boundaryEntries incidence
    & filter (\entry -> sourceIndex entry == targetIndex entry)
    & fmap (\entry -> (sourceIndex entry, boundaryCoefficient entry))
    & Map.fromListWith (+)

addBoundaryIncidence ::
  (Eq r, Num r, Semiring r) =>
  BoundaryIncidence r ->
  BoundaryIncidence r ->
  Either BoundaryIncidenceShapeError (BoundaryIncidence r)
addBoundaryIncidence left right =
  if sourceCardinality left == sourceCardinality right
      && targetCardinality left == targetCardinality right
    then
      let mergedEntries =
            boundaryEntries left
              <> boundaryEntries right
              & fmap
                ( \entry ->
                    ( (sourceIndex entry, targetIndex entry),
                      boundaryCoefficient entry
                    )
                )
              & Map.fromListWith (+)
       in
        mkBoundaryIncidence
          (fromIntegral (sourceCardinality left))
          (fromIntegral (targetCardinality left))
          ( mergedEntries
              & Map.toList
              & fmap
                ( \((sourceValue, targetValue), coefficientValue) ->
                    mkBoundaryEntry
                      (fromIntegral sourceValue)
                      (fromIntegral targetValue)
                      coefficientValue
                )
          )
    else
      Left
        ( BoundaryIncidenceShapeMismatch
            (sourceCardinality left)
            (targetCardinality left)
            (sourceCardinality right)
            (targetCardinality right)
        )

mapBoundaryCoefficients :: (a -> b) -> BoundaryIncidence a -> BoundaryIncidence b
mapBoundaryCoefficients f incidence =
  uncheckedBoundaryIncidence
    (sourceCardinality incidence)
    (targetCardinality incidence)
    ( fmap
        (\entry -> mkBoundaryEntry (fromIntegral (sourceIndex entry)) (fromIntegral (targetIndex entry)) (f (boundaryCoefficient entry)))
        (boundaryEntries incidence)
    )

type BlockBoundaryEntry :: Type -> Type
data BlockBoundaryEntry r = BlockBoundaryEntry
  { blockSourceIndex :: Int,
    blockTargetIndex :: Int,
    blockSubmatrix :: BoundaryIncidence r
  }
  deriving stock (Eq, Show)

mkBlockBoundaryEntry :: Natural -> Natural -> BoundaryIncidence r -> BlockBoundaryEntry r
mkBlockBoundaryEntry blockSourceIndexValue blockTargetIndexValue submatrix =
  BlockBoundaryEntry
    { blockSourceIndex = fromIntegral blockSourceIndexValue,
      blockTargetIndex = fromIntegral blockTargetIndexValue,
      blockSubmatrix = submatrix
    }

type BlockBoundaryIncidence :: Type -> Type
data BlockBoundaryIncidence r = BlockBoundaryIncidence
  { blockSourceDimensions :: [Int],
    blockTargetDimensions :: [Int],
    blockEntries :: [BlockBoundaryEntry r]
  }
  deriving stock (Eq, Show)

mkBlockBoundaryIncidence :: [Natural] -> [Natural] -> [BlockBoundaryEntry r] -> Either BoundaryIncidenceShapeError (BlockBoundaryIncidence r)
mkBlockBoundaryIncidence sourceDimensionsValue targetDimensionsValue entries =
  let sourceDimensions = fmap fromIntegral sourceDimensionsValue
      targetDimensions = fmap fromIntegral targetDimensionsValue
      blockIncidence =
        BlockBoundaryIncidence
          { blockSourceDimensions = sourceDimensions,
            blockTargetDimensions = targetDimensions,
            blockEntries = entries
          }
   in do
        _ <- traverse (validateBlockEntry blockIncidence) entries
        pure blockIncidence

flattenBlockIncidence :: (Eq r, Semiring r) => BlockBoundaryIncidence r -> Either BoundaryIncidenceShapeError (BoundaryIncidence r)
flattenBlockIncidence block =
  let sourceOffsets = prefixSums (blockSourceDimensions block)
      targetOffsets = prefixSums (blockTargetDimensions block)
      totalSourceDim = sum (blockSourceDimensions block)
      totalTargetDim = sum (blockTargetDimensions block)
   in do
        expandedEntries <-
          fmap concat
            (traverse (expandBlockEntry sourceOffsets targetOffsets block) (blockEntries block))
        mkBoundaryIncidence
          (fromIntegral totalSourceDim)
          (fromIntegral totalTargetDim)
          expandedEntries

scaleBoundaryIncidence :: Num r => r -> BoundaryIncidence r -> BoundaryIncidence r
scaleBoundaryIncidence scalar =
  mapBoundaryCoefficients (* scalar)

prefixSums :: [Int] -> [Int]
prefixSums = scanl (+) 0

materializeIncidenceBoundary ::
  (Eq r, Semiring r, Ord target) =>
  (source -> [(r, target)]) ->
  [source] ->
  [target] ->
  Either HomologyFailure (BoundaryIncidence r)
materializeIncidenceBoundary boundaryOf sourceBasis targetBasis =
  materializeBoundary
    (\sourceValue -> boundaryOf sourceValue & fmap (\(coefficientValue, targetValue) -> (coefficientValue, ScopedBoundary IncidenceScope targetValue)))
    sourceBasis
    targetBasis

materializeBoundary ::
  (Eq r, Semiring r, Ord target) =>
  (source -> [(r, ScopedBoundary target)]) ->
  [source] ->
  [target] ->
  Either HomologyFailure (BoundaryIncidence r)
materializeBoundary boundaryOf sourceBasis targetBasis =
  -- The user boundary function is evaluated exactly once per basis element;
  -- the portal-scope gate and the materialization both read the same
  -- precomputed list rather than each paying for a full traversal.
  let boundariesBySource = fmap boundaryOf sourceBasis
      hasPortalTargets =
        boundariesBySource
          & concat
          & any (\(_, scopedTarget) -> boundaryScope scopedTarget == PortalScope)
   in if hasPortalTargets
        then Left (LawViolation IncidenceScopeLaw)
        else materializeIncidence boundariesBySource targetBasis

materializeIncidence ::
  (Eq r, Semiring r, Ord target) =>
  [[(r, ScopedBoundary target)]] ->
  [target] ->
  Either HomologyFailure (BoundaryIncidence r)
materializeIncidence boundariesBySource targetBasis =
  let targetIndexByBasis = Map.fromList (zip targetBasis [0 :: Int ..])
   in do
        entries <-
          fmap concat $
            traverse
              ( \(sourceIndexValue, sourceBoundary) ->
                  traverse
                    ( \(coefficientValue, scopedTarget) ->
                        maybe
                          (Left (InvalidBoundaryIncidence "boundary target is absent from the target basis"))
                          ( \targetIndexValue ->
                              Right
                                ( mkBoundaryEntry
                                    (fromIntegral sourceIndexValue)
                                    (fromIntegral targetIndexValue)
                                    coefficientValue
                                )
                          )
                          (Map.lookup (boundaryValue scopedTarget) targetIndexByBasis)
                    )
                    sourceBoundary
              )
              (zip [0 :: Int ..] boundariesBySource)
        either
          (Left . InvalidBoundaryIncidence . show)
          Right
          ( mkBoundaryIncidence
              (fromIntegral (length boundariesBySource))
              (fromIntegral (length targetBasis))
              entries
          )

uncheckedBoundaryIncidence :: Int -> Int -> [BoundaryEntry r] -> BoundaryIncidence r
uncheckedBoundaryIncidence sourceDimension targetDimension entries =
  BoundaryIncidence
    { sourceCardinality = sourceDimension,
      targetCardinality = targetDimension,
      boundaryEntries = entries
    }

firstOutOfBoundsEntry :: Int -> Int -> [BoundaryEntry r] -> Maybe (BoundaryEntry r)
firstOutOfBoundsEntry sourceDimension targetDimension =
  listToMaybe . filter (not . entryWithinBounds sourceDimension targetDimension)

entryWithinBounds :: Int -> Int -> BoundaryEntry r -> Bool
entryWithinBounds sourceDimension targetDimension entry =
  sourceIndex entry >= 0
    && sourceIndex entry < sourceDimension
    && targetIndex entry >= 0
    && targetIndex entry < targetDimension

entryOutOfBoundsError :: Int -> Int -> BoundaryEntry r -> BoundaryIncidenceShapeError
entryOutOfBoundsError sourceDimension targetDimension entry =
  BoundaryIncidenceEntryOutOfBounds
    (sourceIndex entry)
    (targetIndex entry)
    sourceDimension
    targetDimension

mappingDimension :: (Int -> Maybe Natural) -> Int -> Int
mappingDimension reindex dimension =
  [0 .. dimension - 1]
    & fmap reindex
    & mapMaybe (fmap fromIntegral)
    & maximumMaybe
    & maybe 0 (+ 1)

maximumMaybe :: Ord a => [a] -> Maybe a
maximumMaybe =
  foldr
    ( \value ->
        Just
          . maybe value (max value)
    )
    Nothing

validateBlockEntry :: BlockBoundaryIncidence r -> BlockBoundaryEntry r -> Either BoundaryIncidenceShapeError (BlockBoundaryEntry r)
validateBlockEntry blockIncidence blockEntry = do
  expectedSourceDim <-
    dimensionAt
      (blockSourceDimensions blockIncidence)
      (blockSourceIndex blockEntry)
      (blockIndexError blockIncidence blockEntry)
  expectedTargetDim <-
    dimensionAt
      (blockTargetDimensions blockIncidence)
      (blockTargetIndex blockEntry)
      (blockIndexError blockIncidence blockEntry)
  let submatrix = blockSubmatrix blockEntry
  if sourceCardinality submatrix /= expectedSourceDim || targetCardinality submatrix /= expectedTargetDim
    then
      Left
        ( BoundaryIncidenceBlockShapeMismatch
            expectedSourceDim
            expectedTargetDim
            (sourceCardinality submatrix)
            (targetCardinality submatrix)
        )
    else pure blockEntry

blockIndexError :: BlockBoundaryIncidence r -> BlockBoundaryEntry r -> BoundaryIncidenceShapeError
blockIndexError blockIncidence blockEntry =
  BoundaryIncidenceEntryOutOfBounds
    (blockSourceIndex blockEntry)
    (blockTargetIndex blockEntry)
    (length (blockSourceDimensions blockIncidence))
    (length (blockTargetDimensions blockIncidence))

expandBlockEntry :: [Int] -> [Int] -> BlockBoundaryIncidence r -> BlockBoundaryEntry r -> Either BoundaryIncidenceShapeError [BoundaryEntry r]
expandBlockEntry sourceOffsets targetOffsets blockIncidence blockEntry = do
  _ <- validateBlockEntry blockIncidence blockEntry
  sourceOffset <- offsetAt sourceOffsets (blockSourceIndex blockEntry) (blockIndexError blockIncidence blockEntry)
  targetOffset <- offsetAt targetOffsets (blockTargetIndex blockEntry) (blockIndexError blockIncidence blockEntry)
  traverse (expandScalarEntry sourceOffset targetOffset (blockSubmatrix blockEntry)) (boundaryEntries (blockSubmatrix blockEntry))

dimensionAt :: [Int] -> Int -> errorValue -> Either errorValue Int
dimensionAt dimensions idx errorValue
  | idx < 0 = Left errorValue
  | otherwise =
      case drop idx dimensions of
        dimensionValue : _ -> Right dimensionValue
        [] -> Left errorValue

offsetAt :: [Int] -> Int -> errorValue -> Either errorValue Int
offsetAt offsets idx errorValue
  | idx < 0 = Left errorValue
  | otherwise =
      case drop idx offsets of
        offsetValue : remainingOffsets ->
          if null remainingOffsets
            then Left errorValue
            else Right offsetValue
        [] -> Left errorValue

expandScalarEntry :: Int -> Int -> BoundaryIncidence r -> BoundaryEntry r -> Either BoundaryIncidenceShapeError (BoundaryEntry r)
expandScalarEntry sourceOffset targetOffset submatrix scalarEntry =
  if entryWithinBounds (sourceCardinality submatrix) (targetCardinality submatrix) scalarEntry
    then
      Right
        ( mkBoundaryEntry
            (fromIntegral (sourceIndex scalarEntry + sourceOffset))
            (fromIntegral (targetIndex scalarEntry + targetOffset))
            (boundaryCoefficient scalarEntry)
        )
    else Left (entryOutOfBoundsError (sourceCardinality submatrix) (targetCardinality submatrix) scalarEntry)

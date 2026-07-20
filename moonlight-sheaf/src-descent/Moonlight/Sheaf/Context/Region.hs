{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- | Alexandrov opens over a prepared context poset in two arms behind one abstract type: bit-packed opens for materialized sites, symbolic subcube unions over atom bitmasks for implicit powerset sites.
module Moonlight.Sheaf.Context.Region
  ( ContextRegion,
    RegionTable,
    regionTableObjectCount,
    regionTop,
    regionVoid,
    regionAtKey,
    regionFromKeys,
    regionKeys,
    regionMemberKey,
    regionMeet,
    regionJoin,
    regionComplementIn,
    regionDifference,
    regionEmpty,
    regionEntails,
    regionSize,
    regionIsOpen,
    regionIsDownClosed,
    regionGeneratorKeys,
    regionCubeCount,
    fromGeneratorKeys,
  )
where

import Data.Bits
  ( bit,
    complement,
    countTrailingZeros,
    popCount,
    setBit,
    shiftR,
    testBit,
    xor,
    (.&.),
    (.|.),
  )
import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet
  ( IntSet,
  )
import Data.IntSet qualified as IntSet
import Data.List
  ( group,
    sort,
  )
import Numeric.Natural
  ( Natural,
  )
import Moonlight.Sheaf.Context.Region.Internal
  ( ContextObjectKey (..),
    ContextRegion (..),
    DenseRegionTable (..),
    packKeySet,
    RegionCube (..),
    RegionTable (..),
  )

regionTableObjectCount :: RegionTable owner -> Int
regionTableObjectCount table =
  case table of
    RegionTableDense denseTable -> drtObjectCount denseTable
    RegionTablePowerset atomCount -> bit atomCount
{-# INLINE regionTableObjectCount #-}

regionTop :: RegionTable owner -> ContextRegion owner
regionTop table =
  case table of
    RegionTableDense denseTable -> drtTop denseTable
    RegionTablePowerset atomCount -> RegionCubes [RegionCube 0 (universeMaskOf atomCount)]
{-# INLINE regionTop #-}

regionVoid :: ContextRegion owner
regionVoid =
  RegionVoid
{-# INLINE regionVoid #-}

regionAtKey :: RegionTable owner -> ContextObjectKey owner -> ContextRegion owner
regionAtKey table (ContextObjectKey keyValue) =
  case table of
    RegionTableDense denseTable ->
      IntMap.findWithDefault
        (RegionDense (bit keyValue))
        keyValue
        (drtUpsetByKey denseTable)
    RegionTablePowerset atomCount ->
      RegionCubes
        [RegionCube (keyValue .&. universeMaskOf atomCount) (universeMaskOf atomCount)]
{-# INLINE regionAtKey #-}

regionFromKeys :: RegionTable owner -> [ContextObjectKey owner] -> ContextRegion owner
regionFromKeys table keys =
  case table of
    RegionTableDense _ ->
      packKeySet (IntSet.fromList (fmap contextObjectKeyValue keys))
    RegionTablePowerset atomCount ->
      mkCubes
        [ RegionCube
            (keyValue .&. universeMaskOf atomCount)
            (keyValue .&. universeMaskOf atomCount)
        | ContextObjectKey keyValue <- keys
        ]
{-# INLINE regionFromKeys #-}

regionKeys :: RegionTable owner -> ContextRegion owner -> [ContextObjectKey owner]
regionKeys table region =
  case table of
    RegionTableDense denseTable ->
      [ ContextObjectKey keyValue
        | keyValue <- [0 .. drtObjectCount denseTable - 1],
          regionMemberKey region (ContextObjectKey keyValue)
      ]
    RegionTablePowerset _ ->
      fmap ContextObjectKey (IntSet.toAscList (regionKeySet table region))
{-# INLINE regionKeys #-}

regionKeySet :: RegionTable owner -> ContextRegion owner -> IntSet
regionKeySet table region =
  case table of
    RegionTableDense _ ->
      IntSet.fromDistinctAscList (fmap contextObjectKeyValue (regionKeys table region))
    RegionTablePowerset _ ->
      cubesKeySet (regionCubesIn table region)
{-# INLINE regionKeySet #-}

regionMemberKey :: ContextRegion owner -> ContextObjectKey owner -> Bool
regionMemberKey region (ContextObjectKey keyValue) =
  case region of
    RegionVoid -> False
    RegionDense bits -> testBit bits keyValue
    RegionCubes cubes -> any (cubeMemberKey keyValue) cubes
{-# INLINE regionMemberKey #-}

regionMeet :: ContextRegion owner -> ContextRegion owner -> ContextRegion owner
regionMeet leftRegion rightRegion =
  case (leftRegion, rightRegion) of
    (RegionVoid, _) -> RegionVoid
    (_, RegionVoid) -> RegionVoid
    (RegionDense leftBits, RegionDense rightBits) -> mkDense (leftBits .&. rightBits)
    _ -> mkCubes (cubesMeet (promoteToCubes leftRegion) (promoteToCubes rightRegion))
{-# INLINE regionMeet #-}

regionJoin :: ContextRegion owner -> ContextRegion owner -> ContextRegion owner
regionJoin leftRegion rightRegion =
  case (leftRegion, rightRegion) of
    (RegionVoid, _) -> rightRegion
    (_, RegionVoid) -> leftRegion
    (RegionDense leftBits, RegionDense rightBits) -> RegionDense (leftBits .|. rightBits)
    _ -> mkCubes (promoteToCubes leftRegion <> promoteToCubes rightRegion)
{-# INLINE regionJoin #-}

-- | Complement within the table's key universe. Sends opens to down-closed
-- regions and back; the receiving vocabulary for negative guard atoms.
regionComplementIn :: RegionTable owner -> ContextRegion owner -> ContextRegion owner
regionComplementIn table region =
  case table of
    RegionTableDense denseTable ->
      mkDense (denseTopBits denseTable `xor` denseBitsIn denseTable region)
    RegionTablePowerset atomCount ->
      mkCubes
        ( cubesComplementWithin
            (universeMaskOf atomCount)
            (regionCubesIn table region)
        )
{-# INLINE regionComplementIn #-}

-- | The part of the first region not covered by the second. Unlike set
-- subtraction by enumerated context keys, this preserves the symbolic
-- powerset representation.
regionDifference :: RegionTable owner -> ContextRegion owner -> ContextRegion owner -> ContextRegion owner
regionDifference table wholeRegion removedRegion =
  regionMeet wholeRegion (regionComplementIn table removedRegion)
{-# INLINE regionDifference #-}

regionEmpty :: ContextRegion owner -> Bool
regionEmpty region =
  case region of
    RegionVoid -> True
    RegionDense bits -> bits == 0
    RegionCubes cubes -> null cubes
{-# INLINE regionEmpty #-}

regionEntails :: ContextRegion owner -> ContextRegion owner -> Bool
regionEntails narrowRegion wideRegion =
  case (narrowRegion, wideRegion) of
    (RegionVoid, _) -> True
    (_, RegionVoid) -> regionEmpty narrowRegion
    (RegionDense narrowBits, RegionDense wideBits) ->
      narrowBits .&. wideBits == narrowBits
    _ -> cubesEntail (promoteToCubes narrowRegion) (promoteToCubes wideRegion)
{-# INLINE regionEntails #-}

regionSize :: ContextRegion owner -> Int
regionSize region =
  case region of
    RegionVoid -> 0
    RegionDense bits -> popCount bits
    RegionCubes cubes -> cubesSize cubes
{-# INLINE regionSize #-}

regionIsOpen :: RegionTable owner -> ContextRegion owner -> Bool
regionIsOpen table region =
  all
    (\keyValue -> regionEntails (regionAtKey table keyValue) region)
    (regionKeys table region)
{-# INLINE regionIsOpen #-}

regionIsDownClosed :: RegionTable owner -> ContextRegion owner -> Bool
regionIsDownClosed table region =
  all
    (\keyValue -> regionEntails (strictLowerAtKey table keyValue) region)
    (regionKeys table region)
{-# INLINE regionIsDownClosed #-}

regionGeneratorKeys :: RegionTable owner -> ContextRegion owner -> [ContextObjectKey owner]
regionGeneratorKeys table region =
  case table of
    RegionTableDense _ ->
      [ keyValue
        | keyValue <- regionKeys table region,
          regionEmpty (regionMeet (strictLowerAtKey table keyValue) region)
      ]
    RegionTablePowerset _ ->
      fmap ContextObjectKey (minimalMasks (fmap regionCubeMust (regionCubesIn table region)))
{-# INLINE regionGeneratorKeys #-}

regionCubeCount :: RegionTable owner -> ContextRegion owner -> Int
regionCubeCount table region =
  length (regionCubesIn table region)
{-# INLINE regionCubeCount #-}

fromGeneratorKeys :: RegionTable owner -> [ContextObjectKey owner] -> ContextRegion owner
fromGeneratorKeys table =
  foldl' (\region keyValue -> regionJoin region (regionAtKey table keyValue)) regionVoid
{-# INLINE fromGeneratorKeys #-}

strictLowerAtKey :: RegionTable owner -> ContextObjectKey owner -> ContextRegion owner
strictLowerAtKey table (ContextObjectKey keyValue) =
  case table of
    RegionTableDense denseTable ->
      IntMap.findWithDefault regionVoid keyValue (drtStrictLowerByKey denseTable)
    RegionTablePowerset atomCount ->
      mkCubes
        [ RegionCube 0 (maskValue .&. complement (bit atomIndex))
          | let maskValue = keyValue .&. universeMaskOf atomCount,
            atomIndex <- intBitIndices maskValue
        ]
{-# INLINE strictLowerAtKey #-}

mkDense :: Natural -> ContextRegion owner
mkDense bits
  | bits == 0 = RegionVoid
  | otherwise = RegionDense bits
{-# INLINE mkDense #-}

mkCubes :: [RegionCube] -> ContextRegion owner
mkCubes cubes =
  case pruneCubes (filter cubeValid cubes) of
    [] -> RegionVoid
    prunedCubes -> RegionCubes (sort prunedCubes)
{-# INLINE mkCubes #-}

promoteToCubes :: ContextRegion owner -> [RegionCube]
promoteToCubes region =
  case region of
    RegionVoid -> []
    RegionDense bits -> [RegionCube keyValue keyValue | keyValue <- naturalBitIndices bits]
    RegionCubes cubes -> cubes

regionCubesIn :: RegionTable owner -> ContextRegion owner -> [RegionCube]
regionCubesIn table region =
  case table of
    RegionTablePowerset atomCount ->
      fmap
        ( \cube ->
            RegionCube
              (regionCubeMust cube .&. universeMaskOf atomCount)
              (regionCubeMay cube .&. universeMaskOf atomCount)
        )
        (promoteToCubes region)
    RegionTableDense _ ->
      promoteToCubes region

denseBitsIn :: DenseRegionTable owner -> ContextRegion owner -> Natural
denseBitsIn denseTable region =
  case region of
    RegionVoid -> 0
    RegionDense bits -> bits
    RegionCubes _ ->
      foldl'
        setBit
        0
        [ keyValue
          | keyValue <- [0 .. drtObjectCount denseTable - 1],
            regionMemberKey region (ContextObjectKey keyValue)
        ]

denseTopBits :: DenseRegionTable owner -> Natural
denseTopBits denseTable =
  case drtTop denseTable of
    RegionDense bits -> bits
    _ -> 0
{-# INLINE denseTopBits #-}

universeMaskOf :: Int -> Int
universeMaskOf atomCount =
  bit atomCount - 1
{-# INLINE universeMaskOf #-}

maskSubset :: Int -> Int -> Bool
maskSubset narrowMask wideMask =
  narrowMask .&. wideMask == narrowMask
{-# INLINE maskSubset #-}

cubeValid :: RegionCube -> Bool
cubeValid cube =
  maskSubset (regionCubeMust cube) (regionCubeMay cube)
{-# INLINE cubeValid #-}

cubeContainsCube :: RegionCube -> RegionCube -> Bool
cubeContainsCube outerCube innerCube =
  maskSubset (regionCubeMust outerCube) (regionCubeMust innerCube)
    && maskSubset (regionCubeMay innerCube) (regionCubeMay outerCube)
{-# INLINE cubeContainsCube #-}

cubeMeetMaybe :: RegionCube -> RegionCube -> Maybe RegionCube
cubeMeetMaybe leftCube rightCube =
  let mustMask = regionCubeMust leftCube .|. regionCubeMust rightCube
      mayMask = regionCubeMay leftCube .&. regionCubeMay rightCube
   in if maskSubset mustMask mayMask
        then Just (RegionCube mustMask mayMask)
        else Nothing
{-# INLINE cubeMeetMaybe #-}

cubeMemberKey :: Int -> RegionCube -> Bool
cubeMemberKey keyValue cube =
  maskSubset (regionCubeMust cube) keyValue
    && maskSubset keyValue (regionCubeMay cube)
{-# INLINE cubeMemberKey #-}

cubeSize :: RegionCube -> Int
cubeSize cube =
  bit (popCount (regionCubeMay cube .&. complement (regionCubeMust cube)))
{-# INLINE cubeSize #-}

pruneCubes :: [RegionCube] -> [RegionCube]
pruneCubes =
  foldl' absorb []
  where
    absorb keptCubes candidateCube
      | any (`cubeContainsCube` candidateCube) keptCubes = keptCubes
      | otherwise =
          candidateCube : filter (not . cubeContainsCube candidateCube) keptCubes

cubesMeet :: [RegionCube] -> [RegionCube] -> [RegionCube]
cubesMeet leftCubes rightCubes =
  pruneCubes
    [ metCube
      | leftCube <- leftCubes,
        rightCube <- rightCubes,
        Just metCube <- [cubeMeetMaybe leftCube rightCube]
    ]

cubeComplementWithin :: Int -> RegionCube -> [RegionCube]
cubeComplementWithin universeMask cube =
  pruneCubes
    ( [ RegionCube 0 (universeMask .&. complement (bit atomIndex))
        | atomIndex <- intBitIndices (regionCubeMust cube)
      ]
        <> [ RegionCube (bit atomIndex) universeMask
             | atomIndex <- intBitIndices (universeMask .&. complement (regionCubeMay cube))
           ]
    )

cubesComplementWithin :: Int -> [RegionCube] -> [RegionCube]
cubesComplementWithin universeMask =
  foldl'
    (\accumulated cube -> cubesMeet accumulated (cubeComplementWithin universeMask cube))
    [RegionCube 0 universeMask]

cubesSpan :: [RegionCube] -> Int
cubesSpan =
  foldl' (\accumulated cube -> accumulated .|. regionCubeMay cube) 0

cubesEntail :: [RegionCube] -> [RegionCube] -> Bool
cubesEntail narrowCubes wideCubes =
  all covered narrowCubes
  where
    spanMask = cubesSpan narrowCubes .|. cubesSpan wideCubes
    outside = cubesComplementWithin spanMask wideCubes
    covered cube = null (cubesMeet [cube] outside)

cubesSize :: [RegionCube] -> Int
cubesSize cubes =
  go cubes
  where
    spanMask = cubesSpan cubes
    go [] = 0
    go (cube : rest) =
      cubeSize cube + go (cubesMeet rest (cubeComplementWithin spanMask cube))

cubesKeySet :: [RegionCube] -> IntSet
cubesKeySet cubes =
  IntSet.unions
    [ IntSet.fromList
        [ regionCubeMust cube .|. subMask
          | subMask <- submasksOf (regionCubeMay cube .&. complement (regionCubeMust cube))
        ]
      | cube <- cubes
    ]

minimalMasks :: [Int] -> [Int]
minimalMasks masks =
  [ candidate
    | candidate <- uniqueMasks,
      all
        (\other -> other == candidate || not (maskSubset other candidate))
        uniqueMasks
  ]
  where
    uniqueMasks = concatMap (take 1) (group (sort masks))

intBitIndices :: Int -> [Int]
intBitIndices maskValue
  | maskValue == 0 = []
  | otherwise =
      countTrailingZeros maskValue : intBitIndices (maskValue .&. (maskValue - 1))

naturalBitIndices :: Natural -> [Int]
naturalBitIndices =
  go 0
  where
    go _ 0 = []
    go bitIndex bits
      | testBit bits 0 = bitIndex : go (bitIndex + 1) (shiftR bits 1)
      | otherwise = go (bitIndex + 1) (shiftR bits 1)

submasksOf :: Int -> [Int]
submasksOf freeMask =
  go freeMask
  where
    go 0 = [0]
    go subMask = subMask : go ((subMask - 1) .&. freeMask)

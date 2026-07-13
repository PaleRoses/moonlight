{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- | Alexandrov opens over a prepared context poset in two arms behind one abstract type: bit-packed opens for materialized sites, symbolic subcube unions over atom bitmasks for implicit powerset sites.
module Moonlight.Sheaf.Context.Region
  ( ContextRegion,
    RegionTable,
    regionTableFromUpsets,
    powersetRegionTable,
    regionTableObjectCount,
    regionTop,
    regionVoid,
    regionAtKey,
    regionFromKeys,
    regionKeys,
    regionKeySet,
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
import Data.Kind
  ( Type,
  )
import Data.List
  ( group,
    sort,
  )
import Numeric.Natural
  ( Natural,
  )

type RegionCube :: Type
data RegionCube = RegionCube
  { regionCubeMust :: !Int,
    regionCubeMay :: !Int
  }
  deriving stock (Eq, Ord, Show)

type ContextRegion :: Type
data ContextRegion
  = RegionVoid
  | RegionDense !Natural
  | RegionCubes ![RegionCube]
  deriving stock (Eq, Ord, Show)

type DenseRegionTable :: Type
data DenseRegionTable = DenseRegionTable
  { drtObjectCount :: !Int,
    drtUpsetByKey :: !(IntMap ContextRegion),
    drtStrictLowerByKey :: !(IntMap ContextRegion),
    drtTop :: !ContextRegion
  }
  deriving stock (Eq, Show)

type RegionTable :: Type
data RegionTable
  = RegionTableDense !DenseRegionTable
  | RegionTablePowerset !Int
  deriving stock (Eq, Show)

regionTableFromUpsets :: Int -> IntMap IntSet -> IntMap IntSet -> RegionTable
regionTableFromUpsets objectCount upsetRows strictLowerRows =
  RegionTableDense
    DenseRegionTable
      { drtObjectCount = objectCount,
        drtUpsetByKey = fmap packKeySet upsetRows,
        drtStrictLowerByKey = fmap packKeySet strictLowerRows,
        drtTop = packedTop objectCount
      }
{-# INLINE regionTableFromUpsets #-}

-- | Implicit powerset site over the given atom count (at most 62): object
-- keys ARE atom bitmasks and nothing of size 2^n is ever constructed.
powersetRegionTable :: Int -> RegionTable
powersetRegionTable =
  RegionTablePowerset
{-# INLINE powersetRegionTable #-}

regionTableObjectCount :: RegionTable -> Int
regionTableObjectCount table =
  case table of
    RegionTableDense denseTable -> drtObjectCount denseTable
    RegionTablePowerset atomCount -> bit atomCount
{-# INLINE regionTableObjectCount #-}

regionTop :: RegionTable -> ContextRegion
regionTop table =
  case table of
    RegionTableDense denseTable -> drtTop denseTable
    RegionTablePowerset atomCount -> RegionCubes [RegionCube 0 (universeMaskOf atomCount)]
{-# INLINE regionTop #-}

regionVoid :: ContextRegion
regionVoid =
  RegionVoid
{-# INLINE regionVoid #-}

regionAtKey :: RegionTable -> Int -> ContextRegion
regionAtKey table keyValue =
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

regionFromKeys :: RegionTable -> IntSet -> ContextRegion
regionFromKeys table keySet =
  case table of
    RegionTableDense _ ->
      packKeySet keySet
    RegionTablePowerset atomCount ->
      mkCubes
        [ RegionCube
            (keyValue .&. universeMaskOf atomCount)
            (keyValue .&. universeMaskOf atomCount)
        | keyValue <- IntSet.toAscList keySet
        ]
{-# INLINE regionFromKeys #-}

regionKeys :: RegionTable -> ContextRegion -> [Int]
regionKeys table region =
  case table of
    RegionTableDense denseTable ->
      [ keyValue
        | keyValue <- [0 .. drtObjectCount denseTable - 1],
          regionMemberKey region keyValue
      ]
    RegionTablePowerset _ ->
      IntSet.toAscList (regionKeySet table region)
{-# INLINE regionKeys #-}

regionKeySet :: RegionTable -> ContextRegion -> IntSet
regionKeySet table region =
  case table of
    RegionTableDense _ ->
      IntSet.fromDistinctAscList (regionKeys table region)
    RegionTablePowerset _ ->
      cubesKeySet (regionCubesIn table region)
{-# INLINE regionKeySet #-}

regionMemberKey :: ContextRegion -> Int -> Bool
regionMemberKey region keyValue =
  case region of
    RegionVoid -> False
    RegionDense bits -> testBit bits keyValue
    RegionCubes cubes -> any (cubeMemberKey keyValue) cubes
{-# INLINE regionMemberKey #-}

regionMeet :: ContextRegion -> ContextRegion -> ContextRegion
regionMeet leftRegion rightRegion =
  case (leftRegion, rightRegion) of
    (RegionVoid, _) -> RegionVoid
    (_, RegionVoid) -> RegionVoid
    (RegionDense leftBits, RegionDense rightBits) -> mkDense (leftBits .&. rightBits)
    _ -> mkCubes (cubesMeet (promoteToCubes leftRegion) (promoteToCubes rightRegion))
{-# INLINE regionMeet #-}

regionJoin :: ContextRegion -> ContextRegion -> ContextRegion
regionJoin leftRegion rightRegion =
  case (leftRegion, rightRegion) of
    (RegionVoid, _) -> rightRegion
    (_, RegionVoid) -> leftRegion
    (RegionDense leftBits, RegionDense rightBits) -> RegionDense (leftBits .|. rightBits)
    _ -> mkCubes (promoteToCubes leftRegion <> promoteToCubes rightRegion)
{-# INLINE regionJoin #-}

-- | Complement within the table's key universe. Sends opens to down-closed
-- regions and back; the receiving vocabulary for negative guard atoms.
regionComplementIn :: RegionTable -> ContextRegion -> ContextRegion
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
regionDifference :: RegionTable -> ContextRegion -> ContextRegion -> ContextRegion
regionDifference table wholeRegion removedRegion =
  regionMeet wholeRegion (regionComplementIn table removedRegion)
{-# INLINE regionDifference #-}

regionEmpty :: ContextRegion -> Bool
regionEmpty region =
  case region of
    RegionVoid -> True
    RegionDense bits -> bits == 0
    RegionCubes cubes -> null cubes
{-# INLINE regionEmpty #-}

regionEntails :: ContextRegion -> ContextRegion -> Bool
regionEntails narrowRegion wideRegion =
  case (narrowRegion, wideRegion) of
    (RegionVoid, _) -> True
    (_, RegionVoid) -> regionEmpty narrowRegion
    (RegionDense narrowBits, RegionDense wideBits) ->
      narrowBits .&. wideBits == narrowBits
    _ -> cubesEntail (promoteToCubes narrowRegion) (promoteToCubes wideRegion)
{-# INLINE regionEntails #-}

regionSize :: ContextRegion -> Int
regionSize region =
  case region of
    RegionVoid -> 0
    RegionDense bits -> popCount bits
    RegionCubes cubes -> cubesSize cubes
{-# INLINE regionSize #-}

regionIsOpen :: RegionTable -> ContextRegion -> Bool
regionIsOpen table region =
  all
    (\keyValue -> regionEntails (regionAtKey table keyValue) region)
    (regionKeys table region)
{-# INLINE regionIsOpen #-}

regionIsDownClosed :: RegionTable -> ContextRegion -> Bool
regionIsDownClosed table region =
  all
    (\keyValue -> regionEntails (strictLowerAtKey table keyValue) region)
    (regionKeys table region)
{-# INLINE regionIsDownClosed #-}

regionGeneratorKeys :: RegionTable -> ContextRegion -> [Int]
regionGeneratorKeys table region =
  case table of
    RegionTableDense _ ->
      [ keyValue
        | keyValue <- regionKeys table region,
          regionEmpty (regionMeet (strictLowerAtKey table keyValue) region)
      ]
    RegionTablePowerset _ ->
      minimalMasks (fmap regionCubeMust (regionCubesIn table region))
{-# INLINE regionGeneratorKeys #-}

regionCubeCount :: RegionTable -> ContextRegion -> Int
regionCubeCount table region =
  length (regionCubesIn table region)
{-# INLINE regionCubeCount #-}

fromGeneratorKeys :: RegionTable -> [Int] -> ContextRegion
fromGeneratorKeys table =
  foldl (\region keyValue -> regionJoin region (regionAtKey table keyValue)) regionVoid
{-# INLINE fromGeneratorKeys #-}

strictLowerAtKey :: RegionTable -> Int -> ContextRegion
strictLowerAtKey table keyValue =
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

mkDense :: Natural -> ContextRegion
mkDense bits
  | bits == 0 = RegionVoid
  | otherwise = RegionDense bits
{-# INLINE mkDense #-}

mkCubes :: [RegionCube] -> ContextRegion
mkCubes cubes =
  case pruneCubes (filter cubeValid cubes) of
    [] -> RegionVoid
    prunedCubes -> RegionCubes (sort prunedCubes)
{-# INLINE mkCubes #-}

promoteToCubes :: ContextRegion -> [RegionCube]
promoteToCubes region =
  case region of
    RegionVoid -> []
    RegionDense bits -> [RegionCube keyValue keyValue | keyValue <- naturalBitIndices bits]
    RegionCubes cubes -> cubes

regionCubesIn :: RegionTable -> ContextRegion -> [RegionCube]
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

denseBitsIn :: DenseRegionTable -> ContextRegion -> Natural
denseBitsIn denseTable region =
  case region of
    RegionVoid -> 0
    RegionDense bits -> bits
    RegionCubes _ ->
      foldl
        setBit
        0
        [ keyValue
          | keyValue <- [0 .. drtObjectCount denseTable - 1],
            regionMemberKey region keyValue
        ]

denseTopBits :: DenseRegionTable -> Natural
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
  foldl absorb []
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
  foldl
    (\accumulated cube -> cubesMeet accumulated (cubeComplementWithin universeMask cube))
    [RegionCube 0 universeMask]

cubesSpan :: [RegionCube] -> Int
cubesSpan =
  foldl (\accumulated cube -> accumulated .|. regionCubeMay cube) 0

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

packKeySet :: IntSet -> ContextRegion
packKeySet =
  mkDense . IntSet.foldl' setBit 0
{-# INLINE packKeySet #-}

packedTop :: Int -> ContextRegion
packedTop objectCount =
  mkDense (bit objectCount - 1)
{-# INLINE packedTop #-}

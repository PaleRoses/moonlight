module Moonlight.Analysis.Dynamics.Inertia.Region.Kernel
  ( AABB,
    mkAabb,
    aabbMin,
    aabbMax,
    aabbDimensions,
    VoxelGrid,
    mkVoxelGrid,
    uniformVoxelGrid,
    defaultVoxelGrid,
    RefinementDepth,
    mkRefinementDepth,
    defaultBoundaryRefinement,
    voxelGridDimensions,
    voxelCellSize,
    voxelSamplePoints,
    MassProperties (..),
    massPropertiesFromPointMasses,
    parallelAxisShift,
    composeMassProperties,
    computeRegionInertia,
    computeRegionInertiaBoundaryAware,
    RegionSubdivisionPath (..),
    compileRegionDecompositionBoundaryAware,
    RegionProvenance (..),
    InertiaRegionCell (..),
    InertiaRegionDecomposition (..),
  )
where

import Data.Kind (Type)
import Data.Maybe (mapMaybe)
import Moonlight.Analysis.Dynamics.Inertia.Tensor
  ( PointMass (..),
    centerOfMass,
    inertiaTensorAboutCenterOfMass,
    parallelAxisCorrection,
  )
import Moonlight.LinAlg.Geometry
  ( AABB,
    aabbCenter,
    aabbDimensions,
    aabbMax,
    aabbMin,
    mkAabb,
  )
import Moonlight.LinAlg.Geometry (Symmetric3, diagonalSymmetric3)
import Moonlight.LinAlg.Geometry (Vec3 (..), addVec3, scaleVec3, subVec3, vec3Zero)

type VoxelGrid :: Type
data VoxelGrid = VoxelGrid
  { gridX :: Int,
    gridY :: Int,
    gridZ :: Int
  }
  deriving stock (Eq, Show)

type RefinementDepth :: Type
newtype RefinementDepth = RefinementDepth
  { unRefinementDepth :: Int
  }
  deriving stock (Eq, Show)

type MassProperties :: Type
data MassProperties = MassProperties
  { massPropertiesMass :: Double,
    massPropertiesCenterOfMass :: Vec3,
    massPropertiesInertiaTensor :: Symmetric3 Double
  }
  deriving stock (Eq, Show, Read)

mkVoxelGrid :: Int -> Int -> Int -> Maybe VoxelGrid
mkVoxelGrid samplesX samplesY samplesZ
  | all (> 0) [samplesX, samplesY, samplesZ] =
      Just (VoxelGrid samplesX samplesY samplesZ)
  | otherwise =
      Nothing

uniformVoxelGrid :: Int -> Maybe VoxelGrid
uniformVoxelGrid samplesPerAxis =
  mkVoxelGrid samplesPerAxis samplesPerAxis samplesPerAxis

defaultVoxelGrid :: VoxelGrid
defaultVoxelGrid = VoxelGrid 8 8 8

mkRefinementDepth :: Int -> Maybe RefinementDepth
mkRefinementDepth depthValue
  | depthValue >= 0 =
      Just (RefinementDepth depthValue)
  | otherwise =
      Nothing

defaultBoundaryRefinement :: RefinementDepth
defaultBoundaryRefinement = RefinementDepth 2

voxelGridDimensions :: VoxelGrid -> (Int, Int, Int)
voxelGridDimensions voxelGrid =
  (gridX voxelGrid, gridY voxelGrid, gridZ voxelGrid)

voxelCellSize :: VoxelGrid -> AABB -> Vec3
voxelCellSize voxelGrid boundingBox =
  let Vec3 width height depth = aabbDimensions boundingBox
      (samplesX, samplesY, samplesZ) = voxelGridDimensions voxelGrid
   in Vec3
        (width / fromIntegral samplesX)
        (height / fromIntegral samplesY)
        (depth / fromIntegral samplesZ)

voxelSamplePoints :: VoxelGrid -> AABB -> [Vec3]
voxelSamplePoints voxelGrid boundingBox =
  let minimumCorner = aabbMin boundingBox
      Vec3 stepX stepY stepZ = voxelCellSize voxelGrid boundingBox
      centeredCoordinate :: (Integral index, Fractional scalar) => scalar -> scalar -> index -> scalar
      centeredCoordinate originValue stepValue indexValue =
        originValue + (fromIntegral indexValue + 0.5) * stepValue
      (samplesX, samplesY, samplesZ) = voxelGridDimensions voxelGrid
   in concatMap
        ( \xIndex ->
            concatMap
              ( \yIndex ->
                  map
                    ( \zIndex ->
                        Vec3
                          (centeredCoordinate (vecX minimumCorner) stepX xIndex)
                          (centeredCoordinate (vecY minimumCorner) stepY yIndex)
                          (centeredCoordinate (vecZ minimumCorner) stepZ zIndex)
                    )
                    (sampleIndices samplesZ)
              )
              (sampleIndices samplesY)
        )
        (sampleIndices samplesX)

aabbCorners :: AABB -> [Vec3]
aabbCorners cellBoundingBox =
  let Vec3 minX minY minZ = aabbMin cellBoundingBox
      Vec3 maxX maxY maxZ = aabbMax cellBoundingBox
   in [ Vec3 minX minY minZ,
        Vec3 minX minY maxZ,
        Vec3 minX maxY minZ,
        Vec3 minX maxY maxZ,
        Vec3 maxX minY minZ,
        Vec3 maxX minY maxZ,
        Vec3 maxX maxY minZ,
        Vec3 maxX maxY maxZ
      ]

sampleIndices :: Int -> [Int]
sampleIndices sampleCount = take sampleCount [0 ..]

cellVolume :: Vec3 -> Double
cellVolume (Vec3 width height depth) = width * height * depth

massPropertiesFromPointMasses :: [PointMass] -> Maybe MassProperties
massPropertiesFromPointMasses pointMasses = do
  centerValue <- centerOfMass pointMasses
  inertiaTensorValue <- inertiaTensorAboutCenterOfMass pointMasses
  pure
    MassProperties
      { massPropertiesMass = sum (map pointMassValue pointMasses),
        massPropertiesCenterOfMass = centerValue,
        massPropertiesInertiaTensor = inertiaTensorValue
      }

parallelAxisShift :: Double -> Vec3 -> Symmetric3 Double -> Symmetric3 Double
parallelAxisShift massValue displacement =
  (parallelAxisCorrection massValue displacement <>)

composeMassProperties :: [MassProperties] -> Maybe MassProperties
composeMassProperties massPropertiesValues =
  let positiveMassProperties =
        filter ((> 1.0e-12) . massPropertiesMass) massPropertiesValues
      totalMass =
        sum (map massPropertiesMass positiveMassProperties)
   in if totalMass <= 1.0e-12
        then Nothing
        else
          let totalCenter =
                scaleVec3
                  (1.0 / totalMass)
                  ( foldr
                      ( \massPropertiesValue accumulator ->
                          addVec3
                            accumulator
                            ( scaleVec3
                                (massPropertiesMass massPropertiesValue)
                                (massPropertiesCenterOfMass massPropertiesValue)
                            )
                      )
                      vec3Zero
                      positiveMassProperties
                  )
              totalInertia =
                foldr
                  ( \massPropertiesValue accumulator ->
                      parallelAxisShift
                        (massPropertiesMass massPropertiesValue)
                        (subVec3 (massPropertiesCenterOfMass massPropertiesValue) totalCenter)
                        (massPropertiesInertiaTensor massPropertiesValue)
                        <> accumulator
                  )
                  mempty
                  positiveMassProperties
           in Just
                MassProperties
                  { massPropertiesMass = totalMass,
                    massPropertiesCenterOfMass = totalCenter,
                    massPropertiesInertiaTensor = totalInertia
                  }

cellMassProperties :: Double -> AABB -> MassProperties
cellMassProperties occupancyFraction cellBoundingBox =
  let Vec3 width height depth = aabbDimensions cellBoundingBox
      fullMass = cellVolume (Vec3 width height depth)
      scaledMass = occupancyFraction * fullMass
      diagonalX = scaledMass * (height * height + depth * depth) / 12.0
      diagonalY = scaledMass * (width * width + depth * depth) / 12.0
      diagonalZ = scaledMass * (width * width + height * height) / 12.0
   in MassProperties
        { massPropertiesMass = scaledMass,
          massPropertiesCenterOfMass = aabbCenter cellBoundingBox,
          massPropertiesInertiaTensor = diagonalSymmetric3 diagonalX diagonalY diagonalZ
        }

type RegionProvenance :: Type -> Type
data RegionProvenance site
  = RootRegion
  | RefinedFrom site
  deriving stock (Eq, Show)

type InertiaRegionCell :: Type -> Type
data InertiaRegionCell site = InertiaRegionCell
  { ircBoundingBox :: AABB,
    ircProvenance :: RegionProvenance site
  }
  deriving stock (Eq, Show)

type InertiaRegionDecomposition :: Type -> Type
data InertiaRegionDecomposition site = InertiaRegionDecomposition
  { irdSite :: site,
    irdBoundingBox :: AABB,
    irdChildren :: [InertiaRegionDecomposition site]
  }
  deriving stock (Eq, Show)

type RegionSubdivisionPath :: Type
newtype RegionSubdivisionPath = RegionSubdivisionPath
  { unRegionSubdivisionPath :: [Int]
  }
  deriving stock (Eq, Ord, Show)

computeRegionInertia :: VoxelGrid -> AABB -> (Vec3 -> Double) -> Maybe MassProperties
computeRegionInertia voxelGrid boundingBox sdf =
  massPropertiesFromPointMasses
    (samplePointMasses voxelGrid boundingBox sdf)

computeRegionInertiaBoundaryAware :: VoxelGrid -> RefinementDepth -> AABB -> (Vec3 -> Double) -> Maybe MassProperties
computeRegionInertiaBoundaryAware voxelGrid refinementDepth boundingBox sdf =
  composeMassProperties
    (concatMap (refineCell refinementDepth sdf) (voxelCells voxelGrid boundingBox))

compileRegionDecompositionBoundaryAware ::
  RefinementDepth ->
  AABB ->
  (Vec3 -> Double) ->
  (RegionSubdivisionPath -> AABB -> site) ->
  Maybe (InertiaRegionDecomposition site)
compileRegionDecompositionBoundaryAware refinementDepth rootBoundingBox sdf siteOf =
  unfoldRegionDecomposition (RegionSubdivisionPath []) refinementDepth rootBoundingBox
  where
    unfoldRegionDecomposition currentPath currentDepth currentBoundingBox =
      case classifyCell sdf currentBoundingBox of
        FullyOutside -> Nothing
        FullyInside -> Just (leafDecomposition currentPath currentBoundingBox)
        BoundaryCell _ ->
          case currentDepth of
            RefinementDepth remainingDepth
              | remainingDepth <= 0 ->
                  Just (leafDecomposition currentPath currentBoundingBox)
              | otherwise ->
                  Just
                    InertiaRegionDecomposition
                      { irdSite = siteOf currentPath currentBoundingBox,
                        irdBoundingBox = currentBoundingBox,
                        irdChildren =
                          mapMaybe
                            (uncurry (unfoldChildDecomposition currentPath (remainingDepth - 1)))
                            (zip [0 ..] (subdivideAabb currentBoundingBox))
                      }

    unfoldChildDecomposition parentPath remainingDepth childIndex childBoundingBox =
      unfoldRegionDecomposition
        (appendSubdivisionIndex parentPath childIndex)
        (RefinementDepth remainingDepth)
        childBoundingBox

    leafDecomposition currentPath currentBoundingBox =
      InertiaRegionDecomposition
        { irdSite = siteOf currentPath currentBoundingBox,
          irdBoundingBox = currentBoundingBox,
          irdChildren = []
        }

samplePointMasses :: VoxelGrid -> AABB -> (Vec3 -> Double) -> [PointMass]
samplePointMasses voxelGrid boundingBox sdf =
  let cellMass = cellVolume (voxelCellSize voxelGrid boundingBox)
   in map
        (PointMass cellMass)
        (filter ((<= 0.0) . sdf) (voxelSamplePoints voxelGrid boundingBox))

voxelCells :: VoxelGrid -> AABB -> [AABB]
voxelCells voxelGrid boundingBox =
  let minimumCorner = aabbMin boundingBox
      Vec3 stepX stepY stepZ = voxelCellSize voxelGrid boundingBox
      lowerCoordinate :: (Integral index, Num scalar) => scalar -> scalar -> index -> scalar
      lowerCoordinate originValue stepValue indexValue =
        originValue + fromIntegral indexValue * stepValue
      upperCoordinate :: (Integral index, Num scalar) => scalar -> scalar -> index -> scalar
      upperCoordinate originValue stepValue indexValue =
        lowerCoordinate originValue stepValue indexValue + stepValue
      (samplesX, samplesY, samplesZ) = voxelGridDimensions voxelGrid
   in concatMap
        ( \xIndex ->
            concatMap
              ( \yIndex ->
                  mapMaybe
                    ( \zIndex ->
                        mkAabb
                          ( Vec3
                              (lowerCoordinate (vecX minimumCorner) stepX xIndex)
                              (lowerCoordinate (vecY minimumCorner) stepY yIndex)
                              (lowerCoordinate (vecZ minimumCorner) stepZ zIndex)
                          )
                          ( Vec3
                              (upperCoordinate (vecX minimumCorner) stepX xIndex)
                              (upperCoordinate (vecY minimumCorner) stepY yIndex)
                              (upperCoordinate (vecZ minimumCorner) stepZ zIndex)
                          )
                    )
                    (sampleIndices samplesZ)
              )
              (sampleIndices samplesY)
        )
        (sampleIndices samplesX)

refineCell :: RefinementDepth -> (Vec3 -> Double) -> AABB -> [MassProperties]
refineCell refinementDepth sdf cellBoundingBox =
  case classifyCell sdf cellBoundingBox of
    FullyInside ->
      [cellMassProperties 1.0 cellBoundingBox]
    FullyOutside ->
      []
    BoundaryCell occupancyFraction ->
      case refinementDepth of
        RefinementDepth remainingDepth
          | remainingDepth <= 0 ->
              [cellMassProperties occupancyFraction cellBoundingBox]
          | otherwise ->
              concatMap
                (refineCell (RefinementDepth (remainingDepth - 1)) sdf)
                (subdivideAabb cellBoundingBox)

type CellClassification :: Type
data CellClassification
  = FullyInside
  | FullyOutside
  | BoundaryCell Double

classifyCell :: (Vec3 -> Double) -> AABB -> CellClassification
classifyCell sdf cellBoundingBox =
  let sampleValues = map sdf (boundarySamplePoints cellBoundingBox)
      insideCount =
        foldr
          (\sampleValue accumulated -> if sampleValue <= 0.0 then accumulated + 1 else accumulated)
          0
          sampleValues
      totalCount = length sampleValues
   in if insideCount == totalCount
        then FullyInside
        else
          if insideCount == 0
            then FullyOutside
            else BoundaryCell (fromIntegral insideCount / fromIntegral totalCount)

boundarySamplePoints :: AABB -> [Vec3]
boundarySamplePoints cellBoundingBox =
  aabbCorners cellBoundingBox
    <> [aabbCenter cellBoundingBox]

subdivideAabb :: AABB -> [AABB]
subdivideAabb cellBoundingBox =
  let Vec3 minX minY minZ = aabbMin cellBoundingBox
      Vec3 maxX maxY maxZ = aabbMax cellBoundingBox
      Vec3 midX midY midZ = aabbCenter cellBoundingBox
      splitX = [(minX, midX), (midX, maxX)]
      splitY = [(minY, midY), (midY, maxY)]
      splitZ = [(minZ, midZ), (midZ, maxZ)]
   in concatMap
        ( \(lowerX, upperX) ->
            concatMap
              ( \(lowerY, upperY) ->
                  mapMaybe
                    ( \(lowerZ, upperZ) ->
                        mkAabb
                          (Vec3 lowerX lowerY lowerZ)
                          (Vec3 upperX upperY upperZ)
                    )
                    splitZ
              )
              splitY
        )
        splitX

appendSubdivisionIndex :: RegionSubdivisionPath -> Int -> RegionSubdivisionPath
appendSubdivisionIndex (RegionSubdivisionPath indices) childIndex =
  RegionSubdivisionPath (indices <> [childIndex])

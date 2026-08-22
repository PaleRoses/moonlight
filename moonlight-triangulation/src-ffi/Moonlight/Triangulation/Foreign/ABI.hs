{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Triangulation.Foreign.ABI
  ( CObstruction (..)
  , CMinkowskiReceipt (..)
  , CMesh
  , CRegion
  , CStructuringElement
  , delaunayF64
  , meshInsertManyF64
  , -- | Exported for measurement. @meshInsertManyF64@ is a pointer boundary and
    -- cannot be timed against a Haskell arm without dragging marshalling into
    -- one side only; this is the same route with the pointers already resolved.
    insertGeometryBatch
  , meshSiteUnion
  , meshSiteIntersection
  , meshSiteDifference
  , meshSiteSymmetricDifference
  , meshVertexCount
  , meshTriangleCount
  , meshCopyVerticesF64
  , meshCopyTrianglesU32
  , meshFree
  , regionCreateF64
  , regionCounts
  , regionCopyF64
  , regionUnion
  , regionIntersection
  , regionDifference
  , regionSymmetricDifference
  , regionLocatePointF64
  , regionMeasure
  , regionFree
  , structuringElementCreateF64
  , structuringElementFree
  , regionMinkowskiSum
  , regionOffset
  , regionInset
  , regionOpen
  , regionClose
  ) where

import Control.Exception (SomeException, displayException, try)
import Control.Monad (void)
import Data.Bifunctor (first)
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Word (Word32, Word64)
import Foreign.C.String (peekCString, withCStringLen)
import Foreign.C.Types (CChar, CDouble (..), CSize (..), CUInt (..))
import Foreign.Marshal.Utils (copyBytes, fillBytes)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.StablePtr
  ( StablePtr
  , castPtrToStablePtr
  , castStablePtrToPtr
  , deRefStablePtr
  , freeStablePtr
  , newStablePtr
  )
import Foreign.Storable (Storable (..))
import qualified Data.Vector as V
import qualified Moonlight.Triangulation as T
import Moonlight.Triangulation.Math (canonicalPoint, validatePoint)
import qualified Moonlight.Triangulation.Session as Session

type GeometryMesh = T.DelaunayTriangulation ()

data CMesh
data CRegion
data CStructuringElement

type family HandleValue carrier where
  HandleValue CMesh = GeometryMesh
  HandleValue CRegion = T.PlanarRegion
  HandleValue CStructuringElement = T.StructuringElement

data CObstruction = CObstruction
  { obstructionCode :: !Word32
  , obstructionCoordinateError :: !Word32
  , obstructionInputIndex :: !Word64
  , obstructionFirstIndex :: !Word64
  , obstructionSecondIndex :: !Word64
  , obstructionFirstValue :: !Double
  , obstructionSecondValue :: !Double
  , obstructionPointX :: !Double
  , obstructionPointY :: !Double
  , obstructionMessage :: !String
  }
  deriving stock (Eq, Show)

instance Storable CObstruction where
  sizeOf _ = 320
  alignment _ = alignment (0 :: Word64)
  peek pointer = do
    obstructionCode <- peekByteOff pointer 0
    obstructionCoordinateError <- peekByteOff pointer 4
    obstructionInputIndex <- peekByteOff pointer 8
    obstructionFirstIndex <- peekByteOff pointer 16
    obstructionSecondIndex <- peekByteOff pointer 24
    obstructionFirstValue <- peekByteOff pointer 32
    obstructionSecondValue <- peekByteOff pointer 40
    obstructionPointX <- peekByteOff pointer 48
    obstructionPointY <- peekByteOff pointer 56
    obstructionMessage <- peekCString (castPtr pointer `plusPtr` 64)
    pure CObstruction {..}
  poke pointer CObstruction {..} = do
    pokeByteOff pointer 0 obstructionCode
    pokeByteOff pointer 4 obstructionCoordinateError
    pokeByteOff pointer 8 obstructionInputIndex
    pokeByteOff pointer 16 obstructionFirstIndex
    pokeByteOff pointer 24 obstructionSecondIndex
    pokeByteOff pointer 32 obstructionFirstValue
    pokeByteOff pointer 40 obstructionSecondValue
    pokeByteOff pointer 48 obstructionPointX
    pokeByteOff pointer 56 obstructionPointY
    let messagePointer = castPtr pointer `plusPtr` 64 :: Ptr CChar
    fillBytes messagePointer 0 256
    withCStringLen obstructionMessage $ \(source, lengthInBytes) ->
      copyBytes messagePointer source (min 255 lengthInBytes)

-- | Fixed-width projection of the existing Haskell morphology receipt.
data CMinkowskiReceipt = CMinkowskiReceipt
  { receiptOperation :: !Word32
  , receiptInputComponents :: !Word64
  , receiptConvexPieces :: !Word64
  , receiptGeneratedPieces :: !Word64
  , receiptGeneratedConvolutionEdges :: !Word64
  , receiptOverlayPasses :: !Word64
  , receiptExactCrossings :: !Word64
  , receiptOutputCells :: !Word64
  , receiptExactCoordinateBitGrowth :: !Word64
  }
  deriving stock (Eq, Show)

instance Storable CMinkowskiReceipt where
  sizeOf _ = 72
  alignment _ = alignment (0 :: Word64)
  peek pointer = do
    receiptOperation <- peekByteOff pointer 0
    receiptInputComponents <- peekByteOff pointer 8
    receiptConvexPieces <- peekByteOff pointer 16
    receiptGeneratedPieces <- peekByteOff pointer 24
    receiptGeneratedConvolutionEdges <- peekByteOff pointer 32
    receiptOverlayPasses <- peekByteOff pointer 40
    receiptExactCrossings <- peekByteOff pointer 48
    receiptOutputCells <- peekByteOff pointer 56
    receiptExactCoordinateBitGrowth <- peekByteOff pointer 64
    pure CMinkowskiReceipt {..}
  poke pointer CMinkowskiReceipt {..} = do
    pokeByteOff pointer 0 receiptOperation
    pokeByteOff pointer 4 (0 :: Word32)
    pokeByteOff pointer 8 receiptInputComponents
    pokeByteOff pointer 16 receiptConvexPieces
    pokeByteOff pointer 24 receiptGeneratedPieces
    pokeByteOff pointer 32 receiptGeneratedConvolutionEdges
    pokeByteOff pointer 40 receiptOverlayPasses
    pokeByteOff pointer 48 receiptExactCrossings
    pokeByteOff pointer 56 receiptOutputCells
    pokeByteOff pointer 64 receiptExactCoordinateBitGrowth

data AbiFailure = AbiFailure !CUInt !CObstruction

statusOk, statusNullPointer, statusCountOverflow, statusBufferTooSmall, statusGeometryObstruction, statusRuntimeFailure :: CUInt
statusOk = 0
statusNullPointer = 1
statusCountOverflow = 2
statusBufferTooSmall = 3
statusGeometryObstruction = 4
statusRuntimeFailure = 5

emptyObstruction :: CObstruction
emptyObstruction =
  CObstruction
    { obstructionCode = 0
    , obstructionCoordinateError = 0
    , obstructionInputIndex = maxBound
    , obstructionFirstIndex = 0
    , obstructionSecondIndex = 0
    , obstructionFirstValue = 0
    , obstructionSecondValue = 0
    , obstructionPointX = 0
    , obstructionPointY = 0
    , obstructionMessage = ""
    }

apiFailure :: CUInt -> Word32 -> String -> AbiFailure
apiFailure status code message =
  AbiFailure status emptyObstruction {obstructionCode = code, obstructionMessage = message}

geometryFailure :: Show obstruction => Word32 -> obstruction -> AbiFailure
geometryFailure code obstruction =
  apiFailure statusGeometryObstruction code (show obstruction)

nullPointerFailure :: String -> AbiFailure
nullPointerFailure label = apiFailure statusNullPointer 100 (label <> " must not be null")

countOverflowFailure :: Word64 -> AbiFailure
countOverflowFailure count =
  AbiFailure statusCountOverflow emptyObstruction
    { obstructionCode = 101
    , obstructionFirstIndex = count
    , obstructionMessage = "count exceeds the host Int range"
    }

bufferTooSmallFailure :: Int -> Int -> AbiFailure
bufferTooSmallFailure required capacity =
  AbiFailure statusBufferTooSmall emptyObstruction
    { obstructionCode = 102
    , obstructionFirstIndex = fromIntegral required
    , obstructionSecondIndex = fromIntegral capacity
    , obstructionMessage = "output buffer is smaller than the required element count"
    }

runtimeFailure :: SomeException -> AbiFailure
runtimeFailure = apiFailure statusRuntimeFailure 103 . displayException

runBoundary :: Ptr CObstruction -> IO (Either AbiFailure ()) -> IO CUInt
runBoundary obstructionPointer action = do
  writeObstruction obstructionPointer emptyObstruction
  outcome <- try action :: IO (Either SomeException (Either AbiFailure ()))
  case outcome of
    Left exception -> finishFailure obstructionPointer (runtimeFailure exception)
    Right (Left failure) -> finishFailure obstructionPointer failure
    Right (Right ()) -> pure statusOk

finishFailure :: Ptr CObstruction -> AbiFailure -> IO CUInt
finishFailure obstructionPointer (AbiFailure status obstruction) = do
  writeObstruction obstructionPointer obstruction
  pure status

writeObstruction :: Ptr CObstruction -> CObstruction -> IO ()
writeObstruction pointer obstruction
  | pointer == nullPtr = pure ()
  | otherwise = poke pointer obstruction

requirePointer :: String -> Ptr value -> Either AbiFailure ()
requirePointer label pointer
  | pointer == nullPtr = Left (nullPointerFailure label)
  | otherwise = Right ()

checkedCount :: Int -> CSize -> Either AbiFailure Int
checkedCount elementsPerItem rawCount
  | toInteger rawCount * toInteger elementsPerItem > toInteger (maxBound :: Int) =
      Left (countOverflowFailure (fromIntegral rawCount))
  | otherwise = Right (fromIntegral rawCount)

readPoints :: Ptr CDouble -> Int -> IO (Either AbiFailure (V.Vector T.Point))
readPoints pointer count
  | count == 0 = pure (Right V.empty)
  | pointer == nullPtr = pure (Left (nullPointerFailure "coordinates"))
  | otherwise =
      Right
        <$> V.generateM
              count
              ( \index -> do
                  CDouble x <- peekElemOff pointer (index * 2)
                  CDouble y <- peekElemOff pointer (index * 2 + 1)
                  pure (T.Point x y)
              )

prepareHandleOutput :: Ptr (Ptr carrier) -> IO (Either AbiFailure ())
prepareHandleOutput pointer =
  case requirePointer "result" pointer of
    Left failure -> pure (Left failure)
    Right () -> poke pointer nullPtr >> pure (Right ())

publishHandle :: Ptr (Ptr carrier) -> HandleValue carrier -> IO ()
publishHandle output value = do
  stable <- newStablePtr value
  poke output (castPtr (castStablePtrToPtr stable))

produceHandle
  :: Ptr (Ptr carrier)
  -> IO (Either AbiFailure (HandleValue carrier))
  -> IO (Either AbiFailure ())
produceHandle output obtain = do
  prepared <- prepareHandleOutput output
  case prepared of
    Left failure -> pure (Left failure)
    Right () -> do
      outcome <- obtain
      case outcome of
        Left failure -> pure (Left failure)
        Right value -> publishHandle output value >> pure (Right ())

dereferenceHandle :: Ptr carrier -> IO (HandleValue carrier)
dereferenceHandle pointer = deRefStablePtr (castPtrToStablePtr (castPtr pointer))

freeHandle :: Ptr carrier -> IO ()
freeHandle pointer
  | pointer == nullPtr = pure ()
  | otherwise = freeStablePtr (castPtrToStablePtr (castPtr pointer) :: StablePtr ())

buildFailure :: T.BuildError -> AbiFailure
buildFailure obstruction =
  AbiFailure statusGeometryObstruction (buildErrorObstruction obstruction)

delaunayF64 :: Ptr CDouble -> CSize -> Ptr (Ptr CMesh) -> Ptr CObstruction -> IO CUInt
delaunayF64 coordinates rawCount output obstructionPointer =
  runBoundary obstructionPointer $ produceHandle output $ do
    case checkedCount 2 rawCount of
      Left failure -> pure (Left failure)
      Right count -> do
        points <- readPoints coordinates count
        pure (points >>= first buildFailure . T.delaunayGeometry)

meshInsertManyF64 :: Ptr CMesh -> Ptr CDouble -> CSize -> Ptr (Ptr CMesh) -> Ptr CObstruction -> IO CUInt
meshInsertManyF64 meshPointer coordinates rawCount output obstructionPointer =
  runBoundary obstructionPointer $ produceHandle output $ do
    case requirePointer "mesh" meshPointer >> checkedCount 2 rawCount of
      Left failure -> pure (Left failure)
      Right count -> do
        points <- readPoints coordinates count
        case points of
          Left failure -> pure (Left failure)
          Right admitted -> do
            mesh <- dereferenceHandle meshPointer
            pure (first buildFailure (insertGeometryBatch mesh admitted))

-- | Admission stays where it was — every point is validated before any point
-- is inserted, so a malformed input at the end still refuses the whole batch —
-- but it no longer materializes a second vector to carry the canonical
-- coordinates. @imapM@ over 'Either' cannot fill in place, so the discarded
-- form is the one that costs nothing; canonicalization is exactly what
-- @queryPointValue . validatePoint@ returned, applied where the point is used.
insertGeometryBatch :: GeometryMesh -> V.Vector T.Point -> Either T.BuildError GeometryMesh
insertGeometryBatch mesh points = do
  V.imapM_ (\index point -> () <$ validatePoint (Just index) point) points
  (_, revised, _) <-
    Session.withSession
      mesh
      (V.length points)
      (V.mapM_ (\point -> void (Session.insertVertexAt (canonicalPoint point) ())) points)
  pure revised

meshSiteUnion, meshSiteIntersection, meshSiteDifference, meshSiteSymmetricDifference :: Ptr CMesh -> Ptr CMesh -> Ptr (Ptr CMesh) -> Ptr CObstruction -> IO CUInt
meshSiteUnion = binaryMeshOperation T.union
meshSiteIntersection = binaryMeshOperation T.intersection
meshSiteDifference = binaryMeshOperation T.difference
meshSiteSymmetricDifference = binaryMeshOperation T.symmetricDifference

binaryMeshOperation
  :: (GeometryMesh -> GeometryMesh -> Either T.BuildError GeometryMesh)
  -> Ptr CMesh
  -> Ptr CMesh
  -> Ptr (Ptr CMesh)
  -> Ptr CObstruction
  -> IO CUInt
binaryMeshOperation operation leftPointer rightPointer output obstructionPointer =
  runBoundary obstructionPointer $ produceHandle output $ do
    case requirePointer "left mesh" leftPointer >> requirePointer "right mesh" rightPointer of
      Left failure -> pure (Left failure)
      Right () -> do
        left <- dereferenceHandle leftPointer
        right <- dereferenceHandle rightPointer
        pure (first buildFailure (operation left right))

meshVertexCount, meshTriangleCount :: Ptr CMesh -> Ptr CSize -> Ptr CObstruction -> IO CUInt
meshVertexCount = handleCount "mesh" T.numVertices
meshTriangleCount = handleCount "mesh" (V.length . T.innerFaceVertexTriples)

handleCount
  :: String
  -> (HandleValue carrier -> Int)
  -> Ptr carrier
  -> Ptr CSize
  -> Ptr CObstruction
  -> IO CUInt
handleCount handleLabel observe handlePointer output obstructionPointer =
  runBoundary obstructionPointer $
    case requirePointer handleLabel handlePointer >> requirePointer "count" output of
      Left failure -> pure (Left failure)
      Right () -> do
        value <- dereferenceHandle handlePointer
        poke output (fromIntegral (observe value))
        pure (Right ())

meshCopyVerticesF64 :: Ptr CMesh -> Ptr CDouble -> CSize -> Ptr CSize -> Ptr CObstruction -> IO CUInt
meshCopyVerticesF64 =
  copyHandleProjection
    "mesh"
    "points_written"
    "coordinates"
    T.vertexPoints
    ( \output index (T.Point x y) -> do
        pokeElemOff output (index * 2) (CDouble x)
        pokeElemOff output (index * 2 + 1) (CDouble y)
    )

meshCopyTrianglesU32 :: Ptr CMesh -> Ptr Word32 -> CSize -> Ptr CSize -> Ptr CObstruction -> IO CUInt
meshCopyTrianglesU32 =
  copyHandleProjection
    "mesh"
    "triangles_written"
    "triangles"
    T.innerFaceVertexTriples
    ( \output index (firstVertex, secondVertex, thirdVertex) -> do
        pokeElemOff output (index * 3) (T.unVertexId firstVertex)
        pokeElemOff output (index * 3 + 1) (T.unVertexId secondVertex)
        pokeElemOff output (index * 3 + 2) (T.unVertexId thirdVertex)
    )

copyHandleProjection
  :: String
  -> String
  -> String
  -> (HandleValue carrier -> V.Vector item)
  -> (Ptr element -> Int -> item -> IO ())
  -> Ptr carrier
  -> Ptr element
  -> CSize
  -> Ptr CSize
  -> Ptr CObstruction
  -> IO CUInt
{-# INLINE copyHandleProjection #-}
copyHandleProjection handleLabel writtenLabel outputLabel project writeItem handlePointer output rawCapacity written obstructionPointer =
  runBoundary obstructionPointer $
    case requirePointer handleLabel handlePointer >> requirePointer writtenLabel written >> checkedCount 1 rawCapacity of
      Left failure -> pure (Left failure)
      Right capacity -> do
        value <- dereferenceHandle handlePointer
        let items = project value
            required = V.length items
        poke written (fromIntegral required)
        case requireOutputCapacity outputLabel output required capacity of
          Left failure -> pure (Left failure)
          Right () -> V.imapM_ (writeItem output) items >> pure (Right ())

requireOutputCapacity :: String -> Ptr value -> Int -> Int -> Either AbiFailure ()
requireOutputCapacity label output required capacity
  | capacity < required = Left (bufferTooSmallFailure required capacity)
  | required > 0 = requirePointer label output
  | otherwise = Right ()

meshFree :: Ptr CMesh -> IO ()
meshFree = freeHandle

data RegionCountKind
  = LoopPointCounts
  | ComponentLoopCounts
  deriving stock (Eq, Show)

data RegionLayoutError
  = RegionGroupEmpty !RegionCountKind !Int
  | RegionCountTotalMismatch !RegionCountKind !Integer !Integer
  | RegionStructuringElementEmpty
  deriving stock (Eq, Show)

regionLayoutFailure :: RegionLayoutError -> AbiFailure
regionLayoutFailure layoutError =
  AbiFailure statusGeometryObstruction (layoutObstruction layoutError)

layoutObstruction :: RegionLayoutError -> CObstruction
layoutObstruction layoutError =
  (case layoutError of
    RegionCountTotalMismatch _ actual expected ->
      base
        { obstructionFirstIndex = fromIntegral actual
        , obstructionSecondIndex = fromIntegral expected
        }
    RegionGroupEmpty _ index -> base {obstructionInputIndex = fromIntegral index}
    RegionStructuringElementEmpty -> base
  )
    {obstructionMessage = show layoutError}
 where
  base = emptyObstruction {obstructionCode = 200}

regionValidationFailure :: T.RegionValidationError -> AbiFailure
regionValidationFailure = geometryFailure 201

overlayFailure :: T.OverlayError Bool Bool -> AbiFailure
overlayFailure = geometryFailure 202

regionPublicationFailure :: T.RegionPublicationError -> AbiFailure
regionPublicationFailure = geometryFailure 203

valuationFailure :: T.ValuationError -> AbiFailure
valuationFailure = geometryFailure 204

minkowskiFailure :: T.MinkowskiError -> AbiFailure
minkowskiFailure = geometryFailure 205

pointInputFailure :: Int -> T.Point -> T.PointValidationError -> AbiFailure
pointInputFailure index (T.Point x y) pointError =
  AbiFailure statusGeometryObstruction emptyObstruction
    { obstructionCode = 1
    , obstructionCoordinateError = coordinateErrorCode reason
    , obstructionInputIndex = fromIntegral index
    , obstructionFirstValue = invalidValue
    , obstructionPointX = x
    , obstructionPointY = y
    , obstructionMessage = show pointError
    }
 where
  (invalidValue, reason) =
    case pointError of
      T.InvalidPointX coordinateError -> (x, coordinateError)
      T.InvalidPointY coordinateError -> (y, coordinateError)

projectionFailure :: Int -> T.PointValidationError -> AbiFailure
projectionFailure index pointError =
  AbiFailure statusGeometryObstruction emptyObstruction
    { obstructionCode = 206
    , obstructionCoordinateError =
        coordinateErrorCode
          (case pointError of
             T.InvalidPointX coordinateError -> coordinateError
             T.InvalidPointY coordinateError -> coordinateError)
    , obstructionInputIndex = fromIntegral index
    , obstructionMessage = show pointError
    }

readCounts
  :: String
  -> Ptr CSize
  -> Int
  -> IO (Either AbiFailure (V.Vector Int))
readCounts _ _ 0 = pure (Right V.empty)
readCounts label pointer count
  | pointer == nullPtr = pure (Left (nullPointerFailure label))
  | otherwise = do
      rawCounts <- V.generateM count (peekElemOff pointer)
      pure (V.mapM (checkedCount 1) rawCounts)

validateCounts
  :: RegionCountKind
  -> Int
  -> V.Vector Int
  -> Either RegionLayoutError ()
validateCounts kind expectedTotal counts =
  case V.findIndex (== 0) counts of
    Just index -> Left (RegionGroupEmpty kind index)
    Nothing
      | observedTotal /= toInteger expectedTotal ->
          Left (RegionCountTotalMismatch kind observedTotal (toInteger expectedTotal))
      | otherwise -> Right ()
 where
  observedTotal = V.foldl' (\total count -> total + toInteger count) 0 counts

regionCreateF64
  :: Ptr CDouble
  -> CSize
  -> Ptr CSize
  -> CSize
  -> Ptr CSize
  -> CSize
  -> Ptr (Ptr CRegion)
  -> Ptr CObstruction
  -> IO CUInt
regionCreateF64 coordinates rawPointCount loopPointCounts rawLoopCount componentLoopCounts rawComponentCount output obstructionPointer =
  runBoundary obstructionPointer $
    produceHandle output $
      readRegionF64
        coordinates
        rawPointCount
        loopPointCounts
        rawLoopCount
        componentLoopCounts
        rawComponentCount

readRegionF64
  :: Ptr CDouble
  -> CSize
  -> Ptr CSize
  -> CSize
  -> Ptr CSize
  -> CSize
  -> IO (Either AbiFailure T.PlanarRegion)
readRegionF64 coordinates rawPointCount loopPointCounts rawLoopCount componentLoopCounts rawComponentCount =
  case checkedRegionInputCounts rawPointCount rawLoopCount rawComponentCount of
    Left failure -> pure (Left failure)
    Right (pointCount, loopCount, componentCount) -> do
      points <- readPoints coordinates pointCount
      loopCounts <- readCounts "loop_point_counts" loopPointCounts loopCount
      componentCounts <- readCounts "component_loop_counts" componentLoopCounts componentCount
      pure $ do
        admittedPoints <- points
        admittedLoopCounts <- loopCounts
        admittedComponentCounts <- componentCounts
        first regionLayoutFailure (validateCounts LoopPointCounts pointCount admittedLoopCounts)
        first regionLayoutFailure (validateCounts ComponentLoopCounts loopCount admittedComponentCounts)
        buildRegion admittedPoints admittedLoopCounts admittedComponentCounts

checkedRegionInputCounts
  :: CSize
  -> CSize
  -> CSize
  -> Either AbiFailure (Int, Int, Int)
checkedRegionInputCounts rawPointCount rawLoopCount rawComponentCount = do
  pointCount <- checkedCount 2 rawPointCount
  loopCount <- checkedCount 1 rawLoopCount
  componentCount <- checkedCount 1 rawComponentCount
  pure (pointCount, loopCount, componentCount)

buildRegion
  :: V.Vector T.Point
  -> V.Vector Int
  -> V.Vector Int
  -> Either AbiFailure T.PlanarRegion
buildRegion points loopCounts componentCounts = do
  exactPoints <-
    V.imapM
      (\index point -> first (pointInputFailure index point) (T.exactPointFromPoint point))
      points
  loops <-
    V.imapM
      (buildLoop exactPoints)
      (adjacentOffsets loopCounts)
  components <-
    V.imapM
      (buildComponent loops)
      (adjacentOffsets componentCounts)
  first regionValidationFailure (T.planarRegion (V.toList components))

adjacentOffsets :: V.Vector Int -> V.Vector (Int, Int)
adjacentOffsets counts =
  let offsets = V.scanl' (+) 0 counts
   in V.zip offsets (V.drop 1 offsets)

buildLoop
  :: V.Vector T.ExactPoint
  -> Int
  -> (Int, Int)
  -> Either AbiFailure T.ExactLoop
buildLoop points loopIndex (start, end) =
  case NonEmpty.nonEmpty (V.toList (V.slice start (end - start) points)) of
    Nothing -> Left (regionLayoutFailure (RegionGroupEmpty LoopPointCounts loopIndex))
    Just submitted -> first regionValidationFailure (T.exactLoop submitted)

buildComponent
  :: V.Vector T.ExactLoop
  -> Int
  -> (Int, Int)
  -> Either AbiFailure T.PolygonComponent
buildComponent loops componentIndex (start, end) =
  case NonEmpty.nonEmpty (V.toList (V.slice start (end - start) loops)) of
    Nothing -> Left (regionLayoutFailure (RegionGroupEmpty ComponentLoopCounts componentIndex))
    Just (outer :| holes) -> first regionValidationFailure (T.polygonComponent outer holes)

regionShape
  :: T.PlanarRegion
  -> ([T.PolygonComponent], [[T.ExactLoop]], [T.ExactLoop])
regionShape region =
  let components = T.planarRegionComponents region
      componentLoops component = T.polygonOuterLoop component : T.polygonHoleLoops component
      loopsByComponent = map componentLoops components
   in (components, loopsByComponent, concat loopsByComponent)

regionCounts
  :: Ptr CRegion
  -> Ptr CSize
  -> Ptr CSize
  -> Ptr CSize
  -> Ptr CObstruction
  -> IO CUInt
regionCounts regionPointer componentCountOutput loopCountOutput pointCountOutput obstructionPointer =
  runBoundary obstructionPointer $
    case
      requirePointer "region" regionPointer
        >> requirePointer "component_count" componentCountOutput
        >> requirePointer "loop_count" loopCountOutput
        >> requirePointer "point_count" pointCountOutput
    of
      Left failure -> pure (Left failure)
      Right () -> do
        region <- dereferenceHandle regionPointer
        let (components, _, loops) = regionShape region
            pointCount = sum (map (NonEmpty.length . T.exactLoopPoints) loops)
        poke componentCountOutput (fromIntegral (length components))
        poke loopCountOutput (fromIntegral (length loops))
        poke pointCountOutput (fromIntegral pointCount)
        pure (Right ())

data RegionProjection = RegionProjection
  { projectionPoints :: !(V.Vector T.Point)
  , projectionLoopPointOffsets :: !(V.Vector CSize)
  , projectionComponentLoopOffsets :: !(V.Vector CSize)
  }

regionProjection :: T.PlanarRegion -> Either AbiFailure RegionProjection
regionProjection region = do
  let (_, loopsByComponent, loops) = regionShape region
      exactPoints = concatMap (NonEmpty.toList . T.exactLoopPoints) loops
      loopPointOffsets = scanl (+) 0 (map (NonEmpty.length . T.exactLoopPoints) loops)
      componentLoopOffsets = scanl (+) 0 (map length loopsByComponent)
  projectedPoints <-
    V.fromList
      <$> traverse
            (uncurry projectPoint)
            (zip [0 ..] exactPoints)
  pure
    RegionProjection
      { projectionPoints = projectedPoints
      , projectionLoopPointOffsets = V.fromList (map fromIntegral loopPointOffsets)
      , projectionComponentLoopOffsets = V.fromList (map fromIntegral componentLoopOffsets)
      }
 where
  projectPoint index point =
    first (projectionFailure index) (T.exactPointToEmbeddingCandidate point)

regionCopyF64
  :: Ptr CRegion
  -> Ptr CDouble
  -> CSize
  -> Ptr CSize
  -> CSize
  -> Ptr CSize
  -> CSize
  -> Ptr CObstruction
  -> IO CUInt
regionCopyF64 regionPointer coordinates rawPointCapacity loopPointOffsets rawLoopOffsetCapacity componentLoopOffsets rawComponentOffsetCapacity obstructionPointer =
  runBoundary obstructionPointer $
    case
      ( (,,)
          <$> (requirePointer "region" regionPointer >> checkedCount 2 rawPointCapacity)
          <*> checkedCount 1 rawLoopOffsetCapacity
          <*> checkedCount 1 rawComponentOffsetCapacity
      )
    of
      Left failure -> pure (Left failure)
      Right (pointCapacity, loopOffsetCapacity, componentOffsetCapacity) -> do
        region <- dereferenceHandle regionPointer
        case regionProjection region of
          Left failure -> pure (Left failure)
          Right RegionProjection {..} ->
            case
              requireOutputCapacity "coordinates" coordinates (V.length projectionPoints) pointCapacity
                >> requireOutputCapacity "loop_point_offsets" loopPointOffsets (V.length projectionLoopPointOffsets) loopOffsetCapacity
                >> requireOutputCapacity "component_loop_offsets" componentLoopOffsets (V.length projectionComponentLoopOffsets) componentOffsetCapacity
            of
              Left failure -> pure (Left failure)
              Right () -> do
                V.imapM_
                  (\index (T.Point x y) -> do
                     pokeElemOff coordinates (index * 2) (CDouble x)
                     pokeElemOff coordinates (index * 2 + 1) (CDouble y))
                  projectionPoints
                V.imapM_ (pokeElemOff loopPointOffsets) projectionLoopPointOffsets
                V.imapM_ (pokeElemOff componentLoopOffsets) projectionComponentLoopOffsets
                pure (Right ())

regionUnion, regionIntersection, regionDifference, regionSymmetricDifference :: Ptr CRegion -> Ptr CRegion -> Ptr (Ptr CRegion) -> Ptr CObstruction -> IO CUInt
regionUnion = binaryRegionOperation (\(left, right) -> left || right)
regionIntersection = binaryRegionOperation (\(left, right) -> left && right)
regionDifference = binaryRegionOperation (\(left, right) -> left && not right)
regionSymmetricDifference = binaryRegionOperation (uncurry (/=))

binaryRegionOperation
  :: ((Bool, Bool) -> Bool)
  -> Ptr CRegion
  -> Ptr CRegion
  -> Ptr (Ptr CRegion)
  -> Ptr CObstruction
  -> IO CUInt
binaryRegionOperation selected leftPointer rightPointer output obstructionPointer =
  runBoundary obstructionPointer $ produceHandle output $ do
    case requirePointer "left region" leftPointer >> requirePointer "right region" rightPointer of
      Left failure -> pure (Left failure)
      Right () -> do
        left <- dereferenceHandle leftPointer
        right <- dereferenceHandle rightPointer
        pure (exactRegionBoolean selected left right)

exactRegionBoolean
  :: ((Bool, Bool) -> Bool)
  -> T.PlanarRegion
  -> T.PlanarRegion
  -> Either AbiFailure T.PlanarRegion
exactRegionBoolean selected left right = do
  leftLayer <- first regionValidationFailure (T.planarLayer False (Map.singleton True left))
  rightLayer <- first regionValidationFailure (T.planarLayer False (Map.singleton True right))
  overlay <- first overlayFailure (T.overlayLayers leftLayer rightLayer)
  first regionPublicationFailure (T.overlaySelectedRegion selected overlay)

regionLocatePointF64
  :: Ptr CRegion
  -> CDouble
  -> CDouble
  -> Ptr CUInt
  -> Ptr CObstruction
  -> IO CUInt
regionLocatePointF64 regionPointer (CDouble x) (CDouble y) output obstructionPointer =
  runBoundary obstructionPointer $
    case
      requirePointer "region" regionPointer
        >> requirePointer "location" output
        >> first (pointInputFailure 0 (T.Point x y)) (T.exactPointFromPoint (T.Point x y))
    of
      Left failure -> pure (Left failure)
      Right query -> do
        region <- dereferenceHandle regionPointer
        poke output (regionLocationCode (T.regionPointLocation region query))
        pure (Right ())

regionLocationCode :: T.RegionPointLocation -> CUInt
regionLocationCode location =
  case location of
    T.RegionExterior -> 0
    T.RegionOnBoundary -> 1
    T.RegionInterior -> 2

regionMeasure
  :: Ptr CRegion
  -> Ptr Int64
  -> Ptr CChar
  -> CSize
  -> Ptr CSize
  -> Ptr CDouble
  -> Ptr CDouble
  -> Ptr CObstruction
  -> IO CUInt
regionMeasure regionPointer eulerOutput areaRatioOutput rawAreaCapacity areaBytesWritten perimeterLowerOutput perimeterUpperOutput obstructionPointer =
  runBoundary obstructionPointer $
    case
      requirePointer "region" regionPointer
        >> requirePointer "euler_characteristic" eulerOutput
        >> requirePointer "area_bytes_written" areaBytesWritten
        >> requirePointer "perimeter_lower" perimeterLowerOutput
        >> requirePointer "perimeter_upper" perimeterUpperOutput
        >> checkedCount 1 rawAreaCapacity
    of
      Left failure -> pure (Left failure)
      Right areaCapacity -> do
        region <- dereferenceHandle regionPointer
        case regionMeasurements region of
          Left failure -> pure (Left failure)
          Right (valuations, perimeter) -> do
            let area = T.exactAreaValue (T.valuationArea valuations)
                areaText =
                  show (T.exactRationalNumerator area)
                    <> "/"
                    <> show (T.exactRationalDenominator area)
                bounds = T.exactLengthBounds perimeter
            poke eulerOutput (fromIntegral (T.eulerCharacteristicValue (T.valuationEuler valuations)))
            poke perimeterLowerOutput (CDouble (T.intervalLower bounds))
            poke perimeterUpperOutput (CDouble (T.intervalUpper bounds))
            copyCStringOutput areaText areaRatioOutput areaCapacity areaBytesWritten

regionMeasurements
  :: T.PlanarRegion
  -> Either AbiFailure (T.PlanarValuations, T.ExactLengthMeasurement)
regionMeasurements region = do
  valuations <- first valuationFailure (T.regionValuations region)
  perimeter <- first valuationFailure (T.planarValuationsPerimeter valuations)
  pure (valuations, perimeter)

copyCStringOutput
  :: String
  -> Ptr CChar
  -> Int
  -> Ptr CSize
  -> IO (Either AbiFailure ())
copyCStringOutput value output capacity bytesWritten =
  withCStringLen value $ \(source, byteCount) -> do
    poke bytesWritten (fromIntegral byteCount)
    case requireOutputCapacity "area_ratio_utf8" output (byteCount + 1) capacity of
      Left failure -> pure (Left failure)
      Right () -> do
        copyBytes output source byteCount
        pokeElemOff output byteCount 0
        pure (Right ())

regionFree :: Ptr CRegion -> IO ()
regionFree = freeHandle

structuringElementCreateF64
  :: Ptr CDouble
  -> CSize
  -> Ptr (Ptr CStructuringElement)
  -> Ptr CObstruction
  -> IO CUInt
structuringElementCreateF64 coordinates rawPointCount output obstructionPointer =
  runBoundary obstructionPointer $ produceHandle output $ do
    case checkedCount 2 rawPointCount of
      Left failure -> pure (Left failure)
      Right pointCount -> do
        points <- readPoints coordinates pointCount
        pure (points >>= buildStructuringElement)

buildStructuringElement
  :: V.Vector T.Point
  -> Either AbiFailure T.StructuringElement
buildStructuringElement points = do
  exactPoints <-
    V.imapM
      (\index point -> first (pointInputFailure index point) (T.exactPointFromPoint point))
      points
  submitted <-
    maybe
      (Left (regionLayoutFailure RegionStructuringElementEmpty))
      Right
      (NonEmpty.nonEmpty (V.toList exactPoints))
  polygon <- first minkowskiFailure (T.convexPolygon submitted)
  first minkowskiFailure (T.structuringElement polygon)

structuringElementFree :: Ptr CStructuringElement -> IO ()
structuringElementFree = freeHandle

regionMinkowskiSum
  :: Ptr CRegion
  -> Ptr CRegion
  -> Ptr (Ptr CRegion)
  -> Ptr CMinkowskiReceipt
  -> Ptr CObstruction
  -> IO CUInt
regionMinkowskiSum leftPointer rightPointer output receiptOutput obstructionPointer =
  runBoundary obstructionPointer $
    produceRegionWithReceipt output receiptOutput $ do
      case requirePointer "left region" leftPointer >> requirePointer "right region" rightPointer of
        Left failure -> pure (Left failure)
        Right () -> do
          left <- dereferenceHandle leftPointer
          right <- dereferenceHandle rightPointer
          pure (first minkowskiFailure (T.minkowskiSum left right))

regionOffset, regionInset, regionOpen, regionClose :: Ptr CStructuringElement -> Ptr CRegion -> Ptr (Ptr CRegion) -> Ptr CMinkowskiReceipt -> Ptr CObstruction -> IO CUInt
regionOffset = structuringElementOperation T.polygonOffset
regionInset = structuringElementOperation T.polygonInset
regionOpen = structuringElementOperation T.openWith
regionClose = structuringElementOperation T.closeWith

structuringElementOperation
  :: (T.StructuringElement -> T.PlanarRegion -> Either T.MinkowskiError (T.PlanarRegion, T.MinkowskiReceipt))
  -> Ptr CStructuringElement
  -> Ptr CRegion
  -> Ptr (Ptr CRegion)
  -> Ptr CMinkowskiReceipt
  -> Ptr CObstruction
  -> IO CUInt
structuringElementOperation operation elementPointer regionPointer output receiptOutput obstructionPointer =
  runBoundary obstructionPointer $
    produceRegionWithReceipt output receiptOutput $ do
      case requirePointer "structuring element" elementPointer >> requirePointer "region" regionPointer of
        Left failure -> pure (Left failure)
        Right () -> do
          element <- dereferenceHandle elementPointer
          region <- dereferenceHandle regionPointer
          pure (first minkowskiFailure (operation element region))

produceRegionWithReceipt
  :: Ptr (Ptr CRegion)
  -> Ptr CMinkowskiReceipt
  -> IO (Either AbiFailure (T.PlanarRegion, T.MinkowskiReceipt))
  -> IO (Either AbiFailure ())
produceRegionWithReceipt output receiptOutput obtain = do
  prepared <- prepareHandleOutput output
  case (prepared, requirePointer "receipt" receiptOutput) of
    (Left failure, _) -> pure (Left failure)
    (_, Left failure) -> pure (Left failure)
    (Right (), Right ()) -> do
      outcome <- obtain
      case outcome of
        Left failure -> pure (Left failure)
        Right (region, receipt) -> do
          poke receiptOutput (minkowskiReceiptProjection receipt)
          publishHandle output region
          pure (Right ())

minkowskiReceiptProjection :: T.MinkowskiReceipt -> CMinkowskiReceipt
minkowskiReceiptProjection receipt =
  CMinkowskiReceipt
    { receiptOperation = minkowskiOperationCode (T.minkowskiOperation receipt)
    , receiptInputComponents = fromIntegral (T.minkowskiInputComponents receipt)
    , receiptConvexPieces = fromIntegral (T.minkowskiConvexPieces receipt)
    , receiptGeneratedPieces = fromIntegral (T.minkowskiGeneratedPieces receipt)
    , receiptGeneratedConvolutionEdges = fromIntegral (T.minkowskiGeneratedConvolutionEdges receipt)
    , receiptOverlayPasses = fromIntegral (T.minkowskiOverlayPasses receipt)
    , receiptExactCrossings = fromIntegral (T.minkowskiExactCrossings receipt)
    , receiptOutputCells = fromIntegral (T.minkowskiOutputCells receipt)
    , receiptExactCoordinateBitGrowth = fromIntegral (T.minkowskiExactCoordinateBitGrowth receipt)
    }

minkowskiOperationCode :: T.MinkowskiOperation -> Word32
minkowskiOperationCode operation =
  case operation of
    T.MinkowskiAddition -> 0
    T.MinkowskiErosion -> 1
    T.MinkowskiOpening -> 2
    T.MinkowskiClosing -> 3

buildErrorObstruction :: T.BuildError -> CObstruction
buildErrorObstruction failure =
  (case failure of
    T.InvalidCoordinate inputIndex value reason ->
      emptyObstruction
        { obstructionCode = 1
        , obstructionCoordinateError = coordinateErrorCode reason
        , obstructionInputIndex = maybe maxBound fromIntegral inputIndex
        , obstructionFirstValue = value
        }
    T.PointLocationFailed (T.Point x y) -> pointObstruction 2 x y
    T.LocationWalkExhausted (T.Point x y) steps ->
      (pointObstruction 3 x y) {obstructionFirstIndex = fromIntegral steps}
    T.RefinementInputTopologyInvalid _ -> codeOnly 4
    T.FreshInsertionMatchedExistingVertex firstVertex secondVertex -> indices 5 (T.unVertexId firstVertex) (T.unVertexId secondVertex)
    T.DegenerateLineEndpointMissingOutgoing vertex -> firstIndex 6 (T.unVertexId vertex)
    T.DegenerateLineEndpointTurnMissing index -> firstIndex 7 index
    T.DegenerateLineConnectedVertexMissing index -> firstIndex 8 index
    T.HullStartNotVisible edge -> firstIndex 9 (T.unDirectedEdgeId edge)
    T.OuterRangeDidNotTerminate firstEdge secondEdge steps ->
      (indices 10 (T.unDirectedEdgeId firstEdge) (T.unDirectedEdgeId secondEdge))
        {obstructionFirstValue = fromIntegral steps}
    T.OuterRangeContainsInnerEdge edge face -> indices 11 (T.unDirectedEdgeId edge) (T.unFaceId face)
    T.ConstrainedEdgeFlipRefused edge -> firstIndex 12 (T.unUndirectedEdgeId edge)
    T.RemovalVertexOutOfRange vertex count -> indices 13 (T.unVertexId vertex) count
    T.RemovalEdgeOutOfRange edge count -> indices 14 (T.unUndirectedEdgeId edge) count
    T.RemovalFaceOutOfRange face count -> indices 15 (T.unFaceId face) count
    T.RemovalFaceCycleDidNotTerminate face edge steps ->
      (indices 16 (T.unFaceId face) (T.unDirectedEdgeId edge))
        {obstructionFirstValue = fromIntegral steps}
    T.RemovalEmptyTriangulation vertex -> firstIndex 17 (T.unVertexId vertex)
    T.RemovalTwoPointDegreeMismatch vertex degree -> indices 18 (T.unVertexId vertex) degree
    T.RemovalCollinearDegreeMismatch vertex degree -> indices 19 (T.unVertexId vertex) degree
    T.RemovalBorderTooShort count -> firstIndex 20 count
    T.RemovalBorderArityMismatch count -> firstIndex 21 count
    T.RemovalOutgoingCycleDidNotTerminate vertex edge steps ->
      (indices 22 (T.unVertexId vertex) (T.unDirectedEdgeId edge))
        {obstructionFirstValue = fromIntegral steps}
    T.CircleSweepHullEmpty -> codeOnly 23
    T.OuterCycleDidNotTerminate firstEdge secondEdge steps ->
      (indices 24 (T.unDirectedEdgeId firstEdge) (T.unDirectedEdgeId secondEdge))
        {obstructionFirstValue = fromIntegral steps}
    T.HierarchyLevelPopulationMismatch level expected observed ->
      (indices 25 expected observed) {obstructionFirstValue = fromIntegral level}
    T.HierarchyInsertionHandleMismatch expected observed -> indices 26 (T.unVertexId expected) (T.unVertexId observed)
    T.PointIndexCapacityExhausted count -> firstIndex 27 count
    T.RefinementMinimumAngleNotFinite value -> nonFinite 28 value
    T.RefinementMinimumAngleOutOfRange value -> firstValue 29 value
    T.RefinementMinimumAngleDerivedRatioNotFinite value -> nonFinite 30 value
    T.RefinementMaximumAdditionalVerticesNegative value -> firstValue 31 (fromIntegral value)
    T.RefinementMinimumAreaNotFinite value -> nonFinite 32 value
    T.RefinementMinimumAreaNegative value -> firstValue 33 value
    T.RefinementMaximumAreaNotFinite value -> nonFinite 34 value
    T.RefinementMaximumAreaNotPositive value -> firstValue 35 value
    T.RefinementMaximumRadiusEdgeRatioNotFinite value -> nonFinite 36 value
    T.RefinementMaximumRadiusEdgeRatioNotPositive value -> firstValue 37 value
    T.RefinementMinimumAreaExceedsMaximum minimumArea maximumArea -> values 38 minimumArea maximumArea
    T.RefinementSeedFaceNotActive face count -> indices 39 (T.unFaceId face) count
    T.RefinementDomainInterfaceEdgeNotActive edge count -> indices 40 (T.unUndirectedEdgeId edge) count
    T.RefinementDomainInterfaceMissing edge -> firstIndex 41 (T.unUndirectedEdgeId edge)
    T.RefinementDomainInterfaceExtraneous edge -> firstIndex 42 (T.unUndirectedEdgeId edge)
    T.RefinementDomainTopologyChanged -> codeOnly 43
    T.RefinementDomainRequiresConvexHullPreservation -> codeOnly 44
    T.RefinementDomainRequiresConstraintPreservation -> codeOnly 45
    T.RefinementDomainForbidsOuterFaceExclusion -> codeOnly 46
    T.RefinementDomainWouldCrossInterface edge face -> indices 47 (T.unUndirectedEdgeId edge) (T.unFaceId face)
    T.RefinementDomainWouldRewriteProtectedFace face -> firstIndex 48 (T.unFaceId face)
    T.RefinementDomainProtectedFaceChanged face -> firstIndex 49 (T.unFaceId face)
    T.CapacityExceeded count -> firstIndex 50 count
    T.HalfEdgeCapacityExceeded requested capacity -> indices 51 requested capacity
    T.FaceCapacityExceeded requested capacity -> indices 52 requested capacity
    T.PayloadStorageFailure _ -> codeOnly 53
    T.CoordinatePayloadCountMismatch coordinates payloads -> indices 54 coordinates payloads
    T.CircleSweepRequiresDenseStorage -> codeOnly 55
  )
    {obstructionMessage = show failure}
 where
  codeOnly :: Word32 -> CObstruction
  codeOnly code = emptyObstruction {obstructionCode = code}
  firstIndex :: Integral index => Word32 -> index -> CObstruction
  firstIndex code index = (codeOnly code) {obstructionFirstIndex = fromIntegral index}
  indices :: (Integral first, Integral second) => Word32 -> first -> second -> CObstruction
  indices code firstIndexValue secondIndexValue =
    (codeOnly code)
      { obstructionFirstIndex = fromIntegral firstIndexValue
      , obstructionSecondIndex = fromIntegral secondIndexValue
      }
  firstValue :: Word32 -> Double -> CObstruction
  firstValue code value = (codeOnly code) {obstructionFirstValue = value}
  values :: Word32 -> Double -> Double -> CObstruction
  values code firstCoordinateValue secondCoordinateValue =
    (codeOnly code)
      { obstructionFirstValue = firstCoordinateValue
      , obstructionSecondValue = secondCoordinateValue
      }
  pointObstruction :: Word32 -> Double -> Double -> CObstruction
  pointObstruction code x y =
    (codeOnly code)
      { obstructionPointX = x
      , obstructionPointY = y
      }
  nonFinite :: Word32 -> T.NonFiniteValue -> CObstruction
  nonFinite code value = firstIndex code (nonFiniteCode value)

coordinateErrorCode :: T.CoordinateError -> Word32
coordinateErrorCode reason =
  case reason of
    T.CoordinateNaN -> 1
    T.CoordinateInfinite -> 2
    T.CoordinateTooSmall -> 3
    T.CoordinateTooLarge -> 4

nonFiniteCode :: T.NonFiniteValue -> Word32
nonFiniteCode value =
  case value of
    T.ValueNaN -> 1
    T.ValuePositiveInfinity -> 2
    T.ValueNegativeInfinity -> 3

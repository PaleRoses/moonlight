{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Moonlight.Geometry.Tetrahedralization.Facet
  ( FacetPolyIx(..)
  , FacetPSLGIx(..)
  , PLCFacet(..)
  , PLC3DOptions(..)
  , defaultPLC3DOptions
  , PLCTetrahedralization(..)
  , polylineSegments
  , closedLoopSegments
  , facetPolyToPSLG
  , prepareFacetPSLGsAndTriangles
  , normalizeFacetPSLGIndices
  , facetPSLGBoundaryEdges
  , facetPSLGAllConstraintEdges
  , collectFacetPSLGVertices
  , stableUniqueInts
  , triangulateFacetPSLG3D
  , triangulateFacetPolygons3D
  , triangulateFacetPSLGs3D
  , recoverFacets3D
  , recoverFacet3D
  , facetFacesRepresenting
  , validateRecoveredFacets3D
  , extractPLCTetrahedralization
  , interiorTetIdsByFlood
  ) where

import Control.Monad (foldM)
import Data.Array (Array, bounds, listArray, (!))
import Data.Kind (Constraint, Type)
import Data.List (sortBy)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Ord (comparing)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M

import Moonlight.Core (adjacentPairs)
import qualified Moonlight.Geometry.Triangulation as CDT2
import Moonlight.Geometry.Predicate
  ( Point3
  , orient3Exact, orient3Sign, orientExact
  , crossExact, sub3, cross3, dot3, midpoint3
  , orient2Rat
  , pointOnClosedSegment, projectionParam
  )
import Moonlight.Geometry.Tetrahedralization.Core
  ( SegmentIx, FacetTriIx, TetIx
  , HasXYZ(..)
  , CDT3DOptions(..), defaultCDT3DOptions
  , Mesh(..), FaceKey
  , pointOf, originOf, tetVerts, tetFaceKeys, neighborAcrossFace
  , canon2, canon3, mkArray
  , dedupSegments, recoverSegments, insertSteinerPoint
  )
import Moonlight.Geometry.Tetrahedralization.Intersection
  ( FacetPointClass(..), SegmentFacetHit(..)
  , pointClassOnFacet, segmentTriangleHit
  , triArea2RatByAxis, dominantAxisRat3, crossZero, projectRat
  )

type FacetPolyIx :: Type
data FacetPolyIx = FacetPolyIx
  { facetOuterLoop :: ![Int]
  , facetHoleLoops :: ![[Int]]
  } deriving stock (Eq, Show)

type FacetPSLGIx :: Type
data FacetPSLGIx = FacetPSLGIx
  { facetPSLGOuterLoop        :: ![Int]
  , facetPSLGHoleLoops        :: ![[Int]]
  , facetPSLGInteriorSegments :: ![SegmentIx]
  , facetPSLGInteriorVertices :: ![Int]
  } deriving stock (Eq, Show)

polylineSegments :: [Int] -> [SegmentIx]
polylineSegments = adjacentPairs . stripClosedLoop

closedLoopSegments :: [Int] -> [SegmentIx]
closedLoopSegments = closedPairs . stripClosedLoop

facetPolyToPSLG :: FacetPolyIx -> FacetPSLGIx
facetPolyToPSLG (FacetPolyIx outer holes) = FacetPSLGIx outer holes [] []

type PLCFacet :: Type -> Constraint
class PLCFacet f where
  facetPSLG :: f -> FacetPSLGIx

instance PLCFacet FacetTriIx where
  facetPSLG (a, b, c) = FacetPSLGIx [a, b, c] [] [] []
instance PLCFacet [Int] where
  facetPSLG vs = FacetPSLGIx vs [] [] []
instance PLCFacet [[Int]] where
  facetPSLG [] = FacetPSLGIx [] [] [] []
  facetPSLG (outer:holes) = FacetPSLGIx outer holes [] []
instance PLCFacet ([Int], [[Int]]) where
  facetPSLG (outer, holes) = FacetPSLGIx outer holes [] []
instance PLCFacet ([Int], [[Int]], [SegmentIx]) where
  facetPSLG (outer, holes, segs) = FacetPSLGIx outer holes segs []
instance PLCFacet ([Int], [[Int]], [SegmentIx], [Int]) where
  facetPSLG (outer, holes, segs, verts) = FacetPSLGIx outer holes segs verts
instance PLCFacet FacetPolyIx where
  facetPSLG = facetPolyToPSLG
instance PLCFacet FacetPSLGIx where
  facetPSLG = id

type PLC3DOptions :: Type
data PLC3DOptions = PLC3DOptions
  { plcBaseOptions                :: !CDT3DOptions
  , plcMaxFacetSplits             :: !Int
  , plcSplitSegmentIntersections  :: !Bool
  , plcValidatePLCInput           :: !Bool
  , plcClassifyInterior           :: !Bool
  } deriving stock (Eq, Show)

defaultPLC3DOptions :: PLC3DOptions
defaultPLC3DOptions = PLC3DOptions
  { plcBaseOptions = defaultCDT3DOptions, plcMaxFacetSplits = 20
  , plcSplitSegmentIntersections = True, plcValidatePLCInput = True, plcClassifyInterior = True }

type PLCTetrahedralization :: Type -> Type
data PLCTetrahedralization a = PLCTetrahedralization
  { plcPoints          :: !(Array Int (Point3 a))
  , plcOrigin          :: !(Array Int (Maybe Int))
  , plcSegments        :: ![SegmentIx]
  , plcSupportSegments :: ![SegmentIx]
  , plcSurfaceFacets   :: ![FacetTriIx]
  , plcTetrahedra      :: ![TetIx]
  } deriving stock (Eq, Show)

stableUniqueInts :: [Int] -> [Int]
stableUniqueInts = go IS.empty
  where go !_ [] = []; go !seen (x:xs) = if x `IS.member` seen then go seen xs else x : go (IS.insert x seen) xs

pointTableFromList :: [Point3 a] -> IM.IntMap (Point3 a)
pointTableFromList = IM.fromList . zip [0 :: Int ..]

lookupPointByIndex :: String -> IM.IntMap (Point3 a) -> Int -> Either String (Point3 a)
lookupPointByIndex message pointTable pointId =
  case IM.lookup pointId pointTable of
    Nothing -> Left (message ++ show pointId)
    Just pointValue -> Right pointValue

stripClosedLoop :: Eq a => [a] -> [a]
stripClosedLoop [] = []
stripClosedLoop [value] = [value]
stripClosedLoop (firstValue : remainingValues) =
  case reverse remainingValues of
    [] -> [firstValue]
    lastValue : reversedInit ->
      if firstValue == lastValue
        then firstValue : reverse reversedInit
        else firstValue : remainingValues

closedPairs :: [a] -> [(a, a)]
closedPairs [] = []
closedPairs [value] = [(value, value)]
closedPairs (firstValue : remainingValues) = go firstValue firstValue remainingValues
  where
    go :: a -> a -> [a] -> [(a, a)]
    go startValue currentValue [] = [(currentValue, startValue)]
    go startValue currentValue (nextValue : tailValues) =
      (currentValue, nextValue) : go startValue nextValue tailValues

facetPSLGBoundaryEdges :: FacetPSLGIx -> [SegmentIx]
facetPSLGBoundaryEdges (FacetPSLGIx outer holes _ _) =
  dedupSegments (loopEdges outer ++ concatMap loopEdges holes)
  where
    loopEdges :: [Int] -> [SegmentIx]
    loopEdges = closedPairs

facetPSLGAllConstraintEdges :: FacetPSLGIx -> [SegmentIx]
facetPSLGAllConstraintEdges facet@(FacetPSLGIx _ _ segs _) =
  dedupSegments (facetPSLGBoundaryEdges facet ++ map canon2 segs)

collectFacetPSLGVertices :: FacetPSLGIx -> [Int]
collectFacetPSLGVertices (FacetPSLGIx outer holes segs verts) =
  stableUniqueInts (outer ++ concat holes ++ concatMap (\(u, v) -> [u, v]) segs ++ verts)

normalizeFacetPSLGIndices :: Int -> FacetPSLGIx -> Either String FacetPSLGIx
normalizeFacetPSLGIndices n (FacetPSLGIx outer holes segs verts) = do
  outer' <- normalizeLoop "outer facet loop" n outer
  holes' <- mapM (normalizeLoop "facet hole loop" n) holes
  segs' <- mapM (normalizeSeg "facet interior segment" n) segs
  verts' <- normalizeVertexList "facet interior vertex" n verts
  pure (FacetPSLGIx outer' holes' (dedupSegments segs') verts')
  where
    normalizeLoop label n0 xs0 =
      let xs = stripClosedLoop xs0
          bad = listToMaybe [ i | i <- xs, i < 0 || i >= n0 ]
          dups = repeatedVertices xs
      in case () of
           _ | length xs < 3 -> Left ("validatePLC3D: " ++ label ++ " has fewer than three distinct vertices")
             | Just i <- bad -> Left ("validatePLC3D: " ++ label ++ " contains out-of-bounds vertex index " ++ show i)
             | Just i <- dups -> Left ("validatePLC3D: " ++ label ++ " repeats vertex index " ++ show i)
             | otherwise -> pure xs
    normalizeSeg label n0 (u, v)
      | u < 0 || v < 0 || u >= n0 || v >= n0 = Left ("validatePLC3D: " ++ label ++ " index out of bounds: " ++ show (u, v))
      | u == v = Left ("validatePLC3D: " ++ label ++ " is zero-length: " ++ show (u, v))
      | otherwise = pure (canon2 (u, v))
    normalizeVertexList label n0 xs = case listToMaybe [ i | i <- xs, i < 0 || i >= n0 ] of
      Just i -> Left ("validatePLC3D: " ++ label ++ " index out of bounds: " ++ show i)
      Nothing -> pure (stableUniqueInts xs)
    repeatedVertices xs = go IS.empty xs
      where go _ [] = Nothing; go !seen (y:ys) = if y `IS.member` seen then Just y else go (IS.insert y seen) ys

prepareFacetPSLGsAndTriangles
  :: forall f a. (PLCFacet f, RealFloat a, Show a)
  => [Point3 a] -> [f] -> Either String ([FacetPSLGIx], [FacetTriIx])
prepareFacetPSLGsAndTriangles pts rawFacetSpecs = do
  specs <- mapM (normalizeFacetPSLGIndices (length pts) . facetPSLG) rawFacetSpecs
  tris <- concat <$> mapM (triangulateFacetPSLG3D pts) specs
  pure (specs, tris)

triangulateFacetPolygons3D
  :: forall p f a. (HasXYZ p a, PLCFacet f, RealFloat a, Show a)
  => [p] -> [f] -> Either String [FacetTriIx]
triangulateFacetPolygons3D ptsIn rawFacetSpecs =
  snd <$> prepareFacetPSLGsAndTriangles (map pointXYZ ptsIn) rawFacetSpecs

triangulateFacetPSLGs3D
  :: forall p f a. (HasXYZ p a, PLCFacet f, RealFloat a, Show a)
  => [p] -> [f] -> Either String [FacetTriIx]
triangulateFacetPSLGs3D = triangulateFacetPolygons3D

triangulateFacetPSLG3D
  :: forall a. (RealFloat a, Show a) => [Point3 a] -> FacetPSLGIx -> Either String [FacetTriIx]
triangulateFacetPSLG3D pts facet = do
  let globalIds = collectFacetPSLGVertices facet
      pointTable = pointTableFromList pts
      localToGlobal = listArray (0, length globalIds - 1) globalIds
      globalToLocal = IM.fromList (zip globalIds [0 :: Int ..])
      localizeId label gid = fromMaybe (error ("triangulateFacetPSLG3D: missing local " ++ label ++ " id " ++ show gid)) (IM.lookup gid globalToLocal)
      localizeEdge (u, v) = (localizeId "edge" u, localizeId "edge" v)
  (origin, basisU, basisV) <- facetProjectionBasis pts facet
  localPoints <-
    traverse
      ( \globalId ->
          projectPointToFacet2 origin basisU basisV
            <$> lookupPointByIndex "triangulateFacetPSLG3D: missing global point index " pointTable globalId
      )
      globalIds
  let
      localPointArr = listArray (0, length localPoints - 1) localPoints
      localEdges = map localizeEdge (facetPSLGAllConstraintEdges facet)
      localBoundaryEdges = map localizeEdge (facetPSLGBoundaryEdges facet)
      outerLocal = map (localizeId "outer") (facetPSLGOuterLoop facet)
      localInteriorSegs = map localizeEdge (facetPSLGInteriorSegments facet)
      localInteriorVerts = map (localizeId "interior vertex") (facetPSLGInteriorVertices facet)
      refSign = signedAreaLoop2 localPointArr outerLocal
  if refSign == 0
    then Left ("validatePLC3D: degenerate polygonal facet loop " ++ show (facetPSLGOuterLoop facet))
    else do
      trisLocal <- CDT2.triangulateCDTPartitioned localPoints localEdges localBoundaryEdges (padBBox2 (bboxOfPoints2 localPoints))
      validateLocalFacetPSLGResult localToGlobal localPointArr localInteriorSegs localInteriorVerts trisLocal
      let orientLikeRef (i, j, k) =
            let cur = orientExact (localPointArr ! i) (localPointArr ! j) (localPointArr ! k)
            in if sameRationalSign refSign cur then (i, j, k) else (i, k, j)
      pure [ (localToGlobal ! i, localToGlobal ! j, localToGlobal ! k) | (i, j, k) <- map orientLikeRef trisLocal ]

validateLocalFacetPSLGResult
  :: forall a. RealFloat a
  => Array Int Int -> Array Int (CDT2.Point2 a) -> [SegmentIx] -> [Int] -> [CDT2.TriIx] -> Either String ()
validateLocalFacetPSLGResult localToGlobal localPts featureSegs featureVerts tris = do
  let (_, hi) = bounds localPts; allIds = [0 .. hi]; base = hi + 2
      triEdgeSet = IS.fromList [ packLocalEdge base e | (a, b, c) <- tris, e <- [canon2 (a, b), canon2 (b, c), canon2 (c, a)] ]
      usedVerts = IS.fromList [ v | (a, b, c) <- tris, v <- [a, b, c] ]
      checkFeatureVert v
        | v `IS.member` usedVerts = pure ()
        | otherwise = Left ("validatePLC3D: in-facet isolated vertex " ++ show (localToGlobal ! v) ++ " is not represented in the kept facet triangulation")
      checkFeatureSeg (u, v) =
        let pu = localPts ! u; pv = localPts ! v
            mids = [ i | i <- allIds, i /= u, i /= v, pointOnClosedSegment pu pv (localPts ! i) ]
            chain = u : sortBy (comparing (projectionParam pu pv . (localPts !))) mids ++ [v]
            pieces = [ canon2 (a, b) | (a, b) <- adjacentPairs chain, a /= b ]
        in case listToMaybe [ e | e <- pieces, packLocalEdge base e `IS.notMember` triEdgeSet ] of
             Nothing -> pure ()
             Just (m0, m1) -> Left ("validatePLC3D: in-facet interior segment " ++ show (localToGlobal ! u, localToGlobal ! v)
                                   ++ " is not fully represented; missing piece " ++ show (localToGlobal ! m0, localToGlobal ! m1))
  mapM_ checkFeatureVert (stableUniqueInts featureVerts)
  mapM_ checkFeatureSeg (dedupSegments featureSegs)

packLocalEdge :: Int -> SegmentIx -> Int
packLocalEdge base (u, v) = let (a, b) = canon2 (u, v) in a * base + b

facetProjectionBasis :: forall a. RealFloat a => [Point3 a] -> FacetPSLGIx -> Either String (Point3 a, Point3 a, Point3 a)
facetProjectionBasis pts facet =
  case facetPSLGOuterLoop facet of
    i0 : i1 : rest -> do
      let pointTable = pointTableFromList pts
          candidateIds =
            rest
              ++ concat (facetPSLGHoleLoops facet)
              ++ concatMap (\(u, v) -> [u, v]) (facetPSLGInteriorSegments facet)
              ++ facetPSLGInteriorVertices facet
      p0 <- lookupPointByIndex "validatePLC3D: missing facet point index " pointTable i0
      p1 <- lookupPointByIndex "validatePLC3D: missing facet point index " pointTable i1
      case listToMaybe [ candidateId | candidateId <- candidateIds, goodCandidate pointTable p0 p1 candidateId ] of
        Nothing -> Left ("validatePLC3D: degenerate/collapsed polygonal facet loop " ++ show (facetPSLGOuterLoop facet))
        Just i2 -> do
          p2 <- lookupPointByIndex "validatePLC3D: missing facet point index " pointTable i2
          let u = sub3 p1 p0
              n = cross3 u (sub3 p2 p0)
              v = cross3 n u
              nonCoplanarVertex =
                listToMaybe
                  [ vertexId
                  | vertexId <- collectFacetPSLGVertices facet
                  , maybe False (\pointValue -> orient3Sign p0 p1 p2 pointValue /= EQ) (IM.lookup vertexId pointTable)
                  ]
          case nonCoplanarVertex of
            Just vertexId ->
              Left ("validatePLC3D: non-coplanar facet vertex " ++ show vertexId ++ " in facet loop " ++ show (facetPSLGOuterLoop facet))
            Nothing -> pure (p0, u, v)
    _ -> Left "validatePLC3D: facet must have at least three vertices"
  where
    goodCandidate :: IM.IntMap (Point3 a) -> Point3 a -> Point3 a -> Int -> Bool
    goodCandidate pointTable p0 p1 candidateId =
      maybe False (\pointValue -> not (crossZero (crossExact (sub3 p1 p0) (sub3 pointValue p0)))) (IM.lookup candidateId pointTable)

projectPointToFacet2 :: Num a => Point3 a -> Point3 a -> Point3 a -> Point3 a -> CDT2.Point2 a
projectPointToFacet2 origin basisU basisV p = let q = sub3 p origin in (dot3 q basisU, dot3 q basisV)

bboxOfPoints2 :: RealFloat a => [CDT2.Point2 a] -> CDT2.BBox a
bboxOfPoints2 [] = (0, 0, 1, 1)
bboxOfPoints2 ((x0, y0):ps) = foldl' step (x0, y0, x0, y0) ps
  where
    step :: Ord a => (a, a, a, a) -> (a, a) -> (a, a, a, a)
    step (!xmin, !ymin, !xmax, !ymax) (x, y) = (min xmin x, min ymin y, max xmax x, max ymax y)

padBBox2 :: RealFloat a => CDT2.BBox a -> CDT2.BBox a
padBBox2 (xmin, ymin, xmax, ymax) = let m = max (xmax - xmin) (ymax - ymin) + 1 in (xmin - m, ymin - m, xmax + m, ymax + m)

toRat2 :: Real a => CDT2.Point2 a -> (Rational, Rational)
toRat2 (x, y) = (toRational x, toRational y)

signedAreaLoop2 :: Real a => Array Int (CDT2.Point2 a) -> [Int] -> Rational
signedAreaLoop2 pts ids = sum
  [ let (x1, y1) = toRat2 (pts ! i); (x2, y2) = toRat2 (pts ! j) in x1 * y2 - y1 * x2
  | (i, j) <- closedPairs ids ] / 2

sameRationalSign :: Rational -> Rational -> Bool
sameRationalSign ref cur = ref == 0 || cur == 0 || (ref > 0 && cur > 0) || (ref < 0 && cur < 0)

recoverFacets3D
  :: forall a. (RealFloat a, Show a)
  => Int -> Int -> [FacetTriIx] -> Mesh a -> Either String (Mesh a, [SegmentIx], [FacetTriIx])
recoverFacets3D maxFacetDepth maxSegDepth facets mesh0 = loop 0 mesh0 []
  where
    maxPasses = 4 * max 1 (length facets) + 32
    loop !pass !mesh !support
      | pass > maxPasses = Left ("tetrahedralizePLC3D: facet recovery failed to stabilize within " ++ show maxPasses ++ " passes")
      | otherwise =
          let missing = [ f | f <- facets, facetFacesRepresenting f mesh == Nothing ]
          in if null missing
               then pure (mesh, dedupSegments support, dedupOrientedFacets (concatMap (\f -> fromMaybe [] (facetFacesRepresenting f mesh)) facets))
               else do { (mesh1, support1) <- foldM step (mesh, support) missing; loop (pass + 1) mesh1 support1 }
    step (!mesh, !support) facet = do
      (mesh1, addedSupport) <- recoverFacet3D maxFacetDepth maxSegDepth facet mesh
      pure (mesh1, addedSupport ++ support)

recoverFacet3D
  :: forall a. (RealFloat a, Show a) => Int -> Int -> FacetTriIx -> Mesh a -> Either String (Mesh a, [SegmentIx])
recoverFacet3D maxFacetDepth maxSegDepth facet mesh0 = go 0 facet mesh0 []
  where
    go !depth f@(a, b, c) !mesh !support
      | depth > maxFacetDepth = Left ("tetrahedralizePLC3D: exceeded facet split depth while recovering facet " ++ show facet)
      | otherwise = case facetFacesRepresenting f mesh of
          Just _ -> pure (mesh, support)
          Nothing -> case findExistingFacetSplitter f mesh of
            Just pid -> splitWith pid f depth mesh support
            Nothing -> case findCrossingEdgeFacetSplitter f mesh of
              Just p -> do { (mesh1, pid) <- insertSteinerPoint p mesh; splitWith pid f depth mesh1 support }
              Nothing -> do
                let !p = facetCentroid (pointOf mesh a) (pointOf mesh b) (pointOf mesh c)
                (mesh1, pid) <- insertSteinerPoint p mesh
                splitWith pid f depth mesh1 support
    splitWith pid f@(a, b, c) depth mesh support
      | pid == a || pid == b || pid == c = pure (mesh, support)
      | otherwise = do
          let (!subs, !needSegs) = splitFacetByPoint pid f mesh
          (mesh1, gotSegs) <- recoverSegments maxSegDepth needSegs mesh
          foldM (\(!m, !acc) sf -> do { (m1, acc1) <- go (depth + 1) sf m (gotSegs ++ acc); pure (m1, acc1) })
                (mesh1, support ++ gotSegs) subs

facetCentroid :: Fractional a => Point3 a -> Point3 a -> Point3 a -> Point3 a
facetCentroid (ax, ay, az) (bx, by, bz) (cx, cy, cz) = ((ax+bx+cx)/3, (ay+by+cy)/3, (az+bz+cz)/3)

splitFacetByPoint :: forall a. RealFloat a => Int -> FacetTriIx -> Mesh a -> ([FacetTriIx], [SegmentIx])
splitFacetByPoint pid (a, b, c) mesh =
  case pointClassOnFacet (pointOf mesh a) (pointOf mesh b) (pointOf mesh c) (pointOf mesh pid) of
    FacetAtVertex _ -> ([(a, b, c)], [])
    FacetInside -> ([(a,b,pid),(b,c,pid),(c,a,pid)], dedupSegments [(a,pid),(b,pid),(c,pid)])
    FacetOnEdge 0 -> ([(a,pid,c),(pid,b,c)], dedupSegments [(a,pid),(pid,b),(pid,c)])
    FacetOnEdge 1 -> ([(b,pid,a),(pid,c,a)], dedupSegments [(b,pid),(pid,c),(pid,a)])
    FacetOnEdge 2 -> ([(c,pid,b),(pid,a,b)], dedupSegments [(c,pid),(pid,a),(pid,b)])
    FacetOnEdge _ -> ([(a, b, c)], [])
    FacetOutside -> ([(a, b, c)], [])

findExistingFacetSplitter :: forall a. RealFloat a => FacetTriIx -> Mesh a -> Maybe Int
findExistingFacetSplitter (a, b, c) mesh =
  let pa = pointOf mesh a; pb = pointOf mesh b; pc = pointOf mesh c
  in listToMaybe [ pid | (pid, p) <- IM.toList (meshPoints mesh), pid `IS.notMember` meshSuper mesh, pid /= a, pid /= b, pid /= c, pointClassOnFacet pa pb pc p /= FacetOutside ]

findCrossingEdgeFacetSplitter :: forall a. RealFloat a => FacetTriIx -> Mesh a -> Maybe (Point3 a)
findCrossingEdgeFacetSplitter (a, b, c) mesh =
  listToMaybe $ mapMaybe testEdge (M.keys (meshEdgeCount mesh))
  where
    pa = pointOf mesh a; pb = pointOf mesh b; pc = pointOf mesh c
    boundary = map canon2 [(a, b), (b, c), (c, a)]
    testEdge ek@(u, v)
      | canon2 ek `elem` boundary = Nothing
      | u `IS.member` meshSuper mesh || v `IS.member` meshSuper mesh = Nothing
      | otherwise = case segmentTriangleHit (pointOf mesh u) (pointOf mesh v) pa pb pc of
          NoFacetHit -> Nothing
          HitFacetPoint p -> case pointClassOnFacet pa pb pc p of
            FacetInside -> Just p; FacetOnEdge _ -> if p /= pa && p /= pb && p /= pc then Just p else Nothing; _ -> Nothing
          HitFacetOverlap p q -> listToMaybe (filter (\r -> pointClassOnFacet pa pb pc r == FacetInside) [p, q, midpoint3 p q])

facetFacesRepresenting :: forall a. RealFloat a => FacetTriIx -> Mesh a -> Maybe [FacetTriIx]
facetFacesRepresenting (a, b, c) mesh =
  let pa = pointOf mesh a; pb = pointOf mesh b; pc = pointOf mesh c
      axis = dominantAxisRat3 (crossExact (sub3 pb pa) (sub3 pc pa))
      area0 = triArea2RatByAxis axis pa pb pc
      candidates = [ orientFaceLikeFacet (a,b,c) fk mesh
                   | (fk@(i,j,k), tids) <- M.toList (meshFaceMap mesh), not (null tids)
                   , all (\pid -> orient3Exact pa pb pc (pointOf mesh pid) == 0) [i,j,k]
                   , all (\pid -> pointClassOnFacet pa pb pc (pointOf mesh pid) /= FacetOutside) [i,j,k] ]
      areaSum = sum [ triArea2RatByAxis axis (pointOf mesh i) (pointOf mesh j) (pointOf mesh k) | (i,j,k) <- candidates ]
  in if not (null candidates) && areaSum == area0 then Just candidates else Nothing

orientFaceLikeFacet :: forall a. RealFloat a => FacetTriIx -> FacetTriIx -> Mesh a -> FacetTriIx
orientFaceLikeFacet (a, b, c) (i, j, k) mesh =
  let pa = pointOf mesh a; pb = pointOf mesh b; pc = pointOf mesh c
      axis = dominantAxisRat3 (crossExact (sub3 pb pa) (sub3 pc pa))
      ref = orient2Rat (projectRat axis pa) (projectRat axis pb) (projectRat axis pc)
      cur = orient2Rat (projectRat axis (pointOf mesh i)) (projectRat axis (pointOf mesh j)) (projectRat axis (pointOf mesh k))
  in if ref == 0 || cur == 0 || (ref > 0 && cur > 0) || (ref < 0 && cur < 0) then (i, j, k) else (i, k, j)

dedupOrientedFacets :: [FacetTriIx] -> [FacetTriIx]
dedupOrientedFacets fs = M.elems (foldl' (\m f -> M.insert (canon3 f) f m) M.empty fs)

validateRecoveredFacets3D :: forall a. RealFloat a => Mesh a -> [FacetTriIx] -> [FacetTriIx] -> Either String ()
validateRecoveredFacets3D mesh rawFacets surface = do
  let surfaceKeys = M.fromList [ (canon3 f, ()) | f <- surface ]
  mapM_ (\f -> if M.member (canon3 f) (meshFaceMap mesh) then pure () else Left ("validateRecoveredFacets3D: recovered face not present in mesh: " ++ show f)) surface
  mapM_ (\f -> case facetFacesRepresenting f mesh of
    Just faces | all (\g -> M.member (canon3 g) surfaceKeys) faces -> pure ()
    Just _ -> Left ("validateRecoveredFacets3D: facet " ++ show f ++ " represented, but not all constituent faces exported")
    Nothing -> Left ("validateRecoveredFacets3D: facet not represented in recovered surface: " ++ show f)) rawFacets

extractPLCTetrahedralization
  :: forall a. Bool -> Mesh a -> [SegmentIx] -> [SegmentIx] -> [FacetTriIx] -> PLCTetrahedralization a
extractPLCTetrahedralization classifyInside mesh boundarySegs supportSegs surface =
  let !superSet = meshSuper mesh
      !surfaceSet = M.fromList [ (canon3 f, ()) | f <- surface ]
      !keepTids = if classifyInside then interiorTetIdsByFlood mesh surfaceSet
                  else [ tid | (tid, tet) <- IM.toList (meshTets mesh), let (a,b,c,d) = tetVerts tet, all (`IS.notMember` superSet) [a,b,c,d] ]
      keepIds = [ i | i <- IM.keys (meshPoints mesh), i `IS.notMember` superSet ]
      remap = M.fromList (zip keepIds [0 :: Int ..])
      remapId i = fromMaybe (error ("extractPLCTetrahedralization: missing remap for point " ++ show i)) (M.lookup i remap)
      pts = [ pointOf mesh i | i <- keepIds ]; org = [ originOf mesh i | i <- keepIds ]
      segs1 = [ (remapId u, remapId v) | (u, v) <- dedupSegments boundarySegs, all (`M.member` remap) [u, v] ]
      segs2 = [ (remapId u, remapId v) | (u, v) <- dedupSegments supportSegs, all (`M.member` remap) [u, v] ]
      surf' = [ (remapId a, remapId b, remapId c) | (a, b, c) <- dedupOrientedFacets surface, all (`M.member` remap) [a, b, c] ]
      tets' = [ let (a,b,c,d) = tetVerts tet in (remapId a, remapId b, remapId c, remapId d)
              | tid <- keepTids, let tet = fromMaybe (error ("extractPLCTetrahedralization: missing tet " ++ show tid)) (IM.lookup tid (meshTets mesh)) ]
  in PLCTetrahedralization (mkArray pts) (mkArray org) segs1 segs2 surf' tets'

interiorTetIdsByFlood :: forall a. Mesh a -> M.Map FaceKey () -> [Int]
interiorTetIdsByFlood mesh surfaceSet =
  let seeds = [ tid | (tid, tet) <- IM.toList (meshTets mesh), let (a,b,c,d) = tetVerts tet, any (`IS.member` meshSuper mesh) [a,b,c,d] ]
      outside = bfsTet seeds IS.empty
  in [ tid | (tid, tet) <- IM.toList (meshTets mesh), tid `IS.notMember` outside, let (a,b,c,d) = tetVerts tet, all (`IS.notMember` meshSuper mesh) [a,b,c,d] ]
  where
    bfsTet [] !seen = seen
    bfsTet (tid:todo) !seen
      | tid `IS.member` seen = bfsTet todo seen
      | otherwise = case IM.lookup tid (meshTets mesh) of
          Nothing -> bfsTet todo seen
          Just tet -> bfsTet ([ nbr | fk <- tetFaceKeys tet, M.notMember fk surfaceSet, nbr <- maybe [] (:[]) (neighborAcrossFace fk tid mesh) ] ++ todo) (IS.insert tid seen)

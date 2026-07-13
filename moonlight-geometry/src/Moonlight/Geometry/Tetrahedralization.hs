{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Moonlight.Geometry.Tetrahedralization
  ( Point3
  , HasXYZ(..)
  , SegmentIx
  , FacetTriIx
  , FacetPolyIx(..)
  , FacetPSLGIx(..)
  , PLCFacet(..)
  , TetIx
  , BBox3
  , CDT3DOptions(..)
  , defaultCDT3DOptions
  , PLC3DOptions(..)
  , defaultPLC3DOptions
  , Tetrahedralization(..)
  , PLCTetrahedralization(..)
  , tetrahedralize3D
  , tetrahedralize3DWith
  , tetrahedralizePLC3D
  , tetrahedralizePLC3DWith
  , triangulateFacetPolygons3D
  , triangulateFacetPSLGs3D
  , polylineSegments
  , closedLoopSegments
  , validateInput3D
  , validatePLC3D
  , splitIntersectingSegments3D
  ) where

import Control.Monad (foldM, when)
import Data.Array (Array, listArray, (!))
import Data.List (sortBy)
import Data.Maybe (listToMaybe)
import Data.Ord (comparing)
import qualified Data.Map.Strict as M

import Moonlight.Geometry.Predicate (Point3, BBox3, bboxOfPoints3, unionBBox3)
import Moonlight.Geometry.SpaceFilling (mortonOf)
import Moonlight.Geometry.Tetrahedralization.Core
import Moonlight.Geometry.Tetrahedralization.Intersection (splitIntersectingSegments3D, validatePLC3DPoints)
import Moonlight.Geometry.Tetrahedralization.Facet

orderedIds3 :: RealFloat a => Bool -> BBox3 a -> Array Int (Point3 a) -> [Int] -> [Int]
orderedIds3 False _ _ ids = ids
orderedIds3 True bbox pts ids = map fst $ sortBy (comparing snd) [ (i, mortonOf bbox (pts ! i)) | i <- ids ]

tetrahedralize3D
  :: forall p a. (HasXYZ p a, RealFloat a, Show a)
  => [p] -> [SegmentIx] -> BBox3 a -> Either String (Tetrahedralization a)
tetrahedralize3D = tetrahedralize3DWith defaultCDT3DOptions

tetrahedralize3DWith
  :: forall p a. (HasXYZ p a, RealFloat a, Show a)
  => CDT3DOptions -> [p] -> [SegmentIx] -> BBox3 a -> Either String (Tetrahedralization a)
tetrahedralize3DWith opts ptsIn rawSegments bboxIn = do
  validateInput3D ptsIn rawSegments
  let !pts0 = map pointXYZ ptsIn
      !n0 = length pts0
      !ptsArr0 = listArray (0, n0 - 1) pts0
      !segments0 = splitSegmentsAtInputPoints pts0 rawSegments
      !bbox = unionBBox3 bboxIn (bboxOfPoints3 pts0)
      !order = orderedIds3 (cdt3DUseMorton opts) bbox ptsArr0 [0 .. n0 - 1]
      (!allPts, !allOrigin, !s0, !s1, !s2, !s3) = extendWithSuperTet pts0 bbox
  mesh0 <- initMesh allPts allOrigin s0 s1 s2 s3
  mesh1 <- foldM insertPointById mesh0 order
  (mesh2, segs2) <- recoverSegments (cdt3DMaxSegmentSplits opts) segments0 mesh1
  when (cdt3DValidate opts) $ validateMesh3D mesh2 segs2
  pure (extractTetrahedralization mesh2 segs2)

validateInput3D
  :: forall p a. (HasXYZ p a, RealFloat a, Show a) => [p] -> [SegmentIx] -> Either String ()
validateInput3D [] _ = Left "tetrahedralize3D: empty point set"
validateInput3D ptsIn segs = do
  let !pts = map pointXYZ ptsIn
      !n = length pts
      indexed = zip [0 :: Int ..] pts
      badCoord = listToMaybe [ (i, p) | (i, p@(x, y, z)) <- indexed, any bad [x, y, z] ]
      dupMap = foldl' insertDup M.empty indexed
      dups = [ (p, i0, i1) | (p, (i0, Just i1)) <- M.toList dupMap ]
  case badCoord of
    Just (i, p) -> Left ("tetrahedralize3D: non-finite coordinate at index " ++ show i ++ ": " ++ show p)
    Nothing -> pure ()
  case dups of
    ((p, i0, i1):_) -> Left ("tetrahedralize3D: duplicate points at indices " ++ show i0 ++ " and " ++ show i1 ++ " with coordinates " ++ show p)
    [] -> pure ()
  mapM_ (checkSeg n) segs
  where
    insertDup :: Ord point => M.Map point (Int, Maybe Int) -> (Int, point) -> M.Map point (Int, Maybe Int)
    insertDup acc (i, p) = case M.lookup p acc of
      Nothing -> M.insert p (i, Nothing) acc; Just (j, _) -> M.insert p (j, Just i) acc
    bad :: a -> Bool
    bad x = isNaN x || isInfinite x
    checkSeg :: Int -> SegmentIx -> Either String ()
    checkSeg sz (u, v)
      | u < 0 || v < 0 || u >= sz || v >= sz = Left ("tetrahedralize3D: segment index out of bounds: " ++ show (u, v))
      | u == v = Left ("tetrahedralize3D: zero-length constrained segment at index " ++ show u)
      | otherwise = pure ()

tetrahedralizePLC3D
  :: forall p f a. (HasXYZ p a, PLCFacet f, RealFloat a, Show a)
  => [p] -> [SegmentIx] -> [f] -> BBox3 a -> Either String (PLCTetrahedralization a)
tetrahedralizePLC3D = tetrahedralizePLC3DWith defaultPLC3DOptions

tetrahedralizePLC3DWith
  :: forall p f a. (HasXYZ p a, PLCFacet f, RealFloat a, Show a)
  => PLC3DOptions -> [p] -> [SegmentIx] -> [f] -> BBox3 a -> Either String (PLCTetrahedralization a)
tetrahedralizePLC3DWith opts ptsIn rawSegs rawFacetSpecs bboxIn = do
  let !ptsBase = map pointXYZ ptsIn
  validateInput3D ptsIn rawSegs
  (!pts0, !segs0) <-
    if plcSplitSegmentIntersections opts
      then splitIntersectingSegments3D ptsBase (dedupSegments rawSegs)
      else pure (ptsBase, dedupSegments rawSegs)
  (!facetSpecs, !rawFacets) <- prepareFacetPSLGsAndTriangles pts0 rawFacetSpecs
  when (plcValidatePLCInput opts) $ validatePLC3DPoints pts0 segs0 rawFacets
  let !facetSegs = dedupSegments (concatMap facetPSLGAllConstraintEdges facetSpecs)
      !allBoundarySegs = splitSegmentsAtInputPoints pts0 (segs0 ++ facetSegs)
      !bbox = unionBBox3 bboxIn (bboxOfPoints3 pts0)
      !n0 = length pts0
      !ptsArr0 = listArray (0, n0 - 1) pts0
      !order = orderedIds3 (cdt3DUseMorton (plcBaseOptions opts)) bbox ptsArr0 [0 .. n0 - 1]
      (!allPts, !allOrigin, !s0, !s1, !s2, !s3) = extendWithSuperTet pts0 bbox
  mesh0 <- initMesh allPts allOrigin s0 s1 s2 s3
  mesh1 <- foldM insertPointById mesh0 order
  (mesh2, boundarySegs) <- recoverSegments (cdt3DMaxSegmentSplits (plcBaseOptions opts)) allBoundarySegs mesh1
  (mesh3, supportSegs, surface) <- recoverFacets3D (plcMaxFacetSplits opts) (cdt3DMaxSegmentSplits (plcBaseOptions opts)) rawFacets mesh2
  let !allSegs = dedupSegments (boundarySegs ++ supportSegs)
  when (cdt3DValidate (plcBaseOptions opts) || plcValidatePLCInput opts) $ do
    validateMesh3D mesh3 allSegs
    validateRecoveredFacets3D mesh3 rawFacets surface
  pure (extractPLCTetrahedralization (plcClassifyInterior opts) mesh3 boundarySegs supportSegs surface)

validatePLC3D
  :: forall p f a. (HasXYZ p a, PLCFacet f, RealFloat a, Show a)
  => [p] -> [SegmentIx] -> [f] -> Either String ()
validatePLC3D ptsIn rawSegs rawFacetSpecs = do
  validateInput3D ptsIn rawSegs
  let !pts = map pointXYZ ptsIn
  (_, rawFacets) <- prepareFacetPSLGsAndTriangles pts rawFacetSpecs
  validatePLC3DPoints pts rawSegs rawFacets

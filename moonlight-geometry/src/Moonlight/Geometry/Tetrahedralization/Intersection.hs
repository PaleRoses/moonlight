{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Moonlight.Geometry.Tetrahedralization.Intersection
  ( SegmentIntersection3(..)
  , SegmentFacetHit(..)
  , FacetPointClass(..)
  , SegmentIntersection2(..)
  , segmentSegmentIntersection3
  , segmentTriangleHit
  , pointClassOnFacet
  , triArea2RatByAxis
  , hitPoints
  , uniqPoints3
  , crossZero
  , dominantAxisRat3
  , dominantAxisPointSpan
  , coordRat
  , projectRat
  , splitIntersectingSegments3D
  , validatePLC3DPoints
  , trianglesImproperlyIntersect
  , coplanarTrianglesImproperlyIntersect
  , strictInside2
  , coplanarSegmentsImproper
  , segmentSegment2Rat
  , checkFacet
  , checkSegPair
  , checkSegFacet
  , checkFacetPair
  , segmentIsFacetBoundary
  , facetEdges
  , facetDegenerate
  , indexedPairs
  , sharedVerticesSegSeg
  , pointHitAllowedBySharing
  , overlapAllowedBySharing
  ) where

import Control.Monad (foldM)
import Data.Kind (Type)
import Data.List (sortBy)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Ord (comparing)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M

import Moonlight.Core (adjacentPairs)
import Moonlight.Geometry.Predicate
  ( Point3, orient3Exact
  , crossExact, sub3
  , fst3, snd3, trd3
  , orient2Rat, sub2Rat, cross2Rat, add2Rat, scale2Rat, lerp3Rat
  , pointOnClosedSegment3, projectionParam3
  )
import Moonlight.Geometry.Tetrahedralization.Core
  ( SegmentIx, FacetTriIx, canon2, canon3, dedupSegments )

type SegmentIntersection3 :: Type -> Type
data SegmentIntersection3 a
  = SegDisjoint
  | SegPoint !(Point3 a)
  | SegOverlap !(Point3 a) !(Point3 a)
  deriving stock (Eq, Show)

type SegmentFacetHit :: Type -> Type
data SegmentFacetHit a
  = NoFacetHit
  | HitFacetPoint !(Point3 a)
  | HitFacetOverlap !(Point3 a) !(Point3 a)
  deriving stock (Eq, Show)

type FacetPointClass :: Type
data FacetPointClass
  = FacetOutside
  | FacetAtVertex !Int
  | FacetOnEdge !Int
  | FacetInside
  deriving stock (Eq, Show)

type SegmentIntersection2 :: Type
data SegmentIntersection2
  = Seg2None
  | Seg2Point !(Rational, Rational)
  | Seg2Overlap !(Rational, Rational) !(Rational, Rational)
  deriving stock (Eq, Show)

crossZero :: (Rational, Rational, Rational) -> Bool
crossZero (x, y, z) = x == 0 && y == 0 && z == 0

dominantAxisRat3 :: (Rational, Rational, Rational) -> Int
dominantAxisRat3 (x, y, z)
  | abs x >= abs y && abs x >= abs z = 0
  | abs y >= abs z = 1
  | otherwise = 2

dominantAxisPointSpan :: Real a => Point3 a -> Point3 a -> Int
dominantAxisPointSpan a b =
  let dx = abs (toRational (fst3 b) - toRational (fst3 a))
      dy = abs (toRational (snd3 b) - toRational (snd3 a))
      dz = abs (toRational (trd3 b) - toRational (trd3 a))
  in if dx >= dy && dx >= dz then 0 else if dy >= dz then 1 else 2

coordRat :: Real a => Int -> Point3 a -> Rational
coordRat axis (x, y, z) = case axis of
  0 -> toRational x; 1 -> toRational y; _ -> toRational z

projectRat :: Real a => Int -> Point3 a -> (Rational, Rational)
projectRat axis (x, y, z) = case axis of
  0 -> (toRational y, toRational z)
  1 -> (toRational x, toRational z)
  _ -> (toRational x, toRational y)

segmentSegmentIntersection3 :: forall a. RealFloat a
                            => Point3 a -> Point3 a -> Point3 a -> Point3 a -> SegmentIntersection3 a
segmentSegmentIntersection3 p0 p1 q0 q1 =
  let !r = sub3 p1 p0; !s = sub3 q1 q0
      !c = crossExact r s; !cop = orient3Exact p0 p1 q0 q1
  in if crossZero c
       then if crossZero (crossExact (sub3 q0 p0) r) then collinearCase else SegDisjoint
       else if cop /= 0 then SegDisjoint else properCoplanarCase c
  where
    properCoplanarCase c =
      let axis = dominantAxisRat3 c
          r2 = sub2Rat (projectRat axis p1) (projectRat axis p0)
          s2 = sub2Rat (projectRat axis q1) (projectRat axis q0)
          qp2 = sub2Rat (projectRat axis q0) (projectRat axis p0)
          den = cross2Rat r2 s2
      in if den == 0 then SegDisjoint
         else let t = cross2Rat qp2 s2 / den; u = cross2Rat qp2 r2 / den
              in if t < 0 || t > 1 || u < 0 || u > 1 then SegDisjoint else SegPoint (lerp3Rat t p0 p1)
    collinearCase =
      let axis = dominantAxisPointSpan p0 p1
          c0 = coordRat axis p0; c1 = coordRat axis p1; den = c1 - c0
      in if den == 0 then if p0 == q0 then SegPoint p0 else SegDisjoint
         else let tq0 = (coordRat axis q0 - c0) / den; tq1 = (coordRat axis q1 - c0) / den
                  lo = max 0 (min tq0 tq1); hi = min 1 (max tq0 tq1)
              in if hi < lo then SegDisjoint
                 else if hi == lo then SegPoint (lerp3Rat lo p0 p1)
                 else SegOverlap (lerp3Rat lo p0 p1) (lerp3Rat hi p0 p1)

segmentTriangleHit :: forall a. RealFloat a
                   => Point3 a -> Point3 a -> Point3 a -> Point3 a -> Point3 a -> SegmentFacetHit a
segmentTriangleHit p0 p1 a b c =
  let s0 = orient3Exact a b c p0; s1 = orient3Exact a b c p1
  in if s0 == 0 && s1 == 0 then coplanarCase
     else if (s0 > 0 && s1 > 0) || (s0 < 0 && s1 < 0) then NoFacetHit
     else let t = s0 / (s0 - s1); p = lerp3Rat t p0 p1
          in case pointClassOnFacet a b c p of { FacetOutside -> NoFacetHit; _ -> HitFacetPoint p }
  where
    coplanarCase =
      let cands = concatMap hitPoints [ segmentSegmentIntersection3 p0 p1 a b
                                      , segmentSegmentIntersection3 p0 p1 b c
                                      , segmentSegmentIntersection3 p0 p1 c a ]
          cands1 = if pointClassOnFacet a b c p0 /= FacetOutside then p0 : cands else cands
          cands2 = if pointClassOnFacet a b c p1 /= FacetOutside then p1 : cands1 else cands1
          uniq = uniqPoints3 cands2
      in case uniq of
           [] -> NoFacetHit
           [p] -> HitFacetPoint p
           ps ->
             case sortBy (comparing (projectionParam3 p0 p1)) ps of
               [] -> NoFacetHit
               [pointValue] -> HitFacetPoint pointValue
               firstPoint : remainingPoints ->
                 case reverse remainingPoints of
                   lastPoint : _ -> HitFacetOverlap firstPoint lastPoint
                   [] -> HitFacetPoint firstPoint

hitPoints :: SegmentIntersection3 a -> [Point3 a]
hitPoints SegDisjoint = []; hitPoints (SegPoint p) = [p]; hitPoints (SegOverlap p q) = [p, q]

uniqPoints3 :: Ord a => [Point3 a] -> [Point3 a]
uniqPoints3 xs = M.keys (foldl' (\m p -> M.insert p () m) M.empty xs)

pointTableFromList :: [Point3 a] -> IM.IntMap (Point3 a)
pointTableFromList = IM.fromList . zip [0 :: Int ..]

lookupPointByIndex :: String -> IM.IntMap (Point3 a) -> Int -> Either String (Point3 a)
lookupPointByIndex message pointTable pointId =
  case IM.lookup pointId pointTable of
    Nothing -> Left (message ++ show pointId)
    Just pointValue -> Right pointValue

pointClassOnFacet :: forall a. RealFloat a => Point3 a -> Point3 a -> Point3 a -> Point3 a -> FacetPointClass
pointClassOnFacet a b c p
  | p == a = FacetAtVertex 0
  | p == b = FacetAtVertex 1
  | p == c = FacetAtVertex 2
  | orient3Exact a b c p /= 0 = FacetOutside
  | not inside = FacetOutside
  | pointOnClosedSegment3 a b p = FacetOnEdge 0
  | pointOnClosedSegment3 b c p = FacetOnEdge 1
  | pointOnClosedSegment3 c a p = FacetOnEdge 2
  | otherwise = FacetInside
  where
    axis = dominantAxisRat3 (crossExact (sub3 b a) (sub3 c a))
    a2 = projectRat axis a; b2 = projectRat axis b; c2 = projectRat axis c; p2 = projectRat axis p
    o = orient2Rat a2 b2 c2
    s1 = orient2Rat a2 b2 p2; s2 = orient2Rat b2 c2 p2; s3 = orient2Rat c2 a2 p2
    inside
      | o > 0 = s1 >= 0 && s2 >= 0 && s3 >= 0
      | o < 0 = s1 <= 0 && s2 <= 0 && s3 <= 0
      | otherwise = False

triArea2RatByAxis :: forall a. Real a => Int -> Point3 a -> Point3 a -> Point3 a -> Rational
triArea2RatByAxis axis a b c =
  abs (orient2Rat (projectRat axis a) (projectRat axis b) (projectRat axis c)) / 2

pointHitAllowedBySharing :: forall a. RealFloat a => IM.IntMap (Point3 a) -> Point3 a -> SegmentIx -> FacetTriIx -> Bool
pointHitAllowedBySharing pointTable p (u, v) (a, b, c) =
  any (== p) (mapMaybe (\pointId -> IM.lookup pointId pointTable) [ q | q <- [u, v], q == a || q == b || q == c ])

overlapAllowedBySharing :: forall a. RealFloat a => IM.IntMap (Point3 a) -> (Point3 a, Point3 a) -> SegmentIx -> FacetTriIx -> Bool
overlapAllowedBySharing pointTable (p, q) s f =
  any (\e -> sameSupport e (p, q)) (facetEdges f) && segmentIsFacetBoundary s f
  where
    sameSupport (i, j) (x, y) =
      case (IM.lookup i pointTable, IM.lookup j pointTable) of
        (Just leftPoint, Just rightPoint) ->
          pointOnClosedSegment3 leftPoint rightPoint x && pointOnClosedSegment3 leftPoint rightPoint y
        _ -> False

trianglesImproperlyIntersect :: forall a. RealFloat a => [Point3 a] -> FacetTriIx -> FacetTriIx -> Bool
trianglesImproperlyIntersect pts f@(a, b, c) g@(d, e, h)
  | canon3 f == canon3 g = True
  | otherwise =
      case (,,,,,) <$> IM.lookup a pointTable <*> IM.lookup b pointTable <*> IM.lookup c pointTable <*> IM.lookup d pointTable <*> IM.lookup e pointTable <*> IM.lookup h pointTable of
        Nothing -> False
        Just (pa, pb, pc, pd, pe, ph)
          | coplanarFacets pa pb pc pd pe ph ->
              coplanarTrianglesImproperlyIntersect pa pb pc pd pe ph
          | otherwise ->
              any (improperEdgeHit pa pb pc pd pe ph) [((a, b), g), ((b, c), g), ((c, a), g), ((d, e), f), ((e, h), f), ((h, d), f)]
  where
    pointTable = pointTableFromList pts
    coplanarFacets :: Point3 a -> Point3 a -> Point3 a -> Point3 a -> Point3 a -> Point3 a -> Bool
    coplanarFacets pa pb pc pd pe ph =
      orient3Exact pa pb pc pd == 0 && orient3Exact pa pb pc pe == 0 && orient3Exact pa pb pc ph == 0
    improperEdgeHit pa pb pc pd pe ph ((u, v), (i, j, k)) =
      let selectPoint pointId
            | pointId == a = Just pa
            | pointId == b = Just pb
            | pointId == c = Just pc
            | pointId == d = Just pd
            | pointId == e = Just pe
            | pointId == h = Just ph
            | otherwise = Nothing
       in case (selectPoint u, selectPoint v, selectPoint i, selectPoint j, selectPoint k) of
            (Just pu, Just pv, Just qi, Just qj, Just qk) ->
              case segmentTriangleHit pu pv qi qj qk of
                NoFacetHit -> False
                HitFacetPoint p -> not (pointHitAllowedBySharing pointTable p (u, v) (i, j, k))
                HitFacetOverlap p q -> not (overlapAllowedBySharing pointTable (p, q) (u, v) (i, j, k))
            _ -> False

coplanarTrianglesImproperlyIntersect
  :: forall a. RealFloat a => Point3 a -> Point3 a -> Point3 a -> Point3 a -> Point3 a -> Point3 a -> Bool
coplanarTrianglesImproperlyIntersect a b c d e f =
  let axis = dominantAxisRat3 (crossExact (sub3 b a) (sub3 c a))
      ta = projectRat axis a; tb = projectRat axis b; tc = projectRat axis c
      td = projectRat axis d; te = projectRat axis e; tf = projectRat axis f
      segs1 = [(ta, tb), (tb, tc), (tc, ta)]; segs2 = [(td, te), (te, tf), (tf, td)]
      inters = or [ coplanarSegmentsImproper s t | s <- segs1, t <- segs2 ]
      contain1 = strictInside2 td (ta,tb,tc) || strictInside2 te (ta,tb,tc) || strictInside2 tf (ta,tb,tc)
      contain2 = strictInside2 ta (td,te,tf) || strictInside2 tb (td,te,tf) || strictInside2 tc (td,te,tf)
  in inters || contain1 || contain2

strictInside2 :: (Rational, Rational) -> ((Rational, Rational), (Rational, Rational), (Rational, Rational)) -> Bool
strictInside2 p (a, b, c) =
  let o = orient2Rat a b c; s1 = orient2Rat a b p; s2 = orient2Rat b c p; s3 = orient2Rat c a p
  in if o > 0 then s1 > 0 && s2 > 0 && s3 > 0
     else if o < 0 then s1 < 0 && s2 < 0 && s3 < 0 else False

coplanarSegmentsImproper :: ((Rational, Rational), (Rational, Rational)) -> ((Rational, Rational), (Rational, Rational)) -> Bool
coplanarSegmentsImproper (a, b) (c, d) = case segmentSegment2Rat a b c d of
  Seg2None -> False
  Seg2Point p -> not (p == a || p == b || p == c || p == d)
  Seg2Overlap p q -> not ((p == a || p == b || p == c || p == d) && (q == a || q == b || q == c || q == d))

segmentSegment2Rat :: (Rational, Rational) -> (Rational, Rational) -> (Rational, Rational) -> (Rational, Rational) -> SegmentIntersection2
segmentSegment2Rat p0 p1 q0 q1 =
  let r = sub2Rat p1 p0; s = sub2Rat q1 q0; den = cross2Rat r s; qp = sub2Rat q0 p0
  in if den /= 0
       then let t = cross2Rat qp s / den; u = cross2Rat qp r / den
            in if t < 0 || t > 1 || u < 0 || u > 1 then Seg2None else Seg2Point (add2Rat p0 (scale2Rat t r))
       else if orient2Rat p0 p1 q0 /= 0 then Seg2None else collinear2
  where
    collinear2 =
      let axis :: Int
          axis = if abs (fst p1 - fst p0) >= abs (snd p1 - snd p0) then 0 else 1
          coord0 :: (Rational, Rational) -> Rational
          coord0 q = if axis == 0 then fst q else snd q
          a0 = coord0 p0; a1 = coord0 p1; den = a1 - a0
      in if den == 0 then if p0 == q0 then Seg2Point p0 else Seg2None
         else let tq0 = (coord0 q0 - a0) / den; tq1 = (coord0 q1 - a0) / den
                  lo = max 0 (min tq0 tq1); hi = min 1 (max tq0 tq1)
              in if hi < lo then Seg2None
                 else if hi == lo then Seg2Point (add2Rat p0 (scale2Rat lo (sub2Rat p1 p0)))
                 else Seg2Overlap (add2Rat p0 (scale2Rat lo (sub2Rat p1 p0))) (add2Rat p0 (scale2Rat hi (sub2Rat p1 p0)))

segmentIsFacetBoundary :: SegmentIx -> FacetTriIx -> Bool
segmentIsFacetBoundary e f = canon2 e `elem` map canon2 (facetEdges f)

facetEdges :: FacetTriIx -> [SegmentIx]
facetEdges (a, b, c) = [(a, b), (b, c), (c, a)]

facetDegenerate :: forall a. RealFloat a => Point3 a -> Point3 a -> Point3 a -> Bool
facetDegenerate a b c = let (nx, ny, nz) = crossExact (sub3 b a) (sub3 c a) in nx == 0 && ny == 0 && nz == 0

indexedPairs :: [x] -> [((Int, x), (Int, x))]
indexedPairs xs = [ ((i, xi), (j, xj)) | (i, xi) <- zip [0 :: Int ..] xs, (j, xj) <- zip [0 :: Int ..] xs, i < j ]

sharedVerticesSegSeg :: SegmentIx -> SegmentIx -> [Int]
sharedVerticesSegSeg (a, b) (c, d) = [ v | v <- [a, b], v == c || v == d ]

splitIntersectingSegments3D
  :: forall a. (RealFloat a, Show a) => [Point3 a] -> [SegmentIx] -> Either String ([Point3 a], [SegmentIx])
splitIntersectingSegments3D pts0 segs0 = do
  let !segs = dedupSegments segs0
      pointTable = pointTableFromList pts0
      !baseIndex = M.fromList (zip pts0 [0 :: Int ..])
      pairs = indexedPairs segs
  emptyCuts <-
    foldM
      ( \cutMap segmentValue -> do
          let (u, v) = segmentValue
          startPoint <- lookupPointByIndex "splitIntersectingSegments3D: missing point index " pointTable u
          endPoint <- lookupPointByIndex "splitIntersectingSegments3D: missing point index " pointTable v
          pure (M.insert segmentValue [(0, startPoint), (1, endPoint)] cutMap)
      )
      M.empty
      segs
  cuts <- foldM (accumulatePair pointTable) emptyCuts pairs
  let (!pts1, !index1) = foldl' addCutPoint (pts0, baseIndex) (concat (M.elems cuts))
      splitOne (u, v) = do
        startPoint <- lookupPointByIndex "splitIntersectingSegments3D: missing point index " pointTable u
        endPoint <- lookupPointByIndex "splitIntersectingSegments3D: missing point index " pointTable v
        let cutsUV = fromMaybe [(0, startPoint), (1, endPoint)] (M.lookup (canon2 (u, v)) cuts)
            uniq = uniqueSortedCuts startPoint endPoint cutsUV
        ids <-
          traverse
            ( \(_, pointValue) ->
                case M.lookup pointValue index1 of
                  Nothing -> Left "splitIntersectingSegments3D: missing cut id"
                  Just pointId -> Right pointId
            )
            uniq
        pure [ canon2 (a, b) | (a, b) <- adjacentPairs ids, a /= b ]
  splitSegments <- concat <$> traverse splitOne segs
  pure (pts1, dedupSegments splitSegments)
  where
    addCutPoint :: ([Point3 a], M.Map (Point3 a) Int) -> (Rational, Point3 a) -> ([Point3 a], M.Map (Point3 a) Int)
    addCutPoint (!pts, !ix) (_, p) = case M.lookup p ix of
      Just _ -> (pts, ix)
      Nothing -> let !pid = length pts in (pts ++ [p], M.insert p pid ix)

accumulatePair
  :: forall a. (RealFloat a, Show a)
  => IM.IntMap (Point3 a) -> M.Map SegmentIx [(Rational, Point3 a)]
  -> ((Int, SegmentIx), (Int, SegmentIx)) -> Either String (M.Map SegmentIx [(Rational, Point3 a)])
accumulatePair pointTable cuts ((_, s@(u, v)), (_, t@(x, y)))
  | canon2 s == canon2 t = pure cuts
  | otherwise = do
      pu <- lookupPointByIndex "splitIntersectingSegments3D: missing point index " pointTable u
      pv <- lookupPointByIndex "splitIntersectingSegments3D: missing point index " pointTable v
      px <- lookupPointByIndex "splitIntersectingSegments3D: missing point index " pointTable x
      py <- lookupPointByIndex "splitIntersectingSegments3D: missing point index " pointTable y
      case segmentSegmentIntersection3 pu pv px py of
        SegDisjoint -> pure cuts
        SegPoint p ->
          if not (null shared) && any (\pointId -> maybe False (== p) (IM.lookup pointId pointTable)) shared
            then pure cuts
            else
              pure
                ( M.insertWith (++) (canon2 t) [(projectionParam3 px py p, p)]
                    (M.insertWith (++) (canon2 s) [(projectionParam3 pu pv p, p)] cuts)
                )
        SegOverlap p q -> Left ("splitIntersectingSegments3D: overlapping collinear segments: " ++ show s ++ " and " ++ show t ++ " overlap on " ++ show (p, q))
  where
    shared = sharedVerticesSegSeg s t

uniqueSortedCuts :: forall a. RealFloat a => Point3 a -> Point3 a -> [(Rational, Point3 a)] -> [(Rational, Point3 a)]
uniqueSortedCuts pu pv xs =
  sortBy (comparing fst) [ (projectionParam3 pu pv p, p) | (p, _) <- M.toList (foldl' (\m (t, p) -> M.insert p t m) M.empty (sortBy (comparing fst) xs)) ]

validatePLC3DPoints :: forall a. (RealFloat a, Show a) => [Point3 a] -> [SegmentIx] -> [FacetTriIx] -> Either String ()
validatePLC3DPoints pts rawSegs rawFacets = do
  let !n = length pts
      pointTable = pointTableFromList pts
      !allSegs = dedupSegments (rawSegs ++ concatMap facetEdges rawFacets)
  mapM_ (checkFacet n pointTable) rawFacets
  mapM_ (checkSegPair pointTable) (indexedPairs allSegs)
  mapM_ (checkSegFacet pointTable) [ (s, f) | s <- allSegs, f <- rawFacets, not (segmentIsFacetBoundary s f) ]
  mapM_ (checkFacetPair pts) (indexedPairs rawFacets)
  let edgeInc = foldl' (\m f -> foldl' (\acc e -> M.insertWith (+) (canon2 e) (1 :: Int) acc) m (facetEdges f)) M.empty rawFacets
  case listToMaybe [ (e, k) | (e, k) <- M.toList edgeInc, odd k ] of
    Just (e, k) -> Left ("validatePLC3D: non-manifold/open surface edge " ++ show e ++ " has facet incidence " ++ show k)
    Nothing -> pure ()

checkFacet :: forall a. RealFloat a => Int -> IM.IntMap (Point3 a) -> FacetTriIx -> Either String ()
checkFacet n pointTable (a, b, c)
  | a < 0 || b < 0 || c < 0 || a >= n || b >= n || c >= n = Left ("validatePLC3D: facet index out of bounds: " ++ show (a, b, c))
  | a == b || b == c || c == a = Left ("validatePLC3D: repeated vertex in facet: " ++ show (a, b, c))
  | otherwise = do
      pointA <- lookupPointByIndex "validatePLC3D: missing facet point index " pointTable a
      pointB <- lookupPointByIndex "validatePLC3D: missing facet point index " pointTable b
      pointC <- lookupPointByIndex "validatePLC3D: missing facet point index " pointTable c
      if facetDegenerate pointA pointB pointC
        then Left ("validatePLC3D: degenerate facet: " ++ show (a, b, c))
        else pure ()

checkSegPair :: forall a. (RealFloat a, Show a) => IM.IntMap (Point3 a) -> ((Int, SegmentIx), (Int, SegmentIx)) -> Either String ()
checkSegPair pointTable ((i, s@(u, v)), (j, t@(x, y)))
  | i >= j = pure ()
  | otherwise = do
      pointU <- lookupPointByIndex "validatePLC3D: missing segment point index " pointTable u
      pointV <- lookupPointByIndex "validatePLC3D: missing segment point index " pointTable v
      pointX <- lookupPointByIndex "validatePLC3D: missing segment point index " pointTable x
      pointY <- lookupPointByIndex "validatePLC3D: missing segment point index " pointTable y
      case segmentSegmentIntersection3 pointU pointV pointX pointY of
        SegDisjoint -> pure ()
        SegPoint p | p `elem` mapMaybe (\pointId -> IM.lookup pointId pointTable) (sharedVerticesSegSeg s t) -> pure ()
        SegPoint p -> Left ("validatePLC3D: proper segment intersection between " ++ show s ++ " and " ++ show t ++ " at " ++ show p)
        SegOverlap p q -> Left ("validatePLC3D: overlapping collinear segments " ++ show s ++ " and " ++ show t ++ " overlap on " ++ show (p, q))

checkSegFacet :: forall a. (RealFloat a, Show a) => IM.IntMap (Point3 a) -> (SegmentIx, FacetTriIx) -> Either String ()
checkSegFacet pointTable (s@(u, v), f@(a, b, c)) = do
  pointU <- lookupPointByIndex "validatePLC3D: missing segment point index " pointTable u
  pointV <- lookupPointByIndex "validatePLC3D: missing segment point index " pointTable v
  pointA <- lookupPointByIndex "validatePLC3D: missing facet point index " pointTable a
  pointB <- lookupPointByIndex "validatePLC3D: missing facet point index " pointTable b
  pointC <- lookupPointByIndex "validatePLC3D: missing facet point index " pointTable c
  case segmentTriangleHit pointU pointV pointA pointB pointC of
    NoFacetHit -> pure ()
    HitFacetPoint p | pointHitAllowedBySharing pointTable p s f -> pure ()
    HitFacetPoint p -> Left ("validatePLC3D: segment " ++ show s ++ " intersects facet " ++ show f ++ " at " ++ show p)
    HitFacetOverlap p q | overlapAllowedBySharing pointTable (p, q) s f -> pure ()
    HitFacetOverlap p q -> Left ("validatePLC3D: segment " ++ show s ++ " overlaps facet " ++ show f ++ " on " ++ show (p, q))

checkFacetPair :: forall a. RealFloat a => [Point3 a] -> ((Int, FacetTriIx), (Int, FacetTriIx)) -> Either String ()
checkFacetPair pts ((i, f), (j, g))
  | i >= j = pure ()
  | canon3 f == canon3 g = Left ("validatePLC3D: duplicate facet " ++ show f)
  | trianglesImproperlyIntersect pts f g = Left ("validatePLC3D: improper facet/facet intersection between " ++ show f ++ " and " ++ show g)
  | otherwise = pure ()

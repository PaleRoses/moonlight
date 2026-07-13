{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE BangPatterns #-}

module Moonlight.Geometry.Intersection
  ( polygonsIntersect
  ) where

import Data.Kind (Type)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS

type Pt :: Type
type Pt = (Double, Double)
type BBox :: Type
type BBox = (Double, Double, Double, Double)

type Edge :: Type
data Edge = Edge !Double !Double !Double !Double
type Boxed :: Type -> Type
data Boxed a = Boxed !Int !BBox a
type Step :: Type -> Type
data Step s = Continue !s | Stop !s
type SegmentRelation :: Type
data SegmentRelation = NoRelation | Touching | ProperCrossing deriving stock (Eq)
type PointClass :: Type
data PointClass = Outside | Boundary | Inside deriving stock (Eq)
type EdgePass :: Type
data EdgePass = EdgePass !Bool !Bool
type GridSpec :: Type
data GridSpec = GridSpec
  !Double !Double !Double !Double
  !Double !Double
  !Int !Int

polygonsIntersect :: [(Double, Double)] -> [(Double, Double)] -> Bool
polygonsIntersect rawA rawB =
  let ptsA = normalizePolygon rawA
      ptsB = normalizePolygon rawB
  in case (ptsA, ptsB) of
       ([], _) -> False
       (_, []) -> False
       _ ->
         let bboxA = polygonBBox ptsA
             bboxB = polygonBBox ptsB
             overlap = bboxIntersection bboxA bboxB
         in if not (bboxHasInterior bboxA)
               || not (bboxHasInterior bboxB)
               || not (bboxHasInterior overlap)
              then False
              else
                let edgesA = polygonEdges ptsA
                    edgesB = polygonEdges ptsB
                    EdgePass properCross sawTouch =
                      spatialFoldPairs edgeBBox edgeBBox edgeStep (EdgePass False False) edgesA edgesB
                in properCross
                    || strictlyContainsWitness sawTouch ptsA bboxB edgesB
                    || strictlyContainsWitness sawTouch ptsB bboxA edgesA

edgeStep :: EdgePass -> Edge -> Edge -> Step EdgePass
edgeStep st e1 e2 =
  case segmentRelation e1 e2 of
    ProperCrossing -> Stop (EdgePass True True)
    Touching -> case st of { EdgePass proper _ -> Continue (EdgePass proper True) }
    NoRelation -> Continue st

strictlyContainsWitness :: Bool -> [Pt] -> BBox -> [Edge] -> Bool
strictlyContainsWitness sawTouch subject containerBBox containerEdges = go subject
  where
    go [] = False
    go (p:ps)
      | not (pointInBBoxInclusive p containerBBox) = if sawTouch then go ps else False
      | otherwise = case classifyPointInPolygonEdges p containerEdges of
          Inside -> True
          _ -> if sawTouch then go ps else False

spatialFoldPairs :: (a -> BBox) -> (b -> BBox) -> (s -> a -> b -> Step s) -> s -> [a] -> [b] -> s
spatialFoldPairs bboxA bboxB step s0 as0 bs0 =
  case (indexItems bboxA as0, indexItems bboxB bs0) of
    ([], _) -> s0
    (_, []) -> s0
    (as1, bs1) ->
      let overlap = bboxIntersection (boxedListBBox as1) (boxedListBBox bs1)
      in if bboxIsEmpty overlap then s0
         else let as2 = filter (boxedOverlaps overlap) as1
                  bs2 = filter (boxedOverlaps overlap) bs1
                  nA = length as2; nB = length bs2
              in if nA == 0 || nB == 0 then s0
                 else if nA <= nB
                   then runBucketed overlap nA as2 bs2 step s0
                   else runBucketed overlap nB bs2 as2 (\s b a -> step s a b) s0

runBucketed :: BBox -> Int -> [Boxed a] -> [Boxed b] -> (s -> a -> b -> Step s) -> s -> s
runBucketed overlap baseCount baseItems queryItems step s0 =
  let spec = makeGridSpec overlap baseCount
      grid = buildGrid spec baseItems
      goQueries s [] = s
      goQueries s (q:qs) = case queryAgainst spec grid step s q of
        Continue s' -> goQueries s' qs
        Stop s' -> s'
  in goQueries s0 queryItems

queryAgainst :: GridSpec -> IM.IntMap [Boxed a] -> (s -> a -> b -> Step s) -> s -> Boxed b -> Step s
queryAgainst spec grid step s0 (Boxed _ qbb q) =
  case cellsForBBox spec qbb of
    [] -> Continue s0
    [cell] -> case IM.lookup cell grid of { Nothing -> Continue s0; Just xs -> goBucketNoDedup s0 xs }
    cells -> goCells s0 IS.empty cells
  where
    goCells s _ [] = Continue s
    goCells s seen (cell:rest) = case IM.lookup cell grid of
      Nothing -> goCells s seen rest
      Just xs -> goBucketDedup s seen xs rest

    goBucketNoDedup s [] = Continue s
    goBucketNoDedup s (Boxed _ bb a : xs)
      | not (bboxOverlaps bb qbb) = goBucketNoDedup s xs
      | otherwise = case step s a q of { Continue s' -> goBucketNoDedup s' xs; Stop s' -> Stop s' }

    goBucketDedup s seen [] rest = goCells s seen rest
    goBucketDedup s seen (Boxed bid bb a : xs) rest
      | IS.member bid seen = goBucketDedup s seen xs rest
      | not (bboxOverlaps bb qbb) = goBucketDedup s (IS.insert bid seen) xs rest
      | otherwise = case step s a q of
          Continue s' -> goBucketDedup s' (IS.insert bid seen) xs rest
          Stop s' -> Stop s'

indexItems :: (a -> BBox) -> [a] -> [Boxed a]
indexItems bboxOf = zipWith (\i x -> Boxed i (bboxOf x) x) [0 ..]

boxedOverlaps :: BBox -> Boxed a -> Bool
boxedOverlaps overlap (Boxed _ bb _) = bboxOverlaps bb overlap

boxedListBBox :: [Boxed a] -> BBox
boxedListBBox [] = (0, 0, 0, 0)
boxedListBBox (Boxed _ bb _ : rest) = foldl' (\acc (Boxed _ bb' _) -> bboxUnion acc bb') bb rest

buildGrid :: GridSpec -> [Boxed a] -> IM.IntMap [Boxed a]
buildGrid spec = foldl' (\m boxed@(Boxed _ bb _) -> foldl' (\acc cell -> IM.insertWith (++) cell [boxed] acc) m (cellsForBBox spec bb)) IM.empty

makeGridSpec :: BBox -> Int -> GridSpec
makeGridSpec (minX, minY, maxX, maxY) itemCount =
  let w = maxX - minX; h = maxY - minY; target = max 1 itemCount
      (cols, rows)
        | w <= 0 && h <= 0 = (1, 1) | h <= 0 = (target, 1) | w <= 0 = (1, target)
        | otherwise = let rawCols = sqrt (fromIntegral target * w / h)
                          cols0 | isNaN rawCols || isInfinite rawCols = target | otherwise = round rawCols
                          cols' = max 1 (min target cols0)
                          rows' = max 1 (min target (ceiling (fromIntegral target / (fromIntegral cols' :: Double))))
                      in (cols', rows')
      cellW = if cols <= 1 || w <= 0 then 1 else w / fromIntegral cols
      cellH = if rows <= 1 || h <= 0 then 1 else h / fromIntegral rows
  in GridSpec minX minY maxX maxY cellW cellH cols rows

cellsForBBox :: GridSpec -> BBox -> [Int]
cellsForBBox (GridSpec gx0 gy0 gx1 gy1 cellW cellH cols rows) (sx0, sy0, sx1, sy1) =
  let tx0 = max gx0 sx0; ty0 = max gy0 sy0; tx1 = min gx1 sx1; ty1 = min gy1 sy1
  in if tx1 < tx0 || ty1 < ty0 then []
     else [iy * cols + ix | iy <- [toCellY ty0 .. toCellY ty1], ix <- [toCellX tx0 .. toCellX tx1]]
  where
    toCellX x | cols <= 1 || gx1 <= gx0 = 0 | otherwise = clamp 0 (cols - 1) (floor ((x - gx0) / cellW))
    toCellY y | rows <= 1 || gy1 <= gy0 = 0 | otherwise = clamp 0 (rows - 1) (floor ((y - gy0) / cellH))

normalizePolygon :: [Pt] -> [Pt]
normalizePolygon pts0 =
  let pts1 = stripClosing pts0; pts2 = dedupConsecutive pts1; pts3 = stripClosing pts2
  in if hasAtLeast3 pts3 then pts3 else []

stripClosing :: [Pt] -> [Pt]
stripClosing [] = []; stripClosing [x] = [x]
stripClosing (firstPoint : restPoints) =
  case reverse restPoints of
    [] -> [firstPoint]
    lastPoint : middlePointsReversed ->
      if firstPoint == lastPoint
        then firstPoint : reverse middlePointsReversed
        else firstPoint : restPoints

dedupConsecutive :: [Pt] -> [Pt]
dedupConsecutive [] = []
dedupConsecutive (p:ps) = p : go p ps
  where
    go :: Pt -> [Pt] -> [Pt]
    go _ [] = []
    go prev (q:qs)
      | q == prev = go prev qs
      | otherwise = q : go q qs

hasAtLeast3 :: [a] -> Bool
hasAtLeast3 (_:_:_:_) = True; hasAtLeast3 _ = False

polygonEdges :: [Pt] -> [Edge]
polygonEdges [] = []; polygonEdges [_] = []
polygonEdges (firstPoint : restPoints) = go firstPoint firstPoint restPoints
  where
    mkEdge (ax, ay) (bx, by)
      | ax == bx && ay == by = []
      | otherwise = [Edge ax ay bx by]
    go startPoint previousPoint [] = mkEdge previousPoint startPoint
    go startPoint previousPoint (nextPoint : remainingPoints) =
      mkEdge previousPoint nextPoint ++ go startPoint nextPoint remainingPoints

polygonBBox :: [Pt] -> BBox
polygonBBox [] = (0, 0, 0, 0)
polygonBBox ((x0, y0):ps) = foldl' (\(mnx,mny,mxx,mxy) (x,y) -> (min mnx x, min mny y, max mxx x, max mxy y)) (x0, y0, x0, y0) ps

edgeBBox :: Edge -> BBox
edgeBBox (Edge ax ay bx by) = (min ax bx, min ay by, max ax bx, max ay by)

bboxUnion :: BBox -> BBox -> BBox
bboxUnion (ax0,ay0,ax1,ay1) (bx0,by0,bx1,by1) = (min ax0 bx0, min ay0 by0, max ax1 bx1, max ay1 by1)

bboxIntersection :: BBox -> BBox -> BBox
bboxIntersection (ax0,ay0,ax1,ay1) (bx0,by0,bx1,by1) = (max ax0 bx0, max ay0 by0, min ax1 bx1, min ay1 by1)

bboxOverlaps :: BBox -> BBox -> Bool
bboxOverlaps (ax0,ay0,ax1,ay1) (bx0,by0,bx1,by1) = not (ax1 < bx0 || bx1 < ax0 || ay1 < by0 || by1 < ay0)

bboxIsEmpty :: BBox -> Bool
bboxIsEmpty (x0, y0, x1, y1) = x1 < x0 || y1 < y0

bboxHasInterior :: BBox -> Bool
bboxHasInterior (x0, y0, x1, y1) = x0 < x1 && y0 < y1

segmentRelation :: Edge -> Edge -> SegmentRelation
segmentRelation (Edge ax ay bx by) (Edge cx cy dx dy) =
  let o1 = orient2dSign ax ay bx by cx cy; o2 = orient2dSign ax ay bx by dx dy
      o3 = orient2dSign cx cy dx dy ax ay; o4 = orient2dSign cx cy dx dy bx by
  in if o1 /= 0 && o2 /= 0 && o3 /= 0 && o4 /= 0 && o1 /= o2 && o3 /= o4 then ProperCrossing
     else if (o1 == 0 && onSegmentCollinear cx cy ax ay bx by) || (o2 == 0 && onSegmentCollinear dx dy ax ay bx by)
               || (o3 == 0 && onSegmentCollinear ax ay cx cy dx dy) || (o4 == 0 && onSegmentCollinear bx by cx cy dx dy)
            then Touching else NoRelation

classifyPointInPolygonEdges :: Pt -> [Edge] -> PointClass
classifyPointInPolygonEdges (px, py) = go False
  where
    go inside [] = if inside then Inside else Outside
    go inside (Edge ax ay bx by : es) =
      let o = orient2dSign ax ay bx by px py
      in if o == 0 && betweenInclusive px ax bx && betweenInclusive py ay by then Boundary
         else go (if (ay > py) /= (by > py) && ((by > ay && o > 0) || (by < ay && o < 0)) then not inside else inside) es

pointInBBoxInclusive :: Pt -> BBox -> Bool
pointInBBoxInclusive (x, y) (minX, minY, maxX, maxY) = minX <= x && x <= maxX && minY <= y && y <= maxY

onSegmentCollinear :: Double -> Double -> Double -> Double -> Double -> Double -> Bool
onSegmentCollinear px py ax ay bx by = betweenInclusive px ax bx && betweenInclusive py ay by

betweenInclusive :: Double -> Double -> Double -> Bool
betweenInclusive x a b = min a b <= x && x <= max a b

orient2dSign :: Double -> Double -> Double -> Double -> Double -> Double -> Int
orient2dSign ax ay bx by cx cy =
  let acx = ax - cx; bcx = bx - cx; acy = ay - cy; bcy = by - cy
      detLeft = acx * bcy; detRight = acy * bcx; det = detLeft - detRight
      detSum = abs detLeft + abs detRight
      errBound = (3.0 + 16.0 * 2.2204460492503131e-16) * 2.2204460492503131e-16 * detSum
  in if det > errBound then 1 else if det < negate errBound then -1
     else case compare ((toRational ax - toRational cx) * (toRational by - toRational cy) - (toRational ay - toRational cy) * (toRational bx - toRational cx)) 0 of
            GT -> 1; LT -> -1; EQ -> 0

clamp :: Int -> Int -> Int -> Int
clamp lo hi x | x < lo = lo | x > hi = hi | otherwise = x

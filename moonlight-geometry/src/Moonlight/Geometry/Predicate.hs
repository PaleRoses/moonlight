{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Moonlight.Geometry.Predicate
  ( Point2
  , Point3
  , BBox
  , BBox3
  , epsilon
  , det3
  , det4
  , orientApprox
  , orientExact
  , orientSign
  , orient3Approx
  , orient3Exact
  , orient3Sign
  , incircleApprox
  , incircleExact
  , incircleSign
  , insphereApprox
  , insphereExact
  , insphereSign
  , crossExact
  , dotExact
  , cross3
  , dot3
  , sub3
  , add3
  , scale3
  , midpoint3
  , fst3
  , snd3
  , trd3
  , orient2Rat
  , sub2Rat
  , cross2Rat
  , add2Rat
  , scale2Rat
  , lerp3Rat
  , pointOnClosedSegment
  , properSegmentIntersection
  , segmentsIntersectClosed
  , collinearOverlapMoreThanEndpoint
  , projectionParam
  , pointOnClosedSegment3
  , projectionParam3
  , bboxOfPoints3
  , unionBBox3
  ) where

import Data.Kind (Type)

type Point2 :: Type -> Type
type Point2 a = (a, a)
type Point3 :: Type -> Type
type Point3 a = (a, a, a)
type BBox :: Type -> Type
type BBox a = (a, a, a, a)
type BBox3 :: Type -> Type
type BBox3 a = (a, a, a, a, a, a)

epsilon :: forall a. RealFloat a => a
epsilon = encodeFloat 1 (1 - floatDigits (0 :: a))

det3 :: Num r => r -> r -> r -> r -> r -> r -> r -> r -> r -> r
det3 a11 a12 a13 a21 a22 a23 a31 a32 a33 =
  a11 * (a22 * a33 - a23 * a32)
  - a12 * (a21 * a33 - a23 * a31)
  + a13 * (a21 * a32 - a22 * a31)

det4
  :: Num r
  => r -> r -> r -> r
  -> r -> r -> r -> r
  -> r -> r -> r -> r
  -> r -> r -> r -> r
  -> r
det4 a11 a12 a13 a14
     a21 a22 a23 a24
     a31 a32 a33 a34
     a41 a42 a43 a44 =
  a11 * det3 a22 a23 a24 a32 a33 a34 a42 a43 a44
  - a12 * det3 a21 a23 a24 a31 a33 a34 a41 a43 a44
  + a13 * det3 a21 a22 a24 a31 a32 a34 a41 a42 a44
  - a14 * det3 a21 a22 a23 a31 a32 a33 a41 a42 a43

orientApprox :: RealFloat a => Point2 a -> Point2 a -> Point2 a -> a
orientApprox (ax, ay) (bx, by) (cx, cy) =
  (bx - ax) * (cy - ay) - (by - ay) * (cx - ax)

orientExact :: Real a => Point2 a -> Point2 a -> Point2 a -> Rational
orientExact (ax, ay) (bx, by) (cx, cy) =
  let ax' = toRational ax; ay' = toRational ay
      bx' = toRational bx; by' = toRational by
      cx' = toRational cx; cy' = toRational cy
  in (bx' - ax') * (cy' - ay') - (by' - ay') * (cx' - ax')

orientSign :: RealFloat a => Point2 a -> Point2 a -> Point2 a -> Ordering
orientSign a@(ax, ay) b@(bx, by) c@(cx, cy) =
  let !det = orientApprox a b c
      !permanent = (abs (bx - ax) + abs (cx - ax)) * (abs (by - ay) + abs (cy - ay))
      !err = 16.0 * epsilon * permanent
  in if abs det > err then compare det 0 else compare (orientExact a b c) 0

orient3Approx :: RealFloat a => Point3 a -> Point3 a -> Point3 a -> Point3 a -> a
orient3Approx (ax, ay, az) (bx, by, bz) (cx, cy, cz) (dx, dy, dz) =
  det3 (bx - ax) (by - ay) (bz - az)
       (cx - ax) (cy - ay) (cz - az)
       (dx - ax) (dy - ay) (dz - az)

orient3Exact :: Real a => Point3 a -> Point3 a -> Point3 a -> Point3 a -> Rational
orient3Exact (ax, ay, az) (bx, by, bz) (cx, cy, cz) (dx, dy, dz) =
  let ax' = toRational ax; ay' = toRational ay; az' = toRational az
      bx' = toRational bx; by' = toRational by; bz' = toRational bz
      cx' = toRational cx; cy' = toRational cy; cz' = toRational cz
      dx' = toRational dx; dy' = toRational dy; dz' = toRational dz
  in det3 (bx' - ax') (by' - ay') (bz' - az')
          (cx' - ax') (cy' - ay') (cz' - az')
          (dx' - ax') (dy' - ay') (dz' - az')

orient3Sign :: RealFloat a => Point3 a -> Point3 a -> Point3 a -> Point3 a -> Ordering
orient3Sign a@(ax, ay, az) b@(bx, by, bz) c@(cx, cy, cz) d@(dx, dy, dz) =
  let !det = orient3Approx a b c d
      !mx = abs (bx - ax) + abs (cx - ax) + abs (dx - ax)
      !my = abs (by - ay) + abs (cy - ay) + abs (dy - ay)
      !mz = abs (bz - az) + abs (cz - az) + abs (dz - az)
      !permanent = mx * my * mz
      !err = 64.0 * epsilon * permanent
  in if abs det > err then compare det 0 else compare (orient3Exact a b c d) 0

incircleApprox :: RealFloat a => Point2 a -> Point2 a -> Point2 a -> Point2 a -> a
incircleApprox (ax, ay) (bx, by) (cx, cy) (dx, dy) =
  let !adx = ax - dx; !ady = ay - dy
      !bdx = bx - dx; !bdy = by - dy
      !cdx = cx - dx; !cdy = cy - dy
      !abdet = adx * bdy - bdx * ady
      !bcdet = bdx * cdy - cdx * bdy
      !cadet = cdx * ady - adx * cdy
      !alift = adx * adx + ady * ady
      !blift = bdx * bdx + bdy * bdy
      !clift = cdx * cdx + cdy * cdy
  in alift * bcdet + blift * cadet + clift * abdet

incircleExact :: Real a => Point2 a -> Point2 a -> Point2 a -> Point2 a -> Rational
incircleExact (ax, ay) (bx, by) (cx, cy) (dx, dy) =
  let ax' = toRational ax; ay' = toRational ay
      bx' = toRational bx; by' = toRational by
      cx' = toRational cx; cy' = toRational cy
      dx' = toRational dx; dy' = toRational dy
      adx = ax' - dx'; ady = ay' - dy'
      bdx = bx' - dx'; bdy = by' - dy'
      cdx = cx' - dx'; cdy = cy' - dy'
      abdet = adx * bdy - bdx * ady
      bcdet = bdx * cdy - cdx * bdy
      cadet = cdx * ady - adx * cdy
      alift = adx * adx + ady * ady
      blift = bdx * bdx + bdy * bdy
      clift = cdx * cdx + cdy * cdy
  in alift * bcdet + blift * cadet + clift * abdet

incircleSign :: RealFloat a => Point2 a -> Point2 a -> Point2 a -> Point2 a -> Ordering
incircleSign a b c d =
  let !det = incircleApprox a b c d
      !adx = fst a - fst d; !ady = snd a - snd d
      !bdx = fst b - fst d; !bdy = snd b - snd d
      !cdx = fst c - fst d; !cdy = snd c - snd d
      !abdet = adx * bdy - bdx * ady
      !bcdet = bdx * cdy - cdx * bdy
      !cadet = cdx * ady - adx * cdy
      !alift = adx * adx + ady * ady
      !blift = bdx * bdx + bdy * bdy
      !clift = cdx * cdx + cdy * cdy
      !permanent = abs bcdet * alift + abs cadet * blift + abs abdet * clift
      !err = 128.0 * epsilon * permanent
  in if abs det > err then compare det 0 else compare (incircleExact a b c d) 0

insphereApprox :: RealFloat a => Point3 a -> Point3 a -> Point3 a -> Point3 a -> Point3 a -> a
insphereApprox (ax, ay, az) (bx, by, bz) (cx, cy, cz) (dx, dy, dz) (ex, ey, ez) =
  let ax' = ax - ex; ay' = ay - ey; az' = az - ez
      bx' = bx - ex; by' = by - ey; bz' = bz - ez
      cx' = cx - ex; cy' = cy - ey; cz' = cz - ez
      dx' = dx - ex; dy' = dy - ey; dz' = dz - ez
      al = ax' * ax' + ay' * ay' + az' * az'
      bl = bx' * bx' + by' * by' + bz' * bz'
      cl = cx' * cx' + cy' * cy' + cz' * cz'
      dl = dx' * dx' + dy' * dy' + dz' * dz'
  in det4 ax' ay' az' al bx' by' bz' bl cx' cy' cz' cl dx' dy' dz' dl

insphereExact :: Real a => Point3 a -> Point3 a -> Point3 a -> Point3 a -> Point3 a -> Rational
insphereExact (ax, ay, az) (bx, by, bz) (cx, cy, cz) (dx, dy, dz) (ex, ey, ez) =
  let ax' = toRational ax - toRational ex
      ay' = toRational ay - toRational ey
      az' = toRational az - toRational ez
      bx' = toRational bx - toRational ex
      by' = toRational by - toRational ey
      bz' = toRational bz - toRational ez
      cx' = toRational cx - toRational ex
      cy' = toRational cy - toRational ey
      cz' = toRational cz - toRational ez
      dx' = toRational dx - toRational ex
      dy' = toRational dy - toRational ey
      dz' = toRational dz - toRational ez
      al = ax' * ax' + ay' * ay' + az' * az'
      bl = bx' * bx' + by' * by' + bz' * bz'
      cl = cx' * cx' + cy' * cy' + cz' * cz'
      dl = dx' * dx' + dy' * dy' + dz' * dz'
  in det4 ax' ay' az' al bx' by' bz' bl cx' cy' cz' cl dx' dy' dz' dl

insphereSign :: RealFloat a => Point3 a -> Point3 a -> Point3 a -> Point3 a -> Point3 a -> Ordering
insphereSign a b c d e =
  let !det = insphereApprox a b c d e
      !ax = fst3 a - fst3 e; !ay = snd3 a - snd3 e; !az = trd3 a - trd3 e
      !bx = fst3 b - fst3 e; !by = snd3 b - snd3 e; !bz = trd3 b - trd3 e
      !cx = fst3 c - fst3 e; !cy = snd3 c - snd3 e; !cz = trd3 c - trd3 e
      !dx = fst3 d - fst3 e; !dy = snd3 d - snd3 e; !dz = trd3 d - trd3 e
      !al = ax * ax + ay * ay + az * az
      !bl = bx * bx + by * by + bz * bz
      !cl = cx * cx + cy * cy + cz * cz
      !dl = dx * dx + dy * dy + dz * dz
      !permanent =
           abs ax * abs (det3 by bz bl cy cz cl dy dz dl)
         + abs ay * abs (det3 bx bz bl cx cz cl dx dz dl)
         + abs az * abs (det3 bx by bl cx cy cl dx dy dl)
         + abs al * abs (det3 bx by bz cx cy cz dx dy dz)
      !err = 256.0 * epsilon * permanent
  in if abs det > err then compare det 0 else compare (insphereExact a b c d e) 0

crossExact :: Real a => Point3 a -> Point3 a -> (Rational, Rational, Rational)
crossExact (ax, ay, az) (bx, by, bz) =
  let ax' = toRational ax; ay' = toRational ay; az' = toRational az
      bx' = toRational bx; by' = toRational by; bz' = toRational bz
  in ( ay' * bz' - az' * by'
     , az' * bx' - ax' * bz'
     , ax' * by' - ay' * bx' )

dotExact :: Real a => Point3 a -> Point3 a -> Rational
dotExact (ax, ay, az) (bx, by, bz) =
  toRational ax * toRational bx + toRational ay * toRational by + toRational az * toRational bz

cross3 :: Num a => Point3 a -> Point3 a -> Point3 a
cross3 (ax, ay, az) (bx, by, bz) =
  (ay * bz - az * by, az * bx - ax * bz, ax * by - ay * bx)

dot3 :: Num a => Point3 a -> Point3 a -> a
dot3 (ax, ay, az) (bx, by, bz) = ax * bx + ay * by + az * bz

sub3 :: Num a => Point3 a -> Point3 a -> Point3 a
sub3 (ax, ay, az) (bx, by, bz) = (ax - bx, ay - by, az - bz)

add3 :: Num a => Point3 a -> Point3 a -> Point3 a
add3 (ax, ay, az) (bx, by, bz) = (ax + bx, ay + by, az + bz)

scale3 :: Fractional a => a -> Point3 a -> Point3 a
scale3 s (x, y, z) = (s * x, s * y, s * z)

midpoint3 :: Fractional a => Point3 a -> Point3 a -> Point3 a
midpoint3 a b = scale3 0.5 (add3 a b)

fst3 :: Point3 a -> a
fst3 (x, _, _) = x

snd3 :: Point3 a -> a
snd3 (_, y, _) = y

trd3 :: Point3 a -> a
trd3 (_, _, z) = z

orient2Rat :: (Rational, Rational) -> (Rational, Rational) -> (Rational, Rational) -> Rational
orient2Rat (ax, ay) (bx, by) (cx, cy) =
  (bx - ax) * (cy - ay) - (by - ay) * (cx - ax)

sub2Rat :: (Rational, Rational) -> (Rational, Rational) -> (Rational, Rational)
sub2Rat (ax, ay) (bx, by) = (ax - bx, ay - by)

cross2Rat :: (Rational, Rational) -> (Rational, Rational) -> Rational
cross2Rat (ax, ay) (bx, by) = ax * by - ay * bx

add2Rat :: (Rational, Rational) -> (Rational, Rational) -> (Rational, Rational)
add2Rat (ax, ay) (bx, by) = (ax + bx, ay + by)

scale2Rat :: Rational -> (Rational, Rational) -> (Rational, Rational)
scale2Rat s (x, y) = (s * x, s * y)

lerp3Rat :: forall a. RealFloat a => Rational -> Point3 a -> Point3 a -> Point3 a
lerp3Rat t (ax, ay, az) (bx, by, bz) =
  let s = 1 - t
  in ( fromRational (s * toRational ax + t * toRational bx)
     , fromRational (s * toRational ay + t * toRational by)
     , fromRational (s * toRational az + t * toRational bz) )

pointOnClosedSegment :: RealFloat a => Point2 a -> Point2 a -> Point2 a -> Bool
pointOnClosedSegment a@(ax, ay) b@(bx, by) p@(px, py) =
  orientSign a b p == EQ && px >= min ax bx && px <= max ax bx && py >= min ay by && py <= max ay by

properSegmentIntersection :: RealFloat a => Point2 a -> Point2 a -> Point2 a -> Point2 a -> Bool
properSegmentIntersection a b c d =
  let o1 = orientSign a b c; o2 = orientSign a b d
      o3 = orientSign c d a; o4 = orientSign c d b
  in o1 /= EQ && o2 /= EQ && o3 /= EQ && o4 /= EQ && o1 /= o2 && o3 /= o4

segmentsIntersectClosed :: RealFloat a => Point2 a -> Point2 a -> Point2 a -> Point2 a -> Bool
segmentsIntersectClosed a b c d =
  properSegmentIntersection a b c d
    || pointOnClosedSegment a b c || pointOnClosedSegment a b d
    || pointOnClosedSegment c d a || pointOnClosedSegment c d b

collinearOverlapMoreThanEndpoint :: RealFloat a => Point2 a -> Point2 a -> Point2 a -> Point2 a -> Bool
collinearOverlapMoreThanEndpoint a b c d =
  orientSign a b c == EQ && orientSign a b d == EQ
    && overlap1D (fst a) (fst b) (fst c) (fst d) && overlap1D (snd a) (snd b) (snd c) (snd d)
    && not (onlyTouchAtOneEndpoint a b c d)
  where
    overlap1D :: Ord a => a -> a -> a -> a -> Bool
    overlap1D p q r s = max (min p q) (min r s) < min (max p q) (max r s)
    onlyTouchAtOneEndpoint :: Eq a => a -> a -> a -> a -> Bool
    onlyTouchAtOneEndpoint p q r s =
      length [ () | x <- [p, q], y <- [r, s], x == y ] == 1

projectionParam :: RealFloat a => Point2 a -> Point2 a -> Point2 a -> a
projectionParam (ax, ay) (bx, by) (px, py) =
  let dx = bx - ax; dy = by - ay; den = dx * dx + dy * dy
  in if den == 0 then 0 else ((px - ax) * dx + (py - ay) * dy) / den

pointOnClosedSegment3 :: RealFloat a => Point3 a -> Point3 a -> Point3 a -> Bool
pointOnClosedSegment3 a b p =
  let ab = sub3 b a
      ap = sub3 p a
      (cx, cy, cz) = crossExact ab ap
      !collinear = cx == 0 && cy == 0 && cz == 0
      !t = dotExact ap ab
      !len2 = dotExact ab ab
  in collinear && t >= 0 && t <= len2

projectionParam3 :: Real a => Point3 a -> Point3 a -> Point3 a -> Rational
projectionParam3 a b p =
  let ab = sub3 b a
      ap = sub3 p a
      den = dotExact ab ab
  in if den == 0 then 0 else dotExact ap ab / den

bboxOfPoints3 :: RealFloat a => [Point3 a] -> BBox3 a
bboxOfPoints3 [] = (0, 0, 0, 1, 1, 1)
bboxOfPoints3 ((x0, y0, z0):ps) =
  foldl' step (x0, y0, z0, x0, y0, z0) ps
  where
    step :: Ord a => (a, a, a, a, a, a) -> (a, a, a) -> (a, a, a, a, a, a)
    step (!xmin, !ymin, !zmin, !xmax, !ymax, !zmax) (x, y, z) =
      (min xmin x, min ymin y, min zmin z, max xmax x, max ymax y, max zmax z)

unionBBox3 :: Ord a => BBox3 a -> BBox3 a -> BBox3 a
unionBBox3 (ax0, ay0, az0, ax1, ay1, az1) (bx0, by0, bz0, bx1, by1, bz1) =
  (min ax0 bx0, min ay0 by0, min az0 bz0, max ax1 bx1, max ay1 by1, max az1 bz1)

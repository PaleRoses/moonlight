{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Moonlight.Geometry.SpaceFilling
  ( splitmix64
  , hilbertOf
  , xy2d
  , quantize
  , quantizeX
  , quantizeY
  , mortonOf
  , interleave3
  , quantize64
  , qx, qy, qz
  ) where

import Data.Bits
import Data.Word (Word64)

import Moonlight.Geometry.Predicate (Point2, Point3, BBox, BBox3)

splitmix64 :: Word64 -> Word64
splitmix64 z0 =
  let z1 = z0 + 0x9e3779b97f4a7c15
      z2 = (z1 `xor` (z1 `shiftR` 30)) * 0xbf58476d1ce4e5b9
      z3 = (z2 `xor` (z2 `shiftR` 27)) * 0x94d049bb133111eb
  in z3 `xor` (z3 `shiftR` 31)

hilbertOf :: RealFloat a => BBox a -> Point2 a -> Word64
hilbertOf bbox p = xy2d 65536 (quantizeX bbox p) (quantizeY bbox p)

quantizeX :: RealFloat a => BBox a -> Point2 a -> Int
quantizeX (xmin, _, xmax, _) (x, _) = quantize xmin xmax x

quantizeY :: RealFloat a => BBox a -> Point2 a -> Int
quantizeY (_, ymin, _, ymax) (_, y) = quantize ymin ymax y

quantize :: RealFloat a => a -> a -> a -> Int
quantize lo hi x
  | hi <= lo = 0
  | otherwise = let t = (x - lo) / (hi - lo); q = floor (t * 65535.0 + 0.5) in max 0 (min 65535 q)

xy2d :: Int -> Int -> Int -> Word64
xy2d n x0 y0 = go (n `div` 2) x0 y0 0
  where
    n1 = n - 1
    go 0 !_ !_ !d = d
    go !s !x !y !d =
      let rx = if (x .&. s) /= 0 then 1 else 0
          ry = if (y .&. s) /= 0 then 1 else 0
          d' = d + fromIntegral (s * s * ((3 * rx) `xor` ry))
          (x', y') = rot n1 x y rx ry
      in go (s `div` 2) x' y' d'
    rot :: Int -> Int -> Int -> Int -> Int -> (Int, Int)
    rot !m !x !y !rx !ry
      | ry == 0 = let (x1, y1) = if rx == 1 then (m - x, m - y) else (x, y) in (y1, x1)
      | otherwise = (x, y)

mortonOf :: RealFloat a => BBox3 a -> Point3 a -> Word64
mortonOf bbox (x, y, z) = interleave3 (qx bbox x) (qy bbox y) (qz bbox z)

qx, qy, qz :: RealFloat a => BBox3 a -> a -> Word64
qx (xmin, _, _, xmax, _, _) x = fromIntegral (quantize64 xmin xmax x)
qy (_, ymin, _, _, ymax, _) y = fromIntegral (quantize64 ymin ymax y)
qz (_, _, zmin, _, _, zmax) z = fromIntegral (quantize64 zmin zmax z)

quantize64 :: RealFloat a => a -> a -> a -> Int
quantize64 lo hi x
  | hi <= lo = 0
  | otherwise = let !t = (x - lo) / (hi - lo); !q = floor (t * 2097151.0 + 0.5) in max 0 (min 2097151 q)

interleave3 :: Word64 -> Word64 -> Word64 -> Word64
interleave3 x y z = foldl' step 0 [0 :: Int .. 20]
  where
    step !acc !i =
      let bx = (x `shiftR` i) .&. 1
          by = (y `shiftR` i) .&. 1
          bz = (z `shiftR` i) .&. 1
      in acc .|. (bx `shiftL` (3 * i)) .|. (by `shiftL` (3 * i + 1)) .|. (bz `shiftL` (3 * i + 2))

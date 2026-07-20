{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Moonlight.Geometry.Triangulation
  ( Point2
  , HasXY(..)
  , EdgeIx
  , TriIx
  , BBox
  , CDTOptions(..)
  , defaultCDTOptions
  , triangulateCDT
  , triangulateCDTWith
  , triangulateCDTPartitioned
  , triangulateCDTPartitionedWith
  , validateInputPSLG
  ) where

import Control.Monad (foldM, unless, when)
import Data.Array (Array, bounds, listArray, (!))
import Data.Bits
import Data.Kind (Constraint, Type)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M
import Data.List (minimumBy, sortBy)
import Data.Maybe (fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Ord (comparing)
import Data.Word (Word64)
import Moonlight.Core (adjacentPairs)

--------------------------------------------------------------------------------
-- Public API
--------------------------------------------------------------------------------

type Point2 :: Type -> Type
type Point2 a = (a, a)
type EdgeIx :: Type
type EdgeIx = (Int, Int)
type TriIx :: Type
type TriIx = (Int, Int, Int)
type BBox :: Type -> Type
type BBox a = (a, a, a, a)

type HasXY :: Type -> Type -> Constraint
class HasXY p a | p -> a where
  pointXY :: p -> Point2 a

instance HasXY (a, a) a where
  pointXY = id

type CDTOptions :: Type
data CDTOptions = CDTOptions
  { cdtUseBRIO      :: !Bool
  , cdtFinalRepair  :: !Bool
  , cdtValidate     :: !Bool
  } deriving stock (Eq, Show)

defaultCDTOptions :: CDTOptions
defaultCDTOptions = CDTOptions
  { cdtUseBRIO = True
  , cdtFinalRepair = True
  , cdtValidate = False
  }

triangulateCDT
  :: forall p a. (HasXY p a, RealFloat a, Show a)
  => [p]
  -> [EdgeIx]
  -> BBox a
  -> Either String [TriIx]
triangulateCDT = triangulateCDTWith defaultCDTOptions

triangulateCDTWith
  :: forall p a. (HasXY p a, RealFloat a, Show a)
  => CDTOptions
  -> [p]
  -> [EdgeIx]
  -> BBox a
  -> Either String [TriIx]
triangulateCDTWith opts ptsIn rawEdges bbox =
  triangulateCDTPartitionedWith opts ptsIn rawEdges rawEdges bbox

triangulateCDTPartitioned
  :: forall p a. (HasXY p a, RealFloat a, Show a)
  => [p] -> [EdgeIx] -> [EdgeIx] -> BBox a -> Either String [TriIx]
triangulateCDTPartitioned = triangulateCDTPartitionedWith defaultCDTOptions

triangulateCDTPartitionedWith
  :: forall p a. (HasXY p a, RealFloat a, Show a)
  => CDTOptions -> [p] -> [EdgeIx] -> [EdgeIx] -> BBox a -> Either String [TriIx]
triangulateCDTPartitionedWith opts ptsIn rawEdges rawBoundaryEdges bbox = do
  validateInputPSLG ptsIn rawEdges
  validateInputPSLG ptsIn rawBoundaryEdges
  let !pts0 = map pointXY ptsIn
      !n0 = length pts0
      !ptsArr0 = listArray (0, n0 - 1) pts0
      !basePack = n0 + 4
      !rawEdgeSet = IS.fromList [ packUndirectedBase basePack (normalizeEdge e) | e <- rawEdges ]
      !badBoundary = listToMaybe
        [ e | e <- rawBoundaryEdges, packUndirectedBase basePack (normalizeEdge e) `IS.notMember` rawEdgeSet ]
  case badBoundary of
    Just e -> Left ("triangulateCDT: boundary-classifying constraint is not present in the full constraint set: " ++ show e)
    Nothing -> pure ()
  (_expandedByRaw, edges) <- preprocessConstraints ptsArr0 rawEdges
  let endpointSet = foldl' (\acc (u, v) -> IS.insert u (IS.insert v acc)) IS.empty edges
      allIds = [0 .. n0 - 1]
      endpointIds = IS.toList endpointSet
      interiorIds = filter (`IS.notMember` endpointSet) allIds
      endpointOrder = orderedIds (cdtUseBRIO opts) ptsArr0 bbox endpointIds
      interiorOrder = orderedIds (cdtUseBRIO opts) ptsArr0 bbox interiorIds
      (ptsArr, s0, s1, s2) = extendWithSuperTriangle pts0 bbox
  mesh0 <- initMesh ptsArr n0 s0 s1 s2
  mesh1 <- foldM insertPoint mesh0 endpointOrder
  mesh2 <- foldM insertConstraint mesh1 edges
  mesh3 <- foldM insertPoint mesh2 interiorOrder
  mesh4 <- if cdtFinalRepair opts then repairAll mesh3 else Right mesh3
  when (cdtValidate opts) $ validateMesh mesh4 edges
  pure (extractDomainTriangles mesh4)

validateInputPSLG
  :: forall p a. (HasXY p a, RealFloat a, Show a)
  => [p]
  -> [EdgeIx]
  -> Either String ()
validateInputPSLG [] _ = Left "triangulateCDT: empty point set"
validateInputPSLG ptsIn edges = do
  let !pts = map pointXY ptsIn
  let !n = length pts
      indexed = zip [0 :: Int ..] pts
      badCoord = listToMaybe [ (i, p) | (i, p@(x, y)) <- indexed, any isBad [x, y] ]
      dupMap = foldl' insertDup M.empty indexed
      dups = [ (p, i0, i1) | (p, (i0, Just i1)) <- M.toList dupMap ]
  case badCoord of
    Just (i, p) -> Left ("triangulateCDT: non-finite coordinate at index " ++ show i ++ ": " ++ show p)
    Nothing -> pure ()
  case dups of
    ((p, i0, i1):_) ->
      Left ("triangulateCDT: duplicate points at indices " ++ show i0 ++ " and " ++ show i1 ++ " with coordinates " ++ show p)
    [] -> pure ()
  mapM_ (checkEdge n) edges
  where
    insertDup :: M.Map (Point2 a) (Int, Maybe Int) -> (Int, Point2 a) -> M.Map (Point2 a) (Int, Maybe Int)
    insertDup acc (i, p) =
      case M.lookup p acc of
        Nothing -> M.insert p (i, Nothing) acc
        Just (j, _) -> M.insert p (j, Just i) acc
    isBad :: a -> Bool
    isBad x = isNaN x || isInfinite x
    checkEdge :: Int -> EdgeIx -> Either String ()
    checkEdge n (u, v)
      | u < 0 || v < 0 || u >= n || v >= n =
          Left ("triangulateCDT: edge index out of bounds: " ++ show (u, v))
      | u == v =
          Left ("triangulateCDT: zero-length constraint edge at index " ++ show u)
      | otherwise = pure ()

--------------------------------------------------------------------------------
-- Robust predicates
--------------------------------------------------------------------------------

epsilon :: forall a. RealFloat a => a
epsilon = encodeFloat 1 (1 - floatDigits (0 :: a))

orientApprox :: RealFloat a => Point2 a -> Point2 a -> Point2 a -> a
orientApprox (ax, ay) (bx, by) (cx, cy) =
  (bx - ax) * (cy - ay) - (by - ay) * (cx - ax)

orientExact :: Real a => Point2 a -> Point2 a -> Point2 a -> Rational
orientExact (ax, ay) (bx, by) (cx, cy) =
  let ax' = toRational ax
      ay' = toRational ay
      bx' = toRational bx
      by' = toRational by
      cx' = toRational cx
      cy' = toRational cy
  in (bx' - ax') * (cy' - ay') - (by' - ay') * (cx' - ax')

orientSign :: RealFloat a => Point2 a -> Point2 a -> Point2 a -> Ordering
orientSign a@(ax, ay) b@(bx, by) c@(cx, cy) =
  let !det = orientApprox a b c
      !permanent = (abs (bx - ax) + abs (cx - ax)) * (abs (by - ay) + abs (cy - ay))
      !err = 16.0 * epsilon * permanent
  in if abs det > err then compare det 0 else compare (orientExact a b c) 0

incircleApprox :: RealFloat a => Point2 a -> Point2 a -> Point2 a -> Point2 a -> a
incircleApprox (ax, ay) (bx, by) (cx, cy) (dx, dy) =
  let !adx = ax - dx
      !ady = ay - dy
      !bdx = bx - dx
      !bdy = by - dy
      !cdx = cx - dx
      !cdy = cy - dy
      !abdet = adx * bdy - bdx * ady
      !bcdet = bdx * cdy - cdx * bdy
      !cadet = cdx * ady - adx * cdy
      !alift = adx * adx + ady * ady
      !blift = bdx * bdx + bdy * bdy
      !clift = cdx * cdx + cdy * cdy
  in alift * bcdet + blift * cadet + clift * abdet

incircleExact :: Real a => Point2 a -> Point2 a -> Point2 a -> Point2 a -> Rational
incircleExact (ax, ay) (bx, by) (cx, cy) (dx, dy) =
  let ax' = toRational ax
      ay' = toRational ay
      bx' = toRational bx
      by' = toRational by
      cx' = toRational cx
      cy' = toRational cy
      dx' = toRational dx
      dy' = toRational dy
      adx = ax' - dx'
      ady = ay' - dy'
      bdx = bx' - dx'
      bdy = by' - dy'
      cdx = cx' - dx'
      cdy = cy' - dy'
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
      !adx = fst a - fst d
      !ady = snd a - snd d
      !bdx = fst b - fst d
      !bdy = snd b - snd d
      !cdx = fst c - fst d
      !cdy = snd c - snd d
      !abdet = adx * bdy - bdx * ady
      !bcdet = bdx * cdy - cdx * bdy
      !cadet = cdx * ady - adx * cdy
      !alift = adx * adx + ady * ady
      !blift = bdx * bdx + bdy * bdy
      !clift = cdx * cdx + cdy * cdy
      !permanent = abs bcdet * alift + abs cadet * blift + abs abdet * clift
      !err = 128.0 * epsilon * permanent
  in if abs det > err then compare det 0 else compare (incircleExact a b c d) 0

pointOnClosedSegment :: RealFloat a => Point2 a -> Point2 a -> Point2 a -> Bool
pointOnClosedSegment a@(ax, ay) b@(bx, by) p@(px, py) =
  orientSign a b p == EQ
    && px >= min ax bx && px <= max ax bx
    && py >= min ay by && py <= max ay by

properSegmentIntersection :: RealFloat a => Point2 a -> Point2 a -> Point2 a -> Point2 a -> Bool
properSegmentIntersection a b c d =
  let o1 = orientSign a b c
      o2 = orientSign a b d
      o3 = orientSign c d a
      o4 = orientSign c d b
  in o1 /= EQ && o2 /= EQ && o3 /= EQ && o4 /= EQ && o1 /= o2 && o3 /= o4

collinearOverlapMoreThanEndpoint :: RealFloat a => Point2 a -> Point2 a -> Point2 a -> Point2 a -> Bool
collinearOverlapMoreThanEndpoint a b c d =
  orientSign a b c == EQ
    && orientSign a b d == EQ
    && overlap1D (fst a) (fst b) (fst c) (fst d)
    && overlap1D (snd a) (snd b) (snd c) (snd d)
    && not (onlyTouchAtOneEndpoint a b c d)
  where
    overlap1D :: Ord a => a -> a -> a -> a -> Bool
    overlap1D p q r s = max (min p q) (min r s) < min (max p q) (max r s)
    onlyTouchAtOneEndpoint :: Eq a => a -> a -> a -> a -> Bool
    onlyTouchAtOneEndpoint p q r s =
      let ends = [p, q]
          other = [r, s]
      in length [ () | x <- ends, y <- other, x == y ] == 1

--------------------------------------------------------------------------------
-- Space-filling order
--------------------------------------------------------------------------------

orderedIds :: RealFloat a => Bool -> Array Int (Point2 a) -> BBox a -> [Int] -> [Int]
orderedIds False _ _ ids = ids
orderedIds True pts bbox ids = concatMap (sortRound bbox pts) (brioRounds bbox pts ids)

brioRounds :: RealFloat a => BBox a -> Array Int (Point2 a) -> [Int] -> [[Int]]
brioRounds bbox pts = go 0
  where
    go !_ [] = []
    go !_ xs | length xs <= 64 = [xs]
    go !bitIndex xs =
      let promoted = [ i | i <- xs, testBit (hashId bbox pts i) (bitIndex .&. 63) ]
          stayed   = [ i | i <- xs, not (testBit (hashId bbox pts i) (bitIndex .&. 63)) ]
      in case (promoted, stayed) of
           ([], _) -> [xs]
           (_, []) -> [xs]
           _       -> go (bitIndex + 1) promoted ++ [stayed]

sortRound :: RealFloat a => BBox a -> Array Int (Point2 a) -> [Int] -> [Int]
sortRound bbox pts = sortBy (comparing (hilbertOf bbox . (pts !)))

hashId :: RealFloat a => BBox a -> Array Int (Point2 a) -> Int -> Word64
hashId bbox pts i = splitmix64 (fromIntegral i `xor` hilbertOf bbox (pts ! i))

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
  | otherwise =
      let t = (x - lo) / (hi - lo)
          q = floor (t * 65535.0 + 0.5)
      in max 0 (min 65535 q)

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
      | ry == 0 =
          let (x1, y1) =
                if rx == 1
                  then (m - x, m - y)
                  else (x, y)
          in (y1, x1)
      | otherwise = (x, y)

--------------------------------------------------------------------------------
-- Constraint preprocessing
--------------------------------------------------------------------------------

preprocessConstraints :: RealFloat a => Array Int (Point2 a) -> [EdgeIx] -> Either String (M.Map EdgeIx [EdgeIx], [EdgeIx])
preprocessConstraints pts edges = do
  let (lo, hi) = bounds pts
      ids = [lo .. hi]
      n = hi - lo + 1
      basePack = n + 4
  expandedByRaw <- foldM (accumulateExpanded ids) M.empty edges
  let dedup =
        foldl'
          (\acc pieces -> foldl' (\setAcc edgeValue -> IS.insert (packUndirectedBase basePack edgeValue) setAcc) acc pieces)
          IS.empty
          (M.elems expandedByRaw)
      edges1 = sortBy compareEdge (map (unpackUndirectedBase basePack) (IS.toList dedup))
  validateNoCrossings pts edges1
  pure (expandedByRaw, edges1)
  where
    accumulateExpanded ids accum edgeValue = do
      pieces <- expandEdge pts ids edgeValue
      pure (M.insertWith (++) (normalizeEdge edgeValue) pieces accum)

expandEdge :: RealFloat a => Array Int (Point2 a) -> [Int] -> EdgeIx -> Either String [EdgeIx]
expandEdge pts ids (u0, v0) = do
  let (u, v) = normalizeEdge (u0, v0)
      pu = pts ! u
      pv = pts ! v
  when (pu == pv) $ Left ("triangulateCDT: zero-length geometric constraint between indices " ++ show u ++ " and " ++ show v)
  let mids = [ i | i <- ids, i /= u, i /= v, pointOnClosedSegment pu pv (pts ! i) ]
      proj i = projectionParam pu pv (pts ! i)
      chain = u : sortBy (comparing proj) mids ++ [v]
      pieces = [ normalizeEdge (a, b) | (a, b) <- adjacentPairs chain, a /= b ]
  pure pieces

validateNoCrossings :: RealFloat a => Array Int (Point2 a) -> [EdgeIx] -> Either String ()
validateNoCrossings pts es =
  mapM_ checkPair [ (e1, e2) | (i, e1) <- zip [0 :: Int ..] es, e2 <- drop (i + 1) es ]
  where
    checkPair ((u1, v1), (u2, v2))
      | sharedEndpoint (u1, v1) (u2, v2) = pure ()
      | properSegmentIntersection (pts ! u1) (pts ! v1) (pts ! u2) (pts ! v2) =
          Left ("triangulateCDT: constraints cross in their interiors: " ++ show (u1, v1) ++ " and " ++ show (u2, v2))
      | collinearOverlapMoreThanEndpoint (pts ! u1) (pts ! v1) (pts ! u2) (pts ! v2) =
          Left ("triangulateCDT: overlapping collinear constraints remain after splitting: " ++ show (u1, v1) ++ " and " ++ show (u2, v2))
      | otherwise = pure ()

projectionParam :: RealFloat a => Point2 a -> Point2 a -> Point2 a -> a
projectionParam (ax, ay) (bx, by) (px, py) =
  let dx = bx - ax
      dy = by - ay
      den = dx * dx + dy * dy
  in if den == 0 then 0 else ((px - ax) * dx + (py - ay) * dy) / den

closedPairs :: [a] -> [(a, a)]
closedPairs [] = []
closedPairs [value] = [(value, value)]
closedPairs (firstValue : remainingValues) = go firstValue firstValue remainingValues
  where
    go :: a -> a -> [a] -> [(a, a)]
    go startValue currentValue [] = [(currentValue, startValue)]
    go startValue currentValue (nextValue : tailValues) =
      (currentValue, nextValue) : go startValue nextValue tailValues

--------------------------------------------------------------------------------
-- Mesh representation
--------------------------------------------------------------------------------

type Tri :: Type
data Tri = Tri !Int !Int !Int deriving stock (Eq, Show)

type Mesh :: Type -> Type
data Mesh a = Mesh
  { meshPoints       :: !(Array Int (Point2 a))
  , meshOriginalN    :: !Int
  , meshBase         :: !Int
  , meshTris         :: !(IM.IntMap Tri)
  , meshDirEdge      :: !(IM.IntMap Int)
  , meshIncident     :: !(IM.IntMap IS.IntSet)
  , meshConstrained  :: !IS.IntSet
  , meshInserted     :: !IS.IntSet
  , meshNextTid      :: !Int
  , meshSuper0       :: !Int
  , meshSuper1       :: !Int
  , meshSuper2       :: !Int
  , meshHint         :: !(Maybe Int)
  } deriving stock (Show)

type Location :: Type
data Location
  = LocatedIn !Int
  | LocatedOnEdge !Int !Int !(Maybe Int) !(Maybe Int)
  | LocatedAtVertex !Int
  deriving stock (Eq, Show)

initMesh :: RealFloat a => Array Int (Point2 a) -> Int -> Int -> Int -> Int -> Either String (Mesh a)
initMesh pts originalN s0 s1 s2 = do
  let base = snd (bounds pts) + 2
      mesh0 = Mesh
        { meshPoints = pts
        , meshOriginalN = originalN
        , meshBase = base
        , meshTris = IM.empty
        , meshDirEdge = IM.empty
        , meshIncident = IM.empty
        , meshConstrained = IS.empty
        , meshInserted = IS.fromList [s0, s1, s2]
        , meshNextTid = 0
        , meshSuper0 = s0
        , meshSuper1 = s1
        , meshSuper2 = s2
        , meshHint = Nothing
        }
  addTriangle mesh0 s0 s1 s2

extendWithSuperTriangle :: RealFloat a => [Point2 a] -> BBox a -> (Array Int (Point2 a), Int, Int, Int)
extendWithSuperTriangle pts (xmin, ymin, xmax, ymax) =
  let !n = length pts
      !dx = max 1.0 (xmax - xmin)
      !dy = max 1.0 (ymax - ymin)
      !d  = max dx dy
      !cx = 0.5 * (xmin + xmax)
      !cy = 0.5 * (ymin + ymax)
      p0 = (cx - 32.0 * d, cy - 16.0 * d)
      p1 = (cx, cy + 32.0 * d)
      p2 = (cx + 32.0 * d, cy - 16.0 * d)
      arr = listArray (0, n + 2) (pts ++ [p0, p1, p2])
  in (arr, n, n + 1, n + 2)

normalizeEdge :: EdgeIx -> EdgeIx
normalizeEdge (u, v)
  | u <= v = (u, v)
  | otherwise = (v, u)

compareEdge :: EdgeIx -> EdgeIx -> Ordering
compareEdge (a0, b0) (a1, b1) = compare (a0, b0) (a1, b1)

sharedEndpoint :: EdgeIx -> EdgeIx -> Bool
sharedEndpoint (u0, v0) (u1, v1) = u0 == u1 || u0 == v1 || v0 == u1 || v0 == v1

packDirected :: Mesh a -> Int -> Int -> Int
packDirected mesh u v = u * meshBase mesh + v

packUndirected :: Mesh a -> EdgeIx -> Int
packUndirected mesh (u, v) =
  let (a, b) = normalizeEdge (u, v)
  in a * meshBase mesh + b

packUndirectedBase :: Int -> EdgeIx -> Int
packUndirectedBase base (u, v) =
  let (a, b) = normalizeEdge (u, v)
  in a * base + b

unpackUndirectedBase :: Int -> Int -> EdgeIx
unpackUndirectedBase base k = k `divMod` base

triangleVerts :: Tri -> (Int, Int, Int)
triangleVerts (Tri a b c) = (a, b, c)

triEdges :: Tri -> [EdgeIx]
triEdges (Tri a b c) = [(a, b), (b, c), (c, a)]

triContainsVertex :: Int -> Tri -> Bool
triContainsVertex u (Tri a b c) = u == a || u == b || u == c

thirdOnDirectedEdge :: Tri -> Int -> Int -> Maybe Int
thirdOnDirectedEdge (Tri a b c) u v
  | a == u && b == v = Just c
  | b == u && c == v = Just a
  | c == u && a == v = Just b
  | otherwise = Nothing

lookupTri :: Mesh a -> Int -> Either String Tri
lookupTri mesh tid =
  case IM.lookup tid (meshTris mesh) of
    Nothing -> Left ("internal CDT error: missing triangle id " ++ show tid)
    Just t  -> Right t

lookupEdgeTid :: Mesh a -> Int -> Int -> Maybe Int
lookupEdgeTid mesh u v = IM.lookup (packDirected mesh u v) (meshDirEdge mesh)

edgeExists :: Mesh a -> Int -> Int -> Bool
edgeExists mesh u v = isJust (lookupEdgeTid mesh u v) || isJust (lookupEdgeTid mesh v u)

isConstraintEdge :: Mesh a -> EdgeIx -> Bool
isConstraintEdge mesh e = IS.member (packUndirected mesh e) (meshConstrained mesh)

addConstraintEdge :: Mesh a -> EdgeIx -> Mesh a
addConstraintEdge mesh e = mesh { meshConstrained = IS.insert (packUndirected mesh e) (meshConstrained mesh) }

removeConstraintEdge :: Mesh a -> EdgeIx -> Mesh a
removeConstraintEdge mesh e = mesh { meshConstrained = IS.delete (packUndirected mesh e) (meshConstrained mesh) }

addIncident :: Int -> Int -> IM.IntMap IS.IntSet -> IM.IntMap IS.IntSet
addIncident v tid = IM.insertWith IS.union v (IS.singleton tid)

delIncident :: Int -> Int -> IM.IntMap IS.IntSet -> IM.IntMap IS.IntSet
delIncident v tid im =
  case IM.lookup v im of
    Nothing -> im
    Just s ->
      let s' = IS.delete tid s
      in if IS.null s' then IM.delete v im else IM.insert v s' im

mkCCWTri :: RealFloat a => Array Int (Point2 a) -> Int -> Int -> Int -> Either String Tri
mkCCWTri pts a b c =
  case orientSign (pts ! a) (pts ! b) (pts ! c) of
    GT -> Right (Tri a b c)
    LT -> Right (Tri a c b)
    EQ -> Left ("triangulateCDT: attempted to create zero-area triangle from vertices " ++ show (a, b, c))

addTriangle :: RealFloat a => Mesh a -> Int -> Int -> Int -> Either String (Mesh a)
addTriangle mesh a b c = do
  tri@(Tri x y z) <- mkCCWTri (meshPoints mesh) a b c
  let k1 = packDirected mesh x y
      k2 = packDirected mesh y z
      k3 = packDirected mesh z x
  when (any (\k -> IM.member k (meshDirEdge mesh)) [k1, k2, k3]) $
    Left ("internal CDT error: duplicate directed edge while inserting triangle " ++ show tri)
  let !tid = meshNextTid mesh
      tris' = IM.insert tid tri (meshTris mesh)
      dir' = IM.insert k1 tid . IM.insert k2 tid . IM.insert k3 tid $ meshDirEdge mesh
      inc' = addIncident x tid . addIncident y tid . addIncident z tid $ meshIncident mesh
  pure mesh
    { meshTris = tris'
    , meshDirEdge = dir'
    , meshIncident = inc'
    , meshNextTid = tid + 1
    , meshHint = Just tid
    }

deleteTriangleById :: Mesh a -> Int -> Either String (Mesh a)
deleteTriangleById mesh tid = do
  Tri a b c <- lookupTri mesh tid
  let tris' = IM.delete tid (meshTris mesh)
      dir' = IM.delete (packDirected mesh a b)
           . IM.delete (packDirected mesh b c)
           . IM.delete (packDirected mesh c a)
           $ meshDirEdge mesh
      inc' = delIncident a tid . delIncident b tid . delIncident c tid $ meshIncident mesh
      hint' = case meshHint mesh of
                Just h | h == tid -> Nothing
                _ -> meshHint mesh
  pure mesh
    { meshTris = tris'
    , meshDirEdge = dir'
    , meshIncident = inc'
    , meshHint = hint'
    }

otherTwo :: Tri -> Int -> Maybe (Int, Int)
otherTwo (Tri a b c) u
  | u == a = Just (b, c)
  | u == b = Just (c, a)
  | u == c = Just (a, b)
  | otherwise = Nothing

--------------------------------------------------------------------------------
-- Point location and insertion
--------------------------------------------------------------------------------

insertPoint :: RealFloat a => Mesh a -> Int -> Either String (Mesh a)
insertPoint mesh pid
  | IS.member pid (meshInserted mesh) = pure mesh
  | otherwise = do
      let p = meshPoints mesh ! pid
      loc <- locatePoint mesh p
      case loc of
        LocatedAtVertex v ->
          Left ("triangulateCDT: distinct input points coincide geometrically at indices " ++ show pid ++ " and " ++ show v)
        LocatedIn tid -> do
          mesh1 <- splitTriangleWithPoint mesh tid pid
          pure mesh1 { meshInserted = IS.insert pid (meshInserted mesh1) }
        LocatedOnEdge u v lt rt -> do
          mesh1 <- splitEdgeWithPoint mesh u v lt rt pid
          pure mesh1 { meshInserted = IS.insert pid (meshInserted mesh1) }

locatePoint :: RealFloat a => Mesh a -> Point2 a -> Either String Location
locatePoint mesh p =
  case meshHint mesh <|> fmap fst (IM.lookupMin (meshTris mesh)) of
    Nothing -> Left "internal CDT error: no triangle available for point location"
    Just start -> walk start 0
  where
    triCount = max 1 (IM.size (meshTris mesh))
    walk !tid !steps
      | steps > triCount + 8 = locatePointSlow mesh p
      | otherwise = do
          Tri a b c <- lookupTri mesh tid
          let pa = meshPoints mesh ! a
              pb = meshPoints mesh ! b
              pc = meshPoints mesh ! c
          if p == pa then pure (LocatedAtVertex a)
          else if p == pb then pure (LocatedAtVertex b)
          else if p == pc then pure (LocatedAtVertex c)
          else
            let oab = orientSign pa pb p
                obc = orientSign pb pc p
                oca = orientSign pc pa p
            in if all (/= LT) [oab, obc, oca]
                 then case filter ((== EQ) . snd)
                            [((a, b), oab), ((b, c), obc), ((c, a), oca)] of
                        [] -> pure (LocatedIn tid)
                        (((u, v), _):_) ->
                          pure (LocatedOnEdge u v (lookupEdgeTid mesh u v) (lookupEdgeTid mesh v u))
                 else do
                   let candidates = [ (a, b, oab), (b, c, obc), (c, a, oca) ]
                       nextEdge = listToMaybe [ (u, v) | (u, v, LT) <- candidates ]
                   case nextEdge of
                     Nothing -> locatePointSlow mesh p
                     Just (u, v) ->
                       case lookupEdgeTid mesh v u of
                         Nothing -> locatePointSlow mesh p
                         Just nextTid -> walk nextTid (steps + 1)

locatePointSlow :: RealFloat a => Mesh a -> Point2 a -> Either String Location
locatePointSlow mesh p =
  case mapMaybe classify (IM.toList (meshTris mesh)) of
    (loc:_) -> pure loc
    [] -> Left "triangulateCDT: point location failed"
  where
    classify (tid, Tri a b c)
      | p == meshPoints mesh ! a = Just (LocatedAtVertex a)
      | p == meshPoints mesh ! b = Just (LocatedAtVertex b)
      | p == meshPoints mesh ! c = Just (LocatedAtVertex c)
      | otherwise =
          let pa = meshPoints mesh ! a
              pb = meshPoints mesh ! b
              pc = meshPoints mesh ! c
              oab = orientSign pa pb p
              obc = orientSign pb pc p
              oca = orientSign pc pa p
          in if all (/= LT) [oab, obc, oca]
               then case filter ((== EQ) . snd)
                          [((a, b), oab), ((b, c), obc), ((c, a), oca)] of
                      [] -> Just (LocatedIn tid)
                      (((u, v), _):_) -> Just (LocatedOnEdge u v (lookupEdgeTid mesh u v) (lookupEdgeTid mesh v u))
               else Nothing

splitTriangleWithPoint :: RealFloat a => Mesh a -> Int -> Int -> Either String (Mesh a)
splitTriangleWithPoint mesh tid pid = do
  Tri a b c <- lookupTri mesh tid
  mesh1 <- deleteTriangleById mesh tid
  mesh2 <- addTriangle mesh1 a b pid
  mesh3 <- addTriangle mesh2 b c pid
  mesh4 <- addTriangle mesh3 c a pid
  repairEdges mesh4 [(a, b), (b, c), (c, a)]

splitEdgeWithPoint :: RealFloat a => Mesh a -> Int -> Int -> Maybe Int -> Maybe Int -> Int -> Either String (Mesh a)
splitEdgeWithPoint mesh u v lt rt pid = do
  let leftOpp  = lt >>= oppositeEither u v
      rightOpp = rt >>= oppositeEither v u
  mesh1 <- maybe (Right mesh) (deleteTriangleById mesh) lt
  mesh2 <- maybe (Right mesh1) (deleteTriangleById mesh1) rt
  let constrained = isConstraintEdge mesh (u, v)
      mesh3 = if constrained
                then addConstraintEdge (addConstraintEdge (removeConstraintEdge mesh2 (u, v)) (u, pid)) (pid, v)
                else mesh2
  mesh4 <- case leftOpp of
             Nothing -> pure mesh3
             Just a  -> do
               m1 <- addTriangle mesh3 u pid a
               addTriangle m1 pid v a
  mesh5 <- case rightOpp of
             Nothing -> pure mesh4
             Just b  -> do
               m1 <- addTriangle mesh4 v pid b
               addTriangle m1 pid u b
  let seeds = concat
        [ maybe [] (\a -> [(u, a), (a, v)]) leftOpp
        , maybe [] (\b -> [(v, b), (b, u)]) rightOpp
        ]
  repairEdges mesh5 seeds
  where
    oppositeEither a b tid = do
      tri <- IM.lookup tid (meshTris mesh)
      case thirdOnDirectedEdge tri a b of
        Just w  -> Just w
        Nothing -> thirdOnDirectedEdge tri b a

--------------------------------------------------------------------------------
-- Constraint insertion
--------------------------------------------------------------------------------

insertConstraint :: RealFloat a => Mesh a -> EdgeIx -> Either String (Mesh a)
insertConstraint mesh (u, v)
  | not (IS.member u (meshInserted mesh) && IS.member v (meshInserted mesh)) =
      Left ("triangulateCDT: constraint endpoints must be inserted before the constraint edge " ++ show (u, v))
  | edgeExists mesh u v = pure (addConstraintEdge mesh (u, v))
  | otherwise = do
      walk <- traceSegmentCorridor mesh u v
      let tids = map stepTid walk
      mesh1 <- foldM deleteTriangleById mesh tids
      let (leftChain, rightChain) = buildChains mesh u v walk
      leftTris <- triangulateSidePolygon (meshPoints mesh) leftChain
      rightTris <- triangulateSidePolygon (meshPoints mesh) rightChain
      let mesh2 = addConstraintEdge mesh1 (u, v)
      mesh3 <- foldM (\m (a, b, c) -> addTriangle m a b c) mesh2 (leftTris ++ rightTris)
      repairEdges mesh3 (concatMap triToEdges (leftTris ++ rightTris))

triToEdges :: TriIx -> [EdgeIx]
triToEdges (a, b, c) = [(a, b), (b, c), (c, a)]

type CorridorStep :: Type
data CorridorStep = CorridorStep
  { stepTid   :: !Int
  , stepEntry :: !(Maybe EdgeIx)
  } deriving stock (Eq, Show)

traceSegmentCorridor :: RealFloat a => Mesh a -> Int -> Int -> Either String [CorridorStep]
traceSegmentCorridor mesh u v = do
  startTid <- findStartTriangle mesh u v
  go startTid Nothing []
  where
    pu = meshPoints mesh ! u
    pv = meshPoints mesh ! v

    go !tid !entry !acc = do
      tri <- lookupTri mesh tid
      if triContainsVertex v tri
        then pure (reverse (CorridorStep tid entry : acc))
        else do
          exit <- chooseExitEdge tri entry
          when (isConstraintEdge mesh exit) $
            Left ("triangulateCDT: attempted to insert a constraint that crosses an existing constraint near edge " ++ show exit)
          case lookupEdgeTid mesh (snd exit) (fst exit) of
            Nothing -> Left "internal CDT error: segment corridor walk hit a boundary unexpectedly"
            Just nextTid -> go nextTid (Just exit) (CorridorStep tid entry : acc)

    chooseExitEdge tri mEntry =
      let es = filter (not . sameUndirected mEntry) (triEdges tri)
          hits = [ e | e@(a, b) <- es, properSegmentIntersection pu pv (meshPoints mesh ! a) (meshPoints mesh ! b) ]
      in case hits of
           [e] -> pure e
           []  -> Left ("triangulateCDT: could not find corridor exit edge while inserting constraint " ++ show (u, v))
           _   -> Left ("triangulateCDT: ambiguous corridor exit edge while inserting constraint " ++ show (u, v))

    sameUndirected Nothing _ = False
    sameUndirected (Just e0) e1 = normalizeEdge e0 == normalizeEdge e1

findStartTriangle :: RealFloat a => Mesh a -> Int -> Int -> Either String Int
findStartTriangle mesh u v =
  let pu = meshPoints mesh ! u
      pv = meshPoints mesh ! v
      candidates = maybe [] IS.toList (IM.lookup u (meshIncident mesh))
      good tid = do
        tri <- IM.lookup tid (meshTris mesh)
        (a, b) <- otherTwo tri u
        pure (properSegmentIntersection pu pv (meshPoints mesh ! a) (meshPoints mesh ! b))
      hits = [ tid | tid <- candidates, fromMaybe False (good tid) ]
  in case hits of
       (tid:_) -> Right tid
       [] -> Left ("triangulateCDT: could not locate the first crossed triangle for constraint " ++ show (u, v))

buildChains :: RealFloat a => Mesh a -> Int -> Int -> [CorridorStep] -> ([Int], [Int])
buildChains _ u v [] = ([u, v], [u, v])
buildChains mesh u v (startStep : remainingSteps) =
  let pts = meshPoints mesh
      startTri = meshTris mesh IM.! stepTid startStep
  in case otherTwo startTri u of
       Nothing -> ([u, v], [u, v])
       Just (a0, b0) ->
         let seedLeft = if orientSign (pts ! u) (pts ! v) (pts ! a0) == GT then [u, a0] else [u, b0]
             seedRight = if orientSign (pts ! u) (pts ! v) (pts ! a0) == GT then [u, b0] else [u, a0]
             grow (ls, rs) (CorridorStep tid mEntry) =
               let tri = meshTris mesh IM.! tid
                in if triContainsVertex v tri
                     then (appendIfNew ls v, appendIfNew rs v)
                     else case mEntry of
                       Nothing -> (ls, rs)
                       Just (e0, e1) ->
                         case uniqueVertex tri e0 e1 of
                           Nothing -> (ls, rs)
                           Just w ->
                             case orientSign (pts ! u) (pts ! v) (pts ! w) of
                               GT -> (appendIfNew ls w, rs)
                               LT -> (ls, appendIfNew rs w)
                               EQ -> (ls, rs)
             (left0, right0) = foldl' grow (seedLeft, seedRight) remainingSteps
          in (appendIfNew left0 v, appendIfNew right0 v)

appendIfNew :: [Int] -> Int -> [Int]
appendIfNew [] x = [x]
appendIfNew xs x
  | otherwise =
      case reverse xs of
        lastValue : _ | lastValue == x -> xs
        _ -> xs ++ [x]

uniqueVertex :: Tri -> Int -> Int -> Maybe Int
uniqueVertex (Tri a b c) u v
  | a /= u && a /= v = Just a
  | b /= u && b /= v = Just b
  | c /= u && c /= v = Just c
  | otherwise = Nothing

--------------------------------------------------------------------------------
-- Cavity retriangulation for corridor sides
--------------------------------------------------------------------------------

type CavityStep :: Type
data CavityStep = CavityStep !Int !Int !Int deriving stock (Eq, Show)
type CavityPlan :: Type
data CavityPlan = CavityPlan !(Maybe Int) ![CavityStep] deriving stock (Eq, Show)

type LTri :: Type
data LTri = LTri !Int !Int !Int deriving stock (Eq, Show)

type LocalMesh :: Type -> Type
data LocalMesh a = LocalMesh
  { localPoints   :: !(Array Int (Point2 a))
  , localToGlobal :: !(Array Int Int)
  , localBase     :: !Int
  , localTris     :: !(IM.IntMap LTri)
  , localDirEdge  :: !(IM.IntMap Int)
  , localNextTid  :: !Int
  } deriving stock (Show)

triangulateSidePolygon :: RealFloat a => Array Int (Point2 a) -> [Int] -> Either String [TriIx]
triangulateSidePolygon _ [_, _] = Right []
triangulateSidePolygon pts vs0 = do
  let vs = ensureCCWPolygon pts (dropRepeatedConsecutive vs0)
      !m = length vs
  unless (m >= 2) $
    Left "internal CDT error: empty side polygon"
  let loc2glob = listArray (0, m - 1) vs
      locPts = listArray (0, m - 1) [ pts ! g | g <- vs ]
  plan <- buildCavityPlan locPts vs
  lm0 <- initLocalMesh locPts loc2glob
  lm1 <- case plan of
           CavityPlan Nothing _    -> pure lm0
           CavityPlan (Just mid) _ -> lAddTriangle lm0 0 mid (m - 1)
  lm2 <- foldM insertCavityStep lm1 (cavityPlanSteps plan)
  lm3 <- lRepairAllLocal lm2
  mapM (localTriToGlobal pts loc2glob) (IM.elems (localTris lm3))

cavityPlanSteps :: CavityPlan -> [CavityStep]
cavityPlanSteps (CavityPlan _ xs) = xs

buildCavityPlan :: RealFloat a => Array Int (Point2 a) -> [Int] -> Either String CavityPlan
buildCavityPlan locPts globVs
  | m < 2 = Left "triangulateCDT: cavity chain too short"
  | m == 2 = Right (CavityPlan Nothing [])
  | otherwise = go active0 prev0 next0 []
  where
    (_, hi) = bounds locPts
    m = hi + 1
    p0 = locPts ! 0
    p1 = locPts ! (m - 1)
    distMap = IM.fromList [ (i, abs (orientApprox p0 (locPts ! i) p1)) | i <- [0 .. m - 1] ]
    seed = foldl' (\s i -> splitmix64 (s `xor` fromIntegral i)) 0x9e3779b97f4a7c15 globVs
    prev0 = IM.fromList [ (i, if i == 0 then 0 else i - 1) | i <- [0 .. m - 1] ]
    next0 = IM.fromList [ (i, if i == m - 1 then m - 1 else i + 1) | i <- [0 .. m - 1] ]
    active0 = IS.fromList [0 .. m - 1]

    go !active !prevMap !nextMap !deleted
      | IS.size active < 3 =
          Left "triangulateCDT: cavity point-location planning collapsed below a triangle"
      | IS.size active == 3 =
          case [ i | i <- [1 .. m - 2], IS.member i active ] of
            [mid] -> Right (CavityPlan (Just mid) (reverse deleted))
            _ -> Left "triangulateCDT: failed to isolate the base cavity triangle"
      | otherwise = do
          let interior = [ i | i <- [1 .. m - 2], IS.member i active ]
              eligible = filter (eligibleVertex prevMap nextMap) interior
              pool = if null eligible then interior else eligible
          u <- chooseVertex seed active pool
          let pu = mustLookupIntMap "triangulateCDT: missing cavity prev link" prevMap u
              nu = mustLookupIntMap "triangulateCDT: missing cavity next link" nextMap u
              prevMap' = IM.insert nu pu prevMap
              nextMap' = IM.insert pu nu nextMap
              active' = IS.delete u active
          go active' prevMap' nextMap' (CavityStep u pu nu : deleted)

    eligibleVertex prevMap nextMap u =
      let pu = mustLookupIntMap "triangulateCDT: missing cavity prev link" prevMap u
          nu = mustLookupIntMap "triangulateCDT: missing cavity next link" nextMap u
          du = mustLookupIntMap "triangulateCDT: missing cavity distance" distMap u
          dp = mustLookupIntMap "triangulateCDT: missing cavity distance" distMap pu
          dn = mustLookupIntMap "triangulateCDT: missing cavity distance" distMap nu
      in not (du < dp && du < dn)

chooseVertex :: Word64 -> IS.IntSet -> [Int] -> Either String Int
chooseVertex _ _ [] = Left "triangulateCDT: no interior cavity vertex available"
chooseVertex seed active xs =
  pure (minimumBy (comparing rank) xs)
  where
    rank u = (splitmix64 (seed `xor` fromIntegral (IS.size active) `xor` fromIntegral u), u)

mustLookupIntMap :: String -> IM.IntMap a -> Int -> a
mustLookupIntMap msg im k =
  fromMaybe (error msg) (IM.lookup k im)

initLocalMesh :: Array Int (Point2 a) -> Array Int Int -> Either String (LocalMesh a)
initLocalMesh pts loc2glob =
  pure LocalMesh
    { localPoints = pts
    , localToGlobal = loc2glob
    , localBase = snd (bounds pts) + 2
    , localTris = IM.empty
    , localDirEdge = IM.empty
    , localNextTid = 0
    }

lPackDirected :: LocalMesh a -> Int -> Int -> Int
lPackDirected lm u v = u * localBase lm + v

lTriangleVerts :: LTri -> TriIx
lTriangleVerts (LTri a b c) = (a, b, c)

lTriEdges :: LTri -> [EdgeIx]
lTriEdges (LTri a b c) = [(a, b), (b, c), (c, a)]

lThirdOnDirectedEdge :: LTri -> Int -> Int -> Maybe Int
lThirdOnDirectedEdge (LTri a b c) u v
  | a == u && b == v = Just c
  | b == u && c == v = Just a
  | c == u && a == v = Just b
  | otherwise = Nothing

lLookupTri :: LocalMesh a -> Int -> Either String LTri
lLookupTri lm tid =
  case IM.lookup tid (localTris lm) of
    Nothing -> Left ("internal CDT error: missing local triangle id " ++ show tid)
    Just t  -> Right t

lLookupEdgeTid :: LocalMesh a -> Int -> Int -> Maybe Int
lLookupEdgeTid lm u v = IM.lookup (lPackDirected lm u v) (localDirEdge lm)

lMkCCWTri :: RealFloat a => Array Int (Point2 a) -> Int -> Int -> Int -> Either String LTri
lMkCCWTri pts a b c =
  case orientSign (pts ! a) (pts ! b) (pts ! c) of
    GT -> Right (LTri a b c)
    LT -> Right (LTri a c b)
    EQ -> Left ("triangulateCDT: local cavity produced a zero-area triangle at occurrences " ++ show (a, b, c))

lAddTriangle :: RealFloat a => LocalMesh a -> Int -> Int -> Int -> Either String (LocalMesh a)
lAddTriangle lm a b c = do
  tri@(LTri x y z) <- lMkCCWTri (localPoints lm) a b c
  let k1 = lPackDirected lm x y
      k2 = lPackDirected lm y z
      k3 = lPackDirected lm z x
  when (any (\k -> IM.member k (localDirEdge lm)) [k1, k2, k3]) $
    Left ("triangulateCDT: local cavity edge conflict while inserting triangle " ++ show tri)
  let !tid = localNextTid lm
      tris' = IM.insert tid tri (localTris lm)
      dir' = IM.insert k1 tid . IM.insert k2 tid . IM.insert k3 tid $ localDirEdge lm
  pure lm
    { localTris = tris'
    , localDirEdge = dir'
    , localNextTid = tid + 1
    }

lDeleteTriangleById :: LocalMesh a -> Int -> Either String (LocalMesh a)
lDeleteTriangleById lm tid = do
  LTri a b c <- lLookupTri lm tid
  let tris' = IM.delete tid (localTris lm)
      dir' = IM.delete (lPackDirected lm a b)
           . IM.delete (lPackDirected lm b c)
           . IM.delete (lPackDirected lm c a)
           $ localDirEdge lm
  pure lm
    { localTris = tris'
    , localDirEdge = dir'
    }

insertCavityStep :: RealFloat a => LocalMesh a -> CavityStep -> Either String (LocalMesh a)
insertCavityStep lm (CavityStep u v w) = do
  lm1 <- lInsertVertex lm u v w
  lRepairAllLocal lm1

lInsertVertex :: RealFloat a => LocalMesh a -> Int -> Int -> Int -> Either String (LocalMesh a)
lInsertVertex lm u v w =
  case lLookupEdgeTid lm w v of
    Nothing -> lAddTriangle lm u v w
    Just tid -> do
      tri <- lLookupTri lm tid
      case lThirdOnDirectedEdge tri w v of
        Nothing -> Left "internal CDT error: local cavity adjacency mismatch"
        Just x ->
          let goodOrientation = orientSign (localPoints lm ! u) (localPoints lm ! v) (localPoints lm ! w) == GT
              emptyCircle = incircleSign (localPoints lm ! u) (localPoints lm ! v) (localPoints lm ! w) (localPoints lm ! x) /= GT
          in if goodOrientation && emptyCircle
               then lAddTriangle lm u v w
               else do
                 lm1 <- lDeleteTriangleById lm tid
                 lm2 <- lInsertVertex lm1 u v x
                 lInsertVertex lm2 u x w

lRepairAllLocal :: RealFloat a => LocalMesh a -> Either String (LocalMesh a)
lRepairAllLocal lm = lRepairEdgesLocal lm (lAllInteriorEdgesLocal lm)

lAllInteriorEdgesLocal :: LocalMesh a -> [EdgeIx]
lAllInteriorEdgesLocal lm =
  [ e
  | e <- map (unpackUndirectedBase (localBase lm)) (IS.toList edgeSet)
  , isInterior e
  ]
  where
    edgeSet = foldl' ins IS.empty (IM.elems (localTris lm))
    ins acc tri = foldl' (\s e -> IS.insert (packUndirectedBase (localBase lm) e) s) acc (lTriEdges tri)
    isInterior (u, v) = isJust (lLookupEdgeTid lm u v) && isJust (lLookupEdgeTid lm v u)

lRepairEdgesLocal :: forall a. RealFloat a => LocalMesh a -> [EdgeIx] -> Either String (LocalMesh a)
lRepairEdgesLocal lm0 seeds = go lm0 (map normalizeEdge seeds)
  where
    go :: LocalMesh a -> [EdgeIx] -> Either String (LocalMesh a)
    go !lm [] = pure lm
    go !lm ((u, v):es)
      | u == v = go lm es
      | otherwise =
          case (lLookupEdgeTid lm u v, lLookupEdgeTid lm v u) of
            (Just lt, Just rt) -> do
              triL <- lLookupTri lm lt
              triR <- lLookupTri lm rt
              case (lThirdOnDirectedEdge triL u v, lThirdOnDirectedEdge triR v u) of
                (Just a, Just b)
                  | lShouldFlip lm u v a b -> do
                      lm1 <- lFlipEdgeLocal lm u v a b lt rt
                      let more = map normalizeEdge [(a, u), (u, b), (b, v), (v, a), (a, b)]
                      go lm1 (more ++ es)
                  | otherwise -> go lm es
                _ -> go lm es
            _ -> go lm es

lShouldFlip :: RealFloat a => LocalMesh a -> Int -> Int -> Int -> Int -> Bool
lShouldFlip lm u v a b =
  properSegmentIntersection (localPoints lm ! u) (localPoints lm ! v)
                            (localPoints lm ! a) (localPoints lm ! b)
    && incircleSign (localPoints lm ! u) (localPoints lm ! v) (localPoints lm ! a) (localPoints lm ! b) == GT

lFlipEdgeLocal :: RealFloat a => LocalMesh a -> Int -> Int -> Int -> Int -> Int -> Int -> Either String (LocalMesh a)
lFlipEdgeLocal lm u v a b lt rt = do
  lm1 <- lDeleteTriangleById lm lt
  lm2 <- lDeleteTriangleById lm1 rt
  lm3 <- lAddTriangle lm2 a b u
  lAddTriangle lm3 b a v

localTriToGlobal :: RealFloat a => Array Int (Point2 a) -> Array Int Int -> LTri -> Either String TriIx
localTriToGlobal pts loc2glob tri = do
  let (a, b, c) = lTriangleVerts tri
      ga = loc2glob ! a
      gb = loc2glob ! b
      gc = loc2glob ! c
  when (ga == gb || gb == gc || gc == ga) $
    Left ("triangulateCDT: local cavity generated a topological triangle that collapses geometrically at global vertices " ++ show (ga, gb, gc))
  Tri x y z <- mkCCWTri pts ga gb gc
  pure (x, y, z)

dropRepeatedConsecutive :: [Int] -> [Int]
dropRepeatedConsecutive [] = []
dropRepeatedConsecutive (x:xs) = x : go x xs
  where
    go :: Eq a => a -> [a] -> [a]
    go !_ [] = []
    go !p (y:ys)
      | p == y = go p ys
      | otherwise = y : go y ys

ensureCCWPolygon :: RealFloat a => Array Int (Point2 a) -> [Int] -> [Int]
ensureCCWPolygon _ [] = []
ensureCCWPolygon _ [x] = [x]
ensureCCWPolygon pts vs
  | polygonArea2 pts vs >= 0 = vs
  | otherwise = reverse vs

polygonArea2 :: RealFloat a => Array Int (Point2 a) -> [Int] -> a
polygonArea2 pts vs =
  sum
    [ x0 * y1 - x1 * y0
    | (i0, i1) <- closedPairs vs
    , let (x0, y0) = pts ! i0
    , let (x1, y1) = pts ! i1
    ]
--------------------------------------------------------------------------------
-- Local repair / Lawson flips
--------------------------------------------------------------------------------

repairAll :: RealFloat a => Mesh a -> Either String (Mesh a)
repairAll mesh = repairEdges mesh (allInteriorEdges mesh)

allInteriorEdges :: Mesh a -> [EdgeIx]
allInteriorEdges mesh =
  [ e
  | e <- map (unpackUndirectedBase (meshBase mesh)) (IS.toList edgeSet)
  , isInterior e
  ]
  where
    edgeSet = foldl' ins IS.empty (IM.elems (meshTris mesh))
    ins acc tri = foldl' (\s e -> IS.insert (packUndirected mesh e) s) acc (triEdges tri)
    isInterior (u, v) = isJust (lookupEdgeTid mesh u v) && isJust (lookupEdgeTid mesh v u)

repairEdges :: forall a. RealFloat a => Mesh a -> [EdgeIx] -> Either String (Mesh a)
repairEdges mesh0 seeds = go mesh0 (map normalizeEdge seeds)
  where
    go :: Mesh a -> [EdgeIx] -> Either String (Mesh a)
    go !mesh [] = pure mesh
    go !mesh (e@(u, v):es)
      | u == v = go mesh es
      | isConstraintEdge mesh e = go mesh es
      | otherwise =
          case (lookupEdgeTid mesh u v, lookupEdgeTid mesh v u) of
            (Just lt, Just rt) -> do
              triL <- lookupTri mesh lt
              triR <- lookupTri mesh rt
              case (thirdOnDirectedEdge triL u v, thirdOnDirectedEdge triR v u) of
                (Just a, Just b)
                  | shouldFlip mesh u v a b -> do
                      mesh1 <- flipEdge mesh u v a b lt rt
                      let more = map normalizeEdge [(a, u), (u, b), (b, v), (v, a), (a, b)]
                      go mesh1 (more ++ es)
                  | otherwise -> go mesh es
                _ -> go mesh es
            _ -> go mesh es

    shouldFlip :: Mesh a -> Int -> Int -> Int -> Int -> Bool
    shouldFlip mesh u v a b =
      properSegmentIntersection (meshPoints mesh ! u) (meshPoints mesh ! v)
                                (meshPoints mesh ! a) (meshPoints mesh ! b)
        && incircleSign (meshPoints mesh ! u) (meshPoints mesh ! v) (meshPoints mesh ! a) (meshPoints mesh ! b) == GT

flipEdge :: RealFloat a => Mesh a -> Int -> Int -> Int -> Int -> Int -> Int -> Either String (Mesh a)
flipEdge mesh u v a b lt rt = do
  mesh1 <- deleteTriangleById mesh lt
  mesh2 <- deleteTriangleById mesh1 rt
  mesh3 <- addTriangle mesh2 a b u
  addTriangle mesh3 b a v

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

validateMesh :: RealFloat a => Mesh a -> [EdgeIx] -> Either String ()
validateMesh mesh inputEdges = do
  mapM_ checkConstraint inputEdges
  mapM_ checkTri (IM.elems (meshTris mesh))
  mapM_ checkEdge (allInteriorEdges mesh)
  where
    checkConstraint (u, v)
      | edgeExists mesh u v = pure ()
      | otherwise = Left ("validateMesh: missing constraint edge " ++ show (u, v))
    checkTri (Tri a b c)
      | orientSign (meshPoints mesh ! a) (meshPoints mesh ! b) (meshPoints mesh ! c) == GT = pure ()
      | otherwise = Left ("validateMesh: non-CCW triangle " ++ show (a, b, c))
    checkEdge e@(u, v)
      | isConstraintEdge mesh e = pure ()
      | otherwise =
          case (lookupEdgeTid mesh u v, lookupEdgeTid mesh v u) of
            (Just lt, Just rt) -> do
              triL <- lookupTri mesh lt
              triR <- lookupTri mesh rt
              case (thirdOnDirectedEdge triL u v, thirdOnDirectedEdge triR v u) of
                (Just a, Just b)
                  | incircleSign (meshPoints mesh ! u) (meshPoints mesh ! v) (meshPoints mesh ! a) (meshPoints mesh ! b) == GT ->
                      Left ("validateMesh: non-Delaunay interior edge " ++ show e)
                  | otherwise -> pure ()
                _ -> pure ()
            _ -> pure ()

--------------------------------------------------------------------------------
-- Domain extraction
--------------------------------------------------------------------------------

extractDomainTriangles :: Mesh a -> [TriIx]
extractDomainTriangles mesh =
  let parity = classifyParities mesh
      noConstraints = IS.null (meshConstrained mesh)
      good (tid, Tri a b c) =
        not (any (isSuperVertex mesh) [a, b, c])
          && (if noConstraints then True else IM.findWithDefault 0 tid parity == 1)
      out = [ triangleVerts tri | (tid, tri) <- IM.toList (meshTris mesh), good (tid, tri) ]
  in sortBy compare out

classifyParities :: Mesh a -> IM.IntMap Int
classifyParities mesh = bfs initial IM.empty
  where
    seeds = IS.toList $
      IS.unions $
        map (\v -> IM.findWithDefault IS.empty v (meshIncident mesh))
            [meshSuper0 mesh, meshSuper1 mesh, meshSuper2 mesh]
    initial = map (,0) seeds

    bfs [] seen = seen
    bfs ((tid, p):qs) seen =
      case IM.lookup tid seen of
        Just _ -> bfs qs seen
        Nothing ->
          case IM.lookup tid (meshTris mesh) of
            Nothing -> bfs qs seen
            Just tri ->
              let nbrs = neighbors tid tri p
              in bfs (nbrs ++ qs) (IM.insert tid p seen)

    neighbors tid tri p =
      concatMap step (triEdges tri)
      where
        step (u, v) =
          case lookupEdgeTid mesh v u of
            Nothing -> []
            Just ntid
              | ntid == tid -> []
              | otherwise ->
                  let p' = if isConstraintEdge mesh (u, v) then 1 - p else p
                  in [(ntid, p')]

isSuperVertex :: Mesh a -> Int -> Bool
isSuperVertex mesh v = v >= meshOriginalN mesh

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

infixl 1 <|>

(<|>) :: Maybe a -> Maybe a -> Maybe a
(<|>) (Just x) _ = Just x
(<|>) Nothing  y = y

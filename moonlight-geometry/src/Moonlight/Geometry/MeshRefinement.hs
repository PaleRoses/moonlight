{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Moonlight.Geometry.MeshRefinement
  ( refineUniformCDT
  , validateUniformity
  , MeshStats(..)
  ) where

import Control.Monad (foldM, forM_, when)
import Data.Array (listArray, (!))
import Control.Monad.ST (ST, runST)
import Data.Bits ((.|.), shiftL, testBit)
import Data.Kind (Type)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (sortBy)
import Data.STRef (STRef, modifySTRef', newSTRef, readSTRef, writeSTRef)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

-- The user-provided input omitted the current triangle list, but a local
-- refinement algorithm needs it. This is the canonical entry point.
refineUniformCDT
  :: [(Double, Double)]         -- ^ points
  -> [(Int, Int, Int)]          -- ^ current CDT triangles (required)
  -> [(Int, Int)]               -- ^ constrained edges to preserve exactly
  -> (Int -> Bool)              -- ^ locked boundary vertices
  -> Double                     -- ^ target triangle area
  -> ([(Double, Double)], [(Int, Int, Int)])
refineUniformCDT ptsIn trisIn consIn isBoundary !targetA
  | targetA <= 0.0 = error "refineUniformCDT: target area must be > 0"
  | null trisIn    = (ptsIn, [])
  | otherwise      = runST $ do
      let !nP0 = length ptsIn
          !nT0 = length trisIn
          !xs0 = U.fromList (map fst ptsIn)
          !ys0 = U.fromList (map snd ptsIn)
          !totalArea0 = initialTotalArea xs0 ys0 trisIn
          !desiredTris = max nT0 (ceiling (totalArea0 / targetA))
          !maxLiveTris = max (nT0 + max 256 (nT0 `div` 2))
                            (ceiling (1.10 * fromIntegral desiredTris :: Double))
          !newPtsBudget = max 0 ((maxLiveTris - nT0) `div` 2 + 32)
          !pointCap = max (nP0 + newPtsBudget) (nP0 + 64)
          !triCap = max (nT0 * 10) (maxLiveTris * 6)
          !heapCap = max 1024 (triCap * 2)
          !cfg = Config
            { cfgTargetArea = targetA
            , cfgMinArea = 0.40 * targetA
            , cfgMaxArea = 1.50 * targetA
            , cfgGeomEps = 1.0e-12
            , cfgMedianTauCap = 0.30
            , cfgSmoothPasses = 2
            , cfgPointCap = pointCap
            , cfgTriCap = triCap
            , cfgMaxLiveTris = maxLiveTris
            }
          !constrained = buildConstraintSet consIn

      mesh <- newMesh pointCap triCap nP0 nT0
      initPoints mesh xs0 ys0 isBoundary nP0
      initTriangles cfg mesh xs0 ys0 trisIn constrained nT0
      buildAdjacency mesh constrained nT0

      heap <- newHeap heapCap
      rebuildOversizeHeap cfg mesh heap
      refineLoop cfg mesh heap

      -- Cheap constrained smoothing of movable interior vertices.
      smoothPasses cfg mesh

      -- Top up again after smoothing, because moving interior vertices can
      -- re-create a small number of oversize triangles.
      recalcAllAreas mesh
      rebuildOversizeHeap cfg mesh heap
      refineLoop cfg mesh heap

      packResult mesh

validateUniformity
  :: [(Double, Double)]
  -> [(Int, Int, Int)]
  -> Double
  -> MeshStats
validateUniformity pts tris !targetA
  | targetA <= 0.0 = error "validateUniformity: target area must be > 0"
  | null tris = MeshStats 0 0 0 0 0 0
  | otherwise =
      let !pointArray = listArray (0, length pts - 1) pts
          !minA = 0.30 * targetA
          !maxA = 3.00 * targetA
          step (!mn, !mx, !smallN, !largeN, !tot, !cnt) (!a, !b, !c) =
            let !pa = pointArray ! a
                !pb = pointArray ! b
                !pc = pointArray ! c
                !ar = 0.5 * abs (orientFast pa pb pc)
                !smallN' = if ar < minA then smallN + 1 else smallN
                !largeN' = if ar > maxA then largeN + 1 else largeN
            in (min mn ar, max mx ar, smallN', largeN', tot + ar, cnt + 1)
          (!mnF, !mxF, !smallF, !largeF, !totF, !cntF) =
            foldl' step (1.0/0.0, 0.0, 0, 0, 0.0, 0) tris
      in MeshStats
          { msTriangleCount = cntF
          , msMinAreaObserved = mnF
          , msMaxAreaObserved = mxF
          , msBelowMinCount = smallF
          , msAboveMaxCount = largeF
          , msTotalArea = totF
          }

--------------------------------------------------------------------------------
-- Configuration and mutable mesh state
--------------------------------------------------------------------------------

type Point :: Type
type Point = (Double, Double)

type MeshStats :: Type
data MeshStats = MeshStats
  { msTriangleCount    :: !Int
  , msMinAreaObserved  :: !Double
  , msMaxAreaObserved  :: !Double
  , msBelowMinCount    :: !Int
  , msAboveMaxCount    :: !Int
  , msTotalArea        :: !Double
  } deriving stock (Eq, Show)

type Config :: Type
data Config = Config
  { cfgTargetArea   :: !Double
  , cfgMinArea      :: !Double
  , cfgMaxArea      :: !Double
  , cfgGeomEps      :: !Double
  , cfgMedianTauCap :: !Double
  , cfgSmoothPasses :: !Int
  , cfgPointCap     :: !Int
  , cfgTriCap       :: !Int
  , cfgMaxLiveTris  :: !Int
  }

type Mesh :: Type -> Type
data Mesh s = Mesh
  { mPx        :: !(UM.MVector s Double)
  , mPy        :: !(UM.MVector s Double)
  , mPLocked   :: !(UM.MVector s Int)
  , mPointN    :: !(STRef s Int)
  , mTv0       :: !(UM.MVector s Int)
  , mTv1       :: !(UM.MVector s Int)
  , mTv2       :: !(UM.MVector s Int)
  , mTn0       :: !(UM.MVector s Int)
  , mTn1       :: !(UM.MVector s Int)
  , mTn2       :: !(UM.MVector s Int)
  , mTCMask    :: !(UM.MVector s Int)
  , mTAlive    :: !(UM.MVector s Int)
  , mTArea     :: !(UM.MVector s Double)
  , mTriN      :: !(STRef s Int)
  , mLiveTriN  :: !(STRef s Int)
  }

newMesh :: Int -> Int -> Int -> Int -> ST s (Mesh s)
newMesh !pointCap !triCap !nP0 !nT0 = do
  mPx      <- UM.replicate pointCap 0.0
  mPy      <- UM.replicate pointCap 0.0
  mPLocked <- UM.replicate pointCap 0
  mPointN  <- newSTRef nP0

  mTv0    <- UM.replicate triCap (-1)
  mTv1    <- UM.replicate triCap (-1)
  mTv2    <- UM.replicate triCap (-1)
  mTn0    <- UM.replicate triCap (-1)
  mTn1    <- UM.replicate triCap (-1)
  mTn2    <- UM.replicate triCap (-1)
  mTCMask <- UM.replicate triCap 0
  mTAlive <- UM.replicate triCap 0
  mTArea  <- UM.replicate triCap 0.0
  mTriN   <- newSTRef nT0
  mLiveTriN <- newSTRef nT0

  pure Mesh{..}

initPoints :: Mesh s -> U.Vector Double -> U.Vector Double -> (Int -> Bool) -> Int -> ST s ()
initPoints Mesh{..} !xs0 !ys0 isBoundary !nP0 =
  forM_ [0 .. nP0 - 1] $ \i -> do
    UM.unsafeWrite mPx i (U.unsafeIndex xs0 i)
    UM.unsafeWrite mPy i (U.unsafeIndex ys0 i)
    UM.unsafeWrite mPLocked i (if isBoundary i then 1 else 0)

initTriangles
  :: Config
  -> Mesh s
  -> U.Vector Double
  -> U.Vector Double
  -> [(Int, Int, Int)]
  -> IS.IntSet
  -> Int
  -> ST s ()
initTriangles _cfg Mesh{..} !xs0 !ys0 trisIn !_constrained !nT0 =
  forM_ (zip [0 .. nT0 - 1] trisIn) $ \(i, (a0, b0, c0)) -> do
    let !pa = (U.unsafeIndex xs0 a0, U.unsafeIndex ys0 a0)
        !pb = (U.unsafeIndex xs0 b0, U.unsafeIndex ys0 b0)
        !pc = (U.unsafeIndex xs0 c0, U.unsafeIndex ys0 c0)
        !o  = orientFast pa pb pc
        (!a, !b, !c) = if o >= 0.0 then (a0, b0, c0) else (a0, c0, b0)
        !ar = 0.5 * abs o
    UM.unsafeWrite mTv0 i a
    UM.unsafeWrite mTv1 i b
    UM.unsafeWrite mTv2 i c
    UM.unsafeWrite mTn0 i (-1)
    UM.unsafeWrite mTn1 i (-1)
    UM.unsafeWrite mTn2 i (-1)
    UM.unsafeWrite mTCMask i 0
    UM.unsafeWrite mTAlive i 1
    UM.unsafeWrite mTArea i ar

--------------------------------------------------------------------------------
-- Heap for oversize triangles
--------------------------------------------------------------------------------

type MaxHeap :: Type -> Type
data MaxHeap s = MaxHeap
  { hSize :: !(STRef s Int)
  , hKey  :: !(UM.MVector s Double)
  , hVal  :: !(UM.MVector s Int)
  }

newHeap :: Int -> ST s (MaxHeap s)
newHeap !cap = do
  hSize <- newSTRef 0
  hKey  <- UM.replicate cap 0.0
  hVal  <- UM.replicate cap 0
  pure MaxHeap{..}

heapClear :: MaxHeap s -> ST s ()
heapClear MaxHeap{..} = writeSTRef hSize 0

heapPush :: MaxHeap s -> Double -> Int -> ST s ()
heapPush MaxHeap{..} !key !val = do
  !n <- readSTRef hSize
  let go !i =
        if i == 0
          then do
            UM.unsafeWrite hKey 0 key
            UM.unsafeWrite hVal 0 val
          else do
            let !p = (i - 1) `quot` 2
            !pk <- UM.unsafeRead hKey p
            if pk >= key
              then do
                UM.unsafeWrite hKey i key
                UM.unsafeWrite hVal i val
              else do
                !pv <- UM.unsafeRead hVal p
                UM.unsafeWrite hKey i pk
                UM.unsafeWrite hVal i pv
                go p
  go n
  writeSTRef hSize (n + 1)

heapPop :: MaxHeap s -> ST s (Maybe (Double, Int))
heapPop MaxHeap{..} = do
  !n <- readSTRef hSize
  if n == 0
    then pure Nothing
    else do
      !rootK <- UM.unsafeRead hKey 0
      !rootV <- UM.unsafeRead hVal 0
      let !lastIx = n - 1
      !lastK <- UM.unsafeRead hKey lastIx
      !lastV <- UM.unsafeRead hVal lastIx
      writeSTRef hSize lastIx
      when (lastIx > 0) $ do
        let sift !i = do
              let !l = 2 * i + 1
                  !r = l + 1
              if l >= lastIx
                then do
                  UM.unsafeWrite hKey i lastK
                  UM.unsafeWrite hVal i lastV
                else do
                  !lk <- UM.unsafeRead hKey l
                  if r >= lastIx
                    then if lk <= lastK
                           then do
                             UM.unsafeWrite hKey i lastK
                             UM.unsafeWrite hVal i lastV
                           else do
                             !lv <- UM.unsafeRead hVal l
                             UM.unsafeWrite hKey i lk
                             UM.unsafeWrite hVal i lv
                             UM.unsafeWrite hKey l lastK
                             UM.unsafeWrite hVal l lastV
                    else do
                      !rk <- UM.unsafeRead hKey r
                      let !best = if lk >= rk then l else r
                      !bk <- UM.unsafeRead hKey best
                      if bk <= lastK
                        then do
                          UM.unsafeWrite hKey i lastK
                          UM.unsafeWrite hVal i lastV
                        else do
                          !bv <- UM.unsafeRead hVal best
                          UM.unsafeWrite hKey i bk
                          UM.unsafeWrite hVal i bv
                          sift best
        sift 0
      pure (Just (rootK, rootV))

--------------------------------------------------------------------------------
-- Edge/triangle helpers
--------------------------------------------------------------------------------

edgeKey :: Int -> Int -> Int
edgeKey !a !b =
  let !x = min a b
      !y = max a b
  in (x `shiftL` 32) .|. y

buildConstraintSet :: [(Int, Int)] -> IS.IntSet
buildConstraintSet = IS.fromList . map (uncurry edgeKey)

triEdgeBySlot :: (Int, Int, Int) -> Int -> (Int, Int)
triEdgeBySlot (!_, !b, !c) 0 = (b, c)
triEdgeBySlot (!a, !_, !c) 1 = (c, a)
triEdgeBySlot (!a, !b, !_) _ = (a, b)

readPoint :: Mesh s -> Int -> ST s Point
readPoint Mesh{..} !i = do
  !x <- UM.unsafeRead mPx i
  !y <- UM.unsafeRead mPy i
  pure (x, y)

writePoint :: Mesh s -> Int -> Point -> ST s ()
writePoint Mesh{..} !i (!x, !y) = do
  UM.unsafeWrite mPx i x
  UM.unsafeWrite mPy i y

readTriVerts :: Mesh s -> Int -> ST s (Int, Int, Int)
readTriVerts Mesh{..} !t = do
  !a <- UM.unsafeRead mTv0 t
  !b <- UM.unsafeRead mTv1 t
  !c <- UM.unsafeRead mTv2 t
  pure (a, b, c)

readTriArea :: Mesh s -> Int -> ST s Double
readTriArea Mesh{..} !t = UM.unsafeRead mTArea t

writeTriArea :: Mesh s -> Int -> Double -> ST s ()
writeTriArea Mesh{..} !t !a = UM.unsafeWrite mTArea t a

readTriAlive :: Mesh s -> Int -> ST s Int
readTriAlive Mesh{..} !t = UM.unsafeRead mTAlive t

setTriAlive :: Mesh s -> Int -> Int -> ST s ()
setTriAlive Mesh{..} !t !v = UM.unsafeWrite mTAlive t v

readTriMask :: Mesh s -> Int -> ST s Int
readTriMask Mesh{..} !t = UM.unsafeRead mTCMask t

writeTriMask :: Mesh s -> Int -> Int -> ST s ()
writeTriMask Mesh{..} !t !mask = UM.unsafeWrite mTCMask t mask

readNeighbor :: Mesh s -> Int -> Int -> ST s Int
readNeighbor Mesh{..} !t !slot = case slot of
  0 -> UM.unsafeRead mTn0 t
  1 -> UM.unsafeRead mTn1 t
  _ -> UM.unsafeRead mTn2 t

writeNeighbor :: Mesh s -> Int -> Int -> Int -> ST s ()
writeNeighbor Mesh{..} !t !slot !nb = case slot of
  0 -> UM.unsafeWrite mTn0 t nb
  1 -> UM.unsafeWrite mTn1 t nb
  _ -> UM.unsafeWrite mTn2 t nb

setTriVerts :: Mesh s -> Int -> Int -> Int -> Int -> ST s ()
setTriVerts Mesh{..} !t !a !b !c = do
  UM.unsafeWrite mTv0 t a
  UM.unsafeWrite mTv1 t b
  UM.unsafeWrite mTv2 t c

allocPoint :: Mesh s -> ST s Int
allocPoint Mesh{..} = do
  !n <- readSTRef mPointN
  writeSTRef mPointN (n + 1)
  pure n

allocTri :: Mesh s -> ST s Int
allocTri Mesh{..} = do
  !n <- readSTRef mTriN
  writeSTRef mTriN (n + 1)
  pure n

setConstraintBit :: Mesh s -> Int -> Int -> ST s ()
setConstraintBit Mesh{..} !t !slot = do
  !mask <- UM.unsafeRead mTCMask t
  UM.unsafeWrite mTCMask t (mask .|. (1 `shiftL` slot))

constraintAt :: Mesh s -> Int -> Int -> ST s Bool
constraintAt mesh !t !slot = do
  !mask <- readTriMask mesh t
  pure (testBit mask slot)

findSlotByEdge :: Mesh s -> Int -> Int -> Int -> ST s Int
findSlotByEdge mesh !t !u !v = do
  (!a, !b, !c) <- readTriVerts mesh t
  let (!e0u, !e0v) = triEdgeBySlot (a, b, c) 0
      (!e1u, !e1v) = triEdgeBySlot (a, b, c) 1
      (!e2u, !e2v) = triEdgeBySlot (a, b, c) 2
      same !x0 !y0 = (x0 == u && y0 == v) || (x0 == v && y0 == u)
  pure $ if same e0u e0v then 0 else if same e1u e1v then 1 else if same e2u e2v then 2 else (-1)

buildAdjacency :: Mesh s -> IS.IntSet -> Int -> ST s ()
buildAdjacency mesh !constrained !nT0 = go 0 IM.empty
  where
    go !t !pending
      | t >= nT0 = pure ()
      | otherwise = do
          (!a, !b, !c) <- readTriVerts mesh t
          let !tri = (a, b, c)
          pending1 <- foldEdge t tri 0 pending
          pending2 <- foldEdge t tri 1 pending1
          pending3 <- foldEdge t tri 2 pending2
          go (t + 1) pending3

    foldEdge !t !tri !slot !pending = do
      let (!u, !v) = triEdgeBySlot tri slot
          !k = edgeKey u v
      when (IS.member k constrained) $ setConstraintBit mesh t slot
      case IM.lookup k pending of
        Nothing -> pure (IM.insert k (t, slot) pending)
        Just (!ot, !os) -> do
          writeNeighbor mesh t slot ot
          writeNeighbor mesh ot os t
          pure (IM.delete k pending)

recalcAllAreas :: Mesh s -> ST s ()
recalcAllAreas mesh@Mesh{..} = do
  !n <- readSTRef mTriN
  forM_ [0 .. n - 1] $ \t -> do
    !alive <- UM.unsafeRead mTAlive t
    when (alive /= 0) $ do
      (!a, !b, !c) <- readTriVerts mesh t
      !pa <- readPoint mesh a
      !pb <- readPoint mesh b
      !pc <- readPoint mesh c
      UM.unsafeWrite mTArea t (0.5 * abs (orientFast pa pb pc))

rebuildOversizeHeap :: Config -> Mesh s -> MaxHeap s -> ST s ()
rebuildOversizeHeap Config{..} Mesh{..} heap = do
  heapClear heap
  !n <- readSTRef mTriN
  forM_ [0 .. n - 1] $ \t -> do
    !alive <- UM.unsafeRead mTAlive t
    when (alive /= 0) $ do
      !a <- UM.unsafeRead mTArea t
      when (a > cfgMaxArea) $ heapPush heap a t

--------------------------------------------------------------------------------
-- Geometry predicates and metrics
--------------------------------------------------------------------------------

orientFast :: Point -> Point -> Point -> Double
orientFast (!ax, !ay) (!bx, !by) (!cx, !cy) =
  (ax - cx) * (by - cy) - (ay - cy) * (bx - cx)

orientSign :: Point -> Point -> Point -> Ordering
orientSign (!ax, !ay) (!bx, !by) (!cx, !cy) =
  let !acx = ax - cx
      !acy = ay - cy
      !bcx = bx - cx
      !bcy = by - cy
      !detleft  = acx * bcy
      !detright = acy * bcx
      !det      = detleft - detright
      !detsum
        | detleft > 0.0 && detright <= 0.0 = abs det
        | detleft < 0.0 && detright >= 0.0 = abs det
        | otherwise = abs detleft + abs detright
      !errbound = 3.3306690738754716e-16 * detsum
  in if abs det >= errbound
       then compare det 0.0
       else compare (exactOrient ax ay bx by cx cy) 0

exactOrient :: Double -> Double -> Double -> Double -> Double -> Double -> Rational
exactOrient !ax !ay !bx !by !cx !cy =
  let !axr = toRational ax
      !ayr = toRational ay
      !bxr = toRational bx
      !byr = toRational by
      !cxr = toRational cx
      !cyr = toRational cy
  in (axr - cxr) * (byr - cyr) - (ayr - cyr) * (bxr - cxr)

inCircleSign :: Point -> Point -> Point -> Point -> Ordering
inCircleSign (!ax, !ay) (!bx, !by) (!cx, !cy) (!dx, !dy) =
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
      !det = alift * bcdet + blift * cadet + clift * abdet
      !permanent =
            (abs (bdx * cdy) + abs (cdx * bdy)) * alift
          + (abs (cdx * ady) + abs (adx * cdy)) * blift
          + (abs (adx * bdy) + abs (bdx * ady)) * clift
      !errbound = 1.1102230246251577e-15 * permanent
  in if abs det > errbound
       then compare det 0.0
       else compare (exactInCircle ax ay bx by cx cy dx dy) 0

exactInCircle
  :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Rational
exactInCircle !ax !ay !bx !by !cx !cy !dx !dy =
  let !adx = toRational ax - toRational dx
      !ady = toRational ay - toRational dy
      !bdx = toRational bx - toRational dx
      !bdy = toRational by - toRational dy
      !cdx = toRational cx - toRational dx
      !cdy = toRational cy - toRational dy
      !abdet = adx * bdy - bdx * ady
      !bcdet = bdx * cdy - cdx * bdy
      !cadet = cdx * ady - adx * cdy
      !alift = adx * adx + ady * ady
      !blift = bdx * bdx + bdy * bdy
      !clift = cdx * cdx + cdy * cdy
  in alift * bcdet + blift * cadet + clift * abdet

dist2 :: Point -> Point -> Double
dist2 (!x0, !y0) (!x1, !y1) =
  let !dx = x1 - x0
      !dy = y1 - y0
  in dx * dx + dy * dy

triQuality :: Point -> Point -> Point -> Double
triQuality !pa !pb !pc =
  let !area2 = abs (orientFast pa pb pc)
      !s2 = dist2 pa pb + dist2 pb pc + dist2 pc pa
  in if area2 <= 0.0 || s2 <= 0.0
       then 0.0
       else (2.0 * sqrt 3.0 * area2) / s2

pointInTriangleApprox :: Double -> Point -> Point -> Point -> Point -> Bool
pointInTriangleApprox !eps !p !a !b !c =
  let !o1 = orientFast a b p
      !o2 = orientFast b c p
      !o3 = orientFast c a p
  in o1 >= (-eps) && o2 >= (-eps) && o3 >= (-eps)

circumcenter :: Point -> Point -> Point -> Maybe Point
circumcenter (!ax, !ay) (!bx, !by) (!cx, !cy) =
  let !d = 2.0 * (ax * (by - cy) + bx * (cy - ay) + cx * (ay - by))
  in if abs d <= 1.0e-30
       then Nothing
       else
         let !a2 = ax * ax + ay * ay
             !b2 = bx * bx + by * by
             !c2 = cx * cx + cy * cy
             !ux = (a2 * (by - cy) + b2 * (cy - ay) + c2 * (ay - by)) / d
             !uy = (a2 * (cx - bx) + b2 * (ax - cx) + c2 * (bx - ax)) / d
         in Just (ux, uy)

--------------------------------------------------------------------------------
-- Candidate generation for refinement
--------------------------------------------------------------------------------

type Candidate :: Type
data Candidate = Candidate
  { candP      :: !Point
  , candMinQ   :: !Double
  , candMaxDev :: !Double
  }

chooseCandidates :: Config -> Mesh s -> Int -> ST s [Point]
chooseCandidates Config{..} mesh !t = do
  (!a, !b, !c) <- readTriVerts mesh t
  !pa <- readPoint mesh a
  !pb <- readPoint mesh b
  !pc <- readPoint mesh c
  !ar <- readTriArea mesh t
  let !tau = min cfgMedianTauCap (2.0 * cfgTargetArea / ar)
      medianCand (!u, !v, !w) =
        let !(ux, uy) = u
            !(vx, vy) = v
            !(wx, wy) = w
            !mx = 0.5 * (ux + vx)
            !my = 0.5 * (uy + vy)
        in (mx + tau * (wx - mx), my + tau * (wy - my))
      cands0 =
        [ medianCand (pa, pb, pc)
        , medianCand (pb, pc, pa)
        , medianCand (pc, pa, pb)
        ]
      epsInside = cfgGeomEps * max 1.0 (abs (orientFast pa pb pc))
      cands1 = case circumcenter pa pb pc of
        Nothing -> cands0
        Just cc -> if pointInTriangleApprox epsInside cc pa pb pc then cc : cands0 else cands0
      scored = map (scoreCandidate cfgTargetArea pa pb pc) cands1
      sorted = map candP $ sortBy betterCandidate scored
  pure sorted

scoreCandidate :: Double -> Point -> Point -> Point -> Point -> Candidate
scoreCandidate !targetA !pa !pb !pc !p =
  let !a1 = 0.5 * abs (orientFast pa pb p)
      !a2 = 0.5 * abs (orientFast pb pc p)
      !a3 = 0.5 * abs (orientFast pc pa p)
      !q1 = triQuality pa pb p
      !q2 = triQuality pb pc p
      !q3 = triQuality pc pa p
      !minQ = min q1 (min q2 q3)
      !maxDev = maximum
        [ abs (a1 / targetA - 1.0)
        , abs (a2 / targetA - 1.0)
        , abs (a3 / targetA - 1.0)
        ]
  in Candidate p minQ maxDev

betterCandidate :: Candidate -> Candidate -> Ordering
betterCandidate !c0 !c1 =
  compare (candMinQ c1 - 0.08 * candMaxDev c1)
          (candMinQ c0 - 0.08 * candMaxDev c0)

--------------------------------------------------------------------------------
-- Local cavity insertion (Bowyer-Watson over unconstrained cavity only)
--------------------------------------------------------------------------------

type BoundaryEdge :: Type
data BoundaryEdge = BoundaryEdge !Int !Int !Int !Bool

pointInCircTri :: Mesh s -> Int -> Point -> ST s Bool
pointInCircTri mesh !t !p = do
  (!a, !b, !c) <- readTriVerts mesh t
  !pa <- readPoint mesh a
  !pb <- readPoint mesh b
  !pc <- readPoint mesh c
  let !ordABC = orientSign pa pb pc
  pure $ case ordABC of
    GT -> inCircleSign pa pb pc p == GT
    LT -> inCircleSign pa pc pb p == GT
    EQ -> False

buildCavity
  :: Mesh s
  -> Int
  -> Point
  -> ST s (Maybe ([Int], [BoundaryEdge]))
buildCavity mesh !seed !p = go IS.empty [] [] [seed]
  where
    go !_seen !cavity !boundary [] = pure (Just (reverse cavity, reverse boundary))
    go !seen !cavity !boundary (t:ts)
      | IS.member t seen = go seen cavity boundary ts
      | otherwise = do
          let !seen' = IS.insert t seen
          !alive <- readTriAlive mesh t
          if alive == 0
            then pure Nothing
            else do
              !inside <- pointInCircTri mesh t p
              if not inside
                then go seen' cavity boundary ts
                else do
                  (!a, !b, !c) <- readTriVerts mesh t
                  let !tri = (a, b, c)
                  (ts', boundary') <- walkEdges seen' ts boundary t tri 0
                  go seen' (t : cavity) boundary' ts'

    walkEdges !_seen !ts !boundary !_t !_tri 3 = pure (ts, boundary)
    walkEdges !seen !ts !boundary !t !tri !slot = do
      let (!u, !v) = triEdgeBySlot tri slot
      !blocked <- constraintAt mesh t slot
      !nb <- readNeighbor mesh t slot
      if blocked || nb < 0
        then walkEdges seen ts (BoundaryEdge u v nb blocked : boundary) t tri (slot + 1)
        else do
          !nbAlive <- readTriAlive mesh nb
          if nbAlive == 0
            then walkEdges seen ts (BoundaryEdge u v (-1) False : boundary) t tri (slot + 1)
            else do
              !nbInside <- pointInCircTri mesh nb p
              if nbInside
                then let !ts' = if IS.member nb seen then ts else nb : ts
                     in walkEdges seen ts' boundary t tri (slot + 1)
                else walkEdges seen ts (BoundaryEdge u v nb False : boundary) t tri (slot + 1)

orderBoundary :: [BoundaryEdge] -> Maybe [BoundaryEdge]
orderBoundary [] = Nothing
orderBoundary bes@(e0 : _) =
  let boundaryStart (BoundaryEdge a _ _ _) = a
      boundaryEnd (BoundaryEdge _ b _ _) = b
      ins !m !e = case IM.lookup (boundaryStart e) m of
        Nothing -> IM.insert (boundaryStart e) e m
        Just _  -> IM.empty
      !startMap = foldl' ins IM.empty bes
      !n = length bes
  in if IM.null startMap || IM.size startMap /= n
       then Nothing
       else
         let go !k !cur !acc
               | k == n =
                   if boundaryEnd cur == boundaryStart e0 then Just (reverse (cur : acc)) else Nothing
               | otherwise =
                   case IM.lookup (boundaryEnd cur) startMap of
                     Nothing   -> Nothing
                     Just next -> go (k + 1) next (cur : acc)
         in go 1 e0 []

insertPointIntoMesh
  :: Config
  -> Mesh s
  -> MaxHeap s
  -> Int
  -> Point
  -> ST s Bool
insertPointIntoMesh Config{..} mesh@Mesh{..} heap !seed !p = do
  cavityRes <- buildCavity mesh seed p
  case cavityRes of
    Nothing -> pure False
    Just (cavity, boundary0) ->
      case orderBoundary boundary0 of
        Nothing -> pure False
        Just boundary -> do
          let !k = length boundary
          if k < 3
            then pure False
            else do
              -- Validate orientation before committing.
              good <- allPositive boundary
              if not good
                then pure False
                else do
                  !liveN <- readSTRef mLiveTriN
                  !curPN <- readSTRef mPointN
                  !curTN <- readSTRef mTriN
                  if liveN + 2 > cfgMaxLiveTris || curPN >= cfgPointCap || curTN + k > cfgTriCap
                    then pure False
                    else do
                      !newP <- allocPoint mesh
                      writePoint mesh newP p
                      UM.unsafeWrite mPLocked newP 0

                      newTs <- createNewTriangles newP boundary

                      let !tsLen = length newTs
                          !newTriangleArray = listArray (0, tsLen - 1) newTs
                          ringAt !i = newTriangleArray ! (i `mod` tsLen)
                      forM_ [0 .. tsLen - 1] $ \i -> do
                        let !ti   = ringAt i
                            !tPrev = ringAt (i - 1 + tsLen)
                            !tNext = ringAt (i + 1)
                        writeNeighbor mesh ti 0 tNext
                        writeNeighbor mesh ti 1 tPrev

                      -- Patch outside neighbors.
                      patchOutside newTs boundary

                      -- Retire cavity triangles.
                      forM_ cavity $ \t -> setTriAlive mesh t 0
                      modifySTRef' mLiveTriN (+ (k - length cavity))

                      -- Push any new oversize triangles.
                      forM_ newTs $ \t -> do
                        !ar <- readTriArea mesh t
                        when (ar > cfgMaxArea) $ heapPush heap ar t

                      pure True
  where
    allPositive = goPos
      where
        goPos [] = pure True
        goPos (BoundaryEdge u v _ _ : es) = do
          !pu <- readPoint mesh u
          !pv <- readPoint mesh v
          let !o = orientFast pu pv p
          if o <= cfgGeomEps
            then pure False
            else goPos es

    createNewTriangles !newP = mapM mkTri
      where
        mkTri (BoundaryEdge u v outsideC constrainedE) = do
          !t <- allocTri mesh
          setTriVerts mesh t u v newP
          writeNeighbor mesh t 0 (-1)
          writeNeighbor mesh t 1 (-1)
          writeNeighbor mesh t 2 outsideC
          let !mask = if constrainedE then 4 else 0
          writeTriMask mesh t mask
          setTriAlive mesh t 1
          !pu <- readPoint mesh u
          !pv <- readPoint mesh v
          let !ar = 0.5 * abs (orientFast pu pv p)
          writeTriArea mesh t ar
          pure t

    patchOutside !newTs !boundary = goPatch newTs boundary
      where
        goPatch [] [] = pure ()
        goPatch (t:ts) (BoundaryEdge u v outsideC _ : es)
          | outsideC < 0 = goPatch ts es
          | otherwise = do
              !slot <- findSlotByEdge mesh outsideC u v
              when (slot >= 0) $ writeNeighbor mesh outsideC slot t
              goPatch ts es
        goPatch _ _ = pure ()

--------------------------------------------------------------------------------
-- Refinement loop
--------------------------------------------------------------------------------

refineLoop :: Config -> Mesh s -> MaxHeap s -> ST s ()
refineLoop cfg@Config{..} mesh@Mesh{..} heap = go
  where
    go = do
      !liveN <- readSTRef mLiveTriN
      if liveN >= cfgMaxLiveTris
        then pure ()
        else do
          mp <- heapPop heap
          case mp of
            Nothing -> pure ()
            Just (_prio, t) -> do
              !alive <- readTriAlive mesh t
              if alive == 0
                then go
                else do
                  !ar <- readTriArea mesh t
                  if ar <= cfgMaxArea
                    then go
                    else do
                      cands <- chooseCandidates cfg mesh t
                      ok <- tryCandidates t cands
                      if ok then go else go

    tryCandidates !_t [] = pure False
    tryCandidates !t (p:ps) = do
      ok <- insertPointIntoMesh cfg mesh heap t p
      if ok then pure True else tryCandidates t ps

--------------------------------------------------------------------------------
-- Vertex smoothing on fixed connectivity (boundary vertices locked)
--------------------------------------------------------------------------------

smoothPasses :: Config -> Mesh s -> ST s ()
smoothPasses cfg@Config{..} mesh = loop 0
  where
    loop !k
      | k >= cfgSmoothPasses = pure ()
      | otherwise = do
          smoothOnce cfg mesh
          recalcAllAreas mesh
          loop (k + 1)

smoothOnce :: forall s. Config -> Mesh s -> ST s ()
smoothOnce cfg@Config{..} mesh@Mesh{..} = do
  (!offsets, !triRefs) <- buildIncidentCSR mesh
  !nP <- readSTRef mPointN
  forM_ [0 .. nP - 1] $ \v -> do
    !locked <- UM.unsafeRead mPLocked v
    when (locked == 0) $ do
      let !start = U.unsafeIndex offsets v
          !end   = U.unsafeIndex offsets (v + 1)
      when (end - start >= 3) $ do
        mn <- orderedOneRing mesh v triRefs start end
        case mn of
          Nothing -> pure ()
          Just ring -> do
            !oldP <- readPoint mesh v
            !cand0 <- patchCentroidST mesh oldP ring
            let !cand1 = lerpPoint 0.75 oldP cand0
            !before <- patchScoreST mesh oldP ring cfgTargetArea
            tryMove v 0 oldP cand1 before ring
  where
    tryMove :: Int -> Int -> Point -> Point -> Double -> [Int] -> ST s ()
    tryMove !v !iter !oldP !cand !before !ring
      | iter >= 8 = pure ()
      | otherwise = do
          okKernel <- feasibleInKernelST cfg mesh cand ring
          if not okKernel
            then tryMove v (iter + 1) oldP (midPoint oldP cand) before ring
            else do
              !after <- patchScoreST mesh cand ring cfgTargetArea
              !minAreaCand <- patchMinAreaST mesh cand ring
              if after + 1.0e-12 < before && minAreaCand > 0.0
                then writePoint mesh v cand
                else tryMove v (iter + 1) oldP (midPoint oldP cand) before ring

ringEdges :: [Int] -> [(Int, Int)]
ringEdges [] = []
ringEdges (firstVertex : restVertices) = go firstVertex firstVertex restVertices
  where
    go :: Int -> Int -> [Int] -> [(Int, Int)]
    go startVertex previousVertex [] = [(previousVertex, startVertex)]
    go startVertex previousVertex (nextVertex : remainingVertices) =
      (previousVertex, nextVertex) : go startVertex nextVertex remainingVertices

patchCentroidST :: Mesh s -> Point -> [Int] -> ST s Point
patchCentroidST mesh !oldP !ring = do
  (!sx, !sy, !sw) <- foldM step (0.0, 0.0, 0.0) (ringEdges ring)
  pure $ if sw <= 0.0 then oldP else (sx / sw, sy / sw)
  where
    step (!sx, !sy, !sw) (!u, !v) = do
      !pu <- readPoint mesh u
      !pv <- readPoint mesh v
      let !ar = 0.5 * abs (orientFast oldP pu pv)
          !cx = (fst oldP + fst pu + fst pv) / 3.0
          !cy = (snd oldP + snd pu + snd pv) / 3.0
      pure (sx + ar * cx, sy + ar * cy, sw + ar)

patchScoreST :: Mesh s -> Point -> [Int] -> Double -> ST s Double
patchScoreST mesh !p !ring !targetA = do
  (!mxDev, !sumDev, !minQ, !cnt) <- foldM step (0.0, 0.0, 1.0 / 0.0, 0 :: Int) (ringEdges ring)
  let !meanDev = if cnt == 0 then 0.0 else sumDev / fromIntegral cnt
  pure (mxDev + 0.35 * meanDev + 0.15 * (1.0 - minQ))
  where
    step (!mxDev, !sumDev, !minQ, !cnt) (!u, !v) = do
      !pu <- readPoint mesh u
      !pv <- readPoint mesh v
      let !ar = 0.5 * orientFast pu pv p
          !dev = abs (ar / targetA - 1.0)
          !q = triQuality pu pv p
      pure (max mxDev dev, sumDev + dev, min minQ q, cnt + 1)

patchMinAreaST :: Mesh s -> Point -> [Int] -> ST s Double
patchMinAreaST mesh !p !ring = foldM step (1.0 / 0.0) (ringEdges ring)
  where
    step !mn (!u, !v) = do
      !pu <- readPoint mesh u
      !pv <- readPoint mesh v
      let !ar = 0.5 * orientFast pu pv p
      pure (min mn ar)

feasibleInKernelST :: Config -> Mesh s -> Point -> [Int] -> ST s Bool
feasibleInKernelST Config{..} mesh !p !ring = go (ringEdges ring)
  where
    go [] = pure True
    go ((!u, !v) : es) = do
      !pu <- readPoint mesh u
      !pv <- readPoint mesh v
      if orientFast pu pv p > cfgGeomEps
        then go es
        else pure False

orderedOneRing
  :: Mesh s
  -> Int
  -> U.Vector Int
  -> Int
  -> Int
  -> ST s (Maybe [Int])
orderedOneRing mesh !v !triRefs !start !end = do
  let !deg = end - start
  pairs <- collectPairs [] start
  let insNext :: (Bool, IM.IntMap Int) -> (Int, Int) -> (Bool, IM.IntMap Int)
      insNext (!ok, !m) (!u, !w) =
        case IM.lookup u m of
          Nothing -> (ok, IM.insert u w m)
          Just _  -> (False, m)
      (!okMap, !nextMap) = foldl' insNext (True, IM.empty) pairs
  if not okMap || IM.size nextMap /= deg || null pairs
    then pure Nothing
    else do
      case pairs of
        [] -> pure Nothing
        (startN, _) : _ ->
          let unfold !k !cur !acc
                | k == deg = if cur == startN then Just (reverse acc) else Nothing
                | otherwise = case IM.lookup cur nextMap of
                    Nothing   -> Nothing
                    Just next -> unfold (k + 1) next (cur : acc)
           in pure (unfold 0 startN [])
  where
    collectPairs !acc !i
      | i >= end = pure (reverse acc)
      | otherwise = do
          let !t = U.unsafeIndex triRefs i
          (!a, !b, !c) <- readTriVerts mesh t
          let !pair
                | v == a    = (b, c)
                | v == b    = (c, a)
                | otherwise = (a, b)
          collectPairs (pair : acc) (i + 1)

buildIncidentCSR :: forall s. Mesh s -> ST s (U.Vector Int, U.Vector Int)
buildIncidentCSR mesh@Mesh{..} = do
  !nP <- readSTRef mPointN
  !nT <- readSTRef mTriN
  counts <- UM.replicate nP (0 :: Int)
  forM_ [0 .. nT - 1] $ \t -> do
    !alive <- UM.unsafeRead mTAlive t
    when (alive /= 0) $ do
      (!a, !b, !c) <- readTriVerts mesh t
      bump counts a
      bump counts b
      bump counts c

  offs <- UM.replicate (nP + 1) (0 :: Int)
  let prefix !i !acc
        | i >= nP = UM.unsafeWrite offs nP acc
        | otherwise = do
            UM.unsafeWrite offs i acc
            !c <- UM.unsafeRead counts i
            prefix (i + 1) (acc + c)
  prefix 0 0

  fill <- UM.unsafeNew nP
  forM_ [0 .. nP - 1] $ \i -> do
    !o <- UM.unsafeRead offs i
    UM.unsafeWrite fill i o

  let !refCap = 3 * max 1 nT
  triRefsM <- UM.replicate refCap (-1)
  forM_ [0 .. nT - 1] $ \t -> do
    !alive <- UM.unsafeRead mTAlive t
    when (alive /= 0) $ do
      (!a, !b, !c) <- readTriVerts mesh t
      pushRef fill triRefsM a t
      pushRef fill triRefsM b t
      pushRef fill triRefsM c t

  offsF <- U.unsafeFreeze offs
  triRefsF <- U.unsafeFreeze triRefsM
  pure (offsF, triRefsF)
  where
    bump :: UM.MVector s Int -> Int -> ST s ()
    bump !mv !i = do
      !x <- UM.unsafeRead mv i
      UM.unsafeWrite mv i (x + 1)
    pushRef :: UM.MVector s Int -> UM.MVector s Int -> Int -> Int -> ST s ()
    pushRef !fill !triRefsM !v !t = do
      !i <- UM.unsafeRead fill v
      UM.unsafeWrite triRefsM i t
      UM.unsafeWrite fill v (i + 1)

--------------------------------------------------------------------------------
-- Output packing
--------------------------------------------------------------------------------

packResult :: Mesh s -> ST s ([(Double, Double)], [(Int, Int, Int)])
packResult mesh@Mesh{..} = do
  !nP <- readSTRef mPointN
  !nT <- readSTRef mTriN
  pts <- collectPts 0 nP []
  tris <- collectTris 0 nT []
  pure (reverse pts, reverse tris)
  where
    collectPts !i !n !acc
      | i >= n = pure acc
      | otherwise = do
          !p <- readPoint mesh i
          collectPts (i + 1) n (p : acc)

    collectTris !i !n !acc
      | i >= n = pure acc
      | otherwise = do
          !alive <- UM.unsafeRead mTAlive i
          if alive == 0
            then collectTris (i + 1) n acc
            else do
              !tri@(!a, !b, !c) <- readTriVerts mesh i
              if a < 0 || b < 0 || c < 0
                then collectTris (i + 1) n acc
                else collectTris (i + 1) n (tri : acc)

--------------------------------------------------------------------------------
-- Misc pure helpers
--------------------------------------------------------------------------------

initialTotalArea :: U.Vector Double -> U.Vector Double -> [(Int, Int, Int)] -> Double
initialTotalArea !xs !ys = foldl' step 0.0
  where
    step !acc (!a, !b, !c) =
      let !pa = (U.unsafeIndex xs a, U.unsafeIndex ys a)
          !pb = (U.unsafeIndex xs b, U.unsafeIndex ys b)
          !pc = (U.unsafeIndex xs c, U.unsafeIndex ys c)
      in acc + 0.5 * abs (orientFast pa pb pc)

lerpPoint :: Double -> Point -> Point -> Point
lerpPoint !alpha (!x0, !y0) (!x1, !y1) =
  let !beta = 1.0 - alpha
  in (beta * x0 + alpha * x1, beta * y0 + alpha * y1)

midPoint :: Point -> Point -> Point
midPoint (!x0, !y0) (!x1, !y1) = (0.5 * (x0 + x1), 0.5 * (y0 + y1))

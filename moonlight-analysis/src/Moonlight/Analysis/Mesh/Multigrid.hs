{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Moonlight.Analysis.Mesh.Multigrid
  ( MGTransfer(..)
  , MGHierarchy(..)
  , buildMGHierarchy
  , restrictScalarInto
  , prolongScalarAddInto
  ) where

import Data.Kind (Type)
import Control.Monad.ST (ST, runST)
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.Word (Word8, Word64)
import qualified Data.Vector.Algorithms.Intro as Intro
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

import Moonlight.Core (clampUnitInterval)
import Moonlight.Analysis.Mesh.Graph
  ( Graph(..)
  , edgeRange
  )

type MGTransfer :: Type
data MGTransfer = MGTransfer
  { mgtFineRows             :: !Int
  , mgtCoarseRows           :: !Int
  , mgtFineToCoarse         :: !(VU.Vector Int)
  , mgtFineRestrictW        :: !(VU.Vector Float)
  , mgtCoarseOffsets        :: !(VU.Vector Int)
  , mgtCoarseFaces          :: !(VU.Vector Int)
  , mgtFinePairToCoarsePair :: !(VU.Vector Int)
  }

type MGHierarchy :: Type
data MGHierarchy = MGHierarchy
  { mghGraph    :: !Graph
  , mghMass     :: !(VU.Vector Double)
  , mghTransfer :: !(Maybe MGTransfer)
  , mghCoarse   :: !(Maybe MGHierarchy)
  }

type CoarsenResult :: Type
data CoarsenResult = CoarsenResult !MGTransfer !Graph !(VU.Vector Double)

maxAggregateSize :: Int
maxAggregateSize = 4
{-# INLINE maxAggregateSize #-}

minCoarsenableRows :: Int
minCoarsenableRows = 16
{-# INLINE minCoarsenableRows #-}

packPairKey :: Int -> Int -> Word64
packPairKey !a !b =
  let !lo = min a b
      !hi = max a b
  in (fromIntegral lo `shiftL` 32) .|. fromIntegral hi
{-# INLINE packPairKey #-}

unpackPairKey :: Word64 -> (Int, Int)
unpackPairKey !k =
  ( fromIntegral (k `shiftR` 32)
  , fromIntegral (k .&. 0xffffffff)
  )
{-# INLINE unpackPairKey #-}

identityPerm :: Int -> VU.Vector Int
identityPerm !n = VU.generate n id
{-# INLINE identityPerm #-}

buildMGHierarchy :: Int -> VU.Vector Double -> Graph -> Maybe MGHierarchy
buildMGHierarchy !levels !faceMass !gr
  | VU.length faceMass /= grFaces gr = Nothing
  | otherwise = Just (go levels faceMass gr)
  where
    go :: Int -> VU.Vector Double -> Graph -> MGHierarchy
    go !lvl !mass !g =
      case if lvl <= 0 then Nothing else coarsenOnce mass g of
        Nothing ->
          MGHierarchy
            { mghGraph = g
            , mghMass = mass
            , mghTransfer = Nothing
            , mghCoarse = Nothing
            }
        Just (CoarsenResult tr cg cm) ->
          MGHierarchy
            { mghGraph = g
            , mghMass = mass
            , mghTransfer = Just tr
            , mghCoarse = Just (go (lvl - 1) cm cg)
            }

coarsenOnce :: VU.Vector Double -> Graph -> Maybe CoarsenResult
coarsenOnce !faceMass !gr
  | VU.length faceMass /= grFaces gr = Nothing
  | grFaces gr < minCoarsenableRows = Nothing
  | VU.null (grPairA gr) = Nothing
  | otherwise = runST $ do
      let !rows = grFaces gr
          !pairCount = VU.length (grPairA gr)

          seedScore =
            VU.generate rows $ \i ->
              let (!lo, !hi) = edgeRange gr i
                  go !e !acc
                    | e == hi = acc
                    | otherwise =
                        let !p = VU.unsafeIndex (grEdgePair gr) e
                            !w = VU.unsafeIndex (grPairBaseW gr) p
                        in go (e + 1) (acc + w)
              in go lo 0.0

      fineToCoarseM <- VUM.replicate rows (-1 :: Int)
      seedOrderM <- VUM.unsafeNew rows
      let initSeeds !i
            | i == rows = pure ()
            | otherwise = VUM.unsafeWrite seedOrderM i i >> initSeeds (i + 1)
      initSeeds 0
      Intro.sortBy
        (\i j ->
            case compare (VU.unsafeIndex seedScore j) (VU.unsafeIndex seedScore i) of
              EQ -> compare i j
              ord -> ord)
        seedOrderM

      let growAggregate !aggId !seed = do
            members <- VUM.unsafeNew maxAggregateSize
            VUM.unsafeWrite members 0 seed
            VUM.unsafeWrite fineToCoarseM seed aggId

            let bestBoundary !size = memberLoop 0 (-1) (-1.0 :: Double)
                  where
                    memberLoop !mi !bestJ !bestW
                      | mi == size = pure bestJ
                      | otherwise = do
                          i <- VUM.unsafeRead members mi
                          let (!lo, !hi) = edgeRange gr i
                              edgeLoop !e !candJ !candW
                                | e == hi = pure (candJ, candW)
                                | otherwise = do
                                    let !j = VU.unsafeIndex (grNbrs gr) e
                                    owner <- VUM.unsafeRead fineToCoarseM j
                                    if owner >= 0
                                      then edgeLoop (e + 1) candJ candW
                                      else do
                                        let !p = VU.unsafeIndex (grEdgePair gr) e
                                            !w = VU.unsafeIndex (grPairBaseW gr) p
                                        if w > candW || (w == candW && (candJ < 0 || j < candJ))
                                          then edgeLoop (e + 1) j w
                                          else edgeLoop (e + 1) candJ candW
                          (!j1, !w1) <- edgeLoop lo bestJ bestW
                          memberLoop (mi + 1) j1 w1

                grow !size
                  | size >= maxAggregateSize = pure size
                  | otherwise = do
                      j <- bestBoundary size
                      if j < 0
                        then pure size
                        else do
                          VUM.unsafeWrite members size j
                          VUM.unsafeWrite fineToCoarseM j aggId
                          grow (size + 1)

            grow 1

          seedLoop !k !aggCount
            | k == rows = pure aggCount
            | otherwise = do
                seed <- VUM.unsafeRead seedOrderM k
                owner <- VUM.unsafeRead fineToCoarseM seed
                if owner >= 0
                  then seedLoop (k + 1) aggCount
                  else do
                    _ <- growAggregate aggCount seed
                    seedLoop (k + 1) (aggCount + 1)

      coarseRows <- seedLoop 0 0
      if coarseRows <= 1 || coarseRows >= rows
        then pure Nothing
        else do
          aggMassM <- VUM.replicate coarseRows (0.0 :: Double)

          let faceLoop !i
                | i == rows = pure ()
                | otherwise = do
                    c <- VUM.unsafeRead fineToCoarseM i
                    let !m = max 1.0e-12 (VU.unsafeIndex faceMass i)
                    oldM <- VUM.unsafeRead aggMassM c
                    VUM.unsafeWrite aggMassM c (oldM + m)
                    faceLoop (i + 1)

          faceLoop 0

          coarseMass <- VU.unsafeFreeze aggMassM
          fineToCoarse <- VU.unsafeFreeze fineToCoarseM

          childCountsM <- VUM.replicate coarseRows (0 :: Int)
          let countChildren !i
                | i == rows = pure ()
                | otherwise = do
                    let !c = VU.unsafeIndex fineToCoarse i
                    n <- VUM.unsafeRead childCountsM c
                    VUM.unsafeWrite childCountsM c (n + 1)
                    countChildren (i + 1)
          countChildren 0

          coarseOffsetsM <- VUM.unsafeNew (coarseRows + 1)
          let prefixChildren !c !acc
                | c == coarseRows = VUM.unsafeWrite coarseOffsetsM coarseRows acc
                | otherwise = do
                    VUM.unsafeWrite coarseOffsetsM c acc
                    n <- VUM.unsafeRead childCountsM c
                    prefixChildren (c + 1) (acc + n)
          prefixChildren 0 0

          coarseOffsets <- VU.unsafeFreeze coarseOffsetsM
          childPosM <- VU.thaw (VU.init coarseOffsets)
          coarseFacesM <- VUM.unsafeNew rows
          let fillChildren !i
                | i == rows = pure ()
                | otherwise = do
                    let !c = VU.unsafeIndex fineToCoarse i
                    pos <- VUM.unsafeRead childPosM c
                    VUM.unsafeWrite coarseFacesM pos i
                    VUM.unsafeWrite childPosM c (pos + 1)
                    fillChildren (i + 1)
          fillChildren 0
          coarseFaces <- VU.unsafeFreeze coarseFacesM

          let fineRestrictW =
                VU.generate rows $ \i ->
                  let !c = VU.unsafeIndex fineToCoarse i
                      !m = max 1.0e-12 (VU.unsafeIndex faceMass i)
                      !mc = max 1.0e-12 (VU.unsafeIndex coarseMass c)
                  in realToFrac (m / mc)

          pairKeyOrigM <- VUM.unsafeNew pairCount :: ST s (VUM.MVector s (Word64, Int))
          let initPairKeys !p
                | p == pairCount = pure ()
                | otherwise = do
                    let !a = VU.unsafeIndex fineToCoarse (VU.unsafeIndex (grPairA gr) p)
                        !b = VU.unsafeIndex fineToCoarse (VU.unsafeIndex (grPairB gr) p)
                        !k = if a == b then maxBound else packPairKey a b
                    VUM.unsafeWrite pairKeyOrigM p (k, p)
                    initPairKeys (p + 1)
          initPairKeys 0
          Intro.sortBy compare pairKeyOrigM

          finePairToCoarseM <- VUM.replicate pairCount (-1 :: Int)
          pairAM <- VUM.unsafeNew pairCount
          pairBM <- VUM.unsafeNew pairCount
          pairBaseWM <- VUM.unsafeNew pairCount

          let uniquePairs !ix !coarsePairCount
                | ix == pairCount = pure coarsePairCount
                | otherwise = do
                    (!k, _) <- VUM.unsafeRead pairKeyOrigM ix
                    if k == maxBound
                      then pure coarsePairCount
                      else do
                        let (!a, !b) = unpackPairKey k
                            step !j !prod
                              | j == pairCount = pure (j, prod)
                              | otherwise = do
                                  (!k1, !p1) <- VUM.unsafeRead pairKeyOrigM j
                                  if k1 /= k
                                    then pure (j, prod)
                                    else do
                                      VUM.unsafeWrite finePairToCoarseM p1 coarsePairCount
                                      let !w = clampUnitInterval (VU.unsafeIndex (grPairBaseW gr) p1)
                                      step (j + 1) (prod * (1.0 - w))
                        (!ix1, !prodCut) <- step ix 1.0
                        VUM.unsafeWrite pairAM coarsePairCount a
                        VUM.unsafeWrite pairBM coarsePairCount b
                        VUM.unsafeWrite pairBaseWM coarsePairCount (1.0 - prodCut)
                        uniquePairs ix1 (coarsePairCount + 1)

          coarsePairCount <- uniquePairs 0 0
          pairA <- VU.generateM coarsePairCount (VUM.unsafeRead pairAM)
          pairB <- VU.generateM coarsePairCount (VUM.unsafeRead pairBM)
          pairBaseW <- VU.generateM coarsePairCount (VUM.unsafeRead pairBaseWM)
          finePairToCoarse <- VU.unsafeFreeze finePairToCoarseM

          rowCountsM <- VUM.replicate coarseRows (0 :: Int)
          let countRows !p
                | p == coarsePairCount = pure ()
                | otherwise = do
                    let !a = VU.unsafeIndex pairA p
                        !b = VU.unsafeIndex pairB p
                    na <- VUM.unsafeRead rowCountsM a
                    nb <- VUM.unsafeRead rowCountsM b
                    VUM.unsafeWrite rowCountsM a (na + 1)
                    VUM.unsafeWrite rowCountsM b (nb + 1)
                    countRows (p + 1)
          countRows 0

          offsetsM <- VUM.unsafeNew (coarseRows + 1)
          let prefixRows !i !acc
                | i == coarseRows = VUM.unsafeWrite offsetsM coarseRows acc
                | otherwise = do
                    VUM.unsafeWrite offsetsM i acc
                    n <- VUM.unsafeRead rowCountsM i
                    prefixRows (i + 1) (acc + n)
          prefixRows 0 0
          offsets <- VU.unsafeFreeze offsetsM

          let !adjCount = VU.unsafeIndex offsets coarseRows
          posM <- VU.thaw (VU.init offsets)
          nbrsM <- VUM.unsafeNew adjCount
          edgePairM <- VUM.unsafeNew adjCount

          let emit !src !dst !p = do
                pos <- VUM.unsafeRead posM src
                VUM.unsafeWrite nbrsM pos dst
                VUM.unsafeWrite edgePairM pos p
                VUM.unsafeWrite posM src (pos + 1)

              fillAdj !p
                | p == coarsePairCount = pure ()
                | otherwise = do
                    let !a = VU.unsafeIndex pairA p
                        !b = VU.unsafeIndex pairB p
                    emit a b p
                    emit b a p
                    fillAdj (p + 1)

          fillAdj 0

          nbrs <- VU.unsafeFreeze nbrsM
          edgePair <- VU.unsafeFreeze edgePairM
          outDeg <- VU.unsafeFreeze rowCountsM

          let coarseGraph =
                Graph
                  { grFaces = coarseRows
                  , grOffsets = offsets
                  , grNbrs = nbrs
                  , grEdgePair = edgePair
                  , grPairA = pairA
                  , grPairB = pairB
                  , grPairHasAB = VU.replicate coarsePairCount (1 :: Word8)
                  , grPairHasBA = VU.replicate coarsePairCount (1 :: Word8)
                  , grPairBaseW = pairBaseW
                  , grFaceArea = coarseMass
                  , grPairEdgeLen = VU.replicate coarsePairCount 1.0
                  , grPairCenterDist = VU.replicate coarsePairCount 1.0
                  , grPairNx = VU.replicate coarsePairCount 1.0
                  , grPairNy = VU.replicate coarsePairCount 0.0
                  , grPairMetric11 = VU.replicate coarsePairCount 1.0
                  , grPairMetric12 = VU.replicate coarsePairCount 0.0
                  , grPairMetric22 = VU.replicate coarsePairCount 1.0
                  , grFaceOutDeg = outDeg
                  , grNewToOld = identityPerm coarseRows
                  , grOldToNew = identityPerm coarseRows
                  }

              transfer =
                MGTransfer
                  { mgtFineRows = rows
                  , mgtCoarseRows = coarseRows
                  , mgtFineToCoarse = fineToCoarse
                  , mgtFineRestrictW = fineRestrictW
                  , mgtCoarseOffsets = coarseOffsets
                  , mgtCoarseFaces = coarseFaces
                  , mgtFinePairToCoarsePair = finePairToCoarse
                  }

          pure (Just (CoarsenResult transfer coarseGraph coarseMass))

restrictScalarInto :: MGTransfer -> VUM.MVector s Float -> VUM.MVector s Float -> ST s ()
restrictScalarInto !tr !fine !coarse = do
  let !fineRows = mgtFineRows tr
      !coarseRows = mgtCoarseRows tr
  if VUM.length fine /= fineRows || VUM.length coarse /= coarseRows
    then error "restrictScalarInto: row count mismatch"
    else do
      VUM.set coarse 0
      let !f2c = mgtFineToCoarse tr
          !wts = mgtFineRestrictW tr
          go !i
            | i == fineRows = pure ()
            | otherwise = do
                let !c = VU.unsafeIndex f2c i
                    !w = VU.unsafeIndex wts i
                x <- VUM.unsafeRead fine i
                y <- VUM.unsafeRead coarse c
                VUM.unsafeWrite coarse c (y + w * x)
                go (i + 1)
      go 0

prolongScalarAddInto :: MGTransfer -> VUM.MVector s Float -> VUM.MVector s Float -> ST s ()
prolongScalarAddInto !tr !coarse !fine = do
  let !fineRows = mgtFineRows tr
      !coarseRows = mgtCoarseRows tr
  if VUM.length coarse /= coarseRows || VUM.length fine /= fineRows
    then error "prolongScalarAddInto: row count mismatch"
    else do
      let !f2c = mgtFineToCoarse tr
          go !i
            | i == fineRows = pure ()
            | otherwise = do
                let !c = VU.unsafeIndex f2c i
                xc <- VUM.unsafeRead coarse c
                xf <- VUM.unsafeRead fine i
                VUM.unsafeWrite fine i (xf + xc)
                go (i + 1)
      go 0

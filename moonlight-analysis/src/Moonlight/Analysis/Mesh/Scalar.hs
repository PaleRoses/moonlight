{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Analysis.Mesh.Scalar
  ( KrylovTuning(..)
  , KrylovReport(..)
  , CGWorkspace(..)
  , ScalarLevelOp(..)
  , MGLevelOp
  , MGArena
  , MGArenaBuffer(..)
  , MGArenaLevelShape(..)
  , MGArenaShapeObstruction(..)
  , ScalarSystem(..)
  , ScalarMGPreconditioner
  , BoundarySpec(..)
  , enforceBoundaryValues
  , buildScalarLevelOp
  , refreshScalarLevelOpWeights
  , buildMGOperator
  , newMGArenaFromOp
  , retargetMGArenaFromOp
  , scalarMGPreconditionerForArena
  , scalarLevelRelativeDrift
  , solveScalarMGPCG
  , solveScalarSystemCG
  , newCGWorkspace
  , copyVec
  , solveScalarDiagonal
  , applyScalarLevelOp
  , applyScalarOpWithEdges
  , applyJacobiDiagWith
  ) where

import Control.Monad (replicateM_)
import Control.Monad.ST (ST, runST)
import Data.Kind (Type)
import Data.Word (Word8)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

import Moonlight.Core (MoonlightError (..))
import Moonlight.Analysis.Mesh.Graph
  ( Graph(grEdgePair, grFaceArea, grFaces, grNbrs, grOffsets, grPairA, grPairCenterDist, grPairEdgeLen)
  , edgeRange
  , pairMetricNormalFactor
  )
import Moonlight.Analysis.Mesh.Multigrid
  ( MGHierarchy(mghCoarse, mghGraph, mghTransfer)
  , MGTransfer(mgtFinePairToCoarsePair)
  , prolongScalarAddInto
  , restrictScalarInto
  )
type KrylovTuning :: Type
data KrylovTuning = KrylovTuning
  { ktMaxIterations :: !Int
  , ktRestart :: !Int
  , ktAbsTolerance :: !Double
  , ktRelTolerance :: !Double
  , ktBreakdownEps :: !Double
  }
  deriving stock (Eq, Show, Read)

type KrylovReport :: Type
data KrylovReport = KrylovReport
  { krInitialResidual :: !Float
  , krFinalResidual :: !Float
  , krIterations :: !Int
  , krConverged :: !Bool
  }
  deriving stock (Eq, Show, Read)

type CGWorkspace :: Type -> Type
data CGWorkspace s = CGWorkspace
  { cgwR :: !(VUM.MVector s Float)
  , cgwP :: !(VUM.MVector s Float)
  , cgwZ :: !(VUM.MVector s Float)
  , cgwAp :: !(VUM.MVector s Float)
  }

newScalarVector :: Int -> ST s (VUM.MVector s Float)
newScalarVector !rows =
  VUM.replicate rows 0.0
{-# INLINE newScalarVector #-}

newCGWorkspace :: Int -> ST s (CGWorkspace s)
newCGWorkspace !rows =
  CGWorkspace
    <$> newScalarVector rows
    <*> newScalarVector rows
    <*> newScalarVector rows
    <*> newScalarVector rows

mutableVectorAxis :: VUM.MVector s Float -> VU.Vector Int
mutableVectorAxis vectorValue =
  VU.enumFromN 0 (VUM.length vectorValue)
{-# INLINE mutableVectorAxis #-}

copyVec :: VUM.MVector s Float -> VUM.MVector s Float -> ST s ()
copyVec !dst !src =
  VUM.unsafeCopy dst src
{-# INLINE copyVec #-}

dotVec :: VUM.MVector s Float -> VUM.MVector s Float -> ST s Float
dotVec !leftVector !rightVector =
  realToFrac
    <$> VU.foldM'
      accumulateDot
      (0.0 :: Double)
      (mutableVectorAxis leftVector)
  where
    accumulateDot !accumulator !entryIndex = do
      leftValue <- VUM.unsafeRead leftVector entryIndex
      rightValue <- VUM.unsafeRead rightVector entryIndex
      pure (accumulator + (realToFrac leftValue :: Double) * (realToFrac rightValue :: Double))
{-# INLINE dotVec #-}

normVec :: VUM.MVector s Float -> ST s Float
normVec !vectorValue =
  sqrt <$> dotVec vectorValue vectorValue
{-# INLINE normVec #-}

updateSolutionResidualAndNorm
  :: Float
  -> VUM.MVector s Float
  -> VUM.MVector s Float
  -> VUM.MVector s Float
  -> VUM.MVector s Float
  -> ST s Float
updateSolutionResidualAndNorm
  !alpha
  !searchDirection
  !operatorDirection
  !solution
  !residual = do
    squaredNorm <-
      VUM.ifoldM'
        updateEntry
        (0.0 :: Double)
        searchDirection
    pure (sqrt (realToFrac squaredNorm :: Float))
  where
    updateEntry !squaredNorm !entryIndex !directionValue = do
      solutionValue <- VUM.unsafeRead solution entryIndex
      operatorValue <- VUM.unsafeRead operatorDirection entryIndex
      residualValue <- VUM.unsafeRead residual entryIndex
      let !updatedSolution = solutionValue + alpha * directionValue
          !updatedResidual = residualValue + (-alpha) * operatorValue
          !residualAsDouble = realToFrac updatedResidual :: Double
      VUM.unsafeWrite solution entryIndex updatedSolution
      VUM.unsafeWrite residual entryIndex updatedResidual
      pure (squaredNorm + residualAsDouble * residualAsDouble)
{-# INLINE updateSolutionResidualAndNorm #-}

subVecInto :: VUM.MVector s Float -> VUM.MVector s Float -> VUM.MVector s Float -> ST s ()
subVecInto !leftVector !rightVector !outVector =
  VU.foldM' writeDifference () (mutableVectorAxis leftVector)
  where
    writeDifference () !entryIndex = do
      leftValue <- VUM.unsafeRead leftVector entryIndex
      rightValue <- VUM.unsafeRead rightVector entryIndex
      VUM.unsafeWrite outVector entryIndex (leftValue - rightValue)
{-# INLINE subVecInto #-}

updateDirectionVec :: Float -> VUM.MVector s Float -> VUM.MVector s Float -> ST s ()
updateDirectionVec !beta !zVector !pVector =
  VU.foldM' writeDirection () (mutableVectorAxis zVector)
  where
    writeDirection () !entryIndex = do
      zValue <- VUM.unsafeRead zVector entryIndex
      pValue <- VUM.unsafeRead pVector entryIndex
      VUM.unsafeWrite pVector entryIndex (zValue + beta * pValue)
{-# INLINE updateDirectionVec #-}

maxAbsVec :: VU.Vector Float -> Float
maxAbsVec !vectorValue =
  VU.foldl' max 0.0 (VU.map abs vectorValue)
{-# INLINE maxAbsVec #-}

maxAbsDeltaVec :: VU.Vector Float -> VU.Vector Float -> Either MoonlightError Float
maxAbsDeltaVec !leftVector !rightVector
  | VU.length leftVector /= VU.length rightVector =
      Left
        ( InvariantViolation
            ( "maxAbsDeltaVec length mismatch: left="
                <> show (VU.length leftVector)
                <> ", right="
                <> show (VU.length rightVector)
            )
        )
  | otherwise =
      Right
        ( VU.foldl'
            max
            0.0
            (VU.zipWith (\leftValue rightValue -> abs (leftValue - rightValue)) leftVector rightVector)
        )
{-# INLINE maxAbsDeltaVec #-}

relativeVecDelta :: VU.Vector Float -> VU.Vector Float -> Either MoonlightError Float
relativeVecDelta !oldVector !newVector =
  let !denominator = max 1.0e-6 (max (maxAbsVec oldVector) (maxAbsVec newVector))
   in (/ denominator) <$> maxAbsDeltaVec oldVector newVector
{-# INLINE relativeVecDelta #-}

type BoundarySpec :: Type
data BoundarySpec
  = NoBoundary
  | Dirichlet !(VU.Vector Word8) !(VU.Vector Float)

enforceBoundaryValues :: BoundarySpec -> VUM.MVector s Float -> ST s ()
enforceBoundaryValues !bc !x =
  case bc of
    NoBoundary -> pure ()
    Dirichlet mask vals -> do
      let !n = VU.length mask
          go !i
            | i == n = pure ()
            | VU.unsafeIndex mask i == 0 = go (i + 1)
            | otherwise = do
                VUM.unsafeWrite x i (VU.unsafeIndex vals i)
                go (i + 1)
      go 0

type ScalarLevelOp :: Type
data ScalarLevelOp = ScalarLevelOp
  { sloGraph     :: !Graph
  , sloMass      :: !(VU.Vector Float)
  , sloPairW     :: !(VU.Vector Float)
  , sloEdgeW     :: !(VU.Vector Float)
  , sloRowSum    :: !(VU.Vector Float)
  , sloGeomScale :: !(VU.Vector Float)
  }

type MGLevelKernel :: Type
data MGLevelKernel = MGLevelKernel
  { mgkGraph  :: !Graph
  , mgkMass   :: !(VU.Vector Float)
  , mgkEdgeW  :: !(VU.Vector Float)
  , mgkRowSum :: !(VU.Vector Float)
  }

type MGLevelOp :: Type
data MGLevelOp
  = MGLevelBranch !MGLevelKernel !MGTransfer !MGLevelOp
  | MGLevelCoarsest !MGLevelKernel

type MGLevelWorkspace :: Type -> Type
data MGLevelWorkspace s = MGLevelWorkspace
  { mgwX  :: !(VUM.MVector s Float)
  , mgwB  :: !(VUM.MVector s Float)
  , mgwR  :: !(VUM.MVector s Float)
  , mgwAp :: !(VUM.MVector s Float)
  }

type MGArena :: Type -> Type
data MGArena s
  = MGArenaBranch !MGLevelKernel !MGTransfer !(MGLevelWorkspace s) !(MGArena s)
  | MGArenaCoarsest
      !MGLevelKernel
      !(MGLevelWorkspace s)
      !(VUM.MVector s Float)
      !(VUM.MVector s Float)

type MGArenaBuffer :: Type
data MGArenaBuffer
  = MGArenaX
  | MGArenaB
  | MGArenaR
  | MGArenaP
  | MGArenaZ
  | MGArenaAp
  deriving stock (Eq, Show)

type MGArenaLevelShape :: Type
data MGArenaLevelShape
  = MGArenaBranchLevel
  | MGArenaCoarsestLevel
  deriving stock (Eq, Show)

type MGArenaShapeObstruction :: Type
data MGArenaShapeObstruction
  = MGArenaBufferRowsMismatch
      !Int
      !MGArenaBuffer
      !Int
      !Int
  | MGArenaHierarchyMismatch
      !Int
      !MGArenaLevelShape
      !MGArenaLevelShape
  deriving stock (Eq, Show)

type ScalarSystem :: Type
data ScalarSystem = ScalarSystem
  { ssLevelOp  :: !ScalarLevelOp
  , ssDt       :: !Float
  , ssPull     :: !Float
  , ssDiff     :: !Float
  , ssBoundary :: !BoundarySpec
  }

applyScalarSystem
  :: ScalarSystem
  -> VUM.MVector s Float
  -> VUM.MVector s Float
  -> ST s ()
applyScalarSystem !sys !x !out = do
  let !lvl = ssLevelOp sys
      !gr = sloGraph lvl
      !rows = grFaces gr
      !offs = grOffsets gr
      !cols = grNbrs gr
      !mass = sloMass lvl
      !edgeW = sloEdgeW lvl
      !rowSum = sloRowSum lvl
      !dt = ssDt sys
      !pull = ssPull sys
      !diff = ssDiff sys
      !bc = ssBoundary sys

      faceLoop !i
        | i == rows = pure ()
        | otherwise =
            if isDirichlet bc i
              then do
                xi <- VUM.unsafeRead x i
                VUM.unsafeWrite out i xi
                faceLoop (i + 1)
              else do
                let !lo = VU.unsafeIndex offs i
                    !hi = VU.unsafeIndex offs (i + 1)

                    nbrLoop !e !acc
                      | e == hi = pure acc
                      | otherwise = do
                          let !j = VU.unsafeIndex cols e
                              !w = VU.unsafeIndex edgeW e
                          xj <-
                            if isDirichlet bc j
                              then pure 0.0
                              else VUM.unsafeRead x j
                          nbrLoop (e + 1) (acc + w * xj)

                xi <- VUM.unsafeRead x i
                nbrSum <- nbrLoop lo 0.0
                let !mi = VU.unsafeIndex mass i
                    !diag = mi * (1.0 + dt * pull) + dt * diff * VU.unsafeIndex rowSum i
                VUM.unsafeWrite out i (diag * xi - dt * diff * nbrSum)
                faceLoop (i + 1)

  case bc of
    NoBoundary -> applyScalarLevelOp lvl dt pull diff x out
    Dirichlet {} -> faceLoop 0
{-# INLINE applyScalarSystem #-}

isDirichlet :: BoundarySpec -> Int -> Bool
isDirichlet NoBoundary _ = False
isDirichlet (Dirichlet mask _) i = VU.unsafeIndex mask i /= 0
{-# INLINE isDirichlet #-}

type ScalarMGPreconditioner :: Type -> Type
data ScalarMGPreconditioner s = ScalarMGPreconditioner
  { smgpTuning  :: !KrylovTuning
  , smgpArena   :: !(MGArena s)
  , smgpDt      :: !Float
  , smgpPull    :: !Float
  , smgpDiff    :: !Float
  }

scalarMGPreconditionerForArena
  :: KrylovTuning
  -> MGArena s
  -> Float
  -> Float
  -> Float
  -> ScalarMGPreconditioner s
scalarMGPreconditionerForArena !tuning !arena !dt !pull !diff =
  ScalarMGPreconditioner
    { smgpTuning = tuning
    , smgpArena = arena
    , smgpDt = dt
    , smgpPull = pull
    , smgpDiff = diff
    }
{-# INLINE scalarMGPreconditionerForArena #-}

scatterEdgeWeights :: Graph -> VU.Vector Float -> VU.Vector Float
scatterEdgeWeights !gr !pairW =
  VU.map (\p -> max 0.0 (VU.unsafeIndex pairW p)) (grEdgePair gr)
{-# INLINE scatterEdgeWeights #-}

buildRowSumFromEdges :: Graph -> VU.Vector Float -> VU.Vector Float
buildRowSumFromEdges !gr !edgeW =
  VU.generate (grFaces gr) $ \i ->
    let (!lo, !hi) = edgeRange gr i
        go !e !acc
          | e == hi = acc
          | otherwise = go (e + 1) (acc + VU.unsafeIndex edgeW e)
    in go lo 0.0
{-# INLINE buildRowSumFromEdges #-}

buildScalarLevelOp :: Graph -> VUM.MVector s Float -> ST s ScalarLevelOp
buildScalarLevelOp !gr !finePairWeightsM = do
  pairCond <- VU.freeze finePairWeightsM
  let !pairCount = VU.length (grPairA gr)
      !actualPairs = VU.length pairCond
  if actualPairs /= pairCount
    then error "buildScalarLevelOp: pair count mismatch"
    else do
      let !mass =
            VU.generate (grFaces gr) $ \i ->
              realToFrac (max 1.0e-6 (VU.unsafeIndex (grFaceArea gr) i))
          !geomScale =
            VU.generate pairCount $ \p ->
              let !len = max 1.0e-6 (VU.unsafeIndex (grPairEdgeLen gr) p)
                  !dist = max 1.0e-6 (VU.unsafeIndex (grPairCenterDist gr) p)
                  !metric = pairMetricNormalFactor gr p
              in realToFrac (len * metric / dist)
          !trans =
            VU.generate pairCount $ \p ->
              max 0.0 (VU.unsafeIndex pairCond p) * VU.unsafeIndex geomScale p
          !edgeW = scatterEdgeWeights gr trans
          !rowSum = buildRowSumFromEdges gr edgeW
      pure ScalarLevelOp
        { sloGraph = gr
        , sloMass = mass
        , sloPairW = trans
        , sloEdgeW = edgeW
        , sloRowSum = rowSum
        , sloGeomScale = geomScale
        }
{-# INLINE buildScalarLevelOp #-}

refreshScalarLevelOpWeights :: ScalarLevelOp -> VUM.MVector s Float -> ST s ScalarLevelOp
refreshScalarLevelOpWeights !oldOp !finePairWeightsM = do
  pairCond <- VU.freeze finePairWeightsM
  let !gr = sloGraph oldOp
      !pairCount = VU.length (grPairA gr)
      !actualPairs = VU.length pairCond
  if actualPairs /= pairCount
    then buildScalarLevelOp gr finePairWeightsM
    else do
      let !geomScale = sloGeomScale oldOp
          !trans =
            VU.generate pairCount $ \p ->
              max 0.0 (VU.unsafeIndex pairCond p) * VU.unsafeIndex geomScale p
          !edgeW = scatterEdgeWeights gr trans
          !rowSum = buildRowSumFromEdges gr edgeW
      pure
        oldOp
          { sloPairW = trans
          , sloEdgeW = edgeW
          , sloRowSum = rowSum
          }
{-# INLINE refreshScalarLevelOpWeights #-}

scalarLevelRelativeDrift :: ScalarLevelOp -> ScalarLevelOp -> Either MoonlightError Float
scalarLevelRelativeDrift !oldOp !newOp =
  max
    <$> relativeVecDelta (sloPairW oldOp) (sloPairW newOp)
    <*> relativeVecDelta (sloRowSum oldOp) (sloRowSum newOp)
{-# INLINE scalarLevelRelativeDrift #-}

aggregatePairWeights :: Int -> MGTransfer -> VU.Vector Float -> VU.Vector Float
aggregatePairWeights !coarsePairCount !tr !finePairW = runST $ do
  out <- VUM.replicate coarsePairCount (0.0 :: Float)
  let !mapP = mgtFinePairToCoarsePair tr
      !n = VU.length finePairW
      go !p
        | p == n = pure ()
        | otherwise = do
            let !cp = VU.unsafeIndex mapP p
            if cp >= 0
              then do
                old <- VUM.unsafeRead out cp
                VUM.unsafeWrite out cp (old + max 0.0 (VU.unsafeIndex finePairW p))
                go (p + 1)
              else
                go (p + 1)
  go 0
  VU.unsafeFreeze out
{-# INLINE aggregatePairWeights #-}

buildMGOperator :: MGHierarchy -> ScalarLevelOp -> MGLevelOp
buildMGOperator !hier !fineOp =
  go hier (sloPairW fineOp)
  where
    go !h !pairW =
      let !g = mghGraph h
          !edgeW = scatterEdgeWeights g pairW
          !rowSum = buildRowSumFromEdges g edgeW
          !mass =
            VU.generate (grFaces g) $ \i ->
              realToFrac (max 1.0e-6 (VU.unsafeIndex (grFaceArea g) i))
          !kernel =
            MGLevelKernel
              { mgkGraph = g
              , mgkMass = mass
              , mgkEdgeW = edgeW
              , mgkRowSum = rowSum
              }
      in case (mghTransfer h, mghCoarse h) of
           (Just tr, Just hc) ->
             let !coarsePairCount = VU.length (grPairA (mghGraph hc))
                 !coarsePairW = aggregatePairWeights coarsePairCount tr pairW
             in MGLevelBranch kernel tr (go hc coarsePairW)
           _ -> MGLevelCoarsest kernel

newMGLevelWorkspace :: Int -> ST s (MGLevelWorkspace s)
newMGLevelWorkspace !rows =
  MGLevelWorkspace
    <$> newScalarVector rows
    <*> newScalarVector rows
    <*> newScalarVector rows
    <*> newScalarVector rows

newMGArenaFromOp :: MGLevelOp -> ST s (MGArena s)
newMGArenaFromOp !levelOperator =
  case levelOperator of
    MGLevelBranch kernel transfer coarseOperator ->
      MGArenaBranch kernel transfer
        <$> newMGLevelWorkspace (grFaces (mgkGraph kernel))
        <*> newMGArenaFromOp coarseOperator
    MGLevelCoarsest kernel ->
      MGArenaCoarsest kernel
        <$> newMGLevelWorkspace (grFaces (mgkGraph kernel))
        <*> newScalarVector (grFaces (mgkGraph kernel))
        <*> newScalarVector (grFaces (mgkGraph kernel))

retargetMGArenaFromOp
  :: MGLevelOp
  -> MGArena s
  -> Either MGArenaShapeObstruction (MGArena s)
retargetMGArenaFromOp = retargetMGArenaLevel 0

retargetMGArenaLevel
  :: Int
  -> MGLevelOp
  -> MGArena s
  -> Either MGArenaShapeObstruction (MGArena s)
retargetMGArenaLevel !levelIndex !levelOperator !arena =
  case (levelOperator, arena) of
    ( MGLevelBranch kernel transfer coarseOperator
      , MGArenaBranch _ _ workspace coarseArena
      ) -> do
        validateMGLevelWorkspace levelIndex (grFaces (mgkGraph kernel)) workspace
        retargetedCoarse <-
          retargetMGArenaLevel (levelIndex + 1) coarseOperator coarseArena
        pure (MGArenaBranch kernel transfer workspace retargetedCoarse)
    (MGLevelCoarsest kernel, MGArenaCoarsest _ workspace direction preconditioned) -> do
      let !rows = grFaces (mgkGraph kernel)
      validateMGLevelWorkspace levelIndex rows workspace
      validateMGArenaBuffer levelIndex MGArenaP rows direction
      validateMGArenaBuffer levelIndex MGArenaZ rows preconditioned
      pure (MGArenaCoarsest kernel workspace direction preconditioned)
    (MGLevelBranch _ _ _, MGArenaCoarsest _ _ _ _) ->
      Left
        ( MGArenaHierarchyMismatch
            levelIndex
            MGArenaBranchLevel
            MGArenaCoarsestLevel
        )
    (MGLevelCoarsest _, MGArenaBranch _ _ _ _) ->
      Left
        ( MGArenaHierarchyMismatch
            levelIndex
            MGArenaCoarsestLevel
            MGArenaBranchLevel
        )

validateMGLevelWorkspace
  :: Int
  -> Int
  -> MGLevelWorkspace s
  -> Either MGArenaShapeObstruction ()
validateMGLevelWorkspace !levelIndex !rows !workspace =
  validateMGArenaBuffer levelIndex MGArenaX rows (mgwX workspace)
    *> validateMGArenaBuffer levelIndex MGArenaB rows (mgwB workspace)
    *> validateMGArenaBuffer levelIndex MGArenaR rows (mgwR workspace)
    *> validateMGArenaBuffer levelIndex MGArenaAp rows (mgwAp workspace)

validateMGArenaBuffer
  :: Int
  -> MGArenaBuffer
  -> Int
  -> VUM.MVector s Float
  -> Either MGArenaShapeObstruction ()
validateMGArenaBuffer !levelIndex !buffer !expectedRows !vectorValue =
  let !actualRows = VUM.length vectorValue
  in if actualRows == expectedRows
       then Right ()
       else
         Left
           ( MGArenaBufferRowsMismatch
               levelIndex
               buffer
               expectedRows
               actualRows
           )

mgPreSweeps, mgPostSweeps :: Int
mgPreSweeps = 2
mgPostSweeps = 2
{-# INLINE mgPreSweeps #-}
{-# INLINE mgPostSweeps #-}

mgOmega :: Float
mgOmega = 0.82
{-# INLINE mgOmega #-}

applyScalarOp :: MGLevelKernel -> Float -> Float -> Float -> VUM.MVector s Float -> VUM.MVector s Float -> ST s ()
applyScalarOp !kernel =
  applyScalarOpWithEdges
    (mgkGraph kernel)
    (mgkMass kernel)
    (mgkEdgeW kernel)
    (mgkRowSum kernel)
{-# INLINE applyScalarOp #-}

applyScalarOpWithEdges
  :: Graph
  -> VU.Vector Float
  -> VU.Vector Float
  -> VU.Vector Float
  -> Float
  -> Float
  -> Float
  -> VUM.MVector s Float
  -> VUM.MVector s Float
  -> ST s ()
applyScalarOpWithEdges !gr !mass !edgeW !rowSum !dt !pull !diff !x !out = do
  let !rows = grFaces gr
      !offs = grOffsets gr
      !cols = grNbrs gr

      faceLoop !i
        | i == rows = pure ()
        | otherwise = do
            let !lo = VU.unsafeIndex offs i
                !hi = VU.unsafeIndex offs (i + 1)

                nbrLoop !e !acc
                  | e == hi = pure acc
                  | otherwise = do
                      let !j = VU.unsafeIndex cols e
                          !w = VU.unsafeIndex edgeW e
                      xj <- VUM.unsafeRead x j
                      nbrLoop (e + 1) (acc + w * xj)

            xi <- VUM.unsafeRead x i
            nbrSum <- nbrLoop lo 0.0
            let !mi = VU.unsafeIndex mass i
                !diag = mi * (1.0 + dt * pull) + dt * diff * VU.unsafeIndex rowSum i
            VUM.unsafeWrite out i (diag * xi - dt * diff * nbrSum)
            faceLoop (i + 1)

  faceLoop 0
{-# INLINE applyScalarOpWithEdges #-}

applyScalarLevelOp :: ScalarLevelOp -> Float -> Float -> Float -> VUM.MVector s Float -> VUM.MVector s Float -> ST s ()
applyScalarLevelOp !lvl =
  applyScalarOpWithEdges (sloGraph lvl) (sloMass lvl) (sloEdgeW lvl) (sloRowSum lvl)
{-# INLINE applyScalarLevelOp #-}

applyJacobiDiag :: MGLevelKernel -> Float -> Float -> Float -> VUM.MVector s Float -> VUM.MVector s Float -> ST s ()
applyJacobiDiag !kernel =
  applyJacobiDiagWith (mgkMass kernel) (mgkRowSum kernel)
{-# INLINE applyJacobiDiag #-}

applyJacobiDiagWith
  :: VU.Vector Float
  -> VU.Vector Float
  -> Float
  -> Float
  -> Float
  -> VUM.MVector s Float
  -> VUM.MVector s Float
  -> ST s ()
applyJacobiDiagWith !mass !rowSum !dt !pull !diff !src !dst = do
  let !rows = VU.length rowSum
      go !i
        | i == rows = pure ()
        | otherwise = do
            x <- VUM.unsafeRead src i
            let !mi = VU.unsafeIndex mass i
                !diag = max 1.0e-5 (mi * (1.0 + dt * pull) + dt * diff * VU.unsafeIndex rowSum i)
            VUM.unsafeWrite dst i (x / diag)
            go (i + 1)
  go 0
{-# INLINE applyJacobiDiagWith #-}

solveScalarDiagonal :: VU.Vector Float -> Float -> Float -> VUM.MVector s Float -> VUM.MVector s Float -> ST s Float
solveScalarDiagonal !mass !dt !pull !x !b = do
  let !rows = VUM.length x
      go !i
        | i == rows = pure 0.0
        | otherwise = do
            bi <- VUM.unsafeRead b i
            let !mi = VU.unsafeIndex mass i
                !diag = max 1.0e-5 (mi * (1.0 + dt * pull))
            VUM.unsafeWrite x i (bi / diag)
            go (i + 1)
  go 0

jacobiSweep :: Float -> MGLevelKernel -> Float -> Float -> Float -> VUM.MVector s Float -> VUM.MVector s Float -> VUM.MVector s Float -> ST s ()
jacobiSweep !omega !kernel !dt !pull !diff !x !b !tmp = do
  applyScalarOp kernel dt pull diff x tmp
  let !rows = grFaces (mgkGraph kernel)
      !mass = mgkMass kernel
      !rowSum = mgkRowSum kernel
      go !i
        | i == rows = pure ()
        | otherwise = do
            xi <- VUM.unsafeRead x i
            ai <- VUM.unsafeRead tmp i
            bi <- VUM.unsafeRead b i
            let !mi = VU.unsafeIndex mass i
                !diag = max 1.0e-5 (mi * (1.0 + dt * pull) + dt * diff * VU.unsafeIndex rowSum i)
            VUM.unsafeWrite x i (xi + omega * (bi - ai) / diag)
            go (i + 1)
  go 0

solveCoarsestCG
  :: KrylovTuning
  -> MGLevelKernel
  -> Float
  -> Float
  -> Float
  -> VUM.MVector s Float
  -> VUM.MVector s Float
  -> VUM.MVector s Float
  -> VUM.MVector s Float
  -> VUM.MVector s Float
  -> VUM.MVector s Float
  -> ST s Float
solveCoarsestCG !tuning !kernel !dt !pull !diff !x !b !r !p !z !ap
  | diff <= 1.0e-8 = solveScalarDiagonal (mgkMass kernel) dt pull x b
  | otherwise = do
      let !tolAbs = realToFrac (ktAbsTolerance tuning)
          !tolRel = realToFrac (ktRelTolerance tuning)
          !eps = max 1.0e-10 (realToFrac (ktBreakdownEps tuning) :: Float)
          !iters = max 4 (min 24 (ktMaxIterations tuning))

      applyScalarOp kernel dt pull diff x ap
      subVecInto b ap r
      res0 <- normVec r
      let !tol = max tolAbs (tolRel * res0)

      if res0 <= tol
        then pure res0
        else do
          applyJacobiDiag kernel dt pull diff r z
          rho0 <- dotVec r z
          copyVec p z

          let loop !k !rho !best
                | k >= iters = pure best
                | otherwise = do
                    applyScalarOp kernel dt pull diff p ap
                    pAp <- dotVec p ap
                    if abs pAp <= eps
                      then pure best
                      else do
                        let !alpha = rho / pAp
                        res <-
                          updateSolutionResidualAndNorm
                            alpha
                            p
                            ap
                            x
                            r
                        let !best1 = min best res
                        if res <= tol
                          then pure res
                          else do
                            applyJacobiDiag kernel dt pull diff r z
                            rho1 <- dotVec r z
                            if abs rho1 <= eps
                              then pure best1
                              else do
                                let !beta = rho1 / rho
                                updateDirectionVec beta z p
                                loop (k + 1) rho1 best1

          loop 0 rho0 res0

vcycleScalar :: KrylovTuning -> MGArena s -> Float -> Float -> Float -> ST s ()
vcycleScalar !tuning !arena !dt !pull !diff =
  case arena of
    MGArenaBranch kernel transfer workspace coarseArena -> do
      replicateM_
        mgPreSweeps
        ( jacobiSweep
            mgOmega
            kernel
            dt
            pull
            diff
            (mgwX workspace)
            (mgwB workspace)
            (mgwAp workspace)
        )
      applyScalarOp kernel dt pull diff (mgwX workspace) (mgwAp workspace)
      subVecInto (mgwB workspace) (mgwAp workspace) (mgwR workspace)
      VUM.set (arenaX coarseArena) 0
      restrictScalarInto transfer (mgwR workspace) (arenaB coarseArena)
      vcycleScalar tuning coarseArena dt pull diff
      prolongScalarAddInto transfer (arenaX coarseArena) (mgwX workspace)
      replicateM_
        mgPostSweeps
        ( jacobiSweep
            mgOmega
            kernel
            dt
            pull
            diff
            (mgwX workspace)
            (mgwB workspace)
            (mgwAp workspace)
        )
    MGArenaCoarsest kernel workspace direction preconditioned -> do
      _ <-
        solveCoarsestCG
          tuning
          kernel
          dt
          pull
          diff
          (mgwX workspace)
          (mgwB workspace)
          (mgwR workspace)
          direction
          preconditioned
          (mgwAp workspace)
      pure ()

arenaX :: MGArena s -> VUM.MVector s Float
arenaX !arena =
  case arena of
    MGArenaBranch _ _ workspace _ -> mgwX workspace
    MGArenaCoarsest _ workspace _ _ -> mgwX workspace
{-# INLINE arenaX #-}

arenaB :: MGArena s -> VUM.MVector s Float
arenaB !arena =
  case arena of
    MGArenaBranch _ _ workspace _ -> mgwB workspace
    MGArenaCoarsest _ workspace _ _ -> mgwB workspace
{-# INLINE arenaB #-}

applyMGPrecondScalar :: KrylovTuning -> MGArena s -> Float -> Float -> Float -> VUM.MVector s Float -> VUM.MVector s Float -> ST s ()
applyMGPrecondScalar !tuning !arena !dt !pull !diff !rhs !out = do
  copyVec (arenaB arena) rhs
  VUM.set (arenaX arena) 0
  vcycleScalar tuning arena dt pull diff
  copyVec out (arenaX arena)

applyScalarMGPreconditioner :: ScalarMGPreconditioner s -> VUM.MVector s Float -> VUM.MVector s Float -> ST s ()
applyScalarMGPreconditioner !pc =
  applyMGPrecondScalar
    (smgpTuning pc)
    (smgpArena pc)
    (smgpDt pc)
    (smgpPull pc)
    (smgpDiff pc)
{-# INLINE applyScalarMGPreconditioner #-}

type ScalarCGState :: Type
data ScalarCGState
  = ScalarCGRunning !Float !Float
  | ScalarCGConverged !Int !Float
  | ScalarCGStopped !Int !Float

solveScalarSystemCG ::
  KrylovTuning ->
  ScalarSystem ->
  ScalarMGPreconditioner s ->
  CGWorkspace s ->
  VUM.MVector s Float ->
  VUM.MVector s Float ->
  ST s KrylovReport
solveScalarSystemCG !tuning !systemValue !preconditioner !workspace !x !b = do
  let !tolAbs = realToFrac (ktAbsTolerance tuning)
      !tolRel = realToFrac (ktRelTolerance tuning)
      !eps = max 1.0e-10 (realToFrac (ktBreakdownEps tuning) :: Float)
      !iterations = max 1 (ktMaxIterations tuning)
      !r = cgwR workspace
      !p = cgwP workspace
      !z = cgwZ workspace
      !ap = cgwAp workspace
  applyScalarSystem systemValue x ap
  subVecInto b ap r
  res0 <- normVec r
  let !tol = max tolAbs (tolRel * res0)
  if res0 <= tol
    then pure (scalarCGReport res0 0 res0 True)
    else do
      applyScalarMGPreconditioner preconditioner r z
      rho0 <- dotVec r z
      copyVec p z
      finalState <-
        VU.foldM'
          (scalarCGIteration systemValue preconditioner x r p z ap tol eps)
          (ScalarCGRunning rho0 res0)
          (VU.enumFromN 0 iterations)
      pure (scalarCGStateReport res0 iterations finalState)

scalarCGIteration ::
  ScalarSystem ->
  ScalarMGPreconditioner s ->
  VUM.MVector s Float ->
  VUM.MVector s Float ->
  VUM.MVector s Float ->
  VUM.MVector s Float ->
  VUM.MVector s Float ->
  Float ->
  Float ->
  ScalarCGState ->
  Int ->
  ST s ScalarCGState
scalarCGIteration !systemValue !preconditioner !x !r !p !z !ap !tol !eps !stateValue !iterationIndex =
  case stateValue of
    ScalarCGConverged {} -> pure stateValue
    ScalarCGStopped {} -> pure stateValue
    ScalarCGRunning !rho !bestResidual -> do
      applyScalarSystem systemValue p ap
      pAp <- dotVec p ap
      if abs pAp <= eps
        then pure (ScalarCGStopped iterationIndex bestResidual)
        else do
          let !alpha = rho / pAp
          residual <-
            updateSolutionResidualAndNorm
              alpha
              p
              ap
              x
              r
          let !nextBestResidual = min bestResidual residual
          if residual <= tol
            then pure (ScalarCGConverged (iterationIndex + 1) residual)
            else do
              applyScalarMGPreconditioner preconditioner r z
              rhoNext <- dotVec r z
              if abs rhoNext <= eps
                then pure (ScalarCGStopped (iterationIndex + 1) nextBestResidual)
                else do
                  let !beta = rhoNext / rho
                  updateDirectionVec beta z p
                  pure (ScalarCGRunning rhoNext nextBestResidual)
{-# INLINE scalarCGIteration #-}

scalarCGStateReport :: Float -> Int -> ScalarCGState -> KrylovReport
scalarCGStateReport !initialResidual !iterationLimit !stateValue =
  case stateValue of
    ScalarCGRunning _ bestResidual -> scalarCGReport initialResidual iterationLimit bestResidual False
    ScalarCGStopped iterations bestResidual -> scalarCGReport initialResidual iterations bestResidual False
    ScalarCGConverged iterations finalResidual -> scalarCGReport initialResidual iterations finalResidual True

scalarCGReport :: Float -> Int -> Float -> Bool -> KrylovReport
scalarCGReport !initialResidual !iterations !finalResidual !converged =
  KrylovReport
    { krInitialResidual = initialResidual
    , krFinalResidual = finalResidual
    , krIterations = iterations
    , krConverged = converged
    }

solveScalarMGPCG
  :: KrylovTuning
  -> ScalarLevelOp
  -> MGArena s
  -> Float
  -> Float
  -> Float
  -> VUM.MVector s Float
  -> VUM.MVector s Float
  -> VUM.MVector s Float
  -> VUM.MVector s Float
  -> VUM.MVector s Float
  -> VUM.MVector s Float
  -> ST s KrylovReport
solveScalarMGPCG
  !tuning !fineOp !mga !dt !pull !diff
  !x !b !r !p !z !ap
  | diff <= 1.0e-8 = do
      residual <- solveScalarDiagonal (sloMass fineOp) dt pull x b
      pure
        KrylovReport
          { krInitialResidual = residual
          , krFinalResidual = residual
          , krIterations = 0
          , krConverged = True
          }
  | otherwise = do
      let !ss = ScalarSystem
            { ssLevelOp = fineOp
            , ssDt = dt
            , ssPull = pull
            , ssDiff = diff
            , ssBoundary = NoBoundary
            }
          !pc = scalarMGPreconditionerForArena tuning mga dt pull diff
          !ws = CGWorkspace
            { cgwR = r
            , cgwP = p
            , cgwZ = z
            , cgwAp = ap
            }
      solveScalarSystemCG tuning ss pc ws x b

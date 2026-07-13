{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Analysis.Mesh.Wave
  ( WaveTuning(..)
  , WaveState(..)
  , WaveLevelOp(..)
  , newWaveState
  , stepWaveNewmark
  ) where

import Control.Monad.ST (ST)
import Data.Kind (Type)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

import Moonlight.Analysis.Mesh.Graph (Graph(grFaces))
import Moonlight.Analysis.Mesh.Scalar
  ( BoundarySpec(..)
  , CGWorkspace
  , KrylovReport(..)
  , KrylovTuning
  , ScalarLevelOp(..)
  , ScalarSystem(..)
  , ScalarMGPreconditioner(..)
  , MGLevelOp
  , MGArena
  , copyVec
  , enforceBoundaryValues
  , newCGWorkspace
  , solveScalarSystemCG
  )

type WaveTuning :: Type
data WaveTuning = WaveTuning
  { wtBeta   :: !Float
  , wtGamma  :: !Float
  , wtKrylov :: !KrylovTuning
  }

type WaveState :: Type -> Type
data WaveState s = WaveState
  { wsDisp     :: !(VUM.MVector s Float)
  , wsDispPrev :: !(VUM.MVector s Float)
  , wsVel      :: !(VUM.MVector s Float)
  , wsAccel    :: !(VUM.MVector s Float)
  , wsRhs      :: !(VUM.MVector s Float)
  , wsCG       :: !(CGWorkspace s)
  }

type WaveLevelOp :: Type
data WaveLevelOp = WaveLevelOp
  { wloScalar   :: !ScalarLevelOp
  , wloDamping  :: !(VU.Vector Float)
  , wloBoundary :: !BoundarySpec
  }

newWaveState :: Int -> ST s (WaveState s)
newWaveState !rows = do
  u     <- VUM.replicate rows 0.0
  uPrev <- VUM.replicate rows 0.0
  v     <- VUM.replicate rows 0.0
  a     <- VUM.replicate rows 0.0
  rhs   <- VUM.replicate rows 0.0
  cg    <- newCGWorkspace rows
  pure (WaveState u uPrev v a rhs cg)

stepWaveNewmark
  :: WaveTuning
  -> WaveLevelOp
  -> MGLevelOp
  -> MGArena s
  -> Float
  -> VUM.MVector s Float
  -> WaveState s
  -> ST s Float
stepWaveNewmark !tuning !lvl !mgOp !mga !dt !force !st = do
  let !beta  = max 0.25 (wtBeta tuning)
      !gamma = max 0.5  (wtGamma tuning)
      !a0 = 1.0 / (beta * dt * dt)
      !a1 = gamma / (beta * dt)
      !a2 = 1.0 / (beta * dt)
      !a3 = 1.0 / (2.0 * beta) - 1.0
      !a4 = gamma / beta - 1.0
      !a5 = dt * (gamma / (2.0 * beta) - 1.0)

      !scalar = wloScalar lvl
      !rows = grFaces (sloGraph scalar)
      !damp = wloDamping lvl

      !effRowSum = VU.generate rows $ \i ->
        VU.unsafeIndex (sloRowSum scalar) i + (a0 - 1.0) + a1 * VU.unsafeIndex damp i

      !effScalar = scalar { sloRowSum = effRowSum }
      !effSys = ScalarSystem
        { ssLevelOp = effScalar
        , ssDt      = 1.0
        , ssPull    = 0.0
        , ssDiff    = 1.0
        , ssBoundary = wloBoundary lvl
        }
      !pc = ScalarMGPreconditioner
        { smgpTuning  = wtKrylov tuning
        , smgpLevelOp = mgOp
        , smgpArena   = mga
        , smgpDt      = 1.0
        , smgpPull    = 0.0
        , smgpDiff    = 1.0
        }

  copyVec (wsDispPrev st) (wsDisp st)

  let buildRhs !i
        | i == rows = pure ()
        | otherwise = do
            fi <- VUM.unsafeRead force i
            ui <- VUM.unsafeRead (wsDisp st) i
            vi <- VUM.unsafeRead (wsVel st) i
            ai <- VUM.unsafeRead (wsAccel st) i
            let !ci = VU.unsafeIndex damp i
                !eff =
                  fi
                  + (a0 * ui + a2 * vi + a3 * ai)
                  + ci * (a1 * ui + a4 * vi + a5 * ai)
            VUM.unsafeWrite (wsRhs st) i eff
            buildRhs (i + 1)

  buildRhs 0

  enforceBoundaryValues (wloBoundary lvl) (wsRhs st)
  report <- solveScalarSystemCG (wtKrylov tuning) effSys pc (wsCG st) (wsDisp st) (wsRhs st)
  enforceBoundaryValues (wloBoundary lvl) (wsDisp st)

  let update !i
        | i == rows = pure ()
        | otherwise = do
            u1    <- VUM.unsafeRead (wsDisp st) i
            u0    <- VUM.unsafeRead (wsDispPrev st) i
            v0    <- VUM.unsafeRead (wsVel st) i
            a0old <- VUM.unsafeRead (wsAccel st) i
            let !a1new = a0 * (u1 - u0) - a2 * v0 - a3 * a0old
                !v1    = v0 + dt * ((1.0 - gamma) * a0old + gamma * a1new)
            VUM.unsafeWrite (wsAccel st) i a1new
            VUM.unsafeWrite (wsVel st) i v1
            update (i + 1)

  update 0
  pure (krFinalResidual report)

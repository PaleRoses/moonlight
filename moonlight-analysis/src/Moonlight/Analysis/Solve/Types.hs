module Moonlight.Analysis.Solve.Types
  ( SolverFamily (..),
    SolverFailure (..),
    SolverStats (..),
    Result (..),
    HodgeSolverConfig (..),
    MonotoneSolverConfig (..),
    SemiringSolverConfig (..),
    defaultHodgeSolverConfig,
    defaultMonotoneSolverConfig,
    defaultSemiringSolverConfig,
  )
where

import Data.Kind (Type)
import Data.Set (Set)
import Moonlight.Core (MoonlightError)

type SolverFamily :: Type
data SolverFamily
  = HodgeFamily
  | MonotoneFamily
  | SemiringFamily
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

type SolverFailure :: Type
data SolverFailure
  = SolverIterationBudgetExceeded SolverFamily Int
  | SolverNonconvergence SolverFamily Double
  | SolverInvalidInput SolverFamily String
  | SolverBackendFailure SolverFamily MoonlightError
  deriving stock (Eq, Show)

type SolverStats :: Type
data SolverStats = SolverStats
  { solverIterations :: Int,
    solverResidual :: Double,
    solverConverged :: Bool
  }
  deriving stock (Eq, Show)

type Result :: Type -> Type -> Type
data Result cell state = Result
  { solverResultState :: state,
    solverResultChanged :: Set cell,
    solverResultStats :: SolverStats
  }
  deriving stock (Eq, Show)

type HodgeSolverConfig :: Type
data HodgeSolverConfig = HodgeSolverConfig
  { hodgeMaxIterations :: Int,
    hodgeConvergenceTolerance :: Double,
    hodgeDamping :: Double
  }
  deriving stock (Eq, Show)

type MonotoneSolverConfig :: Type
data MonotoneSolverConfig = MonotoneSolverConfig
  { monotoneMaxIterations :: Int
  }
  deriving stock (Eq, Show)

type SemiringSolverConfig :: Type
data SemiringSolverConfig = SemiringSolverConfig
  { semiringMaxIterations :: Int,
    semiringConvergenceTolerance :: Double
  }
  deriving stock (Eq, Show)

defaultHodgeSolverConfig :: HodgeSolverConfig
defaultHodgeSolverConfig =
  HodgeSolverConfig
    { hodgeMaxIterations = 128,
      hodgeConvergenceTolerance = 1.0e-8,
      hodgeDamping = 0.75
    }

defaultMonotoneSolverConfig :: MonotoneSolverConfig
defaultMonotoneSolverConfig =
  MonotoneSolverConfig
    { monotoneMaxIterations = 1024
    }

defaultSemiringSolverConfig :: SemiringSolverConfig
defaultSemiringSolverConfig =
  SemiringSolverConfig
    { semiringMaxIterations = 128,
      semiringConvergenceTolerance = 1.0e-8
    }

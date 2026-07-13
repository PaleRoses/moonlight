{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

-- | Static analyzers of control programs, defined as folds of the canonical
-- 'Program' and only then wrapped as 'Control' instances. The law kit's
-- fold-agreement property pins each instance to 'programStats' and
-- 'programPhaseList': building through the class and interpreting must
-- coincide with folding the corresponding 'Program'.
module Moonlight.Control.Interpret.Stats
  ( ProgramStats (..),
    emptyProgramStats,
    statsAlgebra,
    programStats,
    StatsBuilder (..),
    Phases (..),
    programPhaseList,
  )
where

import Data.Kind (Type)
import Numeric.Natural (Natural)

import Moonlight.Control.Class (Control (..))
import Moonlight.Control.Program
  ( Program,
    ProgramAlgebra (..),
    foldProgram,
    programPhases,
  )

-- | Phase count, an upper bound on phase executions (repetition multiplies,
-- every choice branch may be attempted), and constructor depth.
data ProgramStats = ProgramStats
  { statsPhaseCount :: !Natural,
    statsMaxPhaseRuns :: !Natural,
    statsDepth :: !Natural
  }
  deriving stock (Eq, Ord, Show)

-- | The statistics of 'Moonlight.Control.Class.skip'. Among reachable
-- normal forms, only 'Moonlight.Control.Class.skip' has these statistics.
emptyProgramStats :: ProgramStats
emptyProgramStats =
  ProgramStats
    { statsPhaseCount = 0,
      statsMaxPhaseRuns = 0,
      statsDepth = 1
    }

-- | The fold computing 'ProgramStats'.
statsAlgebra :: ProgramAlgebra ctx p ProgramStats
statsAlgebra =
  ProgramAlgebra
    { paSkip = emptyProgramStats,
      paPhase = const phaseStats,
      paSeq = combineBranches,
      paOr = combineBranches,
      paUpTo = \repeatCount body ->
        ProgramStats
          { statsPhaseCount = statsPhaseCount body,
            statsMaxPhaseRuns = repeatCount * statsMaxPhaseRuns body,
            statsDepth = 1 + statsDepth body
          },
      paAttempt = nestStats,
      paScoped = const nestStats
    }
  where
    phaseStats =
      ProgramStats
        { statsPhaseCount = 1,
          statsMaxPhaseRuns = 1,
          statsDepth = 1
        }
    combineBranches left right =
      ProgramStats
        { statsPhaseCount = statsPhaseCount left + statsPhaseCount right,
          statsMaxPhaseRuns = statsMaxPhaseRuns left + statsMaxPhaseRuns right,
          statsDepth = 1 + max (statsDepth left) (statsDepth right)
        }
    nestStats body =
      body {statsDepth = 1 + statsDepth body}

-- | Statistics of a program. O(n).
programStats :: Program ctx p -> ProgramStats
programStats = foldProgram statsAlgebra

-- | 'ProgramStats' as a 'Control' carrier. The unit checks in the methods
-- mirror the O(1) root reductions of the canonical 'Program' constructors,
-- which is what makes the fold-agreement law hold.
type StatsBuilder :: Type -> Type -> Type
newtype StatsBuilder ctx p = StatsBuilder
  { builtStats :: ProgramStats
  }
  deriving stock (Eq, Ord, Show)

instance Monoid ctx => Control (StatsBuilder ctx p) where
  type PhaseOf (StatsBuilder ctx p) = p
  type ContextOf (StatsBuilder ctx p) = ctx
  skip = StatsBuilder (paSkip algebra)
  phase phaseValue = StatsBuilder (paPhase algebra phaseValue)
  andThen left right
    | left == skipBuilder = right
    | right == skipBuilder = left
    | otherwise = StatsBuilder (paSeq algebra (builtStats left) (builtStats right))
  orElse left right = StatsBuilder (paOr algebra (builtStats left) (builtStats right))
  upTo repeatCount body
    | repeatCount == 0 || body == skipBuilder = skipBuilder
    | otherwise = StatsBuilder (paUpTo algebra repeatCount (builtStats body))
  attempt body
    | body == skipBuilder = skipBuilder
    | otherwise = StatsBuilder (paAttempt algebra (builtStats body))
  scoped context body
    | body == skipBuilder = skipBuilder
    | otherwise = StatsBuilder (paScoped algebra context (builtStats body))

algebra :: ProgramAlgebra ctx p ProgramStats
algebra = statsAlgebra

skipBuilder :: StatsBuilder ctx p
skipBuilder = StatsBuilder emptyProgramStats

-- | The phases of a program, in left-to-right order, as a 'Control'
-- carrier. List concatenation absorbs units, so no mirroring is needed —
-- only the constructor laws @'upTo' 0 x = 'skip'@ require a check.
type Phases :: Type -> Type -> Type
newtype Phases ctx p = Phases
  { phaseList :: [p]
  }
  deriving stock (Eq, Ord, Show)

instance Monoid ctx => Control (Phases ctx p) where
  type PhaseOf (Phases ctx p) = p
  type ContextOf (Phases ctx p) = ctx
  skip = Phases []
  phase phaseValue = Phases [phaseValue]
  andThen left right = Phases (phaseList left <> phaseList right)
  orElse left right = Phases (phaseList left <> phaseList right)
  upTo repeatCount body
    | repeatCount == 0 = Phases []
    | otherwise = body
  attempt = id
  scoped _context = id

-- | The phases of a program in left-to-right order; the fold-agreement
-- anchor of 'Phases'. O(n).
programPhaseList :: Program ctx p -> [p]
programPhaseList = programPhases

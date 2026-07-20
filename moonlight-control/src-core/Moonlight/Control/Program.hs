{-# LANGUAGE StandaloneKindSignatures #-}

-- | The canonical carrier of the control algebra.
--
-- Construct programs through the 'Moonlight.Control.Class.Control' methods —
-- every constructor is O(1) and composition never normalizes, so building a
-- program of @n@ phases by left- or right-folds of
-- 'Moonlight.Control.Class.andThen' is O(n). 'normalize' reduces to canonical
-- form in one O(n) pass; '==' compares canonical forms.
--
-- Consume programs with 'foldProgram': every interpretation of the algebra —
-- the machine, analyzers, renderers — is a fold of this structure.
module Moonlight.Control.Program
  ( Program,
    normalize,
    ProgramAlgebra (..),
    foldProgram,
    fromProgram,
    programSize,
    programPhases,
    programContexts,
  )
where

import Data.Kind (Type)
import Numeric.Natural (Natural)

import Moonlight.Control.Class (Control (..))
import Moonlight.Control.Program.Internal
  ( Program (..),
    normalize,
  )

-- | An algebra over the seven constructors of 'Program'. The 'paSeq' and
-- 'paOr' eliminators are binary, mirroring the representation; fold a
-- normalized program when spine grouping matters.
type ProgramAlgebra :: Type -> Type -> Type -> Type
data ProgramAlgebra ctx p r = ProgramAlgebra
  { paSkip :: r,
    paPhase :: p -> r,
    paSeq :: r -> r -> r,
    paOr :: r -> r -> r,
    paUpTo :: Natural -> r -> r,
    paAttempt :: r -> r,
    paScoped :: ctx -> r -> r
  }

-- | The catamorphism of the control algebra. O(n).
foldProgram :: ProgramAlgebra ctx p r -> Program ctx p -> r
foldProgram algebra =
  go
  where
    go program =
      case program of
        Skip -> paSkip algebra
        Phase phaseValue -> paPhase algebra phaseValue
        Seq left right -> paSeq algebra (go left) (go right)
        Or left right -> paOr algebra (go left) (go right)
        UpTo repeatCount body -> paUpTo algebra repeatCount (go body)
        Attempt body -> paAttempt algebra (go body)
        Scoped context body -> paScoped algebra context (go body)
{-# INLINABLE foldProgram #-}

-- | The unique interpretation of a program into any 'Control' carrier:
-- 'foldProgram' with the carrier's own methods as the algebra. Applied to
-- 'Program' itself this re-runs smart construction; the law kit's
-- fold-agreement property states that interpreting through an instance
-- coincides with 'fromProgram' into 'Program' followed by the instance's
-- defining fold. O(n).
fromProgram :: Control c => Program (ContextOf c) (PhaseOf c) -> c
fromProgram =
  foldProgram
    ProgramAlgebra
      { paSkip = skip,
        paPhase = phase,
        paSeq = andThen,
        paOr = orElse,
        paUpTo = upTo,
        paAttempt = attempt,
        paScoped = scoped
      }
{-# INLINABLE fromProgram #-}

-- | The number of constructors in the representation. O(n).
programSize :: Program ctx p -> Natural
programSize =
  foldProgram
    ProgramAlgebra
      { paSkip = 1,
        paPhase = const 1,
        paSeq = \left right -> 1 + left + right,
        paOr = \left right -> 1 + left + right,
        paUpTo = \_count body -> 1 + body,
        paAttempt = (1 +),
        paScoped = \_context body -> 1 + body
      }

-- | The phases of a program in left-to-right order. O(n).
programPhases :: Program ctx p -> [p]
programPhases =
  foldr (:) []

-- | The contexts of every scoped region in left-to-right, outside-in order.
-- O(n).
programContexts :: Program ctx p -> [ctx]
programContexts =
  foldProgram
    ProgramAlgebra
      { paSkip = [],
        paPhase = const [],
        paSeq = (<>),
        paOr = (<>),
        paUpTo = const id,
        paAttempt = id,
        paScoped = (:)
      }

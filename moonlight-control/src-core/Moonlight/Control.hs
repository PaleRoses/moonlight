-- | A small algebra of control programs: typed scheduling, guidance, and
-- strategy primitives for deterministic engines.
--
-- == The algebra
--
-- +---------------------+----------------------------------------------+
-- | Primitive           | Meaning                                      |
-- +=====================+==============================================+
-- | 'skip'              | empty program; identity for 'andThen'       |
-- +---------------------+----------------------------------------------+
-- | 'phase' p           | one phase of domain work                     |
-- +---------------------+----------------------------------------------+
-- | 'andThen' x y       | sequential composition (monoid with 'skip') |
-- +---------------------+----------------------------------------------+
-- | 'orElse' x y        | ordered choice (semigroup, /no/ identity)    |
-- +---------------------+----------------------------------------------+
-- | 'upTo' n x          | bounded repetition, stops on no-progress     |
-- +---------------------+----------------------------------------------+
-- | 'attempt' x         | speculative; rolls back on no-progress       |
-- +---------------------+----------------------------------------------+
-- | 'scoped' m x        | modal context over a region (monoid action)  |
-- +---------------------+----------------------------------------------+
--
-- Construction is O(1) per combinator; 'normalize' is one O(n) pass; '=='
-- on 'Program' compares normal forms. The laws — including the deliberate
-- negative laws for 'orElse', 'attempt', and 'upTo' — are documented in
-- "Moonlight.Control.Class" and property-tested by the law kit.
--
-- 'gated' and 'weighted' scope guidance and scheduling weight over program
-- regions; the engine composes enclosing modalities into each phase
-- dispatch.
module Moonlight.Control
  ( -- * The algebra
    Control (..),
    sequenceAll,
    choices,

    -- * The canonical carrier
    Program,
    normalize,
    ProgramAlgebra (..),
    foldProgram,
    fromProgram,
    programSize,
    programPhases,
    programContexts,

    -- * Modal contexts
    Modality (..),
    gateContext,
    weightContext,
    gated,
    weighted,
    gateIsUnit,
  )
where

import Moonlight.Control.Class
  ( Control (..),
    choices,
    sequenceAll,
  )
import Moonlight.Control.Modality
  ( Modality (..),
    gateContext,
    gateIsUnit,
    gated,
    weightContext,
    weighted,
  )
import Moonlight.Control.Program
  ( Program,
    ProgramAlgebra (..),
    foldProgram,
    fromProgram,
    normalize,
    programContexts,
    programPhases,
    programSize,
  )

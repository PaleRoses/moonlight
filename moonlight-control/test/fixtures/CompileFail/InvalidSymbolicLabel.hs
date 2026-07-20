{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}

module InvalidSymbolicLabel where

import Moonlight.Control.Engine.Symbolic
  ( Domain (..),
    KnownPhase (..),
    SymbolicProgram,
  )

data TestDomain

data TestPhase
  = AlphaPhase
  deriving stock (Eq, Ord, Show)

instance Domain TestDomain where
  type PhaseKey TestDomain = TestPhase
  type RuleKey TestDomain = ()

instance KnownPhase TestDomain "alpha" where
  knownPhaseKey =
    AlphaPhase

badProgram :: SymbolicProgram () TestDomain
badProgram =
  #alpah

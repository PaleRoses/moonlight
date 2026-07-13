{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module CompileFail.Phase1BettiLeak (forbiddenPhase1Capability) where

import Moonlight.Core (mkCapability)
import Moonlight.Homology

forbiddenPhase1Capability :: BettiCapability 'Phase1 Int
forbiddenPhase1Capability =
  mkCapability @RequirePhase2 @'Phase1 (BettiReducer (\_ -> Right []))

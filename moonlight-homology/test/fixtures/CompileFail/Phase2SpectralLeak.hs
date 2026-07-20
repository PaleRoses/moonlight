{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module CompileFail.Phase2SpectralLeak (forbiddenPhase2Spectral) where

import Moonlight.Core (mkCapability)
import Moonlight.Homology

forbiddenPhase2Spectral :: SpectralCapability 'Phase2 Int
forbiddenPhase2Spectral =
  mkCapability @RequirePhase4 @'Phase2 (SpectralAdvance Right)

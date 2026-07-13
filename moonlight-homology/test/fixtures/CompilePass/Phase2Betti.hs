{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module CompilePass.Phase2Betti (phase2Result) where

import Moonlight.Core (mkCapability)
import Moonlight.Homology

phase2Result :: Either HomologyFailure [HomologyGroup Int]
phase2Result = do
  finite <- mkFiniteChainComplexChecked (HomologicalDegree 0) (const emptyBoundaryIncidence)
  computeBettiNumbers
    (mkCapability @RequirePhase2 @'Phase2 (BettiReducer (\_ -> Right [])))
    finite

module Moonlight.Saturation.Core
  ( SaturationBudget (..),
    SaturationTermination (..),
    TerminationGoal (..),
    alwaysContinue,
    goal,
    contramapGoal,
    SaturationKernel (..),
    RoundPlan (..),
    ApplyOutcome (..),
    RebuildOutcome (..),
    SaturationRun (..),
    SaturationEffects (..),
    runSaturation,
    runSaturationWith,
    runSaturationSteps,
  )
where

import Moonlight.Saturation.Core.Engine
  ( SaturationEffects (..),
    runSaturation,
    runSaturationWith,
    runSaturationSteps,
  )
import Moonlight.Saturation.Core.Kernel
  ( SaturationKernel (..),
  )
import Moonlight.Saturation.Core.Outcome
  ( ApplyOutcome (..),
    RebuildOutcome (..),
  )
import Moonlight.Saturation.Core.Round
  ( RoundPlan (..),
  )
import Moonlight.Saturation.Core.Run
  ( SaturationRun (..),
  )
import Moonlight.Saturation.Core.Termination
  ( SaturationBudget (..),
    SaturationTermination (..),
    TerminationGoal (..),
    alwaysContinue,
    contramapGoal,
    goal,
  )

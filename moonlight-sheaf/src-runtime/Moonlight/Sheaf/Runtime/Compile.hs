module Moonlight.Sheaf.Runtime.Compile
  ( RuntimeResolutionProgram (..),
    RuntimeResolutionBuilder,
    runtimeResolutionInitialDirtyCells,
    runRuntimeResolutionProgram,
    runRuntimeResolutionProgramInitial,
    mapRuntimeResolutionFailure,
  )
where

import Moonlight.Sheaf.Runtime.Compile.Internal
  ( RuntimeResolutionBuilder,
    RuntimeResolutionProgram (..),
    mapRuntimeResolutionFailure,
    runRuntimeResolutionProgram,
    runRuntimeResolutionProgramInitial,
    runtimeResolutionInitialDirtyCells,
  )

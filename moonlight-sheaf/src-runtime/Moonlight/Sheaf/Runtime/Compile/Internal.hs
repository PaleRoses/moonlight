{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Sheaf.Runtime.Compile.Internal
  ( RuntimeResolutionProgram (..),
    RuntimeResolutionBuilder,
    runtimeResolutionInitialDirtyCells,
    runRuntimeResolutionProgram,
    runRuntimeResolutionProgramInitial,
    mapRuntimeResolutionFailure,
  )
where

import Data.Bifunctor
  ( first,
  )
import Data.Kind
  ( Type,
  )
import Data.Set
  ( Set,
  )
import Moonlight.Sheaf.Operator.GradedComplex
import Moonlight.Sheaf.Section.Model
  ( SheafModel,
  )
import Moonlight.Sheaf.Section.Store.Descent.Execute
import Moonlight.Sheaf.Section.Store.Descent.Prepare
import Moonlight.Sheaf.Section.Store.State
import Moonlight.Sheaf.Section.Store.Types

type RuntimeResolutionProgram :: Type -> Type -> Type -> Type -> Type -> Type
data RuntimeResolutionProgram site cell stalk report failure = RuntimeResolutionProgram
  { rrpInitialDirtyCells :: !(Set cell),
    rrpRunDirtyCells ::
      Set cell ->
      site ->
      TotalSectionStore cell stalk ->
      Either failure (site, TotalSectionStore cell stalk, report)
  }

type RuntimeResolutionBuilder :: Type -> Type -> Type -> Type -> Type -> Type -> Type
type RuntimeResolutionBuilder site cell stalk restrictionWitness report failure =
  site ->
  SheafModel cell restrictionWitness ->
  GradedComplex cell Int ->
  TotalSectionStore cell stalk ->
  RuntimeResolutionProgram site cell stalk report failure

runtimeResolutionInitialDirtyCells ::
  RuntimeResolutionProgram site cell stalk report failure ->
  Set cell
runtimeResolutionInitialDirtyCells =
  rrpInitialDirtyCells
{-# INLINE runtimeResolutionInitialDirtyCells #-}

runRuntimeResolutionProgram ::
  RuntimeResolutionProgram site cell stalk report failure ->
  Set cell ->
  site ->
  TotalSectionStore cell stalk ->
  Either failure (site, TotalSectionStore cell stalk, report)
runRuntimeResolutionProgram =
  rrpRunDirtyCells
{-# INLINE runRuntimeResolutionProgram #-}

runRuntimeResolutionProgramInitial ::
  RuntimeResolutionProgram site cell stalk report failure ->
  site ->
  TotalSectionStore cell stalk ->
  Either failure (site, TotalSectionStore cell stalk, report)
runRuntimeResolutionProgramInitial program =
  rrpRunDirtyCells program (rrpInitialDirtyCells program)
{-# INLINE runRuntimeResolutionProgramInitial #-}

mapRuntimeResolutionFailure ::
  (leftFailure -> rightFailure) ->
  RuntimeResolutionProgram site cell stalk report leftFailure ->
  RuntimeResolutionProgram site cell stalk report rightFailure
mapRuntimeResolutionFailure mapFailure program =
  RuntimeResolutionProgram
    { rrpInitialDirtyCells = rrpInitialDirtyCells program,
      rrpRunDirtyCells =
        \dirtyCells site sectionValue ->
          first mapFailure
            (rrpRunDirtyCells program dirtyCells site sectionValue)
    }
{-# INLINE mapRuntimeResolutionFailure #-}

{-# LANGUAGE GHC2024 #-}

-- | Public error surface for the relational front.
-- Owns host-build obstructions, source/compile/run/application-condition and
-- saturation errors, plus their pretty renderers.
-- Contracts: host inputs must be ground and sort-consistent, and every front
-- boundary keeps its original typed obstruction.
module Moonlight.Rewrite.Relational.Front.Error
  ( HostBuildError (..),
    prettyHostBuildError,
    RelationalSaturationContext (..),
    RelationalSaturationPlanError (..),
    RelationalSaturationObstruction (..),
    relationalSaturationResumeError,
    RelationalProgramError (..),
    prettyRelationalProgramError,
  )
where

import Moonlight.Core
  ( PatternVar,
    UnionFindAllocationError,
  )
import Moonlight.Rewrite.DSL
  ( ProgramError,
    prettyProgramError,
  )
import Moonlight.Rewrite.DSL
  ( ContextName,
  )
import Moonlight.Rewrite.DSL
  ( NodeTag,
    RewriteSignature,
  )
import Moonlight.Rewrite.DSL
  ( SortName,
  )
import Moonlight.Rewrite.Relational
  ( RelationalRuleCompileError,
  )
import Moonlight.Rewrite.Relational.Front.Saturation.Error
  ( RelationalSaturationContext (..),
    RelationalSaturationObstruction (..),
    RelationalSaturationPlanError (..),
    prettyRelationalSaturationObstruction,
    prettyRelationalSaturationPlanError,
    relationalSaturationResumeError,
  )
import Moonlight.Rewrite.Runtime (RewriteApplicationError)
import Moonlight.Rewrite.Algebra
  ( ApplicationConditionPath,
  )
import Moonlight.Rewrite.Relational
  ( RewriteRunError,
  )
import Moonlight.Rewrite.System
  ( RuleName,
    RuleNameError,
  )

data HostBuildError
  = HostTermContainsVariable !String !SortName
  | HostNegativeNodeKey !Int
  | HostNegativeChildKey !Int !Int
  | HostUnknownChildKey !Int !Int
  | HostEmptyNodeClass !Int
  | HostClassSortMismatch !Int !SortName !SortName
  | HostChildSortMismatch !Int !Int !SortName !SortName
  | HostClassIdAllocationFailed !UnionFindAllocationError
  | HostRebuildApplicationError !RewriteApplicationError
  deriving stock (Eq, Ord, Show, Read)

prettyHostBuildError :: HostBuildError -> String
prettyHostBuildError errorValue =
  case errorValue of
    HostTermContainsVariable name sortName ->
      shownPairMessage "rewrite host terms must be ground; variable " name " : " sortName
        <> " was supplied"

    HostNegativeNodeKey nodeKey ->
      "rewrite host node key " <> show nodeKey <> " is invalid; host keys must be non-negative"

    HostNegativeChildKey nodeKey childKey ->
      shownPairMessage "rewrite host node " nodeKey " references invalid child key " childKey
        <> "; host keys must be non-negative"

    HostUnknownChildKey nodeKey childKey ->
      shownPairMessage "rewrite host node " nodeKey " references unknown child class " childKey

    HostEmptyNodeClass nodeKey ->
      "rewrite host node class " <> show nodeKey <> " is empty; class sort cannot be inferred"

    HostClassSortMismatch nodeKey leftSort rightSort ->
      shownPairMessage "rewrite host node class " nodeKey " mixes result sorts " leftSort
        <> " and "
        <> show rightSort

    HostChildSortMismatch nodeKey childKey expectedSort observedSort ->
      shownPairMessage "rewrite host node " nodeKey " expects child class " childKey
        <> " to have sort "
        <> show expectedSort
        <> ", but observed "
        <> show observedSort

    HostClassIdAllocationFailed allocationError ->
      "rewrite host class-id allocation failed: " <> show allocationError

    HostRebuildApplicationError applicationError ->
      "rewrite host rebuild failed with unexpected application obstruction: " <> show applicationError

data RelationalProgramError sig
  = RelationalProgramSourceError !(ProgramError sig)
  | RelationalProgramCompileError !RelationalRuleCompileError
  | RelationalProgramRuleNameError !String !RuleNameError
  | RelationalProgramMatchVariablesMissing !RuleName ![PatternVar]
  | RelationalProgramContextMissing !ContextName
  | RelationalProgramRunError !(RewriteRunError ContextName)
  | RelationalProgramRewriteApplicationError !RewriteApplicationError
  | RelationalProgramApplicationConditionPlanMissing !ApplicationConditionPath
  | RelationalProgramApplicationConditionAnchorSlotMissing !ApplicationConditionPath !PatternVar
  | RelationalProgramSaturationPlanError !RelationalSaturationPlanError
  | RelationalProgramSaturationObstruction !(RelationalSaturationObstruction sig)

deriving stock instance
  (RewriteSignature sig, Show (NodeTag sig)) =>
  Show (RelationalProgramError sig)

prettyRelationalProgramError ::
  (RewriteSignature sig, Show (NodeTag sig)) =>
  RelationalProgramError sig ->
  String
prettyRelationalProgramError errorValue =
  case errorValue of
    RelationalProgramSourceError sourceError ->
      prettyProgramError sourceError

    RelationalProgramCompileError compileError ->
      "relational rewrite compile error: " <> show compileError

    RelationalProgramRuleNameError rawName ruleNameError ->
      shownPairMessage "rewrite rule name " rawName " is invalid: " ruleNameError

    RelationalProgramMatchVariablesMissing ruleNameValue variables ->
      shownPairMessage "relational rewrite rule " ruleNameValue " references pattern variables without typed binders: " variables

    RelationalProgramContextMissing contextNameValue ->
      "rewrite context " <> show contextNameValue <> " is not installed in the engine"

    RelationalProgramRunError runError ->
      "relational rewrite run error: " <> show runError

    RelationalProgramRewriteApplicationError applicationError ->
      "relational rewrite application error: " <> show applicationError

    RelationalProgramApplicationConditionPlanMissing path ->
      "relational application-condition plan missing for atom path " <> show path

    RelationalProgramApplicationConditionAnchorSlotMissing path patternVar ->
      shownPairMessage "relational application-condition atom " path " does not expose anchor variable " patternVar
        <> " as an output slot"

    RelationalProgramSaturationPlanError saturationPlanError ->
      prettyRelationalSaturationPlanError saturationPlanError

    RelationalProgramSaturationObstruction saturationObstruction ->
      prettyRelationalSaturationObstruction saturationObstruction

shownPairMessage :: (Show left, Show right) => String -> left -> String -> right -> String
shownPairMessage prefix leftValue separator rightValue =
  prefix <> show leftValue <> separator <> show rightValue

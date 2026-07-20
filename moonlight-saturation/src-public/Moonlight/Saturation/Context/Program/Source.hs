{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Saturation.Context.Program.Source
  ( ProgramBuildError,
    ProgramM,
    program,
    include,
    base,
    context,
    rewrite,
    rewrites,
    fact,
    facts,
    activateBaseRewrite,
    activateBaseRewrites,
    supportBaseRewrite,
    ProgramFragment,
    emptyProgramFragment,
    appendProgramFragments,
    finishProgram,
    compileProgram,
    compileFragment,
  )
where

import Data.Bifunctor (first)
import Data.Foldable qualified as Foldable
import Data.Kind (Type)
import Data.Sequence qualified as Seq
import Moonlight.Core (RewriteRuleId)
import Moonlight.Saturation.Context.Error
  ( SaturationCompileError (..),
  )
import Moonlight.Saturation.Context.Program.Compile
  ( compileSourceProgram,
  )
import Moonlight.Saturation.Context.Program.Internal.Builder
  ( ProgramBuildError,
    ProgramDecl (..),
    ProgramFragment (..),
    appendProgramFragments,
    emptyProgramFragment,
    finishProgram,
    singletonProgramDecl,
  )
import Moonlight.Saturation.Context.Program.Plan
  ( Plan,
  )
import Moonlight.Saturation.Context.Program.Spec
  ( PlanSpec,
  )
import Moonlight.Saturation.Matching
  ( MatchSite (..),
  )
import Moonlight.Saturation.Substrate

-- | Scoped program emission.
--
-- A 'ProgramM' runs under one current 'MatchSite'. Site-sensitive declarations
-- are stamped with that site when they are emitted.
--
-- A 'ProgramFragment' is already absolute. 'include' preserves the fragment as
-- emitted; it does not re-scope declarations under the current site.
--
-- Validation and normalization are deliberately not live EDSL responsibilities.
-- They remain in 'finishProgram' and the compile pipeline.
type ProgramM :: Type -> Type -> Type
newtype ProgramM u a = ProgramM
  { unProgramM ::
      MatchSite (SatContext u) ->
      (a, ProgramFragment u)
  }

instance Functor (ProgramM u) where
  fmap transform action =
    ProgramM $ \site ->
      case runProgramMAt action site of
        (value, fragment) ->
          (transform value, fragment)

instance Applicative (ProgramM u) where
  pure value =
    ProgramM $ \_site ->
      (value, emptyProgramFragment)

  functionAction <*> valueAction =
    ProgramM $ \site ->
      case runProgramMAt functionAction site of
        (transform, leftFragment) ->
          case runProgramMAt valueAction site of
            (value, rightFragment) ->
              ( transform value,
                appendProgramFragments leftFragment rightFragment
              )

  liftA2 combine leftAction rightAction =
    ProgramM $ \site ->
      case runProgramMAt leftAction site of
        (leftValue, leftFragment) ->
          case runProgramMAt rightAction site of
            (rightValue, rightFragment) ->
              ( combine leftValue rightValue,
                appendProgramFragments leftFragment rightFragment
              )

  leftAction *> rightAction =
    ProgramM $ \site ->
      case runProgramMAt leftAction site of
        (_, leftFragment) ->
          case runProgramMAt rightAction site of
            (rightValue, rightFragment) ->
              ( rightValue,
                appendProgramFragments leftFragment rightFragment
              )

  leftAction <* rightAction =
    ProgramM $ \site ->
      case runProgramMAt leftAction site of
        (leftValue, leftFragment) ->
          case runProgramMAt rightAction site of
            (_, rightFragment) ->
              ( leftValue,
                appendProgramFragments leftFragment rightFragment
              )

instance Monad (ProgramM u) where
  action >>= continue =
    ProgramM $ \site ->
      case runProgramMAt action site of
        (value, leftFragment) ->
          case runProgramMAt (continue value) site of
            (result, rightFragment) ->
              ( result,
                appendProgramFragments leftFragment rightFragment
              )

runProgramMAt ::
  ProgramM u a ->
  MatchSite (SatContext u) ->
  (a, ProgramFragment u)
runProgramMAt action =
  unProgramM action

program ::
  ProgramM u () ->
  ProgramFragment u
program action =
  case runProgramMAt action BaseSite of
    (_, fragment) ->
      fragment

include ::
  ProgramFragment u ->
  ProgramM u ()
include fragment =
  ProgramM $ \_site ->
    ((), fragment)

withSite ::
  MatchSite (SatContext u) ->
  ProgramM u a ->
  ProgramM u a
withSite site action =
  ProgramM $ \_currentSite ->
    runProgramMAt action site

base ::
  ProgramM u a ->
  ProgramM u a
base =
  withSite BaseSite

context ::
  SatContext u ->
  ProgramM u a ->
  ProgramM u a
context contextValue =
  withSite (ContextSite contextValue)

emitAt ::
  (MatchSite (SatContext u) -> ProgramFragment u) ->
  ProgramM u ()
emitAt fragmentAt =
  ProgramM $ \site ->
    ((), fragmentAt site)

emitAtReturning ::
  a ->
  (MatchSite (SatContext u) -> ProgramFragment u) ->
  ProgramM u a
emitAtReturning value fragmentAt =
  ProgramM $ \site ->
    (value, fragmentAt site)

rewrite ::
  forall u.
  RewriteSystem u =>
  SatRuleSource u ->
  ProgramM u RewriteRuleId
rewrite source =
  emitAtReturning
    (rewriteRuleSourceId @u source)
    (\site -> rewriteAt site source)

rewrites ::
  forall u f.
  (RewriteSystem u, Foldable f) =>
  f (SatRuleSource u) ->
  ProgramM u [RewriteRuleId]
rewrites sources =
  let sourceList =
        Foldable.toList sources
      ruleIds =
        fmap (rewriteRuleSourceId @u) sourceList
   in emitAtReturning
        ruleIds
        (\site -> rewritesAt site sourceList)

fact ::
  forall u.
  FactSystem u =>
  SatFactSource u ->
  ProgramM u RewriteRuleId
fact source =
  emitAtReturning
    (factSourceId @u source)
    (\site -> factAt site source)

facts ::
  forall u f.
  (FactSystem u, Foldable f) =>
  f (SatFactSource u) ->
  ProgramM u [RewriteRuleId]
facts sources =
  let sourceList =
        Foldable.toList sources
      ruleIds =
        fmap (factSourceId @u) sourceList
   in emitAtReturning
        ruleIds
        (\site -> factsAt site sourceList)

activateBaseRewrite ::
  RewriteRuleId ->
  ProgramM u ()
activateBaseRewrite ruleIdValue =
  emitAt $ \site ->
    activateBaseAt site ruleIdValue

activateBaseRewrites ::
  Foldable f =>
  f RewriteRuleId ->
  ProgramM u ()
activateBaseRewrites ruleIds =
  emitAt $ \site ->
    activateBasesAt site ruleIds

-- | Declare whole-program support metadata for a base rewrite.
--
-- This declaration is intentionally not scoped by 'base' or 'context'.
supportBaseRewrite ::
  RewriteRuleId ->
  SupportBasis (SatContext u) ->
  ProgramM u ()
supportBaseRewrite ruleIdValue supportBasis =
  include (supportBaseRewriteFragment ruleIdValue supportBasis)

fragmentFromValues ::
  Foldable f =>
  (value -> ProgramDecl u) ->
  f value ->
  ProgramFragment u
fragmentFromValues declarationOf =
  ProgramFragment
    . Seq.fromList
    . fmap declarationOf
    . Foldable.toList

rewriteAt ::
  MatchSite (SatContext u) ->
  SatRuleSource u ->
  ProgramFragment u
rewriteAt site source =
  singletonProgramDecl (DeclareRewrite site source)

rewritesAt ::
  Foldable f =>
  MatchSite (SatContext u) ->
  f (SatRuleSource u) ->
  ProgramFragment u
rewritesAt site =
  fragmentFromValues (DeclareRewrite site)

factAt ::
  MatchSite (SatContext u) ->
  SatFactSource u ->
  ProgramFragment u
factAt site source =
  singletonProgramDecl (DeclareFact site source)

factsAt ::
  Foldable f =>
  MatchSite (SatContext u) ->
  f (SatFactSource u) ->
  ProgramFragment u
factsAt site =
  fragmentFromValues (DeclareFact site)

activateBaseAt ::
  MatchSite (SatContext u) ->
  RewriteRuleId ->
  ProgramFragment u
activateBaseAt site ruleIdValue =
  singletonProgramDecl (ActivateBaseRewrite site ruleIdValue)

activateBasesAt ::
  Foldable f =>
  MatchSite (SatContext u) ->
  f RewriteRuleId ->
  ProgramFragment u
activateBasesAt site =
  fragmentFromValues (ActivateBaseRewrite site)

supportBaseRewriteFragment ::
  RewriteRuleId ->
  SupportBasis (SatContext u) ->
  ProgramFragment u
supportBaseRewriteFragment ruleIdValue supportBasis =
  singletonProgramDecl
    (DeclareBaseRewriteSupport ruleIdValue supportBasis)

compileProgram ::
  forall u carrier schedulerGroup.
  (RewriteSystem u, FactSystem u, Ord (SatContext u)) =>
  PlanSpec u carrier schedulerGroup ->
  ProgramM u () ->
  Either
    (SaturationCompileError u schedulerGroup)
    (Plan u carrier schedulerGroup)
compileProgram spec =
  compileFragment @u spec . program

compileFragment ::
  forall u carrier schedulerGroup.
  (RewriteSystem u, FactSystem u, Ord (SatContext u)) =>
  PlanSpec u carrier schedulerGroup ->
  ProgramFragment u ->
  Either
    (SaturationCompileError u schedulerGroup)
    (Plan u carrier schedulerGroup)
compileFragment spec fragment = do
  sourceProgram <-
    first SaturationSupportProgramInvalid $
      finishProgram @u fragment
  compileSourceProgram @u spec sourceProgram

-- | Runtime interpreter for compiled application conditions.
-- It owns host-revision evidence and atom-result recording while threading caller
-- state through the selective plan compiled by "Moonlight.Rewrite.Kernel.Condition".
module Moonlight.Rewrite.Runtime.Condition
  ( ApplicationConditionAnchor (..),
    ApplicationConditionEvidence (..),
    evaluateCompiledApplicationConditionWithState,
  )
where

import Control.Monad.Trans.State.Strict
  ( StateT (..),
    runStateT,
  )
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Rewrite.Algebra
  ( ApplicationConditionEffect (..),
    ApplicationConditionPath,
    CompiledApplicationCondition,
    CompiledPatternExtension,
    cpePath,
    runCompiledApplicationConditionPlan,
  )

type ApplicationConditionAnchor :: Type -> Type -> Type
data ApplicationConditionAnchor root subst = ApplicationConditionAnchor
  { acaRoot :: !root,
    acaSubstitution :: !subst
  }
  deriving stock (Eq, Ord, Show)

type ApplicationConditionEvidence :: Type -> Type -> Type
data ApplicationConditionEvidence root subst = ApplicationConditionEvidence
  { aceHostRevision :: {-# UNPACK #-} !Int,
    aceAnchor :: !(ApplicationConditionAnchor root subst),
    aceDecision :: !Bool,
    aceAtomResults :: !(Map ApplicationConditionPath Bool)
  }
  deriving stock (Eq, Ord, Show)

type ApplicationConditionEvaluation :: Type -> Type
data ApplicationConditionEvaluation state = ApplicationConditionEvaluation
  { aceEvalState :: !state,
    aceEvalAtomResults :: !(Map ApplicationConditionPath Bool)
  }

evaluateCompiledApplicationConditionWithState ::
  state ->
  Int ->
  ApplicationConditionAnchor root subst ->
  (state -> ApplicationConditionAnchor root subst -> CompiledPatternExtension compiledGuard f -> Either err (state, Bool)) ->
  CompiledApplicationCondition compiledGuard f ->
  Either err (state, ApplicationConditionEvidence root subst)
evaluateCompiledApplicationConditionWithState initialState hostRevision anchor extensionExists condition = do
  (decision, finalEvaluation) <-
    runStateT
      ( runCompiledApplicationConditionPlan
          (evaluateApplicationConditionEffect extensionExists anchor)
          condition
      )
      ApplicationConditionEvaluation
        { aceEvalState = initialState,
          aceEvalAtomResults = Map.empty
        }

  Right
    ( aceEvalState finalEvaluation,
      ApplicationConditionEvidence
        { aceHostRevision = hostRevision,
          aceAnchor = anchor,
          aceDecision = decision,
          aceAtomResults = aceEvalAtomResults finalEvaluation
        }
    )

evaluateApplicationConditionEffect ::
  (state -> ApplicationConditionAnchor root subst -> CompiledPatternExtension compiledGuard f -> Either err (state, Bool)) ->
  ApplicationConditionAnchor root subst ->
  ApplicationConditionEffect compiledGuard f result ->
  StateT (ApplicationConditionEvaluation state) (Either err) result
evaluateApplicationConditionEffect extensionExists anchor effect =
  StateT $ \evaluation -> do
    (state', exists) <-
      extensionExists
        (aceEvalState evaluation)
        anchor
        (aceExtension effect)

    Right
      ( aceResult effect exists,
        evaluation
          { aceEvalState = state',
            aceEvalAtomResults =
              Map.insert
                (cpePath (aceExtension effect))
                exists
                (aceEvalAtomResults evaluation)
          }
      )

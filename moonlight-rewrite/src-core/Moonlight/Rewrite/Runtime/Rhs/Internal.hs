{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE LambdaCase #-}

module Moonlight.Rewrite.Runtime.Rhs.Internal
  ( RhsTemplateRef (..),
    RhsTemplateInput (..),
    RhsTemplateStep (..),
    RhsTemplate (..),
    RhsStaticPlan,
    rhsStaticPlanTemplate,
    RhsInstantiationSpec (..),
    rhsInstantiationSpecPattern,
    rhsInstantiationSpec,
    compileRhsStaticPlan,
    compileRhsTemplate,
  )
where

import Control.Monad.Trans.State.Strict
  ( State,
    get,
    modify',
    runState,
  )
import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.Kind
  ( Type,
  )
import Moonlight.Core
  ( Pattern (..),
    PatternVar,
    patternVarKey,
  )
import Moonlight.Rewrite.Runtime.PostMatch
  ( PostMatchSubst,
  )

type RhsTemplateRef :: Type
newtype RhsTemplateRef = RhsTemplateRef
  { rhsTemplateRefKey :: Int
  }
  deriving stock (Eq, Ord, Show, Read)

type RhsTemplateInput :: Type
newtype RhsTemplateInput = RhsTemplatePrior
  { rhsTemplatePriorRef :: RhsTemplateRef
  }
  deriving stock (Eq, Ord, Show, Read)

type RhsTemplateStep :: (Type -> Type) -> Type
data RhsTemplateStep f
  = RhsUseVar !RhsTemplateRef !PatternVar
  | RhsConstruct !RhsTemplateRef !(f RhsTemplateInput)

deriving stock instance Eq (f RhsTemplateInput) => Eq (RhsTemplateStep f)

deriving stock instance Ord (f RhsTemplateInput) => Ord (RhsTemplateStep f)

deriving stock instance Show (f RhsTemplateInput) => Show (RhsTemplateStep f)

type RhsTemplate :: (Type -> Type) -> Type
data RhsTemplate f = RhsTemplate
  { rhsTemplateSteps :: ![RhsTemplateStep f],
    rhsTemplateRoot :: !RhsTemplateRef
  }

deriving stock instance Eq (f RhsTemplateInput) => Eq (RhsTemplate f)

deriving stock instance Ord (f RhsTemplateInput) => Ord (RhsTemplate f)

deriving stock instance Show (f RhsTemplateInput) => Show (RhsTemplate f)

type RhsStaticPlan :: (Type -> Type) -> Type
newtype RhsStaticPlan f = RhsStaticPlan
  { rhsStaticPlanTemplate :: RhsTemplate f
  }

deriving stock instance Eq (f RhsTemplateInput) => Eq (RhsStaticPlan f)

deriving stock instance Ord (f RhsTemplateInput) => Ord (RhsStaticPlan f)

deriving stock instance Show (f RhsTemplateInput) => Show (RhsStaticPlan f)

type RhsInstantiationSpec :: (Type -> Type) -> Type
data RhsInstantiationSpec f
  = StaticRhs !(Pattern f) !(RhsStaticPlan f)
  | PostMatchRhs !(PostMatchSubst f) !(Pattern f)

deriving stock instance
  (Eq (Pattern f), Eq (PostMatchSubst f), Eq (f RhsTemplateInput)) =>
  Eq (RhsInstantiationSpec f)

deriving stock instance
  (Ord (Pattern f), Ord (PostMatchSubst f), Ord (f RhsTemplateInput)) =>
  Ord (RhsInstantiationSpec f)

deriving stock instance
  (Show (Pattern f), Show (PostMatchSubst f), Show (f RhsTemplateInput)) =>
  Show (RhsInstantiationSpec f)

rhsInstantiationSpecPattern :: RhsInstantiationSpec f -> Pattern f
rhsInstantiationSpecPattern =
  \case
    StaticRhs rhsPattern _ ->
      rhsPattern

    PostMatchRhs _ rhsPattern ->
      rhsPattern

rhsInstantiationSpec ::
  Traversable f =>
  Maybe (PostMatchSubst f) ->
  Pattern f ->
  RhsInstantiationSpec f
rhsInstantiationSpec maybePostMatchSubst rhsPattern =
  case maybePostMatchSubst of
    Nothing ->
      StaticRhs rhsPattern (compileRhsStaticPlan rhsPattern)

    Just postMatchSubst ->
      PostMatchRhs postMatchSubst rhsPattern

type TemplateBuilder :: (Type -> Type) -> Type
data TemplateBuilder f = TemplateBuilder
  { tbNextRef :: !Int,
    tbVarRefs :: !(IntMap RhsTemplateRef),
    tbStepsRev :: ![RhsTemplateStep f]
  }

emptyTemplateBuilder :: TemplateBuilder f
emptyTemplateBuilder =
  TemplateBuilder
    { tbNextRef = 0,
      tbVarRefs = IntMap.empty,
      tbStepsRev = []
    }

freshTemplateRef :: State (TemplateBuilder f) RhsTemplateRef
freshTemplateRef = do
  builder <- get
  let ref =
        RhsTemplateRef (tbNextRef builder)
  modify'
    ( \stateValue ->
        stateValue
          { tbNextRef = tbNextRef stateValue + 1
          }
    )
  pure ref

emitTemplateStep :: RhsTemplateStep f -> State (TemplateBuilder f) ()
emitTemplateStep step =
  modify'
    ( \stateValue ->
        stateValue
          { tbStepsRev = step : tbStepsRev stateValue
          }
    )

compileRhsTemplate ::
  Traversable f =>
  Pattern f ->
  RhsTemplate f
compileRhsTemplate rhsPattern =
  let (rootRef, finalBuilder) =
        runState
          (compilePatternRef rhsPattern)
          emptyTemplateBuilder
   in RhsTemplate
        { rhsTemplateSteps = reverse (tbStepsRev finalBuilder),
          rhsTemplateRoot = rootRef
        }

compileRhsStaticPlan ::
  Traversable f =>
  Pattern f ->
  RhsStaticPlan f
compileRhsStaticPlan =
  RhsStaticPlan . compileRhsTemplate

compilePatternRef ::
  Traversable f =>
  Pattern f ->
  State (TemplateBuilder f) RhsTemplateRef
compilePatternRef =
  \case
    PatternVar patternVar -> do
      builder <- get
      case IntMap.lookup (patternVarKey patternVar) (tbVarRefs builder) of
        Just existingRef ->
          pure existingRef

        Nothing -> do
          ref <- freshTemplateRef
          emitTemplateStep (RhsUseVar ref patternVar)
          modify'
            ( \stateValue ->
                stateValue
                  { tbVarRefs =
                      IntMap.insert
                        (patternVarKey patternVar)
                        ref
                        (tbVarRefs stateValue)
                  }
            )
          pure ref

    PatternNode patternNode -> do
      childRefs <-
        traverse compilePatternRef patternNode
      ref <- freshTemplateRef
      emitTemplateStep (RhsConstruct ref (RhsTemplatePrior <$> childRefs))
      pure ref

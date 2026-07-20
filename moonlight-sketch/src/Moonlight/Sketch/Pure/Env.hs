module Moonlight.Sketch.Pure.Env
  ( SchemaEnv (..),
    schemaEnvRefinementsLens,
    schemaEnvTransformsLens,
    schemaEnvPreprocessorsLens,
    schemaEnvConstraintsLens,
    emptySchemaEnv,
    lookupRefinement,
    lookupTransform,
    lookupPreprocessor,
    lookupConstraint,
    addRefinement,
    addTransform,
    addPreprocessor,
    addConstraint,
    mergeSchemaEnv,
  )
where

import Data.Aeson (Value)
import Data.Kind (Type)
import qualified Data.Map.Strict as Map
import Moonlight.Optics (Lens', lens, over)
import Moonlight.Sketch.Pure.Types
  ( ConstraintId,
    PreprocessId,
    RefinementId,
    SchemaHookContext,
    SchemaIssue,
    TransformFns,
    TransformId,
  )

type SchemaEnv :: Type
data SchemaEnv = SchemaEnv
  { seRefinements :: Map.Map RefinementId (SchemaHookContext -> Value -> [SchemaIssue]),
    seTransforms :: Map.Map TransformId TransformFns,
    sePreprocessors :: Map.Map PreprocessId (SchemaHookContext -> Value -> Value),
    seConstraints :: Map.Map ConstraintId (SchemaHookContext -> Value -> [SchemaIssue])
  }

schemaEnvRefinementsLens :: Lens' SchemaEnv (Map.Map RefinementId (SchemaHookContext -> Value -> [SchemaIssue]))
schemaEnvRefinementsLens = lens seRefinements (\env updated -> env {seRefinements = updated})

schemaEnvTransformsLens :: Lens' SchemaEnv (Map.Map TransformId TransformFns)
schemaEnvTransformsLens = lens seTransforms (\env updated -> env {seTransforms = updated})

schemaEnvPreprocessorsLens :: Lens' SchemaEnv (Map.Map PreprocessId (SchemaHookContext -> Value -> Value))
schemaEnvPreprocessorsLens = lens sePreprocessors (\env updated -> env {sePreprocessors = updated})

schemaEnvConstraintsLens :: Lens' SchemaEnv (Map.Map ConstraintId (SchemaHookContext -> Value -> [SchemaIssue]))
schemaEnvConstraintsLens = lens seConstraints (\env updated -> env {seConstraints = updated})

emptySchemaEnv :: SchemaEnv
emptySchemaEnv =
  SchemaEnv
    { seRefinements = Map.empty,
      seTransforms = Map.empty,
      sePreprocessors = Map.empty,
      seConstraints = Map.empty
    }

lookupRefinement :: RefinementId -> SchemaEnv -> Maybe (SchemaHookContext -> Value -> [SchemaIssue])
lookupRefinement refinementId env = Map.lookup refinementId (seRefinements env)

lookupTransform :: TransformId -> SchemaEnv -> Maybe TransformFns
lookupTransform transformId env = Map.lookup transformId (seTransforms env)

lookupPreprocessor :: PreprocessId -> SchemaEnv -> Maybe (SchemaHookContext -> Value -> Value)
lookupPreprocessor preprocessId env = Map.lookup preprocessId (sePreprocessors env)

lookupConstraint :: ConstraintId -> SchemaEnv -> Maybe (SchemaHookContext -> Value -> [SchemaIssue])
lookupConstraint constraintId env = Map.lookup constraintId (seConstraints env)

addRefinement :: RefinementId -> (SchemaHookContext -> Value -> [SchemaIssue]) -> SchemaEnv -> SchemaEnv
addRefinement refinementId refinementFn =
  over schemaEnvRefinementsLens (Map.insert refinementId refinementFn)

addTransform :: TransformId -> TransformFns -> SchemaEnv -> SchemaEnv
addTransform transformId transformFns =
  over schemaEnvTransformsLens (Map.insert transformId transformFns)

addPreprocessor :: PreprocessId -> (SchemaHookContext -> Value -> Value) -> SchemaEnv -> SchemaEnv
addPreprocessor preprocessId preprocessFn =
  over schemaEnvPreprocessorsLens (Map.insert preprocessId preprocessFn)

addConstraint :: ConstraintId -> (SchemaHookContext -> Value -> [SchemaIssue]) -> SchemaEnv -> SchemaEnv
addConstraint constraintId constraintFn =
  over schemaEnvConstraintsLens (Map.insert constraintId constraintFn)

mergeSchemaEnv :: SchemaEnv -> SchemaEnv -> SchemaEnv
mergeSchemaEnv left right =
  SchemaEnv
    { seRefinements = Map.union (seRefinements left) (seRefinements right),
      seTransforms = Map.union (seTransforms left) (seTransforms right),
      sePreprocessors = Map.union (sePreprocessors left) (sePreprocessors right),
      seConstraints = Map.union (seConstraints left) (seConstraints right)
    }

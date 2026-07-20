module Moonlight.Sketch.Pure.Validate
  ( validate,
    validateWith,
    validateWithCached,
    validateWithCompiled,
    validateNode,
    validateWithCompiledCached,
    validateNodeWithCompiled,
    validateNodeWithCompiledCached,
    CachePolicy,
    CacheInterpreter,
    EvictionPolicy (..),
    MetricsPolicy (..),
    CacheMetrics (..),
    defaultCachePolicy,
    mkCachePolicy,
    mkCachePolicyWithInterpreters,
    unboundedInterpreter,
    boundedLruInterpreter,
    mkCompiledSchemaCache,
    CompiledSchemaEnv,
    compileSchemaEnv,
    CompiledSchemaCache,
    emptyCompiledSchemaCache,
    compileSchemaEnvCached,
    compiledSchemaCacheSizes,
    compiledSchemaCacheMetrics,
    validateString,
    validateNumber,
    validateArray,
    validateObject,
  )
where

import Data.Aeson (Value)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Moonlight.Sketch.Pure.Env (SchemaEnv, emptySchemaEnv)
import Moonlight.Sketch.Pure.Normalize (normalize)
import Moonlight.Sketch.Pure.Types
  ( ArrayConstraint,
    ObjectProperty (..),
    ObjectPropertyF (..),
    PathSegment,
    RefId,
    SchemaIssue,
    SchemaNode (..),
    SchemaRegistry,
    emptySchemaRegistry,
  )
import Moonlight.Sketch.Pure.Validate.Algebra
  ( validateArrayWith,
    validateNumber,
    validateObjectWith,
    validateString,
  )
import Moonlight.Sketch.Pure.Validate.Compile
  ( CachePolicy,
    CompiledSchemaCache,
    CompiledSchemaEnv,
    buildValidatorRuntime,
    buildValidatorRuntimeCached,
    boundedLruInterpreter,
    compileSchemaEnv,
    compileSchemaEnvCached,
    compiledSchemaCacheMetrics,
    compiledSchemaCacheSizes,
    defaultCachePolicy,
    emptyCompiledSchemaCache,
    mkCachePolicy,
    mkCachePolicyWithInterpreters,
    mkCompiledSchemaCache,
    resolveCompiled,
    unboundedInterpreter,
  )
import Moonlight.Sketch.Pure.Validate.Core
  ( ValidationContext (..),
    ValidatorRuntime,
    applyValidationRule,
    lookupRuntimeValidator,
  )
import Moonlight.Sketch.Pure.Validate.Cache
  ( CacheInterpreter,
    CacheMetrics (..),
    EvictionPolicy (..),
    MetricsPolicy (..),
  )

validate :: SchemaNode -> Value -> [SchemaIssue]
validate = validateWith emptySchemaEnv emptySchemaRegistry

validateWith :: SchemaEnv -> SchemaRegistry -> SchemaNode -> Value -> [SchemaIssue]
validateWith env registry node value =
  let compiledSchemaEnv = compileSchemaEnv registry
   in validateWithCompiled env compiledSchemaEnv node value

validateWithCached ::
  SchemaEnv ->
  CompiledSchemaCache ->
  SchemaRegistry ->
  SchemaNode ->
  Value ->
  ([SchemaIssue], CompiledSchemaCache)
validateWithCached env compiledSchemaCache registry node value =
  let (compiledSchemaEnv, updatedCache) =
        compileSchemaEnvCached registry compiledSchemaCache
   in validateWithCompiledCached env updatedCache compiledSchemaEnv node value

validateWithCompiled :: SchemaEnv -> CompiledSchemaEnv -> SchemaNode -> Value -> [SchemaIssue]
validateWithCompiled env compiledSchemaEnv node value =
  validateNodeWithCompiled env compiledSchemaEnv Set.empty [] node value

validateWithCompiledCached ::
  SchemaEnv ->
  CompiledSchemaCache ->
  CompiledSchemaEnv ->
  SchemaNode ->
  Value ->
  ([SchemaIssue], CompiledSchemaCache)
validateWithCompiledCached env compiledSchemaCache compiledSchemaEnv node value =
  validateNodeWithCompiledCached
    env
    compiledSchemaCache
    compiledSchemaEnv
    Set.empty
    []
    node
    value

validateNode ::
  SchemaEnv ->
  SchemaRegistry ->
  Set.Set RefId ->
  [PathSegment] ->
  SchemaNode ->
  Value ->
  [SchemaIssue]
validateNode env registry visited path node value =
  let compiledSchemaEnv = compileSchemaEnv registry
   in validateNodeWithCompiled env compiledSchemaEnv visited path node value

validateNodeWithCompiled ::
  SchemaEnv ->
  CompiledSchemaEnv ->
  Set.Set RefId ->
  [PathSegment] ->
  SchemaNode ->
  Value ->
  [SchemaIssue]
validateNodeWithCompiled env compiledSchemaEnv visited path node value =
  let normalizedNode = normalize (resolveCompiled compiledSchemaEnv node)
      validatorRuntime = buildValidatorRuntime compiledSchemaEnv normalizedNode
   in validateNodeWithRuntime validatorRuntime env visited path normalizedNode value

validateNodeWithCompiledCached ::
  SchemaEnv ->
  CompiledSchemaCache ->
  CompiledSchemaEnv ->
  Set.Set RefId ->
  [PathSegment] ->
  SchemaNode ->
  Value ->
  ([SchemaIssue], CompiledSchemaCache)
validateNodeWithCompiledCached env compiledSchemaCache compiledSchemaEnv visited path node value =
  let normalizedNode = normalize (resolveCompiled compiledSchemaEnv node)
      (validatorRuntime, updatedCache) =
        buildValidatorRuntimeCached compiledSchemaCache compiledSchemaEnv normalizedNode
      issues =
        validateNodeWithRuntime validatorRuntime env visited path normalizedNode value
   in (issues, updatedCache)

validateNodeWithRuntime ::
  ValidatorRuntime ->
  SchemaEnv ->
  Set.Set RefId ->
  [PathSegment] ->
  SchemaNode ->
  Value ->
  [SchemaIssue]
validateNodeWithRuntime validatorRuntime env visited path node value =
  applyValidationRule
    (lookupRuntimeValidator validatorRuntime node)
    ( ValidationContext
        { vcEnv = env,
          vcVisited = visited,
          vcPath = path
        }
    )
    value

validateArray ::
  SchemaEnv ->
  SchemaRegistry ->
  Set.Set RefId ->
  [PathSegment] ->
  SchemaNode ->
  Maybe ArrayConstraint ->
  [Value] ->
  [SchemaIssue]
validateArray env registry visited path elementSchema constraint elements =
  let context =
        ValidationContext
          { vcEnv = env,
            vcVisited = visited,
            vcPath = path
          }
      compiledSchemaEnv = compileSchemaEnv registry
      validatorRuntime = buildValidatorRuntime compiledSchemaEnv (SArray elementSchema constraint)
   in validateArrayWith context (lookupRuntimeValidator validatorRuntime elementSchema) constraint elements

validateObject ::
  SchemaEnv ->
  SchemaRegistry ->
  Set.Set RefId ->
  [PathSegment] ->
  Map.Map Text ObjectProperty ->
  KeyMap.KeyMap Value ->
  [SchemaIssue]
validateObject env registry visited path fieldSchemas keyValues =
  let context =
        ValidationContext
          { vcEnv = env,
            vcVisited = visited,
            vcPath = path
          }
      compiledSchemaEnv = compileSchemaEnv registry
      validatorRuntime = buildValidatorRuntime compiledSchemaEnv (SObject fieldSchemas)
      fieldValidators =
        Map.map
          (\propertyValue ->
             ObjectPropertyF
               { opfRequired = opRequired propertyValue,
                 opfReadonly = opReadonly propertyValue,
                 opfSchema =
                   ( opSchema propertyValue,
                     lookupRuntimeValidator validatorRuntime (opSchema propertyValue)
                   )
               }
          )
          fieldSchemas
   in validateObjectWith context fieldValidators keyValues

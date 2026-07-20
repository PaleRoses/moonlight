module Moonlight.Sketch.Pure.Validate.Compile.Coalgebra
  ( compileSchemaEnv,
    compileSchemaEnvCached,
    resolveCompiled,
    buildValidatorRuntime,
    buildValidatorRuntimeCached,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Moonlight.Sketch.Pure.Resolve (resolve)
import Moonlight.Sketch.Pure.Types
  ( SchemaNode,
    SchemaRegistry (..),
    paraSchema,
  )
import Moonlight.Sketch.Pure.Validate.Algebra (unresolvedRefValidator, validateAlgebra)
import Moonlight.Sketch.Pure.Validate.Compile.Algebra
  ( buildRefComponentGraph,
    mkRegistryIdentity,
  )
import Moonlight.Sketch.Pure.Validate.Compile.Core
  ( CachePolicy (..),
    CompiledSchemaCache (..),
    CompiledSchemaEnv (..),
    NormalForm (..),
    RefComponentGraph (..),
    RegistryIdentity (..),
    RootRuntimeKey (..),
    SchemaIdentity (..),
    TypedCacheFilter (..),
    TypedCacheOps (..),
    ValidatorCache (..),
    lookupValidatorCache,
    mkSchemaIdentity,
    tickCache,
  )
import Moonlight.Sketch.Pure.Validate.Core
  ( RefValidatorLookup,
    Validator,
    ValidatorRuntime (..),
  )

compileSchemaEnv :: SchemaRegistry -> CompiledSchemaEnv
compileSchemaEnv = compileSchemaEnvFromIdentity . mkRegistryIdentity

compileSchemaEnvCached ::
  SchemaRegistry ->
  CompiledSchemaCache ->
  (CompiledSchemaEnv, CompiledSchemaCache)
compileSchemaEnvCached registry compiledSchemaCache =
  let registryIdentity = mkRegistryIdentity registry
      (tickValue, tickedCache) = tickCache compiledSchemaCache
      envInterpreter = cpCompiledEnvInterpreter (cscPolicy tickedCache)
      runtimeInterpreter = cpRootRuntimeInterpreter (cscPolicy tickedCache)
      (foundCompiledSchemaEnv, touchedCompiledEnvCache) =
        tcLookup envInterpreter tickValue registryIdentity (cscCompiledEnvCache tickedCache)
   in case foundCompiledSchemaEnv of
        Just compiledEnv ->
          (compiledEnv, tickedCache {cscCompiledEnvCache = touchedCompiledEnvCache})
        Nothing ->
          let compiledEnv = compileSchemaEnvFromIdentity registryIdentity
              (insertedCompiledEnvCache, evictedRegistryIdentities) =
                tcInsert
                  envInterpreter
                  tickValue
                  registryIdentity
                  compiledEnv
                  touchedCompiledEnvCache
              filteredRootRuntimeCache =
                if Set.null evictedRegistryIdentities
                  then cscRootRuntimeCache tickedCache
                  else
                    tcDeleteWhere
                      runtimeInterpreter
                      (\rootRuntimeKey ->
                         Set.member
                           (rrkRegistryIdentity rootRuntimeKey)
                           evictedRegistryIdentities
                      )
                      (cscRootRuntimeCache tickedCache)
              updatedCache =
                tickedCache
                  { cscCompiledEnvCache = insertedCompiledEnvCache,
                    cscRootRuntimeCache = filteredRootRuntimeCache
                  }
           in (compiledEnv, updatedCache)

compileSchemaEnvFromIdentity :: RegistryIdentity -> CompiledSchemaEnv
compileSchemaEnvFromIdentity registryIdentity = compiledEnv
  where
    definitions = unRegistryIdentity registryIdentity
    componentGraph = buildRefComponentGraph definitions
    componentValidators =
      Map.mapWithKey (\componentId _ -> compileComponent componentId) (rcgMembersByComponent componentGraph)
    lookupRefValidator refId =
      Map.lookup refId (rcgComponentByRef componentGraph)
        >>= \componentId -> Map.lookup componentId componentValidators
        >>= Map.lookup refId
    compileComponent componentId =
      case Map.lookup componentId (rcgMembersByComponent componentGraph) of
        Nothing -> Map.empty
        Just componentMembers ->
          let memberIds = Set.toList componentMembers
              localRefLookup refId =
                case Map.lookup refId localValidators of
                  Just validator -> Just validator
                  Nothing -> lookupRefValidator refId
              localValidators =
                Map.fromList
                  ( map
                      (\refId -> (refId, compileRefValidator localRefLookup refId))
                      memberIds
                  )
           in localValidators
    compileRefValidator localRefLookup refId =
      case Map.lookup refId (rcgDefinitions componentGraph) of
        Nothing -> unresolvedRefValidator refId
        Just schemaIdentity ->
          compileSchemaValidator localRefLookup schemaIdentity
    compiledEnv =
      CompiledSchemaEnv
        { cseRegistryIdentity = registryIdentity,
          cseRegistry = SchemaRegistry (Map.map (unNormalForm . siNormalForm) definitions),
          cseComponentGraph = componentGraph,
          cseComponentValidators = componentValidators
        }

resolveCompiled :: CompiledSchemaEnv -> SchemaNode -> SchemaNode
resolveCompiled compiledEnv = resolve (cseRegistry compiledEnv)

buildValidatorRuntime :: CompiledSchemaEnv -> SchemaNode -> ValidatorRuntime
buildValidatorRuntime compiledEnv rootNode = validatorRuntime
  where
    rootIdentity = mkSchemaIdentity rootNode
    rootCache =
      ValidatorCache
        (Map.singleton rootIdentity (compileSchemaValidator (lookupCompiledRefValidator compiledEnv) rootIdentity))
    lookupSchemaValidator schemaNode =
      let schemaIdentity = mkSchemaIdentity schemaNode
       in case lookupValidatorCache rootCache schemaIdentity of
            Just validator -> validator
            Nothing -> compileSchemaValidator (lookupCompiledRefValidator compiledEnv) schemaIdentity
    validatorRuntime =
      ValidatorRuntime
        { vrSchemaLookup = lookupSchemaValidator
        }

buildValidatorRuntimeCached ::
  CompiledSchemaCache ->
  CompiledSchemaEnv ->
  SchemaNode ->
  (ValidatorRuntime, CompiledSchemaCache)
buildValidatorRuntimeCached compiledSchemaCache compiledEnv rootNode =
  let rootRuntimeKey =
        RootRuntimeKey
          { rrkRegistryIdentity = cseRegistryIdentity compiledEnv,
            rrkSchemaIdentity = mkSchemaIdentity rootNode
          }
      (tickValue, tickedCache) = tickCache compiledSchemaCache
      runtimeInterpreter = cpRootRuntimeInterpreter (cscPolicy tickedCache)
      (foundRuntime, touchedRootRuntimeCache) =
        tcLookup runtimeInterpreter tickValue rootRuntimeKey (cscRootRuntimeCache tickedCache)
   in case foundRuntime of
        Just validatorRuntime ->
          (validatorRuntime, tickedCache {cscRootRuntimeCache = touchedRootRuntimeCache})
        Nothing ->
          let validatorRuntime = buildValidatorRuntime compiledEnv rootNode
              (insertedRootRuntimeCache, _) =
                tcInsert
                  runtimeInterpreter
                  tickValue
                  rootRuntimeKey
                  validatorRuntime
                  touchedRootRuntimeCache
              updatedCache =
                tickedCache
                  { cscRootRuntimeCache = insertedRootRuntimeCache
                  }
           in (validatorRuntime, updatedCache)

lookupCompiledRefValidator :: CompiledSchemaEnv -> RefValidatorLookup
lookupCompiledRefValidator compiledEnv refId =
  Map.lookup refId (rcgComponentByRef (cseComponentGraph compiledEnv))
    >>= \componentId -> Map.lookup componentId (cseComponentValidators compiledEnv)
    >>= Map.lookup refId

compileSchemaValidator :: RefValidatorLookup -> SchemaIdentity -> Validator
compileSchemaValidator refLookup schemaIdentity =
  paraSchema
    (validateAlgebra refLookup)
    (unNormalForm (siNormalForm schemaIdentity))

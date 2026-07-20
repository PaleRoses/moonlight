module Moonlight.Sketch.ValidateSpec
  ( tests,
  )
where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Vector as Vector
import Moonlight.Sketch
  ( ArrayConstraint (..),
    BrandName,
    CacheMetrics (..),
    ConstraintId,
    MetricsPolicy (..),
    LiteralValue (..),
    ObjectProperty (..),
    PathSegment (..),
    RefId,
    SchemaHookContext (..),
    SchemaIssue (..),
    SchemaProblem (..),
    SchemaRegistry (..),
    SchemaNode (..),
    StringConstraint (..),
    addConstraint,
    boundedLruInterpreter,
    compileSchemaEnv,
    compileSchemaEnvCached,
    compiledSchemaCacheMetrics,
    compiledSchemaCacheSizes,
    defaultCachePolicy,
    emptyCompiledSchemaCache,
    emptySchemaEnv,
    emptySchemaRegistry,
    mkBrandName,
    mkCachePolicy,
    mkCachePolicyWithInterpreters,
    mkCompiledSchemaCache,
    mkConstraintId,
    mkRefId,
    unboundedInterpreter,
    validate,
    validateWith,
    validateWithCached,
    validateWithCompiled,
    validateWithCompiledCached,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

tests :: TestTree
tests =
  testGroup
    "validate"
    [ testCase "bool validates bool" $
        validate SBool (Bool True) @?= [],
      testCase "string validates string" $
        validate (SString Nothing Nothing) (String "hello") @?= [],
      testCase "type mismatch yields issues" $
        null (validate SBool (String "x")) @?= False,
      testCase "undefined is not JSON null" $
        null (validate SUndefined Null) @?= False,
      testCase "optional root does not consume JSON null" $
        null (validate (SOptional SBool) Null) @?= False,
      testCase "nullable root consumes JSON null" $
        validate (SNullable SBool) Null @?= [],
      testCase "optional object field is represented by absence" $
        let schema =
              SObject
                (Map.fromList [("flag", ObjectProperty False False SBool)])
            value = Object KeyMap.empty
         in validate schema value @?= [],
      testCase "required field missing" $
        let schema =
              SObject
                (Map.fromList [("name", ObjectProperty True False (SString Nothing Nothing))])
            value = Object KeyMap.empty
         in null (validate schema value) @?= False,
      testCase "array length constraint" $
        let schema =
              SArray
                SBool
                (Just (ArrayConstraint {acMinLength = Just 2, acMaxLength = Nothing, acExactLength = Nothing}))
            value = Array (Vector.fromList [Bool True])
         in null (validate schema value) @?= False,
      testCase "string pattern constraint" $
        let schema = SString (Just (StringConstraint Nothing Nothing (Just "^[a-z]+$"))) Nothing
         in validate schema (String "abc") @?= [],
      testCase "union validates any member" $
        let schema = SUnion [SBool, SLiteral (LitString "ok")]
         in validate schema (String "ok") @?= [],
      testCase "discriminated union branch selection" $
        let memberA = SObject (Map.fromList [("kind", ObjectProperty True False (SLiteral (LitString "a"))), ("value", ObjectProperty True False SBool)])
            memberB = SObject (Map.fromList [("kind", ObjectProperty True False (SLiteral (LitString "b"))), ("value", ObjectProperty True False (SString Nothing Nothing))])
            schema = SDiscriminatedUnion "kind" [memberA, memberB]
            value = Object (KeyMap.fromList [(Key.fromText "kind", String "b"), (Key.fromText "value", String "hello")])
         in validate schema value @?= [],
      testCase "identifier constructors reject invalid tokens" $
        let invalidBrandName = mkBrandName "brand name"
            invalidRefId = mkRefId "missing/slash"
         in (invalidBrandName, invalidRefId) @?= (Nothing :: Maybe BrandName, Nothing :: Maybe RefId),
      testCase "recursive reference terminates with cycle issue" $
        let refId = requiredRefId "node"
            schema = SRef refId
            registry =
              SchemaRegistry
                (Map.fromList [(refId, SObject (Map.fromList [("next", ObjectProperty False False (SRef refId))]))])
            value =
              Object
                ( KeyMap.fromList
                    [ ( Key.fromText "next",
                        Object
                          (KeyMap.fromList [(Key.fromText "next", Object KeyMap.empty)])
                      )
                    ]
                )
         in null (validateWith emptySchemaEnv registry schema value) @?= False,
      testCase "compiled schema env preserves validation semantics" $
        let registry = SchemaRegistry (Map.fromList [(requiredRefId "flag", SBool)])
            schema = SRef (requiredRefId "flag")
            compiledSchemaEnv = compileSchemaEnv registry
            value = Bool True
         in validateWithCompiled emptySchemaEnv compiledSchemaEnv schema value
              @?= validateWith emptySchemaEnv registry schema value,
      testCase "compiled schema env cache returns reusable artifact" $
        let registry = SchemaRegistry (Map.fromList [(requiredRefId "name", SString Nothing Nothing)])
            schema = SRef (requiredRefId "name")
            (compiledA, cacheA) = compileSchemaEnvCached registry emptyCompiledSchemaCache
            (compiledB, _) = compileSchemaEnvCached registry cacheA
            value = String "ember"
         in validateWithCompiled emptySchemaEnv compiledA schema value
              @?= validateWithCompiled emptySchemaEnv compiledB schema value,
      testCase "root runtime cache reuses compiled runtime key" $
        let registry = SchemaRegistry (Map.fromList [(requiredRefId "name", SString Nothing Nothing)])
            schema = SRef (requiredRefId "name")
            value = String "ember"
            (issuesA, cacheA) =
              validateWithCached emptySchemaEnv emptyCompiledSchemaCache registry schema value
            (issuesB, _) =
              validateWithCached emptySchemaEnv cacheA registry schema value
         in issuesA @?= issuesB,
      testCase "compiled + runtime cache path matches direct compiled validation" $
        let registry = SchemaRegistry (Map.fromList [(requiredRefId "flag", SBool)])
            schema = SRef (requiredRefId "flag")
            value = Bool True
            compiledSchemaEnv = compileSchemaEnv registry
            (cachedIssues, _) =
              validateWithCompiledCached
                emptySchemaEnv
                emptyCompiledSchemaCache
                compiledSchemaEnv
                schema
                value
         in cachedIssues @?= validateWithCompiled emptySchemaEnv compiledSchemaEnv schema value,
      testCase "fresh bounded cache starts empty" $
        compiledSchemaCacheSizes (mkCompiledSchemaCache defaultCachePolicy) @?= (0, 0),
      testCase "bounded cache enforces env/runtime capacities" $
        let boundedCache = mkCompiledSchemaCache (mkCachePolicy 1 1)
            registryA = SchemaRegistry (Map.fromList [(requiredRefId "a", SBool)])
            registryB = SchemaRegistry (Map.fromList [(requiredRefId "b", SString Nothing Nothing)])
            schemaA = SRef (requiredRefId "a")
            schemaB = SRef (requiredRefId "b")
            valueA = Bool True
            valueB = String "rose"
            (_, cacheA) =
              validateWithCached emptySchemaEnv boundedCache registryA schemaA valueA
            (_, cacheB) =
              validateWithCached emptySchemaEnv cacheA registryB schemaB valueB
         in compiledSchemaCacheSizes cacheB @?= (1, 1),
      testCase "unbounded interpreters retain all entries" $
        let unboundedPolicy =
              mkCachePolicyWithInterpreters
                (unboundedInterpreter MetricsDisabled)
                (unboundedInterpreter MetricsDisabled)
            unboundedCache = mkCompiledSchemaCache unboundedPolicy
            registryA = SchemaRegistry (Map.fromList [(requiredRefId "a", SBool)])
            registryB = SchemaRegistry (Map.fromList [(requiredRefId "b", SString Nothing Nothing)])
            schemaA = SRef (requiredRefId "a")
            schemaB = SRef (requiredRefId "b")
            valueA = Bool True
            valueB = String "rose"
            (_, cacheA) =
              validateWithCached emptySchemaEnv unboundedCache registryA schemaA valueA
            (_, cacheB) =
              validateWithCached emptySchemaEnv cacheA registryB schemaB valueB
         in compiledSchemaCacheSizes cacheB @?= (2, 2),
      testCase "metrics-enabled interpreters record hits and misses" $
        let metricsPolicy =
              mkCachePolicyWithInterpreters
                (boundedLruInterpreter 8 MetricsEnabled)
                (boundedLruInterpreter 8 MetricsEnabled)
            metricsCache = mkCompiledSchemaCache metricsPolicy
            registry = SchemaRegistry (Map.fromList [(requiredRefId "flag", SBool)])
            schema = SRef (requiredRefId "flag")
            value = Bool True
            (_, cacheA) =
              validateWithCached emptySchemaEnv metricsCache registry schema value
            (_, cacheB) =
              validateWithCached emptySchemaEnv cacheA registry schema value
            (envMetrics, runtimeMetrics) = compiledSchemaCacheMetrics cacheB
         in (cmHits envMetrics > 0, cmHits runtimeMetrics > 0, cmMisses envMetrics > 0, cmMisses runtimeMetrics > 0)
              @?= (True, True, True, True),
      testCase "constraint hook receives the validation path" $
        let constraintId = requiredConstraintId "field_constraint"
            schema =
              SObject
                (Map.fromList [("name", ObjectProperty True False (SConstrain (SString Nothing Nothing) constraintId))])
            env =
              addConstraint
                constraintId
                (\hookContext _ -> [SchemaIssue (shcPath hookContext) StringTooShort])
                emptySchemaEnv
            value = Object (KeyMap.fromList [(Key.fromText "name", String "rose")])
         in validateWith env emptySchemaRegistry schema value
              @?= [SchemaIssue [FieldSegment "name"] StringTooShort]
    ]

requiredRefId :: Text -> RefId
requiredRefId =
  requiredIdentifier mkRefId

requiredConstraintId :: Text -> ConstraintId
requiredConstraintId =
  requiredIdentifier mkConstraintId

requiredIdentifier :: (Text -> Maybe identifier) -> Text -> identifier
requiredIdentifier mkIdentifier rawIdentifier =
  case mkIdentifier rawIdentifier of
    Just identifier -> identifier
    Nothing -> error "expected valid validate test identifier"

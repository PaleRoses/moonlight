module Moonlight.Sketch.Pure.Validate.Algebra.Core
  ( validateAlgebra,
    validateReference,
    unresolvedRefValidator,
    unresolvedRefMessage,
    cyclicRefMessage,
    issue,
  )
where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Map.Strict as Map
import qualified Data.Scientific as Scientific
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Vector as Vector
import Moonlight.Core (canonicalNumberFromDouble)
import Moonlight.Sketch.Pure.Env
  ( lookupConstraint,
    lookupPreprocessor,
    lookupRefinement,
    lookupTransform,
  )
import Moonlight.Sketch.Pure.Types
  ( CanonicalNumber,
    LiteralValue (..),
    ObjectProperty (..),
    PathSegment,
    RefId,
    SchemaHookContext (..),
    SchemaF (..),
    SchemaIssue (..),
    SchemaNode (..),
    SchemaProblem (..),
    TransformFns (..),
    siMessage,
  )
import Moonlight.Sketch.Pure.Validate.Algebra.Composite
  ( validateArrayWith,
    validateObjectWith,
    validateRecordWith,
    validateTupleWith,
  )
import Moonlight.Sketch.Pure.Validate.Algebra.Primitive
  ( collectDetachedRuleIssues,
    issue,
    issueIf,
    validateNumber,
    validateString,
  )
import Moonlight.Sketch.Pure.Validate.Core
  ( RefValidatorLookup,
    ValidationContext (..),
    ValidationRule (..),
    Validator,
  )

typeGuard ::
  (Value -> Maybe a) ->
  SchemaProblem ->
  (ValidationContext -> a -> [SchemaIssue]) ->
  Validator
typeGuard extract mismatchProblem validate =
  ValidationRule
    (\context value ->
       case extract value of
         Just extracted -> validate context extracted
         Nothing -> [issue (vcPath context) mismatchProblem])

nullablePassthrough :: Validator -> Validator
nullablePassthrough innerValidator =
  ValidationRule
    (\context value ->
       case value of
         Null -> []
         _ -> applyValidationRule innerValidator context value)

extractBool :: Value -> Maybe Bool
extractBool (Bool b) = Just b
extractBool _ = Nothing

extractNull :: Value -> Maybe ()
extractNull Null = Just ()
extractNull _ = Nothing

extractString :: Value -> Maybe Text
extractString (String t) = Just t
extractString _ = Nothing

extractNumber :: Value -> Maybe Double
extractNumber (Number n) = Just (Scientific.toRealFloat n)
extractNumber _ = Nothing

extractArray :: Value -> Maybe [Value]
extractArray (Array v) = Just (Vector.toList v)
extractArray _ = Nothing

extractObject :: Value -> Maybe (KeyMap.KeyMap Value)
extractObject (Object km) = Just km
extractObject _ = Nothing

validateAlgebra :: RefValidatorLookup -> SchemaF (SchemaNode, Validator) -> Validator
validateAlgebra refLookup layer =
  case layer of
    SUnknownF -> mempty
    SVoidF ->
      ValidationRule
        (\context _ -> [issue (vcPath context) NoValueConformsToVoid])
    SBoolF -> typeGuard extractBool ExpectedBoolean (\_ _ -> [])
    SNullF -> typeGuard extractNull ExpectedNull (\_ _ -> [])
    SUndefinedF ->
      ValidationRule
        (\context _ -> [issue (vcPath context) ExpectedUndefined])
    SLiteralF literalValue ->
      ValidationRule
        (\context value -> validateLiteral (vcPath context) literalValue value)
    SEnumF values ->
      typeGuard extractString ExpectedStringForEnum
        (\ctx t ->
           if t `elem` values
             then []
             else [issue (vcPath ctx) ValueNotInEnum])
    SStringF constraint formatValue ->
      typeGuard extractString ExpectedString
        (\ctx t -> validateString (vcPath ctx) constraint formatValue t)
    SNumberF constraint ->
      typeGuard extractNumber ExpectedNumber
        (\ctx n -> validateNumber (vcPath ctx) constraint n)
    SArrayF (_, elementValidator) constraint ->
      typeGuard extractArray ExpectedArray
        (\ctx vals -> validateArrayWith ctx elementValidator constraint vals)
    STupleF elements rest ->
      typeGuard extractArray ExpectedArrayForTuple
        (\ctx vals -> validateTupleWith ctx elements rest vals)
    SRecordF (_, valueValidator) ->
      typeGuard extractObject ExpectedObjectForRecord
        (\ctx kv -> validateRecordWith ctx valueValidator kv)
    SObjectF fieldSchemas ->
      typeGuard extractObject ExpectedObject
        (\ctx kv -> validateObjectWith ctx fieldSchemas kv)
    SUnionF members ->
      ValidationRule
        (\context value ->
           if any (null . (\(_, validator) -> applyValidationRule validator context value)) members
             then []
             else [issue (vcPath context) ValueDoesNotMatchUnion])
    SDiscriminatedUnionF tagField members ->
      typeGuard extractObject ExpectedObjectForDiscriminatedUnion
        (\ctx kv ->
           case KeyMap.lookup (Key.fromText tagField) kv of
             Nothing -> [issue (vcPath ctx) (MissingDiscriminantField tagField)]
             Just discriminantValue ->
               let candidateMembers =
                     filter
                       (\(memberNode, _) -> memberAcceptsDiscriminant tagField discriminantValue memberNode)
                       members
                in
                  if null candidateMembers
                    then [issue (vcPath ctx) (NoDiscriminatedUnionMemberAcceptsTag tagField)]
                    else
                      if any (null . (\(_, validator) -> applyValidationRule validator ctx (Object kv))) candidateMembers
                        then []
                        else [issue (vcPath ctx) ValueDoesNotMatchSelectedDiscriminatedUnionMember])
    SOptionalF (_, innerValidator) -> innerValidator
    SNullableF (_, innerValidator) -> nullablePassthrough innerValidator
    SDefaultF (_, innerValidator) _ ->
      innerValidator
    SBrandF (_, innerValidator) _ ->
      innerValidator
    SRefineF (_, innerValidator) refinementId ->
      innerValidator
        <> ValidationRule
          (\context value ->
             case lookupRefinement refinementId (vcEnv context) of
               Nothing -> [issue (vcPath context) (MissingRefinementFunction refinementId)]
               Just refinementFn -> refinementFn (schemaHookContext context) value)
    SPreprocessF (_, innerValidator) preprocessId ->
      ValidationRule
        (\context value ->
             case lookupPreprocessor preprocessId (vcEnv context) of
               Nothing -> [issue (vcPath context) (MissingPreprocessorFunction preprocessId)]
               Just preprocessFn ->
                 applyValidationRule innerValidator context (preprocessFn (schemaHookContext context) value))
    SConstrainF (_, innerValidator) constraintId ->
      innerValidator
        <> ValidationRule
          (\context value ->
             case lookupConstraint constraintId (vcEnv context) of
               Nothing -> [issue (vcPath context) (MissingConstraintFunction constraintId)]
               Just constraintFn -> constraintFn (schemaHookContext context) value)
    STransformF (_, inputValidator) (_, outputValidator) transformId ->
      inputValidator
        <> ValidationRule
          (\context value ->
             case lookupTransform transformId (vcEnv context) of
               Nothing -> [issue (vcPath context) (MissingTransformFunction transformId)]
               Just transformFns ->
                 case tfForward transformFns (schemaHookContext context) value of
                   Left transformIssues -> transformIssues
                   Right transformedValue ->
                     applyValidationRule outputValidator context transformedValue)
    SRefF refId -> validateReference refLookup False refId
    SLazyF refId -> validateReference refLookup True refId

schemaHookContext :: ValidationContext -> SchemaHookContext
schemaHookContext context =
  SchemaHookContext {shcPath = vcPath context}

validateReference :: RefValidatorLookup -> Bool -> RefId -> Validator
validateReference refLookup isLazy refId =
  ValidationRule
    (\context value ->
       case refLookup refId of
         Nothing ->
           [issue (vcPath context) (unresolvedRefProblem isLazy refId)]
         Just referenceValidator ->
           if Set.member refId (vcVisited context)
             then
               [issue (vcPath context) (cyclicRefProblem isLazy refId)]
             else
               let nextContext = context {vcVisited = Set.insert refId (vcVisited context)}
                in applyValidationRule referenceValidator nextContext value)

unresolvedRefValidator :: RefId -> Validator
unresolvedRefValidator refId =
  ValidationRule
    (\context _ ->
       [issue (vcPath context) (unresolvedRefProblem False refId)])

unresolvedRefMessage :: Bool -> RefId -> Text
unresolvedRefMessage isLazy refId =
  siMessage (SchemaIssue [] (unresolvedRefProblem isLazy refId))

cyclicRefMessage :: Bool -> RefId -> Text
cyclicRefMessage isLazy refId =
  siMessage (SchemaIssue [] (cyclicRefProblem isLazy refId))

memberAcceptsDiscriminant :: Text -> Value -> SchemaNode -> Bool
memberAcceptsDiscriminant tagField discriminantValue member =
  case member of
    SObject fields ->
      case Map.lookup tagField fields of
        Nothing -> False
        Just propertyValue -> null (validateTagSchema (opSchema propertyValue) discriminantValue)
    _ -> False

validateTagSchema :: SchemaNode -> Value -> [SchemaIssue]
validateTagSchema tagSchema discriminantValue =
  case (tagSchema, discriminantValue) of
    (SLiteral expected, _) -> validateLiteral [] expected discriminantValue
    (SEnum values, String textValue) ->
      collectDetachedRuleIssues
        (issueIf (not (textValue `elem` values)) [] TagValueNotInEnum)
    (SUnion members, _) ->
      collectDetachedRuleIssues
        ( issueIf
            (not (any (null . (\memberNode -> validateTagSchema memberNode discriminantValue)) members))
            []
            TagValueNotInUnion
        )
    _ ->
      collectDetachedRuleIssues
        (issueIf True [] InvalidTagSchema)

validateLiteral :: [PathSegment] -> LiteralValue -> Value -> [SchemaIssue]
validateLiteral path expected actual =
  case (expected, actual) of
    (LitString expectedText, String actualText) ->
      collectDetachedRuleIssues
        (issueIf (expectedText /= actualText) path LiteralStringMismatch)
    (LitNumber expectedNumber, Number actualNumber) ->
      collectDetachedRuleIssues
        ( issueIf
            (expectedNumber /= canonicalizeObservedNumber (Scientific.toRealFloat actualNumber))
            path
            LiteralNumberMismatch
        )
    (LitBool expectedBool, Bool actualBool) ->
      collectDetachedRuleIssues
        (issueIf (expectedBool /= actualBool) path LiteralBooleanMismatch)
    (LitNull, Null) -> []
    (_, _) ->
      collectDetachedRuleIssues
        (issueIf True path LiteralTypeMismatch)

unresolvedRefProblem :: Bool -> RefId -> SchemaProblem
unresolvedRefProblem isLazy refId =
  if isLazy
    then UnresolvedLazyReference refId
    else UnresolvedReference refId

cyclicRefProblem :: Bool -> RefId -> SchemaProblem
cyclicRefProblem isLazy refId =
  if isLazy
    then CyclicLazyReference refId
    else CyclicReference refId

canonicalizeObservedNumber :: Double -> CanonicalNumber
canonicalizeObservedNumber = canonicalNumberFromDouble

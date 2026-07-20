module Moonlight.Sketch.Pure.Validate.Algebra.Composite
  ( validateArrayWith,
    validateTupleWith,
    validateRecordWith,
    validateObjectWith,
  )
where

import Data.Aeson (Value)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Moonlight.Sketch.Pure.Types
  ( ArrayConstraint (..),
    ObjectPropertyF (..),
    PathSegment (..),
    SchemaIssue,
    SchemaNode,
    SchemaProblem (..),
  )
import Moonlight.Sketch.Pure.Validate.Algebra.Primitive
  ( collectDetachedRuleIssues,
    issueIf,
  )
import Moonlight.Sketch.Pure.Validate.Core
  ( ValidationContext (..),
    ValidationRule (..),
    Validator,
  )

withPathSegment :: ValidationContext -> PathSegment -> ValidationContext
withPathSegment context segment = context {vcPath = vcPath context <> [segment]}

validateArrayWith ::
  ValidationContext ->
  Validator ->
  Maybe ArrayConstraint ->
  [Value] ->
  [SchemaIssue]
validateArrayWith context elementValidator constraint elements =
  let elementIssues =
        foldMap
          (\(index, elementValue) ->
             let elementContext = withPathSegment context (IndexSegment index)
              in applyValidationRule elementValidator elementContext elementValue)
          (zip [0 ..] elements)
      constraintIssues =
        maybe
          []
          (\arrayConstraint ->
             let elementCount = length elements
                 rules =
                   [ issueIf
                       (maybe False (\minLength -> elementCount < minLength) (acMinLength arrayConstraint))
                       (vcPath context)
                       ArrayTooShort,
                     issueIf
                       (maybe False (\maxLength -> elementCount > maxLength) (acMaxLength arrayConstraint))
                       (vcPath context)
                       ArrayTooLong,
                     issueIf
                       (maybe False (\exactLength -> elementCount /= exactLength) (acExactLength arrayConstraint))
                       (vcPath context)
                       ArrayLengthMismatch
                   ]
              in collectDetachedRuleIssues (foldMap id rules))
          constraint
   in elementIssues <> constraintIssues

validateTupleWith ::
  ValidationContext ->
  [(SchemaNode, Validator)] ->
  Maybe (SchemaNode, Validator) ->
  [Value] ->
  [SchemaIssue]
validateTupleWith context elementSchemas restSchema values =
  let fixedCount = length elementSchemas
      fixedPairs = zip [0 ..] (zip elementSchemas values)
      fixedIssues =
        foldMap
          (\(index, ((_, schemaValidator), valueItem)) ->
             let elementContext = withPathSegment context (IndexSegment index)
              in applyValidationRule schemaValidator elementContext valueItem)
          fixedPairs
      lengthIssues =
        collectDetachedRuleIssues
          ( case restSchema of
              Nothing ->
                issueIf
                  (length values /= fixedCount)
                  (vcPath context)
                  TupleLengthMismatch
              Just _ ->
                issueIf
                  (length values < fixedCount)
                  (vcPath context)
                  TupleTooShort
          )
      restIssues =
        case restSchema of
          Nothing -> []
          Just (_, restValidator) ->
            foldMap
              (\(index, valueItem) ->
                 let restContext = withPathSegment context (IndexSegment index)
                  in applyValidationRule restValidator restContext valueItem)
              (zip [fixedCount ..] (drop fixedCount values))
   in fixedIssues <> lengthIssues <> restIssues

validateRecordWith ::
  ValidationContext ->
  Validator ->
  KeyMap.KeyMap Value ->
  [SchemaIssue]
validateRecordWith context valueValidator keyValues =
  foldMap
    (\(keyValue, recordValue) ->
       let recordContext = withPathSegment context (FieldSegment (Key.toText keyValue))
        in applyValidationRule valueValidator recordContext recordValue)
    (KeyMap.toList keyValues)

validateObjectWith ::
  ValidationContext ->
  Map.Map Text (ObjectPropertyF (SchemaNode, Validator)) ->
  KeyMap.KeyMap Value ->
  [SchemaIssue]
validateObjectWith context fieldSchemas keyValues =
  foldMap
    (\(fieldName, propertyValue) ->
       let fieldContext = withPathSegment context (FieldSegment fieldName)
        in case KeyMap.lookup (Key.fromText fieldName) keyValues of
             Nothing ->
               collectDetachedRuleIssues
                 ( issueIf
                     (opfRequired propertyValue)
                     (vcPath fieldContext)
                     RequiredFieldMissing
                 )
             Just fieldValue ->
               let (_, schemaValidator) = opfSchema propertyValue
                in applyValidationRule schemaValidator fieldContext fieldValue)
    (Map.toList fieldSchemas)

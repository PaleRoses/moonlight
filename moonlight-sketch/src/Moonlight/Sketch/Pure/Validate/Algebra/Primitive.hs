module Moonlight.Sketch.Pure.Validate.Algebra.Primitive
  ( validateString,
    validateNumber,
    issue,
    issueIf,
    collectDetachedRuleIssues,
  )
where

import Data.Aeson (Value (..))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Moonlight.Core
  ( canonicalFiniteValue,
    canonicalNumberFromDouble,
    canonicalNumberToMaybeDouble,
  )
import Moonlight.Sketch.Pure.Env (emptySchemaEnv)
import Moonlight.Sketch.Pure.Format (matchFormat)
import Moonlight.Sketch.Pure.Types
  ( CanonicalNumber (..),
    NumberConstraint (..),
    PathSegment,
    SchemaIssue (..),
    SchemaProblem (..),
    StringConstraint (..),
    StringFormat,
  )
import Moonlight.Sketch.Pure.Validate.Core
  ( ValidationContext (..),
    ValidationRule (..),
  )
import Text.Regex.TDFA ((=~))

issue :: [PathSegment] -> SchemaProblem -> SchemaIssue
issue path problem = SchemaIssue path problem

issueIf :: Bool -> [PathSegment] -> SchemaProblem -> ValidationRule
issueIf condition path problem =
  ValidationRule
    (\_ _ ->
       if condition
         then [issue path problem]
         else [])

collectDetachedRuleIssues :: ValidationRule -> [SchemaIssue]
collectDetachedRuleIssues validationRule =
  applyValidationRule validationRule detachedValidationContext Null
  where
    detachedValidationContext :: ValidationContext
    detachedValidationContext =
      ValidationContext
        { vcEnv = emptySchemaEnv,
          vcVisited = Set.empty,
          vcPath = []
        }

validateString :: [PathSegment] -> Maybe StringConstraint -> Maybe StringFormat -> Text -> [SchemaIssue]
validateString path constraint formatValue textValue =
  let constraintRules =
        maybe
          []
          (\stringConstraint ->
             let lengthValue = Text.length textValue
              in
               [ issueIf
                   (maybe False (\minLength -> lengthValue < minLength) (scMinLength stringConstraint))
                   path
                   StringTooShort,
                 issueIf
                   (maybe False (\maxLength -> lengthValue > maxLength) (scMaxLength stringConstraint))
                   path
                   StringTooLong,
                 issueIf
                   (maybe False (\patternText -> not (matchesPattern patternText textValue)) (scPattern stringConstraint))
                   path
                   StringDoesNotMatchPattern
               ])
          constraint
      formatRules =
        maybe
          []
          (\formatSpec ->
             [ issueIf
                 (not (matchFormat formatSpec textValue))
                 path
                 StringDoesNotMatchFormat
             ])
          formatValue
   in collectDetachedRuleIssues (foldMap id (constraintRules <> formatRules))

matchesPattern :: Text -> Text -> Bool
matchesPattern patternText valueText =
  (Text.unpack valueText =~ Text.unpack patternText :: Bool)

validateNumber :: [PathSegment] -> Maybe NumberConstraint -> Double -> [SchemaIssue]
validateNumber path constraint number =
  maybe
    []
    (\numberConstraint ->
       let observedNumber = canonicalizeObservedNumber number
           numberRules =
             [ issueIf
                 (maybe False (violatesMin observedNumber) (ncMin numberConstraint))
                 path
                 NumberBelowMinimum,
               issueIf
                 (maybe False (violatesMax observedNumber) (ncMax numberConstraint))
                 path
                 NumberAboveMaximum,
               issueIf
                 (maybe False (\factorValue -> not (isMultipleOf observedNumber factorValue)) (ncMultipleOf numberConstraint))
                 path
                 NumberNotMultipleOfConstraint,
               issueIf
                 (ncFinite numberConstraint && (isInfinite number || isNaN number))
                 path
                 NumberMustBeFinite,
               issueIf
                 (ncInt numberConstraint && number /= fromIntegral (round number :: Integer))
                 path
                 NumberMustBeInteger,
               issueIf
                 (ncPositive numberConstraint && number <= 0)
                 path
                 NumberMustBePositive,
               issueIf
                 (ncNegative numberConstraint && number >= 0)
                 path
                 NumberMustBeNegative
             ]
        in collectDetachedRuleIssues (foldMap id numberRules))
    constraint

violatesBound ::
  (Double -> Double -> Bool) -> Bool -> Bool ->
  CanonicalNumber -> CanonicalNumber -> Bool
violatesBound cmp posInfResult negInfResult observed bound =
  case (canonicalToFinite observed, bound) of
    (Just obsVal, CanonicalFinite boundVal) -> cmp obsVal (canonicalFiniteValue boundVal)
    (_, PosInf) -> posInfResult
    (_, NegInf) -> negInfResult
    (_, NaN) -> True
    (Nothing, _) -> True

violatesMin :: CanonicalNumber -> CanonicalNumber -> Bool
violatesMin = violatesBound (<) True False

violatesMax :: CanonicalNumber -> CanonicalNumber -> Bool
violatesMax = violatesBound (>) False True

isMultipleOf :: CanonicalNumber -> CanonicalNumber -> Bool
isMultipleOf value factor =
  case (canonicalToFinite value, canonicalToFinite factor) of
    (Just valueNumber, Just factorNumber) ->
      factorNumber /= 0
        && let quotient = valueNumber / factorNumber
            in abs (quotient - fromIntegral (round quotient :: Integer)) < 1e-9
    _ -> False

canonicalizeObservedNumber :: Double -> CanonicalNumber
canonicalizeObservedNumber = canonicalNumberFromDouble

canonicalToFinite :: CanonicalNumber -> Maybe Double
canonicalToFinite = canonicalNumberToMaybeDouble

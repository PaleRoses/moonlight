module Moonlight.Sketch.Pure.Types.Issues
  ( IssueCode (..),
    unIssueCode,
    SchemaProblem (..),
    PathSegment (..),
    SchemaHookContext (..),
    SchemaIssue (..),
    siMessage,
    siCode,
    renderSchemaProblem,
    schemaProblemCode,
  )
where

import Data.Kind (Type)
import Data.Text (Text)
import Moonlight.Sketch.Pure.Types.Identifiers
  ( ConstraintId,
    PreprocessId,
    RefId,
    RefinementId,
    TransformId,
    unConstraintId,
    unPreprocessId,
    unRefId,
    unRefinementId,
    unTransformId,
  )

type IssueCode :: Type
data IssueCode
  = VoidSchemaCode
  | ExpectedBooleanCode
  | ExpectedNullCode
  | ExpectedUndefinedCode
  | ValueNotInEnumCode
  | ExpectedStringForEnumCode
  | ExpectedStringCode
  | ExpectedNumberCode
  | ExpectedArrayCode
  | ExpectedArrayForTupleCode
  | ExpectedObjectForRecordCode
  | ExpectedObjectCode
  | ValueDoesNotMatchUnionCode
  | MissingDiscriminantFieldCode
  | NoDiscriminatedUnionMemberAcceptsTagCode
  | ValueDoesNotMatchSelectedDiscriminatedUnionMemberCode
  | ExpectedObjectForDiscriminatedUnionCode
  | MissingRefinementFunctionCode
  | MissingPreprocessorFunctionCode
  | MissingConstraintFunctionCode
  | MissingTransformFunctionCode
  | UnresolvedReferenceCode
  | UnresolvedLazyReferenceCode
  | CyclicReferenceCode
  | CyclicLazyReferenceCode
  | TagValueNotInEnumCode
  | TagValueNotInUnionCode
  | InvalidTagSchemaCode
  | LiteralStringMismatchCode
  | LiteralNumberMismatchCode
  | LiteralBooleanMismatchCode
  | LiteralTypeMismatchCode
  | StringTooShortCode
  | StringTooLongCode
  | StringDoesNotMatchPatternCode
  | StringDoesNotMatchFormatCode
  | NumberBelowMinimumCode
  | NumberAboveMaximumCode
  | NumberNotMultipleOfConstraintCode
  | NumberMustBeFiniteCode
  | NumberMustBeIntegerCode
  | NumberMustBePositiveCode
  | NumberMustBeNegativeCode
  | ArrayTooShortCode
  | ArrayTooLongCode
  | ArrayLengthMismatchCode
  | TupleLengthMismatchCode
  | TupleTooShortCode
  | RequiredFieldMissingCode
  deriving stock (Eq, Ord, Show, Read)

unIssueCode :: IssueCode -> Text
unIssueCode issueCode =
  case issueCode of
    VoidSchemaCode -> "void_schema"
    ExpectedBooleanCode -> "expected_boolean"
    ExpectedNullCode -> "expected_null"
    ExpectedUndefinedCode -> "expected_undefined"
    ValueNotInEnumCode -> "value_not_in_enum"
    ExpectedStringForEnumCode -> "expected_string_for_enum"
    ExpectedStringCode -> "expected_string"
    ExpectedNumberCode -> "expected_number"
    ExpectedArrayCode -> "expected_array"
    ExpectedArrayForTupleCode -> "expected_array_for_tuple"
    ExpectedObjectForRecordCode -> "expected_object_for_record"
    ExpectedObjectCode -> "expected_object"
    ValueDoesNotMatchUnionCode -> "value_does_not_match_union"
    MissingDiscriminantFieldCode -> "missing_discriminant_field"
    NoDiscriminatedUnionMemberAcceptsTagCode -> "no_discriminated_union_member_accepts_tag"
    ValueDoesNotMatchSelectedDiscriminatedUnionMemberCode -> "value_does_not_match_selected_discriminated_union_member"
    ExpectedObjectForDiscriminatedUnionCode -> "expected_object_for_discriminated_union"
    MissingRefinementFunctionCode -> "missing_refinement_function"
    MissingPreprocessorFunctionCode -> "missing_preprocessor_function"
    MissingConstraintFunctionCode -> "missing_constraint_function"
    MissingTransformFunctionCode -> "missing_transform_function"
    UnresolvedReferenceCode -> "unresolved_reference"
    UnresolvedLazyReferenceCode -> "unresolved_lazy_reference"
    CyclicReferenceCode -> "cyclic_reference"
    CyclicLazyReferenceCode -> "cyclic_lazy_reference"
    TagValueNotInEnumCode -> "tag_value_not_in_enum"
    TagValueNotInUnionCode -> "tag_value_not_in_union"
    InvalidTagSchemaCode -> "invalid_tag_schema"
    LiteralStringMismatchCode -> "literal_string_mismatch"
    LiteralNumberMismatchCode -> "literal_number_mismatch"
    LiteralBooleanMismatchCode -> "literal_boolean_mismatch"
    LiteralTypeMismatchCode -> "literal_type_mismatch"
    StringTooShortCode -> "string_too_short"
    StringTooLongCode -> "string_too_long"
    StringDoesNotMatchPatternCode -> "string_does_not_match_pattern"
    StringDoesNotMatchFormatCode -> "string_does_not_match_format"
    NumberBelowMinimumCode -> "number_below_minimum"
    NumberAboveMaximumCode -> "number_above_maximum"
    NumberNotMultipleOfConstraintCode -> "number_not_multiple_of_constraint"
    NumberMustBeFiniteCode -> "number_must_be_finite"
    NumberMustBeIntegerCode -> "number_must_be_integer"
    NumberMustBePositiveCode -> "number_must_be_positive"
    NumberMustBeNegativeCode -> "number_must_be_negative"
    ArrayTooShortCode -> "array_too_short"
    ArrayTooLongCode -> "array_too_long"
    ArrayLengthMismatchCode -> "array_length_mismatch"
    TupleLengthMismatchCode -> "tuple_length_mismatch"
    TupleTooShortCode -> "tuple_too_short"
    RequiredFieldMissingCode -> "required_field_missing"

type SchemaProblem :: Type
data SchemaProblem
  = NoValueConformsToVoid
  | ExpectedBoolean
  | ExpectedNull
  | ExpectedUndefined
  | ValueNotInEnum
  | ExpectedStringForEnum
  | ExpectedString
  | ExpectedNumber
  | ExpectedArray
  | ExpectedArrayForTuple
  | ExpectedObjectForRecord
  | ExpectedObject
  | ValueDoesNotMatchUnion
  | MissingDiscriminantField Text
  | NoDiscriminatedUnionMemberAcceptsTag Text
  | ValueDoesNotMatchSelectedDiscriminatedUnionMember
  | ExpectedObjectForDiscriminatedUnion
  | MissingRefinementFunction RefinementId
  | MissingPreprocessorFunction PreprocessId
  | MissingConstraintFunction ConstraintId
  | MissingTransformFunction TransformId
  | UnresolvedReference RefId
  | UnresolvedLazyReference RefId
  | CyclicReference RefId
  | CyclicLazyReference RefId
  | TagValueNotInEnum
  | TagValueNotInUnion
  | InvalidTagSchema
  | LiteralStringMismatch
  | LiteralNumberMismatch
  | LiteralBooleanMismatch
  | LiteralTypeMismatch
  | StringTooShort
  | StringTooLong
  | StringDoesNotMatchPattern
  | StringDoesNotMatchFormat
  | NumberBelowMinimum
  | NumberAboveMaximum
  | NumberNotMultipleOfConstraint
  | NumberMustBeFinite
  | NumberMustBeInteger
  | NumberMustBePositive
  | NumberMustBeNegative
  | ArrayTooShort
  | ArrayTooLong
  | ArrayLengthMismatch
  | TupleLengthMismatch
  | TupleTooShort
  | RequiredFieldMissing
  deriving stock (Eq, Ord, Show)

type PathSegment :: Type
data PathSegment
  = FieldSegment Text
  | IndexSegment Int
  | KeySegment
  | ValueSegment
  deriving stock (Eq, Ord, Show, Read)

type SchemaHookContext :: Type
newtype SchemaHookContext = SchemaHookContext
  { shcPath :: [PathSegment]
  }
  deriving stock (Eq, Ord, Show, Read)

type SchemaIssue :: Type
data SchemaIssue = SchemaIssue
  { siPath :: [PathSegment],
    siProblem :: SchemaProblem
  }
  deriving stock (Eq, Ord, Show)

siMessage :: SchemaIssue -> Text
siMessage schemaIssue =
  renderSchemaProblem (siProblem schemaIssue)

siCode :: SchemaIssue -> IssueCode
siCode schemaIssue =
  schemaProblemCode (siProblem schemaIssue)

renderSchemaProblem :: SchemaProblem -> Text
renderSchemaProblem schemaProblem =
  case schemaProblem of
    NoValueConformsToVoid -> "no value conforms to void schema"
    ExpectedBoolean -> "expected boolean"
    ExpectedNull -> "expected null"
    ExpectedUndefined -> "expected undefined (null)"
    ValueNotInEnum -> "value not in enum"
    ExpectedStringForEnum -> "expected string for enum"
    ExpectedString -> "expected string"
    ExpectedNumber -> "expected number"
    ExpectedArray -> "expected array"
    ExpectedArrayForTuple -> "expected array for tuple"
    ExpectedObjectForRecord -> "expected object for record"
    ExpectedObject -> "expected object"
    ValueDoesNotMatchUnion -> "value does not match any union member"
    MissingDiscriminantField tagField -> "missing discriminant field: " <> tagField
    NoDiscriminatedUnionMemberAcceptsTag tagField -> "no discriminated union member accepts tag: " <> tagField
    ValueDoesNotMatchSelectedDiscriminatedUnionMember -> "value does not match selected discriminated union member"
    ExpectedObjectForDiscriminatedUnion -> "expected object for discriminated union"
    MissingRefinementFunction refinementId -> "missing refinement function: " <> unRefinementId refinementId
    MissingPreprocessorFunction preprocessId -> "missing preprocessor function: " <> unPreprocessId preprocessId
    MissingConstraintFunction constraintId -> "missing constraint function: " <> unConstraintId constraintId
    MissingTransformFunction transformId -> "missing transform function: " <> unTransformId transformId
    UnresolvedReference refId -> "unresolved reference: " <> unRefId refId
    UnresolvedLazyReference refId -> "unresolved lazy reference: " <> unRefId refId
    CyclicReference refId -> "cyclic reference during validation: " <> unRefId refId
    CyclicLazyReference refId -> "cyclic lazy reference during validation: " <> unRefId refId
    TagValueNotInEnum -> "tag value not in enum"
    TagValueNotInUnion -> "tag value not in union"
    InvalidTagSchema -> "tag schema must be literal, enum, or union of literals"
    LiteralStringMismatch -> "literal string mismatch"
    LiteralNumberMismatch -> "literal number mismatch"
    LiteralBooleanMismatch -> "literal boolean mismatch"
    LiteralTypeMismatch -> "literal type mismatch"
    StringTooShort -> "string too short"
    StringTooLong -> "string too long"
    StringDoesNotMatchPattern -> "string does not match pattern"
    StringDoesNotMatchFormat -> "string does not match format"
    NumberBelowMinimum -> "number below minimum"
    NumberAboveMaximum -> "number above maximum"
    NumberNotMultipleOfConstraint -> "number not multiple of constraint"
    NumberMustBeFinite -> "number must be finite"
    NumberMustBeInteger -> "number must be integer"
    NumberMustBePositive -> "number must be positive"
    NumberMustBeNegative -> "number must be negative"
    ArrayTooShort -> "array too short"
    ArrayTooLong -> "array too long"
    ArrayLengthMismatch -> "array length mismatch"
    TupleLengthMismatch -> "tuple length mismatch"
    TupleTooShort -> "tuple too short"
    RequiredFieldMissing -> "required field missing"

schemaProblemCode :: SchemaProblem -> IssueCode
schemaProblemCode schemaProblem =
  case schemaProblem of
    NoValueConformsToVoid -> VoidSchemaCode
    ExpectedBoolean -> ExpectedBooleanCode
    ExpectedNull -> ExpectedNullCode
    ExpectedUndefined -> ExpectedUndefinedCode
    ValueNotInEnum -> ValueNotInEnumCode
    ExpectedStringForEnum -> ExpectedStringForEnumCode
    ExpectedString -> ExpectedStringCode
    ExpectedNumber -> ExpectedNumberCode
    ExpectedArray -> ExpectedArrayCode
    ExpectedArrayForTuple -> ExpectedArrayForTupleCode
    ExpectedObjectForRecord -> ExpectedObjectForRecordCode
    ExpectedObject -> ExpectedObjectCode
    ValueDoesNotMatchUnion -> ValueDoesNotMatchUnionCode
    MissingDiscriminantField _ -> MissingDiscriminantFieldCode
    NoDiscriminatedUnionMemberAcceptsTag _ -> NoDiscriminatedUnionMemberAcceptsTagCode
    ValueDoesNotMatchSelectedDiscriminatedUnionMember -> ValueDoesNotMatchSelectedDiscriminatedUnionMemberCode
    ExpectedObjectForDiscriminatedUnion -> ExpectedObjectForDiscriminatedUnionCode
    MissingRefinementFunction _ -> MissingRefinementFunctionCode
    MissingPreprocessorFunction _ -> MissingPreprocessorFunctionCode
    MissingConstraintFunction _ -> MissingConstraintFunctionCode
    MissingTransformFunction _ -> MissingTransformFunctionCode
    UnresolvedReference _ -> UnresolvedReferenceCode
    UnresolvedLazyReference _ -> UnresolvedLazyReferenceCode
    CyclicReference _ -> CyclicReferenceCode
    CyclicLazyReference _ -> CyclicLazyReferenceCode
    TagValueNotInEnum -> TagValueNotInEnumCode
    TagValueNotInUnion -> TagValueNotInUnionCode
    InvalidTagSchema -> InvalidTagSchemaCode
    LiteralStringMismatch -> LiteralStringMismatchCode
    LiteralNumberMismatch -> LiteralNumberMismatchCode
    LiteralBooleanMismatch -> LiteralBooleanMismatchCode
    LiteralTypeMismatch -> LiteralTypeMismatchCode
    StringTooShort -> StringTooShortCode
    StringTooLong -> StringTooLongCode
    StringDoesNotMatchPattern -> StringDoesNotMatchPatternCode
    StringDoesNotMatchFormat -> StringDoesNotMatchFormatCode
    NumberBelowMinimum -> NumberBelowMinimumCode
    NumberAboveMaximum -> NumberAboveMaximumCode
    NumberNotMultipleOfConstraint -> NumberNotMultipleOfConstraintCode
    NumberMustBeFinite -> NumberMustBeFiniteCode
    NumberMustBeInteger -> NumberMustBeIntegerCode
    NumberMustBePositive -> NumberMustBePositiveCode
    NumberMustBeNegative -> NumberMustBeNegativeCode
    ArrayTooShort -> ArrayTooShortCode
    ArrayTooLong -> ArrayTooLongCode
    ArrayLengthMismatch -> ArrayLengthMismatchCode
    TupleLengthMismatch -> TupleLengthMismatchCode
    TupleTooShort -> TupleTooShortCode
    RequiredFieldMissing -> RequiredFieldMissingCode

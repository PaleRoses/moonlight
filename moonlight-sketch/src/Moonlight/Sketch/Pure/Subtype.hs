module Moonlight.Sketch.Pure.Subtype
  ( isSubtype,
    isSubtypeWith,
    subtypeVariance,
    constraintSubtype,
    stringConstraintSubtype,
    numberConstraintSubtype,
    arrayConstraintSubtype,
    formatSubtype,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Moonlight.Core (CanonicalFiniteValue, canonicalFiniteValue)
import Moonlight.Sketch.Pure.Normalize (normalize)
import Moonlight.Sketch.Pure.Resolve (resolve)
import Moonlight.Sketch.Pure.Types
  ( ArrayConstraint (..),
    CanonicalNumber (..),
    ConstraintId,
    LiteralValue (..),
    NumberConstraint (..),
    ObjectProperty (..),
    RefinementId,
    SchemaNode (..),
    SchemaRegistry,
    StringConstraint (..),
    StringFormat,
    Variance (..),
    emptySchemaRegistry,
  )

isSubtype :: SchemaNode -> SchemaNode -> Bool
isSubtype = isSubtypeWith emptySchemaRegistry

isSubtypeWith :: SchemaRegistry -> SchemaNode -> SchemaNode -> Bool
isSubtypeWith registry left right =
  isSubtypeResolved
    (normalize (resolve registry left))
    (normalize (resolve registry right))

isSubtypeResolved :: SchemaNode -> SchemaNode -> Bool
isSubtypeResolved left right =
  if left == right
    then True
    else
      case (left, right) of
        (SOptional leftInner, rightNode) ->
          isSubtypeResolved (SUnion [leftInner, SUndefined]) rightNode
        (SNullable leftInner, rightNode) ->
          isSubtypeResolved (SUnion [leftInner, SNull]) rightNode
        (leftNode, SOptional rightInner) ->
          isSubtypeResolved leftNode (SUnion [rightInner, SUndefined])
        (leftNode, SNullable rightInner) ->
          isSubtypeResolved leftNode (SUnion [rightInner, SNull])
        (_, SUnknown) -> True
        (SVoid, _) -> True
        (_, SVoid) -> False
        (SUnknown, _) -> False
        (SBool, SBool) -> True
        (SNull, SNull) -> True
        (SUndefined, SUndefined) -> True
        (SLiteral (LitString _), SString _ _) -> True
        (SLiteral (LitNumber _), SNumber _) -> True
        (SLiteral (LitBool _), SBool) -> True
        (SLiteral LitNull, SNull) -> True
        (SLiteral literalValue, SLiteral otherLiteralValue) -> literalValue == otherLiteralValue
        (SLiteral (LitString literalText), SEnum values) -> literalText `elem` values
        (SEnum _, SString _ _) -> True
        (SEnum leftValues, SEnum rightValues) ->
          Set.fromList leftValues `Set.isSubsetOf` Set.fromList rightValues
        (SString leftConstraint leftFormat, SString rightConstraint rightFormat) ->
          stringConstraintSubtype leftConstraint rightConstraint
            && formatSubtype leftFormat rightFormat
        (SNumber leftConstraint, SNumber rightConstraint) ->
          numberConstraintSubtype leftConstraint rightConstraint
        (SArray leftElement leftConstraint, SArray rightElement rightConstraint) ->
          isSubtypeResolved leftElement rightElement
            && arrayConstraintSubtype leftConstraint rightConstraint
        (STuple leftElements leftRest, STuple rightElements rightRest) ->
          tupleSubtype leftElements leftRest rightElements rightRest
        (SRecord leftValue, SRecord rightValue) ->
          isSubtypeResolved leftValue rightValue
        (SObject leftFields, SObject rightFields) ->
          all
            (\(fieldName, rightProperty) ->
               case Map.lookup fieldName leftFields of
                 Nothing -> not (opRequired rightProperty)
                 Just leftProperty ->
                   isSubtypeResolved (opSchema leftProperty) (opSchema rightProperty)
                     && (not (opRequired rightProperty) || opRequired leftProperty))
            (Map.toList rightFields)
        (SUnion leftMembers, _) ->
          all (\member -> isSubtypeResolved member right) leftMembers
        (_, SUnion rightMembers) ->
          any (\member -> isSubtypeResolved left member) rightMembers
        (SDiscriminatedUnion _ leftMembers, _) ->
          all (\member -> isSubtypeResolved member right) leftMembers
        (_, SDiscriminatedUnion _ rightMembers) ->
          any (\member -> isSubtypeResolved left member) rightMembers
        (SBrand leftInner leftBrand, SBrand rightInner rightBrand) ->
          leftBrand == rightBrand
            && isSubtypeResolved leftInner rightInner
        (SRefine leftInner _, rightNode) ->
          isSubtypeResolved leftInner rightNode
        (leftNode, SRefine rightInner rightId) ->
          compareRefinementWrapper leftNode rightInner rightId
        (SConstrain leftInner _, rightNode) ->
          isSubtypeResolved leftInner rightNode
        (leftNode, SConstrain rightInner rightId) ->
          compareConstraintWrapper leftNode rightInner rightId
        (SPreprocess leftInner _, rightNode) ->
          isSubtypeResolved leftInner rightNode
        (leftNode, SPreprocess rightInner _) ->
          isSubtypeResolved leftNode rightInner
        (STransform leftInput leftOutput _, STransform rightInput rightOutput _) ->
          isSubtypeResolved rightInput leftInput
            && isSubtypeResolved leftOutput rightOutput
        (_, _) -> False

compareRefinementWrapper :: SchemaNode -> SchemaNode -> RefinementId -> Bool
compareRefinementWrapper leftNode rightInner rightId =
  case leftNode of
    SRefine leftInner leftId ->
      leftId == rightId
        && isSubtypeResolved leftInner rightInner
    _ -> False

compareConstraintWrapper :: SchemaNode -> SchemaNode -> ConstraintId -> Bool
compareConstraintWrapper leftNode rightInner rightId =
  case leftNode of
    SConstrain leftInner leftId ->
      leftId == rightId
        && isSubtypeResolved leftInner rightInner
    _ -> False

tupleSubtype :: [SchemaNode] -> Maybe SchemaNode -> [SchemaNode] -> Maybe SchemaNode -> Bool
tupleSubtype leftElements leftRest rightElements rightRest =
  let leftLength = length leftElements
      rightLength = length rightElements
      prefixCompatible =
        leftLength >= rightLength
          && and
            ( zipWith
                isSubtypeResolved
                (take rightLength leftElements)
                rightElements
            )
      extraFixedCompatible =
        case rightRest of
          Nothing -> leftLength == rightLength
          Just rightRestElement ->
            and
              ( map
                  (\leftExtra -> isSubtypeResolved leftExtra rightRestElement)
                  (drop rightLength leftElements)
              )
      restCompatible =
        case (leftRest, rightRest) of
          (Nothing, _) -> True
          (Just _, Nothing) -> False
          (Just leftRestElement, Just rightRestElement) ->
            isSubtypeResolved leftRestElement rightRestElement
   in prefixCompatible && extraFixedCompatible && restCompatible

subtypeVariance :: Variance -> SchemaNode -> SchemaNode -> Bool
subtypeVariance variance left right =
  case variance of
    Covariant -> isSubtypeResolved left right
    Contravariant -> isSubtypeResolved right left
    Invariant ->
      isSubtypeResolved left right
        && isSubtypeResolved right left
    Bivariant -> True

stringConstraintSubtype :: Maybe StringConstraint -> Maybe StringConstraint -> Bool
stringConstraintSubtype leftConstraint rightConstraint =
  case (leftConstraint, rightConstraint) of
    (_, Nothing) -> True
    (Nothing, Just _) -> False
    (Just leftValue, Just rightValue) ->
      maybeSubtype (>=) (scMinLength leftValue) (scMinLength rightValue)
        && maybeSubtype (<=) (scMaxLength leftValue) (scMaxLength rightValue)
        && maybeSubtype (==) (scPattern leftValue) (scPattern rightValue)

numberConstraintSubtype :: Maybe NumberConstraint -> Maybe NumberConstraint -> Bool
numberConstraintSubtype leftConstraint rightConstraint =
  case (leftConstraint, rightConstraint) of
    (_, Nothing) -> True
    (Nothing, Just _) -> False
    (Just leftValue, Just rightValue) ->
      maybeSubtype canonicalGreaterEqual (ncMin leftValue) (ncMin rightValue)
        && maybeSubtype canonicalLessEqual (ncMax leftValue) (ncMax rightValue)
        && maybeSubtype (==) (ncMultipleOf leftValue) (ncMultipleOf rightValue)
        && boolSubtype (ncFinite leftValue) (ncFinite rightValue)
        && boolSubtype (ncInt leftValue) (ncInt rightValue)
        && boolSubtype (ncPositive leftValue) (ncPositive rightValue)
        && boolSubtype (ncNegative leftValue) (ncNegative rightValue)

arrayConstraintSubtype :: Maybe ArrayConstraint -> Maybe ArrayConstraint -> Bool
arrayConstraintSubtype leftConstraint rightConstraint =
  case (leftConstraint, rightConstraint) of
    (_, Nothing) -> True
    (Nothing, Just _) -> False
    (Just leftValue, Just rightValue) ->
      maybeSubtype (>=) (acMinLength leftValue) (acMinLength rightValue)
        && maybeSubtype (<=) (acMaxLength leftValue) (acMaxLength rightValue)
        && maybeSubtype (==) (acExactLength leftValue) (acExactLength rightValue)

formatSubtype :: Maybe StringFormat -> Maybe StringFormat -> Bool
formatSubtype leftFormat rightFormat =
  case (leftFormat, rightFormat) of
    (_, Nothing) -> True
    (Nothing, Just _) -> False
    (Just leftValue, Just rightValue) -> leftValue == rightValue

constraintSubtype :: SchemaNode -> SchemaNode -> Bool
constraintSubtype = isSubtype

maybeSubtype :: (a -> a -> Bool) -> Maybe a -> Maybe a -> Bool
maybeSubtype comparator leftValue rightValue =
  case (leftValue, rightValue) of
    (_, Nothing) -> True
    (Nothing, Just _) -> False
    (Just leftEntry, Just rightEntry) -> comparator leftEntry rightEntry

boolSubtype :: Bool -> Bool -> Bool
boolSubtype leftFlag rightFlag =
  not rightFlag || leftFlag

canonicalGreaterEqual :: CanonicalNumber -> CanonicalNumber -> Bool
canonicalGreaterEqual leftValue rightValue =
  case (canonicalToFinite leftValue, canonicalToFinite rightValue) of
    (Just leftFinite, Just rightFinite) -> canonicalFiniteValue leftFinite >= canonicalFiniteValue rightFinite
    _ -> False

canonicalLessEqual :: CanonicalNumber -> CanonicalNumber -> Bool
canonicalLessEqual leftValue rightValue =
  case (canonicalToFinite leftValue, canonicalToFinite rightValue) of
    (Just leftFinite, Just rightFinite) -> canonicalFiniteValue leftFinite <= canonicalFiniteValue rightFinite
    _ -> False

canonicalToFinite :: CanonicalNumber -> Maybe CanonicalFiniteValue
canonicalToFinite number =
  case number of
    CanonicalFinite value -> Just value
    PosInf -> Nothing
    NegInf -> Nothing
    NaN -> Nothing

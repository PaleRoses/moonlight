module Moonlight.Sketch.Pure.Normalize
  ( normalize,
    normalizeConstraints,
    normalizeFormat,
    flattenUnion,
    deduplicateUnion,
    sortUnion,
    simplifyOptional,
    simplifyNullable,
    collapseRedundant,
  )
where

import qualified Data.List as List
import Moonlight.Core (canonicalFiniteValue)
import Moonlight.Sketch.Pure.Types
  ( ArrayConstraint (..),
    CanonicalNumber (..),
    FormatElement (..),
    LiteralValue (..),
    NumberConstraint (..),
    SchemaF,
    SchemaNode (..),
    StringConstraint (..),
    StringFormat (..),
    cataSchema,
    embedSchema,
  )

normalize :: SchemaNode -> SchemaNode
normalize node =
  let normalized = normalizeStep node
   in if normalized == node
        then node
        else normalize normalized

normalizeStep :: SchemaNode -> SchemaNode
normalizeStep =
  cataSchema normalizeAlgebra

normalizeAlgebra :: SchemaF SchemaNode -> SchemaNode
normalizeAlgebra =
  collapseRedundant
    . simplifyNullable
    . simplifyOptional
    . sortUnion
    . deduplicateUnion
    . flattenUnion
    . normalizeLiterals
    . normalizeConstraints
    . normalizeFormat
    . embedSchema

flattenUnion :: SchemaNode -> SchemaNode
flattenUnion node =
  case node of
    SUnion members -> SUnion (concatMap flattenUnionMembers members)
    other -> other

flattenUnionMembers :: SchemaNode -> [SchemaNode]
flattenUnionMembers node =
  case node of
    SUnion nested -> concatMap flattenUnionMembers nested
    other -> [other]

deduplicateUnion :: SchemaNode -> SchemaNode
deduplicateUnion node =
  case node of
    SUnion members -> SUnion (List.nub members)
    other -> other

sortUnion :: SchemaNode -> SchemaNode
sortUnion node =
  case node of
    SUnion members -> SUnion (List.sort members)
    other -> other

simplifyOptional :: SchemaNode -> SchemaNode
simplifyOptional node =
  case node of
    SOptional (SOptional inner) -> SOptional inner
    SOptional SUndefined -> SUndefined
    SOptional SVoid -> SUndefined
    SOptional SUnknown -> SUnknown
    other -> other

simplifyNullable :: SchemaNode -> SchemaNode
simplifyNullable node =
  case node of
    SNullable (SNullable inner) -> SNullable inner
    SNullable SNull -> SNull
    SNullable SVoid -> SNull
    SNullable SUnknown -> SUnknown
    other -> other

collapseRedundant :: SchemaNode -> SchemaNode
collapseRedundant node =
  case node of
    SUnion [] -> SVoid
    SUnion [single] -> single
    SUnion members
      | SUnknown `elem` members -> SUnknown
      | SVoid `elem` members -> SUnion (filter (/= SVoid) members)
      | otherwise -> SUnion members
    SDiscriminatedUnion _ [] -> SVoid
    SDiscriminatedUnion _ [single] -> single
    SArray _ (Just constraint)
      | acExactLength constraint == Just 0 -> SArray SVoid (Just constraint)
    other -> other

normalizeLiterals :: SchemaNode -> SchemaNode
normalizeLiterals node =
  case node of
    SLiteral (LitNumber value) ->
      case canonicalizeFinite value of
        Just canonicalValue -> SLiteral (LitNumber canonicalValue)
        Nothing -> SVoid
    other -> other

normalizeConstraints :: SchemaNode -> SchemaNode
normalizeConstraints node =
  case node of
    SString (Just sc) format ->
      let normalized = normalizeStringConstraint sc
       in if isEmptyStringConstraint normalized
            then SString Nothing format
            else SString (Just normalized) format
    SNumber (Just nc) ->
      let normalized = normalizeNumberConstraint nc
       in if isEmptyNumberConstraint normalized
            then SNumber Nothing
            else SNumber (Just normalized)
    SArray element (Just ac) ->
      let normalized = normalizeArrayConstraint ac
       in if isEmptyArrayConstraint normalized
            then SArray element Nothing
            else SArray element (Just normalized)
    other -> other

normalizeStringConstraint :: StringConstraint -> StringConstraint
normalizeStringConstraint sc =
  let normalizedMin =
        case scMinLength sc of
          Just n | n <= 0 -> Nothing
          other -> other
      normalizedMax =
        case scMaxLength sc of
          Just n | n < 0 -> Nothing
          other -> other
      clampedMax =
        case (normalizedMin, normalizedMax) of
          (Just minL, Just maxL) | maxL < minL -> Just minL
          (_, other) -> other
   in sc {scMinLength = normalizedMin, scMaxLength = clampedMax}

normalizeNumberConstraint :: NumberConstraint -> NumberConstraint
normalizeNumberConstraint nc =
  let canonicalMin = canonicalizeBound (ncMin nc)
      canonicalMax = canonicalizeBound (ncMax nc)
      canonicalMultiple = canonicalizeBound (ncMultipleOf nc)
      normalizedMin =
        case canonicalMin of
          Just (CanonicalFinite minValue) | ncPositive nc && canonicalFiniteValue minValue <= 0 -> Nothing
          other -> other
      normalizedMax =
        case canonicalMax of
          Just (CanonicalFinite maxValue) | ncNegative nc && canonicalFiniteValue maxValue >= 0 -> Nothing
          other -> other
      clampedBounds =
        case (normalizedMin, normalizedMax) of
          (Just (CanonicalFinite minValue), Just (CanonicalFinite maxValue))
            | canonicalFiniteValue minValue > canonicalFiniteValue maxValue ->
                (Just (CanonicalFinite minValue), Just (CanonicalFinite minValue))
          other -> other
   in nc
        { ncMin = fst clampedBounds,
          ncMax = snd clampedBounds,
          ncMultipleOf = canonicalMultiple
        }

canonicalizeBound :: Maybe CanonicalNumber -> Maybe CanonicalNumber
canonicalizeBound bound =
  case bound of
    Nothing -> Nothing
    Just value -> canonicalizeFinite value

canonicalizeFinite :: CanonicalNumber -> Maybe CanonicalNumber
canonicalizeFinite number =
  case number of
    CanonicalFinite _ -> Just number
    PosInf -> Nothing
    NegInf -> Nothing
    NaN -> Nothing

normalizeArrayConstraint :: ArrayConstraint -> ArrayConstraint
normalizeArrayConstraint ac =
  case acExactLength ac of
    Just exactValue ->
      let nonNegativeExact = max 0 exactValue
       in ac
            { acMinLength = Nothing,
              acMaxLength = Nothing,
              acExactLength = Just nonNegativeExact
            }
    Nothing ->
      let normalizedMin =
            case acMinLength ac of
              Just n | n <= 0 -> Nothing
              other -> other
          normalizedMax =
            case acMaxLength ac of
              Just n | n < 0 -> Nothing
              other -> other
          clampedMax =
            case (normalizedMin, normalizedMax) of
              (Just minLen, Just maxLen) | maxLen < minLen -> Just minLen
              (_, other) -> other
       in ac {acMinLength = normalizedMin, acMaxLength = clampedMax}

isEmptyStringConstraint :: StringConstraint -> Bool
isEmptyStringConstraint sc =
  scMinLength sc == Nothing
    && scMaxLength sc == Nothing
    && scPattern sc == Nothing

isEmptyNumberConstraint :: NumberConstraint -> Bool
isEmptyNumberConstraint nc =
  ncMin nc == Nothing
    && ncMax nc == Nothing
    && ncMultipleOf nc == Nothing
    && not (ncFinite nc)
    && not (ncInt nc)
    && not (ncPositive nc)
    && not (ncNegative nc)

isEmptyArrayConstraint :: ArrayConstraint -> Bool
isEmptyArrayConstraint ac =
  acMinLength ac == Nothing
    && acMaxLength ac == Nothing
    && acExactLength ac == Nothing

normalizeFormat :: SchemaNode -> SchemaNode
normalizeFormat node =
  case node of
    SString constraint (Just format) ->
      SString constraint (Just (normalizeStringFormat format))
    other -> other

normalizeStringFormat :: StringFormat -> StringFormat
normalizeStringFormat format =
  case format of
    Structural element -> Structural (normalizeFormatElement element)
    other -> other

normalizeFormatElement :: FormatElement -> FormatElement
normalizeFormatElement element =
  case element of
    Sequence [single] -> normalizeFormatElement single
    Choice [single] -> normalizeFormatElement single
    Sequence elements -> Sequence (map normalizeFormatElement elements)
    Choice elements ->
      Choice (List.sort (List.nub (map normalizeFormatElement elements)))
    Group inner quantifier ->
      Group (normalizeFormatElement inner) quantifier
    other -> other

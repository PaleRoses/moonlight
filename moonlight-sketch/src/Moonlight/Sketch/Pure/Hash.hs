module Moonlight.Sketch.Pure.Hash
  ( schemaHash,
    schemaEq,
    encodeSchemaNodeCanonical,
    encodeCanonicalValue,
  )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int64)
import Data.Kind (Type)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Normalize as TextNormalize
import Data.Word (Word64, Word8)
import Moonlight.Core (SipKey (..), canonicalFiniteValue, quantizeForHash, sipHashDigest)
import Moonlight.Sketch.Pure.Normalize (normalize)
import Moonlight.Sketch.Pure.Types
  ( ArrayConstraint (..),
    CanonicalNumber (..),
    CharClass (..),
    DefaultValue (..),
    FormatElement (..),
    LiteralValue (..),
    NumberConstraint (..),
    ObjectPropertyF (..),
    Quantifier (..),
    SchemaHash (..),
    SchemaNode (..),
    SemanticFormat (..),
    StringConstraint (..),
    StringFormat (..),
    SchemaF (..),
    cataSchema,
    unBrandName,
    unConstraintId,
    unPreprocessId,
    unRefId,
    unRefinementId,
    unTransformId,
  )

type CanonicalValue :: Type
data CanonicalValue
  = CInt64 Int64
  | CWord64 Word64
  | CBool Bool
  | CText Text
  | CBytes BS.ByteString
  | CSeq [CanonicalValue]
  | CRecord [(Text, CanonicalValue)]
  | CMap [(CanonicalValue, CanonicalValue)]
  | COption (Maybe CanonicalValue)
  deriving stock (Eq, Ord, Show)

schemaHash :: SchemaNode -> SchemaHash
schemaHash node =
  let normalized = normalize node
      encoded = encodeSchemaNodeCanonical normalized
   in SchemaHash (sipHashDigest schemaHashKey encoded)

schemaEq :: SchemaNode -> SchemaNode -> Bool
schemaEq left right = normalize left == normalize right

encodeSchemaNodeCanonical :: SchemaNode -> BS.ByteString
encodeSchemaNodeCanonical =
  encodeStableHashMessage . pure . toCanonicalSchema

encodeCanonicalValue :: SchemaNode -> BS.ByteString
encodeCanonicalValue = encodeSchemaNodeCanonical

schemaHashKey :: SipKey
schemaHashKey = SipKey 0x6D6F6F6E6C696768 0x74736B6574636831


encodeStableHashMessage :: [CanonicalValue] -> BS.ByteString
encodeStableHashMessage values =
  LBS.toStrict
    ( Builder.toLazyByteString
        ( Builder.byteString "PM_STABLEHASH_V1\0"
            <> Builder.word32LE (fromIntegral (length values))
            <> foldMap encodeValue values
        )
    )

encodeValue :: CanonicalValue -> Builder.Builder
encodeValue value =
  case value of
    CInt64 number -> encodeTagged 0x01 (Builder.int64LE number)
    CWord64 number -> encodeTagged 0x02 (Builder.word64LE number)
    CBool boolValue -> encodeTagged 0x03 (Builder.word8 (if boolValue then 0x01 else 0x00))
    CText textValue ->
      let normalizedText = TextNormalize.normalize TextNormalize.NFC textValue
       in encodeTagged 0x04 (Builder.byteString (TextEncoding.encodeUtf8 normalizedText))
    CBytes bytes -> encodeTagged 0x05 (Builder.byteString bytes)
    CSeq values ->
      encodeTagged
        0x06
        (Builder.word32LE (fromIntegral (length values)) <> foldMap encodeValue values)
    CRecord fields ->
      encodeTagged
        0x07
        ( Builder.word32LE (fromIntegral (length fields))
            <> foldMap
              (\(fieldName, fieldValue) -> encodeValue (CText fieldName) <> encodeValue fieldValue)
              fields
        )
    CMap keyValuePairs ->
      let sortedPairs = List.sortOn (encodedValueBytes . fst) keyValuePairs
       in encodeTagged
            0x08
            ( Builder.word32LE (fromIntegral (length sortedPairs))
                <> foldMap
                  (\(keyValue, entryValue) -> encodeValue keyValue <> encodeValue entryValue)
                  sortedPairs
            )
    COption maybeValue ->
      encodeTagged
        0x09
        ( case maybeValue of
            Nothing -> Builder.word8 0x00
            Just innerValue -> Builder.word8 0x01 <> encodeValue innerValue
        )

encodedValueBytes :: CanonicalValue -> BS.ByteString
encodedValueBytes = LBS.toStrict . Builder.toLazyByteString . encodeValue

encodeTagged :: Word8 -> Builder.Builder -> Builder.Builder
encodeTagged tag payloadBuilder =
  let payloadBytes = LBS.toStrict (Builder.toLazyByteString payloadBuilder)
   in Builder.word8 tag
        <> Builder.word32LE (fromIntegral (BS.length payloadBytes))
        <> Builder.byteString payloadBytes

tagged :: Word8 -> [CanonicalValue] -> CanonicalValue
tagged tagValue fields =
  CRecord
    [ ("tag", CWord64 (fromIntegral tagValue)),
      ("fields", CSeq fields)
    ]

toCanonicalOption :: (a -> CanonicalValue) -> Maybe a -> CanonicalValue
toCanonicalOption encoder = COption . fmap encoder

toCanonicalSchema :: SchemaNode -> CanonicalValue
toCanonicalSchema = cataSchema toCanonicalSchemaAlgebra

toCanonicalSchemaAlgebra :: SchemaF CanonicalValue -> CanonicalValue
toCanonicalSchemaAlgebra layer =
  case layer of
    SStringF constraint formatValue ->
      tagged
        0
        [ toCanonicalOption toCanonicalStringConstraint constraint,
          toCanonicalOption toCanonicalStringFormat formatValue
        ]
    SNumberF constraint ->
      tagged 1 [toCanonicalOption toCanonicalNumberConstraint constraint]
    SBoolF -> tagged 2 []
    SNullF -> tagged 3 []
    SUndefinedF -> tagged 4 []
    SVoidF -> tagged 5 []
    SUnknownF -> tagged 6 []
    SLiteralF value -> tagged 7 [toCanonicalLiteralValue value]
    SEnumF values -> tagged 8 [CSeq (map CText values)]
    SArrayF element constraint ->
      tagged
        9
        [ element,
          toCanonicalOption toCanonicalArrayConstraint constraint
        ]
    STupleF elements rest ->
      tagged
        10
        [ CSeq elements,
          toCanonicalOption id rest
        ]
    SRecordF value -> tagged 11 [value]
    SObjectF fields ->
      tagged
        12
        [ CRecord
            ( map
                (\(fieldName, propertyValue) -> (fieldName, toCanonicalObjectPropertyF propertyValue))
                (Map.toAscList fields)
            )
        ]
    SUnionF members -> tagged 13 [CSeq members]
    SDiscriminatedUnionF tagField members ->
      tagged 14 [CText tagField, CSeq members]
    SOptionalF inner -> tagged 15 [inner]
    SNullableF inner -> tagged 16 [inner]
    SDefaultF inner defaultValue ->
      tagged 17 [inner, toCanonicalDefaultValue defaultValue]
    SBrandF inner brandName -> tagged 18 [inner, CText (unBrandName brandName)]
    SRefineF inner refinementId -> tagged 19 [inner, CText (unRefinementId refinementId)]
    SPreprocessF inner preprocessId -> tagged 20 [inner, CText (unPreprocessId preprocessId)]
    SConstrainF inner constraintId -> tagged 21 [inner, CText (unConstraintId constraintId)]
    STransformF input output transformId ->
      tagged
        22
        [ input,
          output,
          CText (unTransformId transformId)
        ]
    SRefF refId -> tagged 23 [CText (unRefId refId)]
    SLazyF refId -> tagged 24 [CText (unRefId refId)]

toCanonicalStringConstraint :: StringConstraint -> CanonicalValue
toCanonicalStringConstraint stringConstraint =
  CRecord
    [ ("minLength", toCanonicalOption (CInt64 . fromIntegral) (scMinLength stringConstraint)),
      ("maxLength", toCanonicalOption (CInt64 . fromIntegral) (scMaxLength stringConstraint)),
      ("pattern", toCanonicalOption CText (scPattern stringConstraint))
    ]

toCanonicalNumberConstraint :: NumberConstraint -> CanonicalValue
toCanonicalNumberConstraint numberConstraint =
  CRecord
    [ ("min", toCanonicalOption toCanonicalNumber (ncMin numberConstraint)),
      ("max", toCanonicalOption toCanonicalNumber (ncMax numberConstraint)),
      ("multipleOf", toCanonicalOption toCanonicalNumber (ncMultipleOf numberConstraint)),
      ("finite", CBool (ncFinite numberConstraint)),
      ("int", CBool (ncInt numberConstraint)),
      ("positive", CBool (ncPositive numberConstraint)),
      ("negative", CBool (ncNegative numberConstraint))
    ]

toCanonicalArrayConstraint :: ArrayConstraint -> CanonicalValue
toCanonicalArrayConstraint arrayConstraint =
  CRecord
    [ ("minLength", toCanonicalOption (CInt64 . fromIntegral) (acMinLength arrayConstraint)),
      ("maxLength", toCanonicalOption (CInt64 . fromIntegral) (acMaxLength arrayConstraint)),
      ("exactLength", toCanonicalOption (CInt64 . fromIntegral) (acExactLength arrayConstraint))
    ]

toCanonicalObjectPropertyF :: ObjectPropertyF CanonicalValue -> CanonicalValue
toCanonicalObjectPropertyF objectProperty =
  CRecord
    [ ("required", CBool (opfRequired objectProperty)),
      ("readonly", CBool (opfReadonly objectProperty)),
      ("schema", opfSchema objectProperty)
    ]

toCanonicalDefaultValue :: DefaultValue -> CanonicalValue
toCanonicalDefaultValue defaultValue =
  case defaultValue of
    DefaultLiteral literalValue -> tagged 0 [toCanonicalLiteralValue literalValue]
    DefaultRef refId -> tagged 1 [CText (unRefId refId)]

toCanonicalLiteralValue :: LiteralValue -> CanonicalValue
toCanonicalLiteralValue literalValue =
  case literalValue of
    LitString textValue -> tagged 0 [CText textValue]
    LitNumber numberValue -> tagged 1 [toCanonicalNumber numberValue]
    LitBool boolValue -> tagged 2 [CBool boolValue]
    LitNull -> tagged 3 []

toCanonicalStringFormat :: StringFormat -> CanonicalValue
toCanonicalStringFormat stringFormat =
  case stringFormat of
    Semantic semanticFormat -> tagged 0 [toCanonicalSemanticFormat semanticFormat]
    Structural formatElement -> tagged 1 [toCanonicalFormatElement formatElement]

toCanonicalSemanticFormat :: SemanticFormat -> CanonicalValue
toCanonicalSemanticFormat semanticFormat =
  case semanticFormat of
    FUuid -> tagged 0 []
    FEmail -> tagged 1 []
    FUrl -> tagged 2 []
    FIsoDate -> tagged 3 []
    FIsoDateTime -> tagged 4 []
    FIp -> tagged 5 []
    FStartsWith textValue -> tagged 6 [CText textValue]
    FEndsWith textValue -> tagged 7 [CText textValue]
    FContains textValue -> tagged 8 [CText textValue]
    FOneOf values -> tagged 9 [CSeq (map CText values)]

toCanonicalFormatElement :: FormatElement -> CanonicalValue
toCanonicalFormatElement formatElement =
  case formatElement of
    Chars charClass quantifier -> tagged 0 [toCanonicalCharClass charClass, toCanonicalQuantifier quantifier]
    FLiteral textValue -> tagged 1 [CText textValue]
    Sequence elements -> tagged 2 [CSeq (map toCanonicalFormatElement elements)]
    Choice elements -> tagged 3 [CSeq (map toCanonicalFormatElement elements)]
    Group inner quantifier -> tagged 4 [toCanonicalFormatElement inner, toCanonicalQuantifier quantifier]

toCanonicalCharClass :: CharClass -> CanonicalValue
toCanonicalCharClass charClass =
  case charClass of
    Digit -> tagged 0 []
    Lower -> tagged 1 []
    Upper -> tagged 2 []
    Alpha -> tagged 3 []
    Alnum -> tagged 4 []
    Hex -> tagged 5 []
    Word -> tagged 6 []
    Whitespace -> tagged 7 []
    LiteralChars textValue -> tagged 8 [CText textValue]
    CharUnion classes -> tagged 9 [CSeq (map toCanonicalCharClass classes)]
    CharNegate inner -> tagged 10 [toCanonicalCharClass inner]

toCanonicalQuantifier :: Quantifier -> CanonicalValue
toCanonicalQuantifier quantifier =
  case quantifier of
    Exact value -> tagged 0 [CInt64 (fromIntegral value)]
    Range low high -> tagged 1 [CInt64 (fromIntegral low), toCanonicalOption (CInt64 . fromIntegral) high]
    Plus -> tagged 2 []
    Star -> tagged 3 []
    Optional -> tagged 4 []

toCanonicalNumber :: CanonicalNumber -> CanonicalValue
toCanonicalNumber number =
  case number of
    CanonicalFinite value ->
      case quantizeForHash 9 (canonicalFiniteValue value) of
        Right quantized -> tagged 0 [CInt64 quantized]
        Left _ -> tagged 1 [CText "non_quantizable_finite"]
    PosInf -> tagged 2 []
    NegInf -> tagged 3 []
    NaN -> tagged 4 []

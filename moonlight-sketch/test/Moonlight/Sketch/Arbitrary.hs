{-# OPTIONS_GHC -Wno-orphans #-}

module Moonlight.Sketch.Arbitrary () where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Moonlight.Core
  ( canonicalNumberFromDouble,
    canonicalNumberToMaybeDouble,
  )
import qualified Test.Tasty.QuickCheck as QC
import Moonlight.Sketch
  ( ArrayConstraint (..),
    BrandName,
    CanonicalNumber (..),
    CharClass (..),
    ConstraintId,
    FormatElement (..),
    LiteralValue (..),
    NumberConstraint (..),
    ObjectProperty (..),
    PreprocessId,
    Quantifier (..),
    RefinementId,
    RefId,
    SchemaNode (..),
    SchemaRegistry (..),
    SemanticFormat (..),
    StringConstraint (..),
    StringFormat (..),
    TransformId,
    mkBrandName,
    mkConstraintId,
    mkPreprocessId,
    mkRefId,
    mkRefinementId,
    mkTransformId,
  )

instance QC.Arbitrary SchemaNode where
  arbitrary = QC.sized genSchemaNode
  shrink = shrinkSchemaNode

genSchemaNode :: Int -> QC.Gen SchemaNode
genSchemaNode size
  | size <= 0 = QC.oneof primitiveGens
  | otherwise =
      let childSize = max 0 (size `div` 3)
       in QC.frequency
            [ (4, QC.oneof primitiveGens),
              (2, SArray <$> genSchemaNode childSize <*> QC.arbitrary),
              (1, STuple <$> genNonEmptyList 3 (genSchemaNode childSize) <*> QC.frequency [(3, pure Nothing), (1, Just <$> genSchemaNode childSize)]),
              (1, SRecord <$> genSchemaNode childSize),
              (1, genObject childSize),
              (2, SUnion <$> genNonEmptyList 4 (genSchemaNode childSize)),
              (1, SOptional <$> genSchemaNode childSize),
              (1, SNullable <$> genSchemaNode childSize),
              (1, SBrand <$> genSchemaNode childSize <*> QC.arbitrary),
              (1, SRefine <$> genSchemaNode childSize <*> QC.arbitrary),
              (1, SConstrain <$> genSchemaNode childSize <*> QC.arbitrary),
              (1, SPreprocess <$> genSchemaNode childSize <*> QC.arbitrary),
              (1, STransform <$> genSchemaNode childSize <*> genSchemaNode childSize <*> QC.arbitrary),
              (1, SRef <$> QC.arbitrary),
              (1, SLazy <$> QC.arbitrary)
            ]

primitiveGens :: [QC.Gen SchemaNode]
primitiveGens =
  [ SString <$> QC.arbitrary <*> QC.arbitrary,
    SNumber <$> QC.arbitrary,
    pure SBool,
    pure SNull,
    pure SUndefined,
    pure SVoid,
    pure SUnknown,
    SLiteral <$> QC.arbitrary,
    SEnum <$> genNonEmptyList 5 genSmallText
  ]

genObject :: Int -> QC.Gen SchemaNode
genObject size = do
  fieldCount <- QC.chooseInt (1, 4)
  fields <- QC.vectorOf fieldCount (genField size)
  pure (SObject (Map.fromList fields))


genField :: Int -> QC.Gen (Text, ObjectProperty)
genField size = do
  name <- genSmallText
  propertyValue <-
    ObjectProperty
      <$> QC.arbitrary
      <*> QC.arbitrary
      <*> genSchemaNode (max 0 (size `div` 2))
  pure (name, propertyValue)

genSmallText :: QC.Gen Text
genSmallText =
  Text.pack <$> QC.listOf1 (QC.elements ['a' .. 'z'])

shrinkSchemaNode :: SchemaNode -> [SchemaNode]
shrinkSchemaNode node =
  case node of
    SString _ _ -> [SString Nothing Nothing]
    SNumber _ -> [SNumber Nothing]
    SArray element constraint ->
      [element] <> map (\child -> SArray child constraint) (shrinkSchemaNode element)
    STuple elements rest ->
      elements <> map (\elementsValue -> STuple elementsValue rest) (QC.shrink elements)
    SRecord value ->
      value : map SRecord (shrinkSchemaNode value)
    SObject fields ->
      map opSchema (Map.elems fields)
    SUnion members ->
      members <> map SUnion (QC.shrink members)
    SDiscriminatedUnion tagField members ->
      members <> map (SDiscriminatedUnion tagField) (QC.shrink members)
    SOptional inner -> inner : map SOptional (shrinkSchemaNode inner)
    SNullable inner -> inner : map SNullable (shrinkSchemaNode inner)
    SDefault inner _ -> [inner]
    SBrand inner brandName -> inner : map (\child -> SBrand child brandName) (shrinkSchemaNode inner)
    SRefine inner refinementId -> inner : map (\child -> SRefine child refinementId) (shrinkSchemaNode inner)
    SPreprocess inner preprocessId -> inner : map (\child -> SPreprocess child preprocessId) (shrinkSchemaNode inner)
    SConstrain inner constraintId -> inner : map (\child -> SConstrain child constraintId) (shrinkSchemaNode inner)
    STransform input output transformId -> [input, output, STransform input input transformId]
    _ -> []

instance QC.Arbitrary StringConstraint where
  arbitrary =
    StringConstraint
      <$> QC.arbitrary
      <*> QC.arbitrary
      <*> QC.frequency [(2, pure Nothing), (3, Just <$> genSmallText)]

instance QC.Arbitrary NumberConstraint where
  arbitrary =
    NumberConstraint
      <$> genMaybeCanonicalNumber
      <*> genMaybeCanonicalNumber
      <*> genMaybeCanonicalNumber
      <*> QC.arbitrary
      <*> QC.arbitrary
      <*> QC.arbitrary
      <*> QC.arbitrary

instance QC.Arbitrary ArrayConstraint where
  arbitrary =
    ArrayConstraint
      <$> (fmap QC.getNonNegative <$> QC.arbitrary)
      <*> (fmap QC.getNonNegative <$> QC.arbitrary)
      <*> (fmap QC.getNonNegative <$> QC.arbitrary)

instance QC.Arbitrary LiteralValue where
  arbitrary =
    QC.oneof
      [ LitString <$> genSmallText,
        LitNumber <$> QC.arbitrary,
        LitBool <$> QC.arbitrary,
        pure LitNull
      ]

instance QC.Arbitrary CanonicalNumber where
  arbitrary =
    QC.frequency
      [ (6, canonicalNumberFromDouble <$> genFiniteDouble),
        (1, pure PosInf),
        (1, pure NegInf),
        (1, pure NaN)
      ]
  shrink number =
    case number of
      CanonicalFinite _ ->
        maybe [] (map canonicalNumberFromDouble . QC.shrink) (canonicalNumberToMaybeDouble number)
      PosInf -> [canonicalNumberFromDouble 0]
      NegInf -> [canonicalNumberFromDouble 0]
      NaN -> [canonicalNumberFromDouble 0]

instance QC.Arbitrary StringFormat where
  arbitrary =
    QC.oneof
      [ Semantic <$> QC.arbitrary,
        Structural <$> QC.sized genFormatElement
      ]

instance QC.Arbitrary SemanticFormat where
  arbitrary =
    QC.oneof
      [ pure FUuid,
        pure FEmail,
        pure FUrl,
        pure FIsoDate,
        pure FIsoDateTime,
        pure FIp,
        FStartsWith <$> genSmallText,
        FEndsWith <$> genSmallText,
        FContains <$> genSmallText,
        FOneOf <$> genNonEmptyList 5 genSmallText
      ]

instance QC.Arbitrary FormatElement where
  arbitrary = QC.sized genFormatElement

instance QC.Arbitrary CharClass where
  arbitrary =
    QC.oneof
      [ pure Digit,
        pure Lower,
        pure Upper,
        pure Alpha,
        pure Alnum,
        pure Hex,
        pure Word,
        pure Whitespace,
        LiteralChars <$> genSmallText,
        CharUnion <$> genNonEmptyList 4 QC.arbitrary,
        CharNegate <$> QC.arbitrary
      ]

instance QC.Arbitrary Quantifier where
  arbitrary =
    QC.oneof
      [ Exact <$> QC.chooseInt (0, 5),
        Range <$> QC.chooseInt (0, 3) <*> QC.frequency [(1, pure Nothing), (3, Just <$> QC.chooseInt (0, 5))],
        pure Plus,
        pure Star,
        pure Optional
      ]

genFormatElement :: Int -> QC.Gen FormatElement
genFormatElement size
  | size <= 0 =
      QC.oneof
        [ FLiteral <$> genSmallText,
          Chars <$> QC.arbitrary <*> QC.arbitrary
        ]
  | otherwise =
      let childSize = max 0 (size `div` 2)
       in QC.frequency
            [ (2, FLiteral <$> genSmallText),
              (2, Chars <$> QC.arbitrary <*> QC.arbitrary),
              (1, Sequence <$> genNonEmptyList 4 (genFormatElement childSize)),
              (1, Choice <$> genNonEmptyList 4 (genFormatElement childSize)),
              (1, Group <$> genFormatElement childSize <*> QC.arbitrary)
            ]

instance QC.Arbitrary BrandName where
  arbitrary = genIdentifier mkBrandName

instance QC.Arbitrary TransformId where
  arbitrary = genIdentifier mkTransformId

instance QC.Arbitrary RefId where
  arbitrary = genIdentifier mkRefId

instance QC.Arbitrary RefinementId where
  arbitrary = genIdentifier mkRefinementId

instance QC.Arbitrary PreprocessId where
  arbitrary = genIdentifier mkPreprocessId

instance QC.Arbitrary ConstraintId where
  arbitrary = genIdentifier mkConstraintId

instance QC.Arbitrary SchemaRegistry where
  arbitrary = QC.sized $ \size -> do
    entryCount <- QC.chooseInt (0, min size 5)
    entries <- QC.vectorOf entryCount genRegistryEntry
    pure (SchemaRegistry (Map.fromList entries))
    where
      genRegistryEntry = do
        referenceId <- QC.arbitrary
        node <- genSchemaNode 2
        pure (referenceId, node)

genFiniteDouble :: QC.Gen Double
genFiniteDouble = QC.choose (-1000000, 1000000)

genMaybeCanonicalNumber :: QC.Gen (Maybe CanonicalNumber)
genMaybeCanonicalNumber =
  QC.frequency
    [ (2, pure Nothing),
      (3, Just <$> QC.arbitrary)
    ]

genNonEmptyList :: Int -> QC.Gen a -> QC.Gen [a]
genNonEmptyList maxLength generator = do
  listLength <- QC.chooseInt (1, max 1 maxLength)
  QC.vectorOf listLength generator

genIdentifier :: (Text -> Maybe identifier) -> QC.Gen identifier
genIdentifier mkIdentifier =
  unsafeIdentifier mkIdentifier <$> genSmallText

unsafeIdentifier :: (Text -> Maybe identifier) -> Text -> identifier
unsafeIdentifier mkIdentifier rawIdentifier =
  case mkIdentifier rawIdentifier of
    Just identifier -> identifier
    Nothing -> error "expected valid identifier in generator"

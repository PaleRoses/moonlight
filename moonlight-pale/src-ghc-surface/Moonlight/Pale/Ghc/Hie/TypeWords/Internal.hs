{-# LANGUAGE LambdaCase #-}

module Moonlight.Pale.Ghc.Hie.TypeWords.Internal
  ( TypeWords,
    TypeWord (..),
    TypeWordOpcode (..),
    TypeArgumentVisibility (..),
    TypeVariableFlavor (..),
    typeWords,
    typeWordsList,
    outputTypeWords,
    stringTypeWords,
  )
where

import Data.Kind (Type)
import Data.Word (Word64)
import GHC.Iface.Ext.Types (TypeIndex)
import GHC.Utils.Outputable (Outputable, ppr, showSDocUnsafe)
import Numeric.Natural (Natural)

type TypeWords :: Type
newtype TypeWords = TypeWords [TypeWord]
  deriving stock (Eq, Ord, Show)

type TypeWord :: Type
data TypeWord
  = TypeOpcode !TypeWordOpcode
  | TypeArgumentCount !Natural
  | TypeArgumentVisibilityWord !TypeArgumentVisibility
  | TypeVariableFlavorWord !TypeVariableFlavor
  | TypeBoundVariable !Natural
  | TypeOutputText !String
  | TypeOutOfRangeReference !TypeIndex
  | TypeCycleReference !TypeIndex
  deriving stock (Eq, Ord, Show)

type TypeWordOpcode :: Type
data TypeWordOpcode
  = TypeAppOpcode
  | TypeFunOpcode
  | TypeQualOpcode
  | TypeForAllOpcode
  | TypeVariableOpcode
  | TypeCastOpcode
  | TypeCoercionOpcode
  | TypeTyConAppOpcode
  | TypeLiteralOpcode
  | TypeOutOfRangeReferenceOpcode
  | TypeCycleReferenceOpcode
  deriving stock (Eq, Ord, Show, Enum, Bounded)

type TypeArgumentVisibility :: Type
data TypeArgumentVisibility
  = TypeArgumentHidden
  | TypeArgumentVisible
  deriving stock (Eq, Ord, Show, Enum, Bounded)

type TypeVariableFlavor :: Type
data TypeVariableFlavor
  = TypeFreeVariableFlavor
  | TypeBoundVariableFlavor
  deriving stock (Eq, Ord, Show, Enum, Bounded)

typeWords :: [TypeWord] -> TypeWords
typeWords =
  TypeWords

typeWordsList :: TypeWords -> [Word64]
typeWordsList (TypeWords wordsValue) =
  foldMap renderTypeWord wordsValue

outputTypeWords :: Outputable value => value -> [TypeWord]
outputTypeWords =
  stringTypeWords . outputString

stringTypeWords :: String -> [TypeWord]
stringTypeWords textValue =
  [TypeOutputText textValue]

renderTypeWord :: TypeWord -> [Word64]
renderTypeWord = \case
  TypeOpcode opcode ->
    [boundedTagWord opcode]
  TypeArgumentCount count ->
    [naturalWord count]
  TypeArgumentVisibilityWord visibility ->
    [boundedTagWord visibility]
  TypeVariableFlavorWord flavor ->
    [boundedTagWord flavor]
  TypeBoundVariable deBruijnIndex ->
    [naturalWord deBruijnIndex]
  TypeOutputText textValue ->
    fromIntegral (length textValue) : fmap (fromIntegral . fromEnum) textValue
  TypeOutOfRangeReference typeIndex ->
    boundedTagWord TypeOutOfRangeReferenceOpcode : typeIndexWords typeIndex
  TypeCycleReference typeIndex ->
    boundedTagWord TypeCycleReferenceOpcode : typeIndexWords typeIndex

boundedTagWord :: Enum tag => tag -> Word64
boundedTagWord tagValue =
  fromIntegral (fromEnum tagValue + 1)

typeIndexWords :: TypeIndex -> [Word64]
typeIndexWords typeIndex =
  signedIntWords (fromIntegral typeIndex)

signedIntWords :: Int -> [Word64]
signedIntWords value =
  if value < 0
    then [0, fromIntegral (abs value)]
    else [1, fromIntegral value]

naturalWord :: Natural -> Word64
naturalWord =
  fromIntegral

outputString :: Outputable value => value -> String
outputString =
  showSDocUnsafe . ppr

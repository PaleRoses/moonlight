{-# LANGUAGE LambdaCase #-}

module Moonlight.Pale.Ghc.Hie.TypeWords
  ( TypeWords,
    TypeWord (..),
    TypeWordOpcode (..),
    TypeArgumentVisibility (..),
    TypeVariableFlavor (..),
    typeWords,
    typeWordsList,
    tyConTypeWords,
    hieTypeIndexTypeWords,
  )
where

import Data.Array (Array, assocs, bounds, inRange)
import Data.List (elemIndex)
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Iface.Ext.Types (HieArgs (..), HieType (..), HieTypeFlat, TypeIndex)
import GHC.Utils.Outputable (Outputable, ppr, showSDocUnsafe)
import Numeric.Natural (Natural)
import Moonlight.Pale.Ghc.Hie.TypeWords.Internal

tyConTypeWords :: String -> TypeWords
tyConTypeWords tyConName =
  typeWords (TypeOpcode TypeTyConAppOpcode : stringTypeWords tyConName <> [TypeArgumentCount 0])

hieTypeIndexTypeWords :: Array TypeIndex HieTypeFlat -> TypeIndex -> TypeWords
hieTypeIndexTypeWords typeTable =
  typeWords . hieTypeIndexWordsWith Set.empty []
  where
    hieTypeIndexWordsWith :: Set TypeIndex -> [String] -> TypeIndex -> [TypeWord]
    hieTypeIndexWordsWith seen boundNames typeIndex
      | not (inRange (bounds typeTable) typeIndex) =
          [TypeOutOfRangeReference typeIndex]
      | Set.member typeIndex seen =
          [TypeCycleReference typeIndex]
      | otherwise =
          maybe
            [TypeOutOfRangeReference typeIndex]
            (hieTypeWords (Set.insert typeIndex seen) boundNames)
            (lookup typeIndex (assocs typeTable))

    hieTypeWords seen boundNames = \case
      HAppTy functionType argumentTypes ->
        TypeOpcode TypeAppOpcode : hieTypeIndexWordsWith seen boundNames functionType <> hieArgsWords seen boundNames argumentTypes
      HFunTy multiplicityType argumentType resultType ->
        TypeOpcode TypeFunOpcode : foldMap (hieTypeIndexWordsWith seen boundNames) [multiplicityType, argumentType, resultType]
      HQualTy predicateType bodyType ->
        TypeOpcode TypeQualOpcode : hieTypeIndexWordsWith seen boundNames predicateType <> hieTypeIndexWordsWith seen boundNames bodyType
      HForAllTy ((binderName, binderKind), flagValue) bodyType ->
        let binderKey = outputString binderName
         in TypeOpcode TypeForAllOpcode
              : hieTypeIndexWordsWith seen boundNames binderKind
                <> outputTypeWords flagValue
                <> hieTypeIndexWordsWith seen (binderKey : boundNames) bodyType
      HTyVarTy nameValue ->
        case elemIndex (outputString nameValue) boundNames of
          Just deBruijnIndex ->
            [ TypeOpcode TypeVariableOpcode,
              TypeVariableFlavorWord TypeBoundVariableFlavor,
              TypeBoundVariable (naturalFromInt deBruijnIndex)
            ]
          Nothing ->
            TypeOpcode TypeVariableOpcode : TypeVariableFlavorWord TypeFreeVariableFlavor : outputTypeWords nameValue
      HCastTy castType ->
        TypeOpcode TypeCastOpcode : hieTypeIndexWordsWith seen boundNames castType
      HCoercionTy ->
        [TypeOpcode TypeCoercionOpcode]
      HTyConApp tyCon argumentTypes ->
        TypeOpcode TypeTyConAppOpcode : outputTypeWords tyCon <> hieArgsWords seen boundNames argumentTypes
      HLitTy literalType ->
        TypeOpcode TypeLiteralOpcode : outputTypeWords literalType

    hieArgsWords seen boundNames (HieArgs arguments) =
      TypeArgumentCount (naturalFromInt (length arguments)) : foldMap (hieArgumentWords seen boundNames) arguments

    hieArgumentWords seen boundNames (visible, typeIndex) =
      TypeArgumentVisibilityWord (if visible then TypeArgumentVisible else TypeArgumentHidden)
        : hieTypeIndexWordsWith seen boundNames typeIndex

outputString :: Outputable value => value -> String
outputString =
  showSDocUnsafe . ppr

naturalFromInt :: Int -> Natural
naturalFromInt value =
  if value < 0 then 0 else fromIntegral value

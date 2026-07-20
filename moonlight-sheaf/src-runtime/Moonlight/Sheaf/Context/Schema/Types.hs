{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Moonlight.Sheaf.Context.Schema.Types
  ( RestrictionKernelSchema (..),
  )
where

import Data.Aeson
  ( FromJSON (..),
    Options (..),
    ToJSON (..),
    camelTo2,
    defaultOptions,
    genericParseJSON,
    genericToEncoding,
    genericToJSON,
  )
import Data.Char (toLower)
import Data.Kind (Type)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Language.Haskell.TH.Syntax (Lift)

type RestrictionKernelSchema :: Type
data RestrictionKernelSchema = RestrictionKernelSchema
  { restrictionKernelMorphismConstructor :: String,
    restrictionKernelIdentityMorphism :: String,
    restrictionKernelComposeMorphism :: String,
    restrictionKernelCanonicalSection :: String,
    restrictionKernelRestrictSection :: String,
    restrictionKernelGlobalSectionPredicate :: String,
    restrictionKernelRuntimeLawWitnesses :: [String],
    restrictionKernelLeanTheoremWitnesses :: [String],
    restrictionKernelManifestSubset :: [String]
  }
  deriving stock (Eq, Ord, Show, Read, Generic, Lift)

instance FromJSON RestrictionKernelSchema where
  parseJSON =
    genericParseJSON restrictionKernelSchemaJsonOptions

instance ToJSON RestrictionKernelSchema where
  toJSON =
    genericToJSON restrictionKernelSchemaJsonOptions

  toEncoding =
    genericToEncoding restrictionKernelSchemaJsonOptions

restrictionKernelSchemaJsonOptions :: Options
restrictionKernelSchemaJsonOptions =
  defaultOptions
    { fieldLabelModifier =
        camelTo2 '_' . lowerInitial . stripRestrictionKernelPrefix
    }

stripRestrictionKernelPrefix :: String -> String
stripRestrictionKernelPrefix fieldName =
  fromMaybe fieldName (stripPrefix "restrictionKernel" fieldName)

lowerInitial :: String -> String
lowerInitial [] =
  []
lowerInitial (firstCharacter : remainingCharacters) =
  toLower firstCharacter : remainingCharacters

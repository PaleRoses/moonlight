{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Sheaf.Site.Interface.Types
  ( InterfaceDirectionEstimate (..),
    InterfaceMeasure (..),
    InterfaceName,
    MorphismInterface (..),
    interfaceNameFromString,
    interfaceNameFromText,
    interfaceNameText,
  )
where

import Data.Kind (Type)
import Data.Monoid (Any (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text

type InterfaceName :: Type -> Type
newtype InterfaceName tag = InterfaceName
  { interfaceNameText :: Text
  }
  deriving stock (Eq, Ord, Show)

interfaceNameFromText :: Text -> InterfaceName tag
interfaceNameFromText =
  InterfaceName

interfaceNameFromString :: String -> InterfaceName tag
interfaceNameFromString =
  interfaceNameFromText . Text.pack

type InterfaceDirectionEstimate :: Type
newtype InterfaceDirectionEstimate = InterfaceDirectionEstimate
  { interfaceDirectionEstimateValue :: Int
  }
  deriving stock (Eq, Ord, Show)

type MorphismInterface :: Type -> Type
data MorphismInterface tag = MorphismInterface
  { miBoundNames :: Set (InterfaceName tag),
    miDeletedNames :: Set (InterfaceName tag),
    miCreatedNames :: Set (InterfaceName tag),
    miGuarded :: Bool,
    miDirectionEstimate :: InterfaceDirectionEstimate
  }
  deriving stock (Eq, Ord, Show)

type InterfaceMeasure :: Type -> Type
data InterfaceMeasure tag = InterfaceMeasure
  { imBoundNames :: Set (InterfaceName tag),
    imDeletedNames :: Set (InterfaceName tag),
    imCreatedNames :: Set (InterfaceName tag),
    imGuarded :: Any
  }
  deriving stock (Eq, Ord, Show)

instance Semigroup (InterfaceMeasure tag) where
  leftMeasure <> rightMeasure =
    InterfaceMeasure
      { imBoundNames = imBoundNames leftMeasure <> imBoundNames rightMeasure,
        imDeletedNames = imDeletedNames leftMeasure <> imDeletedNames rightMeasure,
        imCreatedNames = imCreatedNames leftMeasure <> imCreatedNames rightMeasure,
        imGuarded = imGuarded leftMeasure <> imGuarded rightMeasure
      }

instance Monoid (InterfaceMeasure tag) where
  mempty =
    InterfaceMeasure
      { imBoundNames = Set.empty,
        imDeletedNames = Set.empty,
        imCreatedNames = Set.empty,
        imGuarded = Any False
      }

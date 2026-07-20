module Moonlight.Sketch.Pure.Types.Format
  ( Variance (..),
    CharClass (..),
    Quantifier (..),
    FormatElement (..),
    StringFormat (..),
    SemanticFormat (..),
  )
where

import Data.Kind (Type)
import Data.Text (Text)
import GHC.Generics (Generic)

type Variance :: Type
data Variance
  = Covariant
  | Contravariant
  | Invariant
  | Bivariant
  deriving stock (Eq, Ord, Show, Read, Bounded, Enum)

type CharClass :: Type
data CharClass
  = Digit
  | Lower
  | Upper
  | Alpha
  | Alnum
  | Hex
  | Word
  | Whitespace
  | LiteralChars Text
  | CharUnion [CharClass]
  | CharNegate CharClass
  deriving stock (Eq, Ord, Show, Read, Generic)

type Quantifier :: Type
data Quantifier
  = Exact Int
  | Range Int (Maybe Int)
  | Plus
  | Star
  | Optional
  deriving stock (Eq, Ord, Show, Read, Generic)

type FormatElement :: Type
data FormatElement
  = Chars CharClass Quantifier
  | FLiteral Text
  | Sequence [FormatElement]
  | Choice [FormatElement]
  | Group FormatElement Quantifier
  deriving stock (Eq, Ord, Show, Read, Generic)

type StringFormat :: Type
data StringFormat
  = Semantic SemanticFormat
  | Structural FormatElement
  deriving stock (Eq, Ord, Show, Read, Generic)

type SemanticFormat :: Type
data SemanticFormat
  = FUuid
  | FEmail
  | FUrl
  | FIsoDate
  | FIsoDateTime
  | FIp
  | FStartsWith Text
  | FEndsWith Text
  | FContains Text
  | FOneOf [Text]
  deriving stock (Eq, Ord, Show, Read, Generic)

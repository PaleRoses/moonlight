module Moonlight.Sketch.Pure.Types.Identifiers
  ( BrandName,
    mkBrandName,
    unBrandName,
    TransformId,
    mkTransformId,
    unTransformId,
    RefId,
    mkRefId,
    unRefId,
    RefinementId,
    mkRefinementId,
    unRefinementId,
    PreprocessId,
    mkPreprocessId,
    unPreprocessId,
    ConstraintId,
    mkConstraintId,
    unConstraintId,
    SchemaHash (..),
  )
where

import Data.Kind (Type)
import Data.Text (Text)
import Data.Word (Word64)
import Moonlight.Core
  ( IdentifierToken,
    mkScopedIdentifier,
    renderScopedIdentifier,
  )

type BrandNameNamespace :: Type
data BrandNameNamespace

type BrandName :: Type
newtype BrandName = MkBrandName (IdentifierToken BrandNameNamespace)
  deriving stock (Eq, Ord, Show)

type TransformIdNamespace :: Type
data TransformIdNamespace

type TransformId :: Type
newtype TransformId = MkTransformId (IdentifierToken TransformIdNamespace)
  deriving stock (Eq, Ord, Show)

type RefIdNamespace :: Type
data RefIdNamespace

type RefId :: Type
newtype RefId = MkRefId (IdentifierToken RefIdNamespace)
  deriving stock (Eq, Ord, Show)

type RefinementIdNamespace :: Type
data RefinementIdNamespace

type RefinementId :: Type
newtype RefinementId = MkRefinementId (IdentifierToken RefinementIdNamespace)
  deriving stock (Eq, Ord, Show)

type PreprocessIdNamespace :: Type
data PreprocessIdNamespace

type PreprocessId :: Type
newtype PreprocessId = MkPreprocessId (IdentifierToken PreprocessIdNamespace)
  deriving stock (Eq, Ord, Show)

type ConstraintIdNamespace :: Type
data ConstraintIdNamespace

type ConstraintId :: Type
newtype ConstraintId = MkConstraintId (IdentifierToken ConstraintIdNamespace)
  deriving stock (Eq, Ord, Show)

type SchemaHash :: Type
newtype SchemaHash = SchemaHash {unSchemaHash :: Word64}
  deriving stock (Eq, Ord, Show, Read)

mkBrandName :: Text -> Maybe BrandName
mkBrandName =
  mkScopedIdentifier MkBrandName

mkTransformId :: Text -> Maybe TransformId
mkTransformId =
  mkScopedIdentifier MkTransformId

mkRefId :: Text -> Maybe RefId
mkRefId =
  mkScopedIdentifier MkRefId

mkRefinementId :: Text -> Maybe RefinementId
mkRefinementId =
  mkScopedIdentifier MkRefinementId

mkPreprocessId :: Text -> Maybe PreprocessId
mkPreprocessId =
  mkScopedIdentifier MkPreprocessId

mkConstraintId :: Text -> Maybe ConstraintId
mkConstraintId =
  mkScopedIdentifier MkConstraintId

unBrandName :: BrandName -> Text
unBrandName =
  renderScopedIdentifier (\(MkBrandName identifierToken) -> identifierToken)

unTransformId :: TransformId -> Text
unTransformId =
  renderScopedIdentifier (\(MkTransformId identifierToken) -> identifierToken)

unRefId :: RefId -> Text
unRefId =
  renderScopedIdentifier (\(MkRefId identifierToken) -> identifierToken)

unRefinementId :: RefinementId -> Text
unRefinementId =
  renderScopedIdentifier (\(MkRefinementId identifierToken) -> identifierToken)

unPreprocessId :: PreprocessId -> Text
unPreprocessId =
  renderScopedIdentifier (\(MkPreprocessId identifierToken) -> identifierToken)

unConstraintId :: ConstraintId -> Text
unConstraintId =
  renderScopedIdentifier (\(MkConstraintId identifierToken) -> identifierToken)

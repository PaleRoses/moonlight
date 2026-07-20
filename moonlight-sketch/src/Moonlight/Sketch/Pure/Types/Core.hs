module Moonlight.Sketch.Pure.Types.Core
  ( SchemaNode (..),
    SchemaF (..),
    ObjectPropertyF (..),
    CanonicalNumber (..),
    StringConstraint (..),
    NumberConstraint (..),
    ArrayConstraint (..),
    LiteralValue (..),
    ObjectProperty (..),
    DefaultValue (..),
    TransformFns (..),
    SchemaRegistry (..),
    emptyStringConstraint,
    emptyNumberConstraint,
    emptyArrayConstraint,
    emptySchemaRegistry,
    projectSchema,
    embedSchema,
    objectPropertyToF,
    objectPropertyFromF,
    cataSchema,
    paraSchema,
    schemaNodeChildren,
  )
where

import Data.Aeson (Value)
import Data.Kind (Type)
import qualified Data.Foldable as Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Functor.Foldable (Base)
import Data.Functor.Foldable (Corecursive (embed), Recursive (project))
import Data.Functor.Foldable (cata, para)
import Moonlight.Core (CanonicalNumber (..))
import Moonlight.Sketch.Pure.Types.Format
  ( StringFormat,
  )
import Moonlight.Sketch.Pure.Types.Identifiers
  ( BrandName,
    ConstraintId,
    PreprocessId,
    RefId,
    RefinementId,
    TransformId,
  )
import Moonlight.Sketch.Pure.Types.Issues
  ( SchemaHookContext,
    SchemaIssue,
  )

type SchemaNode :: Type
data SchemaNode
  = SString (Maybe StringConstraint) (Maybe StringFormat)
  | SNumber (Maybe NumberConstraint)
  | SBool
  | SNull
  | SUndefined
  | SVoid
  | SUnknown
  | SLiteral LiteralValue
  | SEnum [Text]
  | SArray SchemaNode (Maybe ArrayConstraint)
  | STuple [SchemaNode] (Maybe SchemaNode)
  | SRecord SchemaNode
  | SObject (Map Text ObjectProperty)
  | SUnion [SchemaNode]
  | SDiscriminatedUnion Text [SchemaNode]
  | SOptional SchemaNode
  | SNullable SchemaNode
  | SDefault SchemaNode DefaultValue
  | SBrand SchemaNode BrandName
  | SRefine SchemaNode RefinementId
  | SPreprocess SchemaNode PreprocessId
  | SConstrain SchemaNode ConstraintId
  | STransform SchemaNode SchemaNode TransformId
  | SRef RefId
  | SLazy RefId
  deriving stock (Eq, Ord, Show, Generic)

type StringConstraint :: Type
data StringConstraint = StringConstraint
  { scMinLength :: Maybe Int,
    scMaxLength :: Maybe Int,
    scPattern :: Maybe Text
  }
  deriving stock (Eq, Ord, Show, Read, Generic)

type NumberConstraint :: Type
data NumberConstraint = NumberConstraint
  { ncMin :: Maybe CanonicalNumber,
    ncMax :: Maybe CanonicalNumber,
    ncMultipleOf :: Maybe CanonicalNumber,
    ncFinite :: Bool,
    ncInt :: Bool,
    ncPositive :: Bool,
    ncNegative :: Bool
  }
  deriving stock (Eq, Ord, Show, Generic)

type ArrayConstraint :: Type
data ArrayConstraint = ArrayConstraint
  { acMinLength :: Maybe Int,
    acMaxLength :: Maybe Int,
    acExactLength :: Maybe Int
  }
  deriving stock (Eq, Ord, Show, Read, Generic)

emptyStringConstraint :: StringConstraint
emptyStringConstraint =
  StringConstraint
    { scMinLength = Nothing,
      scMaxLength = Nothing,
      scPattern = Nothing
    }

emptyNumberConstraint :: NumberConstraint
emptyNumberConstraint =
  NumberConstraint
    { ncMin = Nothing,
      ncMax = Nothing,
      ncMultipleOf = Nothing,
      ncFinite = False,
      ncInt = False,
      ncPositive = False,
      ncNegative = False
    }

emptyArrayConstraint :: ArrayConstraint
emptyArrayConstraint =
  ArrayConstraint
    { acMinLength = Nothing,
      acMaxLength = Nothing,
      acExactLength = Nothing
    }

type LiteralValue :: Type
data LiteralValue
  = LitString Text
  | LitNumber CanonicalNumber
  | LitBool Bool
  | LitNull
  deriving stock (Eq, Ord, Show, Generic)

type ObjectProperty :: Type
data ObjectProperty = ObjectProperty
  { opRequired :: Bool,
    opReadonly :: Bool,
    opSchema :: SchemaNode
  }
  deriving stock (Eq, Ord, Show, Generic)

type ObjectPropertyF :: Type -> Type
data ObjectPropertyF a = ObjectPropertyF
  { opfRequired :: Bool,
    opfReadonly :: Bool,
    opfSchema :: a
  }
  deriving stock (Eq, Ord, Show, Read, Generic, Functor, Foldable, Traversable)

type DefaultValue :: Type
data DefaultValue
  = DefaultLiteral LiteralValue
  | DefaultRef RefId
  deriving stock (Eq, Ord, Show, Generic)

type TransformFns :: Type
data TransformFns = TransformFns
  { tfForward :: SchemaHookContext -> Value -> Either [SchemaIssue] Value,
    tfReverse :: SchemaHookContext -> Value -> Either [SchemaIssue] Value
  }

type SchemaRegistry :: Type
data SchemaRegistry = SchemaRegistry
  { srSchemas :: Map RefId SchemaNode
  }
  deriving stock (Eq, Ord, Show)

emptySchemaRegistry :: SchemaRegistry
emptySchemaRegistry = SchemaRegistry Map.empty

type SchemaF :: Type -> Type
data SchemaF a
  = SStringF (Maybe StringConstraint) (Maybe StringFormat)
  | SNumberF (Maybe NumberConstraint)
  | SBoolF
  | SNullF
  | SUndefinedF
  | SVoidF
  | SUnknownF
  | SLiteralF LiteralValue
  | SEnumF [Text]
  | SArrayF a (Maybe ArrayConstraint)
  | STupleF [a] (Maybe a)
  | SRecordF a
  | SObjectF (Map Text (ObjectPropertyF a))
  | SUnionF [a]
  | SDiscriminatedUnionF Text [a]
  | SOptionalF a
  | SNullableF a
  | SDefaultF a DefaultValue
  | SBrandF a BrandName
  | SRefineF a RefinementId
  | SPreprocessF a PreprocessId
  | SConstrainF a ConstraintId
  | STransformF a a TransformId
  | SRefF RefId
  | SLazyF RefId
  deriving stock (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

type instance Base SchemaNode = SchemaF

instance Recursive SchemaNode where
  project = projectSchema

instance Corecursive SchemaNode where
  embed = embedSchema

projectSchema :: SchemaNode -> SchemaF SchemaNode
projectSchema node =
  case node of
    SString constraint formatValue -> SStringF constraint formatValue
    SNumber constraint -> SNumberF constraint
    SBool -> SBoolF
    SNull -> SNullF
    SUndefined -> SUndefinedF
    SVoid -> SVoidF
    SUnknown -> SUnknownF
    SLiteral literalValue -> SLiteralF literalValue
    SEnum values -> SEnumF values
    SArray element constraint -> SArrayF element constraint
    STuple elements rest -> STupleF elements rest
    SRecord value -> SRecordF value
    SObject fields -> SObjectF (Map.map objectPropertyToF fields)
    SUnion members -> SUnionF members
    SDiscriminatedUnion tagField members -> SDiscriminatedUnionF tagField members
    SOptional inner -> SOptionalF inner
    SNullable inner -> SNullableF inner
    SDefault inner defaultValue -> SDefaultF inner defaultValue
    SBrand inner brandName -> SBrandF inner brandName
    SRefine inner refinementId -> SRefineF inner refinementId
    SPreprocess inner preprocessId -> SPreprocessF inner preprocessId
    SConstrain inner constraintId -> SConstrainF inner constraintId
    STransform input output transformId -> STransformF input output transformId
    SRef refId -> SRefF refId
    SLazy refId -> SLazyF refId

embedSchema :: SchemaF SchemaNode -> SchemaNode
embedSchema layer =
  case layer of
    SStringF constraint formatValue -> SString constraint formatValue
    SNumberF constraint -> SNumber constraint
    SBoolF -> SBool
    SNullF -> SNull
    SUndefinedF -> SUndefined
    SVoidF -> SVoid
    SUnknownF -> SUnknown
    SLiteralF literalValue -> SLiteral literalValue
    SEnumF values -> SEnum values
    SArrayF element constraint -> SArray element constraint
    STupleF elements rest -> STuple elements rest
    SRecordF value -> SRecord value
    SObjectF fields -> SObject (Map.map objectPropertyFromF fields)
    SUnionF members -> SUnion members
    SDiscriminatedUnionF tagField members -> SDiscriminatedUnion tagField members
    SOptionalF inner -> SOptional inner
    SNullableF inner -> SNullable inner
    SDefaultF inner defaultValue -> SDefault inner defaultValue
    SBrandF inner brandName -> SBrand inner brandName
    SRefineF inner refinementId -> SRefine inner refinementId
    SPreprocessF inner preprocessId -> SPreprocess inner preprocessId
    SConstrainF inner constraintId -> SConstrain inner constraintId
    STransformF input output transformId -> STransform input output transformId
    SRefF refId -> SRef refId
    SLazyF refId -> SLazy refId

objectPropertyToF :: ObjectProperty -> ObjectPropertyF SchemaNode
objectPropertyToF propertyValue =
  ObjectPropertyF
    { opfRequired = opRequired propertyValue,
      opfReadonly = opReadonly propertyValue,
      opfSchema = opSchema propertyValue
    }

objectPropertyFromF :: ObjectPropertyF SchemaNode -> ObjectProperty
objectPropertyFromF propertyValue =
  ObjectProperty
    { opRequired = opfRequired propertyValue,
      opReadonly = opfReadonly propertyValue,
      opSchema = opfSchema propertyValue
    }

cataSchema :: (SchemaF a -> a) -> SchemaNode -> a
cataSchema = cata

paraSchema :: (SchemaF (SchemaNode, a) -> a) -> SchemaNode -> a
paraSchema = para

schemaNodeChildren :: SchemaNode -> [SchemaNode]
schemaNodeChildren = Foldable.toList . projectSchema

module Moonlight.Graph.Pure.Types
  ( NodeRef (..),
    EdgeRef (..),
    EntityRef (..),
    AttrKey (..),
    AttrDelta (..),
    AttrValue (..),
    DiscreteValue (..),
    Attributes (..),
    emptyAttributes,
    attributesFromList,
    insertAttribute,
    deleteAttribute,
    AttrPredicate (..),
    materialize,
  )
where

import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Data.Text (Text)
import Data.Word (Word64)
import Prelude (Bool (..), Double, Eq, Int, Ord, Read, Show, (*), (+))

type NodeRef :: Type
newtype NodeRef = NodeRef {unNodeRef :: Word64}
  deriving stock (Eq, Ord, Show, Read)

type EdgeRef :: Type
newtype EdgeRef = EdgeRef {unEdgeRef :: Word64}
  deriving stock (Eq, Ord, Show, Read)

type EntityRef :: Type
data EntityRef
  = NodeEntity NodeRef
  | EdgeEntity EdgeRef
  deriving stock (Eq, Ord, Show, Read)

type AttrKey :: Type
newtype AttrKey = AttrKey {unAttrKey :: Text}
  deriving stock (Eq, Ord, Show)

type DiscreteValue :: Type
data DiscreteValue
  = DiscreteInt Int
  | DiscreteText Text
  | DiscreteBool Bool
  deriving stock (Eq, Ord, Show)

type AttrValue :: Type
data AttrValue
  = ContinuousVal Double Double Double
  | DiscreteVal DiscreteValue
  | BudgetVal Double
  | TagVal (Set Text)
  deriving stock (Eq, Show)

type Attributes :: Type
newtype Attributes = Attributes {unAttributes :: Map AttrKey AttrValue}
  deriving stock (Eq, Show)

emptyAttributes :: Attributes
emptyAttributes = Attributes Map.empty

attributesFromList :: [(AttrKey, AttrValue)] -> Attributes
attributesFromList entries = Attributes (Map.fromList entries)

insertAttribute :: AttrKey -> AttrValue -> Attributes -> Attributes
insertAttribute key value attrs = Attributes (Map.insert key value (unAttributes attrs))

deleteAttribute :: AttrKey -> Attributes -> Attributes
deleteAttribute key attrs = Attributes (Map.delete key (unAttributes attrs))

type AttrDelta :: Type
data AttrDelta
  = ContinuousDelta Double Double
  | DiscreteDelta DiscreteValue
  | TagDelta (Set Text) (Set Text)
  deriving stock (Eq, Show)

type AttrPredicate :: Type
data AttrPredicate
  = AttrIs AttrValue
  | AttrAtLeast Double
  | AttrHasTag Text
  deriving stock (Eq, Show)

materialize :: AttrValue -> Double
materialize attrValue =
  case attrValue of
    ContinuousVal baseValue pendingAdd pendingMul -> (baseValue + pendingAdd) * pendingMul
    BudgetVal value -> value
    DiscreteVal _ -> 0.0
    TagVal _ -> 0.0

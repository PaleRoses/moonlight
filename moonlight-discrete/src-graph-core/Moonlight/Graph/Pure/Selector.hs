module Moonlight.Graph.Pure.Selector
  ( GraphSelector (..),
    SelectorAmbiguity (..),
    resolveGraphSelector,
    lintGraphSelector,
  )
where

import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Moonlight.Graph.Pure.Types
import Moonlight.Graph.Pure.View (GraphView (..))
import Prelude (Bool (..), Eq, Maybe (..), Show, maybe, (==), (>=))

type GraphSelector :: Type -> Type -> Type
data GraphSelector nodeKind edgeKind
  = ByNodeKind nodeKind
  | ByEdgeKind edgeKind
  | ByNodeRef NodeRef
  | ByEdgeRef EdgeRef
  | ByAttribute AttrKey AttrPredicate
  | Intersection (GraphSelector nodeKind edgeKind) (GraphSelector nodeKind edgeKind)
  | Union (GraphSelector nodeKind edgeKind) (GraphSelector nodeKind edgeKind)

deriving stock instance (Eq nodeKind, Eq edgeKind) => Eq (GraphSelector nodeKind edgeKind)
deriving stock instance (Show nodeKind, Show edgeKind) => Show (GraphSelector nodeKind edgeKind)

type SelectorAmbiguity :: Type
data SelectorAmbiguity
  = SelectorUnresolved
  | SelectorNonSingular [EntityRef]
  deriving stock (Eq, Show)

resolveGraphSelector ::
  (Eq nodeKind, Eq edgeKind) =>
  GraphView graph nodeKind edgeKind ->
  GraphSelector nodeKind edgeKind ->
  graph ->
  Set EntityRef
resolveGraphSelector graphView selectorValue graphValue =
  case selectorValue of
    ByNodeKind nodeKindValue ->
      Map.foldrWithKey
        (\nodeRef (foundKind, _) refs ->
            if foundKind == nodeKindValue
              then Set.insert (NodeEntity nodeRef) refs
              else refs
        )
        Set.empty
        (viewNodes graphView graphValue)
    ByEdgeKind edgeKindValue ->
      Map.foldrWithKey
        (\edgeRef (foundKind, _, _) refs ->
            if foundKind == edgeKindValue
              then Set.insert (EdgeEntity edgeRef) refs
              else refs
        )
        Set.empty
        (viewEdges graphView graphValue)
    ByNodeRef nodeRefValue ->
      if Map.member nodeRefValue (viewNodes graphView graphValue)
        then Set.singleton (NodeEntity nodeRefValue)
        else Set.empty
    ByEdgeRef edgeRefValue ->
      if Map.member edgeRefValue (viewEdges graphView graphValue)
        then Set.singleton (EdgeEntity edgeRefValue)
        else Set.empty
    ByAttribute attrKey predicateValue ->
      Set.union
        (matchNodeAttributes attrKey predicateValue (viewNodes graphView graphValue))
        (matchEdgeAttributes attrKey predicateValue (viewEdges graphView graphValue))
    Intersection leftSelector rightSelector ->
      Set.intersection
        (resolveGraphSelector graphView leftSelector graphValue)
        (resolveGraphSelector graphView rightSelector graphValue)
    Union leftSelector rightSelector ->
      Set.union
        (resolveGraphSelector graphView leftSelector graphValue)
        (resolveGraphSelector graphView rightSelector graphValue)

lintGraphSelector ::
  (Eq nodeKind, Eq edgeKind) =>
  GraphView graph nodeKind edgeKind ->
  GraphSelector nodeKind edgeKind ->
  graph ->
  Maybe SelectorAmbiguity
lintGraphSelector graphView selectorValue graphValue =
  let matches = Set.toList (resolveGraphSelector graphView selectorValue graphValue)
   in case selectorValue of
        ByNodeRef _ ->
          case matches of
            [] -> Just SelectorUnresolved
            [_] -> Nothing
            _ -> Just (SelectorNonSingular matches)
        ByEdgeRef _ ->
          case matches of
            [] -> Just SelectorUnresolved
            [_] -> Nothing
            _ -> Just (SelectorNonSingular matches)
        _ -> Nothing

matchNodeAttributes :: AttrKey -> AttrPredicate -> Map NodeRef (nodeKind, Attributes) -> Set EntityRef
matchNodeAttributes attrKey predicateValue =
  Map.foldrWithKey
    (\nodeRef (_, attributesValue) refs ->
        if matchesAttributes attrKey predicateValue attributesValue
          then Set.insert (NodeEntity nodeRef) refs
          else refs
    )
    Set.empty

matchEdgeAttributes :: AttrKey -> AttrPredicate -> Map EdgeRef (edgeKind, [NodeRef], Attributes) -> Set EntityRef
matchEdgeAttributes attrKey predicateValue =
  Map.foldrWithKey
    (\edgeRef (_, _, attributesValue) refs ->
        if matchesAttributes attrKey predicateValue attributesValue
          then Set.insert (EdgeEntity edgeRef) refs
          else refs
    )
    Set.empty

matchesAttributes :: AttrKey -> AttrPredicate -> Attributes -> Bool
matchesAttributes attrKey predicateValue (Attributes attributeMap) =
  maybe False (matchesPredicate predicateValue) (Map.lookup attrKey attributeMap)

matchesPredicate :: AttrPredicate -> AttrValue -> Bool
matchesPredicate predicateValue attrValue =
  case (predicateValue, attrValue) of
    (AttrIs expectedValue, actualValue) -> expectedValue == actualValue
    (AttrAtLeast threshold, ContinuousVal _ _ _) -> materialize attrValue >= threshold
    (AttrAtLeast threshold, BudgetVal value) -> value >= threshold
    (AttrAtLeast _, _) -> False
    (AttrHasTag tagValue, TagVal tags) -> Set.member tagValue tags
    (AttrHasTag _, _) -> False

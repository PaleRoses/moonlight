module Moonlight.Constraint.Pure.ArcConsistency
  ( CompatibilityTable,
    Domains,
    compatibilityTableFromList,
    buildCompatBy,
    buildCompatWith,
    ac3,
    searchSatisfying,
  )
where

import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

type CompatibilityTable :: Type -> Type -> Type
newtype CompatibilityTable coord value = CompatibilityTable
  { compatibilityEntries :: Map (coord, coord) (Map value (Set value))
  }
  deriving stock (Eq, Show)

type Domains :: Type -> Type -> Type
type Domains coord value = Map coord (Set value)

compatibilityTableFromList ::
  (Ord coord, Ord value) =>
  [((coord, coord), Map value (Set value))] ->
  CompatibilityTable coord value
compatibilityTableFromList =
  CompatibilityTable . Map.fromList . fmap canonicalizeCompatibilityEntry

canonicalizeCompatibilityEntry ::
  (Ord coord, Ord value) =>
  ((coord, coord), Map value (Set value)) ->
  ((coord, coord), Map value (Set value))
canonicalizeCompatibilityEntry ((leftCoordinate, rightCoordinate), compatibility)
  | leftCoordinate <= rightCoordinate =
      ((leftCoordinate, rightCoordinate), compatibility)
  | otherwise =
      ((rightCoordinate, leftCoordinate), transposeCompatibility compatibility)

transposeCompatibility :: Ord value => Map value (Set value) -> Map value (Set value)
transposeCompatibility compatibility =
  Map.fromListWith
    Set.union
    [ (rightValue, Set.singleton leftValue)
    | (leftValue, rightValues) <- Map.toAscList compatibility,
      rightValue <- Set.toAscList rightValues
    ]

canonicalPair :: Ord coord => coord -> coord -> (coord, coord)
canonicalPair leftCoordinate rightCoordinate =
  if leftCoordinate <= rightCoordinate
    then (leftCoordinate, rightCoordinate)
    else (rightCoordinate, leftCoordinate)

compatibilityAllows ::
  (Ord coord, Ord value) =>
  CompatibilityTable coord value ->
  coord ->
  value ->
  coord ->
  value ->
  Bool
compatibilityAllows table leftCoordinate leftValue rightCoordinate rightValue =
  case Map.lookup (canonicalPair leftCoordinate rightCoordinate) (compatibilityEntries table) of
    Nothing -> True
    Just compatibility
      | leftCoordinate <= rightCoordinate ->
          Set.member rightValue (Map.findWithDefault Set.empty leftValue compatibility)
      | otherwise ->
          Set.member leftValue (Map.findWithDefault Set.empty rightValue compatibility)

buildCompatBy :: (Ord value, Ord key) => (value -> key) -> Set value -> Set value -> Map value (Set value)
buildCompatBy project =
  buildCompatWith (\leftValue rightValue -> project leftValue == project rightValue)

buildCompatWith :: Ord value => (value -> value -> Bool) -> Set value -> Set value -> Map value (Set value)
buildCompatWith compatible leftDomain rightDomain =
  Set.foldl'
    ( \compatibility leftValue ->
        Map.insert
          leftValue
          (Set.filter (compatible leftValue) rightDomain)
          compatibility
    )
    Map.empty
    leftDomain

ac3 :: (Ord coord, Ord value) => CompatibilityTable coord value -> Domains coord value -> Domains coord value
ac3 compatibilityTable domains =
  go initialWorklist domains
  where
    initialWorklist =
      Map.keys (compatibilityEntries compatibilityTable)

    requeue changedCoordinate =
      filter
        (\(leftCoordinate, rightCoordinate) -> leftCoordinate == changedCoordinate || rightCoordinate == changedCoordinate)
        initialWorklist

    reviseLeft :: Ord value => Map value (Set value) -> Set value -> Set value -> Set value
    reviseLeft compatibility rightDomain leftDomain =
      Set.filter
        ( \leftValue ->
            maybe
              False
              (not . Set.null . Set.intersection rightDomain)
              (Map.lookup leftValue compatibility)
        )
        leftDomain

    reviseRight :: Ord value => Map value (Set value) -> Set value -> Set value -> Set value
    reviseRight compatibility leftDomain rightDomain =
      Set.filter
        ( \rightValue ->
            any
              ( \leftValue ->
                  maybe
                    False
                    (Set.member rightValue)
                    (Map.lookup leftValue compatibility)
              )
              leftDomain
        )
        rightDomain

    go [] currentDomains =
      currentDomains
    go ((leftCoordinate, rightCoordinate) : worklist) currentDomains =
      case Map.lookup (leftCoordinate, rightCoordinate) (compatibilityEntries compatibilityTable) of
        Nothing ->
          go worklist currentDomains
        Just compatibility ->
          let leftDomain = Map.findWithDefault Set.empty leftCoordinate currentDomains
              rightDomain = Map.findWithDefault Set.empty rightCoordinate currentDomains
              leftDomain' = reviseLeft compatibility rightDomain leftDomain
              rightDomain' = reviseRight compatibility leftDomain' rightDomain
              leftChanged = Set.size leftDomain' < Set.size leftDomain
              rightChanged = Set.size rightDomain' < Set.size rightDomain
              nextDomains =
                Map.insert leftCoordinate leftDomain' (Map.insert rightCoordinate rightDomain' currentDomains)
              nextWorklist =
                (if leftChanged then requeue leftCoordinate else [])
                  <> (if rightChanged then requeue rightCoordinate else [])
                  <> worklist
           in go nextWorklist nextDomains

searchSatisfying :: (Ord coord, Ord value) => [coord] -> Domains coord value -> CompatibilityTable coord value -> ([value] -> Bool) -> [[value]]
searchSatisfying coordinateOrder domains compatibilityTable predicate =
  go coordinateOrder []
  where
    go remainingCoordinates partialAssignments =
      case remainingCoordinates of
        [] ->
          let tuple = fmap snd (reverse partialAssignments)
           in [tuple | predicate tuple]
        coordinateValue : tailCoordinates ->
          foldMap
            ( \entryValue ->
                if allCompatibleWith coordinateValue entryValue partialAssignments
                  then go tailCoordinates ((coordinateValue, entryValue) : partialAssignments)
                  else []
            )
            (Set.toList (Map.findWithDefault Set.empty coordinateValue domains))

    allCompatibleWith coordinateValue entryValue assignedAssignments =
      all
        ( \(otherCoordinate, otherValue) ->
            compatibilityAllows
              compatibilityTable
              otherCoordinate
              otherValue
              coordinateValue
              entryValue
        )
        assignedAssignments

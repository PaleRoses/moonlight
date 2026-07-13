module Moonlight.Constraint.Pure.ArcConsistency
  ( CompatibilityTable,
    Domains,
    buildCompatBy,
    buildCompatWith,
    ac3,
    searchSatisfying,
  )
where

import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set

type CompatibilityTable :: Type -> Type -> Type
type CompatibilityTable coord value = Map (coord, coord) (Map value (Set value))
type Domains :: Type -> Type -> Type
type Domains coord value = Map coord (Set value)

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

ac3 :: (Ord coord, Ord value) => [coord] -> CompatibilityTable coord value -> Domains coord value -> Domains coord value
ac3 coordinateOrder compatibilityTable domains =
  go initialWorklist domains
  where
    initialWorklist =
      Map.keys compatibilityTable

    normalizePair :: Ord coord => coord -> coord -> (coord, coord)
    normalizePair leftCoordinate rightCoordinate =
      if leftCoordinate < rightCoordinate
        then (leftCoordinate, rightCoordinate)
        else (rightCoordinate, leftCoordinate)

    requeue changedCoordinate =
      mapMaybe
        ( \otherCoordinate ->
            let compatibilityKey = normalizePair changedCoordinate otherCoordinate
             in if changedCoordinate /= otherCoordinate && Map.member compatibilityKey compatibilityTable
                  then Just compatibilityKey
                  else Nothing
        )
        coordinateOrder

    rightSupport :: Ord value => Map value (Set value) -> Set value -> value -> Set value
    rightSupport compatibility leftDomain rightValue =
      Set.foldl'
        ( \compatibleLeftValues leftValue ->
            case Map.lookup leftValue compatibility of
              Nothing -> compatibleLeftValues
              Just allowedRightValues ->
                if Set.member rightValue allowedRightValues
                  then Set.insert leftValue compatibleLeftValues
                  else compatibleLeftValues
        )
        Set.empty
        leftDomain

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
            not
              ( Set.null
                  ( Set.intersection
                      leftDomain
                      (rightSupport compatibility leftDomain rightValue)
                  )
              )
        )
        rightDomain

    go [] currentDomains =
      currentDomains
    go ((leftCoordinate, rightCoordinate) : worklist) currentDomains =
      case Map.lookup (leftCoordinate, rightCoordinate) compatibilityTable of
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
    normalizePair :: Ord coord => coord -> coord -> (coord, coord)
    normalizePair leftCoordinate rightCoordinate =
      if leftCoordinate < rightCoordinate
        then (leftCoordinate, rightCoordinate)
        else (rightCoordinate, leftCoordinate)

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
            let compatibilityKey = normalizePair otherCoordinate coordinateValue
             in case Map.lookup compatibilityKey compatibilityTable of
                  Nothing -> True
                  Just compatibility ->
                    let allowedValues =
                          if otherCoordinate < coordinateValue
                            then Map.findWithDefault Set.empty otherValue compatibility
                            else Map.findWithDefault Set.empty entryValue compatibility
                        comparedValue =
                          if otherCoordinate < coordinateValue
                            then entryValue
                            else otherValue
                     in Set.member comparedValue allowedValues
        )
        assignedAssignments

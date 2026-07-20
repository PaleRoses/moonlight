{-# LANGUAGE StandaloneKindSignatures #-}

-- | The budgeted cover-descent kernel: search costs, refusals, and context
-- enumeration.
module Moonlight.Sheaf.Descent.Kernel
  ( CoverDescentKernel (..),
    CoverSearchBudget (..),
    CoverSearchCost (..),
    CoverSearchRefusal (..),
    coverContextAt,
    coverCoordinateRange,
    coverPreparedMeetCompatible,
    coverSearchCost,
    coverSearchWithinBudget,
    descentAtCover,
    fullCoverDescentCheck,
    intAssignmentsToIntMaps,
    nontrivialCover,
    obstructionWhenAssignmentsPresent,
    unboundedCoverSearchBudget,
  )
where

import Control.Monad (foldM)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Kind (Type)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Moonlight.Sheaf.Descent.Core
  ( DescentReport,
    collectDescentReport,
  )
import Moonlight.Sheaf.Verdict
  ( ObstructionVerdict,
    SearchVerdict (..),
    decidedSearchVerdict,
    rejectedFromList,
    searchUndecidedOne,
  )
import Numeric.Natural (Natural)

type CoverDescentKernel :: Type -> Type -> Type -> Type -> Type
data CoverDescentKernel ctx coordinate value obstruction = CoverDescentKernel
  { cdkMaterializedContexts :: ![ctx],
    cdkCoverOf :: ctx -> [ctx],
    cdkCoordinates :: ctx -> [ctx] -> [coordinate],
    cdkDomainAt :: ctx -> [ctx] -> coordinate -> [value],
    cdkCompatible ::
      ctx ->
      [ctx] ->
      coordinate ->
      value ->
      coordinate ->
      value ->
      Bool,
    cdkTupleObstructed :: ctx -> [ctx] -> Map coordinate value -> Bool,
    cdkObstructions :: ctx -> [ctx] -> [Map coordinate value] -> [obstruction],
    cdkVacuousObstruction :: ctx -> [ctx] -> NonEmpty coordinate -> obstruction
  }

type CoverSearchBudget :: Type
newtype CoverSearchBudget = CoverSearchBudget
  { csbMaxAssignments :: Maybe Natural
  }
  deriving stock (Eq, Show)

unboundedCoverSearchBudget :: CoverSearchBudget
unboundedCoverSearchBudget =
  CoverSearchBudget Nothing

type CoverSearchCost :: Type -> Type
data CoverSearchCost coordinate = CoverSearchCost
  { cscCoordinates :: ![coordinate],
    cscDomainSizes :: !(Map coordinate Natural),
    cscAssignmentUpperBound :: !Natural
  }
  deriving stock (Eq, Show)

type CoverSearchRefusal :: Type -> Type
data CoverSearchRefusal coordinate
  = CoverSearchBudgetExceeded !CoverSearchBudget !(CoverSearchCost coordinate)
  deriving stock (Eq, Show)

coverSearchCost :: Ord coordinate => CoverDescentKernel ctx coordinate value obstruction -> ctx -> [ctx] -> CoverSearchCost coordinate
coverSearchCost kernel parentContext coverContexts =
  coverSearchCostFromSpace (coverSearchSpace kernel parentContext coverContexts)

coverSearchCostFromSpace :: Ord coordinate => ([coordinate], Map coordinate [value]) -> CoverSearchCost coordinate
coverSearchCostFromSpace (orderedCoordinates, domainsByCoordinate) =
  let domainSizes =
        Map.map (fromIntegral . length) domainsByCoordinate
   in CoverSearchCost
        { cscCoordinates = orderedCoordinates,
          cscDomainSizes = domainSizes,
          cscAssignmentUpperBound =
            product (fmap (flip (Map.findWithDefault 0) domainSizes) orderedCoordinates)
        }

coverSearchWithinBudget :: CoverSearchBudget -> CoverSearchCost coordinate -> Bool
coverSearchWithinBudget (CoverSearchBudget maybeMaxAssignments) searchCost =
  maybe True (cscAssignmentUpperBound searchCost <=) maybeMaxAssignments

coverSearchSpace :: Ord coordinate => CoverDescentKernel ctx coordinate value obstruction -> ctx -> [ctx] -> ([coordinate], Map coordinate [value])
coverSearchSpace kernel parentContext coverContexts =
  let coordinates =
        cdkCoordinates kernel parentContext coverContexts

      domainsByCoordinate =
        Map.fromList
          [ (coordinate, cdkDomainAt kernel parentContext coverContexts coordinate)
          | coordinate <- coordinates
          ]
   in (orderedCoordinatesByDomainSize domainsByCoordinate coordinates, domainsByCoordinate)

coverCoordinateRange :: [ctx] -> [Int]
coverCoordinateRange coverContexts = [0 .. length coverContexts - 1]

coverContextAt :: Int -> [ctx] -> Maybe ctx
coverContextAt indexValue contexts =
  if indexValue < 0 then Nothing else listToMaybe (drop indexValue contexts)

nontrivialCover :: [ctx] -> Maybe [ctx]
nontrivialCover [] = Nothing
nontrivialCover [_] = Nothing
nontrivialCover coverContexts = Just coverContexts

coverPreparedMeetCompatible ::
  Eq restricted =>
  (coordinate -> coordinate -> Maybe classes) ->
  (classes -> value -> restricted) ->
  coordinate ->
  value ->
  coordinate ->
  value ->
  Bool
coverPreparedMeetCompatible meetClassesAt restrictWith leftCoordinate leftValue rightCoordinate rightValue =
  case meetClassesAt leftCoordinate rightCoordinate of
    Just meetClasses ->
      restrictWith meetClasses leftValue
        == restrictWith meetClasses rightValue
    Nothing ->
      False

intAssignmentsToIntMaps :: [Map Int value] -> [IntMap value]
intAssignmentsToIntMaps = fmap (IntMap.fromList . Map.toAscList)

obstructionWhenAssignmentsPresent :: ([Map coordinate value] -> obstruction) -> [Map coordinate value] -> [obstruction]
obstructionWhenAssignmentsPresent buildObstruction obstructedAssignments =
  [ buildObstruction obstructedAssignments
  | not (null obstructedAssignments)
  ]

descentAtCover ::
  Ord coordinate =>
  CoverSearchBudget ->
  CoverDescentKernel ctx coordinate value obstruction ->
  ctx ->
  SearchVerdict (CoverSearchRefusal coordinate) obstruction
descentAtCover budget kernel parentContext =
  maybe
    SearchAccepted
    (descentAtMaterializedCover budget kernel parentContext)
    (nontrivialCover (cdkCoverOf kernel parentContext))

descentAtMaterializedCover ::
  Ord coordinate =>
  CoverSearchBudget ->
  CoverDescentKernel ctx coordinate value obstruction ->
  ctx ->
  [ctx] ->
  SearchVerdict (CoverSearchRefusal coordinate) obstruction
descentAtMaterializedCover budget kernel parentContext coverContexts =
  let searchSpace =
        coverSearchSpace kernel parentContext coverContexts
      searchCost =
        coverSearchCostFromSpace searchSpace
   in case vacuousCoordinates searchSpace of
        Just vacuousCoordinateSet ->
          SearchRejected
            (cdkVacuousObstruction kernel parentContext coverContexts vacuousCoordinateSet :| [])
        Nothing ->
          if coverSearchWithinBudget budget searchCost
            then
              decidedSearchVerdict
                (descentAtMaterializedCoverDecided kernel parentContext coverContexts searchSpace)
            else
              searchUndecidedOne (CoverSearchBudgetExceeded budget searchCost) []

descentAtMaterializedCoverDecided ::
  Ord coordinate =>
  CoverDescentKernel ctx coordinate value obstruction ->
  ctx ->
  [ctx] ->
  ([coordinate], Map coordinate [value]) ->
  ObstructionVerdict obstruction
descentAtMaterializedCoverDecided kernel parentContext coverContexts searchSpace =
  rejectedFromList
    ( cdkObstructions
        kernel
        parentContext
        coverContexts
        (obstructedCoverAssignmentsInSpace kernel parentContext coverContexts searchSpace)
    )

vacuousCoordinates :: Ord coordinate => ([coordinate], Map coordinate [value]) -> Maybe (NonEmpty coordinate)
vacuousCoordinates (orderedCoordinates, domainsByCoordinate) =
  NonEmpty.nonEmpty
    [ coordinate
    | coordinate <- orderedCoordinates,
      null (Map.findWithDefault [] coordinate domainsByCoordinate)
    ]

fullCoverDescentCheck ::
  Ord coordinate =>
  CoverSearchBudget ->
  CoverDescentKernel ctx coordinate value obstruction ->
  DescentReport ctx (CoverSearchRefusal coordinate) obstruction
fullCoverDescentCheck budget kernel =
  collectDescentReport
    (cdkMaterializedContexts kernel)
    (\contextValue -> length (cdkCoverOf kernel contextValue) >= 2)
    (descentAtCover budget kernel)

obstructedCoverAssignmentsInSpace :: Ord coordinate => CoverDescentKernel ctx coordinate value obstruction -> ctx -> [ctx] -> ([coordinate], Map coordinate [value]) -> [Map coordinate value]
obstructedCoverAssignmentsInSpace kernel parentContext coverContexts (orderedCoordinates, domainsByCoordinate) =
  filter
    (cdkTupleObstructed kernel parentContext coverContexts)
    (foldM extendAssignment Map.empty orderedCoordinates)
  where
    extendAssignment assigned coordinate =
      [ Map.insert coordinate value assigned
      | value <- Map.findWithDefault [] coordinate domainsByCoordinate,
        compatibleWithAssigned coordinate value assigned
      ]

    compatibleWithAssigned coordinate value assigned =
      all
        ( uncurry (compatibleOrdered coordinate value)
        )
        (Map.toAscList assigned)

    compatibleOrdered leftCoordinate leftValue rightCoordinate rightValue =
      case compare leftCoordinate rightCoordinate of
        LT ->
          cdkCompatible kernel parentContext coverContexts leftCoordinate leftValue rightCoordinate rightValue
        EQ ->
          True
        GT ->
          cdkCompatible kernel parentContext coverContexts rightCoordinate rightValue leftCoordinate leftValue

orderedCoordinatesByDomainSize :: Ord coordinate => Map coordinate [value] -> [coordinate] -> [coordinate]
orderedCoordinatesByDomainSize domainsByCoordinate =
  List.sortOn
    ( \coordinate ->
        ( length (Map.findWithDefault [] coordinate domainsByCoordinate),
          coordinate
        )
    )

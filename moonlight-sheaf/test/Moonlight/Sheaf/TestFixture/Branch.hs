module Moonlight.Sheaf.TestFixture.Branch
  ( BranchContext (..),
    BranchStalk (..),
    BranchMismatch (..),
    branchContexts,
    branchContextLattice,
    branchStalk,
    branchStalkAlgebra,
    branchStalkEntries,
    branchLeftCompatibleStalk,
    branchRightCompatibleStalk,
    branchRightIncompatibleStalk,
    branchCompatibleAmalgamatedStalk,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Moonlight.Sheaf.Section.Stalk
  ( StalkAlgebra (..),
    StalkRestrictionKernel (..),
    mismatchObstruction,
  )
import Moonlight.FiniteLattice
  ( ContextLattice,
    compileContextLattice,
    contextOrderDecl
  )

data BranchContext
  = BranchBase
  | BranchLeft
  | BranchRight
  | BranchApex
  deriving stock (Eq, Ord, Show, Enum, Bounded)

newtype BranchStalk = BranchStalk (Map BranchContext Int)
  deriving stock (Eq, Show)

data BranchMismatch
  = BranchMissingCoordinate !BranchContext !(Maybe Int) !(Maybe Int)
  | BranchCoordinateConflict !BranchContext !Int !Int
  deriving stock (Eq, Show)

branchContexts :: [BranchContext]
branchContexts =
  [minBound .. maxBound]

branchContextLattice :: ContextLattice BranchContext
branchContextLattice =
  either
    (error . ("invalid branch fixture lattice: " <>) . show)
    id
    ( compileContextLattice
        (Set.fromList branchContexts)
        ( contextOrderDecl
            BranchApex
            BranchBase
            [ (BranchBase, BranchLeft),
              (BranchBase, BranchRight),
              (BranchLeft, BranchApex),
              (BranchRight, BranchApex)
            ]
        )
    )

branchStalk :: [(BranchContext, Int)] -> BranchStalk
branchStalk =
  BranchStalk . Map.fromList

branchStalkEntries :: BranchStalk -> Map BranchContext Int
branchStalkEntries (BranchStalk entries) =
  entries

branchLeftCompatibleStalk :: BranchStalk
branchLeftCompatibleStalk =
  branchStalk [(BranchLeft, 10), (BranchApex, 7)]

branchRightCompatibleStalk :: BranchStalk
branchRightCompatibleStalk =
  branchStalk [(BranchRight, 20), (BranchApex, 7)]

branchRightIncompatibleStalk :: BranchStalk
branchRightIncompatibleStalk =
  branchStalk [(BranchRight, 20), (BranchApex, 8)]

branchCompatibleAmalgamatedStalk :: BranchStalk
branchCompatibleAmalgamatedStalk =
  branchStalk [(BranchLeft, 10), (BranchRight, 20), (BranchApex, 7)]

branchStalkAlgebra :: StalkAlgebra () BranchStalk BranchMismatch ()
branchStalkAlgebra =
  StalkAlgebra
    { saRestrictionKernel = const (StalkRestrictionMap id),
      saMismatches =
        \(BranchStalk leftEntries) (BranchStalk rightEntries) ->
          mapMaybe
            (branchMismatchAt leftEntries rightEntries)
            (Map.keys (Map.union leftEntries rightEntries)),
      saMerge =
        \leftStalk@(BranchStalk leftEntries) rightStalk@(BranchStalk rightEntries) ->
          case mismatchObstruction (branchMergeMismatches leftStalk rightStalk) of
            Just obstruction -> Left obstruction
            Nothing -> Right (BranchStalk (Map.union leftEntries rightEntries)),
      saRepair = const (Left ()),
      saNormalize = id
    }

branchMergeMismatches :: BranchStalk -> BranchStalk -> [BranchMismatch]
branchMergeMismatches (BranchStalk leftEntries) (BranchStalk rightEntries) =
  mapMaybe
    (branchMergeConflictAt leftEntries rightEntries)
    (Map.keys (Map.intersection leftEntries rightEntries))

branchMismatchAt ::
  Map BranchContext Int ->
  Map BranchContext Int ->
  BranchContext ->
  Maybe BranchMismatch
branchMismatchAt leftEntries rightEntries contextValue =
  case (Map.lookup contextValue leftEntries, Map.lookup contextValue rightEntries) of
    (Just leftValue, Just rightValue)
      | leftValue == rightValue ->
          Nothing
      | otherwise ->
          Just (BranchCoordinateConflict contextValue leftValue rightValue)
    (Nothing, Nothing) ->
      Nothing
    (leftValue, rightValue) ->
      Just (BranchMissingCoordinate contextValue leftValue rightValue)

branchMergeConflictAt ::
  Map BranchContext Int ->
  Map BranchContext Int ->
  BranchContext ->
  Maybe BranchMismatch
branchMergeConflictAt leftEntries rightEntries contextValue =
  case (Map.lookup contextValue leftEntries, Map.lookup contextValue rightEntries) of
    (Just leftValue, Just rightValue)
      | leftValue /= rightValue ->
          Just (BranchCoordinateConflict contextValue leftValue rightValue)
    _ ->
      Nothing

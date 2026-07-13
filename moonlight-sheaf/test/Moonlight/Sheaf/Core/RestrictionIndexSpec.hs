module Moonlight.Sheaf.Core.RestrictionIndexSpec
  ( tests,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Moonlight.Sheaf.Kernel.Basis
  ( SheafBasis,
    mkSheafBasis,
  )
import Moonlight.Sheaf.Section.Morphism
  ( Restriction,
    RestrictionId (..),
    RestrictionKind (..),
    RestrictionParts (..),
    RestrictionPresentation,
    mkIncidenceCoefficient,
    negativeUnitIncidenceRestriction,
    rKind,
    restrictApply,
    unitIncidenceRestriction,
  )
import Moonlight.Sheaf.Kernel.Basis (basisCells)
import Moonlight.Sheaf.Section.ObjectIndex
  ( ObjectKey (..),
    mkObjectIndex,
  )
import Moonlight.Sheaf.Section.Plan
  ( ExtentFrontierPlan (..),
    SheafPlans (..),
    extentFrontierPlanAt,
    sheafPlansFromRestrictionIndex,
  )
import Moonlight.Sheaf.Section.Restriction
  ( RestrictionIndex,
    RestrictionIndexError (..),
    buildRestrictionIndex,
    incidenceRestrictions,
    portalRestrictions,
    restrictionEndpointKeyMap,
    restrictionEndpointKeys,
    restrictionEntries,
    restrictionIds,
    restrictionIdsByArrowKey,
    restrictionIncomingByObject,
    restrictionMultiplicityByArrow,
    restrictionOutgoingByObject,
    restrictionsFrom,
    restrictionsTo,
    updateRestrictionIndexEntriesWithDenseKeys,
  )
import Moonlight.Sheaf.Section.Stalk (StalkAlgebra (..), StalkRestrictionKernel (..))
import Moonlight.Sheaf.TestFixture.Mini
  ( MiniCell (..),
    MiniStalk (..),
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )

data RestrictionSpec = RestrictionSpec
  { rsKind :: !RestrictionKind,
    rsSource :: !MiniCell,
    rsTarget :: !MiniCell,
    rsScale :: !Double
  }
  deriving stock (Eq, Show)

tests :: TestTree
tests =
  testGroup
    "restriction-index"
    [ testCase "rejects unknown source" testRejectsUnknownSource,
      testCase "rejects unknown target" testRejectsUnknownTarget,
      testCase "rejects zero incidence coefficient" testRejectsZeroIncidenceCoefficient,
      testCase "indexes entries by source target arrow and kind" testIndexInverses,
      testCase "keeps dense ids and secondary indexes exact" testDenseIdsAndSecondaryIndexes,
      testCase "updates entries through the same invariant-preserving index law" testUpdatePreservesSecondaryIndexes,
      testCase "applies stored restriction function" testAppliesStoredFunction,
      testCase "preserves parallel restrictions and endpoint keys" testParallelRestrictionsPreserved,
      testCase "prepares extent frontier plans from restriction keys" testSheafPlansExtentFrontier
    ]

basis :: SheafBasis MiniCell
basis =
  mkSheafBasis [Ghost, Cell0, Cell1]

specs :: [RestrictionSpec]
specs =
  [ RestrictionSpec unitIncidenceRestriction Ghost Cell0 3.0,
    RestrictionSpec negativeUnitIncidenceRestriction Cell0 Cell1 2.0,
    RestrictionSpec PortalRestriction Ghost Cell1 5.0
  ]

restrictionSpecAlgebra :: StalkAlgebra RestrictionSpec MiniStalk () ()
restrictionSpecAlgebra =
  StalkAlgebra
    { saRestrictionKernel = \specValue -> StalkRestrictionMap (\(MiniStalk value) -> MiniStalk (rsScale specValue * value)),
      saMismatches = \_ _ -> [],
      saMerge = \left _ -> Right left,
      saRepair = const (Left ()),
      saNormalize = id
    }

presentRestrictionSpec :: RestrictionPresentation RestrictionSpec MiniCell RestrictionSpec
presentRestrictionSpec specValue =
  RestrictionParts
    { partKind = rsKind specValue,
      partSource = rsSource specValue,
      partTarget = rsTarget specValue,
      partWitness = specValue
    }

buildIndex :: Either (RestrictionIndexError MiniCell) (RestrictionIndex MiniCell RestrictionSpec)
buildIndex =
  buildRestrictionIndex
    (mkObjectIndex (basisCells basis))
    presentRestrictionSpec
    specs

testRejectsUnknownSource :: Assertion
testRejectsUnknownSource =
  case
    buildRestrictionIndex
      (mkObjectIndex (basisCells (mkSheafBasis [Cell0])))
      presentRestrictionSpec
      [RestrictionSpec unitIncidenceRestriction Ghost Cell0 1.0]
  of
    Left (RestrictionUnknownSource Ghost) ->
      pure ()
    Left failure ->
      assertFailure ("expected unknown source, received " <> show failure)
    Right _ ->
      assertFailure "expected unknown source rejection"

testRejectsUnknownTarget :: Assertion
testRejectsUnknownTarget =
  case
    buildRestrictionIndex
      (mkObjectIndex (basisCells (mkSheafBasis [Ghost])))
      presentRestrictionSpec
      [RestrictionSpec unitIncidenceRestriction Ghost Cell0 1.0]
  of
    Left (RestrictionUnknownTarget Cell0) ->
      pure ()
    Left failure ->
      assertFailure ("expected unknown target, received " <> show failure)
    Right _ ->
      assertFailure "expected unknown target rejection"

testRejectsZeroIncidenceCoefficient :: Assertion
testRejectsZeroIncidenceCoefficient =
  case mkIncidenceCoefficient 0 of
    Nothing ->
      pure ()
    Just _ ->
      assertFailure "expected zero incidence coefficient to be unconstructable"

testIndexInverses :: Assertion
testIndexInverses =
  case buildIndex of
    Left failure ->
      assertFailure ("expected index construction to succeed, received " <> show failure)
    Right indexValue -> do
      assertEqual "entry count" 3 (length (restrictionEntries indexValue))
      assertEqual "from Ghost" 2 (length (restrictionsFrom (mkObjectIndex (basisCells basis)) Ghost indexValue))
      assertEqual "to Cell1" 2 (length (restrictionsTo (mkObjectIndex (basisCells basis)) Cell1 indexValue))
      assertEqual "incidence count" 2 (length (incidenceRestrictions indexValue))
      assertEqual "portal count" 1 (length (portalRestrictions indexValue))
      assertBool
        "multiplicity map is non-empty"
        (not (Map.null (restrictionMultiplicityByArrow indexValue)))
      assertBool
        "all incidence entries have incidence kind"
        ( all
            ( \restriction ->
                case rKind restriction of
                  IncidenceRestriction {} -> True
                  PortalRestriction -> False
            )
            (incidenceRestrictions indexValue)
        )

testDenseIdsAndSecondaryIndexes :: Assertion
testDenseIdsAndSecondaryIndexes =
  case buildIndex of
    Left failure ->
      assertFailure ("expected index construction to succeed, received " <> show failure)
    Right indexValue -> do
      assertEqual "dense restriction ids" (fmap RestrictionId [0, 1, 2]) (restrictionIds indexValue)
      assertEqual
        "endpoint key map"
        ( IntMap.fromList
            [ (0, (ObjectKey 0, ObjectKey 1)),
              (1, (ObjectKey 1, ObjectKey 2)),
              (2, (ObjectKey 0, ObjectKey 2))
            ]
        )
        (restrictionEndpointKeyMap indexValue)
      assertEqual
        "outgoing inverse index"
        ( IntMap.fromList
            [ (0, IntSet.fromList [0, 2]),
              (1, IntSet.singleton 1)
            ]
        )
        (restrictionOutgoingByObject indexValue)
      assertEqual
        "incoming inverse index"
        ( IntMap.fromList
            [ (1, IntSet.singleton 0),
              (2, IntSet.fromList [1, 2])
            ]
        )
        (restrictionIncomingByObject indexValue)
      assertEqual
        "arrow inverse index"
        ( Map.fromList
            [ ((ObjectKey 0, ObjectKey 1), IntSet.singleton 0),
              ((ObjectKey 0, ObjectKey 2), IntSet.singleton 2),
              ((ObjectKey 1, ObjectKey 2), IntSet.singleton 1)
            ]
        )
        (restrictionIdsByArrowKey indexValue)

testUpdatePreservesSecondaryIndexes :: Assertion
testUpdatePreservesSecondaryIndexes =
  case buildIndex >>= updateRestrictionIndexEntriesWithDenseKeys keyOf (IntSet.singleton 1) remapToGhostPortal of
    Left failure ->
      assertFailure ("expected index update to succeed, received " <> show failure)
    Right indexValue -> do
      assertEqual "dense restriction ids after update" (fmap RestrictionId [0, 1, 2]) (restrictionIds indexValue)
      assertEqual
        "updated endpoint key map"
        ( IntMap.fromList
            [ (0, (ObjectKey 0, ObjectKey 1)),
              (1, (ObjectKey 0, ObjectKey 2)),
              (2, (ObjectKey 0, ObjectKey 2))
            ]
        )
        (restrictionEndpointKeyMap indexValue)
      assertEqual
        "updated outgoing inverse index"
        (IntMap.singleton 0 (IntSet.fromList [0, 1, 2]))
        (restrictionOutgoingByObject indexValue)
      assertEqual
        "updated incoming inverse index"
        ( IntMap.fromList
            [ (1, IntSet.singleton 0),
              (2, IntSet.fromList [1, 2])
            ]
        )
        (restrictionIncomingByObject indexValue)
      assertEqual
        "updated arrow inverse index"
        ( Map.fromList
            [ ((ObjectKey 0, ObjectKey 1), IntSet.singleton 0),
              ((ObjectKey 0, ObjectKey 2), IntSet.fromList [1, 2])
            ]
        )
        (restrictionIdsByArrowKey indexValue)
      assertEqual "updated incidence count" 1 (length (incidenceRestrictions indexValue))
      assertEqual "updated portal count" 2 (length (portalRestrictions indexValue))
  where
    keyOf :: MiniCell -> Maybe Int
    keyOf cell =
      case cell of
        Ghost -> Just 0
        Cell0 -> Just 1
        Cell1 -> Just 2

    remapToGhostPortal :: RestrictionPresentation (Restriction MiniCell RestrictionSpec) MiniCell RestrictionSpec
    remapToGhostPortal _restriction =
      RestrictionParts
        { partKind = PortalRestriction,
          partSource = Ghost,
          partTarget = Cell1,
          partWitness = RestrictionSpec PortalRestriction Ghost Cell1 9.0
        }

testAppliesStoredFunction :: Assertion
testAppliesStoredFunction =
  case buildIndex of
    Left failure ->
      assertFailure ("expected index construction to succeed, received " <> show failure)
    Right indexValue ->
      case restrictionsFrom (mkObjectIndex (basisCells basis)) Ghost indexValue of
        restriction : _ ->
          assertEqual
            "stored function result"
            (MiniStalk 6.0)
            (restrictApply restrictionSpecAlgebra restriction (MiniStalk 2.0))
        [] ->
          assertFailure "expected at least one restriction from Ghost"

parallelSpecs :: [RestrictionSpec]
parallelSpecs =
  [ RestrictionSpec PortalRestriction Ghost Cell0 2.0,
    RestrictionSpec PortalRestriction Ghost Cell0 3.0
  ]

buildParallelIndex :: Either (RestrictionIndexError MiniCell) (RestrictionIndex MiniCell RestrictionSpec)
buildParallelIndex =
  buildRestrictionIndex
    (mkObjectIndex (basisCells basis))
    presentRestrictionSpec
    parallelSpecs

testParallelRestrictionsPreserved :: Assertion
testParallelRestrictionsPreserved =
  case buildParallelIndex of
    Left failure ->
      assertFailure ("expected parallel index construction to succeed, received " <> show failure)
    Right indexValue -> do
      assertEqual "parallel entry count" 2 (length (restrictionEntries indexValue))
      assertEqual "first endpoint keys" (Just (ObjectKey 0, ObjectKey 1)) (restrictionEndpointKeys (RestrictionId 0) indexValue)
      assertEqual "second endpoint keys" (Just (ObjectKey 0, ObjectKey 1)) (restrictionEndpointKeys (RestrictionId 1) indexValue)
      assertEqual
        "parallel arrow ids"
        (Just (IntSet.fromList [0, 1]))
        (Map.lookup (ObjectKey 0, ObjectKey 1) (restrictionIdsByArrowKey indexValue))

testSheafPlansExtentFrontier :: Assertion
testSheafPlansExtentFrontier =
  case buildParallelIndex of
    Left failure ->
      assertFailure ("expected parallel index construction to succeed, received " <> show failure)
    Right indexValue -> do
      let plans = sheafPlansFromRestrictionIndex indexValue
          frontier = extentFrontierPlanAt (ObjectKey 0) plans
          targetFrontier = extentFrontierPlanAt (ObjectKey 1) plans
      assertEqual
        "restriction plans"
        (IntSet.fromList [0, 1])
        (IntMap.keysSet (spRestrictionPlansById plans))
      assertEqual
        "extent restriction ids"
        (IntSet.fromList [0, 1])
        (efpRestrictionIds frontier)
      assertEqual
        "extent target keys"
        (IntSet.singleton 1)
        (efpTargetKeys frontier)
      assertEqual
        "target-side frontier includes incoming restrictions"
        (IntSet.fromList [0, 1])
        (efpRestrictionIds targetFrontier)

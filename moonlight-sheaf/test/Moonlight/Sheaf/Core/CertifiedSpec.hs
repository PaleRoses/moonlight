{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Sheaf.Core.CertifiedSpec
  ( tests,
  )
where

import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Delta.Scope
  ( dirtyScope,
  )
import Moonlight.Sheaf.Kernel.Basis
  ( basisCells,
    mkSheafBasis,
  )
import Moonlight.Sheaf.Index.Dense
  ( denseIndexCount,
  )
import Moonlight.Sheaf.Section.Certified
  ( SectionCertification (..),
    SectionCertificationError (..),
    SectionCertificationFailure (..),
    certifySectionCompatibility,
    certifySectionExtentCompatibility,
    globalUnderlyingSection,
    mkGlobalSection,
  )
import Moonlight.Sheaf.Section.Model
  ( SheafModel,
    prepareSheafModel,
    sheafModelFingerprint,
    sheafModelObjects,
    sheafModelVersion,
  )
import Moonlight.Sheaf.Section.Morphism
  ( RestrictionParts (..),
    unitIncidenceRestriction,
  )
import Moonlight.Sheaf.Section.ObjectIndex
  ( SheafModelVersion (..),
    mkObjectIndex,
  )
import Moonlight.Sheaf.Section.Store.State
import Moonlight.Sheaf.Section.Store.Types
import Moonlight.Sheaf.TestFixture.Assertions (expectRight)
import Moonlight.Sheaf.TestFixture.Mini
  ( MiniCell (..),
    MiniRestriction (..),
    MiniStalk (..),
    miniBasis,
    miniStalkAlgebra,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "certified-section"
    [ testCase "mkGlobalSection certifies a restriction-compatible total section" testGlobalSectionAccepted,
      testCase "mkGlobalSection rejects restriction mismatches semantically" testGlobalSectionRejected,
      testCase "full certification accumulates every restriction mismatch" testFullCertificationAccumulatesRestrictionMismatches,
      testCase "extent certification matches full certification on touched frontier" testExtentCertificationMatchesFull,
      testCase "mkGlobalSection refuses equal-version equal-cardinality foreign model structure" testGlobalSectionRejectsForeignModelStructure
    ]

testGlobalSectionAccepted :: Assertion
testGlobalSectionAccepted = do
  model <- expectRight compatibleModel
  section <- expectRight (compatibleSection model)
  case mkGlobalSection model miniStalkAlgebra section of
    Right globalSection ->
      globalUnderlyingSection globalSection @?= section
    Left certification ->
      assertFailure ("expected global section, received " <> show certification)

testGlobalSectionRejected :: Assertion
testGlobalSectionRejected = do
  model <- expectRight compatibleModel
  section <- expectRight mismatchedSection
  case mkGlobalSection model miniStalkAlgebra section of
    Left (SectionCertificationSemanticallyRejected mismatches)
      | Map.member Cell1 mismatches ->
          pure ()
    Left certification ->
      assertFailure ("expected Cell1 rejection, received " <> show certification)
    Right _ ->
      assertFailure "expected incompatible section rejection"

data MiniRestrictionEdge
  = MiniToCell1
  | MiniToGhost
  deriving stock (Eq, Show)

multiRestrictionModel :: Either String (SheafModel MiniCell MiniRestriction)
multiRestrictionModel =
  case
    prepareSheafModel
      (SheafModelVersion 0)
      (mkObjectIndex (basisCells (mkSheafBasis [Cell0, Cell1, Ghost])))
      ( \edge ->
          RestrictionParts
            { partKind = unitIncidenceRestriction,
              partSource = Cell0,
              partTarget = multiRestrictionTarget edge,
              partWitness = MiniRestriction id
            }
      )
      [MiniToCell1, MiniToGhost]
  of
    Left modelError -> Left (show modelError)
    Right model -> Right model

multiRestrictionTarget :: MiniRestrictionEdge -> MiniCell
multiRestrictionTarget edge =
  case edge of
    MiniToCell1 ->
      Cell1
    MiniToGhost ->
      Ghost

multiMismatchedSection :: SheafModel MiniCell MiniRestriction -> Either (SectionConstructionError MiniCell) (TotalSectionStore MiniCell MiniStalk)
multiMismatchedSection model =
  mkTotalSectionStore
    model
    ( Map.fromList
        [ (Cell0, MiniStalk 0.0),
          (Cell1, MiniStalk 2.0),
          (Ghost, MiniStalk 3.0)
        ]
    )

testFullCertificationAccumulatesRestrictionMismatches :: Assertion
testFullCertificationAccumulatesRestrictionMismatches = do
  model <- expectRight multiRestrictionModel
  section <- expectRight (multiMismatchedSection model)
  case certifySectionCompatibility model miniStalkAlgebra section of
    Right (SectionRejected mismatches) ->
      Map.keysSet mismatches @?= Set.fromList [Cell1, Ghost]
    certification ->
      assertFailure ("expected accumulated rejection map, received " <> show certification)

testExtentCertificationMatchesFull :: Assertion
testExtentCertificationMatchesFull = do
  model <- expectRight compatibleModel
  section <- expectRight mismatchedSection
  certifySectionExtentCompatibility model miniStalkAlgebra (dirtyScope (IntSet.singleton 0)) section
    @?= certifySectionCompatibility model miniStalkAlgebra section

testGlobalSectionRejectsForeignModelStructure :: Assertion
testGlobalSectionRejectsForeignModelStructure = do
  sourceModel <- expectRight compatibleModel
  targetModel <- expectRight oppositeRestrictionModel
  sourceSection <- expectRight (compatibleSection sourceModel)
  sheafModelVersion sourceModel @?= sheafModelVersion targetModel
  denseIndexCount (sheafModelObjects sourceModel) @?= denseIndexCount (sheafModelObjects targetModel)
  assertBool
    "restriction structure changes the model fingerprint"
    (sheafModelFingerprint sourceModel /= sheafModelFingerprint targetModel)
  case mkGlobalSection targetModel miniStalkAlgebra sourceSection of
    Left
      ( SectionCertificationInfrastructureFailed
          ( SectionCertificationStoreFailed
              (SectionStoreModelFingerprintMismatch expectedFingerprint actualFingerprint)
            )
        ) ->
      do
        expectedFingerprint @?= sheafModelFingerprint targetModel
        actualFingerprint @?= sheafModelFingerprint sourceModel
    Left certification ->
      assertFailure ("expected store fingerprint mismatch failure, received " <> show certification)
    Right _ ->
      assertFailure "expected section model mismatch rejection"

compatibleModel :: Either String (SheafModel MiniCell MiniRestriction)
compatibleModel =
  case
    prepareSheafModel
      (SheafModelVersion 0)
      (mkObjectIndex (basisCells miniBasis))
      ( \() ->
          RestrictionParts
            { partKind = unitIncidenceRestriction,
              partSource = Cell0,
              partTarget = Cell1,
              partWitness = MiniRestriction id
            }
      )
      [()]
  of
    Left modelError -> Left (show modelError)
    Right model -> Right model

oppositeRestrictionModel :: Either String (SheafModel MiniCell MiniRestriction)
oppositeRestrictionModel =
  case
    prepareSheafModel
      (SheafModelVersion 0)
      (mkObjectIndex (basisCells miniBasis))
      ( \() ->
          RestrictionParts
            { partKind = unitIncidenceRestriction,
              partSource = Cell1,
              partTarget = Cell0,
              partWitness = MiniRestriction id
            }
      )
      [()]
  of
    Left modelError -> Left (show modelError)
    Right model -> Right model

compatibleSection ::
  SheafModel MiniCell MiniRestriction ->
  Either (SectionConstructionError MiniCell) (TotalSectionStore MiniCell MiniStalk)
compatibleSection model =
  mkTotalSectionStore
    model
    ( Map.fromList
        [ (Cell0, MiniStalk 0.0),
          (Cell1, MiniStalk 0.0)
        ]
    )

mismatchedSection :: Either (SectionConstructionError MiniCell) (TotalSectionStore MiniCell MiniStalk)
mismatchedSection =
  case compatibleModel of
    Left failure -> error failure
    Right model ->
      mkTotalSectionStore
        model
        ( Map.fromList
            [ (Cell0, MiniStalk 0.0),
              (Cell1, MiniStalk 2.0)
            ]
        )

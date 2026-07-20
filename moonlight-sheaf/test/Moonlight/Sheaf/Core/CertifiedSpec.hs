{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}

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
import Moonlight.Sheaf.Section.Certified
  ( SectionCertification (..),
    SectionCertificationFailure (..),
    certifySectionCompatibility,
    certifySectionExtentCompatibility,
    globalUnderlyingSection,
    mkGlobalSection,
  )
import Moonlight.Sheaf.Section.Model
  ( SheafModel,
    withPreparedSheafModel,
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
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "certified-section"
    [ testCase "mkGlobalSection certifies a restriction-compatible total section" testGlobalSectionAccepted,
      testCase "mkGlobalSection rejects restriction mismatches semantically" testGlobalSectionRejected,
      testCase "full certification accumulates every restriction mismatch" testFullCertificationAccumulatesRestrictionMismatches,
      testCase "extent certification matches full certification on touched frontier" testExtentCertificationMatchesFull
    ]

testGlobalSectionAccepted :: Assertion
testGlobalSectionAccepted =
  withCompatibleModel $ \model -> do
  section <- expectRight (compatibleSection model)
  case mkGlobalSection model miniStalkAlgebra section of
    Right globalSection ->
      globalUnderlyingSection globalSection @?= section
    Left certification ->
      assertFailure ("expected global section, received " <> show certification)

testGlobalSectionRejected :: Assertion
testGlobalSectionRejected =
  withCompatibleModel $ \model -> do
  section <- expectRight (mismatchedSection model)
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

withMultiRestrictionModel ::
  (forall owner. SheafModel owner MiniCell MiniRestriction -> Assertion) ->
  Assertion
withMultiRestrictionModel useModel =
  case
    withPreparedSheafModel
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
      useModel
  of
    Left modelError -> assertFailure ("expected multi-restriction model, received " <> show modelError)
    Right assertion -> assertion

multiRestrictionTarget :: MiniRestrictionEdge -> MiniCell
multiRestrictionTarget edge =
  case edge of
    MiniToCell1 ->
      Cell1
    MiniToGhost ->
      Ghost

multiMismatchedSection :: SheafModel owner MiniCell MiniRestriction -> Either (SectionConstructionError MiniCell) (TotalSectionStore owner MiniCell MiniStalk)
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
testFullCertificationAccumulatesRestrictionMismatches =
  withMultiRestrictionModel $ \model -> do
  section <- expectRight (multiMismatchedSection model)
  case certifySectionCompatibility model miniStalkAlgebra section of
    Right (SectionRejected mismatches) ->
      Map.keysSet mismatches @?= Set.fromList [Cell1, Ghost]
    certification ->
      assertFailure ("expected accumulated rejection map, received " <> show certification)

testExtentCertificationMatchesFull :: Assertion
testExtentCertificationMatchesFull =
  withCompatibleModel $ \model -> do
  section <- expectRight (mismatchedSection model)
  certifySectionExtentCompatibility model miniStalkAlgebra (dirtyScope (IntSet.singleton 0)) section
    @?= certifySectionCompatibility model miniStalkAlgebra section

withCompatibleModel ::
  (forall owner. SheafModel owner MiniCell MiniRestriction -> Assertion) ->
  Assertion
withCompatibleModel useModel =
  case
    withPreparedSheafModel
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
      useModel
  of
    Left modelError -> assertFailure ("expected compatible model, received " <> show modelError)
    Right assertion -> assertion

compatibleSection ::
  SheafModel owner MiniCell MiniRestriction ->
  Either (SectionConstructionError MiniCell) (TotalSectionStore owner MiniCell MiniStalk)
compatibleSection model =
  mkTotalSectionStore
    model
    ( Map.fromList
        [ (Cell0, MiniStalk 0.0),
          (Cell1, MiniStalk 0.0)
        ]
    )

mismatchedSection ::
  SheafModel owner MiniCell MiniRestriction ->
  Either (SectionConstructionError MiniCell) (TotalSectionStore owner MiniCell MiniStalk)
mismatchedSection model =
  mkTotalSectionStore
    model
    ( Map.fromList
        [ (Cell0, MiniStalk 0.0),
          (Cell1, MiniStalk 2.0)
        ]
    )

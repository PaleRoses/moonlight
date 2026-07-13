{-# LANGUAGE TypeFamilies #-}

module Moonlight.Cosheaf.Core.LinearSpec
  ( tests,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Cosheaf.Linear
import Moonlight.Cosheaf.SiteIndex
  ( CosheafSiteIndexFailure (..),
  )
import Moonlight.Cosheaf.Test.Fixture
  ( ChainMorphism (..),
    ChainObject (..),
    ChainSite (..),
    ChainSiteMode (..),
    chainAB,
    chainAC,
    chainBC,
    chainGhostToA,
    expectRight,
  )
import Moonlight.Homology
  ( BoundaryIncidence,
    BoundaryIncidenceShapeError,
    emptyBoundaryIncidenceOf,
    identityBoundaryIncidenceOf,
    mkBoundaryEntry,
    mkBoundaryIncidence,
  )
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism (..),
    Site (..),
  )
import Numeric.Natural
  ( Natural,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( Assertion,
    assertEqual,
    assertFailure,
    testCase,
  )

data LinearMatrixMode
  = LinearMatrixGood
  | LinearMatrixSourceShapeMismatch
  | LinearMatrixTargetShapeMismatch
  | LinearMatrixIdentityMismatch
  | LinearMatrixCompositionMismatch
  deriving stock (Eq, Ord, Show)

tests :: TestTree
tests =
  testGroup
    "linear cosheaf"
    [ testCase "constructs matrices over the public site index" testConstructsLinearCosheaf,
      testCase "rejects missing costalk" testRejectsMissingCostalk,
      testCase "rejects unknown costalk object" testRejectsUnknownCostalk,
      testCase "rejects duplicate local basis" testRejectsDuplicateBasis,
      testCase "rejects source dimension mismatch" testRejectsSourceShapeMismatch,
      testCase "rejects target dimension mismatch" testRejectsTargetShapeMismatch,
      testCase "rejects identity law mismatch" testRejectsIdentityMismatch,
      testCase "rejects undefined composition" testRejectsUndefinedComposition,
      testCase "rejects missing composite corestriction" testRejectsMissingCompositeCorestriction,
      testCase "rejects composition law mismatch" testRejectsCompositionMismatch,
      testCase "propagates public site-index failures" testPropagatesSiteIndexFailure
    ]

testConstructsLinearCosheaf :: Assertion
testConstructsLinearCosheaf = do
  cosheaf <- expectRight (chainLinearCosheaf LinearMatrixGood chainLinearCostalks)
  assertEqual "compiled morphisms plus identities" 6 (length (linearCosheafCorestrictions cosheaf))
  assertEqual "A dimension" (Just 2) (linearCostalkDimension <$> linearCostalkAt ChainA cosheaf)
  assertEqual "B dimension" (Just 2) (linearCostalkDimension <$> linearCostalkAt ChainB cosheaf)
  assertEqual "C dimension" (Just 2) (linearCostalkDimension <$> linearCostalkAt ChainC cosheaf)

testRejectsMissingCostalk :: Assertion
testRejectsMissingCostalk =
  case chainLinearCosheaf LinearMatrixGood (Map.delete ChainB chainLinearCostalks) of
    Left (LinearCostalkMissing ChainB) ->
      pure ()
    Left failureValue ->
      assertFailure ("unexpected failure: " <> show failureValue)
    Right _ ->
      assertFailure "expected missing-costalk failure"

testRejectsUnknownCostalk :: Assertion
testRejectsUnknownCostalk =
  case chainLinearCosheaf LinearMatrixGood (Map.insert ChainGhost [0, 1] chainLinearCostalks) of
    Left (LinearCostalkUnknownObject ChainGhost) ->
      pure ()
    Left failureValue ->
      assertFailure ("unexpected failure: " <> show failureValue)
    Right _ ->
      assertFailure "expected unknown-costalk failure"

testRejectsDuplicateBasis :: Assertion
testRejectsDuplicateBasis =
  case chainLinearCosheaf LinearMatrixGood (Map.insert ChainA [0, 0] chainLinearCostalks) of
    Left (LinearCostalkDuplicateBasis ChainA 0) ->
      pure ()
    Left failureValue ->
      assertFailure ("unexpected failure: " <> show failureValue)
    Right _ ->
      assertFailure "expected duplicate-basis failure"

testRejectsSourceShapeMismatch :: Assertion
testRejectsSourceShapeMismatch =
  case chainLinearCosheaf LinearMatrixSourceShapeMismatch chainLinearCostalks of
    Left (LinearCorestrictionShapeMismatch morphismValue expectedSource expectedTarget actualSource actualTarget) -> do
      assertEqual "morphism" chainAB morphismValue
      assertEqual "shape" (2, 2, 1, 2) (expectedSource, expectedTarget, actualSource, actualTarget)
    Left failureValue ->
      assertFailure ("unexpected failure: " <> show failureValue)
    Right _ ->
      assertFailure "expected source-shape failure"

testRejectsTargetShapeMismatch :: Assertion
testRejectsTargetShapeMismatch =
  case chainLinearCosheaf LinearMatrixTargetShapeMismatch chainLinearCostalks of
    Left (LinearCorestrictionShapeMismatch morphismValue expectedSource expectedTarget actualSource actualTarget) -> do
      assertEqual "morphism" chainAB morphismValue
      assertEqual "shape" (2, 2, 2, 1) (expectedSource, expectedTarget, actualSource, actualTarget)
    Left failureValue ->
      assertFailure ("unexpected failure: " <> show failureValue)
    Right _ ->
      assertFailure "expected target-shape failure"

testRejectsIdentityMismatch :: Assertion
testRejectsIdentityMismatch =
  case chainLinearCosheaf LinearMatrixIdentityMismatch chainLinearCostalks of
    Left (LinearCorestrictionIdentityMismatch morphismValue _actual _expected) ->
      assertEqual "identity morphism" (CheckedMorphism ChainA ChainA (ChainId ChainA)) morphismValue
    Left failureValue ->
      assertFailure ("unexpected failure: " <> show failureValue)
    Right _ ->
      assertFailure "expected identity-law failure"

testRejectsUndefinedComposition :: Assertion
testRejectsUndefinedComposition =
  case mkLinearCosheaf (ChainSite ChainMissingCompositeSite) (chainLinearAlgebra LinearMatrixGood) chainLinearCostalks of
    Left (LinearCorestrictionCompositionUndefined outerMorphism innerMorphism) -> do
      assertEqual "outer" chainBC outerMorphism
      assertEqual "inner" chainAB innerMorphism
    Left failureValue ->
      assertFailure ("unexpected failure: " <> show failureValue)
    Right _ ->
      assertFailure "expected undefined-composition failure"

testRejectsMissingCompositeCorestriction :: Assertion
testRejectsMissingCompositeCorestriction =
  case mkLinearCosheaf SparseCompositeSite sparseLinearAlgebra sparseLinearCostalks of
    Left (LinearCorestrictionCompositeMissing morphismValue) ->
      assertEqual "composite" sparseAC morphismValue
    Left failureValue ->
      assertFailure ("unexpected failure: " <> show failureValue)
    Right _ ->
      assertFailure "expected missing-composite-corestriction failure"

testRejectsCompositionMismatch :: Assertion
testRejectsCompositionMismatch =
  case chainLinearCosheaf LinearMatrixCompositionMismatch chainLinearCostalks of
    Left (LinearCorestrictionCompositionMismatch outerMorphism innerMorphism compositeMorphism _sequential _direct) -> do
      assertEqual "outer" chainBC outerMorphism
      assertEqual "inner" chainAB innerMorphism
      assertEqual "composite" chainAC compositeMorphism
    Left failureValue ->
      assertFailure ("unexpected failure: " <> show failureValue)
    Right _ ->
      assertFailure "expected composition-law failure"

testPropagatesSiteIndexFailure :: Assertion
testPropagatesSiteIndexFailure =
  case mkLinearCosheaf (ChainSite ChainUnknownSourceSite) (chainLinearAlgebra LinearMatrixGood) chainLinearCostalks of
    Left (LinearCosheafSiteIndexInvalid (CosheafMorphismSourceUnknown morphismValue)) ->
      assertEqual "bad source morphism" chainGhostToA morphismValue
    Left failureValue ->
      assertFailure ("unexpected failure: " <> show failureValue)
    Right _ ->
      assertFailure "expected site-index failure"

chainLinearCosheaf ::
  LinearMatrixMode ->
  Map ChainObject [Int] ->
  Either
    (LinearCosheafFailure ChainObject ChainMorphism Int Int BoundaryIncidenceShapeError)
    (LinearCosheaf ChainSite Int Int)
chainLinearCosheaf modeValue =
  mkLinearCosheaf (ChainSite ChainGoodSite) (chainLinearAlgebra modeValue)

chainLinearCostalks :: Map ChainObject [Int]
chainLinearCostalks =
  Map.fromList
    [ (ChainA, [0, 1]),
      (ChainB, [0, 1]),
      (ChainC, [0, 1])
    ]

chainLinearAlgebra :: LinearMatrixMode -> LinearCosheafAlgebra ChainSite Int BoundaryIncidenceShapeError
chainLinearAlgebra modeValue =
  LinearCosheafAlgebra
    { lcaCorestrictionMatrix = chainMatrixFor modeValue
    }

chainMatrixFor ::
  LinearMatrixMode ->
  CheckedMorphism ChainObject ChainMorphism ->
  Either BoundaryIncidenceShapeError (BoundaryIncidence Int)
chainMatrixFor modeValue morphismValue =
  case cmWitness morphismValue of
    ChainId ChainA
      | modeValue == LinearMatrixIdentityMismatch ->
          Right (emptyBoundaryIncidenceOf 2 2)
    ChainId _ ->
      Right identity2
    ChainAB
      | modeValue == LinearMatrixSourceShapeMismatch ->
          matrixOf 1 2 []
      | modeValue == LinearMatrixTargetShapeMismatch ->
          matrixOf 2 1 []
      | otherwise ->
          Right identity2
    ChainBC ->
      swap2
    ChainAC
      | modeValue == LinearMatrixCompositionMismatch ->
          Right identity2
      | otherwise ->
          swap2
    ChainGhostToA ->
      matrixOf 2 2 []
    ChainAToGhost ->
      matrixOf 2 2 []

identity2 :: BoundaryIncidence Int
identity2 =
  identityBoundaryIncidenceOf 2

swap2 :: Either BoundaryIncidenceShapeError (BoundaryIncidence Int)
swap2 =
  matrixOf 2 2 [(0, 1, 1), (1, 0, 1)]

matrixOf ::
  Natural ->
  Natural ->
  [(Natural, Natural, Int)] ->
  Either BoundaryIncidenceShapeError (BoundaryIncidence Int)
matrixOf sourceDimension targetDimension entries =
  mkBoundaryIncidence
    sourceDimension
    targetDimension
    (fmap (\(sourceIndexValue, targetIndexValue, coefficientValue) -> mkBoundaryEntry sourceIndexValue targetIndexValue coefficientValue) entries)

data SparseObject
  = SparseA
  | SparseB
  | SparseC
  deriving stock (Eq, Ord, Show)

data SparseMorphism
  = SparseId !SparseObject
  | SparseAB
  | SparseBC
  | SparseAC
  deriving stock (Eq, Ord, Show)

data SparseCompositeSite = SparseCompositeSite
  deriving stock (Eq, Ord, Show)

instance Site SparseCompositeSite where
  type SiteObject SparseCompositeSite = SparseObject
  type SiteMorphism SparseCompositeSite = SparseMorphism

  siteObjects _ =
    [SparseA, SparseB, SparseC]

  siteMorphisms _ =
    [sparseAB, sparseBC]

  identityAt _ objectValue =
    CheckedMorphism objectValue objectValue (SparseId objectValue)

  coversAt _ _ =
    []

  composeChecked _ outerMorphism innerMorphism
    | cmSource outerMorphism /= cmTarget innerMorphism =
        Nothing
    | sparseIsIdentity outerMorphism =
        Just innerMorphism
    | sparseIsIdentity innerMorphism =
        Just outerMorphism
    | cmWitness outerMorphism == SparseBC && cmWitness innerMorphism == SparseAB =
        Just sparseAC
    | otherwise =
        Nothing

  pullbackPair _ _ _ =
    Nothing

sparseAB :: CheckedMorphism SparseObject SparseMorphism
sparseAB =
  CheckedMorphism SparseA SparseB SparseAB

sparseBC :: CheckedMorphism SparseObject SparseMorphism
sparseBC =
  CheckedMorphism SparseB SparseC SparseBC

sparseAC :: CheckedMorphism SparseObject SparseMorphism
sparseAC =
  CheckedMorphism SparseA SparseC SparseAC

sparseIsIdentity :: CheckedMorphism SparseObject SparseMorphism -> Bool
sparseIsIdentity morphismValue =
  case cmWitness morphismValue of
    SparseId _ -> True
    _ -> False

sparseLinearCostalks :: Map SparseObject [Int]
sparseLinearCostalks =
  Map.fromList
    [ (SparseA, [0, 1]),
      (SparseB, [0, 1]),
      (SparseC, [0, 1])
    ]

sparseLinearAlgebra :: LinearCosheafAlgebra SparseCompositeSite Int BoundaryIncidenceShapeError
sparseLinearAlgebra =
  LinearCosheafAlgebra
    { lcaCorestrictionMatrix = sparseMatrixFor
    }

sparseMatrixFor :: CheckedMorphism SparseObject SparseMorphism -> Either BoundaryIncidenceShapeError (BoundaryIncidence Int)
sparseMatrixFor morphismValue =
  case cmWitness morphismValue of
    SparseId _ -> Right identity2
    SparseAB -> Right identity2
    SparseBC -> swap2
    SparseAC -> swap2

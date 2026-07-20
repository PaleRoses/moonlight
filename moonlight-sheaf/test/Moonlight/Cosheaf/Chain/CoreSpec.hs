{-# LANGUAGE TypeFamilies #-}

module Moonlight.Cosheaf.Chain.CoreSpec
  ( tests,
  )
where

import Data.Foldable (traverse_)
import Data.IntMap.Strict qualified as IntMap
import Data.List (findIndex, sort)
import Moonlight.Cosheaf
import Moonlight.Cosheaf.Test.Support
  ( fullFiniteCosheafColimit,
    prepareFullFiniteCosheafChain,
  )
import Moonlight.Cosheaf.Test.Fixture
import Moonlight.Homology
  ( BoundaryEntry,
    HomologicalDegree (..),
    RepresentativeChain (..),
    boundaryCoefficient,
    boundaryEntries,
    sourceIndex,
    targetIndex,
  )
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism,
    Site (..),
  )
import Moonlight.Sheaf.Section.ObjectIndex
  ( ObjectKey (..),
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

tests :: TestTree
tests =
  testGroup
    "bounded finite cosheaf chain assembly"
    [ testCase "C0 cells are all cosheaf representatives" testC0CellsAreRepresentatives,
      testCase "basis tables round-trip dense coordinates" testBasisTablesRoundTrip,
      testCase "basis keys are emitted in canonical order" testBasisKeysAreCanonical,
      testCase "C1 boundary propagates target and subtracts source" testC1BoundaryPropagatesTargetAndSubtractsSource,
      testCase "boundary nilpotence holds through degree two" testBoundaryNilpotence,
      testCase "H0 classes agree with cosheaf colimit classes" testH0Agreement,
      testCase "homology representatives lift through cosheaf basis tables" testLiftRepresentativeFailures,
      testCase "parallel morphism witnesses remain distinct C1 cells" testParallelMorphismsPreserved
    ]

testC0CellsAreRepresentatives :: Assertion
testC0CellsAreRepresentatives = do
  cosheaf <- expectRight (coverCosheaf (CoverSite CoverGoodSite) CoverGoodAlgebra coverRawCostalks)
  colimit <- expectRight (fullFiniteCosheafColimit cosheaf)
  plan <- expectRight (prepareFullFiniteCosheafChain 1 cosheaf)
  assertEqual
    "degree-zero chain cells match representative count"
    (length (cosheafColimitRepresentatives colimit))
    (length (cosheafChainCellsAtDegree (HomologicalDegree 0) plan))

testBasisTablesRoundTrip :: Assertion
testBasisTablesRoundTrip = do
  cosheaf <- expectRight (chainCosheaf (ChainSite ChainGoodSite) chainGoodAlgebra)
  plan <- expectRight (prepareFullFiniteCosheafChain 2 cosheaf)
  traverse_ (assertDegreeRoundTrip plan . HomologicalDegree) [0, 1, 2]

testBasisKeysAreCanonical :: Assertion
testBasisKeysAreCanonical = do
  cosheaf <- expectRight (chainCosheaf (ChainSite ChainGoodSite) chainGoodAlgebra)
  plan <- expectRight (prepareFullFiniteCosheafChain 2 cosheaf)
  traverse_ (assertDegreeBasisKeysCanonical plan . HomologicalDegree) [0, 1, 2]

assertDegreeBasisKeysCanonical ::
  PreparedFiniteCosheafChain ChainSite Int ->
  HomologicalDegree ->
  Assertion
assertDegreeBasisKeysCanonical plan degreeValue =
  assertEqual
    "basis keys are ascending semantic keys"
    (sort basisKeys)
    basisKeys
  where
    basisKeys =
      fmap cosheafChainCellKey (cosheafChainCellsAtDegree degreeValue plan)

assertDegreeRoundTrip ::
  PreparedFiniteCosheafChain ChainSite Int ->
  HomologicalDegree ->
  Assertion
assertDegreeRoundTrip plan degreeValue =
  traverse_
    (assertCellRoundTrip plan degreeValue)
    (zip [0 :: Int ..] (cosheafChainCellsAtDegree degreeValue plan))

assertCellRoundTrip ::
  PreparedFiniteCosheafChain ChainSite Int ->
  HomologicalDegree ->
  (Int, CosheafChainCell ChainObject ChainMorphism Int) ->
  Assertion
assertCellRoundTrip plan degreeValue (basisIndexValue, cellValue) = do
  assertEqual
    "basis index resolves from cell key"
    (Just basisIndexValue)
    (cosheafChainBasisIndexOf degreeValue (cosheafChainCellKey cellValue) plan)
  assertEqual
    "basis key resolves from dense index"
    (Just (cosheafChainCellKey cellValue))
    (cosheafChainBasisKeyAt degreeValue basisIndexValue plan)
  liftedCell <- expectRight (cosheafChainCellByBasisIndex degreeValue basisIndexValue plan)
  assertEqual
    "dense index lifts back to the original cosheaf cell"
    cellValue
    liftedCell

testC1BoundaryPropagatesTargetAndSubtractsSource :: Assertion
testC1BoundaryPropagatesTargetAndSubtractsSource = do
  cosheaf <- expectRight (chainCosheaf (ChainSite ChainGoodSite) chainGoodAlgebra)
  plan <- expectRight (prepareFullFiniteCosheafChain 1 cosheaf)
  sourceCellIndex <-
    expectCellIndex
      "AB source cell at A:0"
      (isCell [chainAB] (chainRep ChainA 0))
      (cosheafChainCellsAtDegree (HomologicalDegree 1) plan)
  targetBIndex <-
    expectCellIndex
      "target cell B:10"
      (isCell [] (chainRep ChainB 10))
      (cosheafChainCellsAtDegree (HomologicalDegree 0) plan)
  targetAIndex <-
    expectCellIndex
      "source cell A:0"
      (isCell [] (chainRep ChainA 0))
      (cosheafChainCellsAtDegree (HomologicalDegree 0) plan)
  let sourceBoundaryEntries =
        filter
          ((== sourceCellIndex) . sourceIndex)
          (boundaryEntries (cosheafBoundaryIncidenceAt (HomologicalDegree 1) plan))
      entrySummary :: BoundaryEntry Int -> (Int, Int)
      entrySummary entryValue =
        (targetIndex entryValue, boundaryCoefficient entryValue)
  assertEqual
    "d(A --AB--> B, 0) = (B, 10) - (A, 0)"
    (sort [(targetBIndex, 1), (targetAIndex, -1)])
    (sort (fmap entrySummary sourceBoundaryEntries))

testBoundaryNilpotence :: Assertion
testBoundaryNilpotence = do
  cosheaf <- expectRight (chainCosheaf (ChainSite ChainGoodSite) chainGoodAlgebra)
  plan <- expectRight (prepareFullFiniteCosheafChain 2 cosheaf)
  expectRight (verifyCosheafBoundaryNilpotence plan)

testH0Agreement :: Assertion
testH0Agreement = do
  cosheaf <- expectRight (coverCosheaf (CoverSite CoverGoodSite) CoverGoodAlgebra coverRawCostalks)
  rankAgreement <- expectRight (verifyCosheafH0RankAgreement cosheaf)
  assertEqual
    "colimit classes and H0 rank agree"
    CosheafH0Agreement
      { chaColimitClassCount = 2,
        chaHomologyFreeRank = 2,
        chaHomologyTorsionInvariants = []
      }
    rankAgreement
  agreement <- expectRight (verifyCosheafH0ClassAgreement cosheaf)
  assertEqual
    "rank agreement is carried by the class agreement"
    rankAgreement
    (ch0RankAgreement agreement)
  assertEqual
    "H0 classes have exactly the cosheaf colimit members"
    expectedCoverMemberClasses
    (sort (fmap classAgreementMembers (ch0ClassAgreements agreement)))
  assertEqual
    "each H0 class agreement contains four C0 cells"
    [4, 4]
    (sort (fmap (length . ch0DegreeZeroCells) (ch0ClassAgreements agreement)))

testLiftRepresentativeFailures :: Assertion
testLiftRepresentativeFailures = do
  cosheaf <- expectRight (chainCosheaf (ChainSite ChainGoodSite) chainGoodAlgebra)
  plan <- expectRight (prepareFullFiniteCosheafChain 1 cosheaf)
  let orderedRepresentative =
        RepresentativeChain
          { representativeDegree = HomologicalDegree 0,
            representativeTerms = [(2 :: Integer, 0), (3, 1)]
          }
  orderedWitness <- expectRight (liftCosheafRepresentative (HomologicalDegree 0) plan orderedRepresentative)
  assertEqual
    "lifted representative preserves coefficient order and basis indices"
    [(2 :: Integer, 0), (3, 1)]
    (fmap liftedTermSummary (chwRepresentativeTerms orderedWitness))
  homologyResult <-
    expectRight
      (cosheafIntegralHomologyResultWithRepresentatives plan [orderedRepresentative])
  assertEqual
    "homology result owns lifted witnesses by degree"
    (Just 1)
    (length <$> IntMap.lookup 0 (chrWitnessesByDegree homologyResult))
  let wrongDegreeRepresentative =
        RepresentativeChain
          { representativeDegree = HomologicalDegree 1,
            representativeTerms = [] :: [(Integer, Int)]
          }
  case liftCosheafRepresentative (HomologicalDegree 0) plan wrongDegreeRepresentative of
    Left (CosheafHomologyRepresentativeDegreeMismatch (HomologicalDegree 0) (HomologicalDegree 1)) ->
      pure ()
    Left otherFailure ->
      assertFailure ("unexpected representative degree failure: " <> show otherFailure)
    Right _ ->
      assertFailure "expected representative degree mismatch"
  let missingBasisRepresentative =
        RepresentativeChain
          { representativeDegree = HomologicalDegree 0,
            representativeTerms = [(1 :: Integer, 999)]
          }
  case liftCosheafRepresentative (HomologicalDegree 0) plan missingBasisRepresentative of
    Left (CosheafHomologyChainFailed (CosheafChainBasisIndexMissing (HomologicalDegree 0) 999)) ->
      pure ()
    Left otherFailure ->
      assertFailure ("unexpected missing basis failure: " <> show otherFailure)
    Right _ ->
      assertFailure "expected missing cosheaf basis obstruction"

testParallelMorphismsPreserved :: Assertion
testParallelMorphismsPreserved = do
  cosheaf <- expectRight (coverCosheaf (CoverSite CoverGoodSite) CoverGoodAlgebra coverRawCostalks)
  plan <- expectRight (prepareFullFiniteCosheafChain 1 cosheaf)
  let degreeOneCells = cosheafChainCellsAtDegree (HomologicalDegree 1) plan
  assertEqual
    "overlap-to-root via left contributes one cell per overlap value"
    2
    (length (filter (hasMorphismChain [coverOverlapToRootViaLeft]) degreeOneCells))
  assertEqual
    "overlap-to-root via right contributes one cell per overlap value"
    2
    (length (filter (hasMorphismChain [coverOverlapToRootViaRight]) degreeOneCells))

isCell ::
  (Eq obj, Eq mor, Eq value) =>
  [CheckedMorphism obj mor] ->
  CosectionRepresentative obj value ->
  CosheafChainCell obj mor value ->
  Bool
isCell morphisms representativeValue cell =
  cosheafNerveChainMorphisms (cosheafChainCellNerveChain cell) == morphisms
    && cosheafChainCellRepresentative cell == representativeValue

hasMorphismChain ::
  (Eq obj, Eq mor) =>
  [CheckedMorphism obj mor] ->
  CosheafChainCell obj mor value ->
  Bool
hasMorphismChain morphisms cell =
  cosheafNerveChainMorphisms (cosheafChainCellNerveChain cell) == morphisms

liftedTermSummary :: LiftedCosheafChainTerm ChainSite Int Integer -> (Integer, Int)
liftedTermSummary term =
  (lcctCoefficient term, lcctBasisIndex term)

classAgreementMembers :: CosheafH0ClassAgreement CoverObject CoverMorphism Int -> [(CoverObject, Int)]
classAgreementMembers agreement =
  sort
    [ (cosectionRepObject representativeValue, cosectionRepValue representativeValue)
    | cellValue <- ch0DegreeZeroCells agreement,
      let representativeValue = cosheafChainCellRepresentative cellValue
    ]

expectedCoverMemberClasses :: [[(CoverObject, Int)]]
expectedCoverMemberClasses =
  sort
    [ sort
        [ (CoverRoot, 100),
          (CoverLeft, 10),
          (CoverRight, 20),
          (CoverOverlap, 0)
        ],
      sort
        [ (CoverRoot, 101),
          (CoverLeft, 11),
          (CoverRight, 21),
          (CoverOverlap, 1)
        ]
    ]

expectCellIndex :: String -> (cell -> Bool) -> [cell] -> IO Int
expectCellIndex label predicate cells =
  maybe
    (assertFailure ("missing chain cell: " <> label))
    pure
    (findIndex predicate cells)

chainRep :: ChainObject -> Int -> CosectionRepresentative ChainObject Int
chainRep objectValue value =
  CosectionRepresentative
    { cosectionRepObject = objectValue,
      cosectionRepValue = value
    }

coverRep :: CoverObject -> Int -> CosectionRepresentative CoverObject Int
coverRep objectValue value =
  CosectionRepresentative
    { cosectionRepObject = objectValue,
      cosectionRepValue = value
    }

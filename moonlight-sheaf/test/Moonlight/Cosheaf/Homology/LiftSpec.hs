{-# LANGUAGE TypeFamilies #-}

module Moonlight.Cosheaf.Homology.LiftSpec
  ( tests,
  )
where

import Moonlight.Cosheaf.Chain
  ( CosheafChainCell,
    CosheafChainFailure (..),
    PreparedFiniteCosheafChain,
    cosheafChainBasisIndexOf,
    cosheafChainCellKey,
    cosheafChainCellNerveChain,
    cosheafNerveChainMorphisms,
  )
import Moonlight.Cosheaf.Homology
  ( CosheafHomologyFailure (..),
    LiftedCosheafChainTerm (..),
    chwRepresentativeTerms,
    liftCosheafRepresentative,
  )
import Moonlight.Cosheaf.Test.Fixture.ConstantSingleton
  ( SingletonCostalk,
    constantSingletonCosheaf,
  )
import Moonlight.Cosheaf.Test.Fixture.Representative
  ( RepresentativeBuildFailure,
    findUniqueCellAtDegree,
    liftedWitnessSupportKeys,
    representativeFromCells,
  )
import Moonlight.Cosheaf.Test.Homology.Expect
  ( expectRight,
  )
import Moonlight.Cosheaf.Test.Support
  ( prepareFullFiniteCosheafChain,
  )
import Moonlight.Cosheaf.Test.Site.CyclicGroup
  ( CyclicGroupMorphism (..),
    CyclicGroupObject,
    CyclicGroupSite,
    cyclicGroupSite,
  )
import Moonlight.Homology
  ( HomologicalDegree (..),
    RepresentativeChain (..),
  )
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism (..),
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

zeroDegree :: HomologicalDegree
zeroDegree =
  HomologicalDegree 0

oneDegree :: HomologicalDegree
oneDegree =
  HomologicalDegree 1

tests :: TestTree
tests =
  testGroup
    "cosheaf homology representative lifting"
    [ testCase "valid representative lifts to exact cosheaf cell" testValidRepresentativeLift,
      testCase "degree mismatch returns typed obstruction" testDegreeMismatchFails,
      testCase "basis index outside degree returns typed obstruction" testBasisIndexOutsideDegreeFails,
      testCase "empty representative lifts to empty support" testEmptyRepresentativeLift,
      testCase "duplicate representative terms preserve coefficients" testDuplicateTermsPreserved
    ]

testValidRepresentativeLift :: Assertion
testValidRepresentativeLift = do
  plan <- prepareCyclicPlan 2 1
  cell <- expectRight (cellByExponents oneDegree [1] plan)
  representative <- expectRight (representativeFromCells oneDegree [(5, cell)] plan)
  witness <- expectRight (liftCosheafRepresentative oneDegree plan representative)
  assertEqual
    "lifted witness owns the exact cosheaf cell"
    [cell]
    (fmap lcctCell (chwRepresentativeTerms witness))
  assertEqual
    "lifted support is reported by semantic basis key"
    [(5, cosheafChainCellKey cell)]
    (liftedWitnessSupportKeys witness)

testDegreeMismatchFails :: Assertion
testDegreeMismatchFails = do
  plan <- prepareCyclicPlan 2 1
  let representative =
        RepresentativeChain
          { representativeDegree = zeroDegree,
            representativeTerms = [] :: [(Integer, Int)]
          }
  case liftCosheafRepresentative oneDegree plan representative of
    Left (CosheafHomologyRepresentativeDegreeMismatch expectedDegree observedDegree) ->
      assertEqual
        "degree mismatch carries expected and observed degrees"
        (oneDegree, zeroDegree)
        (expectedDegree, observedDegree)
    Left otherFailure ->
      assertFailure ("unexpected failure: " <> show otherFailure)
    Right _ ->
      assertFailure "expected degree mismatch obstruction"

testBasisIndexOutsideDegreeFails :: Assertion
testBasisIndexOutsideDegreeFails = do
  plan <- prepareCyclicPlan 2 1
  let missingIndex = 99
      representative =
        RepresentativeChain
          { representativeDegree = oneDegree,
            representativeTerms = [(1 :: Integer, missingIndex)]
          }
  case liftCosheafRepresentative oneDegree plan representative of
    Left (CosheafHomologyChainFailed (CosheafChainBasisIndexMissing degreeValue basisIndexValue)) ->
      assertEqual
        "missing basis coordinate is typed by degree and index"
        (oneDegree, missingIndex)
        (degreeValue, basisIndexValue)
    Left otherFailure ->
      assertFailure ("unexpected failure: " <> show otherFailure)
    Right _ ->
      assertFailure "expected missing basis index obstruction"

testEmptyRepresentativeLift :: Assertion
testEmptyRepresentativeLift = do
  plan <- prepareCyclicPlan 2 1
  let representative =
        RepresentativeChain
          { representativeDegree = oneDegree,
            representativeTerms = [] :: [(Integer, Int)]
          }
  witness <- expectRight (liftCosheafRepresentative oneDegree plan representative)
  assertEqual
    "empty representative remains empty after lifting"
    []
    (chwRepresentativeTerms witness)

testDuplicateTermsPreserved :: Assertion
testDuplicateTermsPreserved = do
  plan <- prepareCyclicPlan 2 1
  cell <- expectRight (cellByExponents oneDegree [1] plan)
  basisIndex <- expectMaybe "cyclic generator basis index" (cosheafChainBasisIndexOf oneDegree (cosheafChainCellKey cell) plan)
  let representative =
        RepresentativeChain
          { representativeDegree = oneDegree,
            representativeTerms = [(1 :: Integer, basisIndex), (2, basisIndex)]
          }
  witness <- expectRight (liftCosheafRepresentative oneDegree plan representative)
  assertEqual
    "lift preserves duplicate coordinates; normalization belongs to the caller"
    [(1 :: Integer, basisIndex), (2, basisIndex)]
    (fmap (\term -> (lcctCoefficient term, lcctBasisIndex term)) (chwRepresentativeTerms witness))

prepareCyclicPlan :: Int -> Int -> IO (PreparedFiniteCosheafChain CyclicGroupSite SingletonCostalk)
prepareCyclicPlan orderValue maxDegreeValue = do
  site <- expectRight (cyclicGroupSite orderValue)
  cosheaf <- expectRight (constantSingletonCosheaf site)
  expectRight (prepareFullFiniteCosheafChain (fromIntegral maxDegreeValue) cosheaf)

cellByExponents ::
  HomologicalDegree ->
  [Int] ->
  PreparedFiniteCosheafChain CyclicGroupSite SingletonCostalk ->
  Either
    (RepresentativeBuildFailure CyclicGroupObject CyclicGroupMorphism SingletonCostalk)
    (CosheafChainCell CyclicGroupObject CyclicGroupMorphism SingletonCostalk)
cellByExponents degreeValue expectedExponents =
  findUniqueCellAtDegree
    degreeValue
    ("cyclic cell " <> show expectedExponents)
    (cellHasExponents expectedExponents)

cellHasExponents :: [Int] -> CosheafChainCell CyclicGroupObject CyclicGroupMorphism value -> Bool
cellHasExponents expectedExponents cellValue =
  fmap (cyclicGroupMorphismExponent . cmWitness) (cosheafNerveChainMorphisms (cosheafChainCellNerveChain cellValue))
    == expectedExponents

expectMaybe :: String -> Maybe value -> IO value
expectMaybe label result =
  case result of
    Just value ->
      pure value
    Nothing ->
      assertFailure ("expected Just: " <> label)

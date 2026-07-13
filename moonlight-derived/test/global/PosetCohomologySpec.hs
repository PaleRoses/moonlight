module PosetCohomologySpec
  ( tests
  ) where

import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.List (subsequences)
import Moonlight.Derived.Pure.Cohomology.Poset
  ( posetCechComplex
  , posetSheafCohomology
  , posetSheafCohomologyDims
  )
import Moonlight.Derived.Pure.Site.Poset (FinObjectId (..), DerivedPoset (..), mkDerivedPosetFromCovers)
import Moonlight.Homology
  ( BoundaryIncidence
  , FiniteChainComplex
  , HomologicalDegree (..)
  , boundaryCoefficient
  , boundaryEntries
  , cohomologyBasisAt
  , composeBoundaryIncidence
  , incidenceMatrixAt
  , maxHomologicalDegree
  , mkBoundaryEntry
  , mkBoundaryIncidence
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion
  , (@?=)
  , assertBool
  , assertFailure
  , testCase
  )

expectPoset :: [FinObjectId] -> [(FinObjectId, FinObjectId)] -> DerivedPoset
expectPoset ns cs = either (error . show) id (mkDerivedPosetFromCovers ns cs)

tests :: TestTree
tests =
  testGroup
    "PosetCohomology"
    [ testCase "discrete two-point poset has two connected components" $
        posetSheafCohomologyDims discretePoset unitStalk unitRestriction @?= Right [2]
    , testCase "constant sheaf on a chain is contractible" $
        posetSheafCohomologyDims chainPoset unitStalk unitRestriction @?= Right [1, 0]
    , testCase "constant-sheaf Cech differentials square to zero on every valid 2- and 3-node cover relation" $
        mapM_ assertCechNilpotence validCoverCases
    , testCase "posetSheafCohomology agrees with cohomologyBasisAt on the induced Cech complex" $
        case posetCechComplex chainPoset unitStalk unitRestriction of
          Left failure -> assertFailure (show failure)
          Right cechComplex ->
            mapM_
              (\degreeValue ->
                 case posetSheafCohomology chainPoset unitStalk unitRestriction degreeValue of
                   Left err -> assertFailure (show err)
                   Right cocycles ->
                     length cocycles
                       @?= length (cohomologyBasisAt cechComplex degreeValue)
              )
              [HomologicalDegree 0, HomologicalDegree 1]
    ]
  where
    discretePoset =
      expectPoset [FinObjectId 0, FinObjectId 1] []
    chainPoset =
      expectPoset [FinObjectId 0, FinObjectId 1] [(FinObjectId 0, FinObjectId 1)]

unitStalk :: FinObjectId -> Int
unitStalk =
  const 1

unitRestriction :: (FinObjectId, FinObjectId) -> BoundaryIncidence Integer
unitRestriction _ =
  case mkBoundaryIncidence 1 1 [mkBoundaryEntry 0 0 1] of
    Left failure ->
      error (show failure)
    Right incidenceValue ->
      incidenceValue

validCoverCases :: [([FinObjectId], [(FinObjectId, FinObjectId)])]
validCoverCases =
  [ (nodes, coverPairs)
  | nodes <- [fmap FinObjectId [0, 1], fmap FinObjectId [0, 1, 2]]
  , coverPairs <- subsequences (allComparablePairs nodes)
  , isValidCoverRelation nodes coverPairs
  ]

allComparablePairs :: [FinObjectId] -> [(FinObjectId, FinObjectId)]
allComparablePairs nodes =
  [ (leftNode, rightNode)
  | leftNode@(FinObjectId leftOrdinal) <- nodes
  , rightNode@(FinObjectId rightOrdinal) <- nodes
  , leftOrdinal < rightOrdinal
  ]

isValidCoverRelation :: [FinObjectId] -> [(FinObjectId, FinObjectId)] -> Bool
isValidCoverRelation nodes coverPairs =
  case mkDerivedPosetFromCovers nodes coverPairs of
    Left _ ->
      False
    Right posetValue ->
      canonicalCoverPairs posetValue == coverPairs

canonicalCoverPairs :: DerivedPoset -> [(FinObjectId, FinObjectId)]
canonicalCoverPairs DerivedPoset{derivedPosetCoversUp} =
  [ (FinObjectId sourceOrdinal, FinObjectId targetOrdinal)
  | (sourceOrdinal, targetOrdinals) <- IM.toAscList derivedPosetCoversUp
  , targetOrdinal <- IS.toAscList targetOrdinals
  ]

assertCechNilpotence :: ([FinObjectId], [(FinObjectId, FinObjectId)]) -> Assertion
assertCechNilpotence (nodes, coverPairs) =
  case posetCechComplex (expectPoset nodes coverPairs) unitStalk unitRestriction of
    Left failure ->
      assertFailure
        ( "expected Cech complex construction to succeed for cover relation "
            <> show coverPairs
            <> ", received "
            <> show failure
        )
    Right cechComplex ->
      let HomologicalDegree maxDegreeValue = maxHomologicalDegree cechComplex
       in mapM_
            (assertDegreeNilpotence coverPairs cechComplex)
            [1 .. maxDegreeValue]

assertDegreeNilpotence ::
  [(FinObjectId, FinObjectId)] ->
  FiniteChainComplex Integer ->
  Int ->
  Assertion
assertDegreeNilpotence coverPairs cechComplex degreeValue =
  case
    composeBoundaryIncidence
      (incidenceMatrixAt cechComplex (HomologicalDegree (degreeValue - 1)))
      (incidenceMatrixAt cechComplex (HomologicalDegree degreeValue))
  of
    Left shapeError ->
      assertFailure
        ( "unexpected boundary shape failure for cover relation "
            <> show coverPairs
            <> " at degree "
            <> show degreeValue
            <> ": "
            <> show shapeError
        )
    Right composedBoundary ->
      assertBool
        ( "expected d ∘ d = 0 for cover relation "
            <> show coverPairs
            <> " at degree "
            <> show degreeValue
        )
        (all ((== 0) . boundaryCoefficient) (boundaryEntries composedBoundary))

{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Differential.Effect.Laws.WCOJ
  ( lawBundles,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Data.Set qualified as Set

import Moonlight.Differential.Effect.Harness.WCOJ
  ( TestGenericWCOJProblem (..),
  )
import Moonlight.Differential.Effect.Harness.WCOJ qualified as Harness
import Moonlight.Differential.Effect.LawNames (LawName (..))
import Moonlight.Differential.Join.WCOJ
  ( JoinAlgebra (..),
    domainSize,
    domainToList,
    existsJoin,
    foldGenericJoin,
  )
import Moonlight.Differential.Join.WCOJ.Dense.Triangle
  ( TriangleBenchmarkStats (..),
    TriangleCount (..),
    buildDenseTriangleTrie,
    countTrianglesWCOJ,
    triangleBenchmarkStats,
  )
import Moonlight.Pale.Test.LawSuite (LawBundle, lawBundleQuickCheck, quickCheckLawDefinition)
import Test.Tasty.QuickCheck qualified as QC

newtype TestTriangleEdges = TestTriangleEdges
  { unTestTriangleEdges :: [(Int, Int)]
  }
  deriving stock (Eq, Show)

instance QC.Arbitrary TestTriangleEdges where
  arbitrary =
    TestTriangleEdges
      <$> QC.listOf
        ((,) <$> QC.chooseInt (-2, 8) <*> QC.chooseInt (-2, 8))

instance QC.Arbitrary TestGenericWCOJProblem where
  arbitrary = do
    universe <- Set.fromList <$> QC.sublistOf [0 .. 5]
    let universePairs =
          (,) <$> Set.toAscList universe <*> Set.toAscList universe
    TestGenericWCOJProblem universe
      <$> (Set.fromList <$> QC.sublistOf universePairs)
      <*> (Set.fromList <$> QC.sublistOf universePairs)
      <*> (Set.fromList <$> QC.sublistOf universePairs)

propGenericWCOJMatchesBruteForce :: TestGenericWCOJProblem -> QC.Property
propGenericWCOJMatchesBruteForce problem =
  QC.conjoin
    [ Harness.genericWCOJDenotation problem QC.=== Harness.bruteForceWCOJDenotation problem,
      genericEnvironments problem (Harness.genericWCOJSlots <> Harness.genericWCOJSlots) IntMap.empty
        QC.=== genericEnvironments problem Harness.genericWCOJSlots IntMap.empty,
      QC.conjoin (fmap (preboundSlotIsPreserved problem) candidateValues)
    ]
  where
    candidateValues =
      (-1) : Set.toAscList (Harness.tgwUniverse problem)

genericEnvironments ::
  TestGenericWCOJProblem ->
  [Int] ->
  IntMap.IntMap Int ->
  Set.Set (IntMap.IntMap Int)
genericEnvironments problem slots env =
  foldGenericJoin
    Harness.genericWCOJAlgebra
    problem
    slots
    env
    (flip Set.insert)
    Set.empty

preboundSlotIsPreserved :: TestGenericWCOJProblem -> Int -> QC.Property
preboundSlotIsPreserved problem value =
  QC.conjoin
    [ prebound QC.=== expected,
      existsJoin Harness.genericWCOJAlgebra problem Harness.genericWCOJSlots initial
        QC.=== not (Set.null expected)
    ]
  where
    initial = IntMap.singleton 0 value
    prebound = genericEnvironments problem Harness.genericWCOJSlots initial
    expected =
      Set.filter ((== Just value) . IntMap.lookup 0)
        (genericEnvironments problem Harness.genericWCOJSlots IntMap.empty)

propGenericWCOJCountMatchesPropose :: TestGenericWCOJProblem -> QC.Property
propGenericWCOJCountMatchesPropose problem =
  QC.conjoin
    [ joinCount Harness.genericWCOJAlgebra problem env slot
        QC.=== domainSize (joinPropose Harness.genericWCOJAlgebra problem env slot)
    | env <- Harness.genericWCOJEnvSamples problem,
      slot <- Harness.genericWCOJSlots
    ]

propIndexedWCOJExtendersMatchSetBaseline :: TestGenericWCOJProblem -> QC.Property
propIndexedWCOJExtendersMatchSetBaseline problem =
  QC.conjoin
    ( denotationMatches
        : [ QC.counterexample ("slot " <> show slot <> ", env " <> show env) $
              indexedExtenderMatchesBaseline env slot
          | env <- Harness.genericWCOJEnvSamples problem,
            slot <- Harness.genericWCOJSlots
          ]
    )
  where
    denotationMatches =
      Harness.indexedAdaptiveWCOJDenotation problem QC.=== Harness.adaptiveWCOJDenotation problem

    indexedExtenderMatchesBaseline env slot =
      QC.conjoin
        [ joinCount Harness.indexedGenericWCOJAlgebra problem env slot
            QC.=== domainSize (joinPropose Harness.indexedGenericWCOJAlgebra problem env slot),
          joinCount Harness.indexedGenericWCOJAlgebra problem env slot
            QC.=== joinCount Harness.genericWCOJAlgebra problem env slot,
          Set.fromList (domainToList (joinPropose Harness.indexedGenericWCOJAlgebra problem env slot))
            QC.=== Set.fromList (domainToList (joinPropose Harness.genericWCOJAlgebra problem env slot))
        ]

propAdaptiveWCOJMatchesGeneric :: TestGenericWCOJProblem -> QC.Property
propAdaptiveWCOJMatchesGeneric problem =
  Harness.adaptiveWCOJDenotation problem QC.=== Harness.genericWCOJDenotation problem

propFusedIndexedWCOJMatchesAdaptive :: TestGenericWCOJProblem -> QC.Property
propFusedIndexedWCOJMatchesAdaptive problem =
  Harness.fusedIndexedWCOJDenotation problem QC.=== Harness.adaptiveWCOJDenotation problem

propFoldAdaptiveWCOJMatchesAdaptive :: TestGenericWCOJProblem -> QC.Property
propFoldAdaptiveWCOJMatchesAdaptive problem =
  Harness.foldAdaptiveWCOJDenotation problem QC.=== Harness.adaptiveWCOJDenotation problem

propExistsWCOJMatchesGeneric :: TestGenericWCOJProblem -> QC.Property
propExistsWCOJMatchesGeneric problem =
  existsJoin Harness.genericWCOJAlgebra problem Harness.genericWCOJSlots IntMap.empty
    QC.=== not (Set.null (Harness.genericWCOJDenotation problem))

propDenseTriangleWCOJMatchesBruteForce :: TestTriangleEdges -> QC.Property
propDenseTriangleWCOJMatchesBruteForce (TestTriangleEdges rawEdges) =
  fmap (tcTriangles . countTrianglesWCOJ) (buildDenseTriangleTrie triangleVertexCount rawEdges)
    QC.=== Right (Harness.bruteForceTriangleCount rawEdges)

propDenseTriangleStatsReportNormalizedEdges :: TestTriangleEdges -> QC.Property
propDenseTriangleStatsReportNormalizedEdges (TestTriangleEdges rawEdges) =
  fmap (tbsEdges . triangleBenchmarkStats) (buildDenseTriangleTrie triangleVertexCount rawEdges)
    QC.=== Right (Set.size (Harness.normalizedEdgeSet rawEdges))

triangleVertexCount :: Int
triangleVertexCount =
  9

lawBundles :: [LawBundle String]
lawBundles =
  [ lawBundleQuickCheck
      "wcoj"
      [ quickCheckLawDefinition GenericJoinAgreesWithBruteForceOracle propGenericWCOJMatchesBruteForce,
        quickCheckLawDefinition GenericJoinCountIsProposedDomainSize propGenericWCOJCountMatchesPropose,
        quickCheckLawDefinition IndexedExtendersAgreeWithSetBaseline propIndexedWCOJExtendersMatchSetBaseline,
        quickCheckLawDefinition FusedIndexedFoldAgreesWithGenericAdaptive propFusedIndexedWCOJMatchesAdaptive,
        quickCheckLawDefinition AdaptiveJoinAgreesWithGeneric propAdaptiveWCOJMatchesGeneric,
        quickCheckLawDefinition AdaptiveFoldAgreesWithMaterialized propFoldAdaptiveWCOJMatchesAdaptive,
        quickCheckLawDefinition JoinExistenceAgreesWithGenericDenotation propExistsWCOJMatchesGeneric,
        quickCheckLawDefinition DenseTriangleAgreesWithBruteForceOracle propDenseTriangleWCOJMatchesBruteForce,
        quickCheckLawDefinition DenseTriangleStatsExposeNormalizedEdgeCount propDenseTriangleStatsReportNormalizedEdges
      ]
  ]

{-# LANGUAGE DataKinds #-}

module Moonlight.Cosheaf.Chain.LinearSpec
  ( tests,
  )
where

import Data.Foldable (toList)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Ratio ((%))
import Moonlight.Algebra
  ( Semiring,
  )
import Moonlight.Cosheaf
  ( CoefficientOps,
    CosheafBlockMorseFailure (..),
    CosheafBoundaryProvenance (..),
    LinearCosheafAlgebra (..),
    LinearCosheafChainFailure (..),
    LinearCosheafChainSpec (..),
    LinearCosheafSupportFailure (..),
    LinearCosheafSupportPlan,
    cmmCriticalCoordinates,
    cmmPairs,
    cmhaGroupsByDegree,
    cmrHomologyAgreement,
    cmrMatching,
    cmrReducedChain,
    cosheafCoordinateDegree,
    blockSchurReduceCosheafChain,
    blockSchurReduceCosheafChainWithPlan,
    cbmrReducedChain,
    cbmrSchurReduction,
    CosheafBlockPivotPlan (..),
    defaultCosheafMorsePolicy,
    defaultWholeCostalkBlockSchurPolicy,
    gf2CoefficientOps,
    gf2PivotOps,
    intCoefficientOps,
    integerCoefficientOps,
    intUnitPivotOps,
    lchaGroupsByDegree,
    linearCosheafHomology,
    linearCosheafSupportPlanFromLists,
    mkLinearCosheaf,
    morseReduceCosheafChain,
    PreparedCosheafChain,
    pccChainComplex,
    prepareLinearCosheafChainFromLinearCosheaf,
    prepareLinearCosheafChainFromSupportPlan,
    preparedCosheafBoundaryEntryProvenance,
    preparedCosheafBoundaryIncidenceAt,
    rationalCoefficientOps,
    rationalPivotOps,
    fullLinearCosheafSupportPlan,
  )
import Moonlight.Homology
  ( BlockSchurFailure (..),
    BlockSchurReduction (..),
    BlockSchurTranscript (..),
    BoundaryEntry,
    BoundaryIncidence,
    BoundaryIncidenceShapeError,
    HomologyBackend (..),
    HomologicalDegree (..),
    boundaryCoefficient,
    boundaryEntries,
    freeRank,
    identityBoundaryIncidenceOf,
    integerUnimodularBlockPivotOps,
    incidenceMatrixAt,
    mkBoundaryEntry,
    mkBoundaryIncidenceFromOrderedEntries,
    sourceCardinality,
    sourceIndex,
    targetIndex,
    torsionInvariants,
  )
import Moonlight.Homology.Effect.Laws
  ( BlockSchurHomologyAgreement (..),
    checkBlockSchurHomologyAgreement,
  )
import Moonlight.LinAlg
  ( BlockMatrixFailure (..),
    GF2 (..),
  )
import Moonlight.Cosheaf.Test.Fixture
  ( ChainMorphism,
    ChainObject (..),
    ChainSite (..),
    ChainSiteMode (..),
    chainAB,
  )
import Moonlight.Sheaf.Site.Analysis.Scaffold
  ( SiteBoundaryAlgebra (..),
    siteChainComplexFromSite,
  )
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism,
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
    "linear cosheaf chain assembly"
    [ testCase "rank-one constant blocks reproduce the site boundary" testRankOneMatchesSiteBoundary,
      testCase "native LinearCosheaf lowers into the chain assembler" testNativeLinearCosheafAssembly,
      testCase "rank-k blocks assemble by local coordinate" testRankKBlockCopies,
      testCase "bad local block shape is a typed obstruction" testBadBlockShapeFails,
      testCase "negative costalk dimension is a typed obstruction" testNegativeCostalkDimensionFails,
      testCase "support pruning skips pruned corestriction blocks" testSupportPruningSkipsPrunedBlocks,
      testCase "boundary provenance survives assembly" testBoundaryProvenance,
      testCase "Morse reduction matches assembled coordinates, not site cells" testCoordinateMorseReduction,
      testCase "integer Morse does not cancel nonunit pivots" testIntegerMorseRejectsNonunitPivot,
      testCase "integral backend records linear cosheaf torsion" testIntegralLinearHomologyDetectsTorsion,
      testCase "rational Morse cancels nonunit pivots over the field" testRationalMorseCancelsNonunitPivot,
      testCase "GF2 Morse cancels field pivots and preserves field Betti" testGF2MorsePreservesBetti,
      testCase "Block-Schur reduction cancels a rank-2 costalk block semantically" testBlockSchurCostalkReduction,
      testCase "Block-Schur integer mode rejects non-unimodular costalk blocks" testBlockSchurRejectsNonUnimodularIntegerBlock
    ]

testRankOneMatchesSiteBoundary :: Assertion
testRankOneMatchesSiteBoundary = do
  chain <- expectRight (prepareFullLinearCosheafChain intCoefficientOps (intervalSpec identityBlock1))
  siteComplex <- expectRight (siteChainComplexFromSite intervalBoundaryAlgebra IntervalSite)
  assertEqual
    "rank-one constant cosheaf boundary is the ordinary site boundary"
    (incidenceMatrixAt siteComplex (HomologicalDegree 1))
    (preparedCosheafBoundaryIncidenceAt (HomologicalDegree 1) chain)

testNativeLinearCosheafAssembly :: Assertion
testNativeLinearCosheafAssembly = do
  cosheaf <-
    expectRight
      ( mkLinearCosheaf
          (ChainSite ChainGoodSite)
          nativeLinearCosheafAlgebra
          nativeLinearCosheafCostalks
      )
  chain <-
    expectRight
      (prepareLinearCosheafChainFromLinearCosheaf intCoefficientOps nativeBoundaryAlgebra cosheaf)
  assertEqual
    "native LinearCosheaf corestriction owns the assembled boundary"
    [(0, 0, 1)]
    (entrySummary <$> boundaryEntries (preparedCosheafBoundaryIncidenceAt (HomologicalDegree 1) chain))

testRankKBlockCopies :: Assertion
testRankKBlockCopies = do
  chain <- expectRight (prepareFullLinearCosheafChain intCoefficientOps (intervalSpec identityBlock2))
  assertEqual
    "rank-two interval has two source coordinates"
    2
    (sourceCardinality (preparedCosheafBoundaryIncidenceAt (HomologicalDegree 1) chain))
  assertEqual
    "each local coordinate gets its own oriented copy"
    [ (0, 0, -1),
      (0, 2, 1),
      (1, 1, -1),
      (1, 3, 1)
    ]
    (entrySummary <$> boundaryEntries (preparedCosheafBoundaryIncidenceAt (HomologicalDegree 1) chain))

testBadBlockShapeFails :: Assertion
testBadBlockShapeFails =
  case prepareFullLinearCosheafChain intCoefficientOps badShapeSpec of
    Left (LinearCosheafChainBlockShapeMismatch EdgeToV0 EdgeCell Vertex0 1 1 2 2) ->
      pure ()
    Left otherFailure ->
      assertFailure ("unexpected failure: " <> show otherFailure)
    Right _ ->
      assertFailure "expected block shape obstruction"

testNegativeCostalkDimensionFails :: Assertion
testNegativeCostalkDimensionFails =
  case prepareFullLinearCosheafChain intCoefficientOps negativeDimensionSpec of
    Left (LinearCosheafChainSupportFailed (LinearCosheafSupportNegativeCostalkDimension EdgeCell (-1))) ->
      pure ()
    Left otherFailure ->
      assertFailure ("unexpected failure: " <> show otherFailure)
    Right _ ->
      assertFailure "expected negative costalk dimension obstruction"

testSupportPruningSkipsPrunedBlocks :: Assertion
testSupportPruningSkipsPrunedBlocks = do
  supportedChain <-
    expectRight
      ( prepareLinearCosheafChainFromSupportPlan
          intCoefficientOps
          vertexOnlySupportPlan
          prunedBlockSpec
      )
  assertEqual
    "supported C0 basis retains only the requested local coordinate"
    1
    (sourceCardinality (preparedCosheafBoundaryIncidenceAt (HomologicalDegree 0) supportedChain))
  assertEqual
    "supported C1 carrier is empty, so no pruned face block is evaluated"
    0
    (sourceCardinality (preparedCosheafBoundaryIncidenceAt (HomologicalDegree 1) supportedChain))
  case prepareFullLinearCosheafChain intCoefficientOps prunedBlockSpec of
    Left (LinearCosheafChainCorestrictionFailed EdgeToV0 PrunedBlockForced) ->
      pure ()
    Left otherFailure ->
      assertFailure ("unexpected full-preparation failure: " <> show otherFailure)
    Right _ ->
      assertFailure "full preparation should force the sentinel block"

testBoundaryProvenance :: Assertion
testBoundaryProvenance = do
  chain <- expectRight (prepareFullLinearCosheafChain intCoefficientOps (intervalSpec identityBlock1))
  provenance <-
    expectRight
      (preparedCosheafBoundaryEntryProvenance (HomologicalDegree 1) 0 0 chain)
  case toList provenance of
    [witness] -> do
      assertEqual "face survives provenance" EdgeToV0 (cbpFace witness)
      assertEqual "orientation survives provenance" (-1) (cbpOrientation witness)
      assertEqual "final coefficient survives provenance" (-1) (cbpFinalCoefficient witness)
    otherWitnesses ->
      assertFailure ("unexpected provenance cardinality: " <> show (length otherWitnesses))

testCoordinateMorseReduction :: Assertion
testCoordinateMorseReduction = do
  chain <- expectRight (prepareFullLinearCosheafChain intCoefficientOps (intervalSpec identityBlock1))
  reduction <- expectRight (morseReduceCosheafChain intHomologyBackend (defaultCosheafMorsePolicy intUnitPivotOps) chain)
  assertEqual "one coordinate pair is cancelled" 1 (length (cmmPairs (cmrMatching reduction)))
  assertEqual
    "one degree-zero coordinate remains critical"
    [HomologicalDegree 0]
    (cosheafCoordinateDegree <$> cmmCriticalCoordinates (cmrMatching reduction))
  assertEqual
    "the reduced chain is a coordinate complex, not a site-cell complex"
    1
    (sourceCardinality (incidenceMatrixAt (pccChainComplex (cmrReducedChain reduction)) (HomologicalDegree 0)))

testIntegerMorseRejectsNonunitPivot :: Assertion
testIntegerMorseRejectsNonunitPivot = do
  chain <- expectRight (prepareFullLinearCosheafChain intCoefficientOps (intervalSpec doubleBlock1))
  reduction <- expectRight (morseReduceCosheafChain intHomologyBackend (defaultCosheafMorsePolicy intUnitPivotOps) chain)
  assertEqual "nonunit integer pivots are not matched" 0 (length (cmmPairs (cmrMatching reduction)))

testIntegralLinearHomologyDetectsTorsion :: Assertion
testIntegralLinearHomologyDetectsTorsion = do
  chain <- expectRight (prepareFullLinearCosheafChain intCoefficientOps (intervalSpec doubleBlock1))
  artifact <- expectRight (linearCosheafHomology intHomologyBackend chain)
  case IntMap.lookup 0 (lchaGroupsByDegree artifact) of
    Just groupValue -> do
      assertEqual "degree-zero free rank survives the doubled interval" 1 (freeRank groupValue)
      assertEqual "degree-zero torsion is detected by Smith backend" [2] (torsionInvariants groupValue)
    Nothing ->
      assertFailure "expected degree-zero homology group"

testRationalMorseCancelsNonunitPivot :: Assertion
testRationalMorseCancelsNonunitPivot = do
  chain <- expectRight (prepareFullLinearCosheafChain rationalCoefficientOps (intervalSpec rationalDoubleBlock1))
  reduction <- expectRight (morseReduceCosheafChain rationalHomologyBackend (defaultCosheafMorsePolicy rationalPivotOps) chain)
  assertEqual "rational nonzero pivot is matched" 1 (length (cmmPairs (cmrMatching reduction)))

testGF2MorsePreservesBetti :: Assertion
testGF2MorsePreservesBetti = do
  chain <- expectRight (prepareFullLinearCosheafChain gf2CoefficientOps (intervalSpec gf2IdentityBlock1))
  reduction <- expectRight (morseReduceCosheafChain gf2HomologyBackend (defaultCosheafMorsePolicy gf2PivotOps) chain)
  originalRanks <- gf2BettiRanks chain
  reducedRanks <- gf2BettiRanks (cmrReducedChain reduction)
  assertEqual "GF2 pivot is matched" 1 (length (cmmPairs (cmrMatching reduction)))
  assertEqual "GF2 field Betti survives Morse reduction" originalRanks reducedRanks
  assertEqual
    "Morse reduction carries backend agreement"
    originalRanks
    (fmap freeRank (IntMap.elems (cmhaGroupsByDegree (cmrHomologyAgreement reduction))))

testBlockSchurCostalkReduction :: Assertion
testBlockSchurCostalkReduction = do
  chain <- expectRight (prepareFullLinearCosheafChain integerCoefficientOps (intervalSpec identityBlock2Integer))
  reduction <-
    expectRight
      ( blockSchurReduceCosheafChain
          (defaultWholeCostalkBlockSchurPolicy integerUnimodularBlockPivotOps integerCoefficientOps)
          chain
      )
  agreement <- expectRight $ checkBlockSchurHomologyAgreement IntegralSmithBackend (cbmrSchurReduction reduction)
  assertEqual
    "rank-2 costalk block pivots as one matrix"
    2
    (length (bstPivotMatrix (bsrTranscript (cbmrSchurReduction reduction))))
  assertEqual
    "degree-one costalk block is removed"
    0
    (sourceCardinality (preparedCosheafBoundaryIncidenceAt (HomologicalDegree 1) (cbmrReducedChain reduction)))
  assertEqual
    "one rank-2 degree-zero residual block remains"
    2
    (sourceCardinality (preparedCosheafBoundaryIncidenceAt (HomologicalDegree 0) (cbmrReducedChain reduction)))
  assertEqual
    "integral block-Schur homology agreement preserves H0 rank two"
    [2, 0]
    (fmap (freeRank . snd) (bshaGroupsByDegree agreement))

testBlockSchurRejectsNonUnimodularIntegerBlock :: Assertion
testBlockSchurRejectsNonUnimodularIntegerBlock = do
  chain <- expectRight (prepareFullLinearCosheafChain integerCoefficientOps (intervalSpec doubleBlock1Integer))
  let policy = defaultWholeCostalkBlockSchurPolicy integerUnimodularBlockPivotOps integerCoefficientOps
      plan = CosheafBlockPivotPlan (HomologicalDegree 1) EdgeCell (HomologicalDegree 0) Vertex0
  case blockSchurReduceCosheafChainWithPlan policy chain plan of
    Left (CosheafBlockMorseSchurFailed (BlockSchurPivotMatrixFailed (BlockMatrixNonUnimodular [[inverseValue]]))) ->
      assertEqual "orientation-preserving nonunit inverse is rejected over Z" ((-1) % 2) inverseValue
    Left failureValue -> assertFailure ("unexpected typed failure: " <> show failureValue)
    Right _ -> assertFailure "non-unimodular integer block unexpectedly reduced"

entrySummary :: BoundaryEntry Int -> (Int, Int, Int)
entrySummary entryValue =
  (sourceIndex entryValue, targetIndex entryValue, boundaryCoefficient entryValue)

intHomologyBackend :: HomologyBackend Int Integer
intHomologyBackend =
  IntegralSmithBackend

rationalHomologyBackend :: HomologyBackend Rational Rational
rationalHomologyBackend =
  RationalRankBackend

gf2HomologyBackend :: HomologyBackend GF2 GF2
gf2HomologyBackend =
  GF2RankBackend

data IntervalSite = IntervalSite
  deriving stock (Eq, Ord, Show)

data IntervalCell
  = Vertex0
  | Vertex1
  | EdgeCell
  deriving stock (Eq, Ord, Show)

data IntervalFace
  = EdgeToV0
  | EdgeToV1
  deriving stock (Eq, Ord, Show)

data BlockFailure
  = BlockFailure
  | PrunedBlockForced
  deriving stock (Eq, Show)

data IntervalBlock coefficient = IntervalBlock
  { intervalRank :: !Int,
    intervalBlockCoefficient :: !coefficient
  }
  deriving stock (Eq, Show)

identityBlock1 :: IntervalBlock Int
identityBlock1 =
  IntervalBlock
    { intervalRank = 1,
      intervalBlockCoefficient = 1
    }

identityBlock2 :: IntervalBlock Int
identityBlock2 =
  IntervalBlock
    { intervalRank = 2,
      intervalBlockCoefficient = 1
    }

identityBlock2Integer :: IntervalBlock Integer
identityBlock2Integer =
  IntervalBlock
    { intervalRank = 2,
      intervalBlockCoefficient = 1
    }

doubleBlock1 :: IntervalBlock Int
doubleBlock1 =
  IntervalBlock
    { intervalRank = 1,
      intervalBlockCoefficient = 2
    }

doubleBlock1Integer :: IntervalBlock Integer
doubleBlock1Integer =
  IntervalBlock
    { intervalRank = 1,
      intervalBlockCoefficient = 2
    }

rationalDoubleBlock1 :: IntervalBlock Rational
rationalDoubleBlock1 =
  IntervalBlock
    { intervalRank = 1,
      intervalBlockCoefficient = 2
    }

gf2IdentityBlock1 :: IntervalBlock GF2
gf2IdentityBlock1 =
  IntervalBlock
    { intervalRank = 1,
      intervalBlockCoefficient = GF2One
    }

badShapeSpec ::
  LinearCosheafChainSpec
    IntervalSite
    IntervalCell
    IntervalFace
    Int
    (IntervalFace, Int, Int, Int)
    BlockFailure
badShapeSpec =
  (intervalSpec identityBlock2)
    { lccsCostalkDimension = const 1
    }

prunedBlockSpec ::
  LinearCosheafChainSpec
    IntervalSite
    IntervalCell
    IntervalFace
    Int
    (IntervalFace, Int, Int, Int)
    BlockFailure
prunedBlockSpec =
  (intervalSpec identityBlock1)
    { lccsCorestrictionBlock = const (Left PrunedBlockForced)
    }

negativeDimensionSpec ::
  LinearCosheafChainSpec
    IntervalSite
    IntervalCell
    IntervalFace
    Int
    (IntervalFace, Int, Int, Int)
    BlockFailure
negativeDimensionSpec =
  (intervalSpec identityBlock1)
    { lccsCostalkDimension =
        \cell ->
          case cell of
            EdgeCell -> -1
            _ -> 1
    }

vertexOnlySupportPlan :: LinearCosheafSupportPlan IntervalCell IntervalFace
vertexOnlySupportPlan =
  linearCosheafSupportPlanFromLists
    [Vertex0]
    []
    [(Vertex0, 0)]

prepareFullLinearCosheafChain ::
  (Ord cell, Ord face, Eq coefficient, Num coefficient, Semiring coefficient) =>
  CoefficientOps coefficient ->
  LinearCosheafChainSpec site cell face coefficient provenance coreFailure ->
  Either
    (LinearCosheafChainFailure cell face coefficient coreFailure)
    (PreparedCosheafChain site cell coefficient (CosheafBoundaryProvenance cell face coefficient provenance))
prepareFullLinearCosheafChain coefficientOps spec =
  case fullLinearCosheafSupportPlan (lccsSite spec) (lccsBoundaryAlgebra spec) (lccsCostalkDimension spec) of
    Left supportFailure ->
      Left (LinearCosheafChainSupportFailed supportFailure)
    Right supportPlan ->
      prepareLinearCosheafChainFromSupportPlan
        coefficientOps
        supportPlan
        spec

intervalSpec ::
  (Eq coefficient, Num coefficient, Semiring coefficient) =>
  IntervalBlock coefficient ->
  LinearCosheafChainSpec
    IntervalSite
    IntervalCell
    IntervalFace
    coefficient
    (IntervalFace, Int, Int, coefficient)
    BlockFailure
intervalSpec block =
  LinearCosheafChainSpec
    { lccsSite = IntervalSite,
      lccsBoundaryAlgebra = intervalBoundaryAlgebra,
      lccsCostalkDimension = const (intervalRank block),
      lccsCorestrictionBlock = const (Right (blockMatrix block)),
      lccsEntryProvenance = \face sourceLocal targetLocal coefficient ->
        (face, sourceLocal, targetLocal, coefficient)
    }

intervalBoundaryAlgebra :: SiteBoundaryAlgebra IntervalSite IntervalCell IntervalFace
intervalBoundaryAlgebra =
  SiteBoundaryAlgebra
    { sbaDepth = const 1,
      sbaCellsAtDimension = \_site degreeValue ->
        case degreeValue of
          0 -> [Vertex0, Vertex1]
          1 -> [EdgeCell]
          _ -> [],
      sbaFaceMorphisms = const [EdgeToV0, EdgeToV1],
      sbaFaceSource = const EdgeCell,
      sbaFaceTarget = \face ->
        case face of
          EdgeToV0 -> Vertex0
          EdgeToV1 -> Vertex1,
      sbaFaceOrientation = \face ->
        case face of
          EdgeToV0 -> -1
          EdgeToV1 -> 1,
      sbaCellDimension = \cell ->
        case cell of
          Vertex0 -> 0
          Vertex1 -> 0
          EdgeCell -> 1
    }

blockMatrix :: (Eq coefficient, Num coefficient, Semiring coefficient) => IntervalBlock coefficient -> BoundaryIncidence coefficient
blockMatrix block
  | intervalBlockCoefficient block == 1 =
      identityBoundaryIncidenceOf (fromIntegral (intervalRank block))
  | otherwise =
      either
        (const (identityBoundaryIncidenceOf 0))
        id
        ( mkBoundaryIncidenceFromOrderedEntries
            (fromIntegral (intervalRank block))
            (fromIntegral (intervalRank block))
            [ mkBoundaryEntry
                (fromIntegral coordinateIndex)
                (fromIntegral coordinateIndex)
                (intervalBlockCoefficient block)
            | coordinateIndex <- [0 .. intervalRank block - 1]
            ]
        )

nativeBoundaryAlgebra ::
  SiteBoundaryAlgebra
    ChainSite
    ChainObject
    (CheckedMorphism ChainObject ChainMorphism)
nativeBoundaryAlgebra =
  SiteBoundaryAlgebra
    { sbaDepth = const 1,
      sbaCellsAtDimension = \_site degreeValue ->
        case degreeValue of
          0 -> [ChainB]
          1 -> [ChainA]
          _ -> [],
      sbaFaceMorphisms = const [chainAB],
      sbaFaceSource = const ChainA,
      sbaFaceTarget = const ChainB,
      sbaFaceOrientation = const 1,
      sbaCellDimension = \cell ->
        case cell of
          ChainB -> 0
          ChainA -> 1
          _ -> 2
    }

nativeLinearCosheafCostalks :: Map.Map ChainObject [Int]
nativeLinearCosheafCostalks =
  Map.fromList
    [ (ChainA, [0]),
      (ChainB, [0]),
      (ChainC, [0])
    ]

nativeLinearCosheafAlgebra ::
  LinearCosheafAlgebra ChainSite Int BoundaryIncidenceShapeError
nativeLinearCosheafAlgebra =
  LinearCosheafAlgebra
    { lcaCorestrictionMatrix = const (Right (identityBoundaryIncidenceOf 1))
    }

gf2BettiRanks :: PreparedCosheafChain site cell GF2 provenance -> IO [Int]
gf2BettiRanks chain =
  fmap (fmap freeRank . IntMap.elems . lchaGroupsByDegree) $
    expectRight (linearCosheafHomology gf2HomologyBackend chain)

expectRight :: (Show failure) => Either failure value -> IO value
expectRight result =
  case result of
    Right value -> pure value
    Left failureValue -> assertFailure ("unexpected failure: " <> show failureValue)

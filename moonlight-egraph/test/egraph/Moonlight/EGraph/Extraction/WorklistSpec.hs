module Moonlight.EGraph.Extraction.WorklistSpec
  ( tests,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Maybe (maybeToList)
import Moonlight.Core (FixpointDivergence, fixpointBounded)
import Moonlight.EGraph.Pure.Change (EGraphMutationResult (..))
import Moonlight.EGraph.Pure.Extraction
  ( AnalysisCostAlgebra (..),
    ExtractionBudgetExhaustion (..),
    ExtractionClass,
    ExtractionResult (..),
    ExtractionTable,
    ExtractionWorkBudget (..),
    depthCost,
    extractAllFromTable,
    extractAllFromTableBounded,
    extractChoiceSection,
    extractChoiceSectionForClass,
    extractFromChoiceSection,
    extractFromTable,
    extractFromTableBounded,
    extractionCanonicalClass,
    extractionClass,
    extractionClassAnalysis,
    extractionClassNodes,
    extractionClasses,
    extractionChoiceSectionTable,
    extractionTable,
    liftCostAlgebra,
    stableExtractionSnapshotFromEGraph,
    stableExtractionSnapshotTable,
    termSize,
  )
import Moonlight.EGraph.Pure.Kernel.HashCons (insertTermsTracked)
import Moonlight.EGraph.Pure.Rebuild (equateClassesTracked, rebuildTracked)
import Moonlight.EGraph.Pure.Types (ClassId (..), EGraph, ENode (..), classIdKey, emptyEGraph)
import Moonlight.EGraph.Test.Arith.Core
  ( ArithF (..),
    NodeCount (..),
    analysisSpec,
    addTermNode,
    mulTermNode,
    negTermNode,
    numTerm,
  )
import Moonlight.EGraph.Test.Arith.Cost (arithCost)
import Data.Fix (Fix)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck qualified as QC

tests :: TestTree
tests =
  testGroup
    "extraction worklist"
    [ QC.testProperty
        "worklist equals the round-fixpoint oracle under size cost"
        (worklistMatchesOracle (liftCostAlgebra arithCost)),
      QC.testProperty
        "worklist equals the round-fixpoint oracle under depth cost"
        (worklistMatchesOracle (liftCostAlgebra depthCost)),
      QC.testProperty
        "dependency-sliced point extraction agrees with all-class extraction"
        pointExtractionAgreesWithAllClassExtraction,
      QC.testProperty
        "budget exhaustion reports well-formed partial coverage"
        budgetExhaustionReportsCoverage,
      testCase
        "non-superior costs fall back to the authoritative round fixed point"
        testNonSuperiorCostUsesRoundFixedPoint,
      testCase
        "bounded point extraction ignores disconnected classes"
        testBoundedPointExtractionIgnoresDisconnectedClasses,
      testCase
        "large general-cost point descends after the global fast path obstructs"
        testLargeGeneralCostPointDescends
    ]

testNonSuperiorCostUsesRoundFixedPoint :: IO ()
testNonSuperiorCostUsesRoundFixedPoint =
  withExtractionTable nonSuperiorClasses $ \table ->
    fmap erCost (extractFromTable nonSuperiorCost (ClassId 0) table)
      @?= Just 0

testBoundedPointExtractionIgnoresDisconnectedClasses :: IO ()
testBoundedPointExtractionIgnoresDisconnectedClasses =
  withExtractionTable targetOnlyClasses $ \targetOnlyTable ->
    withExtractionTable targetAndDisconnectedClasses $ \targetAndDisconnectedTable -> do
      let extractTarget =
            fmap (fmap erCost)
              . extractFromTableBounded
                (ExtractionWorkBudget 1)
                nonSuperiorCost
                (ClassId 0)
      extractTarget targetOnlyTable @?= Right (Just 100)
      extractTarget targetAndDisconnectedTable @?= Right (Just 100)

testLargeGeneralCostPointDescends :: IO ()
testLargeGeneralCostPointDescends =
  withExtractionTable largeGeneralPointClasses $ \table ->
    case extractChoiceSectionForClass nonSuperiorCost (ClassId 0) table of
      Nothing ->
        assertFailure "expected a point choice section"
      Just section -> do
        IntMap.size (extractionClasses (extractionChoiceSectionTable section)) @?= 66
        fmap erCost (extractFromChoiceSection (ClassId 0) section) @?= Just 0

nonSuperiorCost :: AnalysisCostAlgebra ArithF NodeCount Int
nonSuperiorCost =
  AnalysisCostAlgebra $ \_ arithNode ->
    case arithNode of
      Num value -> value
      Var value -> value
      Add _ _ -> 100
      Mul _ _ -> 100
      Neg _ -> 0

nonSuperiorClasses :: IntMap (ExtractionClass ArithF NodeCount)
nonSuperiorClasses =
  IntMap.fromList
    [ (0, extractionClass (NodeCount 1) [ENode (Num 10), ENode (Neg (ClassId 1))]),
      (1, extractionClass (NodeCount 1) [ENode (Num 20)])
    ]

targetOnlyClasses :: IntMap (ExtractionClass ArithF NodeCount)
targetOnlyClasses =
  IntMap.singleton 0 (extractionClass (NodeCount 1) [ENode (Num 100)])

targetAndDisconnectedClasses :: IntMap (ExtractionClass ArithF NodeCount)
targetAndDisconnectedClasses =
  IntMap.insert 1 (extractionClass (NodeCount 1) [ENode (Num 0)]) targetOnlyClasses

largeGeneralPointClasses :: IntMap (ExtractionClass ArithF NodeCount)
largeGeneralPointClasses =
  IntMap.insert 100 (extractionClass (NodeCount 1) [ENode (Num 1)]) $
    IntMap.fromAscList
      ( [ (classKey, extractionClass (NodeCount 1) [ENode (Neg (ClassId (classKey + 1)))])
          | classKey <- [0 .. 64]
        ]
          <> [(65, extractionClass (NodeCount 1) [ENode (Num 10)])]
      )

withExtractionTable ::
  IntMap (ExtractionClass ArithF NodeCount) ->
  (ExtractionTable ArithF NodeCount -> IO ()) ->
  IO ()
withExtractionTable classes action =
  maybe
    (assertFailure "expected valid extraction table")
    action
    ( extractionTable
        classes
        (\classId -> if IntMap.member (classIdKey classId) classes then Just classId else Nothing)
    )

data OracleChoice = OracleChoice
  { ocCost :: !Int,
    ocSize :: !Int,
    ocNode :: !(ENode ArithF)
  }
  deriving stock (Eq, Show)

oracleChoiceKey :: OracleChoice -> (Int, Int, ENode ArithF)
oracleChoiceKey oracleChoice =
  (ocCost oracleChoice, ocSize oracleChoice, ocNode oracleChoice)

chooseBetterOracle :: OracleChoice -> Maybe OracleChoice -> Maybe OracleChoice
chooseBetterOracle candidate maybeCurrent =
  Just
    ( maybe
        candidate
        (\current -> if oracleChoiceKey candidate < oracleChoiceKey current then candidate else current)
        maybeCurrent
    )

-- Independent round-improvement oracle. It deliberately shares neither the
-- production choice type nor its candidate/selection helpers.
oracleChoices ::
  AnalysisCostAlgebra ArithF NodeCount Int ->
  ExtractionTable ArithF NodeCount ->
  Either
    (FixpointDivergence (IntMap (Maybe OracleChoice)))
    (IntMap (Maybe OracleChoice))
oracleChoices costAlgebraValue table =
  fixpointBounded 4096 improve (Nothing <$ extractionClasses table)
  where
    improve currentChoices =
      IntMap.mapWithKey
        ( \classKey extractionClassValue ->
            foldr
              chooseBetterOracle
              (IntMap.findWithDefault Nothing classKey currentChoices)
              (candidateOracleChoices currentChoices extractionClassValue)
        )
        (extractionClasses table)

    candidateOracleChoices currentChoices extractionClassValue =
      extractionClassNodes extractionClassValue
        >>= maybeToList
          . oracleNodeChoice
            currentChoices
            (extractionClassAnalysis extractionClassValue)

    oracleNodeChoice currentChoices classAnalysis nodeValue@(ENode childClassIds) = do
      childChoices <- traverse (lookupChildChoice currentChoices) childClassIds
      let childCosts = fmap (\(childAnalysis, childChoice) -> (childAnalysis, ocCost childChoice)) childChoices
      pure
        OracleChoice
          { ocCost = analysisCostAlgebra costAlgebraValue classAnalysis childCosts,
            ocSize = 1 + sum (fmap (ocSize . snd) childChoices),
            ocNode = nodeValue
          }

    lookupChildChoice currentChoices childClassId = do
      canonicalChildId <- extractionCanonicalClass table childClassId
      childClass <- IntMap.lookup (classIdKey canonicalChildId) (extractionClasses table)
      childChoice <- IntMap.findWithDefault Nothing (classIdKey canonicalChildId) currentChoices
      pure (extractionClassAnalysis childClass, childChoice)

genArithTerm :: Int -> QC.Gen (Fix ArithF)
genArithTerm depth
  | depth <= 0 =
      numTerm <$> QC.choose (0, 4)
  | otherwise =
      QC.oneof
        [ numTerm <$> QC.choose (0, 4),
          addTermNode <$> subTerm <*> subTerm,
          mulTermNode <$> subTerm <*> subTerm,
          negTermNode <$> subTerm
        ]
  where
    subTerm =
      genArithTerm (depth - 1)

genStableGraph :: QC.Gen (EGraph ArithF NodeCount, [ClassId])
genStableGraph = do
  termCount <- QC.choose (1, 8 :: Int)
  terms <- QC.vectorOf termCount (genArithTerm 3)
  insertion <-
    either
      (const QC.discard)
      pure
      (insertTermsTracked terms (emptyEGraph analysisSpec))
  let insertedClasses = reverse (emrResult insertion)
      insertedGraph = emrGraph insertion
  equateCount <- QC.choose (0, 3 :: Int)
  equatePairs <-
    QC.vectorOf
      equateCount
      ( (,)
          <$> QC.elements insertedClasses
          <*> QC.elements insertedClasses
      )
  let dirtyGraph =
        foldl'
          ( \graphSoFar (leftClass, rightClass) ->
              emrGraph (equateClassesTracked leftClass rightClass graphSoFar)
          )
          insertedGraph
          equatePairs
      stableGraph =
        emrGraph (rebuildTracked dirtyGraph)
  pure (stableGraph, insertedClasses)

genStableTable :: QC.Gen (ExtractionTable ArithF NodeCount, [ClassId])
genStableTable = do
  (graph, insertedClasses) <- genStableGraph
  case stableExtractionSnapshotFromEGraph graph of
    Nothing ->
      QC.discard
    Just snapshot ->
      pure (stableExtractionSnapshotTable snapshot, insertedClasses)

worklistMatchesOracle :: AnalysisCostAlgebra ArithF NodeCount Int -> QC.Property
worklistMatchesOracle costAlgebraValue =
  QC.forAllBlind genStableTable $ \(table, _classes) ->
    case oracleChoices costAlgebraValue table of
      Left _divergence ->
        QC.counterexample
          "bounded oracle diverged"
          (QC.property False)
      Right oracleChoiceMap ->
        fmap (\resultValue -> (erCost resultValue, termSize (erTerm resultValue))) (extractAllFromTable costAlgebraValue table)
          QC.=== IntMap.mapMaybe (fmap (\choice -> (ocCost choice, ocSize choice))) oracleChoiceMap

pointExtractionAgreesWithAllClassExtraction :: QC.Property
pointExtractionAgreesWithAllClassExtraction =
  QC.forAllBlind genStableTable $ \(table, insertedClasses) ->
    not (null insertedClasses) QC.==>
      QC.forAll (QC.elements insertedClasses) $ \demandedClass ->
        let costAlgebraValue =
              liftCostAlgebra arithCost
            resultView resultValue =
              (erTerm resultValue, erCost resultValue, erClass resultValue)
            pointResult =
              extractFromTable costAlgebraValue demandedClass table
            allClassResult =
              extractFromChoiceSection
                demandedClass
                (extractChoiceSection costAlgebraValue table)
         in fmap resultView pointResult QC.=== fmap resultView allClassResult

budgetExhaustionReportsCoverage :: QC.Property
budgetExhaustionReportsCoverage =
  QC.forAllBlind genStableTable $ \(table, _classes) ->
    let totalClassCount =
          IntMap.size (extractionClasses table)
        result =
          extractAllFromTableBounded
            (ExtractionWorkBudget 1)
            (liftCostAlgebra arithCost)
            table
     in totalClassCount > 1 QC.==>
          case result of
            Left report ->
              QC.conjoin
                [ QC.counterexample
                    "resolved + unresolved must cover the table"
                    (ebeResolvedClassCount report + ebeUnresolvedClassCount report QC.=== ebeTotalClassCount report),
                  QC.counterexample
                    "a budget of one admits exactly one finalization"
                    (ebeResolvedClassCount report QC.=== 1),
                  QC.counterexample
                    "the report records the exact consumed work"
                    (ebeConsumedWorkSteps report QC.=== 1)
                ]
            Right _choices ->
              QC.property False

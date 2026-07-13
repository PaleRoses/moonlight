module Moonlight.EGraph.Extraction.WorklistSpec
  ( tests,
  )
where

import Control.Monad (foldM)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Foldable (foldl')
import Moonlight.Core (FixpointDivergence, fixpointBounded)
import Moonlight.EGraph.Pure.Change (EGraphMutationResult (..))
import Moonlight.EGraph.Pure.Extraction
  ( depthCost,
    liftCostAlgebra,
    stableExtractionSnapshotFromEGraph,
    stableExtractionSnapshotTable,
  )
import Moonlight.EGraph.Pure.Extraction.Algebra
  ( ExtractionChoiceSection (..),
    extractChoices,
    extractFromChoiceSection,
  )
import Moonlight.EGraph.Pure.Extraction.Core
  ( AnalysisCostAlgebra,
    BestChoice,
    ExtractionConvergenceReport (..),
    ExtractionFixpointBudget (..),
    ExtractionResult (..),
    ExtractionTable,
    extractionClasses,
  )
import Moonlight.EGraph.Pure.Extraction.Worklist
  ( WorklistSeed (..),
    candidateChoices,
    choiceKey,
    chooseBetterChoice,
    worklistChoices,
  )
import Moonlight.EGraph.Pure.Kernel.HashCons (addTerm)
import Moonlight.EGraph.Pure.Rebuild (equateClassesTracked, rebuildTracked)
import Moonlight.EGraph.Pure.Types (ClassId (..), EGraph, ENode (..), classIdKey, emptyEGraph)
import Moonlight.EGraph.Test.Arith.Core
  ( ArithF,
    NodeCount,
    analysisSpec,
    addTermNode,
    mulTermNode,
    negTermNode,
    numTerm,
  )
import Moonlight.EGraph.Test.Arith.Cost (arithCost)
import Data.Fix (Fix)
import Test.Tasty (TestTree, testGroup)
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
        "demand-sliced worklist answers demanded point queries exactly like the full run"
        demandSliceAgreesOnDemandedClasses,
      QC.testProperty
        "budget exhaustion reports well-formed partial coverage"
        budgetExhaustionReportsCoverage
    ]

-- The deleted round-fixpoint engine, hosted verbatim as the specification
-- oracle: repeated global improvement of every class until stable.
oracleChoices ::
  AnalysisCostAlgebra ArithF NodeCount Int ->
  ExtractionTable ArithF NodeCount ->
  Either
    (FixpointDivergence (IntMap (Maybe (BestChoice ArithF Int))))
    (IntMap (Maybe (BestChoice ArithF Int)))
oracleChoices costAlgebraValue table =
  fixpointBounded 4096 improve (Nothing <$ extractionClasses table)
  where
    improve currentChoices =
      IntMap.mapWithKey
        ( \classKey _ ->
            improveClass
              currentChoices
              (ClassId classKey)
              (IntMap.findWithDefault Nothing classKey currentChoices)
        )
        (extractionClasses table)

    improveClass currentChoices classId currentBestChoice =
      foldr
        chooseBetterChoice
        currentBestChoice
        (candidateChoices costAlgebraValue table currentChoices classId)

keyedChoices ::
  IntMap (Maybe (BestChoice ArithF Int)) ->
  IntMap (Maybe (Int, Int, ENode ArithF))
keyedChoices =
  fmap (fmap choiceKey)

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
  (insertedClasses, insertedGraph) <-
    either
      (const QC.discard)
      pure
      ( foldM
          ( \(classesSoFar, graphSoFar) term ->
              fmap
                (\(classId, nextGraph) -> (classId : classesSoFar, nextGraph))
                (addTerm term graphSoFar)
          )
          ([], emptyEGraph analysisSpec)
          terms
      )
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
        emrGraph (rebuildTracked Nothing dirtyGraph)
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
        keyedChoices (extractChoices costAlgebraValue table)
          QC.=== keyedChoices oracleChoiceMap

-- The demand contract: early stopping may leave classes outside the demanded
-- chosen cone unfinalized, so whole-map agreement is deliberately NOT promised.
-- What is promised: the demanded class's finalized choice and its reconstructed
-- extraction result coincide with the full run's.
demandSliceAgreesOnDemandedClasses :: QC.Property
demandSliceAgreesOnDemandedClasses =
  QC.forAllBlind genStableTable $ \(table, insertedClasses) ->
    not (null insertedClasses) QC.==>
      QC.forAll (QC.elements insertedClasses) $ \demandedClass ->
        let costAlgebraValue =
              liftCostAlgebra arithCost
            demanded =
              IntSet.singleton (classIdKey demandedClass)
            sliced =
              worklistChoices
                (ExtractionFixpointBudget 4096)
                costAlgebraValue
                table
                IntMap.empty
                (SeedClasses demanded)
            fullChoices =
              extractChoices costAlgebraValue table
            pointQuery choices =
              extractFromChoiceSection
                demandedClass
                ExtractionChoiceSection {ecsTable = table, ecsChoices = choices}
            resultView resultValue =
              (erTerm resultValue, erCost resultValue, erClass resultValue)
         in case sliced of
              Left _report ->
                QC.property False
              Right slicedChoices ->
                QC.conjoin
                  [ QC.counterexample
                      "demanded class choice must match the full run"
                      ( IntMap.lookup (classIdKey demandedClass) (keyedChoices slicedChoices)
                          QC.=== IntMap.lookup (classIdKey demandedClass) (keyedChoices fullChoices)
                      ),
                    QC.counterexample
                      "demanded point query must reconstruct the same extraction result"
                      ( fmap resultView (pointQuery slicedChoices)
                          QC.=== fmap resultView (pointQuery fullChoices)
                      )
                  ]

budgetExhaustionReportsCoverage :: QC.Property
budgetExhaustionReportsCoverage =
  QC.forAllBlind genStableTable $ \(table, _classes) ->
    let totalClassCount =
          IntMap.size (extractionClasses table)
        result =
          worklistChoices
            (ExtractionFixpointBudget 1)
            (liftCostAlgebra arithCost)
            table
            IntMap.empty
            SeedAllClasses
     in totalClassCount > 1 QC.==>
          case result of
            Left report ->
              QC.conjoin
                [ QC.counterexample
                    "resolved + unresolved must cover the table"
                    (ecrResolvedClassCount report + ecrUnresolvedClassCount report QC.=== ecrTotalClassCount report),
                  QC.counterexample
                    "a budget of one admits exactly one finalization"
                    (ecrResolvedClassCount report QC.=== 1)
                ]
            Right _choices ->
              QC.property False

module Moonlight.EGraph.Extraction.IncrementalExtractionSpec
  ( tests,
  )
where

import Control.Monad (foldM)
import Data.IntMap.Strict (IntMap)
import Moonlight.Core (UnionFindAllocationError)
import Moonlight.EGraph.Pure.Change
  ( EGraphMutationResult (..),
    EGraphMutationTrace,
    appendEGraphMutationTrace,
  )
import Moonlight.EGraph.Pure.Extraction
  ( AnalysisCostAlgebra,
    ExtractionChoiceCache,
    ExtractionWorkBudget (..),
    ExtractionResult (..),
    extractAllCached,
    extractionChoiceCacheFromStableGraph,
    advanceExtractionChoiceCache,
    liftCostAlgebra,
  )
import Moonlight.EGraph.Pure.Kernel.HashCons (addTerm, insertTermTracked)
import Moonlight.EGraph.Pure.Rebuild (equateClassesTracked, rebuildTracked)
import Moonlight.EGraph.Pure.Types (ClassId, EGraph, emptyEGraph)
import Moonlight.EGraph.Test.Arith.Core
  ( ArithF,
    NodeCount,
    addTermNode,
    analysisSpec,
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
    "incremental extraction"
    [ QC.testProperty
        "advanced cache is observationally the freshly built cache"
        (advanceMatchesFreshBuild balancedMergeBias),
      QC.testProperty
        "union-heavy edit scripts preserve the maintained-view law through id transport"
        (advanceMatchesFreshBuild unionHeavyMergeBias)
    ]

data EditOp
  = InsertOp (Fix ArithF)
  | MergeOp Int Int
  deriving stock (Show)

balancedMergeBias :: Int
balancedMergeBias = 1

unionHeavyMergeBias :: Int
unionHeavyMergeBias = 4

cacheBudget :: ExtractionWorkBudget
cacheBudget =
  ExtractionWorkBudget 4096

costAlgebraValue :: AnalysisCostAlgebra ArithF NodeCount Int
costAlgebraValue =
  liftCostAlgebra arithCost

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

genStableStart :: QC.Gen (Either UnionFindAllocationError (EGraph ArithF NodeCount, [ClassId]))
genStableStart = do
  termCount <- QC.choose (1, 5 :: Int)
  terms <- QC.vectorOf termCount (genArithTerm 3)
  pure $
    fmap
      (\(graph, classes) -> (emrGraph (rebuildTracked graph), classes))
      ( foldM
          ( \(graphSoFar, classesSoFar) term ->
              fmap
                (\(classId, nextGraph) -> (nextGraph, classId : classesSoFar))
                (addTerm term graphSoFar)
          )
          (emptyEGraph analysisSpec, [])
          terms
      )

genEditOps :: Int -> QC.Gen [EditOp]
genEditOps mergeBias = do
  opCount <- QC.choose (1, 6 :: Int)
  QC.vectorOf
    opCount
    ( QC.frequency
        [ (1, InsertOp <$> genArithTerm 3),
          (mergeBias, MergeOp <$> QC.choose (0, 16) <*> QC.choose (0, 16))
        ]
    )

applyEditOp ::
  EditOp ->
  (EGraph ArithF NodeCount, [ClassId]) ->
  Either EditApplicationError (EGraphMutationTrace ArithF, EGraph ArithF NodeCount, [ClassId])
applyEditOp op (graph, classes) =
  case op of
    InsertOp term ->
      fmap
        ( \EGraphMutationResult
            { emrResult = classId,
              emrTrace = insertTrace,
              emrGraph = insertedGraph
            } -> (insertTrace, insertedGraph, classId : classes)
        )
        (mapLeft EditAllocationFailed (insertTermTracked term graph))
    MergeOp leftIndex rightIndex -> do
      leftClass <- classAtCyclicIndex leftIndex classes
      rightClass <- classAtCyclicIndex rightIndex classes
      let EGraphMutationResult
            { emrTrace = mergeTrace,
              emrGraph = dirtyGraph
            } =
              equateClassesTracked leftClass rightClass graph
          EGraphMutationResult
            { emrTrace = rebuildTrace,
              emrGraph = rebuiltGraph
            } =
              rebuildTracked dirtyGraph
      pure (appendEGraphMutationTrace mergeTrace rebuildTrace, rebuiltGraph, classes)

data EditApplicationError
  = EditAllocationFailed UnionFindAllocationError
  | EditClassSelectionEmpty
  deriving stock (Eq, Show)

classAtCyclicIndex :: Int -> [ClassId] -> Either EditApplicationError ClassId
classAtCyclicIndex _ [] =
  Left EditClassSelectionEmpty
classAtCyclicIndex indexValue classes =
  case drop (indexValue `mod` length classes) classes of
    classIdValue : _ -> Right classIdValue
    [] -> Left EditClassSelectionEmpty

mapLeft :: (left -> mappedLeft) -> Either left right -> Either mappedLeft right
mapLeft mapError =
  either (Left . mapError) Right

resultsView ::
  ExtractionChoiceCache ArithF NodeCount Int ->
  IntMap (Fix ArithF, Int, ClassId)
resultsView =
  fmap (\resultValue -> (erTerm resultValue, erCost resultValue, erClass resultValue))
    . extractAllCached

advanceMatchesFreshBuild :: Int -> QC.Property
advanceMatchesFreshBuild mergeBias =
  QC.forAllBlind genStableStart $ \startResult ->
    case startResult of
      Left allocationError ->
        QC.counterexample ("initial graph allocation failed: " <> show allocationError) False
      Right (graph0, classes0) ->
        QC.forAll (genEditOps mergeBias) $ \ops ->
          case extractionChoiceCacheFromStableGraph cacheBudget costAlgebraValue graph0 of
            Left obstruction ->
              QC.counterexample ("initial cache build obstructed: " <> show obstruction) False
            Right cache0 ->
              runScript cache0 graph0 classes0 ops

runScript ::
  ExtractionChoiceCache ArithF NodeCount Int ->
  EGraph ArithF NodeCount ->
  [ClassId] ->
  [EditOp] ->
  QC.Property
runScript _cacheValue _graph _classes [] =
  QC.property True
runScript cacheValue graph classes (op : remainingOps) =
  case applyEditOp op (graph, classes) of
    Left editError ->
      QC.counterexample ("edit application failed after " <> show op <> ": " <> show editError) False
    Right (stepTrace, nextGraph, nextClasses) ->
      let advanced =
            advanceExtractionChoiceCache cacheBudget stepTrace nextGraph cacheValue
          fresh =
            extractionChoiceCacheFromStableGraph cacheBudget costAlgebraValue nextGraph
       in case (advanced, fresh) of
        (Right advancedCache, Right freshCache) ->
          QC.conjoin
            [ QC.counterexample
                ("advanced extractions diverge from fresh build after " <> show op)
                (resultsView advancedCache QC.=== resultsView freshCache),
              runScript advancedCache nextGraph nextClasses remainingOps
            ]
        (Left obstruction, _) ->
          QC.counterexample ("cache advance obstructed after " <> show op <> ": " <> show obstruction) False
        (_, Left obstruction) ->
          QC.counterexample ("fresh build obstructed after " <> show op <> ": " <> show obstruction) False

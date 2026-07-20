module Moonlight.Analysis.Mesh.ScalarSpec
  ( tests
  ) where

import Control.Monad.ST (ST, runST)
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import Moonlight.Analysis.Mesh.Graph (Graph(..))
import Moonlight.Analysis.Mesh.Multigrid (MGHierarchy(..), MGTransfer(..))
import Moonlight.Analysis.Mesh.Scalar
  ( KrylovReport(..)
  , KrylovTuning(..)
  , MGArena
  , MGArenaBuffer(..)
  , MGArenaLevelShape(..)
  , MGArenaShapeObstruction(..)
  , ScalarLevelOp(..)
  , buildMGOperator
  , buildScalarLevelOp
  , newMGArenaFromOp
  , retargetMGArenaFromOp
  , solveScalarMGPCG
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Prelude


tests :: TestTree
tests =
  testGroup
    "mesh-scalar"
    [ testCase "scalar level construction preserves pair and row order" scalarLevelConstructionSpec
    , testCase "fused state update preserves exact three-pass Float result" fusedStateUpdateExactnessSpec
    , testCase "one bound multigrid arena gives exact repeated solves" repeatedArenaSolveSpec
    , testCase "retargeted arena binds the replacement operator exactly" retargetedArenaSolveSpec
    , testCase "retargeting reports hierarchy mismatch as typed data" retargetHierarchyMismatchSpec
    , testCase "retargeting reports row mismatch as typed data" retargetRowMismatchSpec
    ]


fusedStateUpdateExactnessSpec :: IO ()
fusedStateUpdateExactnessSpec =
  -- Frozen from the former two axpy passes followed by normVec: ascending
  -- Double square accumulation, narrowed to Float before the Float sqrt.
  runST fusedStateUpdateSolve
    @?= ( KrylovReport
            { krInitialResidual = 3.6293077
            , krFinalResidual = 2.0673652
            , krIterations = 1
            , krConverged = False
            }
        , VU.fromList [0.5183276, -1.4302987, 1.5396543]
        , VU.fromList [0.6020905, 7.5746775e-2, 1.9762964]
        )


fusedStateUpdateSolve :: ST s (KrylovReport, VU.Vector Float, VU.Vector Float)
fusedStateUpdateSolve = do
  systemOperator <- VU.thaw VU.empty >>= buildScalarLevelOp isolatedSystemGraph
  preconditionerOperator <- VU.thaw VU.empty >>= buildScalarLevelOp isolatedPreconditionerGraph
  let multigridOperator = buildMGOperator isolatedPreconditionerHierarchy preconditionerOperator
  multigridArena <- newMGArenaFromOp multigridOperator
  solution <- VU.thaw (VU.fromList [0.5, -0.25, 1.5])
  rightHandSide <- VU.thaw (VU.fromList [1.25, -3.5, 7.75])
  residual <- VUM.replicate 3 0.0
  direction <- VUM.replicate 3 0.0
  preconditioned <- VUM.replicate 3 0.0
  operatorDirection <- VUM.replicate 3 0.0
  report <-
    solveScalarMGPCG
      fusedStateUpdateTuning
      systemOperator
      multigridArena
      1.0
      0.25
      0.5
      solution
      rightHandSide
      residual
      direction
      preconditioned
      operatorDirection
  (,,) report <$> VU.freeze solution <*> VU.freeze residual


fusedStateUpdateTuning :: KrylovTuning
fusedStateUpdateTuning =
  krylovTuning
    { ktMaxIterations = 1
    , ktAbsTolerance = 0.0
    , ktRelTolerance = 0.0
    }


isolatedPreconditionerHierarchy :: MGHierarchy
isolatedPreconditionerHierarchy =
  MGHierarchy
    { mghGraph = isolatedPreconditionerGraph
    , mghMass = grFaceArea isolatedPreconditionerGraph
    , mghTransfer = Nothing
    , mghCoarse = Nothing
    }


isolatedSystemGraph :: Graph
isolatedSystemGraph = isolatedThreeFaceGraph (VU.fromList [1.0, 2.0, 3.0])


isolatedPreconditionerGraph :: Graph
isolatedPreconditionerGraph = isolatedThreeFaceGraph (VU.fromList [7.0, 0.5, 11.0])


isolatedThreeFaceGraph :: VU.Vector Double -> Graph
isolatedThreeFaceGraph faceAreas =
  oneFaceGraph
    { grFaces = 3
    , grOffsets = VU.replicate 4 0
    , grFaceArea = faceAreas
    , grFaceOutDeg = VU.replicate 3 0
    , grNewToOld = VU.enumFromN 0 3
    , grOldToNew = VU.enumFromN 0 3
    }


scalarLevelConstructionSpec :: IO ()
scalarLevelConstructionSpec = do
  let levelOperator = runST (VU.thaw (VU.singleton 2.0) >>= buildScalarLevelOp twoFaceGraph)
  sloMass levelOperator @?= VU.fromList [1.0, 2.0]
  sloPairW levelOperator @?= VU.singleton 3.0
  sloEdgeW levelOperator @?= VU.fromList [3.0, 3.0]
  sloRowSum levelOperator @?= VU.fromList [3.0, 3.0]


repeatedArenaSolveSpec :: IO ()
repeatedArenaSolveSpec =
  let (firstSolve, secondSolve) = runST repeatedSolves
  in firstSolve @?= secondSolve


repeatedSolves :: ST s ((KrylovReport, VU.Vector Float), (KrylovReport, VU.Vector Float))
repeatedSolves = do
  pairWeights <- VU.thaw (VU.singleton 2.0)
  fineOperator <- buildScalarLevelOp twoFaceGraph pairWeights
  let multigridOperator = buildMGOperator coarsestHierarchy fineOperator
  multigridArena <- newMGArenaFromOp multigridOperator
  firstSolve <- solveWithArena fineOperator multigridArena
  secondSolve <- solveWithArena fineOperator multigridArena
  pure (firstSolve, secondSolve)


solveWithArena
  :: ScalarLevelOp
  -> MGArena s
  -> ST s (KrylovReport, VU.Vector Float)
solveWithArena fineOperator multigridArena = do
  solution <- VU.thaw (VU.replicate 2 0.0)
  rightHandSide <- VU.thaw (VU.fromList [1.0, 3.0])
  residual <- VUM.replicate 2 0.0
  direction <- VUM.replicate 2 0.0
  preconditioned <- VUM.replicate 2 0.0
  operatorDirection <- VUM.replicate 2 0.0
  report <-
    solveScalarMGPCG
      krylovTuning
      fineOperator
      multigridArena
      1.0
      0.25
      0.5
      solution
      rightHandSide
      residual
      direction
      preconditioned
      operatorDirection
  frozenSolution <- VU.freeze solution
  pure (report, frozenSolution)


retargetedArenaSolveSpec :: IO ()
retargetedArenaSolveSpec =
  case runST retargetedAndFreshSolves of
    Left obstruction -> assertFailure ("unexpected retarget obstruction: " <> show obstruction)
    Right (retargetedSolve, freshSolve) -> retargetedSolve @?= freshSolve


retargetedAndFreshSolves
  :: ST s
       ( Either
           MGArenaShapeObstruction
           ( (KrylovReport, VU.Vector Float)
           , (KrylovReport, VU.Vector Float)
           )
       )
retargetedAndFreshSolves = do
  initialWeights <- VU.thaw (VU.singleton 2.0)
  initialFineOperator <- buildScalarLevelOp twoFaceGraph initialWeights
  let initialMultigridOperator = buildMGOperator coarsestHierarchy initialFineOperator
  initialArena <- newMGArenaFromOp initialMultigridOperator
  replacementWeights <- VU.thaw (VU.singleton 3.0)
  replacementFineOperator <- buildScalarLevelOp twoFaceGraph replacementWeights
  let replacementMultigridOperator = buildMGOperator coarsestHierarchy replacementFineOperator
  case retargetMGArenaFromOp replacementMultigridOperator initialArena of
    Left obstruction -> pure (Left obstruction)
    Right retargetedArena -> do
      freshArena <- newMGArenaFromOp replacementMultigridOperator
      retargetedSolve <- solveWithArena replacementFineOperator retargetedArena
      freshSolve <- solveWithArena replacementFineOperator freshArena
      pure (Right (retargetedSolve, freshSolve))


retargetHierarchyMismatchSpec :: IO ()
retargetHierarchyMismatchSpec =
  runST hierarchyMismatchObstruction
    @?= Just
      ( MGArenaHierarchyMismatch
          0
          MGArenaBranchLevel
          MGArenaCoarsestLevel
      )


hierarchyMismatchObstruction :: ST s (Maybe MGArenaShapeObstruction)
hierarchyMismatchObstruction = do
  pairWeights <- VU.thaw (VU.singleton 2.0)
  fineOperator <- buildScalarLevelOp twoFaceGraph pairWeights
  coarsestArena <- newMGArenaFromOp (buildMGOperator coarsestHierarchy fineOperator)
  let branchOperator = buildMGOperator branchHierarchy fineOperator
  pure $
    case retargetMGArenaFromOp branchOperator coarsestArena of
      Left obstruction -> Just obstruction
      Right _ -> Nothing


retargetRowMismatchSpec :: IO ()
retargetRowMismatchSpec =
  runST rowMismatchObstruction
    @?= Just
      ( MGArenaBufferRowsMismatch
          0
          MGArenaX
          1
          2
      )


rowMismatchObstruction :: ST s (Maybe MGArenaShapeObstruction)
rowMismatchObstruction = do
  twoFaceWeights <- VU.thaw (VU.singleton 2.0)
  twoFaceOperator <- buildScalarLevelOp twoFaceGraph twoFaceWeights
  twoFaceArena <- newMGArenaFromOp (buildMGOperator coarsestHierarchy twoFaceOperator)
  oneFaceWeights <- VU.thaw VU.empty
  oneFaceOperator <- buildScalarLevelOp oneFaceGraph oneFaceWeights
  let oneFaceMultigridOperator = buildMGOperator oneFaceHierarchy oneFaceOperator
  pure $
    case retargetMGArenaFromOp oneFaceMultigridOperator twoFaceArena of
      Left obstruction -> Just obstruction
      Right _ -> Nothing


coarsestHierarchy :: MGHierarchy
coarsestHierarchy =
  MGHierarchy
    { mghGraph = twoFaceGraph
    , mghMass = VU.fromList [1.0, 2.0]
    , mghTransfer = Nothing
    , mghCoarse = Nothing
    }


branchHierarchy :: MGHierarchy
branchHierarchy =
  MGHierarchy
    { mghGraph = twoFaceGraph
    , mghMass = VU.fromList [1.0, 2.0]
    , mghTransfer = Just twoToOneTransfer
    , mghCoarse = Just oneFaceHierarchy
    }


oneFaceHierarchy :: MGHierarchy
oneFaceHierarchy =
  MGHierarchy
    { mghGraph = oneFaceGraph
    , mghMass = VU.singleton 3.0
    , mghTransfer = Nothing
    , mghCoarse = Nothing
    }


twoToOneTransfer :: MGTransfer
twoToOneTransfer =
  MGTransfer
    { mgtFineRows = 2
    , mgtCoarseRows = 1
    , mgtFineToCoarse = VU.fromList [0, 0]
    , mgtFineRestrictW = VU.fromList [1.0 / 3.0, 2.0 / 3.0]
    , mgtCoarseOffsets = VU.fromList [0, 2]
    , mgtCoarseFaces = VU.fromList [0, 1]
    , mgtFinePairToCoarsePair = VU.singleton (-1)
    }


oneFaceGraph :: Graph
oneFaceGraph =
  twoFaceGraph
    { grFaces = 1
    , grOffsets = VU.fromList [0, 0]
    , grNbrs = VU.empty
    , grEdgePair = VU.empty
    , grPairA = VU.empty
    , grPairB = VU.empty
    , grPairHasAB = VU.empty
    , grPairHasBA = VU.empty
    , grPairBaseW = VU.empty
    , grFaceArea = VU.singleton 3.0
    , grPairEdgeLen = VU.empty
    , grPairCenterDist = VU.empty
    , grPairNx = VU.empty
    , grPairNy = VU.empty
    , grPairMetric11 = VU.empty
    , grPairMetric12 = VU.empty
    , grPairMetric22 = VU.empty
    , grFaceOutDeg = VU.singleton 0
    , grNewToOld = VU.singleton 0
    , grOldToNew = VU.singleton 0
    }


krylovTuning :: KrylovTuning
krylovTuning =
  KrylovTuning
    { ktMaxIterations = 32
    , ktRestart = 32
    , ktAbsTolerance = 1.0e-7
    , ktRelTolerance = 1.0e-7
    , ktBreakdownEps = 1.0e-12
    }


twoFaceGraph :: Graph
twoFaceGraph =
  Graph
    { grFaces = 2
    , grOffsets = VU.fromList [0, 1, 2]
    , grNbrs = VU.fromList [1, 0]
    , grEdgePair = VU.fromList [0, 0]
    , grPairA = VU.singleton 0
    , grPairB = VU.singleton 1
    , grPairHasAB = VU.singleton 1
    , grPairHasBA = VU.singleton 1
    , grPairBaseW = VU.singleton 0.5
    , grFaceArea = VU.fromList [1.0, 2.0]
    , grPairEdgeLen = VU.singleton 2.0
    , grPairCenterDist = VU.singleton 4.0
    , grPairNx = VU.singleton 1.0
    , grPairNy = VU.singleton 0.0
    , grPairMetric11 = VU.singleton 3.0
    , grPairMetric12 = VU.singleton 0.0
    , grPairMetric22 = VU.singleton 1.0
    , grFaceOutDeg = VU.fromList [1, 1]
    , grNewToOld = VU.fromList [0, 1]
    , grOldToNew = VU.fromList [0, 1]
    }

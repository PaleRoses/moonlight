module Test.Moonlight.Flow.Gen.Plan
  ( genPlanProgram,
    genPlanShape,
    atomOrderPair,
  )
where

import Data.Bifunctor (first)
import Moonlight.Flow.Plan.Query.Core
  ( QueryPlanDomain (StructuralQueryPlan),
  )
import Moonlight.Flow.Plan.Residual
  ( ResidualShape (ResidualNone),
  )
import Moonlight.Flow.Plan.Shape.Build qualified as ShapeBuild
import Moonlight.Flow.Plan.Shape.Encode qualified as ShapeEncode
import Moonlight.Flow.Plan.Shape.Term
  ( LogicalPlanTerm (..),
    PlanShape (..),
    PlanStage (RawLogical),
    RawSlot (..),
  )
import Test.Moonlight.Flow.Execution.RelProgram
  ( RelProgram,
    atom,
    program,
    programRawPlanShape,
  )
import Test.QuickCheck
  ( Gen,
    chooseInt,
    elements,
  )

genPlanProgram :: Gen RelProgram
genPlanProgram = do
  variant <- elements [0 :: Int, 1, 2]
  fanout <- chooseInt (1, 4)
  pure $ case variant of
    0 -> program "literal-single" 0 [atom 0 [0] (fmap pure [1 .. fanout])] Nothing
    1 -> program "literal-path" 0 [atom 0 [0, 1] [[1, 10], [2, 20]], atom 1 [1, 2] [[10, 100], [20, 200]]] Nothing
    _ -> program "literal-triangle" 0 [atom 0 [0, 1] [[1, 2], [1, 3]], atom 1 [1, 2] [[2, 4], [3, 5]], atom 2 [0, 2] [[1, 4]]] Nothing

genPlanShape :: Gen (PlanShape 'RawLogical)
genPlanShape = do
  relProgram <- genPlanProgram
  case programRawPlanShape relProgram of
    Right shape -> pure shape
    Left _ -> pure fallbackRawShape


fallbackRawShape :: PlanShape 'RawLogical
fallbackRawShape =
  ShapeBuild.mkPlanShape
    ShapeEncode.logicalPlanTermWords
    LogicalPlanTerm
      { lptDomain = StructuralQueryPlan,
        lptAtoms = [],
        lptRoot = RawSlot 0,
        lptOutputs = [RawSlot 0],
        lptResidual = ResidualNone
      }

atomOrderPair :: Either String (PlanShape 'RawLogical, PlanShape 'RawLogical)
atomOrderPair = do
  left <- first show (programRawPlanShape leftProgram)
  right <- first show (programRawPlanShape rightProgram)
  pure (left, right)
  where
    leftProgram =
      program
        "atom-order-left"
        0
        [ atom 0 [0, 1] [[1, 2], [2, 3]],
          atom 1 [1, 2] [[2, 4], [3, 5]]
        ]
        Nothing
    rightProgram =
      program
        "atom-order-right"
        0
        [ atom 1 [1, 2] [[2, 4], [3, 5]],
          atom 0 [0, 1] [[1, 2], [2, 3]]
        ]
        Nothing


module Moonlight.EGraph.Test.Arith.Fixture
  ( one,
    two,
    three,
    four,
    five,
    zero,
    onePlusZero,
    onePlusZeroPlusZero,
    zeroPlusOne,
    onePlusTwo,
    onePlusThree,
    onePlusFour,
    threePlusFour,
    nestedAdd,
    seedArith,
    seedArithPair,
    seedArithTerms,
    classOfArith,
    assertArithTerm,
    addZeroLeftPattern,
    addZeroRightPattern,
    preferPattern,
    commutedCheckpoint,
    commutedAddGuidance,
    retainAddZeroGuidance,
    emptyArithGraph,
    insertArith,
    onePlusOne,
    twoPlusOne,
  )
where

import Moonlight.Control.Gate (GuideMode (GuidePrefer))
import Control.Monad (foldM)
import Moonlight.Core (Pattern (..))
import Moonlight.Core qualified as EGraph
import Moonlight.Core (UnionFindAllocationError)
import Moonlight.EGraph.Pure.Kernel.HashCons (addTerm)
import Moonlight.EGraph.Pure.Types (ClassId, EGraph, emptyEGraph)
import Moonlight.EGraph.Test.Arith.Core
  ( ArithF (..),
    NodeCount,
    addTermNode,
    analysisSpec,
    numTerm,
    viewArithTerm,
  )
import Moonlight.EGraph.Test.Saturation
  ( GuidanceConfig (GuidanceConfig),
    GuideCheckpoint (GuideCheckpoint, gcMode, gcName, gcTarget),
  )
import Data.Fix (Fix)
import Test.Tasty.HUnit (Assertion, (@?=))

emptyArithGraph :: EGraph ArithF NodeCount
emptyArithGraph =
  emptyEGraph analysisSpec

insertArith :: Fix ArithF -> EGraph ArithF NodeCount -> Either UnionFindAllocationError (ClassId, EGraph ArithF NodeCount)
insertArith =
  addTerm

seedArith :: Fix ArithF -> Either UnionFindAllocationError (ClassId, EGraph ArithF NodeCount)
seedArith term =
  insertArith term emptyArithGraph

seedArithPair :: Fix ArithF -> Fix ArithF -> Either UnionFindAllocationError (ClassId, ClassId, EGraph ArithF NodeCount)
seedArithPair leftTerm rightTerm = do
  (leftClassId, graph1) <- seedArith leftTerm
  (rightClassId, graph2) <- insertArith rightTerm graph1
  pure (leftClassId, rightClassId, graph2)

seedArithTerms :: [Fix ArithF] -> Either UnionFindAllocationError (EGraph ArithF NodeCount)
seedArithTerms =
  foldM (\graph term -> snd <$> insertArith term graph) emptyArithGraph

classOfArith :: Fix ArithF -> EGraph ArithF NodeCount -> Either UnionFindAllocationError ClassId
classOfArith term =
  fmap fst . insertArith term

assertArithTerm :: Fix ArithF -> Fix ArithF -> Assertion
assertArithTerm expected actual =
  viewArithTerm actual @?= viewArithTerm expected

commutedAddGuidance :: GuidanceConfig (Pattern ArithF)
commutedAddGuidance =
  preferPattern "prefer-commuted-add" addZeroLeftPattern

retainAddZeroGuidance :: GuidanceConfig (Pattern ArithF)
retainAddZeroGuidance =
  preferPattern "retain-add-zero" addZeroRightPattern

commutedCheckpoint :: GuideCheckpoint (Pattern ArithF)
commutedCheckpoint =
  GuideCheckpoint
    { gcName = "commuted-add",
      gcMode = GuidePrefer,
      gcTarget = addZeroLeftPattern
    }

preferPattern :: String -> Pattern ArithF -> GuidanceConfig (Pattern ArithF)
preferPattern checkpointName checkpointTarget =
  GuidanceConfig
    [ GuideCheckpoint
        { gcName = checkpointName,
          gcMode = GuidePrefer,
          gcTarget = checkpointTarget
        }
    ]

addZeroLeftPattern :: Pattern ArithF
addZeroLeftPattern =
  PatternNode (Add (PatternNode (Num 0)) (PatternVar (EGraph.mkPatternVar 0)))

addZeroRightPattern :: Pattern ArithF
addZeroRightPattern =
  PatternNode (Add (PatternVar (EGraph.mkPatternVar 0)) (PatternNode (Num 0)))

one :: Fix ArithF
one =
  numTerm 1

two :: Fix ArithF
two =
  numTerm 2

three :: Fix ArithF
three =
  numTerm 3

four :: Fix ArithF
four =
  numTerm 4

five :: Fix ArithF
five =
  numTerm 5

zero :: Fix ArithF
zero =
  numTerm 0

onePlusZero :: Fix ArithF
onePlusZero =
  addTermNode one zero

onePlusZeroPlusZero :: Fix ArithF
onePlusZeroPlusZero =
  addTermNode onePlusZero zero

zeroPlusOne :: Fix ArithF
zeroPlusOne =
  addTermNode zero one

onePlusTwo :: Fix ArithF
onePlusTwo =
  addTermNode one two

onePlusOne :: Fix ArithF
onePlusOne =
  addTermNode one one

twoPlusOne :: Fix ArithF
twoPlusOne =
  addTermNode two one

onePlusThree :: Fix ArithF
onePlusThree =
  addTermNode one three

onePlusFour :: Fix ArithF
onePlusFour =
  addTermNode one four

threePlusFour :: Fix ArithF
threePlusFour =
  addTermNode three four

nestedAdd :: Fix ArithF
nestedAdd =
  addTermNode one (addTermNode two three)

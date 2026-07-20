module Moonlight.Pale.Test.Site.Core
  ( TestBudget (..),
    canonicalTestBudget,
    scopedTestBudget,
    stressTestBudget,
    tightNodeBudget,
    tightIterationBudget,
    mediumPressureBudget,
    generousBudget,
    TestEpsilon (..),
    defaultTestEpsilon,
  )
where

import Data.Kind (Type)

type TestBudget :: Type
data TestBudget = TestBudget
  { testBudgetMaxIterations :: !Int,
    testBudgetMaxNodes :: !Int
  }
  deriving stock (Eq, Show, Read)

canonicalTestBudget :: TestBudget
canonicalTestBudget = TestBudget {testBudgetMaxIterations = 4, testBudgetMaxNodes = 20}

scopedTestBudget :: TestBudget
scopedTestBudget = TestBudget {testBudgetMaxIterations = 3, testBudgetMaxNodes = 20}

stressTestBudget :: TestBudget
stressTestBudget = TestBudget {testBudgetMaxIterations = 30, testBudgetMaxNodes = 1000}

tightNodeBudget :: TestBudget
tightNodeBudget = TestBudget {testBudgetMaxIterations = 30, testBudgetMaxNodes = 15}

tightIterationBudget :: TestBudget
tightIterationBudget = TestBudget {testBudgetMaxIterations = 2, testBudgetMaxNodes = 5000}

mediumPressureBudget :: TestBudget
mediumPressureBudget = TestBudget {testBudgetMaxIterations = 15, testBudgetMaxNodes = 300}

generousBudget :: TestBudget
generousBudget = TestBudget {testBudgetMaxIterations = 30, testBudgetMaxNodes = 1500}

type TestEpsilon :: Type
data TestEpsilon = TestEpsilon
  { testEpsilonPhysics :: !Double,
    testEpsilonExact :: !Double,
    testEpsilonSolver :: !Double
  }
  deriving stock (Eq, Show, Read)

defaultTestEpsilon :: TestEpsilon
defaultTestEpsilon =
  TestEpsilon
    { testEpsilonPhysics = 1.0e-9,
      testEpsilonExact = 1.0e-12,
      testEpsilonSolver = 1.0e-5
    }

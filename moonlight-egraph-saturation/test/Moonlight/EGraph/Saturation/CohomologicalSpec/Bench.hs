module Moonlight.EGraph.Saturation.CohomologicalSpec.Bench
  ( tests,
  )
where

import Control.Monad (foldM)
import Moonlight.Saturation.Matching qualified as GenericMatching
import Moonlight.EGraph.Saturation.CohomologicalSpec.Prelude hiding (PatternRewriteError, RewriteMorphism)
import Data.Set qualified as Set
import Data.Fix (Fix)
import Moonlight.EGraph.Introspection.Core.Rewrite (PatternRewriteError, RewriteMorphism, RewriteSystem, mkRewriteSystem, rewriteMorphismWithInterface)
import Moonlight.EGraph.Saturation.Cohomological.Backend.Matching
  ( cohomologicalMatchingAlgebra,
  )
import Moonlight.Pale.Test.Site.Assertion (expectRight)

assertRight :: Show obstruction => String -> Either obstruction value -> IO value
assertRight failureLabel =
  either (\obstruction -> assertFailure (failureLabel <> ": " <> show obstruction)) pure

tests :: TestTree
tests =
  testGroup
    "bench"
    [ testCase "cohomological algebra produces matches on structured graph" testCohomologicalProducesMatches
    ]

pairRules :: Either (PatternRewriteError TestF) (RewriteSystem TestF)
pairRules =
  let v0 = PatternVar queryVar0
      v1 = PatternVar queryVar1
      both = Set.fromList [queryVar0, queryVar1]
      one = Set.singleton queryVar0
      rule :: Pattern TestF -> Pattern TestF -> Set.Set PatternVar -> String -> Either (PatternRewriteError TestF) (RewriteMorphism TestF)
      rule lhs rhs iface name = rewriteMorphismWithInterface name lhs iface rhs Nothing Nothing
   in fmap
        mkRewriteSystem
        ( sequenceA
            [ rule (PatternNode (Pair v0 (PatternNode (Lit 0)))) v0 one "pair-zero",
              rule (PatternNode (Pair v0 v1)) (PatternNode (Pair v1 v0)) both "commute"
            ]
        )

structuredTerms :: [Fix TestF]
structuredTerms =
  [ pairTerm (litTerm termIndex) (litTerm (termIndex + 1))
  | termIndex <- [0 .. 99]
  ]
    <> [ pairTerm (litTerm termIndex) (litTerm 0)
       | termIndex <- [0 .. 19]
       ]

testCohomologicalProducesMatches :: Assertion
testCohomologicalProducesMatches =
  withTestTerms structuredTerms $ \_ graph -> do
    compiledQuery <- assertRight "failed to compile query" (compileQuery (PatternVar queryVar0))
    rewriteSystem <- assertRight "failed to build pair rules" pairRules
    (rootClass, graphWithRoot) <- assertRight "failed to add the certification root" (addTerm (litTerm 1) graph)
    let backend =
          mkExactWitnessBackendWithRewriteSystem
            rewriteSystem
            (propertyContext rootClass rootClass)
        algebra = cohomologicalMatchingAlgebra backend
        request = mkMatchingRequest compiledQuery
        world = mkMatchingWorld graphWithRoot
    (_, matches) <- assertRight "expected matching query to succeed" (runFullMatchingQuery algebra (GenericMatching.maInitialState algebra) world request)
    assertBool
      ("expected matches on structured graph, got " <> show (length matches))
      (not (null matches))

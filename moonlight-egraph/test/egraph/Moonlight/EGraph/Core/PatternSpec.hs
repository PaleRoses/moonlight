module Moonlight.EGraph.Core.PatternSpec
  ( tests,
  )
where

import Data.List.NonEmpty ( NonEmpty(..) )
import Moonlight.Core.Pattern.Automata
    ( compileConjunctivePatternAutomaton,
      compilePatternAutomaton,
      intersectPatternAutomaton,
      matchPatternAutomaton,
      matchesPatternAutomaton )
import Moonlight.EGraph.Pure.Types ( ClassId(ClassId) )
import Moonlight.EGraph.Test.Arith.Core
    ( ArithF(..), ArithView(..), addTermNode, numTerm, viewArithTerm )
import Moonlight.EGraph.Test.Arith.Matcher
    ( bindingsView, directMatchPattern )
import Data.Fix ( Fix )
import Moonlight.Core
import Moonlight.Core qualified as EGraph
import Moonlight.Core
  ( emptySubstitution,
    extendSubst,
    lookupSubst
  )
import Moonlight.Pale.Test.LawSuite ( lawSuiteGroup, quickCheckLaw )
import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.HUnit ( (@?=), testCase )
import Data.IntMap.Strict qualified as IntMap ( lookup, empty )

patternAutomatonMatchesReferenceProperty :: Pattern ArithF -> Fix ArithF -> Bool
patternAutomatonMatchesReferenceProperty patternValue termValue =
  fmap bindingsView
    (matchPatternAutomaton (compilePatternAutomaton patternValue) termValue IntMap.empty)
    == fmap bindingsView (directMatchPattern patternValue termValue IntMap.empty)

tests :: TestTree
tests =
  testGroup
    "pattern"
    [ testCase "empty substitution is empty" $
        lookupSubst (EGraph.mkPatternVar 0) emptySubstitution @?= Nothing,
      testCase "extend binds unbound variable" $
        let substitution = extendSubst (EGraph.mkPatternVar 0) (ClassId 3) emptySubstitution
         in fmap (lookupSubst (EGraph.mkPatternVar 0)) substitution @?= Just (Just (ClassId 3)),
      testCase "extend accepts the same binding twice" $
        let substitution =
              extendSubst (EGraph.mkPatternVar 0) (ClassId 3) emptySubstitution
                >>= extendSubst (EGraph.mkPatternVar 0) (ClassId 3)
         in fmap (lookupSubst (EGraph.mkPatternVar 0)) substitution @?= Just (Just (ClassId 3)),
      testCase "extend rejects conflicting bindings" $
        let substitution =
              extendSubst (EGraph.mkPatternVar 0) (ClassId 3) emptySubstitution
                >>= extendSubst (EGraph.mkPatternVar 0) (ClassId 4)
         in substitution @?= Nothing,
      testCase "compiled pattern automaton returns exact bindings" $
        let automaton =
              compilePatternAutomaton
                (PatternNode (Add (PatternVar (EGraph.mkPatternVar 0)) (PatternNode (Num 0))))
            termValue =
              addTermNode (numTerm 3) (numTerm 0)
         in fmap (fmap viewArithTerm . IntMap.lookup 0) (matchPatternAutomaton automaton termValue IntMap.empty)
              @?= Just (Just (NumView 3)),
      testCase "compiled pattern automaton rejects repeated-variable mismatches" $
        let automaton =
              compilePatternAutomaton
                (PatternNode (Add (PatternVar (EGraph.mkPatternVar 0)) (PatternVar (EGraph.mkPatternVar 0))))
         in matchesPatternAutomaton automaton (addTermNode (numTerm 1) (numTerm 2))
              @?= False,
      testCase "pattern automata intersect without losing bindings" $
        let leftAutomaton =
              compilePatternAutomaton
                (PatternNode (Add (PatternVar (EGraph.mkPatternVar 0)) (PatternNode (Num 0))))
            rightAutomaton =
              compilePatternAutomaton
                (PatternNode (Add (PatternNode (Num 3)) (PatternVar (EGraph.mkPatternVar 1))))
            termValue =
              addTermNode (numTerm 3) (numTerm 0)
         in fmap
              (\bindings -> (viewArithTerm <$> IntMap.lookup 0 bindings, viewArithTerm <$> IntMap.lookup 1 bindings))
              (matchPatternAutomaton (intersectPatternAutomaton leftAutomaton rightAutomaton) termValue IntMap.empty)
              @?= Just (Just (NumView 3), Just (NumView 0)),
      testCase "conjunctive pattern automaton matches the product automaton result" $
        let leftPattern =
              PatternNode (Add (PatternVar (EGraph.mkPatternVar 0)) (PatternNode (Num 0)))
            rightPattern =
              PatternNode (Add (PatternNode (Num 3)) (PatternVar (EGraph.mkPatternVar 1)))
            termValue =
              addTermNode (numTerm 3) (numTerm 0)
         in fmap (fmap viewArithTerm)
              ( matchPatternAutomaton
                  (compileConjunctivePatternAutomaton (leftPattern :| [rightPattern]))
                  termValue
                  IntMap.empty
              )
              @?= fmap (fmap viewArithTerm)
                ( matchPatternAutomaton
                    ( intersectPatternAutomaton
                        (compilePatternAutomaton leftPattern)
                        (compilePatternAutomaton rightPattern)
                    )
                    termValue
                    IntMap.empty
                )
      ,
      lawSuiteGroup
        "laws"
        [ quickCheckLaw "pattern_automaton_matches_reference" patternAutomatonMatchesReferenceProperty
        ]
    ]

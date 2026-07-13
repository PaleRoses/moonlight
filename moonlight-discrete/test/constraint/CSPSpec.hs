module CSPSpec
  ( tests,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Moonlight.Constraint
  ( Arc (..),
    BinaryConstraint (..),
    ConstraintSatisfactionProblem (..),
    domainFromList,
    domainFromSet,
    domainSingleton,
    domainToAscList,
    lookupDomain,
    mac3,
    revise,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, (@?=), testCase)

lessThanConstraint :: Ord value => Arc variable -> BinaryConstraint variable value
lessThanConstraint constraintArcValue =
  BinaryConstraint
    { binaryConstraintArc = constraintArcValue,
      binaryConstraintSatisfied = (<)
    }

tests :: TestTree
tests =
  testGroup
    "csp"
    [ testCase "domainFromList normalizes ordering and duplicates" $
        domainToAscList (domainFromList [3 :: Int, 1, 3, 2]) @?= [1, 2, 3],
      testCase "domainFromSet preserves support exactly" $
        domainToAscList (domainFromSet (Set.fromList [3 :: Int, 1, 2])) @?= [1, 2, 3],
      testCase "revise prunes unsupported values from the source domain" $ do
        let problem =
              ConstraintSatisfactionProblem
                { cspDomains =
                    Map.fromList
                      [ ('x', domainFromList [1 :: Int, 2]),
                        ('y', domainSingleton 2)
                      ],
                  cspConstraints = [lessThanConstraint (Arc 'x' 'y')]
                }
        case revise problem (Arc 'x' 'y') of
          Left err ->
            assertFailure ("unexpected revise failure: " <> show err)
          Right (revisedProblem, wasRevised) -> do
            wasRevised @?= True
            lookupDomain revisedProblem 'x' @?= Right (domainSingleton 1),
      testCase "mac3 reports inconsistency when propagation empties a domain" $ do
        let problem =
              ConstraintSatisfactionProblem
                { cspDomains =
                    Map.fromList
                      [ ('x', domainSingleton (1 :: Int)),
                        ('y', domainSingleton 1)
                      ],
                  cspConstraints = [lessThanConstraint (Arc 'x' 'y')]
                }
        case mac3 problem of
          Left err ->
            assertFailure ("unexpected mac3 failure: " <> show err)
          Right result ->
            case result of
              Nothing -> pure ()
              Just _ -> assertFailure "expected MAC-3 to detect inconsistency",
      testCase "mac3 propagates arc consistency through dependent neighbors" $ do
        let problem =
              ConstraintSatisfactionProblem
                { cspDomains =
                    Map.fromList
                      [ ('x', domainFromList [1 :: Int, 2, 3]),
                        ('y', domainFromList [1, 2, 3]),
                        ('z', domainSingleton 3)
                      ],
                  cspConstraints =
                    [ lessThanConstraint (Arc 'x' 'y'),
                      lessThanConstraint (Arc 'y' 'z')
                    ]
                }
        case mac3 problem of
          Left err ->
            assertFailure ("unexpected mac3 failure: " <> show err)
          Right Nothing ->
            assertFailure "expected a consistent arc-reduced problem"
          Right (Just reducedProblem) -> do
            lookupDomain reducedProblem 'x' @?= Right (domainSingleton 1)
            lookupDomain reducedProblem 'y' @?= Right (domainSingleton (2 :: Int))
    ]

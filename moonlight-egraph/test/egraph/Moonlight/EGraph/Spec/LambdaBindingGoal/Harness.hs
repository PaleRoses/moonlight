module Moonlight.EGraph.Spec.LambdaBindingGoal.Harness
  ( LambdaGoalScenario (..),
    LambdaGoalCore (..),
    LambdaGoalReport (..),
    LambdaGoalRun (..),
    LambdaBindingHarness (..),
    lookupNamedContext,
    lookupNamedClass,
    lookupNamedNormalForm,
    lookupMetric,
  )
where

import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.EGraph.Pure.Context
  ( ContextEGraph,
  )
import Moonlight.EGraph.Pure.Types (ClassId)

type LambdaGoalScenario :: Type
data LambdaGoalScenario
  = AlphaEquivalenceScenario
  | DynamicBetaScenario
  | CaptureAvoidanceScenario
  | EtaScenario
  | LetFloatScenario
  | LatticeGrowthScenario
  | DeepNestingScenario !Int
  | ProfiledScopeSensitiveScenario !Int
  deriving stock (Eq, Ord, Show)

type LambdaGoalRun :: Type -> (Type -> Type) -> Type -> Type
data LambdaGoalRun ctx f a = LambdaGoalRun
  { lgrCore :: !(LambdaGoalCore ctx f a),
    lgrReport :: !LambdaGoalReport,
    lgrNamedContexts :: !(Map String ctx),
    lgrNamedClasses :: !(Map (String, String) ClassId),
    lgrNamedNormalForms :: !(Map (String, String) String),
    lgrMetrics :: !(Map String Int)
  }

type LambdaGoalCore :: Type -> (Type -> Type) -> Type -> Type
data LambdaGoalCore ctx f a = LambdaGoalCore
  { lgcGraph :: !(ContextEGraph f a ctx),
    lgcContextRevision :: !Int
  }

type LambdaGoalReport :: Type
data LambdaGoalReport = LambdaGoalReport
  { lgrIterations :: !Int,
    lgrMatchesApplied :: !Int
  }

type LambdaBindingHarness :: Type -> (Type -> Type) -> Type -> Type
newtype LambdaBindingHarness ctx f a = LambdaBindingHarness
  { lbhRunScenario :: LambdaGoalScenario -> Either String (LambdaGoalRun ctx f a)
  }

lookupNamedContext :: String -> LambdaGoalRun ctx f a -> Either String ctx
lookupNamedContext label runValue =
  maybe
    (Left ("missing named context: " <> show label))
    Right
    (Map.lookup label (lgrNamedContexts runValue))

lookupNamedClass :: String -> String -> LambdaGoalRun ctx f a -> Either String ClassId
lookupNamedClass contextLabel termLabel runValue =
  maybe
    (Left ("missing named class for " <> show (contextLabel, termLabel)))
    Right
    (Map.lookup (contextLabel, termLabel) (lgrNamedClasses runValue))

lookupNamedNormalForm :: String -> String -> LambdaGoalRun ctx f a -> Either String String
lookupNamedNormalForm contextLabel termLabel runValue =
  maybe
    (Left ("missing named normal form for " <> show (contextLabel, termLabel)))
    Right
    (Map.lookup (contextLabel, termLabel) (lgrNamedNormalForms runValue))

lookupMetric :: String -> LambdaGoalRun ctx f a -> Either String Int
lookupMetric key runValue =
  maybe
    (Left ("missing metric: " <> show key))
    Right
    (Map.lookup key (lgrMetrics runValue))

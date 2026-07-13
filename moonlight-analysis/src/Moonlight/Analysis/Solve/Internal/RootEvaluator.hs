
module Moonlight.Analysis.Solve.Internal.RootEvaluator
  ( ScalarRootFunction (..),
    DifferentiableRootFunction (..),
    evaluateScalarRootFunction,
    evaluateDifferentiableRootFunction,
  )
where

import Data.Kind (Type)
import Moonlight.Analysis.Dual (Dual (..))
import Moonlight.Core (AdditiveGroup, AdditiveMonoid (..))

type ScalarRootFunction :: Type -> Type
newtype ScalarRootFunction a = ScalarRootFunction
  { runScalarRootFunction :: a -> a
  }

type DifferentiableRootFunction :: Type -> Type
data DifferentiableRootFunction a = DifferentiableRootFunction
  { runDifferentiableRootFunction :: forall s. Dual s a -> Dual s a
  }

evaluateScalarRootFunction :: ScalarRootFunction a -> a -> a
evaluateScalarRootFunction function value = runScalarRootFunction function value

evaluateDifferentiableRootFunction :: AdditiveGroup a => DifferentiableRootFunction a -> a -> a
evaluateDifferentiableRootFunction (DifferentiableRootFunction function) value =
  primal (function (Dual value zero))

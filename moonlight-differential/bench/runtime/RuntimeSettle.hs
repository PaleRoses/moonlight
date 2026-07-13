module RuntimeSettle where

import Control.DeepSeq (NFData (..))
import Data.Functor.Identity (Identity (..))
import Moonlight.Differential.Runtime.Settle
import Moonlight.Differential.Time (emptyRuntimeScope)

data PreparedRuntimeSettle = PreparedRuntimeSettle
  { preparedRuntimeSettleLimit :: !Int,
    preparedRuntimeSettleInitial :: !Int
  }

instance NFData PreparedRuntimeSettle where
  rnf preparedCase =
    preparedRuntimeSettleLimit preparedCase
      `seq` preparedRuntimeSettleInitial preparedCase
      `seq` ()

runtimeSettleCase :: Int -> PreparedRuntimeSettle
runtimeSettleCase size =
  PreparedRuntimeSettle
    { preparedRuntimeSettleLimit = size,
      preparedRuntimeSettleInitial = size
    }

runtimeSettleWeight :: PreparedRuntimeSettle -> Either String Int
runtimeSettleWeight preparedCase =
  either
    (Left . show)
    Right
    ( runIdentity
        ( runRuntimeSettleLoop
            (preparedRuntimeSettleLimit preparedCase + 1)
            runtimeSettleStep
            (preparedRuntimeSettleInitial preparedCase)
        )
    )

runtimeScopedSettleWeight :: PreparedRuntimeSettle -> Either String Int
runtimeScopedSettleWeight preparedCase =
  either
    (Left . show)
    Right
    ( runIdentity
        ( runRuntimeSettleLoopScoped
            (const True)
            (preparedRuntimeSettleLimit preparedCase + 1)
            runtimeScopedSettleStep
            (preparedRuntimeSettleInitial preparedCase)
        )
    )

runtimeSettleStep :: RuntimeSettleStep Identity Int Int
runtimeSettleStep =
  RuntimeSettleStep
    { rssDrain = pure . subtractOneUntilZero,
      rssFlush = pure,
      rssQuiescent = (== 0),
      rssResidual = id
    }

runtimeScopedSettleStep :: RuntimeScopedSettleStep Identity Int Int
runtimeScopedSettleStep =
  RuntimeScopedSettleStep
    { rssScopedDrain = \keepScope state -> pure (if keepScope emptyRuntimeScope then subtractOneUntilZero state else state),
      rssScopedFlush = \_keepScope -> pure,
      rssScopedQuiescent = \keepScope state -> keepScope emptyRuntimeScope && state == 0,
      rssScopedResidual = \_keepScope -> id
    }

subtractOneUntilZero :: Int -> Int
subtractOneUntilZero value =
  max 0 (value - 1)

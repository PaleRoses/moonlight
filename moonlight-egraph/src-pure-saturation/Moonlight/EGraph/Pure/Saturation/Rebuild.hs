{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.EGraph.Pure.Saturation.Rebuild
  ( RoundRebuildReport (..),
    runRoundRebuildReport,
  )
where

import Data.Kind (Type)
import Moonlight.Core (Language)
import Moonlight.EGraph.Pure.Context
  ( ContextDeltaError,
    ContextRepairScope (contextRepairScopeObjects),
    cegBase,
    contextRepairScopeFromCachedObjects,
    rebaseContextGraphAtContexts,
  )
import Moonlight.EGraph.Pure.Rebuild
  ( EGraphRebuildDelta,
    rebuildWithDelta,
  )
import Moonlight.EGraph.Pure.Types (eGraphEditDeltaNull, eGraphPendingDelta)
import Moonlight.EGraph.Saturation.Context.State
  ( SaturatingContextEGraph,
    mapSaturatingContextGraph,
    sceContextGraph,
  )

-- | The authoritative result of rebuilding one saturation round.  The base
-- e-graph supplies structural congruence and analysis; the context graph's
-- regional cache supplies every contextual representative and row.  No
-- materialized per-context quotient is retained here.
type RoundRebuildReport :: Type -> Type -> (Type -> Type) -> Type -> Type -> Type
data RoundRebuildReport owner capability f a c = RoundRebuildReport
  { rrrGraph :: !(SaturatingContextEGraph owner capability f a c),
    rrrRebuildDelta :: !EGraphRebuildDelta
  }

runRoundRebuildReport ::
  (Language f, Ord c) =>
  SaturatingContextEGraph owner capability f a c ->
  Either (ContextDeltaError f c) (RoundRebuildReport owner capability f a c)
runRoundRebuildReport saturatingGraph =
  let contextGraph = sceContextGraph saturatingGraph
      (rebuildDelta, rebuiltBase) = rebuildWithDelta (cegBase contextGraph)
      pendingBaseEdit = not (eGraphEditDeltaNull (eGraphPendingDelta (cegBase contextGraph)))
      rebuiltReport rebuiltContextGraph =
        RoundRebuildReport
          { rrrGraph = mapSaturatingContextGraph (const rebuiltContextGraph) saturatingGraph,
            rrrRebuildDelta = rebuildDelta
          }
   in if not pendingBaseEdit
        then Right (rebuiltReport contextGraph)
        else
          fmap rebuiltReport
            ( rebaseContextGraphAtContexts
                ( contextRepairScopeObjects
                    (contextRepairScopeFromCachedObjects contextGraph)
                )
                rebuiltBase
                contextGraph
            )
{-# INLINE runRoundRebuildReport #-}

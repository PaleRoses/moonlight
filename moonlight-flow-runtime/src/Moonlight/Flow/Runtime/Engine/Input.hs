module Moonlight.Flow.Runtime.Engine.Input
  ( RuntimeInput (..),
    RuntimeGeneratedSitePatch (..),
  )
where

import Data.Map.Strict
  ( Map,
  )
import Moonlight.Core
  ( QueryId,
  )
import Moonlight.Flow.Model.Delta
  ( QuotientPatch
  )
import Moonlight.Flow.Runtime.Factor.Program
  ( FactorProgram,
  )
import Moonlight.Flow.Runtime.Spec.FactorProgram
  ( RepairProgramKey,
  )
import Moonlight.Flow.Runtime.Factor.State.Types
  ( RuntimeQueryBinding,
  )
import Moonlight.Flow.Runtime.Topology.Site.Patch
  ( GeneratedSitePatch,
  )

data RuntimeGeneratedSitePatch ctx prop = RuntimeGeneratedSitePatch
  { rgspSitePatch :: !(GeneratedSitePatch ctx prop),
    rgspFactorPrograms :: !(Map RepairProgramKey FactorProgram),
    rgspQueryBindings :: !(Map QueryId RuntimeQueryBinding)
  }

data RuntimeInput ctx prop
  = RuntimeInputQuotientPatch !QuotientPatch
  | RuntimeInputGeneratedSitePatch !(RuntimeGeneratedSitePatch ctx prop)
  | RuntimeInputSettle

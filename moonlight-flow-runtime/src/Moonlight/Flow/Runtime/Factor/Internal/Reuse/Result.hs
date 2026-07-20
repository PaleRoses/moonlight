{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Runtime.Factor.Internal.Reuse.Result
  ( ExactReuseResult (..),
    FactorReuseMaterialization (..),
    LowerBoundReuseResult (..),
    ExactByCoverReuseResult (..),
  )
where

import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
  )
import Moonlight.Flow.Carrier.Core.Delta
  ( RelationalCarrierDelta,
  )
import Moonlight.Flow.Carrier.Morphism.Subsumption
  ( CarrierReuse,
  )
import Moonlight.Flow.Runtime.Carrier.Reuse.CoverMaterialization
  ( CoverMaterializationPlan,
  )

data ExactReuseResult ctx prop boundary evidence = ExactReuseResult
  { errSnapshots :: ![RelationalCarrierDelta ctx Carrier prop boundary evidence],
    errDeltas :: ![RelationalCarrierDelta ctx Carrier prop boundary evidence]
  }
  deriving stock (Eq, Show)

data FactorReuseMaterialization ctx prop boundary evidence = FactorReuseMaterialization
  { frumReuse :: !(CarrierReuse ctx prop),
    frumSourceSnapshot :: !(RelationalCarrierDelta ctx Carrier prop boundary evidence),
    frumProjectedSnapshot :: !(RelationalCarrierDelta ctx Carrier prop boundary evidence),
    frumProjectedDelta :: !(RelationalCarrierDelta ctx Carrier prop boundary evidence)
  }
  deriving stock (Eq, Show)

data LowerBoundReuseResult ctx prop boundary evidence = LowerBoundReuseResult
  { lbrrMaterializations :: ![FactorReuseMaterialization ctx prop boundary evidence]
  }
  deriving stock (Eq, Show)

data ExactByCoverReuseResult ctx prop boundary evidence = ExactByCoverReuseResult
  { ebcrPlans :: ![CoverMaterializationPlan ctx prop evidence]
  }
  deriving stock (Eq, Show)

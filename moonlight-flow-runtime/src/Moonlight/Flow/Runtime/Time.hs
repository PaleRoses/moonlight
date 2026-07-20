module Moonlight.Flow.Runtime.Time
  ( RuntimeEventTime,
  )
where

import Moonlight.Flow.Carrier.Core.Time
  ( RelationalCarrierTime,
  )

type RuntimeEventTime ctx =
  RelationalCarrierTime ctx

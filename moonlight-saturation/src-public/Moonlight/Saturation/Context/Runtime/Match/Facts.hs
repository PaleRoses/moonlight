{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Saturation.Context.Runtime.Match.Facts
  ( effectiveContextMatchFactsAt,
  )
where

import Moonlight.Saturation.Context.Runtime.State
  ( RuntimeCore,
    runtimeCoreFactDerivationsAt,
    runtimeCoreFactsAt,
  )
import Moonlight.Saturation.Substrate

effectiveContextMatchFactsAt ::
  forall u schedulerGroup.
  (FactSystem u, Ord (SatContext u), Semigroup (SatFactIndex u)) =>
  SatContext u ->
  SatFactStore u ->
  SatFactIndex u ->
  SatContext u ->
  RuntimeCore u schedulerGroup ->
  (SatFactStore u, SatFactIndex u)
effectiveContextMatchFactsAt baseContext baseFacts baseDerivations contextValue coreState =
  let (contextFacts, contextDerivations) =
        if contextValue == baseContext
          then
            ( emptyFactStore @u,
              emptyFactIndex @u
            )
          else
            ( runtimeCoreFactsAt @u contextValue coreState,
              runtimeCoreFactDerivationsAt @u contextValue coreState
            )
   in ( unionFactStores @u baseFacts contextFacts,
        baseDerivations <> contextDerivations
      )
{-# INLINE effectiveContextMatchFactsAt #-}

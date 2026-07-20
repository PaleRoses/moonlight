{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.EGraph.Pure.Saturation.Logic.Seed
  ( SeedFacts (..),
    emptySeedFacts,
    appendSeedFacts,
    singletonSeedFacts,
    resolveSeedFacts,
    resolveSeedSite,
  )
where

import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Saturation.Matching (MatchSite (..))
import Moonlight.Saturation.Substrate
  ( FactSystem,
    SatContext,
    SatFactStore,
    SatGraph,
    SaturationGraph,
    graphBaseContext,
    unionFactStores,
  )

type SeedFacts :: Type -> Type
newtype SeedFacts u = SeedFacts
  { seedFactEntries :: [(MatchSite (SatContext u), SatFactStore u)]
  }

emptySeedFacts :: SeedFacts u
emptySeedFacts =
  SeedFacts []
{-# INLINE emptySeedFacts #-}

appendSeedFacts :: SeedFacts u -> SeedFacts u -> SeedFacts u
appendSeedFacts leftSeeds rightSeeds =
  SeedFacts (seedFactEntries leftSeeds <> seedFactEntries rightSeeds)
{-# INLINE appendSeedFacts #-}

singletonSeedFacts :: MatchSite (SatContext u) -> SatFactStore u -> SeedFacts u
singletonSeedFacts site facts =
  SeedFacts [(site, facts)]
{-# INLINE singletonSeedFacts #-}

resolveSeedFacts ::
  forall u.
  (FactSystem u, SaturationGraph u, Ord (SatContext u)) =>
  SatGraph u ->
  SeedFacts u ->
  Map (SatContext u) (SatFactStore u)
resolveSeedFacts graph seeds =
  Map.fromListWith
    (unionFactStores @u)
    (fmap resolveEntry (seedFactEntries seeds))
  where
    resolveEntry ::
      (MatchSite (SatContext u), SatFactStore u) ->
      (SatContext u, SatFactStore u)
    resolveEntry (site, facts) =
      (resolveSeedSite @u graph site, facts)
{-# INLINE resolveSeedFacts #-}

resolveSeedSite ::
  forall u.
  SaturationGraph u =>
  SatGraph u ->
  MatchSite (SatContext u) ->
  SatContext u
resolveSeedSite graph site =
  case site of
    BaseSite ->
      graphBaseContext @u graph
    ContextSite contextValue ->
      contextValue
{-# INLINE resolveSeedSite #-}

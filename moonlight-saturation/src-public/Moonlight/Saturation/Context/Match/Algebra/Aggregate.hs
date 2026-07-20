{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Saturation.Context.Match.Algebra.Aggregate
  ( aggregateSupportedMatches,
  )
where

import Data.Foldable (foldlM)
import Data.Map.Strict qualified as Map
import Moonlight.Saturation.Substrate

aggregateSupportedMatches ::
  forall u.
  (MatchView u, Ord (SatRuleKey u), Ord (SatClassId u)) =>
  SatGraph u ->
  [SatSupportedMatch u] ->
  Either (SatObstruction u) [SatSupportedMatch u]
aggregateSupportedMatches graph =
  fmap Map.elems . foldlM (insertSupportedMatch @u graph) Map.empty
{-# INLINE aggregateSupportedMatches #-}

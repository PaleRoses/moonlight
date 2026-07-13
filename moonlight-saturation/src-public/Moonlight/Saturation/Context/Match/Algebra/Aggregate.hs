{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Saturation.Context.Match.Algebra.Aggregate
  ( SupportedMatchMap,
    aggregateSupportedMatches,
    insertSupportedMatch,
  )
where

import Data.Foldable (foldlM)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Core (Substitution)
import Moonlight.Saturation.Substrate

type SupportedMatchMap u =
  Map (SatRuleKey u, SatClassId u, Substitution) (SatSupportedMatch u)

aggregateSupportedMatches ::
  forall u.
  (MatchView u, Ord (SatRuleKey u), Ord (SatClassId u)) =>
  SatGraph u ->
  [SatSupportedMatch u] ->
  Either (SatObstruction u) [SatSupportedMatch u]
aggregateSupportedMatches graph =
  fmap Map.elems . foldlM (insertSupportedMatch @u graph) Map.empty
{-# INLINE aggregateSupportedMatches #-}

insertSupportedMatch ::
  forall u.
  (MatchView u, Ord (SatRuleKey u), Ord (SatClassId u)) =>
  SatGraph u ->
  SupportedMatchMap u ->
  SatSupportedMatch u ->
  Either (SatObstruction u) (SupportedMatchMap u)
insertSupportedMatch graph accumulatedMatches supportedMatch =
  Map.alterF
    (mergeAtKey supportedMatch)
    (matchKey @u (supportedMatchInner @u supportedMatch))
    accumulatedMatches
  where
    mergeAtKey candidate maybeExistingMatch =
      case maybeExistingMatch of
        Nothing ->
          Right (Just candidate)
        Just existingMatch ->
          Just <$> mergeSupportedMatch @u graph existingMatch candidate
{-# INLINE insertSupportedMatch #-}

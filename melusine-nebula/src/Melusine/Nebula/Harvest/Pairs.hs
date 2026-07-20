{-# LANGUAGE StandaloneKindSignatures #-}

module Melusine.Nebula.Harvest.Pairs
  ( SiteRow (..),
    SiteRef (..),
    PairLedgerEntry (..),
    PairLedger (..),
    siteRow,
    siteRowsByIdentity,
    siteRefsByIdentity,
    groupLedgerKey,
    buildGroupPairs,
    buildPairLedger,
    advancePairLedger,
    admittedSitePairs,
  )
where

import Data.Kind (Type)
import Data.List (sortOn, tails)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Melusine.Nebula.Core (TypeEvidence)
import Melusine.Nebula.Discovery.Choose
  ( CandidateSite (..),
    CandidateSiteKind,
    sitePairKey,
  )
import Moonlight.EGraph.Introspection.Core.HsExpr (ScopeCtx, SourceRegion)
import Moonlight.EGraph.Pure.Types (ClassId)

type SiteRow :: Type
data SiteRow = SiteRow
  { srBindingName :: !String,
    srSiteKind :: !CandidateSiteKind,
    srRegion :: !(Maybe SourceRegion),
    srContext :: !ScopeCtx,
    srClass :: !ClassId,
    srOriginalSize :: !Int,
    srSize :: !Int,
    srFreeScopeWidth :: !Int,
    srTypeEvidence :: !(Maybe TypeEvidence)
  }
  deriving stock (Eq, Ord, Show)

type SiteRef :: Type
data SiteRef = SiteRef
  { srfRow :: !SiteRow,
    srfOccurrence :: !Int
  }
  deriving stock (Eq, Ord, Show)

type PairLedgerEntry :: Type
data PairLedgerEntry = PairLedgerEntry
  { pleLeft :: !SiteRef,
    pleRight :: !SiteRef
  }
  deriving stock (Eq, Ord, Show)

type PairLedger :: Type
newtype PairLedger = PairLedger
  { pairLedgerGroups :: Map SiteRow [PairLedgerEntry]
  }
  deriving stock (Eq, Show)

siteRow :: CandidateSite -> SiteRow
siteRow site =
  SiteRow
    { srBindingName = csBindingName site,
      srSiteKind = csSiteKind site,
      srRegion = csRegion site,
      srContext = csContext site,
      srClass = csClass site,
      srOriginalSize = csOriginalSize site,
      srSize = csSize site,
      srFreeScopeWidth = csFreeScopeWidth site,
      srTypeEvidence = csTypeEvidence site
    }

siteRowsByIdentity :: [CandidateSite] -> Map SiteRow [CandidateSite]
siteRowsByIdentity sites =
  Map.map (sortOn csOrdinal) $
    Map.fromListWith (<>) [(siteRow site, [site]) | site <- sites]

siteRefsByIdentity :: [CandidateSite] -> [(CandidateSite, SiteRef)]
siteRefsByIdentity sites =
  [ (site, SiteRef rowValue occurrence)
  | (rowValue, rowMembers) <- Map.toAscList (siteRowsByIdentity sites),
    (occurrence, site) <- zip [0 ..] rowMembers
  ]

groupLedgerKey :: [CandidateSite] -> Maybe SiteRow
groupLedgerKey =
  foldr keepLeast Nothing
  where
    keepLeast site currentLeast =
      Just (maybe (siteRow site) (min (siteRow site)) currentLeast)

buildGroupPairs :: Int -> [CandidateSite] -> [PairLedgerEntry]
buildGroupPairs pairLimit groupSites
  | pairLimit <= 0 =
      []
  | otherwise =
      Map.elems (foldl' admitTail Map.empty (tails referencedSites))
  where
    referencedSites =
      sortOn (csOrdinal . fst) (siteRefsByIdentity groupSites)

    admitTail kept referencedTail =
      case referencedTail of
        [] ->
          kept
        leftReferenced : laterReferenced ->
          foldl' (admitPair leftReferenced) kept laterReferenced

    admitPair (leftSite, leftRef) kept (rightSite, rightRef)
      | csOrdinal leftSite /= csOrdinal rightSite,
        csClass leftSite /= csClass rightSite =
          admitKeyed
            (sitePairKey leftSite rightSite)
            (PairLedgerEntry leftRef rightRef)
            kept
      | otherwise =
          kept

    admitKeyed pairKey entry kept
      | Map.size kept < pairLimit =
          Map.insert pairKey entry kept
      | otherwise =
          case Map.lookupMax kept of
            Just (worstKey, _)
              | pairKey < worstKey ->
                  Map.insert pairKey entry (Map.deleteMax kept)
            _ ->
              kept

buildPairLedger :: Int -> [[CandidateSite]] -> PairLedger
buildPairLedger pairLimit groups =
  PairLedger
    ( Map.fromList
        [ (keyValue, buildGroupPairs pairLimit groupSites)
        | groupSites <- groups,
          keyValue <- maybe [] pure (groupLedgerKey groupSites)
        ]
    )

advancePairLedger ::
  Int ->
  [[CandidateSite]] ->
  [[CandidateSite]] ->
  PairLedger ->
  PairLedger
advancePairLedger pairLimit unaffectedGroups affectedGroups previousLedger =
  PairLedger (Map.fromList (carriedRows <> freshRows))
  where
    carriedRows =
      [ ( keyValue,
          Map.findWithDefault
            (buildGroupPairs pairLimit groupSites)
            keyValue
            (pairLedgerGroups previousLedger)
        )
      | groupSites <- unaffectedGroups,
        keyValue <- maybe [] pure (groupLedgerKey groupSites)
      ]

    freshRows =
      [ (keyValue, buildGroupPairs pairLimit groupSites)
      | groupSites <- affectedGroups,
        keyValue <- maybe [] pure (groupLedgerKey groupSites)
      ]

admittedSitePairs ::
  Int ->
  [CandidateSite] ->
  PairLedger ->
  [(CandidateSite, CandidateSite)]
admittedSitePairs pairLimit sites ledger
  | pairLimit <= 0 =
      []
  | otherwise =
      take pairLimit (sortOn (uncurry sitePairKey) resolvedPairs)
  where
    rowMembers =
      siteRowsByIdentity sites

    resolveRef ref =
      case drop (srfOccurrence ref) (Map.findWithDefault [] (srfRow ref) rowMembers) of
        site : _ ->
          Just site
        [] ->
          Nothing

    resolvedPairs =
      [ (leftSite, rightSite)
      | entries <- Map.elems (pairLedgerGroups ledger),
        entry <- entries,
        Just leftSite <- pure (resolveRef (pleLeft entry)),
        Just rightSite <- pure (resolveRef (pleRight entry))
      ]

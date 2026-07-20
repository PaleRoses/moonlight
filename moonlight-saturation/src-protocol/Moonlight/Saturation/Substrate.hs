{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Moonlight.Saturation.Substrate
  ( module Moonlight.Saturation.Substrate.Types,
    ContextLattice (..),
    SupportBasis,
    trivialLattice,
    SaturationGraph (..),
    BaseGraphEmbedding (..),
    CapabilitySystem (..),
    QueryIndex (..),
    FactSystem (..),
    RewriteSystem (..),
    MatchView (..),
    SupportedMatchMap,
    insertSupportedMatch,
    MatchingBackend (..),
    ApplicationResultSystem (..),
    GraphApply (..),
    FactViewGraphChanges (..),
    RebuildSystem (..),
    ProofCarrier (..),
    contextSupportedMatchesPreparedViaContexts,
    runSingleMatchingRequest,
  )
where

import Data.Foldable (foldlM)
import Data.IntMap.Strict (IntMap)
import Data.Kind (Constraint, Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Moonlight.Core (RewriteRuleId, SupportIndexedRule (..))
import Moonlight.Core (QueryId)
import Moonlight.Saturation.Matching
  ( QueryFingerprint,
  )
import Moonlight.Core (Substitution)
import Moonlight.Saturation.Core.Outcome
  ( ApplyOutcome,
  )
import Moonlight.Saturation.Substrate.Types
import Moonlight.Sheaf.Context.Site
  ( PreparedContextSite,
    SupportCarrier,
    contextFragmentObjects,
    contextObjectKeyFor,
    preparedContextFragment,
    preparedDefaultContext,
    supportCarrierContainsKey,
  )
import Moonlight.FiniteLattice
  ( ContextLattice (..),
    singletonContextLattice
  )
import Moonlight.FiniteLattice
  ( SupportBasis
  )


trivialLattice :: ContextLattice TrivialContext
trivialLattice =
  singletonContextLattice ()
{-# INLINE trivialLattice #-}

type FactViewGraphChanges :: Type -> Type
data FactViewGraphChanges context = FactViewGraphChanges
  { fvgcBaseChanged :: !Bool,
    fvgcChangedFiberAuthors :: !(Set context)
  }
  deriving stock (Eq, Show)

instance Ord context => Semigroup (FactViewGraphChanges context) where
  leftChanges <> rightChanges =
    FactViewGraphChanges
      { fvgcBaseChanged =
          fvgcBaseChanged leftChanges || fvgcBaseChanged rightChanges,
        fvgcChangedFiberAuthors =
          fvgcChangedFiberAuthors leftChanges
            <> fvgcChangedFiberAuthors rightChanges
      }

instance Ord context => Monoid (FactViewGraphChanges context) where
  mempty =
    FactViewGraphChanges
      { fvgcBaseChanged = False,
        fvgcChangedFiberAuthors = mempty
      }

type SaturationGraph :: Type -> Constraint
class SaturationGraph u where
  graphCanonicalizeClass ::
    SatClassId u ->
    SatGraph u ->
    SatClassId u

  graphClassCount ::
    SatGraph u ->
    Int

  graphNodeCount ::
    SatGraph u ->
    Int

  graphBase ::
    SatGraph u ->
    SatBaseGraph u

  baseGraphEquals ::
    SatBaseGraph u ->
    SatBaseGraph u ->
    Bool

  graphPreparedSite ::
    SatGraph u ->
    PreparedContextSite (SatContextOwner u) (SatContext u)

  graphBaseContext ::
    SatGraph u ->
    SatContext u
  graphBaseContext =
    preparedDefaultContext . graphPreparedSite @u

  graphContextLattice ::
    SatGraph u ->
    ContextLattice (SatContext u)

  graphExecutionContexts ::
    SatGraph u ->
    [SatContext u]
  graphExecutionContexts =
    contextFragmentObjects . preparedContextFragment . graphPreparedSite @u

  graphPendingMerges ::
    SatGraph u ->
    Int

  graphConvergenceStateEquals ::
    SatGraph u ->
    SatGraph u ->
    Bool

  graphContextClassProjection ::
    SatContext u ->
    SatGraph u ->
    Either (SatObstruction u) (IntMap (SatClassId u))

  graphContextClasses ::
    SatContext u ->
    SatGraph u ->
    Either (SatObstruction u) (Set (SatClassId u))

type BaseGraphEmbedding :: Type -> Type -> Constraint
class
  ( SaturationGraph u,
    SatContext u ~ TrivialContext
  ) =>
  BaseGraphEmbedding u carrier
  where
  embedBaseGraph ::
    SatBaseGraph u ->
    carrier

type CapabilitySystem :: Type -> Constraint
class SaturationGraph u => CapabilitySystem u where
  emptyCapabilityResolver ::
    SatCapabilityResolver u

type QueryIndex :: Type -> Constraint
class (SaturationGraph u, Monoid (SatMatchingDelta u)) => QueryIndex u where
  queryFingerprint ::
    SatQuery u ->
    Either (SatObstruction u) QueryFingerprint

  matchSnapshotKey ::
    SatMatchSnapshot u ->
    QueryFingerprint

  fullMatchingDelta ::
    SatMatchingDelta u

  registerQueries ::
    [SatQuery u] ->
    SatGraph u ->
    Either (SatObstruction u) (SatGraph u)

  contextMatchSections ::
    SatGraph u ->
    Map (SatContext u) (SatMatchSection u)

  lookupQueryId ::
    QueryFingerprint ->
    SatGraph u ->
    Maybe QueryId

type FactSystem :: Type -> Constraint
class (QueryIndex u, CapabilitySystem u) => FactSystem u where
  type SatFactRuleIdentity u :: Type

  emptyFactStore ::
    SatFactStore u

  emptyFactIndex ::
    SatFactIndex u

  canonicalizeFactStore ::
    SatGraph u ->
    SatFactStore u ->
    SatFactStore u

  canonicalizeFactIndex ::
    SatGraph u ->
    SatFactIndex u ->
    SatFactIndex u

  canonicalizeFactStoreBase ::
    SatBaseGraph u ->
    SatFactStore u ->
    SatFactStore u

  canonicalizeFactIndexBase ::
    SatBaseGraph u ->
    SatFactIndex u ->
    SatFactIndex u

  -- | Canonicalize facts against the quotient represented by one prepared
  -- context.  This is deliberately a graph-level operation: contextual
  -- backends must answer through their native view and may not manufacture a
  -- copied base graph merely to reuse 'canonicalizeFactStoreBase'.
  canonicalizeFactStoreAtContext ::
    SatContext u ->
    SatGraph u ->
    SatFactStore u ->
    Either (SatObstruction u) (SatFactStore u)

  -- | Canonicalize derivation indices against the same contextual quotient.
  canonicalizeFactIndexAtContext ::
    SatContext u ->
    SatGraph u ->
    SatFactIndex u ->
    Either (SatObstruction u) (SatFactIndex u)

  unionFactStores ::
    SatFactStore u ->
    SatFactStore u ->
    SatFactStore u

  factChangeMatchingDelta ::
    SatGraph u ->
    Map (SatContext u) (SatFactStore u) ->
    Map (SatContext u) (SatFactStore u) ->
    SatMatchingDelta u

  compileFactRules ::
    [SatFactSource u] ->
    Either (SatFactCompileError u) [SatFactRule u]

  factRuleQuery ::
    SatFactRule u ->
    SatQuery u

  factRuleId ::
    SatFactRule u ->
    RewriteRuleId

  factRuleIdentity ::
    SatFactRule u ->
    Either (SatObstruction u) (SatFactRuleIdentity u)

  factSourceId ::
    SatFactSource u ->
    RewriteRuleId

  deriveFactClosure ::
    SatCapabilityResolver u ->
    SatFactStore u ->
    [SatFactRule u] ->
    SatBaseGraph u ->
    SatFactStore u ->
    SatFactIndex u ->
    Either
      (SatObstruction u)
      (SatFactStore u, SatFactIndex u, [SatFactRound u])

  -- | Fact closure at a context of the shared substrate.  There is no
  -- materialized-graph default: every backend must give the contextual view
  -- an explicit semantics.
  deriveFactClosureAtContext ::
    SatCapabilityResolver u ->
    SatFactStore u ->
    [SatFactRule u] ->
    SatGraph u ->
    SatContext u ->
    SatFactStore u ->
    SatFactIndex u ->
    Either
      (SatObstruction u)
      (SatFactStore u, SatFactIndex u, [SatFactRound u])
  -- | Fact closures for a family of contexts of the shared substrate, each
  -- seeded from its own input store. The default derives each context
  -- independently; backends able to share matching work across contexts
  -- override it.
  deriveFactClosuresAtContexts ::
    Ord (SatContext u) =>
    SatCapabilityResolver u ->
    SatGraph u ->
    Map (SatContext u) (SatFactStore u, [SatFactRule u]) ->
    Either
      (SatObstruction u)
      (Map (SatContext u) (SatFactStore u, SatFactIndex u, [SatFactRound u]))
  deriveFactClosuresAtContexts capabilityResolver graph =
    Map.traverseWithKey
      ( \contextValue (contextFactStore, factRules) ->
          deriveFactClosureAtContext @u
            capabilityResolver
            contextFactStore
            factRules
            graph
            contextValue
            contextFactStore
            (emptyFactIndex @u)
      )

type RewriteSystem :: Type -> Constraint
class (QueryIndex u, CapabilitySystem u) => RewriteSystem u where
  type SatRewriteRuleIdentity u :: Type

  compileRewriteRules ::
    [SatRuleSource u] ->
    Either (SatRuleCompileError u) [SatRule u]

  rewriteRuleSourceId ::
    SatRuleSource u ->
    RewriteRuleId

  rewriteRuleId ::
    SatRule u ->
    RewriteRuleId

  rewriteRuleIdentity ::
    SatRule u ->
    Either (SatObstruction u) (SatRewriteRuleIdentity u)

  rewriteRuleKey ::
    SatRule u ->
    SatRuleKey u

  rewriteRuleQuery ::
    SatRule u ->
    SatQuery u

  defaultRewriteContext ::
    SatRewriteContext u

  rewriteCapabilityResolver ::
    SatRewriteContext u ->
    SatGraph u ->
    SatCapabilityResolver u

type MatchView :: Type -> Constraint
class RewriteSystem u => MatchView u where
  matchKey ::
    SatMatch u ->
    (SatRuleKey u, SatClassId u, Substitution)

  matchRuleKey ::
    SatMatch u ->
    SatRuleKey u

  supportedMatchInner ::
    SatSupportedMatch u ->
    SatMatch u

  setSupportedMatchInner ::
    SatMatch u ->
    SatSupportedMatch u ->
    SatSupportedMatch u

  supportedMatchBasis ::
    SatSupportedMatch u ->
    SupportBasis (SatContext u)

  supportedMatchWitnesses ::
    SatSupportedMatch u ->
    Map (SatContext u) (SatSupportWitness u)

  mergeSupportedMatch ::
    SatGraph u ->
    SatSupportedMatch u ->
    SatSupportedMatch u ->
    Either (SatObstruction u) (SatSupportedMatch u)

type SupportedMatchMap :: Type -> Type
type SupportedMatchMap u =
  Map (SatRuleKey u, SatClassId u, Substitution) (SatSupportedMatch u)

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

type MatchingBackend :: Type -> Constraint
class (FactSystem u, RewriteSystem u, MatchView u) => MatchingBackend u where
  initialMatchState ::
    SatMatchStrategy u ->
    SatRewriteContext u ->
    SatMatchState u

  runMatchingRequests ::
    SatMatchingDelta u ->
    SatMatchWorld u ->
    [SatMatchingRequest u] ->
    SatMatchState u ->
    (SatMatchState u, Either (SatObstruction u) [[SatRequestMatch u]])

  materializeRawMatch ::
    SatRewriteContext u ->
    SatCapabilityResolver u ->
    SatContext u ->
    SatFactStore u ->
    SatFactIndex u ->
    SatBaseGraph u ->
    SatRawMatch u ->
    Either (SatRawMatchRejection u) (SatSupportedMatch u)

  -- | Materialize a context site's raw matches against the backend's native
  -- contextual quotient.  No default exists because a base-graph fallback
  -- would erase the very context this method is required to preserve.
  materializeRawMatchesAtContextView ::
    SatRewriteContext u ->
    SatCapabilityResolver u ->
    SatContext u ->
    SatFactStore u ->
    SatFactIndex u ->
    SatGraph u ->
    [SatRawMatch u] ->
    Either (SatObstruction u) [SatSupportedMatch u]
  rawBaseMatchesPrepared ::
    SatRewriteContext u ->
    Int ->
    SatMatchingDelta u ->
    SatGraph u ->
    SatFactStore u ->
    [SatRule u] ->
    SatMatchState u ->
    Either (SatObstruction u) (SatMatchState u, [SatRawMatch u])

  rawContextMatchesPrepared ::
    SatRewriteContext u ->
    SatContext u ->
    Int ->
    SatMatchingDelta u ->
    SatGraph u ->
    SatFactStore u ->
    SatFactIndex u ->
    [SatRule u] ->
    SatMatchState u ->
    Either (SatObstruction u) (SatMatchState u, [SatRawMatch u])

  contextSupportedMatchesPrepared ::
    ( Ord (SatRuleKey u),
      Ord (SatContext u),
      Ord (SatClassId u)
    ) =>
    SatRewriteContext u ->
    SatCapabilityResolver u ->
    Int ->
    SatMatchingDelta u ->
    SatGraph u ->
    Map (SatContext u) (SatFactStore u, SatFactIndex u, [SatRule u]) ->
    [(SupportIndexedRule (SupportBasis (SatContext u)) (SatRule u), SupportCarrier (SatContextOwner u) (SatContext u))] ->
    SatMatchState u ->
    Either (SatObstruction u) (SatMatchState u, [SatSupportedMatch u])
  contextSupportedMatchesPrepared =
    contextSupportedMatchesPreparedViaContexts @u

  consumedDerivations ::
    SatSupportedMatch u ->
    SatFactIndex u

  rawMatchRuleKey ::
    SatRawMatch u ->
    SatRuleKey u

  filterSupportedMatches ::
    SatRewriteContext u ->
    SatFactStore u ->
    SatMatchState u ->
    [(annotation, SatSupportedMatch u)] ->
    SatGraph u ->
    [(annotation, SatSupportedMatch u)]

  advanceMatchStateForRound ::
    SatMatchingDelta u ->
    SatGraph u ->
    SatMatchState u ->
    SatMatchState u

  advanceMatchStateAfterRebuild ::
    SatRebuild u ->
    SatMatchState u ->
    SatMatchState u

  recordScheduledMatches ::
    [SatSupportedMatch u] ->
    SatMatchState u ->
    SatMatchState u

  recordApplicationResult ::
    SatGraph u ->
    SatApplicationResult u ->
    SatMatchState u ->
    SatMatchState u

type ApplicationResultSystem :: Type -> Constraint
class MatchView u => ApplicationResultSystem u where
  applicationResultCount ::
    SatApplicationResult u ->
    Int

type GraphApply :: Type -> Constraint
class (FactSystem u, ApplicationResultSystem u) => GraphApply u where
  applyBaseMatches ::
    SatRewriteContext u ->
    SatFactStore u ->
    [SatSupportedMatch u] ->
    SatGraph u ->
    Either
      (SatApplicationError u)
      (ApplyOutcome (SatApplicationResult u) (SatGraph u))

  applyContextualMatches ::
    SatRewriteContext u ->
    [SatSupportedMatch u] ->
    SatGraph u ->
    Either
      (SatApplicationError u)
      (ApplyOutcome (SatApplicationResult u) (SatGraph u))

type RebuildSystem :: Type -> Constraint
class (MatchingBackend u, Monoid (SatChangeSummary u)) => RebuildSystem u where
  rebuildGraph ::
    SatGraph u ->
    SatFactStore u ->
    SatFactIndex u ->
    Either (SatObstruction u) (SatGraph u, SatRebuild u)

  rebuildEpoch ::
    SatRebuild u ->
    Int

  rebuildMatchingDelta ::
    SatRebuild u ->
    SatMatchingDelta u

  factViewGraphChanges ::
    SatChangeSummary u ->
    FactViewGraphChanges (SatContext u)

  postApplyMatchingDelta ::
    SatMatchState u ->
    [SatSupportedMatch u] ->
    SatApplicationResult u ->
    SatRebuild u ->
    SatMatchingDelta u

  postApplyChangeSummary ::
    SatMatchState u ->
    [SatSupportedMatch u] ->
    SatApplicationResult u ->
    SatRebuild u ->
    SatChangeSummary u

type ProofCarrier :: Type -> Type -> Constraint
class (RebuildSystem u, ApplicationResultSystem u) => ProofCarrier u p where
  proofGraphContext ::
    SatProofGraph u p ->
    SatGraph u

  setProofGraphContext ::
    SatGraph u ->
    SatProofGraph u p ->
    SatProofGraph u p

  applyProofMatches ::
    SatRewriteContext u ->
    SatProofBuilder u p ->
    Maybe (SatContext u) ->
    [SatSupportedMatch u] ->
    SatProofGraph u p ->
    Either
      (SatApplicationError u)
      (ApplyOutcome (SatApplicationResult u) (SatProofGraph u p))

contextSupportedMatchesPreparedViaContexts ::
  forall u.
  ( MatchingBackend u,
    Ord (SatRuleKey u),
    Ord (SatContext u),
    Ord (SatClassId u)
  ) =>
  SatRewriteContext u ->
  SatCapabilityResolver u ->
  Int ->
  SatMatchingDelta u ->
  SatGraph u ->
  Map (SatContext u) (SatFactStore u, SatFactIndex u, [SatRule u]) ->
  [(SupportIndexedRule (SupportBasis (SatContext u)) (SatRule u), SupportCarrier (SatContextOwner u) (SatContext u))] ->
  SatMatchState u ->
  Either (SatObstruction u) (SatMatchState u, [SatSupportedMatch u])
contextSupportedMatchesPreparedViaContexts rewriteContext capabilityResolver iterationIndex matchingDelta graph contextInputs supportedRules startingMatchState =
  fmap
    (\(nextMatchState, supportedMatchMap) -> (nextMatchState, Map.elems supportedMatchMap))
    ( foldlM
        contextStep
        (startingMatchState, Map.empty)
        (Map.toAscList inputsWithSupportedRules)
    )
  where
    inputsWithSupportedRules =
      Map.mapWithKey
        (\contextValue (factStore, factIndex, directRules) ->
            ( factStore,
              factIndex,
              directRules
                <> [ sirRule indexedRule
                     | (indexedRule, carrier) <- supportedRules,
                       Right contextKey <- [contextObjectKeyFor (graphPreparedSite @u graph) contextValue],
                       supportCarrierContainsKey (graphPreparedSite @u graph) carrier contextKey
                   ]
            )
        )
        contextInputs

    contextStep ::
      ( SatMatchState u,
        SupportedMatchMap u
      ) ->
      (SatContext u, (SatFactStore u, SatFactIndex u, [SatRule u])) ->
      Either
        (SatObstruction u)
        ( SatMatchState u,
          SupportedMatchMap u
        )
    contextStep (currentMatchState, supportedMatchMap) (contextValue, (factStore, factIndex, rules)) = do
      (nextMatchState, rawMatches) <-
        rawContextMatchesPrepared @u
          rewriteContext
          contextValue
          iterationIndex
          matchingDelta
          graph
          factStore
          factIndex
          rules
          currentMatchState
      supportedMatches <-
        materializeRawMatchesAtContextView @u
          rewriteContext
          capabilityResolver
          contextValue
          factStore
          factIndex
          graph
          rawMatches
      nextSupportedMap <-
        foldlM
          (insertSupportedMatch @u graph)
          supportedMatchMap
          supportedMatches
      pure (nextMatchState, nextSupportedMap)

runSingleMatchingRequest ::
  (Int -> SatObstruction u) ->
  ( SatMatchingDelta u ->
    SatMatchWorld u ->
    [SatMatchingRequest u] ->
    SatMatchState u ->
    (SatMatchState u, Either (SatObstruction u) [[SatRequestMatch u]])
  ) ->
  SatMatchingDelta u ->
  SatMatchWorld u ->
  SatMatchingRequest u ->
  SatMatchState u ->
  (SatMatchState u, Either (SatObstruction u) [SatRequestMatch u])
runSingleMatchingRequest arityObstruction runBatch matchingDelta matchWorld request initialState =
  case runBatch matchingDelta matchWorld [request] initialState of
    (nextState, Left obstruction) ->
      (nextState, Left obstruction)
    (nextState, Right [matches]) ->
      (nextState, Right matches)
    (nextState, Right matchesByRequest) ->
      (nextState, Left (arityObstruction (length matchesByRequest)))
{-# INLINE runSingleMatchingRequest #-}

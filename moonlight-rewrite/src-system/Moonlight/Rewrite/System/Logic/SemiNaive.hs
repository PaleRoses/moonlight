{-# LANGUAGE DerivingStrategies #-}

-- | Inflationary semi-naive closure driver for system facts.
-- Owns seeded closure state, retained rounds, limit checks, and the
-- watched-literal engine that carries matches across rounds.
-- Contracts: facts are never retracted, seeds canonicalize only at entry,
-- structural matching runs once per rule at entry, and per-round deltas wake
-- only the ground literals that watch them.
module Moonlight.Rewrite.System.Logic.SemiNaive
  ( SemiNaiveRound (..),
    SemiNaiveClosure (..),
    SemiNaiveMatcher,
    mkSemiNaiveMatcher,
    runSemiNaiveMatcher,
    FactClosureRun (..),
    deriveSeededFactClosureWithStateAndConfig,
    module Moonlight.Rewrite.System.Logic.SemiNaive.Config,
    module Moonlight.Rewrite.System.Logic.SemiNaive.Input,
  )
where

import Data.Bifunctor (first)
import Data.Kind (Type)
import Moonlight.Core (ClassId, Substitution)
import Moonlight.Rewrite.System.Logic.Guard
  ( GuardCapabilityResolver,
    GuardTerm,
  )
import Moonlight.Rewrite.System.Logic.Rule
  ( CompiledFactRule,
    FactDerivationIndex,
    canonicalizeFactDerivationIndex,
    differenceFactDerivationIndexes,
    factDerivationCount,
    factStoreFromDerivations,
    mkSemiNaiveMatcher,
    nullFactDerivationIndex,
    runSemiNaiveMatcher,
    SemiNaiveMatcher,
  )
import Moonlight.Rewrite.System.Logic.SemiNaive.Engine
  ( initFactClosureEngine,
    stepFactClosureEngine,
  )
import Moonlight.Rewrite.System.Logic.SemiNaive.Config
import Moonlight.Rewrite.System.Logic.SemiNaive.Input
import Moonlight.Rewrite.System.Logic.Store
  ( FactStore,
    canonicalizeFactStore,
    differenceFactStores,
    factStoreSize,
    nullFactStore,
    unionFactStores,
  )

type SemiNaiveRound :: Type
data SemiNaiveRound = SemiNaiveRound
  { snrRoundIndex :: {-# UNPACK #-} !Int,
    snrDeltaFacts :: !FactStore,
    snrDeltaDerivations :: !FactDerivationIndex
  }
  deriving stock (Eq, Show)

type SemiNaiveClosure :: Type
data SemiNaiveClosure = SemiNaiveClosure
  { sncFacts :: !FactStore,
    sncDerivations :: !FactDerivationIndex,
    sncRounds :: ![SemiNaiveRound]
  }
  deriving stock (Eq, Show)

type FactClosureRun :: Type -> Type -> Type -> (Type -> Type) -> Type -> Type -> Type
data FactClosureRun capability state root f host obstruction = FactClosureRun
  { fcrConfig :: !SemiNaiveConfig,
    fcrCapabilityResolver :: !(GuardCapabilityResolver capability),
    fcrInitialFacts :: !FactStore,
    fcrSeedDerivations :: !FactDerivationIndex,
    fcrInitialState :: !state,
    fcrMatcher :: !(SemiNaiveMatcher capability state root f host obstruction),
    fcrResolveTerm :: !(root -> Substitution -> GuardTerm f -> Maybe ClassId),
    fcrCanonicalClass :: !(ClassId -> ClassId),
    fcrRules :: ![CompiledFactRule capability f],
    fcrHost :: !host
  }

-- | Inflationary semi-naive closure over the fact rules. Guards are CNF with
-- polarity and facts are never retracted, so this is an inflationary fixpoint
-- rather than stratified Datalog: accumulated facts and derivations are never
-- withdrawn. The host, resolver, and canonicalizer are fixed for the whole
-- closure and matching never reads the fact store, so structural matching
-- runs exactly once per rule at entry; each match's guard is grounded into
-- watched fact literals, and later rounds wake only the matches whose
-- watched witnesses appear in the delta. Positive literals flip to satisfied
-- when their witness arrives and negative literals flip to unsatisfied, each
-- at most once, so total propagation work is linear in ground literals.
-- Seeds and initial facts are canonicalized once at entry; every derivation
-- is canonical by construction, so no per-round re-canonicalization occurs.
deriveSeededFactClosureWithStateAndConfig ::
  FactClosureRun capability state root f host obstruction ->
  Either (FactClosureRunError obstruction) (state, SemiNaiveClosure)
deriveSeededFactClosureWithStateAndConfig run =
  let canonicalizeClassId =
        fcrCanonicalClass run
      canonicalSeedDerivations =
        canonicalizeFactDerivationIndex canonicalizeClassId (fcrSeedDerivations run)
      canonicalInitialFacts =
        canonicalizeFactStore
          canonicalizeClassId
          ( unionFactStores
              (fcrInitialFacts run)
              (factStoreFromDerivations canonicalSeedDerivations)
          )
      initialInput =
        initialSemiNaiveInput canonicalInitialFacts canonicalSeedDerivations
      initialDerivationCount =
        factDerivationCount canonicalSeedDerivations
      initialStats =
        FactClosureStats
          { fcsRoundsCompleted = 0,
            fcsFactCount = factStoreSize canonicalInitialFacts,
            fcsDeltaFactCount = factStoreSize canonicalInitialFacts,
            fcsDerivationCount = initialDerivationCount,
            fcsDeltaDerivationCount = initialDerivationCount
          }
   in case checkFactClosureLimits (sncLimits (fcrConfig run)) initialStats of
        Left limitError ->
          Left limitError
        Right () ->
          let (matcherState, initResult) =
                initFactClosureEngine
                  (fcrCapabilityResolver run)
                  initialInput
                  (fcrInitialState run)
                  (fcrMatcher run)
                  (fcrResolveTerm run)
                  (fcrCanonicalClass run)
                  (fcrRules run)
                  (fcrHost run)
           in case first FactClosureMatcherError initResult of
                Left matcherError ->
                  Left matcherError
                Right (engine, initialCandidates) ->
                  go
                    matcherState
                    engine
                    initialInput
                    initialCandidates
                    (emptyRoundAccumulator (sncRoundRetention (fcrConfig run)))
  where
    go matcherState engine input candidateDerivations retained =
      let deltaDerivations =
            differenceFactDerivationIndexes
              candidateDerivations
              (sniAllDerivations input)

          deltaFacts =
            differenceFactStores
              (factStoreFromDerivations deltaDerivations)
              (sniAllFacts input)
       in if nullFactStore deltaFacts && nullFactDerivationIndex deltaDerivations
            then
              Right
                ( matcherState,
                  SemiNaiveClosure
                    { sncFacts = sniAllFacts input,
                      sncDerivations = sniAllDerivations input,
                      sncRounds = retainedRounds retained
                    }
                )
            else
              let nextFacts =
                    unionFactStores (sniAllFacts input) deltaFacts
                  nextDerivations =
                    sniAllDerivations input <> deltaDerivations
                  completedRounds =
                    sniRoundIndex input + 1
                  stats =
                    FactClosureStats
                      { fcsRoundsCompleted = completedRounds,
                        fcsFactCount = factStoreSize nextFacts,
                        fcsDeltaFactCount = factStoreSize deltaFacts,
                        fcsDerivationCount = factDerivationCount nextDerivations,
                        fcsDeltaDerivationCount = factDerivationCount deltaDerivations
                      }
                  roundValue =
                    SemiNaiveRound
                      { snrRoundIndex = sniRoundIndex input,
                        snrDeltaFacts = deltaFacts,
                        snrDeltaDerivations = deltaDerivations
                      }
                  nextInput =
                    advanceSemiNaiveInput
                      nextFacts
                      deltaFacts
                      nextDerivations
                      deltaDerivations
                      input
                  (nextEngine, nextCandidates) =
                    stepFactClosureEngine deltaFacts engine
               in case checkFactClosureLimits (sncLimits (fcrConfig run)) stats of
                    Left limitError ->
                      Left limitError
                    Right () ->
                      go
                        matcherState
                        nextEngine
                        nextInput
                        nextCandidates
                        (recordRetainedRound roundValue retained)

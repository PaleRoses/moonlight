{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

-- | Watched-literal propagation engine for the fact closure.
-- Owns per-match ground guards, clause satisfaction counts, and the
-- witness wake index that routes delta facts to affected matches.
-- Contracts: the host, resolver, and canonicalizer are fixed for a whole
-- closure, so structural matching runs exactly once per rule; facts are never
-- retracted, so every ground fact literal flips at most once; emissions
-- reproduce the guard evaluator's evidence at the post-delta store.
module Moonlight.Rewrite.System.Logic.SemiNaive.Engine
  ( FactClosureEngine,
    initFactClosureEngine,
    stepFactClosureEngine,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Traversable (mapAccumL)
import Moonlight.Core (ClassId, Substitution)
import Moonlight.Rewrite.Algebra (cpqCondition)
import Moonlight.Rewrite.System.Logic.Guard
  ( GroundGuardLiteral (..),
    GuardCapabilityResolver,
    GuardTerm,
    groundCompiledGuard,
    guardRefTerm,
  )
import Moonlight.Rewrite.System.Logic.Rule
  ( CompiledFactRule (..),
    FactDerivation (..),
    FactDerivationIndex,
    FactRuleId,
    SemiNaiveMatcher,
    emptyFactDerivationIndex,
    runSemiNaiveMatcher,
    singletonFactDerivationIndex,
  )
import Moonlight.Rewrite.System.Logic.SemiNaive.Input (SemiNaiveInput (..))
import Moonlight.Rewrite.System.Logic.Store
  ( FactStore,
    FactTuple (..),
    FactWitness (..),
    GuardClauseEvidence (..),
    GuardLiteralEvidence (..),
    factWitnesses,
    guardEvidenceFromClauses,
    hasFact,
  )

type EngineSlot :: Type
data EngineSlot
  = SlotStatic ![GuardLiteralEvidence]
  | SlotFact !Bool !FactWitness !Bool
  deriving stock (Eq, Show)

type EngineClause :: Type
data EngineClause = EngineClause
  { ecSlots :: !(IntMap EngineSlot),
    ecSatisfiedCount :: {-# UNPACK #-} !Int,
    ecPendingPositive :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Show)

type EngineMatch :: Type
data EngineMatch = EngineMatch
  { emRuleId :: !FactRuleId,
    emRuleName :: !String,
    emFactWitness :: !FactWitness,
    emClauses :: !(IntMap EngineClause),
    emUnsatisfiedClauses :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Show)

type WatchTarget :: Type
data WatchTarget = WatchTarget
  { wtMatch :: {-# UNPACK #-} !Int,
    wtClause :: {-# UNPACK #-} !Int,
    wtSlot :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Show)

-- | Closure-lifetime propagation state: the surviving conditioned matches and
-- the wake index from fact witnesses to the ground literals watching them.
-- Watch entries are consumed on arrival and matches whose guards become
-- permanently unsatisfiable are pruned, so both maps shrink monotonically
-- after initialization.
type FactClosureEngine :: Type
data FactClosureEngine = FactClosureEngine
  { fceMatches :: !(IntMap EngineMatch),
    fceWatchers :: !(Map FactWitness [WatchTarget])
  }
  deriving stock (Eq, Show)

-- | Match every rule once against the fixed host, ground each match's guard,
-- and evaluate it against the initial store. Returns the engine holding all
-- matches that can still change verdicts, together with the derivations
-- already satisfied at the initial store. Unconditioned matches and matches
-- whose guards hold no fact literals emit here and are never stored; matches
-- containing a permanently unsatisfiable clause are discarded outright.
initFactClosureEngine ::
  GuardCapabilityResolver capability ->
  SemiNaiveInput FactStore FactDerivationIndex ->
  state ->
  SemiNaiveMatcher capability state root f host obstruction ->
  (root -> Substitution -> GuardTerm f -> Maybe ClassId) ->
  (ClassId -> ClassId) ->
  [CompiledFactRule capability f] ->
  host ->
  (state, Either obstruction (FactClosureEngine, FactDerivationIndex))
initFactClosureEngine capabilityResolver input initialState matcher resolveTerm canonicalizeClassId compiledRules host =
  ( finalState,
    maybe
      (Right (engine, emissions))
      Left
      maybeObstruction
  )
  where
    initialFacts =
      sniAllFacts input

    ((finalState, maybeObstruction), ruleMatches) =
      mapAccumL step (initialState, Nothing) compiledRules

    step (currentState, Just obstruction) _ =
      ((currentState, Just obstruction), [])
    step (currentState, Nothing) compiledRule =
      case runSemiNaiveMatcher matcher currentState input compiledRule host of
        (nextState, Left obstruction) ->
          ((nextState, Just obstruction), [])
        (nextState, Right matches) ->
          ((nextState, Nothing), fmap ((,) compiledRule) matches)

    (engine, emissions, _) =
      foldl'
        insertMatch
        (FactClosureEngine IntMap.empty Map.empty, emptyFactDerivationIndex, 0)
        (concat ruleMatches)

    insertMatch (currentEngine, currentEmissions, nextKey) (compiledRule, (rootValue, substitution)) =
      case groundMatch capabilityResolver resolveTerm canonicalizeClassId initialFacts compiledRule rootValue substitution of
        GroundDead ->
          (currentEngine, currentEmissions, nextKey)
        GroundSettled derivation ->
          (currentEngine, currentEmissions <> singletonFactDerivationIndex derivation, nextKey)
        GroundLive engineMatch watchSlots ->
          let watchedEngine =
                FactClosureEngine
                  { fceMatches = IntMap.insert nextKey engineMatch (fceMatches currentEngine),
                    fceWatchers =
                      foldl'
                        ( \watchers (clauseKey, slotKey, factWitness) ->
                            Map.insertWith
                              (<>)
                              factWitness
                              [WatchTarget nextKey clauseKey slotKey]
                              watchers
                        )
                        (fceWatchers currentEngine)
                        watchSlots
                  }
              settledEmissions =
                if emUnsatisfiedClauses engineMatch == 0
                  then currentEmissions <> singletonFactDerivationIndex (matchDerivation engineMatch)
                  else currentEmissions
           in (watchedEngine, settledEmissions, nextKey + 1)

type GroundOutcome :: Type
data GroundOutcome
  = GroundDead
  | GroundSettled !FactDerivation
  | GroundLive !EngineMatch ![(Int, Int, FactWitness)]

groundMatch ::
  GuardCapabilityResolver capability ->
  (root -> Substitution -> GuardTerm f -> Maybe ClassId) ->
  (ClassId -> ClassId) ->
  FactStore ->
  CompiledFactRule capability f ->
  root ->
  Substitution ->
  GroundOutcome
groundMatch capabilityResolver resolveTerm canonicalizeClassId initialFacts compiledRule rootValue substitution =
  case projectionWitness of
    Nothing ->
      GroundDead
    Just factWitness ->
      case cpqCondition (cfrCompiledQuery compiledRule) of
        Nothing ->
          GroundSettled (derivationHead factWitness Nothing)
        Just compiledGuard ->
          let groundClauses =
                fmap
                  groundClause
                  (groundCompiledGuard capabilityResolver canonicalizeClassId (resolveTerm rootValue substitution) compiledGuard)
              engineMatch =
                EngineMatch
                  { emRuleId = cfrId compiledRule,
                    emRuleName = cfrName compiledRule,
                    emFactWitness = factWitness,
                    emClauses = IntMap.fromList (zip [0 ..] groundClauses),
                    emUnsatisfiedClauses =
                      length (filter ((== 0) . ecSatisfiedCount) groundClauses)
                  }
              watchSlots =
                [ (clauseKey, slotKey, slotWitness)
                  | (clauseKey, clause) <- zip [0 ..] groundClauses,
                    (slotKey, SlotFact polarity slotWitness satisfied) <-
                      IntMap.toList (ecSlots clause),
                    satisfied /= polarity
                ]
           in if any clauseDead groundClauses
                then GroundDead
                else
                  if null watchSlots
                    then
                      if emUnsatisfiedClauses engineMatch == 0
                        then GroundSettled (matchDerivation engineMatch)
                        else GroundDead
                    else GroundLive engineMatch watchSlots
  where
    derivationHead factWitness guardEvidence =
      FactDerivation
        { fdRuleId = cfrId compiledRule,
          fdRuleName = cfrName compiledRule,
          fdFactWitness = factWitness,
          fdGuardEvidence = guardEvidence
        }

    projectionWitness =
      FactWitness (cfrFactId compiledRule) . FactTuple
        <$> traverse
          (\guardRef -> canonicalizeClassId <$> resolveTerm rootValue substitution (guardRefTerm guardRef))
          (cfrProjection compiledRule)

    groundClause groundLiterals =
      let slots =
            [ slot
              | groundLiteral <- groundLiterals,
                slot <- groundSlot groundLiteral
            ]
       in EngineClause
            { ecSlots = IntMap.fromList (zip [0 ..] slots),
              ecSatisfiedCount = length (filter slotSatisfied slots),
              ecPendingPositive =
                length
                  [ ()
                    | SlotFact True _ False <- slots
                  ]
            }

    groundSlot = \case
      GroundStaticLiteral satisfied evidences ->
        [SlotStatic evidences | satisfied]
      GroundFactLiteral polarity slotWitness ->
        let present =
              hasFact (fwFactId slotWitness) (fwTuple slotWitness) initialFacts
         in [SlotFact polarity slotWitness (polarity == present)]

    slotSatisfied = \case
      SlotStatic _ -> True
      SlotFact _ _ satisfied -> satisfied

clauseDead :: EngineClause -> Bool
clauseDead clause =
  ecSatisfiedCount clause == 0 && ecPendingPositive clause == 0

-- | Propagate one round's delta facts. Every consumed watch entry flips its
-- ground literal to the terminal truth an arrival implies; all flips land
-- before any evidence is assembled, so touched matches report exactly the
-- evaluator's verdict at the post-delta store. Matches left with a dead
-- clause are pruned; stale watch targets to pruned matches are skipped.
stepFactClosureEngine ::
  FactStore ->
  FactClosureEngine ->
  (FactClosureEngine, FactDerivationIndex)
stepFactClosureEngine deltaFacts engine =
  (prunedEngine, emissions)
  where
    (firedTargets, remainingWatchers) =
      Set.foldl'
        ( \(targets, watchers) factWitness ->
            case Map.lookup factWitness watchers of
              Nothing ->
                (targets, watchers)
              Just entries ->
                (entries <> targets, Map.delete factWitness watchers)
        )
        ([], fceWatchers engine)
        (factWitnesses deltaFacts)

    (flippedMatches, touchedKeys) =
      foldl' applyFlip (fceMatches engine, IntSet.empty) firedTargets

    (prunedMatches, emissions) =
      IntSet.foldl'
        ( \(matches, currentEmissions) matchKey ->
            case IntMap.lookup matchKey matches of
              Nothing ->
                (matches, currentEmissions)
              Just engineMatch
                | any clauseDead (emClauses engineMatch) ->
                    (IntMap.delete matchKey matches, currentEmissions)
                | emUnsatisfiedClauses engineMatch == 0 ->
                    (matches, currentEmissions <> singletonFactDerivationIndex (matchDerivation engineMatch))
                | otherwise ->
                    (matches, currentEmissions)
        )
        (flippedMatches, emptyFactDerivationIndex)
        touchedKeys

    prunedEngine =
      FactClosureEngine
        { fceMatches = prunedMatches,
          fceWatchers = remainingWatchers
        }

applyFlip ::
  (IntMap EngineMatch, IntSet) ->
  WatchTarget ->
  (IntMap EngineMatch, IntSet)
applyFlip (matches, touched) (WatchTarget matchKey clauseKey slotKey) =
  case flippedMatch of
    Nothing ->
      (matches, touched)
    Just engineMatch ->
      (IntMap.insert matchKey engineMatch matches, IntSet.insert matchKey touched)
  where
    flippedMatch = do
      engineMatch <- IntMap.lookup matchKey matches
      clause <- IntMap.lookup clauseKey (emClauses engineMatch)
      case IntMap.lookup slotKey (ecSlots clause) of
        Just (SlotFact polarity slotWitness satisfied)
          | satisfied /= polarity ->
              let flippedClause =
                    EngineClause
                      { ecSlots =
                          IntMap.insert slotKey (SlotFact polarity slotWitness polarity) (ecSlots clause),
                        ecSatisfiedCount =
                          ecSatisfiedCount clause + (if polarity then 1 else -1),
                        ecPendingPositive =
                          ecPendingPositive clause - (if polarity then 1 else 0)
                      }
                  unsatisfiedDelta
                    | polarity && ecSatisfiedCount clause == 0 = -1
                    | not polarity && ecSatisfiedCount flippedClause == 0 = 1
                    | otherwise = 0
               in Just
                    engineMatch
                      { emClauses = IntMap.insert clauseKey flippedClause (emClauses engineMatch),
                        emUnsatisfiedClauses = emUnsatisfiedClauses engineMatch + unsatisfiedDelta
                      }
        _ ->
          Nothing

matchDerivation :: EngineMatch -> FactDerivation
matchDerivation engineMatch =
  FactDerivation
    { fdRuleId = emRuleId engineMatch,
      fdRuleName = emRuleName engineMatch,
      fdFactWitness = emFactWitness engineMatch,
      fdGuardEvidence =
        Just
          ( guardEvidenceFromClauses
              (fmap clauseEvidenceOf (IntMap.elems (emClauses engineMatch)))
          )
    }
  where
    clauseEvidenceOf clause =
      GuardClauseEvidence (concatMap slotEvidence (IntMap.elems (ecSlots clause)))

    slotEvidence = \case
      SlotStatic evidences ->
        evidences
      SlotFact polarity slotWitness satisfied
        | not satisfied ->
            []
        | polarity ->
            [GuardFactPresent slotWitness]
        | otherwise ->
            [GuardFactAbsent (fwFactId slotWitness) (fwTuple slotWitness)]

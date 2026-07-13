-- | Law bundles for the canonical control algebra and scheduler.
module Moonlight.Control.Laws
  ( LawBundle (..),
    programLawBundles,
    schedulerLaws,
    sequenceMonoidLaws,
    choiceSemigroupLaws,
    regionUnitLaws,
    scopedActionLaws,
    normalizeLaws,
    obstructionLaws,
    foldAgreement,
  )
where

import Data.Foldable qualified as Foldable
import Data.Functor.Identity (runIdentity)
import Numeric.Natural (Natural)
import Test.QuickCheck
  ( Gen,
    Property,
    chooseInt,
    counterexample,
    forAll,
    forAllShrink,
    oneof,
    (=/=),
    (===),
    (==>),
  )

import Moonlight.Control.Class (Control (..))
import Moonlight.Control.Candidate
  ( finiteCandidateSpace,
    scheduledBatchCount,
    scheduledBatchMatches,
  )
import Moonlight.Control.Laws.Gen (genRepeatCount)
import Moonlight.Control.Program
  ( Program,
    ProgramAlgebra,
    foldProgram,
    fromProgram,
    normalize,
  )
import Moonlight.Control.Program.Internal (structuralEq)
import Moonlight.Control.Schedule
  ( ScheduleOrder (DeficitRoundRobin),
    SchedulerConfig (..),
    TracePolicy (NoTrace, TraceAll),
    defaultDeficitRoundRobinConfig,
    defaultSchedulerConfig,
    drrBaseQuantum,
    drrMaxCarryMultiplier,
    foldTracePolicy,
    traceLastEntries,
  )
import Moonlight.Control.Schedule.Round
  ( ScheduleOutcome (..),
    SchedulerState,
    emptySchedulerState,
    scheduleCandidateSpace,
    schedulerTrace,
  )
import Moonlight.Control.Weight
  ( nonCriticalPriorityRank,
    priorityEvidence,
    priorityProfileFromList,
  )

-- | A named group of named properties, ready to be lifted into any test
-- runner.
data LawBundle = LawBundle
  { lawBundleName :: String,
    lawBundleProperties :: [(String, Property)]
  }

-- | Every law bundle of the canonical carrier, obstructions included.
programLawBundles ::
  (Monoid ctx, Eq ctx, Show ctx, Eq p, Show p) =>
  Gen (Program ctx p) ->
  (Program ctx p -> [Program ctx p]) ->
  Gen ctx ->
  (ctx -> [ctx]) ->
  [LawBundle]
programLawBundles gen shrinker genContext shrinkContext =
  [ sequenceMonoidLaws gen shrinker,
    choiceSemigroupLaws gen shrinker,
    regionUnitLaws gen shrinker genContext,
    scopedActionLaws gen shrinker genContext shrinkContext,
    normalizeLaws gen shrinker,
    obstructionLaws gen shrinker
  ]

schedulerLaws :: Gen ScheduleOrder -> LawBundle
schedulerLaws genScheduleOrder =
  LawBundle
    "scheduler"
    [ ( "round budget is conserved",
        forAll genScheduleOrder $ \scheduleOrder ->
          forAll genSchedulerBudget $ \roundBudget ->
            forAll genSchedulerGroups $ \groups ->
              let firstOutcome =
                    runSchedulerLaw
                      scheduleOrder
                      NoTrace
                      roundBudget
                      0
                      groups
                      emptySchedulerState
                  secondOutcome =
                    runSchedulerLaw
                      scheduleOrder
                      NoTrace
                      roundBudget
                      1
                      groups
                      (soSchedulerState firstOutcome)
                  outcomes = [firstOutcome, secondOutcome]
               in counterexample
                    ( show
                        ( fmap
                            (\outcome -> (soScheduledCount outcome, scheduledBatchCount (soScheduledBatch outcome)))
                            outcomes,
                          roundBudget
                        )
                    )
                    ( all
                        ( \outcome ->
                            soScheduledCount outcome <= roundBudget
                              && soScheduledCount outcome == scheduledBatchCount (soScheduledBatch outcome)
                        )
                        outcomes
                    )
      ),
      ( "ordering is deterministic",
        forAll genScheduleOrder $ \scheduleOrder ->
          forAll genSchedulerBudget $ \roundBudget ->
            forAll genSchedulerGroups $ \groups ->
              let config =
                    (schedulerLawConfig scheduleOrder TraceAll)
                      { scPriorityProfile =
                          priorityProfileFromList
                            [ (group, priorityEvidence (abs group) (abs group `mod` 5) 0 nonCriticalPriorityRank)
                            | (group, _) <- groups
                            ]
                      }
                  runSchedule :: ScheduleOutcome Int () Int
                  runSchedule =
                    runIdentity
                      ( scheduleCandidateSpace
                          config
                          roundBudget
                          0
                          (finiteCandidateSpace groups)
                          emptySchedulerState
                      )
                  firstOutcome = runSchedule
                  replayedOutcome = runSchedule
               in firstOutcome === replayedOutcome
      ),
      ( "trace retention matches its policy",
        forAll genScheduleOrder $ \scheduleOrder ->
          forAll genTracePolicy $ \tracePolicy ->
            forAll genSchedulerBudget $ \roundBudget ->
              forAll genSchedulerGroups $ \groups ->
                traceRetentionProperty scheduleOrder tracePolicy roundBudget groups
      ),
      ( "stable DRR frontiers satisfy the starvation bound",
        forAll (chooseInt (1, 6)) $ \groupCount ->
          forAll (chooseInt (1, 8)) $ \rawBudget ->
            deficitRoundRobinStarvationProperty groupCount (fromIntegral rawBudget)
      )
    ]

genSchedulerBudget :: Gen Natural
genSchedulerBudget =
  fromIntegral <$> chooseInt (0, 32)

genSchedulerGroups :: Gen [(Int, [Int])]
genSchedulerGroups = do
  groupCount <- chooseInt (0, 8)
  traverse
    ( \group -> do
        matchCount <- chooseInt (0, 8)
        pure (group, replicate matchCount group)
    )
    [0 .. groupCount - 1]

genTracePolicy :: Gen TracePolicy
genTracePolicy =
  oneof
    [ pure NoTrace,
      pure TraceAll,
      traceLastEntries <$> chooseInt (1, 8)
    ]

schedulerLawConfig :: ScheduleOrder -> TracePolicy -> SchedulerConfig Int
schedulerLawConfig scheduleOrder tracePolicy =
  defaultSchedulerConfig
    { scOrder = scheduleOrder,
      scTracePolicy = tracePolicy
    }

runSchedulerLaw ::
  ScheduleOrder ->
  TracePolicy ->
  Natural ->
  Int ->
  [(Int, [Int])] ->
  SchedulerState Int ->
  ScheduleOutcome Int () Int
runSchedulerLaw scheduleOrder tracePolicy roundBudget roundIndex groups schedulerState =
  runIdentity
    ( scheduleCandidateSpace
        (schedulerLawConfig scheduleOrder tracePolicy)
        roundBudget
        roundIndex
        (finiteCandidateSpace groups)
        schedulerState
    )

traceRetentionProperty ::
  ScheduleOrder ->
  TracePolicy ->
  Natural ->
  [(Int, [Int])] ->
  Property
traceRetentionProperty scheduleOrder tracePolicy roundBudget groups =
  let firstOutcome =
        runSchedulerLaw scheduleOrder tracePolicy roundBudget 0 groups emptySchedulerState
      secondOutcome =
        runSchedulerLaw
          scheduleOrder
          tracePolicy
          roundBudget
          1
          groups
          (soSchedulerState firstOutcome)
      allEntries = soSchedulerTraceDelta firstOutcome <> soSchedulerTraceDelta secondOutcome
      retainedEntries = schedulerTrace (soSchedulerState secondOutcome)
      expectedEntries =
        foldTracePolicy
          []
          (\retainedCount -> drop (max 0 (length allEntries - retainedCount)) allEntries)
          allEntries
          tracePolicy
   in counterexample (show (allEntries, retainedEntries)) (retainedEntries == expectedEntries)

data SchedulerStarvationRun = SchedulerStarvationRun
  { ssrState :: !(SchedulerState Int),
    ssrServedGroups :: ![Int]
  }

deficitRoundRobinStarvationProperty :: Int -> Natural -> Property
deficitRoundRobinStarvationProperty groupCount roundBudget =
  let deficitRoundRobinPolicy = defaultDeficitRoundRobinConfig
      scheduleOrder = DeficitRoundRobin deficitRoundRobinPolicy
      groupIds = [0 .. groupCount - 1]
      groups = fmap (\group -> (group, [group])) groupIds
      carryCap =
        drrMaxCarryMultiplier deficitRoundRobinPolicy
          * drrBaseQuantum deficitRoundRobinPolicy
      numerator = fromIntegral (groupCount - 1) * carryCap + 1
      roundBound = naturalCeilingDivide numerator roundBudget
      finalRun =
        Foldable.foldl'
          (advanceStarvationRun scheduleOrder roundBudget groups)
          SchedulerStarvationRun
            { ssrState = emptySchedulerState,
              ssrServedGroups = []
            }
          [0 .. naturalToSmallInt roundBound - 1]
   in counterexample
        (show (groupIds, roundBudget, roundBound, ssrServedGroups finalRun))
        (all (`elem` ssrServedGroups finalRun) groupIds)

advanceStarvationRun ::
  ScheduleOrder ->
  Natural ->
  [(Int, [Int])] ->
  SchedulerStarvationRun ->
  Int ->
  SchedulerStarvationRun
advanceStarvationRun scheduleOrder roundBudget groups starvationRun roundIndex =
  let outcome =
        runSchedulerLaw
          scheduleOrder
          NoTrace
          roundBudget
          roundIndex
          groups
          (ssrState starvationRun)
   in SchedulerStarvationRun
        { ssrState = soSchedulerState outcome,
          ssrServedGroups = ssrServedGroups starvationRun <> scheduledBatchMatches (soScheduledBatch outcome)
        }

naturalCeilingDivide :: Natural -> Natural -> Natural
naturalCeilingDivide numerator denominator =
  (numerator + denominator - 1) `div` denominator

naturalToSmallInt :: Natural -> Int
naturalToSmallInt = fromIntegral

-- | 'Moonlight.Control.Class.andThen' is a monoid with
-- 'Moonlight.Control.Class.skip'.
sequenceMonoidLaws ::
  (Monoid ctx, Eq ctx, Show ctx, Eq p, Show p) =>
  Gen (Program ctx p) ->
  (Program ctx p -> [Program ctx p]) ->
  LawBundle
sequenceMonoidLaws gen shrinker =
  LawBundle
    "andThen monoid"
    [ ( "associativity",
        forAllTriple gen shrinker $ \(x, y, z) ->
          andThen (andThen x y) z === andThen x (andThen y z)
      ),
      ( "skip left identity",
        forAllShrink gen shrinker $ \x ->
          andThen skip x === x
      ),
      ( "skip right identity",
        forAllShrink gen shrinker $ \x ->
          andThen x skip === x
      )
    ]

-- | 'Moonlight.Control.Class.orElse' is a semigroup — and only a semigroup;
-- its missing identity is asserted by 'obstructionLaws'.
choiceSemigroupLaws ::
  (Monoid ctx, Eq ctx, Show ctx, Eq p, Show p) =>
  Gen (Program ctx p) ->
  (Program ctx p -> [Program ctx p]) ->
  LawBundle
choiceSemigroupLaws gen shrinker =
  LawBundle
    "orElse semigroup"
    [ ( "associativity",
        forAllTriple gen shrinker $ \(x, y, z) ->
          orElse (orElse x y) z === orElse x (orElse y z)
      )
    ]

-- | The region combinators collapse on empty bodies and zero bounds.
regionUnitLaws ::
  (Monoid ctx, Eq ctx, Show ctx, Eq p, Show p) =>
  Gen (Program ctx p) ->
  (Program ctx p -> [Program ctx p]) ->
  Gen ctx ->
  LawBundle
regionUnitLaws gen shrinker genContext =
  LawBundle
    "region units"
    [ ( "upTo 0 x = skip",
        forAllShrink gen shrinker $ \x ->
          upTo 0 x === skip
      ),
      ( "upTo n skip = skip",
        forAll gen $ \x ->
          forAll genRepeatCount $ \repeatCount ->
            upTo repeatCount (skip `asTypeOf` x) === skip
      ),
      ( "attempt skip = skip",
        forAll gen $ \x ->
          attempt (skip `asTypeOf` x) === skip
      ),
      ( "scoped c skip = skip",
        forAll gen $ \x ->
          forAll genContext $ \context ->
            scoped context (skip `asTypeOf` x) === skip
      )
    ]

-- | Scoping is a monoid action: the 'Monoid' superclass of
-- 'Moonlight.Control.Class.ContextOf' made the precondition a type, and
-- this bundle makes the action equation a property.
scopedActionLaws ::
  (Monoid ctx, Eq ctx, Show ctx, Eq p, Show p) =>
  Gen (Program ctx p) ->
  (Program ctx p -> [Program ctx p]) ->
  Gen ctx ->
  (ctx -> [ctx]) ->
  LawBundle
scopedActionLaws gen shrinker genContext shrinkContext =
  LawBundle
    "scoped monoid action"
    [ ( "scoped (a <> b) = scoped a . scoped b",
        forAllShrink gen shrinker $ \x ->
          forAllShrink genContext shrinkContext $ \outerContext ->
            forAllShrink genContext shrinkContext $ \innerContext ->
              scoped (outerContext <> innerContext) x
                === scoped outerContext (scoped innerContext x)
      )
    ]

-- | 'normalize' is a structural fixpoint, and canonical equality cannot see
-- it.
normalizeLaws ::
  (Monoid ctx, Eq ctx, Show ctx, Eq p, Show p) =>
  Gen (Program ctx p) ->
  (Program ctx p -> [Program ctx p]) ->
  LawBundle
normalizeLaws gen shrinker =
  LawBundle
    "normalize"
    [ ( "idempotent up to structural equality",
        forAllShrink gen shrinker $ \x ->
          let normalForm = normalize x
           in counterexample
                (show normalForm)
                (structuralEq normalForm (normalize normalForm))
      ),
      ( "canonical equality is normalize-invariant",
        forAllShrink gen shrinker $ \x ->
          normalize x === x
      )
    ]

-- | The deliberate inequalities of the algebra, first-class. Each one is a
-- typed obstruction: it fails in the canonical carrier, so it can never be
-- promoted to a law of the class.
obstructionLaws ::
  (Monoid ctx, Eq ctx, Show ctx, Eq p, Show p) =>
  Gen (Program ctx p) ->
  (Program ctx p -> [Program ctx p]) ->
  LawBundle
obstructionLaws gen shrinker =
  LawBundle
    "obstructions (deliberate inequalities)"
    [ ( "orElse x skip ≠ x",
        forAllShrink gen shrinker $ \x ->
          orElse x skip =/= x
      ),
      ( "orElse skip x ≠ x",
        forAllShrink gen shrinker $ \x ->
          orElse skip x =/= x
      ),
      ( "attempt (attempt x) ≠ attempt x, x ≠ skip",
        forAllShrink gen shrinker $ \x ->
          x /= skip ==> attempt (attempt x) =/= attempt x
      ),
      ( "upTo 1 x ≠ x, x ≠ skip",
        forAllShrink gen shrinker $ \x ->
          x /= skip ==> upTo 1 x =/= x
      ),
      ( "upTo m (upTo n x) ≠ upTo (m·n) x, x ≠ skip, m n ≥ 1",
        forAllShrink gen shrinker $ \x ->
          forAll genPositiveCount $ \outerCount ->
            forAll genPositiveCount $ \innerCount ->
              x /= skip
                ==> upTo outerCount (upTo innerCount x)
                =/= upTo (outerCount * innerCount) x
      )
    ]

-- | The anchor property for a non-canonical instance: interpreting a term
-- built through the instance's methods coincides with building the same
-- term through 'Program' and applying the instance's defining fold.
foldAgreement ::
  forall c r.
  ( Control c,
    Show (ContextOf c),
    Show (PhaseOf c),
    Eq r,
    Show r
  ) =>
  String ->
  (c -> r) ->
  ProgramAlgebra (ContextOf c) (PhaseOf c) r ->
  Gen (Program (ContextOf c) (PhaseOf c)) ->
  (Program (ContextOf c) (PhaseOf c) -> [Program (ContextOf c) (PhaseOf c)]) ->
  LawBundle
foldAgreement carrierName interpretCarrier algebra gen shrinker =
  LawBundle
    (carrierName <> " fold agreement")
    [ ( "interpret . fromProgram = foldProgram algebra . fromProgram",
        forAllShrink gen shrinker $ \program ->
          interpretCarrier (fromProgram program)
            === foldProgram
              algebra
              (fromProgram program :: Program (ContextOf c) (PhaseOf c))
      )
    ]

forAllTriple ::
  Show a =>
  Gen a ->
  (a -> [a]) ->
  ((a, a, a) -> Property) ->
  Property
forAllTriple gen shrinker =
  forAllShrink genTriple shrinkTriple
  where
    genTriple =
      (,,) <$> gen <*> gen <*> gen
    shrinkTriple (x, y, z) =
      [(x', y, z) | x' <- shrinker x]
        <> [(x, y', z) | y' <- shrinker y]
        <> [(x, y, z') | z' <- shrinker z]

genPositiveCount :: Gen Natural
genPositiveCount =
  fromIntegral <$> chooseInt (1, 5)

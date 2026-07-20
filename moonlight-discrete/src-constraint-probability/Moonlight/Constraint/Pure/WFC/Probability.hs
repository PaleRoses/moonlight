module Moonlight.Constraint.Pure.WFC.Probability
  ( ProbabilisticWFCProblem (..),
    ProbabilisticWFCPolicyProblem (..),
    SlotSelectionStrategy (..),
    CandidateSelectionStrategy (..),
    WFCProbabilityOptions (..),
    defaultWFCProbabilityOptions,
    compileProbabilisticWFCProblem,
    compileProbabilisticWFCPolicyProblem,
    slotDistribution,
    selectNextEntropySlot,
    solveProbabilisticWFC,
    solveProbabilisticWFCWith,
    solveProbabilisticWFCPolicy,
    solveProbabilisticWFCPolicyWith,
  )
where

import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.List (minimumBy, sortBy)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..), comparing)
import Data.Set qualified as Set
import Data.Word (Word64)
import Moonlight.Constraint.Pure.CSP
  ( ConstraintSatisfactionProblem (..),
    Domain,
    domainCardinality,
    domainFromSet,
    domainToAscList,
    lookupDomain,
  )
import Moonlight.Constraint.Pure.WFC.Algebra
  ( propagateCSP,
    selectNextSlot,
  )
import Moonlight.Constraint.Pure.WFC.Compile
  ( compileWFCPolicyProblem,
    compileWFCProblem,
    projectCompiledPolicyError,
    projectCompiledPolicySearchResult,
  )
import Moonlight.Constraint.Pure.WFC.Search
  ( SearchContext (..),
    searchWithContext,
  )
import Moonlight.Constraint.Pure.WFC.Types
  ( AdjacencyRule (..),
    BacktrackLimit (..),
    CompiledPolicySlot (..),
    CompiledPolicyValue (..),
    SlotId (..),
    WFCRule,
    WFCError (..),
    WFCPolicyProblem (..),
    WFCProblem (..),
    WFCSearchResult (..),
    WFCTopology,
  )
import Moonlight.Probability
  ( Categorical,
    categoricalFoldMap,
    categoricalRestrict,
    categoricalSupport,
    categoricalTraverse,
    entropyValue,
    positiveProbValue,
    shannonEntropy,
    uniformCategorical,
  )
import Moonlight.Probability.Sample
  ( PureGen,
    genFromSeed,
    runSampleWith,
    sampleCategorical,
  )
import Prelude

type ProbabilisticWFCProblem :: Type -> Type -> Type
data ProbabilisticWFCProblem slot value = ProbabilisticWFCProblem
  { pwfcProblemDomains :: Map (SlotId slot) (Categorical value),
    pwfcProblemAdjacencyRules :: [AdjacencyRule slot value]
  }

type ProbabilisticWFCPolicyProblem :: Type -> Type -> Type
data ProbabilisticWFCPolicyProblem slot value = ProbabilisticWFCPolicyProblem
  { pwfcPolicyDomains :: Map (SlotId slot) (Categorical value),
    pwfcPolicyTopology :: WFCTopology slot,
    pwfcPolicyRules :: [WFCRule slot value]
  }

type SlotSelectionStrategy :: Type
data SlotSelectionStrategy
  = MinimumRemainingValues
  | MinimumEntropy
  deriving stock (Eq, Ord, Show, Read)

type CandidateSelectionStrategy :: Type
data CandidateSelectionStrategy
  = WeightedDescending
  | WeightedSampling
  deriving stock (Eq, Ord, Show, Read)

type WFCProbabilityOptions :: Type
data WFCProbabilityOptions = WFCProbabilityOptions
  { wfcProbabilityBacktrackLimit :: BacktrackLimit,
    wfcProbabilitySeed :: Word64,
    wfcProbabilitySlotSelection :: SlotSelectionStrategy,
    wfcProbabilityCandidateSelection :: CandidateSelectionStrategy
  }
  deriving stock (Eq, Ord, Show, Read)

defaultWFCProbabilityOptions :: WFCProbabilityOptions
defaultWFCProbabilityOptions =
  WFCProbabilityOptions
    { wfcProbabilityBacktrackLimit = BacktrackLimit 256,
      wfcProbabilitySeed = 0,
      wfcProbabilitySlotSelection = MinimumEntropy,
      wfcProbabilityCandidateSelection = WeightedDescending
    }

compileProbabilisticWFCProblem ::
  ProbabilisticWFCProblem slot value ->
  WFCProblem slot value
compileProbabilisticWFCProblem problem =
  WFCProblem
    { wfcProblemDomains =
        fmap (domainFromSet . categoricalSupport) (pwfcProblemDomains problem),
      wfcProblemAdjacencyRules = pwfcProblemAdjacencyRules problem
    }

compileProbabilisticWFCPolicyProblem ::
  (Ord slot, Ord value) =>
  ProbabilisticWFCPolicyProblem slot value ->
  Maybe (ProbabilisticWFCProblem (CompiledPolicySlot slot) (CompiledPolicyValue slot value))
compileProbabilisticWFCPolicyProblem problem = do
  let deterministicPolicyProblem =
        WFCPolicyProblem
          { wfcPolicyDomains =
              fmap (domainFromSet . categoricalSupport) (pwfcPolicyDomains problem),
            wfcPolicyTopology = pwfcPolicyTopology problem,
            wfcPolicyRules = pwfcPolicyRules problem
          }
      compiledProblem = compileWFCPolicyProblem deterministicPolicyProblem
  compiledDomains <-
    traverse
      (liftCompiledDomain (pwfcPolicyDomains problem))
      (Map.toAscList (wfcProblemDomains compiledProblem))
  pure
    ProbabilisticWFCProblem
      { pwfcProblemDomains = Map.fromList compiledDomains,
        pwfcProblemAdjacencyRules = wfcProblemAdjacencyRules compiledProblem
      }

slotDistribution ::
  (Ord slot, Ord value) =>
  ProbabilisticWFCProblem slot value ->
  ConstraintSatisfactionProblem (SlotId slot) value ->
  SlotId slot ->
  Either (WFCError slot) (Maybe (Categorical value))
slotDistribution problem compiledProblem slotId = do
  domainValue <- either (Left . WFCCSPError) Right (lookupDomain compiledProblem slotId)
  case Map.lookup slotId (pwfcProblemDomains problem) of
    Nothing -> Left WFCProjectionInvariantViolation
    Just priorDistribution ->
      Right
        ( categoricalRestrict
            (Set.fromList (domainToAscList domainValue))
            priorDistribution
        )

selectNextEntropySlot ::
  (Ord slot, Ord value) =>
  ProbabilisticWFCProblem slot value ->
  ConstraintSatisfactionProblem (SlotId slot) value ->
  Maybe (SlotId slot)
selectNextEntropySlot problem compiledProblem =
  fmap candidateSlot (minimumByMaybe candidateOrder entropyCandidates)
  where
    entropyCandidates =
      mapMaybe (entropyCandidate problem)
        (Map.toAscList (cspDomains compiledProblem))
    candidateOrder :: Ord slot => EntropyCandidate slot -> EntropyCandidate slot -> Ordering
    candidateOrder leftCandidate rightCandidate =
      compare
        ( candidateEntropy leftCandidate,
          candidateCardinality leftCandidate,
          candidateSlot leftCandidate
        )
        ( candidateEntropy rightCandidate,
          candidateCardinality rightCandidate,
          candidateSlot rightCandidate
        )

type EntropyCandidate :: Type -> Type
data EntropyCandidate slot = EntropyCandidate
  { candidateSlot :: SlotId slot,
    candidateEntropy :: Double,
    candidateCardinality :: Int
  }

entropyCandidate ::
  (Ord slot, Ord value) =>
  ProbabilisticWFCProblem slot value ->
  (SlotId slot, Domain value) ->
  Maybe (EntropyCandidate slot)
entropyCandidate problem (slotId, domainValue) =
  let cardinality = domainCardinality domainValue
   in if cardinality <= 1
        then Nothing
        else
          case Map.lookup slotId (pwfcProblemDomains problem) of
            Nothing -> Nothing
            Just priorDistribution ->
              let outcomes = domainToAscList domainValue
               in fmap
                    (\restrictedDistribution ->
                       EntropyCandidate
                         { candidateSlot = slotId,
                           candidateEntropy =
                             entropyValue (shannonEntropy restrictedDistribution),
                           candidateCardinality = cardinality
                         }
                    )
                    (categoricalRestrict (Set.fromList outcomes) priorDistribution)

solveProbabilisticWFC ::
  (Ord slot, Ord value) =>
  ProbabilisticWFCProblem slot value ->
  Either (WFCError slot) (WFCSearchResult slot value)
solveProbabilisticWFC =
  solveProbabilisticWFCWith defaultWFCProbabilityOptions

solveProbabilisticWFCPolicy ::
  (Ord slot, Ord value) =>
  ProbabilisticWFCPolicyProblem slot value ->
  Either (WFCError slot) (WFCSearchResult slot value)
solveProbabilisticWFCPolicy =
  solveProbabilisticWFCPolicyWith defaultWFCProbabilityOptions

solveProbabilisticWFCWith ::
  (Ord slot, Ord value) =>
  WFCProbabilityOptions ->
  ProbabilisticWFCProblem slot value ->
  Either (WFCError slot) (WFCSearchResult slot value)
solveProbabilisticWFCWith options problem = do
  initialPropagation <- propagateCSP (compileWFCProblem (compileProbabilisticWFCProblem problem))
  case initialPropagation of
    Nothing -> Right WFCUnsatisfiable
    Just propagatedProblem -> do
      let BacktrackLimit remainingBacktracks = wfcProbabilityBacktrackLimit options
      (_, _, result) <-
        searchWithContext
          (probabilisticSearchContext options problem)
          remainingBacktracks
          (genFromSeed (wfcProbabilitySeed options))
          propagatedProblem
      pure result

solveProbabilisticWFCPolicyWith ::
  (Ord slot, Ord value) =>
  WFCProbabilityOptions ->
  ProbabilisticWFCPolicyProblem slot value ->
  Either (WFCError slot) (WFCSearchResult slot value)
solveProbabilisticWFCPolicyWith options problem =
  case compileProbabilisticWFCPolicyProblem problem of
    Nothing -> Right WFCUnsatisfiable
    Just compiledProblem ->
      case solveProbabilisticWFCWith options compiledProblem of
        Left compiledError -> Left (projectCompiledPolicyError compiledError)
        Right compiledResult -> projectCompiledPolicySearchResult compiledResult

probabilisticSearchContext ::
  (Ord slot, Ord value) =>
  WFCProbabilityOptions ->
  ProbabilisticWFCProblem slot value ->
  SearchContext PureGen slot value
probabilisticSearchContext options problem =
  SearchContext
    { searchSelectSlot =
        \_ compiledProblem -> selectSlot options problem compiledProblem,
      searchCandidates =
        \generator compiledProblem slotId ->
          candidateOrderForSlot
            options
            problem
            compiledProblem
            slotId
            generator
    }

selectSlot ::
  (Ord slot, Ord value) =>
  WFCProbabilityOptions ->
  ProbabilisticWFCProblem slot value ->
  ConstraintSatisfactionProblem (SlotId slot) value ->
  Maybe (SlotId slot)
selectSlot options problem compiledProblem =
  case wfcProbabilitySlotSelection options of
    MinimumRemainingValues -> selectNextSlot compiledProblem
    MinimumEntropy -> selectNextEntropySlot problem compiledProblem

candidateOrderForSlot ::
  (Ord slot, Ord value) =>
  WFCProbabilityOptions ->
  ProbabilisticWFCProblem slot value ->
  ConstraintSatisfactionProblem (SlotId slot) value ->
  SlotId slot ->
  PureGen ->
  Either (WFCError slot) ([value], PureGen)
candidateOrderForSlot options problem compiledProblem slotId generator = do
  maybeDistribution <- slotDistribution problem compiledProblem slotId
  case maybeDistribution of
    Nothing -> Right ([], generator)
    Just restrictedDistribution ->
      case wfcProbabilityCandidateSelection options of
        WeightedDescending ->
          Right (weightedDescendingCandidates restrictedDistribution, generator)
        WeightedSampling ->
          let descendingCandidates =
                weightedDescendingCandidates restrictedDistribution
              (sampledCandidate, nextGenerator) =
                runSampleWith (sampleCategorical restrictedDistribution) generator
              remainingCandidates = filter (/= sampledCandidate) descendingCandidates
           in Right (sampledCandidate : remainingCandidates, nextGenerator)

weightedDescendingCandidates :: Categorical value -> [value]
weightedDescendingCandidates =
  fmap fst
    . sortBy (comparing (Down . positiveProbValue . snd))
    . categoricalFoldMap (\weightedOutcome -> [weightedOutcome])

minimumByMaybe :: (value -> value -> Ordering) -> [value] -> Maybe value
minimumByMaybe comparator values =
  case values of
    [] -> Nothing
    _ -> Just (minimumBy comparator values)

liftCompiledDomain ::
  (Ord slot, Ord value) =>
  Map (SlotId slot) (Categorical value) ->
  ( SlotId (CompiledPolicySlot slot),
    Domain (CompiledPolicyValue slot value)
  ) ->
  Maybe
    ( SlotId (CompiledPolicySlot slot),
      Categorical (CompiledPolicyValue slot value)
    )
liftCompiledDomain priorDomains (compiledSlotId, domainValue) =
  case compiledSlot of
    CompiledBaseSlot slot -> do
      priorDistribution <- Map.lookup (SlotId slot) priorDomains
      restrictedDistribution <-
        categoricalRestrict
          (Set.fromList (domainToAscList domainValue))
          (mapCategorical CompiledBaseValue priorDistribution)
      pure (compiledSlotId, restrictedDistribution)
    CompiledPresenceWitnessSlot _ -> do
      witnessValues <- NonEmpty.nonEmpty (domainToAscList domainValue)
      pure (compiledSlotId, uniformCategorical witnessValues)
  where
    SlotId compiledSlot = compiledSlotId

mapCategorical ::
  Ord target =>
  (source -> target) ->
  Categorical source ->
  Categorical target
mapCategorical liftOutcome categorical =
  runIdentity
    ( categoricalTraverse
        (\(outcome, _) -> Identity (liftOutcome outcome))
        categorical
    )

module Moonlight.EGraph.Pure.Extraction.Guide
  ( GuidedOrderingKey,
    PlainOrderingKey,
    PreparedGuidance,
    prepareGuidance,
    guidedExtractionResult,
    guidedExtractionResultWithPrepared,
    candidateExtractions,
    candidateExtractionsWithPrepared,
    guidedCandidateWithPrepared,
    guidedOrderingKey,
    guideEvidenceForTerm,
    patternMatchesTerm,
    matchPattern,
  )
where

import Control.Monad.Trans.State.Strict (StateT (..), runStateT)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Kind (Type)
import Data.Maybe (mapMaybe, maybeToList)
import Moonlight.EGraph.Pure.Extraction
  ( AnalysisCostAlgebra (..),
    ExtractionChoiceSection,
    ExtractionResult (..),
    extractFromChoiceSection,
    extractionCanonicalClass,
    extractionClassAnalysis,
    extractionClassNodes,
    extractionChoiceSectionCostAlgebra,
    extractionChoiceSectionTable,
    lookupExtractionClass,
    minimumMaybe,
    termSize,
  )
import Moonlight.Control.Gate
  ( GuideCheckpoint (..),
    GuideCheckpointHit (..),
    GuideEvidence (..),
    GuideMode (..),
    GuidanceConfig (..),
  )
import Moonlight.Core
  ( Language,
    OrderedFix (..),
    Pattern (..),
    ZipMatch (..),
    patternVarKey,
  )
import Moonlight.Core.Pattern.Automata
  ( PatternAutomaton,
    compilePatternAutomaton,
    matchPatternAutomaton,
    matchesPatternAutomaton,
  )
import Moonlight.EGraph.Pure.Types
  ( ClassId,
    ENode (..),
  )
import Data.Fix (Fix (..))

type GuidedOrderingKey :: (Type -> Type) -> Type -> Type
type GuidedOrderingKey f cost = (Int, Int, cost, Int, OrderedFix f)

type PlainOrderingKey :: (Type -> Type) -> Type -> Type
type PlainOrderingKey f cost = (cost, Int, OrderedFix f)

guidedExtractionResult :: (Language f, ZipMatch f, Ord cost) => GuidanceConfig (Pattern f) -> ExtractionChoiceSection f a cost -> ClassId -> Maybe (ExtractionResult f cost)
guidedExtractionResult guidanceConfig section classId =
  guidedExtractionResultWithPrepared
    (prepareGuidance guidanceConfig)
    section
    classId

guidedExtractionResultWithPrepared ::
  (Language f, ZipMatch f, Ord cost) =>
  PreparedGuidance f ->
  ExtractionChoiceSection f a cost ->
  ClassId ->
  Maybe (ExtractionResult f cost)
guidedExtractionResultWithPrepared preparedGuidance section classId =
  minimumMaybe (candidateExtractionsWithPrepared preparedGuidance section classId)

candidateExtractions :: (Language f, ZipMatch f, Ord cost) => GuidanceConfig (Pattern f) -> ExtractionChoiceSection f a cost -> ClassId -> [(GuidedOrderingKey f cost, ExtractionResult f cost)]
candidateExtractions guidanceConfig section classId =
  candidateExtractionsWithPrepared
    (prepareGuidance guidanceConfig)
    section
    classId

type PreparedGuideCheckpoint :: (Type -> Type) -> Type
type PreparedGuideCheckpoint f =
  (GuideCheckpoint (Pattern f), PatternAutomaton f)

type PreparedGuidance :: (Type -> Type) -> Type
data PreparedGuidance f = PreparedGuidance
  { pgCheckpoints :: ![PreparedGuideCheckpoint f],
    pgRequiresCheckpoint :: !Bool
  }

prepareGuidance ::
  (Language f, ZipMatch f) =>
  GuidanceConfig (Pattern f) ->
  PreparedGuidance f
prepareGuidance guidanceConfig =
  PreparedGuidance
    { pgCheckpoints =
        fmap
          (\checkpoint -> (checkpoint, compilePatternAutomaton (gcTarget checkpoint)))
          (gcCheckpoints guidanceConfig),
      pgRequiresCheckpoint =
        any ((== GuideRequire) . gcMode) (gcCheckpoints guidanceConfig)
    }

candidateExtractionsWithPrepared ::
  (Language f, ZipMatch f, Ord cost) =>
  PreparedGuidance f ->
  ExtractionChoiceSection f a cost ->
  ClassId ->
  [(GuidedOrderingKey f cost, ExtractionResult f cost)]
candidateExtractionsWithPrepared preparedGuidance section classId =
  mapMaybe
    (guidedCandidateWithPrepared preparedGuidance)
    ( unguidedCandidate
        <> foldMap
          ( \(checkpoint, _automaton) ->
              patternExtractionCandidates
                section
                (gcTarget checkpoint)
                classId
          )
          (pgCheckpoints preparedGuidance)
    )
  where
    unguidedCandidate =
      maybeToList (extractFromChoiceSection classId section)

guidedCandidateWithPrepared ::
  Language f =>
  PreparedGuidance f ->
  ExtractionResult f cost ->
  Maybe (GuidedOrderingKey f cost, ExtractionResult f cost)
guidedCandidateWithPrepared preparedGuidance extractionResult =
  if not (pgRequiresCheckpoint preparedGuidance)
    || hasRequiredHit
    then
      Just
        ( guidedOrderingKeyFromSummary hitSummary extractionResult,
          extractionResult
        )
    else Nothing
  where
    hitSummary@(_hitCount, hasRequiredHit) =
      guideSummaryForTerm preparedGuidance (erTerm extractionResult)

patternExtractionCandidates ::
  (Language f, ZipMatch f, Ord cost) =>
  ExtractionChoiceSection f a cost ->
  Pattern f ->
  ClassId ->
  [ExtractionResult f cost]
patternExtractionCandidates section patternValue classId =
  fmap fst (patternCandidates patternValue classId IntMap.empty)
  where
    costAlgebraValue =
      extractionChoiceSectionCostAlgebra section

    table =
      extractionChoiceSectionTable section

    patternCandidates currentPattern currentClass bindings = do
      canonicalClass <- maybeToList (extractionCanonicalClass table currentClass)
      extractionClassValue <- maybeToList (lookupExtractionClass table canonicalClass)
      case currentPattern of
        PatternVar patternVariable ->
          case IntMap.lookup (patternVarKey patternVariable) bindings of
            Just boundClass
              | boundClass /= canonicalClass ->
                  []
            _ ->
              fmap
                (\resultValue -> (resultValue, IntMap.insert (patternVarKey patternVariable) canonicalClass bindings))
                (maybeToList (extractFromChoiceSection canonicalClass section))
        PatternNode patternNode -> do
          ENode childClasses <- extractionClassNodes extractionClassValue
          patternChildren <- maybeToList (zipMatch patternNode childClasses)
          (childResults, nextBindings) <-
            runStateT (traverse matchChild patternChildren) bindings
          classResults <- maybeToList (zipMatch childClasses childResults)
          childAnalysisCosts <-
            maybeToList (traverse childAnalysisCost classResults)
          pure
            ( ExtractionResult
                { erTerm = Fix (fmap erTerm childResults),
                  erCost =
                    analysisCostAlgebra
                      costAlgebraValue
                      (extractionClassAnalysis extractionClassValue)
                      childAnalysisCosts,
                  erClass = canonicalClass
                },
              nextBindings
            )

    matchChild (childPattern, childClass) =
      StateT (patternCandidates childPattern childClass)

    childAnalysisCost (childClass, childResult) = do
      childExtractionClass <- lookupExtractionClass table childClass
      pure (extractionClassAnalysis childExtractionClass, erCost childResult)

guidedOrderingKey :: (Language f, ZipMatch f) => GuidanceConfig (Pattern f) -> ExtractionResult f cost -> GuidedOrderingKey f cost
guidedOrderingKey guidanceConfig extractionResult =
  guidedOrderingKeyFromSummary
    (guideSummaryForTerm (prepareGuidance guidanceConfig) (erTerm extractionResult))
    extractionResult

guidedOrderingKeyFromSummary :: Language f => (Int, Bool) -> ExtractionResult f cost -> GuidedOrderingKey f cost
guidedOrderingKeyFromSummary (hitCount, hasRequiredHit) extractionResult =
  let selectionPenalty =
        if hasRequiredHit
          then 0
          else
            if hitCount > 0
              then 1
              else 2
   in (selectionPenalty, negate hitCount, erCost extractionResult, termSize (erTerm extractionResult), OrderedFix (erTerm extractionResult))

guideEvidenceForTerm :: (Language f, ZipMatch f) => GuidanceConfig (Pattern f) -> ClassId -> Fix f -> Maybe (GuideEvidence ClassId)
guideEvidenceForTerm guidanceConfig classId term =
  case guideCheckpointHitsForTerm (prepareGuidance guidanceConfig) classId term of
    [] -> Nothing
    checkpointHits -> Just (GuideEvidence checkpointHits)

guideSummaryForTerm :: Language f => PreparedGuidance f -> Fix f -> (Int, Bool)
guideSummaryForTerm preparedGuidance term =
  foldr
    ( \(guideCheckpoint, automaton) summary@(hitCount, hasRequiredHit) ->
        if matchesPatternAutomaton automaton term
          then (hitCount + 1, gcMode guideCheckpoint == GuideRequire || hasRequiredHit)
          else summary
    )
    (0, False)
    (pgCheckpoints preparedGuidance)

guideCheckpointHitsForTerm :: Language f => PreparedGuidance f -> ClassId -> Fix f -> [GuideCheckpointHit ClassId]
guideCheckpointHitsForTerm preparedGuidance classId term =
  mapMaybe
    ( \(guideCheckpoint, automaton) ->
        if matchesPatternAutomaton automaton term
          then
            Just
              GuideCheckpointHit
                { gchCheckpointName = gcName guideCheckpoint,
                  gchMode = gcMode guideCheckpoint,
                  gchPreviewClass = classId
                }
          else Nothing
    )
    (pgCheckpoints preparedGuidance)

patternMatchesTerm :: (Language f, ZipMatch f) => Pattern f -> Fix f -> Bool
patternMatchesTerm patternValue term =
  matchesPatternAutomaton (compilePatternAutomaton patternValue) term

matchPattern :: (Language f, ZipMatch f) => Pattern f -> Fix f -> IntMap (Fix f) -> Maybe (IntMap (Fix f))
matchPattern patternValue term bindings =
  matchPatternAutomaton
    (compilePatternAutomaton patternValue)
    term
    bindings

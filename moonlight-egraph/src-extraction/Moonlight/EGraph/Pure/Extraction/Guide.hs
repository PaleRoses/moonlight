module Moonlight.EGraph.Pure.Extraction.Guide
  ( GuidedOrderingKey,
    PlainOrderingKey,
    guidedExtractionResult,
    candidateExtractions,
    guidedOrderingKey,
    guideEvidenceForTerm,
    patternMatchesTerm,
    matchPattern,
  )
where

import Moonlight.Core (ZipMatch)
import Data.IntMap.Strict (IntMap)
import Data.Kind (Type)
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Moonlight.EGraph.Pure.Extraction.Algebra
  ( buildExtractionResult,
  )
import Moonlight.EGraph.Pure.Extraction.Core
  ( AnalysisCostAlgebra,
    BestChoice,
    ExtractionResult (..),
    ExtractionTable,
    minimumMaybe,
    termSize,
  )
import Moonlight.EGraph.Pure.Extraction.Worklist
  ( candidateChoices,
  )
import Moonlight.Core (OrderedFix (..))
import Moonlight.Control.Gate
  ( GuideCheckpoint (..),
    GuideCheckpointHit (..),
    GuideEvidence (..),
    GuideMode (..),
    GuidanceConfig (..),
  )
import Moonlight.Core (Language)
import Moonlight.Core
  ( Pattern
  )
import Moonlight.Core.Pattern.Automata
  ( compilePatternAutomaton,
    matchPatternAutomaton,
    matchesPatternAutomaton,
  )
import Moonlight.EGraph.Pure.Types
  ( ClassId,
  )
import Data.Fix (Fix)

type GuidedOrderingKey :: (Type -> Type) -> Type -> Type
type GuidedOrderingKey f cost = (Int, Int, cost, Int, OrderedFix f)

type PlainOrderingKey :: (Type -> Type) -> Type -> Type
type PlainOrderingKey f cost = (cost, Int, OrderedFix f)

guidedExtractionResult :: (Language f, ZipMatch f, Ord cost) => GuidanceConfig (Pattern f) -> AnalysisCostAlgebra f a cost -> IntMap (Maybe (BestChoice f cost)) -> ExtractionTable f a -> ClassId -> Maybe (ExtractionResult f cost)
guidedExtractionResult guidanceConfig costAlgebraValue stableChoices table classId =
  let candidateResults =
        candidateExtractions guidanceConfig costAlgebraValue stableChoices table classId
   in minimumMaybe candidateResults

candidateExtractions :: (Language f, ZipMatch f, Ord cost) => GuidanceConfig (Pattern f) -> AnalysisCostAlgebra f a cost -> IntMap (Maybe (BestChoice f cost)) -> ExtractionTable f a -> ClassId -> [(GuidedOrderingKey f cost, ExtractionResult f cost)]
candidateExtractions guidanceConfig costAlgebraValue stableChoices table classId =
  mapMaybe
    ( \bestChoice ->
        ( \extractionResult ->
            (guidedOrderingKey guidanceConfig extractionResult, extractionResult)
        )
          <$> buildExtractionResult stableChoices table classId Set.empty bestChoice
    )
    (candidateChoices costAlgebraValue table stableChoices classId)

guidedOrderingKey :: (Language f, ZipMatch f) => GuidanceConfig (Pattern f) -> ExtractionResult f cost -> GuidedOrderingKey f cost
guidedOrderingKey guidanceConfig extractionResult =
  let guideEvidence =
        guideEvidenceForTerm guidanceConfig (erClass extractionResult) (erTerm extractionResult)
      checkpointHits =
        maybe [] geCheckpointHits guideEvidence
      hitCount =
        length checkpointHits
      requiredHitCount =
        length (filter ((== GuideRequire) . gchMode) checkpointHits)
      selectionPenalty =
        if requiredHitCount > 0
          then 0
          else
            if hitCount > 0
              then 1
              else 2
   in (selectionPenalty, negate hitCount, erCost extractionResult, termSize (erTerm extractionResult), OrderedFix (erTerm extractionResult))

guideEvidenceForTerm :: (Language f, ZipMatch f) => GuidanceConfig (Pattern f) -> ClassId -> Fix f -> Maybe (GuideEvidence ClassId)
guideEvidenceForTerm guidanceConfig classId term =
  let checkpointHits =
        fmap
          (\guideCheckpoint ->
             GuideCheckpointHit
               { gchCheckpointName = gcName guideCheckpoint,
                 gchMode = gcMode guideCheckpoint,
                 gchPreviewClass = classId
               }
          )
          ( filter
              (\guideCheckpoint -> patternMatchesTerm (gcTarget guideCheckpoint) term)
              (gcCheckpoints guidanceConfig)
          )
   in if null checkpointHits
        then Nothing
        else Just (GuideEvidence checkpointHits)

patternMatchesTerm :: (Language f, ZipMatch f) => Pattern f -> Fix f -> Bool
patternMatchesTerm patternValue term =
  matchesPatternAutomaton (compilePatternAutomaton patternValue) term

matchPattern :: (Language f, ZipMatch f) => Pattern f -> Fix f -> IntMap (Fix f) -> Maybe (IntMap (Fix f))
matchPattern patternValue term bindings =
  matchPatternAutomaton
    (compilePatternAutomaton patternValue)
    term
    bindings

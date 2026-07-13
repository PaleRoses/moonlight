module Moonlight.EGraph.Pure.Extraction.Algebra
  ( candidateChoices,
    ExtractionChoiceSection (..),
    extractChoices,
    extractChoicesBounded,
    extractChoiceSection,
    extractChoiceSectionBounded,
    extendChoiceSectionWithGraphClasses,
    extractFromChoiceSection,
    extractAllFromChoiceSection,
    extractAllFromTable,
    extractAllFromTableBounded,
    extractFromTable,
    extractFromTableBounded,
    buildExtractionResult,
    reconstructTerm,
    reconstructChild,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.Set qualified as Set
import Moonlight.Core (Language)
import Moonlight.EGraph.Pure.Extraction.Core
  ( AnalysisCostAlgebra,
    BestChoice (..),
    ExtractionConvergenceReport,
    ExtractionFixpointBudget,
    ExtractionResult (..),
    ExtractionTable,
    extractionCanonicalClass,
    extractionClass,
    extractionClasses,
    uncheckedExtractionTable,
  )
import Moonlight.EGraph.Pure.Extraction.Worklist
  ( WorklistSeed (..),
    candidateChoices,
    chooseBetterChoice,
    worklistChoices,
    worklistChoicesTotal,
  )
import Moonlight.EGraph.Pure.Types
  ( ClassId (..),
    EGraph,
    ENode (..),
    canonicalizeClassId,
    classIdKey,
    eGraphAnalysis,
    eGraphClassNodes,
  )
import Data.Fix (Fix (..))
import Data.IntSet (IntSet)

type ExtractionChoiceSection :: (Type -> Type) -> Type -> Type -> Type
data ExtractionChoiceSection f a cost = ExtractionChoiceSection
  { ecsTable :: !(ExtractionTable f a),
    ecsChoices :: !(IntMap (Maybe (BestChoice f cost)))
  }

extractChoices :: (Language f, Ord cost) => AnalysisCostAlgebra f a cost -> ExtractionTable f a -> IntMap (Maybe (BestChoice f cost))
extractChoices costAlgebraValue table =
  worklistChoicesTotal costAlgebraValue table IntMap.empty SeedAllClasses

extractChoiceSection :: (Language f, Ord cost) => AnalysisCostAlgebra f a cost -> ExtractionTable f a -> ExtractionChoiceSection f a cost
extractChoiceSection costAlgebraValue table =
  ExtractionChoiceSection
    { ecsTable = table,
      ecsChoices = extractChoices costAlgebraValue table
    }

-- | Budget-capped best-choice extraction.  The budget bounds finalizations
-- (one per class); 'Left' reports the partial coverage when the cap is reached
-- before every reachable class is finalized.
extractChoicesBounded :: (Language f, Ord cost) => ExtractionFixpointBudget -> AnalysisCostAlgebra f a cost -> ExtractionTable f a -> Either ExtractionConvergenceReport (IntMap (Maybe (BestChoice f cost)))
extractChoicesBounded budget costAlgebraValue table =
  worklistChoices budget costAlgebraValue table IntMap.empty SeedAllClasses

-- | Extend a choice section with classes freshly inserted into @graph@ on top
-- of the section's own class universe.  Sound whenever the inserted classes
-- were created by pure node insertion (no merges applied): each fresh key is
-- its own canonical and its node children are either section classes or fresh
-- keys with smaller allocation order, so ascending-key processing is
-- dependency order and existing best choices are undisturbed.  Keys already
-- covered by the section, and keys unknown to @graph@'s analysis, are skipped.
extendChoiceSectionWithGraphClasses ::
  (Language f, Ord cost) =>
  AnalysisCostAlgebra f a cost ->
  EGraph f a ->
  IntSet ->
  ExtractionChoiceSection f a cost ->
  ExtractionChoiceSection f a cost
extendChoiceSectionWithGraphClasses costAlgebraValue graph insertedKeys section =
  let table =
        ecsTable section
      analysisMap =
        eGraphAnalysis graph
      freshKeys =
        IntSet.filter
          ( \classKey ->
              not (IntMap.member classKey (extractionClasses table))
                && IntMap.member classKey analysisMap
          )
          insertedKeys
   in if IntSet.null freshKeys
        then section
        else
          let sectionCanonical =
                extractionCanonicalClass table
              extendedCanonical classId =
                case sectionCanonical classId of
                  Just canonicalId ->
                    Just canonicalId
                  Nothing ->
                    let graphCanonical = canonicalizeClassId graph classId
                     in if IntSet.member (classIdKey graphCanonical) freshKeys
                          then Just graphCanonical
                          else Nothing
              classFor classKey analysisValue =
                extractionClass
                  analysisValue
                  (Set.toAscList (eGraphClassNodes graph (ClassId classKey)))
              extendedClasses =
                IntMap.union
                  (extractionClasses table)
                  ( IntMap.mapWithKey
                      classFor
                      (IntMap.restrictKeys analysisMap freshKeys)
                  )
              extendedTable =
                uncheckedExtractionTable extendedClasses extendedCanonical
              addChoice choices classKey =
                IntMap.insert
                  classKey
                  ( foldr
                      chooseBetterChoice
                      Nothing
                      (candidateChoices costAlgebraValue extendedTable choices (ClassId classKey))
                  )
                  choices
           in ExtractionChoiceSection
                { ecsTable = extendedTable,
                  ecsChoices =
                    foldl' addChoice (ecsChoices section) (IntSet.toAscList freshKeys)
                }

extractChoiceSectionBounded :: (Language f, Ord cost) => ExtractionFixpointBudget -> AnalysisCostAlgebra f a cost -> ExtractionTable f a -> Either ExtractionConvergenceReport (ExtractionChoiceSection f a cost)
extractChoiceSectionBounded budget costAlgebraValue table =
  fmap
    ( \stableChoices ->
        ExtractionChoiceSection
          { ecsTable = table,
            ecsChoices = stableChoices
          }
    )
    (extractChoicesBounded budget costAlgebraValue table)

extractAllFromTable :: (Language f, Ord cost) => AnalysisCostAlgebra f a cost -> ExtractionTable f a -> IntMap (ExtractionResult f cost)
extractAllFromTable costAlgebraValue table =
  extractAllFromChoiceSection (extractChoiceSection costAlgebraValue table)

extractAllFromTableBounded :: (Language f, Ord cost) => ExtractionFixpointBudget -> AnalysisCostAlgebra f a cost -> ExtractionTable f a -> Either ExtractionConvergenceReport (IntMap (ExtractionResult f cost))
extractAllFromTableBounded budget costAlgebraValue table =
  fmap
    extractAllFromChoiceSection
    (extractChoiceSectionBounded budget costAlgebraValue table)

extractFromTable :: (Language f, Ord cost) => AnalysisCostAlgebra f a cost -> ClassId -> ExtractionTable f a -> Maybe (ExtractionResult f cost)
extractFromTable costAlgebraValue classId table =
  extractFromChoiceSection
    classId
    ExtractionChoiceSection
      { ecsTable = table,
        ecsChoices =
          worklistChoicesTotal
            costAlgebraValue
            table
            IntMap.empty
            (SeedClasses (IntSet.singleton (classIdKey classId)))
      }

extractFromTableBounded :: (Language f, Ord cost) => ExtractionFixpointBudget -> AnalysisCostAlgebra f a cost -> ClassId -> ExtractionTable f a -> Either ExtractionConvergenceReport (Maybe (ExtractionResult f cost))
extractFromTableBounded budget costAlgebraValue classId table =
  fmap
    (extractFromChoiceSection classId)
    (extractChoiceSectionBounded budget costAlgebraValue table)

extractFromChoiceSection :: (Language f, Ord cost) => ClassId -> ExtractionChoiceSection f a cost -> Maybe (ExtractionResult f cost)
extractFromChoiceSection classId section = do
  canonicalClassId <- extractionCanonicalClass (ecsTable section) classId
  bestChoice <-
    IntMap.findWithDefault Nothing (classIdKey canonicalClassId) (ecsChoices section)
  buildExtractionResult (ecsChoices section) (ecsTable section) canonicalClassId Set.empty bestChoice

extractAllFromChoiceSection :: (Language f, Ord cost) => ExtractionChoiceSection f a cost -> IntMap (ExtractionResult f cost)
extractAllFromChoiceSection section =
  extractedResultsFromChoices (ecsChoices section) (ecsTable section)

buildExtractionResult :: (Language f, Ord cost) => IntMap (Maybe (BestChoice f cost)) -> ExtractionTable f a -> ClassId -> Set.Set ClassId -> BestChoice f cost -> Maybe (ExtractionResult f cost)
buildExtractionResult stableChoices table classId visitedClasses bestChoice = do
  canonicalClassId <- extractionCanonicalClass table classId
  if Set.member canonicalClassId visitedClasses
    then Nothing
    else do
      let nextVisitedClasses = Set.insert canonicalClassId visitedClasses
      term <- reconstructTerm stableChoices table nextVisitedClasses (bcNode bestChoice)
      pure
        ExtractionResult
          { erTerm = term,
            erCost = bcCost bestChoice,
            erClass = canonicalClassId
          }

reconstructTerm :: (Language f, Ord cost) => IntMap (Maybe (BestChoice f cost)) -> ExtractionTable f a -> Set.Set ClassId -> ENode f -> Maybe (Fix f)
reconstructTerm stableChoices table visitedClasses (ENode childClassIds) =
  Fix <$> traverse (reconstructChild stableChoices table visitedClasses) childClassIds

reconstructChild :: (Language f, Ord cost) => IntMap (Maybe (BestChoice f cost)) -> ExtractionTable f a -> Set.Set ClassId -> ClassId -> Maybe (Fix f)
reconstructChild stableChoices table visitedClasses classId = do
  canonicalClassId <- extractionCanonicalClass table classId
  bestChoice <-
    IntMap.findWithDefault Nothing (classIdKey canonicalClassId) stableChoices
  erTerm <$> buildExtractionResult stableChoices table canonicalClassId visitedClasses bestChoice

extractedResultsFromChoices :: (Language f, Ord cost) => IntMap (Maybe (BestChoice f cost)) -> ExtractionTable f a -> IntMap (ExtractionResult f cost)
extractedResultsFromChoices stableChoices table =
  IntMap.mapMaybeWithKey
    (\classKey bestChoice -> bestChoice >>= buildExtractionResult stableChoices table (ClassId classKey) Set.empty)
    stableChoices

module Moonlight.EGraph.Pure.Rewrite.Instantiate
  ( RewriteWitnessChoices,
    rewriteWitnessChoicesFromEGraph,
    extendRewriteWitnessChoices,
    extractClassWitness,
    extractClassWitnessFromChoices,
    instantiatePatternTerm,
    bindingPatternResolver,
    bindingPatternResolverMaybe,
    bindingPatternResolverFromChoicesMaybe,
    resolveExistingPatternClass,
    resolveExistingPatternClassWith,
    patternFromFix,
  )
where

import Data.IntSet (IntSet)
import Moonlight.Core
  ( Language,
    Pattern (..),
    PatternVar,
    Substitution,
    lookupSubst,
  )
import Moonlight.EGraph.Pure.Extraction
  ( ExtractionChoiceSection,
    ExtractionResult (..),
    depthCost,
    extendChoiceSectionWithGraphClasses,
    extract,
    extractChoiceSection,
    extractFromChoiceSection,
    liftCostAlgebra,
    stableExtractionSnapshotFromEGraph,
    stableExtractionSnapshotTable,
  )
import Moonlight.EGraph.Pure.Kernel.HashCons
  ( lookupLeastENode,
  )
import Moonlight.EGraph.Pure.Types
  ( ClassId,
    EGraph,
    ENode (..),
    canonicalizeClassId,
  )
import Data.Fix (Fix (..))
import Moonlight.Rewrite.Runtime
  ( ExecutableRewriteMatch (..),
  )
import Moonlight.Rewrite.Runtime (RewriteApplicationError (..))
import Moonlight.Rewrite.Runtime (BinderSubstAlgebra)

instantiatePatternTerm ::
  Language f =>
  Pattern f ->
  Substitution ->
  EGraph f a ->
  Either RewriteApplicationError (Fix f)
instantiatePatternTerm patternValue substitution graph =
  case patternValue of
    PatternVar patternVar ->
      boundVariableWitness substitution graph patternVar
    PatternNode childPatterns ->
      Fix <$> traverse (\childPattern -> instantiatePatternTerm childPattern substitution graph) childPatterns

boundVariableWitness ::
  Language f =>
  Substitution ->
  EGraph f a ->
  PatternVar ->
  Either RewriteApplicationError (Fix f)
boundVariableWitness substitution graph patternVar = do
  boundClassId <-
    maybe
      (Left (RewriteMissingBinding patternVar))
      Right
      (lookupSubst patternVar substitution)
  extractClassWitness (canonicalizeClassId graph boundClassId) graph

bindingPatternResolver ::
  Language f =>
  Substitution ->
  EGraph f a ->
  PatternVar ->
  Either RewriteApplicationError (Pattern f)
bindingPatternResolver substitution graph patternVar =
  patternFromFix <$> boundVariableWitness substitution graph patternVar

bindingPatternResolverMaybe ::
  Language f =>
  Maybe (BinderSubstAlgebra f) ->
  ExecutableRewriteMatch compiledGuard guardEvidence guideEvidence f ->
  EGraph f a ->
  Maybe (PatternVar -> Either RewriteApplicationError (Pattern f))
bindingPatternResolverMaybe maybeBinderSubstAlgebra rewriteMatch graph =
  bindingPatternResolver (ermSubstitution rewriteMatch) graph <$ maybeBinderSubstAlgebra

extractClassWitness ::
  Language f =>
  ClassId ->
  EGraph f a ->
  Either RewriteApplicationError (Fix f)
extractClassWitness classId graph =
  maybe
    (Left (RewriteMissingEClass classId))
    (Right . erTerm)
    (stableExtractionSnapshotFromEGraph graph >>= extract depthCost classId)

-- | Depth-cost witness choices shared across many extractions from one graph
-- lineage.  'Nothing' marks an unstable source graph and reproduces the
-- per-extraction failure of 'extractClassWitness' at every use site.
type RewriteWitnessChoices f a = Maybe (ExtractionChoiceSection f a Int)

rewriteWitnessChoicesFromEGraph ::
  Language f =>
  EGraph f a ->
  RewriteWitnessChoices f a
rewriteWitnessChoicesFromEGraph graph =
  extractChoiceSection (liftCostAlgebra depthCost) . stableExtractionSnapshotTable
    <$> stableExtractionSnapshotFromEGraph graph

-- | Extend shared witness choices with classes freshly inserted into @graph@
-- on top of the choices' own universe; inherits the soundness conditions of
-- 'extendChoiceSectionWithGraphClasses'.
extendRewriteWitnessChoices ::
  Language f =>
  EGraph f a ->
  IntSet ->
  RewriteWitnessChoices f a ->
  RewriteWitnessChoices f a
extendRewriteWitnessChoices graph insertedKeys =
  fmap (extendChoiceSectionWithGraphClasses graph insertedKeys)

extractClassWitnessFromChoices ::
  Language f =>
  ClassId ->
  RewriteWitnessChoices f a ->
  Either RewriteApplicationError (Fix f)
extractClassWitnessFromChoices classId witnessChoices =
  maybe
    (Left (RewriteMissingEClass classId))
    (Right . erTerm)
    (extractFromChoiceSection classId =<< witnessChoices)

boundVariableWitnessFromChoices ::
  Language f =>
  Substitution ->
  RewriteWitnessChoices f a ->
  PatternVar ->
  Either RewriteApplicationError (Fix f)
boundVariableWitnessFromChoices substitution witnessChoices patternVar = do
  boundClassId <-
    maybe
      (Left (RewriteMissingBinding patternVar))
      Right
      (lookupSubst patternVar substitution)
  extractClassWitnessFromChoices boundClassId witnessChoices

bindingPatternResolverFromChoicesMaybe ::
  Language f =>
  Maybe (BinderSubstAlgebra f) ->
  ExecutableRewriteMatch compiledGuard guardEvidence guideEvidence f ->
  RewriteWitnessChoices f a ->
  Maybe (PatternVar -> Either RewriteApplicationError (Pattern f))
bindingPatternResolverFromChoicesMaybe maybeBinderSubstAlgebra rewriteMatch witnessChoices =
  ( fmap patternFromFix
      . boundVariableWitnessFromChoices (ermSubstitution rewriteMatch) witnessChoices
  )
    <$ maybeBinderSubstAlgebra

resolveExistingPatternClass ::
  Language f =>
  EGraph f a ->
  Substitution ->
  Pattern f ->
  Maybe (Maybe ClassId)
resolveExistingPatternClass graph substitution patternValue =
  resolveExistingPatternClassWith
    (canonicalizeClassId graph)
    (`lookupLeastENode` graph)
    substitution
    patternValue

resolveExistingPatternClassWith ::
  Language f =>
  (ClassId -> ClassId) ->
  (ENode f -> Maybe ClassId) ->
  Substitution ->
  Pattern f ->
  Maybe (Maybe ClassId)
resolveExistingPatternClassWith canonicalize lookupNode substitution patternValue =
  case patternValue of
    PatternVar patternVar ->
      Just (canonicalize <$> lookupSubst patternVar substitution)
    PatternNode childPatterns ->
      fmap
        ( \maybeChildren ->
            canonicalize <$> lookupNode (ENode maybeChildren)
        )
        (sequenceA =<< traverse (resolveExistingPatternClassWith canonicalize lookupNode substitution) childPatterns)

patternFromFix :: Functor f => Fix f -> Pattern f
patternFromFix (Fix termValue) =
  PatternNode (fmap patternFromFix termValue)

{-# LANGUAGE StandaloneKindSignatures #-}

-- | Region-valued compiled guard evaluation over annotated context buckets.
module Moonlight.EGraph.Pure.Guard.Region
  ( ContextFactStoreLookup (..),
    CompiledGuardRegion,
    compileGuardRegion,
    guardRegion,
    guardRegionEvidenceAtKey,
  )
where

import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Constraint
  ( Literal,
    literalPolarity,
    literalVariable,
  )
import Moonlight.Core
  ( ClassId (..),
    Language,
    Substitution,
    classIdKey,
  )
import Moonlight.EGraph.Pure.Context.AnnotatedDelta
  ( AnnotatedDeltaBuckets,
    annotatedEquivalentRegion,
    annotatedInhabitedRegion,
  )
import Moonlight.EGraph.Pure.Context.AnnotatedView
  ( annotatedContextViewAtKey,
    annotatedViewCanonicalize,
    annotatedViewLookupLeastENode,
    annotatedViewProjectChildAt,
  )
import Moonlight.EGraph.Pure.Guard.Evaluation
  ( GuardGraphView (..),
    graphGuardView,
    resolveGuardTermWith,
  )
import Moonlight.EGraph.Pure.Types
  ( EGraph,
  )
import Moonlight.Rewrite.System
  ( CompiledGuard,
    GuardAtom (..),
    GuardCapabilityResolver (..),
    GuardClauseEvidence (..),
    GuardEvidence,
    GuardLiteralEvidence (..),
    GuardTerm,
    compiledGuardClauses,
  )
import Moonlight.Rewrite.System
  ( FactId,
    FactTuple (..),
    FactWitness (..),
    FactStore,
    canonicalizeFactStore,
    guardEvidenceFromClauses,
    hasFact,
  )
import Moonlight.Sheaf.Context.Region
  ( ContextRegion,
    RegionTable,
    fromGeneratorKeys,
    regionComplementIn,
    regionFromKeys,
    regionJoin,
    regionKeys,
    regionMeet,
    regionMemberKey,
    regionTop,
    regionVoid,
  )
import Moonlight.Sheaf.Context.Site (ContextObjectKey)

type ContextFactStoreLookup :: Type -> Type
newtype ContextFactStoreLookup owner = ContextFactStoreLookup
  { contextFactStoresByKey :: Map (ContextObjectKey owner) FactStore
  }

type CompiledGuardRegion :: Type -> (Type -> Type) -> Type
data CompiledGuardRegion owner f = CompiledGuardRegion
  { cgrRegion :: !(ContextRegion owner),
    cgrEvidenceAtKey :: !(ContextObjectKey owner -> Maybe GuardEvidence)
  }

compileGuardRegion ::
  Language f =>
  RegionTable owner ->
  ContextRegion owner ->
  AnnotatedDeltaBuckets owner f ->
  EGraph f a ->
  ContextFactStoreLookup owner ->
  GuardCapabilityResolver capability ->
  ClassId ->
  Substitution ->
  CompiledGuard capability f ->
  CompiledGuardRegion owner f
compileGuardRegion table domainRegion buckets graph factStoreLookup capabilityResolver rootClassId substitution compiledGuard =
  let baseView = graphGuardView graph
      baseRootClassId = ggvCanonicalize baseView rootClassId
      environment = GuardRegionEnvironment table domainRegion buckets graph factStoreLookup capabilityResolver baseView baseRootClassId substitution
      compiledClauses = fmap (compileClauseRegion environment) (compiledGuardClauses compiledGuard)
      regionValue = regionMeet domainRegion (foldr (regionMeet . gcrRegion) (regionTop table) compiledClauses)
   in CompiledGuardRegion
        { cgrRegion = regionValue,
          cgrEvidenceAtKey = guardEvidenceFromCompiledClauses compiledClauses
        }

guardRegion :: CompiledGuardRegion owner f -> ContextRegion owner
guardRegion =
  cgrRegion
{-# INLINE guardRegion #-}

guardRegionEvidenceAtKey :: CompiledGuardRegion owner f -> ContextObjectKey owner -> Maybe GuardEvidence
guardRegionEvidenceAtKey =
  cgrEvidenceAtKey
{-# INLINE guardRegionEvidenceAtKey #-}

type GuardRegionEnvironment :: Type -> (Type -> Type) -> Type -> Type -> Type
data GuardRegionEnvironment owner f a capability = GuardRegionEnvironment
  { greTable :: !(RegionTable owner),
    greDomainRegion :: !(ContextRegion owner),
    greBuckets :: !(AnnotatedDeltaBuckets owner f),
    greGraph :: !(EGraph f a),
    greFactStoreLookup :: !(ContextFactStoreLookup owner),
    greCapabilityResolver :: !(GuardCapabilityResolver capability),
    greBaseView :: !(GuardGraphView f),
    greBaseRootClassId :: !ClassId,
    greSubstitution :: !Substitution
  }

type GuardClauseRegion :: Type -> Type
data GuardClauseRegion owner = GuardClauseRegion
  { gcrRegion :: !(ContextRegion owner),
    gcrEvidenceAtKey :: !(ContextObjectKey owner -> Maybe GuardClauseEvidence)
  }

type GuardLiteralRegion :: Type -> Type
data GuardLiteralRegion owner = GuardLiteralRegion
  { glrRegion :: !(ContextRegion owner),
    glrEvidenceAtKey :: !(ContextObjectKey owner -> [GuardLiteralEvidence])
  }

compileClauseRegion ::
  Language f =>
  GuardRegionEnvironment owner f a capability ->
  Set.Set (Literal (GuardAtom capability f)) ->
  GuardClauseRegion owner
compileClauseRegion environment literals =
  let compiledLiterals = fmap (compileLiteralRegion environment) (Set.toAscList literals)
      regionValue = foldr (regionJoin . glrRegion) regionVoid compiledLiterals
   in GuardClauseRegion
        { gcrRegion = regionValue,
          gcrEvidenceAtKey = clauseEvidenceAtKey compiledLiterals regionValue
        }

compileLiteralRegion ::
  Language f =>
  GuardRegionEnvironment owner f a capability ->
  Literal (GuardAtom capability f) ->
  GuardLiteralRegion owner
compileLiteralRegion environment literal =
  case literalVariable literal of
    ClassesEquivalent leftTerm rightTerm ->
      compileClassesEquivalentLiteral environment (literalPolarity literal) leftTerm rightTerm
    HasFact factId guardTerms ->
      compileFactLiteralRegion environment (literalPolarity literal) factId guardTerms
    HasCapability capability guardTerms ->
      compileStaticLiteral environment (literalPolarity literal) (capabilityAssessment environment capability guardTerms)

compileClassesEquivalentLiteral ::
  Language f =>
  GuardRegionEnvironment owner f a capability ->
  Bool ->
  GuardTerm f ->
  GuardTerm f ->
  GuardLiteralRegion owner
compileClassesEquivalentLiteral environment positive leftTerm rightTerm =
  let table = greTable environment
      positiveRegion = classesEquivalentPositiveRegion environment leftTerm rightTerm
      regionValue =
        if positive
          then positiveRegion
          else regionComplementIn table positiveRegion
      evidenceAtKey contextKey =
        if regionMemberKey regionValue contextKey
          then classesEquivalentEvidenceAtKey environment positive contextKey leftTerm rightTerm
          else []
   in GuardLiteralRegion
        { glrRegion = regionValue,
          glrEvidenceAtKey = evidenceAtKey
        }

compileFactLiteralRegion ::
  Language f =>
  GuardRegionEnvironment owner f a capability ->
  Bool ->
  FactId ->
  [GuardTerm f] ->
  GuardLiteralRegion owner
compileFactLiteralRegion environment positive factId guardTerms =
  let table = greTable environment
      assessments =
        Map.fromList
          [ (contextKey, assessment)
            | contextKey <- Map.keys (contextFactStoresByKey (greFactStoreLookup environment)),
              regionMemberKey (greDomainRegion environment) contextKey,
              Just assessment <- [factAssessmentAtKey environment contextKey factId guardTerms]
          ]
      satisfyingAssessments =
        Map.filter (factLiteralSatisfied positive) assessments
      regionValue =
        regionFromKeys table (Map.keys satisfyingAssessments)
      evidenceAtKey contextKey =
        maybe
          []
          (factLiteralEvidence positive)
          (Map.lookup contextKey satisfyingAssessments)
   in GuardLiteralRegion
        { glrRegion = regionValue,
          glrEvidenceAtKey = evidenceAtKey
        }

factLiteralSatisfied :: Bool -> GuardAtomAssessment -> Bool
factLiteralSatisfied positive assessment =
  gaaSatisfied assessment == positive

factLiteralEvidence :: Bool -> GuardAtomAssessment -> [GuardLiteralEvidence]
factLiteralEvidence positive assessment =
  if positive
    then gaaPositiveEvidence assessment
    else gaaNegativeEvidence assessment

compileStaticLiteral ::
  GuardRegionEnvironment owner f a capability ->
  Bool ->
  GuardAtomAssessment ->
  GuardLiteralRegion owner
compileStaticLiteral environment positive assessment =
  let table = greTable environment
      satisfied =
        if positive
          then gaaSatisfied assessment
          else not (gaaSatisfied assessment)
      evidence =
        if satisfied
          then
            if positive
              then gaaPositiveEvidence assessment
              else gaaNegativeEvidence assessment
          else []
      regionValue =
        if satisfied
          then regionTop table
          else regionVoid
   in GuardLiteralRegion
        { glrRegion = regionValue,
          glrEvidenceAtKey = const evidence
        }

classesEquivalentPositiveRegion ::
  Language f =>
  GuardRegionEnvironment owner f a capability ->
  GuardTerm f ->
  GuardTerm f ->
  ContextRegion owner
classesEquivalentPositiveRegion environment leftTerm rightTerm =
  case (,) <$> resolveBaseCanonicalTerm environment leftTerm <*> resolveBaseCanonicalTerm environment rightTerm of
    Just (leftClassId, rightClassId)
      | leftClassId == rightClassId ->
          regionTop (greTable environment)
      | otherwise ->
          annotatedEquivalentRegion
            (greTable environment)
            (greBuckets environment)
            (classIdKey leftClassId)
            (classIdKey rightClassId)
    _ ->
      fromGeneratorKeys
        (greTable environment)
        [ contextKey
          | contextKey <- regionKeys (greTable environment) (annotatedInhabitedRegion (greBuckets environment)),
            Just (leftClassId, rightClassId) <- [resolveContextPair environment contextKey leftTerm rightTerm],
            leftClassId == rightClassId
        ]

classesEquivalentEvidenceAtKey ::
  Language f =>
  GuardRegionEnvironment owner f a capability ->
  Bool ->
  ContextObjectKey owner ->
  GuardTerm f ->
  GuardTerm f ->
  [GuardLiteralEvidence]
classesEquivalentEvidenceAtKey environment positive contextKey leftTerm rightTerm =
  case resolveContextPair environment contextKey leftTerm rightTerm of
    Just (leftClassId, rightClassId)
      | positive -> [GuardClassesEqual leftClassId rightClassId]
      | otherwise -> [GuardClassesDistinct leftClassId rightClassId]
    Nothing -> [GuardAtomUnresolved]

factAssessmentAtKey ::
  Language f =>
  GuardRegionEnvironment owner f a capability ->
  ContextObjectKey owner ->
  FactId ->
  [GuardTerm f] ->
  Maybe GuardAtomAssessment
factAssessmentAtKey environment contextKey factId guardTerms = do
  factStore <-
    Map.lookup
      contextKey
      (contextFactStoresByKey (greFactStoreLookup environment))
  factTuple <-
    FactTuple
      <$> traverse
        (resolveContextCanonicalTerm environment contextKey)
        guardTerms
  let contextView = guardViewAtKey environment contextKey
      contextFactStore =
        canonicalizeFactStore (ggvCanonicalize contextView) factStore
      factWitness = FactWitness factId factTuple
      factPresent = hasFact factId factTuple contextFactStore
  pure
    GuardAtomAssessment
      { gaaSatisfied = factPresent,
        gaaPositiveEvidence =
          if factPresent
            then [GuardFactPresent factWitness]
            else [],
        gaaNegativeEvidence =
          if factPresent
            then []
            else [GuardFactAbsent factId factTuple]
      }

capabilityAssessment ::
  Language f =>
  GuardRegionEnvironment owner f a capability ->
  capability ->
  [GuardTerm f] ->
  GuardAtomAssessment
capabilityAssessment environment capability guardTerms =
  case traverse (resolveBaseCanonicalTerm environment) guardTerms of
    Nothing -> unresolvedAtom
    Just classIds ->
      let capabilityHeld = runGuardCapabilityResolver (greCapabilityResolver environment) capability classIds
       in GuardAtomAssessment
            { gaaSatisfied = capabilityHeld,
              gaaPositiveEvidence =
                if capabilityHeld
                  then [GuardCapabilityHeld]
                  else [],
              gaaNegativeEvidence =
                if capabilityHeld
                  then []
                  else [GuardCapabilityMissing]
            }

resolveBaseCanonicalTerm ::
  Language f =>
  GuardRegionEnvironment owner f a capability ->
  GuardTerm f ->
  Maybe ClassId
resolveBaseCanonicalTerm environment guardTerm =
  let view = greBaseView environment
   in ggvCanonicalize view <$> resolveGuardTermWith view (greBaseRootClassId environment) (greSubstitution environment) guardTerm

resolveContextPair ::
  Language f =>
  GuardRegionEnvironment owner f a capability ->
  ContextObjectKey owner ->
  GuardTerm f ->
  GuardTerm f ->
  Maybe (ClassId, ClassId)
resolveContextPair environment contextKey leftTerm rightTerm =
  (,) <$> resolveContextCanonicalTerm environment contextKey leftTerm <*> resolveContextCanonicalTerm environment contextKey rightTerm

resolveContextCanonicalTerm ::
  Language f =>
  GuardRegionEnvironment owner f a capability ->
  ContextObjectKey owner ->
  GuardTerm f ->
  Maybe ClassId
resolveContextCanonicalTerm environment contextKey guardTerm =
  let view = guardViewAtKey environment contextKey
      rootClassId = ggvCanonicalize view (greBaseRootClassId environment)
   in ggvCanonicalize view <$> resolveGuardTermWith view rootClassId (greSubstitution environment) guardTerm

guardViewAtKey ::
  Language f =>
  GuardRegionEnvironment owner f a capability ->
  ContextObjectKey owner ->
  GuardGraphView f
guardViewAtKey environment contextKey =
  let annotatedView = annotatedContextViewAtKey contextKey (greBuckets environment)
      graph = greGraph environment
   in GuardGraphView
        { ggvCanonicalize = annotatedViewCanonicalize annotatedView graph,
          ggvLookupLeastENode = annotatedViewLookupLeastENode annotatedView graph,
          ggvChildAt = annotatedViewProjectChildAt annotatedView graph
        }

clauseEvidenceAtKey ::
  [GuardLiteralRegion owner] ->
  ContextRegion owner ->
  ContextObjectKey owner ->
  Maybe GuardClauseEvidence
clauseEvidenceAtKey compiledLiterals regionValue contextKey =
  if regionMemberKey regionValue contextKey
    then
      Just
        ( GuardClauseEvidence
            ( foldMap
                (\compiledLiteral ->
                   if regionMemberKey (glrRegion compiledLiteral) contextKey
                     then glrEvidenceAtKey compiledLiteral contextKey
                     else []
                )
                compiledLiterals
            )
        )
    else Nothing

guardEvidenceFromCompiledClauses ::
  [GuardClauseRegion owner] ->
  ContextObjectKey owner ->
  Maybe GuardEvidence
guardEvidenceFromCompiledClauses compiledClauses contextKey =
  guardEvidenceFromClauses <$> traverse (\compiledClause -> gcrEvidenceAtKey compiledClause contextKey) compiledClauses

type GuardAtomAssessment :: Type
data GuardAtomAssessment = GuardAtomAssessment
  { gaaSatisfied :: !Bool,
    gaaPositiveEvidence :: ![GuardLiteralEvidence],
    gaaNegativeEvidence :: ![GuardLiteralEvidence]
  }

unresolvedAtom :: GuardAtomAssessment
unresolvedAtom =
  GuardAtomAssessment
    { gaaSatisfied = False,
      gaaPositiveEvidence = [],
      gaaNegativeEvidence = [GuardAtomUnresolved]
    }

{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.EGraph.Pure.Saturation.Apply
  ( EGraphApplicationResult (..),
    EGraphRewriteApplicationError (..),
    ProofTraceProjectionError (..),
    proofUpdateFromTrace,
    egraphApplyMatchesContextualReported,
    egraphApplyMatchesBaseReported,
    engineApplyMatchesWithProofReported,
    applySupportedProofRewritesReported,
  )
where

import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Maybe (maybeToList)
import Data.Set qualified as Set
import Moonlight.Differential.Context.Restriction
  ( contextRestrictionPairs,
  )
import Moonlight.Core (HasConstructorTag, Language)
import Moonlight.EGraph.Pure.Context
  ( ContextDeltaError,
    ContextEGraph,
    ContextMergePlan,
    ContextMutationTrace (..),
    ContextRebaseBatch,
    ContextRebaseReport (..),
    beginContextRebaseBatch,
    commitContextRebaseBatch,
    contextMutationTraceFromBase,
    contextCachedObjectsForExecution,
    contextRebaseBatchBaseGraph,
    contextRebaseBatchSite,
    contextRebaseBatchTrace,
    emptyContextMutationTrace,
    rebaseContextGraphAtContexts,
    planContextMerges,
    stageContextMerges,
    stageGlobalMerge,
    stageTermWithSupport,
  )
import Moonlight.EGraph.Pure.Context
  ( cegBase,
    cegSite,
    contextPreparedObjects,
  )
import Moonlight.EGraph.Pure.Change
  ( EGraphMutationResult (..),
    emtInsertedClassKeys,
    emtObservedClassUnions,
    observedClassUnionKeys,
  )
import Moonlight.EGraph.Pure.Context.Proof
  ( ProofGraph (..),
    contextualProofEvidence,
    recordAnnotatedProofStep,
    supportAwareProofEvidence,
  )
import Moonlight.EGraph.Pure.Rewrite.Env
  ( EGraphRewriteEnv (..),
  )
import Moonlight.EGraph.Pure.Rewrite.Instantiate
  ( RewriteWitnessChoices,
    bindingPatternResolverFromChoicesMaybe,
    extendRewriteWitnessChoices,
    extractClassWitnessFromChoices,
    resolveExistingPatternClass,
    rewriteWitnessChoicesFromEGraph,
  )
import Moonlight.EGraph.Pure.Rewrite.Program
  ( RewriteProgramPreview (..),
    runExecutableRewriteMatchesEGraphCommitted,
    runRewriteRhsEGraphPreviewWithResolver,
  )
import Moonlight.EGraph.Pure.Extraction
  ( ExtractionResult (..),
    ExtractionTable,
    depthCost,
    extractFromTable,
    liftCostAlgebra,
  )
import Moonlight.EGraph.Pure.Saturation.Extraction
  ( ContextualExtractionObstruction,
    contextualExtractionTable,
  )
import Moonlight.EGraph.Pure.Types (ClassId, EGraph, canonicalizeClassId)
import Moonlight.EGraph.Saturation.Context.State
  ( SaturatingProofEGraph,
    mapSaturatingContextGraph,
    sceContextGraph,
  )
import Data.Fix (Fix (..))
import Moonlight.Rewrite.Runtime
  ( ExecutableRewriteMatch (..),
  )
import Moonlight.Rewrite.System (GuardCapabilityResolver)
import Moonlight.Rewrite.System
  ( FactStore,
  )
import Moonlight.Rewrite.ProofContext
  ( ProofAnnotationBuilder,
    ProofAnnotationInput (..),
    ProofContextEvidence (..),
    ProofContextRestriction (..),
    SupportAwareProofEvidence (..),
    proofRegistryRetention,
  )
import Moonlight.Rewrite.System
  ( proofRetentionStoresAnyLog,
  )
import Moonlight.Rewrite.Runtime
  ( RewriteApplicationError,
    RulePlan,
    rpId,
    rulePlanPostSubst,
    rulePlanRhsPattern,
  )
import Moonlight.Rewrite.Runtime
  ( RewriteRuntimeCapabilities,
    runtimeBinderSubstAlgebra,
  )
import Moonlight.Rewrite.ProofContext
  ( SupportMatchWitness (..),
    SupportedRewriteMatch (..),
  )
import Moonlight.Sheaf.Context.Site
  ( PreparedContextSite,
    PreparedContextSiteError,
    contextRestrictionRegistryForObjects,
    defaultPreparedSupport,
  )
import Moonlight.Saturation.Core
  ( ApplyOutcome (..),
  )
import Moonlight.FiniteLattice
  ( SupportBasis,
    supportGenerators
  )

type EGraphApplicationResult :: Type -> Type -> Type -> (Type -> Type) -> Type
data EGraphApplicationResult owner c capability f = EGraphApplicationResult
  { egarTrace :: !(ContextMutationTrace owner c f),
    egarAppliedMatches :: ![SupportedRewriteMatch c capability f],
    egarProofRestrictionRegistryConstructions :: !Int,
    egarProofExtractionTableConstructions :: !Int
  }

type AppliedSupportedRewrite :: (Type -> Type) -> Type -> Type -> Type
data AppliedSupportedRewrite f a c = AppliedSupportedRewrite
  { asrLeftClassId :: !ClassId,
    asrRightClassId :: !ClassId,
    asrWitnessApplication :: !(Maybe (AppliedRewriteWitness f))
  }

type StagedSupportedRewrite :: (Type -> Type) -> Type -> Type -> Type
data StagedSupportedRewrite f a c = StagedSupportedRewrite
  { ssrMergeIntent :: !(SupportedMergeIntent c),
    ssrApplication :: !(AppliedSupportedRewrite f a c)
  }

type SupportedMergeIntent :: Type -> Type
data SupportedMergeIntent c
  = SupportedGlobalMerge !ClassId !ClassId
  | SupportedLocalMerge !(ContextMergePlan c)

type RewriteConstructionBatch :: Type -> Type -> (Type -> Type) -> Type -> Type -> Type
data RewriteConstructionBatch owner capability f a c = RewriteConstructionBatch
  { rcbBatch :: !(ContextRebaseBatch owner f a c),
    rcbChoices :: !(RewriteWitnessChoices f a),
    rcbStagedRewritesReversed :: ![(SupportedRewriteMatch c capability f, StagedSupportedRewrite f a c)]
  }

type ProofApplicationArtifacts :: (Type -> Type) -> Type -> Type -> Type
data ProofApplicationArtifacts f a c
  = ProofArtifactsOmitted !(ProofContextEvidence c)
  | ProofArtifactsRetained
      !(ProofContextEvidence c)
      !(Map.Map (SupportBasis c) (SupportAwareProofEvidence c))
      !Int

type AppliedRewriteWitness :: (Type -> Type) -> Type
data AppliedRewriteWitness f = AppliedRewriteWitness
  { arwLhsWitness :: !(Fix f),
    arwRhsWitness :: !(Fix f)
  }

type EGraphRewriteApplicationError :: (Type -> Type) -> Type -> Type
data EGraphRewriteApplicationError f c
  = EGraphRewriteApplicationFailed !RewriteApplicationError
  | EGraphRewriteContextDeltaFailed !(ContextDeltaError f c)
  | EGraphRewriteContextualExtractionFailed !(ContextualExtractionObstruction c)
  | EGraphRewriteContextualWitnessMissing !c !ClassId
  | EGraphRewriteProofRestrictionRegistryFailed !(PreparedContextSiteError c)
  | EGraphRewriteProofSupportEvidenceMissing !(SupportBasis c)
  | EGraphRewriteProofContextEvidenceCardinality !Int
  | EGraphRewritePreparedWitnessMissing !Int

deriving stock instance Eq c => Eq (EGraphRewriteApplicationError f c)

instance Show c => Show (EGraphRewriteApplicationError f c) where
  showsPrec precedence errorValue =
    showParen (precedence > 10) $
      case errorValue of
        EGraphRewriteApplicationFailed rewriteError ->
          showString "EGraphRewriteApplicationFailed "
            . showsPrec 11 rewriteError
        EGraphRewriteContextDeltaFailed contextDeltaError ->
          showString "EGraphRewriteContextDeltaFailed "
            . showsPrec 11 contextDeltaError
        EGraphRewriteContextualExtractionFailed extractionError ->
          showString "EGraphRewriteContextualExtractionFailed "
            . showsPrec 11 extractionError
        EGraphRewriteContextualWitnessMissing contextValue classId ->
          showString "EGraphRewriteContextualWitnessMissing "
            . showsPrec 11 contextValue
            . showChar ' '
            . showsPrec 11 classId
        EGraphRewriteProofRestrictionRegistryFailed registryError ->
          showString "EGraphRewriteProofRestrictionRegistryFailed "
            . showsPrec 11 registryError
        EGraphRewriteProofSupportEvidenceMissing supportValue ->
          showString "EGraphRewriteProofSupportEvidenceMissing "
            . showsPrec 11 supportValue
        EGraphRewriteProofContextEvidenceCardinality evidenceCount ->
          showString "EGraphRewriteProofContextEvidenceCardinality "
            . showsPrec 11 evidenceCount
        EGraphRewritePreparedWitnessMissing matchIndex ->
          showString "EGraphRewritePreparedWitnessMissing "
            . showsPrec 11 matchIndex

type ProofTraceProjectionError :: Type
newtype ProofTraceProjectionError
  = ProofTraceProjectionMissingJustification IntSet
  deriving stock (Eq, Show)

proofUpdateFromTrace ::
  ContextMutationTrace owner c f ->
  ProofGraph graph f c p ->
  Either ProofTraceProjectionError (ProofGraph graph f c p)
proofUpdateFromTrace traceValue proofGraph =
  let unionKeys =
        proofTraceUnionKeys traceValue
   in if IntSet.null unionKeys
        then Right proofGraph
        else Left (ProofTraceProjectionMissingJustification unionKeys)

proofTraceUnionKeys ::
  ContextMutationTrace owner c f ->
  IntSet
proofTraceUnionKeys traceValue =
  observedClassUnionKeys (emtObservedClassUnions (cmtBaseTrace traceValue))
    <> observedClassUnionKeys (cmtObservedLocalUnions traceValue)
{-# INLINE proofTraceUnionKeys #-}

prepareProofApplicationArtifacts ::
  Ord c =>
  Maybe c ->
  [SupportedRewriteMatch c capability f] ->
  SaturatingProofEGraph owner capability' f a c p ->
  Either (EGraphRewriteApplicationError f c) (ProofApplicationArtifacts f a c)
prepareProofApplicationArtifacts activeContext matches proofGraph
  | proofRetentionStoresAnyLog (proofRegistryRetention (pgProofRegistry proofGraph)) = do
      restrictionRegistry <-
        first EGraphRewriteProofRestrictionRegistryFailed
          (contextRestrictionRegistryForObjects registryContexts (cegSite contextGraph))
      let evidenceGroups =
            Map.fromListWith
              combineProofRestrictionEvidenceGroups
              ( (contextualContextSet, (True, Set.empty))
                  : [ (supportContextSet supportValue, (False, Set.singleton supportValue))
                    | supportValue <- Set.toAscList supports
                    ]
              )
          canonicalRestrictionPairs =
            contextRestrictionPairs restrictionRegistry
          materializedEvidenceGroups =
            fmap
              (materializeProofRestrictionEvidenceGroup activeContext canonicalRestrictionPairs)
              (Map.toAscList evidenceGroups)
          contextEvidenceValues =
            foldMap fst materializedEvidenceGroups
          supportEvidence =
            Map.unions (fmap snd materializedEvidenceGroups)
      case contextEvidenceValues of
        [contextEvidence] ->
          Right
            ( ProofArtifactsRetained
                contextEvidence
                supportEvidence
                0
            )
        _ ->
          Left
            (EGraphRewriteProofContextEvidenceCardinality (length contextEvidenceValues))
  | otherwise =
      Right (ProofArtifactsOmitted (contextualProofEvidence activeContext []))
  where
    contextGraph =
      sceContextGraph (pgGraph proofGraph)
    preparedContexts =
      contextPreparedObjects contextGraph
    preparedContextSet =
      Set.fromList preparedContexts
    contextualContextSet =
      Set.union
        (Set.fromList (maybeToList activeContext))
        preparedContextSet
    supports =
      Set.fromList (fmap srmSupport matches)
    supportContextSet supportValue =
      Set.union
        (Set.fromList (supportGenerators supportValue))
        preparedContextSet
    evidenceContextSets =
      contextualContextSet
        : fmap supportContextSet (Set.toAscList supports)
    registryContexts =
      Set.unions evidenceContextSets

combineProofRestrictionEvidenceGroups ::
  Ord c =>
  (Bool, Set.Set (SupportBasis c)) ->
  (Bool, Set.Set (SupportBasis c)) ->
  (Bool, Set.Set (SupportBasis c))
combineProofRestrictionEvidenceGroups (leftHasContext, leftSupports) (rightHasContext, rightSupports) =
  ( leftHasContext || rightHasContext,
    Set.union leftSupports rightSupports
  )

materializeProofRestrictionEvidenceGroup ::
  Ord c =>
  Maybe c ->
  [(c, c)] ->
  (Set.Set c, (Bool, Set.Set (SupportBasis c))) ->
  ([ProofContextEvidence c], Map.Map (SupportBasis c) (SupportAwareProofEvidence c))
materializeProofRestrictionEvidenceGroup activeContext canonicalPairs (contexts, (includeContextEvidence, supports)) =
  let restrictions =
        fmap
          (uncurry ProofContextRestriction)
          ( filter
              ( \(sourceContext, targetContext) ->
                  Set.member sourceContext contexts
                    && Set.member targetContext contexts
              )
              canonicalPairs
          )
      contextEvidence =
        [ ProofContextEvidence
            { pceActiveContext = activeContext,
              pceRestrictions = restrictions
            }
        | includeContextEvidence
        ]
      supportEvidence =
        Map.fromSet
          ( \supportValue ->
              SupportAwareProofEvidence
                { sapeSupport = supportValue,
                  sapeRestrictions = restrictions
                }
          )
          supports
   in forceProofContextRestrictions restrictions
        `seq` (contextEvidence, supportEvidence)

forceProofContextRestrictions :: [ProofContextRestriction c] -> ()
forceProofContextRestrictions =
  foldr
    ( \(ProofContextRestriction sourceContext targetContext) remainingRestrictions ->
        sourceContext `seq` targetContext `seq` remainingRestrictions
    )
    ()

proofContextEvidence :: ProofApplicationArtifacts f a c -> ProofContextEvidence c
proofContextEvidence proofArtifacts =
  case proofArtifacts of
    ProofArtifactsOmitted contextEvidence ->
      contextEvidence
    ProofArtifactsRetained contextEvidence _ _ ->
      contextEvidence

proofSupportEvidence ::
  Ord c =>
  SupportBasis c ->
  ProofApplicationArtifacts f a c ->
  Either (EGraphRewriteApplicationError f c) (SupportAwareProofEvidence c)
proofSupportEvidence supportValue proofArtifacts =
  case proofArtifacts of
    ProofArtifactsOmitted _ ->
      Right (supportAwareProofEvidence supportValue [])
    ProofArtifactsRetained _ supportEvidence _ ->
      maybe
        (Left (EGraphRewriteProofSupportEvidenceMissing supportValue))
        Right
        (Map.lookup supportValue supportEvidence)

proofArtifactConstructionCounts :: ProofApplicationArtifacts f a c -> (Int, Int)
proofArtifactConstructionCounts proofArtifacts =
  case proofArtifacts of
    ProofArtifactsOmitted _ ->
      (0, 0)
    ProofArtifactsRetained _ _ extractionTableCount ->
      (1, extractionTableCount)

proofUpdateFromCommittedApplication ::
  Ord c =>
  ProofAnnotationBuilder c p ->
  ProofApplicationArtifacts f a c ->
  SupportedRewriteMatch c capability f ->
  AppliedSupportedRewrite f a c ->
  SaturatingProofEGraph owner capability' f a c p ->
  Either
    (EGraphRewriteApplicationError f c)
    (SaturatingProofEGraph owner capability' f a c p)
proofUpdateFromCommittedApplication proofAnnotationBuilder proofArtifacts supportedRewriteMatch appliedRewrite proofGraphAfter = do
  supportEvidence <-
    proofSupportEvidence (srmSupport supportedRewriteMatch) proofArtifacts
  let rewriteMatch = srmMatch supportedRewriteMatch
      preparedRewrite = ermRule rewriteMatch
      substitution = ermSubstitution rewriteMatch
      consumedDerivations =
        foldMap smwFactDerivations (srmWitnesses supportedRewriteMatch)
      annotationInput =
        ProofAnnotationInput
          { paiRewriteRuleId = rpId preparedRewrite,
            paiLhsClass = asrLeftClassId appliedRewrite,
            paiRhsClass = asrRightClassId appliedRewrite,
            paiSubstitution = substitution,
            paiGuardEvidence = ermGuardEvidence rewriteMatch,
            paiGuideEvidence = ermGuideEvidence rewriteMatch,
            paiFactDerivations = consumedDerivations,
            paiContextEvidence = Just (proofContextEvidence proofArtifacts),
            paiSupportEvidence = Just supportEvidence
          }
      maybeWitnessApplication =
        asrWitnessApplication appliedRewrite
      canonicalize =
        canonicalizeClassId (cegBase (sceContextGraph (pgGraph proofGraphAfter)))
  Right
    ( recordAnnotatedProofStep
        canonicalize
        proofAnnotationBuilder
        annotationInput
        (arwLhsWitness <$> maybeWitnessApplication)
        (arwRhsWitness <$> maybeWitnessApplication)
        proofGraphAfter
    )

prepareProofLeftWitnesses ::
  (Language f, Ord c) =>
  ProofApplicationArtifacts f a c ->
  ContextEGraph owner f a c ->
  [(Int, SupportedRewriteMatch c capability f)] ->
  Either
    (EGraphRewriteApplicationError f c)
    (ProofApplicationArtifacts f a c, IntMap (Fix f))
prepareProofLeftWitnesses proofArtifacts contextGraph indexedMatches =
  case proofArtifacts of
    ProofArtifactsOmitted _ ->
      Right (proofArtifacts, IntMap.empty)
    ProofArtifactsRetained contextEvidence supportEvidence _ -> do
      witnessGroups <-
        traverse
          (extractProofWitnessGroup contextGraph)
          (Map.toAscList groupedMatches)
      let extractionTableCount =
            Map.size groupedMatches
      Right
        ( ProofArtifactsRetained
            contextEvidence
            supportEvidence
            extractionTableCount,
          IntMap.fromList (concat witnessGroups)
        )
  where
    groupedMatches =
      Map.fromListWith
        (flip (<>))
        [ (contextValue, [(matchIndex, supportedRewriteMatch)])
        | (matchIndex, supportedRewriteMatch) <- indexedMatches,
          Just (contextValue, _supportWitness) <- [Map.lookupMin (srmWitnesses supportedRewriteMatch)]
        ]

extractProofWitnessGroup ::
  (Language f, Ord c) =>
  ContextEGraph owner f a c ->
  (c, [(Int, SupportedRewriteMatch c capability f)]) ->
  Either
    (EGraphRewriteApplicationError f c)
    [(Int, Fix f)]
extractProofWitnessGroup contextGraph (contextValue, indexedMatches) = do
  table <-
    first EGraphRewriteContextualExtractionFailed
      (contextualExtractionTable contextValue contextGraph)
  traverse
    ( \(matchIndex, supportedRewriteMatch) ->
        do
          leftWitness <-
            contextualLeftWitness contextValue supportedRewriteMatch table
          forceProofWitnessStructure leftWitness
            `seq` Right (matchIndex, leftWitness)
    )
    indexedMatches

forceProofWitnessStructure :: Foldable f => Fix f -> ()
forceProofWitnessStructure (Fix termLayer) =
  foldr
    (\childWitness remainingWitnesses -> forceProofWitnessStructure childWitness `seq` remainingWitnesses)
    ()
    termLayer

attachPreparedLeftWitness ::
  ProofApplicationArtifacts f a c ->
  IntMap (Fix f) ->
  (Int, SupportedRewriteMatch c capability f) ->
  Either
    (EGraphRewriteApplicationError f c)
    (SupportedRewriteMatch c capability f, Maybe (Fix f))
attachPreparedLeftWitness proofArtifacts witnessesByMatchIndex (matchIndex, supportedRewriteMatch) =
  case proofArtifacts of
    ProofArtifactsOmitted _ ->
      Right (supportedRewriteMatch, Nothing)
    ProofArtifactsRetained _ _ _ ->
      case Map.lookupMin (srmWitnesses supportedRewriteMatch) of
        Nothing ->
          Right (supportedRewriteMatch, Nothing)
        Just _ ->
          maybe
            (Left (EGraphRewritePreparedWitnessMissing matchIndex))
            (Right . (,) supportedRewriteMatch . Just)
            (IntMap.lookup matchIndex witnessesByMatchIndex)

contextualLeftWitness ::
  Language f =>
  c ->
  SupportedRewriteMatch c capability f ->
  ExtractionTable f a ->
  Either (EGraphRewriteApplicationError f c) (Fix f)
contextualLeftWitness contextValue supportedRewriteMatch table =
  let leftClassId =
        ermRootClass (srmMatch supportedRewriteMatch)
   in maybe
        (Left (EGraphRewriteContextualWitnessMissing contextValue leftClassId))
        (Right . erTerm)
        (extractFromTable (liftCostAlgebra depthCost) leftClassId table)

stageSupportedRewriteConstruction ::
  (HasConstructorTag f, Ord c) =>
  RewriteRuntimeCapabilities (GuardCapabilityResolver capability) f ->
  RewriteConstructionBatch owner capability f a c ->
  (SupportedRewriteMatch c capability f, Maybe (Fix f)) ->
  Either (EGraphRewriteApplicationError f c) (RewriteConstructionBatch owner capability f a c)
stageSupportedRewriteConstruction runtimeCapabilities constructionBatch (supportedRewriteMatch, maybeLeftWitness) = do
  let supportValue =
        srmSupport supportedRewriteMatch
      leftClassId =
        ermRootClass (srmMatch supportedRewriteMatch)
      batchValue =
        rcbBatch constructionBatch
      baseGraph =
        contextRebaseBatchBaseGraph batchValue
      stagedChoices =
        extendRewriteWitnessChoices
          baseGraph
          (emtInsertedClassKeys (cmtBaseTrace (contextRebaseBatchTrace batchValue)))
          (rcbChoices constructionBatch)
  rhsValue <-
    instantiateSupportedRewriteRhs
      runtimeCapabilities
      supportedRewriteMatch
      stagedChoices
      baseGraph
  (rhsClassId, insertionBatch) <-
    first
      EGraphRewriteContextDeltaFailed
      (stageTermWithSupport supportValue rhsValue batchValue)
  mergeIntent <-
    first EGraphRewriteContextDeltaFailed
      (planSupportMergeIntent (contextRebaseBatchSite insertionBatch) supportValue leftClassId rhsClassId insertionBatch)
  let stagedApplication =
        AppliedSupportedRewrite
          { asrLeftClassId = leftClassId,
            asrRightClassId = rhsClassId,
            asrWitnessApplication =
              ( \leftWitness ->
                  AppliedRewriteWitness
                    { arwLhsWitness = leftWitness,
                      arwRhsWitness = rhsValue
                    }
              )
                <$> maybeLeftWitness
          }
  pure
    constructionBatch
      { rcbBatch = insertionBatch,
        rcbChoices = stagedChoices,
        rcbStagedRewritesReversed =
          ( supportedRewriteMatch,
            StagedSupportedRewrite
              { ssrMergeIntent = mergeIntent,
                ssrApplication = stagedApplication
              }
          )
            : rcbStagedRewritesReversed constructionBatch
      }

instantiateSupportedRewriteRhs ::
  Language f =>
  RewriteRuntimeCapabilities (GuardCapabilityResolver capability) f ->
  SupportedRewriteMatch c capability f ->
  RewriteWitnessChoices f a ->
  EGraph f a ->
  Either (EGraphRewriteApplicationError f c) (Fix f)
instantiateSupportedRewriteRhs runtimeCapabilities supportedRewriteMatch witnessChoices baseGraph =
  do
    let rewriteMatch =
          srmMatch supportedRewriteMatch
        rewriteRule =
          ermRule rewriteMatch
        previewRhs = do
          rewriteProgramPreview <-
            first
              EGraphRewriteApplicationFailed
              ( runRewriteRhsEGraphPreviewWithResolver
                  ( bindingPatternResolverFromChoicesMaybe
                      (runtimeBinderSubstAlgebra runtimeCapabilities)
                      rewriteMatch
                      witnessChoices
                  )
                  runtimeCapabilities
                  rewriteMatch
                  baseGraph
              )
          Right
            ( rppResult rewriteProgramPreview,
              extendRewriteWitnessChoices
                (rppPreviewGraph rewriteProgramPreview)
                (emtInsertedClassKeys (rppInsertionTrace rewriteProgramPreview))
                witnessChoices
            )
    (rhsClassId, rhsChoices) <-
      case rulePlanPostSubst rewriteRule of
        Nothing ->
          case
              resolveExistingPatternClass
                baseGraph
                (ermSubstitution rewriteMatch)
                (rulePlanRhsPattern rewriteRule)
            of
            Just (Just existingRhsClassId) ->
              Right (existingRhsClassId, witnessChoices)
            _ ->
              previewRhs
        Just _postMatchSubst ->
          previewRhs
    first
      EGraphRewriteApplicationFailed
      (extractClassWitnessFromChoices rhsClassId rhsChoices)

planSupportMergeIntent ::
  (Language f, Ord c) =>
  PreparedContextSite owner c ->
  SupportBasis c ->
  ClassId ->
  ClassId ->
  ContextRebaseBatch owner f a c ->
  Either (ContextDeltaError f c) (SupportedMergeIntent c)
planSupportMergeIntent site supportValue leftClassId rightClassId batchValue =
  if isPreparedGlobalSupport site supportValue
    then Right (SupportedGlobalMerge leftClassId rightClassId)
    else
      fmap SupportedLocalMerge
        ( planContextMerges
            (supportGenerators supportValue)
            leftClassId
            rightClassId
            batchValue
        )

stageSupportMergeIntent ::
  (Language f, Ord c) =>
  SupportedMergeIntent c ->
  ContextRebaseBatch owner f a c ->
  Either (ContextDeltaError f c) (ContextRebaseBatch owner f a c)
stageSupportMergeIntent mergeIntent batchValue =
  case mergeIntent of
    SupportedGlobalMerge leftClass rightClass ->
      stageGlobalMerge leftClass rightClass batchValue
    SupportedLocalMerge mergePlan ->
      stageContextMerges mergePlan batchValue

stageConstructedRewriteMerge ::
  (Language f, Ord c) =>
  ContextRebaseBatch owner f a c ->
  (SupportedRewriteMatch c capability f, StagedSupportedRewrite f a c) ->
  Either (EGraphRewriteApplicationError f c) (ContextRebaseBatch owner f a c)
stageConstructedRewriteMerge batchValue (_, stagedRewrite) =
  first
    EGraphRewriteContextDeltaFailed
    (stageSupportMergeIntent (ssrMergeIntent stagedRewrite) batchValue)

stageConstructedRewriteMerges ::
  (Language f, Ord c) =>
  RewriteConstructionBatch owner capability f a c ->
  Either (EGraphRewriteApplicationError f c) (ContextRebaseBatch owner f a c)
stageConstructedRewriteMerges constructionBatch =
  foldM
    stageConstructedRewriteMerge
    (rcbBatch constructionBatch)
    (reverse (rcbStagedRewritesReversed constructionBatch))

egraphApplyMatchesContextualReported ::
  ( HasConstructorTag f,
    Ord c
  ) =>
  RewriteRuntimeCapabilities (GuardCapabilityResolver capability) f ->
  [SupportedRewriteMatch c capability f] ->
  ContextEGraph owner f a c ->
  Either
    (EGraphRewriteApplicationError f c)
    (ApplyOutcome (EGraphApplicationResult owner c capability f) (ContextEGraph owner f a c))
egraphApplyMatchesContextualReported runtimeCapabilities matches contextGraph =
  case matches of
    [] ->
      Right (ApplyOutcome contextGraph (emptyEGraphApplicationResult contextGraph))
    _ -> do
      constructionBatch <-
        foldM
          (stageSupportedRewriteConstruction runtimeCapabilities)
          RewriteConstructionBatch
            { rcbBatch = beginContextRebaseBatch contextGraph,
              rcbChoices = rewriteWitnessChoicesFromEGraph (cegBase contextGraph),
              rcbStagedRewritesReversed = []
            }
          (fmap (\supportedRewriteMatch -> (supportedRewriteMatch, Nothing)) matches)
      stagedBatch <- stageConstructedRewriteMerges constructionBatch
      (rebaseReport, updatedGraph) <-
        first EGraphRewriteContextDeltaFailed (commitContextRebaseBatch stagedBatch)
      Right
        ApplyOutcome
          { aoState = updatedGraph,
            aoEffect =
              EGraphApplicationResult
                { egarTrace = crrTrace rebaseReport,
                  egarAppliedMatches = matches,
                  egarProofRestrictionRegistryConstructions = 0,
                  egarProofExtractionTableConstructions = 0
                }
          }

emptyEGraphApplicationResult ::
  ContextEGraph owner f a c ->
  EGraphApplicationResult owner c capability f
emptyEGraphApplicationResult contextGraph =
  EGraphApplicationResult
    { egarTrace = emptyContextMutationTrace (cegBase contextGraph),
      egarAppliedMatches = [],
      egarProofRestrictionRegistryConstructions = 0,
      egarProofExtractionTableConstructions = 0
    }
{-# INLINE emptyEGraphApplicationResult #-}

egraphApplyMatchesBaseReported ::
  (Language f, Ord c) =>
  RewriteRuntimeCapabilities (GuardCapabilityResolver capability) f ->
  FactStore ->
  [SupportedRewriteMatch c capability f] ->
  ContextEGraph owner f a c ->
  Either (EGraphRewriteApplicationError f c) (ApplyOutcome (EGraphApplicationResult owner c capability f) (ContextEGraph owner f a c))
egraphApplyMatchesBaseReported runtimeCapabilities factStore matches contextGraph = do
  rewriteResult <-
    first
      EGraphRewriteApplicationFailed
      ( runExecutableRewriteMatchesEGraphCommitted
          ( EGraphRewriteEnv
              { ereFactStore = factStore,
                ereRuntimeCapabilities = runtimeCapabilities
              }
          )
          (fmap srmMatch matches)
          (cegBase contextGraph)
      )
  updatedGraph <-
    first EGraphRewriteContextDeltaFailed
      ( rebaseContextGraphAtContexts
          (Set.fromList (contextCachedObjectsForExecution contextGraph))
          (emrGraph rewriteResult)
          contextGraph
      )
  pure
    ApplyOutcome
      { aoState = updatedGraph,
        aoEffect =
          EGraphApplicationResult
            { egarTrace = contextMutationTraceFromBase (emrTrace rewriteResult),
              egarAppliedMatches = matches,
              egarProofRestrictionRegistryConstructions = 0,
              egarProofExtractionTableConstructions = 0
            }
      }

engineApplyMatchesWithProofReported ::
  ( HasConstructorTag f,
    Ord c
  ) =>
  RewriteRuntimeCapabilities (GuardCapabilityResolver capability) f ->
  ProofAnnotationBuilder c p ->
  Maybe c ->
  [SupportedRewriteMatch c capability f] ->
  SaturatingProofEGraph owner capability f a c p ->
  Either
    (EGraphRewriteApplicationError f c)
    (ApplyOutcome (EGraphApplicationResult owner c capability f) (SaturatingProofEGraph owner capability f a c p))
engineApplyMatchesWithProofReported runtimeCapabilities proofBuilder activeContext matches proofGraph =
  fmap
    ( \(updatedProofGraph, applicationResult) ->
        ApplyOutcome
          { aoState = updatedProofGraph,
            aoEffect = applicationResult
          }
    )
    ( applySupportedProofRewritesReported
        activeContext
        runtimeCapabilities
        proofBuilder
        matches
        proofGraph
    )

applySupportedProofRewritesReported ::
  (HasConstructorTag f, Ord c) =>
  Maybe c ->
  RewriteRuntimeCapabilities (GuardCapabilityResolver capability) f ->
  ProofAnnotationBuilder c p ->
  [SupportedRewriteMatch c capability f] ->
  SaturatingProofEGraph owner capability f a c p ->
  Either
    (EGraphRewriteApplicationError f c)
    (SaturatingProofEGraph owner capability f a c p, EGraphApplicationResult owner c capability f)
applySupportedProofRewritesReported activeContext runtimeCapabilities proofAnnotationBuilder matches proofGraph =
  case matches of
    [] ->
      Right
        ( proofGraph,
          emptyEGraphApplicationResult (sceContextGraph (pgGraph proofGraph))
        )
    _ -> do
      initialProofArtifacts <-
        prepareProofApplicationArtifacts activeContext matches proofGraph
      let authoritativeGraph =
            sceContextGraph (pgGraph proofGraph)
          indexedMatches =
            zip [0 :: Int ..] matches
      (finalProofArtifacts, witnessesByMatchIndex) <-
        prepareProofLeftWitnesses
          initialProofArtifacts
          authoritativeGraph
          indexedMatches
      constructionInputs <-
        traverse
          (attachPreparedLeftWitness finalProofArtifacts witnessesByMatchIndex)
          indexedMatches
      constructionBatch <-
        foldM
          (stageSupportedRewriteConstruction runtimeCapabilities)
          RewriteConstructionBatch
            { rcbBatch = beginContextRebaseBatch authoritativeGraph,
              rcbChoices = rewriteWitnessChoicesFromEGraph (cegBase authoritativeGraph),
              rcbStagedRewritesReversed = []
            }
          constructionInputs
      stagedBatch <- stageConstructedRewriteMerges constructionBatch
      (rebaseReport, updatedContextGraph) <-
        first EGraphRewriteContextDeltaFailed (commitContextRebaseBatch stagedBatch)
      let committedProofGraph =
            proofGraph
              { pgGraph =
                  mapSaturatingContextGraph
                    (const updatedContextGraph)
                    (pgGraph proofGraph)
              }
          committedApplications =
            [ (supportedRewriteMatch, ssrApplication stagedRewrite)
            | (supportedRewriteMatch, stagedRewrite) <-
                reverse (rcbStagedRewritesReversed constructionBatch)
            ]
          (restrictionRegistryConstructions, extractionTableConstructions) =
            proofArtifactConstructionCounts finalProofArtifacts
      recordedProofGraph <-
        foldM
          ( \currentProofGraph (supportedRewriteMatch, appliedRewrite) ->
              proofUpdateFromCommittedApplication
                proofAnnotationBuilder
                finalProofArtifacts
                supportedRewriteMatch
                appliedRewrite
                currentProofGraph
          )
          committedProofGraph
          committedApplications
      Right
        ( recordedProofGraph,
          EGraphApplicationResult
            { egarTrace = crrTrace rebaseReport,
              egarAppliedMatches = matches,
              egarProofRestrictionRegistryConstructions = restrictionRegistryConstructions,
              egarProofExtractionTableConstructions = extractionTableConstructions
            }
        )

isPreparedGlobalSupport :: Eq c => PreparedContextSite owner c -> SupportBasis c -> Bool
isPreparedGlobalSupport site supportValue =
  defaultPreparedSupport site == supportValue
{-# INLINE isPreparedGlobalSupport #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Moonlight.EGraph.Pure.Saturation.Front.Binding.Language
  ( BindingLanguageRelations (..),
    declareBindingLanguageRelations,
    substitutionAllowedGuard,
    BindingLanguageSyntax (..),
    BindingSubstitutionSite (..),
    BindingSubstitutionDecision (..),
    BindingSubstitutionOutcome (..),
    BindingFresheningPlan (..),
    BindingFresheningCosection (..),
    BindingFresheningSyntax (..),
    BindingRewriteGuard (..),
    BindingGeneratedRewrite (..),
    BindingElaboration (..),
    BindingLanguageReport (..),
    BindingLanguageIngestion (..),
    BindingLanguageError (..),
    compileBindingLanguageTerm,
    elaborateBindingLanguagePlan,
    compileBindingElaboration,
    elaborateBindingElaborationPlan,
    emitBindingElaboration,
    ingestBindingLanguageTerm,
  )
where

import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Cosheaf.Cosection
  ( CosectionClassKey (..),
    CosectionRepresentative (..),
    GlobalCosection (..),
  )
import Moonlight.EGraph.Pure.Saturation.Front
  ( ContextRef,
    EGraphFrontM,
    FrontGuardAtom,
    RelationRef,
    RulesetM,
    RulesetRef,
    Term,
    atContext,
    has,
    relationNamed,
    rewriteNamed,
    rulesetNamed,
    when_,
    (==>),
  )
import Moonlight.EGraph.Pure.Saturation.Front.Binding
  ( BindingChild (..),
    BindingFact (..),
    BindingFactArg (..),
    BindingFactArgs (..),
    BindingIngestError,
    BindingIngestion (..),
    BindingPath,
    BindingPathSegment,
    BindingPlan,
    BindingPlanEntry (..),
    BindingRootName,
    appendBindingPlanEntries,
    augmentBindingPlanFacts,
    bindingPathChildNamed,
    bindingPathName,
    bindingPlanEntries,
    bindingPlanRootPath,
    emitBindingPlan,
  )
import Moonlight.EGraph.Pure.Saturation.Front.Binding.Scoped
  ( ScopedBindingNode (..),
    ScopedBindingResolvedChild (..),
    ScopedBindingSyntax,
    ScopedBindingTree (..),
    bindingPlanFromScopedBindingTree,
    compileScopedBindingTree,
  )
import Data.Fix (Fix)
import Moonlight.Rewrite.DSL qualified as DSLRule
import Moonlight.Rewrite.DSL (RewriteSignature)

data BindingLanguageRelations sig = BindingLanguageRelations
  { blrSubstitutionAllowed :: !(RelationRef '["Expr"])
  }

declareBindingLanguageRelations ::
  String ->
  EGraphFrontM sig analysis context (BindingLanguageRelations sig)
declareBindingLanguageRelations rawPrefix =
  BindingLanguageRelations
    <$> relationNamed (rawPrefix <> "/substitution-allowed")

substitutionAllowedGuard ::
  BindingLanguageRelations sig ->
  Term sig "Expr" ->
  DSLRule.Guard sig FrontGuardAtom
substitutionAllowedGuard relations =
  has (blrSubstitutionAllowed relations)

data BindingLanguageSyntax f binder = BindingLanguageSyntax
  { blsOccurrencesAt :: !(BindingPath -> Fix f -> Set binder),
    blsBindersEnteringChild :: !(BindingPath -> Fix f -> BindingPathSegment -> Fix f -> Set binder),
    blsSubstitutionSitesAt :: !(forall context scope. ScopedBindingNode f context scope -> Either (BindingLanguageError binder) [BindingSubstitutionSite binder])
  }

data BindingSubstitutionSite binder = BindingSubstitutionSite
  { bssBinder :: !binder,
    bssBodyPath :: !BindingPath,
    bssArgumentPath :: !BindingPath
  }
  deriving stock (Eq, Ord, Show)

data BindingSubstitutionDecision binder = BindingSubstitutionDecision
  { bsdRedexPath :: !BindingPath,
    bsdBinder :: !binder,
    bsdBodyPath :: !BindingPath,
    bsdArgumentPath :: !BindingPath,
    bsdArgumentFreeBinders :: !(Set binder),
    bsdBodyCapturingBinders :: !(Set binder),
    bsdAllowed :: !Bool
  }
  deriving stock (Eq, Ord, Show)

data BindingSubstitutionOutcome binder
  = BindingSubstitutionAlreadyAllowed !(BindingSubstitutionDecision binder)
  | BindingSubstitutionNeedsFreshening !(BindingFresheningPlan binder)
  deriving stock (Eq, Ord, Show)

data BindingFresheningPlan binder = BindingFresheningPlan
  { bfpRedexPath :: !BindingPath,
    bfpFreshenedPath :: !BindingPath,
    bfpCapturedBinders :: !(Set binder),
    bfpRenames :: !(Map binder binder)
  }
  deriving stock (Eq, Ord, Show)

data BindingFresheningCosection binder = BindingFresheningCosection
  { bfcRepresentatives :: ![CosectionRepresentative BindingPath (BindingFresheningPlan binder)],
    bfcGlobals :: ![GlobalCosection BindingPath (BindingFresheningPlan binder)]
  }
  deriving stock (Eq, Ord, Show)

data BindingFresheningSyntax f sig binder = BindingFresheningSyntax
  { bfsFreshenBinders ::
      !( BindingSubstitutionDecision binder ->
         Either (BindingLanguageError binder) (Map binder binder)
       ),
    bfsFreshenedRedex ::
      !( forall context scope.
         Map binder binder ->
         ScopedBindingTree f context scope ->
         BindingSubstitutionDecision binder ->
         Either (BindingLanguageError binder) (Term sig "Expr")
       ),
    bfsContractedResult ::
      !( forall context scope.
         Map binder binder ->
         ScopedBindingTree f context scope ->
         BindingSubstitutionDecision binder ->
         Either (BindingLanguageError binder) (Term sig "Expr")
       )
  }

data BindingRewriteGuard sig
  = BindingRewriteUnguarded
  | BindingRewriteRequiresFact !(RelationRef '["Expr"]) !(Term sig "Expr")

data BindingGeneratedRewrite sig = BindingGeneratedRewrite
  { bgrName :: !String,
    bgrContextPath :: !BindingPath,
    bgrLhs :: !(Term sig "Expr"),
    bgrRhs :: !(Term sig "Expr"),
    bgrGuard :: !(BindingRewriteGuard sig)
  }

data BindingElaboration sig context binder = BindingElaboration
  { bePlan :: !(BindingPlan sig context),
    beReport :: !(BindingLanguageReport binder),
    beGeneratedRules :: ![BindingGeneratedRewrite sig]
  }

data BindingLanguageReport binder = BindingLanguageReport
  { blrSubstitutionDecisions :: ![BindingSubstitutionDecision binder],
    blrSubstitutionOutcomes :: ![BindingSubstitutionOutcome binder],
    blrCaptureObstructions :: ![BindingSubstitutionDecision binder],
    blrFresheningCosection :: !(BindingFresheningCosection binder)
  }
  deriving stock (Eq, Ord, Show)

data BindingLanguageIngestion sig context binder = BindingLanguageIngestion
  { bliBindingIngestion :: !(BindingIngestion sig context),
    bliReport :: !(BindingLanguageReport binder)
  }

data BindingLanguageError binder
  = BindingLanguageIngestError !BindingIngestError
  | BindingLanguageUnknownPath !BindingPath
  | BindingLanguageMissingGeneratedContext !BindingPath
  | BindingLanguageUnexpectedSubstitutionShape !BindingPath
  | BindingLanguageFreshNameExhausted !binder
  deriving stock (Eq, Ord, Show)

data LexicalSummary binder = LexicalSummary
  { lsFreeBinders :: !(Set binder),
    lsCapturersByTarget :: !(Map binder (Set binder))
  }
  deriving stock (Eq, Ord, Show)

occurrenceLexicalSummary ::
  Set binder ->
  LexicalSummary binder
occurrenceLexicalSummary occurrences =
  LexicalSummary
    { lsFreeBinders = occurrences,
      lsCapturersByTarget = Map.fromSet (const Set.empty) occurrences
    }
{-# INLINE occurrenceLexicalSummary #-}

appendLexicalSummary ::
  Ord binder =>
  LexicalSummary binder ->
  LexicalSummary binder ->
  LexicalSummary binder
appendLexicalSummary left right =
  LexicalSummary
    { lsFreeBinders = Set.union (lsFreeBinders left) (lsFreeBinders right),
      lsCapturersByTarget =
        Map.unionWith
          Set.union
          (lsCapturersByTarget left)
          (lsCapturersByTarget right)
    }
{-# INLINE appendLexicalSummary #-}

bindChildLexicalSummary ::
  Ord binder =>
  Set binder ->
  LexicalSummary binder ->
  LexicalSummary binder
bindChildLexicalSummary edgeBinders childSummary =
  LexicalSummary
    { lsFreeBinders = Set.difference (lsFreeBinders childSummary) edgeBinders,
      lsCapturersByTarget =
        Map.foldlWithKey'
          liftTarget
          Map.empty
          (lsCapturersByTarget childSummary)
    }
  where
    liftTarget summaries targetBinder capturers
      | Set.member targetBinder edgeBinders =
          summaries
      | otherwise =
          Map.insertWith
            Set.union
            targetBinder
            (Set.union capturers (Set.delete targetBinder edgeBinders))
            summaries
{-# INLINE bindChildLexicalSummary #-}

compileBindingLanguageTerm ::
  Ord binder =>
  BindingLanguageRelations sig ->
  BindingLanguageSyntax f binder ->
  ScopedBindingSyntax f sig context scope ->
  BindingRootName ->
  Fix f ->
  Either
    (BindingLanguageError binder)
    (BindingLanguageReport binder, BindingPlan sig context)
compileBindingLanguageTerm relations languageSyntax scopedSyntax rawRootName rootTerm = do
  tree <-
    first BindingLanguageIngestError $
      compileScopedBindingTree scopedSyntax rawRootName rootTerm
  plan <-
    first BindingLanguageIngestError $
      bindingPlanFromScopedBindingTree scopedSyntax tree
  elaborateBindingLanguagePlan relations languageSyntax tree plan

elaborateBindingLanguagePlan ::
  Ord binder =>
  BindingLanguageRelations sig ->
  BindingLanguageSyntax f binder ->
  ScopedBindingTree f context scope ->
  BindingPlan sig context ->
  Either
    (BindingLanguageError binder)
    (BindingLanguageReport binder, BindingPlan sig context)
elaborateBindingLanguagePlan relations languageSyntax tree plan = do
  decisions <-
    bindingLanguageDecisions languageSyntax tree
  let factsByPath =
        substitutionAllowedFactsByPath relations decisions
      report =
        safetyOnlyReport decisions
  elaboratedPlan <-
    first BindingLanguageIngestError $
      augmentBindingPlanFacts
        (\entry -> Map.findWithDefault [] (bpePath entry) factsByPath)
        plan
  pure (report, elaboratedPlan)

compileBindingElaboration ::
  Ord binder =>
  BindingLanguageRelations sig ->
  BindingLanguageSyntax f binder ->
  BindingFresheningSyntax f sig binder ->
  ScopedBindingSyntax f sig context scope ->
  BindingRootName ->
  Fix f ->
  Either
    (BindingLanguageError binder)
    (BindingElaboration sig context binder)
compileBindingElaboration relations languageSyntax fresheningSyntax scopedSyntax rawRootName rootTerm = do
  tree <-
    first BindingLanguageIngestError $
      compileScopedBindingTree scopedSyntax rawRootName rootTerm
  plan <-
    first BindingLanguageIngestError $
      bindingPlanFromScopedBindingTree scopedSyntax tree
  elaborateBindingElaborationPlan relations languageSyntax fresheningSyntax tree plan

elaborateBindingElaborationPlan ::
  Ord binder =>
  BindingLanguageRelations sig ->
  BindingLanguageSyntax f binder ->
  BindingFresheningSyntax f sig binder ->
  ScopedBindingTree f context scope ->
  BindingPlan sig context ->
  Either
    (BindingLanguageError binder)
    (BindingElaboration sig context binder)
elaborateBindingElaborationPlan relations languageSyntax fresheningSyntax tree plan = do
  decisions <-
    bindingLanguageDecisions languageSyntax tree
  artifacts <-
    bindingFresheningArtifacts relations fresheningSyntax tree plan decisions
  planWithAllowedFacts <-
    first BindingLanguageIngestError $
      augmentBindingPlanFacts
        (\entry -> Map.findWithDefault [] (bpePath entry) (bfaAllowedFactsByPath artifacts))
        plan
  elaboratedPlan <-
    first BindingLanguageIngestError $
      appendBindingPlanEntries (bfaFreshenedEntries artifacts) planWithAllowedFacts
  pure
    BindingElaboration
      { bePlan = elaboratedPlan,
        beReport =
          BindingLanguageReport
            { blrSubstitutionDecisions = decisions,
              blrSubstitutionOutcomes = bfaOutcomes artifacts,
              blrCaptureObstructions = filter (not . bsdAllowed) decisions,
              blrFresheningCosection = fresheningCosection (bfaFresheningPlans artifacts)
            },
        beGeneratedRules = bfaGeneratedRules artifacts
      }

emitBindingElaboration ::
  (RewriteSignature sig, Ord context) =>
  BindingElaboration sig context binder ->
  EGraphFrontM
    sig
    analysis
    context
    ( Either
        (BindingLanguageError binder)
        (BindingLanguageIngestion sig context binder, RulesetRef)
    )
emitBindingElaboration elaboration = do
  emitted <-
    emitBindingPlan (bePlan elaboration)
  case emitted of
    Left ingestError ->
      pure (Left (BindingLanguageIngestError ingestError))
    Right ingestion ->
      case traverse (resolveGeneratedRewriteContext ingestion) (beGeneratedRules elaboration) of
        Left languageError ->
          pure (Left languageError)
        Right resolvedRules -> do
          rulesRef <-
            rulesetNamed (generatedRulesetName (bePlan elaboration)) $
              traverse_ emitResolvedGeneratedRewrite resolvedRules
          pure
            ( Right
                ( BindingLanguageIngestion
                    { bliBindingIngestion = ingestion,
                      bliReport = beReport elaboration
                    },
                  rulesRef
                )
            )

ingestBindingLanguageTerm ::
  (Ord binder, Ord context) =>
  BindingLanguageRelations sig ->
  BindingLanguageSyntax f binder ->
  ScopedBindingSyntax f sig context scope ->
  BindingRootName ->
  Fix f ->
  EGraphFrontM
    sig
    analysis
    context
    ( Either
        (BindingLanguageError binder)
        (BindingLanguageIngestion sig context binder)
    )
ingestBindingLanguageTerm relations languageSyntax scopedSyntax rawRootName rootTerm =
  case compileBindingLanguageTerm relations languageSyntax scopedSyntax rawRootName rootTerm of
    Left languageError ->
      pure (Left languageError)
    Right (report, plan) -> do
      emitted <- emitBindingPlan plan
      pure $
        case emitted of
          Left ingestError ->
            Left (BindingLanguageIngestError ingestError)
          Right ingestion ->
            Right
              BindingLanguageIngestion
                { bliBindingIngestion = ingestion,
                  bliReport = report
                }

bindingLanguageDecisions ::
  Ord binder =>
  BindingLanguageSyntax f binder ->
  ScopedBindingTree f context scope ->
  Either (BindingLanguageError binder) [BindingSubstitutionDecision binder]
bindingLanguageDecisions languageSyntax tree = do
  (_rootSummary, summaries) <-
    summarizeBindingTree languageSyntax tree
  substitutionDecisions languageSyntax summaries tree

summarizeBindingTree ::
  Ord binder =>
  BindingLanguageSyntax f binder ->
  ScopedBindingTree f context scope ->
  Either
    (BindingLanguageError binder)
    (LexicalSummary binder, Map BindingPath (LexicalSummary binder))
summarizeBindingTree languageSyntax tree = do
  (combinedSummary, childMaps) <-
    foldM
      (summarizeChild languageSyntax node)
      (ownSummary, Map.empty)
      (sbtChildren tree)
  pure
    ( combinedSummary,
      Map.insert (sbnPath node) combinedSummary childMaps
    )
  where
    node =
      sbtNode tree
    ownSummary =
      occurrenceLexicalSummary
        (blsOccurrencesAt languageSyntax (sbnPath node) (sbnTerm node))

summarizeChild ::
  Ord binder =>
  BindingLanguageSyntax f binder ->
  ScopedBindingNode f context scope ->
  (LexicalSummary binder, Map BindingPath (LexicalSummary binder)) ->
  (ScopedBindingResolvedChild f context scope, ScopedBindingTree f context scope) ->
  Either
    (BindingLanguageError binder)
    (LexicalSummary binder, Map BindingPath (LexicalSummary binder))
summarizeChild languageSyntax parentNode (currentSummary, currentMaps) (resolvedChild, childTree) = do
  (childSummary, childMap) <-
    summarizeBindingTree languageSyntax childTree
  let child =
        sbrcChild resolvedChild
      edgeBinders =
        blsBindersEnteringChild
          languageSyntax
          (sbnPath parentNode)
          (sbnTerm parentNode)
          (bcSegment child)
          (bcTerm child)
      scopedChildSummary =
        bindChildLexicalSummary edgeBinders childSummary
  pure
    ( appendLexicalSummary currentSummary scopedChildSummary,
      Map.union childMap currentMaps
    )

substitutionDecisions ::
  Ord binder =>
  BindingLanguageSyntax f binder ->
  Map BindingPath (LexicalSummary binder) ->
  ScopedBindingTree f context scope ->
  Either (BindingLanguageError binder) [BindingSubstitutionDecision binder]
substitutionDecisions languageSyntax summaries tree = do
  ownSites <-
    blsSubstitutionSitesAt languageSyntax (sbtNode tree)
  ownDecisions <-
    traverse
      (substitutionDecision summaries (sbnPath (sbtNode tree)))
      ownSites
  childDecisions <-
    fmap concat $
      traverse
        (substitutionDecisions languageSyntax summaries . snd)
        (sbtChildren tree)
  pure (ownDecisions <> childDecisions)

substitutionDecision ::
  Ord binder =>
  Map BindingPath (LexicalSummary binder) ->
  BindingPath ->
  BindingSubstitutionSite binder ->
  Either (BindingLanguageError binder) (BindingSubstitutionDecision binder)
substitutionDecision summaries redexPath site = do
  bodySummary <-
    lookupSummary (bssBodyPath site)
  argumentSummary <-
    lookupSummary (bssArgumentPath site)
  let argumentFree =
        lsFreeBinders argumentSummary
      bodyCapturers =
        Map.findWithDefault
          Set.empty
          (bssBinder site)
          (lsCapturersByTarget bodySummary)
      allowed =
        Set.null (Set.intersection argumentFree bodyCapturers)
  pure
    BindingSubstitutionDecision
      { bsdRedexPath = redexPath,
        bsdBinder = bssBinder site,
        bsdBodyPath = bssBodyPath site,
        bsdArgumentPath = bssArgumentPath site,
        bsdArgumentFreeBinders = argumentFree,
        bsdBodyCapturingBinders = bodyCapturers,
        bsdAllowed = allowed
      }
  where
    lookupSummary path =
      maybe
        (Left (BindingLanguageUnknownPath path))
        Right
        (Map.lookup path summaries)

data BindingFresheningArtifacts sig context binder = BindingFresheningArtifacts
  { bfaAllowedFactsByPath :: !(Map BindingPath [BindingFact sig]),
    bfaFreshenedEntries :: ![BindingPlanEntry sig context],
    bfaGeneratedRules :: ![BindingGeneratedRewrite sig],
    bfaOutcomes :: ![BindingSubstitutionOutcome binder],
    bfaFresheningPlans :: ![BindingFresheningPlan binder]
  }

bindingFresheningArtifacts ::
  Ord binder =>
  BindingLanguageRelations sig ->
  BindingFresheningSyntax f sig binder ->
  ScopedBindingTree f context scope ->
  BindingPlan sig context ->
  [BindingSubstitutionDecision binder] ->
  Either (BindingLanguageError binder) (BindingFresheningArtifacts sig context binder)
bindingFresheningArtifacts relations fresheningSyntax tree plan decisions =
  materializeBindingFresheningArtifacts
    <$> foldM
      (appendDecisionArtifacts relations fresheningSyntax tree planMap)
      emptyBindingFresheningArtifacts
      (zip [0 :: Int ..] decisions)
  where
    planMap =
      Map.fromList
        [ (bpePath entry, entry)
        | entry <- bindingPlanEntries plan
        ]

emptyBindingFresheningArtifacts :: BindingFresheningArtifacts sig context binder
emptyBindingFresheningArtifacts =
  BindingFresheningArtifacts
    { bfaAllowedFactsByPath = Map.empty,
      bfaFreshenedEntries = [],
      bfaGeneratedRules = [],
      bfaOutcomes = [],
      bfaFresheningPlans = []
    }

materializeBindingFresheningArtifacts ::
  BindingFresheningArtifacts sig context binder ->
  BindingFresheningArtifacts sig context binder
materializeBindingFresheningArtifacts artifacts =
  artifacts
    { bfaFreshenedEntries = reverse (bfaFreshenedEntries artifacts),
      bfaGeneratedRules = reverse (bfaGeneratedRules artifacts),
      bfaOutcomes = reverse (bfaOutcomes artifacts),
      bfaFresheningPlans = reverse (bfaFresheningPlans artifacts)
    }

appendDecisionArtifacts ::
  Ord binder =>
  BindingLanguageRelations sig ->
  BindingFresheningSyntax f sig binder ->
  ScopedBindingTree f context scope ->
  Map BindingPath (BindingPlanEntry sig context) ->
  BindingFresheningArtifacts sig context binder ->
  (Int, BindingSubstitutionDecision binder) ->
  Either (BindingLanguageError binder) (BindingFresheningArtifacts sig context binder)
appendDecisionArtifacts relations fresheningSyntax tree planMap artifacts (decisionIndex, decision)
  | bsdAllowed decision = do
      entry <- lookupPlanEntry planMap (bsdRedexPath decision)
      contractedTerm <-
        bfsContractedResult fresheningSyntax Map.empty tree decision
      pure
        artifacts
          { bfaAllowedFactsByPath =
              Map.insertWith
                (<>)
                (bsdRedexPath decision)
                [substitutionAllowedFact relations]
                (bfaAllowedFactsByPath artifacts),
            bfaGeneratedRules =
              betaRewrite
                relations
                ("binding-beta-" <> show decisionIndex)
                decision
                (bpeTerm entry)
                contractedTerm
                : bfaGeneratedRules artifacts,
            bfaOutcomes =
              BindingSubstitutionAlreadyAllowed decision : bfaOutcomes artifacts
          }
  | otherwise = do
      entry <- lookupPlanEntry planMap (bsdRedexPath decision)
      freshenedPath <-
        first BindingLanguageIngestError $
          bindingPathChildNamed (bsdRedexPath decision) ("alpha-freshened-" <> show decisionIndex)
      renames <-
        bfsFreshenBinders fresheningSyntax decision
      freshenedTerm <-
        bfsFreshenedRedex fresheningSyntax renames tree decision
      contractedTerm <-
        bfsContractedResult fresheningSyntax renames tree decision
      let planValue =
            BindingFresheningPlan
              { bfpRedexPath = bsdRedexPath decision,
                bfpFreshenedPath = freshenedPath,
                bfpCapturedBinders = capturedBinders decision,
                bfpRenames = renames
              }
          freshenedEntry =
            BindingPlanEntry
              { bpePath = freshenedPath,
                bpeContext = bpeContext entry,
                bpeTerm = freshenedTerm,
                bpeFacts = [substitutionAllowedFact relations]
              }
      pure
        artifacts
          { bfaFreshenedEntries = freshenedEntry : bfaFreshenedEntries artifacts,
            bfaGeneratedRules =
              betaRewrite
                relations
                ("binding-beta-freshened-" <> show decisionIndex)
                decision
                freshenedTerm
                contractedTerm
                : alphaRewrite
                  ("binding-alpha-" <> show decisionIndex)
                  decision
                  (bpeTerm entry)
                  freshenedTerm
                  : bfaGeneratedRules artifacts,
            bfaOutcomes =
              BindingSubstitutionNeedsFreshening planValue : bfaOutcomes artifacts,
            bfaFresheningPlans =
              planValue : bfaFresheningPlans artifacts
          }

lookupPlanEntry ::
  Map BindingPath (BindingPlanEntry sig context) ->
  BindingPath ->
  Either (BindingLanguageError binder) (BindingPlanEntry sig context)
lookupPlanEntry planMap path =
  maybe
    (Left (BindingLanguageUnknownPath path))
    Right
    (Map.lookup path planMap)

capturedBinders ::
  Ord binder =>
  BindingSubstitutionDecision binder ->
  Set binder
capturedBinders decision =
  Set.intersection
    (bsdArgumentFreeBinders decision)
    (bsdBodyCapturingBinders decision)
{-# INLINE capturedBinders #-}

alphaRewrite ::
  String ->
  BindingSubstitutionDecision binder ->
  Term sig "Expr" ->
  Term sig "Expr" ->
  BindingGeneratedRewrite sig
alphaRewrite rawName decision lhs rhs =
  BindingGeneratedRewrite
    { bgrName = rawName,
      bgrContextPath = bsdRedexPath decision,
      bgrLhs = lhs,
      bgrRhs = rhs,
      bgrGuard = BindingRewriteUnguarded
    }

betaRewrite ::
  BindingLanguageRelations sig ->
  String ->
  BindingSubstitutionDecision binder ->
  Term sig "Expr" ->
  Term sig "Expr" ->
  BindingGeneratedRewrite sig
betaRewrite relations rawName decision lhs rhs =
  BindingGeneratedRewrite
    { bgrName = rawName,
      bgrContextPath = bsdRedexPath decision,
      bgrLhs = lhs,
      bgrRhs = rhs,
      bgrGuard = BindingRewriteRequiresFact (blrSubstitutionAllowed relations) lhs
    }

fresheningCosection ::
  [BindingFresheningPlan binder] ->
  BindingFresheningCosection binder
fresheningCosection plans =
  BindingFresheningCosection
    { bfcRepresentatives = representatives,
      bfcGlobals =
        [ GlobalCosection
            { globalCosectionClass = CosectionClassKey indexValue,
              globalCosectionRepresentative = representative
            }
        | (indexValue, representative) <- zip [0 ..] representatives
        ]
    }
  where
    representatives =
      [ CosectionRepresentative
          { cosectionRepObject = bfpRedexPath planValue,
            cosectionRepValue = planValue
          }
      | planValue <- plans
      ]

safetyOnlyReport ::
  [BindingSubstitutionDecision binder] ->
  BindingLanguageReport binder
safetyOnlyReport decisions =
  BindingLanguageReport
    { blrSubstitutionDecisions = decisions,
      blrSubstitutionOutcomes = BindingSubstitutionAlreadyAllowed <$> filter bsdAllowed decisions,
      blrCaptureObstructions = filter (not . bsdAllowed) decisions,
      blrFresheningCosection = fresheningCosection []
    }

substitutionAllowedFactsByPath ::
  BindingLanguageRelations sig ->
  [BindingSubstitutionDecision binder] ->
  Map BindingPath [BindingFact sig]
substitutionAllowedFactsByPath relations decisions =
  Map.fromList
    [ (path, [substitutionAllowedFact relations])
    | (path, allowed) <- Map.toAscList (redexAllowedMap decisions),
      allowed
    ]

redexAllowedMap ::
  [BindingSubstitutionDecision binder] ->
  Map BindingPath Bool
redexAllowedMap =
  foldl'
    ( \allowedByPath decision ->
        Map.insertWith
          (&&)
          (bsdRedexPath decision)
          (bsdAllowed decision)
          allowedByPath
    )
    Map.empty

substitutionAllowedFact ::
  BindingLanguageRelations sig ->
  BindingFact sig
substitutionAllowedFact relations =
  BindingFact
    (blrSubstitutionAllowed relations)
    (BindingHere `BindingFactCons` BindingFactNil)

data ResolvedGeneratedRewrite sig context = ResolvedGeneratedRewrite
  { rgrContext :: !(ContextRef context),
    rgrRewrite :: !(BindingGeneratedRewrite sig)
  }

resolveGeneratedRewriteContext ::
  BindingIngestion sig context ->
  BindingGeneratedRewrite sig ->
  Either (BindingLanguageError binder) (ResolvedGeneratedRewrite sig context)
resolveGeneratedRewriteContext ingestion generatedRewrite =
  maybe
    (Left (BindingLanguageMissingGeneratedContext (bgrContextPath generatedRewrite)))
    (\contextRef -> Right (ResolvedGeneratedRewrite contextRef generatedRewrite))
    (Map.lookup (bgrContextPath generatedRewrite) (biPathScopes ingestion))

emitResolvedGeneratedRewrite ::
  RewriteSignature sig =>
  ResolvedGeneratedRewrite sig context ->
  RulesetM sig ()
emitResolvedGeneratedRewrite resolvedRewrite =
  rewriteNamed (bgrName generatedRewrite) $
    atContext (rgrContext resolvedRewrite) $
      guardedRewrite (bgrGuard generatedRewrite) (bgrLhs generatedRewrite ==> bgrRhs generatedRewrite)
  where
    generatedRewrite =
      rgrRewrite resolvedRewrite

guardedRewrite ::
  BindingRewriteGuard sig ->
  DSLRule.RuleBody sig FrontGuardAtom ->
  DSLRule.RuleBody sig FrontGuardAtom
guardedRewrite =
  \case
    BindingRewriteUnguarded ->
      id
    BindingRewriteRequiresFact relationRef termValue ->
      (`when_` has relationRef termValue)

generatedRulesetName :: BindingPlan sig context -> String
generatedRulesetName plan =
  bindingPathName (bindingPlanRootPath plan) <> "/binding-generated"

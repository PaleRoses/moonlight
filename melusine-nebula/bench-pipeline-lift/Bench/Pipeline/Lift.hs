{-# LANGUAGE LambdaCase #-}

-- | Shared case-split discovery, branch-fact staging, and parent case-lift machinery.
module Bench.Pipeline.Lift
  ( CaseAlternativeRecord (..),
    CaseFactCandidate (..),
    CaseFactRefusal (..),
    StructuralRewriteCandidate (..),
    CaseFactDiscovery (..),
    CaseLiftOutcome (..),
    LiftMerge (..),
    discoverCaseFacts,
    stageCaseFacts,
    stageStructuralRewrites,
    acceptedLiftMerges,
    benchLevelCaseLift,
    stageLiftMerges,
    stageLiftMergeGoal,
    renderLiftMerge,
  )
where

import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.Fix (Fix (..))
import Data.Foldable (toList)
import Data.Functor ((<&>))
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import GHC.Types.Name.Occurrence (mkVarOcc, occNameString)
import GHC.Types.Name.Reader (mkRdrUnqual, rdrNameOcc)
import Melusine.Nebula.Source.Ingest (IngestedModule (..), bindingDisplayName)
import Moonlight.Core (ClassId, Pattern (..))
import Moonlight.EGraph.Introspection.Core.HsExpr
  ( BinderAnn,
    ConvertedModule (..),
    HsExprF (..),
    HsPatF (..),
    HsVarRef (..),
    ScopeCtx (..),
    SourceRegion,
    SpanClassRow (..),
    SpannedExpr (..),
    ScopedExpr (..),
    TopLevelBinding (..),
    eraseScopedExpr,
    patBinders,
  )
import Moonlight.EGraph.Pure.Context
  ( ContextEGraph,
    ContextMergePlan,
    ContextRebaseBatch,
    beginContextRebaseBatch,
    commitContextRebaseBatch,
    planContextMerges,
    stageContextMerges,
    stageTermAtContext,
  )

data CaseFactDiscovery = CaseFactDiscovery
  { cfdAlternatives :: ![CaseAlternativeRecord],
    cfdCandidates :: ![CaseFactCandidate],
    cfdStructuralRewrites :: ![StructuralRewriteCandidate],
    cfdRefusals :: ![CaseFactRefusal]
  }

instance Semigroup CaseFactDiscovery where
  left <> right =
    CaseFactDiscovery
      { cfdAlternatives = cfdAlternatives left <> cfdAlternatives right,
        cfdCandidates = cfdCandidates left <> cfdCandidates right,
        cfdStructuralRewrites = cfdStructuralRewrites left <> cfdStructuralRewrites right,
        cfdRefusals = cfdRefusals left <> cfdRefusals right
      }

instance Monoid CaseFactDiscovery where
  mempty = CaseFactDiscovery [] [] [] []

data CaseAlternativeRecord = CaseAlternativeRecord
  { carModuleName :: !String,
    carBindingName :: !String,
    carParentContext :: !ScopeCtx,
    carBranchIndex :: !Int
  }
  deriving stock (Eq, Ord, Show)

data CaseFactCandidate = CaseFactCandidate
  { cfcModuleName :: !String,
    cfcBindingName :: !String,
    cfcBranchIndex :: !Int,
    cfcParentContext :: !ScopeCtx,
    cfcAltContext :: !ScopeCtx,
    cfcScrutineeClass :: !ClassId,
    cfcPatternTerm :: !(Fix HsExprF),
    cfcPatternLabel :: !String
  }

data CaseFactRefusal = CaseFactRefusal
  { cfrModuleName :: !String,
    cfrBindingName :: !String,
    cfrBranchIndex :: !Int,
    cfrReason :: !String
  }
  deriving stock (Eq, Show)

data StructuralRewriteCandidate = StructuralRewriteCandidate
  { srcModuleName :: !String,
    srcBindingName :: !String,
    srcBranchIndex :: !Int,
    srcParentContext :: !ScopeCtx,
    srcContext :: !ScopeCtx,
    srcRedexClass :: !ClassId,
    srcRedexPattern :: !(Pattern HsExprF),
    srcRedexTerm :: !(Fix HsExprF),
    srcReplacementPattern :: !(Pattern HsExprF),
    srcReplacementTerm :: !(Fix HsExprF),
    srcBranchBinders :: !(Set.Set BinderAnn),
    srcRuleLabel :: !String
  }

discoverCaseFacts :: IngestedModule -> CaseFactDiscovery
discoverCaseFacts ingested =
  let classByRegion = Map.fromList [(scrRegion row, scrClass row) | row <- imSpanRows ingested]
      moduleName = imPath ingested
   in foldMap
        (bindingCaseFacts moduleName classByRegion)
        (cmBindings (imConverted ingested))

bindingCaseFacts :: FilePath -> Map SourceRegion ClassId -> TopLevelBinding -> CaseFactDiscovery
bindingCaseFacts moduleName classByRegion bindingValue =
  case bindingDisplayName bindingValue of
    bindingName -> scopedCaseFacts moduleName bindingName classByRegion (tlbScopedTerm bindingValue) (tlbSpannedTerm bindingValue)

scopedCaseFacts :: FilePath -> String -> Map SourceRegion ClassId -> ScopedExpr -> SpannedExpr -> CaseFactDiscovery
scopedCaseFacts moduleName bindingName classByRegion scopedExpr spannedExpr =
  let childFacts = foldMap (uncurry (scopedCaseFacts moduleName bindingName classByRegion)) (zipScopedSpannedChildren scopedExpr spannedExpr)
   in case (seNode scopedExpr, sxNode spannedExpr) of
        (CaseF scrutineeScoped branchesScoped, CaseF _ branchesSpanned) ->
          case sxRegion (spannedScrutinee spannedExpr) >>= (`Map.lookup` classByRegion) of
            Nothing ->
              childFacts
                <> CaseFactDiscovery
                  []
                  []
                  []
                  [ CaseFactRefusal moduleName bindingName (-1) "scrutinee source region has no class row" ]
            Just scrutineeClass ->
              childFacts
                <> foldMap
                  (caseBranchFact moduleName bindingName classByRegion (ActualScope (seOccScope scopedExpr)) scrutineeScoped scrutineeClass)
                  (zip [0 :: Int ..] (zip branchesScoped branchesSpanned))
        _ -> childFacts
  where
    spannedScrutinee = \case
      SpannedExpr _ (CaseF scrutineeValue _) -> scrutineeValue
      _ -> spannedExpr

caseBranchFact :: FilePath -> String -> Map SourceRegion ClassId -> ScopeCtx -> ScopedExpr -> ClassId -> (Int, ((HsPatF, ScopedExpr), (HsPatF, SpannedExpr))) -> CaseFactDiscovery
caseBranchFact moduleName bindingName classByRegion parentContext scrutineeScoped scrutineeClass (branchIndex, ((patternValue, branchScoped), (_, branchSpanned))) =
  let altContext = ActualScope (seOccScope branchScoped)
      altRecord = CaseAlternativeRecord moduleName bindingName parentContext branchIndex
      structuralRewrites =
        structuralRewritesInBranch moduleName bindingName branchIndex parentContext altContext classByRegion scrutineeScoped patternValue branchScoped branchSpanned
   in case patternToExpressionTerm patternValue of
        Left reason ->
          CaseFactDiscovery
            [altRecord]
            []
            structuralRewrites
            [CaseFactRefusal moduleName bindingName branchIndex reason]
        Right patternTerm ->
          if null (patBinders patternValue)
            then CaseFactDiscovery [altRecord] [] [] [CaseFactRefusal moduleName bindingName branchIndex "binderless alternative has no branch-local fact carrier in current paper semantics"]
            else
              CaseFactDiscovery
                [altRecord]
                [ CaseFactCandidate
                    { cfcModuleName = moduleName,
                      cfcBindingName = bindingName,
                      cfcBranchIndex = branchIndex,
                      cfcParentContext = parentContext,
                      cfcAltContext = altContext,
                      cfcScrutineeClass = scrutineeClass,
                      cfcPatternTerm = patternTerm,
                      cfcPatternLabel = show patternValue
                    }
                ]
                structuralRewrites
                []

structuralRewritesInBranch ::
  FilePath ->
  String ->
  Int ->
  ScopeCtx ->
  ScopeCtx ->
  Map SourceRegion ClassId ->
  ScopedExpr ->
  HsPatF ->
  ScopedExpr ->
  SpannedExpr ->
  [StructuralRewriteCandidate]
structuralRewritesInBranch moduleName bindingName branchIndex parentContext altContext classByRegion scrutineeScoped patternValue scopedExpr spannedExpr =
  currentRewrite <> childRewrites
  where
    childRewrites =
      foldMap
        (uncurry (structuralRewritesInBranch moduleName bindingName branchIndex parentContext altContext classByRegion scrutineeScoped patternValue))
        (zipScopedSpannedChildren scopedExpr spannedExpr)

    currentRewrite =
      case (seNode scopedExpr, sxRegion spannedExpr >>= (`Map.lookup` classByRegion)) of
        (AppF functionScoped argumentScoped, Just redexClass)
          | eraseScopedExpr argumentScoped == eraseScopedExpr scrutineeScoped ->
              structuralReplacementFor patternValue functionScoped <&> \replacement ->
                StructuralRewriteCandidate
                  { srcModuleName = moduleName,
                    srcBindingName = bindingName,
                    srcBranchIndex = branchIndex,
                    srcParentContext = parentContext,
                    srcContext = altContext,
                    srcRedexClass = redexClass,
                    srcRedexPattern = eraseScopedExpr scopedExpr,
                    srcRedexTerm = scopedExprTerm scopedExpr,
                    srcReplacementPattern = termPattern (srReplacementTerm replacement),
                    srcReplacementTerm = srReplacementTerm replacement,
                    srcBranchBinders = Set.fromList (patBinders patternValue),
                    srcRuleLabel = srRuleLabel replacement
                  }
        _ -> []

data StructuralReplacement = StructuralReplacement
  { srRuleLabel :: !String,
    srReplacementTerm :: !(Fix HsExprF)
  }

structuralReplacementFor :: HsPatF -> ScopedExpr -> [StructuralReplacement]
structuralReplacementFor patternValue functionScoped =
  consReplacements <> eitherReplacements
  where
    consReplacements =
      case (globalFunctionName functionScoped, listConsBinders patternValue) of
        (Just "null", Just _) ->
          [StructuralReplacement "null-cons" (globalVarTerm "False")]
        (Just "head", Just (headBinder, _)) ->
          [StructuralReplacement "head-cons" (Fix (VarF (LocalName headBinder)))]
        (Just "tail", Just (_, tailBinder)) ->
          [StructuralReplacement "tail-cons" (Fix (VarF (LocalName tailBinder)))]
        _ ->
          []

    eitherReplacements =
      case (eitherConstTrueFunction functionScoped, eitherConstructorPattern patternValue) of
        (True, Just _) ->
          [StructuralReplacement "either-const-true" (globalVarTerm "True")]
        _ ->
          []

globalFunctionName :: ScopedExpr -> Maybe String
globalFunctionName functionScoped =
  case scopedCoreNode functionScoped of
    VarF (GlobalName rdrName) -> Just (occNameString (rdrNameOcc rdrName))
    _ -> Nothing

scopedCoreNode :: ScopedExpr -> HsExprF ScopedExpr
scopedCoreNode scopedExpr =
  case seNode scopedExpr of
    ParF inner -> scopedCoreNode inner
    node -> node

eitherConstTrueFunction :: ScopedExpr -> Bool
eitherConstTrueFunction functionScoped =
  case scopedCoreNode functionScoped of
    AppF leftFunction rightHandler ->
      case scopedCoreNode leftFunction of
        AppF eitherFunction leftHandler ->
          globalFunctionName eitherFunction == Just "either"
            && isConstTrueApplication leftHandler
            && isConstTrueApplication rightHandler
        _ ->
          False
    _ ->
      False

isConstTrueApplication :: ScopedExpr -> Bool
isConstTrueApplication scopedExpr =
  case scopedCoreNode scopedExpr of
    AppF functionExpr argumentExpr ->
      globalFunctionName functionExpr == Just "const" && globalFunctionName argumentExpr == Just "True"
    _ ->
      False

listConsBinders :: HsPatF -> Maybe (BinderAnn, BinderAnn)
listConsBinders = \case
  PConP constructorName [PVarP headBinder, PVarP tailBinder]
    | occNameString (rdrNameOcc constructorName) == ":" ->
        Just (headBinder, tailBinder)
  _ ->
    Nothing

eitherConstructorPattern :: HsPatF -> Maybe String
eitherConstructorPattern = \case
  PConP constructorName [_] ->
    let constructorText = occNameString (rdrNameOcc constructorName)
     in if constructorText == "Left" || constructorText == "Right"
          then Just constructorText
          else Nothing
  _ ->
    Nothing

globalVarTerm :: String -> Fix HsExprF
globalVarTerm nameText =
  Fix (VarF (GlobalName (mkRdrUnqual (mkVarOcc nameText))))

scopedExprTerm :: ScopedExpr -> Fix HsExprF
scopedExprTerm scopedExpr =
  Fix (fmap scopedExprTerm (seNode scopedExpr))

termPattern :: Fix HsExprF -> Pattern HsExprF
termPattern (Fix nodeValue) =
  PatternNode (fmap termPattern nodeValue)

termLocalBinders :: Fix HsExprF -> Set.Set BinderAnn
termLocalBinders (Fix nodeValue) =
  case nodeValue of
    VarF (LocalName binderAnn) ->
      Set.singleton binderAnn
    _ ->
      foldMap termLocalBinders nodeValue

zipScopedSpannedChildren :: ScopedExpr -> SpannedExpr -> [(ScopedExpr, SpannedExpr)]
zipScopedSpannedChildren scopedExpr spannedExpr =
  zip (toList (seNode scopedExpr)) (toList (sxNode spannedExpr))

patternToExpressionTerm :: HsPatF -> Either String (Fix HsExprF)
patternToExpressionTerm = \case
  PVarP binder -> Right (Fix (VarF (LocalName binder)))
  PConP constructorName subPatterns ->
    applyMany (Fix (VarF (GlobalName constructorName))) <$> traverse patternToExpressionTerm subPatterns
  PTupleP subPatterns -> Fix . ExplicitTupleF <$> traverse patternToExpressionTerm subPatterns
  PListP subPatterns -> Fix . ExplicitListF <$> traverse patternToExpressionTerm subPatterns
  PLitP literalValue -> Right (Fix (LitF literalValue))
  POverLitP literalValue -> Right (Fix (OverLitF literalValue))
  PAsP binder _ -> Right (Fix (VarF (LocalName binder)))
  PBangP subPattern -> patternToExpressionTerm subPattern
  PLazyP subPattern -> patternToExpressionTerm subPattern
  PParP subPattern -> patternToExpressionTerm subPattern
  PWildP -> Left "wildcard pattern has no expression witness"
  PRecP {} -> Left "record pattern lowering not implemented in bench"
  PLossyP {} -> Left "lossy pattern lowering refused"

applyMany :: Fix HsExprF -> [Fix HsExprF] -> Fix HsExprF
applyMany functionTerm arguments =
  List.foldl' (\acc argument -> Fix (AppF acc argument)) functionTerm arguments

data ContextMergeConstruction a = ContextMergeConstruction
  { cmcBatch :: !(ContextRebaseBatch HsExprF a ScopeCtx),
    cmcPlansReversed :: ![ContextMergePlan ScopeCtx]
  }

beginContextMergeConstruction :: ContextEGraph HsExprF a ScopeCtx -> ContextMergeConstruction a
beginContextMergeConstruction contextGraph =
  ContextMergeConstruction
    { cmcBatch = beginContextRebaseBatch contextGraph,
      cmcPlansReversed = []
    }

freezeContextMergePlan ::
  ContextRebaseBatch HsExprF a ScopeCtx ->
  ContextMergePlan ScopeCtx ->
  ContextMergeConstruction a ->
  ContextMergeConstruction a
freezeContextMergePlan batchValue mergePlan construction =
  construction
    { cmcBatch = batchValue,
      cmcPlansReversed = mergePlan : cmcPlansReversed construction
    }

commitContextMergeConstruction :: ContextMergeConstruction a -> Either String (ContextEGraph HsExprF a ScopeCtx)
commitContextMergeConstruction construction =
  foldM
    (\batchValue mergePlan -> first show (stageContextMerges mergePlan batchValue))
    (cmcBatch construction)
    (reverse (cmcPlansReversed construction))
    >>= fmap snd . first show . commitContextRebaseBatch

stageCaseFacts :: [CaseFactCandidate] -> ContextEGraph HsExprF a ScopeCtx -> Either String (Int, ContextEGraph HsExprF a ScopeCtx)
stageCaseFacts candidates contextGraph0 = do
  construction <-
    foldM
      planCandidate
      (beginContextMergeConstruction contextGraph0)
      candidates
  contextGraph1 <- commitContextMergeConstruction construction
  pure (length candidates, contextGraph1)
  where
    planCandidate :: ContextMergeConstruction a -> CaseFactCandidate -> Either String (ContextMergeConstruction a)
    planCandidate construction candidate = do
      (patternClass, batchWithPattern) <- first show (stageTermAtContext (cfcAltContext candidate) (cfcPatternTerm candidate) (cmcBatch construction))
      mergePlan <- first show (planContextMerges [cfcAltContext candidate] (cfcScrutineeClass candidate) patternClass batchWithPattern)
      pure (freezeContextMergePlan batchWithPattern mergePlan construction)

stageStructuralRewrites :: [StructuralRewriteCandidate] -> ContextEGraph HsExprF a ScopeCtx -> Either String (Int, ContextEGraph HsExprF a ScopeCtx)
stageStructuralRewrites candidates contextGraph0 = do
  construction <-
    foldM
      planCandidate
      (beginContextMergeConstruction contextGraph0)
      candidates
  contextGraph1 <- commitContextMergeConstruction construction
  pure (length candidates, contextGraph1)
  where
    planCandidate :: ContextMergeConstruction a -> StructuralRewriteCandidate -> Either String (ContextMergeConstruction a)
    planCandidate construction candidate = do
      (replacementClass, batchWithReplacement) <- first show (stageTermAtContext (srcContext candidate) (srcReplacementTerm candidate) (cmcBatch construction))
      mergePlan <- first show (planContextMerges [srcContext candidate] (srcRedexClass candidate) replacementClass batchWithReplacement)
      pure (freezeContextMergePlan batchWithReplacement mergePlan construction)

data CaseLiftOutcome a = CaseLiftOutcome
  { cloGraph :: !(Either String (ContextEGraph HsExprF a ScopeCtx)),
    cloLiftCount :: !Int,
    cloLiftedFacts :: ![String],
    cloWhyNotCount :: !Int
  }

data LiftGroup = LiftGroup
  { lgModuleName :: !String,
    lgBindingName :: !String,
    lgParentContext :: !ScopeCtx,
    lgBranches :: !(Set.Set Int),
    lgRedexTerm :: !(Fix HsExprF),
    lgReplacementTerm :: !(Fix HsExprF),
    lgParentVisible :: !Bool
  }

instance Semigroup LiftGroup where
  left <> right =
    LiftGroup
      { lgModuleName = lgModuleName left,
        lgBindingName = lgBindingName left,
        lgParentContext = lgParentContext left,
        lgBranches = lgBranches left <> lgBranches right,
        lgRedexTerm = lgRedexTerm left,
        lgReplacementTerm = lgReplacementTerm left,
        lgParentVisible = lgParentVisible left && lgParentVisible right
      }

data LiftMerge = LiftMerge
  { lmModuleName :: !String,
    lmBindingName :: !String,
    lmParentContext :: !ScopeCtx,
    lmRedexTerm :: !(Fix HsExprF),
    lmReplacementTerm :: !(Fix HsExprF)
  }

acceptedLiftMerges :: [CaseAlternativeRecord] -> [StructuralRewriteCandidate] -> [LiftMerge]
acceptedLiftMerges alternatives structuralRewrites =
  let expectedBranches = expectedCaseBranches alternatives
      grouped = Map.elems (Map.fromListWith (<>) (fmap liftGroupEntry structuralRewrites))
   in mapMaybe (liftMergeFor expectedBranches) grouped

benchLevelCaseLift :: [CaseAlternativeRecord] -> [StructuralRewriteCandidate] -> ContextEGraph HsExprF a ScopeCtx -> CaseLiftOutcome a
benchLevelCaseLift alternatives structuralRewrites contextGraph0 =
  let grouped = Map.elems (Map.fromListWith (<>) (fmap liftGroupEntry structuralRewrites))
      acceptedMerges = acceptedLiftMerges alternatives structuralRewrites
   in CaseLiftOutcome
        { cloGraph = stageLiftMerges acceptedMerges contextGraph0,
          cloLiftCount = length acceptedMerges,
          cloLiftedFacts = fmap renderLiftMerge acceptedMerges,
          cloWhyNotCount = length grouped - length acceptedMerges
        }

type CaseCoverKey = (String, String, ScopeCtx)

type LiftGroupKey = (String, String, ScopeCtx, Pattern HsExprF, Pattern HsExprF)

expectedCaseBranches :: [CaseAlternativeRecord] -> Map CaseCoverKey (Set.Set Int)
expectedCaseBranches alternatives =
  Map.fromListWith
    (<>)
    [ ((carModuleName alternative, carBindingName alternative, carParentContext alternative), Set.singleton (carBranchIndex alternative))
      | alternative <- alternatives
    ]

liftGroupEntry :: StructuralRewriteCandidate -> (LiftGroupKey, LiftGroup)
liftGroupEntry candidate =
  ( (srcModuleName candidate, srcBindingName candidate, srcParentContext candidate, srcRedexPattern candidate, srcReplacementPattern candidate),
    LiftGroup
      { lgModuleName = srcModuleName candidate,
        lgBindingName = srcBindingName candidate,
        lgParentContext = srcParentContext candidate,
        lgBranches = Set.singleton (srcBranchIndex candidate),
        lgRedexTerm = srcRedexTerm candidate,
        lgReplacementTerm = srcReplacementTerm candidate,
        lgParentVisible =
          parentVisibleTerm (srcBranchBinders candidate) (srcRedexTerm candidate)
            && parentVisibleTerm (srcBranchBinders candidate) (srcReplacementTerm candidate)
      }
  )

parentVisibleTerm :: Set.Set BinderAnn -> Fix HsExprF -> Bool
parentVisibleTerm branchBinders termValue =
  Set.null (Set.intersection branchBinders (termLocalBinders termValue))

liftMergeFor :: Map CaseCoverKey (Set.Set Int) -> LiftGroup -> Maybe LiftMerge
liftMergeFor expectedBranches groupValue =
  case Map.lookup (lgModuleName groupValue, lgBindingName groupValue, lgParentContext groupValue) expectedBranches of
    Just expected
      | lgParentVisible groupValue,
        Set.size expected >= 2,
        lgBranches groupValue == expected ->
          Just
            LiftMerge
              { lmModuleName = lgModuleName groupValue,
                lmBindingName = lgBindingName groupValue,
                lmParentContext = lgParentContext groupValue,
                lmRedexTerm = lgRedexTerm groupValue,
                lmReplacementTerm = lgReplacementTerm groupValue
              }
    _ ->
      Nothing

stageLiftMerges :: [LiftMerge] -> ContextEGraph HsExprF a ScopeCtx -> Either String (ContextEGraph HsExprF a ScopeCtx)
stageLiftMerges merges contextGraph0 = do
  foldM
    planMerge
    (beginContextMergeConstruction contextGraph0)
    merges
    >>= commitContextMergeConstruction
  where
    planMerge :: ContextMergeConstruction a -> LiftMerge -> Either String (ContextMergeConstruction a)
    planMerge construction mergeValue = do
      (redexClass, batchWithRedex) <- first show (stageTermAtContext (lmParentContext mergeValue) (lmRedexTerm mergeValue) (cmcBatch construction))
      (replacementClass, batchWithReplacement) <- first show (stageTermAtContext (lmParentContext mergeValue) (lmReplacementTerm mergeValue) batchWithRedex)
      mergePlan <- first show (planContextMerges [lmParentContext mergeValue] redexClass replacementClass batchWithReplacement)
      pure (freezeContextMergePlan batchWithReplacement mergePlan construction)

-- | Inserts a lift merge's redex and replacement terms at the parent context without merging,
-- yielding the parent-context class pair the lift would identify and the graph carrying both terms.
stageLiftMergeGoal ::
  LiftMerge ->
  ContextEGraph HsExprF a ScopeCtx ->
  Either String (ScopeCtx, ClassId, ClassId, ContextEGraph HsExprF a ScopeCtx)
stageLiftMergeGoal mergeValue contextGraph0 = do
  (redexClass, batchWithRedex) <-
    first show (stageTermAtContext (lmParentContext mergeValue) (lmRedexTerm mergeValue) (beginContextRebaseBatch contextGraph0))
  (replacementClass, batchWithReplacement) <-
    first show (stageTermAtContext (lmParentContext mergeValue) (lmReplacementTerm mergeValue) batchWithRedex)
  (_, contextGraph1) <- first show (commitContextRebaseBatch batchWithReplacement)
  pure (lmParentContext mergeValue, redexClass, replacementClass, contextGraph1)

renderLiftMerge :: LiftMerge -> String
renderLiftMerge mergeValue =
  lmModuleName mergeValue
    <> ":"
    <> lmBindingName mergeValue
    <> " @ "
    <> show (lmParentContext mergeValue)
    <> " lifted branch-cover equality"

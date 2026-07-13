{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Moonlight.EGraph.Introspection.Core.Rewrite
  ( RewriteContextPresentationError (..),
    RewriteTag,
    RewriteOriginAtom,
    RewriteMorphism,
    PatternRewriteError,
    CompositionError,
    IdentifiedRewriteSpan,
    RewriteRule,
    CompiledRewriteRule,
    irsRuleId,
    irsSpan,
    RuntimeRuleIdentity (..),
    rewriteMorphism,
    rewriteMorphismName,
    rewriteMorphismLeft,
    rewriteMorphismRight,
    rewriteMorphismPostSubst,
    rewriteMorphismWithInterface,
    identifiedSpanFromRewriteRule,
    identifiedSpanFromEGraphRewriteRule,
    identifiedSpanFromCompiledRule,
    RewriteContext,
    rcOrdinal,
    rcObjects,
    mkRewriteContext,
    RewriteSystem,
    mkRewriteNerveSite,
    rsCategory,
    rsContexts,
    rsRuleIdentities,
    mkRewriteSystem,
    mkRewriteSystemFromGenerators,
    mkRewriteSystemWithContexts,
    mkIdentifiedRewriteSystem,
    mkIdentifiedRewriteSystemWithContexts,
    ruleObjectContext,
    resolveRuntimeRuleIdentity,
    sameRuntimeRewriteMorphism,
    rewriteRuleIdOf,
    validateRewriteSystemContexts,
  )
where

import Data.Function ((&))
import Data.Foldable (toList)
import Data.Kind (Type)
import Data.List (find, intercalate, nubBy, sort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (isJust)
import Data.Monoid (Any (..))
import Data.Set qualified as Set
import Moonlight.Analysis.Cohomology (CoboundaryNilpotenceEvidence, coboundaryNilpotenceEvidenceFromResult)
import Moonlight.Core (ZipMatch (..), HasConstructorTag, Language, Pattern, PatternVar, RewriteRuleId, patternVarKey, patternVariables)
import Moonlight.Algebra (JoinSemilattice (..), Lattice, MeetSemilattice (..))
import Moonlight.Sheaf.Cochain.Cohomology
  ( SiteCoboundaryRealization (..),
    SiteCochainInput (..),
    buildGrothendieckCochainArtifact,
  )
import Moonlight.Sheaf.Site.Context.GeneratorCover
  ( ContextGeneratorCover (..),
    contextClosure,
  )
import Moonlight.Sheaf.Site
  ( ContextPresentation (..),
    ContextPresentationSystem (..),
    ContextPairStrategy (..),
  )
import Moonlight.Sheaf.Site (mkGrothendieckSite)
import Moonlight.Sheaf.Site (GrothendieckNilpotentSystem (..))
import Moonlight.Sheaf.Site.Stalk.Interface.Linearization
  ( interfaceStalkBasisLinearization,
  )
import Moonlight.Sheaf.Site (ContextOrdinalSystem (..))
import Moonlight.Sheaf.Site
  ( AnalyzableSystem (..),
    InterfaceDirectionEstimate (..),
    InterfaceName,
    MorphismInterface (..),
    interfaceNameFromString,
  )
import Moonlight.Rewrite.Algebra (FiniteRewriteCategory (..), finiteRewriteCategory)
import Moonlight.Rewrite.System
  ( LogicalDecoration,
    ldCondition,
    ldPostSubst,
    logicalDecoration,
  )
import Moonlight.Rewrite.System (CompiledGuard, GuardTerm, RewriteCondition)
import Moonlight.Rewrite.Algebra (rewriteNerve)
import Moonlight.Rewrite.Algebra (simplexRewrites, simplexSourcePattern)
import Moonlight.Rewrite.Runtime
  ( RulePlan (..),
    rulePlanCondition,
    rulePlanPostSubst,
    rulePlanPrimaryPattern,
    rulePlanRhsPattern
  )
import Moonlight.Rewrite.System (RawRewriteRule (..))
import Moonlight.Rewrite.Runtime (PostMatchSubst)
import Moonlight.Rewrite.Algebra qualified as KernelCompose
import Moonlight.Sheaf.Site (NerveMorphism, NerveSite, NerveSiteAlgebra (..), NerveSource, mkNerveSite)
import Moonlight.Sheaf.Site (InterfaceDomain (..), InterfaceMeasure (..))
import Moonlight.Rewrite.Algebra
  ( PatternRewrite,
    RewriteOrigin (..),
    identityPatternRewrite,
    mkPatternRewrite,
    patternInterfaceVariables,
    patternRewriteCreatedVars,
    patternRewriteDeletedVars,
    prDecoration,
    prInterface,
    prLeft,
    prOrigin,
    prRight,
  )
import Moonlight.Rewrite.Algebra qualified as KernelRewrite
import Numeric.Natural (Natural)
import Moonlight.Pale.Ghc.Expr (ScopeCtx)

type PatternRewriteError f = KernelRewrite.PatternRewriteError (LogicalDecoration ScopeCtx) f

type CompositionError f = KernelCompose.CompositionError (LogicalDecoration ScopeCtx) f

type RewriteTag :: (Type -> Type) -> Type
data RewriteTag (f :: Type -> Type)

type RewriteMorphism :: (Type -> Type) -> Type
type RewriteMorphism f = PatternRewrite RewriteOriginAtom (LogicalDecoration ScopeCtx) f

type RewriteRule :: (Type -> Type) -> Type
type RewriteRule f = RawRewriteRule (RewriteCondition ScopeCtx f) f

type CompiledRewriteRule :: (Type -> Type) -> Type
type CompiledRewriteRule f = RulePlan (CompiledGuard ScopeCtx f) f

type RewriteOriginAtom :: Type
data RewriteOriginAtom
  = NamedRewriteOrigin !String
  | RuntimeRewriteOrigin !RewriteRuleId
  deriving stock (Eq, Ord, Show)

type RewriteContextPresentationError :: (Type -> Type) -> Type
data RewriteContextPresentationError f
  = ContextContainsUnknownObjects Int
  | NonAntitoneVisibility Int Int
  deriving stock (Eq, Ord, Show)

type RewriteContext :: (Type -> Type) -> Type
data RewriteContext f = RewriteContext
  { rcOrdinal :: Int,
    rcObjects :: [Pattern f]
  }

instance (forall a. Ord a => Ord (f a)) => Eq (RewriteContext f) where
  leftContext == rightContext =
    rcObjects leftContext == rcObjects rightContext

instance (forall a. Ord a => Ord (f a)) => Ord (RewriteContext f) where
  compare leftContext rightContext =
    compare (rcObjects leftContext) (rcObjects rightContext)

instance Show (RewriteContext f) where
  show contextValue =
    "RewriteContext " <> show (rcOrdinal contextValue)

type IdentifiedRewriteSpan :: (Type -> Type) -> Type
data IdentifiedRewriteSpan f = IdentifiedRewriteSpan
  { irsRuleId :: RewriteRuleId,
    irsSpan :: RewriteMorphism f
  }

type RuntimeRuleIdentity :: Type
data RuntimeRuleIdentity
  = NoRuntimeRuleIdentity
  | UniqueRuntimeRuleIdentity RewriteRuleId
  | AmbiguousRuntimeRuleIdentity (NonEmpty RewriteRuleId)
  deriving stock (Eq, Show)

type RewriteSystem :: (Type -> Type) -> Type
data RewriteSystem f = RewriteSystem
  { rsCategory :: FiniteRewriteCategory RewriteOriginAtom (LogicalDecoration ScopeCtx) f
  , rsContexts :: [RewriteContext f],
    rsPresentationContexts :: [RewriteContext f],
    rsPairStrategy :: ContextPairStrategy (RewriteContext f),
    rsRuleIdentities :: [IdentifiedRewriteSpan f]
  }

mkRewriteNerveSite ::
  ( HasConstructorTag f,
    ZipMatch f,
    Ord (NerveSource (RewriteTag f)),
    Ord (NerveMorphism (RewriteTag f))
  ) =>
  RewriteSystem f ->
  Natural ->
  NerveSite (RewriteTag f)
mkRewriteNerveSite rewriteSystem depthValue =
  mkNerveSite (rsCategory rewriteSystem) depthValue

mkRewriteSystem :: Ord (Pattern f) => [RewriteMorphism f] -> RewriteSystem f
mkRewriteSystem ruleSpans =
  let categoryValue = finiteRewriteCategory ruleSpans
   in buildRewriteSystem categoryValue [] [] ExhaustivePairs

mkRewriteSystemFromGenerators ::
  (HasConstructorTag f, ZipMatch f, Ord (Pattern f)) =>
  [RewriteMorphism f] ->
  RewriteSystem f
mkRewriteSystemFromGenerators ruleSpans =
  let categoryValue = finiteRewriteCategory ruleSpans
      generatorContexts =
        frcRewrites categoryValue
          & zipWith
            (\ordinalValue -> mkRewriteContext ordinalValue . ruleObjectContextObjects)
            [0 ..]
      generatedSystem =
        buildRewriteSystem
          categoryValue
          (fmap rcObjects generatorContexts)
          []
          ExhaustivePairs
      closedSystem =
        buildRewriteSystem
          categoryValue
          (fmap rcObjects (contextClosure generatedSystem))
          []
          (GeneratorSeededPairs generatorContexts)
   in closedSystem {rsPresentationContexts = generatorContexts}

mkRewriteSystemWithContexts :: Ord (Pattern f) => [RewriteMorphism f] -> [[Pattern f]] -> Either (RewriteContextPresentationError f) (RewriteSystem f)
mkRewriteSystemWithContexts ruleSpans contextObjects =
  let categoryValue = finiteRewriteCategory ruleSpans
      rewriteSystem =
        buildRewriteSystem
          categoryValue
          contextObjects
          []
          ExhaustivePairs
   in validateRewriteSystemContexts rewriteSystem

mkIdentifiedRewriteSystem ::
  (Eq (RewriteMorphism f), Ord (Pattern f)) =>
  [IdentifiedRewriteSpan f] ->
  RewriteSystem f
mkIdentifiedRewriteSystem identifiedSpans =
  let categoryValue = finiteRewriteCategory (structuralRewriteMorphisms (fmap irsSpan identifiedSpans))
   in buildRewriteSystem categoryValue [] identifiedSpans ExhaustivePairs

mkIdentifiedRewriteSystemWithContexts ::
  (Eq (RewriteMorphism f), Ord (Pattern f)) =>
  [IdentifiedRewriteSpan f] ->
  [[Pattern f]] ->
  Either (RewriteContextPresentationError f) (RewriteSystem f)
mkIdentifiedRewriteSystemWithContexts identifiedSpans contextObjects =
  let categoryValue = finiteRewriteCategory (structuralRewriteMorphisms (fmap irsSpan identifiedSpans))
      rewriteSystem =
        buildRewriteSystem
          categoryValue
          contextObjects
          identifiedSpans
          ExhaustivePairs
   in validateRewriteSystemContexts rewriteSystem

mkRewriteContext :: Ord (Pattern f) => Int -> [Pattern f] -> RewriteContext f
mkRewriteContext ordinal =
  RewriteContext ordinal . canonicalizeContextObjects

ruleObjectContext :: Ord (Pattern f) => RewriteMorphism f -> RewriteContext f
ruleObjectContext =
  mkRewriteContext 0 . ruleObjectContextObjects

identifiedSpanFromRewriteRule ::
  (Language f, Ord (GuardTerm f)) =>
  RewriteRule f ->
  Either (PatternRewriteError f) (IdentifiedRewriteSpan f)
identifiedSpanFromRewriteRule rewriteRule = do
  rewriteSpan <- rewriteFromRewriteRule rewriteRule
  pure
    IdentifiedRewriteSpan
      { irsRuleId = rrId rewriteRule,
        irsSpan = rewriteSpan
      }

identifiedSpanFromEGraphRewriteRule ::
  (Language f, Ord (GuardTerm f)) =>
  RewriteRule f ->
  Either (PatternRewriteError f) (IdentifiedRewriteSpan f)
identifiedSpanFromEGraphRewriteRule rewriteRule = do
  rewriteSpan <- rewriteFromEGraphRewriteRule rewriteRule
  pure
    IdentifiedRewriteSpan
      { irsRuleId = rrId rewriteRule,
        irsSpan = rewriteSpan
      }

identifiedSpanFromCompiledRule ::
  (Language f, Ord (GuardTerm f)) =>
  CompiledRewriteRule f ->
  Either (PatternRewriteError f) (IdentifiedRewriteSpan f)
identifiedSpanFromCompiledRule compiledRewriteRule = do
  rewriteSpan <- rewriteFromCompiledRule compiledRewriteRule
  pure
    IdentifiedRewriteSpan
      { irsRuleId = rpId compiledRewriteRule,
        irsSpan = rewriteSpan
      }

rewriteFromRewriteRule ::
  (Language f, Ord (GuardTerm f)) =>
  RewriteRule f ->
  Either (PatternRewriteError f) (RewriteMorphism f)
rewriteFromRewriteRule rewriteRule =
  rewriteMorphismWithOrigin
    (RewriteAtomic (RuntimeRewriteOrigin (rrId rewriteRule)))
    (rrLhs rewriteRule)
    (Set.intersection (patternVariables (rrLhs rewriteRule)) (patternVariables (rrRhs rewriteRule)))
    (rrRhs rewriteRule)
    Nothing
    (rrPostSubst rewriteRule)

rewriteFromEGraphRewriteRule ::
  (Language f, Ord (GuardTerm f)) =>
  RewriteRule f ->
  Either (PatternRewriteError f) (RewriteMorphism f)
rewriteFromEGraphRewriteRule rewriteRule = do
  rewriteValue <- rewriteFromRewriteRule rewriteRule
  pure
    rewriteValue
      { prDecoration = logicalDecoration Nothing Nothing
      }

rewriteFromCompiledRule ::
  (Language f, Ord (GuardTerm f)) =>
  CompiledRewriteRule f ->
  Either (PatternRewriteError f) (RewriteMorphism f)
rewriteFromCompiledRule compiledRule =
  rewriteMorphismWithOrigin
    (RewriteAtomic (RuntimeRewriteOrigin (rpId compiledRule)))
    (rulePlanPrimaryPattern compiledRule)
    (Set.intersection (patternVariables (rulePlanPrimaryPattern compiledRule)) (patternVariables (rulePlanRhsPattern compiledRule)))
    (rulePlanRhsPattern compiledRule)
    (rulePlanCondition compiledRule)
    (rulePlanPostSubst compiledRule)

rewriteMorphism ::
  (Language f, Ord (GuardTerm f)) =>
  String ->
  Pattern f ->
  Pattern f ->
  Maybe (CompiledGuard ScopeCtx f) ->
  Maybe (PostMatchSubst f) ->
  Either (PatternRewriteError f) (RewriteMorphism f)
rewriteMorphism name leftPattern rightPattern =
  rewriteMorphismWithInterface
    name
    leftPattern
    (Set.intersection (patternVariables leftPattern) (patternVariables rightPattern))
    rightPattern

rewriteMorphismWithInterface ::
  (Language f, Ord (GuardTerm f)) =>
  String ->
  Pattern f ->
  Set.Set PatternVar ->
  Pattern f ->
  Maybe (CompiledGuard ScopeCtx f) ->
  Maybe (PostMatchSubst f) ->
  Either (PatternRewriteError f) (RewriteMorphism f)
rewriteMorphismWithInterface name =
  rewriteMorphismWithOrigin (RewriteAtomic (NamedRewriteOrigin name))

rewriteMorphismWithOrigin ::
  (Language f, Ord (GuardTerm f)) =>
  RewriteOrigin RewriteOriginAtom ->
  Pattern f ->
  Set.Set PatternVar ->
  Pattern f ->
  Maybe (CompiledGuard ScopeCtx f) ->
  Maybe (PostMatchSubst f) ->
  Either (PatternRewriteError f) (RewriteMorphism f)
rewriteMorphismWithOrigin origin leftPattern interfaceVars rightPattern condition postSubst =
  mkPatternRewrite
    origin
    leftPattern
    interfaceVars
    rightPattern
    (logicalDecoration condition postSubst)

rewriteMorphismName :: RewriteMorphism f -> String
rewriteMorphismName =
  intercalate "+" . fmap rewriteOriginAtomName . toList . prOrigin

rewriteOriginAtomName :: RewriteOriginAtom -> String
rewriteOriginAtomName originAtom =
  case originAtom of
    NamedRewriteOrigin name ->
      name
    RuntimeRewriteOrigin ruleId ->
      show ruleId

rewriteMorphismLeft :: RewriteMorphism f -> Pattern f
rewriteMorphismLeft =
  prLeft

rewriteMorphismRight :: RewriteMorphism f -> Pattern f
rewriteMorphismRight =
  prRight

rewriteMorphismPostSubst :: RewriteMorphism f -> Maybe (PostMatchSubst f)
rewriteMorphismPostSubst =
  ldPostSubst . prDecoration

rewriteRuleIdOf :: Eq (RewriteMorphism f) => RewriteSystem f -> RewriteMorphism f -> Maybe RewriteRuleId
rewriteRuleIdOf rewriteSystem spanValue =
  case resolveRuntimeRuleIdentity rewriteSystem spanValue of
    UniqueRuntimeRuleIdentity ruleId -> Just ruleId
    _ -> Nothing

resolveRuntimeRuleIdentity :: Eq (RewriteMorphism f) => RewriteSystem f -> RewriteMorphism f -> RuntimeRuleIdentity
resolveRuntimeRuleIdentity rewriteSystem spanValue =
  case fmap irsRuleId (filter (\identifiedSpan -> sameRuntimeRewriteMorphism (irsSpan identifiedSpan) spanValue) (rsRuleIdentities rewriteSystem)) of
    [] -> NoRuntimeRuleIdentity
    [ruleId] -> UniqueRuntimeRuleIdentity ruleId
    ruleId : remainingRuleIds -> AmbiguousRuntimeRuleIdentity (ruleId :| remainingRuleIds)

sameRuntimeRewriteMorphism :: Eq (RewriteMorphism f) => RewriteMorphism f -> RewriteMorphism f -> Bool
sameRuntimeRewriteMorphism leftRewrite rightRewrite =
  structuralRewriteMorphism leftRewrite == structuralRewriteMorphism rightRewrite

structuralRewriteMorphism :: RewriteMorphism f -> RewriteMorphism f
structuralRewriteMorphism rewriteValue =
  rewriteValue {prOrigin = RewriteIdentity}

structuralRewriteMorphisms :: Eq (RewriteMorphism f) => [RewriteMorphism f] -> [RewriteMorphism f]
structuralRewriteMorphisms =
  nubBy sameRuntimeRewriteMorphism . fmap structuralRewriteMorphism

instance (HasConstructorTag f, ZipMatch f) => AnalyzableSystem (RewriteSystem f) where
  type SystemTag (RewriteSystem f) = RewriteTag f
  type SystemOb (RewriteSystem f) = Pattern f
  type SystemMor (RewriteSystem f) = RewriteMorphism f
  type SystemCtx (RewriteSystem f) = RewriteContext f
  type SystemMismatch (RewriteSystem f) = CompositionError f

  allContexts = rsContexts
  contextLeq _ smallerContext largerContext =
    all (`elem` rcObjects largerContext) (rcObjects smallerContext)
  systemObjectsInContext _ = rcObjects
  systemMorphismsInContext rewriteSystem contextValue =
    filter
      (morphismVisibleInContext contextValue)
      (frcRewrites (rsCategory rewriteSystem))
  restrictObject rewriteSystem sourceContext targetContext objectValue =
    if contextLeq rewriteSystem targetContext sourceContext
        && objectVisibleInContext sourceContext objectValue
        && objectVisibleInContext targetContext objectValue
      then Just objectValue
      else Nothing
  restrictMorphism rewriteSystem sourceContext targetContext morphismValue =
    if contextLeq rewriteSystem targetContext sourceContext
        && morphismVisibleInContext sourceContext morphismValue
        && morphismVisibleInContext targetContext morphismValue
      then Just morphismValue
      else Nothing
  identityMorphism _ _ = identityPatternRewrite
  morphismSource _ = prLeft
  morphismTarget _ = prRight
  composeMorphisms _ _ leftSpan rightSpan =
    KernelCompose.crRewrite <$> KernelCompose.composePatternRewrites rightSpan leftSpan
  morphismInterface _ spanValue =
    MorphismInterface
      { miBoundNames = renderVars (patternInterfaceVariables (prInterface spanValue)),
        miDeletedNames = renderVars (patternRewriteDeletedVars spanValue),
        miCreatedNames = renderVars (patternRewriteCreatedVars spanValue),
        miGuarded = isJust (ldCondition (prDecoration spanValue)),
        miDirectionEstimate =
          InterfaceDirectionEstimate
            ( Set.size (patternInterfaceVariables (prInterface spanValue))
                + if isJust (ldCondition (prDecoration spanValue))
                  then 0
                  else 1
            )
      }
  normalizeMorphism _ _ spanValue =
    spanValue {prOrigin = RewriteIdentity}

instance (HasConstructorTag f, ZipMatch f) => ContextPresentationSystem (RewriteSystem f) where
  systemContextPresentation rewriteSystem =
    ContextPresentation
      { cpSystem = rewriteSystem,
        cpContexts = rsPresentationContexts rewriteSystem,
        cpPairStrategy = rsPairStrategy rewriteSystem
      }

instance (HasConstructorTag f, ZipMatch f) => ContextOrdinalSystem (RewriteSystem f) where
  contextOrdinal _ = rcOrdinal

instance (HasConstructorTag f, ZipMatch f, Ord (Pattern f)) => ContextGeneratorCover (RewriteSystem f) where
  contextGenerators rewriteSystem =
    frcRewrites (rsCategory rewriteSystem)
      & zipWith
        (\ordinalValue -> mkRewriteContext ordinalValue . ruleObjectContextObjects)
        [0 ..]
  contextIsBottom rewriteSystem contextValue =
    null (systemObjectsInContext rewriteSystem contextValue)

rewriteGrothendieckCoboundaryNilpotenceEvidence ::
  (HasConstructorTag f, ZipMatch f, Ord (Pattern f)) =>
  RewriteSystem f ->
  Natural ->
  CoboundaryNilpotenceEvidence
rewriteGrothendieckCoboundaryNilpotenceEvidence rewriteSystem depthValue =
  coboundaryNilpotenceEvidenceFromResult
    (length (rsContexts rewriteSystem))
    ( buildGrothendieckCochainArtifact
        (ExplicitSiteCoboundary interfaceStalkBasisLinearization)
        Right
        (MaterializedSite (mkGrothendieckSite rewriteSystem depthValue))
    )

instance (HasConstructorTag f, ZipMatch f, Ord (Pattern f)) => GrothendieckNilpotentSystem (RewriteSystem f) where
  grothendieckCoboundaryNilpotenceEvidence =
    rewriteGrothendieckCoboundaryNilpotenceEvidence

instance (HasConstructorTag f, ZipMatch f) => InterfaceDomain (RewriteTag f) where
  type InterfaceObject (RewriteTag f) = Pattern f
  type InterfaceMorphism (RewriteTag f) = RewriteMorphism f
  type InterfaceComposeError (RewriteTag f) = CompositionError f

  measureObject objectValue =
    InterfaceMeasure
      { imBoundNames = renderVars (patternVariables objectValue),
        imDeletedNames = Set.empty,
        imCreatedNames = Set.empty,
        imGuarded = Any False
      }

  measureMorphism spanValue =
    InterfaceMeasure
      { imBoundNames = renderVars (patternInterfaceVariables (prInterface spanValue)),
        imDeletedNames = renderVars (patternRewriteDeletedVars spanValue),
        imCreatedNames = renderVars (patternRewriteCreatedVars spanValue),
        imGuarded = Any (isJust (ldCondition (prDecoration spanValue)))
      }

  composeMorphismChain morphismValues =
    case morphismValues of
      [] -> Left KernelCompose.EmptyRewriteChain
      [singleSpan] -> Right singleSpan
      (firstSpan : remainingSpans) ->
        foldl'
          (\accResult nextSpan ->
              accResult >>= \accSpan -> KernelCompose.crRewrite <$> KernelCompose.composePatternRewrites nextSpan accSpan
          )
          (Right firstSpan)
          remainingSpans

instance (HasConstructorTag f, ZipMatch f) => NerveSiteAlgebra (RewriteTag f) where
  type NerveCategory (RewriteTag f) = FiniteRewriteCategory RewriteOriginAtom (LogicalDecoration ScopeCtx) f
  type NerveSource (RewriteTag f) = Pattern f
  type NerveMorphism (RewriteTag f) = RewriteMorphism f

  buildSiteNerve = rewriteNerve
  simplexSourceValue = simplexSourcePattern
  simplexMorphismChain = simplexRewrites

renderVars :: Set.Set PatternVar -> Set.Set (InterfaceName tag)
renderVars =
  Set.map (interfaceNameFromString . show . patternVarKey)

objectVisibleInContext :: Eq (Pattern f) => RewriteContext f -> Pattern f -> Bool
objectVisibleInContext contextValue objectValue =
  objectValue `elem` rcObjects contextValue

morphismVisibleInContext :: Eq (Pattern f) => RewriteContext f -> RewriteMorphism f -> Bool
morphismVisibleInContext contextValue morphismValue =
  objectVisibleInContext contextValue (prLeft morphismValue)
    && objectVisibleInContext contextValue (prRight morphismValue)

validateRewriteSystemContexts :: Ord (Pattern f) => RewriteSystem f -> Either (RewriteContextPresentationError f) (RewriteSystem f)
validateRewriteSystemContexts rewriteSystem =
  case invalidCarrierContext rewriteSystem of
    Just invalidContext ->
      Left (ContextContainsUnknownObjects (rcOrdinal invalidContext))
    Nothing ->
      case nonAntitonePair rewriteSystem of
        Just (smallerContext, largerContext) ->
          Left (NonAntitoneVisibility (rcOrdinal smallerContext) (rcOrdinal largerContext))
        Nothing ->
          Right rewriteSystem

buildRewriteSystem ::
  Ord (Pattern f) =>
  FiniteRewriteCategory RewriteOriginAtom (LogicalDecoration ScopeCtx) f ->
  [[Pattern f]] ->
  [IdentifiedRewriteSpan f] ->
  ContextPairStrategy (RewriteContext f) ->
  RewriteSystem f
buildRewriteSystem categoryValue contextObjects identifiedSpans pairStrategy =
  let fullContext = frcObjects categoryValue
      normalizedContexts =
        normalizeContextObjects fullContext contextObjects
          & zipWith
            mkRewriteContext
            [0 ..]
   in RewriteSystem
        { rsCategory = categoryValue,
          rsContexts = normalizedContexts,
          rsPresentationContexts = normalizedContexts,
          rsPairStrategy = pairStrategy,
          rsRuleIdentities = identifiedSpans
        }

normalizeContextObjects :: Ord pattern => [pattern] -> [[pattern]] -> [[pattern]]
normalizeContextObjects fullContext providedContexts =
  let canonicalFullContext = canonicalizeContextObjects fullContext
      requestedContexts = fmap canonicalizeContextObjects providedContexts
      contextsWithRoot =
        if any (== canonicalFullContext) requestedContexts
          then requestedContexts
          else canonicalFullContext : requestedContexts
   in nubBy (==) contextsWithRoot

canonicalizeContextObjects :: Ord pattern => [pattern] -> [pattern]
canonicalizeContextObjects =
  nubBy (==) . sort

ruleObjectContextObjects :: RewriteMorphism f -> [Pattern f]
ruleObjectContextObjects spanValue =
  [prLeft spanValue, prRight spanValue]

instance Ord (Pattern f) => JoinSemilattice (RewriteContext f) where
  join leftContext rightContext =
    RewriteContext
      { rcOrdinal = min (rcOrdinal leftContext) (rcOrdinal rightContext),
        rcObjects = canonicalizeContextObjects (rcObjects leftContext <> rcObjects rightContext)
      }

instance Ord (Pattern f) => MeetSemilattice (RewriteContext f) where
  meet leftContext rightContext =
    RewriteContext
      { rcOrdinal = min (rcOrdinal leftContext) (rcOrdinal rightContext),
        rcObjects = filter (`elem` rcObjects rightContext) (rcObjects leftContext)
      }

instance Ord (Pattern f) => Lattice (RewriteContext f)

invalidCarrierContext :: Ord (Pattern f) => RewriteSystem f -> Maybe (RewriteContext f)
invalidCarrierContext rewriteSystem =
  let carrierObjects = frcObjects (rsCategory rewriteSystem)
   in find
        (\contextValue ->
            any (`notElem` carrierObjects) (rcObjects contextValue)
        )
        (rsContexts rewriteSystem)

nonAntitonePair :: Ord (Pattern f) => RewriteSystem f -> Maybe (RewriteContext f, RewriteContext f)
nonAntitonePair rewriteSystem =
  let contexts = rsContexts rewriteSystem
      morphisms = frcRewrites (rsCategory rewriteSystem)
      downwardPairs =
        contexts
          >>= (\smallerContext ->
                  contexts
                    & filter
                      (\largerContext ->
                          rcObjects smallerContext /= rcObjects largerContext
                            && all (`elem` rcObjects largerContext) (rcObjects smallerContext)
                      )
                    & fmap (\largerContext -> (smallerContext, largerContext))
             )
   in find
        (\(smallerContext, largerContext) ->
            any
              (\morphismValue ->
                  morphismVisibleInContext smallerContext morphismValue
                    && not (morphismVisibleInContext largerContext morphismValue)
              )
              morphisms
        )
        downwardPairs

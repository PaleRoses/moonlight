{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Moonlight.EGraph.Pure.Saturation.Front
  ( FrontPhase (..),
    EGraphFront,
    EGraphFrontError (..),
    EGraphFrontReport (..),
    EGraphFrontObservedReport (..),
    egraph,
    CompiledEGraphFront,
    FrontSeedTerm,
    frontSeedTerm,
    frontSeedTermNamed,
    compileEGraphFront,
    runCompiledEGraphFront,
    runCompiledEGraphFrontObserved,
    runEGraphFront,
    runEGraphFrontObserved,
    EGraphFrontM,
    RulesetM,
    FrontSchedule,
    FrontOutput,
    Extracted,
    FrontNameError (..),
    FrontRulesetName,
    mkFrontRulesetName,
    frontRulesetNameString,
    FrontSeedName,
    mkFrontSeedName,
    frontSeedNameString,
    FrontRelationName,
    mkFrontRelationName,
    frontRelationNameString,
    ContextRef,
    RulesetRef,
    TermRef,
    RelationRef,
    relationRefFactId,
    relationRefWithFactId,
    FrontCheck,
    FrontGuardAtom,
    context,
    contextNamed,
    ruleset,
    rulesetNamed,
    rewrite,
    rewriteNamed,
    birewrite,
    birewriteNamed,
    def,
    defNamed,
    defAt,
    defAtNamed,
    relation,
    relationNamed,
    fact,
    factArgs,
    factRule,
    factRuleNamed,
    has,
    hasArgs,
    FactArgs (..),
    FactRefArgs (..),
    run,
    saturate,
    runFor,
    runUntil,
    skipSchedule,
    seqSchedule,
    repeatSchedule,
    done,
    check,
    checkAt,
    extract,
    extractAt,
    (===),
    atContext,
    defaultFrontBudget,
    frontErrorMessage,
    Term,
    node,
    (==>),
    (=:=),
    (=/=),
    when_,
    requires_,
    forbids_,
    extension,
    rootExtension,
    globalExtension,
    SaturationBudget (..),
  )
where

import Data.Bifunctor (first)
import Data.Char (isAlphaNum, isSpace)
import Data.Foldable (fold, traverse_)
import Data.Functor.Identity (Identity (..), runIdentity)
import Data.Kind (Type)
import Data.List (dropWhileEnd, genericReplicate)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Fix
  ( Fix (..),
  )
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Control.Monad (ap, foldM)
import Moonlight.Algebra (JoinSemilattice)
import Moonlight.Core (ZipMatch)
import Moonlight.EGraph.Effect.CoveringSurface
  ( SurfaceKind,
    surfaceKindDigest,
  )
import Moonlight.Control.Class
  ( attempt,
    choices,
    phase,
    sequenceAll,
    skip,
    upTo,
  )
import Moonlight.Control.Program
  ( ProgramAlgebra (..),
    foldProgram,
  )
import Moonlight.Control.Program qualified as ControlProgram
import Moonlight.Core
  ( ClassId,
    Pattern (..),
    RewriteRuleId,
  )
import Moonlight.Core qualified as EGraph
import Moonlight.Core
  ( MatchActivationIndex (..),
    SiteIndex (..),
    SiteProgram (..),
  )
import Moonlight.EGraph.Pure.Change
  ( EGraphMutationTrace (..),
    EGraphRebuildTrace (..),
  )
import Moonlight.EGraph.Pure.Context
  ( ContextDeltaError (..),
    ContextMutationTrace (..),
    ContextRebaseBatch,
    ContextRebaseReport (..),
    beginContextRebaseBatch,
    commitContextRebaseBatch,
    stageTermAtContext,
    stageTermGlobally,
    stageTermsGlobally,
  )
import Moonlight.EGraph.Pure.Extraction
  ( AnalysisCostAlgebra,
    ExtractionResult (..),
  )
import Moonlight.EGraph.Pure.Saturation.Front.PackedNode
  ( PackedNode,
    packAnalysisCostAlgebra,
    packFix,
    packedNode,
    unpackExtractionResult,
  )
import Moonlight.EGraph.Pure.Saturation.Front.PackedPlan
  ( PackedPlanError,
    packCompiledFactRule,
    packRulePlanSet,
  )
import Moonlight.EGraph.Pure.Saturation.Logic.Observation
  ( StableObservation (..),
    runStableObservation,
  )
import Moonlight.EGraph.Pure.Saturation.Logic.Run
  ( EGraphLogicError (..),
    EGraphLogicObservedReport (..),
    EGraphLogicReport (..),
    logic,
    runCompiledEGraphLogic,
    runCompiledEGraphLogicObserved,
    seedFacts,
  )
import Moonlight.EGraph.Pure.Saturation.Matching
  ( MatchingStrategy (GenericJoinMatching),
  )
import Moonlight.EGraph.Pure.Saturation.Substrate
  ( EGraphU,
  )
import Moonlight.EGraph.Saturation.Context.State
  ( SaturatingContextEGraph,
    emptySaturatingContextEGraph,
    sceContextGraph,
  )
import Moonlight.EGraph.Pure.Structural.Store
  ( StructuralTuplePatch (..),
  )
import Moonlight.EGraph.Pure.Types
  ( ENode (..),
  )
import Moonlight.Rewrite.DSL
  ( CanonicalProgram,
    canonicalSupportIndex,
    compileProgramRuleSet,
  )
import Moonlight.Rewrite.DSL
  ( ProgramError,
    prettyProgramError,
  )
import Moonlight.Rewrite.DSL qualified as DSLProgram
import Moonlight.Rewrite.DSL qualified as DSLRule
import Moonlight.Rewrite.DSL
  ( HTraversable (..),
    K (..),
    Node (..),
    NodeTag,
    RewriteSignature (..),
  )
import Moonlight.Rewrite.DSL
  ( SomeTypedVar (..),
    Term (..),
    node,
    someTypedVarName,
    someTypedVarSort,
    sortNameString,
  )
import Moonlight.Rewrite.Algebra
  ( PatternExtensionScope,
  )
import Moonlight.Rewrite.System
  ( GuardTerm,
    guardHasFactTerms,
    data GuardRoot,
  )
import Moonlight.Rewrite.System
  ( CompiledFactRule,
    FactRule,
    FactRuleCompileError,
    FactRuleId (..),
    RawFactRule (..),
    compileFactRules,
  )
import Moonlight.Rewrite.System
  ( FactId (..),
    FactStore,
    FactTuple (..),
    emptyFactStore,
    insertFact,
  )
import Moonlight.Rewrite.System
  ( RulePlan,
    rpId,
    RulePlanSet,
    lookupRulePlan,
  )
import Moonlight.Rewrite.System
  ( RuleName,
    RuleNameError,
    mkRuleName,
    ruleNameString,
  )
import Moonlight.Rewrite.System
  ( RuleSupportIndex,
    baseSupportRuleNames,
    contextSupportEntries,
  )
import Moonlight.Saturation.Context.Driver
  ( ContextRunSpec (..),
    plainContextRunSpec,
  )
import Moonlight.Saturation.Context.Program.Plan
  ( Program,
    ProgramStage (CompiledProgramStage),
    Plan,
    mkPlan,
    planPlanSpec,
  )
import Moonlight.Saturation.Context.Program.Spec
  ( PlanSpec,
    defaultPlanSpec,
  )
import Moonlight.Saturation.Context.Runtime.Report
  ( srCarrier,
  )
import Moonlight.Saturation.Context.Runtime.Engine
  ( RuntimeIOTiming,
  )
import Moonlight.Saturation.Core
  ( SaturationBudget (..),
    TerminationGoal (..),
  )
import Moonlight.Saturation.Matching
  ( MatchSite (BaseSite),
  )
import Moonlight.Saturation.Substrate
  ( SatRule,
  )
import Numeric.Natural (Natural)

import Moonlight.Rewrite.DSL
  ( (=/=),
    (=:=),
    (==>),
    extension,
    forbids_,
    globalExtension,
    requires_,
    rootExtension,
    when_,
  )

data FrontNameError
  = EmptyFrontName
  | InvalidFrontName
  deriving stock (Eq, Ord, Show)

newtype FrontRulesetName = FrontRulesetName
  { unFrontRulesetName :: String
  }
  deriving stock (Eq, Ord, Show)

newtype FrontSeedName = FrontSeedName
  { unFrontSeedName :: String
  }
  deriving stock (Eq, Ord, Show)

newtype FrontRelationName = FrontRelationName
  { unFrontRelationName :: String
  }
  deriving stock (Eq, Ord, Show)

mkFrontRulesetName :: String -> Either FrontNameError FrontRulesetName
mkFrontRulesetName =
  mkFrontName FrontRulesetName

mkFrontSeedName :: String -> Either FrontNameError FrontSeedName
mkFrontSeedName =
  mkFrontName FrontSeedName

mkFrontRelationName :: String -> Either FrontNameError FrontRelationName
mkFrontRelationName =
  mkFrontName FrontRelationName

frontRulesetNameString :: FrontRulesetName -> String
frontRulesetNameString =
  unFrontRulesetName

frontSeedNameString :: FrontSeedName -> String
frontSeedNameString =
  unFrontSeedName

frontRelationNameString :: FrontRelationName -> String
frontRelationNameString =
  unFrontRelationName

mkFrontName :: (String -> name) -> String -> Either FrontNameError name
mkFrontName wrapName raw =
  case stripFrontName raw of
    normalized
      | null normalized ->
          Left EmptyFrontName
    normalized ->
      if isValidFrontNamePath normalized
        then Right (wrapName normalized)
        else Left InvalidFrontName

stripFrontName :: String -> String
stripFrontName =
  dropWhile isSpace . dropWhileEnd isSpace

isValidFrontNamePath :: String -> Bool
isValidFrontNamePath =
  all isValidFrontIdentifier . frontNameSegments

frontNameSegments :: String -> [String]
frontNameSegments raw =
  case break (== '/') raw of
    (segment, []) ->
      [segment]
    (segment, _slash : rest) ->
      segment : frontNameSegments rest

isValidFrontIdentifier :: String -> Bool
isValidFrontIdentifier candidate =
  not (null candidate) && all isFrontIdentifierCharacter candidate

isFrontIdentifierCharacter :: Char -> Bool
isFrontIdentifierCharacter character =
  isAlphaNum character || character == '-' || character == '_'

data FrontNameInput name error = FrontNameInput
  { frontNameInputRaw :: !String,
    frontNameInputParsed :: !(Either error name)
  }
  deriving stock (Eq, Ord, Show)

frontNameInput :: (String -> Either error name) -> String -> FrontNameInput name error
frontNameInput parse raw =
  FrontNameInput
    { frontNameInputRaw = raw,
      frontNameInputParsed = parse raw
    }

requireFrontNameInput ::
  (String -> error -> EGraphFrontError owner sig analysis context) ->
  FrontNameInput name error ->
  Either (EGraphFrontError owner sig analysis context) name
requireFrontNameInput makeError input =
  first (makeError (frontNameInputRaw input)) (frontNameInputParsed input)

-- | The front's phase index.  The authored program is the source of truth;
-- compiled/seeded/saturated values are derived views.
type FrontPhase :: Type
data FrontPhase
  = Authored
  | Compiled
  | Seeded
  | Saturated

-- | Canonical egglog-like front program.
type EGraphFront :: FrontPhase -> Type -> (Symbol -> (Symbol -> Type) -> Type) -> Type -> Type -> Type -> Type
data EGraphFront phase owner sig analysis context result where
  AuthoredFront :: !(FrontProgram sig analysis context) -> !(FrontOutput owner sig analysis context result) -> EGraphFront 'Authored owner sig analysis context result
  CompiledFront :: !(CompiledFrontState owner sig analysis context) -> !(FrontOutput owner sig analysis context result) -> EGraphFront 'Compiled owner sig analysis context result

type CompiledEGraphFront owner sig analysis context result =
  EGraphFront 'Compiled owner sig analysis context result

type EGraphFrontM :: (Symbol -> (Symbol -> Type) -> Type) -> Type -> Type -> Type -> Type
newtype EGraphFrontM sig analysis context result = EGraphFrontM
  { runEGraphFrontM :: FrontBuilder sig analysis context -> (result, FrontBuilder sig analysis context)
  }

instance Functor (EGraphFrontM sig analysis context) where
  fmap transform action =
    EGraphFrontM
      ( \builder ->
          let (value, nextBuilder) = runEGraphFrontM action builder
           in (transform value, nextBuilder)
      )

instance Applicative (EGraphFrontM sig analysis context) where
  pure value =
    EGraphFrontM (\builder -> (value, builder))

  (<*>) = ap

instance Monad (EGraphFrontM sig analysis context) where
  action >>= continue =
    EGraphFrontM
      ( \builder ->
          let (value, nextBuilder) = runEGraphFrontM action builder
           in runEGraphFrontM (continue value) nextBuilder
      )

type RulesetM :: (Symbol -> (Symbol -> Type) -> Type) -> Type -> Type
newtype RulesetM sig result = RulesetM
  { runRulesetM :: RulesetBuilder sig -> (result, RulesetBuilder sig)
  }

instance Functor (RulesetM sig) where
  fmap transform action =
    RulesetM
      ( \builder ->
          let (value, nextBuilder) = runRulesetM action builder
           in (transform value, nextBuilder)
      )

instance Applicative (RulesetM sig) where
  pure value =
    RulesetM (\builder -> (value, builder))

  (<*>) = ap

instance Monad (RulesetM sig) where
  action >>= continue =
    RulesetM
      ( \builder ->
          let (value, nextBuilder) = runRulesetM action builder
           in runRulesetM (continue value) nextBuilder
      )

type FrontSchedule :: (Symbol -> (Symbol -> Type) -> Type) -> Type -> Type -> Type
newtype FrontSchedule sig analysis context = FrontSchedule
  { frontScheduleTree :: ControlProgram.Program () (FrontSchedulePhase sig analysis context)
  }

instance Semigroup (FrontSchedule sig analysis context) where
  left <> right =
    seqSchedule [left, right]

instance Monoid (FrontSchedule sig analysis context) where
  mempty =
    skipSchedule

type Extracted :: (Symbol -> (Symbol -> Type) -> Type) -> Symbol -> Type -> Type
type Extracted sig _sort cost = ExtractionResult (Node sig) cost

type FrontOutput :: Type -> (Symbol -> (Symbol -> Type) -> Type) -> Type -> Type -> Type -> Type
newtype FrontOutput owner sig analysis context result = FrontOutput
  { stageFrontOutput ::
      ObservationSeedState owner sig analysis context ->
      Either
        (EGraphFrontError owner sig analysis context)
        (ObservationSeedState owner sig analysis context, FrontResolvedOutput owner sig analysis context result)
  }

type FrontResolvedOutput :: Type -> (Symbol -> (Symbol -> Type) -> Type) -> Type -> Type -> Type -> Type
newtype FrontResolvedOutput owner sig analysis context result = FrontResolvedOutput
  { resolveFrontOutput ::
      SaturatingContextEGraph owner SurfaceKind (PackedNode sig) analysis context ->
      Either (EGraphFrontError owner sig analysis context) result
  }

instance Functor (FrontOutput owner sig analysis context) where
  fmap transform output =
    FrontOutput $ \state -> do
      (nextState, resolved) <- stageFrontOutput output state
      pure (nextState, fmap transform resolved)

instance Applicative (FrontOutput owner sig analysis context) where
  pure value =
    FrontOutput $ \state ->
      Right (state, pure value)

  leftOutput <*> rightOutput =
    FrontOutput $ \state -> do
      (leftState, leftResolved) <- stageFrontOutput leftOutput state
      (rightState, rightResolved) <- stageFrontOutput rightOutput leftState
      pure (rightState, leftResolved <*> rightResolved)

instance Functor (FrontResolvedOutput owner sig analysis context) where
  fmap transform resolved =
    FrontResolvedOutput $ \graph ->
      transform <$> resolveFrontOutput resolved graph

instance Applicative (FrontResolvedOutput owner sig analysis context) where
  pure value =
    FrontResolvedOutput $ \_graph ->
      Right value

  transformOutput <*> valueOutput =
    FrontResolvedOutput $ \graph ->
      resolveFrontOutput transformOutput graph <*> resolveFrontOutput valueOutput graph

data FrontBuilder sig analysis context = FrontBuilder
  { fbContexts :: ![ContextDecl context],
    fbRulesets :: ![RulesetDecl sig],
    fbRelations :: ![SomeRelationDecl],
    fbSeeds :: ![SeedDecl sig context],
    fbFacts :: ![SeedFactDecl sig],
    fbSchedules :: ![FrontSchedule sig analysis context],
    fbNextRelationId :: !Int
  }

data RulesetBuilder sig = RulesetBuilder
  { rbRules :: ![RuleDecl sig]
  }

data ContextRef context = ContextRef
  { contextRefName :: !(FrontNameInput DSLRule.ContextName DSLRule.ContextNameError),
    contextRefValue :: !context
  }

data RulesetRef = RulesetRef
  { rulesetRefName :: !(FrontNameInput FrontRulesetName FrontNameError)
  }
  deriving stock (Eq, Ord, Show)

newtype TermRef sig (sort :: Symbol) = TermRef
  { termRefName :: FrontNameInput FrontSeedName FrontNameError
  }
  deriving stock (Eq, Ord, Show)

data RelationRef (sorts :: [Symbol]) = RelationRef
  { relationRefName :: !(FrontNameInput FrontRelationName FrontNameError),
    relationRefFactId :: !FactId
  }
  deriving stock (Eq, Ord, Show)

data FrontCheck sig where
  FrontCheckEq :: !(TermRef sig sort) -> !(Term sig sort) -> FrontCheck sig

(===) :: TermRef sig sort -> Term sig sort -> FrontCheck sig
(===) =
  FrontCheckEq

infix 4 ===

data FactArgs sig (sorts :: [Symbol]) where
  FactNil :: FactArgs sig '[]
  (:&) :: Term sig sort -> FactArgs sig sorts -> FactArgs sig (sort ': sorts)

infixr 5 :&

data FactRefArgs sig (sorts :: [Symbol]) where
  FactRefNil :: FactRefArgs sig '[]
  (:@&) :: TermRef sig sort -> FactRefArgs sig sorts -> FactRefArgs sig (sort ': sorts)

infixr 5 :@&

data FrontGuardAtom sig where
  FrontHasFact :: !(RelationRef sorts) -> !(FactArgs sig sorts) -> FrontGuardAtom sig

instance DSLRule.RewriteGuardAtom FrontGuardAtom where
  type GuardCapabilityKey FrontGuardAtom = SurfaceKind

  guardCapabilityDigest _ =
    surfaceKindDigest

  lowerGuardAtom lower (FrontHasFact relationRef args) =
    guardHasFactTerms (relationRefFactId relationRef)
      <$> traverseFactArgs lower args

data ContextDecl context = ContextDecl
  { cdFrontName :: !(FrontNameInput DSLRule.ContextName DSLRule.ContextNameError),
    cdContextValue :: !context
  }

data RulesetDecl sig = RulesetDecl
  { rsdName :: !(FrontNameInput FrontRulesetName FrontNameError),
    rsdRules :: ![RuleDecl sig]
  }

data RuleDecl sig
  = RewriteDecl !(FrontNameInput RuleName RuleNameError) !(DSLRule.RuleBody sig FrontGuardAtom)
  | FactRuleDecl !(FrontNameInput RuleName RuleNameError) !FactId !(SomeTerm sig)

data SomeRelationDecl where
  SomeRelationDecl :: !(RelationRef sorts) -> SomeRelationDecl

data SomeTerm sig where
  SomeTerm :: !(Term sig sort) -> SomeTerm sig

data SeedDecl sig context where
  SeedDecl :: !(FrontNameInput FrontSeedName FrontNameError) -> !(Maybe (ContextRef context)) -> !(Term sig sort) -> SeedDecl sig context

data FrontSeedTerm sig where
  FrontSeedTerm :: !(FrontNameInput FrontSeedName FrontNameError) -> !(Term sig sort) -> FrontSeedTerm sig

data FrontSeedSlot context = FrontSeedSlot
  { fssName :: !(FrontNameInput FrontSeedName FrontNameError),
    fssContext :: !(Maybe (ContextRef context))
  }

data RuntimeSeedDecl sig context where
  RuntimeSeedDecl :: !FrontSeedName -> !(Maybe (ContextRef context)) -> !(Term sig sort) -> RuntimeSeedDecl sig context

data SeedFactDecl sig where
  SeedFactDecl :: !(RelationRef sorts) -> !(FactRefArgs sig sorts) -> SeedFactDecl sig

data FrontSchedulePhase sig analysis context = FrontRunRuleset
  { fspBudget :: !SaturationBudget,
    fspRuleset :: !RulesetRef,
    fspGoal :: !(Maybe (FrontCheck sig))
  }

data FrontRuntimeSchedulePhase owner sig analysis context = FrontRuntimeRunRuleset
  { frspPlan :: !(FrontCompiledPlan owner sig analysis context),
    frspGoal :: !(TerminationGoal (SaturatingContextEGraph owner SurfaceKind (PackedNode sig) analysis context))
  }

type FrontCompiledProgram owner sig analysis context =
  Program 'CompiledProgramStage (EGraphU owner SurfaceKind (PackedNode sig) analysis context)

type FrontCompiledPlan owner sig analysis context =
  Plan
    (EGraphU owner SurfaceKind (PackedNode sig) analysis context)
    (SaturatingContextEGraph owner SurfaceKind (PackedNode sig) analysis context)
    RewriteRuleId

data FrontPlanKey = FrontPlanKey
  { fpkRuleset :: !FrontRulesetName,
    fpkBudget :: !SaturationBudget
  }
  deriving stock (Eq, Ord, Show)

type FrontRuntimeSchedule :: Type -> (Symbol -> (Symbol -> Type) -> Type) -> Type -> Type -> Type
newtype FrontRuntimeSchedule owner sig analysis context = FrontRuntimeSchedule
  { frontRuntimeScheduleTree :: ControlProgram.Program () (FrontRuntimeSchedulePhase owner sig analysis context)
  }

data CompiledFrontState owner sig analysis context = CompiledFrontState
  { cfCanonical :: !(CanonicalProgram sig FrontGuardAtom),
    cfRulePlans :: !(RulePlanSet SurfaceKind (PackedNode sig)),
    cfRulesets :: !(Map FrontRulesetName [RuleName]),
    cfFactRulesets :: !(Map FrontRulesetName [CompiledFactRule SurfaceKind (PackedNode sig)]),
    cfContexts :: !(Map DSLRule.ContextName context),
    cfSeedSlots :: ![FrontSeedSlot context],
    cfSeedFacts :: ![SeedFactDecl sig],
    cfSchedules :: ![FrontSchedule sig analysis context],
    cfRulesetPrograms :: !(Map FrontRulesetName (FrontCompiledProgram owner sig analysis context)),
    cfRulesetPlans :: !(Map FrontRulesetName (Map SaturationBudget (FrontCompiledPlan owner sig analysis context)))
  }

data FrontProgram sig analysis context = FrontProgram
  { fpContexts :: ![ContextDecl context],
    fpRulesets :: ![RulesetDecl sig],
    fpRelations :: ![SomeRelationDecl],
    fpSeeds :: ![SeedDecl sig context],
    fpFacts :: ![SeedFactDecl sig],
    fpSchedules :: ![FrontSchedule sig analysis context]
  }

data EGraphFrontError owner sig analysis context
  = EGraphFrontProgramError !(ProgramError sig)
  | EGraphFrontInvalidContextName !String !DSLRule.ContextNameError
  | EGraphFrontInvalidRulesetName !String !FrontNameError
  | EGraphFrontInvalidSeedName !String !FrontNameError
  | EGraphFrontInvalidRelationName !String !FrontNameError
  | EGraphFrontDuplicateContext !DSLRule.ContextName
  | EGraphFrontDuplicateRuleset !FrontRulesetName
  | EGraphFrontDuplicateSeed !FrontSeedName
  | EGraphFrontDuplicateRelation !FrontRelationName
  | EGraphFrontInvalidRuleName !String
  | EGraphFrontUnknownRuleset !FrontRulesetName
  | EGraphFrontUnknownSeed !FrontSeedName
  | EGraphFrontMissingCompiledRule !RuleName
  | EGraphFrontMissingCompiledPlan !FrontRulesetName !SaturationBudget
  | EGraphFrontUnknownCompiledContext !DSLRule.ContextName
  | EGraphFrontFactCompileError !FactRuleCompileError
  | EGraphFrontPackedPlanInvalid !PackedPlanError
  | EGraphFrontGroundTermContainsVariable !String !String
  | EGraphFrontInternalVariableClosureMiss !String !String
  | EGraphFrontContextDeltaError !(ContextDeltaError (Node sig) context)
  | EGraphFrontLogicError !(EGraphLogicError owner SurfaceKind (PackedNode sig) analysis context)
  | EGraphFrontScheduleChoiceFailed

-- | Runtime report for the front: schedule reports are engine reports, named
-- observations are keyed by the author-facing names, and seedTrace is the single
-- mutation trace owner for all front staging edits.
data EGraphFrontReport owner sig analysis context result = EGraphFrontReport
  { efrResult :: !result,
    efrFinalGraph :: !(SaturatingContextEGraph owner SurfaceKind (PackedNode sig) analysis context),
    efrScheduleReports :: ![EGraphLogicReport owner SurfaceKind (PackedNode sig) analysis context],
    efrSeedClasses :: !(Map FrontSeedName ClassId),
    efrSeedTrace :: !(ContextMutationTrace owner context (Node sig))
  }

data EGraphFrontObservedReport owner sig analysis context result = EGraphFrontObservedReport
  { eforReport :: !(EGraphFrontReport owner sig analysis context result),
    eforScheduleTimings :: ![RuntimeIOTiming]
  }

data PreparedFrontRun owner sig analysis context result = PreparedFrontRun
  { pfrCompiled :: !(CompiledFrontState owner sig analysis context),
    pfrSeeded :: !(SeededFrontState owner sig analysis context result)
  }

egraph :: EGraphFrontM sig analysis context (FrontOutput owner sig analysis context result) -> EGraphFront 'Authored owner sig analysis context result
egraph action =
  let (output, builder) =
        runEGraphFrontM action emptyFrontBuilder
   in AuthoredFront
        (frontProgramFromBuilder builder)
        output

frontName :: forall name. KnownSymbol name => String
frontName =
  symbolVal (Proxy @name)

context :: forall name sig analysis context. KnownSymbol name => context -> EGraphFrontM sig analysis context (ContextRef context)
context contextValue =
  contextNamed (frontName @name) contextValue

contextNamed :: String -> context -> EGraphFrontM sig analysis context (ContextRef context)
contextNamed rawName contextValue =
  EGraphFrontM
    ( \builder ->
        let nameInput =
              frontNameInput DSLRule.contextName rawName
            ref = ContextRef nameInput contextValue
         in ( ref,
              builder
                { fbContexts = ContextDecl nameInput contextValue : fbContexts builder
                }
            )
    )

ruleset :: forall name sig analysis context. KnownSymbol name => RulesetM sig () -> EGraphFrontM sig analysis context RulesetRef
ruleset =
  rulesetNamed (frontName @name)

rulesetNamed :: String -> RulesetM sig () -> EGraphFrontM sig analysis context RulesetRef
rulesetNamed rawName action =
  EGraphFrontM
    ( \builder ->
        let rulesetBuilder = snd (runRulesetM action emptyRulesetBuilder)
            nameInput =
              frontNameInput mkFrontRulesetName rawName
            ref = RulesetRef nameInput
         in ( ref,
              builder
                { fbRulesets =
                    RulesetDecl
                      { rsdName = nameInput,
                        rsdRules = reverse (rbRules rulesetBuilder)
                      }
                      : fbRulesets builder
                }
            )
    )

rewrite :: forall name sig. (KnownSymbol name, RewriteSignature sig) => DSLRule.RuleBody sig FrontGuardAtom -> RulesetM sig ()
rewrite =
  rewriteNamed (frontName @name)

rewriteNamed :: RewriteSignature sig => String -> DSLRule.RuleBody sig FrontGuardAtom -> RulesetM sig ()
rewriteNamed rawName body =
  appendRuleDecl (RewriteDecl (frontNameInput mkRuleName rawName) (closeRuleBody body))

birewrite :: forall name sig sort. (KnownSymbol name, RewriteSignature sig) => Term sig sort -> Term sig sort -> RulesetM sig ()
birewrite =
  birewriteNamed (frontName @name)

birewriteNamed :: RewriteSignature sig => String -> Term sig sort -> Term sig sort -> RulesetM sig ()
birewriteNamed rawName leftTerm rightTerm = do
  rewriteNamed (rawName <> ".forward") (leftTerm ==> rightTerm)
  rewriteNamed (rawName <> ".backward") (rightTerm ==> leftTerm)

def :: forall name sig analysis context sort. KnownSymbol name => Term sig sort -> EGraphFrontM sig analysis context (TermRef sig sort)
def =
  defNamed (frontName @name)

defNamed :: String -> Term sig sort -> EGraphFrontM sig analysis context (TermRef sig sort)
defNamed rawName =
  seedWithContext rawName Nothing

defAt :: forall name sig analysis context sort. KnownSymbol name => ContextRef context -> Term sig sort -> EGraphFrontM sig analysis context (TermRef sig sort)
defAt contextRef =
  defAtNamed (frontName @name) contextRef

defAtNamed :: String -> ContextRef context -> Term sig sort -> EGraphFrontM sig analysis context (TermRef sig sort)
defAtNamed rawName contextRef =
  seedWithContext rawName (Just contextRef)

frontSeedTerm :: forall name sig sort. KnownSymbol name => Term sig sort -> FrontSeedTerm sig
frontSeedTerm =
  frontSeedTermNamed (frontName @name)

frontSeedTermNamed :: String -> Term sig sort -> FrontSeedTerm sig
frontSeedTermNamed rawName =
  FrontSeedTerm (frontNameInput mkFrontSeedName rawName)

relation :: forall name sig analysis context sorts. KnownSymbol name => EGraphFrontM sig analysis context (RelationRef sorts)
relation =
  relationNamed (frontName @name)

relationNamed :: String -> EGraphFrontM sig analysis context (RelationRef sorts)
relationNamed rawName =
  EGraphFrontM
    ( \builder ->
        let ref = RelationRef (frontNameInput mkFrontRelationName rawName) (FactId (fbNextRelationId builder))
         in ( ref,
              builder
                { fbRelations = SomeRelationDecl ref : fbRelations builder,
                  fbNextRelationId = fbNextRelationId builder + 1
                }
            )
    )

relationRefWithFactId :: String -> FactId -> RelationRef sorts
relationRefWithFactId rawName =
  RelationRef (frontNameInput mkFrontRelationName rawName)

fact :: RelationRef '[sort] -> TermRef sig sort -> EGraphFrontM sig analysis context ()
fact relationRef termRef =
  factArgs relationRef (termRef :@& FactRefNil)

factArgs :: RelationRef sorts -> FactRefArgs sig sorts -> EGraphFrontM sig analysis context ()
factArgs relationRef args =
  EGraphFrontM
    ( \builder ->
        ( (),
          builder
            { fbFacts = SeedFactDecl relationRef args : fbFacts builder
            }
        )
    )

factRule :: forall name sig sort. KnownSymbol name => RelationRef '[sort] -> Term sig sort -> RulesetM sig ()
factRule relationRef termValue =
  factRuleNamed (frontName @name) relationRef termValue

factRuleNamed :: String -> RelationRef '[sort] -> Term sig sort -> RulesetM sig ()
factRuleNamed rawName relationRef termValue =
  appendRuleDecl (FactRuleDecl (frontNameInput mkRuleName rawName) (relationRefFactId relationRef) (SomeTerm termValue))

has :: RelationRef '[sort] -> Term sig sort -> DSLRule.Guard sig FrontGuardAtom
has relationRef termValue =
  hasArgs relationRef (termValue :& FactNil)

hasArgs :: RelationRef sorts -> FactArgs sig sorts -> DSLRule.Guard sig FrontGuardAtom
hasArgs relationRef args =
  DSLRule.atom_ (FrontHasFact relationRef args)

run :: FrontSchedule sig analysis context -> EGraphFrontM sig analysis context ()
run schedule =
  EGraphFrontM
    ( \builder ->
        ( (),
          builder
            { fbSchedules = schedule : fbSchedules builder
            }
        )
    )

defaultFrontBudget :: SaturationBudget
defaultFrontBudget =
  SaturationBudget
    { sbMaxIterations = 100,
      sbMaxNodes = 100000
    }

saturate :: RulesetRef -> FrontSchedule sig analysis context
saturate =
  runFor defaultFrontBudget

runFor :: SaturationBudget -> RulesetRef -> FrontSchedule sig analysis context
runFor budget rulesetRef =
  FrontSchedule (phase (FrontRunRuleset budget rulesetRef Nothing))

runUntil :: FrontCheck sig -> FrontSchedule sig analysis context -> FrontSchedule sig analysis context
runUntil checkValue schedule =
  FrontSchedule (fmap attachGoal (frontScheduleTree schedule))
  where
    attachGoal phaseValue =
      phaseValue {fspGoal = Just checkValue}

skipSchedule :: FrontSchedule sig analysis context
skipSchedule =
  FrontSchedule skip

seqSchedule :: [FrontSchedule sig analysis context] -> FrontSchedule sig analysis context
seqSchedule schedules =
  FrontSchedule (sequenceAll (fmap frontScheduleTree schedules))

repeatSchedule :: Natural -> FrontSchedule sig analysis context -> FrontSchedule sig analysis context
repeatSchedule repeatCount schedule =
  FrontSchedule (upTo repeatCount (frontScheduleTree schedule))

done :: FrontOutput owner sig analysis context ()
done =
  pure ()

check :: forall name owner sig analysis context. (KnownSymbol name, RewriteSignature sig, Ord (NodeTag sig), Ord context) => FrontCheck sig -> EGraphFrontM sig analysis context (FrontOutput owner sig analysis context Bool)
check =
  pure . checkOutput (frontName @name) Nothing

checkAt :: forall name owner sig analysis context. (KnownSymbol name, RewriteSignature sig, Ord (NodeTag sig), Ord context) => ContextRef context -> FrontCheck sig -> EGraphFrontM sig analysis context (FrontOutput owner sig analysis context Bool)
checkAt contextRef =
  pure . checkOutput (frontName @name) (Just contextRef)

extract :: forall name owner sig analysis context cost sort. (KnownSymbol name, RewriteSignature sig, Ord (NodeTag sig), Ord context, Ord cost) => AnalysisCostAlgebra (Node sig) analysis cost -> TermRef sig sort -> EGraphFrontM sig analysis context (FrontOutput owner sig analysis context (Maybe (ExtractionResult (Node sig) cost)))
extract =
  extractOutput (frontName @name) Nothing

extractAt :: forall name owner sig analysis context cost sort. (KnownSymbol name, RewriteSignature sig, Ord (NodeTag sig), Ord context, Ord cost) => ContextRef context -> AnalysisCostAlgebra (Node sig) analysis cost -> TermRef sig sort -> EGraphFrontM sig analysis context (FrontOutput owner sig analysis context (Maybe (ExtractionResult (Node sig) cost)))
extractAt contextRef =
  extractOutput (frontName @name) (Just contextRef)

atContext :: ContextRef context -> DSLRule.RuleBody sig atom -> DSLRule.RuleBody sig atom
atContext contextRef =
  DSLRule.at (frontNameInputRaw (contextRefName contextRef))

compileEGraphFront ::
  forall owner sig analysis context result.
  ( RewriteSignature sig,
    ZipMatch (Node sig),
    Ord (NodeTag sig),
    Show (NodeTag sig),
    Ord analysis,
    JoinSemilattice analysis,
    Ord context
  ) =>
  EGraphFront 'Authored owner sig analysis context result ->
  Either (EGraphFrontError owner sig analysis context) (EGraphFront 'Compiled owner sig analysis context result)
compileEGraphFront (AuthoredFront frontProgramValue authoredResult) = do
  validateFrontProgram frontProgramValue
  contextsByName <-
    contextRegistry frontProgramValue
  rewriteProgram <-
    rewriteDslProgram frontProgramValue
  (canonicalProgramValue, rulePlanSet) <-
    first EGraphFrontProgramError $
      compileProgramRuleSet rewriteProgram
  packedRulePlanSet <-
    first EGraphFrontPackedPlanInvalid $
      packRulePlanSet rulePlanSet
  rulesetRules <-
    traverseRulesetRuleNames frontProgramValue
  factRules <-
    traverseRulesetFactRules frontProgramValue
  let compiledWithoutPrograms =
        CompiledFrontState
          { cfCanonical = canonicalProgramValue,
            cfRulePlans = packedRulePlanSet,
            cfRulesets = rulesetRules,
            cfFactRulesets = factRules,
            cfContexts = contextsByName,
            cfSeedSlots = fmap seedSlotFromDecl (fpSeeds frontProgramValue),
            cfSeedFacts = fpFacts frontProgramValue,
            cfSchedules = fpSchedules frontProgramValue,
            cfRulesetPrograms = Map.empty,
            cfRulesetPlans = Map.empty
          }
  schedulePlanKeys <-
    compiledFrontSchedulePlanKeys (fpSchedules frontProgramValue)
  rulesetPrograms <-
    traverse
      (compileProgramForRulesetName compiledWithoutPrograms)
      (Map.keys rulesetRules)
  let compiledWithPrograms =
        compiledWithoutPrograms
          { cfRulesetPrograms = Map.fromList (zip (Map.keys rulesetRules) rulesetPrograms)
          }
  rulesetPlans <-
    compileRulesetPlans compiledWithPrograms schedulePlanKeys
  pure
    ( CompiledFront
        compiledWithPrograms
          { cfRulesetPlans = rulesetPlans
          }
        authoredResult
    )

prepareFrontRun ::
  forall owner sig analysis context result.
  ( RewriteSignature sig,
    ZipMatch (Node sig),
    Ord (NodeTag sig),
    Show (NodeTag sig),
    Ord analysis,
    JoinSemilattice analysis,
    Ord context
  ) =>
  EGraphFront 'Authored owner sig analysis context result ->
  SaturatingContextEGraph owner SurfaceKind (PackedNode sig) analysis context ->
  Either (EGraphFrontError owner sig analysis context) (PreparedFrontRun owner sig analysis context result)
prepareFrontRun authoredFront initialGraph = do
  let seedTerms =
        authoredFrontSeedTerms authoredFront
  compiledFront <- compileEGraphFront authoredFront
  prepareCompiledFrontRun compiledFront initialGraph seedTerms

prepareCompiledFrontRun ::
  forall owner sig analysis context result.
  ( RewriteSignature sig,
    Ord (NodeTag sig),
    Ord context
  ) =>
  EGraphFront 'Compiled owner sig analysis context result ->
  SaturatingContextEGraph owner SurfaceKind (PackedNode sig) analysis context ->
  [FrontSeedTerm sig] ->
  Either (EGraphFrontError owner sig analysis context) (PreparedFrontRun owner sig analysis context result)
prepareCompiledFrontRun compiledFront initialGraph seedTerms = do
  let (compiled, authoredOutput) =
        compiledFrontValue compiledFront
  seeded <-
    stageCompiledFrontSeeds
      authoredOutput
      initialGraph
      compiled
      seedTerms
  pure
    PreparedFrontRun
      { pfrCompiled = compiled,
        pfrSeeded = seeded
      }

frontReportFromRun ::
  forall owner sig analysis context result.
  SeededFrontState owner sig analysis context result ->
  SaturatingContextEGraph owner SurfaceKind (PackedNode sig) analysis context ->
  [EGraphLogicReport owner SurfaceKind (PackedNode sig) analysis context] ->
  Either (EGraphFrontError owner sig analysis context) (EGraphFrontReport owner sig analysis context result)
frontReportFromRun seeded finalGraph scheduleReports = do
  result <-
    resolveFrontOutput
      (sfsOutput seeded)
      finalGraph
  pure
    EGraphFrontReport
      { efrResult = result,
        efrFinalGraph = finalGraph,
        efrScheduleReports = scheduleReports,
        efrSeedClasses = sfsSeeds seeded,
        efrSeedTrace = sfsTrace seeded
      }

initialScheduleRunState ::
  timing ->
  SeededFrontState owner sig analysis context result ->
  ScheduleRunState owner timing sig analysis context
initialScheduleRunState timings seeded =
  ScheduleRunState
    { srsGraph = sfsGraph seeded,
      srsFactStore = sfsFactStore seeded,
      srsReports = [],
      srsTimings = timings
    }

runPreparedFrontWith ::
  Monad m =>
  timing ->
  ( CompiledFrontState owner sig analysis context ->
    ScheduleRunState owner timing sig analysis context ->
    FrontRuntimeSchedulePhase owner sig analysis context ->
    m (Either (EGraphFrontError owner sig analysis context) (ScheduleRunState owner timing sig analysis context))
  ) ->
  (EGraphFrontReport owner sig analysis context result -> ScheduleRunState owner timing sig analysis context -> output) ->
  PreparedFrontRun owner sig analysis context result ->
  m (Either (EGraphFrontError owner sig analysis context) output)
runPreparedFrontWith initialTimings runPhase decorate PreparedFrontRun {pfrCompiled = compiled, pfrSeeded = seeded} = do
  scheduleRunResult <-
    foldFrontSchedulesWith
      (runFrontScheduleWith (runPhase compiled))
      (initialScheduleRunState initialTimings seeded)
      (sfsSchedules seeded)
  pure $ do
    scheduleRun <- scheduleRunResult
    report <-
      frontReportFromRun seeded (srsGraph scheduleRun) (scheduleRunReports scheduleRun)
    pure (decorate report scheduleRun)

runPreparedFront ::
  forall owner sig analysis context result.
  ( RewriteSignature sig,
    Ord (NodeTag sig),
    Show (NodeTag sig),
    Ord analysis,
    JoinSemilattice analysis,
    Ord context
  ) =>
  PreparedFrontRun owner sig analysis context result ->
  Either (EGraphFrontError owner sig analysis context) (EGraphFrontReport owner sig analysis context result)
runPreparedFront =
  runIdentity
    . runPreparedFrontWith
      ()
      ( \compiled phaseState phaseValue ->
          Identity (runSchedulePhase compiled phaseState phaseValue)
      )
      (\report _scheduleRun -> report)

runPreparedFrontObserved ::
  forall owner sig analysis context result.
  ( RewriteSignature sig,
    Ord (NodeTag sig),
    Show (NodeTag sig),
    Ord analysis,
    JoinSemilattice analysis,
    Ord context
  ) =>
  PreparedFrontRun owner sig analysis context result ->
  IO (Either (EGraphFrontError owner sig analysis context) (EGraphFrontObservedReport owner sig analysis context result))
runPreparedFrontObserved =
  runPreparedFrontWith
    []
    runSchedulePhaseObserved
    ( \report scheduleRun ->
        EGraphFrontObservedReport
          { eforReport = report,
            eforScheduleTimings = reverse (srsTimings scheduleRun)
          }
    )

runEGraphFront ::
  forall owner sig analysis context result.
  ( RewriteSignature sig,
    ZipMatch (Node sig),
    Ord (NodeTag sig),
    Show (NodeTag sig),
    Ord analysis,
    JoinSemilattice analysis,
    Ord context
  ) =>
  EGraphFront 'Authored owner sig analysis context result ->
  SaturatingContextEGraph owner SurfaceKind (PackedNode sig) analysis context ->
  Either (EGraphFrontError owner sig analysis context) (EGraphFrontReport owner sig analysis context result)
runEGraphFront authoredFront initialGraph =
  prepareFrontRun authoredFront initialGraph >>= runPreparedFront

runCompiledEGraphFront ::
  forall owner sig analysis context result.
  ( RewriteSignature sig,
    Ord (NodeTag sig),
    Show (NodeTag sig),
    Ord analysis,
    JoinSemilattice analysis,
    Ord context
  ) =>
  CompiledEGraphFront owner sig analysis context result ->
  SaturatingContextEGraph owner SurfaceKind (PackedNode sig) analysis context ->
  [FrontSeedTerm sig] ->
  Either (EGraphFrontError owner sig analysis context) (EGraphFrontReport owner sig analysis context result)
runCompiledEGraphFront compiledFront initialGraph seedTerms =
  prepareCompiledFrontRun compiledFront initialGraph seedTerms >>= runPreparedFront

runEGraphFrontObserved ::
  forall owner sig analysis context result.
  ( RewriteSignature sig,
    ZipMatch (Node sig),
    Ord (NodeTag sig),
    Show (NodeTag sig),
    Ord analysis,
    JoinSemilattice analysis,
    Ord context
  ) =>
  EGraphFront 'Authored owner sig analysis context result ->
  SaturatingContextEGraph owner SurfaceKind (PackedNode sig) analysis context ->
  IO (Either (EGraphFrontError owner sig analysis context) (EGraphFrontObservedReport owner sig analysis context result))
runEGraphFrontObserved authoredFront initialGraph =
  either
    (pure . Left)
    runPreparedFrontObserved
    (prepareFrontRun authoredFront initialGraph)

runCompiledEGraphFrontObserved ::
  forall owner sig analysis context result.
  ( RewriteSignature sig,
    Ord (NodeTag sig),
    Show (NodeTag sig),
    Ord analysis,
    JoinSemilattice analysis,
    Ord context
  ) =>
  CompiledEGraphFront owner sig analysis context result ->
  SaturatingContextEGraph owner SurfaceKind (PackedNode sig) analysis context ->
  [FrontSeedTerm sig] ->
  IO (Either (EGraphFrontError owner sig analysis context) (EGraphFrontObservedReport owner sig analysis context result))
runCompiledEGraphFrontObserved compiledFront initialGraph seedTerms =
  either
    (pure . Left)
    runPreparedFrontObserved
    (prepareCompiledFrontRun compiledFront initialGraph seedTerms)

frontErrorMessage ::
  (RewriteSignature sig, Show (NodeTag sig)) =>
  EGraphFrontError owner sig analysis context ->
  String
frontErrorMessage =
  \case
    EGraphFrontProgramError programError ->
      prettyProgramError programError
    EGraphFrontInvalidContextName rawName _ ->
      "invalid front context name: " <> show rawName
    EGraphFrontInvalidRulesetName rawName _ ->
      "invalid front ruleset name: " <> show rawName
    EGraphFrontInvalidSeedName rawName _ ->
      "invalid front seed name: " <> show rawName
    EGraphFrontInvalidRelationName rawName _ ->
      "invalid front relation name: " <> show rawName
    EGraphFrontDuplicateContext contextNameValue ->
      "duplicate front context: " <> show (DSLRule.contextNameString contextNameValue)
    EGraphFrontDuplicateRuleset rulesetName ->
      "duplicate front ruleset: " <> show (frontRulesetNameString rulesetName)
    EGraphFrontDuplicateSeed seedName ->
      "duplicate front seed: " <> show (frontSeedNameString seedName)
    EGraphFrontDuplicateRelation relationName ->
      "duplicate front relation: " <> show (frontRelationNameString relationName)
    EGraphFrontInvalidRuleName rawName ->
      "invalid front rule name: " <> show rawName
    EGraphFrontUnknownRuleset rulesetName ->
      "unknown front ruleset: " <> show (frontRulesetNameString rulesetName)
    EGraphFrontUnknownSeed seedName ->
      "unknown front seed: " <> show (frontSeedNameString seedName)
    EGraphFrontMissingCompiledRule ruleNameValue ->
      "compiled rewrite rule missing from plan set: " <> show (ruleNameString ruleNameValue)
    EGraphFrontMissingCompiledPlan rulesetName budget ->
      "compiled front plan missing for ruleset " <> show (frontRulesetNameString rulesetName) <> " and budget " <> show budget
    EGraphFrontUnknownCompiledContext contextNameValue ->
      "compiled rewrite context missing from front registry: " <> show (DSLRule.contextNameString contextNameValue)
    EGraphFrontFactCompileError _ ->
      "front fact rule failed to compile"
    EGraphFrontPackedPlanInvalid packedPlanError ->
      "front packed plan is invalid: " <> show packedPlanError
    EGraphFrontGroundTermContainsVariable name sortName ->
      "front ground term contains variable " <> show name <> " at sort " <> show sortName
    EGraphFrontInternalVariableClosureMiss name sortName ->
      "front variable closure missed " <> show name <> " at sort " <> show sortName
    EGraphFrontContextDeltaError _ ->
      "front context staging failed"
    EGraphFrontLogicError _ ->
      "front schedule runtime failed"
    EGraphFrontScheduleChoiceFailed ->
      "front schedule choice exhausted all branches"

compiledFrontValue :: EGraphFront 'Compiled owner sig analysis context result -> (CompiledFrontState owner sig analysis context, FrontOutput owner sig analysis context result)
compiledFrontValue (CompiledFront compiled authoredResult) =
  (compiled, authoredResult)

authoredFrontSeedTerms :: EGraphFront 'Authored owner sig analysis context result -> [FrontSeedTerm sig]
authoredFrontSeedTerms (AuthoredFront frontProgramValue _) =
  fmap seedTermFromDecl (fpSeeds frontProgramValue)

seedTermFromDecl :: SeedDecl sig context -> FrontSeedTerm sig
seedTermFromDecl (SeedDecl nameInput _ termValue) =
  FrontSeedTerm nameInput termValue

seedSlotFromDecl :: SeedDecl sig context -> FrontSeedSlot context
seedSlotFromDecl (SeedDecl nameInput maybeContext _) =
  FrontSeedSlot
    { fssName = nameInput,
      fssContext = maybeContext
    }

seedWithContext :: String -> Maybe (ContextRef context) -> Term sig sort -> EGraphFrontM sig analysis context (TermRef sig sort)
seedWithContext rawName maybeContext termValue =
  EGraphFrontM
    ( \builder ->
        let nameInput =
              frontNameInput mkFrontSeedName rawName
            ref = TermRef nameInput
         in ( ref,
              builder
                { fbSeeds = SeedDecl nameInput maybeContext termValue : fbSeeds builder
                }
            )
    )

checkOutput :: (RewriteSignature sig, Ord (NodeTag sig), Ord context) => String -> Maybe (ContextRef context) -> FrontCheck sig -> FrontOutput owner sig analysis context Bool
checkOutput rawName maybeContext =
  \case
    FrontCheckEq termRef termValue ->
      FrontOutput $ \state -> do
        leftClass <- seedClassFor rawName termRef state
        (rightClass, nextBatch) <- stageObservationTerm maybeContext termValue (ossBatch state)
        let observation =
              case maybeContext of
                Nothing -> CheckEquivalentBase leftClass rightClass
                Just contextRef -> CheckEquivalentAt (contextRefValue contextRef) leftClass rightClass
            resolved =
              FrontResolvedOutput $
                first (EGraphFrontLogicError . EGraphLogicObservationError)
                  . runStableObservation observation
        pure
          ( state
              { ossBatch = nextBatch
              },
            resolved
          )

extractOutput ::
  (RewriteSignature sig, Ord (NodeTag sig), Ord context, Ord cost) =>
  String ->
  Maybe (ContextRef context) ->
  AnalysisCostAlgebra (Node sig) analysis cost ->
  TermRef sig sort ->
  EGraphFrontM sig analysis context (FrontOutput owner sig analysis context (Maybe (ExtractionResult (Node sig) cost)))
extractOutput rawName maybeContext costAlgebra termRef =
  pure $
    FrontOutput $ \state -> do
      classId <- seedClassFor rawName termRef state
      let observation =
            case maybeContext of
              Nothing -> ExtractBase (packAnalysisCostAlgebra costAlgebra) classId
              Just contextRef -> ExtractAt (contextRefValue contextRef) (packAnalysisCostAlgebra costAlgebra) classId
          resolved =
            FrontResolvedOutput $
              fmap (fmap unpackExtractionResult)
                .
              first (EGraphFrontLogicError . EGraphLogicObservationError)
                . runStableObservation observation
      pure (state, resolved)

unpackContextDeltaError ::
  ContextDeltaError (PackedNode sig) context ->
  ContextDeltaError (Node sig) context
unpackContextDeltaError =
  \case
    ContextClassIdAllocationFailed allocationError ->
      ContextClassIdAllocationFailed allocationError
    ContextSupportSiteFailed err ->
      ContextSupportSiteFailed err
    ContextLocalUnionCanonicalizationFailed err ->
      ContextLocalUnionCanonicalizationFailed err
    ContextRegionalClosureFailed err ->
      ContextRegionalClosureFailed err
    ContextConstructionAfterMerge ->
      ContextConstructionAfterMerge

unpackContextMutationTrace ::
  (RewriteSignature sig, Ord (NodeTag sig)) =>
  ContextMutationTrace owner context (PackedNode sig) ->
  ContextMutationTrace owner context (Node sig)
unpackContextMutationTrace traceValue =
  ContextMutationTrace
    { cmtBaseTrace = unpackEGraphMutationTrace (cmtBaseTrace traceValue),
      cmtContextTouchedKeys = cmtContextTouchedKeys traceValue,
      cmtDirtyContexts = cmtDirtyContexts traceValue,
      cmtObservedLocalUnions = cmtObservedLocalUnions traceValue,
      cmtObservedLocalUnionsByContext = cmtObservedLocalUnionsByContext traceValue,
      cmtSupportDelta = cmtSupportDelta traceValue
    }

unpackEGraphMutationTrace ::
  (RewriteSignature sig, Ord (NodeTag sig)) =>
  EGraphMutationTrace (PackedNode sig) ->
  EGraphMutationTrace (Node sig)
unpackEGraphMutationTrace traceValue =
  EGraphMutationTrace
    { emtRevisionBefore = emtRevisionBefore traceValue,
      emtRevisionAfter = emtRevisionAfter traceValue,
      emtPhaseBefore = emtPhaseBefore traceValue,
      emtPhaseAfter = emtPhaseAfter traceValue,
      emtTouchedClassKeys = emtTouchedClassKeys traceValue,
      emtInsertedClassKeys = emtInsertedClassKeys traceValue,
      emtAnalysisChangedKeys = emtAnalysisChangedKeys traceValue,
      emtObservedClassUnions = emtObservedClassUnions traceValue,
      emtRebuildTraces = fmap unpackEGraphRebuildTrace (emtRebuildTraces traceValue)
    }

unpackEGraphRebuildTrace ::
  (RewriteSignature sig, Ord (NodeTag sig)) =>
  EGraphRebuildTrace (PackedNode sig) ->
  EGraphRebuildTrace (Node sig)
unpackEGraphRebuildTrace traceValue =
  EGraphRebuildTrace
    { egrtRebuildDelta = egrtRebuildDelta traceValue,
      egrtTuplePatch = unpackStructuralTuplePatch (egrtTuplePatch traceValue)
    }

unpackStructuralTuplePatch ::
  (RewriteSignature sig, Ord (NodeTag sig)) =>
  StructuralTuplePatch (PackedNode sig) ->
  StructuralTuplePatch (Node sig)
unpackStructuralTuplePatch patchValue =
  StructuralTuplePatch
    { stpRemoved = fmap unpackENodeSet (stpRemoved patchValue),
      stpInserted = fmap unpackENodeSet (stpInserted patchValue)
    }

unpackENodeSet ::
  (RewriteSignature sig, Ord (NodeTag sig)) =>
  Set (ENode (PackedNode sig)) ->
  Set (ENode (Node sig))
unpackENodeSet =
  Set.map unpackENode

unpackENode ::
  ENode (PackedNode sig) ->
  ENode (Node sig)
unpackENode (ENode packed) =
  ENode (packedNode packed)

appendRuleDecl :: RuleDecl sig -> RulesetM sig ()
appendRuleDecl ruleDecl =
  RulesetM
    ( \builder ->
        ( (),
          builder
            { rbRules = ruleDecl : rbRules builder
            }
        )
    )

emptyFrontBuilder :: FrontBuilder sig analysis context
emptyFrontBuilder =
  FrontBuilder
    { fbContexts = [],
      fbRulesets = [],
      fbRelations = [],
      fbSeeds = [],
      fbFacts = [],
      fbSchedules = [],
      fbNextRelationId = 0
    }

emptyRulesetBuilder :: RulesetBuilder sig
emptyRulesetBuilder =
  RulesetBuilder
    { rbRules = []
    }

frontProgramFromBuilder :: FrontBuilder sig analysis context -> FrontProgram sig analysis context
frontProgramFromBuilder builder =
  FrontProgram
    { fpContexts = reverse (fbContexts builder),
      fpRulesets = reverse (fbRulesets builder),
      fpRelations = reverse (fbRelations builder),
      fpSeeds = reverse (fbSeeds builder),
      fpFacts = reverse (fbFacts builder),
      fpSchedules = reverse (fbSchedules builder)
    }

contextRegistry :: FrontProgram sig analysis context -> Either (EGraphFrontError owner sig analysis context) (Map DSLRule.ContextName context)
contextRegistry frontProgramValue =
  fmap Map.fromList $
    traverse
      ( \contextDecl -> do
          contextNameValue <- contextDeclName contextDecl
          pure (contextNameValue, cdContextValue contextDecl)
      )
      (fpContexts frontProgramValue)

validateFrontProgram :: FrontProgram sig analysis context -> Either (EGraphFrontError owner sig analysis context) ()
validateFrontProgram frontProgramValue =
  rejectDuplicateName EGraphFrontDuplicateContext contextDeclName (fpContexts frontProgramValue)
    *> rejectDuplicateName EGraphFrontDuplicateRuleset rulesetDeclName (fpRulesets frontProgramValue)
    *> rejectDuplicateName EGraphFrontDuplicateSeed seedDeclName (fpSeeds frontProgramValue)
    *> rejectDuplicateName EGraphFrontDuplicateRelation relationDeclName (fpRelations frontProgramValue)
    *> traverse_ validateRulesetRuleDecls (fpRulesets frontProgramValue)

rejectDuplicateName ::
  Ord name =>
  (name -> EGraphFrontError owner sig analysis context) ->
  (value -> Either (EGraphFrontError owner sig analysis context) name) ->
  [value] ->
  Either (EGraphFrontError owner sig analysis context) ()
rejectDuplicateName makeError nameOf values = do
  names <- traverse nameOf values
  maybe
    (Right ())
    (Left . makeError)
    (firstDuplicateName names)

firstDuplicateName :: Ord name => [name] -> Maybe name
firstDuplicateName =
  fst
    . foldl'
      ( \(duplicate, seen) name ->
          case duplicate of
            Just _ ->
              (duplicate, seen)
            Nothing ->
              if Set.member name seen
                then (Just name, seen)
                else (Nothing, Set.insert name seen)
      )
      (Nothing, Set.empty)

contextDeclName :: ContextDecl context -> Either (EGraphFrontError owner sig analysis context) DSLRule.ContextName
contextDeclName =
  requireContextName . cdFrontName

rulesetDeclName :: RulesetDecl sig -> Either (EGraphFrontError owner sig analysis context) FrontRulesetName
rulesetDeclName =
  requireRulesetName . rsdName

seedDeclName :: SeedDecl sig context -> Either (EGraphFrontError owner sig analysis context) FrontSeedName
seedDeclName (SeedDecl nameInput _ _) =
  requireSeedName nameInput

relationDeclName :: SomeRelationDecl -> Either (EGraphFrontError owner sig analysis context) FrontRelationName
relationDeclName (SomeRelationDecl relationRef) =
  requireRelationName (relationRefName relationRef)

validateRulesetRuleDecls :: RulesetDecl sig -> Either (EGraphFrontError owner sig analysis context) ()
validateRulesetRuleDecls rulesetDecl =
  traverse_ ruleDeclName (rsdRules rulesetDecl)

requireContextName :: FrontNameInput DSLRule.ContextName DSLRule.ContextNameError -> Either (EGraphFrontError owner sig analysis context) DSLRule.ContextName
requireContextName =
  requireFrontNameInput EGraphFrontInvalidContextName

requireRulesetName :: FrontNameInput FrontRulesetName FrontNameError -> Either (EGraphFrontError owner sig analysis context) FrontRulesetName
requireRulesetName =
  requireFrontNameInput EGraphFrontInvalidRulesetName

requireSeedName :: FrontNameInput FrontSeedName FrontNameError -> Either (EGraphFrontError owner sig analysis context) FrontSeedName
requireSeedName =
  requireFrontNameInput EGraphFrontInvalidSeedName

requireRelationName :: FrontNameInput FrontRelationName FrontNameError -> Either (EGraphFrontError owner sig analysis context) FrontRelationName
requireRelationName =
  requireFrontNameInput EGraphFrontInvalidRelationName

requireRuleName :: FrontNameInput RuleName RuleNameError -> Either (EGraphFrontError owner sig analysis context) RuleName
requireRuleName input =
  first (const (EGraphFrontInvalidRuleName (frontNameInputRaw input))) (frontNameInputParsed input)

rewriteDslProgram :: forall owner sig analysis context. FrontProgram sig analysis context -> Either (EGraphFrontError owner sig analysis context) (DSLProgram.Program sig FrontGuardAtom)
rewriteDslProgram frontProgramValue = do
  contextNames <-
    traverse contextDeclName (fpContexts frontProgramValue)
  ruleDecls <-
    traverse rulesetDslRuleDecls (fpRulesets frontProgramValue)
  pure $
    DSLProgram.program $
      traverse_ (DSLProgram.context . DSLRule.contextNameString) contextNames
        *> traverse_ (traverse_ emitRuleDecl) ruleDecls
  where
    rulesetDslRuleDecls :: RulesetDecl sig -> Either (EGraphFrontError owner sig analysis context) [(RuleName, DSLRule.RuleBody sig FrontGuardAtom)]
    rulesetDslRuleDecls rulesetDecl =
      fold <$> traverse rewriteDeclForDsl (rsdRules rulesetDecl)

    rewriteDeclForDsl :: RuleDecl sig -> Either (EGraphFrontError owner sig analysis context) [(RuleName, DSLRule.RuleBody sig FrontGuardAtom)]
    rewriteDeclForDsl =
      \case
        RewriteDecl nameInput body ->
          fmap (\ruleNameValue -> [(ruleNameValue, body)]) (requireRuleName nameInput)
        FactRuleDecl {} ->
          Right []

    emitRuleDecl :: (RuleName, DSLRule.RuleBody sig FrontGuardAtom) -> DSLProgram.ProgramM sig FrontGuardAtom ()
    emitRuleDecl (ruleNameValue, body) =
      DSLProgram.rule (ruleNameString ruleNameValue) body

traverseRulesetRuleNames :: forall owner sig analysis context. FrontProgram sig analysis context -> Either (EGraphFrontError owner sig analysis context) (Map FrontRulesetName [RuleName])
traverseRulesetRuleNames frontProgramValue =
  fmap Map.fromList $
    traverse
      ( \rulesetDecl -> do
          rulesetName <- rulesetDeclName rulesetDecl
          ruleNames <- traverse ruleDeclName (rewriteRuleDecls (rsdRules rulesetDecl))
          pure (rulesetName, ruleNames)
      )
      (fpRulesets frontProgramValue)
  where
    rewriteRuleDecls :: [RuleDecl sig] -> [RuleDecl sig]
    rewriteRuleDecls =
      foldMap
        ( \case
            rewriteDecl@RewriteDecl {} -> [rewriteDecl]
            FactRuleDecl {} -> []
        )

ruleDeclName :: RuleDecl sig -> Either (EGraphFrontError owner sig analysis context) RuleName
ruleDeclName =
  \case
    RewriteDecl nameInput _ ->
      requireRuleName nameInput
    FactRuleDecl nameInput _ _ ->
      requireRuleName nameInput

traverseRulesetFactRules ::
  forall owner sig analysis context.
  (RewriteSignature sig, Ord (NodeTag sig)) =>
  FrontProgram sig analysis context ->
  Either (EGraphFrontError owner sig analysis context) (Map FrontRulesetName [CompiledFactRule SurfaceKind (PackedNode sig)])
traverseRulesetFactRules frontProgramValue =
  fmap Map.fromList $
    traverse
      ( \rulesetDecl -> do
          rulesetName <- rulesetDeclName rulesetDecl
          factRules <- compileRulesetFacts rulesetName rulesetDecl
          pure (rulesetName, factRules)
      )
      (fpRulesets frontProgramValue)

compileRulesetFacts ::
  forall owner sig analysis context.
  (RewriteSignature sig, Ord (NodeTag sig)) =>
  FrontRulesetName ->
  RulesetDecl sig ->
  Either (EGraphFrontError owner sig analysis context) [CompiledFactRule SurfaceKind (PackedNode sig)]
compileRulesetFacts rulesetName rulesetDecl = do
  factSources <-
    traverse
      (factRuleSource rulesetName)
      (zip [0 ..] (rsdRules rulesetDecl))
  compiledFactRules <-
    first EGraphFrontFactCompileError $
      compileFactRules (fold factSources)
  first EGraphFrontPackedPlanInvalid $
    traverse packCompiledFactRule compiledFactRules
  where
    factRuleSource :: FrontRulesetName -> (Int, RuleDecl sig) -> Either (EGraphFrontError owner sig analysis context) [FactRule SurfaceKind (Node sig)]
    factRuleSource factRulesetName (offset, ruleDecl) =
      case ruleDecl of
        RewriteDecl {} ->
          Right []
        FactRuleDecl nameInput factId (SomeTerm termValue) -> do
          ruleNameValue <- requireRuleName nameInput
          fmap
            ( \patternValue ->
                [ FactRule
                    { frId = FactRuleId (100000 + offset),
                      frName = frontRulesetNameString factRulesetName <> "." <> ruleNameString ruleNameValue,
                      frPattern = patternValue,
                      frProjection = [GuardRoot],
                      frFactId = factId,
                      frCondition = Nothing
                    }
                ]
            )
            (termPattern termValue)

termPattern :: RewriteSignature sig => Term sig sort -> Either (EGraphFrontError owner sig analysis context) (Pattern (Node sig))
termPattern termValue =
  lowerWithVars variableMap termValue
  where
    variableMap =
      Map.fromList (zip (Set.toAscList (termVariables termValue)) (fmap EGraph.mkPatternVar [0 ..]))

    lowerWithVars :: RewriteSignature sig => Map SomeTypedVar EGraph.PatternVar -> Term sig sort' -> Either (EGraphFrontError owner sig analysis context) (Pattern (Node sig))
    lowerWithVars vars =
      \case
        TVar typedVariable ->
          maybe
            ( Left
                ( EGraphFrontInternalVariableClosureMiss
                    (someTypedVarName (SomeTypedVar typedVariable))
                    (sortNameString (someTypedVarSort (SomeTypedVar typedVariable)))
                )
            )
            (Right . PatternVar)
            (Map.lookup (SomeTypedVar typedVariable) vars)
        TNode sigNode ->
          PatternNode . Node
            <$> htraverse
              (fmap K . lowerWithVars vars)
              sigNode

compiledFrontSchedulePlanKeys ::
  forall owner sig analysis context.
  [FrontSchedule sig analysis context] ->
  Either (EGraphFrontError owner sig analysis context) (Set FrontPlanKey)
compiledFrontSchedulePlanKeys schedules =
  fold <$> traverse frontSchedulePlanKeys schedules

frontSchedulePlanKeys ::
  forall owner sig analysis context.
  FrontSchedule sig analysis context ->
  Either (EGraphFrontError owner sig analysis context) (Set FrontPlanKey)
frontSchedulePlanKeys schedule =
  foldProgram
    ProgramAlgebra
      { paSkip = Right Set.empty,
        paPhase = \phaseValue -> do
          rulesetName <-
            requireRulesetName (rulesetRefName (fspRuleset phaseValue))
          pure $
            Set.singleton
              FrontPlanKey
                { fpkRuleset = rulesetName,
                  fpkBudget = fspBudget phaseValue
                },
        paSeq = \leftKeys rightKeys ->
          Set.union <$> leftKeys <*> rightKeys,
        paOr = \leftKeys rightKeys ->
          Set.union <$> leftKeys <*> rightKeys,
        paUpTo = \_repeatCount keys ->
          keys,
        paAttempt = id,
        paScoped = const id
      }
    (frontScheduleTree schedule)

frontPlanSpec ::
  forall owner sig analysis context.
  ( RewriteSignature sig,
    Ord (NodeTag sig),
    Show (NodeTag sig),
    Ord analysis,
    JoinSemilattice analysis,
    Ord context
  ) =>
  SaturationBudget ->
  PlanSpec
    (EGraphU owner SurfaceKind (PackedNode sig) analysis context)
    (SaturatingContextEGraph owner SurfaceKind (PackedNode sig) analysis context)
    RewriteRuleId
frontPlanSpec budget =
  defaultPlanSpec @(EGraphU owner SurfaceKind (PackedNode sig) analysis context) @RewriteRuleId
    budget
    GenericJoinMatching

compileRulesetPlans ::
  forall owner sig analysis context.
  ( RewriteSignature sig,
    Ord (NodeTag sig),
    Show (NodeTag sig),
    Ord analysis,
    JoinSemilattice analysis,
    Ord context
  ) =>
  CompiledFrontState owner sig analysis context ->
  Set FrontPlanKey ->
  Either
    (EGraphFrontError owner sig analysis context)
    (Map FrontRulesetName (Map SaturationBudget (FrontCompiledPlan owner sig analysis context)))
compileRulesetPlans compiled keys =
  Map.fromListWith Map.union <$> traverse compileRulesetPlan (Set.toAscList keys)
  where
    compileRulesetPlan ::
      FrontPlanKey ->
      Either
        (EGraphFrontError owner sig analysis context)
        (FrontRulesetName, Map SaturationBudget (FrontCompiledPlan owner sig analysis context))
    compileRulesetPlan key = do
      compiledProgram <- compiledRulesetProgram compiled (fpkRuleset key)
      pure
        ( fpkRuleset key,
          Map.singleton
            (fpkBudget key)
            (mkPlan (frontPlanSpec @owner @sig @analysis @context (fpkBudget key)) compiledProgram)
        )

compileProgramForRulesetName ::
  forall owner sig analysis context.
  Ord context =>
  CompiledFrontState owner sig analysis context ->
  FrontRulesetName ->
  Either (EGraphFrontError owner sig analysis context) (FrontCompiledProgram owner sig analysis context)
compileProgramForRulesetName compiled rulesetName = do
  selectedRules <-
    maybe
      (Left (EGraphFrontUnknownRuleset rulesetName))
      Right
      (Map.lookup rulesetName (cfRulesets compiled))
  selectedFacts <-
    maybe
      (Left (EGraphFrontUnknownRuleset rulesetName))
      Right
      (Map.lookup rulesetName (cfFactRulesets compiled))
  let selectedSet = Set.fromList selectedRules
      supportIndex = canonicalSupportIndex (cfCanonical compiled)
      lookupPlan ruleNameValue = lookupRulePlan ruleNameValue (cfRulePlans compiled)
  baseRules <-
    selectRulePlans lookupPlan selectedRules (Set.intersection selectedSet (baseRuleNames supportIndex))
  contextRules <-
    traverseContextRules lookupPlan selectedRules supportIndex (cfContexts compiled)
  pure
    SiteProgram
      { spFactRules =
          SiteIndex
            { siBase = selectedFacts,
              siContexts = Map.empty
            },
        spRewriteRules =
          SiteIndex
            { siBase = baseRules,
              siContexts = contextRules
            },
        spSupportedFactRules = [],
        spSupportedRewriteRules = Map.empty,
        spRewriteActivation =
          MatchActivationIndex
            { maiBase = Set.fromList (fmap rpId baseRules),
              maiContexts = Map.empty
            },
        spBaseRewriteSupport = Map.empty
      }

baseRuleNames :: RuleSupportIndex context -> Set RuleName
baseRuleNames =
  baseSupportRuleNames

traverseContextRules ::
  forall owner sig analysis context.
  Ord context =>
  (RuleName -> Maybe (SatRule (EGraphU owner SurfaceKind (PackedNode sig) analysis context))) ->
  [RuleName] ->
  RuleSupportIndex DSLRule.ContextName ->
  Map DSLRule.ContextName context ->
  Either (EGraphFrontError owner sig analysis context) (Map context [SatRule (EGraphU owner SurfaceKind (PackedNode sig) analysis context)])
traverseContextRules lookupPlan selectedRules supportIndex contextsByName =
  fmap Map.fromList $
    traverse
      ( \(contextNameValue, names) -> do
          contextValue <-
            maybe
              (Left (EGraphFrontUnknownCompiledContext contextNameValue))
              Right
              (Map.lookup contextNameValue contextsByName)
          rules <-
            selectRulePlans lookupPlan selectedRules (Set.intersection selectedSet names)
          pure (contextValue, rules)
      )
      (contextSupportEntries supportIndex)
  where
    selectedSet = Set.fromList selectedRules

selectRulePlans ::
  (RuleName -> Maybe (SatRule (EGraphU owner SurfaceKind (PackedNode sig) analysis context))) ->
  [RuleName] ->
  Set RuleName ->
  Either (EGraphFrontError owner sig analysis context) [SatRule (EGraphU owner SurfaceKind (PackedNode sig) analysis context)]
selectRulePlans lookupPlan selectedRules supportNames =
  traverse
    ( \ruleNameValue ->
        maybe
          (Left (EGraphFrontMissingCompiledRule ruleNameValue))
          Right
          (lookupPlan ruleNameValue)
    )
    (filter (`Set.member` supportNames) selectedRules)

compiledRulesetProgram ::
  CompiledFrontState owner sig analysis context ->
  FrontRulesetName ->
  Either (EGraphFrontError owner sig analysis context) (FrontCompiledProgram owner sig analysis context)
compiledRulesetProgram compiled rulesetName =
  maybe
    (Left (EGraphFrontUnknownRuleset rulesetName))
    Right
    (Map.lookup rulesetName (cfRulesetPrograms compiled))

compiledRulesetPlan ::
  CompiledFrontState owner sig analysis context ->
  FrontRulesetName ->
  SaturationBudget ->
  Either (EGraphFrontError owner sig analysis context) (FrontCompiledPlan owner sig analysis context)
compiledRulesetPlan compiled rulesetName budget =
  maybe
    (Left (EGraphFrontUnknownRuleset rulesetName))
    ( maybe
        (Left (EGraphFrontMissingCompiledPlan rulesetName budget))
        Right
        . Map.lookup budget
    )
    (Map.lookup rulesetName (cfRulesetPlans compiled))

runSchedulePhase ::
  forall owner sig analysis context.
  ( RewriteSignature sig,
    Ord (NodeTag sig),
    Show (NodeTag sig),
    Ord analysis,
    JoinSemilattice analysis,
    Ord context
  ) =>
  CompiledFrontState owner sig analysis context ->
  ScheduleRunState owner () sig analysis context ->
  FrontRuntimeSchedulePhase owner sig analysis context ->
  Either (EGraphFrontError owner sig analysis context) (ScheduleRunState owner () sig analysis context)
runSchedulePhase _compiled state phaseValue = do
  let plan =
        frspPlan phaseValue
      logicValue =
        logic (seedFacts BaseSite (srsFactStore state))
      planSpecValue =
        planPlanSpec plan
      runSpec = plainContextRunSpec planSpecValue (frspGoal phaseValue)
  report <-
    first EGraphFrontLogicError $
      runCompiledEGraphLogic
        (crsExecution runSpec)
        plan
        logicValue
        (srsGraph state)
  pure
    state
      { srsGraph = srCarrier (elrSaturation report),
        srsReports = report : srsReports state
      }

foldFrontSchedulesWith ::
  Monad m =>
  (state -> FrontRuntimeSchedule owner sig analysis context -> m (Either (EGraphFrontError owner sig analysis context) state)) ->
  state ->
  [FrontRuntimeSchedule owner sig analysis context] ->
  m (Either (EGraphFrontError owner sig analysis context) state)
foldFrontSchedulesWith runSchedule =
  foldM
    ( \stateResult schedule ->
        case stateResult of
          Left err ->
            pure (Left err)
          Right state ->
            runSchedule state schedule
    )
    . Right
{-# INLINE foldFrontSchedulesWith #-}

runFrontScheduleWith ::
  Monad m =>
  (state -> FrontRuntimeSchedulePhase owner sig analysis context -> m (Either (EGraphFrontError owner sig analysis context) state)) ->
  state ->
  FrontRuntimeSchedule owner sig analysis context ->
  m (Either (EGraphFrontError owner sig analysis context) state)
runFrontScheduleWith runPhase state schedule =
  runScheduleTree (frontRuntimeScheduleTree schedule) state
  where
    runScheduleTree =
      foldProgram
        ProgramAlgebra
          { paSkip = pure . Right,
            paPhase = \phaseValue phaseState ->
              runPhase phaseState phaseValue,
            paSeq = \leftAction rightAction startState ->
              leftAction startState >>= \case
                Left err ->
                  pure (Left err)
                Right nextState ->
                  rightAction nextState,
            paOr = \leftAction rightAction startState ->
              leftAction startState >>= \case
                Right nextState -> pure (Right nextState)
                Left _ -> rightAction startState,
            paUpTo = \repeatCount action startState ->
              foldM
                ( \stateResult _ ->
                    case stateResult of
                      Left err ->
                        pure (Left err)
                      Right nextState ->
                        action nextState
                )
                (Right startState)
                (genericReplicate repeatCount ()),
            paAttempt = \action startState ->
              action startState >>= \case
                Right nextState -> pure (Right nextState)
                Left _ -> pure (Right startState),
            paScoped = const id
          }
{-# INLINE runFrontScheduleWith #-}

runSchedulePhaseObserved ::
  forall owner sig analysis context.
  ( RewriteSignature sig,
    Ord (NodeTag sig),
    Show (NodeTag sig),
    Ord analysis,
    JoinSemilattice analysis,
    Ord context
  ) =>
  CompiledFrontState owner sig analysis context ->
  ScheduleRunState owner [RuntimeIOTiming] sig analysis context ->
  FrontRuntimeSchedulePhase owner sig analysis context ->
  IO (Either (EGraphFrontError owner sig analysis context) (ScheduleRunState owner [RuntimeIOTiming] sig analysis context))
runSchedulePhaseObserved _compiled state phaseValue = do
  let plan =
        frspPlan phaseValue
      logicValue =
        logic (seedFacts BaseSite (srsFactStore state))
      planSpecValue =
        planPlanSpec plan
      runSpec = plainContextRunSpec planSpecValue (frspGoal phaseValue)
  observedReport <-
    runCompiledEGraphLogicObserved
      (crsExecution runSpec)
      plan
      logicValue
      (srsGraph state)
  pure $ do
    reportValue <-
      first EGraphFrontLogicError observedReport
    let report =
          elorReport reportValue
    pure
      state
        { srsGraph = srCarrier (elrSaturation report),
          srsReports = report : srsReports state,
          srsTimings = elorTiming reportValue : srsTimings state
        }

data ScheduleRunState owner timing sig analysis context = ScheduleRunState
  { srsGraph :: !(SaturatingContextEGraph owner SurfaceKind (PackedNode sig) analysis context),
    srsFactStore :: !FactStore,
    srsReports :: ![EGraphLogicReport owner SurfaceKind (PackedNode sig) analysis context],
    srsTimings :: !timing
  }

scheduleRunReports ::
  ScheduleRunState owner timing sig analysis context ->
  [EGraphLogicReport owner SurfaceKind (PackedNode sig) analysis context]
scheduleRunReports =
  reverse . srsReports
{-# INLINE scheduleRunReports #-}

data SeededFrontState owner sig analysis context result = SeededFrontState
  { sfsGraph :: !(SaturatingContextEGraph owner SurfaceKind (PackedNode sig) analysis context),
    sfsSeeds :: !(Map FrontSeedName ClassId),
    sfsFactStore :: !FactStore,
    sfsSchedules :: ![FrontRuntimeSchedule owner sig analysis context],
    sfsOutput :: !(FrontResolvedOutput owner sig analysis context result),
    sfsTrace :: !(ContextMutationTrace owner context (Node sig))
  }

stageCompiledFrontSeeds ::
  forall owner sig analysis context result.
  (RewriteSignature sig, Ord (NodeTag sig), Ord context) =>
  FrontOutput owner sig analysis context result ->
  SaturatingContextEGraph owner SurfaceKind (PackedNode sig) analysis context ->
  CompiledFrontState owner sig analysis context ->
  [FrontSeedTerm sig] ->
  Either (EGraphFrontError owner sig analysis context) (SeededFrontState owner sig analysis context result)
stageCompiledFrontSeeds output graph compiled seedTerms = do
  runtimeSeeds <-
    runtimeSeedDecls compiled seedTerms
  stagedSeeds <-
    stageRuntimeSeedDecls
      (StagingState (beginContextRebaseBatch (sceContextGraph graph)) Map.empty)
      runtimeSeeds
  factStoreValue <-
    foldM
      (resolveSeedFact (stSeeds stagedSeeds))
      emptyFactStore
      (cfSeedFacts compiled)
  (scheduled, runtimeSchedules) <-
    stageFrontSchedules
      compiled
      (cfSchedules compiled)
      (ObservationSeedState (stBatch stagedSeeds) (stSeeds stagedSeeds))
  (staged, resolvedOutput) <-
    stageFrontOutput
      output
      scheduled
  (rebaseReport, contextGraph) <-
    first (EGraphFrontContextDeltaError . unpackContextDeltaError) $
      commitContextRebaseBatch (ossBatch staged)
  pure
    SeededFrontState
      { sfsGraph = emptySaturatingContextEGraph contextGraph,
        sfsSeeds = ossSeeds staged,
        sfsFactStore = factStoreValue,
        sfsSchedules = runtimeSchedules,
        sfsOutput = resolvedOutput,
        sfsTrace = unpackContextMutationTrace (crrTrace rebaseReport)
      }

data StagingState owner sig analysis context = StagingState
  { stBatch :: !(ContextRebaseBatch owner (PackedNode sig) analysis context),
    stSeeds :: !(Map FrontSeedName ClassId)
  }

data GlobalSeedDecl sig where
  GlobalSeedDecl :: !FrontSeedName -> !(Term sig sort) -> GlobalSeedDecl sig

data SeedBatchState owner sig analysis context = SeedBatchState
  { sbsPendingGlobalSeeds :: ![GlobalSeedDecl sig],
    sbsStagingState :: !(StagingState owner sig analysis context)
  }

runtimeSeedDecls ::
  forall owner sig analysis context.
  CompiledFrontState owner sig analysis context ->
  [FrontSeedTerm sig] ->
  Either (EGraphFrontError owner sig analysis context) [RuntimeSeedDecl sig context]
runtimeSeedDecls compiled seedTerms = do
  seedValues <- runtimeSeedTermMap (cfSeedSlots compiled) seedTerms
  traverse (runtimeSeedDecl seedValues) (cfSeedSlots compiled)

runtimeSeedTermMap ::
  forall owner sig analysis context.
  [FrontSeedSlot context] ->
  [FrontSeedTerm sig] ->
  Either (EGraphFrontError owner sig analysis context) (Map FrontSeedName (SomeTerm sig))
runtimeSeedTermMap seedSlots seedTerms = do
  declaredNames <-
    Set.fromList <$> traverse seedSlotName seedSlots
  suppliedTerms <-
    foldM insertRuntimeSeedTerm Map.empty seedTerms
  traverse_ (rejectUnknownRuntimeSeed declaredNames) (Map.keys suppliedTerms)
  pure suppliedTerms

seedSlotName :: FrontSeedSlot context -> Either (EGraphFrontError owner sig analysis context) FrontSeedName
seedSlotName =
  requireSeedName . fssName

insertRuntimeSeedTerm ::
  Map FrontSeedName (SomeTerm sig) ->
  FrontSeedTerm sig ->
  Either (EGraphFrontError owner sig analysis context) (Map FrontSeedName (SomeTerm sig))
insertRuntimeSeedTerm seedTerms (FrontSeedTerm nameInput termValue) = do
  seedName <- requireSeedName nameInput
  case Map.lookup seedName seedTerms of
    Just _existingTerm ->
      Left (EGraphFrontDuplicateSeed seedName)
    Nothing ->
      Right (Map.insert seedName (SomeTerm termValue) seedTerms)

rejectUnknownRuntimeSeed ::
  Set FrontSeedName ->
  FrontSeedName ->
  Either (EGraphFrontError owner sig analysis context) ()
rejectUnknownRuntimeSeed declaredNames seedName =
  if Set.member seedName declaredNames
    then Right ()
    else Left (EGraphFrontUnknownSeed seedName)

runtimeSeedDecl ::
  Map FrontSeedName (SomeTerm sig) ->
  FrontSeedSlot context ->
  Either (EGraphFrontError owner sig analysis context) (RuntimeSeedDecl sig context)
runtimeSeedDecl seedValues seedSlot = do
  seedName <- seedSlotName seedSlot
  case Map.lookup seedName seedValues of
    Just (SomeTerm termValue) ->
      Right (RuntimeSeedDecl seedName (fssContext seedSlot) termValue)
    Nothing ->
      Left (EGraphFrontUnknownSeed seedName)

stageFrontSchedules ::
  forall owner sig analysis context.
  (RewriteSignature sig, Ord (NodeTag sig), Ord context) =>
  CompiledFrontState owner sig analysis context ->
  [FrontSchedule sig analysis context] ->
  ObservationSeedState owner sig analysis context ->
  Either
    (EGraphFrontError owner sig analysis context)
    (ObservationSeedState owner sig analysis context, [FrontRuntimeSchedule owner sig analysis context])
stageFrontSchedules compiled schedules initialState = do
  (finalState, runtimeSchedules) <-
    foldM
      ( \(state, compiledSchedules) schedule -> do
          (nextState, runtimeSchedule) <- stageFrontSchedule compiled schedule state
          pure (nextState, runtimeSchedule : compiledSchedules)
      )
      (initialState, [])
      schedules
  pure (finalState, reverse runtimeSchedules)

stageFrontSchedule ::
  forall owner sig analysis context.
  (RewriteSignature sig, Ord (NodeTag sig), Ord context) =>
  CompiledFrontState owner sig analysis context ->
  FrontSchedule sig analysis context ->
  ObservationSeedState owner sig analysis context ->
  Either
    (EGraphFrontError owner sig analysis context)
    (ObservationSeedState owner sig analysis context, FrontRuntimeSchedule owner sig analysis context)
stageFrontSchedule compiled schedule initialState =
  fmap FrontRuntimeSchedule <$> stageScheduleTree (frontScheduleTree schedule) initialState
  where
    stageScheduleTree ::
      ControlProgram.Program () (FrontSchedulePhase sig analysis context) ->
      ObservationSeedState owner sig analysis context ->
      Either
        (EGraphFrontError owner sig analysis context)
        (ObservationSeedState owner sig analysis context, ControlProgram.Program () (FrontRuntimeSchedulePhase owner sig analysis context))
    stageScheduleTree =
      foldProgram
        ProgramAlgebra
          { paSkip = \state -> Right (state, skip),
            paPhase = stageFrontSchedulePhase compiled,
            paSeq = \leftAction rightAction state -> do
              (leftState, leftTree) <- leftAction state
              (rightState, rightTree) <- rightAction leftState
              pure (rightState, sequenceAll [leftTree, rightTree]),
            paOr = \leftAction rightAction state -> do
              (leftState, leftTree) <- leftAction state
              (rightState, rightTree) <- rightAction leftState
              pure (rightState, choices (leftTree :| [rightTree])),
            paUpTo = \repeatCount action state -> do
              (nextState, tree) <- action state
              pure (nextState, upTo repeatCount tree),
            paAttempt = \action state -> do
              (nextState, tree) <- action state
              pure (nextState, attempt tree),
            paScoped = const id
          }

stageFrontSchedulePhase ::
  forall owner sig analysis context.
  (RewriteSignature sig, Ord (NodeTag sig), Ord context) =>
  CompiledFrontState owner sig analysis context ->
  FrontSchedulePhase sig analysis context ->
  ObservationSeedState owner sig analysis context ->
  Either
    (EGraphFrontError owner sig analysis context)
    (ObservationSeedState owner sig analysis context, ControlProgram.Program () (FrontRuntimeSchedulePhase owner sig analysis context))
stageFrontSchedulePhase compiled phaseValue state = do
  rulesetName <-
    requireRulesetName (rulesetRefName (fspRuleset phaseValue))
  planValue <-
    compiledRulesetPlan compiled rulesetName (fspBudget phaseValue)
  (nextState, goalValue) <-
    case fspGoal phaseValue of
      Nothing ->
        Right (state, mempty)
      Just checkValue ->
        stageFrontGoal checkValue state
  pure
    ( nextState,
      phase
        FrontRuntimeRunRuleset
          { frspPlan = planValue,
            frspGoal = goalValue
          }
    )

stageFrontGoal ::
  forall owner sig analysis context.
  (RewriteSignature sig, Ord (NodeTag sig), Ord context) =>
  FrontCheck sig ->
  ObservationSeedState owner sig analysis context ->
  Either
    (EGraphFrontError owner sig analysis context)
    (ObservationSeedState owner sig analysis context, TerminationGoal (SaturatingContextEGraph owner SurfaceKind (PackedNode sig) analysis context))
stageFrontGoal =
  \case
    FrontCheckEq termRef termValue ->
      \state -> do
        leftClass <- seedClassFor "schedule-goal" termRef state
        (rightClass, nextBatch) <- stageObservationTerm Nothing termValue (ossBatch state)
        let observation = CheckEquivalentBase leftClass rightClass
            goalValue =
              TerminationGoal $
                either (const False) id . runStableObservation observation
        pure (state {ossBatch = nextBatch}, goalValue)

stageRuntimeSeedDecls ::
  forall owner sig analysis context.
  (RewriteSignature sig, Ord (NodeTag sig), Ord context) =>
  StagingState owner sig analysis context ->
  [RuntimeSeedDecl sig context] ->
  Either (EGraphFrontError owner sig analysis context) (StagingState owner sig analysis context)
stageRuntimeSeedDecls initialState seedDecls =
  sbsStagingState
    <$> ( foldM
            stageRuntimeSeedDecl
            SeedBatchState
              { sbsPendingGlobalSeeds = [],
                sbsStagingState = initialState
              }
            seedDecls
            >>= flushGlobalSeedBatch
        )

stageRuntimeSeedDecl ::
  forall owner sig analysis context.
  (RewriteSignature sig, Ord (NodeTag sig), Ord context) =>
  SeedBatchState owner sig analysis context ->
  RuntimeSeedDecl sig context ->
  Either (EGraphFrontError owner sig analysis context) (SeedBatchState owner sig analysis context)
stageRuntimeSeedDecl state (RuntimeSeedDecl seedName maybeContext termValue) =
  case maybeContext of
    Nothing ->
      Right
        state
          { sbsPendingGlobalSeeds =
              GlobalSeedDecl seedName termValue : sbsPendingGlobalSeeds state
          }
    Just contextRef -> do
      flushedState <- flushGlobalSeedBatch state
      stagingState <- stageContextSeedDecl (sbsStagingState flushedState) seedName contextRef termValue
      Right (flushedState {sbsStagingState = stagingState})

stageContextSeedDecl ::
  forall owner sig analysis context sort.
  (RewriteSignature sig, Ord (NodeTag sig), Ord context) =>
  StagingState owner sig analysis context ->
  FrontSeedName ->
  ContextRef context ->
  Term sig sort ->
  Either (EGraphFrontError owner sig analysis context) (StagingState owner sig analysis context)
stageContextSeedDecl state seedName contextRef termValue = do
  fixTerm <- groundTermFix termValue
  (classId, nextBatch) <-
    first (EGraphFrontContextDeltaError . unpackContextDeltaError) $
      stageTermAtContext (contextRefValue contextRef) fixTerm (stBatch state)
  insertSeedClass seedName classId state {stBatch = nextBatch}

flushGlobalSeedBatch ::
  forall owner sig analysis context.
  (RewriteSignature sig, Ord (NodeTag sig), Ord context) =>
  SeedBatchState owner sig analysis context ->
  Either (EGraphFrontError owner sig analysis context) (SeedBatchState owner sig analysis context)
flushGlobalSeedBatch state =
  case reverse (sbsPendingGlobalSeeds state) of
    [] ->
      Right state
    globalSeeds -> do
      staged <- stageGlobalSeedDecls (sbsStagingState state) globalSeeds
      Right
        state
          { sbsPendingGlobalSeeds = [],
            sbsStagingState = staged
          }

stageGlobalSeedDecls ::
  forall owner sig analysis context.
  (RewriteSignature sig, Ord (NodeTag sig), Ord context) =>
  StagingState owner sig analysis context ->
  [GlobalSeedDecl sig] ->
  Either (EGraphFrontError owner sig analysis context) (StagingState owner sig analysis context)
stageGlobalSeedDecls state globalSeeds = do
  preparedSeeds <- traverse prepareGlobalSeedDecl globalSeeds
  let seedNames =
        fmap fst preparedSeeds
      fixTerms =
        fmap snd preparedSeeds
  (classIds, nextBatch) <-
    first (EGraphFrontContextDeltaError . unpackContextDeltaError) $
      stageTermsGlobally fixTerms (stBatch state)
  let
      stagedState =
        state {stBatch = nextBatch}
  foldM
    (\currentState (seedName, classId) -> insertSeedClass seedName classId currentState)
    stagedState
    (zip seedNames classIds)

insertSeedClass ::
  FrontSeedName ->
  ClassId ->
  StagingState owner sig analysis context ->
  Either (EGraphFrontError owner sig analysis context) (StagingState owner sig analysis context)
insertSeedClass seedName classId state =
  case Map.lookup seedName (stSeeds state) of
    Just _existingClassId ->
      Left (EGraphFrontDuplicateSeed seedName)
    Nothing ->
      Right
        state
          { stSeeds = Map.insert seedName classId (stSeeds state)
          }

prepareGlobalSeedDecl ::
  RewriteSignature sig =>
  GlobalSeedDecl sig ->
  Either (EGraphFrontError owner sig analysis context) (FrontSeedName, Fix (PackedNode sig))
prepareGlobalSeedDecl (GlobalSeedDecl seedName termValue) =
  fmap (\fixTerm -> (seedName, fixTerm)) (groundTermFix termValue)

resolveSeedFact ::
  Map FrontSeedName ClassId ->
  FactStore ->
  SeedFactDecl sig ->
  Either (EGraphFrontError owner sig analysis context) FactStore
resolveSeedFact seededClasses factStoreValue (SeedFactDecl relationRef args) = do
  classIds <- resolveFactRefArgs seededClasses args
  pure
    ( insertFact
        (relationRefFactId relationRef)
        (FactTuple classIds)
        factStoreValue
    )

resolveFactRefArgs ::
  Map FrontSeedName ClassId ->
  FactRefArgs sig sorts ->
  Either (EGraphFrontError owner sig analysis context) [ClassId]
resolveFactRefArgs seededClasses =
  \case
    FactRefNil ->
      Right []
    termRef :@& rest -> do
      seedName <- requireSeedName (termRefName termRef)
      classId <-
        maybe
          (Left (EGraphFrontUnknownSeed seedName))
          Right
          (Map.lookup seedName seededClasses)
      (classId :) <$> resolveFactRefArgs seededClasses rest

data ObservationSeedState owner sig analysis context = ObservationSeedState
  { ossBatch :: !(ContextRebaseBatch owner (PackedNode sig) analysis context),
    ossSeeds :: !(Map FrontSeedName ClassId)
  }

seedClassFor ::
  String ->
  TermRef sig sort ->
  ObservationSeedState owner sig analysis context ->
  Either (EGraphFrontError owner sig analysis context) ClassId
seedClassFor _rawObservation termRef state =
  do
    seedName <- requireSeedName (termRefName termRef)
    maybe
      (Left (EGraphFrontUnknownSeed seedName))
      Right
      (Map.lookup seedName (ossSeeds state))

stageObservationTerm ::
  (RewriteSignature sig, Ord (NodeTag sig), Ord context) =>
  Maybe (ContextRef context) ->
  Term sig sort ->
  ContextRebaseBatch owner (PackedNode sig) analysis context ->
  Either (EGraphFrontError owner sig analysis context) (ClassId, ContextRebaseBatch owner (PackedNode sig) analysis context)
stageObservationTerm maybeContext termValue batchValue = do
  fixTerm <- groundTermFix termValue
  case maybeContext of
    Nothing ->
      first (EGraphFrontContextDeltaError . unpackContextDeltaError) $
        stageTermGlobally fixTerm batchValue
    Just contextRef ->
      first (EGraphFrontContextDeltaError . unpackContextDeltaError) $
        stageTermAtContext (contextRefValue contextRef) fixTerm batchValue

groundTermFix ::
  RewriteSignature sig =>
  Term sig sort ->
  Either (EGraphFrontError owner sig analysis context) (Fix (PackedNode sig))
groundTermFix =
  fmap packFix . groundNodeTermFix

groundNodeTermFix ::
  RewriteSignature sig =>
  Term sig sort ->
  Either (EGraphFrontError owner sig analysis context) (Fix (Node sig))
groundNodeTermFix =
  \case
    TVar typedVariable ->
      Left
        ( EGraphFrontGroundTermContainsVariable
            (someTypedVarName (SomeTypedVar typedVariable))
            (sortNameString (someTypedVarSort (SomeTypedVar typedVariable)))
        )
    TNode sigNode ->
      Fix . Node
        <$> htraverse
          (fmap K . groundNodeTermFix)
          sigNode

closeRuleBody :: HTraversable sig => DSLRule.RuleBody sig FrontGuardAtom -> DSLRule.RuleBody sig FrontGuardAtom
closeRuleBody (DSLRule.RuleBody binders leftTerm rightTerm guards applicationConditions scope) =
  DSLRule.RuleBody
    (binders <> foldMap binderFor missingVariables)
    leftTerm
    rightTerm
    guards
    applicationConditions
    scope
  where
    declaredVariables =
      Set.fromList (fmap DSLRule.rbTypedVar (DSLRule.ruleBinderList binders))
    freeVariables =
      termVariables leftTerm
        <> termVariables rightTerm
        <> foldMap guardVariables guards
        <> foldMap applicationConditionVariables applicationConditions
    missingVariables =
      Set.toAscList (Set.difference freeVariables declaredVariables)

binderFor :: SomeTypedVar -> DSLRule.RuleBinders sig
binderFor (SomeTypedVar typedVariable) =
  DSLRule.bindTypedVar typedVariable

termVariables :: HTraversable sig => Term sig sort -> Set SomeTypedVar
termVariables =
  \case
    TVar typedVariable ->
      Set.singleton (SomeTypedVar typedVariable)
    TNode sigNode ->
      hfoldMap termVariables sigNode

guardVariables :: HTraversable sig => DSLRule.Guard sig FrontGuardAtom -> Set SomeTypedVar
guardVariables =
  \case
    DSLRule.GuardEq leftTerm rightTerm ->
      termVariables leftTerm <> termVariables rightTerm
    DSLRule.GuardAtom atomValue ->
      guardAtomVariables atomValue
    DSLRule.GuardNot child ->
      guardVariables child
    DSLRule.GuardAnd children ->
      foldMap guardVariables children
    DSLRule.GuardOr children ->
      foldMap guardVariables children

guardAtomVariables :: HTraversable sig => FrontGuardAtom sig -> Set SomeTypedVar
guardAtomVariables (FrontHasFact _ args) =
  factArgsVariables args

factArgsVariables :: HTraversable sig => FactArgs sig sorts -> Set SomeTypedVar
factArgsVariables =
  \case
    FactNil ->
      Set.empty
    termValue :& rest ->
      termVariables termValue <> factArgsVariables rest

applicationConditionVariables :: HTraversable sig => DSLRule.ApplicationConditionDSL sig FrontGuardAtom -> Set SomeTypedVar
applicationConditionVariables =
  \case
    DSLRule.Requires extensionValue ->
      extensionVariables extensionValue
    DSLRule.Forbids extensionValue ->
      extensionVariables extensionValue

extensionVariables :: HTraversable sig => DSLRule.Extension sig FrontGuardAtom -> Set SomeTypedVar
extensionVariables (DSLRule.Extension termValue guards (_ :: PatternExtensionScope)) =
  termVariables termValue <> foldMap guardVariables guards

traverseFactArgs ::
  Applicative m =>
  (forall sort. Term sig sort -> m (GuardTerm (Node sig))) ->
  FactArgs sig sorts ->
  m [GuardTerm (Node sig)]
traverseFactArgs lower =
  \case
    FactNil ->
      pure []
    termValue :& rest ->
      liftA2 (:) (lower termValue) (traverseFactArgs lower rest)

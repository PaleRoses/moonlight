{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Sheaf.ContextSpec
  ( tests,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Core
  ( AtomId,
    QueryId,
    mkAtomId,
    mkQueryId,
    mkSlotId,
  )
import Moonlight.Differential.Context.RowsCache
  ( ContextRowsCache,
    ContextRowsRuntime (..),
    contextRowsKey,
    emptyContextRowsCache,
  )
import Moonlight.Differential.Proposition
  ( PropositionKey (..),
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier (..),
    QueryCarrierNode (..),
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
    carrierAddr,
  )
import Moonlight.Flow.Carrier.View.Section
  ( RelationalSection (..),
  )
import Moonlight.Delta.Signed
  ( MultiplicityChange (..)
  )
import Moonlight.Differential.Row.Patch
  ( plainRowPatchFromList
  )
import Moonlight.Flow.Model.Scope
  ( DepsDelta (..),
    RelationalScope (..),
    relationalScopeNull,
  )
import Moonlight.Differential.Row.Tuple
  ( RepKey (..),
    tupleKeyFromRepKeys,
  )
import Moonlight.Flow.Runtime.Create
  ( createRuntimeWithOptions,
  )
import Moonlight.Flow.Runtime.Spec.Schema
  ( RuntimeAtom,
    RuntimeSpec,
    runtimeAtom,
    runtimeContextSchema,
    runtimeSchema,
    runtimeSpec,
  )
import Moonlight.Flow.Runtime.Types
  ( Runtime,
    RuntimeCreateOptions (..),
    defaultRuntimeCreateOptions,
  )
import Moonlight.Flow.Query qualified as Rel
import Moonlight.Flow.Runtime.Spec.Schema qualified as Rel
import Moonlight.Flow.Patch
  ( Patch,
    emptyPatch,
    insert,
  )
import Moonlight.Sheaf.Context.Algebra
  ( ContextAlgebraSite (..),
    propagationTargets,
    restrictionMap,
  )
import Moonlight.Sheaf.Context.Core
  ( ClassSiteSupport,
    ContextLattice,
    ContextPropagationFailure (..),
    ContextPropagationReport,
    contextPropagationChangedContexts,
    contextPropagationSettled,
    contextRefinesTo,
    settledPropagationReport,
  )
import Moonlight.Sheaf.Context.Runtime
  ( ContextPropagationInvariantFailure (..),
    ContextPropagationState (..),
    ContextRefreshPrepared (..),
    ContextRuntime (..),
    ContextRuntimeCacheIdentity (..),
    bootstrapContextSectionsWithBudget,
  )
import Moonlight.Sheaf.Context.Site
  ( ClassSupportIndex,
    PreparedContextSite,
    PreparedContextSiteError (..),
    PreparedContextSupportError (..),
    classSupportDeltaTouchedClassKeys,
    classSupportIndexEntries,
    classSupportIndexFromEntries,
    classSupportIndexInsertMany,
    fromFiniteLattice,
    preparedSupportFromContexts,
    preparedSupportReachableObjects,
  )
import Moonlight.Sheaf.Context.Runtime.Row
  ( ContextRowRefreshSpec (..),
    RowRuntimeIdentity (..),
    StoredRowRuntime,
    compileContextRowRefresh,
    defaultRebuildResolvedContextSection,
  )
import Moonlight.Sheaf.Descent.Context
  ( QuotientDescentObstruction (..),
    DescentReport (..),
    descentAt,
    fullDescentCheck,
  )
import Moonlight.Sheaf.Verdict
  ( SearchVerdict (..),
  )
import Moonlight.Sheaf.Context.Witness
  ( contextAnalysisGlobalSectionInvariant,
    contextAnalysisRestrictionComposition,
    contextGlobalSectionInvariant,
    contextRestrictionFunctorialAction,
    contextRestrictionIdentity,
    mkContextMorphism,
  )
import Moonlight.Sheaf.Section.Restriction.Witness
  ( contextMorphismAssociative,
    contextMorphismLeftIdentity,
    contextMorphismRightIdentity,
    identityContextMorphism,
  )
import Moonlight.Sheaf.Section.Store.Types
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertFailure,
    testCase,
    (@?=),
  )
import Moonlight.FiniteLattice
  ( compileContextLattice,
    contextOrderDecl,
    upperCovers
  )
import Moonlight.FiniteLattice
  ( supportGenerators
  )

type ChainCtx :: Type
data ChainCtx
  = GlobalCtx
  | ModuleCtx
  | LocalCtx
  deriving stock (Eq, Ord, Show, Enum, Bounded)

type CoverCtx :: Type
data CoverCtx
  = RootCtx
  | LeftCtx
  | MiddleCtx
  | RightCtx
  | FocusCtx
  deriving stock (Eq, Ord, Show, Enum, Bounded)

type BranchContext :: Type
data BranchContext
  = BranchBase
  | BranchLeft
  | BranchRight
  | BranchApex
  deriving stock (Eq, Ord, Show, Enum, Bounded)

type MiniStalk :: Type
newtype MiniStalk = MiniStalk Double
  deriving stock (Eq, Show)

type MockStore :: Type -> Type
data MockStore ctx = MockStore
  { msLattice :: !(ContextLattice ctx),
    msContexts :: ![ctx],
    msGlobalRep :: !(Int -> Int),
    msSupportIndex :: !(ClassSupportIndex ctx),
    msClasses :: !(Map ctx (IntMap Int)),
    msAnalysis :: !(Map ctx (IntMap Int))
  }

instance Ord ctx => ContextAlgebraSite (MockStore ctx) ctx Int Int where
  contextPreparedSite = fromFiniteLattice . msLattice
  contextCachedContexts = msContexts
  contextGlobalRepresentative classId store = msGlobalRep store classId
  classesFor contextValue store = Right (Map.findWithDefault IntMap.empty contextValue (msClasses store))
  contextAnalysisFor contextValue store = Right (Map.findWithDefault IntMap.empty contextValue (msAnalysis store))
  contextAnalysisJoin _ = max
  contextClassSupportIndex = msSupportIndex

expectRight :: Show err => Either err value -> IO value
expectRight =
  either (assertFailure . show) pure

expectRightJust :: Show err => String -> Either err (Maybe value) -> IO value
expectRightJust failureMessage eitherMaybeValue =
  expectRight eitherMaybeValue >>= maybe (assertFailure failureMessage) pure

compiledFixtureLattice ::
  (Ord ctx, Show ctx) =>
  [ctx] ->
  ctx ->
  ctx ->
  [(ctx, ctx)] ->
  ContextLattice ctx
compiledFixtureLattice contexts topContext bottomContext orderEdges =
  either
    (error . ("invalid context fixture lattice: " <>) . show)
    id
    ( compileContextLattice
        (Set.fromList contexts)
        (contextOrderDecl topContext bottomContext orderEdges)
    )

compiledFixtureSupport ::
  (Ord ctx, Show ctx) =>
  ContextLattice ctx ->
  [ctx] ->
  ClassSiteSupport ctx
compiledFixtureSupport latticeValue =
  either
    (error . ("invalid context fixture support: " <>) . show)
    id
    . preparedSupportFromContexts (fromFiniteLattice latticeValue)

compiledFixtureSupportIndex ::
  (Ord ctx, Show ctx) =>
  ContextLattice ctx ->
  IntMap (ClassSiteSupport ctx) ->
  ClassSupportIndex ctx
compiledFixtureSupportIndex latticeValue =
  either
    (error . ("invalid context fixture support index: " <>) . show)
    id
    . classSupportIndexFromEntries (fromFiniteLattice latticeValue)

chainLattice :: ContextLattice ChainCtx
chainLattice =
  compiledFixtureLattice
    [GlobalCtx, ModuleCtx, LocalCtx]
    LocalCtx
    GlobalCtx
    [(GlobalCtx, ModuleCtx), (ModuleCtx, LocalCtx)]

chainSupport :: [ChainCtx] -> ClassSiteSupport ChainCtx
chainSupport = compiledFixtureSupport chainLattice

branchContextLattice :: ContextLattice BranchContext
branchContextLattice =
  compiledFixtureLattice
    [BranchBase, BranchLeft, BranchRight, BranchApex]
    BranchApex
    BranchBase
    [ (BranchBase, BranchLeft),
      (BranchBase, BranchRight),
      (BranchLeft, BranchApex),
      (BranchRight, BranchApex)
    ]

branchSupport :: [BranchContext] -> ClassSiteSupport BranchContext
branchSupport = compiledFixtureSupport branchContextLattice

coverLattice :: ContextLattice CoverCtx
coverLattice =
  compiledFixtureLattice
    [RootCtx, LeftCtx, MiddleCtx, RightCtx, FocusCtx]
    FocusCtx
    RootCtx
    [ (RootCtx, LeftCtx),
      (RootCtx, MiddleCtx),
      (RootCtx, RightCtx),
      (LeftCtx, FocusCtx),
      (MiddleCtx, FocusCtx),
      (RightCtx, FocusCtx)
    ]

coverSupport :: [CoverCtx] -> ClassSiteSupport CoverCtx
coverSupport = compiledFixtureSupport coverLattice

chainStore :: MockStore ChainCtx
chainStore =
  MockStore
    { msLattice = chainLattice,
      msContexts = [GlobalCtx, ModuleCtx, LocalCtx],
      msGlobalRep = id,
      msSupportIndex =
        compiledFixtureSupportIndex chainLattice $
          IntMap.fromList
            [ (1, chainSupport [GlobalCtx]),
              (2, chainSupport [ModuleCtx]),
              (3, chainSupport [LocalCtx])
            ],
      msClasses =
        Map.fromList
          [ (GlobalCtx, IntMap.fromList [(1, 1), (2, 2), (3, 3)]),
            (ModuleCtx, IntMap.fromList [(1, 1), (2, 1), (3, 3)]),
            (LocalCtx, IntMap.fromList [(1, 1), (2, 1), (3, 1)])
          ],
      msAnalysis =
        Map.fromList
          [ (GlobalCtx, IntMap.fromList [(1, 1), (2, 0), (3, 2)]),
            (ModuleCtx, IntMap.fromList [(1, 1), (3, 2)]),
            (LocalCtx, IntMap.fromList [(1, 2)])
          ]
    }

supportObstructedStore :: MockStore CoverCtx
supportObstructedStore =
  MockStore
    { msLattice = coverLattice,
      msContexts = [RootCtx, LeftCtx, MiddleCtx, RightCtx, FocusCtx],
      msGlobalRep = id,
      msSupportIndex =
        compiledFixtureSupportIndex coverLattice $
          IntMap.fromList
            [ (0, coverSupport [RootCtx]),
              (7, coverSupport [LeftCtx, MiddleCtx, RightCtx])
            ],
      msClasses =
        Map.fromList
          [ (RootCtx, IntMap.fromList [(0, 0), (7, 0)]),
            (LeftCtx, IntMap.fromList [(0, 0), (7, 0)]),
            (MiddleCtx, IntMap.fromList [(0, 0), (7, 0)]),
            (RightCtx, IntMap.fromList [(0, 0), (7, 0)]),
            (FocusCtx, IntMap.fromList [(0, 0), (7, 0)])
          ],
      msAnalysis = Map.fromList [(contextValue, IntMap.empty) | contextValue <- [RootCtx, LeftCtx, MiddleCtx, RightCtx, FocusCtx]]
    }

supportVisibleStore :: MockStore CoverCtx
supportVisibleStore =
  supportObstructedStore
    { msSupportIndex =
        compiledFixtureSupportIndex coverLattice $
          IntMap.fromList
            [ (0, coverSupport [RootCtx]),
              (7, coverSupport [RootCtx])
            ]
    }

diamondSatisfiedStore :: MockStore BranchContext
diamondSatisfiedStore =
  MockStore
    { msLattice = branchContextLattice,
      msContexts = [BranchBase, BranchLeft, BranchRight, BranchApex],
      msGlobalRep = id,
      msSupportIndex =
        compiledFixtureSupportIndex branchContextLattice $
          IntMap.fromList
            [ (1, branchSupport [BranchBase]),
              (2, branchSupport [BranchBase]),
              (3, branchSupport [BranchBase])
            ],
      msClasses =
        Map.fromList
          [ (BranchBase, IntMap.fromList [(1, 1), (2, 1), (3, 3)]),
            (BranchLeft, IntMap.fromList [(1, 1), (2, 1), (3, 3)]),
            (BranchRight, IntMap.fromList [(1, 1), (2, 1), (3, 3)]),
            (BranchApex, IntMap.fromList [(1, 1), (2, 1), (3, 1)])
          ],
      msAnalysis =
        Map.fromList
          [ (BranchBase, IntMap.fromList [(1, 1), (3, 2)]),
            (BranchLeft, IntMap.fromList [(1, 1), (3, 2)]),
            (BranchRight, IntMap.fromList [(1, 1), (3, 2)]),
            (BranchApex, IntMap.fromList [(1, 2)])
          ]
    }

diamondObstructedStore :: MockStore BranchContext
diamondObstructedStore =
  MockStore
    { msLattice = branchContextLattice,
      msContexts = [BranchBase, BranchLeft, BranchRight, BranchApex],
      msGlobalRep = id,
      msSupportIndex =
        compiledFixtureSupportIndex branchContextLattice $
          IntMap.fromList
            [ (0, branchSupport [BranchBase]),
              (7, branchSupport [BranchLeft, BranchRight])
            ],
      msClasses =
        Map.fromList
          [ (BranchBase, IntMap.fromList [(0, 0), (7, 0)]),
            (BranchLeft, IntMap.fromList [(0, 0), (7, 0)]),
            (BranchRight, IntMap.fromList [(0, 0), (7, 7)]),
            (BranchApex, IntMap.fromList [(0, 0), (7, 0)])
          ],
      msAnalysis =
        Map.fromList
          [(contextValue, IntMap.empty) | contextValue <- [BranchBase, BranchLeft, BranchRight, BranchApex]]
    }

type AnatomyCtx :: Type
data AnatomyCtx
  = AWhole
  | AUpper
  | ALower
  | AHead
  | ATorso
  | ALegLeft
  | ALegRight
  | ALocal
  deriving stock (Eq, Ord, Show, Enum, Bounded)

anatomyLeq :: AnatomyCtx -> AnatomyCtx -> Bool
anatomyLeq leftContext rightContext
  | leftContext == rightContext = True
  | leftContext == AWhole = True
  | rightContext == ALocal = True
  | leftContext == AUpper && rightContext `elem` [AHead, ATorso] = True
  | leftContext == ALower && rightContext `elem` [ALegLeft, ALegRight] = True
  | otherwise = False

anatomyLattice :: ContextLattice AnatomyCtx
anatomyLattice =
  compiledFixtureLattice
    [minBound .. maxBound]
    ALocal
    AWhole
    [ (AWhole, AUpper),
      (AWhole, ALower),
      (AUpper, AHead),
      (AUpper, ATorso),
      (ALower, ALegLeft),
      (ALower, ALegRight),
      (AHead, ALocal),
      (ATorso, ALocal),
      (ALegLeft, ALocal),
      (ALegRight, ALocal)
    ]

anatomySupport :: [AnatomyCtx] -> ClassSiteSupport AnatomyCtx
anatomySupport = compiledFixtureSupport anatomyLattice

anatomyStore :: MockStore AnatomyCtx
anatomyStore =
  let allCtxs = [minBound .. maxBound]
      globalSupport = anatomySupport [AWhole]
      headMergedClasses :: AnatomyCtx -> IntMap Int
      headMergedClasses ctx
        | anatomyLeq AHead ctx = IntMap.fromList [(1, 1), (2, 1), (3, 3)]
        | otherwise = IntMap.fromList [(1, 1), (2, 2), (3, 3)]
      headMergedAnalysis :: AnatomyCtx -> IntMap Int
      headMergedAnalysis ctx
        | anatomyLeq AHead ctx = IntMap.fromList [(1, 1), (3, 2)]
        | otherwise = IntMap.fromList [(1, 0), (2, 1), (3, 2)]
   in MockStore
        { msLattice = anatomyLattice,
          msContexts = allCtxs,
          msGlobalRep = id,
          msSupportIndex =
            compiledFixtureSupportIndex anatomyLattice $
              IntMap.fromList
                [ (1, globalSupport),
                  (2, globalSupport),
                  (3, globalSupport)
                ],
          msClasses = Map.fromList [(ctx, headMergedClasses ctx) | ctx <- allCtxs],
          msAnalysis = Map.fromList [(ctx, headMergedAnalysis ctx) | ctx <- allCtxs]
        }

type RefreshCtx :: Type
data RefreshCtx
  = LiveRefreshCtx
  | StaleRefreshCtx
  deriving stock (Eq, Ord, Show)

type RefreshSite :: Type
data RefreshSite = RefreshSite
  { rsPreparedSite :: !(PreparedContextSite RefreshCtx),
    rsDirtyContexts :: !(Set RefreshCtx),
    rsStoredSections :: !(Map RefreshCtx MiniStalk),
    rsStoredRuntimeCacheIdentity :: !(Maybe ContextRuntimeCacheIdentity),
    rsStoredRuntime :: !(Maybe (StoredRowRuntime RefreshCtx Int)),
    rsRowsCache :: !(ContextRowsCache RefreshCtx (RelationalSection RefreshCtx Carrier Int)),
    rsPropagationState ::
      !( ContextPropagationState
          RefreshCtx
          (ContextPropagationReport RefreshCtx)
          String
       )
  }

refreshLattice :: ContextLattice RefreshCtx
refreshLattice =
  compiledFixtureLattice
    [LiveRefreshCtx]
    LiveRefreshCtx
    LiveRefreshCtx
    []

refreshRuntime :: ContextRuntime RefreshSite RefreshCtx MiniStalk MiniStalk (ContextPropagationReport RefreshCtx) String
refreshRuntime =
  ContextRuntime
    { crPreparedSite = rsPreparedSite,
      crCachedContexts = const [LiveRefreshCtx],
      crFreshSection = \_ _ -> MiniStalk 0.0,
      crResolveFreshSection = id,
      crRuntimeCacheIdentity = const (ContextRuntimeCacheIdentity 0 0),
      crStoredRuntimeCacheIdentity = rsStoredRuntimeCacheIdentity,
      crSetStoredRuntimeCacheIdentity = \identity site -> site {rsStoredRuntimeCacheIdentity = identity},
      crPropagationState = rsPropagationState,
      crDirtyContexts = rsDirtyContexts,
      crSetDirtyContexts = \dirty site -> site {rsDirtyContexts = dirty},
      crStoredSections = rsStoredSections,
      crSetSections = \sections site -> site {rsStoredSections = sections},
      crSetPropagationState = \propagationState site -> site {rsPropagationState = propagationState},
      crCompileContextRefresh =
        compileContextRowRefresh refreshRowRefreshSpec
    }

refreshRowRefreshSpec ::
  ContextRowRefreshSpec
    RefreshSite
    RefreshCtx
    (RelationalSection RefreshCtx Carrier Int)
    MiniStalk
    Int
    (ContextPropagationReport RefreshCtx)
    String
refreshRowRefreshSpec =
  ContextRowRefreshSpec
    { crrsStoredRuntime =
        rsStoredRuntime,
      crrsSetStoredRuntime =
        \runtime site -> site {rsStoredRuntime = runtime},
      crrsRuntimeIdentity =
        refreshRuntimeIdentity,
      crrsRowsCache =
        rsRowsCache,
      crrsSetRowsCache =
        \cache site -> site {rsRowsCache = cache},
      crrsRowsRuntime =
        refreshRowsRuntime,
      crrsBuildRuntime =
        \cacheBudget prepared dirtyContexts site section0 ->
          buildRefreshRuntime cacheBudget prepared site dirtyContexts section0,
      crrsDirtyContextsToRelationalScope =
        \dirtyContexts _site _section0 ->
          if Set.isSubsetOf dirtyContexts (Set.fromList [LiveRefreshCtx])
            then Right (refreshRelationalScope dirtyContexts)
            else Left "row refresh received non-materialized dirty contexts",
      crrsRelationalScopeToSite =
        \_dirtyKeys _dirtyContexts site _section0 ->
          Right site,
      crrsRelationalScopeToPatch =
        \_prepared dirtyKeys _dirtyContexts _dirtyRows _site _section0 ->
          refreshPatch dirtyKeys,
      crrsVisibleSectionToSection =
        \contextValue _site _section0 ->
          refreshVisibleSectionToSection contextValue,
      crrsRebuildResolvedSection =
        defaultRebuildResolvedContextSection show,
      crrsReportFromRuntime =
        \_prepared dirtyContexts _selection _site _runtime _section0 _resolvedSection ->
          Right (settledPropagationReport dirtyContexts),
      crrsRuntimeApplyFailure =
        show,
      crrsRuntimeReadFailure =
        show
    }

refreshRuntimeIdentity ::
  ContextRefreshPrepared RefreshSite RefreshCtx MiniStalk ->
  Set RefreshCtx ->
  RefreshSite ->
  TotalSectionStore RefreshCtx MiniStalk ->
  RowRuntimeIdentity
refreshRuntimeIdentity _prepared _dirtyContexts _site _section0 =
  RowRuntimeIdentity
    { rriGeneratedSiteFingerprint = 0,
      rriContextLatticeFingerprint = 0,
      rriPlanFingerprint = 0,
      rriQuotientEpochFingerprint = 0,
      rriLiveEpochFingerprint = 0,
      rriRuntimeFingerprint = 0,
      rriRoutingFingerprint = 0,
      rriVisibleCachePolicyFingerprint = 0
    }

buildRefreshRuntime ::
  Int ->
  ContextRefreshPrepared RefreshSite RefreshCtx MiniStalk ->
  RefreshSite ->
  Set RefreshCtx ->
  TotalSectionStore RefreshCtx MiniStalk ->
  Either String (Runtime RefreshCtx Int)
buildRefreshRuntime cacheBudget _prepared _site _dirtyContexts _section0 =
  refreshRuntimeSpec >>= \specValue ->
  firstShow $
    createRuntimeWithOptions
      specValue
      defaultRuntimeCreateOptions
        { rcoVisibleCacheBudgetBytes = cacheBudget
        }

refreshRuntimeSpec :: Either String (RuntimeSpec RefreshCtx Int)
refreshRuntimeSpec = do
  queryValue <-
    firstShow $
      Rel.query
        [Rel.runtimeMatch refreshRuntimeAtom]
        Rel.selectAll
  planValue <-
    firstShow $
      Rel.runtimePlanQuery LiveRefreshCtx refreshProposition queryValue
  pure $
    runtimeSpec
      ( runtimeSchema
          [ ( LiveRefreshCtx,
              runtimeContextSchema
                [refreshRuntimeAtom]
                [refreshProposition]
            )
          ]
      )
      [planValue]

refreshRowsRuntime ::
  ContextRefreshPrepared RefreshSite RefreshCtx MiniStalk ->
  RefreshSite ->
  TotalSectionStore RefreshCtx MiniStalk ->
  ContextRowsRuntime
    (Either String)
    RefreshCtx
    (RelationalSection RefreshCtx Carrier Int)
refreshRowsRuntime prepared _site _section0 =
  ContextRowsRuntime
    { crrKeyFor =
        \contextValue ->
              let cacheRevision =
                    crciBaseRevision (crpRuntimeCacheIdentity prepared)
           in contextRowsKey cacheRevision 0 cacheRevision contextValue,
      crrChooseRestrictionSource =
        \_cachedContexts _targetContext ->
          Right Nothing,
      crrMaterializeRootRows =
        \targetContext ->
          Right (refreshMaterializedRows targetContext),
      crrDeriveByRestriction =
        \_sourceContext targetContext _sourceRows ->
          Right (refreshMaterializedRows targetContext),
      crrRowsBytes =
        const 1
    }

refreshMaterializedRows ::
  RefreshCtx ->
  RelationalSection RefreshCtx Carrier Int
refreshMaterializedRows contextValue =
  RelationalSection
    { rsCarriers =
        Map.singleton
          (refreshCarrierAddress contextValue)
          (plainRowPatchFromList [(tupleKeyFromRepKeys [RepKey 1], MultiplicityChange 1)])
    }

refreshVisibleSectionToSection ::
  RefreshCtx ->
  RelationalSection RefreshCtx Carrier Int ->
  Either String MiniStalk
refreshVisibleSectionToSection _contextValue visibleSection =
  if Map.null (rsCarriers visibleSection)
    then Left "relational runtime produced no visible carrier for the live refresh context"
    else Right (MiniStalk 1.0)

refreshRelationalScope ::
  Set RefreshCtx ->
  RelationalScope
refreshRelationalScope dirtyContexts =
  if Set.null dirtyContexts
    then mempty
    else mempty {rsDeps = DepsDelta (IntSet.singleton refreshAtomKey)}

refreshPatch ::
  RelationalScope ->
  Either String Patch
refreshPatch dirtyKeys =
  if relationalScopeNull dirtyKeys
    then Right emptyPatch
    else firstShow $
      insert
          refreshRuntimeAtom
          [tupleKeyFromRepKeys [RepKey 1]]

refreshRuntimeAtom :: RuntimeAtom RefreshCtx Int
refreshRuntimeAtom =
  runtimeAtom refreshAtomId [mkSlotId 0]

refreshCarrierAddress ::
  RefreshCtx ->
  CarrierAddr RefreshCtx Carrier Int
refreshCarrierAddress contextValue =
  carrierAddr
    contextValue
    refreshProposition
    (QueryCarrier refreshQueryId (QueryAtom refreshAtomId))

refreshProposition ::
  PropositionKey Int
refreshProposition =
  PropositionKey 0

refreshQueryId ::
  QueryId
refreshQueryId =
  mkQueryId 0

refreshAtomId ::
  AtomId
refreshAtomId =
  mkAtomId refreshAtomKey

refreshAtomKey ::
  Int
refreshAtomKey =
  0

firstShow ::
  Show left =>
  Either left right ->
  Either String right
firstShow =
  either (Left . show) Right

initialRefreshSite :: RefreshSite
initialRefreshSite =
  RefreshSite
    { rsPreparedSite = fromFiniteLattice refreshLattice,
      rsDirtyContexts = Set.fromList [LiveRefreshCtx, StaleRefreshCtx],
      rsStoredSections =
        Map.fromList
          [ (LiveRefreshCtx, MiniStalk 0.0),
            (StaleRefreshCtx, MiniStalk 9.0)
          ],
      rsStoredRuntimeCacheIdentity = Nothing,
      rsStoredRuntime = Nothing,
      rsRowsCache = emptyContextRowsCache 1024,
      rsPropagationState = ContextPropagationUnknown
    }

refreshPropagationReport :: RefreshSite -> Maybe (ContextPropagationReport RefreshCtx)
refreshPropagationReport site =
  case rsPropagationState site of
    ContextPropagationSettled report ->
      Just report
    ContextPropagationFailed maybeReport _failure ->
      maybeReport
    ContextPropagationUnknown ->
      Nothing

refreshPropagationFailure ::
  RefreshSite ->
  Maybe (ContextPropagationFailure RefreshCtx (ContextPropagationInvariantFailure RefreshCtx) String)
refreshPropagationFailure site =
  case rsPropagationState site of
    ContextPropagationFailed _maybeReport failureValue ->
      Just failureValue
    ContextPropagationSettled _report ->
      Nothing
    ContextPropagationUnknown ->
      Nothing

tests :: TestTree
tests =
  testGroup
    "context"
    [ testCase "restriction maps compose along comparable contexts" testRestrictionComposition,
      testCase "propagationTargets honor support intersection" testPropagationTargets,
      testCase "context witness laws hold on a coherent chain" testContextWitnessLaws,
      testCase "analysis restriction laws hold on the same chain" testAnalysisWitnessLaws,
      testCase "upperCovers detects the minimal cover in a star lattice" testImmediateChildren,
      testCase "fullDescentCheck is satisfied once generators are visible at the parent" testVisibleSupportDescent,
      testCase "diamond: descent satisfied when children agree" testDiamondDescentSatisfied,
      testCase "diamond: descent obstructed when children disagree" testDiamondDescentObstructed,
      testCase "descent rejects cover sections missing parent members" testVacuousCoverDescent,
      testCase "diamond: witness laws hold on a coherent diamond" testDiamondWitnessLaws,
      testCase "anatomy: upperCovers detects k-ary covers" testAnatomyImmediateChildren,
      testCase "anatomy: descent satisfied when head merge propagates consistently" testAnatomyDescentSatisfied,
      testCase "anatomy: witness laws hold on a hierarchical lattice" testAnatomyWitnessLaws,
      testCase "bootstrapContextSectionsWithBudget discards stale dirty contexts before hydration" testContextRefreshDiscardsStaleDirtyContexts,
      testCase "bootstrapContextSectionsWithBudget surfaces invalid restriction-edge covers" testContextRefreshRejectsInvalidRestrictionEdges,
      testCase "support basis normalizes closures to minimal generators" testSupportBasisCanonicalization,
      testCase "class support index stores explicit local support instead of unioning default" testClassSupportIndexLocalInsert
    ]

testRestrictionComposition :: Assertion
testRestrictionComposition = do
  localToModule <- expectRight (restrictionMap LocalCtx ModuleCtx chainStore)
  moduleToGlobal <- expectRight (restrictionMap ModuleCtx GlobalCtx chainStore)
  localToGlobal <- expectRight (restrictionMap LocalCtx GlobalCtx chainStore)
  let composed =
        IntMap.map
          (\classId -> IntMap.findWithDefault classId classId moduleToGlobal)
          localToModule
  composed @?= localToGlobal
  restrictionMap ModuleCtx LocalCtx chainStore
    @?= Left (PreparedContextRestrictionUnavailable ModuleCtx LocalCtx)

testPropagationTargets :: Assertion
testPropagationTargets =
  expectRight (propagationTargets GlobalCtx 2 3 chainStore)
    >>= (@?= [LocalCtx])

testContextWitnessLaws :: Assertion
testContextWitnessLaws = do
  localToModule <- expectRightJust "expected LocalCtx -> ModuleCtx morphism" (mkContextMorphism chainLattice LocalCtx ModuleCtx)
  moduleToGlobal <- expectRightJust "expected ModuleCtx -> GlobalCtx morphism" (mkContextMorphism chainLattice ModuleCtx GlobalCtx)
  globalId <- expectRightJust "expected GlobalCtx identity morphism" (mkContextMorphism chainLattice GlobalCtx GlobalCtx)
  localToGlobal <- expectRightJust "expected LocalCtx -> GlobalCtx morphism" (mkContextMorphism chainLattice LocalCtx GlobalCtx)
  contextRestrictionIdentity ModuleCtx chainStore @?= Right True
  contextMorphismLeftIdentity (contextRefinesTo chainLattice) localToModule @?= Right True
  contextMorphismRightIdentity (contextRefinesTo chainLattice) localToModule @?= Right True
  contextMorphismAssociative (contextRefinesTo chainLattice) localToModule moduleToGlobal globalId @?= Right True
  contextRestrictionFunctorialAction localToModule moduleToGlobal chainStore @?= Right True
  contextGlobalSectionInvariant localToGlobal chainStore @?= Right True

testAnalysisWitnessLaws :: Assertion
testAnalysisWitnessLaws = do
  localToModule <- expectRightJust "expected LocalCtx -> ModuleCtx morphism" (mkContextMorphism chainLattice LocalCtx ModuleCtx)
  moduleToGlobal <- expectRightJust "expected ModuleCtx -> GlobalCtx morphism" (mkContextMorphism chainLattice ModuleCtx GlobalCtx)
  localToGlobal <- expectRightJust "expected LocalCtx -> GlobalCtx morphism" (mkContextMorphism chainLattice LocalCtx GlobalCtx)
  contextAnalysisRestrictionComposition localToModule moduleToGlobal chainStore @?= Right True
  contextAnalysisGlobalSectionInvariant localToGlobal chainStore @?= Right True

testImmediateChildren :: Assertion
testImmediateChildren =
  expectRight (upperCovers coverLattice RootCtx)
    >>= (@?= [LeftCtx, MiddleCtx, RightCtx])

testVisibleSupportDescent :: Assertion
testVisibleSupportDescent =
  let report = fullDescentCheck supportVisibleStore
   in do
        descentAt RootCtx supportVisibleStore @?= SearchAccepted
        drSatisfied report @?= True
        drObstructionCount report @?= 0

testDiamondDescentSatisfied :: Assertion
testDiamondDescentSatisfied =
  let report = fullDescentCheck diamondSatisfiedStore
   in do
        descentAt BranchBase diamondSatisfiedStore @?= SearchAccepted
        drSatisfied report @?= True
        drObstructionCount report @?= 0

testDiamondDescentObstructed :: Assertion
testDiamondDescentObstructed =
  case descentAt BranchBase diamondObstructedStore of
    SearchAccepted ->
      assertFailure "expected descent obstruction at BranchBase when BranchLeft and BranchRight disagree"
    SearchRejected (DescentMonotonicityObstruction obstructionContext coverContext _parentClass divergentImages missingMembers :| _) -> do
      obstructionContext @?= BranchBase
      coverContext @?= BranchRight
      Set.fromList divergentImages @?= Set.fromList [0, 7]
      missingMembers @?= []
    SearchRejected (_lookupObstruction :| _) ->
      assertFailure "expected a section-monotonicity descent obstruction"
    SearchUndecided {} ->
      assertFailure "unbounded diamond descent should decide"

vacuousCoverStore :: MockStore CoverCtx
vacuousCoverStore =
  supportVisibleStore
    { msClasses =
        Map.insert MiddleCtx IntMap.empty (msClasses supportVisibleStore)
    }

testVacuousCoverDescent :: Assertion
testVacuousCoverDescent =
  case descentAt RootCtx vacuousCoverStore of
    SearchAccepted ->
      assertFailure "expected a missing-member cover obstruction"
    SearchRejected (DescentMonotonicityObstruction obstructionContext coverContext _parentClass divergentImages missingMembers :| _) -> do
      obstructionContext @?= RootCtx
      coverContext @?= MiddleCtx
      divergentImages @?= []
      Set.fromList missingMembers @?= Set.fromList [0, 7]
    SearchRejected obstruction ->
      assertFailure ("expected missing-member cover obstruction, got " <> show obstruction)
    SearchUndecided {} ->
      assertFailure "missing-member descent should decide"

testDiamondWitnessLaws :: Assertion
testDiamondWitnessLaws = do
  leftToBottom <- expectRightJust "expected BranchLeft -> BranchBase morphism" (mkContextMorphism branchContextLattice BranchLeft BranchBase)
  topToLeft <- expectRightJust "expected BranchApex -> BranchLeft morphism" (mkContextMorphism branchContextLattice BranchApex BranchLeft)
  topToBottom <- expectRightJust "expected BranchApex -> BranchBase morphism" (mkContextMorphism branchContextLattice BranchApex BranchBase)
  contextRestrictionIdentity BranchLeft diamondSatisfiedStore @?= Right True
  contextMorphismLeftIdentity (contextRefinesTo branchContextLattice) leftToBottom @?= Right True
  contextMorphismRightIdentity (contextRefinesTo branchContextLattice) leftToBottom @?= Right True
  contextRestrictionFunctorialAction topToLeft leftToBottom diamondSatisfiedStore @?= Right True
  contextGlobalSectionInvariant topToBottom diamondSatisfiedStore @?= Right True

testAnatomyImmediateChildren :: Assertion
testAnatomyImmediateChildren = do
  expectRight (upperCovers anatomyLattice AWhole)
    >>= (@?= [AUpper, ALower])
  expectRight (upperCovers anatomyLattice AUpper)
    >>= (@?= [AHead, ATorso])
  expectRight (upperCovers anatomyLattice ALower)
    >>= (@?= [ALegLeft, ALegRight])

testAnatomyDescentSatisfied :: Assertion
testAnatomyDescentSatisfied =
  let report = fullDescentCheck anatomyStore
   in do
        drSatisfied report @?= True
        drObstructionCount report @?= 0
        descentAt AWhole anatomyStore @?= SearchAccepted
        descentAt AUpper anatomyStore @?= SearchAccepted

testAnatomyWitnessLaws :: Assertion
testAnatomyWitnessLaws = do
  headToUpper <- expectRightJust "expected AHead -> AUpper morphism" (mkContextMorphism anatomyLattice AHead AUpper)
  upperToWhole <- expectRightJust "expected AUpper -> AWhole morphism" (mkContextMorphism anatomyLattice AUpper AWhole)
  headToWhole <- expectRightJust "expected AHead -> AWhole morphism" (mkContextMorphism anatomyLattice AHead AWhole)
  contextRestrictionIdentity AUpper anatomyStore @?= Right True
  contextMorphismAssociative (contextRefinesTo anatomyLattice) headToUpper upperToWhole (identityContextMorphism AWhole) @?= Right True
  contextRestrictionFunctorialAction headToUpper upperToWhole anatomyStore @?= Right True
  contextGlobalSectionInvariant headToWhole anatomyStore @?= Right True
  contextAnalysisRestrictionComposition headToUpper upperToWhole anatomyStore @?= Right True

testContextRefreshDiscardsStaleDirtyContexts :: Assertion
testContextRefreshDiscardsStaleDirtyContexts = do
  let refreshedSite =
        bootstrapContextSectionsWithBudget
          1024
          refreshRuntime
          initialRefreshSite
  rsDirtyContexts refreshedSite @?= Set.empty
  assertBool
    "stale context was not stored"
    (Map.notMember StaleRefreshCtx (rsStoredSections refreshedSite))
  Map.lookup LiveRefreshCtx (rsStoredSections refreshedSite) @?= Just (MiniStalk 1.0)
  case refreshPropagationReport refreshedSite of
    Nothing ->
      assertFailure "refresh did not persist a runtime observation report"
    Just report -> do
      contextPropagationChangedContexts report @?= [LiveRefreshCtx]
      contextPropagationSettled report @?= True
  assertBool
    "refresh should not fail while discarding stale dirty contexts"
    (refreshPropagationFailure refreshedSite == Nothing)

testContextRefreshRejectsInvalidRestrictionEdges :: Assertion
testContextRefreshRejectsInvalidRestrictionEdges = do
  let invalidRuntime =
        refreshRuntime
          { crCachedContexts = const [LiveRefreshCtx, StaleRefreshCtx]
          }
      refreshedSite =
        bootstrapContextSectionsWithBudget
          1024
          invalidRuntime
          initialRefreshSite
  refreshPropagationReport refreshedSite @?= Nothing
  refreshPropagationFailure refreshedSite
    @?= Just
      ( ContextPropagationInvariantViolation
          (ContextPropagationRestrictionRegistryFailed (PreparedContextSiteObjectMissing StaleRefreshCtx))
      )
  assertBool
    "invalid lattice cover must not materialize stale sections"
    (Map.notMember StaleRefreshCtx (rsStoredSections refreshedSite))

testSupportBasisCanonicalization :: Assertion
testSupportBasisCanonicalization = do
  supportGenerators (chainSupport [GlobalCtx, ModuleCtx, LocalCtx]) @?= [GlobalCtx]
  supportGenerators (chainSupport [ModuleCtx, LocalCtx]) @?= [ModuleCtx]
  supportGenerators (coverSupport [RootCtx, LeftCtx, MiddleCtx, RightCtx, FocusCtx]) @?= [RootCtx]
  supportGenerators (coverSupport [LeftCtx, MiddleCtx, RightCtx]) @?= [LeftCtx, MiddleCtx, RightCtx]
  supportGenerators (branchSupport [BranchBase, BranchLeft, BranchRight, BranchApex]) @?= [BranchBase]
  supportGenerators (branchSupport [BranchLeft, BranchRight]) @?= [BranchLeft, BranchRight]
  supportGenerators (anatomySupport [minBound .. maxBound]) @?= [AWhole]

testClassSupportIndexLocalInsert :: Assertion
testClassSupportIndexLocalInsert = do
  let site = fromFiniteLattice chainLattice
      localSupport = chainSupport [ModuleCtx]
  (supportIndex, supportDelta) <-
    expectRight
      (classSupportIndexInsertMany site localSupport (IntSet.singleton 42) (compiledFixtureSupportIndex chainLattice IntMap.empty))
  expectRight (classSupportIndexEntries site supportIndex) >>= (@?= IntMap.singleton 42 localSupport)
  classSupportDeltaTouchedClassKeys supportDelta @?= IntSet.singleton 42
  expectRight (preparedSupportReachableObjects site (Set.fromList [GlobalCtx, ModuleCtx, LocalCtx]) localSupport)
    >>= (@?= Set.fromList [ModuleCtx, LocalCtx])

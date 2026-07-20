{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.EGraph.Saturation.CohomologicalSpec.Fixture.Section
  ( TestF (..),
    TestTag (..),
    TestScope (..),
    testScopeLattice,
    analysisSpec,
    litTerm,
    pairTerm,
    withTestTerms,
    withTwoTestTerms,
    withThreeTestTerms,
    withFiveTestTerms,
    mkFineRegion,
    propertyContext,
    queryVar0,
    queryVar1,
    exactWitnessPolicy,
    pairWitnessRewriteSystem,
    runFullMatchingQuery,
    mkExactWitnessBackend,
    mkBackendWithRewriteSystem,
    mkExactWitnessBackendWithRewriteSystem,
    compileQuery,
    compileGuardedQuery,
    mkMatchingRequest,
    mkMatchingWorld,
    withBackendResolution
  )
where

import Moonlight.Pale.Ghc.Expr (ScopeCtx)
import Data.IntSet qualified as IntSet
import Data.Set qualified as Set
import Moonlight.Core (HasConstructorTag (..), zipSameNodeShape)
import Moonlight.Core qualified as EGraph
import Moonlight.Delta.Scope qualified as Delta
import Moonlight.EGraph.Introspection.Analysis.Resolution (ResolutionBundle)
import Moonlight.EGraph.Introspection.Core.Rewrite
  ( PatternRewriteError,
    RewriteSystem,
    mkRewriteSystem,
    rewriteMorphismWithInterface
  )
import Moonlight.EGraph.Pure.Analysis (AnalysisSpec (..))
import Moonlight.EGraph.Pure.Saturation.Matching
  ( EGraphMatchingObstruction,
    MatchingAlgebra,
    MatchingRequest,
    MatchingWorld
  )
import Moonlight.EGraph.Pure.Types
  ( ClassId,
    EGraph,
    classIdKey,
    emptyEGraph
  )
import Moonlight.EGraph.Pure.Change (EGraphMutationResult (..))
import Moonlight.EGraph.Pure.Kernel.HashCons (insertTermsTracked)
import Moonlight.EGraph.Saturation.Cohomological.Backend.Instance
  ( CohomologicalBackend,
    mkCohomologicalBackend,
    withRewriteSystemWitness
  )
import Moonlight.EGraph.Saturation.Cohomological.Backend.Instance.Internal.Prepared
  ( cohomologicalBackendResolutionBundle,
  )
import Moonlight.EGraph.Saturation.Cohomological.Types
  ( PatternOccurrence (..),
    EGraphSectionCertification,
    cachePolicyFromEnvironmentFingerprint
  )
import Data.Fix (Fix (..))
import Moonlight.Core
import Moonlight.Core (Substitution)
import Moonlight.Rewrite.Algebra
  ( CompiledPatternQuery,
    compilePatternQuery,
    guardedPatternQuery,
    singlePatternQuery
  )
import Moonlight.Rewrite.System
  ( CompiledGuard,
    RewriteCondition,
    combineCompiledGuards,
    compileGuard,
    emptyGuardCapabilityResolver
  )
import Moonlight.Rewrite.System (emptyFactDerivationIndex)
import Moonlight.Rewrite.System (emptyFactStore)
import Moonlight.Saturation.Matching qualified as GenericMatching
import Moonlight.Sheaf.Obstruction
  ( CandidateRegion (crRoot),
    CandidateStalk (..),
    CohomologicalPolicy,
    CohomologicalProfile (ExactWitnessProfile),
    OccurrenceId (..),
    RegionScale (FineRegion),
    emptyCapabilityLabelAlgebra,
    emptyTypedCapabilityEnvironment,
    mkCandidateRegion,
    mkSectionCertificationAlgebraWithCachePolicy,
    profilePolicy,
    regionCarrierPlanFromList
  )
import Moonlight.Algebra
  ( BoundedJoinSemilattice (..),
    BoundedMeetSemilattice (..),
    JoinSemilattice (..),
    Lattice,
    MeetSemilattice (..)
  )
import Numeric.Natural (Natural)
import Test.Tasty.HUnit (Assertion, assertFailure)
import Moonlight.FiniteLattice
  ( ContextLattice,
    latticeContext
  )

data TestF a
  = Lit Int
  | Pair a a
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

data TestTag
  = LitTag
  | PairTag
  deriving stock (Eq, Ord, Show)

instance HasConstructorTag TestF where
  type ConstructorTag TestF = TestTag

  constructorTag testValue =
    case testValue of
      Lit {} -> LitTag
      Pair {} -> PairTag

instance ZipMatch TestF where
  zipMatch =
    zipSameNodeShape

data TestScope
  = GlobalScope
  | LocalScope
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance JoinSemilattice TestScope where
  join =
    max

instance BoundedJoinSemilattice TestScope where
  bottom =
    GlobalScope

instance MeetSemilattice TestScope where
  meet =
    min

instance BoundedMeetSemilattice TestScope where
  top =
    LocalScope

instance Lattice TestScope

testScopeLattice :: ContextLattice TestScope
testScopeLattice =
  case latticeContext of
    Right latticeValue -> latticeValue
    Left compileError ->
      error ("invalid TestScope lattice fixture: " <> show compileError)

analysisSpec :: AnalysisSpec TestF ()
analysisSpec =
  AnalysisSpec
    { asMake = const (),
      asJoin = \_ _ -> (),
      asJoinChanged = \_ _ -> ((), False)
    }

litTerm :: Int -> Fix TestF
litTerm value =
  Fix (Lit value)

pairTerm :: Fix TestF -> Fix TestF -> Fix TestF
pairTerm leftTerm rightTerm =
  Fix (Pair leftTerm rightTerm)

withTestTerms ::
  [Fix TestF] ->
  ([ClassId] -> EGraph TestF () -> Assertion) ->
  Assertion
withTestTerms terms useGraph = do
  mutation <-
    assertRight
      "test graph allocation failed"
      (insertTermsTracked terms (emptyEGraph analysisSpec))
  useGraph (emrResult mutation) (emrGraph mutation)

withTwoTestTerms ::
  Fix TestF ->
  Fix TestF ->
  (ClassId -> ClassId -> EGraph TestF () -> Assertion) ->
  Assertion
withTwoTestTerms firstTerm secondTerm useGraph =
  withTestTerms [firstTerm, secondTerm] $ \classIds graphValue ->
    case classIds of
      [firstClassId, secondClassId] ->
        useGraph firstClassId secondClassId graphValue
      _ ->
        assertFailure "two-term test graph returned the wrong number of class ids"

withThreeTestTerms ::
  Fix TestF ->
  Fix TestF ->
  Fix TestF ->
  (ClassId -> ClassId -> ClassId -> EGraph TestF () -> Assertion) ->
  Assertion
withThreeTestTerms firstTerm secondTerm thirdTerm useGraph =
  withTestTerms [firstTerm, secondTerm, thirdTerm] $ \classIds graphValue ->
    case classIds of
      [firstClassId, secondClassId, thirdClassId] ->
        useGraph firstClassId secondClassId thirdClassId graphValue
      _ ->
        assertFailure "three-term test graph returned the wrong number of class ids"

withFiveTestTerms ::
  Fix TestF ->
  Fix TestF ->
  Fix TestF ->
  Fix TestF ->
  Fix TestF ->
  (ClassId -> ClassId -> ClassId -> ClassId -> ClassId -> EGraph TestF () -> Assertion) ->
  Assertion
withFiveTestTerms firstTerm secondTerm thirdTerm fourthTerm fifthTerm useGraph =
  withTestTerms [firstTerm, secondTerm, thirdTerm, fourthTerm, fifthTerm] $ \classIds graphValue ->
    case classIds of
      [firstClassId, secondClassId, thirdClassId, fourthClassId, fifthClassId] ->
        useGraph firstClassId secondClassId thirdClassId fourthClassId fifthClassId graphValue
      _ ->
        assertFailure "five-term test graph returned the wrong number of class ids"

assertRight :: Show obstruction => String -> Either obstruction value -> IO value
assertRight failureLabel =
  either
    (assertFailure . ((failureLabel <> ": ") <>) . show)
    pure

mkFineRegion :: ClassId -> Int -> CandidateRegion ClassId
mkFineRegion rootClass fingerprintValue =
  mkCandidateRegion
    rootClass
    (IntSet.singleton (classIdKey rootClass))
    0
    FineRegion
    fingerprintValue

propertyContext ::
  ClassId ->
  ClassId ->
  EGraphSectionCertification owner c TestF
propertyContext leftRoot rightRoot =
  mkSectionCertificationAlgebraWithCachePolicy
    (emptyTypedCapabilityEnvironment emptyCapabilityLabelAlgebra)
    ( \patternValue ->
        [ PatternOccurrence
            { poId = OccurrenceId 0,
              poPath = [],
              poPattern = patternValue,
              poBoundVariable = Just queryVar0
            }
        ]
    )
    ( \_ _ ->
        regionCarrierPlanFromList
          [ mkFineRegion leftRoot 401,
            mkFineRegion rightRoot 402
          ]
    )
    (\_ _ _ -> [])
    ( \_ _ regionValue ->
        CandidateStalk
          (IntSet.singleton (classIdKey (crRoot regionValue)))
    )
    ( \_ _ _ ->
        CandidateStalk
          (IntSet.fromList [classIdKey leftRoot, classIdKey rightRoot])
    )
    (const 17)
    (\request -> cachePolicyFromEnvironmentFingerprint (GenericMatching.qrSite request) Nothing)

queryVar0 :: EGraph.PatternVar
queryVar0 =
  EGraph.mkPatternVar 0

queryVar1 :: EGraph.PatternVar
queryVar1 =
  EGraph.mkPatternVar 1

exactWitnessPolicy :: CohomologicalPolicy
exactWitnessPolicy =
  profilePolicy ExactWitnessProfile

pairWitnessRewriteSystem :: Either (PatternRewriteError TestF) (RewriteSystem TestF)
pairWitnessRewriteSystem =
  let variable = queryVar0
      expandedPattern =
        PatternNode (Pair (PatternVar variable) (PatternNode (Lit 0)))
   in fmap
        mkRewriteSystem
        ( sequenceA
            [ rewriteMorphismWithInterface "expand-pair" (PatternVar variable) (Set.singleton variable) expandedPattern Nothing Nothing,
              rewriteMorphismWithInterface "shrink-pair" expandedPattern (Set.singleton variable) (PatternVar variable) Nothing Nothing
            ]
        )

runFullMatchingQuery ::
  MatchingAlgebra state owner c ScopeCtx f a ->
  state ->
  MatchingWorld owner c ScopeCtx f a ->
  MatchingRequest owner c ScopeCtx f a ->
  Either EGraphMatchingObstruction (state, [(ClassId, Substitution)])
runFullMatchingQuery algebra state matchingWorld request =
  let (preparedState, matchingFrontier) =
        GenericMatching.prepareSingleQuery algebra state Delta.fullDelta matchingWorld request
   in case GenericMatching.runSingleQuery algebra preparedState matchingWorld matchingFrontier request of
        (_, Left obstruction) ->
          Left obstruction
        (nextState, Right matches) ->
          Right (nextState, matches)

mkExactWitnessBackend ::
  EGraphSectionCertification owner c TestF ->
  CohomologicalBackend owner c TestF
mkExactWitnessBackend context =
  mkCohomologicalBackend context exactWitnessPolicy

mkBackendWithRewriteSystem ::
  RewriteSystem TestF ->
  EGraphSectionCertification owner c TestF ->
  CohomologicalPolicy ->
  CohomologicalBackend owner c TestF
mkBackendWithRewriteSystem rewriteSystem context policy =
  withRewriteSystemWitness rewriteSystem (mkCohomologicalBackend context policy)

mkExactWitnessBackendWithRewriteSystem ::
  RewriteSystem TestF ->
  EGraphSectionCertification owner c TestF ->
  CohomologicalBackend owner c TestF
mkExactWitnessBackendWithRewriteSystem rewriteSystem context =
  mkBackendWithRewriteSystem rewriteSystem context exactWitnessPolicy

compileQuery :: Pattern TestF -> Either [EGraph.PatternVar] (CompiledPatternQuery (CompiledGuard ScopeCtx TestF) TestF)
compileQuery patternValue =
  compilePatternQuery combineCompiledGuards compileGuard (singlePatternQuery patternValue)

compileGuardedQuery :: Pattern TestF -> RewriteCondition ScopeCtx TestF -> Either [EGraph.PatternVar] (CompiledPatternQuery (CompiledGuard ScopeCtx TestF) TestF)
compileGuardedQuery patternValue guardValue =
  compilePatternQuery combineCompiledGuards compileGuard (guardedPatternQuery (singlePatternQuery patternValue) guardValue)

mkMatchingRequest :: CompiledPatternQuery (CompiledGuard ScopeCtx TestF) TestF -> MatchingRequest owner c ScopeCtx TestF a
mkMatchingRequest compiledQuery =
  GenericMatching.QueryRequest
    { GenericMatching.qrSite = GenericMatching.BaseSite,
      GenericMatching.qrSnapshot = Nothing,
      GenericMatching.qrQuery = compiledQuery,
      GenericMatching.qrPurpose = GenericMatching.RawMatchPurpose
    }

mkMatchingWorld :: EGraph TestF a -> MatchingWorld owner c ScopeCtx TestF a
mkMatchingWorld graphValue =
  GenericMatching.MatchWorld
    { GenericMatching.mwGraph = graphValue,
      GenericMatching.mwFacts = emptyFactStore,
      GenericMatching.mwFactDerivations = emptyFactDerivationIndex,
      GenericMatching.mwCapabilities = emptyGuardCapabilityResolver,
      GenericMatching.mwProofContext = Nothing,
      GenericMatching.mwIteration = 0
    }

withBackendResolution :: Natural -> CohomologicalBackend owner c TestF -> (ResolutionBundle TestF -> Assertion) -> Assertion
withBackendResolution depthValue backend continuation =
  case cohomologicalBackendResolutionBundle depthValue backend of
    Left homologyFailure ->
      assertFailure ("expected backend resolution to build, got: " <> show homologyFailure)
    Right Nothing ->
      assertFailure "expected rewrite-system witness to produce a cached resolution bundle"
    Right (Just resolutionValue) ->
      continuation resolutionValue

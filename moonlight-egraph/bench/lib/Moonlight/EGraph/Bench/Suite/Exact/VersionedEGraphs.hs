{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Direct replay of a serialized action log emitted by the published
-- Versioned E-Graphs parameter-analysis generator.
module Moonlight.EGraph.Bench.Suite.Exact.VersionedEGraphs
  ( parseVersionedParameterTrace,
    versionedEGraphBenchmarks,
  )
where

import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List (find)
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Moonlight.Core
  ( ClassId,
    UnionFindAllocationError,
    ZipMatch (..),
    classIdKey,
    zipSameNodeShape,
  )
import Moonlight.EGraph.Bench.Corpus (resolveBenchmarkFixturePath)
import Moonlight.EGraph.Bench.Harness.Digest (contextGraphDigest)
import Moonlight.EGraph.Bench.Harness.Run (abortBench, requireRight)
import Moonlight.EGraph.Pure.Analysis (AnalysisSpec (..))
import Moonlight.EGraph.Pure.Context
  ( ContextEGraph,
    contextMerge,
    contextRepresentativeAt,
    emptyContextEGraphFromSite,
  )
import Moonlight.EGraph.Pure.Context.Core
  ( cegBase,
    cegContextRevision,
  )
import Moonlight.EGraph.Pure.Kernel.HashCons (addENode)
import Moonlight.EGraph.Pure.Types
  ( EGraph,
    ENode (..),
    eGraphClassCount,
    eGraphNodeCount,
    emptyEGraph,
  )
import Moonlight.Sheaf.Context.Site
  ( PreparedContextSite,
    fromPowersetAtoms,
  )
import Test.Tasty.Bench
  ( Benchmark,
    bench,
    bgroup,
    env,
    nf,
  )
import Text.Read (readMaybe)

versionedEGraphBenchmarks :: Benchmark
versionedEGraphBenchmarks =
  bgroup
    "versioned-egraphs"
    [ env prepareVersionedParameterFixture $ \fixture ->
        bgroup
          "parameter-analysis/N=1/A=5/V=32/U=32/F=32/S=1234"
          [bench "moonlight-context" (nf replayVersionedParameterFixture fixture)]
    ]

data ParameterF child = ParameterNode !Int ![child]
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance ZipMatch ParameterF where
  zipMatch =
    zipSameNodeShape

data ParameterAction
  = ParameterAdd !Int ![Int]
  | ParameterUnion !Int !Int
  | ParameterFind !Int
  | ParameterCheckout !Int
  | ParameterBranchout
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data LocatedParameterAction = LocatedParameterAction
  { lpaLine :: !Int,
    lpaAction :: !ParameterAction
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

newtype ParameterTrace = ParameterTrace
  { unParameterTrace :: [LocatedParameterAction]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data ParameterTraceShape = ParameterTraceShape
  { ptsActions :: !Int,
    ptsElements :: !Int,
    ptsVersions :: !Int,
    ptsUnions :: !Int,
    ptsFinds :: !Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data ParameterTraceObstruction
  = ParameterMalformedAction !FilePath !Int !String
  | ParameterInvalidIndex !FilePath !Int !String
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data ParameterReplayObstruction
  = ParameterAddOutsidePrefix !Int
  | ParameterElementMissing !Int !Int
  | ParameterVersionMissing !Int !Int
  | ParameterSitePreparationFailed !String
  | ParameterContextMergeFailed !Int !String
  | ParameterContextQueryFailed !Int !String
  | ParameterClassIdAllocationFailed !UnionFindAllocationError
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data ParameterReplayOutcome = ParameterReplayOutcome
  { proDigest :: !Int,
    proNodeCount :: !Int,
    proClassCount :: !Int,
    proVersionCount :: !Int,
    proUnionCount :: !Int,
    proFindCount :: !Int,
    proContextRevision :: !Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data VersionedParameterFixture = VersionedParameterFixture
  { vpfTrace :: !ParameterTrace,
    vpfShape :: !ParameterTraceShape,
    vpfValidatedOutcome :: !ParameterReplayOutcome
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

type VersionContext = Set Int

data ParameterBuildState = ParameterBuildState
  { pbsGraph :: !(EGraph ParameterF ()),
    pbsClasses :: !(IntMap ClassId),
    pbsNextElement :: !Int
  }

data ParameterReplayState = ParameterReplayState
  { prsGraph :: !(ContextEGraph ParameterF () VersionContext),
    prsVersions :: !(IntMap VersionContext),
    prsCheckout :: !VersionContext,
    prsNextAtom :: !Int,
    prsFindDigest :: !Int
  }

parameterTraceFixturePath :: FilePath
parameterTraceFixturePath =
  "bench/fixtures/versioned-egraphs/parameter-N1-A5-V32-U32-F32-S1234.trace"

expectedParameterTraceShape :: ParameterTraceShape
expectedParameterTraceShape =
  ParameterTraceShape
    { ptsActions = 193,
      ptsElements = 1,
      ptsVersions = 33,
      ptsUnions = 32,
      ptsFinds = 32
    }

prepareVersionedParameterFixture :: IO VersionedParameterFixture
prepareVersionedParameterFixture = do
  path <- resolveBenchmarkFixturePath parameterTraceFixturePath
  source <- readFile path
  trace <- requireRight "versioned-egraphs parameter trace parse" (parseVersionedParameterTrace path source)
  let shape = parameterTraceShape trace
  if shape == expectedParameterTraceShape
    then pure ()
    else
      abortBench
        ( "versioned-egraphs parameter trace shape mismatch: expected "
            <> show expectedParameterTraceShape
            <> ", observed "
            <> show shape
        )
  outcome <-
    requireRight
      "versioned-egraphs Moonlight replay validation"
      (replayVersionedParameterTrace trace)
      >>= evaluate . force
  let fixture =
        VersionedParameterFixture
          { vpfTrace = trace,
            vpfShape = shape,
            vpfValidatedOutcome = outcome
          }
  putStrLn
    ( "versioned-egraphs-parameter "
        <> show shape
        <> " outcome="
        <> show outcome
    )
  evaluate (force fixture)

replayVersionedParameterFixture ::
  VersionedParameterFixture ->
  Either ParameterReplayObstruction ParameterReplayOutcome
replayVersionedParameterFixture =
  replayVersionedParameterTrace . vpfTrace

parseVersionedParameterTrace ::
  FilePath ->
  String ->
  Either ParameterTraceObstruction ParameterTrace
parseVersionedParameterTrace path source =
  ParameterTrace
    <$> traverse
      (parseLocatedParameterAction path)
      (zip [1 ..] (lines source))

parseLocatedParameterAction ::
  FilePath ->
  (Int, String) ->
  Either ParameterTraceObstruction LocatedParameterAction
parseLocatedParameterAction path (lineNumber, sourceLine) =
  fmap (LocatedParameterAction lineNumber) $
    case words sourceLine of
      "add" : operator : arguments ->
        ParameterAdd
          <$> parseParameterIndex path lineNumber operator
          <*> traverse (parseParameterIndex path lineNumber) arguments
      ["union", left, right] ->
        ParameterUnion
          <$> parseParameterIndex path lineNumber left
          <*> parseParameterIndex path lineNumber right
      ["find", element] ->
        ParameterFind <$> parseParameterIndex path lineNumber element
      ["checkout", version] ->
        ParameterCheckout <$> parseParameterIndex path lineNumber version
      ["branchout"] ->
        Right ParameterBranchout
      _ ->
        Left (ParameterMalformedAction path lineNumber sourceLine)

parseParameterIndex ::
  FilePath ->
  Int ->
  String ->
  Either ParameterTraceObstruction Int
parseParameterIndex path lineNumber source =
  case readMaybe source of
    Just value
      | value >= 0 -> Right value
    _ -> Left (ParameterInvalidIndex path lineNumber source)

parameterTraceShape :: ParameterTrace -> ParameterTraceShape
parameterTraceShape (ParameterTrace actions) =
  foldl' accumulateParameterTraceShape initialParameterTraceShape actions

initialParameterTraceShape :: ParameterTraceShape
initialParameterTraceShape =
  ParameterTraceShape
    { ptsActions = 0,
      ptsElements = 0,
      ptsVersions = 1,
      ptsUnions = 0,
      ptsFinds = 0
    }

accumulateParameterTraceShape ::
  ParameterTraceShape ->
  LocatedParameterAction ->
  ParameterTraceShape
accumulateParameterTraceShape shape locatedAction =
  case lpaAction locatedAction of
    ParameterAdd {} ->
      shape
        { ptsActions = ptsActions shape + 1,
          ptsElements = ptsElements shape + 1
        }
    ParameterUnion {} ->
      shape
        { ptsActions = ptsActions shape + 1,
          ptsUnions = ptsUnions shape + 1
        }
    ParameterFind {} ->
      shape
        { ptsActions = ptsActions shape + 1,
          ptsFinds = ptsFinds shape + 1
        }
    ParameterCheckout {} ->
      shape {ptsActions = ptsActions shape + 1}
    ParameterBranchout ->
      shape
        { ptsActions = ptsActions shape + 1,
          ptsVersions = ptsVersions shape + 1
        }

replayVersionedParameterTrace ::
  ParameterTrace ->
  Either ParameterReplayObstruction ParameterReplayOutcome
replayVersionedParameterTrace trace@(ParameterTrace actions) = do
  let (addActions, replayActions) = span (isParameterAdd . lpaAction) actions
  case find (isParameterAdd . lpaAction) replayActions of
    Just lateAdd -> Left (ParameterAddOutsidePrefix (lpaLine lateAdd))
    Nothing -> pure ()
  buildState <- foldM applyParameterAdd initialParameterBuildState addActions
  let shape = parameterTraceShape trace
      branchCount = ptsVersions shape - 1
  site <-
    first
      (ParameterSitePreparationFailed . show)
      (fromPowersetAtoms [0 .. branchCount - 1])
  replayState <-
    foldM
      (applyParameterReplayAction (pbsClasses buildState))
      (initialParameterReplayState site (pbsGraph buildState))
      replayActions
  pure (parameterReplayOutcome shape replayState)

isParameterAdd :: ParameterAction -> Bool
isParameterAdd action =
  case action of
    ParameterAdd {} -> True
    _ -> False

initialParameterBuildState :: ParameterBuildState
initialParameterBuildState =
  ParameterBuildState
    { pbsGraph = emptyEGraph parameterAnalysisSpec,
      pbsClasses = IntMap.empty,
      pbsNextElement = 0
    }

applyParameterAdd ::
  ParameterBuildState ->
  LocatedParameterAction ->
  Either ParameterReplayObstruction ParameterBuildState
applyParameterAdd state locatedAction =
  case lpaAction locatedAction of
    ParameterAdd operator arguments -> do
      childClasses <-
        traverse
          (lookupParameterElement (lpaLine locatedAction) (pbsClasses state))
          arguments
      (classId, nextGraph) <-
        first ParameterClassIdAllocationFailed $
          addENode
              (ENode (ParameterNode operator childClasses))
              ()
              (pbsGraph state)
      let elementIndex = pbsNextElement state
      pure
        ParameterBuildState
          { pbsGraph = nextGraph,
            pbsClasses = IntMap.insert elementIndex classId (pbsClasses state),
            pbsNextElement = elementIndex + 1
          }
    _ -> Left (ParameterAddOutsidePrefix (lpaLine locatedAction))

initialParameterReplayState ::
  PreparedContextSite VersionContext ->
  EGraph ParameterF () ->
  ParameterReplayState
initialParameterReplayState site baseGraph =
  ParameterReplayState
    { prsGraph = emptyContextEGraphFromSite site baseGraph,
      prsVersions = IntMap.singleton 0 Set.empty,
      prsCheckout = Set.empty,
      prsNextAtom = 0,
      prsFindDigest = 0
    }

applyParameterReplayAction ::
  IntMap ClassId ->
  ParameterReplayState ->
  LocatedParameterAction ->
  Either ParameterReplayObstruction ParameterReplayState
applyParameterReplayAction classes state locatedAction =
  case lpaAction locatedAction of
    ParameterAdd {} ->
      Left (ParameterAddOutsidePrefix (lpaLine locatedAction))
    ParameterCheckout versionIndex -> do
      contextValue <-
        maybe
          (Left (ParameterVersionMissing (lpaLine locatedAction) versionIndex))
          Right
          (IntMap.lookup versionIndex (prsVersions state))
      pure state {prsCheckout = contextValue}
    ParameterBranchout ->
      let nextVersionIndex = IntMap.size (prsVersions state)
          childContext = Set.insert (prsNextAtom state) (prsCheckout state)
       in Right
            state
              { prsVersions = IntMap.insert nextVersionIndex childContext (prsVersions state),
                prsCheckout = childContext,
                prsNextAtom = prsNextAtom state + 1
              }
    ParameterUnion leftIndex rightIndex -> do
      leftClass <- lookupParameterElement (lpaLine locatedAction) classes leftIndex
      rightClass <- lookupParameterElement (lpaLine locatedAction) classes rightIndex
      nextGraph <-
        first
          (ParameterContextMergeFailed (lpaLine locatedAction) . show)
          (contextMerge (prsCheckout state) leftClass rightClass (prsGraph state))
      pure state {prsGraph = nextGraph}
    ParameterFind elementIndex -> do
      classId <- lookupParameterElement (lpaLine locatedAction) classes elementIndex
      representative <-
        first
          (ParameterContextQueryFailed (lpaLine locatedAction) . show)
          (contextRepresentativeAt (prsCheckout state) classId (prsGraph state))
      pure
        state
          { prsFindDigest =
              (16777619 * prsFindDigest state) + classIdKey representative
          }

lookupParameterElement ::
  Int ->
  IntMap ClassId ->
  Int ->
  Either ParameterReplayObstruction ClassId
lookupParameterElement lineNumber classes elementIndex =
  maybe
    (Left (ParameterElementMissing lineNumber elementIndex))
    Right
    (IntMap.lookup elementIndex classes)

parameterReplayOutcome ::
  ParameterTraceShape ->
  ParameterReplayState ->
  ParameterReplayOutcome
parameterReplayOutcome shape state =
  let contextGraph = prsGraph state
      baseGraph = cegBase contextGraph
   in ParameterReplayOutcome
        { proDigest = contextGraphDigest contextGraph + prsFindDigest state,
          proNodeCount = eGraphNodeCount baseGraph,
          proClassCount = eGraphClassCount baseGraph,
          proVersionCount = IntMap.size (prsVersions state),
          proUnionCount = ptsUnions shape,
          proFindCount = ptsFinds shape,
          proContextRevision = fromIntegral (cegContextRevision contextGraph)
        }

parameterAnalysisSpec :: AnalysisSpec ParameterF ()
parameterAnalysisSpec =
  AnalysisSpec
    { asMake = const (),
      asJoin = \_ _ -> (),
      asJoinChanged = \_ _ -> ((), False)
    }

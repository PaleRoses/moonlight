{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Public
  ( publicBenchmarkPreflight,
    publicBenchmarks,
  )
where

import Control.Monad (void)
import Control.DeepSeq (NFData (..))
import Data.Foldable (traverse_)
import Data.Maybe (listToMaybe)
import Moonlight.Rewrite
  ( ApplyResult (..),
    ApplyStatus (..),
    ClassId,
    ContextName,
    Engine,
    Extracted,
    ExtractError,
    Host,
    Match,
    MatchQuery (..),
    NoGuardAtom,
    Program,
    RelationalProgramError (..),
    RewriteTarget (..),
    RuleName,
    SaturationRound (..),
    SaturationResult (..),
    Term,
    apply,
    at,
    bind,
    compile,
    context,
    contextName,
    defaultApplyConfig,
    defaultSaturationConfig,
    emptyHost,
    engineHost,
    extension,
    extract,
    extractedCost,
    forall_,
    hostClassCount,
    hostFromTerm,
    hostFromTerms,
    match,
    prepare,
    program,
    removeContext,
    requires_,
    rule,
    mkRuleName,
    saturate,
    setContext,
    sortWitness,
    symbolToken,
    var,
    (==>),
  )
import Common
  ( BenchSig,
    benchCost,
    benchProgram,
    benchSizes,
    benchTerms,
    caseLabel,
    eitherWeight,
    expectBench,
    expectMaybeBench,
    leaf,
    nestedTerm,
    wrap,
  )
import Test.Tasty.Bench
  ( Benchmark,
    bench,
    bgroup,
    env,
    nf,
  )

data PipelineFixture = PipelineFixture
  { pipelineProgram :: !(Program BenchSig NoGuardAtom),
    pipelineHost :: !(Host BenchSig),
    pipelineRoots :: ![ClassId],
    pipelineEngine :: !(Engine BenchSig NoGuardAtom),
    pipelineMatchedEngine :: !(Engine BenchSig NoGuardAtom),
    pipelineMatch :: !Match,
    pipelineRuleName :: !RuleName
  }

data ContextFixture = ContextFixture
  { contextNameValue :: !ContextName,
    contextUpdatedHost :: !(Host BenchSig),
    contextBaseEngine :: !(Engine BenchSig NoGuardAtom),
    contextEngine :: !(Engine BenchSig NoGuardAtom),
    contextRuleName :: !RuleName
  }

instance NFData PipelineFixture where
  rnf fixture =
    pipelineProgram fixture
      `seq` pipelineHost fixture
      `seq` pipelineRoots fixture
      `seq` pipelineEngine fixture
      `seq` pipelineMatchedEngine fixture
      `seq` pipelineMatch fixture
      `seq` pipelineRuleName fixture
      `seq` ()

instance NFData ContextFixture where
  rnf fixture =
    contextNameValue fixture
      `seq` contextUpdatedHost fixture
      `seq` contextBaseEngine fixture
      `seq` contextEngine fixture
      `seq` contextRuleName fixture
      `seq` ()

publicBenchmarks :: Benchmark
publicBenchmarks =
  bgroup
    "public"
    [ bgroup "host" (benchSizes >>= hostBenchmarksForSize),
      env publicFixture $ \fixture ->
        bgroup
          "pipeline"
          (pipelineBenchmarks "" fixture <> [bench "extract" (nf extractWeight fixture)]),
      env contextFixture $ \fixture ->
        bgroup
          "context"
          [ bench "setContext/update" (nf setContextUpdateWeight fixture),
            bench "match/context" (nf contextMatchWeight fixture),
            bench "saturate/context" (nf contextSaturateWeight fixture),
            bench "removeContext/verified" (nf removeContextVerifiedWeight fixture)
          ],
      env applicationConditionFixture $ \fixture ->
        bgroup "application-condition" (pipelineBenchmarks " requires-extension" fixture)
    ]

publicBenchmarkPreflight :: IO ()
publicBenchmarkPreflight = do
  traverse_ hostSizePreflight benchSizes
  publicPipelineFixture <- publicFixture
  applicationConditionPipelineFixture <- applicationConditionFixture
  contextPipelineFixture <- contextFixture
  sequence_
    [ pipelinePreflight "public" publicPipelineFixture,
      extractPreflight publicPipelineFixture,
      pipelinePreflight "application-condition" applicationConditionPipelineFixture,
      contextPreflight contextPipelineFixture
    ]

hostSizePreflight :: Int -> IO ()
hostSizePreflight size =
  sequence_
    [ void (expectBench (caseLabel "hostFromTerm" size) (hostFromTerm 0 (nestedTerm size))),
      void (expectBench (caseLabel "hostFromTerms" size) (hostFromTerms 0 (benchTerms size)))
    ]

pipelinePreflight :: String -> PipelineFixture -> IO ()
pipelinePreflight label fixture =
  sequence_
    [ void
        ( expectBench
            (label <> " compile/prepare/cold-match")
            ( compile (pipelineProgram fixture)
                >>= matchRule RewriteBase (pipelineRuleName fixture) Nothing
                  . (`prepare` pipelineHost fixture)
            )
        ),
      void
        ( expectBench
            (label <> " match")
            (matchRule RewriteBase (pipelineRuleName fixture) Nothing (pipelineEngine fixture))
        ),
      applyPreflight label fixture,
      void
        ( expectBench
            (label <> " saturate")
            (saturate RewriteBase defaultSaturationConfig (pipelineEngine fixture))
        )
    ]

applyPreflight :: String -> PipelineFixture -> IO ()
applyPreflight label fixture =
  expectBench
    (label <> " apply")
    (apply defaultApplyConfig (pipelineMatch fixture) (pipelineMatchedEngine fixture))
    >>= \(_engineValue, applyResult) ->
      case applyResultStatus applyResult of
        ApplyRejected rejection ->
          fail (label <> " apply rejected: " <> show rejection)
        ApplyExecuted _ _ ->
          pure ()

extractPreflight :: PipelineFixture -> IO ()
extractPreflight fixture =
  expectMaybeBench "public extract root" (listToMaybe (pipelineRoots fixture))
    >>= \rootClass ->
      void
        ( expectBench
            "public extract"
            (extract (sortWitness @"Expr") benchCost rootClass (pipelineHost fixture) :: Either ExtractError (Extracted BenchSig Int "Expr"))
        )

contextPreflight :: ContextFixture -> IO ()
contextPreflight fixture =
  sequence_
    [ void
        ( expectBench
            "context setContext/update"
            (matchContextRule fixture (setContext (contextNameValue fixture) (contextUpdatedHost fixture) (contextBaseEngine fixture)))
        ),
      void (expectBench "context match" (matchContextRule fixture (contextEngine fixture))),
      void
        ( expectBench
            "context saturate"
            (saturate (RewriteContext (contextNameValue fixture)) defaultSaturationConfig (contextEngine fixture))
        ),
      void
        (expectMaybeBench "context removeContext/verified" (removeContextVerifiedWeight fixture))
    ]

pipelineBenchmarks :: String -> PipelineFixture -> [Benchmark]
pipelineBenchmarks suffix fixture =
  [ bench ("compile/prepare/cold-match" <> suffix) (nf compilePrepareWeight fixture),
    bench ("match" <> suffix) (nf matchWeight fixture),
    bench ("apply/single-step" <> suffix) (nf applyWeight fixture),
    bench ("saturate" <> suffix) (nf baseSaturateWeight fixture)
  ]

hostBenchmarksForSize :: Int -> [Benchmark]
hostBenchmarksForSize size =
  [ bench (caseLabel "hostFromTerm" size) (nf hostFromTermWeight size),
    bench (caseLabel "hostFromTerms" size) (nf hostFromTermsWeight size)
  ]

publicFixture :: IO PipelineFixture
publicFixture =
  pipelineFixture "public" benchProgram (hostFromTerms 0 (benchTerms 16)) "unwrap"

applicationConditionFixture :: IO PipelineFixture
applicationConditionFixture =
  pipelineFixture
    "application-condition"
    applicationConditionProgram
    (fmap (\(host, root) -> (host, [root])) (hostFromTerm 20 (wrap (leaf 0))))
    "requires.leaf"

pipelineFixture ::
  Show errorValue =>
  String ->
  Program BenchSig NoGuardAtom ->
  Either errorValue (Host BenchSig, [ClassId]) ->
  String ->
  IO PipelineFixture
pipelineFixture label sourceProgram hostResult ruleRawName = do
  (host, roots) <- expectBench (label <> " host") hostResult
  rulesValue <- expectBench (label <> " compile") (compile sourceProgram)
  ruleValue <- expectBench (label <> " rule") (mkRuleName ruleRawName)
  let engineValue = prepare rulesValue host
  (matchedEngine, matches) <- expectBench (label <> " match") (matchRule RewriteBase ruleValue Nothing engineValue)
  firstMatch <- expectMaybeBench (label <> " first match") (listToMaybe matches)
  pure
    PipelineFixture
      { pipelineProgram = sourceProgram,
        pipelineHost = host,
        pipelineRoots = roots,
        pipelineEngine = engineValue,
        pipelineMatchedEngine = matchedEngine,
        pipelineMatch = firstMatch,
        pipelineRuleName = ruleValue
      }

contextFixture :: IO ContextFixture
contextFixture = do
  (host, _roots) <- expectBench "context hostFromTerms" (hostFromTerms 10 (benchTerms 16))
  (updatedHost, _updatedRoot) <- expectBench "context updated hostFromTerm" (hostFromTerm 11 (wrap (leaf 0)))
  rulesValue <- expectBench "context compile" (compile contextProgram)
  contextValue <- expectBench "contextName" (contextName contextBenchName)
  unwrapName <- expectBench "context rule name" (mkRuleName "ctx.unwrap")
  let baseEngine = prepare rulesValue emptyHost
  pure
    ContextFixture
      { contextNameValue = contextValue,
        contextUpdatedHost = updatedHost,
        contextBaseEngine = baseEngine,
        contextEngine = setContext contextValue host baseEngine,
        contextRuleName = unwrapName
      }

hostFromTermWeight :: Int -> Maybe Int
hostFromTermWeight size =
  eitherWeight (hostClassCount . fst) (hostFromTerm 0 (nestedTerm size))

hostFromTermsWeight :: Int -> Maybe Int
hostFromTermsWeight size =
  eitherWeight (\(host, roots) -> hostClassCount host + length roots) (hostFromTerms 0 (benchTerms size))

compilePrepareWeight :: PipelineFixture -> Maybe Int
compilePrepareWeight fixture =
  either
    (const Nothing)
    (matchResultWeight . matchRule RewriteBase (pipelineRuleName fixture) Nothing . (`prepare` pipelineHost fixture))
    (compile (pipelineProgram fixture))

matchWeight :: PipelineFixture -> Maybe Int
matchWeight fixture =
  matchResultWeight (matchRule RewriteBase (pipelineRuleName fixture) Nothing (pipelineEngine fixture))

applyWeight :: PipelineFixture -> Maybe Int
applyWeight fixture =
  either (const Nothing) appliedWeight (apply defaultApplyConfig (pipelineMatch fixture) (pipelineMatchedEngine fixture))

baseSaturateWeight :: PipelineFixture -> Maybe Int
baseSaturateWeight fixture =
  saturateWeight RewriteBase (pipelineEngine fixture)

extractWeight :: PipelineFixture -> Maybe Int
extractWeight fixture =
  listToMaybe (pipelineRoots fixture) >>= extractRootWeight
  where
    extractRootWeight rootClass =
      eitherWeight extractedCost $
        (extract (sortWitness @"Expr") benchCost rootClass (pipelineHost fixture) :: Either ExtractError (Extracted BenchSig Int "Expr"))

setContextUpdateWeight :: ContextFixture -> Maybe Int
setContextUpdateWeight fixture =
  matchResultWeight (matchContextRule fixture (setContext (contextNameValue fixture) (contextUpdatedHost fixture) (contextBaseEngine fixture)))

contextMatchWeight :: ContextFixture -> Maybe Int
contextMatchWeight fixture =
  matchResultWeight (matchContextRule fixture (contextEngine fixture))

contextSaturateWeight :: ContextFixture -> Maybe Int
contextSaturateWeight fixture =
  saturateWeight (RewriteContext (contextNameValue fixture)) (contextEngine fixture)

removeContextVerifiedWeight :: ContextFixture -> Maybe Int
removeContextVerifiedWeight fixture =
  case matchContextRule fixture (removeContext expectedContext (contextEngine fixture)) of
    Left (RelationalProgramContextMissing observedContext)
      | observedContext == expectedContext -> Just 1
    _ -> Nothing
  where
    expectedContext = contextNameValue fixture

matchRule ::
  RewriteTarget ->
  RuleName ->
  Maybe ClassId ->
  Engine BenchSig NoGuardAtom ->
  Either (RelationalProgramError BenchSig) (Engine BenchSig NoGuardAtom, [Match])
matchRule target ruleValue maybeRoot =
  match
    MatchQuery
      { matchQueryTarget = target,
        matchQueryRule = ruleValue,
        matchQueryRoot = maybeRoot
      }

matchContextRule :: ContextFixture -> Engine BenchSig NoGuardAtom -> Either (RelationalProgramError BenchSig) (Engine BenchSig NoGuardAtom, [Match])
matchContextRule fixture =
  matchRule (RewriteContext (contextNameValue fixture)) (contextRuleName fixture) Nothing

matchResultWeight :: Either (RelationalProgramError BenchSig) (Engine BenchSig NoGuardAtom, [Match]) -> Maybe Int
matchResultWeight =
  eitherWeight (\(engineValue, matches) -> hostClassCount (engineHost engineValue) + length matches)

saturateWeight :: RewriteTarget -> Engine BenchSig NoGuardAtom -> Maybe Int
saturateWeight target engineValue =
  eitherWeight saturatedWeight (saturate target defaultSaturationConfig engineValue)

appliedWeight :: (Engine BenchSig NoGuardAtom, ApplyResult) -> Maybe Int
appliedWeight (engineValue, applyResult) =
  (+ hostClassCount (engineHost engineValue)) <$> applyStatusWeight (applyResultStatus applyResult)

saturatedWeight :: (Engine BenchSig NoGuardAtom, SaturationResult BenchSig) -> Int
saturatedWeight (engineValue, result) =
  hostClassCount (engineHost engineValue)
    + hostClassCount (saturationHost result)
    + sum (fmap (length . saturationRoundExecuted) (saturationRounds result))

applyStatusWeight :: ApplyStatus -> Maybe Int
applyStatusWeight status =
  case status of
    ApplyRejected _ -> Nothing
    ApplyExecuted _ changed -> Just (if changed then 1 else 2)

contextBenchName :: String
contextBenchName =
  "benchContext"

contextProgram :: Program BenchSig NoGuardAtom
contextProgram =
  program $ do
    context contextBenchName
    rule
      "ctx.unwrap"
      ( at contextBenchName $
          forall_
            (bind (symbolToken @"x") (symbolToken @"Expr"))
            (wrap xExpr ==> xExpr)
      )

applicationConditionProgram :: Program BenchSig NoGuardAtom
applicationConditionProgram =
  program $
    rule
      "requires.leaf"
      ( forall_
          (bind (symbolToken @"x") (symbolToken @"Expr"))
          ((wrap xExpr ==> xExpr) `requires_` extension (wrap xExpr))
      )

xExpr :: Term BenchSig "Expr"
xExpr =
  var (symbolToken @"x") (symbolToken @"Expr")

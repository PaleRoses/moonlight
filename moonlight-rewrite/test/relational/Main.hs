module Main (main) where

import Control.Monad (unless)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Hedgehog (Gen, Property, evalEither, forAll, property, (===))
import Hedgehog qualified
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Moonlight.Core (SlotId, mkAtomId, mkSlotId)
import Moonlight.Pale.Test.Site.Assertion (expectRightWithLabel)
import Moonlight.Flow.Execution.Prepared.Backend
  ( PreparedBackend (..),
    PreparedScopeView (..),
  )
import Moonlight.Differential.Row.Tuple (RowTupleKey, tupleKeyFromInts)
import Moonlight.Flow.Plan.Compile.Build qualified as PlanBuild
import Moonlight.Flow.Plan.Query.Core
  ( AtomSpec,
    QueryPlan,
    mkAtomSpec,
    mkQueryAtomId,
    mkSourceAtomId,
    mkStalkRecipe,
  )
import Moonlight.Flow.Storage.Relation (relationRows)
import Moonlight.Rewrite.Relational
  ( Count (..),
    Limit (..),
    MatchKey,
    RelationalPlanSet (..),
    RelationalPreparedSystem,
    RelationalRewriteMatch,
    RewriteRelationalBackend,
    RewriteRelationalHost,
    RewriteRelationalPatch (..),
    RewriteRestriction (..),
    RewriteRunError,
    RewriteRunLimit (..),
    RewriteRunMetrics (..),
    RewriteRunResult (..),
    RewriteRunStats,
    advanceRelationalSystemHost,
    checkRewriteRunLimits,
    defaultRewriteRunConfig,
    defaultRewriteRunLimits,
    emptyRewriteRelationalHost,
    emptyRewriteRunStats,
    matchKeyAtomId,
    matchKeyFromInts,
    matchKeyPinnedRow,
    noRewriteRunLimits,
    prepareRelationalSystem,
    preparedRelationalSystemCachedBaseRevisions,
    preparedRelationalSystemRevision,
    replaceRewriteRelationalHost,
    rrcLimits,
    rrcRestriction,
    rewritePreparedBackend,
    rewriteRelationalHostRevision,
    rewriteRelationalHostSections,
    runExistsMatch,
    runMatchRule,
    runRuleSupport,
  )
import Moonlight.Rewrite.System
  ( RuleName,
    baseRuleSupportIndex,
    mkRuleName,
  )
import Test.Tasty.HUnit (assertEqual)

main :: IO ()
main = do
  assertEqual "default run config is unrestricted" RewriteUnrestricted (rrcRestriction defaultRewriteRunConfig)
  assertEqual "default run config owns explicit limits" defaultRewriteRunLimits (rrcLimits defaultRewriteRunConfig)
  assertEqual "default round limit is explicit" (Limit (Just 1024)) (rrmRounds defaultRewriteRunLimits)
  assertHostReplacementLaws
  assertRunLimitObstruction
  assertMatchKeyProjection
  assertPatchMatchesRebuildOracle
  assertRelationalSystemAdvanceLaws

assertHostReplacementLaws :: IO ()
assertHostReplacementLaws = do
  assertEqual
    "empty host starts at revision zero"
    0
    (rewriteRelationalHostRevision (emptyRewriteRelationalHost :: RewriteRelationalHost String))
  let sections :: Map.Map String (IntMap.IntMap [RowTupleKey])
      sections = Map.singleton "Node" (IntMap.singleton 4 [tupleKeyFromInts [1, 2]])
      host = replaceRewriteRelationalHost 9 sections
  assertEqual "replacement owns the supplied revision" 9 (rewriteRelationalHostRevision host)
  assertEqual "replacement owns the supplied projected sections" sections (rewriteRelationalHostSections host)

assertRunLimitObstruction :: IO ()
assertRunLimitObstruction =
  assertEqual
    "result-row limits report a typed obstruction with the observed stats"
    (Left (MaxResultRowsExceeded 1, observedStats))
    (checkRewriteRunLimits tightLimits observedStats)
  where
    tightLimits =
      noRewriteRunLimits
        { rrmResultRows = Limit (Just 1)
        }

    observedStats =
      emptyRewriteRunStats
        { rrmResultRows = Count 2
        }

assertMatchKeyProjection :: IO ()
assertMatchKeyProjection = do
  let atomId = mkAtomId 5
      matchKey = matchKeyFromInts atomId [8, 13] :: MatchKey
  assertEqual "match keys expose their atom id" atomId (matchKeyAtomId matchKey)
  assertEqual "match keys expose their pinned tuple" (tupleKeyFromInts [8, 13]) (matchKeyPinnedRow matchKey)

type SectionTag = String

type PatchTestBackend = RewriteRelationalBackend () () Int () SectionTag ()

type PatchTestPlan = QueryPlan () (RelationalRewriteMatch () Int) () SectionTag () Int

patchTestBackend :: PatchTestBackend
patchTestBackend =
  rewritePreparedBackend

rootSlot :: SlotId
rootSlot = mkSlotId 0

leftSlot :: SlotId
leftSlot = mkSlotId 1

rightSlot :: SlotId
rightSlot = mkSlotId 2

sectionAtomSpec :: Int -> SectionTag -> [SlotId] -> AtomSpec SectionTag () Int
sectionAtomSpec atomKey tagValue columns =
  let schema = Vector.fromList columns
   in mkAtomSpec
        (mkQueryAtomId atomKey)
        (mkSourceAtomId (mkAtomId atomKey))
        tagValue
        0
        schema
        (mkStalkRecipe (Vector.replicate (Vector.length schema) []))

patchTestPlan :: Either [PlanBuild.QueryPlanError] PatchTestPlan
patchTestPlan =
  PlanBuild.mkQueryPlan
    PlanBuild.QueryPlanInput
      { PlanBuild.qpiDomain = PlanBuild.StructuralQueryPlan,
        PlanBuild.qpiCompiled = (),
        PlanBuild.qpiDigest = 23,
        PlanBuild.qpiAtoms =
          Vector.fromList
            [ sectionAtomSpec 0 "f" [rootSlot, leftSlot],
              sectionAtomSpec 1 "g" [rootSlot, rightSlot],
              sectionAtomSpec 2 "f" [leftSlot, rightSlot]
            ],
        PlanBuild.qpiSchemaOrder = Just (Vector.fromList [rootSlot, leftSlot, rightSlot]),
        PlanBuild.qpiRootSlot = rootSlot,
        PlanBuild.qpiOutputs =
          fmap (`PlanBuild.PlanOutputBinding` ()) [rootSlot, leftSlot, rightSlot],
        PlanBuild.qpiResidual = PlanBuild.NoQueryPlanResidual
      }

genResultRows :: Int -> Gen [RowTupleKey]
genResultRows resultKey = do
  children <- Gen.set (Range.linear 0 3) (Gen.int (Range.linear 0 12))
  pure [tupleKeyFromInts [resultKey, child] | child <- Set.toList children]

genTagSection :: Gen (IntMap [RowTupleKey])
genTagSection = do
  resultKeys <- Gen.set (Range.linear 0 6) (Gen.int (Range.linear 0 9))
  entries <- traverse (\resultKey -> fmap ((,) resultKey) (genResultRows resultKey)) (Set.toList resultKeys)
  pure (IntMap.fromList [(resultKey, rows) | (resultKey, rows) <- entries, not (null rows)])

genSections :: Gen (Map SectionTag (IntMap [RowTupleKey]))
genSections = do
  fSection <- genTagSection
  gSection <- genTagSection
  pure (Map.fromList [("f", fSection), ("g", gSection)])

genDirtyRewrite ::
  Map SectionTag (IntMap [RowTupleKey]) ->
  Gen (IntSet, Map SectionTag (IntMap [RowTupleKey]))
genDirtyRewrite sections = do
  dirtyKeys <- Gen.set (Range.linear 0 5) (Gen.int (Range.linear 0 9))
  let dirty = IntSet.fromList (Set.toList dirtyKeys)
  rewritten <- traverse (rewriteTagSection dirty) sections
  pure (dirty, rewritten)

rewriteTagSection :: IntSet -> IntMap [RowTupleKey] -> Gen (IntMap [RowTupleKey])
rewriteTagSection dirty oldRows = do
  freshEntries <- traverse (\resultKey -> fmap ((,) resultKey) (genResultRows resultKey)) (IntSet.toList dirty)
  pure
    ( IntMap.union
        (IntMap.fromList [(resultKey, rows) | (resultKey, rows) <- freshEntries, not (null rows)])
        (IntMap.withoutKeys oldRows dirty)
    )

patchMatchesRebuildOracle :: PatchTestPlan -> Property
patchMatchesRebuildOracle plan =
  property $ do
    sections0 <- forAll genSections
    (dirty, sections1) <- forAll (genDirtyRewrite sections0)
    let host0 = replaceRewriteRelationalHost 0 sections0
        host1 = replaceRewriteRelationalHost 1 sections1
    base0 <- evalEither (pbBuildBase patchTestBackend plan host0)
    (patched, patch) <- evalEither (pbPatchBase patchTestBackend host0 host1 dirty base0)
    rebuilt <- evalEither (pbBuildBase patchTestBackend plan host1)
    rrpDirtyResults patch === dirty
    fmap relationRows (psvFibers (pbBaseScopeView patchTestBackend patched))
      === fmap relationRows (psvFibers (pbBaseScopeView patchTestBackend rebuilt))

assertPatchMatchesRebuildOracle :: IO ()
assertPatchMatchesRebuildOracle = do
  plan <-
    either
      (\errors -> fail ("patch oracle plan failed to build: " <> show errors))
      pure
      patchTestPlan
  passed <-
    Hedgehog.check
      (Hedgehog.withTests 200 (patchMatchesRebuildOracle plan))
  unless passed (fail "incremental pbPatchBase diverged from the pbBuildBase oracle")

type PatchPreparedSystem =
  RelationalPreparedSystem () () () () Int () SectionTag ()

advanceLawSections0 :: Map SectionTag (IntMap [RowTupleKey])
advanceLawSections0 =
  Map.fromList
    [ ( "f",
        IntMap.fromList
          [ (1, [tupleKeyFromInts [1, 10]]),
            (10, [tupleKeyFromInts [10, 20]])
          ]
      ),
      ("g", IntMap.singleton 1 [tupleKeyFromInts [1, 20]])
    ]

advanceLawSections1 :: Map SectionTag (IntMap [RowTupleKey])
advanceLawSections1 =
  Map.fromList
    [ ( "f",
        IntMap.fromList
          [ (1, [tupleKeyFromInts [1, 12]]),
            (2, [tupleKeyFromInts [2, 11]]),
            (11, [tupleKeyFromInts [11, 21]]),
            (12, [tupleKeyFromInts [12, 20]])
          ]
      ),
      ( "g",
        IntMap.fromList
          [ (1, [tupleKeyFromInts [1, 20]]),
            (2, [tupleKeyFromInts [2, 21]])
          ]
      )
    ]

advanceLawDirtyResults :: IntSet
advanceLawDirtyResults =
  IntSet.fromList [1, 2, 10, 11, 12]

assertRelationalSystemAdvanceLaws :: IO ()
assertRelationalSystemAdvanceLaws = do
  ruleValue <-
    either
      (\ruleError -> fail ("advance law rule name failed to build: " <> show ruleError))
      pure
      (mkRuleName "advance.law")
  plan <-
    either
      (\errors -> fail ("advance law plan failed to build: " <> show errors))
      pure
      patchTestPlan
  let host0 =
        replaceRewriteRelationalHost 0 advanceLawSections0
      host1 =
        replaceRewriteRelationalHost 1 advanceLawSections1
      existsKey =
        matchKeyFromInts (mkAtomId 0) [1, 12]
  (prepared0, _initialMatches) <-
    runMatchSet ruleValue (preparedSystemFor ruleValue host0 plan)
  assertEqual "initial run installs base revision zero" [0] (preparedRelationalSystemCachedBaseRevisions prepared0)
  let advanced =
        advanceRelationalSystemHost host1 advanceLawDirtyResults prepared0
      oracle0 =
        preparedSystemFor ruleValue host1 plan
  assertEqual "advance moves the prepared-system host revision" 1 (preparedRelationalSystemRevision advanced)
  assertEqual "advance patches the cached prepared base revision" [1] (preparedRelationalSystemCachedBaseRevisions advanced)
  (advancedAfterMatches, advancedMatches) <-
    runMatchSet ruleValue advanced
  (oracleAfterMatches, oracleMatches) <-
    runMatchSet ruleValue oracle0
  assertEqual "advanced match results agree with full prepare" oracleMatches advancedMatches
  assertEqual "patched base stays cached after the first advanced run" [1] (preparedRelationalSystemCachedBaseRevisions advancedAfterMatches)
  (advancedAfterSupport, advancedSupport) <-
    runSupportStats ruleValue advancedAfterMatches
  (oracleAfterSupport, oracleSupport) <-
    runSupportStats ruleValue oracleAfterMatches
  assertEqual "advanced support results agree with full prepare" oracleSupport advancedSupport
  (advancedAfterExists, advancedExists) <-
    runExistsPinned ruleValue existsKey advancedAfterSupport
  (_oracleAfterExists, oracleExists) <-
    runExistsPinned ruleValue existsKey oracleAfterSupport
  assertEqual "advanced pinned-exists result agrees with full prepare" oracleExists advancedExists
  assertEqual "advanced run preserves patched base cache through match/support/exists" [1] (preparedRelationalSystemCachedBaseRevisions advancedAfterExists)
  (emptyBase, emptyMatches0) <-
    runMatchSet ruleValue (preparedSystemFor ruleValue host0 plan)
  let emptyAdvanced =
        advanceRelationalSystemHost host0 IntSet.empty emptyBase
  assertEqual "empty advance preserves host revision" 0 (preparedRelationalSystemRevision emptyAdvanced)
  assertEqual "empty advance preserves cached base revision" [0] (preparedRelationalSystemCachedBaseRevisions emptyAdvanced)
  (emptyAdvancedAfterRun, emptyMatches1) <-
    runMatchSet ruleValue emptyAdvanced
  assertEqual "empty advance preserves match observations" emptyMatches0 emptyMatches1
  assertEqual "empty advance remains on the same cached base" [0] (preparedRelationalSystemCachedBaseRevisions emptyAdvancedAfterRun)

preparedSystemFor ::
  RuleName ->
  RewriteRelationalHost SectionTag ->
  PatchTestPlan ->
  PatchPreparedSystem
preparedSystemFor ruleValue host plan =
  prepareRelationalSystem
    host
    (baseRuleSupportIndex (Set.singleton ruleValue))
    (RelationalPlanSet (Map.singleton ruleValue plan))

runMatchSet ::
  RuleName ->
  PatchPreparedSystem ->
  IO (PatchPreparedSystem, Set.Set (RelationalRewriteMatch () Int))
runMatchSet ruleValue system =
  fmap
    (\(system', result) -> (system', Set.fromList (rrrValue result)))
    (expectRewriteRun "match" (runMatchRule defaultRewriteRunConfig ruleValue system))

runSupportStats ::
  RuleName ->
  PatchPreparedSystem ->
  IO (PatchPreparedSystem, RewriteRunStats)
runSupportStats ruleValue =
  fmap (fmap rrrStats)
    . expectRewriteRun "support"
    . runRuleSupport defaultRewriteRunConfig ruleValue

runExistsPinned ::
  RuleName ->
  MatchKey ->
  PatchPreparedSystem ->
  IO (PatchPreparedSystem, Bool)
runExistsPinned ruleValue matchKeyValue =
  fmap (fmap rrrValue)
    . expectRewriteRun "exists"
    . runExistsMatch defaultRewriteRunConfig ruleValue matchKeyValue

expectRewriteRun ::
  String ->
  Either (RewriteRunError ()) (PatchPreparedSystem, RewriteRunResult () result) ->
  IO (PatchPreparedSystem, RewriteRunResult () result)
expectRewriteRun label result =
  expectRightWithLabel (label <> " run") result

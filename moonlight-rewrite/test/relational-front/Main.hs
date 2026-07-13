{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Control.Monad
  ( foldM,
  )
import Data.Foldable
  ( toList,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import GHC.TypeLits (Symbol)
import Moonlight.Core
  ( Language,
    ZipMatch (..),
    sameNodeShape,
  )
import Moonlight.Core.EGraph.Program
  ( EGraphProgram,
    addNode,
    emptyEGraphProgramEffect,
    mergeClasses,
  )
import Moonlight.EGraph.Pure.Extraction.Core
  ( ExtractionConvergenceReport (..),
    ExtractionFixpointBudget (..),
  )
import Vocabulary
  ( ComboSig (..),
    allAtomKinds,
    allComboKinds,
    allEntityKinds,
    allOutcomeKinds,
    allStatusKinds,
  )
import Moonlight.Rewrite
  ( ClassId,
    ContextName,
    Cost (..),
    ExtractConfig (..),
    ExtractError (..),
    ExtractRoundLimit (..),
    Extracted,
    HTraversable (..),
    Engine,
    Host,
    HostBuildError (..),
    HostProgramResult (..),
    HostRebuildResult (..),
    K (..),
    Match,
    MatchQuery (..),
    NoGuardAtom,
    Program,
    RelationalProgramError (..),
    RewriteSignature (..),
    RewriteTarget (..),
    RuleName,
    SaturationConfig (..),
    SaturationResult (..),
    Term,
    at,
    bind,
    compile,
    context,
    contextName,
    contextNameString,
    defaultSaturationConfig,
    emptyHost,
    engineHost,
    extract,
    extractedClass,
    extractedCost,
    extractedTerm,
    extractWith,
    hostCanonicalClass,
    hostClassCount,
    hostFromNodeClasses,
    hostFromTerm,
    hostLookupTermClass,
    hostNodeClasses,
    hostRevision,
    hostSections,
    hostSectionsFromClasses,
    rebuildHostBarrier,
    runHostRewriteProgram,
    match,
    matchBindings,
    matchRoot,
    matchVarName,
    matchVarSort,
    node,
    program,
    rule,
    prepare,
    mkRuleName,
    saturate,
    setContext,
    sortNameString,
    sortWitness,
    admitPBPOUnit,
    classIdKey,
    symbolToken,
    var,
    forall_,
    (==>),
  )
import Moonlight.Rewrite.DSL
  ( Node (..),
    SortWitness (..),
  )
import ComboAtlasSpec
  ( comboAtlasTests,
  )
import PackageDisclosureSpec qualified
import Moonlight.Rewrite.Runtime
  ( RewriteApplicationError (..),
  )
import Moonlight.Rewrite.Relational.Front
  ( RelationalSaturationObstruction (..),
    RelationalSaturationPlanError (..),
    prettyRelationalProgramError,
    relationalSaturationResumeError,
  )
import Moonlight.Saturation.Context.Error
  ( RuntimeResumeError (..),
  )
import Moonlight.Sheaf.Context.Site
  ( PreparedContextSupportError (..),
  )
import Hedgehog
  ( Gen,
    PropertyT,
    assert,
    evalEither,
    forAll,
    property,
    (===),
  )
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty
  ( TestTree,
    defaultMain,
    testGroup,
  )
import Test.Tasty.Hedgehog
  ( testProperty,
  )
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertFailure,
    testCase,
    (@?=),
  )

data TinyTag
  = TinyA
  | TinyB
  | TinyFlag
  | TinyWrap
  deriving stock (Eq, Ord, Show)

data TinySig (result :: Symbol) r where
  LitA :: TinySig "Expr" r
  LitB :: TinySig "Expr" r
  LitFlag :: TinySig "Flag" r
  Wrap :: r "Expr" -> TinySig "Expr" r

instance HTraversable TinySig where
  htraverseWithSort transform = \case
    LitA ->
      pure LitA

    LitB ->
      pure LitB

    LitFlag ->
      pure LitFlag

    Wrap child ->
      Wrap <$> transform SortWitness child

instance RewriteSignature TinySig where
  type NodeTag TinySig = TinyTag

  nodeTag = \case
    LitA ->
      TinyA

    LitB ->
      TinyB

    LitFlag ->
      TinyFlag

    Wrap _ ->
      TinyWrap

  nodeTagDigest _ tagValue =
    case tagValue of
      TinyA ->
        1

      TinyB ->
        2

      TinyFlag ->
        3

      TinyWrap ->
        4

  nodeResultSort = \case
    LitA ->
      SortWitness

    LitB ->
      SortWitness

    LitFlag ->
      SortWitness

    Wrap _ ->
      SortWitness

instance ZipMatch (Node TinySig) where
  zipMatch leftNode rightNode =
    case (leftNode, rightNode) of
      (Node LitA, Node LitA) ->
        Just (Node LitA)

      (Node LitB, Node LitB) ->
        Just (Node LitB)

      (Node LitFlag, Node LitFlag) ->
        Just (Node LitFlag)

      (Node (Wrap leftChild), Node (Wrap rightChild)) ->
        Just (Node (Wrap (K (unK leftChild, unK rightChild))))

      _ ->
        Nothing

main :: IO ()
main =
  defaultMain
    ( testGroup
        "moonlight-rewrite:relational-front"
        [ termHostMatchesNamedBindings,
          nodeClassesSupportMultipleNodesPerKey,
          saturationNodeBudgetCountsNodesNotClasses,
          emptyHostProgramIsIdentity,
          saturationIsIdempotentAtFace,
          hostProgramExecutionIsDeterministic,
          extractionReturnsTypedCostedRepresentative,
          extractionChoosesLowestCostSameClassRepresentative,
          extractionExhaustionPreservesConvergenceReport,
          extractionReportsTypedObstructions,
          saturationRunErrorsRetainDistinctCauses,
          hostProgramCanonicalizationLaws,
          hostBarrierMatchesEagerOracle,
          hostSectionsMatchProjectionOracle,
          tinySigZipMatchPairsChildren,
          comboSigZipMatchPairsChildren,
          hostTermVariablesAreRejected,
          existsQueriesDoNotNeedMatchKeys,
          contextualRuntimeSupportsRepeatedContextUpdates,
          contextualRuntimeReportsMissingContexts,
          pbpoPlusAlgebraIsOnMainSurface,
          comboAtlasTests,
          PackageDisclosureSpec.tests
        ]
    )

assertZipMatchPairsChildren ::
  (ZipMatch t, Language t) =>
  t Int ->
  t Int ->
  PropertyT IO ()
assertZipMatchPairsChildren leftNode rightNode =
  case zipMatch leftNode rightNode of
    Just zipped -> do
      assert (sameNodeShape leftNode rightNode)
      toList zipped === zip (toList leftNode) (toList rightNode)
    Nothing ->
      assert (not (sameNodeShape leftNode rightNode))

tinySigZipMatchPairsChildren :: TestTree
tinySigZipMatchPairsChildren =
  testProperty "hand-written TinySig zipMatch pairs every child and rejects shape mismatches" $
    property $ do
      leftNode <- forAll genTinyNode
      rightNode <- forAll genTinyNode
      assertZipMatchPairsChildren leftNode rightNode
  where
    genTinyNode :: Gen (Node TinySig Int)
    genTinyNode =
      Gen.choice
        [ pure (Node LitA),
          pure (Node LitB),
          pure (Node LitFlag),
          Node . Wrap . K <$> Gen.int (Range.linear 0 9)
        ]

comboSigZipMatchPairsChildren :: TestTree
comboSigZipMatchPairsChildren =
  testProperty "TH-generated ComboSig zipMatch pairs every child and rejects shape mismatches" $
    property $ do
      leftNode <- forAll genComboNode
      rightNode <- forAll genComboNode
      assertZipMatchPairsChildren leftNode rightNode
  where
    genComboNode :: Gen (Node ComboSig Int)
    genComboNode =
      Gen.choice
        [ Node . Atom <$> Gen.element allAtomKinds,
          Node . Outcome <$> Gen.element allOutcomeKinds,
          Node . Entity <$> Gen.element allEntityKinds,
          Node . Target . K <$> genChild,
          Node <$> (Status <$> Gen.element allStatusKinds <*> (K <$> genChild)),
          Node <$> (Fuse <$> (K <$> genChild) <*> (K <$> genChild)),
          Node <$> (Combo <$> Gen.element allComboKinds <*> (K <$> genChild))
        ]

    genChild :: Gen Int
    genChild =
      Gen.int (Range.linear 0 9)

pbpoPlusAlgebraIsOnMainSurface :: TestTree
pbpoPlusAlgebraIsOnMainSurface =
  testCase "main Moonlight.Rewrite surface exports PBPO+ algebra" $ do
    admittedHost <-
      assertRight (admitPBPOUnit ("engine failure" :: String) ("surface" :: String) (41 :: Int) Right)
    admittedHost @?= 41
    case admitPBPOUnit ("rejected" :: String) ("surface" :: String) (41 :: Int) (const (Left "rejected")) of
      Left "rejected" -> pure ()
      Left otherFailure -> assertFailure ("unexpected admission failure " <> otherFailure)
      Right _ -> assertFailure "expected unit admission to propagate the rejection"

termHostMatchesNamedBindings :: TestTree
termHostMatchesNamedBindings =
  testCase "hostFromTerm builds a host and match returns named sorted bindings" $ do
    (host, rootKey) <-
      assertRight (hostFromTerm 7 (wrap litA))

    hostRevision host @?= 7
    classIdKey rootKey @?= 1

    engine0 <-
      prepareProgram unwrapProgram host

    unwrapRuleName <-
      expectRuleName "unwrap"
    (_engine1, matches) <-
      assertRight (matchBaseRule unwrapRuleName Nothing engine0)

    case matches of
      [matchValue] -> do
        matchRoot matchValue @?= rootKey
        case Map.toList (matchBindings matchValue) of
          [(matchVariable, boundKey)] -> do
            matchVarName matchVariable @?= "x"
            fmap sortNameString (matchVarSort matchVariable) @?= Just "Expr"
            classIdKey boundKey @?= 0

          bindings ->
            assertFailure ("expected exactly one binding, got " <> show bindings)

      otherMatches ->
        assertFailure ("expected exactly one match, got " <> show otherMatches)

nodeClassesSupportMultipleNodesPerKey :: TestTree
nodeClassesSupportMultipleNodesPerKey =
  testCase "hostFromNodeClasses supports multiple nodes per key" $ do
    host <-
      assertRight
        ( hostFromNodeClasses
            3
            ( IntMap.singleton
                0
                [ Node LitA,
                  Node LitB
                ]
            )
        )

    engine0 <-
      prepareProgram literalProgram host

    isARuleName <-
      expectRuleName "is-a"
    isBRuleName <-
      expectRuleName "is-b"
    (engine1, aMatches) <-
      assertRight (matchBaseRule isARuleName Nothing engine0)
    (_engine2, bMatches) <-
      assertRight (matchBaseRule isBRuleName Nothing engine1)

    fmap (classIdKey . matchRoot) aMatches @?= [0]
    fmap (classIdKey . matchRoot) bMatches @?= [0]

saturationNodeBudgetCountsNodesNotClasses :: TestTree
saturationNodeBudgetCountsNodesNotClasses =
  testCase "saturation node budget counts nodes in multi-node classes" $ do
    host <-
      assertRight
        ( hostFromNodeClasses
            0
            ( IntMap.singleton
                0
                [ Node LitA,
                  Node LitB
                ]
            )
        )
    engine0 <-
      prepareProgram growProgram host
    let baseConfig =
          defaultSaturationConfig :: SaturationConfig TinySig
        config =
          baseConfig
            { scHostNodeLimit = Just 1
            }
    (_engine1, result) <-
      assertRight (saturate RewriteBase config engine0)
    hostClassCount host @?= 1
    sum (fmap (length . snd) (hostNodeClasses host)) @?= 2
    saturationRounds result @?= []
    hostNodeClasses (saturationHost result) @?= hostNodeClasses host

emptyHostProgramIsIdentity :: TestTree
emptyHostProgramIsIdentity =
  testCase "empty host rewrite program is the identity on the public host face" $ do
    (host, _rootKey) <-
      assertRight (hostFromTerm 17 (wrap (wrap litA)))

    HostProgramResult
      { hprHost = observedHost,
        hprValue = observedValue,
        hprEffect = observedEffect,
        hprDelta = observedDelta,
        hprDirtyResultKeys = observedDirtyKeys
      } <-
      assertRight (runHostRewriteProgram emptyHostRewriteProgram host)

    observedValue @?= ()
    observedEffect @?= emptyEGraphProgramEffect
    observedDelta @?= mempty
    observedDirtyKeys @?= IntSet.empty
    assertHostObservablyEqual host observedHost

saturationIsIdempotentAtFace :: TestTree
saturationIsIdempotentAtFace =
  testCase "saturating an already saturated public host makes no further host change" $ do
    (host, _rootKey) <-
      assertRight (hostFromTerm 19 (wrap (wrap litA)))

    engine0 <-
      prepareProgram unwrapProgram host
    (engine1, saturatedOnce) <-
      assertRight (saturate RewriteBase (defaultSaturationConfig :: SaturationConfig TinySig) engine0)
    (engine2, saturatedTwice) <-
      assertRight (saturate RewriteBase (defaultSaturationConfig :: SaturationConfig TinySig) engine1)

    assertHostObservablyEqual (saturationHost saturatedOnce) (saturationHost saturatedTwice)
    assertHostObservablyEqual (engineHost engine1) (engineHost engine2)

hostProgramExecutionIsDeterministic :: TestTree
hostProgramExecutionIsDeterministic =
  testCase "same host rewrite program on the same public host is deterministic" $ do
    (host, rootKey) <-
      assertRight (hostFromTerm 23 litA)

    firstResult <-
      assertRight (runHostRewriteProgram (deterministicHostProgram rootKey) host)
    secondResult <-
      assertRight (runHostRewriteProgram (deterministicHostProgram rootKey) host)

    assertHostProgramResultsObservablyEqual firstResult secondResult

extractionReturnsTypedCostedRepresentative :: TestTree
extractionReturnsTypedCostedRepresentative =
  testCase "extract returns a typed costed representative for a host class" $ do
    (host, rootKey) <-
      assertRight (hostFromTerm 13 (wrap litB))

    case extract (sortWitness @"Expr") tinyCost rootKey host ::
      Either ExtractError (Extracted TinySig Int "Expr") of
      Left extractError ->
        assertFailure ("expected extraction success, got " <> show extractError)

      Right extracted -> do
        extractedClass extracted @?= rootKey
        extractedCost extracted @?= 2
        hostLookupTermClass (extractedTerm extracted) host @?= Right (Just rootKey)

extractionChoosesLowestCostSameClassRepresentative :: TestTree
extractionChoosesLowestCostSameClassRepresentative =
  testCase "extract chooses the lowest-cost representative from a multi-node class" $ do
    host <-
      assertRight
        ( hostFromNodeClasses
            0
            ( IntMap.singleton
                0
                [ Node LitA,
                  Node LitB
                ]
            )
        )
    rootClass <-
      expectHostClassId host 0

    case extract (sortWitness @"Expr") tinyCost rootClass host ::
      Either ExtractError (Extracted TinySig Int "Expr") of
      Left extractError ->
        assertFailure ("expected extraction success, got " <> show extractError)

      Right extracted -> do
        extractedClass extracted @?= rootClass
        extractedCost extracted @?= 1
        hostLookupTermClass (extractedTerm extracted) host @?= Right (Just rootClass)

extractionExhaustionPreservesConvergenceReport :: TestTree
extractionExhaustionPreservesConvergenceReport =
  testCase "extract preserves every convergence count when its budget is exhausted" $ do
    (host, rootClass) <-
      assertRight (hostFromTerm 37 litA)
    case
      extractWith
        ExtractConfig {extractRoundLimit = ExtractMaxRounds 0}
        (sortWitness @"Expr")
        tinyCost
        rootClass
        host ::
      Either ExtractError (Extracted TinySig Int "Expr") of
      Left (ExtractFixpointExhausted convergenceReport) -> do
        ecrBudget convergenceReport @?= ExtractionFixpointBudget 0
        ecrTotalClassCount convergenceReport @?= 1
        ecrResolvedClassCount convergenceReport @?= 0
        ecrUnresolvedClassCount convergenceReport @?= 1

      Left otherError ->
        assertFailure ("expected convergence report, got " <> show otherError)

      Right _extracted ->
        assertFailure "expected zero-budget extraction to report exhaustion"

saturationRunErrorsRetainDistinctCauses :: TestTree
saturationRunErrorsRetainDistinctCauses =
  testCase "saturation support and resume failures retain exact boundary constructors" $ do
    let supportObstruction :: RelationalSaturationObstruction TinySig
        supportObstruction =
          RelationalSaturationPreparedSupportFailed PreparedContextSupportDefaultMissing

    case supportObstruction of
      RelationalSaturationPreparedSupportFailed observedSupportError ->
        observedSupportError @?= PreparedContextSupportDefaultMissing

      otherError ->
        assertFailure ("expected prepared-support obstruction, got " <> show otherError)
    prettyRelationalProgramError
      (RelationalProgramSaturationObstruction supportObstruction)
      @?= "relational saturation prepared support failed: PreparedContextSupportDefaultMissing"
    relationalSaturationResumeError RuntimeResumeMissingPlanIdentity
      @?= RelationalSaturationResumeMissingPlanIdentity
    relationalSaturationResumeError RuntimeResumePlanChanged
      @?= RelationalSaturationResumePlanChanged

extractionReportsTypedObstructions :: TestTree
extractionReportsTypedObstructions =
  testCase "extract and host construction report typed sort/finite-representative obstructions" $ do
    case hostFromNodeClasses 0 (IntMap.singleton 0 [] :: IntMap.IntMap [Node TinySig Int]) of
      Left (HostEmptyNodeClass 0) ->
        pure ()

      Left otherError ->
        assertFailure ("expected empty-class obstruction, got " <> show otherError)

      Right _host ->
        assertFailure "expected empty-class obstruction, got host"

    case hostFromNodeClasses 0 (IntMap.singleton 0 [Node LitA, Node LitFlag]) of
      Left (HostClassSortMismatch 0 leftSort rightSort) -> do
        sortNameString leftSort @?= "Expr"
        sortNameString rightSort @?= "Flag"

      Left otherError ->
        assertFailure ("expected sort-mismatch obstruction, got " <> show otherError)

      Right _host ->
        assertFailure "expected sort-mismatch obstruction, got host"

    case
      hostFromNodeClasses
        0
        ( IntMap.fromList
            [ (0, [Node LitFlag]),
              (1, [Node (Wrap (K 0))])
            ]
        ) of
      Left (HostChildSortMismatch 1 0 expectedSort observedSort) -> do
        sortNameString expectedSort @?= "Expr"
        sortNameString observedSort @?= "Flag"

      Left otherError ->
        assertFailure ("expected child-sort obstruction, got " <> show otherError)

      Right _host ->
        assertFailure "expected child-sort obstruction, got host"

    flagHost <-
      assertRight (hostFromNodeClasses 0 (IntMap.singleton 0 [Node LitFlag]))
    flagRoot <-
      expectHostClassId flagHost 0
    case extract (sortWitness @"Expr") tinyCost flagRoot flagHost ::
      Either ExtractError (Extracted TinySig Int "Expr") of
      Left (ExtractSortMismatch classId expectedSort observedSort)
        | classId == flagRoot -> do
            sortNameString expectedSort @?= "Expr"
            sortNameString observedSort @?= "Flag"

      Left otherError ->
        assertFailure ("expected extraction sort mismatch, got " <> show otherError)

      Right _extracted ->
        assertFailure "expected extraction sort mismatch, got representative"

    cycleHost <-
      assertRight (hostFromNodeClasses 0 (IntMap.singleton 0 [Node (Wrap (K 0))]))
    cycleRoot <-
      expectHostClassId cycleHost 0
    case
      extractWith
        ExtractConfig {extractRoundLimit = ExtractMaxRounds 4}
        (sortWitness @"Expr")
        tinyCost
        cycleRoot
        cycleHost ::
      Either ExtractError (Extracted TinySig Int "Expr") of
      Left (ExtractNoFiniteRepresentative classId)
        | classId == cycleRoot -> pure ()

      Left otherError ->
        assertFailure ("expected finite-representative obstruction, got " <> show otherError)

      Right _extracted ->
        assertFailure "expected finite-representative obstruction, got representative"

hostProgramCanonicalizationLaws :: TestTree
hostProgramCanonicalizationLaws =
  testCase "host rebuild barrier follows observable egraph quotient laws" $ do
    host <-
      assertRight
        ( hostFromNodeClasses
            0
            ( IntMap.fromList
                [ (0, [Node LitA]),
                  (1, [Node (Wrap (K 0))]),
                  (2, [Node LitB]),
                  (3, [Node (Wrap (K 2))]),
                  (4, [Node LitFlag])
                ]
            )
        )
    class0 <- expectHostClassId host 0
    class1 <- expectHostClassId host 1
    class2 <- expectHostClassId host 2
    class3 <- expectHostClassId host 3
    class4 <- expectHostClassId host 4
    missingHost <-
      assertRight (hostFromNodeClasses 0 (IntMap.singleton 99 [Node LitA]))
    missingClass <- expectHostClassId missingHost 99
    HostProgramResult {hprHost = mergedHost} <-
      assertRight (runHostRewriteProgram (mergeClasses class0 class2) host)

    hostCanonicalClass mergedHost class0 @?= hostCanonicalClass mergedHost class2
    assertBool
      "congruence closure is deferred until the rebuild barrier"
      (hostCanonicalClass mergedHost class1 /= hostCanonicalClass mergedHost class3)

    HostRebuildResult {hrrHost = rebuiltHost} <-
      assertRight (rebuildHostBarrier mergedHost)

    hostCanonicalClass rebuiltHost class0 @?= hostCanonicalClass rebuiltHost class2
    hostCanonicalClass rebuiltHost class1 @?= hostCanonicalClass rebuiltHost class3

    HostProgramResult {hprHost = idempotentHost} <-
      assertRight (runHostRewriteProgram (mergeClasses class1 class3) rebuiltHost)
    hostNodeClasses idempotentHost @?= hostNodeClasses rebuiltHost

    case runHostRewriteProgram (mergeClasses class0 missingClass) host of
      Left (RewriteMissingEClass classId)
        | classId == missingClass -> pure ()
      _ ->
        assertFailure "expected missing-class obstruction"

    case runHostRewriteProgram (mergeClasses class0 class4) host of
      Left (RewriteClassSortMismatch leftClass rightClass)
        | leftClass == class0 && rightClass == class4 -> pure ()
      _ ->
        assertFailure "expected sort-mismatch obstruction"

    engine0 <-
      prepareProgram literalProgram rebuiltHost
    litAName <-
      expectRuleName "is-a"
    (engine1, aMatches) <-
      assertRight (matchBaseRule litAName Nothing engine0)
    litBName <-
      expectRuleName "is-b"
    (_engine2, bMatches) <-
      assertRight (matchBaseRule litBName Nothing engine1)
    fmap (hostCanonicalClass rebuiltHost . matchRoot) aMatches @?= fmap (const (hostCanonicalClass rebuiltHost class0)) aMatches
    fmap (hostCanonicalClass rebuiltHost . matchRoot) bMatches @?= fmap (const (hostCanonicalClass rebuiltHost class0)) bMatches

hostBarrierMatchesEagerOracle :: TestTree
hostBarrierMatchesEagerOracle =
  testProperty "batched merges with one barrier match the eager per-merge oracle" $
    property $ do
      rawTargets <-
        forAll (Gen.list (Range.linear 0 6) (Gen.int (Range.linear 0 63)))
      let wrapClasses =
            [ (2 + offset, [Node (Wrap (K (rawTarget `mod` (2 + offset))))])
              | (offset, rawTarget) <- zip [0 ..] rawTargets
            ]
      initialHost <-
        evalEither
          ( hostFromNodeClasses
              0
              (IntMap.fromList ([(0, [Node LitA]), (1, [Node LitB])] <> wrapClasses))
          )
      let classCatalog =
            hostClassOrdinalCatalog initialHost
          classCount =
            IntMap.size classCatalog
      mergePairs <-
        forAll
          ( Gen.list
              (Range.linear 1 8)
              ( (,)
                  <$> Gen.int (Range.linear 0 (classCount - 1))
                  <*> Gen.int (Range.linear 0 (classCount - 1))
              )
          )
      eagerHost <-
        evalEither (foldM (eagerMergeStep classCatalog) initialHost mergePairs)
      batchedMergedHost <-
        evalEither (foldM (batchedMergeStep classCatalog) initialHost mergePairs)
      batchedHost <-
        hrrHost <$> evalEither (rebuildHostBarrier batchedMergedHost)
      let classIds =
            IntMap.elems classCatalog
          quotientOf observedHost =
            [ hostCanonicalClass observedHost leftId == hostCanonicalClass observedHost rightId
              | leftId <- classIds,
                rightId <- classIds
            ]
          nodeCountsOf observedHost =
            [ fmap length (flip Map.lookup (Map.fromList (hostNodeClasses observedHost)) =<< hostCanonicalClass observedHost classId)
              | classId <- classIds
            ]
      quotientOf eagerHost === quotientOf batchedHost
      hostClassCount eagerHost === hostClassCount batchedHost
      nodeCountsOf eagerHost === nodeCountsOf batchedHost
  where
    eagerMergeStep ::
      IntMap.IntMap ClassId ->
      Host TinySig ->
      (Int, Int) ->
      Either String (Host TinySig)
    eagerMergeStep classCatalog host (leftKey, rightKey) = do
      leftClass <- lookupClassIdByOrdinal classCatalog leftKey
      rightClass <- lookupClassIdByOrdinal classCatalog rightKey
      HostProgramResult {hprHost = mergedHost} <-
        renderRewriteProgramError (runHostRewriteProgram (mergeClasses leftClass rightClass) host)
      hrrHost <$> renderRewriteProgramError (rebuildHostBarrier mergedHost)

    batchedMergeStep ::
      IntMap.IntMap ClassId ->
      Host TinySig ->
      (Int, Int) ->
      Either String (Host TinySig)
    batchedMergeStep classCatalog host (leftKey, rightKey) = do
      leftClass <- lookupClassIdByOrdinal classCatalog leftKey
      rightClass <- lookupClassIdByOrdinal classCatalog rightKey
      hprHost <$> renderRewriteProgramError (runHostRewriteProgram (mergeClasses leftClass rightClass) host)

hostSectionsMatchProjectionOracle :: TestTree
hostSectionsMatchProjectionOracle =
  testProperty "incremental host sections match the full-fold projection oracle" $
    property $ do
      rawTargets <-
        forAll (Gen.list (Range.linear 0 4) (Gen.int (Range.linear 0 63)))
      rawOperations <-
        forAll
          ( Gen.list
              (Range.linear 0 10)
              ( Gen.choice
                  [ fmap Left (Gen.int (Range.linear 0 63)),
                    fmap
                      Right
                      ( (,)
                          <$> Gen.int (Range.linear 0 63)
                          <*> Gen.int (Range.linear 0 63)
                      )
                  ]
              )
          )
      let wrapClasses =
            [ (2 + offset, [Node (Wrap (K (rawTarget `mod` (2 + offset))))])
              | (offset, rawTarget) <- zip [0 ..] rawTargets
            ]
      initialHost <-
        evalEither
          ( hostFromNodeClasses
              0
              (IntMap.fromList ([(0, [Node LitA]), (1, [Node LitB])] <> wrapClasses))
          )
      sectionsAgree initialHost
      (programHost, _classCatalog) <-
        evalEither (foldM applyOperation (initialHost, hostClassOrdinalCatalog initialHost) rawOperations)
      sectionsAgree programHost
      barrierHost <-
        hrrHost <$> evalEither (rebuildHostBarrier programHost)
      sectionsAgree barrierHost
  where
    sectionsAgree :: Host TinySig -> PropertyT IO ()
    sectionsAgree observedHost =
      hostSections observedHost
        === hostSectionsFromClasses
          ( IntMap.fromList
              [ (classIdKey classId, nodes)
                | (classId, nodes) <- hostNodeClasses observedHost
              ]
          )

    applyOperation ::
      (Host TinySig, IntMap.IntMap ClassId) ->
      Either Int (Int, Int) ->
      Either String (Host TinySig, IntMap.IntMap ClassId)
    applyOperation (host, classCatalog) rawOperation =
      let keyBound =
            IntMap.size classCatalog
       in case rawOperation of
            Left rawTarget -> do
              targetClass <- lookupClassIdByOrdinal classCatalog (rawTarget `mod` keyBound)
              HostProgramResult {hprHost = host', hprValue = addedClass} <-
                renderRewriteProgramError
                  (runHostRewriteProgram (addNode (Node (Wrap (K targetClass)))) host)
              let classCatalog' =
                    if hostRevision host' == hostRevision host
                      then classCatalog
                      else IntMap.insert keyBound addedClass classCatalog
              Right (host', classCatalog')
            Right (rawLeft, rawRight) -> do
              leftClass <- lookupClassIdByOrdinal classCatalog (rawLeft `mod` keyBound)
              rightClass <- lookupClassIdByOrdinal classCatalog (rawRight `mod` keyBound)
              HostProgramResult {hprHost = host'} <-
                renderRewriteProgramError
                  (runHostRewriteProgram (mergeClasses leftClass rightClass) host)
              Right (host', classCatalog)

hostTermVariablesAreRejected :: TestTree
hostTermVariablesAreRejected =
  testCase "host terms reject variables as typed host build errors" $
    case hostFromTerm 0 (var (symbolToken @"x") (symbolToken @"Expr") :: Term TinySig "Expr") of
      Left (HostTermContainsVariable name sortNameValue) -> do
        name @?= "x"
        sortNameString sortNameValue @?= "Expr"

      Left hostError ->
        assertFailure ("expected variable obstruction, got " <> show hostError)

      Right (_host, rootKey) ->
        assertFailure ("expected host build failure, got root " <> show rootKey)

existsQueriesDoNotNeedMatchKeys :: TestTree
existsQueriesDoNotNeedMatchKeys =
  testCase "exists and existsAtRoot work without public match keys" $ do
    (host, rootKey) <-
      assertRight (hostFromTerm 11 (wrap litA))

    engine0 <-
      prepareProgram unwrapProgram host

    unwrapRuleName <-
      expectRuleName "unwrap"
    (engine1, anyExists) <-
      assertRight (existsBaseRule unwrapRuleName Nothing engine0)
    anyExists @?= True

    (engine2, rootExists) <-
      assertRight (existsBaseRule unwrapRuleName (Just rootKey) engine1)
    rootExists @?= True

    (engine3, matches) <-
      assertRight (matchBaseRule unwrapRuleName Nothing engine2)

    case matches >>= (Map.elems . matchBindings) of
      [childKey] -> do
        (_engine4, childExists) <-
          assertRight (existsBaseRule unwrapRuleName (Just childKey) engine3)
        childExists @?= False

      bindings ->
        assertFailure ("expected exactly one child binding, got " <> show bindings)

contextualRuntimeSupportsRepeatedContextUpdates :: TestTree
contextualRuntimeSupportsRepeatedContextUpdates =
  testCase "engine matches repeated changing contextual state without user-built backend state" $ do
    (contextAHost0, contextARoot0) <-
      assertRight (hostFromTerm 1 (wrap litA))
    (contextBHost0, contextBRoot0) <-
      assertRight (hostFromTerm 1 (wrap litB))

    engine0 <-
      prepareProgram contextualProgram emptyHost

    contextA <-
      expectContextName "ctx-a"
    contextB <-
      expectContextName "ctx-b"

    let engine1 =
          setContext contextB contextBHost0 (setContext contextA contextAHost0 engine0)
    (ctxWrapARuleName, ctxWrapBRuleName) <-
      (,) <$> expectRuleName "ctx-wrap-a" <*> expectRuleName "ctx-wrap-b"

    (engine2, contextAHasA) <-
      assertRight (existsContextRule contextA ctxWrapARuleName Nothing engine1)
    contextAHasA @?= True

    (engine3, contextAHasB0) <-
      assertRight (existsContextRule contextA ctxWrapBRuleName Nothing engine2)
    contextAHasB0 @?= False

    (engine4, contextBHasB0) <-
      assertRight (existsContextRule contextB ctxWrapBRuleName (Just contextBRoot0) engine3)
    contextBHasB0 @?= True

    (contextAHost1, contextARoot1) <-
      assertRight (hostFromTerm 2 (wrap litB))

    let engine5 =
          setContext contextA contextAHost1 engine4

    (engine6, contextAHasA1) <-
      assertRight (existsContextRule contextA ctxWrapARuleName (Just contextARoot0) engine5)
    contextAHasA1 @?= False

    (engine7, contextAHasB1) <-
      assertRight (existsContextRule contextA ctxWrapBRuleName (Just contextARoot1) engine6)
    contextAHasB1 @?= True

    (_engine8, contextBStillHasB) <-
      assertRight (existsContextRule contextB ctxWrapBRuleName (Just contextBRoot0) engine7)
    contextBStillHasB @?= True

contextualRuntimeReportsMissingContexts :: TestTree
contextualRuntimeReportsMissingContexts =
  testCase "engine reports missing contexts as typed relational errors" $ do
    engine0 <-
      prepareProgram contextualProgram emptyHost

    contextA <-
      expectContextName "ctx-a"

    ctxWrapARuleName <-
      expectRuleName "ctx-wrap-a"
    case existsContextRule contextA ctxWrapARuleName Nothing engine0 of
      Left (RelationalProgramContextMissing missingContext) ->
        contextNameString missingContext @?= "ctx-a"

      Left otherError ->
        assertFailure ("expected missing context, got " <> show otherError)

      Right (_engine, result) ->
        assertFailure ("expected missing context, got result " <> show result)

litA :: Term TinySig "Expr"
litA =
  node LitA

litB :: Term TinySig "Expr"
litB =
  node LitB

wrap :: Term TinySig "Expr" -> Term TinySig "Expr"
wrap child =
  node (Wrap child)

tinyCost :: Cost TinySig Int
tinyCost =
  Cost $ \case
    LitA ->
      10

    LitB ->
      1

    LitFlag ->
      1

    Wrap (K childCost) ->
      childCost + 1

unwrapProgram :: Program TinySig NoGuardAtom
unwrapProgram =
  program
    ( rule
        "unwrap"
        ( forall_
            (bind (symbolToken @"x") (symbolToken @"Expr"))
            (wrap (var (symbolToken @"x") (symbolToken @"Expr")) ==> var (symbolToken @"x") (symbolToken @"Expr"))
        )
    )

literalProgram :: Program TinySig NoGuardAtom
literalProgram =
  program
    ( do
        rule "is-a" (litA ==> litA)
        rule "is-b" (litB ==> litB)
    )

growProgram :: Program TinySig NoGuardAtom
growProgram =
  program
    (rule "grow-a" (litA ==> wrap litA))

contextualProgram :: Program TinySig NoGuardAtom
contextualProgram =
  program
    ( do
        context "ctx-a"
        context "ctx-b"
        rule "ctx-wrap-a" (at "ctx-a" (at "ctx-b" (wrap litA ==> litA)))
        rule "ctx-wrap-b" (at "ctx-a" (at "ctx-b" (wrap litB ==> litB)))
    )

emptyHostRewriteProgram :: EGraphProgram RewriteApplicationError (Node TinySig ClassId) ()
emptyHostRewriteProgram =
  pure ()

deterministicHostProgram :: ClassId -> EGraphProgram RewriteApplicationError (Node TinySig ClassId) ClassId
deterministicHostProgram rootClass = do
  wrappedClass <-
    addNode (Node (Wrap (K rootClass)))
  mergeClasses rootClass wrappedClass

assertHostObservablyEqual :: Host TinySig -> Host TinySig -> Assertion
assertHostObservablyEqual leftHost rightHost = do
  hostRevision leftHost @?= hostRevision rightHost
  hostClassCount leftHost @?= hostClassCount rightHost
  hostNodeClasses leftHost @?= hostNodeClasses rightHost
  hostSections leftHost @?= hostSections rightHost

assertHostProgramResultsObservablyEqual ::
  (Eq resultValue, Show resultValue) =>
  HostProgramResult TinySig resultValue ->
  HostProgramResult TinySig resultValue ->
  Assertion
assertHostProgramResultsObservablyEqual leftResult rightResult = do
  hprValue leftResult @?= hprValue rightResult
  hprEffect leftResult @?= hprEffect rightResult
  hprDelta leftResult @?= hprDelta rightResult
  hprDirtyResultKeys leftResult @?= hprDirtyResultKeys rightResult
  assertHostObservablyEqual (hprHost leftResult) (hprHost rightResult)

assertRight :: (Show errorValue) => Either errorValue value -> IO value
assertRight eitherValue =
  case eitherValue of
    Left errorValue ->
      assertFailure ("expected Right, got Left " <> show errorValue)

    Right value ->
      pure value

expectHostClassId :: Host sig -> Int -> IO ClassId
expectHostClassId host key =
  maybe
    (assertFailure ("expected host class key " <> show key))
    pure
    (hostClassIdByKey host key)

hostClassIdByKey :: Host sig -> Int -> Maybe ClassId
hostClassIdByKey host key =
  fmap fst
    ( List.find
        (\(classId, _nodes) -> classIdKey classId == key)
        (hostNodeClasses host)
    )

hostClassOrdinalCatalog :: Host sig -> IntMap.IntMap ClassId
hostClassOrdinalCatalog =
  IntMap.fromList
    . zip [0 ..]
    . fmap fst
    . hostNodeClasses

lookupClassIdByOrdinal :: IntMap.IntMap ClassId -> Int -> Either String ClassId
lookupClassIdByOrdinal classCatalog ordinal =
  maybe
    (Left ("missing host class ordinal " <> show ordinal))
    Right
    (IntMap.lookup ordinal classCatalog)

renderRewriteProgramError :: Either RewriteApplicationError value -> Either String value
renderRewriteProgramError =
  \case
    Left rewriteError ->
      Left (show rewriteError)
    Right value ->
      Right value

prepareProgram ::
  Program TinySig NoGuardAtom ->
  Host TinySig ->
  IO (Engine TinySig NoGuardAtom)
prepareProgram programValue host = do
  rulesValue <-
    assertRight (compile programValue)

  pure (prepare rulesValue host)

matchBaseRule ::
  RuleName ->
  Maybe ClassId ->
  Engine TinySig NoGuardAtom ->
  Either (RelationalProgramError TinySig) (Engine TinySig NoGuardAtom, [Match])
matchBaseRule ruleNameValue maybeRoot =
  match
    MatchQuery
      { matchQueryTarget = RewriteBase,
        matchQueryRule = ruleNameValue,
        matchQueryRoot = maybeRoot
      }

existsBaseRule ::
  RuleName ->
  Maybe ClassId ->
  Engine TinySig NoGuardAtom ->
  Either (RelationalProgramError TinySig) (Engine TinySig NoGuardAtom, Bool)
existsBaseRule ruleNameValue maybeRoot engineValue =
  fmap (fmap (not . null)) (matchBaseRule ruleNameValue maybeRoot engineValue)

existsContextRule ::
  ContextName ->
  RuleName ->
  Maybe ClassId ->
  Engine TinySig NoGuardAtom ->
  Either (RelationalProgramError TinySig) (Engine TinySig NoGuardAtom, Bool)
existsContextRule contextNameValue ruleNameValue maybeRoot engineValue =
  fmap (fmap (not . null)) $
    match
      MatchQuery
        { matchQueryTarget = RewriteContext contextNameValue,
          matchQueryRule = ruleNameValue,
          matchQueryRoot = maybeRoot
        }
      engineValue

expectRuleName :: String -> IO RuleName
expectRuleName =
  assertRight . mkRuleName

expectContextName :: String -> IO ContextName
expectContextName =
  assertRight . contextName

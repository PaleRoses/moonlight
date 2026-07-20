{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Data.Foldable qualified as Foldable
import Data.IntSet qualified as IntSet
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy (Proxy (..))
import Data.Set qualified as Set
import Moonlight.Category
  ( AdhesiveCategory (..),
    Category (..),
    HasPullbacks (..),
    HasPushouts (..),
    MonicMatchComponents (..),
    PBPOAdhesiveCategory,
    PushoutComplementComponents (..),
    composeMor,
    mkComposableChain,
    witnessMonic,
  )
import Moonlight.Category.Effect.Harness.Adhesive
  ( adhesiveWitnessMonicSound,
    pbpoPullbackSquareCommutes,
    pbpoPushoutSquareCommutes,
    pullbackMediatorCommutes,
    pushoutComplementSquareCommutes,
  )
import Moonlight.Core
  ( HasConstructorTag (..),
    Pattern (..),
    PatternVar,
    ZipMatch (..),
    patternVariables,
    sameNodeShape,
    zipSameNodeShape,
  )
import Moonlight.Core qualified as EGraph
import Moonlight.Pale.Test.Site.Assertion (expectRight)
import Moonlight.Rewrite.Algebra
  ( CompositionError (..),
    CompositionResult (..),
    DecorationError (..),
    FinRewriteOb (..),
    FiniteRewriteCategory,
    PBPOLegName (..),
    PBPOLegs (..),
    PBPOMatch (..),
    PBPOEndpointPosition (..),
    PBPOEndpointRef (..),
    PBPOEndpointConstraint (..),
    PBPORuleObstruction (..),
    PBPORule,
    PBPOStep (..),
    PBPOStepFailure (..),
    PBPOTypingSquareSide (..),
    PBPOUntypedStep (..),
    PatternProjection (..),
    PatternQuery (..),
    PatternRewrite,
    PatternRewriteError (..),
    PatternSpanModel,
    PatternUnifier (..),
    ProductDecoration,
    ProjectedInterface (..),
    RewriteDecoration (..),
    RewriteOb (..),
    RewriteOrigin (..),
    SpanModel (..),
    SpanOverlap (..),
    TermCat (..),
    TermMor,
    TermOb (..),
    UnificationError (..),
    UnitDecoration (..),
    applyPBPO,
    applyPBPOPlus,
    canonicalPatternRenaming,
    canonicalizePatternRewrite,
    composePatternRewrites,
    emptyPatternProjection,
    finiteRewriteCategory,
    guardedPatternQuery,
    identityPatternRewrite,
    identityTypedRule,
    matchPattern,
    mkPBPORule,
    mkPatternRewrite,
    patternInterfaceLeg,
    patternInterfaceVariables,
    patternQueryConditions,
    prDecoration,
    prInterface,
    prLeft,
    prRight,
    projectPattern,
    projectedInterface,
    renamePattern,
    samePatternRewriteShape,
    simplexComposedRewrite,
    singlePatternQuery,
    termIdentity,
    termMor,
    termMorSource,
    termMorTarget,
    termSubstFromList,
    unifyPatterns,
    unifyPatternsWithApexFreshFrom,
  )
import RefCat
  ( RefCat (..),
    RefMor,
    RefOb (..),
    refInclusion,
    refMorFrom,
    refMorTo,
  )
import SpanModelLaws
  ( checkComposedInterfaceLegLaw,
    checkOverlapProjectionLaw,
  )
import Moonlight.Category.Simplicial
  ( nerveSimplexFromChain,
  )
import Hedgehog
  ( Gen,
    assert,
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
    assertEqual,
    assertFailure,
    testCase,
    (@?=),
  )

main :: IO ()
main =
  defaultMain
    ( testGroup
        "moonlight-rewrite-algebra laws"
        [ refCatLawTests,
          pbpoEngineTests,
          termCatTests,
          patternRewriteConstructorTests,
          decorationLawTests,
          pullbackTests,
          modelLawTests,
          simplexTests,
          compositionLawTests,
          queryConditionOrderTests,
          testPatternZipMatchPairsChildren
        ]
    )

queryConditionOrderTests :: TestTree
queryConditionOrderTests =
  testCase "query conditions preserve inner-to-outer and conjunction order" $ do
    let guardedBranch :: String -> Pattern TestPatternF -> PatternQuery String TestPatternF
        guardedBranch branchName patternValue =
          guardedPatternQuery
            (guardedPatternQuery (singlePatternQuery patternValue) (branchName <> ".inner"))
            (branchName <> ".outer")
        query :: PatternQuery String TestPatternF
        query =
          ConjunctivePatternQuery
            (guardedBranch "left" (patternVar 0) :| [guardedBranch "right" (patternVar 1)])
    patternQueryConditions query
      @?= ["left.inner", "left.outer", "right.inner", "right.outer"]

testPatternZipMatchPairsChildren :: TestTree
testPatternZipMatchPairsChildren =
  testProperty "TestPatternF zipMatch pairs every child and rejects shape mismatches" $
    property $ do
      leftNode <- forAll genTestPatternNode
      rightNode <- forAll genTestPatternNode
      case zipMatch leftNode rightNode of
        Just zipped -> do
          assert (sameNodeShape leftNode rightNode)
          Foldable.toList zipped
            === zip (Foldable.toList leftNode) (Foldable.toList rightNode)
        Nothing ->
          assert (not (sameNodeShape leftNode rightNode))
  where
    genTestPatternNode :: Gen (TestPatternF Int)
    genTestPatternNode =
      Gen.choice
        [ TestPair <$> genChild <*> genChild,
          TestLeaf <$> genChild
        ]

    genChild :: Gen Int
    genChild =
      Gen.int (Range.linear 0 9)

refIncl :: [Int] -> [Int] -> IO RefMor
refIncl fromRefs toRefs =
  maybe
    (assertFailure ("expected inclusion " <> show fromRefs <> " into " <> show toRefs))
    pure
    (refInclusion (IntSet.fromList fromRefs) (IntSet.fromList toRefs))

refSet :: [Int] -> RefOb
refSet =
  RefOb . IntSet.fromList

refCatLawTests :: TestTree
refCatLawTests =
  testGroup
    "RefCat adhesive laws"
    [ testCase "monic witnesses are sound for inclusions" testRefCatMonicSound,
      testCase "pushout complement square commutes" testRefCatComplementSquare,
      testCase "pbpo complement pullback and pushout squares commute" testRefCatPBPOSquares,
      testCase "pullback mediator commutes with the cone" testRefCatMediatorLaw
    ]

testRefCatMonicSound :: Assertion
testRefCatMonicSound = do
  matchArrow <- refIncl [1, 2] [1, 2, 3]
  assertBool
    "inclusion monic witness should be sound"
    ( adhesiveWitnessMonicSound @RefCat
        RefCat
        (\morphism -> refMorFrom morphism `IntSet.isSubsetOf` refMorTo morphism)
        matchArrow
    )

testRefCatComplementSquare :: Assertion
testRefCatComplementSquare = do
  ruleLeg <- refIncl [1] [1, 2]
  matchArrow <- refIncl [1, 2] [1, 2, 3]
  assertBool
    "pushout complement square should commute"
    (pushoutComplementSquareCommutes @RefCat RefCat ruleLeg matchArrow)

testRefCatPBPOSquares :: Assertion
testRefCatPBPOSquares = do
  ruleLeg <- refIncl [1] [1, 2]
  matchArrow <- refIncl [1, 2] [1, 2, 3]
  assertBool
    "pbpo pullback square should commute"
    (pbpoPullbackSquareCommutes @RefCat RefCat ruleLeg matchArrow)
  assertBool
    "pbpo pushout square should commute"
    (pbpoPushoutSquareCommutes @RefCat RefCat ruleLeg matchArrow)

testRefCatMediatorLaw :: Assertion
testRefCatMediatorLaw = do
  leftBase <- refIncl [1, 2] [1, 2, 3]
  rightBase <- refIncl [1, 3] [1, 2, 3]
  coneLeft <- refIncl [1] [1, 2]
  coneRight <- refIncl [1] [1, 3]
  assertBool
    "mediator should commute with the cone"
    (pullbackMediatorCommutes @RefCat RefCat leftBase rightBase coneLeft coneRight)
  mediator <-
    maybe
      (assertFailure "expected a pullback mediator for the commuting cone")
      pure
      (pullbackMediator @RefCat RefCat leftBase rightBase coneLeft coneRight)
  Just mediator @?= refInclusion (IntSet.fromList [1]) (IntSet.fromList [1])

pbpoEngineTests :: TestTree
pbpoEngineTests =
  testGroup
    "PBPO+ engine"
    [ testCase "rule constructor rejects mismatched legs" testRuleEndpointMismatch,
      testCase "rule constructor reports endpoint computation failure" testRuleEndpointComputationFailure,
      testCase "rule constructor reports context-side composition failure" testRuleContextCompositionFailure,
      testCase "rule constructor reports left-side composition failure" testRuleLeftCompositionFailure,
      testCase "rule constructor distinguishes square disagreement from composition failure" testRuleSquareDisagreement,
      testCase "identity-typed rules assemble with commuting typing square" testIdentityTypedRule,
      testCase "identity-typed rules act as identity on hosts" testIdentityTypedRuleActsAsIdentity,
      testCase "step failure constructors are reachable and distinguished" testPBPOStepFailureTaxonomy,
      testCase "rule obstruction constructors are reachable and distinguished" testPBPORuleObstructionTaxonomy,
      testCase "untyped step deletes outside the interface and glues the replacement" testUntypedStep,
      testCase "typed step retains exactly the context licensed by the interface typing" testTypedStepPermissive,
      testCase "restrictive interface typing drops unlicensed context" testTypedStepRestrictive,
      testCase "adherence disagreeing with the left typing is rejected" testAdherenceRejected
    ]

testRuleEndpointMismatch :: Assertion
testRuleEndpointMismatch = do
  leftLeg <- refIncl [1] [1, 2]
  strayRightLeg <- refIncl [2] [2, 4]
  case identityTypedRule RefCat ("bad-rule" :: String) leftLeg strayRightLeg of
    Left (PBPOEndpointConstraintMismatch "bad-rule" constraint leftEndpoint rightEndpoint) -> do
      constraint
        @?= PBPOEndpointConstraint
          (PBPOEndpointRef PBPOLeftLeg PBPOSourceEndpoint)
          (PBPOEndpointRef PBPORightLeg PBPOSourceEndpoint)
      leftEndpoint @?= refSet [1]
      rightEndpoint @?= refSet [2]
    Left otherError ->
      assertFailure ("expected endpoint mismatch, got " <> show otherError)
    Right _ ->
      assertFailure "expected endpoint mismatch rejection"

data RuleTestCat = RuleTestCat

newtype RuleTestOb = RuleTestOb String
  deriving stock (Eq, Show)

data RuleTestError
  = RuleTestEndpointFailure String
  | RuleTestCompositionFailure String
  deriving stock (Eq, Show)

data RuleTestMor = RuleTestMor
  { rtmLabel :: !String,
    rtmSourceResult :: !(Either RuleTestError RuleTestOb),
    rtmTargetResult :: !(Either RuleTestError RuleTestOb),
    rtmComposeFailure :: !(Maybe RuleTestError)
  }
  deriving stock (Eq, Show)

newtype RuleTestCompositor = RuleTestCompositor ()

instance Category RuleTestCat where
  type Ob RuleTestCat = RuleTestOb
  type Mor RuleTestCat = RuleTestMor
  type Compositor RuleTestCat = RuleTestCompositor
  type CategoryError RuleTestCat = RuleTestError

  identity _ objectValue =
    Right
      RuleTestMor
        { rtmLabel = "id",
          rtmSourceResult = Right objectValue,
          rtmTargetResult = Right objectValue,
          rtmComposeFailure = Nothing
        }

  compose _ outer inner =
    case rtmComposeFailure outer of
      Just failure ->
        Left failure
      Nothing -> do
        outerSource <- rtmSourceResult outer
        innerTarget <- rtmTargetResult inner
        if outerSource == innerTarget
          then
            Right
              ( RuleTestMor
                  { rtmLabel = rtmLabel outer <> "/" <> rtmLabel inner,
                    rtmSourceResult = rtmSourceResult inner,
                    rtmTargetResult = rtmTargetResult outer,
                    rtmComposeFailure = Nothing
                  },
                RuleTestCompositor ()
              )
          else Left (RuleTestCompositionFailure "endpoint mismatch")

  source _ =
    rtmSourceResult

  target _ =
    rtmTargetResult

ruleTestOb :: String -> RuleTestOb
ruleTestOb =
  RuleTestOb

ruleTestMor :: String -> String -> String -> RuleTestMor
ruleTestMor label sourceName targetName =
  RuleTestMor
    { rtmLabel = label,
      rtmSourceResult = Right (ruleTestOb sourceName),
      rtmTargetResult = Right (ruleTestOb targetName),
      rtmComposeFailure = Nothing
    }

ruleTestLegs :: PBPOLegs RuleTestCat
ruleTestLegs =
  PBPOLegs
    { plLeftLeg = ruleTestMor "left" "interface" "left",
      plRightLeg = ruleTestMor "right" "interface" "right",
      plLeftTyping = ruleTestMor "leftTyping" "left" "type",
      plInterfaceTyping = ruleTestMor "interfaceTyping" "interface" "context",
      plContextLeg = ruleTestMor "contextLeg" "context" "type"
    }

testRuleEndpointComputationFailure :: Assertion
testRuleEndpointComputationFailure = do
  let failure =
        RuleTestEndpointFailure "left source"
      legs =
        ruleTestLegs
          { plLeftLeg = (plLeftLeg ruleTestLegs) {rtmSourceResult = Left failure}
          }
  mkPBPORule RuleTestCat ("endpoint-failure" :: String) legs
    @?= Left
      ( PBPOEndpointComputationFailed
          "endpoint-failure"
          (PBPOEndpointRef PBPOLeftLeg PBPOSourceEndpoint)
          failure
      )

testRuleContextCompositionFailure :: Assertion
testRuleContextCompositionFailure = do
  let failure =
        RuleTestCompositionFailure "context side"
      legs =
        ruleTestLegs
          { plContextLeg = (plContextLeg ruleTestLegs) {rtmComposeFailure = Just failure}
          }
  mkPBPORule RuleTestCat ("context-composition" :: String) legs
    @?= Left
      (PBPOTypingSquareCompositionFailed "context-composition" PBPOTypingSquareViaContext failure)

testRuleLeftCompositionFailure :: Assertion
testRuleLeftCompositionFailure = do
  let failure =
        RuleTestCompositionFailure "left side"
      legs =
        ruleTestLegs
          { plLeftTyping = (plLeftTyping ruleTestLegs) {rtmComposeFailure = Just failure}
          }
  mkPBPORule RuleTestCat ("left-composition" :: String) legs
    @?= Left
      (PBPOTypingSquareCompositionFailed "left-composition" PBPOTypingSquareViaLeft failure)

testRuleSquareDisagreement :: Assertion
testRuleSquareDisagreement = do
  contextComposite <-
    expectRight (composeMor RuleTestCat (plContextLeg ruleTestLegs) (plInterfaceTyping ruleTestLegs))
  leftComposite <-
    expectRight (composeMor RuleTestCat (plLeftTyping ruleTestLegs) (plLeftLeg ruleTestLegs))
  mkPBPORule RuleTestCat ("square-disagreement" :: String) ruleTestLegs
    @?= Left (PBPOTypingSquareDisagrees "square-disagreement" contextComposite leftComposite)

testPBPORuleObstructionTaxonomy :: Assertion
testPBPORuleObstructionTaxonomy =
  Foldable.traverse_
    ( \(caseName, acceptsFailure, craftedResult) ->
        expectPBPORuleObstruction caseName acceptsFailure craftedResult
    )
    [ ( "endpoint-constraint mismatch",
        \case
          PBPOEndpointConstraintMismatch "rule-taxonomy" _ _ _ ->
            True
          _ ->
            False,
        mkPBPORule
          RuleTestCat
          ("rule-taxonomy" :: String)
          ruleTestLegs {plRightLeg = ruleTestMor "strayRight" "stray" "right"}
      ),
      ( "endpoint-computation failure",
        \case
          PBPOEndpointComputationFailed "rule-taxonomy" (PBPOEndpointRef PBPOLeftLeg PBPOSourceEndpoint) (RuleTestEndpointFailure "left source") ->
            True
          _ ->
            False,
        mkPBPORule
          RuleTestCat
          ("rule-taxonomy" :: String)
          ruleTestLegs {plLeftLeg = (plLeftLeg ruleTestLegs) {rtmSourceResult = Left (RuleTestEndpointFailure "left source")}}
      ),
      ( "typing-square composition failure",
        \case
          PBPOTypingSquareCompositionFailed "rule-taxonomy" PBPOTypingSquareViaContext (RuleTestCompositionFailure "context side") ->
            True
          _ ->
            False,
        mkPBPORule
          RuleTestCat
          ("rule-taxonomy" :: String)
          ruleTestLegs {plContextLeg = (plContextLeg ruleTestLegs) {rtmComposeFailure = Just (RuleTestCompositionFailure "context side")}}
      ),
      ( "typing-square disagreement",
        \case
          PBPOTypingSquareDisagrees "rule-taxonomy" _ _ ->
            True
          _ ->
            False,
        mkPBPORule RuleTestCat ("rule-taxonomy" :: String) ruleTestLegs
      )
    ]

expectPBPORuleObstruction ::
  Show value =>
  String ->
  (PBPORuleObstruction RuleTestCat String -> Bool) ->
  Either (PBPORuleObstruction RuleTestCat String) value ->
  Assertion
expectPBPORuleObstruction caseName acceptsFailure =
  \case
    Left failure
      | acceptsFailure failure ->
          pure ()
      | otherwise ->
          assertFailure ("expected distinguished rule obstruction for " <> caseName <> ", got " <> show failure)
    Right value ->
      assertFailure ("expected rule obstruction for " <> caseName <> ", got Right " <> show value)

testIdentityTypedRule :: Assertion
testIdentityTypedRule = do
  leftLeg <- refIncl [1] [1, 2]
  rightLeg <- refIncl [1] [1, 4]
  _rule <-
    expectRight (identityTypedRule RefCat ("identity-typed" :: String) leftLeg rightLeg)
  pure ()

testIdentityTypedRuleActsAsIdentity :: Assertion
testIdentityTypedRuleActsAsIdentity = do
  identityLeg <- refIncl [1, 2, 3] [1, 2, 3]
  rule <- expectRight (identityTypedRule RefCat ("identity-action" :: String) identityLeg identityLeg)
  monicMatch <-
    maybe (assertFailure "expected identity match to be monic") pure (witnessMonic RefCat identityLeg)
  untypedStep <- expectRight (applyPBPO RefCat rule monicMatch)
  typedStep <-
    expectRight
      ( applyPBPOPlus
          RefCat
          rule
          PBPOMatch
            { pbpoMatchMonic = monicMatch,
              pbpoMatchAdherence = identityLeg
            }
      )
  pbpoUntypedHost untypedStep @?= refSet [1, 2, 3]
  pbpoStepHost typedStep @?= refSet [1, 2, 3]

testUntypedStep :: Assertion
testUntypedStep = do
  leftLeg <- refIncl [1] [1, 2]
  rightLeg <- refIncl [1] [1, 4]
  rule <- expectRight (identityTypedRule RefCat ("delete-2-add-4" :: String) leftLeg rightLeg)
  matchArrow <- refIncl [1, 2] [1, 2, 3]
  monicMatch <-
    maybe (assertFailure "expected monic match witness") pure (witnessMonic RefCat matchArrow)
  step <- expectRight (applyPBPO RefCat rule monicMatch)
  pbpoUntypedHost step @?= refSet [1, 3, 4]

typedRefRule :: [Int] -> IO (PBPOMatch RefCat -> Either String (PBPOStep RefCat String))
typedRefRule contextInterfaceRefs = do
  leftLeg <- refIncl [1] [1, 2]
  rightLeg <- refIncl [1] [1, 4]
  leftTyping <- refIncl [1, 2] [1, 2, 3]
  interfaceTyping <- refIncl [1] contextInterfaceRefs
  contextLeg <- refIncl contextInterfaceRefs [1, 2, 3]
  rule <-
    expectRight
      ( mkPBPORule
          RefCat
          ("typed-rule" :: String)
          PBPOLegs
            { plLeftLeg = leftLeg,
              plRightLeg = rightLeg,
              plLeftTyping = leftTyping,
              plInterfaceTyping = interfaceTyping,
              plContextLeg = contextLeg
            }
      )
  pure
    ( \match ->
        either (Left . show) Right (applyPBPOPlus RefCat rule match)
    )

typedRefMatch :: IO (PBPOMatch RefCat)
typedRefMatch = do
  matchArrow <- refIncl [1, 2] [1, 2, 3]
  adherence <- refIncl [1, 2, 3] [1, 2, 3]
  monicMatch <-
    maybe (assertFailure "expected monic match witness") pure (witnessMonic RefCat matchArrow)
  pure
    PBPOMatch
      { pbpoMatchMonic = monicMatch,
        pbpoMatchAdherence = adherence
      }

testTypedStepPermissive :: Assertion
testTypedStepPermissive = do
  applyTyped <- typedRefRule [1, 3]
  match <- typedRefMatch
  step <- expectRight (applyTyped match)
  pbpoStepContext step @?= refSet [1, 3]
  pbpoStepHost step @?= refSet [1, 3, 4]

testTypedStepRestrictive :: Assertion
testTypedStepRestrictive = do
  applyTyped <- typedRefRule [1]
  match <- typedRefMatch
  step <- expectRight (applyTyped match)
  pbpoStepContext step @?= refSet [1]
  pbpoStepHost step @?= refSet [1, 4]

testAdherenceRejected :: Assertion
testAdherenceRejected = do
  leftLeg <- refIncl [1] [1, 2]
  rightLeg <- refIncl [1] [1, 4]
  leftTyping <- refIncl [1, 2] [1, 2, 3]
  interfaceTyping <- refIncl [1] [1, 3]
  contextLeg <- refIncl [1, 3] [1, 2, 3]
  rule <-
    expectRight
      ( mkPBPORule
          RefCat
          ("typed-rule" :: String)
          PBPOLegs
            { plLeftLeg = leftLeg,
              plRightLeg = rightLeg,
              plLeftTyping = leftTyping,
              plInterfaceTyping = interfaceTyping,
              plContextLeg = contextLeg
            }
      )
  matchArrow <- refIncl [1, 2] [1, 2, 3]
  strayAdherence <- refIncl [1, 2, 3] [1, 2, 3, 9]
  monicMatch <-
    maybe (assertFailure "expected monic match witness") pure (witnessMonic RefCat matchArrow)
  expectedAdherenceTyping <- refIncl [1, 2] [1, 2, 3, 9]
  case applyPBPOPlus RefCat rule PBPOMatch {pbpoMatchMonic = monicMatch, pbpoMatchAdherence = strayAdherence} of
    Left (PBPOAdherenceMismatch "typed-rule" actualAdherenceTyping expectedLeftTyping) -> do
      actualAdherenceTyping @?= expectedAdherenceTyping
      expectedLeftTyping @?= leftTyping
    Left otherFailure ->
      assertFailure ("expected adherence mismatch, got " <> show otherFailure)
    Right _ ->
      assertFailure "expected adherence mismatch rejection"

data StepTestMode
  = StepPermit
  | StepAdherenceComputationFailure
  | StepIllFormedConeFailure
  | StepNoPullbackFailure
  | StepNoMediatorFailure
  | StepNoPushoutFailure
  | StepNoComplementFailure
  deriving stock (Eq, Show)

newtype StepTestCat = StepTestCat
  { stepTestMode :: StepTestMode
  }

data StepTestOb
  = StepInterfaceOb
  | StepLeftOb
  | StepRightOb
  | StepContextOb
  | StepTypeOb
  | StepHostOb
  | StepPullbackOb
  | StepPushoutOb
  | StepComplementOb
  deriving stock (Eq, Show)

data StepTestMorName
  = StepIdentityMor
  | StepLeftLegMor
  | StepRightLegMor
  | StepLeftTypingMor
  | StepInterfaceTypingMor
  | StepContextLegMor
  | StepTypedSquareMor
  | StepMatchMor
  | StepAdherenceMor
  | StepStrayAdherenceMor
  | StepInterfaceToPriorMor
  | StepMediatorMor
  | StepPullbackToLeftMor
  | StepPullbackToRightMor
  | StepPushoutLeftMor
  | StepPushoutRightMor
  | StepBorrowedLegMor
  | StepResidualLegMor
  | StepCompositeMor
  deriving stock (Eq, Show)

data StepTestMor = StepTestMor
  { stmName :: !StepTestMorName,
    stmSource :: !StepTestOb,
    stmTarget :: !StepTestOb
  }
  deriving stock (Eq, Show)

data StepTestError
  = StepTestEndpointMismatch
  | StepTestCompositionBlocked StepTestMode
  deriving stock (Eq, Show)

data StepTestTwoMor

newtype StepTestCompositor = StepTestCompositor ()

instance Category StepTestCat where
  type Ob StepTestCat = StepTestOb
  type Mor StepTestCat = StepTestMor
  type TwoMor StepTestCat = StepTestTwoMor
  type Compositor StepTestCat = StepTestCompositor
  type CategoryError StepTestCat = StepTestError

  identity _ objectValue =
    Right (StepTestMor StepIdentityMor objectValue objectValue)

  compose categoryValue outer inner
    | stepTestMode categoryValue == StepAdherenceComputationFailure
        && outer == stepAdherenceMor
        && inner == stepMatchMor =
        Left (StepTestCompositionBlocked StepAdherenceComputationFailure)
    | stepTestMode categoryValue == StepIllFormedConeFailure
        && outer == stepMatchMor
        && inner == stepLeftLegMor =
        Left (StepTestCompositionBlocked StepIllFormedConeFailure)
    | stmTarget inner == stmSource outer =
        Right (stepTestComposite outer inner, StepTestCompositor ())
    | otherwise =
        Left StepTestEndpointMismatch

  source _ =
    Right . stmSource

  target _ =
    Right . stmTarget

instance HasPullbacks StepTestCat where
  pullback categoryValue leftBase rightBase
    | stepTestMode categoryValue == StepNoPullbackFailure
        && leftBase == stepAdherenceMor
        && rightBase == stepContextLegMor =
        Nothing
    | stmTarget leftBase == stmTarget rightBase =
        Just
          ( StepPullbackOb,
            StepTestMor StepPullbackToLeftMor StepPullbackOb (stmSource leftBase),
            StepTestMor StepPullbackToRightMor StepPullbackOb (stmSource rightBase)
          )
    | otherwise =
        Nothing

  pullbackMediator categoryValue leftBase rightBase coneLeft coneRight
    | stepTestMode categoryValue == StepNoMediatorFailure
        && leftBase == stepAdherenceMor
        && rightBase == stepContextLegMor
        && coneLeft == stepInterfaceToPriorMor
        && coneRight == stepInterfaceTypingMor =
        Nothing
    | stmTarget leftBase == stmTarget rightBase
        && stmTarget coneLeft == stmSource leftBase
        && stmTarget coneRight == stmSource rightBase
        && stmSource coneLeft == stmSource coneRight =
        Just stepMediatorMor
    | otherwise =
        Nothing

instance HasPushouts StepTestCat where
  pushout categoryValue leftLeg rightLeg
    | stepTestMode categoryValue == StepNoPushoutFailure
        && leftLeg == stepMediatorMor
        && rightLeg == stepRightLegMor =
        Nothing
    | stmSource leftLeg == stmSource rightLeg =
        Just
          ( StepPushoutOb,
            StepTestMor StepPushoutLeftMor (stmTarget leftLeg) StepPushoutOb,
            StepTestMor StepPushoutRightMor (stmTarget rightLeg) StepPushoutOb
          )
    | otherwise =
        Nothing

instance AdhesiveCategory StepTestCat where
  monicMatchComponents _ morphism =
    Just (MonicMatchComponents morphism)

  pushoutComplementComponents categoryValue _ _
    | stepTestMode categoryValue == StepNoComplementFailure =
        Nothing
    | otherwise =
        Just
          PushoutComplementComponents
            { pushoutComplementComponentObject = StepComplementOb,
              pushoutComplementComponentBorrowedLeg = StepTestMor StepBorrowedLegMor StepComplementOb StepHostOb,
              pushoutComplementComponentResidualLeg = stepResidualLegMor
            }

instance PBPOAdhesiveCategory StepTestCat

stepLeftLegMor :: StepTestMor
stepLeftLegMor =
  StepTestMor StepLeftLegMor StepInterfaceOb StepLeftOb

stepRightLegMor :: StepTestMor
stepRightLegMor =
  StepTestMor StepRightLegMor StepInterfaceOb StepRightOb

stepLeftTypingMor :: StepTestMor
stepLeftTypingMor =
  StepTestMor StepLeftTypingMor StepLeftOb StepTypeOb

stepInterfaceTypingMor :: StepTestMor
stepInterfaceTypingMor =
  StepTestMor StepInterfaceTypingMor StepInterfaceOb StepContextOb

stepContextLegMor :: StepTestMor
stepContextLegMor =
  StepTestMor StepContextLegMor StepContextOb StepTypeOb

stepTypedSquareMor :: StepTestMor
stepTypedSquareMor =
  StepTestMor StepTypedSquareMor StepInterfaceOb StepTypeOb

stepMatchMor :: StepTestMor
stepMatchMor =
  StepTestMor StepMatchMor StepLeftOb StepHostOb

stepAdherenceMor :: StepTestMor
stepAdherenceMor =
  StepTestMor StepAdherenceMor StepHostOb StepTypeOb

stepStrayAdherenceMor :: StepTestMor
stepStrayAdherenceMor =
  StepTestMor StepStrayAdherenceMor StepHostOb StepContextOb

stepInterfaceToPriorMor :: StepTestMor
stepInterfaceToPriorMor =
  StepTestMor StepInterfaceToPriorMor StepInterfaceOb StepHostOb

stepMediatorMor :: StepTestMor
stepMediatorMor =
  StepTestMor StepMediatorMor StepInterfaceOb StepPullbackOb

stepResidualLegMor :: StepTestMor
stepResidualLegMor =
  StepTestMor StepResidualLegMor StepInterfaceOb StepComplementOb

stepTestComposite :: StepTestMor -> StepTestMor -> StepTestMor
stepTestComposite outer inner
  | outer == stepAdherenceMor && inner == stepMatchMor =
      stepLeftTypingMor
  | outer == stepContextLegMor && inner == stepInterfaceTypingMor =
      stepTypedSquareMor
  | outer == stepLeftTypingMor && inner == stepLeftLegMor =
      stepTypedSquareMor
  | outer == stepMatchMor && inner == stepLeftLegMor =
      stepInterfaceToPriorMor
  | otherwise =
      StepTestMor StepCompositeMor (stmSource inner) (stmTarget outer)

stepTestLegs :: PBPOLegs StepTestCat
stepTestLegs =
  PBPOLegs
    { plLeftLeg = stepLeftLegMor,
      plRightLeg = stepRightLegMor,
      plLeftTyping = stepLeftTypingMor,
      plInterfaceTyping = stepInterfaceTypingMor,
      plContextLeg = stepContextLegMor
    }

stepTestRule :: IO (PBPORule StepTestCat String)
stepTestRule =
  expectRight (mkPBPORule (StepTestCat StepPermit) ("step-law" :: String) stepTestLegs)

stepTestMatch :: StepTestMor -> IO (PBPOMatch StepTestCat)
stepTestMatch adherence =
  do
    monicMatch <-
      maybe
        (assertFailure "expected step-test match to be monic")
        pure
        (witnessMonic (StepTestCat StepPermit) stepMatchMor)
    pure
      PBPOMatch
        { pbpoMatchMonic = monicMatch,
          pbpoMatchAdherence = adherence
        }

testPBPOStepFailureTaxonomy :: Assertion
testPBPOStepFailureTaxonomy = do
  rule <- stepTestRule
  ordinaryMatch <- stepTestMatch stepAdherenceMor
  strayMatch <- stepTestMatch stepStrayAdherenceMor
  monicMatch <-
    maybe
      (assertFailure "expected step-test untyped match to be monic")
      pure
      (witnessMonic (StepTestCat StepPermit) stepMatchMor)
  Foldable.traverse_
    ( \(caseName, expectedFailure, actualFailure) ->
        assertEqual caseName (Left expectedFailure) actualFailure
    )
    [ ( "adherence composition",
        PBPOAdherenceCompositionFailed
          "step-law"
          stepAdherenceMor
          stepMatchMor
          (StepTestCompositionBlocked StepAdherenceComputationFailure),
        () <$ applyPBPOPlus (StepTestCat StepAdherenceComputationFailure) rule ordinaryMatch
      ),
      ( "adherence mismatch",
        PBPOAdherenceMismatch
          "step-law"
          (stepTestComposite stepStrayAdherenceMor stepMatchMor)
          stepLeftTypingMor,
        () <$ applyPBPOPlus (StepTestCat StepPermit) rule strayMatch
      ),
      ( "missing pullback",
        PBPONoPullback "step-law" stepAdherenceMor stepContextLegMor,
        () <$ applyPBPOPlus (StepTestCat StepNoPullbackFailure) rule ordinaryMatch
      ),
      ( "cone composition",
        PBPOConeCompositionFailed
          "step-law"
          stepMatchMor
          stepLeftLegMor
          (StepTestCompositionBlocked StepIllFormedConeFailure),
        () <$ applyPBPOPlus (StepTestCat StepIllFormedConeFailure) rule ordinaryMatch
      ),
      ( "missing mediator",
        PBPONoMediator "step-law" stepInterfaceToPriorMor stepInterfaceTypingMor,
        () <$ applyPBPOPlus (StepTestCat StepNoMediatorFailure) rule ordinaryMatch
      ),
      ( "missing pushout",
        PBPONoPushout "step-law" stepMediatorMor stepRightLegMor,
        () <$ applyPBPOPlus (StepTestCat StepNoPushoutFailure) rule ordinaryMatch
      ),
      ( "missing complement",
        PBPONoComplement "step-law" stepLeftLegMor,
        () <$ applyPBPO (StepTestCat StepNoComplementFailure) rule monicMatch
      )
    ]

termCatTests :: TestTree
termCatTests =
  testGroup
    "TermCat limits"
    [ testCase "identity laws hold for term morphisms" testTermCatIdentityLaws,
      testCase "composition is associative" testTermCatAssociativity,
      testCase "pullback is the relative generalization with commuting square" testTermCatPullbackSquare,
      testCase "pullback mediator commutes with the cone" testTermCatMediatorLaw,
      testCase "pushout over a trivial generalizer agrees with unification" testTermCatPushoutAgreesWithUnifier,
      testCase "term morphisms are occurrence-determined, so hom-sets are thin" testTermCatHomSetsThin,
      testCase "wide matching rejects a repeated-variable conflict at its first possible position" $
        assertWidePatternConflict WideConflictFirst,
      testCase "wide matching rejects a repeated-variable conflict in the middle" $
        assertWidePatternConflict WideConflictMiddle,
      testCase "wide matching rejects a repeated-variable conflict at the last position" $
        assertWidePatternConflict WideConflictLast
    ]

data WideConflictPosition
  = WideConflictFirst
  | WideConflictMiddle
  | WideConflictLast

assertWidePatternConflict :: WideConflictPosition -> Assertion
assertWidePatternConflict conflictPosition =
  case matchPattern generalPattern targetPattern of
    Nothing ->
      pure ()
    Just _ ->
      assertFailure "expected repeated-variable conflict"
  where
    repeatedVariable = EGraph.mkPatternVar 0
    distinctVariable offset = EGraph.mkPatternVar (offset + 1)
    childCount = 65
    repeatedPositions =
      case conflictPosition of
        WideConflictFirst ->
          (0, 1)
        WideConflictMiddle ->
          (0, childCount `div` 2)
        WideConflictLast ->
          (0, childCount - 1)
    generalChildren =
      fmap
        ( \position ->
            if position == fst repeatedPositions || position == snd repeatedPositions
              then PatternVar repeatedVariable
              else PatternVar (distinctVariable position)
        )
        [0 .. childCount - 1]
    leaf = PatternNode []
    distinctLeaf = PatternNode [leaf]
    targetChildren =
      fmap
        ( \position ->
            if position == snd repeatedPositions
              then distinctLeaf
              else leaf
        )
        [0 .. childCount - 1]
    generalPattern = PatternNode generalChildren
    targetPattern = PatternNode targetChildren

termFromList :: Pattern TestPatternF -> [(PatternVar, Pattern TestPatternF)] -> TermMor TestPatternF
termFromList sourcePattern bindings =
  termMor sourcePattern (termSubstFromList bindings)

testTermCatIdentityLaws :: Assertion
testTermCatIdentityLaws = do
  let morphism =
        termFromList (testPairPattern 0 1) [(EGraph.mkPatternVar 0, testLeafPattern 1)]
  composeMor (TermCat @TestPatternF) morphism (termIdentity (termMorSource morphism)) @?= Right morphism
  composeMor (TermCat @TestPatternF) (termIdentity (termMorTarget morphism)) morphism @?= Right morphism

testTermCatAssociativity :: Assertion
testTermCatAssociativity = do
  let first' =
        termFromList (patternVar 0) [(EGraph.mkPatternVar 0, testPairPattern 1 2)]

      second' =
        termFromList (testPairPattern 1 2) [(EGraph.mkPatternVar 1, testLeafPattern 1)]

      third' =
        termFromList
          (PatternNode (TestPair (testLeafPattern 1) (patternVar 2)))
          [(EGraph.mkPatternVar 2, testLeafPattern 2)]

      leftAssociated = do
        inner <- composeMor (TermCat @TestPatternF) second' first'
        composeMor (TermCat @TestPatternF) third' inner

      rightAssociated = do
        outer <- composeMor (TermCat @TestPatternF) third' second'
        composeMor (TermCat @TestPatternF) outer first'

  assertBool "expected both association orders to compose" (case leftAssociated of Right _ -> True; Left _ -> False)
  leftAssociated @?= rightAssociated

termPullbackFixture ::
  ( TermMor TestPatternF,
    TermMor TestPatternF
  )
termPullbackFixture =
  ( termFromList
      (PatternNode (TestPair (patternVar 0) (testLeafPattern 2)))
      [(EGraph.mkPatternVar 0, testLeafPattern 1)],
    termFromList
      (PatternNode (TestPair (testLeafPattern 1) (patternVar 1)))
      [(EGraph.mkPatternVar 1, testLeafPattern 2)]
  )

testTermCatPullbackSquare :: Assertion
testTermCatPullbackSquare = do
  let (leftBase, rightBase) =
        termPullbackFixture
  (TermOb apexPattern, projLeft, projRight) <-
    maybe (assertFailure "expected a term pullback") pure (pullback (TermCat @TestPatternF) leftBase rightBase)
  apexPattern @?= testPairPattern 0 1
  composeMor (TermCat @TestPatternF) leftBase projLeft @?= composeMor (TermCat @TestPatternF) rightBase projRight
  assertBool
    "pullback square should land in the cospan target"
    (case composeMor (TermCat @TestPatternF) leftBase projLeft of Right _ -> True; Left _ -> False)

testTermCatMediatorLaw :: Assertion
testTermCatMediatorLaw = do
  let (leftBase, rightBase) =
        termPullbackFixture

      coneLeft =
        termFromList
          (testPairPattern 5 6)
          [(EGraph.mkPatternVar 5, patternVar 0), (EGraph.mkPatternVar 6, testLeafPattern 2)]

      coneRight =
        termFromList
          (testPairPattern 5 6)
          [(EGraph.mkPatternVar 5, testLeafPattern 1), (EGraph.mkPatternVar 6, patternVar 1)]

  assertBool
    "mediator should commute with the cone"
    (pullbackMediatorCommutes @(TermCat TestPatternF) (TermCat @TestPatternF) leftBase rightBase coneLeft coneRight)
  assertBool
    "expected a mediator for the commuting cone"
    (pullbackMediator (TermCat @TestPatternF) leftBase rightBase coneLeft coneRight /= Nothing)

testTermCatPushoutAgreesWithUnifier :: Assertion
testTermCatPushoutAgreesWithUnifier = do
  let leftPattern =
        testPairPattern 0 1

      rightPattern =
        PatternNode (TestPair (patternVar 2) (testLeafPattern 5))

      spanApex =
        patternVar 9

      leftLeg =
        termFromList spanApex [(EGraph.mkPatternVar 9, leftPattern)]

      rightLeg =
        termFromList spanApex [(EGraph.mkPatternVar 9, rightPattern)]

  (TermOb pushoutPattern, _, _) <-
    maybe (assertFailure "expected a term pushout") pure (pushout (TermCat @TestPatternF) leftLeg rightLeg)

  unifierWitness <-
    expectRight (unifyPatterns leftPattern rightPattern)

  canonicalShape pushoutPattern @?= canonicalShape (puUnifiedPattern unifierWitness)
  where
    canonicalShape :: Pattern TestPatternF -> Pattern TestPatternF
    canonicalShape patternValue =
      renamePattern
        (canonicalPatternRenaming (patternVariables patternValue))
        patternValue

testTermCatHomSetsThin :: Assertion
testTermCatHomSetsThin = do
  let groundMatch =
        termFromList
          (testPairPattern 0 1)
          [(EGraph.mkPatternVar 0, testLeafPattern 1), (EGraph.mkPatternVar 1, testLeafPattern 1)]

      relabel =
        termFromList
          (testPairPattern 7 8)
          [(EGraph.mkPatternVar 7, patternVar 0), (EGraph.mkPatternVar 8, patternVar 1)]

      swappedAttempt =
        termFromList
          (testPairPattern 7 8)
          [(EGraph.mkPatternVar 7, patternVar 1), (EGraph.mkPatternVar 8, patternVar 0)]

  matchPattern (testPairPattern 7 8) (testPairPattern 0 1)
    @?= Just (termSubstFromList [(EGraph.mkPatternVar 7, patternVar 0), (EGraph.mkPatternVar 8, patternVar 1)])
  assertBool
    "the swapped binding lands in a different target, not a second parallel morphism"
    (termMorTarget swappedAttempt /= termMorSource groundMatch)
  composeMor (TermCat @TestPatternF) groundMatch swappedAttempt @?= Left ()
  assertBool
    "the occurrence-determined morphism composes with the ground match"
    (case composeMor (TermCat @TestPatternF) groundMatch relabel of Right _ -> True; Left _ -> False)

data TestPatternF a
  = TestPair a a
  | TestLeaf Int
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

data TestPatternTag
  = TestPairTag
  | TestLeafTag Int
  deriving stock (Eq, Ord)

instance HasConstructorTag TestPatternF where
  type ConstructorTag TestPatternF = TestPatternTag

  constructorTag =
    \case
      TestPair _ _ ->
        TestPairTag
      TestLeaf leafKey ->
        TestLeafTag leafKey

instance ZipMatch TestPatternF where
  zipMatch =
    zipSameNodeShape

patternVar :: Int -> Pattern TestPatternF
patternVar =
  PatternVar . EGraph.mkPatternVar

testPairPattern :: Int -> Int -> Pattern TestPatternF
testPairPattern leftVar rightVar =
  PatternNode (TestPair (patternVar leftVar) (patternVar rightVar))

testLeafPattern :: Int -> Pattern TestPatternF
testLeafPattern =
  PatternNode . TestLeaf

testSwapRewrite :: String -> IO (PatternRewrite String UnitDecoration TestPatternF)
testSwapRewrite originName =
  expectRight
    ( mkPatternRewrite
        (RewriteAtomic originName)
        (testPairPattern 0 1)
        (Set.fromList [EGraph.mkPatternVar 0, EGraph.mkPatternVar 1])
        (testPairPattern 1 0)
        UnitDecoration
    )

newtype VariableDecoration f = VariableDecoration (Set.Set PatternVar)
  deriving stock (Eq, Show)

instance RewriteDecoration VariableDecoration where
  emptyDecoration =
    VariableDecoration Set.empty

  decorationVariables (VariableDecoration variables) =
    variables

  renameDecoration _ =
    id

  projectDecoration _ =
    Right

  composeDecoration (VariableDecoration leftVariables) (VariableDecoration rightVariables) =
    Right (VariableDecoration (leftVariables <> rightVariables))

patternRewriteConstructorTests :: TestTree
patternRewriteConstructorTests =
  testGroup
    "PatternRewrite constructor rejections"
    [ testCase "interface variables must occur in the left pattern" testPatternRewriteRejectsInterfaceMissingFromLeft,
      testCase "interface variables must occur in the right pattern" testPatternRewriteRejectsInterfaceMissingFromRight,
      testCase "both interface sides report together before decoration validation" testPatternRewriteReportsBothMissingInterfaceSides,
      testCase "invalid decoration is rejected after interface validation" testPatternRewriteRejectsInvalidDecoration
    ]

testPatternRewriteRejectsInterfaceMissingFromLeft :: Assertion
testPatternRewriteRejectsInterfaceMissingFromLeft =
  mkPatternRewrite
    (RewriteAtomic "missing-left")
    (patternVar 0)
    (Set.fromList [EGraph.mkPatternVar 0, EGraph.mkPatternVar 1])
    (testPairPattern 0 1)
    UnitDecoration
    @?= Left (RewriteInterfaceNotInLeft [EGraph.mkPatternVar 1])

testPatternRewriteRejectsInterfaceMissingFromRight :: Assertion
testPatternRewriteRejectsInterfaceMissingFromRight =
  mkPatternRewrite
    (RewriteAtomic "missing-right")
    (testPairPattern 0 1)
    (Set.fromList [EGraph.mkPatternVar 0, EGraph.mkPatternVar 1])
    (patternVar 0)
    UnitDecoration
    @?= Left (RewriteInterfaceNotInRight [EGraph.mkPatternVar 1])

testPatternRewriteReportsBothMissingInterfaceSides :: Assertion
testPatternRewriteReportsBothMissingInterfaceSides =
  mkPatternRewrite
    (RewriteAtomic "missing-both")
    (patternVar 0)
    (Set.fromList [EGraph.mkPatternVar 0, EGraph.mkPatternVar 1])
    (patternVar 1)
    (VariableDecoration (Set.singleton (EGraph.mkPatternVar 9)))
    @?= Left
      ( RewriteInterfaceNotInBoth
          [EGraph.mkPatternVar 1]
          [EGraph.mkPatternVar 0]
      )

testPatternRewriteRejectsInvalidDecoration :: Assertion
testPatternRewriteRejectsInvalidDecoration =
  mkPatternRewrite
    (RewriteAtomic "invalid-decoration")
    (patternVar 0)
    (Set.singleton (EGraph.mkPatternVar 0))
    (patternVar 0)
    (VariableDecoration (Set.singleton (EGraph.mkPatternVar 1)))
    @?= Left
      ( RewriteInvalidDecoration
          (DecorationUnboundVariables [EGraph.mkPatternVar 1])
      )

decorationLawTests :: TestTree
decorationLawTests =
  testGroup
    "Decoration laws"
    [ testCase "UnitDecoration empty validates for any variable set" testUnitEmptyDecorationValidates,
      testCase "ProductDecoration empty validates for any variable set" testProductEmptyDecorationValidates,
      testCase "projection failures surface the decoration-owned obstruction" testDecorationProjectionTypedObstruction,
      testCase "identity rewrite has full interface" testIdentityRewriteHasFullInterface
    ]

testUnitEmptyDecorationValidates :: Assertion
testUnitEmptyDecorationValidates =
  validateDecoration
    (Set.fromList [EGraph.mkPatternVar 0, EGraph.mkPatternVar 1])
    (emptyDecoration :: UnitDecoration TestPatternF)
    @?= Right ()

testProductEmptyDecorationValidates :: Assertion
testProductEmptyDecorationValidates =
  validateDecoration
    (Set.fromList [EGraph.mkPatternVar 0, EGraph.mkPatternVar 1])
    (emptyDecoration :: ProductDecoration UnitDecoration UnitDecoration TestPatternF)
    @?= Right ()

testDecorationProjectionTypedObstruction :: Assertion
testDecorationProjectionTypedObstruction =
  projectDecoration
    (emptyPatternProjection :: PatternProjection TestPatternF)
    (RejectProjectedDecoration :: RejectingDecoration TestPatternF)
    @?= Left (DecorationInvalidProjection RejectingProjectionRejected)

testIdentityRewriteHasFullInterface :: Assertion
testIdentityRewriteHasFullInterface =
  let patternValue = testPairPattern 0 1
      rewriteValue = identityPatternRewrite patternValue :: PatternRewrite String UnitDecoration TestPatternF
   in patternInterfaceVariables (prInterface rewriteValue) @?= patternVariables patternValue

pullbackTests :: TestTree
pullbackTests =
  testGroup
    "Overlap laws"
    [ testCase "left projection maps the left boundary to the apex" testPullbackLeftProjectionReachesApex,
      testCase "right projection maps the right boundary to the apex" testPullbackRightProjectionReachesApex,
      testCase "apex variables avoid the forbidden set" testPullbackApexAvoidsForbiddenVars
    ]

testPullbackLeftBoundary :: Pattern TestPatternF
testPullbackLeftBoundary =
  PatternNode (TestPair (PatternVar (EGraph.mkPatternVar 0)) (PatternVar (EGraph.mkPatternVar 1)))

testPullbackRightBoundary :: Pattern TestPatternF
testPullbackRightBoundary =
  PatternNode (TestPair (PatternVar (EGraph.mkPatternVar 1)) (PatternVar (EGraph.mkPatternVar 2)))

testFirstOrderOverlap ::
  Set.Set PatternVar ->
  Pattern TestPatternF ->
  Pattern TestPatternF ->
  Either UnificationError (PatternUnifier TestPatternF)
testFirstOrderOverlap =
  unifyPatternsWithApexFreshFrom

testPullbackLeftProjectionReachesApex :: Assertion
testPullbackLeftProjectionReachesApex =
  case testFirstOrderOverlap Set.empty testPullbackLeftBoundary testPullbackRightBoundary of
    Left overlapError ->
      assertFailure ("expected overlap witness, got " <> show overlapError)
    Right witness ->
      projectPattern (PatternProjection (puLeftMap witness)) testPullbackLeftBoundary
        @?= puUnifiedPattern witness

testPullbackRightProjectionReachesApex :: Assertion
testPullbackRightProjectionReachesApex =
  case testFirstOrderOverlap Set.empty testPullbackLeftBoundary testPullbackRightBoundary of
    Left overlapError ->
      assertFailure ("expected overlap witness, got " <> show overlapError)
    Right witness ->
      projectPattern (PatternProjection (puRightMap witness)) testPullbackRightBoundary
        @?= puUnifiedPattern witness

testPullbackApexAvoidsForbiddenVars :: Assertion
testPullbackApexAvoidsForbiddenVars =
  let forbiddenVars = Set.fromList [EGraph.mkPatternVar 0, EGraph.mkPatternVar 1, EGraph.mkPatternVar 2]
   in case testFirstOrderOverlap forbiddenVars (PatternVar (EGraph.mkPatternVar 0) :: Pattern TestPatternF) (PatternVar (EGraph.mkPatternVar 1)) of
        Left overlapError ->
          assertFailure ("expected overlap witness, got " <> show overlapError)
        Right witness ->
          Set.intersection forbiddenVars (patternVariables (puUnifiedPattern witness))
            @?= Set.empty

modelLawTests :: TestTree
modelLawTests =
  testGroup
    "Span model laws"
    [ testCase "overlap projections reach the model apex" testModelOverlapProjectionLaw,
      testCase "composed interface legs validate" testModelComposedInterfaceLegLaw
    ]

testPatternSpanModel :: Proxy (PatternSpanModel TestPatternF)
testPatternSpanModel =
  Proxy

testModelOverlapProjectionLaw :: Assertion
testModelOverlapProjectionLaw =
  case checkOverlapProjectionLaw testPatternSpanModel Set.empty testPullbackLeftBoundary testPullbackRightBoundary of
    Left overlapError ->
      assertFailure ("expected model overlap witness, got " <> show overlapError)
    Right failures ->
      failures @?= []

testModelComposedInterfaceLegLaw :: Assertion
testModelComposedInterfaceLegLaw =
  case spanOverlapFreshFrom testPatternSpanModel Set.empty testPullbackLeftBoundary testPullbackRightBoundary of
    Left overlapError ->
      assertFailure ("expected model overlap witness, got " <> show overlapError)
    Right witness -> do
      let leftProjected =
            projectedInterface
              testPatternSpanModel
              (spanOverlapLeftProjection testPatternSpanModel witness)
              testPullbackLeftBoundary
              (spanIdentityInterface testPatternSpanModel testPullbackLeftBoundary)
              patternInterfaceLeg

          rightProjected =
            projectedInterface
              testPatternSpanModel
              (spanOverlapRightProjection testPatternSpanModel witness)
              testPullbackRightBoundary
              (spanIdentityInterface testPatternSpanModel testPullbackRightBoundary)
              patternInterfaceLeg

      case spanComposeInterfaces testPatternSpanModel witness leftProjected rightProjected of
        Left modelError ->
          assertFailure ("expected composed model interface, got " <> show modelError)
        Right composedInterface ->
          checkComposedInterfaceLegLaw
            testPatternSpanModel
            (piObject leftProjected)
            (piObject rightProjected)
            composedInterface
            @?= []

simplexTests :: TestTree
simplexTests =
  testGroup
    "Rewrite nerve simplex composition"
    [ testCase "empty simplex reports the empty rewrite-chain obstruction" testEmptySimplexCompositionReportsEmptyChain
    ]

testEmptySimplexCompositionReportsEmptyChain :: Assertion
testEmptySimplexCompositionReportsEmptyChain = do
  let categoryValue :: FiniteRewriteCategory String UnitDecoration TestPatternF
      categoryValue =
        finiteRewriteCategory []
      startObject :: FinRewriteOb String UnitDecoration TestPatternF
      startObject =
        FinRewriteOb
          (RewriteOb (patternVar 0))
  chainValue <-
    case mkComposableChain categoryValue startObject [] of
      Right chainValue ->
        pure chainValue
      Left _ ->
        assertFailure "expected empty composable chain"
  simplexComposedRewrite (nerveSimplexFromChain chainValue)
    @?= Left EmptyRewriteChain

compositionLawTests :: TestTree
compositionLawTests =
  testGroup
    "Composition laws"
    [ testCase "left identity" testCompositionLeftIdentity,
      testCase "right identity" testCompositionRightIdentity,
      testCase "associativity modulo canonicalization" testCompositionAssociativeModuloCanonicalization,
      testCase "incompatible boundaries fail deterministically" testCompositionIncompatibleBoundaryDeterministic,
      testCase "invalid composed decorations are reported explicitly" testCompositionInvalidDecorationFailure,
      testCase "invalid composed rewrites are reported explicitly" testCompositionInvalidRewriteFailure,
      testCase "decoration projection happens before decoration composition" testDecorationProjectionBeforeComposition
    ]

composeRewriteOnly ::
  PatternRewrite String UnitDecoration TestPatternF ->
  PatternRewrite String UnitDecoration TestPatternF ->
  Either (CompositionError UnitDecoration TestPatternF) (PatternRewrite String UnitDecoration TestPatternF)
composeRewriteOnly leftRewrite rightRewrite =
  crRewrite <$> composePatternRewrites leftRewrite rightRewrite

assertSameCanonicalShape ::
  PatternRewrite String UnitDecoration TestPatternF ->
  PatternRewrite String UnitDecoration TestPatternF ->
  Assertion
assertSameCanonicalShape leftRewrite rightRewrite =
  assertBool
    "expected same canonical rewrite shape"
    ( samePatternRewriteShape
        (canonicalizePatternRewrite leftRewrite)
        (canonicalizePatternRewrite rightRewrite)
    )

testCompositionLeftIdentity :: Assertion
testCompositionLeftIdentity = do
  rewriteValue <- testSwapRewrite "r"
  composedRewrite <-
    expectRight
      (composeRewriteOnly (identityPatternRewrite (prLeft rewriteValue)) rewriteValue)
  assertSameCanonicalShape composedRewrite rewriteValue

testCompositionRightIdentity :: Assertion
testCompositionRightIdentity = do
  rewriteValue <- testSwapRewrite "r"
  composedRewrite <-
    expectRight
      (composeRewriteOnly rewriteValue (identityPatternRewrite (prRight rewriteValue)))
  assertSameCanonicalShape composedRewrite rewriteValue

testCompositionAssociativeModuloCanonicalization :: Assertion
testCompositionAssociativeModuloCanonicalization = do
  rewriteR <- testSwapRewrite "r"
  rewriteS <- testSwapRewrite "s"
  rewriteT <- testSwapRewrite "t"

  rs <- expectRight (composeRewriteOnly rewriteR rewriteS)
  leftAssociated <- expectRight (composeRewriteOnly rs rewriteT)

  st <- expectRight (composeRewriteOnly rewriteS rewriteT)
  rightAssociated <- expectRight (composeRewriteOnly rewriteR st)

  assertSameCanonicalShape leftAssociated rightAssociated

testCompositionIncompatibleBoundaryDeterministic :: Assertion
testCompositionIncompatibleBoundaryDeterministic = do
  leftRewrite <-
    expectRight
      ( mkPatternRewrite
          (RewriteAtomic "left")
          (testLeafPattern 0)
          Set.empty
          (testLeafPattern 0)
          UnitDecoration
      )
  rightRewrite <-
    expectRight
      ( mkPatternRewrite
          (RewriteAtomic "right")
          (testLeafPattern 1)
          Set.empty
          (testLeafPattern 1)
          UnitDecoration
      )

  case composePatternRewrites leftRewrite rightRewrite of
    Left (IncompatibleBoundary _ _ ConstructorMismatch) ->
      pure ()
    Left otherError ->
      assertFailure ("expected constructor mismatch, got " <> show otherError)
    Right _ ->
      assertFailure "expected incompatible boundary failure"

newtype TraceDecoration f = TraceDecoration [String]
  deriving stock (Eq, Show)

instance RewriteDecoration TraceDecoration where
  emptyDecoration =
    TraceDecoration []

  decorationVariables _ =
    Set.empty

  renameDecoration _ =
    id

  projectDecoration _ (TraceDecoration events) =
    Right (TraceDecoration (events <> ["project"]))

  composeDecoration (TraceDecoration leftEvents) (TraceDecoration rightEvents) =
    Right (TraceDecoration (leftEvents <> rightEvents <> ["compose"]))

  validateDecoration _ _ =
    Right ()

data RejectingDecoration f
  = AcceptingDecoration
  | RejectProjectedDecoration
  deriving stock (Eq, Show)

data RejectingDecorationObstruction
  = RejectingProjectionRejected
  deriving stock (Eq, Show)

instance RewriteDecoration RejectingDecoration where
  type DecorationObstruction RejectingDecoration f = RejectingDecorationObstruction

  emptyDecoration =
    AcceptingDecoration

  decorationVariables _ =
    Set.empty

  renameDecoration _ =
    id

  projectDecoration _ decoration =
    case decoration of
      AcceptingDecoration ->
        Right AcceptingDecoration
      RejectProjectedDecoration ->
        Left (DecorationInvalidProjection RejectingProjectionRejected)

  composeDecoration _ _ =
    Right AcceptingDecoration

  validateDecoration _ _ =
    Right ()

newtype LeakingDecoration f = LeakingDecoration (Set.Set PatternVar)

instance RewriteDecoration LeakingDecoration where
  emptyDecoration =
    LeakingDecoration Set.empty

  decorationVariables (LeakingDecoration variables) =
    variables

  renameDecoration _ =
    id

  projectDecoration _ =
    Right

  composeDecoration _ _ =
    Right (LeakingDecoration (Set.singleton (EGraph.mkPatternVar 99)))

testTraceRewrite ::
  String ->
  Pattern TestPatternF ->
  Pattern TestPatternF ->
  [String] ->
  IO (PatternRewrite String TraceDecoration TestPatternF)
testTraceRewrite originName leftPattern rightPattern events =
  expectRight
    ( mkPatternRewrite
        (RewriteAtomic originName)
        leftPattern
        (Set.intersection (patternVariables leftPattern) (patternVariables rightPattern))
        rightPattern
        (TraceDecoration events)
    )

testCompositionInvalidDecorationFailure :: Assertion
testCompositionInvalidDecorationFailure = do
  leftRewrite <-
    expectRight
      ( mkPatternRewrite
          (RewriteAtomic "left")
          (testPairPattern 0 1)
          (Set.fromList [EGraph.mkPatternVar 0, EGraph.mkPatternVar 1])
          (testPairPattern 1 0)
          RejectProjectedDecoration
      )
  rightRewrite <-
    expectRight
      ( mkPatternRewrite
          (RewriteAtomic "right")
          (testPairPattern 2 3)
          (Set.fromList [EGraph.mkPatternVar 2, EGraph.mkPatternVar 3])
          (testPairPattern 3 2)
          AcceptingDecoration
      )
  case composePatternRewrites leftRewrite rightRewrite of
    Left (InvalidComposedDecoration (DecorationInvalidProjection RejectingProjectionRejected)) ->
      pure ()
    Left otherError ->
      assertFailure ("expected invalid composed decoration, got " <> show otherError)
    Right _ ->
      assertFailure "expected invalid composed decoration failure"

testCompositionInvalidRewriteFailure :: Assertion
testCompositionInvalidRewriteFailure = do
  leftRewrite <-
    expectRight
      ( mkPatternRewrite
          (RewriteAtomic "left")
          (testPairPattern 0 1)
          (Set.fromList [EGraph.mkPatternVar 0, EGraph.mkPatternVar 1])
          (testPairPattern 1 0)
          (LeakingDecoration Set.empty)
      )
  rightRewrite <-
    expectRight
      ( mkPatternRewrite
          (RewriteAtomic "right")
          (testPairPattern 2 3)
          (Set.fromList [EGraph.mkPatternVar 2, EGraph.mkPatternVar 3])
          (testPairPattern 3 2)
          (LeakingDecoration Set.empty)
      )
  case composePatternRewrites leftRewrite rightRewrite of
    Left (InvalidComposedRewrite (RewriteInvalidDecoration (DecorationUnboundVariables [missingVar]))) ->
      missingVar @?= EGraph.mkPatternVar 99
    Left otherError ->
      assertFailure ("expected invalid composed rewrite, got " <> show otherError)
    Right _ ->
      assertFailure "expected invalid composed rewrite failure"

testDecorationProjectionBeforeComposition :: Assertion
testDecorationProjectionBeforeComposition = do
  leftRewrite <-
    testTraceRewrite
      "left"
      (testPairPattern 0 1)
      (testPairPattern 1 0)
      ["left"]

  rightRewrite <-
    testTraceRewrite
      "right"
      (testPairPattern 2 3)
      (testPairPattern 3 2)
      ["right"]

  compositionResult <-
    expectRight (composePatternRewrites leftRewrite rightRewrite)

  prDecoration (crRewrite compositionResult)
    @?= TraceDecoration ["left", "project", "right", "project", "compose"]

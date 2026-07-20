{-# LANGUAGE TypeFamilies #-}

module Moonlight.Sheaf.Surface.PresentationSpec
  ( tests,
  )
where

import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Moonlight.Sheaf
  ( CheckedMorphism (..),
    CompiledRestriction,
    CoverGluingFailure (..),
    FiniteMeetSite,
    FiniteMeetSiteBuildError (..),
    FiniteMeetSiteSpec (..),
    GluingAlgebra (..),
    GluingObstruction (..),
    amalgamatedStalk,
    compatibleMatchingFamilyUnderlying,
    compile,
    glue,
    matching,
    matchingSections,
    finiteMeetMorphism,
    mkFiniteMeetSite,
    preparedCovers,
    siteSpec,
  )
import Moonlight.Sheaf.Presentation
  ( CompiledPresentation,
    FinitePresheaf,
    FinitePresheafFailure (..),
    FinitePresheafMorphismFailure (..),
    Presentation,
    PresentationObstruction (..),
    PresentedRestrictionFailure (..),
    StalkRestrictionKernel (..),
    componentAt,
    compilePresentation,
    declareCell,
    declareComposition,
    declareCover,
    declareFiber,
    declareIdentityMorphism,
    declareMorphism,
    declarePresheaf,
    declareRefinement,
    finiteFiberAt,
    finiteFiberValues,
    finitePresheafMorphismComponentAt,
    finitePresheafMorphismComponents,
    finitePresheafMorphismSource,
    finitePresheafMorphismTarget,
    presentationMorphismAt,
    presentationPresheafAt,
    presentationSite,
    restrictPresentedPresheaf,
    restricts,
  )
import Moonlight.Sheaf.Stalk
  ( DiscreteMismatch (..),
    DiscreteRepairObstruction,
    StalkAlgebra,
    discreteStalkAlgebra,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)

data ToyCell
  = Bottom
  | LeftCell
  | RightCell
  | Whole
  deriving stock (Eq, Ord, Show)

newtype ToyStalk = ToyStalk Int
  deriving stock (Eq, Ord, Show)

type ToyMismatch = DiscreteMismatch ToyStalk

type ToyRepair = DiscreteRepairObstruction ToyStalk

data ToyPresheafName
  = MainPresheaf
  | SourcePresheaf
  | TargetPresheaf
  | UnknownPresheaf
  deriving stock (Eq, Ord, Show)

data ToyMorphismName
  = SwapMorphism
  | ZeroMorphism
  | IdentityMorphism
  | LeftUnitMorphism
  | RightUnitMorphism
  | SwapAfterZeroMorphism
  | ZeroAfterSwapMorphism
  | AssocLeftMorphism
  | AssocRightMorphism
  | TestMorphism
  | OuterMorphism
  | InnerMorphism
  | CompositeMorphism
  | UnknownMorphism
  deriving stock (Eq, Ord, Show)

type ToyPresentation a =
  Presentation
    ToyCell
    ToyPresheafName
    ToyMorphismName
    ToyStalk
    ToyMismatch
    a

type ToyCompiled =
  CompiledPresentation
    ToyCell
    ToyPresheafName
    ToyMorphismName
    ToyStalk
    ToyMismatch

type ToyPresentationObstruction =
  PresentationObstruction
    ToyCell
    ToyPresheafName
    ToyMorphismName
    ToyStalk
    ToyMismatch

data ToyGluingFailure = ToyGluingRejected
  deriving stock (Eq, Show)

toyMismatches :: ToyCell -> ToyStalk -> ToyStalk -> [ToyMismatch]
toyMismatches _cellValue leftValue rightValue =
  [DiscreteMismatch leftValue rightValue | leftValue /= rightValue]

toyNormalize :: ToyCell -> ToyStalk -> ToyStalk
toyNormalize _cellValue =
  id

toyAlgebra ::
  StalkAlgebra (CompiledRestriction (FiniteMeetSite ToyCell)) ToyStalk ToyMismatch ToyRepair
toyAlgebra =
  discreteStalkAlgebra

toyCells :: [ToyCell]
toyCells =
  [Bottom, LeftCell, RightCell, Whole]

toyFiberValues :: [ToyStalk]
toyFiberValues =
  [ToyStalk 0, ToyStalk 1]

swapStalk :: ToyStalk -> ToyStalk
swapStalk (ToyStalk 0) = ToyStalk 1
swapStalk (ToyStalk 1) = ToyStalk 0
swapStalk other = other

zeroStalk :: ToyStalk -> ToyStalk
zeroStalk _ =
  ToyStalk 0

directDiamondSpec :: FiniteMeetSiteSpec ToyCell
directDiamondSpec =
  FiniteMeetSiteSpec
    { fmssCells = Bottom :| [LeftCell, RightCell, Whole],
      fmssRefinements = Set.fromList toyRefinements,
      fmssCovers =
        Map.fromList [(Whole, [LeftCell :| [RightCell]])]
    }

toyRefinements :: [(ToyCell, ToyCell)]
toyRefinements =
  [ (Bottom, LeftCell),
    (Bottom, RightCell),
    (LeftCell, Whole),
    (RightCell, Whole)
  ]

toyRestrictionEdges :: [(ToyCell, ToyCell)]
toyRestrictionEdges =
  toyRefinements <> [(Bottom, Whole)]

declareDiamondSite :: ToyPresentation ()
declareDiamondSite = do
  traverse_ declareCell toyCells
  traverse_ (uncurry declareRefinement) toyRefinements

declareToyFibers ::
  ToyPresheafName ->
  [ToyCell] ->
  ToyPresentation ()
declareToyFibers presheafName =
  traverse_ (\cellValue -> declareFiber presheafName cellValue toyFiberValues)

declareDiamondRestrictions ::
  ToyPresheafName ->
  ToyPresentation ()
declareDiamondRestrictions presheafName =
  traverse_
    (\(finerCell, coarserCell) -> restricts presheafName finerCell coarserCell StalkRestrictionIdentity)
    toyRestrictionEdges

declareComponentsAt ::
  ToyMorphismName ->
  [ToyCell] ->
  (ToyStalk -> ToyStalk) ->
  ToyPresentation ()
declareComponentsAt morphismName cells componentAction =
  traverse_ (\cellValue -> componentAt morphismName cellValue componentAction) cells

declareToyPresheaf ::
  ToyPresheafName ->
  ToyPresentation ()
declareToyPresheaf presheafName = do
  declarePresheaf presheafName toyMismatches toyNormalize
  declareToyFibers presheafName toyCells
  declareDiamondRestrictions presheafName

goldenBuilder :: ToyPresentation ()
goldenBuilder = do
  declareDiamondSite
  declareCover Whole (LeftCell :| [RightCell])
  declareToyPresheaf MainPresheaf
  declareMorphism SwapMorphism MainPresheaf MainPresheaf
  declareComponentsAt SwapMorphism toyCells swapStalk
  declareMorphism ZeroMorphism MainPresheaf MainPresheaf
  declareComponentsAt ZeroMorphism toyCells zeroStalk
  declareIdentityMorphism IdentityMorphism MainPresheaf
  declareComposition LeftUnitMorphism IdentityMorphism SwapMorphism
  declareComposition RightUnitMorphism SwapMorphism IdentityMorphism
  declareComposition SwapAfterZeroMorphism SwapMorphism ZeroMorphism
  declareComposition ZeroAfterSwapMorphism ZeroMorphism SwapMorphism
  declareComposition AssocLeftMorphism SwapAfterZeroMorphism SwapMorphism
  declareComposition AssocRightMorphism SwapMorphism ZeroAfterSwapMorphism

compiledGolden ::
  Either
    ToyPresentationObstruction
    ((), ToyCompiled)
compiledGolden =
  compilePresentation goldenBuilder

withGolden :: (ToyCompiled -> Assertion) -> Assertion
withGolden continue =
  case compiledGolden of
    Left obstruction ->
      assertFailure ("expected golden presentation to compile, received " <> show obstruction)
    Right ((), compiled) ->
      continue compiled

fiberValuesAt ::
  ToyCell ->
  FinitePresheaf (FiniteMeetSite ToyCell) ToyStalk mismatch restrictionFailure ->
  [ToyStalk]
fiberValuesAt cellValue presheaf =
  maybe [] finiteFiberValues (finiteFiberAt cellValue presheaf)

morphismComponents ::
  String ->
  ToyMorphismName ->
  ToyCompiled ->
  IO (Map ToyCell [(ToyStalk, ToyStalk)])
morphismComponents label morphismName compiled =
  case presentationMorphismAt morphismName compiled of
    Nothing -> assertFailure ("expected declared morphism " <> label)
    Just morphismValue -> pure (finitePresheafMorphismComponents morphismValue)

morphismEndpointProfile ::
  String ->
  ToyMorphismName ->
  ToyCompiled ->
  IO ([(ToyCell, [ToyStalk])], [(ToyCell, [ToyStalk])])
morphismEndpointProfile label morphismName compiled =
  case presentationMorphismAt morphismName compiled of
    Nothing -> assertFailure ("expected declared morphism " <> label)
    Just morphismValue ->
      pure
        ( profileOver (finitePresheafMorphismSource morphismValue),
          profileOver (finitePresheafMorphismTarget morphismValue)
        )
  where
    profileOver ::
      FinitePresheaf (FiniteMeetSite ToyCell) ToyStalk mismatch restrictionFailure ->
      [(ToyCell, [ToyStalk])]
    profileOver presheaf =
      [(cellValue, fiberValuesAt cellValue presheaf) | cellValue <- toyCells]

expectedSwapComponents :: Map ToyCell [(ToyStalk, ToyStalk)]
expectedSwapComponents =
  Map.fromList
    [ (cellValue, [(ToyStalk 0, ToyStalk 1), (ToyStalk 1, ToyStalk 0)])
    | cellValue <- toyCells
    ]

toyGluingAlgebra :: GluingAlgebra owner (FiniteMeetSite ToyCell) ToyStalk ToyGluingFailure
toyGluingAlgebra =
  GluingAlgebra
    { gaAmalgamate = \_site compatibleFamily ->
        maybe
          (Left (GluingRejected ToyGluingRejected))
          (Right . fst)
          ( Vector.uncons
              (matchingSections (compatibleMatchingFamilyUnderlying compatibleFamily))
          )
    }

tests :: TestTree
tests =
  testGroup
    "presentation authoring surface"
    [ testCase "golden authoring compiles the site, presheaf, and morphism components" testGoldenAuthoring,
      testCase "identity and composition declarations lower to the owner operations" testCategoryLoweringRegression,
      testCase "antisymmetric refinement cycle surfaces the site build error" testAntisymmetryObstruction,
      testCase "missing fiber surfaces the presheaf build error" testMissingFiberObstruction,
      testCase "missing restriction surfaces a typed obstruction" testMissingRestrictionObstruction,
      testCase "duplicate restriction surfaces a typed obstruction" testDuplicateRestrictionObstruction,
      testCase "missing component surfaces a typed obstruction" testMissingComponentObstruction,
      testCase "duplicate component surfaces a typed obstruction" testDuplicateComponentObstruction,
      testCase "unnatural declared components are rejected" testNaturalityObstruction,
      testCase "composition of non-meeting endpoints is refused" testCompositionMiddleMismatch,
      testCase "structural declarations reject an unknown cell" testUnknownCell,
      testCase "structural declarations reject an unknown presheaf name" testUnknownPresheaf,
      testCase "structural declarations reject an unknown morphism name" testUnknownMorphism,
      testCase "structural presheaf names cannot be redeclared" testDuplicatePresheaf,
      testCase "structural morphism names cannot be redeclared" testDuplicateMorphism,
      testCase "duplicate fiber declaration is rejected" testDuplicateFiber,
      testCase "empty presentation reports no cells" testEmptyPresentation,
      testCase "compiled restrictions reject forged site morphisms" testForgedRestriction,
      testCase "compiled presentation drives the public descent to an amalgamation" testCompiledDescentGlue
    ]

testGoldenAuthoring :: Assertion
testGoldenAuthoring =
  withGolden $ \compiled -> do
    case mkFiniteMeetSite directDiamondSpec of
      Left buildError ->
        assertFailure ("expected direct site construction, received " <> show buildError)
      Right directSite ->
        assertEqual
          "presentation site equals direct meet-site construction"
          directSite
          (presentationSite compiled)
    swapComponents <- morphismComponents "swap" SwapMorphism compiled
    assertEqual
      "swap component maps invert the toy fiber at every cell"
      expectedSwapComponents
      swapComponents
    case presentationMorphismAt SwapMorphism compiled of
      Nothing -> assertFailure "expected declared swap morphism"
      Just swapMorphism ->
        assertEqual
          "swap sends 0 to 1 at LeftCell"
          (Just (ToyStalk 1))
          (finitePresheafMorphismComponentAt LeftCell (ToyStalk 0) swapMorphism)
    case presentationPresheafAt MainPresheaf compiled of
      Nothing -> assertFailure "expected declared presheaf"
      Just presheaf ->
        assertEqual
          "declared fiber at Whole retains the toy stalk values"
          (Set.fromList toyFiberValues)
          (Set.fromList (fiberValuesAt Whole presheaf))

testCategoryLoweringRegression :: Assertion
testCategoryLoweringRegression =
  withGolden $ \compiled -> do
    swapComponents <- morphismComponents "swap" SwapMorphism compiled
    leftUnitComponents <- morphismComponents "identity . swap" LeftUnitMorphism compiled
    rightUnitComponents <- morphismComponents "swap . identity" RightUnitMorphism compiled
    assertEqual "left identity" swapComponents leftUnitComponents
    assertEqual "right identity" swapComponents rightUnitComponents
    assocLeftComponents <- morphismComponents "(swap . zero) . swap" AssocLeftMorphism compiled
    assocRightComponents <- morphismComponents "swap . (zero . swap)" AssocRightMorphism compiled
    assertEqual "associativity of declared compositions" assocLeftComponents assocRightComponents
    (assocLeftSource, assocLeftTarget) <- morphismEndpointProfile "(swap . zero) . swap" AssocLeftMorphism compiled
    (assocRightSource, assocRightTarget) <- morphismEndpointProfile "swap . (zero . swap)" AssocRightMorphism compiled
    assertEqual "associativity source fibers" assocLeftSource assocRightSource
    assertEqual "associativity target fibers" assocLeftTarget assocRightTarget

antisymmetryBuilder :: ToyPresentation ()
antisymmetryBuilder = do
  declareCell LeftCell
  declareCell RightCell
  declareRefinement LeftCell RightCell
  declareRefinement RightCell LeftCell

testAntisymmetryObstruction :: Assertion
testAntisymmetryObstruction =
  case compilePresentation antisymmetryBuilder of
    Left obstruction ->
      assertEqual
        "antisymmetric refinement cycle"
        (PresentationSiteBuildFailed (FiniteMeetAntisymmetryViolation LeftCell RightCell))
        obstruction
    Right _ ->
      assertFailure "expected antisymmetry violation"

missingFiberBuilder :: ToyPresentation ()
missingFiberBuilder = do
  declareDiamondSite
  declarePresheaf MainPresheaf toyMismatches toyNormalize
  declareToyFibers MainPresheaf [Bottom, LeftCell, RightCell]
  declareDiamondRestrictions MainPresheaf

testMissingFiberObstruction :: Assertion
testMissingFiberObstruction =
  case compilePresentation missingFiberBuilder of
    Left (PresentationPresheafBuildFailed _presheafRef failure) ->
      assertEqual
        "presheaf missing a fiber at the uncovered cell"
        (FiniteFiberMissing Whole)
        failure
    Left other ->
      assertFailure ("expected presheaf build failure, received " <> show other)
    Right _ ->
      assertFailure "expected missing fiber"

missingRestrictionBuilder :: ToyPresentation ()
missingRestrictionBuilder = do
  declareDiamondSite
  declarePresheaf MainPresheaf toyMismatches toyNormalize
  declareToyFibers MainPresheaf toyCells
  traverse_
    (\(finerCell, coarserCell) -> restricts MainPresheaf finerCell coarserCell StalkRestrictionIdentity)
    toyRefinements

duplicateRestrictionBuilder :: ToyPresentation ()
duplicateRestrictionBuilder = do
  declareDiamondSite
  declareToyPresheaf MainPresheaf
  restricts MainPresheaf Bottom LeftCell StalkRestrictionIdentity

missingComponentBuilder :: ToyPresentation ()
missingComponentBuilder = do
  declareDiamondSite
  declareToyPresheaf MainPresheaf
  declareMorphism TestMorphism MainPresheaf MainPresheaf
  declareComponentsAt TestMorphism [Bottom, LeftCell, RightCell] id

duplicateComponentBuilder :: ToyPresentation ()
duplicateComponentBuilder = do
  declareDiamondSite
  declareToyPresheaf MainPresheaf
  declareMorphism TestMorphism MainPresheaf MainPresheaf
  declareComponentsAt TestMorphism toyCells id
  componentAt TestMorphism Bottom id

unnaturalComponentBuilder :: ToyPresentation ()
unnaturalComponentBuilder = do
  declareDiamondSite
  declareToyPresheaf MainPresheaf
  declareMorphism TestMorphism MainPresheaf MainPresheaf
  declareComponentsAt TestMorphism [Bottom, LeftCell, RightCell] id
  componentAt TestMorphism Whole swapStalk

testMissingRestrictionObstruction :: Assertion
testMissingRestrictionObstruction =
  case compilePresentation missingRestrictionBuilder of
    Left (PresentationRestrictionMissing MainPresheaf morphismValue) -> do
      assertEqual "missing restriction source" Bottom (cmSource morphismValue)
      assertEqual "missing restriction target" Whole (cmTarget morphismValue)
    Left other ->
      assertFailure ("expected missing restriction, received " <> show other)
    Right _ ->
      assertFailure "expected a missing restriction obstruction"

testDuplicateRestrictionObstruction :: Assertion
testDuplicateRestrictionObstruction =
  case compilePresentation duplicateRestrictionBuilder of
    Left (PresentationDuplicateRestriction MainPresheaf Bottom LeftCell) -> pure ()
    Left other ->
      assertFailure ("expected duplicate restriction, received " <> show other)
    Right _ ->
      assertFailure "expected a duplicate restriction obstruction"

testMissingComponentObstruction :: Assertion
testMissingComponentObstruction =
  case compilePresentation missingComponentBuilder of
    Left (PresentationComponentMissing TestMorphism Whole) -> pure ()
    Left other ->
      assertFailure ("expected missing component, received " <> show other)
    Right _ ->
      assertFailure "expected a missing component obstruction"

testDuplicateComponentObstruction :: Assertion
testDuplicateComponentObstruction =
  case compilePresentation duplicateComponentBuilder of
    Left (PresentationDuplicateComponent TestMorphism Bottom) -> pure ()
    Left other ->
      assertFailure ("expected duplicate component, received " <> show other)
    Right _ ->
      assertFailure "expected a duplicate component obstruction"

testNaturalityObstruction :: Assertion
testNaturalityObstruction =
  case compilePresentation unnaturalComponentBuilder of
    Left
      ( PresentationMorphismBuildFailed
          TestMorphism
          FinitePresheafMorphismNaturalityMismatch {}
        ) -> pure ()
    Left other ->
      assertFailure ("expected naturality rejection, received " <> show other)
    Right _ ->
      assertFailure "expected unnatural components to be rejected"

compositionMismatchBuilder :: ToyPresentation ()
compositionMismatchBuilder = do
  declareDiamondSite
  declareToyPresheaf SourcePresheaf
  declareToyPresheaf TargetPresheaf
  declareMorphism OuterMorphism SourcePresheaf TargetPresheaf
  declareComponentsAt OuterMorphism toyCells id
  declareMorphism InnerMorphism SourcePresheaf TargetPresheaf
  declareComponentsAt InnerMorphism toyCells id
  declareComposition CompositeMorphism OuterMorphism InnerMorphism

testCompositionMiddleMismatch :: Assertion
testCompositionMiddleMismatch =
  case compilePresentation compositionMismatchBuilder of
    Left (PresentationCompositionMiddleMismatch OuterMorphism InnerMorphism) -> pure ()
    Left other ->
      assertFailure ("expected composition middle mismatch, received " <> show other)
    Right _ ->
      assertFailure "expected composition of non-meeting endpoints to be refused"

unknownCellBuilder :: ToyPresentation ()
unknownCellBuilder = do
  declareCell Bottom
  declarePresheaf MainPresheaf toyMismatches toyNormalize
  declareFiber MainPresheaf Whole toyFiberValues

testUnknownCell :: Assertion
testUnknownCell =
  case compilePresentation unknownCellBuilder of
    Left (PresentationUnknownCell Whole) -> pure ()
    Left other ->
      assertFailure ("expected unknown cell, received " <> show other)
    Right _ ->
      assertFailure "expected an unknown cell obstruction"

unknownPresheafBuilder :: ToyPresentation ()
unknownPresheafBuilder = do
  declareCell Bottom
  declareFiber UnknownPresheaf Bottom toyFiberValues

testUnknownPresheaf :: Assertion
testUnknownPresheaf =
  case compilePresentation unknownPresheafBuilder of
    Left (PresentationUnknownPresheaf UnknownPresheaf) -> pure ()
    Left other ->
      assertFailure ("expected unknown presheaf, received " <> show other)
    Right _ ->
      assertFailure "expected an unknown presheaf obstruction"

unknownMorphismBuilder :: ToyPresentation ()
unknownMorphismBuilder = do
  declareCell Bottom
  componentAt UnknownMorphism Bottom id

testUnknownMorphism :: Assertion
testUnknownMorphism =
  case compilePresentation unknownMorphismBuilder of
    Left (PresentationUnknownMorphism UnknownMorphism) -> pure ()
    Left other ->
      assertFailure ("expected unknown morphism, received " <> show other)
    Right _ ->
      assertFailure "expected an unknown morphism obstruction"

duplicatePresheafBuilder :: ToyPresentation ()
duplicatePresheafBuilder = do
  declareCell Bottom
  declarePresheaf MainPresheaf toyMismatches toyNormalize
  declarePresheaf MainPresheaf toyMismatches toyNormalize

testDuplicatePresheaf :: Assertion
testDuplicatePresheaf =
  case compilePresentation duplicatePresheafBuilder of
    Left (PresentationDuplicatePresheaf MainPresheaf) -> pure ()
    Left other ->
      assertFailure ("expected duplicate presheaf name, received " <> show other)
    Right _ ->
      assertFailure "expected a duplicate presheaf-name obstruction"

duplicateMorphismBuilder :: ToyPresentation ()
duplicateMorphismBuilder = do
  declareCell Bottom
  declarePresheaf MainPresheaf toyMismatches toyNormalize
  declareFiber MainPresheaf Bottom toyFiberValues
  declareIdentityMorphism TestMorphism MainPresheaf
  declareIdentityMorphism TestMorphism MainPresheaf

testDuplicateMorphism :: Assertion
testDuplicateMorphism =
  case compilePresentation duplicateMorphismBuilder of
    Left (PresentationDuplicateMorphism TestMorphism) -> pure ()
    Left other ->
      assertFailure ("expected duplicate morphism name, received " <> show other)
    Right _ ->
      assertFailure "expected a duplicate morphism-name obstruction"

duplicateFiberBuilder :: ToyPresentation ()
duplicateFiberBuilder = do
  declareDiamondSite
  declarePresheaf MainPresheaf toyMismatches toyNormalize
  declareToyFibers MainPresheaf toyCells
  declareFiber MainPresheaf LeftCell toyFiberValues

testDuplicateFiber :: Assertion
testDuplicateFiber =
  case compilePresentation duplicateFiberBuilder of
    Left (PresentationDuplicateFiber MainPresheaf cellValue) ->
      assertEqual "duplicate fiber cell" LeftCell cellValue
    Left other ->
      assertFailure ("expected duplicate fiber, received " <> show other)
    Right _ ->
      assertFailure "expected duplicate fiber rejection"

emptyBuilder :: ToyPresentation ()
emptyBuilder =
  pure ()

testEmptyPresentation :: Assertion
testEmptyPresentation =
  case compilePresentation emptyBuilder of
    Left obstruction ->
      assertEqual "empty presentation has no cells" PresentationNoCells obstruction
    Right _ ->
      assertFailure "expected an empty presentation to report no cells"

testForgedRestriction :: Assertion
testForgedRestriction =
  withGolden $ \compiled ->
    case presentationPresheafAt MainPresheaf compiled of
      Nothing ->
        assertFailure "expected the named presheaf"
      Just presheaf ->
        case finiteMeetMorphism (presentationSite compiled) Bottom LeftCell of
          Nothing ->
            assertFailure "expected the valid bottom-to-left restriction"
          Just validMorphism ->
            let forgedMorphism =
                  validMorphism
                    { cmSource = RightCell,
                      cmTarget = LeftCell
                    }
             in assertEqual
                  "restriction callbacks reject morphisms outside the compiled site"
                  (Left (PresentedRestrictionUnavailable RightCell LeftCell))
                  (restrictPresentedPresheaf forgedMorphism (ToyStalk 0) presheaf)

testCompiledDescentGlue :: Assertion
testCompiledDescentGlue =
  withGolden $ \compiled ->
    case
      compile
        (siteSpec (presentationSite compiled))
        ( \preparedSite ->
            case preparedCovers preparedSite Whole of
              Left refusal ->
                assertFailure ("expected a known cover target, received " <> show refusal)
              Right [coverPlan] ->
                let sections = Vector.fromList [ToyStalk 7, ToyStalk 7]
                 in case first CoverMatchingFamilyConstructionFailed (matching coverPlan sections)
                      >>= glue toyAlgebra toyGluingAlgebra of
                      Left failure ->
                        assertFailure ("expected a successful amalgamation, received " <> show failure)
                      Right amalgamation ->
                        assertEqual
                          "amalgamated stalk over the compatible cover"
                          (ToyStalk 7)
                          (amalgamatedStalk amalgamation)
              Right coverPlans ->
                assertFailure
                  ("expected exactly one cover on Whole, received " <> show (length coverPlans))
        )
      of
      Left failure ->
        assertFailure ("expected the presentation site to prepare, received " <> show failure)
      Right preparedAssertion ->
        preparedAssertion

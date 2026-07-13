{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Moonlight.Sheaf.Cochain.StressSpec
  ( tests,
    stressOptions,
    StressRoundCount (..),
  )
where

import Data.Bifunctor (first)
import Data.Either (fromRight)
import Data.Kind (Type)
import Data.List (mapAccumL)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Void (Void)
import Moonlight.Sheaf.Cochain.Coboundary
  ( CoboundaryContribution (..),
    CoboundaryMatrix,
    CoboundaryReducer (..),
    applyCoboundary,
    buildCoboundary,
    buildCoboundaryComplex,
    checkCoboundaryNilpotence,
    collapseCoboundary,
  )
import Moonlight.Sheaf.Cochain.Laplacian
  ( LaplacianKind (HodgeLaplacian),
    SheafLaplacian,
    buildHodgeLaplacian0,
    laplacianApplySparse,
    laplacianIsSquare,
    laplacianIsSymmetric,
    laplacianResidualSquaredNorm,
    slDomainBasis,
  )
import Moonlight.Sheaf.Operator.LinearBasis
  ( LinearBasis,
    linearBasisIndexedCoordinates,
    linearCoordinateCell,
  )
import Moonlight.Sheaf.Presheaf.Core
  ( CompiledRestriction (..),
    Presheaf (..),
  )
import Moonlight.Sheaf.Kernel.Basis
  ( SheafBasis,
    mkSheafBasis,
  )
import Moonlight.Sheaf.Section.Model
  ( SheafModel,
    SheafModelVersion (..),
    emptySheafModel,
    modelCells,
    prepareSheafModel,
  )
import Moonlight.Sheaf.Section.Morphism
  ( RestrictionParts (..),
    unitIncidenceRestriction,
  )
import Moonlight.Sheaf.Kernel.Basis (basisCells)
import Moonlight.Sheaf.Section.ObjectIndex
  ( mkObjectIndex,
  )
import Moonlight.Sheaf.Section.Stalk
  ( StalkAlgebra (..),
    StalkRestrictionKernel (..),
  )
import Moonlight.Sheaf.Section.Store.State
import Moonlight.Sheaf.Section.Store.Types
import Moonlight.Sheaf.Sheaf.Gluing
  ( GluingAlgebra (..),
    GluingObstruction (..),
    MatchingFamily,
    MatchingFamilyConstructionError,
    amalgamateMatchingFamilyWith,
    amalgamatedStalk,
    compatibleMatchingFamilyUnderlying,
    matchingFamilySections,
    matchingFamilyTarget,
    mkMatchingFamily,
  )
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism (..),
    CoveringFamily,
    Site (..),
    coverArrows,
  )
import Moonlight.Sheaf.Site.Plan
  ( EffectiveCoverPlanFailure,
    prepareEffectiveCoverPlan,
  )
import Moonlight.Sheaf.Site.Context
  ( ContextArrow,
  )
import Moonlight.Sheaf.TestFixture.Site
  ( SampleContext (..),
    SampleSystem,
    sampleSystem,
  )
import Moonlight.Sheaf.TestFixture.Assertions (assertRight)
import Moonlight.Sheaf.TestFixture.Triangle
  ( TriangleCell (..),
    triangleCoboundarySpec0,
    triangleCoboundarySpec1,
    triangleEdgeBasis,
    triangleRestrictionIndex,
    triangleUnitCoboundaryBlock,
    triangleUnitStalkAlgebra,
    triangleUnitStalkDimension,
    triangleVertexBasis,
  )
import Data.Proxy (Proxy (..))
import Test.Tasty
  ( TestTree,
    askOption,
    testGroup,
  )
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )
import Test.Tasty.Options
  ( IsOption (..),
    OptionDescription (..),
    safeRead,
  )

tests :: TestTree
tests =
  askOption $ \(StressRoundCount roundCount) ->
    testGroup
      "sheaf-stress"
      [ testCase
          ( "repeated restrict/glue/coboundary/laplacian/propagation trajectory preserves invariants ("
              <> show roundCount
              <> " rounds)"
          )
          (testRepeatedSheafTrajectory roundCount)
      ]

stressOptions :: [OptionDescription]
stressOptions =
  [Option (Proxy :: Proxy StressRoundCount)]

type StressRoundCount :: Type
newtype StressRoundCount = StressRoundCount Int
  deriving stock (Eq, Ord, Show)

instance IsOption StressRoundCount where
  defaultValue = StressRoundCount 96

  parseValue input =
    case safeRead input of
      Just roundCount | roundCount > 0 -> Just (StressRoundCount roundCount)
      _ -> Nothing

  optionName = pure "stress-rounds"

  optionHelp =
    pure "Number of trajectory rounds the sheaf-stress suite hammers (positive Int, default 96)."

  showDefaultValue (StressRoundCount roundCount) =
    Just (show roundCount)

type ScalarStalk :: Type
newtype ScalarStalk = ScalarStalk
  { unScalarStalk :: Int
  }
  deriving stock (Eq, Ord, Show)

type StressStalk :: Type
data StressStalk = StressStalk
  { stressLeft :: !Int,
    stressRight :: !Int,
    stressMeet :: !Int,
    stressNoise :: !Int
  }
  deriving stock (Eq, Ord, Show)

type StressMismatch :: Type
data StressMismatch = StressMismatch !String !Int !Int
  deriving stock (Eq, Ord, Show)

type TriangleRestrictionWitness :: Type
type TriangleRestrictionWitness = (TriangleCell, TriangleCell, Int)

stressStalkAlgebra :: StalkAlgebra witness StressStalk StressMismatch ()
stressStalkAlgebra =
  StalkAlgebra
    { saRestrictionKernel = const StalkRestrictionIdentity,
      saMismatches =
        \leftValue rightValue ->
          concat
            [ fieldMismatch "left" stressLeft leftValue rightValue,
              fieldMismatch "right" stressRight leftValue rightValue,
              fieldMismatch "meet" stressMeet leftValue rightValue,
              fieldMismatch "noise" stressNoise leftValue rightValue
            ],
      saMerge =
        \leftValue rightValue ->
          Right
            StressStalk
              { stressLeft = max (stressLeft leftValue) (stressLeft rightValue),
                stressRight = max (stressRight leftValue) (stressRight rightValue),
                stressMeet = max (stressMeet leftValue) (stressMeet rightValue),
                stressNoise = max (stressNoise leftValue) (stressNoise rightValue)
              },
      saRepair = const (Left ()),
      saNormalize = id
    }

stressCompiledStalkAlgebra :: StalkAlgebra (CompiledRestriction SampleSystem) StressStalk StressMismatch ()
stressCompiledStalkAlgebra =
  StalkAlgebra
    { saRestrictionKernel = \restriction -> StalkRestrictionMap (restrictAlong (crSite restriction) (crMorphism restriction)),
      saMismatches = saMismatches stressStalkAlgebra,
      saMerge = saMerge stressStalkAlgebra,
      saRepair = const (Left ()),
      saNormalize = saNormalize stressStalkAlgebra
    }

fieldMismatch :: String -> (StressStalk -> Int) -> StressStalk -> StressStalk -> [StressMismatch]
fieldMismatch fieldName project leftValue rightValue =
  let leftField = project leftValue
      rightField = project rightValue
   in ([StressMismatch fieldName leftField rightField | leftField /= rightField])

normalizeAt :: SampleContext -> StressStalk -> StressStalk
normalizeAt contextValue stalkValue =
  case contextValue of
    RootCtx ->
      stalkValue
    LeftCtx ->
      StressStalk
        { stressLeft = stressLeft stalkValue,
          stressRight = 0,
          stressMeet = stressMeet stalkValue,
          stressNoise = 0
        }
    RightCtx ->
      StressStalk
        { stressLeft = 0,
          stressRight = stressRight stalkValue,
          stressMeet = stressMeet stalkValue,
          stressNoise = 0
        }
    MeetCtx ->
      StressStalk
        { stressLeft = 0,
          stressRight = 0,
          stressMeet = stressMeet stalkValue,
          stressNoise = 0
        }

instance Presheaf SampleSystem StressStalk where
  restrictAlong _site morphism stalkValue
    | cmSource morphism == cmTarget morphism = stalkValue
    | otherwise = normalizeAt (cmSource morphism) stalkValue

stressGluingAlgebra :: GluingAlgebra SampleSystem StressStalk Void
stressGluingAlgebra =
  GluingAlgebra
    { gaAmalgamate = \_site compatibleFamily ->
        let matchingFamily = compatibleMatchingFamilyUnderlying compatibleFamily
            localSections = matchingFamilySections matchingFamily
         in maybe
              (Left (GluingUnavailable (matchingFamilyTarget matchingFamily)))
              Right
              ( case matchingFamilyTarget matchingFamily of
                  RootCtx ->
                    glueRootCover localSections
                  LeftCtx ->
                    glueSingleContext LeftCtx localSections
                  RightCtx ->
                    glueSingleContext RightCtx localSections
                  MeetCtx ->
                    localSections Vector.!? 0
              )
    }

mkMatchingFamilyForCover ::
  Site site =>
  site ->
  CoveringFamily (SiteObject site) (SiteMorphism site) ->
  Vector stalk ->
  Either
    (Either (EffectiveCoverPlanFailure (SiteObject site) (SiteMorphism site)) MatchingFamilyConstructionError)
    (MatchingFamily site stalk)
mkMatchingFamilyForCover site coverValue sections = do
  effectiveCover <-
    case prepareEffectiveCoverPlan site coverValue of
      Left failure ->
        Left (Left failure)
      Right planValue ->
        Right planValue
  case mkMatchingFamily effectiveCover sections of
    Left failure ->
      Left (Right failure)
    Right matchingFamily ->
      Right matchingFamily

glueRootCover :: Vector StressStalk -> Maybe StressStalk
glueRootCover localSections =
  case Vector.toList localSections of
    [leftSection, rightSection]
      | normalizeAt MeetCtx leftSection == normalizeAt MeetCtx rightSection ->
          Just
            StressStalk
              { stressLeft = stressLeft leftSection,
                stressRight = stressRight rightSection,
                stressMeet = stressMeet leftSection,
                stressNoise = 0
              }
    _ ->
      Nothing

glueSingleContext :: SampleContext -> Vector StressStalk -> Maybe StressStalk
glueSingleContext targetContext localSections =
  normalizeAt targetContext <$> localSections Vector.!? 0

type ContextRestrictionWitness :: Type
newtype ContextRestrictionWitness = ContextRestrictionWitness
  { crwTarget :: SampleContext
  }
  deriving stock (Eq, Show)

type StressArtifacts :: Type
data StressArtifacts = StressArtifacts
  { saCoboundary0 :: !(CoboundaryMatrix TriangleCell TriangleRestrictionWitness),
    saCoboundary1 :: !(CoboundaryMatrix TriangleCell TriangleRestrictionWitness),
    saLaplacian0 :: !(SheafLaplacian 'HodgeLaplacian TriangleCell),
    saVertexModel :: !(SheafModel TriangleCell ()),
    saEdgeModel :: !(SheafModel TriangleCell ()),
    saContextModel :: !(SheafModel SampleContext ContextRestrictionWitness)
  }

type TrajectoryState :: Type
data TrajectoryState = TrajectoryState
  { tsSeed :: !Int,
    tsRoot :: !StressStalk,
    tsVertexSection :: !(TotalSectionStore TriangleCell ScalarStalk),
    tsContextSection :: !(TotalSectionStore SampleContext StressStalk)
  }
  deriving stock (Eq, Show)

testRepeatedSheafTrajectory :: Int -> Assertion
testRepeatedSheafTrajectory roundCount = do
  artifacts <- buildStressArtifacts
  initialState <- buildInitialTrajectoryState
  _finalState <- foldMStrict (stressRound artifacts) initialState [0 .. roundCount - 1]
  pure ()

buildStressArtifacts :: IO StressArtifacts
buildStressArtifacts = do
  restrictions <-
    assertRight
      "triangle restriction index"
      triangleRestrictionIndex
  coboundary0 <-
    assertRight
      "degree-0 triangle coboundary"
      (buildCoboundary triangleCoboundarySpec0 restrictions)
  coboundary1 <-
    assertRight
      "degree-1 triangle coboundary"
      (buildCoboundary triangleCoboundarySpec1 restrictions)
  complex <-
    assertRight
      "triangle coboundary complex"
      ( buildCoboundaryComplex
          scalarStalkAt
          triangleUnitStalkDimension
          triangleUnitCoboundaryBlock
          triangleCoboundarySpec0
          triangleCoboundarySpec1
          restrictions
      )
  assertBool
    "expected triangle coboundary complex to satisfy d^2 = 0 before stress rounds"
    (checkCoboundaryNilpotence complex)
  laplacian0 <-
    assertRight
      "degree-0 hodge laplacian"
      (buildHodgeLaplacian0 complex)
  assertBool "expected degree-0 Hodge Laplacian to be square" (laplacianIsSquare laplacian0)
  assertBool "expected degree-0 Hodge Laplacian to be symmetric" (laplacianIsSymmetric laplacian0)
  vertexModel <-
    assertRight
      "triangle vertex sheaf model"
      triangleVertexSheafModel
  edgeModel <-
    assertRight
      "triangle edge sheaf model"
      triangleEdgeSheafModel
  contextModel <-
    assertRight
      "context sheaf model"
      contextSheafModel
  pure
    StressArtifacts
      { saCoboundary0 = coboundary0,
        saCoboundary1 = coboundary1,
        saLaplacian0 = laplacian0,
        saVertexModel = vertexModel,
        saEdgeModel = edgeModel,
        saContextModel = contextModel
      }

buildInitialTrajectoryState :: IO TrajectoryState
buildInitialTrajectoryState = do
  vertexModel <-
    assertRight
      "triangle vertex sheaf model"
      triangleVertexSheafModel
  contextModel <-
    assertRight
      "context sheaf model"
      contextSheafModel
  vertexSection <-
    assertRight
      "initial vertex section"
      ( mkTotalSectionStore
          vertexModel
          ( Map.fromList
              [ (V0, ScalarStalk 3),
                (V1, ScalarStalk (-2)),
                (V2, ScalarStalk 5)
              ]
          )
      )
  contextSection <-
    assertRight
      "initial context section"
      (contextSectionFromRoot contextModel initialRootStalk)
  pure
    TrajectoryState
      { tsSeed = 1729,
        tsRoot = initialRootStalk,
        tsVertexSection = vertexSection,
        tsContextSection = contextSection
      }

stressRound :: StressArtifacts -> TrajectoryState -> Int -> IO TrajectoryState
stressRound artifacts stateValue roundIndex = do
  let (seedAfterGlue, leftDelta) = drawSigned (tsSeed stateValue) 7
      (seedAfterRight, rightDelta) = drawSigned seedAfterGlue 7
      (seedAfterMeet, meetDelta) = drawSigned seedAfterRight 3
      localMeet = stressMeet (tsRoot stateValue) + meetDelta
      leftLocal =
        normalizeAt
          LeftCtx
          (tsRoot stateValue)
            { stressLeft = stressLeft (tsRoot stateValue) + leftDelta,
              stressMeet = localMeet
            }
      rightLocal =
        normalizeAt
          RightCtx
          (tsRoot stateValue)
            { stressRight = stressRight (tsRoot stateValue) + rightDelta,
              stressMeet = localMeet
            }
  gluedRoot <- assertCompatibleRootGlue roundIndex leftLocal rightLocal
  let (seedAfterVertices, perturbedVertexSectionEither) =
        perturbVertexSection (saVertexModel artifacts) seedAfterMeet (tsVertexSection stateValue)
  perturbedVertexSection <-
    assertRight
      ("round " <> show roundIndex <> " perturbed vertex section")
      perturbedVertexSectionEither
  residualEnergy <-
    assertRight
      ("round " <> show roundIndex <> " laplacian residual norm")
      (laplacianResidualSquaredNorm scalarCoordinates (saLaplacian0 artifacts) (saVertexModel artifacts) perturbedVertexSection)
  assertBool
    ("round " <> show roundIndex <> " laplacian residual norm should stay finite and non-negative")
    (finiteDouble residualEnergy && residualEnergy >= 0.0)
  assertCoboundaryNilpotenceOnSection
    roundIndex
    (saCoboundary0 artifacts)
    (saCoboundary1 artifacts)
    (saVertexModel artifacts)
    (saEdgeModel artifacts)
    perturbedVertexSection
  steppedVertexSection <-
    assertRight
      ("round " <> show roundIndex <> " laplacian step")
      (laplacianStepScalar (saVertexModel artifacts) (saLaplacian0 artifacts) perturbedVertexSection)
  contextSectionWithRoot <-
    assertRight
      ("round " <> show roundIndex <> " install root context")
      (updateStalkAtChecked (saContextModel artifacts) RootCtx (const gluedRoot) (tsContextSection stateValue))
  propagatedSection <-
    assertPropagationIdempotent
      roundIndex
      (saContextModel artifacts)
      contextSectionWithRoot
  pure
    TrajectoryState
      { tsSeed = seedAfterVertices,
        tsRoot = gluedRoot,
        tsVertexSection = steppedVertexSection,
        tsContextSection = propagatedSection
      }

assertCompatibleRootGlue :: Int -> StressStalk -> StressStalk -> IO StressStalk
assertCompatibleRootGlue roundIndex leftLocal rightLocal = do
  coverValue <- rootCover
  matchingFamily <-
    assertRight
      ("round " <> show roundIndex <> " matching family")
      ( mkMatchingFamilyForCover
          sampleSystem
          coverValue
          (Vector.fromList [leftLocal, rightLocal])
      )
  amalgamation <-
    assertRight
      ("round " <> show roundIndex <> " compatible root gluing")
      (amalgamateMatchingFamilyWith stressCompiledStalkAlgebra (gaAmalgamate stressGluingAlgebra) sampleSystem matchingFamily)
  let gluedRoot = amalgamatedStalk amalgamation
      arrowsBySource =
        Map.fromList
          [ (cmSource arrow, arrow)
            | arrow <- NE.toList (coverArrows coverValue)
          ]
  leftArrow <-
    maybe
      (assertFailure "root cover did not contain LeftCtx")
      pure
      (Map.lookup LeftCtx arrowsBySource)
  rightArrow <-
    maybe
      (assertFailure "root cover did not contain RightCtx")
      pure
      (Map.lookup RightCtx arrowsBySource)
  assertEqual
    ("round " <> show roundIndex <> " glued root restricts back to left local section")
    leftLocal
    (restrictAlong sampleSystem leftArrow gluedRoot)
  assertEqual
    ("round " <> show roundIndex <> " glued root restricts back to right local section")
    rightLocal
    (restrictAlong sampleSystem rightArrow gluedRoot)
  pure gluedRoot

assertCoboundaryNilpotenceOnSection ::
  Int ->
  CoboundaryMatrix TriangleCell TriangleRestrictionWitness ->
  CoboundaryMatrix TriangleCell TriangleRestrictionWitness ->
  SheafModel TriangleCell () ->
  SheafModel TriangleCell () ->
  TotalSectionStore TriangleCell ScalarStalk ->
  Assertion
assertCoboundaryNilpotenceOnSection roundIndex coboundary0 coboundary1 vertexModel edgeModel vertexSection = do
  edgeContributions <-
    assertRight
      ("round " <> show roundIndex <> " apply degree-0 coboundary")
      (applyCoboundary triangleUnitStalkAlgebra coboundary0 vertexModel vertexSection)
  edgeSection <-
    assertRight
      ("round " <> show roundIndex <> " edge cochain section")
      (mkTotalSectionStore edgeModel (collapseCoboundary scalarReducer edgeContributions))
  triangleContributions <-
    assertRight
      ("round " <> show roundIndex <> " apply degree-1 coboundary")
      (applyCoboundary triangleUnitStalkAlgebra coboundary1 edgeModel edgeSection)
  assertEqual
    ("round " <> show roundIndex <> " direct cochain trajectory should satisfy d^2 = 0")
    (Map.fromList [(T012, ScalarStalk 0)])
    (collapseCoboundary scalarReducer triangleContributions)

laplacianStepScalar ::
  SheafModel TriangleCell () ->
  SheafLaplacian 'HodgeLaplacian TriangleCell ->
  TotalSectionStore TriangleCell ScalarStalk ->
  Either (SectionConstructionError TriangleCell) (TotalSectionStore TriangleCell ScalarStalk)
laplacianStepScalar model laplacian sectionValue = do
  inputVector <- scalarSectionVector model (slDomainBasis laplacian) sectionValue
  let residual = laplacianApplySparse laplacian inputVector
      steppedEntries =
        Map.fromList
          [ ( linearCoordinateCell coordinate,
              ScalarStalk
                ( round
                    ( Map.findWithDefault 0.0 indexValue inputVector
                        - 0.125 * Map.findWithDefault 0.0 indexValue residual
                    )
                )
            )
            | (indexValue, coordinate) <- linearBasisIndexedCoordinates (slDomainBasis laplacian)
          ]
  let residualValues = Map.elems residual
  if all finiteDouble residualValues
    then mkTotalSectionStore model steppedEntries
    else
      Left
        SectionConstructionError
          { sceMissingCells = Set.empty,
            sceExtraCells = Set.fromList (modelCells model)
          }

scalarSectionVector ::
  Ord cell =>
  SheafModel cell witness ->
  LinearBasis cell ->
  TotalSectionStore cell ScalarStalk ->
  Either (SectionConstructionError cell) (Map Int Double)
scalarSectionVector model basis sectionValue =
  fmap Map.fromList
    ( traverse
        coordinateValue
        (linearBasisIndexedCoordinates basis)
    )
  where
    coordinateValue (indexValue, coordinate) =
      case totalStalkAt model (linearCoordinateCell coordinate) sectionValue of
        Right (ScalarStalk value) -> Right (indexValue, fromIntegral value)
        Left _ ->
          Left
            SectionConstructionError
              { sceMissingCells = Set.singleton (linearCoordinateCell coordinate),
                sceExtraCells = Set.empty
              }

assertPropagationIdempotent ::
  Int ->
  SheafModel SampleContext ContextRestrictionWitness ->
  TotalSectionStore SampleContext StressStalk ->
  IO (TotalSectionStore SampleContext StressStalk)
assertPropagationIdempotent roundIndex model sectionValue = do
  (firstSection, firstReport) <-
    assertRight
      ("round " <> show roundIndex <> " propagation")
      (runRootPropagation model sectionValue)
  assertBool
    ("round " <> show roundIndex <> " first propagation should converge")
    (rrrSettled firstReport)
  assertBool
    ("round " <> show roundIndex <> " changed contexts should remain in the context basis")
    (Set.isSubsetOf (rrrChangedContexts firstReport) (Set.fromList contextCells))
  (secondSection, secondReport) <-
    assertRight
      ("round " <> show roundIndex <> " idempotent propagation")
      (runRootPropagation model firstSection)
  assertBool
    ("round " <> show roundIndex <> " second propagation should converge")
    (rrrSettled secondReport)
  assertEqual
    ("round " <> show roundIndex <> " propagation should be idempotent after convergence")
    firstSection
    secondSection
  pure firstSection

runRootPropagation ::
  SheafModel SampleContext ContextRestrictionWitness ->
  TotalSectionStore SampleContext StressStalk ->
  Either
    String
    ( TotalSectionStore SampleContext StressStalk,
      RootResolutionReport
    )
runRootPropagation model sectionValue = do
  deltaValue <- rootResolutionDelta model (Set.singleton RootCtx) sectionValue
  (resolvedSection, changedContexts) <-
    rootResolutionApply model deltaValue (sectionValue, Set.empty)
  Right
    ( resolvedSection,
      RootResolutionReport
        { rrrSettled = True,
          rrrChangedContexts = changedContexts
        }
    )

type RootResolutionReport :: Type
data RootResolutionReport = RootResolutionReport
  { rrrSettled :: !Bool,
    rrrChangedContexts :: !(Set.Set SampleContext)
  }
  deriving stock (Eq, Show)

rootResolutionDelta ::
  SheafModel SampleContext ContextRestrictionWitness ->
  Set.Set SampleContext ->
  TotalSectionStore SampleContext StressStalk ->
  Either String (Maybe (Map SampleContext StressStalk))
rootResolutionDelta model frontier sectionValue
  | Set.disjoint frontier (Set.fromList [RootCtx, LeftCtx, RightCtx]) =
      Right Nothing
  | otherwise =
      case totalStalkAt model RootCtx sectionValue of
        Left lookupError ->
          Left (show lookupError)
        Right rootStalk ->
          Right
            ( Just
                ( Map.fromList
                    [ (contextValue, normalizeAt contextValue rootStalk)
                      | contextValue <- contextCells
                    ]
                )
            )

rootResolutionApply ::
  SheafModel SampleContext ContextRestrictionWitness ->
  Maybe (Map SampleContext StressStalk) ->
  (TotalSectionStore SampleContext StressStalk, Set.Set SampleContext) ->
  Either String (TotalSectionStore SampleContext StressStalk, Set.Set SampleContext)
rootResolutionApply _ Nothing stateValue =
  Right stateValue
rootResolutionApply model (Just desiredEntries) (sectionValue, _) =
  let currentEntries =
        fromRight Map.empty (totalSectionEntries model sectionValue)
      changedContexts =
        Set.fromList
          [ contextValue
            | contextValue <- contextCells,
              Map.lookup contextValue currentEntries /= Map.lookup contextValue desiredEntries
          ]
   in case mkTotalSectionStore model desiredEntries of
        Left constructionError ->
          Left (show constructionError)
        Right nextSection ->
          Right (nextSection, changedContexts)

scalarStalkAt :: TriangleCell -> ScalarStalk
scalarStalkAt _ =
  ScalarStalk 0

scalarReducer :: CoboundaryReducer ScalarStalk
scalarReducer =
  CoboundaryReducer
    { runCoboundaryReducer =
        \contributions ->
          ScalarStalk
            ( sum
                [ contributionOrientation contribution * unScalarStalk (contributionValue contribution)
                  | contribution <- contributions
                ]
            )
    }

contextSheafModel ::
  Either
    String
    (SheafModel SampleContext ContextRestrictionWitness)
contextSheafModel =
  case
    prepareSheafModel
      (SheafModelVersion 0)
      (mkObjectIndex (basisCells contextBasis))
      ( \(sourceContext, targetContext) ->
          RestrictionParts
            { partKind = unitIncidenceRestriction,
              partSource = sourceContext,
              partTarget = targetContext,
              partWitness = ContextRestrictionWitness targetContext
            }
      )
      contextRestrictionEdges
    of
      Left modelError -> Left (show modelError)
      Right model -> Right model

contextRestrictionEdges :: [(SampleContext, SampleContext)]
contextRestrictionEdges =
  [ (RootCtx, LeftCtx),
    (RootCtx, RightCtx),
    (RootCtx, MeetCtx),
    (LeftCtx, MeetCtx),
    (RightCtx, MeetCtx)
  ]

contextBasis :: SheafBasis SampleContext
contextBasis =
  mkSheafBasis contextCells

triangleVertexSheafModel :: Either String (SheafModel TriangleCell ())
triangleVertexSheafModel =
  first show (emptySheafModel (SheafModelVersion 0) (mkObjectIndex (basisCells triangleVertexBasis)))

triangleEdgeSheafModel :: Either String (SheafModel TriangleCell ())
triangleEdgeSheafModel =
  first show (emptySheafModel (SheafModelVersion 0) (mkObjectIndex (basisCells triangleEdgeBasis)))

contextCells :: [SampleContext]
contextCells =
  [RootCtx, LeftCtx, RightCtx, MeetCtx]

contextSectionFromRoot :: SheafModel SampleContext ContextRestrictionWitness -> StressStalk -> Either (SectionConstructionError SampleContext) (TotalSectionStore SampleContext StressStalk)
contextSectionFromRoot model rootStalk =
  mkTotalSectionStore
    model
    ( Map.fromList
        [ (contextValue, normalizeAt contextValue rootStalk)
          | contextValue <- contextCells
        ]
    )

rootCover :: IO (CoveringFamily SampleContext (ContextArrow SampleContext))
rootCover =
  case coversAt sampleSystem RootCtx of
    [] ->
      assertFailure "expected sampleSystem to expose a non-trivial RootCtx cover"
    coverValue : _ ->
      pure coverValue

initialRootStalk :: StressStalk
initialRootStalk =
  StressStalk
    { stressLeft = 11,
      stressRight = 19,
      stressMeet = 5,
      stressNoise = 0
    }

perturbVertexSection ::
  SheafModel TriangleCell () ->
  Int ->
  TotalSectionStore TriangleCell ScalarStalk ->
  (Int, Either (SectionConstructionError TriangleCell) (TotalSectionStore TriangleCell ScalarStalk))
perturbVertexSection model seed sectionValue =
  let (nextSeed, entries) =
        mapAccumL perturbOne seed (modelCells model)
   in ( nextSeed,
        mkTotalSectionStore model (Map.fromList entries)
      )
  where
    existingEntries = fromRight Map.empty (totalSectionEntries model sectionValue)

    perturbOne currentSeed cellValue =
      let (nextSeed, deltaValue) = drawSigned currentSeed 5
          oldValue =
            maybe 0 unScalarStalk (Map.lookup cellValue existingEntries)
       in (nextSeed, (cellValue, ScalarStalk (oldValue + deltaValue)))

drawSigned :: Int -> Int -> (Int, Int)
drawSigned seed radius =
  let nextSeed = (1103515245 * seed + 12345) `mod` 2147483647
      width = 2 * max 0 radius + 1
      value = (nextSeed `mod` width) - max 0 radius
   in (nextSeed, value)

scalarCoordinates :: ScalarStalk -> [Double]
scalarCoordinates (ScalarStalk value) =
  [fromIntegral value]

finiteDouble :: Double -> Bool
finiteDouble value =
  not (isNaN value) && not (isInfinite value)

foldMStrict :: Monad m => (state -> input -> m state) -> state -> [input] -> m state
foldMStrict step = go
  where
    go !stateValue remainingInputs =
      case remainingInputs of
        [] -> pure stateValue
        inputValue : rest -> do
          nextState <- step stateValue inputValue
          go nextState rest

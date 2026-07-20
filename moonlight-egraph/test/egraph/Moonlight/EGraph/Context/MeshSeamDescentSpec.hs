{-# LANGUAGE LambdaCase #-}

module Moonlight.EGraph.Context.MeshSeamDescentSpec
  ( tests,
  )
where

import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Monoid (Sum (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Sheaf.Descent.Assignment qualified as AssignmentDescent
import Moonlight.Sheaf.Descent.Kernel
  ( unboundedCoverSearchBudget,
  )
import Moonlight.Sheaf.Verdict
  ( SearchVerdict (..),
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertFailure,
    testCase,
    (@?=),
  )

data MeshVertex
  = VertexA
  | VertexB
  | VertexC
  | VertexD
  deriving stock (Eq, Ord, Show)

data MeshEdge = MeshEdge MeshVertex MeshVertex
  deriving stock (Eq, Ord, Show)

data MeshFace
  = FaceABC
  | FaceACD
  deriving stock (Eq, Ord, Show)

data MeshPatch
  = PatchWhole
  | PatchLeft
  | PatchRight
  deriving stock (Eq, Ord, Show)

data MeshCell
  = Mesh0 MeshVertex
  | Mesh1 MeshEdge
  | Mesh2 MeshFace
  deriving stock (Eq, Ord, Show)

data MeshCellComplex = MeshCellComplex
  { mccCells :: Set MeshCell,
    mccPatchFaces :: Map MeshPatch (Set MeshFace)
  }
  deriving stock (Eq, Show)

newtype SeamChain = SeamChain
  { unSeamChain :: NonEmpty MeshVertex
  }
  deriving stock (Eq, Ord, Show)

data SeamChainError
  = SeamChainIllegalStep MeshVertex MeshVertex
  deriving stock (Eq, Show)

data LocalSeamSection = LocalSeamSection
  { lssPatch :: MeshPatch,
    lssChains :: Set SeamChain
  }
  deriving stock (Eq, Show)

newtype GlobalSeamLayout = GlobalSeamLayout
  { gslSeamEdges :: Set MeshEdge
  }
  deriving stock (Eq, Show)

data SeamDescentObstruction
  = MissingPatchSection MeshPatch
  | PatchSectionKeyMismatch MeshPatch MeshPatch
  | OverlapSeamMismatch MeshPatch MeshPatch (Set MeshEdge) (Set MeshEdge)
  deriving stock (Eq, Ord, Show)

data CandidateName
  = BalancedDiagonal
  | RaggedBoundary
  | BrokenOverlap
  deriving stock (Eq, Ord, Show)

data SeamCandidate = SeamCandidate
  { scName :: CandidateName,
    scSections :: Map MeshPatch LocalSeamSection
  }
  deriving stock (Eq, Show)

data SeamCost = SeamCost
  { seamCostAreaImbalance :: Int,
    seamCostLength :: Int,
    seamCostJaggedness :: Int
  }
  deriving stock (Eq, Ord, Show)

tests :: TestTree
tests =
  testGroup
    "mesh seam descent"
    [ testCase "compatible local seam sections glue through the shared algebraic edge" testCompatibleSectionsGlue,
      testCase "Moonlight assignment descent sees the mesh-overlap seam sheaf" testMoonlightAssignmentDescentSeesOverlap,
      testCase "overlap disagreement is a typed descent obstruction" testOverlapMismatchRejects,
      testCase "rewrite-equivalent seam serializations normalize to the same edge set" testRewriteEquivalentSerializationsNormalize,
      testCase "cost extraction chooses the lowest-cost lawful seam layout" testCostExtractionChoosesLawfulLayout,
      testCase "seam-chain construction rejects non-incidence jumps" testIllegalGraphJumpRejects
    ]

testCompatibleSectionsGlue :: Assertion
testCompatibleSectionsGlue = do
  withChain (VertexA :| [VertexC]) $ \diagonalChain ->
    let leftSection = localSection PatchLeft [diagonalChain]
        rightSection = localSection PatchRight [diagonalChain]
        sections =
          Map.fromList
            [ (PatchLeft, leftSection),
              (PatchRight, rightSection)
            ]
     in glueLocalSeamSections toyMesh sections
          @?= Right (GlobalSeamLayout (Set.singleton diagonalEdge))

testMoonlightAssignmentDescentSeesOverlap :: Assertion
testMoonlightAssignmentDescentSeesOverlap = do
  withChain (VertexA :| [VertexC]) $ \diagonalChain ->
    withChain (VertexC :| [VertexD]) $ \rightBoundary ->
      let compatibleSections =
            Map.fromList
              [ (PatchWhole, localSection PatchWhole [diagonalChain]),
                (PatchLeft, localSection PatchLeft [diagonalChain]),
                (PatchRight, localSection PatchRight [diagonalChain])
              ]
          mismatchedSections =
            Map.fromList
              [ (PatchWhole, localSection PatchWhole [diagonalChain, rightBoundary]),
                (PatchLeft, localSection PatchLeft [diagonalChain]),
                (PatchRight, localSection PatchRight [rightBoundary])
              ]
       in do
            AssignmentDescent.descentAt unboundedCoverSearchBudget (meshSeamDescentKernel compatibleSections) PatchWhole
              @?= SearchAccepted
            case AssignmentDescent.descentAt unboundedCoverSearchBudget (meshSeamDescentKernel mismatchedSections) PatchWhole of
              SearchAccepted ->
                assertFailure "expected mesh-overlap assignment descent obstruction"
              SearchRejected (AssignmentDescent.DescentConflictObstruction obstruction :| _) -> do
                AssignmentDescent.doCoverElements obstruction @?= [PatchLeft, PatchRight]
                Map.lookup
                  (PatchLeft, PatchRight)
                  (AssignmentDescent.doPairAdmissibility obstruction)
                  @?= Just
                    AssignmentDescent.CompatibilityEvidence
                      { AssignmentDescent.ceSatisfied = False,
                        AssignmentDescent.ceWitness = (),
                        AssignmentDescent.ceCost = Sum 0
                      }
              SearchRejected (_ :| _) ->
                assertFailure "expected mesh-overlap assignment conflict obstruction"
              SearchUndecided {} ->
                assertFailure "unbounded mesh-overlap assignment descent should decide"

testOverlapMismatchRejects :: Assertion
testOverlapMismatchRejects = do
  withChain (VertexA :| [VertexC]) $ \leftDiagonal ->
    withChain (VertexC :| [VertexD]) $ \rightBoundary ->
      let sections =
            Map.fromList
              [ (PatchLeft, localSection PatchLeft [leftDiagonal]),
                (PatchRight, localSection PatchRight [rightBoundary])
              ]
       in glueLocalSeamSections toyMesh sections
            @?= Left
              ( OverlapSeamMismatch
                  PatchLeft
                  PatchRight
                  (Set.singleton diagonalEdge)
                  Set.empty
              )

testRewriteEquivalentSerializationsNormalize :: Assertion
testRewriteEquivalentSerializationsNormalize = do
  withChain (VertexA :| [VertexC, VertexD]) $ \forwardChain ->
    withChain (VertexD :| [VertexC, VertexA]) $ \reversedChain ->
      rewriteNormalizeSeamChain forwardChain
        @?= rewriteNormalizeSeamChain reversedChain

testCostExtractionChoosesLawfulLayout :: Assertion
testCostExtractionChoosesLawfulLayout = do
  withChain (VertexA :| [VertexC]) $ \diagonalChain ->
    withChain (VertexA :| [VertexB, VertexC]) $ \leftBoundary ->
      withChain (VertexC :| [VertexD, VertexA]) $ \rightBoundary ->
        let diagonalCandidate =
              SeamCandidate
                BalancedDiagonal
                ( Map.fromList
                    [ (PatchLeft, localSection PatchLeft [diagonalChain]),
                      (PatchRight, localSection PatchRight [diagonalChain])
                    ]
                )
            raggedCandidate =
              SeamCandidate
                RaggedBoundary
                ( Map.fromList
                    [ (PatchLeft, localSection PatchLeft [leftBoundary]),
                      (PatchRight, localSection PatchRight [rightBoundary])
                    ]
                )
            brokenCandidate =
              SeamCandidate
                BrokenOverlap
                ( Map.fromList
                    [ (PatchLeft, localSection PatchLeft [diagonalChain]),
                      (PatchRight, localSection PatchRight [rightBoundary])
                    ]
                )
         in extractLowestCostLawful
              (diagonalCandidate :| [raggedCandidate, brokenCandidate])
              @?= Just
                ( BalancedDiagonal,
                  SeamCost
                    { seamCostAreaImbalance = 0,
                      seamCostLength = 1,
                      seamCostJaggedness = 0
                    },
                  GlobalSeamLayout (Set.singleton diagonalEdge)
                )

testIllegalGraphJumpRejects :: Assertion
testIllegalGraphJumpRejects = do
  mkSeamChain toyMesh (VertexB :| [VertexD])
    @?= Left (SeamChainIllegalStep VertexB VertexD)

toyMesh :: MeshCellComplex
toyMesh =
  MeshCellComplex
    { mccCells =
        Set.fromList
          [ Mesh0 VertexA,
            Mesh0 VertexB,
            Mesh0 VertexC,
            Mesh0 VertexD,
            Mesh1 (canonicalEdge VertexA VertexB),
            Mesh1 (canonicalEdge VertexB VertexC),
            Mesh1 diagonalEdge,
            Mesh1 (canonicalEdge VertexC VertexD),
            Mesh1 (canonicalEdge VertexA VertexD),
            Mesh2 FaceABC,
            Mesh2 FaceACD
          ],
      mccPatchFaces =
        Map.fromList
          [ (PatchLeft, Set.singleton FaceABC),
            (PatchRight, Set.singleton FaceACD),
            (PatchWhole, Set.fromList [FaceABC, FaceACD])
          ]
    }

diagonalEdge :: MeshEdge
diagonalEdge =
  canonicalEdge VertexA VertexC

canonicalEdge :: MeshVertex -> MeshVertex -> MeshEdge
canonicalEdge leftVertex rightVertex =
  case compare leftVertex rightVertex of
    GT -> MeshEdge rightVertex leftVertex
    _ -> MeshEdge leftVertex rightVertex

faceBoundaryEdges :: MeshFace -> Set MeshEdge
faceBoundaryEdges faceValue =
  Set.fromList $
    case faceValue of
      FaceABC ->
        [ canonicalEdge VertexA VertexB,
          canonicalEdge VertexB VertexC,
          diagonalEdge
        ]
      FaceACD ->
        [ diagonalEdge,
          canonicalEdge VertexC VertexD,
          canonicalEdge VertexA VertexD
        ]

patchEdges :: MeshCellComplex -> MeshPatch -> Set MeshEdge
patchEdges mesh patchValue =
  foldMap faceBoundaryEdges $
    Set.toList (Map.findWithDefault Set.empty patchValue (mccPatchFaces mesh))

overlapEdges :: MeshCellComplex -> MeshPatch -> MeshPatch -> Set MeshEdge
overlapEdges mesh leftPatch rightPatch =
  Set.intersection
    (patchEdges mesh leftPatch)
    (patchEdges mesh rightPatch)

isMeshEdge :: MeshCellComplex -> MeshVertex -> MeshVertex -> Bool
isMeshEdge mesh leftVertex rightVertex =
  Set.member (Mesh1 (canonicalEdge leftVertex rightVertex)) (mccCells mesh)

mkSeamChain :: MeshCellComplex -> NonEmpty MeshVertex -> Either SeamChainError SeamChain
mkSeamChain mesh vertices =
  case NonEmpty.nonEmpty (invalidSteps mesh vertices) of
    Just ((leftVertex, rightVertex) :| _) ->
      Left (SeamChainIllegalStep leftVertex rightVertex)
    Nothing ->
      Right (SeamChain vertices)

invalidSteps :: MeshCellComplex -> NonEmpty MeshVertex -> [(MeshVertex, MeshVertex)]
invalidSteps mesh vertices =
  filter
    (not . uncurry (isMeshEdge mesh))
    (zip (NonEmpty.toList vertices) (drop 1 (NonEmpty.toList vertices)))

seamChainEdges :: SeamChain -> Set MeshEdge
seamChainEdges =
  Set.fromList
    . fmap (uncurry canonicalEdge)
    . adjacentPairs
    . NonEmpty.toList
    . unSeamChain

adjacentPairs :: [a] -> [(a, a)]
adjacentPairs values =
  zip values (drop 1 values)

rewriteNormalizeSeamChain :: SeamChain -> Set MeshEdge
rewriteNormalizeSeamChain =
  seamChainEdges

localSection :: MeshPatch -> [SeamChain] -> LocalSeamSection
localSection patchValue chains =
  LocalSeamSection
    { lssPatch = patchValue,
      lssChains = Set.fromList chains
    }

localSeamEdges :: LocalSeamSection -> Set MeshEdge
localSeamEdges =
  foldMap seamChainEdges . Set.toList . lssChains

type MeshSeamDescentKernel =
  AssignmentDescent.DescentKernel MeshPatch LocalSeamSection MeshEdge Bool () (Sum Int)

meshSeamDescentKernel :: Map MeshPatch LocalSeamSection -> MeshSeamDescentKernel
meshSeamDescentKernel sections =
  AssignmentDescent.DescentKernel
    { AssignmentDescent.dkCoverOf =
        \case
          PatchWhole -> [PatchLeft, PatchRight]
          PatchLeft -> []
          PatchRight -> [],
      AssignmentDescent.dkMaterializedContexts =
        [PatchWhole, PatchLeft, PatchRight],
      AssignmentDescent.dkSectionAt =
        \patchValue ->
          Map.findWithDefault
            (localSection patchValue [])
            patchValue
            sections,
      AssignmentDescent.dkAssignmentOf =
        sectionSeamTruthAssignment,
      AssignmentDescent.dkAdmissibility =
        AssignmentDescent.trivialAdmissibility
    }

sectionSeamTruthAssignment :: LocalSeamSection -> Map MeshEdge Bool
sectionSeamTruthAssignment section =
  let seamEdges = localSeamEdges section
   in Map.fromSet
        (`Set.member` seamEdges)
        (patchEdges toyMesh (lssPatch section))

restrictSectionToOverlap :: MeshCellComplex -> MeshPatch -> MeshPatch -> LocalSeamSection -> Set MeshEdge
restrictSectionToOverlap mesh leftPatch rightPatch section =
  Set.intersection
    (overlapEdges mesh leftPatch rightPatch)
    (localSeamEdges section)

glueLocalSeamSections ::
  MeshCellComplex ->
  Map MeshPatch LocalSeamSection ->
  Either SeamDescentObstruction GlobalSeamLayout
glueLocalSeamSections mesh sections =
  do
    leftSection <- sectionFor PatchLeft sections
    rightSection <- sectionFor PatchRight sections
    let leftOverlap =
          restrictSectionToOverlap mesh PatchLeft PatchRight leftSection
        rightOverlap =
          restrictSectionToOverlap mesh PatchLeft PatchRight rightSection
    if leftOverlap == rightOverlap
      then Right (GlobalSeamLayout (localSeamEdges leftSection <> localSeamEdges rightSection))
      else Left (OverlapSeamMismatch PatchLeft PatchRight leftOverlap rightOverlap)

sectionFor :: MeshPatch -> Map MeshPatch LocalSeamSection -> Either SeamDescentObstruction LocalSeamSection
sectionFor expectedPatch sections =
  case Map.lookup expectedPatch sections of
    Nothing ->
      Left (MissingPatchSection expectedPatch)
    Just section
      | lssPatch section == expectedPatch ->
          Right section
      | otherwise ->
          Left (PatchSectionKeyMismatch expectedPatch (lssPatch section))

seamCost :: GlobalSeamLayout -> SeamCost
seamCost layout =
  let seamEdges = gslSeamEdges layout
   in SeamCost
        { seamCostAreaImbalance =
            if Set.member diagonalEdge seamEdges
              then 0
              else 2,
          seamCostLength =
            Set.size seamEdges,
          seamCostJaggedness =
            Set.size (Set.filter jaggedOuterEdge seamEdges)
        }

jaggedOuterEdge :: MeshEdge -> Bool
jaggedOuterEdge edge =
  edge /= diagonalEdge

extractLowestCostLawful ::
  NonEmpty SeamCandidate ->
  Maybe (CandidateName, SeamCost, GlobalSeamLayout)
extractLowestCostLawful =
  fmap acceptedCandidateResult
    . safeMinimumByCost
    . mapMaybe evaluateCandidate
    . NonEmpty.toList

data AcceptedCandidate = AcceptedCandidate
  { acName :: CandidateName,
    acCost :: SeamCost,
    acLayout :: GlobalSeamLayout
  }
  deriving stock (Eq, Show)

evaluateCandidate :: SeamCandidate -> Maybe AcceptedCandidate
evaluateCandidate candidate =
  case glueLocalSeamSections toyMesh (scSections candidate) of
    Left _ ->
      Nothing
    Right layout ->
      Just
        AcceptedCandidate
          { acName = scName candidate,
            acCost = seamCost layout,
            acLayout = layout
          }

safeMinimumByCost :: [AcceptedCandidate] -> Maybe AcceptedCandidate
safeMinimumByCost =
  fmap NonEmpty.head
    . NonEmpty.nonEmpty
    . List.sortOn acCost

acceptedCandidateResult :: AcceptedCandidate -> (CandidateName, SeamCost, GlobalSeamLayout)
acceptedCandidateResult acceptedCandidate =
  (acName acceptedCandidate, acCost acceptedCandidate, acLayout acceptedCandidate)

withChain :: NonEmpty MeshVertex -> (SeamChain -> Assertion) -> Assertion
withChain vertices onChain =
  case mkSeamChain toyMesh vertices of
    Left failureValue ->
      assertFailure ("expected legal seam chain, got " <> show failureValue)
    Right chain ->
      onChain chain

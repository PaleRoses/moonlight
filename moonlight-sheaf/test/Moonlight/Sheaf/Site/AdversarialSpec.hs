{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Sheaf.Site.AdversarialSpec
  ( siteAdversarialTests,
    prop_adversarialGeneratedSites,
    GeneratedSite (..),
    FiniteGeneratedSite (..),
    H1SanityFailure (..),
    H1Stats (..),
    finiteSiteLawFailures,
    finiteSiteH1Sanity,
    finiteSiteH1Stats,
    genGeneratedSite,
    pointSite,
    chainSite,
    parallelArrowSite,
    mobiusParallelArrowSite,
  )
where

import Control.Monad (void, when)
import Data.Bifunctor (first)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, isNothing, mapMaybe)
import Data.Set qualified as Set
import Hedgehog (Gen, Property)
import Hedgehog qualified as HH
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Moonlight.Homology
  ( BoundaryEntry,
    BoundaryIncidence,
    HomologicalDegree (..),
    HomologyFailure,
    HomologyGroup (..),
    boundaryCoefficient,
    boundaryEntries,
    composeBoundaryIncidence,
    emptyBoundaryIncidenceOf,
    integralHomologyGroupsOf,
    mkBoundaryEntry,
    mkBoundaryIncidence,
    mkFiniteChainComplexChecked,
  )
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism (..),
    CoveringFamily,
    PullbackSquare (..),
    Site (..),
    SiteLawFailure (..),
    coverSize,
    mkCoveringFamily,
  )
import Moonlight.Sheaf.Site.Class.Validation
  ( allAssociativityFailures,
    allCompositionClosureFailures,
    allLeftIdentityFailures,
    allPullbackSquareCommutativityFailures,
    allRightIdentityFailures,
    checkAssociativityLaw,
    checkPullbackSquareCommutativityLaw,
    siteLawFailures,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.Hedgehog (testProperty)

newtype GObj = GObj
  { unGObj :: Int
  }
  deriving stock (Eq, Ord, Show)

newtype GMorId = GMorId
  { unGMorId :: Int
  }
  deriving stock (Eq, Ord, Show)

data GMor = GMor
  { gmId :: !GMorId,
    gmDeclaredSource :: !GObj,
    gmDeclaredTarget :: !GObj
  }
  deriving stock (Eq, Ord, Show)

data FiniteGeneratedSite = FiniteGeneratedSite
  { fgsObjects :: ![GObj],
    fgsMorphisms :: ![CheckedMorphism GObj GMor],
    fgsIdentities :: !(Map GObj (CheckedMorphism GObj GMor)),
    fgsComposition :: !(Map (GMorId, GMorId) (CheckedMorphism GObj GMor)),
    fgsPullbacks :: !(Map (GMorId, GMorId) (PullbackSquare GObj GMor)),
    fgsCovers :: !(Map GObj [CoveringFamily GObj GMor]),
    fgsEdgeRestrictions :: !(Map (GMorId, GObj) Integer)
  }
  deriving stock (Eq, Show)

instance Site FiniteGeneratedSite where
  type SiteObject FiniteGeneratedSite = GObj
  type SiteMorphism FiniteGeneratedSite = GMor

  siteObjects =
    fgsObjects

  siteMorphisms =
    fgsMorphisms

  identityAt site objectValue =
    Map.findWithDefault
      (fallbackIdentity objectValue)
      objectValue
      (fgsIdentities site)

  coversAt site objectValue =
    Map.findWithDefault [] objectValue (fgsCovers site)

  composeChecked site outerMorphism innerMorphism
    | cmSource outerMorphism /= cmTarget innerMorphism =
        Nothing
    | otherwise =
        Map.lookup
          (morphismId outerMorphism, morphismId innerMorphism)
          (fgsComposition site)

  pullbackPair site leftMorphism rightMorphism
    | cmTarget leftMorphism /= cmTarget rightMorphism =
        Nothing
    | otherwise =
        Map.lookup
          (morphismId leftMorphism, morphismId rightMorphism)
          (fgsPullbacks site)

data SiteFault
  = FaultBreakIdentity !GObj !GMorId
  | FaultDropComposition !GMorId !GMorId
  | FaultReplaceComposition !GMorId !GMorId !GMorId
  | FaultDropPullback !GMorId !GMorId
  | FaultReplacePullbackRightLeg !GMorId !GMorId !GMorId
  deriving stock (Eq, Ord, Show)

data GeneratedSite = GeneratedSite
  { gsFaults :: ![SiteFault],
    gsSite :: !FiniteGeneratedSite
  }
  deriving stock (Eq, Show)

data H1SanityFailure
  = H1MissingComposite !(CheckedMorphism GObj GMor) !(CheckedMorphism GObj GMor)
  | H1CompositeOutsideC1 !(CheckedMorphism GObj GMor) !(CheckedMorphism GObj GMor) !(CheckedMorphism GObj GMor)
  | H1NonNilpotent ![BoundaryEntry Integer]
  | H1ChainComplexRejected !HomologyFailure
  | H1ProductionFailure !String
  deriving stock (Eq, Show)

data H1Stats = H1Stats
  { h1sEdgeCount :: !Int,
    h1sSimplexCount :: !Int,
    h1sFreeRank :: !Int,
    h1sTorsion :: ![Integer]
  }
  deriving stock (Eq, Show)

data TwoSimplex = TwoSimplex
  { tsInner :: !(CheckedMorphism GObj GMor),
    tsOuter :: !(CheckedMorphism GObj GMor),
    tsComposite :: !(CheckedMorphism GObj GMor)
  }
  deriving stock (Eq, Show)

siteAdversarialTests :: TestTree
siteAdversarialTests =
  testGroup
    "site/adversarial"
    [ testGroup
        "specific categorical failures"
        [ testCase "undeclared composite is a composition-closure failure" testCompositionClosureFailure,
          testCase "wrong left unit is a left-identity failure" testLeftIdentityFailure,
          testCase "wrong right unit is a right-identity failure" testRightIdentityFailure,
          testCase "unequal association paths are an associativity failure" testAssociativityFailure,
          testCase "noncommuting square is a pullback-commutativity failure" testPullbackCommutativityFailure
        ],
      testProperty
        "generated sites either expose site-law witnesses or have sane H1"
        prop_adversarialGeneratedSites,
      testGroup "topology-sensitivity probe" topologyProbeTestCases
    ]

topologyProbeTestCases :: [TestTree]
topologyProbeTestCases =
  [ testCase
      "point: H1 = 0, no torsion"
      (assertH1 pointSite 0 []),
    testCase
      "chain of 3 (poset, contractible): H1 = 0, no torsion"
      (assertH1 (chainSite 3) 0 []),
    testCase
      "chain of 5 (poset, contractible): H1 = 0, no torsion"
      (assertH1 (chainSite 5) 0 []),
    testCase
      "2 parallel arrows (S^1): H1 = Z (rank 1)"
      (assertH1 (parallelArrowSite 2) 1 []),
    testCase
      "3 parallel arrows (wedge of 2 circles): H1 = Z^2"
      (assertH1 (parallelArrowSite 3) 2 []),
    testCase
      "4 parallel arrows (wedge of 3 circles): H1 = Z^3"
      (assertH1 (parallelArrowSite 4) 3 []),
    testCase
      "Mobius bundle on S^1: H1 = Z/2 (rank 0, torsion [2])"
      (assertH1 (mobiusParallelArrowSite 2) 0 [2])
  ]

testCompositionClosureFailure :: Assertion
testCompositionClosureFailure =
  let site = chainSite 3
   in withMorphism site (GObj 0) (GObj 1) $ \innerMorphism ->
        withMorphism site (GObj 1) (GObj 2) $ \outerMorphism ->
          let undeclaredComposite = makeMor 999 (GObj 0) (GObj 2)
              brokenSite =
                replaceComposition
                  outerMorphism
                  innerMorphism
                  undeclaredComposite
                  site
              expectedFailure =
                CompositeOutsideSiteMorphisms
                  outerMorphism
                  innerMorphism
                  undeclaredComposite
           in assertBool
                ("expected composition-closure failure, received " <> show (allCompositionClosureFailures brokenSite))
                (expectedFailure `elem` allCompositionClosureFailures brokenSite)

testLeftIdentityFailure :: Assertion
testLeftIdentityFailure =
  let site = chainSite 2
   in withMorphism site (GObj 0) (GObj 1) $ \morphismValue ->
        let leftIdentity = identityAt site (cmTarget morphismValue)
            brokenSite =
              replaceComposition
                leftIdentity
                morphismValue
                leftIdentity
                site
            expectedFailure =
              LeftIdentityLawFailed morphismValue (Just leftIdentity)
         in assertBool
              ("expected left-identity failure, received " <> show (allLeftIdentityFailures brokenSite))
              (expectedFailure `elem` allLeftIdentityFailures brokenSite)

testRightIdentityFailure :: Assertion
testRightIdentityFailure =
  let site = chainSite 2
   in withMorphism site (GObj 0) (GObj 1) $ \morphismValue ->
        let rightIdentity = identityAt site (cmSource morphismValue)
            brokenSite =
              replaceComposition
                morphismValue
                rightIdentity
                rightIdentity
                site
            expectedFailure =
              RightIdentityLawFailed morphismValue (Just rightIdentity)
         in assertBool
              ("expected right-identity failure, received " <> show (allRightIdentityFailures brokenSite))
              (expectedFailure `elem` allRightIdentityFailures brokenSite)

testAssociativityFailure :: Assertion
testAssociativityFailure =
  let site = chainSite 4
   in withMorphism site (GObj 0) (GObj 1) $ \innerMorphism ->
        withMorphism site (GObj 1) (GObj 2) $ \middleMorphism ->
          withMorphism site (GObj 2) (GObj 3) $ \outerMorphism ->
            withMorphism site (GObj 0) (GObj 2) $ \innerComposite ->
              let brokenSite =
                    replaceComposition
                      outerMorphism
                      innerComposite
                      middleMorphism
                      site
               in case checkAssociativityLaw brokenSite outerMorphism middleMorphism innerMorphism of
                    Just failure@(AssociativityLawFailed _ _ _ _ _) ->
                      assertBool
                        ("expected enumerated associativity failure, received " <> show (allAssociativityFailures brokenSite))
                        (failure `elem` allAssociativityFailures brokenSite)
                    Just failure ->
                      assertFailure ("expected associativity failure, received " <> show failure)
                    Nothing ->
                      assertFailure "expected associativity failure"

testPullbackCommutativityFailure :: Assertion
testPullbackCommutativityFailure =
  let site = chainSite 3
   in withMorphism site (GObj 0) (GObj 2) $ \leftMorphism ->
        withMorphism site (GObj 1) (GObj 2) $ \rightMorphism ->
          withPullbackSquare site leftMorphism rightMorphism $ \square ->
            let brokenSquare =
                  square
                    { psToRight = identityAt site (GObj 0)
                    }
                brokenSite =
                  site
                    { fgsPullbacks =
                        Map.insert
                          (morphismId leftMorphism, morphismId rightMorphism)
                          brokenSquare
                          (fgsPullbacks site)
                    }
             in case checkPullbackSquareCommutativityLaw brokenSite brokenSquare of
                  Just failure@(PullbackSquareDoesNotCommute _ _ _) ->
                    assertBool
                      ("expected enumerated pullback-commutativity failure, received " <> show (allPullbackSquareCommutativityFailures brokenSite))
                      (failure `elem` allPullbackSquareCommutativityFailures brokenSite)
                  Just failure ->
                    assertFailure ("expected pullback-commutativity failure, received " <> show failure)
                  Nothing ->
                    assertFailure "expected pullback-commutativity failure"

replaceComposition ::
  CheckedMorphism GObj GMor ->
  CheckedMorphism GObj GMor ->
  CheckedMorphism GObj GMor ->
  FiniteGeneratedSite ->
  FiniteGeneratedSite
replaceComposition outerMorphism innerMorphism replacement site =
  site
    { fgsComposition =
        Map.insert
          (morphismId outerMorphism, morphismId innerMorphism)
          replacement
          (fgsComposition site)
    }

withMorphism ::
  FiniteGeneratedSite ->
  GObj ->
  GObj ->
  (CheckedMorphism GObj GMor -> Assertion) ->
  Assertion
withMorphism site sourceObject targetObject continue =
  case
      find
        (\morphismValue -> cmSource morphismValue == sourceObject && cmTarget morphismValue == targetObject)
        (fgsMorphisms site)
    of
      Nothing ->
        assertFailure ("missing fixture morphism " <> show (sourceObject, targetObject))
      Just morphismValue ->
        continue morphismValue

withPullbackSquare ::
  FiniteGeneratedSite ->
  CheckedMorphism GObj GMor ->
  CheckedMorphism GObj GMor ->
  (PullbackSquare GObj GMor -> Assertion) ->
  Assertion
withPullbackSquare site leftMorphism rightMorphism continue =
  case
      Map.lookup
        (morphismId leftMorphism, morphismId rightMorphism)
        (fgsPullbacks site)
    of
      Nothing ->
        assertFailure ("missing fixture pullback " <> show (leftMorphism, rightMorphism))
      Just square ->
        continue square

assertH1 :: FiniteGeneratedSite -> Int -> [Integer] -> Assertion
assertH1 site expectedRank expectedTorsion = do
  case finiteSiteH1Stats site of
    Left h1Failure ->
      assertBool
        ("expected production H1 to compute, got failure: " <> show h1Failure)
        False
    Right stats -> do
      assertEqual "H1 free rank" expectedRank (h1sFreeRank stats)
      assertEqual "H1 torsion invariants" expectedTorsion (h1sTorsion stats)

prop_adversarialGeneratedSites :: Property
prop_adversarialGeneratedSites =
  HH.property $ do
    generated <- HH.forAll genGeneratedSite
    let site = gsSite generated
        lawFailures = finiteSiteLawFailures site
        h1Result = finiteSiteH1Stats site
        isLawful = null lawFailures
        hasFlips = not (Map.null (fgsEdgeRestrictions site))
        nonTrivialRank =
          either (const False) ((> 0) . h1sFreeRank) h1Result
        hasTorsion =
          either (const False) (not . null . h1sTorsion) h1Result
    HH.annotateShow generated
    HH.cover 25 "lawful" isLawful
    HH.cover 15 "lawful with H1 free rank > 0" (isLawful && nonTrivialRank)
    HH.cover 10 "lawful with H1 torsion" (isLawful && hasTorsion)
    HH.cover 25 "faulty" (not isLawful)
    HH.cover 10 "lawful, no flips (constant Z coefficients)" (isLawful && not hasFlips)
    case lawFailures of
      [] ->
        case h1Result of
          Right stats -> do
            assertConstantZTorsionFree site stats
            assertFreeRankWithinBounds site stats
            HH.success
          Left h1Failure -> do
            HH.annotate
              ( "site passed finite site-law checks but H1 sanity failed: "
                  <> show h1Failure
              )
            HH.failure
      failures -> do
        HH.annotate (renderLawFailures failures)
        HH.assert (all siteLawFailureHasWitness failures)

assertConstantZTorsionFree ::
  FiniteGeneratedSite ->
  H1Stats ->
  HH.PropertyT IO ()
assertConstantZTorsionFree site stats =
  when (Map.null (fgsEdgeRestrictions site) && not (null (h1sTorsion stats))) $ do
    HH.annotate
      ( "site has no restriction flips (constant Z sheaf), so H1 must be torsion-free; "
          <> "production returned torsion="
          <> show (h1sTorsion stats)
      )
    HH.failure

assertFreeRankWithinBounds ::
  FiniteGeneratedSite ->
  H1Stats ->
  HH.PropertyT IO ()
assertFreeRankWithinBounds _site stats = do
  let freeRankValue = h1sFreeRank stats
      edgeCount = h1sEdgeCount stats
  when (freeRankValue < 0 || freeRankValue > edgeCount) $ do
    HH.annotate
      ( "H1 free rank "
          <> show freeRankValue
          <> " is outside the valid range [0, "
          <> show edgeCount
          <> "]"
      )
    HH.failure

finiteSiteLawFailures ::
  FiniteGeneratedSite ->
  [SiteLawFailure GObj GMor]
finiteSiteLawFailures =
  siteLawFailures

finiteSiteH1Sanity ::
  FiniteGeneratedSite ->
  Either H1SanityFailure ()
finiteSiteH1Sanity =
  void . finiteSiteH1Stats

finiteSiteH1Stats ::
  FiniteGeneratedSite ->
  Either H1SanityFailure H1Stats
finiteSiteH1Stats site = do
  case missingComposites of
    (innerMorphism, outerMorphism) : _ ->
      Left (H1MissingComposite innerMorphism outerMorphism)
    [] ->
      pure ()
  d0Incidence <-
    h1BoundaryIncidence
      "Moonlight.Sheaf.Site.AdversarialSpec.finiteSiteH1Stats.d0"
      objectCount
      edgeCount
      (concatMap d0EntriesFor nonIdentityMorphisms)
  d1Incidence <- buildD1Incidence
  case composeBoundaryIncidence d1Incidence d0Incidence of
    Left _ ->
      Left
        ( H1ProductionFailure
            "shape mismatch composing d^1 . d^0 (impossible by construction)"
        )
    Right composedIncidence -> do
      let nonZeroComposite =
            filter ((/= 0) . boundaryCoefficient) (boundaryEntries composedIncidence)
      case nonZeroComposite of
        _ : _ ->
          Left (H1NonNilpotent nonZeroComposite)
        [] ->
          pure ()
  chainComplex <-
    first H1ChainComplexRejected $
      mkFiniteChainComplexChecked
        (HomologicalDegree 2)
        ( \(HomologicalDegree degreeValue) ->
            case degreeValue of
              0 ->
                emptyBoundaryIncidenceOf (fromIntegral simplexCount) 0
              1 ->
                d1Incidence
              2 ->
                d0Incidence
              _ ->
                emptyBoundaryIncidenceOf 0 (fromIntegral objectCount)
        )
  case integralHomologyGroupsOf chainComplex of
    Left failure ->
      Left (H1ProductionFailure (show failure))
    Right groups ->
      case drop 1 groups of
        h1Group : _ ->
          Right
            H1Stats
              { h1sEdgeCount = edgeCount,
                h1sSimplexCount = simplexCount,
                h1sFreeRank = freeRank h1Group,
                h1sTorsion = torsionInvariants h1Group
              }
        [] ->
          Left
            ( H1ProductionFailure
                "expected at least 2 homology groups (H_0, H_1)"
            )
  where
    objects =
      fgsObjects site

    objectCount =
      length objects

    objectIndex =
      Map.fromList (zip objects [0 :: Int ..])

    nonIdentityMorphisms =
      filter isNonIdentity (fgsMorphisms site)

    edgeCount =
      length nonIdentityMorphisms

    edgeIndex =
      Map.fromList
        [ (morphismId morphismValue, edgeOrdinal)
        | (edgeOrdinal, morphismValue) <- zip [0 :: Int ..] nonIdentityMorphisms
        ]

    missingComposites =
      [ (innerMorphism, outerMorphism)
      | innerMorphism <- nonIdentityMorphisms,
        outerMorphism <- nonIdentityMorphisms,
        cmTarget innerMorphism == cmSource outerMorphism,
        isNothing (composeChecked site outerMorphism innerMorphism)
      ]

    twoSimplices =
      [ TwoSimplex
          { tsInner = innerMorphism,
            tsOuter = outerMorphism,
            tsComposite = compositeMorphism
          }
      | innerMorphism <- nonIdentityMorphisms,
        outerMorphism <- nonIdentityMorphisms,
        cmTarget innerMorphism == cmSource outerMorphism,
        Just compositeMorphism <- [composeChecked site outerMorphism innerMorphism],
        isNonIdentity compositeMorphism
      ]

    simplexCount =
      length twoSimplices

    restrictionSign morphismValue vertexValue =
      Map.findWithDefault 1 (morphismId morphismValue, vertexValue) (fgsEdgeRestrictions site)

    d0EntriesFor morphismValue =
      case
        ( Map.lookup (morphismId morphismValue) edgeIndex,
          Map.lookup (cmSource morphismValue) objectIndex,
          Map.lookup (cmTarget morphismValue) objectIndex
        )
      of
        (Just edgeOrdinal, Just sourceOrdinal, Just targetOrdinal) ->
          let sourceSign = restrictionSign morphismValue (cmSource morphismValue)
              targetSign = restrictionSign morphismValue (cmTarget morphismValue)
           in [ mkBoundaryEntry
                  (fromIntegral sourceOrdinal)
                  (fromIntegral edgeOrdinal)
                  (negate sourceSign),
                mkBoundaryEntry
                  (fromIntegral targetOrdinal)
                  (fromIntegral edgeOrdinal)
                  targetSign
              ]
        _ ->
          []

    buildD1Incidence ::
      Either H1SanityFailure (BoundaryIncidence Integer)
    buildD1Incidence = do
      entryGroups <- traverse d1EntriesFor (zip [0 :: Int ..] twoSimplices)
      h1BoundaryIncidence
        "Moonlight.Sheaf.Site.AdversarialSpec.finiteSiteH1Stats.d1"
        edgeCount
        simplexCount
        (concat entryGroups)

    h1BoundaryIncidence ::
      String ->
      Int ->
      Int ->
      [BoundaryEntry Integer] ->
      Either H1SanityFailure (BoundaryIncidence Integer)
    h1BoundaryIncidence label sourceCount targetCount entries =
      first
        (H1ProductionFailure . ((label <> ": ") <>) . show)
        ( mkBoundaryIncidence
            (fromIntegral sourceCount)
            (fromIntegral targetCount)
            entries
        )

    d1EntriesFor (triangleOrdinal, simplex) =
      case
        ( Map.lookup (morphismId (tsInner simplex)) edgeIndex,
          Map.lookup (morphismId (tsOuter simplex)) edgeIndex,
          Map.lookup (morphismId (tsComposite simplex)) edgeIndex
        )
      of
        (Just innerOrdinal, Just outerOrdinal, Just compositeOrdinal) ->
          Right
            [ mkBoundaryEntry
                (fromIntegral innerOrdinal)
                (fromIntegral triangleOrdinal)
                1,
              mkBoundaryEntry
                (fromIntegral outerOrdinal)
                (fromIntegral triangleOrdinal)
                1,
              mkBoundaryEntry
                (fromIntegral compositeOrdinal)
                (fromIntegral triangleOrdinal)
                (-1)
            ]
        _ ->
          Left
            ( H1CompositeOutsideC1
                (tsInner simplex)
                (tsOuter simplex)
                (tsComposite simplex)
            )

genGeneratedSite :: Gen GeneratedSite
genGeneratedSite =
  Gen.frequency
    [ (3, genLawfulGeneratedSite),
      (2, genFaultedGeneratedSite)
    ]

genLawfulGeneratedSite :: Gen GeneratedSite
genLawfulGeneratedSite =
  Gen.frequency
    [ (1, GeneratedSite [] . lawfulBipartiteSite <$> genBipartiteSpec),
      (3, GeneratedSite [] . parallelArrowSite <$> Gen.element [2, 3, 4]),
      (3, pure (GeneratedSite [] (mobiusParallelArrowSite 2))),
      (1, GeneratedSite [] . chainSite <$> Gen.element [1, 2, 3, 4, 5])
    ]

genFaultedGeneratedSite :: Gen GeneratedSite
genFaultedGeneratedSite = do
  spec <- genBipartiteSpec
  let lawfulSite = lawfulBipartiteSite spec
  case Map.keys (fgsComposition lawfulSite) of
    [] ->
      pure (GeneratedSite [] lawfulSite)
    firstCompositionPair : remainingCompositionPairs -> do
      requiredCompositionPair <-
        Gen.element (firstCompositionPair : remainingCompositionPairs)
      maybeFaults <- Gen.list (Range.constant 0 4) (genFault lawfulSite)
      let faults =
            catMaybes maybeFaults
              <> [uncurry FaultDropComposition requiredCompositionPair]
          site = foldl' (flip applyFault) lawfulSite faults
      pure
        GeneratedSite
          { gsFaults = faults,
            gsSite = site
          }

data BipartiteSpec = BipartiteSpec
  { bsSourceCount :: !Int,
    bsTargetCount :: !Int,
    bsEdgeMultiplicities :: !(Map (Int, Int) Int),
    bsRestrictionFlips :: !(Set.Set (Int, Int, Int))
  }
  deriving stock (Eq, Show)

genBipartiteSpec :: Gen BipartiteSpec
genBipartiteSpec = do
  sourceCount <- Gen.element [1, 2, 3]
  targetCount <- Gen.element [1, 2, 3]
  multiplicityEntries <-
    traverse
      ( \pairKey -> do
          parallelCount <-
            Gen.frequency
              [ (2, pure 0),
                (3, pure 1),
                (3, pure 2),
                (2, pure 3),
                (1, pure 4)
              ]
          pure (pairKey, parallelCount)
      )
      [(s, t) | s <- [0 .. sourceCount - 1], t <- [0 .. targetCount - 1]]
  let edges =
        Map.fromList (filter ((> 0) . snd) multiplicityEntries)
      arrowTriples =
        [ (s, t, k)
        | ((s, t), parallelCount) <- Map.toList edges,
          k <- [0 .. parallelCount - 1]
        ]
  allowFlips <- Gen.frequency [(2, pure False), (3, pure True)]
  flippedArrows <-
    if not allowFlips
      then pure []
      else
        catMaybes
          <$>
          traverse
            ( \arrow -> do
                shouldFlip <- Gen.frequency [(2, pure False), (1, pure True)]
                pure (if shouldFlip then Just arrow else Nothing)
            )
            arrowTriples
  pure
    BipartiteSpec
      { bsSourceCount = sourceCount,
        bsTargetCount = targetCount,
        bsEdgeMultiplicities = edges,
        bsRestrictionFlips = Set.fromList flippedArrows
      }

lawfulBipartiteSite :: BipartiteSpec -> FiniteGeneratedSite
lawfulBipartiteSite spec =
  FiniteGeneratedSite
    { fgsObjects = objects,
      fgsMorphisms = identityMorphisms <> arrowMorphisms,
      fgsIdentities = identityMap,
      fgsComposition = compositionMap,
      fgsPullbacks = pullbackMap,
      fgsCovers = Map.empty,
      fgsEdgeRestrictions = restrictionMap
    }
  where
    sourceCount =
      max 0 (bsSourceCount spec)

    targetCount =
      max 0 (bsTargetCount spec)

    sourceObjects =
      [GObj i | i <- [0 .. sourceCount - 1]]

    targetObjects =
      [GObj (sourceCount + j) | j <- [0 .. targetCount - 1]]

    objects =
      sourceObjects <> targetObjects

    identityMorphisms =
      [makeMor objectKey objectValue objectValue | objectValue@(GObj objectKey) <- objects]

    identityMap =
      Map.fromList (zip objects identityMorphisms)

    arrowOffset =
      sourceCount + targetCount

    arrowTriples =
      [ (s, t, k)
      | ((s, t), parallelCount) <- Map.toAscList (bsEdgeMultiplicities spec),
        k <- [0 .. parallelCount - 1]
      ]

    arrowIdLookup :: Map (Int, Int, Int) Int
    arrowIdLookup =
      Map.fromList (zip arrowTriples [arrowOffset ..])

    arrowMorphisms =
      [ makeMor arrowId (GObj s) (GObj (sourceCount + t))
      | ((s, t, _k), arrowId) <- Map.toAscList arrowIdLookup
      ]

    allMorphisms =
      identityMorphisms <> arrowMorphisms

    identityFor objectValue =
      Map.findWithDefault
        (fallbackIdentity objectValue)
        objectValue
        identityMap

    compositionMap =
      Map.fromList $
        [ ((morphismId (identityFor (cmTarget m)), morphismId m), m)
        | m <- allMorphisms
        ]
          <> [ ((morphismId m, morphismId (identityFor (cmSource m))), m)
             | m <- allMorphisms
             ]

    pullbackMap =
      identityAndDiagonalPullbacks identityFor allMorphisms

    restrictionMap =
      Map.fromList
        [ ((GMorId arrowId, GObj (sourceCount + t)), -1)
        | (s, t, k) <- Set.toList (bsRestrictionFlips spec),
          Just arrowId <- [Map.lookup (s, t, k) arrowIdLookup]
        ]

genFault :: FiniteGeneratedSite -> Gen (Maybe SiteFault)
genFault site =
  Gen.choice
    [ genBreakIdentity site,
      genDropComposition site,
      genReplaceComposition site,
      genDropPullback site,
      genReplacePullbackRightLeg site
    ]

genBreakIdentity :: FiniteGeneratedSite -> Gen (Maybe SiteFault)
genBreakIdentity site = do
  maybeObject <- genMaybeElement (fgsObjects site)
  maybeReplacement <- genMaybeElement (filter isNonIdentity (fgsMorphisms site))
  pure $ case (maybeObject, maybeReplacement) of
    (Just objectValue, Just replacement) ->
      Just (FaultBreakIdentity objectValue (morphismId replacement))
    _ ->
      Nothing

genDropComposition :: FiniteGeneratedSite -> Gen (Maybe SiteFault)
genDropComposition site =
  fmap
    (fmap (uncurry FaultDropComposition))
    (genMaybeElement (Map.keys (fgsComposition site)))

genReplaceComposition :: FiniteGeneratedSite -> Gen (Maybe SiteFault)
genReplaceComposition site = do
  maybePair <- genMaybeElement (Map.keys (fgsComposition site))
  maybeReplacement <- genMaybeElement (fgsMorphisms site)
  pure $ case (maybePair, maybeReplacement) of
    (Just (outerId, innerId), Just replacement) ->
      Just (FaultReplaceComposition outerId innerId (morphismId replacement))
    _ ->
      Nothing

genDropPullback :: FiniteGeneratedSite -> Gen (Maybe SiteFault)
genDropPullback site =
  fmap
    (fmap (uncurry FaultDropPullback))
    (genMaybeElement (Map.keys (fgsPullbacks site)))

genReplacePullbackRightLeg :: FiniteGeneratedSite -> Gen (Maybe SiteFault)
genReplacePullbackRightLeg site = do
  maybePair <- genMaybeElement (Map.keys (fgsPullbacks site))
  maybeReplacement <- genMaybeElement (fgsMorphisms site)
  pure $ case (maybePair, maybeReplacement) of
    (Just (leftId, rightId), Just replacement) ->
      Just (FaultReplacePullbackRightLeg leftId rightId (morphismId replacement))
    _ ->
      Nothing

genMaybeElement :: [value] -> Gen (Maybe value)
genMaybeElement values =
  case values of
    [] ->
      pure Nothing
    _ : _ ->
      Just <$> Gen.element values

lawfulFiniteSite ::
  Int ->
  Map GObj [[GObj]] ->
  FiniteGeneratedSite
lawfulFiniteSite objectCount coverSpecs =
  FiniteGeneratedSite
    { fgsObjects = objects,
      fgsMorphisms = morphisms,
      fgsIdentities = identityMap,
      fgsComposition = compositionMap,
      fgsPullbacks = pullbackMap,
      fgsCovers = coverMap,
      fgsEdgeRestrictions = Map.empty
    }
  where
    normalizedCount =
      max 1 objectCount

    objects =
      fmap GObj [0 .. normalizedCount - 1]

    endpointPairs =
      [ (sourceObject, targetObject)
      | sourceObject <- objects,
        targetObject <- objects,
        sourceObject <= targetObject
      ]

    morphisms =
      fmap toCheckedMorphism (zip [0 :: Int ..] endpointPairs)

    toCheckedMorphism (ordinal, (sourceObject, targetObject)) =
      CheckedMorphism
        { cmSource = sourceObject,
          cmTarget = targetObject,
          cmWitness =
            GMor
              { gmId = GMorId ordinal,
                gmDeclaredSource = sourceObject,
                gmDeclaredTarget = targetObject
              }
        }

    morphismByEndpoint =
      Map.fromList
        [ ((cmSource morphismValue, cmTarget morphismValue), morphismValue)
        | morphismValue <- morphisms
        ]

    identityMap =
      Map.fromList
        ( mapMaybe
            ( \objectValue ->
                fmap
                  (objectValue,)
                  (Map.lookup (objectValue, objectValue) morphismByEndpoint)
            )
            objects
        )

    compositionMap =
      Map.fromList
        [ ((morphismId outerMorphism, morphismId innerMorphism), compositeMorphism)
        | innerMorphism <- morphisms,
          outerMorphism <- morphisms,
          cmTarget innerMorphism == cmSource outerMorphism,
          Just compositeMorphism <-
            [Map.lookup (cmSource innerMorphism, cmTarget outerMorphism) morphismByEndpoint]
        ]

    pullbackMap =
      Map.fromList
        [ ((morphismId leftMorphism, morphismId rightMorphism), square)
        | leftMorphism <- morphisms,
          rightMorphism <- morphisms,
          cmTarget leftMorphism == cmTarget rightMorphism,
          Just square <- [pullbackFor leftMorphism rightMorphism]
        ]

    pullbackFor leftMorphism rightMorphism = do
      let apex = min (cmSource leftMorphism) (cmSource rightMorphism)
      leftLeg <- Map.lookup (apex, cmSource leftMorphism) morphismByEndpoint
      rightLeg <- Map.lookup (apex, cmSource rightMorphism) morphismByEndpoint
      pure
        PullbackSquare
          { psLeftBase = leftMorphism,
            psRightBase = rightMorphism,
            psApex = apex,
            psToLeft = leftLeg,
            psToRight = rightLeg
          }

    coverMap =
      Map.fromList
        [ (targetObject, covers)
        | targetObject <- objects,
          let requestedCovers = Map.findWithDefault [] targetObject coverSpecs
              covers =
                mapMaybe
                  (mkCoverFromSources morphismByEndpoint targetObject . normalizeCoverSources targetObject)
                  requestedCovers,
          not (null covers)
        ]

normalizeCoverSources :: GObj -> [GObj] -> [GObj]
normalizeCoverSources targetObject =
  Set.toAscList
    . Set.filter (<= targetObject)
    . Set.fromList

mkCoverFromSources ::
  Map (GObj, GObj) (CheckedMorphism GObj GMor) ->
  GObj ->
  [GObj] ->
  Maybe (CoveringFamily GObj GMor)
mkCoverFromSources morphismByEndpoint targetObject sourceObjects =
  case traverse (\sourceObject -> Map.lookup (sourceObject, targetObject) morphismByEndpoint) sourceObjects of
    Just (firstArrow : remainingArrows) ->
      coveringFamilyMaybe targetObject (firstArrow :| remainingArrows)
    _ ->
      Nothing

coveringFamilyMaybe ::
  Eq obj =>
  obj ->
  NonEmpty (CheckedMorphism obj mor) ->
  Maybe (CoveringFamily obj mor)
coveringFamilyMaybe targetObject arrows =
  case mkCoveringFamily targetObject arrows of
    Left _ ->
      Nothing
    Right coveringFamily ->
      Just coveringFamily

applyFault :: SiteFault -> FiniteGeneratedSite -> FiniteGeneratedSite
applyFault fault site =
  case fault of
    FaultBreakIdentity objectValue replacementId ->
      case Map.lookup replacementId morphismById of
        Nothing ->
          site
        Just replacement ->
          site
            { fgsIdentities =
                Map.insert objectValue replacement (fgsIdentities site)
            }
    FaultDropComposition outerId innerId ->
      site
        { fgsComposition =
            Map.delete (outerId, innerId) (fgsComposition site)
        }
    FaultReplaceComposition outerId innerId replacementId ->
      case Map.lookup replacementId morphismById of
        Nothing ->
          site
        Just replacement ->
          site
            { fgsComposition =
                Map.insert
                  (outerId, innerId)
                  replacement
                  (fgsComposition site)
            }
    FaultDropPullback leftId rightId ->
      site
        { fgsPullbacks =
            Map.delete (leftId, rightId) (fgsPullbacks site)
        }
    FaultReplacePullbackRightLeg leftId rightId replacementId ->
      case (Map.lookup (leftId, rightId) (fgsPullbacks site), Map.lookup replacementId morphismById) of
        (Just square, Just replacement) ->
          site
            { fgsPullbacks =
                Map.insert
                  (leftId, rightId)
                  square {psToRight = replacement}
                  (fgsPullbacks site)
            }
        _ ->
          site
  where
    morphismById =
      morphismsById site

morphismsById ::
  FiniteGeneratedSite ->
  Map GMorId (CheckedMorphism GObj GMor)
morphismsById site =
  Map.fromList
    [ (morphismId morphismValue, morphismValue)
    | morphismValue <- fgsMorphisms site
    ]

morphismId :: CheckedMorphism GObj GMor -> GMorId
morphismId =
  gmId . cmWitness

fallbackIdentity :: GObj -> CheckedMorphism GObj GMor
fallbackIdentity objectValue =
  CheckedMorphism
    { cmSource = objectValue,
      cmTarget = objectValue,
      cmWitness =
        GMor
          { gmId = GMorId (negate (unGObj objectValue + 1)),
            gmDeclaredSource = objectValue,
            gmDeclaredTarget = objectValue
          }
    }

isNonIdentity :: CheckedMorphism GObj GMor -> Bool
isNonIdentity morphismValue =
  cmSource morphismValue /= cmTarget morphismValue

siteLawFailureHasWitness :: SiteLawFailure GObj GMor -> Bool
siteLawFailureHasWitness =
  \case
    IdentityCoverMalformed _ ->
      True
    CompositionUnavailable _ _ ->
      True
    CompositeOutsideSiteMorphisms _ _ _ ->
      True
    LeftIdentityLawFailed _ _ ->
      True
    RightIdentityLawFailed _ _ ->
      True
    AssociativityLawFailed _ _ _ _ _ ->
      True
    PullbackSquareDoesNotCommute _ _ _ ->
      True
    PullbackConstructionFailed _ coverValue _ ->
      coverSize coverValue > 0
    PullbackCoverWrongTarget _ coverValue _ ->
      coverSize coverValue > 0
    TransitivityConstructionFailed coverValue _ ->
      coverSize coverValue > 0
    TransitiveCoverWrongTarget coverValue _ ->
      coverSize coverValue > 0

renderLawFailures :: [SiteLawFailure GObj GMor] -> String
renderLawFailures failures =
  unlines
    [ show ordinal <> ": " <> show failureValue
    | (ordinal, failureValue) <- zip [0 :: Int ..] failures
    ]

pointSite :: FiniteGeneratedSite
pointSite =
  lawfulFiniteSite 1 Map.empty

chainSite :: Int -> FiniteGeneratedSite
chainSite n =
  lawfulFiniteSite (max 1 n) Map.empty

parallelArrowSite :: Int -> FiniteGeneratedSite
parallelArrowSite rawK =
  FiniteGeneratedSite
    { fgsObjects = [objectA, objectB],
      fgsMorphisms = morphisms,
      fgsIdentities = identities,
      fgsComposition = composition,
      fgsPullbacks = identityAndDiagonalPullbacks identityOf morphisms,
      fgsCovers = Map.empty,
      fgsEdgeRestrictions = Map.empty
    }
  where
    arrowCount =
      max 0 rawK

    objectA =
      GObj 0

    objectB =
      GObj 1

    idA =
      makeMor 0 objectA objectA

    idB =
      makeMor 1 objectB objectB

    parallelArrows =
      [ makeMor (2 + i) objectA objectB
      | i <- [0 .. arrowCount - 1]
      ]

    morphisms =
      idA : idB : parallelArrows

    identities =
      Map.fromList [(objectA, idA), (objectB, idB)]

    identityOf objectValue
      | objectValue == objectA =
          idA
      | otherwise =
          idB

    composition =
      Map.fromList
        ( [ ((morphismId (identityOf (cmTarget m)), morphismId m), m)
          | m <- morphisms
          ]
            <> [ ((morphismId m, morphismId (identityOf (cmSource m))), m)
               | m <- morphisms
               ]
        )

identityAndDiagonalPullbacks ::
  (GObj -> CheckedMorphism GObj GMor) ->
  [CheckedMorphism GObj GMor] ->
  Map (GMorId, GMorId) (PullbackSquare GObj GMor)
identityAndDiagonalPullbacks identityFor morphisms =
  Map.fromList (concatMap pullbacksFor morphisms)
  where
    pullbacksFor morphismValue =
      let sourceIdentity = identityFor (cmSource morphismValue)
          targetIdentity = identityFor (cmTarget morphismValue)
       in [ ( (morphismId targetIdentity, morphismId morphismValue),
              PullbackSquare
                { psLeftBase = targetIdentity,
                  psRightBase = morphismValue,
                  psApex = cmSource morphismValue,
                  psToLeft = morphismValue,
                  psToRight = sourceIdentity
                }
            ),
            ( (morphismId morphismValue, morphismId targetIdentity),
              PullbackSquare
                { psLeftBase = morphismValue,
                  psRightBase = targetIdentity,
                  psApex = cmSource morphismValue,
                  psToLeft = sourceIdentity,
                  psToRight = morphismValue
                }
            ),
            ( (morphismId morphismValue, morphismId morphismValue),
              PullbackSquare
                { psLeftBase = morphismValue,
                  psRightBase = morphismValue,
                  psApex = cmSource morphismValue,
                  psToLeft = sourceIdentity,
                  psToRight = sourceIdentity
                }
            )
          ]

makeMor :: Int -> GObj -> GObj -> CheckedMorphism GObj GMor
makeMor morIdInt sourceObject targetObject =
  CheckedMorphism
    { cmSource = sourceObject,
      cmTarget = targetObject,
      cmWitness =
        GMor
          { gmId = GMorId morIdInt,
            gmDeclaredSource = sourceObject,
            gmDeclaredTarget = targetObject
          }
    }

mobiusParallelArrowSite :: Int -> FiniteGeneratedSite
mobiusParallelArrowSite rawK =
  let baseSite =
        parallelArrowSite rawK
      flippedArrows =
        filter
          (\arrow -> cmSource arrow /= cmTarget arrow)
          (fgsMorphisms baseSite)
   in case flippedArrows of
        [] ->
          baseSite
        firstArrow : _ ->
          let targetObject =
                cmTarget firstArrow
           in baseSite
                { fgsEdgeRestrictions =
                    Map.singleton (morphismId firstArrow, targetObject) (-1)
                }

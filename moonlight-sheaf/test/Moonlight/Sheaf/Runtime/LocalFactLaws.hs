module Moonlight.Sheaf.Runtime.LocalFactLaws
  ( tests,
  )
where

import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Core
  ( BoundaryOps (..),
  )
import Moonlight.Differential.Proposition
  ( PropositionKey (..),
  )
import Moonlight.Differential.Fact.Local
  ( LocalFact,
    lfAddress,
    lfEvidence,
    mkLocalFact,
    LocalFactCompatibility (..),
    LocalFactObstruction (..),
    carrierEmpty,
    carrierContexts,
    closure,
    compatibleFacts,
    compatibleOnOverlap,
    dominates,
    emptyFactAntichain,
    exportBoundary,
    insertAntichain,
    laCarrier,
    laProp,
    laSupport,
    lookupByKey,
    membersAntichain,
    minimizeSupport,
    mkLocalAddress,
    overlapBetween,
    restrictBoundary,
    subsumes,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertFailure,
    testCase,
    (@?=),
  )
import Test.Tasty.QuickCheck qualified as QC
import Moonlight.FiniteLattice
  ( ContextLattice,
    compileContextLattice,
    contextOrderDecl
  )
import Moonlight.FiniteLattice
  ( SupportBasis,
    supportBasis
  )

tests :: TestTree
tests =
  testGroup
    "local-fact-laws"
    [ QC.testProperty "mkLocalAddress normalizes support and caches closure" propMkLocalAddressInvariants,
      QC.testProperty "dominates is reflexive" propDominatesReflexive,
      QC.testProperty "dominates is transitive on a nested chain" propDominatesTransitive,
      QC.testProperty "mutual dominance forces canonical identity" propDominatesAntisymmetric,
      QC.testProperty "dominates preserves carrier inclusion" propDominatesCarrierConsistency,
      QC.testProperty "FactAntichain rejects comparable members within a proposition bucket" propAntichainInvariant,
      QC.testProperty "boundary subsumption is reflexive" propBoundarySubsumesReflexive,
      QC.testProperty "boundary subsumption is transitive" propBoundarySubsumesTransitive,
      QC.testProperty "restriction is monotone under subsumption" propBoundaryRestrictionMonotone,
      QC.testProperty "compatibility on overlap is symmetric" propCompatibilitySymmetric,
      QC.testProperty "stronger boundaries do not yield stronger overlap obstructions" propBoundaryObstructionWeakening,
      testCase "compatibleFacts is vacuous on empty carrier intersection" testCompatibleFactsVacuousCarrier,
      testCase "compatibleFacts rejects nonempty carrier boundary disagreement" testCompatibleFactsRejectsBoundaryDisagreement
    ]

data TestContext
  = BottomContext
  | LeftContext
  | RightContext
  | TopContext
  deriving stock (Eq, Ord, Show, Enum, Bounded)

newtype MockBoundary = MockBoundary
  { unMockBoundary :: Set Int
  }
  deriving stock (Eq, Ord, Show)

data StrictBoundary = StrictBoundary !Int
  deriving stock (Eq, Ord, Show)

instance BoundaryOps MockBoundary where
  type BoundaryOverlap MockBoundary = Set Int

  overlapBetweenBoundary (MockBoundary leftBoundary) (MockBoundary rightBoundary) =
    Set.intersection leftBoundary rightBoundary

  restrictBoundaryRaw overlapValue (MockBoundary boundaryValue) =
    MockBoundary (Set.intersection overlapValue boundaryValue)

  compatibleBoundaryRaw leftBoundary rightBoundary =
    if leftBoundary == rightBoundary
      then Right leftBoundary
      else Left (mockBoundaryDifference leftBoundary rightBoundary)

  subsumesBoundaryRaw (MockBoundary leftBoundary) (MockBoundary rightBoundary) =
    Set.isSubsetOf rightBoundary leftBoundary

instance BoundaryOps StrictBoundary where
  type BoundaryOverlap StrictBoundary = ()

  overlapBetweenBoundary _leftBoundary _rightBoundary =
    ()

  restrictBoundaryRaw _overlapValue boundaryValue =
    boundaryValue

  compatibleBoundaryRaw leftBoundary rightBoundary =
    if leftBoundary == rightBoundary
      then Right leftBoundary
      else Left leftBoundary

  subsumesBoundaryRaw =
    (==)

propMkLocalAddressInvariants :: QC.Property
propMkLocalAddressInvariants =
  QC.forAll genSupportBasis $ \supportValue ->
    let address =
          fixtureValue "local address"
            ( mkLocalAddress
                testLattice
                (PropositionKey (0 :: Int))
                supportValue
            )
        minimizedSupport =
          fixtureValue "minimized support"
            (minimizeSupport testLattice supportValue)
        closedCarrier =
          fixtureValue "closed carrier"
            (closure testLattice (laSupport address))
     in laSupport address == minimizedSupport
          && laCarrier address == closedCarrier

propDominatesReflexive :: QC.Property
propDominatesReflexive =
  QC.forAll genFact $ \localFactValue ->
    dominates localFactValue localFactValue

propDominatesTransitive :: QC.Property
propDominatesTransitive =
  let factA = mkFact 0 [TopContext] [1, 2, 3]
      factB = mkFact 0 [LeftContext] [1, 2]
      factC = mkFact 0 [BottomContext] [1]
   in QC.property
        ( dominates factA factB
            && dominates factB factC
            && dominates factA factC
        )

propDominatesAntisymmetric :: QC.Property
propDominatesAntisymmetric =
  QC.withNumTests 100 $
    QC.forAll genDistinctCanonicalFactPair $ \(leftInputContexts, rightInputContexts, leftFact, rightFact) ->
      let mutuallyDominating =
            dominates leftFact rightFact
              && dominates rightFact leftFact
       in leftInputContexts /= rightInputContexts
            && lfEvidence leftFact /= lfEvidence rightFact
            && (not mutuallyDominating || canonicalIdentityMatches leftFact rightFact)

propDominatesCarrierConsistency :: QC.Property
propDominatesCarrierConsistency =
  QC.forAll genFact $ \leftFact ->
    QC.forAll genFact $ \rightFact ->
      not (dominates leftFact rightFact)
        || Set.isSubsetOf
          (carrierContexts (laCarrier (lfAddress leftFact)))
          (carrierContexts (laCarrier (lfAddress rightFact)))

propAntichainInvariant :: QC.Property
propAntichainInvariant =
  QC.forAll (QC.listOf genFact) $ \facts ->
    let antichain =
          foldr insertAntichain emptyFactAntichain facts
        buckets =
          fmap (`lookupByKey` antichain) (distinctKeys (membersAntichain antichain))
     in all bucketIsAntichain buckets

propBoundarySubsumesReflexive :: QC.Property
propBoundarySubsumesReflexive =
  QC.forAll genBoundary $ \boundaryValue ->
    let factValue = mkFactWithBoundary 0 [LeftContext] boundaryValue
        summaryValue = exportBoundary factValue
     in subsumes summaryValue summaryValue

propBoundarySubsumesTransitive :: QC.Property
propBoundarySubsumesTransitive =
  let summaryA = exportBoundary (mkFactWithBoundary 0 [LeftContext] (MockBoundary (Set.fromList [1, 2, 3])))
      summaryB = exportBoundary (mkFactWithBoundary 0 [LeftContext] (MockBoundary (Set.fromList [1, 2])))
      summaryC = exportBoundary (mkFactWithBoundary 0 [LeftContext] (MockBoundary (Set.singleton 1)))
   in QC.property
        ( subsumes summaryA summaryB
            && subsumes summaryB summaryC
            && subsumes summaryA summaryC
        )

propBoundaryRestrictionMonotone :: QC.Property
propBoundaryRestrictionMonotone =
  let stronger = MockBoundary (Set.fromList [1, 2, 3])
      weaker = MockBoundary (Set.fromList [1, 2])
      reference = MockBoundary (Set.fromList [1, 3])
      overlapValue = overlapBetweenBoundary weaker reference
   in QC.property
        ( subsumesBoundaryRaw stronger weaker
            && subsumesBoundaryRaw
              (restrictBoundaryRaw overlapValue stronger)
              (restrictBoundaryRaw overlapValue weaker)
        )

propCompatibilitySymmetric :: QC.Property
propCompatibilitySymmetric =
  QC.forAll genBoundaryPair $ \(leftBoundary, rightBoundary) ->
    let leftFact = mkFactWithBoundary 0 [LeftContext] leftBoundary
        rightFact = mkFactWithBoundary 0 [LeftContext] rightBoundary
        overlapValue = overlapBetween (exportBoundary leftFact) (exportBoundary rightFact)
     in compatibleOnOverlap
          (restrictBoundary overlapValue (exportBoundary leftFact))
          (restrictBoundary overlapValue (exportBoundary rightFact))
          ==
          compatibleOnOverlap
            (restrictBoundary overlapValue (exportBoundary rightFact))
            (restrictBoundary overlapValue (exportBoundary leftFact))

propBoundaryObstructionWeakening :: QC.Property
propBoundaryObstructionWeakening =
  let stronger = MockBoundary (Set.fromList [1, 2, 3])
      weaker = MockBoundary (Set.fromList [1, 2])
      reference = MockBoundary (Set.fromList [2, 4])
      overlapValue = overlapBetweenBoundary stronger reference
      strongerResult = compatibleBoundaryRaw (restrictBoundaryRaw overlapValue stronger) (restrictBoundaryRaw overlapValue reference)
      weakerResult = compatibleBoundaryRaw (restrictBoundaryRaw overlapValue weaker) (restrictBoundaryRaw overlapValue reference)
   in QC.property (obstructionMagnitude strongerResult <= obstructionMagnitude weakerResult)

testCompatibleFactsVacuousCarrier :: Assertion
testCompatibleFactsVacuousCarrier =
  case compatibleFacts emptyCarrierFact leftStrictFact of
    Right (LocalFactNoCarrierOverlap carrierValue) ->
      assertBool "empty carrier intersection must be explicit" (carrierEmpty carrierValue)
    outcome ->
      assertFailure ("expected vacuous compatibility, got " <> show outcome)

testCompatibleFactsRejectsBoundaryDisagreement :: Assertion
testCompatibleFactsRejectsBoundaryDisagreement =
  case compatibleFacts leftStrictFact rightStrictFact of
    Left obstruction -> do
      carrierContexts (lfoObstructedCarrier obstruction) @?= Set.singleton TopContext
    outcome ->
      assertFailure ("expected boundary obstruction on shared carrier, got " <> show outcome)

genFact :: QC.Gen (LocalFact TestContext Int () MockBoundary)
genFact =
  mkFactWithBoundary
    <$> QC.chooseInt (0, 3)
    <*> QC.sublistOf allContexts
    <*> genBoundary

genDistinctCanonicalFactPair ::
  QC.Gen
    ( [TestContext],
      [TestContext],
      LocalFact TestContext Int Int MockBoundary,
      LocalFact TestContext Int Int MockBoundary
    )
genDistinctCanonicalFactPair = do
  propositionKey <- QC.chooseInt (0, 3)
  boundaryValue <- genBoundary
  (leftInputContexts, rightInputContexts) <- QC.elements equivalentSupportInputs
  (leftEvidence, rightEvidence) <-
    (,)
      <$> QC.chooseInt (0, 1024)
      <*> QC.chooseInt (1025, 2048)
  pure
    ( leftInputContexts,
      rightInputContexts,
      mkFactWithEvidence propositionKey leftInputContexts boundaryValue leftEvidence,
      mkFactWithEvidence propositionKey rightInputContexts boundaryValue rightEvidence
    )

genBoundary :: QC.Gen MockBoundary
genBoundary =
  fmap
    (MockBoundary . Set.fromList)
    (QC.sublistOf [1 :: Int, 2, 3, 4])

genBoundaryPair :: QC.Gen (MockBoundary, MockBoundary)
genBoundaryPair =
  (,)
    <$> genBoundary
    <*> genBoundary

genSupportBasis :: QC.Gen (SupportBasis TestContext)
genSupportBasis =
  fmap supportBasisFromContexts (QC.sublistOf allContexts)

mkFact :: Int -> [TestContext] -> [Int] -> LocalFact TestContext Int () MockBoundary
mkFact propositionKey contexts boundaryEntries =
  mkFactWithBoundary propositionKey contexts (MockBoundary (Set.fromList boundaryEntries))

mkFactWithBoundary :: Int -> [TestContext] -> MockBoundary -> LocalFact TestContext Int () MockBoundary
mkFactWithBoundary propositionKey contexts boundaryValue =
  mkFactWithEvidence propositionKey contexts boundaryValue ()

mkFactWithEvidence ::
  Int ->
  [TestContext] ->
  MockBoundary ->
  evidence ->
  LocalFact TestContext Int evidence MockBoundary
mkFactWithEvidence propositionKey contexts =
  mkLocalFact
    ( fixtureValue "local fact address"
        ( mkLocalAddress
            testLattice
            (PropositionKey propositionKey)
            (supportBasisFromContexts contexts)
        )
    )

mkStrictFact :: [TestContext] -> StrictBoundary -> LocalFact TestContext Int () StrictBoundary
mkStrictFact contexts boundaryValue =
  mkLocalFact
    ( fixtureValue "strict local fact address"
        ( mkLocalAddress
            testLattice
            (PropositionKey 0)
            (supportBasisFromContexts contexts)
        )
    )
    boundaryValue
    ()

emptyCarrierFact :: LocalFact TestContext Int () StrictBoundary
emptyCarrierFact =
  mkStrictFact [] (StrictBoundary 1)

leftStrictFact :: LocalFact TestContext Int () StrictBoundary
leftStrictFact =
  mkStrictFact [LeftContext] (StrictBoundary 1)

rightStrictFact :: LocalFact TestContext Int () StrictBoundary
rightStrictFact =
  mkStrictFact [RightContext] (StrictBoundary 2)

supportBasisFromContexts :: [TestContext] -> SupportBasis TestContext
supportBasisFromContexts =
  fixtureValue "local fact fixture support"
    . supportBasis testLattice

fixtureValue :: Show err => String -> Either err value -> value
fixtureValue label =
  either (error . ((label <> ": ") <>) . show) id

distinctKeys :: [LocalFact TestContext Int () MockBoundary] -> [PropositionKey Int]
distinctKeys =
  Set.toList . Set.fromList . fmap (laProp . lfAddress)

bucketIsAntichain :: [LocalFact TestContext Int () MockBoundary] -> Bool
bucketIsAntichain facts =
  and
    [ not (dominates leftFact rightFact) && not (dominates rightFact leftFact)
    | leftFact <- facts,
      rightFact <- facts,
      leftFact /= rightFact
    ]

mockBoundaryDifference :: MockBoundary -> MockBoundary -> MockBoundary
mockBoundaryDifference (MockBoundary leftBoundary) (MockBoundary rightBoundary) =
  MockBoundary
    (Set.union (Set.difference leftBoundary rightBoundary) (Set.difference rightBoundary leftBoundary))

obstructionMagnitude :: Either MockBoundary MockBoundary -> Int
obstructionMagnitude =
  either (Set.size . unMockBoundary) (const 0)

canonicalIdentityMatches ::
  LocalFact TestContext Int leftEvidence MockBoundary ->
  LocalFact TestContext Int rightEvidence MockBoundary ->
  Bool
canonicalIdentityMatches leftFact rightFact =
  let leftAddress = lfAddress leftFact
      rightAddress = lfAddress rightFact
      leftBoundary = exportBoundary leftFact
      rightBoundary = exportBoundary rightFact
   in laProp leftAddress == laProp rightAddress
        && laSupport leftAddress == laSupport rightAddress
        && laCarrier leftAddress == laCarrier rightAddress
        && subsumes leftBoundary rightBoundary
        && subsumes rightBoundary leftBoundary

equivalentSupportInputs :: [([TestContext], [TestContext])]
equivalentSupportInputs =
  [ ([LeftContext], [BottomContext, LeftContext]),
    ([BottomContext, LeftContext], [LeftContext]),
    ([RightContext], [BottomContext, RightContext]),
    ([BottomContext, RightContext], [RightContext]),
    ([TopContext], [BottomContext, TopContext]),
    ([BottomContext, TopContext], [TopContext]),
    ([TopContext], [LeftContext, TopContext]),
    ([LeftContext, TopContext], [TopContext]),
    ([TopContext], [RightContext, TopContext]),
    ([RightContext, TopContext], [TopContext])
  ]

allContexts :: [TestContext]
allContexts =
  [BottomContext, LeftContext, RightContext, TopContext]

testLattice :: ContextLattice TestContext
testLattice =
  either
    (error . ("invalid local fact fixture lattice: " <>) . show)
    id
    ( compileContextLattice
        (Set.fromList allContexts)
        ( contextOrderDecl
            TopContext
            BottomContext
            [ (BottomContext, LeftContext),
              (BottomContext, RightContext),
              (LeftContext, TopContext),
              (RightContext, TopContext)
            ]
        )
    )

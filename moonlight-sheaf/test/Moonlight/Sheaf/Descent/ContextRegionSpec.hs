{-# LANGUAGE RankNTypes #-}

module Moonlight.Sheaf.Descent.ContextRegionSpec
  ( tests,
  )
where

import Data.Bits (bit, testBit, (.&.), (.|.))
import Data.List (sort, subsequences)
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.FiniteLattice
  ( ContextLattice,
    ContextLatticeCompileError,
    compileContextLattice,
    contextOrderDecl,
    joinContext,
    leqContext,
    meetContext,
    supportBasis,
  )
import Moonlight.Sheaf.Context.Region
  ( ContextRegion,
    RegionTable,
    fromGeneratorKeys,
    regionAtKey,
    regionComplementIn,
    regionCubeCount,
    regionDifference,
    regionEmpty,
    regionEntails,
    regionFromKeys,
    regionGeneratorKeys,
    regionIsDownClosed,
    regionIsOpen,
    regionJoin,
    regionKeys,
    regionMeet,
    regionMemberKey,
    regionSize,
    regionTop,
    regionVoid,
  )
import Moonlight.Sheaf.Context.Site
  ( ContextObjectKey,
    PreparedContextSite,
    SupportCarrier,
    contextFragmentLattice,
    contextFragmentObjects,
    contextObjectKeyFor,
    contextObjectKeyValue,
    preparedContextAtKey,
    preparedContextFragment,
    preparedRegionAt,
    preparedRegionTable,
    supportCarrierFromSupport,
    supportCarrierMeet,
    supportCarrierRegion,
    supportCarrierUnion,
    withPreparedContextSiteFromFiniteLattice,
    withPreparedContextSiteFromPowersetAtoms,
  )
import Moonlight.Sheaf.TestFixture.Branch
  ( branchContextLattice,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertFailure,
    testCase,
    (@?=),
  )

tests :: TestTree
tests =
  case anatomyLatticeResult of
    Left latticeError ->
      testGroup
        "context region algebra"
        [ testCase "anatomy fixture lattice compiles" $
            assertFailure ("invalid anatomy fixture lattice: " <> show latticeError)
        ]
    Right anatomyLattice ->
      testGroup
        "context region algebra"
        [ testGroup
            "anatomy site (non-distributive)"
            (withPreparedContextSiteFromFiniteLattice anatomyLattice siteLaws),
          testGroup
            "branch site"
            (withPreparedContextSiteFromFiniteLattice branchContextLattice siteLaws),
          testCase
            "anatomy lattice is genuinely non-distributive"
            (testAnatomyNonDistributive anatomyLattice),
          testGroup
            "powerset table (symbolic arm, n=3)"
            symbolicPowersetLawTrees,
          testCase
            "symbolic difference stays compact at twenty atoms"
            testSymbolicDifferenceTwentyAtoms,
          testGroup
            "cross-arm bitset oracle (n=3, exhaustive)"
            crossArmOracleLaws
        ]

siteLaws :: (Ord c, Show c) => PreparedContextSite owner c -> [TestTree]
siteLaws site =
  [ testCase "principal regions are open" (testPrincipalRegionsOpen site),
    testCase "annotation law: region of a join is the meet of regions" (testAnnotationLaw site),
    testCase "meets and joins of opens stay open (frame closure)" (testFrameClosure site),
    testCase "entailment mirrors the lattice order" (testEntailmentMirrorsOrder site),
    testCase "complements of opens are down-closed and involutive" (testComplementLaws site),
    testCase "difference preserves the disjoint residual" (testDifferenceLaws site),
    testCase "generator antichain round-trips every generated open" (testGeneratorRoundTrip site),
    testCase "region meet agrees with the support carrier meet" (testCarrierMeetAgreement site),
    testCase "region join agrees with the support carrier union" (testCarrierUnionAgreement site),
    testCase "meet is void exactly when no upper bound is shared" (testMeetEmptiness site)
  ]

data AnatomyContext
  = Whole
  | Upper
  | Lower
  | Head
  | Torso
  | ArmLeft
  | ArmRight
  | LegLeft
  | LegRight
  | Local
  deriving stock (Eq, Ord, Show, Enum, Bounded)

anatomyLatticeResult :: Either (ContextLatticeCompileError AnatomyContext) (ContextLattice AnatomyContext)
anatomyLatticeResult =
  compileContextLattice
    (Set.fromList [minBound .. maxBound])
    ( contextOrderDecl
        Local
        Whole
        [ (Whole, Upper),
          (Whole, Lower),
          (Upper, Head),
          (Upper, Torso),
          (Upper, ArmLeft),
          (Upper, ArmRight),
          (Lower, LegLeft),
          (Lower, LegRight),
          (Head, Local),
          (Torso, Local),
          (ArmLeft, Local),
          (ArmRight, Local),
          (LegLeft, Local),
          (LegRight, Local)
        ]
    )

requireFixture :: Show errorValue => String -> Either errorValue value -> value
requireFixture label =
  either (error . ((label <> ": ") <>) . show) id

siteFragmentObjects :: PreparedContextSite owner c -> [c]
siteFragmentObjects =
  contextFragmentObjects . preparedContextFragment

siteFragmentLattice :: PreparedContextSite owner c -> ContextLattice c
siteFragmentLattice =
  contextFragmentLattice . preparedContextFragment

requireRegion ::
  (Ord c, Show c) =>
  PreparedContextSite owner c ->
  c ->
  ContextRegion owner
requireRegion site contextValue =
  requireFixture "principal region unavailable" (preparedRegionAt site contextValue)

generatedOpens ::
  (Ord c, Show c) =>
  PreparedContextSite owner c ->
  [ContextRegion owner]
generatedOpens site =
  [ regionJoin (requireRegion site left) (requireRegion site right)
    | left <- siteFragmentObjects site,
      right <- siteFragmentObjects site
  ]

carrierBasis ::
  (Ord c, Show c) =>
  PreparedContextSite owner c ->
  [SupportCarrier owner c]
carrierBasis site =
  [ requireFixture
      "support carrier fixture"
      ( supportCarrierFromSupport
          site
          (requireFixture "support basis fixture" (supportBasis (siteFragmentLattice site) contexts))
      )
    | contexts <- generatorSelections
  ]
  where
    objects = siteFragmentObjects site
    generatorSelections =
      fmap pure objects
        <> [[left, right] | left <- objects, right <- objects, left /= right]

testPrincipalRegionsOpen ::
  (Ord c, Show c) =>
  PreparedContextSite owner c ->
  Assertion
testPrincipalRegionsOpen site =
  assertBool
    "every principal region must be up-closed"
    (all (regionIsOpen (preparedRegionTable site) . requireRegion site) (siteFragmentObjects site))

testAnnotationLaw ::
  (Ord c, Show c) =>
  PreparedContextSite owner c ->
  Assertion
testAnnotationLaw site =
  sequence_
    [ case joinContext (siteFragmentLattice site) left right of
        Left lookupError ->
          assertFailure ("join lookup failed: " <> show lookupError)
        Right joined ->
          requireRegion site joined
            @?= regionMeet (requireRegion site left) (requireRegion site right)
      | left <- siteFragmentObjects site,
        right <- siteFragmentObjects site
    ]

testFrameClosure ::
  (Ord c, Show c) =>
  PreparedContextSite owner c ->
  Assertion
testFrameClosure site =
  assertBool
    "meets and joins of principal opens must remain open"
    ( and
        [ regionIsOpen table (regionMeet left right)
            && regionIsOpen table (regionJoin left right)
          | left <- principals,
            right <- principals
        ]
    )
  where
    table = preparedRegionTable site
    principals = fmap (requireRegion site) (siteFragmentObjects site)

testEntailmentMirrorsOrder ::
  (Ord c, Show c) =>
  PreparedContextSite owner c ->
  Assertion
testEntailmentMirrorsOrder site =
  sequence_
    [ leqContext (siteFragmentLattice site) left right
        @?= Right (regionEntails (requireRegion site right) (requireRegion site left))
      | left <- siteFragmentObjects site,
        right <- siteFragmentObjects site
    ]

testComplementLaws ::
  (Ord c, Show c) =>
  PreparedContextSite owner c ->
  Assertion
testComplementLaws site =
  assertBool
    "complements of opens must be down-closed and involutive"
    ( and
        [ regionIsDownClosed table complementRegion
            && regionComplementIn table complementRegion == open
          | open <- generatedOpens site,
            let complementRegion = regionComplementIn table open
        ]
    )
  where
    table = preparedRegionTable site

testDifferenceLaws ::
  (Ord c, Show c) =>
  PreparedContextSite owner c ->
  Assertion
testDifferenceLaws site =
  assertBool
    "difference must be disjoint from the removed region and recompose its whole"
    ( and
        [ regionEmpty (regionMeet residual removed)
            && sameRegion residualWhole whole
            && regionEntails residual whole
          | whole <- generatedOpens site,
            removed <- generatedOpens site,
            let residual = regionDifference table whole removed
                residualWhole = regionJoin residual (regionMeet whole removed)
        ]
    )
  where
    table = preparedRegionTable site
    sameRegion :: ContextRegion owner -> ContextRegion owner -> Bool
    sameRegion left right =
      regionEntails left right && regionEntails right left

testGeneratorRoundTrip ::
  (Ord c, Show c) =>
  PreparedContextSite owner c ->
  Assertion
testGeneratorRoundTrip site =
  sequence_
    [ fromGeneratorKeys table (regionGeneratorKeys table open) @?= open
      | open <- generatedOpens site
    ]
  where
    table = preparedRegionTable site

testCarrierMeetAgreement ::
  (Ord c, Show c) =>
  PreparedContextSite owner c ->
  Assertion
testCarrierMeetAgreement site =
  sequence_
    [ supportCarrierRegion site (supportCarrierMeet site leftCarrier rightCarrier)
        @?= regionMeet
          (supportCarrierRegion site leftCarrier)
          (supportCarrierRegion site rightCarrier)
      | leftCarrier <- carrierBasis site,
        rightCarrier <- carrierBasis site
    ]

testCarrierUnionAgreement ::
  (Ord c, Show c) =>
  PreparedContextSite owner c ->
  Assertion
testCarrierUnionAgreement site =
  sequence_
    [ supportCarrierRegion site (supportCarrierUnion site leftCarrier rightCarrier)
        @?= regionJoin
          (supportCarrierRegion site leftCarrier)
          (supportCarrierRegion site rightCarrier)
      | leftCarrier <- carrierBasis site,
        rightCarrier <- carrierBasis site
    ]

testMeetEmptiness ::
  (Ord c, Show c) =>
  PreparedContextSite owner c ->
  Assertion
testMeetEmptiness site =
  sequence_
    [ regionEmpty (regionMeet (requireRegion site left) (requireRegion site right))
        @?= not (sharesUpperBound left right)
      | left <- siteFragmentObjects site,
        right <- siteFragmentObjects site
    ]
  where
    sharesUpperBound left right =
      any
        ( \candidate ->
            leqContext (siteFragmentLattice site) left candidate == Right True
              && leqContext (siteFragmentLattice site) right candidate == Right True
        )
        (siteFragmentObjects site)

testAnatomyNonDistributive :: ContextLattice AnatomyContext -> Assertion
testAnatomyNonDistributive anatomyLattice =
  assertBool
    "expected at least one distributivity violation"
    ( or
        [ (meetContext anatomyLattice a =<< joinContext anatomyLattice b c)
            /= ( do
                   leftMeet <- meetContext anatomyLattice a b
                   rightMeet <- meetContext anatomyLattice a c
                   joinContext anatomyLattice leftMeet rightMeet
               )
          | a <- [minBound .. maxBound],
            b <- [minBound .. maxBound],
            c <- [minBound .. maxBound]
        ]
    )

powersetAtomCount :: Int
powersetAtomCount = 3

powersetMasks :: Int -> [Int]
powersetMasks atomCount =
  [0 .. bit atomCount - 1]

maskContext :: Int -> Int -> Set Int
maskContext atomCount maskValue =
  Set.fromList
    [ atomIndex
      | atomIndex <- [0 .. atomCount - 1],
        testBit maskValue atomIndex
    ]

keyForMask ::
  PreparedContextSite owner (Set Int) ->
  Int ->
  Int ->
  ContextObjectKey owner
keyForMask site atomCount maskValue =
  requireFixture
    "powerset object key"
    (contextObjectKeyFor site (maskContext atomCount maskValue))

keyValues :: [ContextObjectKey owner] -> [Int]
keyValues =
  fmap contextObjectKeyValue

data PowersetRegionExpr
  = PrincipalAt Int
  | ExprJoin PowersetRegionExpr PowersetRegionExpr
  | ExprMeet PowersetRegionExpr PowersetRegionExpr
  | ExprComplement PowersetRegionExpr
  | ExprDifference PowersetRegionExpr PowersetRegionExpr

evalRegionExpr ::
  PreparedContextSite owner (Set Int) ->
  Int ->
  PowersetRegionExpr ->
  ContextRegion owner
evalRegionExpr site atomCount expr =
  case expr of
    PrincipalAt maskValue -> regionAtKey table (keyForMask site atomCount maskValue)
    ExprJoin left right -> regionJoin (eval left) (eval right)
    ExprMeet left right -> regionMeet (eval left) (eval right)
    ExprComplement inner -> regionComplementIn table (eval inner)
    ExprDifference whole removed ->
      regionDifference table (eval whole) (eval removed)
  where
    table = preparedRegionTable site
    eval = evalRegionExpr site atomCount

openPowersetExprs :: Int -> [PowersetRegionExpr]
openPowersetExprs atomCount =
  fmap PrincipalAt masks
    <> [ ExprJoin (PrincipalAt left) (PrincipalAt right)
         | left <- masks,
           right <- masks
       ]
  where
    masks = powersetMasks atomCount

mixedPowersetExprs :: Int -> [PowersetRegionExpr]
mixedPowersetExprs atomCount =
  opens
    <> fmap ExprComplement opens
    <> [ExprMeet open (ExprComplement other) | open <- opens, other <- opens]
    <> [ExprDifference whole removed | whole <- opens, removed <- opens]
  where
    opens = openPowersetExprs atomCount

symbolicPowersetLawTrees :: [TestTree]
symbolicPowersetLawTrees =
  case
    withPreparedContextSiteFromPowersetAtoms
      [0 .. powersetAtomCount - 1]
      (symbolicPowersetLaws powersetAtomCount)
    of
      Left siteError ->
        [ testCase "symbolic powerset fixture prepares" $
            assertFailure ("invalid symbolic powerset fixture: " <> show siteError)
        ]
      Right lawTrees ->
        lawTrees

symbolicPowersetLaws ::
  Int ->
  PreparedContextSite owner (Set Int) ->
  [TestTree]
symbolicPowersetLaws atomCount site =
  [ testCase "principal regions are open" (testSymbolicPrincipalsOpen atomCount site),
    testCase "annotation law: region of a join is the meet of regions" (testSymbolicAnnotationLaw atomCount site),
    testCase "meets and joins of opens stay open (frame closure)" (testSymbolicFrameClosure atomCount site),
    testCase "entailment mirrors the mask order" (testSymbolicEntailmentMirrorsOrder atomCount site),
    testCase "complements of opens are down-closed and involutive" (testSymbolicComplementLaws atomCount site),
    testCase "difference preserves the disjoint residual without enumeration" (testSymbolicDifferenceLaws atomCount site),
    testCase "generator antichain round-trips every generated open" (testSymbolicGeneratorRoundTrip atomCount site),
    testCase "meet is void exactly against the complement" (testSymbolicMeetEmptiness atomCount site),
    testCase "size agrees with enumerated keys on every expression" (testSymbolicSizeLaw atomCount site),
    testCase "top and void are complementary" (testSymbolicTopVoidDuality site)
  ]

symbolicOpens ::
  Int ->
  PreparedContextSite owner (Set Int) ->
  [ContextRegion owner]
symbolicOpens atomCount site =
  fmap (evalRegionExpr site atomCount) (openPowersetExprs atomCount)

sameRegionKeys ::
  RegionTable owner ->
  ContextRegion owner ->
  ContextRegion owner ->
  Bool
sameRegionKeys table leftRegion rightRegion =
  keyValues (regionKeys table leftRegion) == keyValues (regionKeys table rightRegion)

testSymbolicPrincipalsOpen ::
  Int ->
  PreparedContextSite owner (Set Int) ->
  Assertion
testSymbolicPrincipalsOpen atomCount site =
  assertBool
    "every principal region must be up-closed"
    ( all
        (regionIsOpen table . regionAtKey table . keyForMask site atomCount)
        (powersetMasks atomCount)
    )
  where
    table = preparedRegionTable site

testSymbolicAnnotationLaw ::
  Int ->
  PreparedContextSite owner (Set Int) ->
  Assertion
testSymbolicAnnotationLaw atomCount site =
  sequence_
    [ regionAtKey table (keyForMask site atomCount (left .|. right))
        @?= regionMeet
          (regionAtKey table (keyForMask site atomCount left))
          (regionAtKey table (keyForMask site atomCount right))
      | left <- powersetMasks atomCount,
        right <- powersetMasks atomCount
    ]
  where
    table = preparedRegionTable site

testSymbolicFrameClosure ::
  Int ->
  PreparedContextSite owner (Set Int) ->
  Assertion
testSymbolicFrameClosure atomCount site =
  assertBool
    "meets and joins of principal opens must remain open"
    ( and
        [ regionIsOpen table (regionMeet left right)
            && regionIsOpen table (regionJoin left right)
          | left <- principals,
            right <- principals
        ]
    )
  where
    table = preparedRegionTable site
    principals = fmap (regionAtKey table . keyForMask site atomCount) (powersetMasks atomCount)

testSymbolicEntailmentMirrorsOrder ::
  Int ->
  PreparedContextSite owner (Set Int) ->
  Assertion
testSymbolicEntailmentMirrorsOrder atomCount site =
  sequence_
    [ (left .&. right == left)
        @?= regionEntails
          (regionAtKey table (keyForMask site atomCount right))
          (regionAtKey table (keyForMask site atomCount left))
      | left <- powersetMasks atomCount,
        right <- powersetMasks atomCount
    ]
  where
    table = preparedRegionTable site

testSymbolicComplementLaws ::
  Int ->
  PreparedContextSite owner (Set Int) ->
  Assertion
testSymbolicComplementLaws atomCount site =
  assertBool
    "complements of opens must be down-closed and involutive"
    ( and
        [ regionIsDownClosed table complementRegion
            && sameRegionKeys table (regionComplementIn table complementRegion) open
          | open <- symbolicOpens atomCount site,
            let complementRegion = regionComplementIn table open
        ]
    )
  where
    table = preparedRegionTable site

testSymbolicDifferenceLaws ::
  Int ->
  PreparedContextSite owner (Set Int) ->
  Assertion
testSymbolicDifferenceLaws atomCount site =
  assertBool
    "symbolic difference must be disjoint from the removed region and recompose its whole"
    ( and
        [ regionEmpty (regionMeet residual removed)
            && sameRegionKeys table (regionJoin residual (regionMeet whole removed)) whole
          | whole <- symbolicOpens atomCount site,
            removed <- symbolicOpens atomCount site,
            let residual = regionDifference table whole removed
        ]
    )
  where
    table = preparedRegionTable site

testSymbolicDifferenceTwentyAtoms :: Assertion
testSymbolicDifferenceTwentyAtoms =
  case withPreparedContextSiteFromPowersetAtoms [0 .. 19] runLaw of
    Left siteError ->
      assertFailure ("invalid twenty-atom powerset fixture: " <> show siteError)
    Right assertion ->
      assertion
  where
    runLaw :: PreparedContextSite owner (Set Int) -> Assertion
    runLaw site =
      let table = preparedRegionTable site
          atomZero = keyForMask site 20 (bit 0)
          atomZeroOne = keyForMask site 20 (bit 0 .|. bit 1)
          atomZeroTwo = keyForMask site 20 (bit 0 .|. bit 2)
          residual =
            regionDifference
              table
              (regionAtKey table atomZero)
              (regionAtKey table atomZeroOne)
       in assertBool
            "subtracting one principal region must retain one symbolic residual cube"
            ( regionCubeCount table residual == 1
                && regionSize residual == bit 18
                && regionMemberKey residual atomZero
                && regionMemberKey residual atomZeroTwo
                && not (regionMemberKey residual atomZeroOne)
            )

testSymbolicGeneratorRoundTrip ::
  Int ->
  PreparedContextSite owner (Set Int) ->
  Assertion
testSymbolicGeneratorRoundTrip atomCount site =
  sequence_
    [ fromGeneratorKeys table (regionGeneratorKeys table open) @?= open
      | open <- symbolicOpens atomCount site
    ]
  where
    table = preparedRegionTable site

testSymbolicMeetEmptiness ::
  Int ->
  PreparedContextSite owner (Set Int) ->
  Assertion
testSymbolicMeetEmptiness atomCount site =
  assertBool
    "principal meets share the top; opens meet their complements in void"
    ( and
        [ not
            ( regionEmpty
                ( regionMeet
                    (regionAtKey table (keyForMask site atomCount left))
                    (regionAtKey table (keyForMask site atomCount right))
                )
            )
          | left <- powersetMasks atomCount,
            right <- powersetMasks atomCount
        ]
        && all
          (\open -> regionEmpty (regionMeet open (regionComplementIn table open)))
          (symbolicOpens atomCount site)
    )
  where
    table = preparedRegionTable site

testSymbolicSizeLaw ::
  Int ->
  PreparedContextSite owner (Set Int) ->
  Assertion
testSymbolicSizeLaw atomCount site =
  sequence_
    [ regionSize region @?= length (regionKeys table region)
      | expr <- mixedPowersetExprs atomCount,
        let region = evalRegionExpr site atomCount expr
    ]
  where
    table = preparedRegionTable site

testSymbolicTopVoidDuality ::
  PreparedContextSite owner (Set Int) ->
  Assertion
testSymbolicTopVoidDuality site =
  assertBool
    "complement exchanges top and void"
    ( regionEmpty (regionComplementIn table (regionTop table))
        && sameRegionKeys table (regionComplementIn table regionVoid) (regionTop table)
    )
  where
    table = preparedRegionTable site

powersetLattice ::
  Int ->
  Either (ContextLatticeCompileError (Set Int)) (ContextLattice (Set Int))
powersetLattice atomCount =
  compileContextLattice
    (Set.fromList contexts)
    ( contextOrderDecl
        (Set.fromList atoms)
        Set.empty
        [ (contextValue, Set.insert atomValue contextValue)
          | contextValue <- contexts,
            atomValue <- atoms,
            not (Set.member atomValue contextValue)
        ]
    )
  where
    atoms = [0 .. atomCount - 1]
    contexts = fmap Set.fromList (subsequences atoms)

crossArmOracleLaws :: [TestTree]
crossArmOracleLaws =
  case powersetLattice powersetAtomCount of
    Left latticeError ->
      [ testCase "dense powerset fixture compiles" $
          assertFailure ("invalid dense powerset fixture: " <> show latticeError)
      ]
    Right lattice ->
      withPreparedContextSiteFromFiniteLattice lattice $ \denseSite ->
        case
          withPreparedContextSiteFromPowersetAtoms
            [0 .. powersetAtomCount - 1]
            (crossArmLawsFor powersetAtomCount denseSite)
          of
            Left siteError ->
              [ testCase "symbolic powerset fixture prepares" $
                  assertFailure ("invalid symbolic powerset fixture: " <> show siteError)
              ]
            Right lawTrees ->
              lawTrees

crossArmLawsFor ::
  Int ->
  PreparedContextSite denseOwner (Set Int) ->
  PreparedContextSite symbolicOwner (Set Int) ->
  [TestTree]
crossArmLawsFor atomCount denseSite symbolicSite =
  [ testCase "membership decode agrees on every expression" (testOracleMembership atomCount denseSite symbolicSite),
    testCase "keys, size, emptiness, and generators agree on every expression" (testOracleProjections atomCount denseSite symbolicSite),
    testCase "openness and down-closure verdicts agree on every expression" (testOracleClosureVerdicts atomCount denseSite symbolicSite),
    testCase "entailment agrees across all open pairs" (testOracleEntailment atomCount denseSite symbolicSite),
    testCase "meet and join descend independently and agree" (testOracleOperations atomCount denseSite symbolicSite),
    testCase "raw key sets meet symbolic opens exactly" (testOracleRawKeyPromotion atomCount denseSite symbolicSite)
  ]

pairedOracleRegions ::
  Int ->
  PreparedContextSite denseOwner (Set Int) ->
  PreparedContextSite symbolicOwner (Set Int) ->
  [(ContextRegion denseOwner, ContextRegion symbolicOwner)]
pairedOracleRegions atomCount denseSite symbolicSite =
  [ (evalRegionExpr denseSite atomCount expr, evalRegionExpr symbolicSite atomCount expr)
    | expr <- mixedPowersetExprs atomCount
  ]

pairedOracleOpens ::
  Int ->
  PreparedContextSite denseOwner (Set Int) ->
  PreparedContextSite symbolicOwner (Set Int) ->
  [(ContextRegion denseOwner, ContextRegion symbolicOwner)]
pairedOracleOpens atomCount denseSite symbolicSite =
  [ (evalRegionExpr denseSite atomCount expr, evalRegionExpr symbolicSite atomCount expr)
    | expr <- openPowersetExprs atomCount
  ]

contextMask :: Set Int -> Int
contextMask =
  Set.foldl' (\maskValue atomIndex -> maskValue .|. bit atomIndex) 0

decodedRegionMasks ::
  PreparedContextSite owner (Set Int) ->
  ContextRegion owner ->
  [Int]
decodedRegionMasks site region =
  sort
    [ contextMask
        ( maybe
            (error ("prepared region returned unknown key " <> show (contextObjectKeyValue objectKey)))
            id
            (preparedContextAtKey site objectKey)
        )
      | objectKey <- regionKeys (preparedRegionTable site) region
    ]

decodedGeneratorMasks ::
  PreparedContextSite owner (Set Int) ->
  ContextRegion owner ->
  [Int]
decodedGeneratorMasks site region =
  sort
    [ contextMask
        ( maybe
            (error ("prepared region returned unknown generator " <> show (contextObjectKeyValue objectKey)))
            id
            (preparedContextAtKey site objectKey)
        )
      | objectKey <- regionGeneratorKeys (preparedRegionTable site) region
    ]

sameDecodedKeys ::
  PreparedContextSite denseOwner (Set Int) ->
  ContextRegion denseOwner ->
  PreparedContextSite symbolicOwner (Set Int) ->
  ContextRegion symbolicOwner ->
  Bool
sameDecodedKeys denseSite denseRegion symbolicSite symbolicRegion =
  decodedRegionMasks denseSite denseRegion
    == decodedRegionMasks symbolicSite symbolicRegion

testOracleMembership ::
  Int ->
  PreparedContextSite denseOwner (Set Int) ->
  PreparedContextSite symbolicOwner (Set Int) ->
  Assertion
testOracleMembership atomCount denseSite symbolicSite =
  assertBool
    "dense and symbolic arms must decode identically"
    ( and
        [ regionMemberKey denseRegion (keyForMask denseSite atomCount keyValue)
            == regionMemberKey symbolicRegion (keyForMask symbolicSite atomCount keyValue)
          | (denseRegion, symbolicRegion) <- pairedOracleRegions atomCount denseSite symbolicSite,
            keyValue <- powersetMasks atomCount
        ]
    )

testOracleProjections ::
  Int ->
  PreparedContextSite denseOwner (Set Int) ->
  PreparedContextSite symbolicOwner (Set Int) ->
  Assertion
testOracleProjections atomCount denseSite symbolicSite =
  sequence_
    ( concat
        [ [ decodedRegionMasks denseSite denseRegion
              @?= decodedRegionMasks symbolicSite symbolicRegion,
            regionSize denseRegion @?= regionSize symbolicRegion,
            regionEmpty denseRegion @?= regionEmpty symbolicRegion,
            decodedGeneratorMasks denseSite denseRegion
              @?= decodedGeneratorMasks symbolicSite symbolicRegion
          ]
          | (denseRegion, symbolicRegion) <- pairedOracleRegions atomCount denseSite symbolicSite
        ]
    )

testOracleClosureVerdicts ::
  Int ->
  PreparedContextSite denseOwner (Set Int) ->
  PreparedContextSite symbolicOwner (Set Int) ->
  Assertion
testOracleClosureVerdicts atomCount denseSite symbolicSite =
  sequence_
    ( concat
        [ [ regionIsOpen denseTable denseRegion
              @?= regionIsOpen symbolicTable symbolicRegion,
            regionIsDownClosed denseTable denseRegion
              @?= regionIsDownClosed symbolicTable symbolicRegion
          ]
          | (denseRegion, symbolicRegion) <- pairedOracleRegions atomCount denseSite symbolicSite
        ]
    )
  where
    denseTable = preparedRegionTable denseSite
    symbolicTable = preparedRegionTable symbolicSite

testOracleEntailment ::
  Int ->
  PreparedContextSite denseOwner (Set Int) ->
  PreparedContextSite symbolicOwner (Set Int) ->
  Assertion
testOracleEntailment atomCount denseSite symbolicSite =
  sequence_
    [ regionEntails denseLeft denseRight
        @?= regionEntails symbolicLeft symbolicRight
      | (denseLeft, symbolicLeft) <- opens,
        (denseRight, symbolicRight) <- opens
    ]
  where
    opens = pairedOracleOpens atomCount denseSite symbolicSite

testOracleOperations ::
  Int ->
  PreparedContextSite denseOwner (Set Int) ->
  PreparedContextSite symbolicOwner (Set Int) ->
  Assertion
testOracleOperations atomCount denseSite symbolicSite =
  assertBool
    "owner-local meet and join operations must agree after decoding"
    ( and
        [ sameDecodedKeys denseSite (regionMeet denseLeft denseRight) symbolicSite (regionMeet symbolicLeft symbolicRight)
            && sameDecodedKeys denseSite (regionJoin denseLeft denseRight) symbolicSite (regionJoin symbolicLeft symbolicRight)
          | (denseLeft, symbolicLeft) <- opens,
            (denseRight, symbolicRight) <- opens
        ]
    )
  where
    opens = pairedOracleOpens atomCount denseSite symbolicSite

testOracleRawKeyPromotion ::
  Int ->
  PreparedContextSite denseOwner (Set Int) ->
  PreparedContextSite symbolicOwner (Set Int) ->
  Assertion
testOracleRawKeyPromotion atomCount denseSite symbolicSite =
  assertBool
    "raw key sets must meet symbolic opens exactly at their common keys"
    ( and
        [ sameDecodedKeys
            denseSite
            (regionMeet denseRawRegion denseOpen)
            symbolicSite
            (regionMeet symbolicRawRegion symbolicOpen)
          | maskSubset <- subsequences (powersetMasks atomCount),
            let denseRawRegion =
                  regionFromKeys denseTable (fmap (keyForMask denseSite atomCount) maskSubset),
            let symbolicRawRegion =
                  regionFromKeys symbolicTable (fmap (keyForMask symbolicSite atomCount) maskSubset),
            (denseOpen, symbolicOpen) <- pairedOracleOpens atomCount denseSite symbolicSite
        ]
    )
  where
    denseTable = preparedRegionTable denseSite
    symbolicTable = preparedRegionTable symbolicSite

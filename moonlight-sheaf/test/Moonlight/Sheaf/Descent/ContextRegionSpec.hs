module Moonlight.Sheaf.Descent.ContextRegionSpec
  ( tests,
  )
where

import Data.Bits (bit, (.&.), (.|.))
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List (subsequences)
import Data.Set qualified as Set
import Moonlight.FiniteLattice
  ( ContextLattice,
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
    powersetRegionTable,
    regionAtKey,
    regionComplementIn,
    regionCubeCount,
    regionDifference,
    regionEmpty,
    regionEntails,
    fromGeneratorKeys,
    regionFromKeys,
    regionGeneratorKeys,
    regionIsDownClosed,
    regionIsOpen,
    regionJoin,
    regionKeys,
    regionMeet,
    regionMemberKey,
    regionSize,
    regionTableFromUpsets,
    regionTop,
    regionVoid,
  )
import Moonlight.Sheaf.Context.Site
  ( PreparedContextSite,
    SupportCarrier,
    preparedContextLattice,
    preparedContextObjects,
    fromFiniteLattice,
    preparedRegionAt,
    preparedRegionTable,
    supportCarrierFromSupport,
    supportCarrierMeet,
    supportCarrierRegion,
    supportCarrierUnion,
  )
import Moonlight.Sheaf.TestFixture.Branch
  ( BranchContext,
    branchContextLattice,
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
  testGroup
    "context region algebra"
    [ testGroup "anatomy site (non-distributive)" (siteLaws anatomySite),
      testGroup "branch site" (siteLaws branchSite),
      testCase "anatomy lattice is genuinely non-distributive" testAnatomyNonDistributive,
      testGroup "powerset table (symbolic arm, n=3)" symbolicPowersetLaws,
      testCase "symbolic difference stays compact at twenty atoms" testSymbolicDifferenceTwentyAtoms,
      testGroup "cross-arm bitset oracle (n=3, exhaustive)" crossArmOracleLaws
    ]

siteLaws :: (Ord c, Show c) => PreparedContextSite c -> [TestTree]
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

anatomyLattice :: ContextLattice AnatomyContext
anatomyLattice =
  either
    (error . ("invalid anatomy fixture lattice: " <>) . show)
    id
    ( compileContextLattice
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
    )

anatomySite :: PreparedContextSite AnatomyContext
anatomySite =
  fromFiniteLattice anatomyLattice

branchSite :: PreparedContextSite BranchContext
branchSite =
  fromFiniteLattice branchContextLattice

requireRegion :: (Ord c, Show c) => PreparedContextSite c -> c -> ContextRegion
requireRegion site contextValue =
  either
    (\lookupError -> error ("principal region unavailable: " <> show lookupError))
    id
    (preparedRegionAt site contextValue)

generatedOpens :: (Ord c, Show c) => PreparedContextSite c -> [ContextRegion]
generatedOpens site =
  [ regionJoin (requireRegion site left) (requireRegion site right)
    | left <- preparedContextObjects site,
      right <- preparedContextObjects site
  ]

carrierBasis :: (Ord c, Show c) => PreparedContextSite c -> [SupportCarrier c]
carrierBasis site =
  [ carrier
    | contexts <- generatorSelections,
      Right basis <- [supportBasis (preparedContextLattice site) contexts],
      Right carrier <- [supportCarrierFromSupport site basis]
  ]
  where
    objects = preparedContextObjects site
    generatorSelections =
      fmap pure objects
        <> [[left, right] | left <- objects, right <- objects, left /= right]

testPrincipalRegionsOpen :: (Ord c, Show c) => PreparedContextSite c -> Assertion
testPrincipalRegionsOpen site =
  assertBool
    "every principal region must be up-closed"
    (all (regionIsOpen (preparedRegionTable site) . requireRegion site) (preparedContextObjects site))

testAnnotationLaw :: (Ord c, Show c) => PreparedContextSite c -> Assertion
testAnnotationLaw site =
  sequence_
    [ case joinContext (preparedContextLattice site) left right of
        Left lookupError ->
          assertFailure ("join lookup failed: " <> show lookupError)
        Right joined ->
          requireRegion site joined
            @?= regionMeet (requireRegion site left) (requireRegion site right)
      | left <- preparedContextObjects site,
        right <- preparedContextObjects site
    ]

testFrameClosure :: (Ord c, Show c) => PreparedContextSite c -> Assertion
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
    principals = fmap (requireRegion site) (preparedContextObjects site)

testEntailmentMirrorsOrder :: (Ord c, Show c) => PreparedContextSite c -> Assertion
testEntailmentMirrorsOrder site =
  sequence_
    [ leqContext (preparedContextLattice site) left right
        @?= Right (regionEntails (requireRegion site right) (requireRegion site left))
      | left <- preparedContextObjects site,
        right <- preparedContextObjects site
    ]

testComplementLaws :: (Ord c, Show c) => PreparedContextSite c -> Assertion
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

testDifferenceLaws :: (Ord c, Show c) => PreparedContextSite c -> Assertion
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
    sameRegion left right =
      regionEntails left right && regionEntails right left

testGeneratorRoundTrip :: (Ord c, Show c) => PreparedContextSite c -> Assertion
testGeneratorRoundTrip site =
  sequence_
    [ fromGeneratorKeys table (regionGeneratorKeys table open) @?= open
      | open <- generatedOpens site
    ]
  where
    table = preparedRegionTable site

testCarrierMeetAgreement :: (Ord c, Show c) => PreparedContextSite c -> Assertion
testCarrierMeetAgreement site =
  sequence_
    [ supportCarrierRegion site (supportCarrierMeet site leftCarrier rightCarrier)
        @?= regionMeet
          (supportCarrierRegion site leftCarrier)
          (supportCarrierRegion site rightCarrier)
      | leftCarrier <- carrierBasis site,
        rightCarrier <- carrierBasis site
    ]

testCarrierUnionAgreement :: (Ord c, Show c) => PreparedContextSite c -> Assertion
testCarrierUnionAgreement site =
  sequence_
    [ supportCarrierRegion site (supportCarrierUnion site leftCarrier rightCarrier)
        @?= regionJoin
          (supportCarrierRegion site leftCarrier)
          (supportCarrierRegion site rightCarrier)
      | leftCarrier <- carrierBasis site,
        rightCarrier <- carrierBasis site
    ]

testMeetEmptiness :: (Ord c, Show c) => PreparedContextSite c -> Assertion
testMeetEmptiness site =
  sequence_
    [ regionEmpty (regionMeet (requireRegion site left) (requireRegion site right))
        @?= not (sharesUpperBound left right)
      | left <- preparedContextObjects site,
        right <- preparedContextObjects site
    ]
  where
    sharesUpperBound left right =
      any
        ( \candidate ->
            leqContext (preparedContextLattice site) left candidate == Right True
              && leqContext (preparedContextLattice site) right candidate == Right True
        )
        (preparedContextObjects site)

testAnatomyNonDistributive :: Assertion
testAnatomyNonDistributive =
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

powersetMasks :: [Int]
powersetMasks =
  [0 .. bit powersetAtomCount - 1]

symbolicPowersetTable :: RegionTable
symbolicPowersetTable =
  powersetRegionTable powersetAtomCount

densePowersetTable :: RegionTable
densePowersetTable =
  regionTableFromUpsets
    (bit powersetAtomCount)
    ( IntMap.fromList
        [ (maskValue, IntSet.fromList [wider | wider <- powersetMasks, maskValue .&. wider == maskValue])
          | maskValue <- powersetMasks
        ]
    )
    ( IntMap.fromList
        [ (maskValue, IntSet.fromList [lower | lower <- powersetMasks, lower .&. maskValue == lower, lower /= maskValue])
          | maskValue <- powersetMasks
        ]
    )

data PowersetRegionExpr
  = PrincipalAt Int
  | ExprJoin PowersetRegionExpr PowersetRegionExpr
  | ExprMeet PowersetRegionExpr PowersetRegionExpr
  | ExprComplement PowersetRegionExpr
  | ExprDifference PowersetRegionExpr PowersetRegionExpr

evalRegionExpr :: RegionTable -> PowersetRegionExpr -> ContextRegion
evalRegionExpr table expr =
  case expr of
    PrincipalAt maskValue -> regionAtKey table maskValue
    ExprJoin left right -> regionJoin (evalRegionExpr table left) (evalRegionExpr table right)
    ExprMeet left right -> regionMeet (evalRegionExpr table left) (evalRegionExpr table right)
    ExprComplement inner -> regionComplementIn table (evalRegionExpr table inner)
    ExprDifference whole removed ->
      regionDifference table (evalRegionExpr table whole) (evalRegionExpr table removed)

openPowersetExprs :: [PowersetRegionExpr]
openPowersetExprs =
  fmap PrincipalAt powersetMasks
    <> [ ExprJoin (PrincipalAt left) (PrincipalAt right)
         | left <- powersetMasks,
           right <- powersetMasks
       ]

mixedPowersetExprs :: [PowersetRegionExpr]
mixedPowersetExprs =
  openPowersetExprs
    <> fmap ExprComplement openPowersetExprs
    <> [ ExprMeet open (ExprComplement other)
         | open <- openPowersetExprs,
           other <- openPowersetExprs
       ]
    <> [ ExprDifference whole removed
         | whole <- openPowersetExprs,
           removed <- openPowersetExprs
       ]

symbolicOpens :: [ContextRegion]
symbolicOpens =
  fmap (evalRegionExpr symbolicPowersetTable) openPowersetExprs

symbolicPowersetLaws :: [TestTree]
symbolicPowersetLaws =
  [ testCase "principal regions are open" testSymbolicPrincipalsOpen,
    testCase "annotation law: region of a join is the meet of regions" testSymbolicAnnotationLaw,
    testCase "meets and joins of opens stay open (frame closure)" testSymbolicFrameClosure,
    testCase "entailment mirrors the mask order" testSymbolicEntailmentMirrorsOrder,
    testCase "complements of opens are down-closed and involutive" testSymbolicComplementLaws,
    testCase "difference preserves the disjoint residual without enumeration" testSymbolicDifferenceLaws,
    testCase "generator antichain round-trips every generated open" testSymbolicGeneratorRoundTrip,
    testCase "meet is void exactly against the complement" testSymbolicMeetEmptiness,
    testCase "size agrees with enumerated keys on every expression" testSymbolicSizeLaw,
    testCase "top and void are complementary" testSymbolicTopVoidDuality
  ]

symbolicSameKeys :: ContextRegion -> ContextRegion -> Bool
symbolicSameKeys leftRegion rightRegion =
  regionKeys symbolicPowersetTable leftRegion
    == regionKeys symbolicPowersetTable rightRegion

testSymbolicPrincipalsOpen :: Assertion
testSymbolicPrincipalsOpen =
  assertBool
    "every principal region must be up-closed"
    ( all
        (regionIsOpen symbolicPowersetTable . regionAtKey symbolicPowersetTable)
        powersetMasks
    )

testSymbolicAnnotationLaw :: Assertion
testSymbolicAnnotationLaw =
  sequence_
    [ regionAtKey symbolicPowersetTable (left .|. right)
        @?= regionMeet
          (regionAtKey symbolicPowersetTable left)
          (regionAtKey symbolicPowersetTable right)
      | left <- powersetMasks,
        right <- powersetMasks
    ]

testSymbolicFrameClosure :: Assertion
testSymbolicFrameClosure =
  assertBool
    "meets and joins of principal opens must remain open"
    ( and
        [ regionIsOpen symbolicPowersetTable (regionMeet left right)
            && regionIsOpen symbolicPowersetTable (regionJoin left right)
          | left <- principals,
            right <- principals
        ]
    )
  where
    principals = fmap (regionAtKey symbolicPowersetTable) powersetMasks

testSymbolicEntailmentMirrorsOrder :: Assertion
testSymbolicEntailmentMirrorsOrder =
  sequence_
    [ (left .&. right == left)
        @?= regionEntails
          (regionAtKey symbolicPowersetTable right)
          (regionAtKey symbolicPowersetTable left)
      | left <- powersetMasks,
        right <- powersetMasks
    ]

testSymbolicComplementLaws :: Assertion
testSymbolicComplementLaws =
  assertBool
    "complements of opens must be down-closed and involutive"
    ( and
        [ regionIsDownClosed symbolicPowersetTable complementRegion
            && symbolicSameKeys
              (regionComplementIn symbolicPowersetTable complementRegion)
              open
          | open <- symbolicOpens,
            let complementRegion = regionComplementIn symbolicPowersetTable open
        ]
    )

testSymbolicDifferenceLaws :: Assertion
testSymbolicDifferenceLaws =
  assertBool
    "symbolic difference must be disjoint from the removed region and recompose its whole"
    ( and
        [ regionEmpty (regionMeet residual removed)
            && symbolicSameKeys
              (regionJoin residual (regionMeet whole removed))
              whole
          | whole <- symbolicOpens,
            removed <- symbolicOpens,
            let residual = regionDifference symbolicPowersetTable whole removed
        ]
    )

testSymbolicDifferenceTwentyAtoms :: Assertion
testSymbolicDifferenceTwentyAtoms =
  assertBool
    "subtracting one principal region must retain one symbolic residual cube"
    ( regionCubeCount table residual == 1
        && regionSize residual == bit 18
        && regionMemberKey residual atomZero
        && regionMemberKey residual (atomZero .|. atomTwo)
        && not (regionMemberKey residual (atomZero .|. atomOne))
    )
  where
    table = powersetRegionTable 20
    atomZero = bit 0
    atomOne = bit 1
    atomTwo = bit 2
    residual =
      regionDifference
        table
        (regionAtKey table atomZero)
        (regionAtKey table (atomZero .|. atomOne))

testSymbolicGeneratorRoundTrip :: Assertion
testSymbolicGeneratorRoundTrip =
  sequence_
    [ fromGeneratorKeys
        symbolicPowersetTable
        (regionGeneratorKeys symbolicPowersetTable open)
        @?= open
      | open <- symbolicOpens
    ]

testSymbolicMeetEmptiness :: Assertion
testSymbolicMeetEmptiness =
  assertBool
    "principal meets share the top; opens meet their complements in void"
    ( and
        [ not
            ( regionEmpty
                ( regionMeet
                    (regionAtKey symbolicPowersetTable left)
                    (regionAtKey symbolicPowersetTable right)
                )
            )
          | left <- powersetMasks,
            right <- powersetMasks
        ]
        && all
          ( \open ->
              regionEmpty
                (regionMeet open (regionComplementIn symbolicPowersetTable open))
          )
          symbolicOpens
    )

testSymbolicSizeLaw :: Assertion
testSymbolicSizeLaw =
  sequence_
    [ regionSize region @?= length (regionKeys symbolicPowersetTable region)
      | expr <- mixedPowersetExprs,
        let region = evalRegionExpr symbolicPowersetTable expr
    ]

testSymbolicTopVoidDuality :: Assertion
testSymbolicTopVoidDuality =
  assertBool
    "complement exchanges top and void"
    ( regionEmpty
        (regionComplementIn symbolicPowersetTable (regionTop symbolicPowersetTable))
        && symbolicSameKeys
          (regionComplementIn symbolicPowersetTable regionVoid)
          (regionTop symbolicPowersetTable)
    )

crossArmOracleLaws :: [TestTree]
crossArmOracleLaws =
  [ testCase "membership decode agrees on every expression" testOracleMembership,
    testCase "keys, size, emptiness, and generators agree on every expression" testOracleProjections,
    testCase "openness and down-closure verdicts agree on every expression" testOracleClosureVerdicts,
    testCase "entailment agrees across all open pairs" testOracleEntailment,
    testCase "mixed-arm operands agree with single-arm results" testOraclePromotion,
    testCase "raw key sets meet symbolic opens exactly" testOracleRawKeyPromotion
  ]

pairedOracleRegions :: [(ContextRegion, ContextRegion)]
pairedOracleRegions =
  [ ( evalRegionExpr densePowersetTable expr,
      evalRegionExpr symbolicPowersetTable expr
    )
    | expr <- mixedPowersetExprs
  ]

pairedOracleOpens :: [(ContextRegion, ContextRegion)]
pairedOracleOpens =
  [ ( evalRegionExpr densePowersetTable expr,
      evalRegionExpr symbolicPowersetTable expr
    )
    | expr <- openPowersetExprs
  ]

testOracleMembership :: Assertion
testOracleMembership =
  assertBool
    "dense and symbolic arms must decode identically"
    ( and
        [ regionMemberKey denseRegion keyValue == regionMemberKey symbolicRegion keyValue
          | (denseRegion, symbolicRegion) <- pairedOracleRegions,
            keyValue <- powersetMasks
        ]
    )

testOracleProjections :: Assertion
testOracleProjections =
  sequence_
    ( concat
        [ [ regionKeys densePowersetTable denseRegion
              @?= regionKeys symbolicPowersetTable symbolicRegion,
            regionSize denseRegion @?= regionSize symbolicRegion,
            regionEmpty denseRegion @?= regionEmpty symbolicRegion,
            regionGeneratorKeys densePowersetTable denseRegion
              @?= regionGeneratorKeys symbolicPowersetTable symbolicRegion
          ]
          | (denseRegion, symbolicRegion) <- pairedOracleRegions
        ]
    )

testOracleClosureVerdicts :: Assertion
testOracleClosureVerdicts =
  sequence_
    ( concat
        [ [ regionIsOpen densePowersetTable denseRegion
              @?= regionIsOpen symbolicPowersetTable symbolicRegion,
            regionIsDownClosed densePowersetTable denseRegion
              @?= regionIsDownClosed symbolicPowersetTable symbolicRegion
          ]
          | (denseRegion, symbolicRegion) <- pairedOracleRegions
        ]
    )

testOracleEntailment :: Assertion
testOracleEntailment =
  sequence_
    [ regionEntails denseLeft denseRight
        @?= regionEntails symbolicLeft symbolicRight
      | (denseLeft, symbolicLeft) <- pairedOracleOpens,
        (denseRight, symbolicRight) <- pairedOracleOpens
    ]

testOraclePromotion :: Assertion
testOraclePromotion =
  assertBool
    "promoted mixed-arm operations must match their single-arm verdicts"
    ( and
        [ decodesLike (regionMeet denseLeft symbolicRight) (regionMeet denseLeft denseRight)
            && decodesLike (regionJoin denseLeft symbolicRight) (regionJoin denseLeft denseRight)
            && regionEntails denseLeft symbolicRight == regionEntails denseLeft denseRight
            && regionEntails symbolicLeft denseRight == regionEntails denseLeft denseRight
          | (denseLeft, symbolicLeft) <- pairedOracleOpens,
            (denseRight, symbolicRight) <- pairedOracleOpens
        ]
    )
  where
    decodesLike mixedRegion denseRegion =
      all
        (\keyValue -> regionMemberKey mixedRegion keyValue == regionMemberKey denseRegion keyValue)
        powersetMasks

testOracleRawKeyPromotion :: Assertion
testOracleRawKeyPromotion =
  assertBool
    "raw key sets must meet symbolic opens exactly at their common keys"
    ( and
        [ all
            ( \keyValue ->
                regionMemberKey (regionMeet rawRegion symbolicOpen) keyValue
                  == (IntSet.member keyValue rawKeys && regionMemberKey symbolicOpen keyValue)
            )
            powersetMasks
          | rawKeys <- fmap IntSet.fromList (subsequences powersetMasks),
            let rawRegion = regionFromKeys symbolicPowersetTable rawKeys,
            symbolicOpen <- symbolicOpens
        ]
    )

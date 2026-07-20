{-# LANGUAGE RankNTypes #-}

module Moonlight.Sheaf.Descent.ContextSitePowersetSpec
  ( tests,
  )
where

import Data.Bits (bit, (.|.))
import Data.IntMap.Strict qualified as IntMap
import Data.List (elemIndex, sort, subsequences)
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.FiniteLattice
  ( ContextLattice,
    ContextLatticeCompileError,
    SupportBasis,
    compileContextLattice,
    contextLatticeElements,
    contextOrderDecl,
    principalSupport,
  )
import Moonlight.Sheaf.Context.Region
  ( regionMemberKey,
    regionTableObjectCount,
  )
import Moonlight.Sheaf.Context.Site
  ( ContextObjectKey,
    contextObjectKeyValue,
    PowersetSitePreparationError (..),
    PreparedContextSite,
    PreparedContextSupportError (..),
    RefusedSiteEnumeration (..),
    SiteEnumerability (..),
    SiteEnumerationRefusal (..),
    classKeysVisibleAtKey,
    classSupportIndexFromEntries,
    contextFragmentJoinClosure,
    contextFragmentKeyedObjects,
    contextFragmentLattice,
    contextFragmentObjectSet,
    contextFragmentObjects,
    contextFragmentRestrictionPairs,
    contextObjectKeyFor,
    contextRestrictionRegistryForObjects,
    defaultPreparedSupport,
    joinClosureOverContexts,
    meetPreparedSupport,
    preparedContextAtKey,
    preparedContextFragment,
    preparedContextRestrictsTo,
    preparedJoinClosureOver,
    preparedMaterializedJoinClosure,
    preparedMaterializedKeyedObjects,
    preparedMaterializedLattice,
    preparedMaterializedObjectSet,
    preparedMaterializedObjects,
    preparedMaterializedRestrictionPairs,
    preparedRegionAt,
    preparedRegionTable,
    preparedRestrictionSources,
    preparedRestrictionSourcesAmong,
    preparedRestrictionTargets,
    preparedRestrictionTargetsAmong,
    preparedSiteEnumerability,
    preparedSupportFromContexts,
    preparedSupportObjects,
    supportCarrierContainsKey,
    supportCarrierFromSupport,
    supportCarrierGeneratorCount,
    supportCarrierRegion,
    unionPreparedSupport,
    withPreparedContextSiteFromFiniteLattice,
    withPreparedContextSiteFromPowersetAtoms,
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
    "implicit powerset site"
    [ testGroup
        "preparation"
        [ testCase "refuses atom universes beyond the 62-bit budget" testBudgetRefusal,
          testCase "refuses duplicated atoms" testDuplicateRefusal,
          testCase "keys are ascending-atom rank masks, permutation-invariant" testKeyCoordinates,
          testCase "unknown contexts are refused as missing objects" testUnknownContextRefusal
        ],
      testGroup
        "query agreement with the materialized powerset (n <= 4)"
        [ testCase "point key decoding agrees without enumerating the site" testPointKeyDecodingAgreement,
          testCase "restriction order agrees on every context pair" testRestrictionOrderAgreement,
          testCase "scoped restriction neighborhoods agree across arms and with their definition" testRestrictionNeighborhoodScoping,
          testCase "principal regions decode identically" testPrincipalRegionAgreement,
          testCase "support bases agree on every generator family" testSupportBasisAgreement,
          testCase "support union and meet agree on every candidate pair" testSupportAlgebraAgreement,
          testCase "carrier containment and antichain size agree at every context" testCarrierContainmentAgreement,
          testCase "carrier regions decode identically" testCarrierRegionAgreement,
          testCase "class visibility agrees for the indexed candidate supports" testClassVisibilityAgreement,
          testCase "inhabited join closure agrees with the lattice closure" testJoinClosureAgreement,
          testCase "inhabited join closure reports unknown contexts as typed failures" testJoinClosurePoisonAgreement,
          testCase "restriction registries agree on every object subfamily" testRestrictionRegistryAgreement,
          testCase "default support is the principal bottom on both arms" testDefaultSupportAgreement,
          testCase "the region table is the Phase 1 powerset table" testRegionTableWiring
        ],
      testGroup
        "enumeration refusals and the bottom fragment"
        [ testCase "materialized queries refuse with the matching payload" testMaterializedRefusals,
          testCase "either-channel queries refuse through the support error" testSupportChannelRefusals,
          testCase "materialized queries project the classic queries on materialized sites" testMaterializedProjection,
          testCase "classic queries answer the bottom fragment on the implicit site" testBottomFragment
        ]
    ]

atoms3 :: [Char]
atoms3 =
  "abc"

atoms4 :: [Char]
atoms4 =
  "abcd"

subsetsOf :: Ord a => [a] -> [Set a]
subsetsOf =
  fmap Set.fromList . subsequences

maskOf :: [Char] -> Set Char -> Int
maskOf atomList subset =
  foldl
    (\maskValue atomValue -> maybe maskValue ((maskValue .|.) . bit) (elemIndex atomValue (sort atomList)))
    0
    (Set.toAscList subset)

powersetLattice :: [Char] -> Either (ContextLatticeCompileError (Set Char)) (ContextLattice (Set Char))
powersetLattice atomList =
  compileContextLattice
    (Set.fromList subsets)
    ( contextOrderDecl
        (Set.fromList atomList)
        Set.empty
        [ (subset, Set.insert atomValue subset)
          | subset <- subsets,
            atomValue <- atomList,
            not (Set.member atomValue subset)
        ]
    )
  where
    subsets = subsetsOf atomList

withTwinSites ::
  [Char] ->
  ( forall denseOwner symbolicOwner.
    PreparedContextSite denseOwner (Set Char) ->
    PreparedContextSite symbolicOwner (Set Char) ->
    Assertion
  ) ->
  Assertion
withTwinSites atomList useSites =
  case powersetLattice atomList of
    Left latticeError ->
      assertFailure ("invalid powerset fixture lattice: " <> show latticeError)
    Right lattice ->
      withPreparedContextSiteFromFiniteLattice lattice $ \denseSite ->
        case withPreparedContextSiteFromPowersetAtoms atomList (useSites denseSite) of
          Left siteError ->
            assertFailure ("invalid powerset fixture site: " <> show siteError)
          Right assertion ->
            assertion

withSymbolicSite ::
  [Char] ->
  (forall owner. PreparedContextSite owner (Set Char) -> Assertion) ->
  Assertion
withSymbolicSite atomList useSite =
  case withPreparedContextSiteFromPowersetAtoms atomList useSite of
    Left siteError ->
      assertFailure ("invalid powerset fixture site: " <> show siteError)
    Right assertion ->
      assertion

subsets3 :: [Set Char]
subsets3 =
  subsetsOf atoms3

subsets4 :: [Set Char]
subsets4 =
  subsetsOf atoms4

generatorFamilies3 :: [[Set Char]]
generatorFamilies3 =
  filter (not . null) (subsequences subsets3)

candidateBasesFor :: PreparedContextSite owner (Set Char) -> [SupportBasis (Set Char)]
candidateBasesFor site =
  [ requireRight (preparedSupportFromContexts site family)
    | family <- fmap pure subsets3 <> [[left, right] | left <- subsets3, right <- subsets3, left /= right]
  ]

requireRight :: Show errorValue => Either errorValue value -> value
requireRight resultValue =
  case resultValue of
    Left errorValue -> error ("expected Right, got " <> show errorValue)
    Right value -> value

keyFor :: PreparedContextSite owner (Set Char) -> Set Char -> ContextObjectKey owner
keyFor site subset =
  requireRight (contextObjectKeyFor site subset)

testBudgetRefusal :: Assertion
testBudgetRefusal =
  preparationRefusal
    (withPreparedContextSiteFromPowersetAtoms (take 63 ['\0' ..]) (const ()))
    @?= Just (PowersetAtomBudgetExceeded 63)

testDuplicateRefusal :: Assertion
testDuplicateRefusal =
  preparationRefusal
    (withPreparedContextSiteFromPowersetAtoms "aba" (const ()))
    @?= Just (PowersetAtomDuplicated 'a')

preparationRefusal ::
  Either (PowersetSitePreparationError a) () ->
  Maybe (PowersetSitePreparationError a)
preparationRefusal = either Just (const Nothing)

testKeyCoordinates :: Assertion
testKeyCoordinates =
  withSymbolicSite atoms4 $ \symbolicSite4 ->
    withSymbolicSite (reverse atoms4) $ \reversedSymbolicSite4 ->
      sequence_
        [ do
            fmap contextObjectKeyValue (contextObjectKeyFor symbolicSite4 subset)
              @?= Right (maskOf atoms4 subset)
            fmap contextObjectKeyValue (contextObjectKeyFor reversedSymbolicSite4 subset)
              @?= fmap contextObjectKeyValue (contextObjectKeyFor symbolicSite4 subset)
          | subset <- subsets4
        ]

testUnknownContextRefusal :: Assertion
testUnknownContextRefusal =
  withSymbolicSite atoms3 $ \symbolicSite3 ->
    contextObjectKeyFor symbolicSite3 (Set.fromList "z")
      @?= Left (PreparedContextSupportObjectMissing (Set.fromList "z"))

testRestrictionOrderAgreement :: Assertion
testRestrictionOrderAgreement =
  withTwinSites atoms4 $ \denseSite4 symbolicSite4 ->
    sequence_
      [ preparedContextRestrictsTo symbolicSite4 source target
          @?= preparedContextRestrictsTo denseSite4 source target
        | source <- subsets4,
          target <- subsets4
      ]

testRestrictionNeighborhoodScoping :: Assertion
testRestrictionNeighborhoodScoping =
  withTwinSites atoms3 $ \denseSite3 symbolicSite3 ->
    sequence_
      [ do
          preparedRestrictionSourcesAmong scope focus symbolicSite3
            @?= preparedRestrictionSourcesAmong scope focus denseSite3
          preparedRestrictionTargetsAmong scope focus symbolicSite3
            @?= preparedRestrictionTargetsAmong scope focus denseSite3
          requireRight (preparedRestrictionSourcesAmong scope focus symbolicSite3)
            @?= [ source
                  | source <- scope,
                    source /= focus,
                    requireRight (preparedContextRestrictsTo symbolicSite3 source focus)
                ]
          requireRight (preparedRestrictionTargetsAmong scope focus symbolicSite3)
            @?= [ target
                  | target <- scope,
                    target /= focus,
                    requireRight (preparedContextRestrictsTo symbolicSite3 focus target)
                ]
          Set.fromList (requireRight (preparedRestrictionSourcesAmong subsets3 focus denseSite3))
            @?= Set.fromList (requireRight (preparedRestrictionSources focus denseSite3))
          Set.fromList (requireRight (preparedRestrictionTargetsAmong subsets3 focus denseSite3))
            @?= Set.fromList (requireRight (preparedRestrictionTargets focus denseSite3))
        | scope <- neighborhoodScopes3,
          focus <- subsets3
      ]

neighborhoodScopes3 :: [[Set Char]]
neighborhoodScopes3 =
  [ subsets3,
    reverse subsets3,
    [Set.empty, Set.fromList "a", Set.fromList "ab"],
    []
  ]

regionDecode :: PreparedContextSite owner (Set Char) -> [Set Char] -> Set Char -> Set (Set Char)
regionDecode site universe subset =
  Set.fromList
    [ member
      | let region = requireRight (preparedRegionAt site subset),
        member <- universe,
        regionMemberKey region (keyFor site member)
    ]

testPointKeyDecodingAgreement :: Assertion
testPointKeyDecodingAgreement =
  withTwinSites atoms3 $ \denseSite3 symbolicSite3 ->
    sequence_
      [ do
          preparedContextAtKey symbolicSite3 (keyFor symbolicSite3 subset) @?= Just subset
          preparedContextAtKey denseSite3 (keyFor denseSite3 subset) @?= Just subset
        | subset <- subsets3
      ]

testPrincipalRegionAgreement :: Assertion
testPrincipalRegionAgreement =
  withTwinSites atoms3 $ \denseSite3 symbolicSite3 ->
    sequence_
      [ regionDecode symbolicSite3 subsets3 subset @?= regionDecode denseSite3 subsets3 subset
        | subset <- subsets3
      ]

testSupportBasisAgreement :: Assertion
testSupportBasisAgreement =
  withTwinSites atoms3 $ \denseSite3 symbolicSite3 ->
    sequence_
      [ preparedSupportFromContexts symbolicSite3 family
          @?= preparedSupportFromContexts denseSite3 family
        | family <- generatorFamilies3
      ]

testSupportAlgebraAgreement :: Assertion
testSupportAlgebraAgreement =
  withTwinSites atoms3 $ \denseSite3 symbolicSite3 ->
    let candidateBases3 = candidateBasesFor denseSite3
     in sequence_
          [ do
              unionPreparedSupport symbolicSite3 leftBasis rightBasis
                @?= unionPreparedSupport denseSite3 leftBasis rightBasis
              meetPreparedSupport symbolicSite3 leftBasis rightBasis
                @?= meetPreparedSupport denseSite3 leftBasis rightBasis
            | leftBasis <- candidateBases3,
              rightBasis <- candidateBases3
          ]

testCarrierContainmentAgreement :: Assertion
testCarrierContainmentAgreement =
  withTwinSites atoms3 $ \denseSite3 symbolicSite3 ->
    sequence_
      [ do
          supportCarrierGeneratorCount symbolicCarrier @?= supportCarrierGeneratorCount denseCarrier
          sequence_
            [ supportCarrierContainsKey symbolicSite3 symbolicCarrier (keyFor symbolicSite3 subset)
                @?= supportCarrierContainsKey denseSite3 denseCarrier (keyFor denseSite3 subset)
              | subset <- subsets3
            ]
        | basis <- candidateBasesFor denseSite3,
          let symbolicCarrier = requireRight (supportCarrierFromSupport symbolicSite3 basis),
          let denseCarrier = requireRight (supportCarrierFromSupport denseSite3 basis)
      ]

testCarrierRegionAgreement :: Assertion
testCarrierRegionAgreement =
  withTwinSites atoms3 $ \denseSite3 symbolicSite3 ->
    sequence_
      [ symbolicMembers @?= denseMembers
        | basis <- candidateBasesFor denseSite3,
          let symbolicRegion = supportCarrierRegion symbolicSite3 (requireRight (supportCarrierFromSupport symbolicSite3 basis)),
          let denseRegion = supportCarrierRegion denseSite3 (requireRight (supportCarrierFromSupport denseSite3 basis)),
          let symbolicMembers =
                Set.fromList
                  [ subset
                    | subset <- subsets3,
                      regionMemberKey symbolicRegion (keyFor symbolicSite3 subset)
                  ],
          let denseMembers =
                Set.fromList
                  [ subset
                    | subset <- subsets3,
                      regionMemberKey denseRegion (keyFor denseSite3 subset)
                  ]
      ]

testClassVisibilityAgreement :: Assertion
testClassVisibilityAgreement =
  withTwinSites atoms3 $ \denseSite3 symbolicSite3 -> do
    let entries = IntMap.fromList (zip [0 ..] (candidateBasesFor denseSite3))
        symbolicIndex = requireRight (classSupportIndexFromEntries symbolicSite3 entries)
        denseIndex = requireRight (classSupportIndexFromEntries denseSite3 entries)
    sequence_
      [ classKeysVisibleAtKey symbolicSite3 symbolicIndex (keyFor symbolicSite3 subset)
          @?= classKeysVisibleAtKey denseSite3 denseIndex (keyFor denseSite3 subset)
        | subset <- subsets3
      ]

testJoinClosureAgreement :: Assertion
testJoinClosureAgreement =
  withTwinSites atoms3 $ \denseSite3 symbolicSite3 ->
    let denseFragment = preparedContextFragment denseSite3
     in
    sequence_
      [ do
          preparedJoinClosureOver symbolicSite3 family @?= preparedJoinClosureOver denseSite3 family
          preparedJoinClosureOver denseSite3 family
            @?= joinClosureOverContexts (contextFragmentLattice denseFragment) family
        | family <- generatorFamilies3
      ]

testJoinClosurePoisonAgreement :: Assertion
testJoinClosurePoisonAgreement =
  withTwinSites atoms3 $ \denseSite3 symbolicSite3 -> do
    let poisonedBase = [Set.fromList "a", Set.fromList "z"]
        (symbolicClosure, symbolicFailures) = preparedJoinClosureOver symbolicSite3 poisonedBase
        (denseClosure, denseFailures) = preparedJoinClosureOver denseSite3 poisonedBase
    symbolicClosure @?= denseClosure
    Set.fromList symbolicFailures @?= Set.fromList denseFailures
    assertBool "the unknown context must surface in the failures" (not (null symbolicFailures))

testRestrictionRegistryAgreement :: Assertion
testRestrictionRegistryAgreement =
  withTwinSites atoms3 $ \denseSite3 symbolicSite3 ->
    sequence_
      [ contextRestrictionRegistryForObjects (Set.fromList family) symbolicSite3
          @?= contextRestrictionRegistryForObjects (Set.fromList family) denseSite3
        | family <- subsequences subsets3
      ]

testDefaultSupportAgreement :: Assertion
testDefaultSupportAgreement =
  withTwinSites atoms3 $ \denseSite3 symbolicSite3 -> do
    defaultPreparedSupport symbolicSite3 @?= principalSupport Set.empty
    defaultPreparedSupport denseSite3 @?= principalSupport Set.empty

testRegionTableWiring :: Assertion
testRegionTableWiring =
  withSymbolicSite atoms3 $ \symbolicSite3 ->
    regionTableObjectCount (preparedRegionTable symbolicSite3) @?= 8

refusalOf :: RefusedSiteEnumeration -> SiteEnumerationRefusal
refusalOf =
  SiteEnumerationRefusal 3

testMaterializedRefusals :: Assertion
testMaterializedRefusals =
  withSymbolicSite atoms3 $ \symbolicSite3 -> do
    preparedMaterializedObjects symbolicSite3 @?= Left (refusalOf RefusedContextObjects)
    preparedMaterializedKeyedObjects symbolicSite3 @?= Left (refusalOf RefusedContextKeyedObjects)
    preparedMaterializedObjectSet symbolicSite3 @?= Left (refusalOf RefusedContextObjectSet)
    preparedMaterializedRestrictionPairs symbolicSite3 @?= Left (refusalOf RefusedRestrictionPairs)
    case preparedMaterializedJoinClosure symbolicSite3 of
      Left refusal -> refusal @?= refusalOf RefusedJoinClosure
      Right _ -> assertFailure "join closure must refuse on the implicit site"
    case preparedMaterializedLattice symbolicSite3 of
      Left refusal -> refusal @?= refusalOf RefusedContextLattice
      Right _ -> assertFailure "lattice materialization must refuse on the implicit site"

testSupportChannelRefusals :: Assertion
testSupportChannelRefusals =
  withSymbolicSite atoms3 $ \symbolicSite3 -> do
    preparedRestrictionSources (Set.fromList "a") symbolicSite3
      @?= Left (PreparedContextSymbolicEnumerationRefused (refusalOf RefusedRestrictionSources))
    preparedRestrictionTargets (Set.fromList "a") symbolicSite3
      @?= Left (PreparedContextSymbolicEnumerationRefused (refusalOf RefusedRestrictionTargets))
    preparedSupportObjects symbolicSite3 (principalSupport (Set.fromList "a"))
      @?= Left (PreparedContextSymbolicEnumerationRefused (refusalOf RefusedSupportObjects))

testMaterializedProjection :: Assertion
testMaterializedProjection =
  withTwinSites atoms3 $ \denseSite3 _ -> do
    let denseFragment = preparedContextFragment denseSite3
    preparedMaterializedObjects denseSite3 @?= Right (contextFragmentObjects denseFragment)
    preparedMaterializedKeyedObjects denseSite3 @?= Right (contextFragmentKeyedObjects denseFragment)
    preparedMaterializedObjectSet denseSite3 @?= Right (contextFragmentObjectSet denseFragment)
    preparedMaterializedRestrictionPairs denseSite3
      @?= Right (contextFragmentRestrictionPairs denseFragment)
    preparedMaterializedJoinClosure denseSite3
      @?= Right (contextFragmentJoinClosure denseFragment)
    fmap contextLatticeElements (preparedMaterializedLattice denseSite3)
      @?= Right (contextLatticeElements (contextFragmentLattice denseFragment))

testBottomFragment :: Assertion
testBottomFragment =
  withTwinSites atoms3 $ \denseSite3 symbolicSite3 -> do
    let symbolicFragment = preparedContextFragment symbolicSite3
    contextFragmentObjects symbolicFragment @?= [Set.empty]
    fmap
      (\(objectKey, contextValue) -> (contextObjectKeyValue objectKey, contextValue))
      (contextFragmentKeyedObjects symbolicFragment)
      @?= [(0, Set.empty)]
    contextFragmentObjectSet symbolicFragment @?= Set.singleton Set.empty
    contextFragmentRestrictionPairs symbolicFragment @?= []
    contextFragmentJoinClosure symbolicFragment @?= ([Set.empty], [])
    contextLatticeElements (contextFragmentLattice symbolicFragment) @?= [Set.empty]
    preparedSiteEnumerability symbolicSite3 @?= SiteImplicitPowerset 3
    preparedSiteEnumerability denseSite3 @?= SiteFullyMaterialized 8

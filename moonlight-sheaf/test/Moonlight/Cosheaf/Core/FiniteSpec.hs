{-# LANGUAGE TypeFamilies #-}

module Moonlight.Cosheaf.Core.FiniteSpec
  ( tests,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Moonlight.Cosheaf
import Moonlight.Cosheaf.Test.Support
  ( compileFullTropicalCostTable,
    fullFiniteCosheafColimit,
  )
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism (..),
    CoveringFamily,
    PullbackSquare (..),
    Site (..),
    mkCoveringFamily,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )

data SampleObject
  = Root
  | LeftObj
  | RightObj
  | Overlap
  deriving stock (Eq, Ord, Show)

data SampleMorphism
  = Id SampleObject
  | LeftToRoot
  | RightToRoot
  | OverlapToLeft
  | OverlapToRight
  | OverlapToRoot
  deriving stock (Eq, Ord, Show)

data SampleSite = SampleSite
  deriving stock (Eq, Show)

instance Site SampleSite where
  type SiteObject SampleSite = SampleObject
  type SiteMorphism SampleSite = SampleMorphism

  siteObjects _ =
    [Root, LeftObj, RightObj, Overlap]

  siteMorphisms _ =
    [ leftToRoot,
      rightToRoot,
      overlapToLeft,
      overlapToRight,
      overlapToRoot
    ]

  identityAt _ objectValue =
    CheckedMorphism objectValue objectValue (Id objectValue)

  coversAt _ objectValue =
    case objectValue of
      Root ->
        either (const []) pure sampleCover
      _ -> []

  composeChecked _ outerMorphism innerMorphism
    | cmSource outerMorphism /= cmTarget innerMorphism =
        Nothing
    | isIdentity outerMorphism =
        Just innerMorphism
    | isIdentity innerMorphism =
        Just outerMorphism
    | cmWitness outerMorphism == LeftToRoot && cmWitness innerMorphism == OverlapToLeft =
        Just overlapToRoot
    | cmWitness outerMorphism == RightToRoot && cmWitness innerMorphism == OverlapToRight =
        Just overlapToRoot
    | otherwise =
        Nothing

  pullbackPair _ leftMorphism rightMorphism
    | cmWitness leftMorphism == LeftToRoot && cmWitness rightMorphism == RightToRoot =
        Just leftRightPullback
    | cmWitness leftMorphism == RightToRoot && cmWitness rightMorphism == LeftToRoot =
        Just rightLeftPullback
    | otherwise =
        Nothing

tests :: TestTree
tests =
  testGroup
    "finite cosheaf"
    [ testCase "constructs finite cosheaf colimit cover coequalizer and tropical plan" testFiniteCosheafMilestone,
      testCase "rejects corestriction outside target costalk" testRejectsOutsideTarget,
      testCase "thin transitive chain validates over strict Hasse basis" testThinTransitiveChainUsesStrictBasis,
      testCase "thin transitive chain rejects long composite mismatch through basis" testThinTransitiveChainRejectsLongCompositeMismatch,
      testCase "non-thin site keeps full composition validation basis" testNonThinSiteKeepsFullCompositionValidationBasis,
      testCase "thin validation basis covers identity-inner pairs" testThinValidationBasisCoversIdentityInnerPairs,
      testCase "thin cyclic preorder keeps full basis and rejects composite mismatch" testThinCyclicPreorderKeepsFullBasis
    ]

testFiniteCosheafMilestone :: Assertion
testFiniteCosheafMilestone = do
  coverValue <- expectRight sampleCover
  cosheaf <- expectRight (sampleCosheaf goodAlgebra)
  colimitValue <- expectRight (fullFiniteCosheafColimit cosheaf)
  assertEqual "H0 class count" 1 (length (cosheafColimitClassKeys colimitValue))
  coequalizerValue <- expectRight (coverCosheafCoequalizer coverValue cosheaf)
  assertEqual "cover quotient target classes" 1 (length (cccClassTargets coequalizerValue))
  costTable <- expectRight (compileFullTropicalCostTable colimitValue sampleCostModel)
  tropicalPlan <- expectRight (planTropicalCosections costTable)
  case IntMap.toAscList (tcpClassChoices tropicalPlan) of
    [(_classKey, choice)] ->
      assertEqual "least propagated cost" (MinPlusFinite 1) (tccCost choice)
    choices ->
      assertFailure ("expected one tropical class choice, got " <> show choices)

testRejectsOutsideTarget :: Assertion
testRejectsOutsideTarget =
  case sampleCosheaf badOutsideAlgebra of
    Left (FiniteCorestrictionOutsideCostalk morphismValue 0 1) ->
      assertEqual "rejected morphism" leftToRoot morphismValue
    Left otherFailure ->
      assertFailure ("unexpected failure: " <> show otherFailure)
    Right _ ->
      assertFailure "expected outside-costalk failure"

sampleCosheaf ::
  FiniteCosheafAlgebra SampleSite Int () () ->
  Either (FiniteCosheafFailure SampleObject SampleMorphism Int () ()) (FiniteCosheaf SampleSite Int)
sampleCosheaf algebra =
  mkFiniteCosheaf
    SampleSite
    algebra
    ( Map.fromList
        [ (Root, [0]),
          (LeftObj, [0]),
          (RightObj, [0]),
          (Overlap, [0])
        ]
    )

goodAlgebra :: FiniteCosheafAlgebra SampleSite Int () ()
goodAlgebra =
  FiniteCosheafAlgebra
    { fcaCorestrict = \_morphism value -> Right value,
      fcaMismatches = \_object leftValue rightValue -> [() | leftValue /= rightValue],
      fcaNormalize = \_object value -> value
    }

badOutsideAlgebra :: FiniteCosheafAlgebra SampleSite Int () ()
badOutsideAlgebra =
  goodAlgebra
    { fcaCorestrict = \morphismValue value ->
        if cmWitness morphismValue == LeftToRoot
          then Right (value + 1)
          else Right value
    }

sampleCostModel :: TropicalCostModel SampleSite Int
sampleCostModel =
  TropicalCostModel
    { tcmRepresentativeCost = \representativeValue ->
        case cosectionRepObject representativeValue of
          Root -> Right (MinPlusFinite 10)
          LeftObj -> Right (MinPlusFinite 1)
          RightObj -> Right (MinPlusFinite 4)
          Overlap -> Right (MinPlusFinite 3),
      tcmTransitionCost = \_transition -> Right minPlusOne
    }

sampleCover :: Either String (CoveringFamily SampleObject SampleMorphism)
sampleCover =
  mapLeft show (mkCoveringFamily Root (leftToRoot :| [rightToRoot]))

leftToRoot :: CheckedMorphism SampleObject SampleMorphism
leftToRoot =
  CheckedMorphism LeftObj Root LeftToRoot

rightToRoot :: CheckedMorphism SampleObject SampleMorphism
rightToRoot =
  CheckedMorphism RightObj Root RightToRoot

overlapToLeft :: CheckedMorphism SampleObject SampleMorphism
overlapToLeft =
  CheckedMorphism Overlap LeftObj OverlapToLeft

overlapToRight :: CheckedMorphism SampleObject SampleMorphism
overlapToRight =
  CheckedMorphism Overlap RightObj OverlapToRight

overlapToRoot :: CheckedMorphism SampleObject SampleMorphism
overlapToRoot =
  CheckedMorphism Overlap Root OverlapToRoot

leftRightPullback :: PullbackSquare SampleObject SampleMorphism
leftRightPullback =
  PullbackSquare
    { psLeftBase = leftToRoot,
      psRightBase = rightToRoot,
      psApex = Overlap,
      psToLeft = overlapToLeft,
      psToRight = overlapToRight
    }

rightLeftPullback :: PullbackSquare SampleObject SampleMorphism
rightLeftPullback =
  PullbackSquare
    { psLeftBase = rightToRoot,
      psRightBase = leftToRoot,
      psApex = Overlap,
      psToLeft = overlapToRight,
      psToRight = overlapToLeft
    }

isIdentity :: CheckedMorphism SampleObject SampleMorphism -> Bool
isIdentity morphismValue =
  case cmWitness morphismValue of
    Id _ -> True
    _ -> False

expectRight :: (Show left) => Either left right -> IO right
expectRight result =
  case result of
    Left failureValue ->
      assertFailure (show failureValue)
    Right value ->
      pure value

mapLeft :: (left -> left') -> Either left right -> Either left' right
mapLeft f result =
  case result of
    Left leftValue -> Left (f leftValue)
    Right rightValue -> Right rightValue

data ThinChainObject
  = Thin0
  | Thin1
  | Thin2
  | Thin3
  | Thin4
  deriving stock (Eq, Ord, Show)

data ThinChainMorphism
  = ThinId !ThinChainObject
  | ThinArrow !ThinChainObject !ThinChainObject
  deriving stock (Eq, Ord, Show)

data ThinChainSite = ThinChainSite
  deriving stock (Eq, Show)

instance Site ThinChainSite where
  type SiteObject ThinChainSite = ThinChainObject
  type SiteMorphism ThinChainSite = ThinChainMorphism

  siteObjects _ =
    thinChainObjects

  siteMorphisms _ =
    [ thinChainMorphism sourceObject targetObject
    | sourceObject <- thinChainObjects,
      targetObject <- thinChainObjects,
      sourceObject < targetObject
    ]

  identityAt _ objectValue =
    CheckedMorphism objectValue objectValue (ThinId objectValue)

  coversAt _ _ =
    []

  composeChecked _ outerMorphism innerMorphism
    | cmSource outerMorphism /= cmTarget innerMorphism =
        Nothing
    | thinChainIsIdentity outerMorphism =
        Just innerMorphism
    | thinChainIsIdentity innerMorphism =
        Just outerMorphism
    | cmSource innerMorphism <= cmTarget outerMorphism =
        Just (thinChainMorphism (cmSource innerMorphism) (cmTarget outerMorphism))
    | otherwise =
        Nothing

  pullbackPair _ _ _ =
    Nothing

testThinTransitiveChainUsesStrictBasis :: Assertion
testThinTransitiveChainUsesStrictBasis = do
  siteIndex <- expectRight (buildCosheafSiteIndex ThinChainSite)
  assertBool
    "thin basis should be smaller than full composable-pair enumeration"
    (length (cosheafCompositionValidationBasis siteIndex) < length (cosheafComposableMorphismPairs siteIndex))
  _cosheaf <- expectRight (thinChainCosheaf thinChainGoodAlgebra)
  pure ()

testThinTransitiveChainRejectsLongCompositeMismatch :: Assertion
testThinTransitiveChainRejectsLongCompositeMismatch =
  case thinChainCosheaf thinChainLongCompositeMismatchAlgebra of
    Left (FiniteCorestrictionCompositionMismatch _ _ _ _ _) ->
      pure ()
    Left otherFailure ->
      assertFailure ("unexpected failure: " <> show otherFailure)
    Right _ ->
      assertFailure "expected long-composite composition mismatch"

thinChainObjects :: [ThinChainObject]
thinChainObjects =
  [Thin0, Thin1, Thin2, Thin3, Thin4]

thinChainMorphism :: ThinChainObject -> ThinChainObject -> CheckedMorphism ThinChainObject ThinChainMorphism
thinChainMorphism sourceObject targetObject =
  if sourceObject == targetObject
    then CheckedMorphism sourceObject targetObject (ThinId sourceObject)
    else CheckedMorphism sourceObject targetObject (ThinArrow sourceObject targetObject)

thinChainIsIdentity :: CheckedMorphism ThinChainObject ThinChainMorphism -> Bool
thinChainIsIdentity morphismValue =
  case cmWitness morphismValue of
    ThinId _ -> True
    ThinArrow _ _ -> False

thinChainRawCostalks :: Map.Map ThinChainObject [Int]
thinChainRawCostalks =
  Map.fromList (fmap (\objectValue -> (objectValue, [0, 1])) thinChainObjects)

thinChainCosheaf ::
  FiniteCosheafAlgebra ThinChainSite Int () () ->
  Either (FiniteCosheafFailure ThinChainObject ThinChainMorphism Int () ()) (FiniteCosheaf ThinChainSite Int)
thinChainCosheaf algebra =
  mkFiniteCosheaf ThinChainSite algebra thinChainRawCostalks

thinChainGoodAlgebra :: FiniteCosheafAlgebra ThinChainSite Int () ()
thinChainGoodAlgebra =
  thinChainAlgebra (\_morphism value -> Right value)

thinChainLongCompositeMismatchAlgebra :: FiniteCosheafAlgebra ThinChainSite Int () ()
thinChainLongCompositeMismatchAlgebra =
  thinChainAlgebra $ \morphismValue value ->
    case (cmWitness morphismValue, value) of
      (ThinArrow Thin0 Thin4, 0) -> Right 1
      _ -> Right value

thinChainAlgebra ::
  (CheckedMorphism ThinChainObject ThinChainMorphism -> Int -> Either () Int) ->
  FiniteCosheafAlgebra ThinChainSite Int () ()
thinChainAlgebra corestrictAction =
  FiniteCosheafAlgebra
    { fcaCorestrict = corestrictAction,
      fcaMismatches = \_objectValue leftValue rightValue -> [() | leftValue /= rightValue],
      fcaNormalize = \_objectValue value -> value
    }

data CyclicPreorderObject
  = CyclicA
  | CyclicB
  | CyclicC
  deriving stock (Eq, Ord, Show)

data CyclicPreorderMorphism
  = CyclicId !CyclicPreorderObject
  | CyclicArrow !CyclicPreorderObject !CyclicPreorderObject
  deriving stock (Eq, Ord, Show)

data CyclicPreorderSite = CyclicPreorderSite
  deriving stock (Eq, Show)

instance Site CyclicPreorderSite where
  type SiteObject CyclicPreorderSite = CyclicPreorderObject
  type SiteMorphism CyclicPreorderSite = CyclicPreorderMorphism

  siteObjects _ =
    cyclicPreorderObjects

  siteMorphisms _ =
    [ cyclicPreorderMorphism sourceObject targetObject
    | sourceObject <- cyclicPreorderObjects,
      targetObject <- cyclicPreorderObjects,
      sourceObject /= targetObject
    ]

  identityAt _ objectValue =
    CheckedMorphism objectValue objectValue (CyclicId objectValue)

  coversAt _ _ =
    []

  composeChecked _ outerMorphism innerMorphism
    | cmSource outerMorphism /= cmTarget innerMorphism =
        Nothing
    | otherwise =
        Just (cyclicPreorderMorphism (cmSource innerMorphism) (cmTarget outerMorphism))

  pullbackPair _ _ _ =
    Nothing

testThinCyclicPreorderKeepsFullBasis :: Assertion
testThinCyclicPreorderKeepsFullBasis = do
  siteIndex <- expectRight (buildCosheafSiteIndex CyclicPreorderSite)
  assertEqual
    "cyclic thin site must not shrink its validation basis"
    (length (cosheafComposableMorphismPairs siteIndex))
    (length (cosheafCompositionValidationBasis siteIndex))
  case cyclicPreorderCosheaf cyclicPreorderCompositeMismatchAlgebra of
    Left (FiniteCorestrictionCompositionMismatch _ _ _ _ _) ->
      pure ()
    Left otherFailure ->
      assertFailure ("unexpected failure: " <> show otherFailure)
    Right _ ->
      assertFailure "expected cyclic composite mismatch rejection"

cyclicPreorderObjects :: [CyclicPreorderObject]
cyclicPreorderObjects =
  [CyclicA, CyclicB, CyclicC]

cyclicPreorderMorphism :: CyclicPreorderObject -> CyclicPreorderObject -> CheckedMorphism CyclicPreorderObject CyclicPreorderMorphism
cyclicPreorderMorphism sourceObject targetObject =
  if sourceObject == targetObject
    then CheckedMorphism sourceObject targetObject (CyclicId sourceObject)
    else CheckedMorphism sourceObject targetObject (CyclicArrow sourceObject targetObject)

cyclicPreorderRawCostalks :: Map.Map CyclicPreorderObject [Int]
cyclicPreorderRawCostalks =
  Map.fromList (fmap (\objectValue -> (objectValue, [0, 1])) cyclicPreorderObjects)

cyclicPreorderCosheaf ::
  FiniteCosheafAlgebra CyclicPreorderSite Int () () ->
  Either (FiniteCosheafFailure CyclicPreorderObject CyclicPreorderMorphism Int () ()) (FiniteCosheaf CyclicPreorderSite Int)
cyclicPreorderCosheaf algebra =
  mkFiniteCosheaf CyclicPreorderSite algebra cyclicPreorderRawCostalks

cyclicPreorderCompositeMismatchAlgebra :: FiniteCosheafAlgebra CyclicPreorderSite Int () ()
cyclicPreorderCompositeMismatchAlgebra =
  FiniteCosheafAlgebra
    { fcaCorestrict = \morphismValue value ->
        case (cmWitness morphismValue, value) of
          (CyclicArrow CyclicA CyclicB, 0) -> Right 1
          _ -> Right value,
      fcaMismatches = \_objectValue leftValue rightValue -> [() | leftValue /= rightValue],
      fcaNormalize = \_objectValue value -> value
    }

data ParallelObject
  = ParallelA
  | ParallelB
  | ParallelC
  deriving stock (Eq, Ord, Show)

data ParallelMorphism
  = ParallelId !ParallelObject
  | ParallelLeft
  | ParallelRight
  | ParallelBC
  | ParallelACLeft
  | ParallelACRight
  deriving stock (Eq, Ord, Show)

data ParallelSite = ParallelSite
  deriving stock (Eq, Show)

instance Site ParallelSite where
  type SiteObject ParallelSite = ParallelObject
  type SiteMorphism ParallelSite = ParallelMorphism

  siteObjects _ =
    [ParallelA, ParallelB, ParallelC]

  siteMorphisms _ =
    [ parallelLeft,
      parallelRight,
      parallelBC,
      parallelACLeft,
      parallelACRight
    ]

  identityAt _ objectValue =
    CheckedMorphism objectValue objectValue (ParallelId objectValue)

  coversAt _ _ =
    []

  composeChecked _ outerMorphism innerMorphism
    | cmSource outerMorphism /= cmTarget innerMorphism =
        Nothing
    | parallelIsIdentity outerMorphism =
        Just innerMorphism
    | parallelIsIdentity innerMorphism =
        Just outerMorphism
    | cmWitness outerMorphism == ParallelBC && cmWitness innerMorphism == ParallelLeft =
        Just parallelACLeft
    | cmWitness outerMorphism == ParallelBC && cmWitness innerMorphism == ParallelRight =
        Just parallelACRight
    | otherwise =
        Nothing

  pullbackPair _ _ _ =
    Nothing

testNonThinSiteKeepsFullCompositionValidationBasis :: Assertion
testNonThinSiteKeepsFullCompositionValidationBasis = do
  siteIndex <- expectRight (buildCosheafSiteIndex ParallelSite)
  assertEqual
    "non-thin validation basis"
    (cosheafComposableMorphismPairs siteIndex)
    (cosheafCompositionValidationBasis siteIndex)
  case parallelCosheaf parallelCompositionMismatchAlgebra of
    Left (FiniteCorestrictionCompositionMismatch _ _ _ _ _) ->
      pure ()
    Left otherFailure ->
      assertFailure ("unexpected failure: " <> show otherFailure)
    Right _ ->
      assertFailure "expected parallel composition mismatch"

parallelLeft :: CheckedMorphism ParallelObject ParallelMorphism
parallelLeft =
  CheckedMorphism ParallelA ParallelB ParallelLeft

parallelRight :: CheckedMorphism ParallelObject ParallelMorphism
parallelRight =
  CheckedMorphism ParallelA ParallelB ParallelRight

parallelBC :: CheckedMorphism ParallelObject ParallelMorphism
parallelBC =
  CheckedMorphism ParallelB ParallelC ParallelBC

parallelACLeft :: CheckedMorphism ParallelObject ParallelMorphism
parallelACLeft =
  CheckedMorphism ParallelA ParallelC ParallelACLeft

parallelACRight :: CheckedMorphism ParallelObject ParallelMorphism
parallelACRight =
  CheckedMorphism ParallelA ParallelC ParallelACRight

parallelIsIdentity :: CheckedMorphism ParallelObject ParallelMorphism -> Bool
parallelIsIdentity morphismValue =
  case cmWitness morphismValue of
    ParallelId _ -> True
    _ -> False

parallelRawCostalks :: Map.Map ParallelObject [Int]
parallelRawCostalks =
  Map.fromList
    [ (ParallelA, [0, 1]),
      (ParallelB, [0, 1]),
      (ParallelC, [0, 1])
    ]

parallelCosheaf ::
  FiniteCosheafAlgebra ParallelSite Int () () ->
  Either (FiniteCosheafFailure ParallelObject ParallelMorphism Int () ()) (FiniteCosheaf ParallelSite Int)
parallelCosheaf algebra =
  mkFiniteCosheaf ParallelSite algebra parallelRawCostalks

parallelCompositionMismatchAlgebra :: FiniteCosheafAlgebra ParallelSite Int () ()
parallelCompositionMismatchAlgebra =
  parallelAlgebra $ \morphismValue value ->
    case (cmWitness morphismValue, value) of
      (ParallelACRight, 0) -> Right 1
      _ -> Right value

parallelAlgebra ::
  (CheckedMorphism ParallelObject ParallelMorphism -> Int -> Either () Int) ->
  FiniteCosheafAlgebra ParallelSite Int () ()
parallelAlgebra corestrictAction =
  FiniteCosheafAlgebra
    { fcaCorestrict = corestrictAction,
      fcaMismatches = \_objectValue leftValue rightValue -> [() | leftValue /= rightValue],
      fcaNormalize = \_objectValue value -> value
    }

data IdentityInnerObject
  = IdentityInnerA
  | IdentityInnerB
  deriving stock (Eq, Ord, Show)

data IdentityInnerMorphism
  = IdentityInnerId !IdentityInnerObject
  | IdentityInnerArrow
  | IdentityInnerComposite
  deriving stock (Eq, Ord, Show)

data IdentityInnerSite = IdentityInnerSite
  deriving stock (Eq, Show)

instance Site IdentityInnerSite where
  type SiteObject IdentityInnerSite = IdentityInnerObject
  type SiteMorphism IdentityInnerSite = IdentityInnerMorphism

  siteObjects _ =
    [IdentityInnerA, IdentityInnerB]

  siteMorphisms _ =
    [identityInnerArrow]

  identityAt _ objectValue =
    CheckedMorphism objectValue objectValue (IdentityInnerId objectValue)

  coversAt _ _ =
    []

  composeChecked _ outerMorphism innerMorphism
    | cmSource outerMorphism /= cmTarget innerMorphism =
        Nothing
    | identityInnerIsIdentity outerMorphism =
        Just innerMorphism
    | identityInnerIsIdentity innerMorphism && cmWitness outerMorphism == IdentityInnerArrow =
        Just identityInnerComposite
    | identityInnerIsIdentity innerMorphism =
        Just outerMorphism
    | otherwise =
        Nothing

  pullbackPair _ _ _ =
    Nothing

testThinValidationBasisCoversIdentityInnerPairs :: Assertion
testThinValidationBasisCoversIdentityInnerPairs =
  case identityInnerCosheaf identityInnerCompositionMismatchAlgebra of
    Left (FiniteCorestrictionCompositionMismatch _ _ _ _ _) ->
      pure ()
    Left otherFailure ->
      assertFailure ("unexpected failure: " <> show otherFailure)
    Right _ ->
      assertFailure "expected identity-inner composition mismatch"

identityInnerArrow :: CheckedMorphism IdentityInnerObject IdentityInnerMorphism
identityInnerArrow =
  CheckedMorphism IdentityInnerA IdentityInnerB IdentityInnerArrow

identityInnerComposite :: CheckedMorphism IdentityInnerObject IdentityInnerMorphism
identityInnerComposite =
  CheckedMorphism IdentityInnerA IdentityInnerB IdentityInnerComposite

identityInnerIsIdentity :: CheckedMorphism IdentityInnerObject IdentityInnerMorphism -> Bool
identityInnerIsIdentity morphismValue =
  case cmWitness morphismValue of
    IdentityInnerId _ -> True
    _ -> False

identityInnerRawCostalks :: Map.Map IdentityInnerObject [Int]
identityInnerRawCostalks =
  Map.fromList
    [ (IdentityInnerA, [0]),
      (IdentityInnerB, [0, 1])
    ]

identityInnerCosheaf ::
  FiniteCosheafAlgebra IdentityInnerSite Int () () ->
  Either (FiniteCosheafFailure IdentityInnerObject IdentityInnerMorphism Int () ()) (FiniteCosheaf IdentityInnerSite Int)
identityInnerCosheaf algebra =
  mkFiniteCosheaf IdentityInnerSite algebra identityInnerRawCostalks

identityInnerCompositionMismatchAlgebra :: FiniteCosheafAlgebra IdentityInnerSite Int () ()
identityInnerCompositionMismatchAlgebra =
  identityInnerAlgebra $ \morphismValue value ->
    case (cmWitness morphismValue, value) of
      (IdentityInnerComposite, 0) -> Right 1
      _ -> Right value

identityInnerAlgebra ::
  (CheckedMorphism IdentityInnerObject IdentityInnerMorphism -> Int -> Either () Int) ->
  FiniteCosheafAlgebra IdentityInnerSite Int () ()
identityInnerAlgebra corestrictAction =
  FiniteCosheafAlgebra
    { fcaCorestrict = corestrictAction,
      fcaMismatches = \_objectValue leftValue rightValue -> [() | leftValue /= rightValue],
      fcaNormalize = \_objectValue value -> value
    }

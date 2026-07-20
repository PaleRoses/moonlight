{-# LANGUAGE TypeFamilies #-}

module Moonlight.Cosheaf.Test.Fixture
  ( ChainObject (..),
    ChainMorphism (..),
    ChainSite (..),
    ChainSiteMode (..),
    ChainCoreFailure (..),
    chainAB,
    chainBC,
    chainAC,
    chainGhostToA,
    chainAToGhost,
    chainRawCostalks,
    chainGoodAlgebra,
    chainIdentityMismatchAlgebra,
    chainCompositionMismatchAlgebra,
    chainCoreFailureAlgebra,
    chainCosheaf,
    chainCorestrictValue,
    chainMismatch,
    CoverObject (..),
    CoverMorphism (..),
    CoverSite (..),
    CoverSiteMode (..),
    CoverAlgebraMode (..),
    coverLeftToRoot,
    coverRightToRoot,
    coverSyntheticLeftToRoot,
    coverOverlapToLeft,
    coverOverlapToRight,
    coverOverlapToRootViaLeft,
    coverOverlapToRootViaRight,
    coverFamily,
    coverFamilyWithSyntheticLeft,
    coverRawCostalks,
    coverRawCostalksWithSingletonRoot,
    coverCosheaf,
    coverClassLabel,
    coverRepresentativeCost,
    coverTransitionCost,
    coverTropicalCostModel,
    twoWaySite,
    twoWayCosheaf,
    twoWayNegativeCostModel,
    expectRight,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Cosheaf
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism (..),
    CoveringFamily,
    PullbackSquare (..),
    Site (..),
    coveringFamilyFromTargetedWitnesses,
  )
import Test.Tasty.HUnit
  ( assertFailure,
  )

-- | A three-object covariant chain used to prove the raw finite cosheaf laws.
data ChainObject
  = ChainA
  | ChainB
  | ChainC
  | ChainGhost
  deriving stock (Eq, Ord, Show)

data ChainMorphism
  = ChainId !ChainObject
  | ChainAB
  | ChainBC
  | ChainAC
  | ChainGhostToA
  | ChainAToGhost
  deriving stock (Eq, Ord, Show)

data ChainSiteMode
  = ChainGoodSite
  | ChainMissingCompositeSite
  | ChainUnknownSourceSite
  | ChainUnknownTargetSite
  deriving stock (Eq, Ord, Show)

newtype ChainSite = ChainSite
  { chainSiteMode :: ChainSiteMode
  }
  deriving stock (Eq, Ord, Show)

data ChainCoreFailure = ChainCoreFailure
  deriving stock (Eq, Ord, Show)

instance Site ChainSite where
  type SiteObject ChainSite = ChainObject
  type SiteMorphism ChainSite = ChainMorphism

  siteObjects _ =
    [ChainA, ChainB, ChainC]

  siteMorphisms site =
    case chainSiteMode site of
      ChainGoodSite ->
        [chainAB, chainBC, chainAC]
      ChainMissingCompositeSite ->
        [chainAB, chainBC, chainAC]
      ChainUnknownSourceSite ->
        [chainGhostToA]
      ChainUnknownTargetSite ->
        [chainAToGhost]

  identityAt _ objectValue =
    CheckedMorphism objectValue objectValue (ChainId objectValue)

  coversAt _ _ =
    []

  composeChecked site outerMorphism innerMorphism
    | cmSource outerMorphism /= cmTarget innerMorphism =
        Nothing
    | chainIsIdentity outerMorphism =
        Just innerMorphism
    | chainIsIdentity innerMorphism =
        Just outerMorphism
    | chainSiteMode site == ChainMissingCompositeSite
        && cmWitness outerMorphism == ChainBC
        && cmWitness innerMorphism == ChainAB =
        Nothing
    | cmWitness outerMorphism == ChainBC && cmWitness innerMorphism == ChainAB =
        Just chainAC
    | otherwise =
        Nothing

  pullbackPair _ _ _ =
    Nothing

chainAB :: CheckedMorphism ChainObject ChainMorphism
chainAB =
  CheckedMorphism ChainA ChainB ChainAB

chainBC :: CheckedMorphism ChainObject ChainMorphism
chainBC =
  CheckedMorphism ChainB ChainC ChainBC

chainAC :: CheckedMorphism ChainObject ChainMorphism
chainAC =
  CheckedMorphism ChainA ChainC ChainAC

chainGhostToA :: CheckedMorphism ChainObject ChainMorphism
chainGhostToA =
  CheckedMorphism ChainGhost ChainA ChainGhostToA

chainAToGhost :: CheckedMorphism ChainObject ChainMorphism
chainAToGhost =
  CheckedMorphism ChainA ChainGhost ChainAToGhost

chainRawCostalks :: Map ChainObject [Int]
chainRawCostalks =
  Map.fromList
    [ (ChainA, [0, 1]),
      (ChainB, [10, 11]),
      (ChainC, [100, 101])
    ]

chainGoodAlgebra :: FiniteCosheafAlgebra ChainSite Int () ChainCoreFailure
chainGoodAlgebra =
  chainAlgebra chainCorestrictValue

chainIdentityMismatchAlgebra :: FiniteCosheafAlgebra ChainSite Int () ChainCoreFailure
chainIdentityMismatchAlgebra =
  chainAlgebra $ \morphismValue value ->
    case (cmWitness morphismValue, value) of
      (ChainId ChainA, 0) -> Right 1
      _ -> chainCorestrictValue morphismValue value

chainCompositionMismatchAlgebra :: FiniteCosheafAlgebra ChainSite Int () ChainCoreFailure
chainCompositionMismatchAlgebra =
  chainAlgebra $ \morphismValue value ->
    case (cmWitness morphismValue, value) of
      (ChainAC, 0) -> Right 101
      _ -> chainCorestrictValue morphismValue value

chainCoreFailureAlgebra :: FiniteCosheafAlgebra ChainSite Int () ChainCoreFailure
chainCoreFailureAlgebra =
  chainAlgebra $ \morphismValue value ->
    case (cmWitness morphismValue, value) of
      (ChainAB, 0) -> Left ChainCoreFailure
      _ -> chainCorestrictValue morphismValue value

chainCosheaf ::
  ChainSite ->
  FiniteCosheafAlgebra ChainSite Int () ChainCoreFailure ->
  Either (FiniteCosheafFailure ChainObject ChainMorphism Int () ChainCoreFailure) (FiniteCosheaf ChainSite Int)
chainCosheaf site algebra =
  mkFiniteCosheaf site algebra chainRawCostalks

chainCorestrictValue :: CheckedMorphism ChainObject ChainMorphism -> Int -> Either ChainCoreFailure Int
chainCorestrictValue morphismValue value =
  case cmWitness morphismValue of
    ChainId _ -> Right value
    ChainAB -> lookupCore [(0, 10), (1, 11)] value
    ChainBC -> lookupCore [(10, 100), (11, 101)] value
    ChainAC -> lookupCore [(0, 100), (1, 101)] value
    ChainGhostToA -> Left ChainCoreFailure
    ChainAToGhost -> Left ChainCoreFailure

chainMismatch :: ChainObject -> Int -> Int -> [()]
chainMismatch _ leftValue rightValue =
  [() | leftValue /= rightValue]

chainAlgebra ::
  (CheckedMorphism ChainObject ChainMorphism -> Int -> Either ChainCoreFailure Int) ->
  FiniteCosheafAlgebra ChainSite Int () ChainCoreFailure
chainAlgebra corestrictAction =
  FiniteCosheafAlgebra
    { fcaCorestrict = corestrictAction,
      fcaMismatches = chainMismatch,
      fcaNormalize = \_objectValue value -> value
    }

chainIsIdentity :: CheckedMorphism ChainObject ChainMorphism -> Bool
chainIsIdentity morphismValue =
  case cmWitness morphismValue of
    ChainId _ -> True
    _ -> False

-- | A finite cover square with a real overlap and two quotient classes.
data CoverObject
  = CoverRoot
  | CoverLeft
  | CoverRight
  | CoverOverlap
  deriving stock (Eq, Ord, Show)

data CoverMorphism
  = CoverId !CoverObject
  | CoverLeftToRoot
  | CoverRightToRoot
  | CoverSyntheticLeftToRoot
  | CoverOverlapToLeft
  | CoverOverlapToRight
  | CoverOverlapToRootViaLeft
  | CoverOverlapToRootViaRight
  deriving stock (Eq, Ord, Show)

data CoverSiteMode
  = CoverGoodSite
  | CoverMissingPullbackSite
  | CoverMissingCompiledCoverCorestrictionSite
  deriving stock (Eq, Ord, Show)

newtype CoverSite = CoverSite
  { coverSiteMode :: CoverSiteMode
  }
  deriving stock (Eq, Ord, Show)

data CoverAlgebraMode
  = CoverGoodAlgebra
  | CoverConflictAlgebra
  | CoverNonSurjectiveAlgebra
  | CoverNonInjectiveAlgebra
  deriving stock (Eq, Ord, Show)

instance Site CoverSite where
  type SiteObject CoverSite = CoverObject
  type SiteMorphism CoverSite = CoverMorphism

  siteObjects _ =
    [CoverRoot, CoverLeft, CoverRight, CoverOverlap]

  siteMorphisms site =
    case coverSiteMode site of
      CoverGoodSite -> coverSiteMorphisms
      CoverMissingPullbackSite -> coverSiteMorphisms
      CoverMissingCompiledCoverCorestrictionSite -> coverSiteMorphisms

  identityAt _ objectValue =
    CheckedMorphism objectValue objectValue (CoverId objectValue)

  coversAt _ objectValue =
    case objectValue of
      CoverRoot -> [coverFamily]
      _ -> []

  composeChecked _ outerMorphism innerMorphism
    | cmSource outerMorphism /= cmTarget innerMorphism =
        Nothing
    | coverIsIdentity outerMorphism =
        Just innerMorphism
    | coverIsIdentity innerMorphism =
        Just outerMorphism
    | cmWitness outerMorphism == CoverLeftToRoot && cmWitness innerMorphism == CoverOverlapToLeft =
        Just coverOverlapToRootViaLeft
    | cmWitness outerMorphism == CoverRightToRoot && cmWitness innerMorphism == CoverOverlapToRight =
        Just coverOverlapToRootViaRight
    | otherwise =
        Nothing

  pullbackPair site leftMorphism rightMorphism
    | coverSiteMode site == CoverMissingPullbackSite =
        Nothing
    | cmWitness leftMorphism == CoverLeftToRoot && cmWitness rightMorphism == CoverRightToRoot =
        Just leftRightPullback
    | cmWitness leftMorphism == CoverSyntheticLeftToRoot && cmWitness rightMorphism == CoverRightToRoot =
        Just syntheticLeftRightPullback
    | otherwise =
        Nothing

coverSiteMorphisms :: [CheckedMorphism CoverObject CoverMorphism]
coverSiteMorphisms =
  [ coverLeftToRoot,
    coverRightToRoot,
    coverOverlapToLeft,
    coverOverlapToRight,
    coverOverlapToRootViaLeft,
    coverOverlapToRootViaRight
  ]

coverLeftToRoot :: CheckedMorphism CoverObject CoverMorphism
coverLeftToRoot =
  CheckedMorphism CoverLeft CoverRoot CoverLeftToRoot

coverRightToRoot :: CheckedMorphism CoverObject CoverMorphism
coverRightToRoot =
  CheckedMorphism CoverRight CoverRoot CoverRightToRoot

coverSyntheticLeftToRoot :: CheckedMorphism CoverObject CoverMorphism
coverSyntheticLeftToRoot =
  CheckedMorphism CoverLeft CoverRoot CoverSyntheticLeftToRoot

coverOverlapToLeft :: CheckedMorphism CoverObject CoverMorphism
coverOverlapToLeft =
  CheckedMorphism CoverOverlap CoverLeft CoverOverlapToLeft

coverOverlapToRight :: CheckedMorphism CoverObject CoverMorphism
coverOverlapToRight =
  CheckedMorphism CoverOverlap CoverRight CoverOverlapToRight

coverOverlapToRootViaLeft :: CheckedMorphism CoverObject CoverMorphism
coverOverlapToRootViaLeft =
  CheckedMorphism CoverOverlap CoverRoot CoverOverlapToRootViaLeft

coverOverlapToRootViaRight :: CheckedMorphism CoverObject CoverMorphism
coverOverlapToRootViaRight =
  CheckedMorphism CoverOverlap CoverRoot CoverOverlapToRootViaRight

coverFamily :: CoveringFamily CoverObject CoverMorphism
coverFamily =
  coveringFamilyFromTargetedWitnesses
    CoverRoot
    ((CoverLeft, CoverLeftToRoot) :| [(CoverRight, CoverRightToRoot)])

coverFamilyWithSyntheticLeft :: CoveringFamily CoverObject CoverMorphism
coverFamilyWithSyntheticLeft =
  coveringFamilyFromTargetedWitnesses
    CoverRoot
    ((CoverLeft, CoverSyntheticLeftToRoot) :| [(CoverRight, CoverRightToRoot)])

coverRawCostalks :: Map CoverObject [Int]
coverRawCostalks =
  Map.fromList
    [ (CoverRoot, [100, 101]),
      (CoverLeft, [10, 11]),
      (CoverRight, [20, 21]),
      (CoverOverlap, [0, 1])
    ]

coverRawCostalksWithSingletonRoot :: Map CoverObject [Int]
coverRawCostalksWithSingletonRoot =
  Map.insert CoverRoot [100] coverRawCostalks

coverCosheaf ::
  CoverSite ->
  CoverAlgebraMode ->
  Map CoverObject [Int] ->
  Either (FiniteCosheafFailure CoverObject CoverMorphism Int () ChainCoreFailure) (FiniteCosheaf CoverSite Int)
coverCosheaf site algebraMode rawCostalks =
  mkFiniteCosheaf site (coverAlgebra algebraMode) rawCostalks

coverClassLabel :: CosectionRepresentative CoverObject Int -> Int
coverClassLabel representativeValue =
  case cosectionRepValue representativeValue of
    0 -> 0
    10 -> 0
    20 -> 0
    100 -> 0
    _ -> 1

coverRepresentativeCost :: CosectionRepresentative CoverObject Int -> MinPlusWeight
coverRepresentativeCost representativeValue =
  case cosectionRepObject representativeValue of
    CoverRoot -> MinPlusFinite (20 + fromIntegral (coverClassLabel representativeValue))
    CoverLeft -> MinPlusFinite (1 + fromIntegral (coverClassLabel representativeValue))
    CoverRight -> MinPlusFinite (7 + fromIntegral (coverClassLabel representativeValue))
    CoverOverlap -> MinPlusFinite (4 + fromIntegral (coverClassLabel representativeValue))

coverTransitionCost :: TropicalTransition CoverObject CoverMorphism Int -> MinPlusWeight
coverTransitionCost transitionValue =
  case cmWitness (tropicalTransitionMorphism transitionValue) of
    CoverLeftToRoot -> MinPlusFinite 5
    CoverRightToRoot -> MinPlusFinite 2
    CoverOverlapToLeft -> MinPlusFinite 3
    CoverOverlapToRight -> MinPlusFinite 1
    CoverOverlapToRootViaLeft -> MinPlusFinite 9
    CoverOverlapToRootViaRight -> MinPlusFinite 4
    CoverId _ -> minPlusOne
    CoverSyntheticLeftToRoot -> MinPlusFinite 13

coverTropicalCostModel :: TropicalCostModel CoverSite Int
coverTropicalCostModel =
  TropicalCostModel
    { tcmRepresentativeCost = Right . coverRepresentativeCost,
      tcmTransitionCost = Right . coverTransitionCost
    }

leftRightPullback :: PullbackSquare CoverObject CoverMorphism
leftRightPullback =
  PullbackSquare
    { psLeftBase = coverLeftToRoot,
      psRightBase = coverRightToRoot,
      psApex = CoverOverlap,
      psToLeft = coverOverlapToLeft,
      psToRight = coverOverlapToRight
    }

syntheticLeftRightPullback :: PullbackSquare CoverObject CoverMorphism
syntheticLeftRightPullback =
  PullbackSquare
    { psLeftBase = coverSyntheticLeftToRoot,
      psRightBase = coverRightToRoot,
      psApex = CoverOverlap,
      psToLeft = coverOverlapToLeft,
      psToRight = coverOverlapToRight
    }

coverAlgebra :: CoverAlgebraMode -> FiniteCosheafAlgebra CoverSite Int () ChainCoreFailure
coverAlgebra algebraMode =
  FiniteCosheafAlgebra
    { fcaCorestrict = coverCorestrictValue algebraMode,
      fcaMismatches = \_objectValue leftValue rightValue -> [() | leftValue /= rightValue],
      fcaNormalize = \_objectValue value -> value
    }

coverCorestrictValue :: CoverAlgebraMode -> CheckedMorphism CoverObject CoverMorphism -> Int -> Either ChainCoreFailure Int
coverCorestrictValue algebraMode morphismValue value =
  case cmWitness morphismValue of
    CoverId _ -> Right value
    CoverOverlapToLeft -> lookupCore [(0, 10), (1, 11)] value
    CoverOverlapToRight -> lookupCore [(0, 20), (1, 21)] value
    CoverLeftToRoot -> leftToRootValue algebraMode value
    CoverRightToRoot -> rightToRootValue algebraMode value
    CoverOverlapToRootViaLeft -> overlapToRootViaLeftValue algebraMode value
    CoverOverlapToRootViaRight -> overlapToRootViaRightValue algebraMode value
    CoverSyntheticLeftToRoot -> leftToRootValue algebraMode value

leftToRootValue :: CoverAlgebraMode -> Int -> Either ChainCoreFailure Int
leftToRootValue algebraMode value =
  case algebraMode of
    CoverGoodAlgebra -> lookupCore [(10, 100), (11, 101)] value
    CoverConflictAlgebra -> lookupCore [(10, 100), (11, 101)] value
    CoverNonSurjectiveAlgebra -> lookupCore [(10, 100), (11, 100)] value
    CoverNonInjectiveAlgebra -> lookupCore [(10, 100), (11, 100)] value

rightToRootValue :: CoverAlgebraMode -> Int -> Either ChainCoreFailure Int
rightToRootValue algebraMode value =
  case algebraMode of
    CoverGoodAlgebra -> lookupCore [(20, 100), (21, 101)] value
    CoverConflictAlgebra -> lookupCore [(20, 101), (21, 101)] value
    CoverNonSurjectiveAlgebra -> lookupCore [(20, 100), (21, 100)] value
    CoverNonInjectiveAlgebra -> lookupCore [(20, 100), (21, 100)] value

overlapToRootViaLeftValue :: CoverAlgebraMode -> Int -> Either ChainCoreFailure Int
overlapToRootViaLeftValue algebraMode value =
  case algebraMode of
    CoverGoodAlgebra -> lookupCore [(0, 100), (1, 101)] value
    CoverConflictAlgebra -> lookupCore [(0, 100), (1, 101)] value
    CoverNonSurjectiveAlgebra -> lookupCore [(0, 100), (1, 100)] value
    CoverNonInjectiveAlgebra -> lookupCore [(0, 100), (1, 100)] value

overlapToRootViaRightValue :: CoverAlgebraMode -> Int -> Either ChainCoreFailure Int
overlapToRootViaRightValue algebraMode value =
  case algebraMode of
    CoverGoodAlgebra -> lookupCore [(0, 100), (1, 101)] value
    CoverConflictAlgebra -> lookupCore [(0, 101), (1, 101)] value
    CoverNonSurjectiveAlgebra -> lookupCore [(0, 100), (1, 100)] value
    CoverNonInjectiveAlgebra -> lookupCore [(0, 100), (1, 100)] value

coverIsIdentity :: CheckedMorphism CoverObject CoverMorphism -> Bool
coverIsIdentity morphismValue =
  case cmWitness morphismValue of
    CoverId _ -> True
    _ -> False

lookupCore :: [(Int, Int)] -> Int -> Either ChainCoreFailure Int
lookupCore pairs value =
  maybe (Left ChainCoreFailure) Right (Map.lookup value (Map.fromList pairs))

-- | A lawful two-way site whose negative tropical transition costs form a cycle.
data TwoWayObject
  = TwoWayLeft
  | TwoWayRight
  deriving stock (Eq, Ord, Show)

data TwoWayMorphism
  = TwoWayId !TwoWayObject
  | TwoWayForward
  | TwoWayBackward
  deriving stock (Eq, Ord, Show)

data TwoWaySite = TwoWaySite
  deriving stock (Eq, Ord, Show)

instance Site TwoWaySite where
  type SiteObject TwoWaySite = TwoWayObject
  type SiteMorphism TwoWaySite = TwoWayMorphism

  siteObjects _ =
    [TwoWayLeft, TwoWayRight]

  siteMorphisms _ =
    [twoWayForward, twoWayBackward]

  identityAt _ objectValue =
    CheckedMorphism objectValue objectValue (TwoWayId objectValue)

  coversAt _ _ =
    []

  composeChecked _ outerMorphism innerMorphism
    | cmSource outerMorphism /= cmTarget innerMorphism =
        Nothing
    | twoWayIsIdentity outerMorphism =
        Just innerMorphism
    | twoWayIsIdentity innerMorphism =
        Just outerMorphism
    | cmWitness outerMorphism == TwoWayBackward && cmWitness innerMorphism == TwoWayForward =
        Just (identityAt TwoWaySite TwoWayLeft)
    | cmWitness outerMorphism == TwoWayForward && cmWitness innerMorphism == TwoWayBackward =
        Just (identityAt TwoWaySite TwoWayRight)
    | otherwise =
        Nothing

  pullbackPair _ _ _ =
    Nothing

twoWaySite :: TwoWaySite
twoWaySite =
  TwoWaySite

twoWayForward :: CheckedMorphism TwoWayObject TwoWayMorphism
twoWayForward =
  CheckedMorphism TwoWayLeft TwoWayRight TwoWayForward

twoWayBackward :: CheckedMorphism TwoWayObject TwoWayMorphism
twoWayBackward =
  CheckedMorphism TwoWayRight TwoWayLeft TwoWayBackward

twoWayCosheaf :: Either (FiniteCosheafFailure TwoWayObject TwoWayMorphism Int () ChainCoreFailure) (FiniteCosheaf TwoWaySite Int)
twoWayCosheaf =
  mkFiniteCosheaf
    TwoWaySite
    FiniteCosheafAlgebra
      { fcaCorestrict = \_morphism value -> Right value,
        fcaMismatches = \_objectValue leftValue rightValue -> [() | leftValue /= rightValue],
        fcaNormalize = \_objectValue value -> value
      }
    (Map.fromList [(TwoWayLeft, [0]), (TwoWayRight, [0])])

twoWayNegativeCostModel :: TropicalCostModel TwoWaySite Int
twoWayNegativeCostModel =
  TropicalCostModel
    { tcmRepresentativeCost = \_representative -> Right minPlusOne,
      tcmTransitionCost = \transitionValue ->
        case cmWitness (tropicalTransitionMorphism transitionValue) of
          TwoWayForward -> Right (MinPlusFinite (-1))
          TwoWayBackward -> Right (MinPlusFinite (-1))
          TwoWayId _ -> Right minPlusOne
    }

twoWayIsIdentity :: CheckedMorphism TwoWayObject TwoWayMorphism -> Bool
twoWayIsIdentity morphismValue =
  case cmWitness morphismValue of
    TwoWayId _ -> True
    _ -> False

expectRight :: (Show left) => Either left right -> IO right
expectRight result =
  case result of
    Left failureValue ->
      assertFailure (show failureValue)
    Right value ->
      pure value

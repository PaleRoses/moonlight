{-# LANGUAGE TypeFamilies #-}

module Moonlight.Cosheaf.Homology.CoverSpec
  ( tests,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Moonlight.Cosheaf
  ( CoverChainFailure (..),
    CoverChainSpec (..),
    CoverFace (..),
    CoverIntersectionCell (..),
    CoverNervePlan (..),
    chaGroupsByDegree,
    coverHomology,
    coverNervePlanFromEffectiveCoverPlan,
    intCoefficientOps,
    rationalCoefficientOps,
  )
import Moonlight.Homology
  ( HomologicalDegree (..),
    HomologyBackend (..),
    HomologyGroup (..),
    identityBoundaryIncidenceOf,
  )
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism (..),
    PullbackSquare (..),
    Site (..),
    mkCoveringFamily,
  )
import Moonlight.Sheaf.Site.Plan
  ( CoverSlotKey (..),
    EffectiveCoverPlan,
    prepareEffectiveCoverPlan,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( Assertion,
    assertEqual,
    assertFailure,
    testCase,
  )

tests :: TestTree
tests =
  testGroup
    "cover homology"
    [ testCase "degree-one effective cover nerve has H0 of the coequalizer interval" testIntervalCoverH0,
      testCase "higher automatic cover intersections report a typed obstruction" testHigherEffectiveCoverObstruction,
      testCase "explicit unfilled cover cycle has nonzero H1" testExplicitCoverCycleH1
    ]

testIntervalCoverH0 :: Assertion
testIntervalCoverH0 = do
  effectivePlan <- intervalEffectiveCoverPlan
  nervePlan <- expectRight (coverNerveFromEffective 1 effectivePlan)
  artifact <- expectRight (coverHomology IntegralSmithBackend intCoefficientOps (rankOneCoverSpec nervePlan))
  assertHomologyRank "H0 of interval cover" 0 1 (chaGroupsByDegree artifact)
  assertHomologyRank "H1 of interval cover" 1 0 (chaGroupsByDegree artifact)

testHigherEffectiveCoverObstruction :: Assertion
testHigherEffectiveCoverObstruction = do
  effectivePlan <- intervalEffectiveCoverPlan
  case coverNerveFromEffective 2 effectivePlan of
    Left (CoverChainHigherIntersectionsRequireExplicitFaceProjections 2) -> pure ()
    Left otherFailure -> assertFailure ("unexpected obstruction: " <> show otherFailure)
    Right _ -> assertFailure "expected explicit-projection obstruction for automatic degree-two cover nerve"

testExplicitCoverCycleH1 :: Assertion
testExplicitCoverCycleH1 = do
  nervePlan <- explicitCycleNervePlan
  artifact <- expectRight (coverHomology RationalRankBackend rationalCoefficientOps (rankOneCoverSpec nervePlan))
  assertHomologyRank "H0 of connected cover cycle" 0 1 (chaGroupsByDegree artifact)
  assertHomologyRank "H1 of unfilled cover cycle" 1 1 (chaGroupsByDegree artifact)

coverNerveFromEffective ::
  Int ->
  EffectiveCoverPlan CoverObject CoverMorphism ->
  Either (CoverChainFailure CoverObject CoverMorphism Int ()) (CoverNervePlan CoverObject CoverMorphism)
coverNerveFromEffective =
  coverNervePlanFromEffectiveCoverPlan

rankOneCoverSpec ::
  Num coefficient =>
  CoverNervePlan CoverObject CoverMorphism ->
  CoverChainSpec CoverObject CoverMorphism coefficient (CoverFace CoverObject CoverMorphism, Int, Int, coefficient) ()
rankOneCoverSpec nervePlan =
  CoverChainSpec
    { ccsNervePlan = nervePlan,
      ccsCostalkDimension = const 1,
      ccsCorestrictionBlock = const (Right (identityBoundaryIncidenceOf 1)),
      ccsEntryProvenance = \face sourceLocal targetLocal coefficient -> (face, sourceLocal, targetLocal, coefficient)
    }

intervalEffectiveCoverPlan :: IO (EffectiveCoverPlan CoverObject CoverMorphism)
intervalEffectiveCoverPlan = do
  cover <- expectRight (mkCoveringFamily Root (u0ToRoot :| [u1ToRoot]))
  expectRight (prepareEffectiveCoverPlan CoverSite cover)

explicitCycleNervePlan :: IO (CoverNervePlan CoverObject CoverMorphism)
explicitCycleNervePlan = do
  cover <- expectRight (mkCoveringFamily Root (u0ToRoot :| [u1ToRoot, u2ToRoot]))
  effectivePlan <- expectRight (prepareEffectiveCoverPlan CoverSite cover)
  let vertices = [cell0, cell1, cell2]
      edges = [edge01, edge12, edge02]
  pure
    CoverNervePlan
      { cnpEffectiveCoverPlan = effectivePlan,
        cnpMaxDegree = HomologicalDegree 1,
        cnpCellsByDegree = Map.fromList [(HomologicalDegree 0, vertices), (HomologicalDegree 1, edges)],
        cnpFaces = edgeFaces edge01 slot0 cell1 slot1 cell0 <> edgeFaces edge12 slot1 cell2 slot2 cell1 <> edgeFaces edge02 slot0 cell2 slot2 cell0
      }

edgeFaces ::
  CoverIntersectionCell CoverObject CoverMorphism ->
  CoverSlotKey ->
  CoverIntersectionCell CoverObject CoverMorphism ->
  CoverSlotKey ->
  CoverIntersectionCell CoverObject CoverMorphism ->
  [CoverFace CoverObject CoverMorphism]
edgeFaces edge leftSlot rightCell rightSlot leftCell =
  [ CoverFace
      { coverFaceSource = edge,
        coverFaceTarget = rightCell,
        coverFaceDroppedSlot = leftSlot,
        coverFaceDroppedOffset = 0,
        coverFaceProjection = Nothing
      },
    CoverFace
      { coverFaceSource = edge,
        coverFaceTarget = leftCell,
        coverFaceDroppedSlot = rightSlot,
        coverFaceDroppedOffset = 1,
        coverFaceProjection = Nothing
      }
  ]

assertHomologyRank :: String -> Int -> Int -> IntMap.IntMap (HomologyGroup coefficient) -> Assertion
assertHomologyRank label degree expectedRank groups =
  case IntMap.lookup degree groups of
    Nothing -> assertFailure (label <> ": missing group")
    Just groupValue -> assertEqual label expectedRank (freeRank groupValue)

expectRight :: Show failure => Either failure value -> IO value
expectRight result =
  case result of
    Right value -> pure value
    Left failure -> assertFailure ("unexpected failure: " <> show failure)

data CoverObject
  = Root
  | U0
  | U1
  | U2
  | U01
  | U12
  | U02
  deriving stock (Eq, Ord, Show, Read)

data CoverMorphism = CoverMorphism CoverObject CoverObject
  deriving stock (Eq, Ord, Show, Read)

data CoverSite = CoverSite
  deriving stock (Eq, Ord, Show, Read)

instance Site CoverSite where
  type SiteObject CoverSite = CoverObject
  type SiteMorphism CoverSite = CoverMorphism

  siteObjects _ = [Root, U0, U1, U2, U01, U12, U02]

  siteMorphisms _ = []

  identityAt _ objectValue = coverArrow objectValue objectValue

  coversAt _ _ = []

  composeChecked _ outer inner
    | cmTarget inner == cmSource outer =
        Just (coverArrow (cmSource inner) (cmTarget outer))
    | otherwise =
        Nothing

  pullbackPair _ left right
    | cmTarget left /= cmTarget right =
        Nothing
    | otherwise = do
        apex <- coverIntersection (cmSource left) (cmSource right)
        pure
          PullbackSquare
            { psLeftBase = left,
              psRightBase = right,
              psApex = apex,
              psToLeft = coverArrow apex (cmSource left),
              psToRight = coverArrow apex (cmSource right)
            }

slot0, slot1, slot2 :: CoverSlotKey
slot0 = CoverSlotKey 0
slot1 = CoverSlotKey 1
slot2 = CoverSlotKey 2

u0ToRoot, u1ToRoot, u2ToRoot :: CheckedMorphism CoverObject CoverMorphism
u0ToRoot = coverArrow U0 Root
u1ToRoot = coverArrow U1 Root
u2ToRoot = coverArrow U2 Root

coverArrow :: CoverObject -> CoverObject -> CheckedMorphism CoverObject CoverMorphism
coverArrow source target =
  CheckedMorphism source target (CoverMorphism source target)

coverIntersection :: CoverObject -> CoverObject -> Maybe CoverObject
coverIntersection left right
  | left == right = Just left
  | otherwise =
      case (left, right) of
        (U0, U1) -> Just U01
        (U1, U0) -> Just U01
        (U1, U2) -> Just U12
        (U2, U1) -> Just U12
        (U0, U2) -> Just U02
        (U2, U0) -> Just U02
        _ -> Nothing

cell0, cell1, cell2, edge01, edge12, edge02 :: CoverIntersectionCell CoverObject CoverMorphism
cell0 = CoverIntersectionCell (HomologicalDegree 0) (slot0 :| []) U0 Nothing
cell1 = CoverIntersectionCell (HomologicalDegree 0) (slot1 :| []) U1 Nothing
cell2 = CoverIntersectionCell (HomologicalDegree 0) (slot2 :| []) U2 Nothing
edge01 = CoverIntersectionCell (HomologicalDegree 1) (slot0 :| [slot1]) U01 Nothing
edge12 = CoverIntersectionCell (HomologicalDegree 1) (slot1 :| [slot2]) U12 Nothing
edge02 = CoverIntersectionCell (HomologicalDegree 1) (slot0 :| [slot2]) U02 Nothing

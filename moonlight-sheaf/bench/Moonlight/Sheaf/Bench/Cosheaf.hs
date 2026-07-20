{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Sheaf.Bench.Cosheaf
  ( finiteCosheafBenchmarks,
  )
where

import Control.DeepSeq (NFData (..))
import Control.Exception (evaluate)
import Test.Tasty.Bench (Benchmark, bench, bgroup, env, nf, whnf)
import Data.Bifunctor (first)
import Data.IntMap.Strict qualified as IntMap
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Moonlight.Cosheaf
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism (..),
    CoveringFamily,
    PullbackSquare (..),
    Site (..),
    mkCoveringFamily,
  )
import System.Environment (lookupEnv)

finiteCosheafBenchmarks :: IO Benchmark
finiteCosheafBenchmarks = do
  includeGiant <- isJust <$> lookupEnv "MOONLIGHT_COSHEAF_BENCH_ENABLE_GIANT"
  putStrLn (benchmarkNotice includeGiant)
  pure
    ( bgroup
        "cosheaf"
        [ bgroup "finite-cosheaf-construction" (fmap constructionBenchmark (chainCases includeGiant)),
          bgroup "finite-cosheaf-h0-colimit" (fmap colimitBenchmark (chainCases includeGiant)),
          bgroup "finite-cosheaf-cover-coequalizer" (fmap coverBenchmark (coverCases includeGiant)),
          bgroup "finite-cosheaf-tropical" (fmap tropicalBenchmark (chainCases includeGiant))
        ]
    )

benchmarkNotice :: Bool -> String
benchmarkNotice includeGiant =
  if includeGiant
    then "giant finite cosheaf benchmark cases enabled via MOONLIGHT_COSHEAF_BENCH_ENABLE_GIANT."
    else "giant finite cosheaf benchmark cases skipped by default. Set MOONLIGHT_COSHEAF_BENCH_ENABLE_GIANT=1 to opt in."

data BenchMeasurement
  = BenchChecksum !Int !Int !Int
  | BenchObstruction !String
  deriving stock (Show)

instance NFData BenchMeasurement where
  rnf measurement =
    case measurement of
      BenchChecksum leftValue middleValue rightValue -> rnf leftValue `seq` rnf middleValue `seq` rnf rightValue
      BenchObstruction failureMessage -> rnf failureMessage

data BenchChainCase = BenchChainCase
  { bccLabel :: !String,
    bccObjectCount :: !Int,
    bccCostalkCardinality :: !Int,
    bccQuotientClassCount :: !Int
  }
  deriving stock (Eq, Show)

data BenchCoverCase = BenchCoverCase
  { bvcLabel :: !String,
    bvcCoverArity :: !Int,
    bvcCostalkCardinality :: !Int
  }
  deriving stock (Eq, Show)

chainCases :: Bool -> [BenchChainCase]
chainCases includeGiant =
  [ BenchChainCase "objects-8/morphisms-36/costalk-8/classes-8" 8 8 8,
    BenchChainCase "objects-16/morphisms-136/costalk-16/classes-4" 16 16 4,
    BenchChainCase "objects-24/morphisms-300/costalk-16/classes-4" 24 16 4,
    BenchChainCase "objects-32/morphisms-528/costalk-16/classes-4" 32 16 4
  ]
    <> [BenchChainCase "objects-64/morphisms-2080/costalk-32/classes-8" 64 32 8 | includeGiant]

coverCases :: Bool -> [BenchCoverCase]
coverCases includeGiant =
  [ BenchCoverCase "cover-arity-2/overlaps-1/costalk-8" 2 8,
    BenchCoverCase "cover-arity-4/overlaps-6/costalk-12" 4 12
  ]
    <> [BenchCoverCase "cover-arity-8/overlaps-28/costalk-24" 8 24 | includeGiant]

ordinalRange :: Int -> [Int]
ordinalRange countValue =
  [0 .. max 0 (countValue - 1)]

ordinalPairs :: Int -> [(Int, Int)]
ordinalPairs countValue =
  [ (leftValue, rightValue)
  | leftValue <- ordinalRange countValue,
    rightValue <- [leftValue + 1 .. max 0 (countValue - 1)]
  ]

constructionBenchmark :: BenchChainCase -> Benchmark
constructionBenchmark benchmarkCase =
  bench (bccLabel benchmarkCase <> "/from-scratch") (nf constructionSummary benchmarkCase)

colimitBenchmark :: BenchChainCase -> Benchmark
colimitBenchmark benchmarkCase =
  env
    (prepareOrFail (buildBenchChainCosheaf benchmarkCase))
    (\cosheaf -> bench (bccLabel benchmarkCase <> "/prepared-cosheaf") (nf colimitSummary cosheaf))

coverBenchmark :: BenchCoverCase -> Benchmark
coverBenchmark benchmarkCase =
  env
    (prepareOrFail (buildBenchCoverFixture benchmarkCase))
    (\fixture -> bench (bvcLabel benchmarkCase <> "/prepared-cover") (nf coverSummary fixture))

tropicalBenchmark :: BenchChainCase -> Benchmark
tropicalBenchmark benchmarkCase =
  env
    (prepareOrFail (buildBenchChainColimit benchmarkCase))
    (\colimit -> bench (bccLabel benchmarkCase <> "/prepared-colimit") (whnf tropicalSummary colimit))

prepareOrFail :: Show failure => Either failure value -> IO value
prepareOrFail result =
  case result of
    Left failureValue -> fail (show failureValue)
    Right value -> evaluate value

constructionSummary :: BenchChainCase -> BenchMeasurement
constructionSummary benchmarkCase =
  either
    (BenchObstruction . show)
    finiteCosheafChecksum
    (buildBenchChainCosheaf benchmarkCase)

colimitSummary :: FiniteCosheaf BenchChainSite Int -> BenchMeasurement
colimitSummary cosheaf =
  either
    (BenchObstruction . show)
    colimitChecksum
    (fullBenchColimit cosheaf)

coverSummary :: BenchCoverFixture -> BenchMeasurement
coverSummary fixture =
  either
    (BenchObstruction . show)
    coverCoequalizerChecksum
    (coverCosheafCoequalizer (bcfCover fixture) (bcfCosheaf fixture))

tropicalSummary :: CosheafColimit BenchChainSite Int -> BenchMeasurement
tropicalSummary colimit =
  case fullBenchTropicalCostTable colimit benchChainTropicalCostModel of
    Left failureValue -> BenchObstruction (show failureValue)
    Right costTable ->
      either
        (BenchObstruction . show)
        tropicalPlanChecksum
        (planTropicalCosections costTable)

finiteCosheafChecksum :: FiniteCosheaf site value -> BenchMeasurement
finiteCosheafChecksum cosheaf =
  BenchChecksum
    (IntMap.size (fcCostalks cosheaf))
    (IntMap.size (fcCorestrictions cosheaf))
    (sum (fmap (IntMap.size . ccSourceToTarget) (finiteCosheafCorestrictions cosheaf)))

colimitChecksum :: CosheafColimit site value -> BenchMeasurement
colimitChecksum colimit =
  BenchChecksum
    (length (cosheafColimitRepresentatives colimit))
    (length (cosheafColimitClassKeys colimit))
    (sum (fmap cosectionClassKeyInt (cosheafColimitClassKeys colimit)))

coverCoequalizerChecksum :: CoverCosheafCoequalizer site value -> BenchMeasurement
coverCoequalizerChecksum coequalizerValue =
  BenchChecksum
    (IntMap.size (cccClassTargets coequalizerValue))
    (sum (fmap unCostalkKey (IntMap.elems (cccClassTargets coequalizerValue))))
    (length (IntMap.elems (cccClassTargets coequalizerValue)))

tropicalPlanChecksum :: TropicalCosectionPlan site value -> BenchMeasurement
tropicalPlanChecksum tropicalPlan =
  BenchChecksum
    (IntMap.size (tcpClassChoices tropicalPlan))
    (sum (fmap (cosectionRepKeyInt . tccRepresentativeKey) choices))
    (sum (fmap (minPlusWeightChecksum . tccCost) choices))
  where
    choices =
      IntMap.elems (tcpClassChoices tropicalPlan)


minPlusWeightChecksum :: MinPlusWeight -> Int
minPlusWeightChecksum weight =
  case weight of
    MinPlusFinite rationalValue ->
      floor rationalValue
    MinPlusInfinity ->
      0

buildBenchChainColimit :: BenchChainCase -> Either String (CosheafColimit BenchChainSite Int)
buildBenchChainColimit benchmarkCase = do
  cosheaf <- first show (buildBenchChainCosheaf benchmarkCase)
  fullBenchColimit cosheaf

fullBenchColimit ::
  FiniteCosheaf BenchChainSite Int ->
  Either String (CosheafColimit BenchChainSite Int)
fullBenchColimit cosheaf = do
  supportPlan <- first show (fullFiniteCosheafChainSupportPlan 1 cosheaf)
  first show (finiteCosheafColimitFromSupportPlan supportPlan cosheaf)

fullBenchTropicalCostTable ::
  CosheafColimit BenchChainSite Int ->
  TropicalCostModel BenchChainSite Int ->
  Either String (TropicalCostTable BenchChainSite Int)
fullBenchTropicalCostTable colimit costModel = do
  supportPlan <- first show (fullFiniteCosheafChainSupportPlan 1 (ccCosheaf colimit))
  first show (compileTropicalCostTableFromSupportPlan supportPlan colimit costModel)

buildBenchChainCosheaf ::
  BenchChainCase ->
  Either (FiniteCosheafFailure BenchObject BenchMorphism Int () ()) (FiniteCosheaf BenchChainSite Int)
buildBenchChainCosheaf benchmarkCase =
  mkFiniteCosheaf
    site
    (benchChainAlgebra benchmarkCase)
    (benchChainCostalks benchmarkCase)
  where
    site =
      BenchChainSite (bccObjectCount benchmarkCase)

benchChainCostalks :: BenchChainCase -> Map BenchObject [Int]
benchChainCostalks benchmarkCase =
  Map.fromList
    [ (BenchObject objectIndex, ordinalRange (bccCostalkCardinality benchmarkCase))
    | objectIndex <- ordinalRange (bccObjectCount benchmarkCase)
    ]

benchChainAlgebra :: BenchChainCase -> FiniteCosheafAlgebra BenchChainSite Int () ()
benchChainAlgebra benchmarkCase =
  FiniteCosheafAlgebra
    { fcaCorestrict = \morphismValue value ->
        case cmWitness morphismValue of
          BenchChainId _ -> Right value
          BenchChainArrow _ _ -> Right (value `mod` max 1 (bccQuotientClassCount benchmarkCase)),
      fcaMismatches = \_objectValue leftValue rightValue -> [() | leftValue /= rightValue],
      fcaNormalize = \_objectValue value -> value
    }

data BenchObject = BenchObject !Int
  deriving stock (Eq, Ord, Show)

data BenchMorphism
  = BenchChainId !BenchObject
  | BenchChainArrow !Int !Int
  deriving stock (Eq, Ord, Show)

newtype BenchChainSite = BenchChainSite
  { benchChainObjectCount :: Int
  }
  deriving stock (Eq, Ord, Show)

instance NFData (FiniteCosheaf BenchChainSite Int) where
  rnf =
    rnf . finiteCosheafChecksum

instance NFData (CosheafColimit BenchChainSite Int) where
  rnf =
    rnf . colimitChecksum

instance Site BenchChainSite where
  type SiteObject BenchChainSite = BenchObject
  type SiteMorphism BenchChainSite = BenchMorphism

  siteObjects site =
    fmap BenchObject (ordinalRange (benchChainObjectCount site))

  siteMorphisms site =
    [ CheckedMorphism (BenchObject sourceIndex) (BenchObject targetIndex) (BenchChainArrow sourceIndex targetIndex)
    | (sourceIndex, targetIndex) <- ordinalPairs (benchChainObjectCount site)
    ]

  identityAt _ objectValue =
    CheckedMorphism objectValue objectValue (BenchChainId objectValue)

  coversAt _ _ =
    []

  composeChecked _ outerMorphism innerMorphism
    | cmSource outerMorphism /= cmTarget innerMorphism =
        Nothing
    | benchChainIsIdentity outerMorphism =
        Just innerMorphism
    | benchChainIsIdentity innerMorphism =
        Just outerMorphism
    | otherwise =
        case (cmSource innerMorphism, cmTarget outerMorphism) of
          (BenchObject sourceIndex, BenchObject targetIndex) ->
            Just (CheckedMorphism (BenchObject sourceIndex) (BenchObject targetIndex) (BenchChainArrow sourceIndex targetIndex))

  pullbackPair _ _ _ =
    Nothing

benchChainIsIdentity :: CheckedMorphism BenchObject BenchMorphism -> Bool
benchChainIsIdentity morphismValue =
  case cmWitness morphismValue of
    BenchChainId _ -> True
    BenchChainArrow _ _ -> False

benchChainTropicalCostModel :: TropicalCostModel BenchChainSite Int
benchChainTropicalCostModel =
  TropicalCostModel
    { tcmRepresentativeCost = \representativeValue ->
        Right (MinPlusFinite (fromIntegral (cosectionRepValue representativeValue + benchObjectOrdinal (cosectionRepObject representativeValue)))),
      tcmTransitionCost = \transitionValue ->
        Right (MinPlusFinite (fromIntegral (benchTransitionSpan (tropicalTransitionMorphism transitionValue))))
    }

benchObjectOrdinal :: BenchObject -> Int
benchObjectOrdinal (BenchObject objectIndex) =
  objectIndex

benchTransitionSpan :: CheckedMorphism BenchObject BenchMorphism -> Int
benchTransitionSpan morphismValue =
  case cmWitness morphismValue of
    BenchChainId _ -> 0
    BenchChainArrow sourceIndex targetIndex -> max 0 (targetIndex - sourceIndex)

data BenchCoverFixture = BenchCoverFixture
  { bcfCover :: !(CoveringFamily BenchCoverObject BenchCoverMorphism),
    bcfCosheaf :: !(FiniteCosheaf BenchCoverSite Int)
  }

instance NFData BenchCoverFixture where
  rnf =
    rnf . finiteCosheafChecksum . bcfCosheaf

instance NFData (FiniteCosheaf BenchCoverSite Int) where
  rnf =
    rnf . finiteCosheafChecksum

buildBenchCoverFixture :: BenchCoverCase -> Either String BenchCoverFixture
buildBenchCoverFixture benchmarkCase = do
  coverValue <- benchmarkCoverFamily benchmarkCase
  cosheaf <- first show (buildBenchCoverCosheaf benchmarkCase coverValue)
  pure
    BenchCoverFixture
      { bcfCover = coverValue,
        bcfCosheaf = cosheaf
      }

buildBenchCoverCosheaf ::
  BenchCoverCase ->
  CoveringFamily BenchCoverObject BenchCoverMorphism ->
  Either (FiniteCosheafFailure BenchCoverObject BenchCoverMorphism Int () ()) (FiniteCosheaf BenchCoverSite Int)
buildBenchCoverCosheaf benchmarkCase coverValue =
  mkFiniteCosheaf
    (BenchCoverSite (bvcCoverArity benchmarkCase) coverValue)
    benchCoverAlgebra
    (benchCoverCostalks benchmarkCase)

benchmarkCoverFamily :: BenchCoverCase -> Either String (CoveringFamily BenchCoverObject BenchCoverMorphism)
benchmarkCoverFamily benchmarkCase =
  case benchmarkCoverArrows benchmarkCase of
    [] -> Left "empty benchmark cover"
    firstArrow : remainingArrows ->
      first show (mkCoveringFamily BenchCoverRoot (firstArrow :| remainingArrows))

benchmarkCoverArrows :: BenchCoverCase -> [CheckedMorphism BenchCoverObject BenchCoverMorphism]
benchmarkCoverArrows benchmarkCase =
  fmap benchLeafToRoot (ordinalRange (bvcCoverArity benchmarkCase))

benchCoverCostalks :: BenchCoverCase -> Map BenchCoverObject [Int]
benchCoverCostalks benchmarkCase =
  Map.fromList
    ( (BenchCoverRoot, costalkValues)
        : leafCostalks
          <> overlapCostalks
    )
  where
    costalkValues =
      ordinalRange (bvcCostalkCardinality benchmarkCase)

    leafCostalks =
      [ (BenchCoverLeaf leafIndex, costalkValues)
      | leafIndex <- ordinalRange (bvcCoverArity benchmarkCase)
      ]

    overlapCostalks =
      [ (BenchCoverOverlap leftIndex rightIndex, costalkValues)
      | (leftIndex, rightIndex) <- ordinalPairs (bvcCoverArity benchmarkCase)
      ]

benchCoverAlgebra :: FiniteCosheafAlgebra BenchCoverSite Int () ()
benchCoverAlgebra =
  FiniteCosheafAlgebra
    { fcaCorestrict = \_morphism value -> Right value,
      fcaMismatches = \_objectValue leftValue rightValue -> [() | leftValue /= rightValue],
      fcaNormalize = \_objectValue value -> value
    }

data BenchCoverObject
  = BenchCoverRoot
  | BenchCoverLeaf !Int
  | BenchCoverOverlap !Int !Int
  deriving stock (Eq, Ord, Show)

data BenchCoverMorphism
  = BenchCoverId !BenchCoverObject
  | BenchLeafToRoot !Int
  | BenchOverlapToLeaf !Int !Int !Int
  | BenchOverlapToRootViaLeaf !Int !Int !Int
  deriving stock (Eq, Ord, Show)

data BenchCoverSite = BenchCoverSite
  { benchCoverArity :: !Int,
    benchCoverRootFamily :: !(CoveringFamily BenchCoverObject BenchCoverMorphism)
  }

instance Site BenchCoverSite where
  type SiteObject BenchCoverSite = BenchCoverObject
  type SiteMorphism BenchCoverSite = BenchCoverMorphism

  siteObjects site =
    BenchCoverRoot
      : fmap BenchCoverLeaf leafIndices
      <> [ BenchCoverOverlap leftIndex rightIndex
         | leftIndex <- leafIndices,
           rightIndex <- [leftIndex + 1 .. max 0 (benchCoverArity site - 1)]
         ]
    where
      leafIndices =
        ordinalRange (benchCoverArity site)

  siteMorphisms site =
    fmap benchLeafToRoot leafIndices
      <> concatMap overlapMorphisms overlapPairs
    where
      leafIndices =
        ordinalRange (benchCoverArity site)

      overlapPairs =
        ordinalPairs (benchCoverArity site)

      overlapMorphisms (leftIndex, rightIndex) =
        [ benchOverlapToLeaf leftIndex rightIndex leftIndex,
          benchOverlapToLeaf leftIndex rightIndex rightIndex,
          benchOverlapToRootViaLeaf leftIndex rightIndex leftIndex,
          benchOverlapToRootViaLeaf leftIndex rightIndex rightIndex
        ]

  identityAt _ objectValue =
    CheckedMorphism objectValue objectValue (BenchCoverId objectValue)

  coversAt site objectValue =
    case objectValue of
      BenchCoverRoot -> [benchCoverRootFamily site]
      BenchCoverLeaf _ -> []
      BenchCoverOverlap _ _ -> []

  composeChecked _ outerMorphism innerMorphism
    | cmSource outerMorphism /= cmTarget innerMorphism =
        Nothing
    | benchCoverIsIdentity outerMorphism =
        Just innerMorphism
    | benchCoverIsIdentity innerMorphism =
        Just outerMorphism
    | otherwise =
        case (cmWitness outerMorphism, cmWitness innerMorphism) of
          (BenchLeafToRoot leafIndex, BenchOverlapToLeaf leftIndex rightIndex overlapLeafIndex)
            | leafIndex == overlapLeafIndex ->
                Just (benchOverlapToRootViaLeaf leftIndex rightIndex leafIndex)
          _ -> Nothing

  pullbackPair _ leftMorphism rightMorphism =
    case (cmWitness leftMorphism, cmWitness rightMorphism) of
      (BenchLeafToRoot leftIndex, BenchLeafToRoot rightIndex)
        | leftIndex < rightIndex ->
            Just (benchPullback leftIndex rightIndex)
      _ -> Nothing

benchLeafToRoot :: Int -> CheckedMorphism BenchCoverObject BenchCoverMorphism
benchLeafToRoot leafIndex =
  CheckedMorphism (BenchCoverLeaf leafIndex) BenchCoverRoot (BenchLeafToRoot leafIndex)

benchOverlapToLeaf :: Int -> Int -> Int -> CheckedMorphism BenchCoverObject BenchCoverMorphism
benchOverlapToLeaf leftIndex rightIndex leafIndex =
  CheckedMorphism (BenchCoverOverlap leftIndex rightIndex) (BenchCoverLeaf leafIndex) (BenchOverlapToLeaf leftIndex rightIndex leafIndex)

benchOverlapToRootViaLeaf :: Int -> Int -> Int -> CheckedMorphism BenchCoverObject BenchCoverMorphism
benchOverlapToRootViaLeaf leftIndex rightIndex leafIndex =
  CheckedMorphism (BenchCoverOverlap leftIndex rightIndex) BenchCoverRoot (BenchOverlapToRootViaLeaf leftIndex rightIndex leafIndex)

benchPullback :: Int -> Int -> PullbackSquare BenchCoverObject BenchCoverMorphism
benchPullback leftIndex rightIndex =
  PullbackSquare
    { psLeftBase = benchLeafToRoot leftIndex,
      psRightBase = benchLeafToRoot rightIndex,
      psApex = BenchCoverOverlap leftIndex rightIndex,
      psToLeft = benchOverlapToLeaf leftIndex rightIndex leftIndex,
      psToRight = benchOverlapToLeaf leftIndex rightIndex rightIndex
    }

benchCoverIsIdentity :: CheckedMorphism BenchCoverObject BenchCoverMorphism -> Bool
benchCoverIsIdentity morphismValue =
  case cmWitness morphismValue of
    BenchCoverId _ -> True
    _ -> False

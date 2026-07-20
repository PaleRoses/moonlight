{-# LANGUAGE NamedFieldPuns #-}

module Site
  ( benchmarks
  , probeCases
  ) where

import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Category
  ( FinCat
  , FinGeneratorId (..)
  , FinMorphismId (..)
  , FinObjectId (..)
  , SiteManifest (..)
  , mkFinCat
  )
import Fixture
  ( BenchmarkFixture (..)
  , BenchmarkResult
  , ProbeCase
  , ProbeFamily (..)
  , RawPosetSpec (..)
  , benchmarkEitherWith
  , benchmarkFailure
  , checksumPoset
  )
import Registry
  ( BenchCase
  , benchCase
  , familyBenchmarks
  , hostileProbeCases
  )
import Moonlight.Derived.Site
  ( DerivedPoset
  , derivedPosetCoversUp
  , derivedPosetFromFinCat
  , derivedPosetFromSiteManifest
  , derivedPosetNodes
  , derivedPosetUpper
  , mkDerivedPosetFromOrderEdges
  )
import Test.Tasty.Bench (Benchmark)

benchmarks :: [BenchmarkFixture] -> Benchmark
benchmarks fixtures =
  familyBenchmarks "site" siteFamilies fixtures

probeCases :: [BenchmarkFixture] -> [ProbeCase]
probeCases =
  hostileProbeCases "site" ProbeFamilyStructural siteFamilies

siteFamilies :: [BenchCase]
siteFamilies =
  [ benchCase "order-edges" runOrderEdges
  , benchCase "fincat-lowering" runFinCatLowering
  , benchCase "site-manifest-lowering" runSiteManifestLowering
  ]

runOrderEdges :: BenchmarkFixture -> BenchmarkResult
runOrderEdges fixture =
  let RawPosetSpec{rpsNodes, rpsCovers} = bfAmbientRaw fixture
   in benchmarkEitherWith checksumPoset (mkDerivedPosetFromOrderEdges rpsNodes rpsCovers)

runFinCatLowering :: BenchmarkFixture -> BenchmarkResult
runFinCatLowering fixture =
  case posetAsFinCat (bfAmbientPoset fixture) of
    Left failureMessage ->
      benchmarkFailure failureMessage
    Right categoryValue ->
      benchmarkEitherWith checksumPoset (derivedPosetFromFinCat categoryValue)

runSiteManifestLowering :: BenchmarkFixture -> BenchmarkResult
runSiteManifestLowering fixture =
  benchmarkEitherWith checksumPoset (derivedPosetFromSiteManifest (posetAsSiteManifest (bfAmbientPoset fixture)))

posetAsFinCat :: DerivedPoset -> Either String FinCat
posetAsFinCat posetValue =
  either
    (Left . show)
    Right
    ( mkFinCat
        objects
        (Map.fromList morphismBuckets)
        (Map.fromList compositionEntries)
    )
  where
    strictEdges =
      strictOrderEdges posetValue
    objects =
      Set.fromAscList (derivedPosetNodeList posetValue)
    morphismBuckets =
      fmap
        ( \(sourceKey, targetKey) ->
            ( (FinObjectId sourceKey, FinObjectId targetKey)
            , [thinMorphismId sourceKey targetKey]
            )
        )
        strictEdges
    compositionEntries =
      [ ( (thinMorphismId middleKey targetKey, thinMorphismId sourceKey middleKey)
        , thinMorphismId sourceKey targetKey
        )
      | (sourceKey, middleKey) <- strictEdges
      , (middleKey', targetKey) <- strictEdges
      , middleKey == middleKey'
      , IntSet.member targetKey (upperSet sourceKey)
      ]
    upperSet nodeKey =
      IntMap.findWithDefault IntSet.empty nodeKey (derivedPosetUpper posetValue)

posetAsSiteManifest :: DerivedPoset -> SiteManifest Int
posetAsSiteManifest posetValue =
  SiteManifest
    { siteObjects = Set.fromAscList (fmap unFinObjectId (derivedPosetNodeList posetValue))
    , siteImports =
        Map.fromAscList
          [ (sourceKey, intSetAsSet targetKeys)
          | (sourceKey, targetKeys) <- IntMap.toAscList (derivedPosetCoversUp posetValue)
          ]
    , siteCovers =
        Map.fromAscList
          [ (sourceKey, intSetAsSet (IntSet.delete sourceKey targetKeys))
          | (sourceKey, targetKeys) <- IntMap.toAscList (derivedPosetUpper posetValue)
          ]
    }

strictOrderEdges :: DerivedPoset -> [(Int, Int)]
strictOrderEdges posetValue =
  [ (sourceKey, targetKey)
  | FinObjectId sourceKey <- derivedPosetNodeList posetValue
  , targetKey <- IntSet.toAscList (IntMap.findWithDefault IntSet.empty sourceKey (derivedPosetUpper posetValue))
  , sourceKey /= targetKey
  ]

derivedPosetNodeList :: DerivedPoset -> [FinObjectId]
derivedPosetNodeList =
  foldMap pure . derivedPosetNodes

thinMorphismId :: Int -> Int -> FinMorphismId
thinMorphismId sourceKey targetKey =
  FinGeneratorMorphismId (FinGeneratorId (sourceKey * 4096 + targetKey))

intSetAsSet :: IntSet.IntSet -> Set.Set Int
intSetAsSet =
  Set.fromAscList . IntSet.toAscList

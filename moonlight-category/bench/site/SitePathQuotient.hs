module SitePathQuotient
  ( sitePathQuotientBenchmarks,
  )
where

import BenchSupport (BenchSetup (..), prepareBenchValue)
import Control.DeepSeq (NFData (..))
import Data.Bifunctor (first)
import Data.Function ((&))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import FinCat
  ( finCatWeight,
    finMorphismWeight,
    finObjectIdWeight,
  )
import Moonlight.Category.Pure.FinCat (FinObjectId)
import Moonlight.Category.Pure.Site.Category
  ( SitePathCategory,
    SitePathMorphism,
    sitePathCategory,
    sitePathCategoryCodomain,
    sitePathCategoryObjectIds,
    sitePathManifest,
    sitePathMorphismCodomain,
    sitePathMorphismNodes,
    sitePathMorphismsBetween,
  )
import Moonlight.Category.Pure.Site.Compile (thinSiteKernel)
import Moonlight.Category.Pure.Site.Core (SiteManifest (..))
import Moonlight.Category.Pure.Site.Quotient
  ( SitePathQuotient,
    quotientMapMorphism,
    sitePathQuotient,
    sitePathQuotientCodomain,
    sitePathQuotientDomain,
    sitePathQuotientObjectIds,
  )
import SiteCases
  ( SiteCase,
    pathSiteCases,
    siteCaseLabel,
    siteEndpoints,
    siteManifestFromCase,
  )
import Test.Tasty.Bench (Benchmark, bench, bgroup, env, nf)

sitePathQuotientBenchmarks :: Benchmark
sitePathQuotientBenchmarks =
  bgroup
    "path category and quotient"
    (pathSiteCases & fmap sitePathBenchmark)

sitePathBenchmark :: SiteCase -> Benchmark
sitePathBenchmark siteCase =
  env (prepareBenchValue (preparedPathSiteCase siteCase)) $ \prepared ->
    bgroup
      (siteCaseLabel siteCase)
      [ bench "sitePathMorphismsBetween" (nf preparedPathEnumerationWeight prepared),
        bench "sitePathQuotient map morphisms" (nf preparedPathQuotientWeight prepared)
      ]

data PreparedPathSiteCase = PreparedPathSiteCase
  { preparedPathCategory :: !(SitePathCategory Int),
    preparedPathQuotient :: !(SitePathQuotient Int),
    preparedPathSource :: !Int,
    preparedPathTarget :: !Int
  }

instance NFData PreparedPathSiteCase where
  rnf prepared =
    rnfSitePathCategory (preparedPathCategory prepared)
      `seq` rnfSitePathQuotient (preparedPathQuotient prepared)
      `seq` preparedPathSource prepared
      `seq` preparedPathTarget prepared
      `seq` ()

preparedPathSiteCase :: SiteCase -> BenchSetup PreparedPathSiteCase
preparedPathSiteCase siteCase =
  BenchSetup $ do
    kernel <- first show (thinSiteKernel manifest)
    let categoryValue = sitePathCategory kernel
        quotientValue = sitePathQuotient categoryValue
    pure
      PreparedPathSiteCase
        { preparedPathCategory = categoryValue,
          preparedPathQuotient = quotientValue,
          preparedPathSource = sourceValue,
          preparedPathTarget = targetValue
        }
  where
    manifest = siteManifestFromCase siteCase
    (sourceValue, targetValue) = siteEndpoints siteCase

preparedPathEnumerationWeight :: PreparedPathSiteCase -> Int
preparedPathEnumerationWeight prepared =
  sitePathMorphismsBetween
    (preparedPathCategory prepared)
    (preparedPathSource prepared)
    (preparedPathTarget prepared)
    & fmap sitePathMorphismWeight
    & sum

preparedPathQuotientWeight :: PreparedPathSiteCase -> Int
preparedPathQuotientWeight prepared =
  sitePathMorphismsBetween
    (preparedPathCategory prepared)
    (preparedPathSource prepared)
    (preparedPathTarget prepared)
    & fmap
      ( \morphism ->
          either
            (const 0)
            finMorphismWeight
            (quotientMapMorphism (preparedPathQuotient prepared) morphism)
      )
    & sum

rnfSitePathCategory :: SitePathCategory Int -> ()
rnfSitePathCategory categoryValue =
  sitePathCategoryDeepWeight categoryValue `seq` ()

rnfSitePathQuotient :: SitePathQuotient Int -> ()
rnfSitePathQuotient quotientValue =
  sitePathQuotientDeepWeight quotientValue `seq` ()

sitePathCategoryDeepWeight :: SitePathCategory Int -> Int
sitePathCategoryDeepWeight categoryValue =
  siteManifestWeight (sitePathManifest categoryValue)
    + finCatWeight (sitePathCategoryCodomain categoryValue)
    + objectIdMapWeight (sitePathCategoryObjectIds categoryValue)

sitePathQuotientDeepWeight :: SitePathQuotient Int -> Int
sitePathQuotientDeepWeight quotientValue =
  sitePathCategoryDeepWeight (sitePathQuotientDomain quotientValue)
    + finCatWeight (sitePathQuotientCodomain quotientValue)
    + objectIdMapWeight (sitePathQuotientObjectIds quotientValue)

siteManifestWeight :: SiteManifest Int -> Int
siteManifestWeight manifest =
  intSetWeight (siteObjects manifest)
    + intSetMapWeight (siteImports manifest)
    + intSetMapWeight (siteCovers manifest)

intSetWeight :: Set Int -> Int
intSetWeight =
  sum . Set.toAscList

intSetMapWeight :: Map Int (Set Int) -> Int
intSetMapWeight =
  Map.foldlWithKey'
    ( \accumulated objectValue coveredValues ->
        accumulated + objectValue + intSetWeight coveredValues
    )
    0

objectIdMapWeight :: Map Int FinObjectId -> Int
objectIdMapWeight =
  Map.foldlWithKey'
    ( \accumulated objectValue objectId ->
        accumulated + objectValue + finObjectIdWeight objectId
    )
    0

sitePathMorphismWeight :: SitePathMorphism Int -> Int
sitePathMorphismWeight morphism =
  length (sitePathMorphismNodes morphism)
    + finMorphismWeight (sitePathMorphismCodomain morphism)

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Melusine.Nebula.Spec.HarvestSpec (tests) where

import Data.Foldable (traverse_)
import Data.Kind (Type)
import Data.List (partition, sort, tails)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import GHC.Types.Name.Occurrence (mkVarOcc)
import GHC.Types.Name.Reader (mkRdrUnqual)
import Melusine.Nebula.Discovery.Choose
  ( CandidateSite (..),
    CandidateSiteKind (..),
    ShapeBucket (..),
    assignCandidateOrdinals,
    shapeBuckets,
    sitePairKey,
  )
import Melusine.Nebula.Harvest.Core
  ( candidateSiteSupportGroups,
    candidateSiteSupportGroupsFromIndex,
    candidateSiteSupportGroupsIncremental,
    harvestDirtyBuckets,
    harvestIndexDelta,
    siteBucketIndex,
    siteRow,
    siteRowsByIdentity,
    supportGroups,
  )
import Melusine.Nebula.Harvest.Pairs
  ( PairLedger (..),
    advancePairLedger,
    admittedSitePairs,
    buildPairLedger,
    groupLedgerKey,
  )
import Moonlight.Cosheaf.Colimit
  ( CosheafColimit,
    CosheafColimitFactor (..),
    factorCosheafColimit,
    finiteCosheafColimitFromSupportPlan,
  )
import Moonlight.Cosheaf.Cosection (CosectionRepresentative (..))
import Moonlight.Cosheaf.Finite (FiniteCosheafAlgebra (..), mkFiniteCosheaf)
import Moonlight.Cosheaf.Support (h0SupportPlan)
import Moonlight.EGraph.Introspection.Core.HsExpr
  ( HsExprF (..),
    HsExprTag (..),
    HsVarRef (..),
    ScopeCtx (..),
    TagSignature,
    tagSignatureFromTag,
  )
import Moonlight.EGraph.Pure.Types (ClassId (..))
import Data.Fix (Fix (..))
import Moonlight.Sheaf.Site.Class (CheckedMorphism (..), PullbackSquare (..), Site (..), identityCover)
import Moonlight.Pale.Ghc.Expr (rootScopeId)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "harvest grouping"
    [ testCase "compiled support grouping is the transitive closure of the cosheaf oracle covering" $
        traverse_ assertClosureLaw candidateSiteFixtures,
      testCase "compiled support groups are ordered by ordinal" $
        traverse_ assertCompiledOrder candidateSiteFixtures,
      testCase "chain-connected sites descend to one component" $
        assertChainComponentLaw chainCandidateSites,
      testCase "site bucket index delta is identity on unchanged harvest rows" $
        assertHarvestDeltaIdentity identicalShapeSites,
      pairLedgerCases
    ]

pairLedgerCases :: TestTree
pairLedgerCases =
  testGroup
    "pair ledger"
    [ testCase "ledger admission equals the retired bounded enumeration at every limit" $
        traverse_ assertAdmissionOracleLaw [0, 1, 3, 5, 100],
      testCase "advanced ledger equals a ledger built from scratch on the advanced groups" $
        assertLedgerMaintenanceLaw,
      testCase "retracting an admitted pair refills admission from lower-ranked groups" $
        assertRetractionRefillLaw,
      testCase "admission is invariant under order-preserving ordinal reindexing" $
        assertRenumberingInvarianceLaw,
      testCase "a delta confined to one group carries every other group's ledger entries" $
        assertLedgerLocalityLaw,
      testCase "carried groups re-expand to fresh sites after ordinal renumbering" $
        assertGroupRefreshLaw,
      testCase "an advance with no affected groups returns the ledger unchanged" $
        assertEmptyDeltaDegeneracy
    ]

oracleIndexedCandidatePairs :: Int -> [[CandidateSite]] -> [(CandidateSite, CandidateSite)]
oracleIndexedCandidatePairs pairLimit siteGroups =
  fmap snd (Map.toAscList (boundedPairsFromGroups siteGroups))
  where
    boundedPairsFromGroups groups
      | pairLimit <= 0 =
          Map.empty
      | otherwise =
          oracleFoldCandidatePairs admitPair Map.empty groups

    admitPair keptPairs (leftSite, rightSite) =
      let pairKey =
            sitePairKey leftSite rightSite
          pairValue =
            (leftSite, rightSite)
       in if Map.size keptPairs < pairLimit
            then Map.insert pairKey pairValue keptPairs
            else
              case Map.lookupMax keptPairs of
                Just (worstKey, _)
                  | pairKey < worstKey ->
                      Map.insert pairKey pairValue (Map.deleteMax keptPairs)
                _ ->
                  keptPairs

oracleFoldCandidatePairs ::
  (acc -> (CandidateSite, CandidateSite) -> acc) ->
  acc ->
  [[CandidateSite]] ->
  acc
oracleFoldCandidatePairs step =
  foldl' oracleFoldBucketPairs
  where
    oracleFoldBucketPairs kept bucketSites =
      foldl' oracleFoldSiteTail kept (tails bucketSites)

    oracleFoldSiteTail kept siteTail =
      case siteTail of
        [] ->
          kept
        leftSite : laterSites ->
          foldl' (oracleFoldRightSite leftSite) kept laterSites

    oracleFoldRightSite leftSite kept rightSite
      | csOrdinal leftSite /= csOrdinal rightSite,
        csClass leftSite /= csClass rightSite =
          step kept (leftSite, rightSite)
      | otherwise =
          kept

pairFixtureSite :: Int -> String -> Fix HsExprF -> Int -> CandidateSite
pairFixtureSite classKey bindingName termValue sizeValue =
  CandidateSite
    { csOrdinal = 0,
      csBindingName = bindingName,
      csSiteKind = BindingCandidateSite,
      csRegion = Nothing,
      csContext = ActualScope rootScopeId,
      csClass = ClassId classKey,
      csTerm = termValue,
      csSourceTerm = termValue,
      csOriginalSize = sizeValue,
      csSize = sizeValue,
      csFreeScopeWidth = 0,
      csSignature = fixtureTermSignature termValue,
      csTypeEvidence = Nothing
    }

ledgerFixtureSites :: [CandidateSite]
ledgerFixtureSites =
  assignCandidateOrdinals
    [ pairFixtureSite 10 "appRemoveMe" (applicationTerm "f" "x") 3,
      pairFixtureSite 11 "varA" (globalVariable "va") 1,
      pairFixtureSite 12 "appB" (applicationTerm "g" "y") 3,
      pairFixtureSite 13 "listA" (listTerm "la") 2,
      pairFixtureSite 14 "varB" (globalVariable "vb") 1,
      pairFixtureSite 15 "appC" (applicationTerm "h" "z") 3,
      pairFixtureSite 16 "varC" (globalVariable "vc") 1,
      pairFixtureSite 17 "listB" (listTerm "lb") 2,
      pairFixtureSite 18 "varD" (globalVariable "vd") 1
    ]

ledgerFixtureRetractedSites :: [CandidateSite]
ledgerFixtureRetractedSites =
  assignCandidateOrdinals
    (filter ((/= "appRemoveMe") . csBindingName) ledgerFixtureSites)

requirePairGroups :: [CandidateSite] -> IO [[CandidateSite]]
requirePairGroups sites =
  either
    (\failure -> assertFailure ("pair fixture grouping failed: " <> show failure))
    pure
    (candidateSiteSupportGroups sites)

admittedNamePairs :: Int -> [CandidateSite] -> PairLedger -> [(String, String)]
admittedNamePairs pairLimit sites ledger =
  [ (csBindingName leftSite, csBindingName rightSite)
  | (leftSite, rightSite) <- admittedSitePairs pairLimit sites ledger
  ]

splitAffectedByName :: (String -> Bool) -> [[CandidateSite]] -> ([[CandidateSite]], [[CandidateSite]])
splitAffectedByName isAffectedName =
  partition (any (isAffectedName . csBindingName))

assertAdmissionOracleLaw :: Int -> IO ()
assertAdmissionOracleLaw pairLimit = do
  groups <- requirePairGroups ledgerFixtureSites
  let admitted =
        admittedSitePairs pairLimit ledgerFixtureSites (buildPairLedger pairLimit groups)
      oracle =
        oracleIndexedCandidatePairs pairLimit groups
  assertEqual
    ("limit " <> show pairLimit <> " admission matches the retired enumeration")
    (fmap ordinalPair oracle)
    (fmap ordinalPair admitted)
  where
    ordinalPair (leftSite, rightSite) =
      (csOrdinal leftSite, csOrdinal rightSite)

assertLedgerMaintenanceLaw :: IO ()
assertLedgerMaintenanceLaw = do
  previousGroups <- requirePairGroups ledgerFixtureSites
  advancedGroups <- requirePairGroups ledgerFixtureRetractedSites
  let pairLimit = 3
      previousLedger = buildPairLedger pairLimit previousGroups
      (affectedGroups, unaffectedGroups) =
        splitAffectedByName appBindingName advancedGroups
      advancedLedger =
        advancePairLedger pairLimit unaffectedGroups affectedGroups previousLedger
  assertEqual
    "advanced ledger equals the from-scratch ledger"
    (buildPairLedger pairLimit advancedGroups)
    advancedLedger

assertRetractionRefillLaw :: IO ()
assertRetractionRefillLaw = do
  previousGroups <- requirePairGroups ledgerFixtureSites
  advancedGroups <- requirePairGroups ledgerFixtureRetractedSites
  let pairLimit = 3
      previousAdmitted =
        admittedNamePairs pairLimit ledgerFixtureSites (buildPairLedger pairLimit previousGroups)
      (affectedGroups, unaffectedGroups) =
        splitAffectedByName appBindingName advancedGroups
      advancedLedger =
        advancePairLedger pairLimit unaffectedGroups affectedGroups (buildPairLedger pairLimit previousGroups)
      advancedAdmitted =
        admittedNamePairs pairLimit ledgerFixtureRetractedSites advancedLedger
  assertBool
    "before retraction every admitted pair is an application pair"
    (all (\(leftName, rightName) -> appBindingName leftName && appBindingName rightName) previousAdmitted)
  assertEqual "admission stays saturated after the retraction" 3 (length advancedAdmitted)
  assertBool
    "the retraction refills admission from the list and variable groups"
    (any (\(leftName, rightName) -> not (appBindingName leftName) || not (appBindingName rightName)) advancedAdmitted)

assertRenumberingInvarianceLaw :: IO ()
assertRenumberingInvarianceLaw = do
  groups <- requirePairGroups ledgerFixtureSites
  let pairLimit = 3
      renumberedSites = fmap renumber ledgerFixtureSites
      renumber site = site {csOrdinal = 2 * csOrdinal site + 5}
      renumberedGroups = fmap (fmap renumber) groups
      original =
        admittedNamePairs pairLimit ledgerFixtureSites (buildPairLedger pairLimit groups)
      renumbered =
        admittedNamePairs pairLimit renumberedSites (buildPairLedger pairLimit renumberedGroups)
  assertEqual "admitted pairs are stable under monotone reindexing" original renumbered

assertLedgerLocalityLaw :: IO ()
assertLedgerLocalityLaw = do
  previousGroups <- requirePairGroups ledgerFixtureSites
  advancedGroups <- requirePairGroups ledgerFixtureRetractedSites
  let pairLimit = 3
      previousLedger = buildPairLedger pairLimit previousGroups
      (affectedGroups, unaffectedGroups) =
        splitAffectedByName appBindingName advancedGroups
      advancedLedger =
        advancePairLedger pairLimit unaffectedGroups affectedGroups previousLedger
  traverse_
    ( \groupSites ->
        case groupLedgerKey groupSites of
          Nothing ->
            assertFailure "unaffected fixture group has no ledger key"
          Just keyValue ->
            assertEqual
              "unaffected group ledger entries are carried"
              (Map.lookup keyValue (pairLedgerGroups previousLedger))
              (Map.lookup keyValue (pairLedgerGroups advancedLedger))
    )
    unaffectedGroups

assertGroupRefreshLaw :: IO ()
assertGroupRefreshLaw = do
  previousGroups <- requirePairGroups ledgerFixtureSites
  let previousIndex = siteBucketIndex ledgerFixtureSites
      nextIndex = siteBucketIndex ledgerFixtureRetractedSites
      rowSites = siteRowsByIdentity ledgerFixtureRetractedSites
      dirtyBuckets = harvestDirtyBuckets (harvestIndexDelta previousIndex nextIndex)
      changedRows =
        Set.fromList
          (fmap siteRow (filter ((== "appRemoveMe") . csBindingName) ledgerFixtureSites))
  (mergedGroups, _, _) <-
    either
      (\failure -> assertFailure ("incremental grouping failed: " <> show failure))
      pure
      ( candidateSiteSupportGroupsIncremental
          rowSites
          previousGroups
          previousIndex
          nextIndex
          dirtyBuckets
          changedRows
      )
  freshGroups <-
    either
      (\failure -> assertFailure ("fresh grouping failed: " <> show failure))
      pure
      (candidateSiteSupportGroupsFromIndex rowSites nextIndex)
  assertEqual
    "incrementally maintained groups equal fresh groups, ordinals included"
    (fmap (fmap csOrdinal) freshGroups)
    (fmap (fmap csOrdinal) mergedGroups)

assertEmptyDeltaDegeneracy :: IO ()
assertEmptyDeltaDegeneracy = do
  groups <- requirePairGroups ledgerFixtureSites
  let pairLimit = 3
      ledger = buildPairLedger pairLimit groups
  assertEqual
    "empty advance is the identity on the ledger"
    ledger
    (advancePairLedger pairLimit groups [] ledger)

appBindingName :: String -> Bool
appBindingName bindingName =
  take 3 bindingName == "app"

type ShapeSupportObject :: Type
data ShapeSupportObject
  = ShapeSiteBucketObject !Int !ShapeBucket
  | ShapeBucketObject !ShapeBucket
  deriving stock (Eq, Ord, Show)

type ShapeSupportMorphism :: Type
data ShapeSupportMorphism
  = ShapeIdentityMorphism !ShapeSupportObject
  | ShapeSiteToBucketMorphism !Int !ShapeBucket
  deriving stock (Eq, Ord, Show)

type ShapeSupportSite :: Type
newtype ShapeSupportSite = ShapeSupportSite
  { shapeSiteBuckets :: Map.Map Int [ShapeBucket]
  }
  deriving stock (Eq, Show)

type ShapeSupportCorestrictionFailure :: Type
data ShapeSupportCorestrictionFailure
  = ShapeSupportBucketMismatch !ShapeSupportMorphism !ShapeBucket !ShapeBucket
  deriving stock (Eq, Ord, Show)

instance Site ShapeSupportSite where
  type SiteObject ShapeSupportSite = ShapeSupportObject
  type SiteMorphism ShapeSupportSite = ShapeSupportMorphism

  siteObjects =
    shapeSupportObjects

  siteMorphisms site =
    fmap (shapeIdentityMorphism site) (shapeSupportObjects site)
      <> shapeSupportInclusionMorphisms site

  identityAt _ =
    shapeIdentityAt

  coversAt site objectValue =
    [identityCover site objectValue]

  composeChecked _ outerMorphism innerMorphism
    | cmTarget innerMorphism /= cmSource outerMorphism =
        Nothing
    | otherwise =
        composeShapeMorphism outerMorphism innerMorphism

  pullbackPair _ leftMorphism rightMorphism
    | cmTarget leftMorphism /= cmTarget rightMorphism =
        Nothing
    | leftMorphism == rightMorphism =
        let apex = cmSource leftMorphism
            identityMorphism = shapeIdentityAt apex
         in Just
              PullbackSquare
                { psLeftBase = leftMorphism,
                  psRightBase = rightMorphism,
                  psApex = apex,
                  psToLeft = identityMorphism,
                  psToRight = identityMorphism
                }
    | shapeMorphismIsIdentity leftMorphism =
        let apex = cmSource rightMorphism
         in Just
              PullbackSquare
                { psLeftBase = leftMorphism,
                  psRightBase = rightMorphism,
                  psApex = apex,
                  psToLeft = rightMorphism,
                  psToRight = shapeIdentityAt apex
                }
    | shapeMorphismIsIdentity rightMorphism =
        let apex = cmSource leftMorphism
         in Just
              PullbackSquare
                { psLeftBase = leftMorphism,
                  psRightBase = rightMorphism,
                  psApex = apex,
                  psToLeft = shapeIdentityAt apex,
                  psToRight = leftMorphism
                }
    | otherwise =
        Nothing

shapeSupportObjects :: ShapeSupportSite -> [ShapeSupportObject]
shapeSupportObjects site =
  [ ShapeSiteBucketObject siteOrdinal bucketValue
  | (siteOrdinal, buckets) <- Map.toAscList (shapeSiteBuckets site),
    bucketValue <- buckets
  ]
    <> fmap ShapeBucketObject (Set.toAscList (foldMap Set.fromList (Map.elems (shapeSiteBuckets site))))

shapeSupportInclusionMorphisms :: ShapeSupportSite -> [CheckedMorphism ShapeSupportObject ShapeSupportMorphism]
shapeSupportInclusionMorphisms site =
  [ CheckedMorphism
      { cmSource = ShapeSiteBucketObject siteOrdinal bucketValue,
        cmTarget = ShapeBucketObject bucketValue,
        cmWitness = ShapeSiteToBucketMorphism siteOrdinal bucketValue
      }
  | (siteOrdinal, buckets) <- Map.toAscList (shapeSiteBuckets site),
    bucketValue <- buckets
  ]

shapeIdentityMorphism :: ShapeSupportSite -> ShapeSupportObject -> CheckedMorphism ShapeSupportObject ShapeSupportMorphism
shapeIdentityMorphism _ =
  shapeIdentityAt

shapeIdentityAt :: ShapeSupportObject -> CheckedMorphism ShapeSupportObject ShapeSupportMorphism
shapeIdentityAt objectValue =
  CheckedMorphism
    { cmSource = objectValue,
      cmTarget = objectValue,
      cmWitness = ShapeIdentityMorphism objectValue
    }

composeShapeMorphism ::
  CheckedMorphism ShapeSupportObject ShapeSupportMorphism ->
  CheckedMorphism ShapeSupportObject ShapeSupportMorphism ->
  Maybe (CheckedMorphism ShapeSupportObject ShapeSupportMorphism)
composeShapeMorphism outerMorphism innerMorphism =
  case (cmWitness outerMorphism, cmWitness innerMorphism) of
    (ShapeIdentityMorphism {}, _) ->
      Just innerMorphism
    (_, ShapeIdentityMorphism {}) ->
      Just outerMorphism
    _ ->
      Nothing

shapeMorphismIsIdentity :: CheckedMorphism ShapeSupportObject ShapeSupportMorphism -> Bool
shapeMorphismIsIdentity morphismValue =
  case cmWitness morphismValue of
    ShapeIdentityMorphism {} ->
      True
    ShapeSiteToBucketMorphism {} ->
      False

shapeSupportAlgebra ::
  FiniteCosheafAlgebra
    ShapeSupportSite
    ShapeBucket
    (ShapeBucket, ShapeBucket)
    ShapeSupportCorestrictionFailure
shapeSupportAlgebra =
  FiniteCosheafAlgebra
    { fcaCorestrict = corestrictShapeBucket,
      fcaMismatches = \_objectValue leftBucket rightBucket ->
        if leftBucket == rightBucket then [] else [(leftBucket, rightBucket)],
      fcaNormalize = \_objectValue bucketValue -> bucketValue
    }

corestrictShapeBucket ::
  CheckedMorphism ShapeSupportObject ShapeSupportMorphism ->
  ShapeBucket ->
  Either ShapeSupportCorestrictionFailure ShapeBucket
corestrictShapeBucket morphismValue bucketValue =
  case cmWitness morphismValue of
    ShapeIdentityMorphism {} ->
      Right bucketValue
    ShapeSiteToBucketMorphism _ targetBucket
      | bucketValue == targetBucket ->
          Right targetBucket
      | otherwise ->
          Left (ShapeSupportBucketMismatch (cmWitness morphismValue) bucketValue targetBucket)

oracleCandidateSiteSupportGroups :: [CandidateSite] -> Either String [[CandidateSite]]
oracleCandidateSiteSupportGroups sites = do
  colimit <- shapeSupportColimit sites
  factors <-
    either
      (Left . ("shape support colimit factoring failed: " <>) . show)
      Right
      (factorCosheafColimit cosectionRepValue colimit)
  pure (mapMaybe (shapeFactorSites (siteByOrdinal sites)) factors)

shapeSupportColimit ::
  [CandidateSite] ->
  Either
    String
    (CosheafColimit ShapeSupportSite ShapeBucket)
shapeSupportColimit sites = do
  cosheafValue <-
    either
      (Left . show)
      Right
      (mkFiniteCosheaf supportSite shapeSupportAlgebra (shapeSupportCostalks supportSite))
  supportPlan <-
    either
      (Left . show)
      Right
      (h0SupportPlan cosheafValue)
  either
    (Left . show)
    Right
    (finiteCosheafColimitFromSupportPlan supportPlan cosheafValue)
  where
    supportSite =
      shapeSupportSite sites

shapeSupportSite :: [CandidateSite] -> ShapeSupportSite
shapeSupportSite sites =
  ShapeSupportSite
    { shapeSiteBuckets =
        Map.fromList
          [ (csOrdinal site, shapeBuckets site)
          | site <- sites
          ]
    }

shapeSupportCostalks :: ShapeSupportSite -> Map.Map ShapeSupportObject [ShapeBucket]
shapeSupportCostalks site =
  Map.fromList
    [ (objectValue, costalkValues objectValue)
    | objectValue <- shapeSupportObjects site
    ]
  where
    costalkValues objectValue =
      case objectValue of
        ShapeSiteBucketObject _siteOrdinal bucketValue ->
          [bucketValue]
        ShapeBucketObject bucketValue ->
          [bucketValue]

siteByOrdinal :: [CandidateSite] -> Map.Map Int CandidateSite
siteByOrdinal sites =
  Map.fromList [(csOrdinal site, site) | site <- sites]

shapeFactorSites ::
  Map.Map Int CandidateSite ->
  CosheafColimitFactor ShapeSupportObject ShapeBucket ShapeBucket ->
  Maybe [CandidateSite]
shapeFactorSites sitesByOrdinal factor =
  case mapMaybe siteWitness (ccfWitnesses factor) of
    [] ->
      Nothing
    siteWitnesses ->
      Just siteWitnesses
  where
    siteWitness representative =
      case cosectionRepObject representative of
        ShapeSiteBucketObject siteOrdinal _bucketValue ->
          Map.lookup siteOrdinal sitesByOrdinal
        ShapeBucketObject {} ->
          Nothing

type CandidateSiteFixture :: Type
type CandidateSiteFixture = (String, [CandidateSite])

candidateSiteFixtures :: [CandidateSiteFixture]
candidateSiteFixtures =
  [ ("identical shapes", identicalShapeSites),
    ("disjoint shapes", disjointShapeSites),
    ("chain shapes", chainCandidateSites)
  ]

identicalShapeSites :: [CandidateSite]
identicalShapeSites =
  [ fixtureCandidateSite 3 "sameA" BindingCandidateSite (globalVariable "left") 1,
    fixtureCandidateSite 7 "sameB" RegionCandidateSite (globalVariable "middle") 1,
    fixtureCandidateSite 1 "sameC" BindingCandidateSite (globalVariable "right") 1
  ]

disjointShapeSites :: [CandidateSite]
disjointShapeSites =
  [ fixtureCandidateSite 3 "disjointA" BindingCandidateSite (globalVariable "alpha") 1,
    fixtureCandidateSite 7 "disjointB" RegionCandidateSite (applicationTerm "beta" "input") 3,
    fixtureCandidateSite 1 "disjointC" BindingCandidateSite (listTerm "gamma") 2
  ]

chainCandidateSites :: [CandidateSite]
chainCandidateSites =
  [ fixtureCandidateSite 3 "chainA" BindingCandidateSite (globalVariable "chain") 2,
    fixtureCandidateSite 7 "chainB" RegionCandidateSite (globalVariable "chain") 6,
    fixtureCandidateSite 1 "chainC" BindingCandidateSite (globalVariable "chain") 10
  ]

fixtureCandidateSite :: Int -> String -> CandidateSiteKind -> Fix HsExprF -> Int -> CandidateSite
fixtureCandidateSite ordinal bindingName siteKind termValue sizeValue =
  CandidateSite
    { csOrdinal = ordinal,
      csBindingName = bindingName,
      csSiteKind = siteKind,
      csRegion = Nothing,
      csContext = ActualScope rootScopeId,
      csClass = ClassId ordinal,
      csTerm = termValue,
      csSourceTerm = termValue,
      csOriginalSize = sizeValue,
      csSize = sizeValue,
      csFreeScopeWidth = 0,
      csSignature = fixtureTermSignature termValue,
      csTypeEvidence = Nothing
    }

fixtureTermSignature :: Fix HsExprF -> TagSignature
fixtureTermSignature (Fix nodeValue) =
  tagSignatureFromTag (fixtureRootTag nodeValue) <> foldMap fixtureTermSignature nodeValue

fixtureRootTag :: HsExprF child -> HsExprTag
fixtureRootTag = \case
  VarF {} -> VarTag
  AppF {} -> AppTag
  ExplicitListF {} -> ExplicitListTag
  _ -> OpaqueTag

globalVariable :: String -> Fix HsExprF
globalVariable variableName =
  Fix (VarF (GlobalName (mkRdrUnqual (mkVarOcc variableName))))

applicationTerm :: String -> String -> Fix HsExprF
applicationTerm functionName argumentName =
  Fix (AppF (globalVariable functionName) (globalVariable argumentName))

listTerm :: String -> Fix HsExprF
listTerm elementName =
  Fix (ExplicitListF [globalVariable elementName])

-- | The lawful relationship between the demoted cosheaf machinery and the
-- compiled grouping, established empirically by the chain fixture: the
-- cosheaf colimit factors are per-bucket stars (a 'ShapeSiteBucketObject' is
-- bucket-specific, so two buckets sharing a site are never connected by a
-- morphism), a COVERING that need not be a partition.  The compiled grouping
-- is exactly the transitive closure of that covering — the connected
-- components the plan specified.  Equality of the two holds only when no
-- site bridges buckets; the corpus gate separately pins that the admitted
-- pair sets coincide on real data.
assertClosureLaw :: CandidateSiteFixture -> IO ()
assertClosureLaw (fixtureName, sites) = do
  compiled <- requireCompiledGroups fixtureName sites
  oracle <- requireOracleGroups fixtureName sites
  let compiledPartition = partitionOrdinals compiled
      oracleCovering = partitionOrdinals oracle
  assertEqual
    (fixtureName <> " covers the same sites")
    (Set.unions oracleCovering)
    (Set.unions compiledPartition)
  assertBool
    (fixtureName <> " every oracle group is contained in one compiled component")
    ( all
        (\oracleGroup -> any (oracleGroup `Set.isSubsetOf`) (Set.toList compiledPartition))
        (Set.toList oracleCovering)
    )
  assertEqual
    (fixtureName <> " compiled partition is the transitive closure of the oracle covering")
    (transitiveClosure oracleCovering)
    compiledPartition

transitiveClosure :: Set.Set (Set.Set Int) -> Set.Set (Set.Set Int)
transitiveClosure covering =
  let merged = mergeOnce (Set.toList covering)
   in if Set.fromList merged == covering
        then covering
        else transitiveClosure (Set.fromList merged)
  where
    mergeOnce :: [Set.Set Int] -> [Set.Set Int]
    mergeOnce [] = []
    mergeOnce (groupValue : rest) =
      let (overlapping, disjoint) =
            span' (\other -> not (Set.disjoint groupValue other)) rest
       in Set.unions (groupValue : overlapping) : mergeOnce disjoint

    span' :: (Set.Set Int -> Bool) -> [Set.Set Int] -> ([Set.Set Int], [Set.Set Int])
    span' keep =
      foldr
        (\other (kept, dropped) ->
           if keep other
             then (other : kept, dropped)
             else (kept, other : dropped))
        ([], [])

assertCompiledOrder :: CandidateSiteFixture -> IO ()
assertCompiledOrder (fixtureName, sites) = do
  compiled <- requireCompiledGroups fixtureName sites
  let ordinalRows = fmap (fmap csOrdinal) compiled
  assertBool (fixtureName <> " has no empty production groups") (not (any null ordinalRows))
  assertEqual (fixtureName <> " group member order") (fmap sort ordinalRows) ordinalRows
  assertEqual (fixtureName <> " group order") (sort (mapMaybe firstOrdinal ordinalRows)) (mapMaybe firstOrdinal ordinalRows)

assertChainComponentLaw :: [CandidateSite] -> IO ()
assertChainComponentLaw sites = do
  compiled <- requireCompiledGroups "chain shapes" sites
  assertEqual "chain component count" 1 (length compiled)
  assertEqual "chain component ordinals" [Set.fromList [1, 3, 7]] (fmap (Set.fromList . fmap csOrdinal) compiled)

partitionOrdinals :: [[CandidateSite]] -> Set.Set (Set.Set Int)
partitionOrdinals =
  Set.fromList . fmap (Set.fromList . fmap csOrdinal)

firstOrdinal :: [Int] -> Maybe Int
firstOrdinal ordinals =
  case ordinals of
    [] ->
      Nothing
    ordinal : _ ->
      Just ordinal

requireCompiledGroups :: String -> [CandidateSite] -> IO [[CandidateSite]]
requireCompiledGroups fixtureName sites =
  either
    (\failure -> assertFailure (fixtureName <> " compiled grouping failed: " <> show failure))
    pure
    (supportGroups csOrdinal csOrdinal shapeBuckets sites)

requireOracleGroups :: String -> [CandidateSite] -> IO [[CandidateSite]]
requireOracleGroups fixtureName sites =
  either
    (\failure -> assertFailure (fixtureName <> " oracle grouping failed: " <> failure))
    pure
    (oracleCandidateSiteSupportGroups sites)

assertHarvestDeltaIdentity :: [CandidateSite] -> IO ()
assertHarvestDeltaIdentity sites =
  let indexValue = siteBucketIndex sites
      deltaValue = harvestIndexDelta indexValue indexValue
   in assertEqual "unchanged harvest index has no dirty buckets" Set.empty (harvestDirtyBuckets deltaValue)

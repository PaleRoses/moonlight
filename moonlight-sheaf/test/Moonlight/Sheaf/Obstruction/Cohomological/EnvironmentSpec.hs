{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Sheaf.Obstruction.Cohomological.EnvironmentSpec
  ( environmentTests,
  )
where

import Data.Dependent.Map qualified as DMap
import Data.Dependent.Sum (DSum ((:=>)))
import Data.EqP (EqP (..))
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.GADT.Compare
  ( GCompare (..),
    GEq (..),
    GOrdering (..),
  )
import Data.OrdP (OrdP (..))
import Data.Type.Equality ((:~:) (Refl))
import Moonlight.Sheaf.Obstruction
  ( IndexedEnvironment,
    IndexedEnvironmentAlgebra,
    IndexedEnvironmentBuilder (..),
    ObstructionEnvironmentAlgebra (..),
    buildIndexedEnvironment,
    environmentBuilderKeys,
    indexedEnvironmentAlgebraFromList,
    lookupEnvironmentBinding,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)

type EnvironmentKey :: Type -> Type -> Type
data EnvironmentKey runtime value where
  RegionKey :: EnvironmentKey runtime Int
  SummaryKey :: EnvironmentKey runtime String

eqEnvironmentKey ::
  EnvironmentKey runtime left ->
  EnvironmentKey runtime right ->
  Bool
eqEnvironmentKey leftKey rightKey =
  case (leftKey, rightKey) of
    (RegionKey, RegionKey) -> True
    (SummaryKey, SummaryKey) -> True
    _ -> False

compareEnvironmentKey ::
  EnvironmentKey runtime left ->
  EnvironmentKey runtime right ->
  Ordering
compareEnvironmentKey leftKey rightKey =
  case (leftKey, rightKey) of
    (RegionKey, RegionKey) -> EQ
    (RegionKey, SummaryKey) -> LT
    (SummaryKey, RegionKey) -> GT
    (SummaryKey, SummaryKey) -> EQ

instance Eq (EnvironmentKey runtime value) where
  (==) =
    eqEnvironmentKey

instance Ord (EnvironmentKey runtime value) where
  compare =
    compareEnvironmentKey

instance EqP (EnvironmentKey runtime) where
  eqp =
    eqEnvironmentKey

instance OrdP (EnvironmentKey runtime) where
  comparep =
    compareEnvironmentKey

instance GEq (EnvironmentKey runtime) where
  geq RegionKey RegionKey = Just Refl
  geq SummaryKey SummaryKey = Just Refl
  geq _ _ = Nothing

instance GCompare (EnvironmentKey runtime) where
  gcompare RegionKey RegionKey = GEQ
  gcompare RegionKey SummaryKey = GLT
  gcompare SummaryKey RegionKey = GGT
  gcompare SummaryKey SummaryKey = GEQ

type Request :: Type -> Type
newtype Request runtime = Request Int
  deriving stock (Eq, Show)

indexedAlgebraForRuntime ::
  IndexedEnvironmentAlgebra (Request runtime) Int Int () (EnvironmentKey runtime)
indexedAlgebraForRuntime =
  indexedEnvironmentAlgebraFromList
    [ RegionKey :=>
        IndexedEnvironmentBuilder
          (\(Request request) region occurrences guards ->
             request + region + length occurrences + length guards
          ),
      SummaryKey :=>
        IndexedEnvironmentBuilder
          (\(Request request) region occurrences guards ->
             show (request, region, length occurrences, length guards)
          )
    ]

environmentTests :: TestTree
environmentTests =
  testGroup
    "environment"
    [ testCase "indexed environment algebra builds heterogeneous bindings" $
        let environment :: IndexedEnvironment (EnvironmentKey ())
            environment =
              buildIndexedEnvironment (Request 3) 7 [1, 2 :: Int] [()] indexedAlgebraForRuntime
         in assertEqual
              "builders populate typed bindings"
              (Just (13 :: Int), Just "(3,7,2,1)")
              ( lookupEnvironmentBinding RegionKey environment,
                lookupEnvironmentBinding SummaryKey environment
              )
    , testCase "obstruction environment algebra carries query planning and indexed builders together" $
        let algebra :: ObstructionEnvironmentAlgebra Request EnvironmentKey Int Int () Int
            algebra =
              ObstructionEnvironmentAlgebra
                { oeaCollectOccurrences = \query -> [query, query + 1],
                  oeaEnumerateRegions = \(Request request) query -> [request + query],
                  oeaRefineRegion = \(Request request) query region -> [request + query + region],
                  oeaIndexedEnvironmentAlgebra = indexedAlgebraForRuntime,
                  oeaQueryFingerprint = (* 10),
                  oeaEnvironmentFingerprint = \(Request request) -> Just (request + 5)
                }
            environment :: IndexedEnvironment (EnvironmentKey ())
            environment =
              buildIndexedEnvironment
                (Request 3)
                7
                [1, 2 :: Int]
                [()]
                (oeaIndexedEnvironmentAlgebra algebra)
         in assertEqual
              "top-level algebra composes region planning with indexed environment builders"
              ( [7 :: Int, 8],
                [10 :: Int],
                [20 :: Int],
                Just (13 :: Int),
                Just "(3,7,2,1)",
                40 :: Int,
                Just (8 :: Int)
              )
              ( oeaCollectOccurrences algebra 7,
                oeaEnumerateRegions algebra (Request 3) 7,
                oeaRefineRegion algebra (Request 3) 7 10,
                lookupEnvironmentBinding RegionKey environment,
                lookupEnvironmentBinding SummaryKey environment,
                oeaQueryFingerprint algebra 4,
                oeaEnvironmentFingerprint algebra (Request 3)
              )
    , testCase "dependent-map difference reports missing typed bindings in key order" $
        let actualKeys :: DMap.DMap (EnvironmentKey ()) Proxy
            actualKeys = DMap.singleton RegionKey Proxy
         in case DMap.toAscList (DMap.difference (environmentBuilderKeys indexedAlgebraForRuntime) actualKeys) of
              [SummaryKey :=> Proxy] -> pure ()
              otherKeys ->
                assertFailure
                  ("expected only SummaryKey to be missing, got " <> show (length otherKeys) <> " keys")
    ]

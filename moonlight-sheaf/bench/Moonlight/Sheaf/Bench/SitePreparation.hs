{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- | Receipts for implicit powerset site preparation: n = 20 construction in
-- microseconds against the materialized counterfactual, plus steady-state
-- subset-query and carrier-meet probes at n = 20.
module Moonlight.Sheaf.Bench.SitePreparation
  ( sitePreparationBenchmarks,
  )
where

import Control.DeepSeq (NFData (..))
import Control.Exception (evaluate)
import Data.Bifunctor (first)
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.FiniteLattice
  ( ContextLattice,
    compileContextLattice,
    contextOrderDecl,
  )
import Moonlight.Sheaf.Context.Site
  ( PreparedContextSite,
    contextObjectKeyValue,
    contextObjectKeyFor,
    preparedContextRestrictsTo,
    preparedSupportFromContexts,
    supportCarrierFromSupport,
    supportCarrierGeneratorCount,
    supportCarrierMeet,
    withPreparedContextSiteFromFiniteLattice,
    withPreparedContextSiteFromPowersetAtoms,
  )
import Test.Tasty.Bench (Benchmark, bench, bgroup, env, nf)

sitePreparationBenchmarks :: Benchmark
sitePreparationBenchmarks =
  bgroup
    "site preparation"
    [ validatedPreparationBenchmark "prepare implicit powerset n=20 (key probe)" implicitPreparationProbe 20,
      validatedPreparationBenchmark "prepare implicit powerset n=62 (key probe)" implicitPreparationProbe 62,
      validatedPreparationBenchmark "prepare materialized powerset n=8 (256 objects, key probe)" materializedPreparationProbe 8,
      env (setupImplicitSite 20) $ \site ->
        bench "restriction order query at n=20 (implicit)" (nf runSomeRestrictionProbe site),
      env (setupImplicitSite 20) $ \site ->
        bench "carrier meet 8x8 generators at n=20 (implicit)" (nf runSomeCarrierMeetProbe site)
    ]

data SomePreparedContextSite where
  SomePreparedContextSite :: !(PreparedContextSite owner (Set Int)) -> SomePreparedContextSite

instance NFData SomePreparedContextSite where
  rnf (SomePreparedContextSite site) = site `seq` ()

runSomeRestrictionProbe :: SomePreparedContextSite -> Either String Int
runSomeRestrictionProbe (SomePreparedContextSite site) =
  restrictionProbe site 20

runSomeCarrierMeetProbe :: SomePreparedContextSite -> Either String Int
runSomeCarrierMeetProbe (SomePreparedContextSite site) =
  carrierMeetProbe site ()

validatedPreparationBenchmark :: String -> (Int -> Either String Int) -> Int -> Benchmark
validatedPreparationBenchmark label prepareProbe atomCount =
  env
    (validatePreparationProbe prepareProbe atomCount)
    (\preparedAtomCount -> bench label (nf prepareProbe preparedAtomCount))

validatePreparationProbe :: (Int -> Either String Int) -> Int -> IO Int
validatePreparationProbe prepareProbe atomCount =
  case prepareProbe atomCount of
    Left failureMessage -> fail failureMessage
    Right probeValue -> evaluate probeValue >> pure atomCount

setupImplicitSite :: Int -> IO SomePreparedContextSite
setupImplicitSite atomCount =
  either (fail . show) pure
    ( withPreparedContextSiteFromPowersetAtoms
        [0 .. atomCount - 1]
        (SomePreparedContextSite . id)
    )

withImplicitSite ::
  Int ->
  (forall owner. PreparedContextSite owner (Set Int) -> Either String result) ->
  Either String result
withImplicitSite atomCount useSite =
  first show (withPreparedContextSiteFromPowersetAtoms [0 .. atomCount - 1] useSite)
    >>= id

probeSubset :: Int -> Set Int
probeSubset atomCount =
  Set.fromList (filter even [0 .. atomCount - 1])

keyProbe :: PreparedContextSite owner (Set Int) -> Set Int -> Either String Int
keyProbe site subset =
  first show (contextObjectKeyValue <$> contextObjectKeyFor site subset)

implicitPreparationProbe :: Int -> Either String Int
implicitPreparationProbe atomCount =
  withImplicitSite atomCount $ \site ->
    keyProbe site (probeSubset atomCount)

materializedLattice :: Int -> Either String (ContextLattice (Set Int))
materializedLattice atomCount =
  first show (compileContextLattice (Set.fromList subsets) orderDecl)
  where
    atoms = [0 .. atomCount - 1]
    subsets = fmap Set.fromList (subsequencesOf atoms)
    orderDecl =
      contextOrderDecl
        (Set.fromList atoms)
        Set.empty
        [ (subset, Set.insert atomValue subset)
          | subset <- subsets,
            atomValue <- atoms,
            not (Set.member atomValue subset)
        ]

subsequencesOf :: [a] -> [[a]]
subsequencesOf =
  foldr (\headValue grown -> grown <> fmap (headValue :) grown) [[]]

materializedPreparationProbe :: Int -> Either String Int
materializedPreparationProbe atomCount = do
  lattice <- materializedLattice atomCount
  withPreparedContextSiteFromFiniteLattice lattice $ \site ->
    keyProbe site (probeSubset atomCount)

restrictionProbe :: PreparedContextSite owner (Set Int) -> Int -> Either String Int
restrictionProbe site atomCount =
  length . filter id
    <$> traverse
      ( first show
          . preparedContextRestrictsTo site (probeSubset atomCount)
          . Set.fromList
          . enumFromThenTo 0 2
      )
      [0 .. atomCount - 1]

carrierMeetProbe :: PreparedContextSite owner (Set Int) -> () -> Either String Int
carrierMeetProbe site () = do
  leftCarrier <- carrierOf [0 .. 7]
  rightCarrier <- carrierOf [4 .. 11]
  pure (supportCarrierGeneratorCount (supportCarrierMeet site leftCarrier rightCarrier))
  where
    carrierOf offsets =
      first show
        ( preparedSupportFromContexts
            site
            [Set.fromList [offset, offset + 3, offset + 7] | offset <- offsets]
            >>= supportCarrierFromSupport site
        )

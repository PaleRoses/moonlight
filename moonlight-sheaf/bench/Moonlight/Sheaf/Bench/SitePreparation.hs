-- | Receipts for implicit powerset site preparation: n = 20 construction in
-- microseconds against the materialized counterfactual, plus steady-state
-- subset-query and carrier-meet probes at n = 20.
module Moonlight.Sheaf.Bench.SitePreparation
  ( sitePreparationBenchmarks,
  )
where

import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.FiniteLattice
  ( ContextLattice,
    compileContextLattice,
    contextOrderDecl,
  )
import Moonlight.Sheaf.Context.Site
  ( ContextObjectKey (..),
    PreparedContextSite,
    contextObjectKeyFor,
    fromFiniteLattice,
    fromPowersetAtoms,
    preparedContextRestrictsTo,
    preparedSupportFromContexts,
    supportCarrierFromSupport,
    supportCarrierGeneratorCount,
    supportCarrierMeet,
  )
import Test.Tasty.Bench (Benchmark, bench, bgroup, nf)

sitePreparationBenchmarks :: Benchmark
sitePreparationBenchmarks =
  bgroup
    "site preparation"
    [ bench "fromPowersetAtoms n=20 (implicit, key probe)" (nf implicitPreparationProbe 20),
      bench "fromPowersetAtoms n=62 (implicit, key probe)" (nf implicitPreparationProbe 62),
      bench "fromFiniteLattice n=8 powerset (materialized counterfactual, 256 objects)" (nf materializedPreparationProbe 8),
      bench "restriction order query at n=20 (implicit)" (nf restrictionProbe 20),
      bench "carrier meet 8x8 generators at n=20 (implicit)" (nf carrierMeetProbe 20)
    ]

implicitSite :: Int -> PreparedContextSite (Set Int)
implicitSite atomCount =
  either
    (error . ("implicit bench site refused: " <>) . show)
    id
    (fromPowersetAtoms [0 .. atomCount - 1])

probeSubset :: Int -> Set Int
probeSubset atomCount =
  Set.fromList (filter even [0 .. atomCount - 1])

keyProbe :: PreparedContextSite (Set Int) -> Set Int -> Int
keyProbe site subset =
  either (const (-1)) contextObjectKeyValue (contextObjectKeyFor site subset)

implicitPreparationProbe :: Int -> Int
implicitPreparationProbe atomCount =
  keyProbe (implicitSite atomCount) (probeSubset atomCount)

materializedLattice :: Int -> ContextLattice (Set Int)
materializedLattice atomCount =
  either
    (error . ("materialized bench lattice refused: " <>) . show)
    id
    (compileContextLattice (Set.fromList subsets) orderDecl)
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

materializedPreparationProbe :: Int -> Int
materializedPreparationProbe atomCount =
  keyProbe (fromFiniteLattice (materializedLattice atomCount)) (probeSubset atomCount)

restrictionProbe :: Int -> Int
restrictionProbe atomCount =
  length
    ( filter
        (== Right True)
        [ preparedContextRestrictsTo site (probeSubset atomCount) (Set.fromList [0, 2 .. sourceBound])
          | sourceBound <- [0 .. atomCount - 1]
        ]
    )
  where
    site = implicitSite atomCount

carrierMeetProbe :: Int -> Int
carrierMeetProbe atomCount =
  supportCarrierGeneratorCount (supportCarrierMeet site leftCarrier rightCarrier)
  where
    site = implicitSite atomCount
    carrierOf offsets =
      either
        (error . ("bench carrier refused: " <>) . show)
        id
        ( preparedSupportFromContexts
            site
            [Set.fromList [offset, offset + 3, offset + 7] | offset <- offsets]
            >>= supportCarrierFromSupport site
        )
    leftCarrier = carrierOf [0 .. 7]
    rightCarrier = carrierOf [4 .. 11]

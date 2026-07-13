{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}

module Moonlight.EGraph.Bench.Suite.Context.SheafStress
  ( sheafStressBenchmarks,
  ) where

import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Kind (Type)
import Moonlight.Core (ClassId (..))
import Moonlight.Core (UnionFind)
import Moonlight.Core qualified as UnionFind
import Moonlight.EGraph.Pure.Types (ENode (..))
import Moonlight.EGraph.Sheaf.IncidenceSite
  ( EGraphIncidenceTag,
    egraphIncidenceCategoryFromSnapshot,
    egraphIncidenceNerveSite,
  )
import Moonlight.Sheaf.Site
  ( NerveSite,
    nerveSiteCells,
  )
import Test.Tasty.Bench (Benchmark, bench, bgroup, nf)

type BenchF :: Type -> Type
data BenchF a
  = BenchLeaf !Int
  | BenchPair !a !a
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

sheafStressBenchmarks :: Benchmark
sheafStressBenchmarks =
  bgroup
    "sheaf-stress"
    [ bgroup
        "incidence-nerve-site-snapshot"
        [ bench "snapshot-1k" (nf stressCellCount 1024),
          bench "snapshot-2k" (nf stressCellCount 2048),
          bench "snapshot-4k" (nf stressCellCount 4096),
          bench "snapshot-8k" (nf stressCellCount 8192),
          bench "snapshot-64k" (nf stressCellCount 65536)
        ]
    ]

stressCellCount :: Int -> Either String Int
stressCellCount classCount =
  fmap (length . nerveSiteCells) (stressSite classCount)

stressSite :: Int -> Either String (NerveSite (EGraphIncidenceTag BenchF))
stressSite classCount = do
  unionFind <- makeUnionFind classCount
  fmap
    (`egraphIncidenceNerveSite` 2)
    (first show (egraphIncidenceCategoryFromSnapshot unionFind (seedMembership classCount)))

makeUnionFind :: Int -> Either String UnionFind
makeUnionFind classCount =
  first show $
  foldM
    (\unionFind _ -> snd <$> UnionFind.makeSet unionFind)
    UnionFind.emptyUnionFind
    [1 .. max 0 classCount]

seedMembership :: Int -> IntMap [ENode BenchF]
seedMembership classCount =
  IntMap.fromAscList
    ( fmap
        (\classKey -> (classKey, seedENodesFor classKey))
        [0 .. classCount - 1]
    )

seedENodesFor :: Int -> [ENode BenchF]
seedENodesFor classKey
  | classKey < 2 = [ENode (BenchLeaf classKey)]
  | otherwise =
      [ ENode
          ( BenchPair
              (ClassId (classKey - 1))
              (ClassId (classKey - 2))
          )
      ]

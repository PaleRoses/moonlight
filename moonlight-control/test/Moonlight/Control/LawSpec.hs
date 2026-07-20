{-# LANGUAGE TypeApplications #-}

module Moonlight.Control.LawSpec
  ( tests,
  )
where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck (Gen, chooseInt, listOf, resize, shrink)

import Moonlight.Control.Interpret.Stats
  ( Phases (..),
    StatsBuilder (..),
    statsAlgebra,
  )
import Moonlight.Control.Laws
  ( LawBundle (..),
    foldAgreement,
    programLawBundles,
  )
import Moonlight.Control.Laws.Gen
  ( genProgram,
    shrinkProgram,
  )
import Moonlight.Control.Program
  ( Program,
    ProgramAlgebra (..),
  )

tests :: TestTree
tests =
  testGroup
    "Moonlight.Control law kit"
    ( fmap
        bundleTree
        ( programLawBundles genTestProgram shrinkTestProgram genContext shrink
            <> [ foldAgreement
                   @(StatsBuilder [Int] Int)
                   "StatsBuilder"
                   builtStats
                   statsAlgebra
                   genTestProgram
                   shrinkTestProgram,
                 foldAgreement
                   @(Phases [Int] Int)
                   "Phases"
                   phaseList
                   phasesAlgebra
                   genTestProgram
                   shrinkTestProgram
               ]
        )
    )

bundleTree :: LawBundle -> TestTree
bundleTree bundle =
  testGroup
    (lawBundleName bundle)
    (fmap (uncurry testProperty) (lawBundleProperties bundle))

genTestProgram :: Gen (Program [Int] Int)
genTestProgram =
  genProgram genContext genPhasePayload

shrinkTestProgram :: Program [Int] Int -> [Program [Int] Int]
shrinkTestProgram =
  shrinkProgram shrink shrink

genContext :: Gen [Int]
genContext =
  resize 3 (listOf (chooseInt (-5, 5)))

genPhasePayload :: Gen Int
genPhasePayload =
  chooseInt (0, 9)

phasesAlgebra :: ProgramAlgebra ctx p [p]
phasesAlgebra =
  ProgramAlgebra
    { paSkip = [],
      paPhase = pure,
      paSeq = (<>),
      paOr = (<>),
      paUpTo = const id,
      paAttempt = id,
      paScoped = const id
    }

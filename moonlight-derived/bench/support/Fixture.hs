{-# LANGUAGE NamedFieldPuns #-}

module Fixture
  ( WorkloadShape (..)
  , RawPosetSpec (..)
  , AxisSlice (..)
  , BlockSeed (..)
  , DifferentialSpec (..)
  , CaseId (..)
  , BenchmarkCaseRegistryEntry (..)
  , BenchmarkFixture (..)
  , BenchmarkCaseClass (..)
  , ProbeFamily (..)
  , ProbeBudgetClass (..)
  , ProbeBudget (..)
  , BenchmarkResult
  , ProbeRunResult (..)
  , ProbeCase (..)
  , BenchmarkChecksum (..)
  , benchmarkCaseRegistry
  , benchmarkFixtures
  , loadBenchmarkFixtures
  , qualifyCaseId
  , probeBudgetClassForFamily
  , resolveProbeBudget
  , mkProbeCase
  , mkProbeCaseForFixture
  , mkSafeMicroProbeCase
  , mkSafeMicroProbeCaseForFixture
  , mkHostileProbeCase
  , mkHostileProbeCaseForFixture
  , buildBlockedFromDifferentialSpec
  , concentratedDerivedFromSlices
  , derivedFromDifferentialSpec
  , forceChecksum
  , benchmarkSuccess
  , benchmarkEitherWith
  , benchmarkFailure
  , probeRunFromBenchmarkResult
  , checksumPoset
  , checksumDenseMatGF2
  , checksumBlockedMatGF2
  , checksumInjectiveComplexGF2
  , checksumDerivedGF2
  )
where

import Data.Bits (xor)
import Data.Bifunctor (first)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Moonlight.Core (MoonlightError (..))
import Moonlight.Category (FinObjectId (..))
import Moonlight.Derived.Pure.Functor.ClosedSupport
  ( ClosedSupport
  , mkClosedSupport
  )
import Moonlight.Derived.Pure.Functor.ProperPullback
  ( PreparedProperPullback
  , prepareProperPullback
  )
import Moonlight.Derived.Pure.Pipeline
  ( PreparedMicrosupport
  , prepareMicrosupport
  )
import Moonlight.Derived.Pure.Site.InjectiveComplex
  ( Derived (..)
  , InjectiveComplex (..)
  , mkNormalizedDerived
  )
import Moonlight.Derived.Pure.Site.LabeledMatrix
  ( BlockedMat (..)
  , DenseMat (..)
  , GroupedAxis (..)
  , appendAxisLabel
  , axisSize
  , blockAt
  , emptyAxis
  , expandBlocked
  , fromExpanded
  , zeroBlocked
  , zeroMat
  )
import Moonlight.Derived.Pure.Failure (derivedFailureToMoonlightError)
import Moonlight.Derived.Pure.Site.Microsupport
  ( LocalClosed
  , mkLocalClosed
  )
import Moonlight.Derived.Pure.Site.Poset
  ( DerivedPoset (..)
  , DerivedPosetFunctor
  , mkDerivedPosetFunctor
  , mkDerivedPosetFromCovers
  )
import Moonlight.LinAlg (GF2)

data WorkloadShape
  = WideDiscrete
  | Chain
  | MultiplicityHeavy
  deriving stock (Eq, Ord, Read, Show)

data RawPosetSpec = RawPosetSpec
  { rpsNodes :: ![FinObjectId]
  , rpsCovers :: ![(FinObjectId, FinObjectId)]
  }

data AxisSlice = AxisSlice
  { asNode :: !FinObjectId
  , asMultiplicity :: !Int
  }
  deriving stock (Eq, Show)

data BlockSeed = BlockSeed
  { bsRowNode :: !FinObjectId
  , bsColNode :: !FinObjectId
  , bsSeed :: !Int
  }
  deriving stock (Eq, Show)

data DifferentialSpec = DifferentialSpec
  { dsRows :: ![AxisSlice]
  , dsCols :: ![AxisSlice]
  , dsBlocks :: ![BlockSeed]
  }
  deriving stock (Eq, Show)

data BenchmarkCaseClass
  = SafeMicro
  | HostileProbe
  deriving stock (Eq, Ord, Show)

data ProbeFamily
  = ProbeFamilyFunctor
  | ProbeFamilyMorse
  | ProbeFamilyStructural
  deriving stock (Eq, Ord, Show)

data ProbeBudgetClass
  = ProbeBudgetModerate
  | ProbeBudgetHeavy
  | ProbeBudgetSevere
  deriving stock (Eq, Ord, Show)

newtype CaseId = CaseId
  { unCaseId :: String
  }
  deriving stock (Eq, Ord, Read, Show)

data BenchmarkCaseRegistryEntry = BenchmarkCaseRegistryEntry
  { bcreCaseId :: !CaseId
  , bcreLabel :: !String
  , bcreCaseClass :: !BenchmarkCaseClass
  , bcreProbeBudgetClass :: !ProbeBudgetClass
  }
  deriving stock (Eq, Ord, Show)

data BenchmarkFixture = BenchmarkFixture
  { bfCaseId :: !CaseId
  , bfLabel :: !String
  , bfShape :: !WorkloadShape
  , bfCaseClass :: !BenchmarkCaseClass
  , bfProbeBudgetClass :: !ProbeBudgetClass
  , bfAmbientRaw :: !RawPosetSpec
  , bfAmbientPoset :: !DerivedPoset
  , bfTargetRaw :: !RawPosetSpec
  , bfTargetPoset :: !DerivedPoset
  , bfMapToTarget :: !(FinObjectId -> FinObjectId)
  , bfFunctor :: !DerivedPosetFunctor
  , bfOuterLocalClosed :: !LocalClosed
  , bfOuterClosedSupport :: !ClosedSupport
  , bfPreparedMicrosupport :: !(PreparedMicrosupport GF2)
  , bfPreparedProperPullback :: !(PreparedProperPullback GF2)
  , bfInnerSupport :: !IntSet
  , bfSourceDerived :: !(Derived GF2)
  , bfSecondaryDerived :: !(Derived GF2)
  , bfTargetDerived :: !(Derived GF2)
  , bfMaterializationBlocked :: !(BlockedMat GF2)
  , bfExpandedRows :: !(Vector FinObjectId)
  , bfExpandedCols :: !(Vector FinObjectId)
  , bfExpandedDense :: !(DenseMat GF2)
  , bfAxisScrambledComplex :: !(InjectiveComplex GF2)
  , bfBoundaryScrambledComplex :: !(InjectiveComplex GF2)
  }

data ProbeBudget = ProbeBudget
  { pbTimeoutSeconds :: !Int
  , pbHeapMegabytes :: !Int
  }
  deriving stock (Eq, Ord, Show)

type BenchmarkResult = Either String Int

data ProbeRunResult
  = ProbeRunSucceeded !Int
  | ProbeRunRejected !String
  deriving stock (Eq, Ord, Read, Show)

data ProbeCase = ProbeCase
  { pcId :: !CaseId
  , pcLabel :: !String
  , pcClass :: !BenchmarkCaseClass
  , pcBudgetClass :: !ProbeBudgetClass
  , pcRun :: !(IO ProbeRunResult)
  }

newtype BenchmarkChecksum = BenchmarkChecksum
  { unBenchmarkChecksum :: Int
  }
  deriving stock (Eq, Ord, Show)

benchmarkCaseRegistry :: Bool -> [BenchmarkCaseRegistryEntry]
benchmarkCaseRegistry includeLarge =
  fmap wrRegistryEntry (selectedWorkloadRegistrations includeLarge)

benchmarkFixtures :: Bool -> Either MoonlightError [BenchmarkFixture]
benchmarkFixtures includeLarge =
  traverse
    buildBenchmarkFixture
    (selectedWorkloadRegistrations includeLarge)

loadBenchmarkFixtures :: Bool -> IO [BenchmarkFixture]
loadBenchmarkFixtures includeLarge =
  either (fail . show) pure (benchmarkFixtures includeLarge)

qualifyCaseId :: String -> CaseId -> CaseId
qualifyCaseId qualifier (CaseId caseIdValue) =
  CaseId (qualifier <> "/" <> caseIdValue)

probeBudgetClassForFamily :: ProbeFamily -> BenchmarkFixture -> ProbeBudgetClass
probeBudgetClassForFamily probeFamily fixture =
  max familyFloor (bfProbeBudgetClass fixture)
  where
    familyFloor =
      case (probeFamily, bfShape fixture) of
        (ProbeFamilyFunctor, MultiplicityHeavy) ->
          ProbeBudgetHeavy
        (ProbeFamilyFunctor, _) ->
          ProbeBudgetModerate
        (_, Chain) ->
          ProbeBudgetModerate
        (_, _) ->
          ProbeBudgetHeavy

resolveProbeBudget :: ProbeBudgetClass -> ProbeBudget
resolveProbeBudget probeBudgetClass =
  case probeBudgetClass of
    ProbeBudgetModerate ->
      ProbeBudget
        { pbTimeoutSeconds = 5
        , pbHeapMegabytes = 256
        }
    ProbeBudgetHeavy ->
      ProbeBudget
        { pbTimeoutSeconds = 15
        , pbHeapMegabytes = 512
        }
    ProbeBudgetSevere ->
      ProbeBudget
        { pbTimeoutSeconds = 20
        , pbHeapMegabytes = 768
        }

mkProbeCase :: String -> String -> BenchmarkCaseClass -> ProbeBudgetClass -> IO ProbeRunResult -> ProbeCase
mkProbeCase caseIdValue labelValue caseClassValue budgetClass runValue =
  ProbeCase
    { pcId = CaseId caseIdValue
    , pcLabel = labelValue
    , pcClass = caseClassValue
    , pcBudgetClass = budgetClass
    , pcRun = runValue
    }

mkProbeCaseForFixture :: String -> BenchmarkCaseClass -> ProbeBudgetClass -> BenchmarkFixture -> IO ProbeRunResult -> ProbeCase
mkProbeCaseForFixture caseStem caseClassValue budgetClass fixture =
  mkProbeCase
    (unCaseId (qualifyCaseId caseStem (bfCaseId fixture)))
    (caseStem <> "/" <> bfLabel fixture)
    caseClassValue
    budgetClass

mkSafeMicroProbeCase :: String -> ProbeBudgetClass -> IO ProbeRunResult -> ProbeCase
mkSafeMicroProbeCase caseIdValue =
  mkProbeCase caseIdValue caseIdValue SafeMicro

mkSafeMicroProbeCaseForFixture :: String -> ProbeBudgetClass -> BenchmarkFixture -> IO ProbeRunResult -> ProbeCase
mkSafeMicroProbeCaseForFixture caseStem =
  mkProbeCaseForFixture caseStem SafeMicro

mkHostileProbeCase :: String -> ProbeBudgetClass -> IO ProbeRunResult -> ProbeCase
mkHostileProbeCase caseIdValue =
  mkProbeCase caseIdValue caseIdValue HostileProbe

mkHostileProbeCaseForFixture :: String -> ProbeBudgetClass -> BenchmarkFixture -> IO ProbeRunResult -> ProbeCase
mkHostileProbeCaseForFixture caseStem =
  mkProbeCaseForFixture caseStem HostileProbe

buildBlockedFromDifferentialSpec :: DifferentialSpec -> BlockedMat GF2
buildBlockedFromDifferentialSpec DifferentialSpec {dsRows, dsCols, dsBlocks} =
  foldl' placeBlock initialMatrix dsBlocks
  where
    rowAxis = axisFromSlices dsRows
    colAxis = axisFromSlices dsCols
    rowMultiplicity = multiplicityMap dsRows
    colMultiplicity = multiplicityMap dsCols
    initialMatrix = zeroBlocked rowAxis colAxis

    placeBlock blockedValue BlockSeed {bsRowNode, bsColNode, bsSeed} =
      let rowCount = IntMap.findWithDefault 0 (unFinObjectId bsRowNode) rowMultiplicity
          colCount = IntMap.findWithDefault 0 (unFinObjectId bsColNode) colMultiplicity
          currentBlock = blockAt bsRowNode bsColNode blockedValue
          nextBlock = denseGF2 bsSeed rowCount colCount
       in blockedValue
            { bmBlocks =
                IntMap.insertWith
                  IntMap.union
                  (unFinObjectId bsRowNode)
                  (IntMap.singleton (unFinObjectId bsColNode) (if isZeroDense currentBlock then nextBlock else denseAdd currentBlock nextBlock))
                  (bmBlocks blockedValue)
            }

concentratedDerivedFromSlices :: DerivedPoset -> [AxisSlice] -> Either MoonlightError (Derived GF2)
concentratedDerivedFromSlices posetValue slicesValue =
  mkNormalizedDerived
    posetValue
    InjectiveComplex
      { icStart = 0
      , icDiffs = Vector.singleton (zeroBlocked emptyAxis (axisFromSlices slicesValue))
      }

derivedFromDifferentialSpec :: DerivedPoset -> DifferentialSpec -> Either MoonlightError (Derived GF2)
derivedFromDifferentialSpec posetValue differentialSpec =
  mkNormalizedDerived
    posetValue
    InjectiveComplex
      { icStart = 0
      , icDiffs = Vector.singleton (buildBlockedFromDifferentialSpec differentialSpec)
      }

forceChecksum :: BenchmarkChecksum -> Int
forceChecksum = unBenchmarkChecksum

benchmarkSuccess :: BenchmarkChecksum -> BenchmarkResult
benchmarkSuccess =
  Right . forceChecksum

benchmarkFailure :: String -> BenchmarkResult
benchmarkFailure =
  Left

benchmarkEitherWith :: Show errorValue => (value -> BenchmarkChecksum) -> Either errorValue value -> BenchmarkResult
benchmarkEitherWith checksumValue =
  either (benchmarkFailure . show) (benchmarkSuccess . checksumValue)

probeRunFromBenchmarkResult :: BenchmarkResult -> ProbeRunResult
probeRunFromBenchmarkResult benchmarkResult =
  case benchmarkResult of
    Left failureMessage ->
      ProbeRunRejected failureMessage
    Right checksumValue ->
      ProbeRunSucceeded checksumValue

checksumPoset :: DerivedPoset -> BenchmarkChecksum
checksumPoset DerivedPoset {derivedPosetNodes, derivedPosetUpper, derivedPosetLower, derivedPosetCoversUp, derivedPosetTopoDesc, derivedPosetTopoAsc} =
  foldChecksums
    id
    [ checksumVector checksumNode derivedPosetNodes
    , checksumIntMap checksumIntSet derivedPosetUpper
    , checksumIntMap checksumIntSet derivedPosetLower
    , checksumIntMap checksumIntSet derivedPosetCoversUp
    , checksumVector checksumNode derivedPosetTopoDesc
    , checksumVector checksumNode derivedPosetTopoAsc
    ]

checksumDenseMatGF2 :: DenseMat GF2 -> BenchmarkChecksum
checksumDenseMatGF2 DenseMat {dmRows, dmCols, dmData} =
  foldChecksums
    id
    [ checksumInt dmRows
    , checksumInt dmCols
    , checksumVector (checksumVector checksumGF2) dmData
    ]

checksumBlockedMatGF2 :: BlockedMat GF2 -> BenchmarkChecksum
checksumBlockedMatGF2 BlockedMat {bmRows, bmCols, bmBlocks} =
  foldChecksums
    id
    [ checksumAxis bmRows
    , checksumAxis bmCols
    , checksumIntMap (checksumIntMap checksumDenseMatGF2) bmBlocks
    ]

checksumInjectiveComplexGF2 :: InjectiveComplex GF2 -> BenchmarkChecksum
checksumInjectiveComplexGF2 InjectiveComplex {icStart, icDiffs} =
  foldChecksums
    id
    [ checksumInt icStart
    , checksumVector checksumBlockedMatGF2 icDiffs
    ]

checksumDerivedGF2 :: Derived GF2 -> BenchmarkChecksum
checksumDerivedGF2 (Derived _ injectiveComplex) =
  checksumInjectiveComplexGF2 injectiveComplex

data WorkloadSpec
  = WideDiscreteSpec !Int !Int
  | ChainSpec !Int !Int
  | MultiplicityHeavySpec !Int !Int !Int

data WorkloadRegistration = WorkloadRegistration
  { wrRegistryEntry :: !BenchmarkCaseRegistryEntry
  , wrWorkloadSpec :: !WorkloadSpec
  }

safeMicroWorkloadRegistrations :: [WorkloadRegistration]
safeMicroWorkloadRegistrations =
  [ mkWorkloadRegistration
      "wide-discrete/safe-micro"
      "wide-discrete-24x6"
      SafeMicro
      ProbeBudgetModerate
      (WideDiscreteSpec 24 6)
  , mkWorkloadRegistration
      "chain/safe-micro"
      "chain-6x3"
      SafeMicro
      ProbeBudgetModerate
      (ChainSpec 6 3)
  , mkWorkloadRegistration
      "multiplicity-heavy/safe-micro"
      "multiplicity-heavy-5x3-m2"
      SafeMicro
      ProbeBudgetModerate
      (MultiplicityHeavySpec 5 3 2)
  ]

hostileProbeWorkloadRegistrations :: [WorkloadRegistration]
hostileProbeWorkloadRegistrations =
  [ mkWorkloadRegistration
      "wide-discrete/hostile-probe/32x8"
      "wide-discrete-32x8"
      HostileProbe
      ProbeBudgetHeavy
      (WideDiscreteSpec 32 8)
  , mkWorkloadRegistration
      "wide-discrete/hostile-probe/48x10"
      "wide-discrete-48x10"
      HostileProbe
      ProbeBudgetSevere
      (WideDiscreteSpec 48 10)
  , mkWorkloadRegistration
      "wide-discrete/hostile-probe/64x12"
      "wide-discrete-64x12"
      HostileProbe
      ProbeBudgetSevere
      (WideDiscreteSpec 64 12)
  , mkWorkloadRegistration
      "chain/hostile-probe/8x4"
      "chain-8x4"
      HostileProbe
      ProbeBudgetHeavy
      (ChainSpec 8 4)
  , mkWorkloadRegistration
      "chain/hostile-probe/10x5"
      "chain-10x5"
      HostileProbe
      ProbeBudgetHeavy
      (ChainSpec 10 5)
  , mkWorkloadRegistration
      "multiplicity-heavy/hostile-probe/6x3-m3"
      "multiplicity-heavy-6x3-m3"
      HostileProbe
      ProbeBudgetHeavy
      (MultiplicityHeavySpec 6 3 3)
  , mkWorkloadRegistration
      "multiplicity-heavy/hostile-probe/6x3-m4"
      "multiplicity-heavy-6x3-m4"
      HostileProbe
      ProbeBudgetSevere
      (MultiplicityHeavySpec 6 3 4)
  , mkWorkloadRegistration
      "multiplicity-heavy/hostile-probe/7x4-m3"
      "multiplicity-heavy-7x4-m3"
      HostileProbe
      ProbeBudgetSevere
      (MultiplicityHeavySpec 7 4 3)
  , mkWorkloadRegistration
      "wide-discrete/hostile-probe/128x16"
      "wide-discrete-128x16"
      HostileProbe
      ProbeBudgetSevere
      (WideDiscreteSpec 128 16)
  , mkWorkloadRegistration
      "wide-discrete/hostile-probe/256x24"
      "wide-discrete-256x24"
      HostileProbe
      ProbeBudgetSevere
      (WideDiscreteSpec 256 24)
  , mkWorkloadRegistration
      "chain/hostile-probe/14x7"
      "chain-14x7"
      HostileProbe
      ProbeBudgetSevere
      (ChainSpec 14 7)
  , mkWorkloadRegistration
      "chain/hostile-probe/20x10"
      "chain-20x10"
      HostileProbe
      ProbeBudgetSevere
      (ChainSpec 20 10)
  , mkWorkloadRegistration
      "multiplicity-heavy/hostile-probe/8x4-m6"
      "multiplicity-heavy-8x4-m6"
      HostileProbe
      ProbeBudgetSevere
      (MultiplicityHeavySpec 8 4 6)
  ]

selectedWorkloadRegistrations :: Bool -> [WorkloadRegistration]
selectedWorkloadRegistrations includeLarge =
  safeMicroWorkloadRegistrations
    <> if includeLarge
      then hostileProbeWorkloadRegistrations
      else []

mkWorkloadRegistration :: String -> String -> BenchmarkCaseClass -> ProbeBudgetClass -> WorkloadSpec -> WorkloadRegistration
mkWorkloadRegistration caseIdValue labelValue caseClassValue budgetClassValue workloadSpec =
  WorkloadRegistration
    { wrRegistryEntry =
        BenchmarkCaseRegistryEntry
          { bcreCaseId = CaseId caseIdValue
          , bcreLabel = labelValue
          , bcreCaseClass = caseClassValue
          , bcreProbeBudgetClass = budgetClassValue
          }
    , wrWorkloadSpec = workloadSpec
    }

buildBenchmarkFixture :: WorkloadRegistration -> Either MoonlightError BenchmarkFixture
buildBenchmarkFixture WorkloadRegistration {wrRegistryEntry, wrWorkloadSpec} =
  case wrWorkloadSpec of
    WideDiscreteSpec nodeCount targetCount ->
      buildWideDiscreteFixture wrRegistryEntry nodeCount targetCount
    ChainSpec nodeCount targetCount ->
      buildChainFixture wrRegistryEntry nodeCount targetCount
    MultiplicityHeavySpec nodeCount targetCount baseMultiplicity ->
      buildMultiplicityHeavyFixture wrRegistryEntry nodeCount targetCount baseMultiplicity

buildWideDiscreteFixture :: BenchmarkCaseRegistryEntry -> Int -> Int -> Either MoonlightError BenchmarkFixture
buildWideDiscreteFixture registryEntry nodeCount targetCount = do
  let ambientRaw = discreteRawPoset nodeCount
      targetRaw = discreteRawPoset targetCount
      sourceHalf = nodeCount `div` 2
      outerSupport =
        IntSet.fromList
          ( [0 .. max 0 (sourceHalf `div` 2) - 1]
              <> [sourceHalf .. sourceHalf + max 0 (sourceHalf `div` 2) - 1]
          )
      innerSupport =
        IntSet.fromList
          ( [0 .. max 0 (targetCount `div` 2) - 1]
              <> [sourceHalf .. sourceHalf + max 0 (targetCount `div` 2) - 1]
          )
      sourceSpec =
        DifferentialSpec
          { dsRows = unitSlices (nodeRange 0 sourceHalf)
          , dsCols = unitSlices (nodeRange sourceHalf (nodeCount - sourceHalf))
          , dsBlocks =
              concatMap
                wideDiscreteBlocks
                [0 .. sourceHalf - 1]
          }
      targetSpec =
        DifferentialSpec
          { dsRows = unitSlices (nodeRange 0 (targetCount `div` 2))
          , dsCols = unitSlices (nodeRange (targetCount `div` 2) (targetCount - (targetCount `div` 2)))
          , dsBlocks =
              concatMap
                targetDiscreteBlocks
                [0 .. (targetCount `div` 2) - 1]
          }
      secondarySlices =
        fmap
          (\nodeValue@(FinObjectId nodeKey) -> AxisSlice nodeValue (if even nodeKey then 1 else 2))
          (fmap FinObjectId [0, 3 .. nodeCount - 1])
      mapToTarget (FinObjectId nodeKey) = FinObjectId (nodeKey `mod` max 1 targetCount)
  ambientPoset <- posetFromRaw ambientRaw
  targetPoset <- posetFromRaw targetRaw
  sourceDerived <-
    derivedFromDifferentialSpec
      ambientPoset
      sourceSpec {dsBlocks = []}
  secondaryDerived <- concentratedDerivedFromSlices ambientPoset secondarySlices
  targetDerived <-
    derivedFromDifferentialSpec
      targetPoset
      targetSpec {dsBlocks = []}
  let materializationBlocked = buildBlockedFromDifferentialSpec sourceSpec
  assembleFixture
        registryEntry
        WideDiscrete
        ambientRaw
        ambientPoset
        targetRaw
        targetPoset
        mapToTarget
        outerSupport
        innerSupport
        sourceDerived
        secondaryDerived
        targetDerived
        materializationBlocked
  where
    wideDiscreteBlocks rowIndex =
      let rowNode = FinObjectId rowIndex
          columnBase = nodeCount `div` 2
          firstColumn = FinObjectId (columnBase + ((rowIndex * 5 + 7) `mod` max 1 (nodeCount - columnBase)))
          secondColumn = FinObjectId (columnBase + ((rowIndex * 11 + 3) `mod` max 1 (nodeCount - columnBase)))
       in [ BlockSeed rowNode firstColumn (17 + rowIndex)
          , BlockSeed rowNode secondColumn (83 + rowIndex)
          ]

    targetDiscreteBlocks rowIndex =
      let rowNode = FinObjectId rowIndex
          columnBase = targetCount `div` 2
          columnNode = FinObjectId (columnBase + (rowIndex `mod` max 1 (targetCount - columnBase)))
       in [BlockSeed rowNode columnNode (151 + rowIndex)]

buildChainFixture :: BenchmarkCaseRegistryEntry -> Int -> Int -> Either MoonlightError BenchmarkFixture
buildChainFixture registryEntry nodeCount targetCount = do
  let ambientRaw = chainRawPoset nodeCount
      targetRaw = chainRawPoset targetCount
      outerSupport = IntSet.fromList [0 .. min (nodeCount - 1) 5]
      innerSupport = IntSet.fromList [0 .. min (targetCount - 1) 2]
      sourceSpec =
        DifferentialSpec
          { dsRows = unitSlices (nodeRange 0 (nodeCount - 1))
          , dsCols = unitSlices (nodeRange 1 (nodeCount - 1))
          , dsBlocks =
              fmap
                (\rowIndex -> BlockSeed (FinObjectId rowIndex) (FinObjectId (rowIndex + 1)) (211 + rowIndex))
                [0 .. nodeCount - 2]
          }
      targetSpec =
        DifferentialSpec
          { dsRows = unitSlices (nodeRange 0 (targetCount - 1))
          , dsCols = unitSlices (nodeRange 1 (targetCount - 1))
          , dsBlocks =
              fmap
                (\rowIndex -> BlockSeed (FinObjectId rowIndex) (FinObjectId (rowIndex + 1)) (281 + rowIndex))
                [0 .. targetCount - 2]
          }
      secondarySlices =
        fmap
          (\nodeValue@(FinObjectId nodeKey) -> AxisSlice nodeValue (if even nodeKey then 1 else 2))
          (fmap FinObjectId [2, 5 .. nodeCount - 1])
      mapToTarget (FinObjectId nodeKey) = FinObjectId (min (targetCount - 1) (nodeKey `div` 2))
  ambientPoset <- posetFromRaw ambientRaw
  targetPoset <- posetFromRaw targetRaw
  sourceDerived <- derivedFromDifferentialSpec ambientPoset sourceSpec
  secondaryDerived <- concentratedDerivedFromSlices ambientPoset secondarySlices
  targetDerived <- derivedFromDifferentialSpec targetPoset targetSpec
  let materializationBlocked = buildBlockedFromDifferentialSpec sourceSpec
  assembleFixture
        registryEntry
        Chain
        ambientRaw
        ambientPoset
        targetRaw
        targetPoset
        mapToTarget
        outerSupport
        innerSupport
        sourceDerived
        secondaryDerived
        targetDerived
        materializationBlocked

buildMultiplicityHeavyFixture :: BenchmarkCaseRegistryEntry -> Int -> Int -> Int -> Either MoonlightError BenchmarkFixture
buildMultiplicityHeavyFixture registryEntry nodeCount targetCount baseMultiplicity = do
  let ambientRaw = chainRawPoset nodeCount
      targetRaw = chainRawPoset targetCount
      outerSupport = IntSet.fromList [0 .. min (nodeCount - 1) 4]
      innerSupport = IntSet.fromList [0 .. min (targetCount - 1) 2]
      sourceSpec =
        DifferentialSpec
          { dsRows = multiplicitySlices baseMultiplicity (nodeRange 0 (nodeCount - 1))
          , dsCols = multiplicitySlices (baseMultiplicity + 1) (nodeRange 1 (nodeCount - 1))
          , dsBlocks =
              fmap
                (\rowIndex -> BlockSeed (FinObjectId rowIndex) (FinObjectId (rowIndex + 1)) (331 + rowIndex))
                [0 .. nodeCount - 2]
          }
      targetSpec =
        DifferentialSpec
          { dsRows = multiplicitySlices (baseMultiplicity - 1) (nodeRange 0 (targetCount - 1))
          , dsCols = multiplicitySlices baseMultiplicity (nodeRange 1 (targetCount - 1))
          , dsBlocks =
              fmap
                (\rowIndex -> BlockSeed (FinObjectId rowIndex) (FinObjectId (rowIndex + 1)) (401 + rowIndex))
                [0 .. targetCount - 2]
          }
      secondarySlices =
        fmap
          (\nodeValue@(FinObjectId nodeKey) -> AxisSlice nodeValue (2 + nodeKey `mod` 2))
          (fmap FinObjectId [2 .. nodeCount - 1])
      mapToTarget (FinObjectId nodeKey) = FinObjectId (min (targetCount - 1) (nodeKey `div` 2))
  ambientPoset <- posetFromRaw ambientRaw
  targetPoset <- posetFromRaw targetRaw
  sourceDerived <- derivedFromDifferentialSpec ambientPoset sourceSpec
  secondaryDerived <- concentratedDerivedFromSlices ambientPoset secondarySlices
  targetDerived <- derivedFromDifferentialSpec targetPoset targetSpec
  let materializationBlocked = buildBlockedFromDifferentialSpec sourceSpec
  assembleFixture
        registryEntry
        MultiplicityHeavy
        ambientRaw
        ambientPoset
        targetRaw
        targetPoset
        mapToTarget
        outerSupport
        innerSupport
        sourceDerived
        secondaryDerived
        targetDerived
        materializationBlocked

assembleFixture :: BenchmarkCaseRegistryEntry -> WorkloadShape -> RawPosetSpec -> DerivedPoset -> RawPosetSpec -> DerivedPoset -> (FinObjectId -> FinObjectId) -> IntSet -> IntSet -> Derived GF2 -> Derived GF2 -> Derived GF2 -> BlockedMat GF2 -> Either MoonlightError BenchmarkFixture
assembleFixture registryEntry shapeValue ambientRaw ambientPoset targetRaw targetPoset mapToTarget outerSupport innerSupport sourceDerived secondaryDerived targetDerived materializationBlocked = do
  outerLocalClosed <-
    first derivedFailureToMoonlightError (mkLocalClosed ambientPoset outerSupport)
  outerClosedSupport <- mkClosedSupport ambientPoset outerSupport
  functorValue <-
    first (InvariantViolation . show)
      ( mkDerivedPosetFunctor
          ambientPoset
          targetPoset
          ( Map.fromList
              [ (FinObjectId sourceKey, FinObjectId targetKey)
              | sourceNode@(FinObjectId sourceKey) <- Vector.toList (derivedPosetNodes ambientPoset)
              , let FinObjectId targetKey = mapToTarget sourceNode
              ]
          )
      )
  properPullbackDerived <-
    case shapeValue of
      WideDiscrete ->
        mkNormalizedDerived
          ambientPoset
          InjectiveComplex
            { icStart = 0
            , icDiffs =
                Vector.singleton
                  (zeroBlocked (bmRows materializationBlocked) (bmCols materializationBlocked))
            }
      Chain -> Right sourceDerived
      MultiplicityHeavy -> Right sourceDerived
  preparedMicrosupport <-
    first derivedFailureToMoonlightError
      (prepareMicrosupport functorValue properPullbackDerived)
  preparedProperPullback <-
    first derivedFailureToMoonlightError
      (prepareProperPullback outerLocalClosed properPullbackDerived)
  let (expandedRows, expandedCols, expandedDense) = expandBlocked materializationBlocked
      axisScrambledComplex = scrambledAxisComplex materializationBlocked
      boundaryScrambledComplex = scrambledBoundaryComplex materializationBlocked
  pure BenchmarkFixture
        { bfCaseId = bcreCaseId registryEntry
        , bfLabel = bcreLabel registryEntry
        , bfShape = shapeValue
        , bfCaseClass = bcreCaseClass registryEntry
        , bfProbeBudgetClass = bcreProbeBudgetClass registryEntry
        , bfAmbientRaw = ambientRaw
        , bfAmbientPoset = ambientPoset
        , bfTargetRaw = targetRaw
        , bfTargetPoset = targetPoset
        , bfMapToTarget = mapToTarget
        , bfFunctor = functorValue
        , bfOuterLocalClosed = outerLocalClosed
        , bfOuterClosedSupport = outerClosedSupport
        , bfPreparedMicrosupport = preparedMicrosupport
        , bfPreparedProperPullback = preparedProperPullback
        , bfInnerSupport = innerSupport
        , bfSourceDerived = sourceDerived
        , bfSecondaryDerived = secondaryDerived
        , bfTargetDerived = targetDerived
        , bfMaterializationBlocked = materializationBlocked
        , bfExpandedRows = expandedRows
        , bfExpandedCols = expandedCols
        , bfExpandedDense = expandedDense
        , bfAxisScrambledComplex = axisScrambledComplex
        , bfBoundaryScrambledComplex = boundaryScrambledComplex
        }

scrambledAxisComplex :: BlockedMat GF2 -> InjectiveComplex GF2
scrambledAxisComplex blockedValue =
  let (expandedRows, expandedCols, expandedDense) = expandBlocked blockedValue
   in InjectiveComplex
        { icStart = 0
        , icDiffs = Vector.singleton (fromExpanded (Vector.reverse expandedRows) (Vector.reverse expandedCols) expandedDense)
        }

scrambledBoundaryComplex :: BlockedMat GF2 -> InjectiveComplex GF2
scrambledBoundaryComplex blockedValue =
  let axisComplex = scrambledAxisComplex blockedValue
      scrambledBlocked = case icDiffs axisComplex Vector.!? 0 of
        Just blockedResult -> blockedResult
        Nothing -> zeroBlocked emptyAxis emptyAxis
   in InjectiveComplex
        { icStart = -1
        , icDiffs =
            Vector.fromList
              [ zeroBlocked (bmCols scrambledBlocked) emptyAxis
              , scrambledBlocked
              ]
        }

posetFromRaw :: RawPosetSpec -> Either MoonlightError DerivedPoset
posetFromRaw RawPosetSpec {rpsNodes, rpsCovers} =
  mkDerivedPosetFromCovers rpsNodes rpsCovers

discreteRawPoset :: Int -> RawPosetSpec
discreteRawPoset nodeCount =
  RawPosetSpec
    { rpsNodes = nodeRange 0 nodeCount
    , rpsCovers = []
    }

chainRawPoset :: Int -> RawPosetSpec
chainRawPoset nodeCount =
  let nodesValue = nodeRange 0 nodeCount
   in RawPosetSpec
        { rpsNodes = nodesValue
        , rpsCovers = zip nodesValue (drop 1 nodesValue)
        }

nodeRange :: Int -> Int -> [FinObjectId]
nodeRange start count =
  fmap FinObjectId [start .. start + max 0 count - 1]

unitSlices :: [FinObjectId] -> [AxisSlice]
unitSlices = fmap (`AxisSlice` 1)

multiplicitySlices :: Int -> [FinObjectId] -> [AxisSlice]
multiplicitySlices baseMultiplicity =
  fmap
    (\nodeValue@(FinObjectId nodeKey) -> AxisSlice nodeValue (baseMultiplicity + nodeKey `mod` 3))

axisFromSlices :: [AxisSlice] -> GroupedAxis
axisFromSlices =
  foldl'
    (\axisValue AxisSlice {asNode, asMultiplicity} -> appendAxisLabel asNode asMultiplicity axisValue)
    emptyAxis

multiplicityMap :: [AxisSlice] -> IntMap Int
multiplicityMap =
  foldl'
    (\acc AxisSlice {asNode, asMultiplicity} -> IntMap.insert (unFinObjectId asNode) asMultiplicity acc)
    IntMap.empty

denseGF2 :: Int -> Int -> Int -> DenseMat GF2
denseGF2 seedValue rowCount columnCount
  | rowCount <= 0 || columnCount <= 0 = zeroMat rowCount columnCount
  | otherwise =
      DenseMat
        rowCount
        columnCount
        ( Vector.generate
            rowCount
            ( \rowIndex ->
                Vector.generate
                  columnCount
                  ( \columnIndex ->
                      if columnIndex == (seedValue + rowIndex * 3) `mod` columnCount
                        || (seedValue + rowIndex * 5 + columnIndex * 7) `mod` 5 == 0
                        then 1
                        else 0
                  )
            )
        )

isZeroDense :: DenseMat GF2 -> Bool
isZeroDense DenseMat {dmData} =
  Vector.all (Vector.all (== 0)) dmData

denseAdd :: DenseMat GF2 -> DenseMat GF2 -> DenseMat GF2
denseAdd leftMatrix rightMatrix =
  DenseMat
    (dmRows leftMatrix)
    (dmCols leftMatrix)
    ( Vector.zipWith
        (Vector.zipWith (+))
        (dmData leftMatrix)
        (dmData rightMatrix)
    )

checksumInt :: Int -> BenchmarkChecksum
checksumInt value =
  BenchmarkChecksum (value * 65599 + 17)

checksumNode :: FinObjectId -> BenchmarkChecksum
checksumNode (FinObjectId nodeKey) =
  checksumInt nodeKey

checksumGF2 :: GF2 -> BenchmarkChecksum
checksumGF2 value =
  checksumInt (if value == 0 then 0 else 1)

checksumAxis :: GroupedAxis -> BenchmarkChecksum
checksumAxis GroupedAxis {gaOrder, gaMult} =
  foldChecksums
    id
    [ checksumVector checksumNode gaOrder
    , checksumIntMap checksumInt gaMult
    , checksumInt (axisSize (GroupedAxis gaOrder gaMult))
    ]

checksumIntSet :: IntSet -> BenchmarkChecksum
checksumIntSet =
  checksumList checksumInt . IntSet.toAscList

checksumIntMap :: (value -> BenchmarkChecksum) -> IntMap value -> BenchmarkChecksum
checksumIntMap checksumValue =
  checksumList
    (\(entryKey, entryValue) -> foldChecksums id [checksumInt entryKey, checksumValue entryValue])
    . IntMap.toAscList

checksumList :: (value -> BenchmarkChecksum) -> [value] -> BenchmarkChecksum
checksumList = foldChecksums

checksumVector :: (value -> BenchmarkChecksum) -> Vector value -> BenchmarkChecksum
checksumVector checksumValue =
  checksumList checksumValue . Vector.toList

foldChecksums :: Foldable container => (value -> BenchmarkChecksum) -> container value -> BenchmarkChecksum
foldChecksums checksumValue =
  foldl'
    (\acc value -> mixChecksums acc (checksumValue value))
    (BenchmarkChecksum 16777619)

mixChecksums :: BenchmarkChecksum -> BenchmarkChecksum -> BenchmarkChecksum
mixChecksums (BenchmarkChecksum leftValue) (BenchmarkChecksum rightValue) =
  BenchmarkChecksum ((leftValue * 16777619) `xor` rightValue)

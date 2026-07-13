{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Cosheaf.Test.Homology.Performance
  ( TimedAction (..),
    HomologyExampleCounterExpectations (..),
    HomologyExampleCounters (..),
    timedActionWith,
    homologyExampleCounters,
    assertHomologyExampleCounters,
    planCellTotal,
    witnessSupportSize,
  )
where

import Data.Kind (Type)
import Data.Time.Clock
  ( NominalDiffTime,
    diffUTCTime,
    getCurrentTime,
  )
import Moonlight.Cosheaf.Chain
  ( PreparedFiniteCosheafChain (..),
    cosheafBoundaryIncidenceAt,
    cosheafChainCellsAtDegree,
  )
import Moonlight.Cosheaf.Finite
  ( fcSite,
  )
import Moonlight.Cosheaf.Homology
  ( CosheafHomologyWitness (..),
  )
import Moonlight.Homology
  ( HomologicalDegree (..),
    boundaryEntries,
  )
import Moonlight.Sheaf.Site.Class
  ( Site (..),
  )
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
  )

-- | A deliberately small wall-clock timing wrapper. The measured action is
-- still a test boundary effect; the cosheaf/homology counters stay pure views
-- over the prepared plan and lifted witness.
type TimedAction :: Type -> Type
data TimedAction value = TimedAction
  { timedActionElapsed :: !NominalDiffTime,
    timedActionValue :: value
  }
  deriving stock (Eq, Show)

type HomologyExampleCounterExpectations :: Type
data HomologyExampleCounterExpectations = HomologyExampleCounterExpectations
  { expectedObjectCount :: !Int,
    expectedNonidentityMorphismCount :: !Int,
    expectedCellsByDegree :: ![(Int, Int)],
    expectedBoundaryNonzerosByDegree :: ![(Int, Int)],
    expectedRepresentativeSupportSize :: !Int
  }
  deriving stock (Eq, Show)

type HomologyExampleCounters :: Type
data HomologyExampleCounters = HomologyExampleCounters
  { hecObjectCount :: !Int,
    hecNonidentityMorphismCount :: !Int,
    hecCellsByDegree :: ![(Int, Int)],
    hecBoundaryNonzerosByDegree :: ![(Int, Int)],
    hecRepresentativeSupportSize :: !Int,
    hecPrepareElapsed :: !NominalDiffTime,
    hecHomologyElapsed :: !NominalDiffTime,
    hecLiftElapsed :: !NominalDiffTime
  }
  deriving stock (Eq, Show)

timedActionWith ::
  (value -> Int) ->
  IO value ->
  IO (TimedAction value)
timedActionWith forceMetric action = do
  startTime <- getCurrentTime
  value <- action
  let metric = forceMetric value
  metric `seq` pure ()
  endTime <- getCurrentTime
  pure
    TimedAction
      { timedActionElapsed = diffUTCTime endTime startTime,
        timedActionValue = value
      }

homologyExampleCounters ::
  Site site =>
  [Int] ->
  TimedAction (PreparedFiniteCosheafChain site value) ->
  TimedAction homologyResult ->
  TimedAction (CosheafHomologyWitness site value Integer) ->
  HomologyExampleCounters
homologyExampleCounters degreeInts timedPlan timedHomology timedWitness =
  HomologyExampleCounters
    { hecObjectCount = length (siteObjects site),
      hecNonidentityMorphismCount = length (siteMorphisms site),
      hecCellsByDegree = cellCountsByDegree degreeInts plan,
      hecBoundaryNonzerosByDegree = boundaryNonzeroCountsByDegree degreeInts plan,
      hecRepresentativeSupportSize = witnessSupportSize (timedActionValue timedWitness),
      hecPrepareElapsed = timedActionElapsed timedPlan,
      hecHomologyElapsed = timedActionElapsed timedHomology,
      hecLiftElapsed = timedActionElapsed timedWitness
    }
  where
    plan =
      timedActionValue timedPlan

    site =
      fcSite (pfccCosheaf plan)

assertHomologyExampleCounters ::
  String ->
  HomologyExampleCounterExpectations ->
  HomologyExampleCounters ->
  Assertion
assertHomologyExampleCounters label expected actual = do
  assertEqual (label <> ": object count") expectedObjectCountValue (hecObjectCount actual)
  assertEqual (label <> ": nonidentity morphism count") expectedMorphismCountValue (hecNonidentityMorphismCount actual)
  assertEqual (label <> ": cells by degree") expectedCellCounts (hecCellsByDegree actual)
  assertEqual (label <> ": boundary nonzeros by degree") expectedBoundaryCounts (hecBoundaryNonzerosByDegree actual)
  assertEqual (label <> ": representative support size") expectedSupportSize (hecRepresentativeSupportSize actual)
  assertNonnegativeElapsed (label <> ": prepare elapsed") (hecPrepareElapsed actual)
  assertNonnegativeElapsed (label <> ": homology elapsed") (hecHomologyElapsed actual)
  assertNonnegativeElapsed (label <> ": lift elapsed") (hecLiftElapsed actual)
  where
    expectedObjectCountValue =
      expectedObjectCount expected

    expectedMorphismCountValue =
      expectedNonidentityMorphismCount expected

    expectedCellCounts =
      expectedCellsByDegree expected

    expectedBoundaryCounts =
      expectedBoundaryNonzerosByDegree expected

    expectedSupportSize =
      expectedRepresentativeSupportSize expected

planCellTotal ::
  [Int] ->
  PreparedFiniteCosheafChain site value ->
  Int
planCellTotal degreeInts plan =
  sum (fmap snd (cellCountsByDegree degreeInts plan))
{-# INLINE planCellTotal #-}

witnessSupportSize ::
  CosheafHomologyWitness site value coefficient ->
  Int
witnessSupportSize =
  length . chwRepresentativeTerms
{-# INLINE witnessSupportSize #-}

cellCountsByDegree ::
  [Int] ->
  PreparedFiniteCosheafChain site value ->
  [(Int, Int)]
cellCountsByDegree degreeInts plan =
  fmap
    (\degreeInt -> (degreeInt, length (cosheafChainCellsAtDegree (HomologicalDegree degreeInt) plan)))
    degreeInts

boundaryNonzeroCountsByDegree ::
  [Int] ->
  PreparedFiniteCosheafChain site value ->
  [(Int, Int)]
boundaryNonzeroCountsByDegree degreeInts plan =
  fmap
    (\degreeInt -> (degreeInt, length (boundaryEntries (cosheafBoundaryIncidenceAt (HomologicalDegree degreeInt) plan))))
    degreeInts

assertNonnegativeElapsed :: String -> NominalDiffTime -> Assertion
assertNonnegativeElapsed label elapsedValue =
  assertBool label (elapsedValue >= 0)

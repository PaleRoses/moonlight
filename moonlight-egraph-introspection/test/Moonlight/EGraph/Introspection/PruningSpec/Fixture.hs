module Moonlight.EGraph.Introspection.PruningSpec.Fixture
  ( singletonPoset,
    sphereLikePoset,
    zeroDerived,
    withPreparedSphereVerdier,
    verdierSeed,
    chainPoset,
    incomingDerived,
    keptCell,
    prunedCell,
    spectralOracle,
  )
where

import Data.Map.Strict qualified as Map
import Data.Vector qualified as V
import Moonlight.Core (RegionNodeId (..))
import Moonlight.Derived.Complex
  ( Derived,
  )
import Moonlight.Derived.Matrix
  ( DenseMat,
    mkDenseMat,
  )
import Moonlight.Derived.Presentation.Builder
  ( derivedObject,
    differentialDense,
    object,
  )
import Moonlight.Derived.Triangulated qualified as Triangulated
import Moonlight.Derived.Site (FinObjectId (..), DerivedPoset, mkDerivedPosetFromOrderEdges)
import Moonlight.Derived.Pruning ()
import Moonlight.Derived.Pruning
  ( SpectralPruningOracle,
    mkSpectralPruningOracle
  )
import Moonlight.Derived.Pruning
  ( PreparedVerdierPruning,
    VerdierPreparation (..),
    prepareVerdierPruning
  )
import Moonlight.Homology
  ( BasisCellRef (..),
    FormalMap (..),
    HomologicalDegree (..),
    HomologyGroup (..),
    SpectralPage (..),
    mkBidegree
  )
import Moonlight.LinAlg (GF2)
import Moonlight.Sheaf.Obstruction (CandidateRegionSeed, mkCandidateRegionSeed)
import Test.Tasty.HUnit (Assertion, assertFailure)

expectPoset :: [FinObjectId] -> [(FinObjectId, FinObjectId)] -> DerivedPoset
expectPoset ns cs = either (error . show) id (mkDerivedPosetFromOrderEdges ns cs)

expectRight :: Show errorValue => Either errorValue value -> value
expectRight =
  either (error . show) id

singletonPoset :: DerivedPoset
singletonPoset =
  expectPoset [FinObjectId 0] []

sphereLikePoset :: DerivedPoset
sphereLikePoset =
  expectPoset [FinObjectId 0, FinObjectId 1] []

zeroDerived :: Derived GF2
zeroDerived =
  Triangulated.zeroDerived sphereLikePoset

withPreparedSphereVerdier :: (PreparedVerdierPruning -> Assertion) -> Assertion
withPreparedSphereVerdier assertions =
  case prepareVerdierPruning zeroDerived of
    Left preparationFailure ->
      assertFailure ("unexpected Verdier preparation failure: " <> show preparationFailure)
    Right VerdierNotApplicable ->
      assertFailure "expected Verdier preparation on the S^0 face poset"
    Right (VerdierPrepared preparedPruning) ->
      assertions preparedPruning

verdierSeed :: CandidateRegionSeed ()
verdierSeed =
  mkCandidateRegionSeed () (RegionNodeId 0) 17

chainPoset :: DerivedPoset
chainPoset =
  expectPoset [FinObjectId 0, FinObjectId 1] [(FinObjectId 0, FinObjectId 1)]

incomingDerived :: Derived GF2
incomingDerived =
  expectRight
    ( derivedObject chainPoset $ do
        (sourceObject, _) <- object 0 [FinObjectId 1]
        (targetObject, _) <- object 1 [FinObjectId 0]
        differentialDense
          sourceObject
          targetObject
          (expectRight (mkDenseMat 1 1 (V.fromList [V.fromList [1]])) :: DenseMat GF2)
    )

keptCell :: BasisCellRef
keptCell =
  BasisCellRef
    { cellDegree = HomologicalDegree 1,
      cellIndex = 0
    }

prunedCell :: BasisCellRef
prunedCell =
  BasisCellRef
    { cellDegree = HomologicalDegree 1,
      cellIndex = 1
    }

spectralOracle :: SpectralPruningOracle Rational
spectralOracle =
  mkSpectralPruningOracle
    [spectralPage0, spectralPage1]
    ( \basisCellRef ->
        if cellIndex basisCellRef == 0
          then mkBidegree 0 0
          else mkBidegree 1 0
    )

spectralPage0 :: SpectralPage Rational
spectralPage0 =
  mkSpectralPage 0 (\_ _ -> 1)

spectralPage1 :: SpectralPage Rational
spectralPage1 =
  mkSpectralPage
    1
    ( \filtrationDegreeValue complementaryDegreeValue ->
        if (filtrationDegreeValue, complementaryDegreeValue) == (1, 0)
          then 0
          else 1
    )

mkSpectralPage :: Int -> (Int -> Int -> Int) -> SpectralPage Rational
mkSpectralPage pageNumberValue rankAt =
  SpectralPage
    { pageIndex = pageNumberValue,
      groupAt =
        \filtrationDegreeValue complementaryDegreeValue ->
          HomologyGroup
            { freeRank = rankAt filtrationDegreeValue complementaryDegreeValue,
              torsionInvariants = []
            },
      diffMap = \_ _ -> emptyFormalMap,
      pageEntryMap = Map.empty,
      pageDifferentialMap = Map.empty,
      pageAdvanceSource = Nothing,
      pageAdvanceState = Nothing
    }

emptyFormalMap :: FormalMap Rational
emptyFormalMap =
  FormalMap
    { formalMatrix = [],
      formalDomainBasis = [],
      formalCodomainBasis = []
    }

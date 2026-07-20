module Moonlight.EGraph.Introspection.NerveSpec.Global.Persistence
  ( tests,
  )
where

import Moonlight.Analysis.Spectral
  ( DecategorificationSensitivity (..),
    ScalarShadowModeSeries (..),
    ScalarShadowSeries (..),
    SpectralGapSample (..),
    SpectralModeSample (..),
    ThresholdGapSpread (..),
    ThresholdModeTransport (..),
    analyzeScalarShadowSensitivity,
  )
import Moonlight.EGraph.Introspection.NerveSpec.Global.Prelude
import Moonlight.EGraph.Introspection.NerveSpec.Fixture
import Moonlight.Sheaf.Site (grothendieckChainComplexFromSite)
import Numeric.Natural (Natural)

tests :: TestTree
tests =
  testGroup
    "persistence"
    [ testCase "persistence builds filtered complexes and persistence summaries" testPersistence,
      testCase "decategorification sensitivity compares scalar shadows over the same carrier" testDecategorificationSensitivity
    ]

testPersistence :: Assertion
testPersistence =
  case
      ( buildFiltrationFor reversibleSystem 2,
        persistentObstructionsFor reversibleSystem 2,
        spectralPersistenceFor reversibleSystem 2
      ) of
    (Left failure, _, _) ->
      assertFailure (show failure)
    (_, Left failure, _) ->
      assertFailure (show failure)
    (_, _, Left failure) ->
      assertFailure (show failure)
    (Right filteredComplex, Right persistencePairsValue, Right spectralPoints) -> do
      assertBool
        "expected the filtered Grothendieck complex to assign births to every retained cell"
        (not (null (filteredCellBirths filteredComplex)))
      assertBool
        "expected persistence to produce at least one barcode"
        (not (null persistencePairsValue))
      assertEqual
        "expected barcode materialization to preserve persistence multiplicity"
        (length persistencePairsValue)
        (length (obstructionBarcodes persistencePairsValue))
      assertBool
        "expected spectral persistence to evaluate at least one filtration threshold"
        (not (null spectralPoints))

testDecategorificationSensitivity :: Assertion
testDecategorificationSensitivity =
  let sensitivityValue = decategorificationSensitivity
   in do
      assertEqual
        "expected one spectral series per scalar shadow"
        (length allScalarShadows)
        (length (dcsSeries sensitivityValue))
      assertEqual
        "expected one mode-transport series per scalar shadow"
        (length allScalarShadows)
        (length (dcsModeSeries sensitivityValue))
      assertBool
        "expected every scalar shadow to contribute at least one spectral sample"
        (all (not . null . sssSamples) (dcsSeries sensitivityValue))
      assertBool
        "expected every scalar shadow to contribute at least one mode sample"
        (all (not . null . ssmsSamples) (dcsModeSeries sensitivityValue))
      assertBool
        "expected decategorification spread values to stay non-negative"
        (all ((>= 0.0) . tgsSpread) (dcsThresholdSpreads sensitivityValue))
      assertBool
        "expected mode transport similarities to stay normalized whenever present"
        ( all
            (\transportValue -> maybe True (\value -> value >= 0.0 && value <= 1.0) (tmtMinimumTransport transportValue))
            (dcsThresholdModeTransports sensitivityValue)
            && all
              (\transportValue -> maybe True (\value -> value >= 0.0 && value <= 1.0) (tmtAverageTransport transportValue))
              (dcsThresholdModeTransports sensitivityValue)
        )
      assertBool
        "expected aggregate scalar-shadow sensitivity summaries to remain normalized when present"
        ( maybe True (>= 0.0) (dcsMaxGapSpread sensitivityValue)
            && maybe True (>= 0.0) (dcsAverageGapSpread sensitivityValue)
            && maybe True (\value -> value >= 0.0 && value <= 1.0) (dcsMinimumModeTransport sensitivityValue)
            && maybe True (\value -> value >= 0.0 && value <= 1.0) (dcsAverageModeTransport sensitivityValue)
        )

buildFiltrationFor ::
  RewriteSystem ArithF ->
  Natural ->
  Either HomologyFailure (FilteredFiniteChainComplex Int)
buildFiltrationFor rewriteSystem depthValue = do
  chainComplexValue <- grothendieckChainComplexFromSite (mkGrothendieckSite rewriteSystem depthValue)
  buildFiltration chainComplexValue (birthAssignments chainComplexValue)

persistentObstructionsFor ::
  RewriteSystem ArithF ->
  Natural ->
  Either HomologyFailure [PersistencePair FiltrationValue]
persistentObstructionsFor rewriteSystem depthValue = do
  chainComplexValue <- grothendieckChainComplexFromSite (mkGrothendieckSite rewriteSystem depthValue)
  persistentObstructions chainComplexValue (birthAssignments chainComplexValue)

spectralPersistenceFor ::
  RewriteSystem ArithF ->
  Natural ->
  Either HomologyFailure [SpectralPersistencePoint]
spectralPersistenceFor rewriteSystem depthValue = do
  chainComplexValue <- grothendieckChainComplexFromSite (mkGrothendieckSite rewriteSystem depthValue)
  spectralPersistence chainComplexValue (birthAssignments chainComplexValue)

birthAssignments :: FiniteChainComplex Int -> [(BasisCellRef, FiltrationValue)]
birthAssignments chainComplexValue =
  allBasisCellRefsLocal chainComplexValue
    & fmap
      (\basisCellRef -> (basisCellRef, FiltrationValue (fromIntegral (basisCellBirth basisCellRef))))

allBasisCellRefsLocal :: FiniteChainComplex Int -> [BasisCellRef]
allBasisCellRefsLocal chainComplexValue =
  let HomologicalDegree maxDegreeValue = maxHomologicalDegree chainComplexValue
   in [0 .. maxDegreeValue]
        >>= \degreeValue ->
          let cellCount = sourceCardinality (incidenceMatrixAt chainComplexValue (HomologicalDegree degreeValue))
           in fmap
                (\cellIndexValue ->
                    BasisCellRef
                      { cellDegree = HomologicalDegree degreeValue,
                        cellIndex = cellIndexValue
                      }
                )
                [0 .. max 0 (cellCount - 1)]

basisCellBirth :: BasisCellRef -> Int
basisCellBirth BasisCellRef {cellDegree = HomologicalDegree degreeValue, cellIndex = indexValue} =
  max 0 degreeValue + indexValue

decategorificationSensitivity :: DecategorificationSensitivity ScalarShadow
decategorificationSensitivity =
  analyzeScalarShadowSensitivity spectralSeries modeSeries
  where
    thresholds = [FiltrationValue 0.0, FiltrationValue 1.0]
    modeFor :: Integral a => a -> [GraphSpectralMode]
    modeFor shadowIndex =
      [ GraphSpectralMode
          { spectralEigenvalue = 0.5 + fromIntegral shadowIndex / 10.0,
            spectralCoefficients = [(0, 1.0), (1, 0.5)],
            spectralPositiveSupport = [0, 1],
            spectralNegativeSupport = [],
            spectralSupportCriticality = 0.0
          }
      ]
    spectralSeries =
      zip [0 :: Int ..] allScalarShadows
        & fmap
          ( \(shadowIndex, shadowValue) ->
              ScalarShadowSeries
                { sssShadow = shadowValue,
                  sssSamples =
                    zipWith
                      (\thresholdValue gapValue -> SpectralGapSample thresholdValue (Just gapValue))
                      thresholds
                      [0.25 + fromIntegral shadowIndex / 20.0, 0.75 + fromIntegral shadowIndex / 20.0]
                }
          )
    modeSeries =
      zip [0 :: Int ..] allScalarShadows
        & fmap
          ( \(shadowIndex, shadowValue) ->
              ScalarShadowModeSeries
                { ssmsShadow = shadowValue,
                  ssmsSamples =
                    fmap
                      (\thresholdValue -> SpectralModeSample thresholdValue (modeFor shadowIndex))
                      thresholds
                }
          )

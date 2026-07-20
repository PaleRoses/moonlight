{-# LANGUAGE RankNTypes #-}

module Moonlight.Stochastic.Sheaf.Spec
  ( tests,
  )
where

import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Monoid (Sum (..))
import Moonlight.Probability
  ( categoricalFoldMap,
    categoricalSupport,
    certainCategorical,
    mkCategorical,
    positiveProbValue,
  )
import Moonlight.Sheaf.Kernel.Basis
  ( SheafBasis,
    basisCells,
  )
import Moonlight.Sheaf.Section.Model
  ( SheafModel,
    withPreparedSheafModel,
  )
import Moonlight.Sheaf.Section.ObjectIndex (SheafModelVersion (..))
import Moonlight.Sheaf.Section.ObjectIndex (mkObjectIndex)
import Moonlight.Sheaf.Section.Store.State
  ( evaluateRestrictionInSection,
    mkTotalSectionStore,
    totalSectionEntries,
  )
import Moonlight.Sheaf.Section.Store.Types
  ( SectionRestrictionResult (..),
    TotalSectionStore,
  )
import Moonlight.Sheaf.Section.Morphism
  ( Restriction (..),
    RestrictionId (..),
    RestrictionParts (..),
    unitIncidenceRestriction,
  )
import Moonlight.Stochastic.Sheaf
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, (@?=), testCase)

type Cell :: Type
data Cell
  = LeftCell
  | RightCell
  deriving stock (Eq, Ord, Show)

type Tile :: Type
data Tile
  = Sun
  | Moon
  deriving stock (Eq, Ord, Show)

approxEq :: Double -> Double -> Double -> Bool
approxEq tolerance left right = abs (left - right) <= tolerance

tests :: TestTree
tests =
  testGroup
    "stochastic-sheaf"
    [ testCase "identity kernel satisfies restriction for matching dirac stalks" $
        let restriction :: Restriction Cell (StochasticKernelWitness Tile)
            restriction =
                    stochasticKernelRestriction
                      (RestrictionId 0)
                      LeftCell
                      RightCell
                      unitIncidenceRestriction
                      identityKernel
         in case withIdentityModel restriction identityModelAssertion of
              Left modelError ->
                assertFailure ("unexpected model construction failure: " <> show modelError)
              Right assertion ->
                assertion,
      testCase "pushforward preserves normalization" $
        case mkCategorical (Map.fromList [(Sun, 1.0), (Moon, 3.0)]) of
          Left err -> assertFailure (show err)
          Right sourceDistribution ->
            let kernel =
                  MarkovKernel
                    (\tile ->
                       case tile of
                         Sun -> stochasticStalk (certainCategorical Sun)
                         Moon -> stochasticStalk (certainCategorical Moon))
                totalProbability =
                  getSum
                    ( categoricalFoldMap
                        (Sum . positiveProbValue . snd)
                        (unstochasticStalk (pushforward kernel (stochasticStalk sourceDistribution)))
                    )
             in assertBool "pushforward remains normalized" (approxEq 1.0e-12 totalProbability 1.0),
      testCase "compile detects missing initial distributions" $
        case mkCategorical (Map.fromList [(Sun, 1.0), (Moon, 1.0)]) of
          Left err -> assertFailure (show err)
          Right distribution ->
            case
              compileStochasticSection
                StochasticSite
                  { stochasticCells = [LeftCell, RightCell],
                    stochasticInitial = Map.fromList [(LeftCell, stochasticStalk distribution)],
                    stochasticKernels = []
                  }
                (const ()) of
              Left violations -> violations @?= [MissingInitialDistribution RightCell]
              Right _ -> assertFailure "expected missing-initial violation",
      testCase "supportPushforward matches support of weighted pushforward" $
        case mkCategorical (Map.fromList [(Sun, 1.0), (Moon, 3.0)]) of
          Left err -> assertFailure (show err)
          Right sourceDistribution ->
            case mkCategorical (Map.fromList [(Sun, 1.0), (Moon, 1.0)]) of
              Left err -> assertFailure (show err)
              Right mixedDistribution ->
                let kernel =
                      MarkovKernel
                        ( \tile ->
                            case tile of
                              Sun -> stochasticStalk (certainCategorical Sun)
                              Moon -> stochasticStalk mixedDistribution
                        )
                    weightedSupport =
                      categoricalSupport
                        (unstochasticStalk (pushforward kernel (stochasticStalk sourceDistribution)))
                    possibilisticSupport =
                      unpossibilisticStalk
                        (supportPushforward kernel (possibilisticStalkFromCategorical sourceDistribution))
                 in possibilisticSupport @?= weightedSupport,
      testCase "compilePossibilisticSection projects stochastic support" $
        case mkCategorical (Map.fromList [(Sun, 1.0), (Moon, 1.0)]) of
          Left err -> assertFailure (show err)
          Right distribution ->
            let site =
                  StochasticSite
                    { stochasticCells = [LeftCell, RightCell],
                      stochasticInitial =
                        Map.fromList
                          [ (LeftCell, stochasticStalk distribution),
                            (RightCell, stochasticStalk (certainCategorical Sun))
                          ],
                      stochasticKernels =
                        [ ( LeftCell,
                            RightCell,
                            MarkovKernel
                              (\tile ->
                                 case tile of
                                   Sun -> stochasticStalk (certainCategorical Sun)
                                   Moon -> stochasticStalk (certainCategorical Moon))
                          )
                        ]
                    }
             in case
                  ( compileStochasticSection site stochasticProjection,
                    compilePossibilisticSection site possibilisticProjection
                  )
                of
                  (Right (stochasticCells, stochasticEntries), Right (possibilisticCells, possibilisticEntries)) -> do
                    stochasticCells @?= possibilisticCells
                    possibilisticEntries @?= fmap possibilisticStalkFromStochastic stochasticEntries
                  (Left violations, _) -> assertFailure ("unexpected stochastic compile failure: " <> show violations)
                  (_, Left violations) -> assertFailure ("unexpected possibilistic compile failure: " <> show violations)
    ]
  where
    identityModelAssertion model =
      case
        mkTotalSectionStore
          model
          ( Map.fromList
              [ (LeftCell, stochasticStalk (certainCategorical Sun)),
                (RightCell, stochasticStalk (certainCategorical Sun))
              ]
          )
      of
        Left constructionError ->
          assertFailure ("unexpected section construction failure: " <> show constructionError)
        Right section ->
          case evaluateRestrictionInSection stochasticStalkOps model section restrictionIdentity of
            Right SectionRestrictionSatisfied -> pure ()
            Right _ -> assertFailure "expected satisfied restriction"
            Left lookupError -> assertFailure ("unexpected lookup error: " <> show lookupError)

    restrictionIdentity =
      stochasticKernelRestriction
        (RestrictionId 0)
        LeftCell
        RightCell
        unitIncidenceRestriction
        identityKernel

    stochasticProjection artifacts =
      ( basisCells (stochasticBasis artifacts),
        sectionMapOnBasis
          (stochasticModel artifacts)
          (stochasticBasis artifacts)
          (stochasticSection artifacts)
      )

    possibilisticProjection artifacts =
      ( basisCells (possibilisticBasis artifacts),
        sectionMapOnBasis
          (possibilisticModel artifacts)
          (possibilisticBasis artifacts)
          (possibilisticSection artifacts)
      )

sectionMapOnBasis ::
  Ord cell =>
  SheafModel owner cell witness ->
  SheafBasis cell ->
  TotalSectionStore owner cell stalk ->
  Map.Map cell stalk
sectionMapOnBasis model basis section =
  case totalSectionEntries model section of
    Left _ ->
      Map.empty
    Right entries ->
      Map.fromList
        ( basisCells basis
            >>= (\cell -> maybe [] (\stalk -> [(cell, stalk)]) (Map.lookup cell entries))
        )

withIdentityModel ::
  Restriction Cell (StochasticKernelWitness Tile) ->
  (forall owner. SheafModel owner Cell (StochasticKernelWitness Tile) -> result) ->
  Either String result
withIdentityModel restriction useModel =
  case
    withPreparedSheafModel
      (SheafModelVersion 0)
      (mkObjectIndex [LeftCell, RightCell])
      ( \storedRestriction ->
          RestrictionParts
            { partKind = rKind storedRestriction,
              partSource = rSource storedRestriction,
              partTarget = rTarget storedRestriction,
              partWitness = rWitness storedRestriction
            }
      )
      [restriction]
      useModel
    of
    Left modelError -> Left (show modelError)
    Right model -> Right model

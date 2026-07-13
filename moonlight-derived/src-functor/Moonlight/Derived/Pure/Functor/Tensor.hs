{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Derived.Pure.Functor.Tensor
  ( tensorProduct
  , tensorProductPresentation
  , internalHom
  , TensorProfileStage (..)
  , TensorProfileSummary (..)
  , tensorProfileStageSummary
  ) where

import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Vector qualified as V
import Moonlight.Algebra (EuclideanDomain)
import Moonlight.Core (Field, MoonlightError (..))
import Moonlight.Derived.Pure.Gluing.Peeling
  ( minimizeComplexFromFrontierWithRank
  )
import Moonlight.Derived.Pure.LinAlg.Interpreter (rankDense)
import Moonlight.Derived.Pure.Functor.Tensor.Assembly
  ( blockedBlockCellCount
  , blockedBlockCount
  , blockedBlockNonZeroCount
  , tensorBlockedDifferentials
  , tensorPresentationFromBlocks
  , tensorReducedBlockedDifferentials
  )
import Moonlight.Derived.Pure.Functor.Tensor.Layout
  ( DegreeLayout (..)
  , RestrictionCache
  , TensorLayoutInput (..)
  , expandedBasisCellCount
  , expandedComplex
  , expandedDegreeCount
  , sumVector
  , supportPresentationCache
  , tensorLayoutInput
  , tensorPairInput
  )
import Moonlight.Derived.Pure.Functor.VerdierDual (verdierDualComplex)
import Moonlight.Derived.Pure.Site.InjectiveComplex
  ( Derived (..)
  , InjectiveComplex (..)
  , mkNormalizedDerivedTrusted
  , trustLawfulInjectiveComplex
  )
import Moonlight.Derived.Pure.Site.LabeledMatrix (BlockedMat)
import Moonlight.Derived.Pure.Site.Poset
  ( DerivedPoset (..)
  , FinObjectId
  )
import Moonlight.LinAlg (GF2)
import Moonlight.LinAlg.Dense.Field (DenseRankBackend)

type TensorPresentationResult :: Type -> Type
data TensorPresentationResult a = TensorPresentationResult
  { tprComplex :: !(InjectiveComplex a)
  , tprMinimizationFrontier :: ![(Int, FinObjectId)]
  }

data TensorProfileStage
  = TensorSupportStage
  | TensorExpansionStage
  | TensorPairStage
  | TensorLayoutStage
  | TensorDifferentialStage
  | TensorPresentationStage
  deriving stock (Eq, Ord, Show)

data TensorProfileSummary = TensorProfileSummary
  { tpsSupportPresentations :: !Int
  , tpsExpandedDegrees :: !Int
  , tpsExpandedBasisCells :: !Int
  , tpsPairInstances :: !Int
  , tpsSummands :: !Int
  , tpsLayoutDegrees :: !Int
  , tpsLayoutBasisCells :: !Int
  , tpsDifferentials :: !Int
  , tpsDifferentialCells :: !Int
  , tpsDifferentialNonZeros :: !Int
  , tpsRestrictionCacheEntries :: !Int
  , tpsPresentationDifferentials :: !Int
  , tpsPresentationBlocks :: !Int
  , tpsPresentationBlockCells :: !Int
  , tpsPresentationBlockNonZeros :: !Int
  }
  deriving stock (Eq, Ord, Show)

tensorProduct ::
  (Eq a, Field a, EuclideanDomain a, Num a, DenseRankBackend a) =>
  Derived a ->
  Derived a ->
  Either MoonlightError (Derived a)
tensorProduct leftDerived rightDerived = do
  tensorPresentation <- tensorProductPresentationResult posetValue leftDerived rightDerived
  minimizedComplex <-
    case tprMinimizationFrontier tensorPresentation of
      [] ->
        Right (tprComplex tensorPresentation)
      frontier ->
        minimizeComplexFromFrontierWithRank rankDense frontier (tprComplex tensorPresentation)
  pure
    ( mkNormalizedDerivedTrusted
        posetValue
        (trustLawfulInjectiveComplex minimizedComplex)
    )
  where
    posetValue = derivedPoset leftDerived

tensorProductPresentation ::
  (Eq a, Field a, Num a) =>
  Derived a ->
  Derived a ->
  Either MoonlightError (InjectiveComplex a)
tensorProductPresentation leftDerived rightDerived =
  tprComplex <$> tensorProductPresentationResult posetValue leftDerived rightDerived
  where
    posetValue = derivedPoset leftDerived

tensorProductPresentationResult ::
  forall a.
  (Eq a, Field a, Num a) =>
  DerivedPoset ->
  Derived a ->
  Derived a ->
  Either MoonlightError (TensorPresentationResult a)
tensorProductPresentationResult posetValue leftDerived rightDerived
  | derivedPoset leftDerived /= posetValue || derivedPoset rightDerived /= posetValue =
      Left (InvariantViolation "tensor: operands do not belong to the supplied site")
  | otherwise = do
      let leftComplex = getDerived leftDerived
          rightComplex = getDerived rightDerived
      layoutInput <- tensorLayoutInput posetValue leftComplex rightComplex
      (blockedDiffs, _, minimizationFrontier) <-
        tensorReducedBlockedDifferentials
          layoutInput
          posetValue
          leftComplex
          rightComplex
      pure
        TensorPresentationResult
          { tprComplex = tensorPresentationFromBlocks layoutInput blockedDiffs
          , tprMinimizationFrontier = minimizationFrontier
          }

tensorProfileStageSummary ::
  (Eq a, Num a) =>
  TensorProfileStage ->
  Derived a ->
  Derived a ->
  Either MoonlightError TensorProfileSummary
tensorProfileStageSummary stage leftDerived rightDerived
  | derivedPoset rightDerived /= posetValue =
      Left (InvariantViolation "tensor profile: operands belong to different sites")
  | otherwise =
      case stage of
        TensorSupportStage ->
          emptyTensorProfileSummary <$> supportPresentationCache posetValue leftComplex rightComplex
        TensorExpansionStage ->
          Right
            (emptyTensorProfileSummary Map.empty)
              { tpsExpandedDegrees =
                  expandedDegreeCount leftExpanded + expandedDegreeCount rightExpanded
              , tpsExpandedBasisCells =
                  expandedBasisCellCount leftExpanded + expandedBasisCellCount rightExpanded
              }
        TensorPairStage ->
          profileLayoutInput <$> pairOnlyInput
        TensorLayoutStage ->
          profileLayoutInput <$> layoutInput
        TensorDifferentialStage -> do
          preparedInput <- layoutInput
          (blockedDiffs, restrictionCache, _) <- tensorBlockedDifferentials preparedInput posetValue leftComplex rightComplex
          pure (profileBlockedDifferentials preparedInput blockedDiffs restrictionCache)
        TensorPresentationStage -> do
          preparedInput <- layoutInput
          (blockedDiffs, restrictionCache, _) <- tensorBlockedDifferentials preparedInput posetValue leftComplex rightComplex
          let presentation = tensorPresentationFromBlocks preparedInput blockedDiffs
          pure
            (profilePresentation preparedInput blockedDiffs restrictionCache presentation)
              { tpsPresentationDifferentials =
                  V.length (icDiffs presentation)
              }
  where
    posetValue = derivedPoset leftDerived
    leftComplex = getDerived leftDerived
    rightComplex = getDerived rightDerived
    leftExpanded = expandedComplex leftComplex
    rightExpanded = expandedComplex rightComplex
    layoutInput = tensorLayoutInput posetValue leftComplex rightComplex
    pairOnlyInput = tensorPairInput posetValue leftComplex rightComplex leftExpanded rightExpanded

emptyTensorProfileSummary :: Map k v -> TensorProfileSummary
emptyTensorProfileSummary supportCache =
  TensorProfileSummary
    { tpsSupportPresentations = Map.size supportCache
    , tpsExpandedDegrees = 0
    , tpsExpandedBasisCells = 0
    , tpsPairInstances = 0
    , tpsSummands = 0
    , tpsLayoutDegrees = 0
    , tpsLayoutBasisCells = 0
    , tpsDifferentials = 0
    , tpsDifferentialCells = 0
    , tpsDifferentialNonZeros = 0
    , tpsRestrictionCacheEntries = 0
    , tpsPresentationDifferentials = 0
    , tpsPresentationBlocks = 0
    , tpsPresentationBlockCells = 0
    , tpsPresentationBlockNonZeros = 0
    }

profileLayoutInput :: TensorLayoutInput a -> TensorProfileSummary
profileLayoutInput layoutInput =
  (emptyTensorProfileSummary (tliSupportCache layoutInput))
    { tpsExpandedDegrees =
        expandedDegreeCount (tliLeftExpanded layoutInput) + expandedDegreeCount (tliRightExpanded layoutInput)
    , tpsExpandedBasisCells =
        expandedBasisCellCount (tliLeftExpanded layoutInput) + expandedBasisCellCount (tliRightExpanded layoutInput)
    , tpsPairInstances = length (tliPairInstances layoutInput)
    , tpsSummands = sumVector (V.map length (tliSummandsByDegree layoutInput))
    , tpsLayoutDegrees = V.length (tliLayouts layoutInput)
    , tpsLayoutBasisCells = sumVector (V.map (V.length . dlLabels) (tliLayouts layoutInput))
    }

profileBlockedDifferentials ::
  (Eq a, Num a) =>
  TensorLayoutInput a ->
  V.Vector (BlockedMat a) ->
  RestrictionCache a ->
  TensorProfileSummary
profileBlockedDifferentials layoutInput blockedDiffs restrictionCache =
  (profileLayoutInput layoutInput)
    { tpsDifferentials = V.length blockedDiffs
    , tpsDifferentialCells = sumVector (V.map blockedBlockCellCount blockedDiffs)
    , tpsDifferentialNonZeros = sumVector (V.map blockedBlockNonZeroCount blockedDiffs)
    , tpsRestrictionCacheEntries = Map.size restrictionCache
    }

profilePresentation ::
  (Eq a, Num a) =>
  TensorLayoutInput a ->
  V.Vector (BlockedMat a) ->
  RestrictionCache a ->
  InjectiveComplex a ->
  TensorProfileSummary
profilePresentation layoutInput blockedDiffs restrictionCache InjectiveComplex {icDiffs} =
  (profileBlockedDifferentials layoutInput blockedDiffs restrictionCache)
    { tpsPresentationBlocks = sumVector (V.map blockedBlockCount icDiffs)
    , tpsPresentationBlockCells = sumVector (V.map blockedBlockCellCount icDiffs)
    , tpsPresentationBlockNonZeros = sumVector (V.map blockedBlockNonZeroCount icDiffs)
    }

internalHom ::
  Derived GF2 ->
  Derived GF2 ->
  Either MoonlightError (Derived GF2)
internalHom sourceComplex targetComplex = do
  dualTarget <- verdierDualComplex targetComplex
  tensorValue <- tensorProduct sourceComplex dualTarget
  verdierDualComplex tensorValue

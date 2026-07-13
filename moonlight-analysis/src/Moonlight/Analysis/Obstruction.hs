module Moonlight.Analysis.Obstruction
  ( ConstantDerivedProfile (..),
    WitnessClassifier (..),
    ObstructionClass (..),
    ObstructionInterpretation (..),
    ObstructionContext (..),
    mkObstructionContext,
    ambientPosetFromCellCovers,
    harmonicEnrichmentFromComplex,
    interpretObstructionRepresentative,
    obstructionClassesAtDegree,
    obstructionTower,
  )
where

import Data.Bifunctor (first)
import Data.Kind (Type)
import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Derived.Functor (prepareProperPullback, properPullback)
import Moonlight.Derived.Functor (properPushforward)
import Moonlight.Derived.Morse (hypercohomologyDims)
import Moonlight.Derived.Complex
  ( Derived
  , LocallyRestrictableDerived
  , forgetLocallyRestrictableDerived
  , mkLocallyRestrictableDerived
  )
import Moonlight.Derived.Site (FinObjectId (..), DerivedPoset, mkLocalClosed, mkDerivedPosetFromOrderEdges)
import Moonlight.Homology
  ( BasisCellRef (..),
    FiniteChainComplex,
    HarmonicLoop (..),
    HomologicalDegree (..),
    HomologyFailure (..),
    MacroScaffoldIR (..),
    RepresentativeCocycle,
    RepresentativeChain (..),
    TopologyObservationConfig (..),
    cohomologyBasisAt,
    defaultTopologyObservationConfig,
    observeTopologyWitness,
    topologyMacroScaffold,
  )
import Moonlight.LinAlg (GF2)

type ConstantDerivedProfile :: Type
data ConstantDerivedProfile = ConstantDerivedProfile
  { cdpAmbientHypercohomology :: IntMap Int,
    cdpSupportHypercohomology :: IntMap Int,
    cdpExtendedSupportHypercohomology :: IntMap Int
  }

type WitnessClassifier :: Type -> Type
data WitnessClassifier witness = WitnessClassifier
  { wcIsObstructed :: witness -> Bool,
    wcIsComposed :: witness -> Bool
  }

type ObstructionClass :: Type -> Type -> Type
data ObstructionClass cell witness = ObstructionClass
  { ocDegree :: HomologicalDegree,
    ocCocycleRepresentative :: RepresentativeCocycle Rational Int,
    ocSupportingCells :: [cell],
    ocDerivedProfile :: ConstantDerivedProfile,
    ocInterpretation :: ObstructionInterpretation cell witness
  }

type ObstructionInterpretation :: Type -> Type -> Type
data ObstructionInterpretation cell witness = ObstructionInterpretation
  { oiCellEvaluations :: [(cell, Rational)],
    oiWitnessEvidence :: [(cell, witness)],
    oiObstructedCells :: [cell],
    oiComposedCells :: [cell],
    oiHarmonicLoops :: [HarmonicLoop],
    oiHarmonicFailure :: Maybe HomologyFailure
  }

type ObstructionContext :: Type -> Type -> Type
data ObstructionContext cell witness = ObstructionContext
  { ocChainComplex :: FiniteChainComplex Int,
    ocBasisRefs :: Map cell BasisCellRef,
    ocBasisNodeId :: BasisCellRef -> Int,
    ocWitnessAtCell :: cell -> witness,
    ocWitnessClassifier :: WitnessClassifier witness,
    ocAmbientPoset :: DerivedPoset,
    ocAmbientDerived :: LocallyRestrictableDerived GF2,
    ocAmbientHypercohomology :: IntMap Int,
    ocHarmonicLoops :: [HarmonicLoop],
    ocHarmonicFailure :: Maybe HomologyFailure
  }

mkObstructionContext ::
  FiniteChainComplex Int ->
  Map cell BasisCellRef ->
  (BasisCellRef -> Int) ->
  (cell -> witness) ->
  WitnessClassifier witness ->
  DerivedPoset ->
  Derived GF2 ->
  IntMap Int ->
  [HarmonicLoop] ->
  Maybe HomologyFailure ->
  Either HomologyFailure (ObstructionContext cell witness)
mkObstructionContext chainComplexValue basisRefs basisNodeId witnessAtCell witnessClassifier ambientPoset ambientDerived ambientHypercohomology harmonicLoops harmonicFailure = do
  restrictableDerived <-
    first (BackendFailure . show)
      (mkLocallyRestrictableDerived ambientPoset ambientDerived)
  Right ObstructionContext
    { ocChainComplex = chainComplexValue,
      ocBasisRefs = basisRefs,
      ocBasisNodeId = basisNodeId,
      ocWitnessAtCell = witnessAtCell,
      ocWitnessClassifier = witnessClassifier,
      ocAmbientPoset = ambientPoset,
      ocAmbientDerived = restrictableDerived,
      ocAmbientHypercohomology = ambientHypercohomology,
      ocHarmonicLoops = harmonicLoops,
      ocHarmonicFailure = harmonicFailure
    }

ambientPosetFromCellCovers ::
  (Ord cell, Show cell) =>
  Map cell BasisCellRef ->
  (BasisCellRef -> Int) ->
  [cell] ->
  [(cell, cell)] ->
  Either HomologyFailure DerivedPoset
ambientPosetFromCellCovers basisRefs basisNodeId allCells coverPairs = do
  siteNodes <- traverse (nodeForCell basisRefs basisNodeId) allCells
  siteCovers <- traverse (\(targetCell, sourceCell) -> (,) <$> nodeForCell basisRefs basisNodeId targetCell <*> nodeForCell basisRefs basisNodeId sourceCell) coverPairs
  case mkDerivedPosetFromOrderEdges siteNodes siteCovers of
    Left posetError -> Left (InvalidTopologyInput (show posetError))
    Right posetValue -> Right posetValue

harmonicEnrichmentFromComplex ::
  FiniteChainComplex Int ->
  ([HarmonicLoop], Maybe HomologyFailure)
harmonicEnrichmentFromComplex finiteComplex =
  let observationConfig :: TopologyObservationConfig r
      observationConfig =
        defaultTopologyObservationConfig
          { observationLowModeCount = 1
          }
   in case observeTopologyWitness observationConfig finiteComplex of
        Left failure -> ([], Just failure)
        Right topologyWitness ->
          ( maybe [] macroScaffoldHarmonicLoops (topologyMacroScaffold topologyWitness),
            Nothing
          )

interpretObstructionRepresentative ::
  (Ord cell, Show cell) =>
  ObstructionContext cell witness ->
  RepresentativeCocycle Rational Int ->
  Either HomologyFailure (ObstructionClass cell witness)
interpretObstructionRepresentative obstructionContext cocycleRepresentative = do
  let evaluationByBasisIndex = normalizedEvaluations cocycleRepresentative
      supportingCellFor = lookupSupportingCell obstructionContext cocycleRepresentative
  evaluatedCells <-
    traverse
      (\(basisIndexValue, coefficientValue) -> fmap (\cellValue -> (cellValue, coefficientValue)) (supportingCellFor basisIndexValue))
      (Map.toAscList evaluationByBasisIndex)
  derivedProfile <- constantDerivedProfileForCells obstructionContext (fmap fst evaluatedCells)
  supportBasisRefs <- traverse (lookupBasisRef obstructionContext) (fmap fst evaluatedCells)
  let witnessEvidence =
        fmap
          (\(cellValue, _) -> (cellValue, ocWitnessAtCell obstructionContext cellValue))
          evaluatedCells
      (obstructedCells, composedCells) =
        interpretWitnessEvidence
          (ocWitnessClassifier obstructionContext)
          witnessEvidence
      harmonicLoops =
        filter
          (harmonicLoopTouches (Set.fromList supportBasisRefs))
          (ocHarmonicLoops obstructionContext)
  pure
    ObstructionClass
      { ocDegree = representativeDegree cocycleRepresentative,
        ocCocycleRepresentative = cocycleRepresentative,
        ocSupportingCells = fmap fst evaluatedCells,
        ocDerivedProfile = derivedProfile,
        ocInterpretation =
          ObstructionInterpretation
            { oiCellEvaluations = evaluatedCells,
              oiWitnessEvidence = witnessEvidence,
              oiObstructedCells = obstructedCells,
              oiComposedCells = composedCells,
              oiHarmonicLoops = harmonicLoops,
              oiHarmonicFailure = ocHarmonicFailure obstructionContext
            }
      }

obstructionClassesAtDegree ::
  (Ord cell, Show cell) =>
  ObstructionContext cell witness ->
  HomologicalDegree ->
  Either HomologyFailure [ObstructionClass cell witness]
obstructionClassesAtDegree obstructionContext degreeValue =
  if degreeIndex degreeValue < 0
    then Right []
    else traverse (interpretObstructionRepresentative obstructionContext) (cohomologyBasisAt (ocChainComplex obstructionContext) degreeValue)

obstructionTower ::
  (Ord cell, Show cell) =>
  ObstructionContext cell witness ->
  [HomologicalDegree] ->
  Either HomologyFailure [[ObstructionClass cell witness]]
obstructionTower obstructionContext =
  traverse (obstructionClassesAtDegree obstructionContext)

normalizedEvaluations ::
  RepresentativeCocycle Rational Int ->
  Map Int Rational
normalizedEvaluations cocycleRepresentative =
  Map.fromListWith (+)
    (fmap (\(coefficientValue, basisIndexValue) -> (basisIndexValue, coefficientValue)) (representativeTerms cocycleRepresentative))

lookupSupportingCell ::
  ObstructionContext cell witness ->
  RepresentativeCocycle Rational Int ->
  Int ->
  Either HomologyFailure cell
lookupSupportingCell obstructionContext cocycleRepresentative =
  let degreeValue = representativeDegree cocycleRepresentative
      cellByBasisIndex = basisIndexCellMapAtDegreeLocal degreeValue (ocBasisRefs obstructionContext)
   in \basisIndexValue ->
        maybe
          (Left (InvalidTopologyInput ("missing cell for basis index " <> show basisIndexValue <> " at degree " <> show degreeValue)))
          Right
          (Map.lookup basisIndexValue cellByBasisIndex)

lookupBasisRef ::
  (Ord cell, Show cell) =>
  ObstructionContext cell witness ->
  cell ->
  Either HomologyFailure BasisCellRef
lookupBasisRef obstructionContext cellValue =
  maybe
    (Left (InvalidTopologyInput ("missing basis reference for cell " <> show cellValue)))
    Right
    (Map.lookup cellValue (ocBasisRefs obstructionContext))

interpretWitnessEvidence ::
  WitnessClassifier witness ->
  [(cell, witness)] ->
  ([cell], [cell])
interpretWitnessEvidence witnessClassifier =
  foldr
    (\(cellValue, witnessValue) (obstructedCells, composedCells) ->
        if wcIsObstructed witnessClassifier witnessValue
          then (cellValue : obstructedCells, composedCells)
          else
            if wcIsComposed witnessClassifier witnessValue
              then (obstructedCells, cellValue : composedCells)
              else (obstructedCells, composedCells)
    )
    ([], [])

constantDerivedProfileForCells ::
  (Ord cell, Show cell) =>
  ObstructionContext cell witness ->
  [cell] ->
  Either HomologyFailure ConstantDerivedProfile
constantDerivedProfileForCells obstructionContext supportCells = do
  supportNodeSet <- supportNodeSetFromCells obstructionContext supportCells
  supportValue <-
    first
      (BackendFailure . show)
      (mkLocalClosed (ocAmbientPoset obstructionContext) supportNodeSet)
  preparedPullback <-
    first
      (BackendFailure . show)
      (prepareProperPullback supportValue (ocAmbientDerived obstructionContext))
  let supportDerived =
        forgetLocallyRestrictableDerived (properPullback preparedPullback)
  extendedSupportDerived <-
    first
      (BackendFailure . show)
      (properPushforward (ocAmbientPoset obstructionContext) supportNodeSet supportDerived)
  supportHypercohomology <-
    first (BackendFailure . show) (hypercohomologyDims supportDerived)
  extendedSupportHypercohomology <-
    first (BackendFailure . show) (hypercohomologyDims extendedSupportDerived)
  pure
    ConstantDerivedProfile
      { cdpAmbientHypercohomology = ocAmbientHypercohomology obstructionContext,
        cdpSupportHypercohomology = supportHypercohomology,
        cdpExtendedSupportHypercohomology = extendedSupportHypercohomology
      }

supportNodeSetFromCells ::
  (Ord cell, Show cell) =>
  ObstructionContext cell witness ->
  [cell] ->
  Either HomologyFailure IntSet
supportNodeSetFromCells obstructionContext supportCells =
  fmap
    (IntSet.fromList . fmap (ocBasisNodeId obstructionContext))
    (traverse (lookupBasisRef obstructionContext) supportCells)

harmonicLoopTouches :: Set.Set BasisCellRef -> HarmonicLoop -> Bool
harmonicLoopTouches supportBasisRefs harmonicLoopValue =
  let cycleSupport =
        Set.fromList (fmap snd (representativeTerms (harmonicLoopCycle harmonicLoopValue)))
      cocycleSupport =
        Set.fromList (fmap snd (representativeTerms (harmonicLoopCocycle harmonicLoopValue)))
   in not (Set.null (Set.intersection supportBasisRefs (Set.union cycleSupport cocycleSupport)))

nodeForCell ::
  (Ord cell, Show cell) =>
  Map cell BasisCellRef ->
  (BasisCellRef -> Int) ->
  cell ->
  Either HomologyFailure FinObjectId
nodeForCell basisRefs basisNodeId cellValue =
  maybe
    (Left (InvalidTopologyInput ("missing basis reference for cell " <> show cellValue)))
    (Right . FinObjectId . basisNodeId)
    (Map.lookup cellValue basisRefs)

basisIndexCellMapAtDegreeLocal ::
  HomologicalDegree ->
  Map cell BasisCellRef ->
  Map Int cell
basisIndexCellMapAtDegreeLocal degreeValue =
  Map.fromList
    . fmap (\(cellValue, basisCellRef) -> (cellIndex basisCellRef, cellValue))
    . filter (\(_, basisCellRef) -> cellDegree basisCellRef == degreeValue)
    . Map.toList

degreeIndex :: HomologicalDegree -> Int
degreeIndex (HomologicalDegree degreeValue) = degreeValue

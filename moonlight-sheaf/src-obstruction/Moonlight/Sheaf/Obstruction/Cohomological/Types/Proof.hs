module Moonlight.Sheaf.Obstruction.Cohomological.Types.Proof
  ( CycleDescriptor (..),
    ObstructionLift (..),
    ObstructionReason (..),
    ObstructionWitness (..),
    C1LocalConflict (..),
    Nerve1Cochain (..),
    NervePotential (..),
    CycleExactness (..),
    H1Class (..),
    CycleCohomologyReport (..),
    AnalysisTruncationCause (..),
    AnalysisCompleteness (..),
    CoverCohomologyReport (..),
    QuotientCoverCohomologyReport,
    AssignmentCoverCohomologyReport,
    TupleWitnessCoverCohomologyReport,
    AssignmentWitnessCoverCohomologyReport,
  )
where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Moonlight.Homology (FiniteChainComplex)
import Moonlight.Sheaf.Descent.Assignment qualified as AssignmentDescent
import Moonlight.Sheaf.Descent.Kernel
  ( CoverSearchBudget (..),
    CoverSearchRefusal,
  )
import Moonlight.Sheaf.Descent.Quotient qualified as QuotientDescent
import Moonlight.Sheaf.Obstruction.Cohomological.Types.Core (ConstraintId, CycleId)
import Moonlight.Sheaf.Obstruction.Cohomological.Types.Fact
  ( ExpandedObstructionCell,
    ExpandedStalk,
    ObstructionCell,
    RelationFlavor (..),
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Types.Provenance (CoverNerve, OrientedNerveEdge)
import Moonlight.Sheaf.Obstruction.Cohomological.Types.Witness
  ( AssignmentWitnessStalk,
    TupleWitnessStalk,
  )
import Moonlight.Sheaf.Operator.GradedComplex
import Moonlight.Sheaf.Section.Store.Types (TotalSectionStore)
import Moonlight.Sheaf.Section.Restriction (RestrictionIndex)
import Moonlight.Pale.Diagnostic.Site.Cohomology (CoboundaryConstructionError)
import Numeric.Natural (Natural)

type CycleDescriptor :: Type
data CycleDescriptor = CycleDescriptor
  { ccdId :: !CycleId,
    ccdBoundary :: ![ExpandedObstructionCell]
  }
  deriving stock (Eq, Show, Read)

type ObstructionLift :: Type -> Type -> Type -> Type
data ObstructionLift root region witness = ObstructionLift
  { olRegion :: !region,
    olExpandedComplex :: !(FiniteChainComplex Integer),
    olRestrictions :: !(RestrictionIndex ExpandedObstructionCell witness),
    olCoboundaryCache :: !(GradedComplex ExpandedObstructionCell Int),
    olSection0 :: !(TotalSectionStore ExpandedObstructionCell ExpandedStalk),
    olRoot :: !root,
    olExactH1Eligible :: !Bool
  }

type ObstructionReason :: Type -> Type -> Type
data ObstructionReason region diagnostic
  = EmptyLocalDomain !ObstructionCell
  | EmptyConstraintFiber !ConstraintId
  | EmptyGuardFiber !ConstraintId
  | EmptyRelationFiber !RelationFlavor !ConstraintId
  | ModalityCoverageMismatch !diagnostic
  | KernelVerdictObstructed
  | MalformedCohomologyComplex !CoboundaryConstructionError
  | PositiveFirstCohomology !Int
  | CoarseRegionObstructed !region
  deriving stock (Eq, Show, Read)

type ObstructionWitness :: Type -> Type -> Type -> Type -> Type
data ObstructionWitness root region purpose diagnostic = ObstructionWitness
  { owRootClass :: !root,
    owRegion :: !region,
    owPurpose :: !purpose,
    owPatternFingerprint :: !Int,
    owEnvironmentFingerprint :: !(Maybe Int),
    owCells :: ![ObstructionCell],
    owReason :: !(ObstructionReason region diagnostic),
    owKernelRankLowerBound :: !Int,
    owImageRankUpperBound :: !Int,
    owRepresentativeCocycleCount :: !Int
  }
  deriving stock (Eq, Show, Read)

type C1LocalConflict :: Type -> Type -> Type -> Type
data C1LocalConflict ctx obstruction witness = C1LocalConflict
  { c1cContext :: !ctx,
    c1cDescentObstruction :: !obstruction,
    c1cWitness :: !witness,
    c1cSupport :: !(Set ctx)
  }
  deriving stock (Eq, Show)

type Nerve1Cochain :: Type -> Type -> Type
newtype Nerve1Cochain ctx witness = Nerve1Cochain
  { n1cEdges :: Map (OrientedNerveEdge ctx) witness
  }
  deriving stock (Eq, Show)

type NervePotential :: Type -> Type -> Type
newtype NervePotential ctx witness = NervePotential
  { npVertices :: Map ctx witness
  }
  deriving stock (Eq, Show)

type CycleExactness :: Type -> Type -> Type
data CycleExactness ctx witness
  = ExactOnCycle !(NervePotential ctx witness)
  | NonExactOnCycle !(H1Class ctx witness)
  deriving stock (Eq, Show)

type H1Class :: Type -> Type -> Type
data H1Class ctx witness = H1Class
  { h1cCycle :: !(NonEmpty ctx),
    h1cRepresentative :: !(Nerve1Cochain ctx witness),
    h1cIntegral :: !witness,
    h1cSupport :: !(Set ctx),
    h1cTouchedLocalContexts :: !(Set ctx),
    h1cMagnitude :: !Int
  }
  deriving stock (Eq, Show)

type CycleCohomologyReport :: Type -> Type -> Type
data CycleCohomologyReport ctx witness = CycleCohomologyReport
  { ccrCycle :: !(NonEmpty ctx),
    ccrRepresentative :: !(Nerve1Cochain ctx witness),
    ccrIntegral :: !witness,
    ccrSupport :: !(Set ctx),
    ccrTouchedLocalContexts :: !(Set ctx),
    ccrExactness :: !(CycleExactness ctx witness)
  }
  deriving stock (Eq, Show)

type AnalysisTruncationCause :: Type
data AnalysisTruncationCause
  = TruncatedByDescentSearchRefusal !CoverSearchBudget !Natural
  | TruncatedByExactCoverageSkipped
  deriving stock (Eq, Show)

instance Ord AnalysisTruncationCause where
  compare leftCause rightCause =
    case (leftCause, rightCause) of
      (TruncatedByExactCoverageSkipped, TruncatedByExactCoverageSkipped) ->
        EQ
      (TruncatedByExactCoverageSkipped, TruncatedByDescentSearchRefusal {}) ->
        LT
      (TruncatedByDescentSearchRefusal {}, TruncatedByExactCoverageSkipped) ->
        GT
      (TruncatedByDescentSearchRefusal (CoverSearchBudget leftBudget) leftBound, TruncatedByDescentSearchRefusal (CoverSearchBudget rightBudget) rightBound) ->
        compare (leftBudget, leftBound) (rightBudget, rightBound)

type AnalysisCompleteness :: Type
data AnalysisCompleteness
  = AnalysisComplete
  | AnalysisTruncated !(NonEmpty AnalysisTruncationCause)
  deriving stock (Eq, Show)

type CoverCohomologyReport :: Type -> Type -> Type -> Type -> Type
data CoverCohomologyReport ctx report obstruction witness = CoverCohomologyReport
  { corNerve :: !(CoverNerve ctx),
    corDescentReport :: !report,
    corCompleteness :: !AnalysisCompleteness,
    corLocalC1Conflicts :: ![C1LocalConflict ctx obstruction witness],
    corCycleReports :: !(Map (NonEmpty ctx) (CycleCohomologyReport ctx witness)),
    corH1Obstructions :: ![H1Class ctx witness]
  }
  deriving stock (Eq, Show)

type QuotientCoverCohomologyReport :: Type -> Type -> Type -> Type
type QuotientCoverCohomologyReport ctx rep witness =
  CoverCohomologyReport ctx (QuotientDescent.DescentReport ctx (CoverSearchRefusal Int) (QuotientDescent.QuotientDescentObstruction ctx rep)) (QuotientDescent.QuotientDescentObstruction ctx rep) witness

type AssignmentCoverCohomologyReport :: Type -> Type -> Type -> Type -> Type -> Type -> Type
type AssignmentCoverCohomologyReport ctx coord value admissibilityWitness admissibilityCost witness =
  CoverCohomologyReport
    ctx
    (AssignmentDescent.DescentReport ctx (CoverSearchRefusal ctx) (AssignmentDescent.AssignmentDescentObstruction ctx coord value admissibilityWitness admissibilityCost))
    (AssignmentDescent.AssignmentDescentObstruction ctx coord value admissibilityWitness admissibilityCost)
    witness

type TupleWitnessCoverCohomologyReport :: Type -> Type -> Type
type TupleWitnessCoverCohomologyReport ctx rep =
  QuotientCoverCohomologyReport ctx rep (TupleWitnessStalk ctx rep)

type AssignmentWitnessCoverCohomologyReport :: Type -> Type -> Type -> Type -> Type -> Type
type AssignmentWitnessCoverCohomologyReport ctx coord value admissibilityWitness admissibilityCost =
  AssignmentCoverCohomologyReport
    ctx
    coord
    value
    admissibilityWitness
    admissibilityCost
    (AssignmentWitnessStalk ctx coord value)

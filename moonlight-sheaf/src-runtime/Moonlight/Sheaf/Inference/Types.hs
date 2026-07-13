

module Moonlight.Sheaf.Inference.Types where

import Data.IntMap.Strict (IntMap)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Vector qualified as VB
import Data.Vector.Unboxed qualified as VU

type DomainIndex :: Type -> Type -> Type
data DomainIndex pid obj = DomainIndex
  { diVars        :: !(VB.Vector pid)
  , diVarIx       :: !(Map pid Int)
  , diDomains     :: !(VB.Vector (VB.Vector obj))
  , diObjIx       :: !(VB.Vector (Map obj Int))
  , diDomainSizes :: !(VU.Vector Int)
  }

type FactorRow :: Type
data FactorRow = FactorRow
  { frAssign    :: !(VU.Vector Int)
  , frLogWeight :: !Double
  }

type WeightedFactor :: Type
data WeightedFactor = WeightedFactor
  { wfScope :: !(VU.Vector Int)
  , wfRows  :: !(VB.Vector FactorRow)
  }

type FactorSpec :: Type -> Type -> Type
data FactorSpec pid obj = FactorSpec
  { fsScope  :: !(Set pid)
  , fsTuples :: ![(Map pid obj, Double)]
  }

type FactorCompileError :: Type -> Type -> Type
data FactorCompileError pid obj
  = FactorScopeUnknownVariable !pid
  | FactorTupleMissingVariable !pid
  | FactorTupleObjectOutOfDomain !pid !obj
  deriving stock (Eq, Ord, Show)

type BlueprintError :: Type -> Type -> Type
data BlueprintError pid obj
  = BlueprintEmptyDomain !pid
  | BlueprintFactorCompileError !(FactorCompileError pid obj)
  | BlueprintFactorScopeIndexOutOfRange !Int !Int !Int
  | BlueprintFactorScopeNotStrictlyAscending !Int !(VU.Vector Int)
  | BlueprintFactorRowArityMismatch !Int !Int !Int
  | BlueprintFactorRowValueOutOfBounds !Int !Int !Int !Int
  | BlueprintFactorDuplicateAssignment !Int !Int
  deriving stock (Eq, Show)

type InferenceExecutionError :: Type
data InferenceExecutionError
  = InferenceMissingDownstreamAssignment !Int !Int
  | InferenceMissingArgmaxChoice !Int !Int
  | InferenceResidualFactorAfterElimination !(VU.Vector Int)
  | InferenceJoinScopeInvariantViolation !Int
  deriving stock (Eq, Show)

type WeightedBlueprint :: Type -> Type -> Type
data WeightedBlueprint pid obj = WeightedBlueprint
  { wbIndex   :: !(DomainIndex pid obj)
  , wbFactors :: !(VB.Vector WeightedFactor)
  }

type EliminationHeuristic :: Type
data EliminationHeuristic
  = MinFill
  | MinDegree
  deriving stock (Eq, Show)

type InferenceConfig :: Type
data InferenceConfig = InferenceConfig
  { icEliminationHeuristic :: !EliminationHeuristic
  } deriving stock (Eq, Show)

type MapSolution :: Type -> Type -> Type
data MapSolution pid obj = MapSolution
  { msAssignment :: !(Map pid obj)
  , msLogScore   :: !Double
  }

type SectionPosterior :: Type -> Type -> Type
data SectionPosterior pid obj = SectionPosterior
  { spLogPartition :: !Double
  , spMarginals    :: !(Map pid (Map obj Double))
  , spMap          :: !(MapSolution pid obj)
  }

type DecisionFactor :: Type
data DecisionFactor = DecisionFactor
  { dfVar    :: !Int
  , dfScope  :: !(VU.Vector Int)
  , dfChoice :: !(IntMap Int)
  }

type JoinPlan :: Type
data JoinPlan = JoinPlan
  { jpUnionScope  :: !(VU.Vector Int)
  , jpUnionDims   :: !(VU.Vector Int)
  , jpUnionStrides :: !(VU.Vector Int)
  , jpOverlapPosA :: !(VU.Vector Int)
  , jpOverlapPosB :: !(VU.Vector Int)
  , jpOverlapDims :: !(VU.Vector Int)
  , jpOverlapStrides :: !(VU.Vector Int)
  , jpFromA       :: !(VU.Vector Bool)
  , jpSourcePos   :: !(VU.Vector Int)
  }

defaultInferenceConfig :: InferenceConfig
defaultInferenceConfig =
  InferenceConfig
    { icEliminationHeuristic = MinFill
    }

module Moonlight.EGraph.Pure.AntiUnify
  ( AntiUnifyObstruction (..),
    AntiUnifyInputIndex (..),
    BinaryLGGResult (..),
    NaryLGGResult (..),
    antiUnify,
    antiUnifyAll,
  )
where

import Moonlight.Core (ZipMatch)
import Data.Bifunctor (first)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Moonlight.EGraph.Pure.Extraction
  ( CostAlgebra,
    ExtractionChoiceSection,
    ExtractionResult (..),
    extractChoiceSection,
    extractFromChoiceSection,
    liftCostAlgebra,
    stableExtractionSnapshotFromEGraph,
    stableExtractionSnapshotTable,
  )
import Moonlight.EGraph.Pure.Kernel.HashCons (addTerm)
import Moonlight.Core (Language, UnionFindAllocationError)
import Moonlight.EGraph.Pure.Types (ClassId, EGraph)
import Data.Fix (Fix)
import Numeric.Natural (Natural)
import Moonlight.Core.Pattern.AntiUnify
  ( BinaryLGGResult (..),
    NaryLGGResult (..),
    antiUnifyAllWithTermStore,
    antiUnifyWithTermStore,
  )

type AntiUnifyInputIndex :: Type
newtype AntiUnifyInputIndex = AntiUnifyInputIndex
  { antiUnifyInputIndex :: Natural
  }
  deriving stock (Eq, Ord, Show)

type AntiUnifyObstruction :: Type
data AntiUnifyObstruction
  = AntiUnifyGraphNotStable
  | AntiUnifyClassNotExtractable !AntiUnifyInputIndex !ClassId
  | AntiUnifyClassIdAllocationFailed !UnionFindAllocationError
  deriving stock (Eq, Ord, Show)

antiUnify ::
  (Language f, ZipMatch f, Ord cost) =>
  CostAlgebra f cost ->
  ClassId ->
  ClassId ->
  EGraph f a ->
  Either AntiUnifyObstruction (BinaryLGGResult f ClassId)
antiUnify costAlgebra leftClassId rightClassId graph = do
  snapshot <-
    maybe
      (Left AntiUnifyGraphNotStable)
      Right
      (stableExtractionSnapshotFromEGraph graph)
  let choiceSection =
        extractChoiceSection (liftCostAlgebra costAlgebra) (stableExtractionSnapshotTable snapshot)
  leftTerm <-
    representativeTerm (AntiUnifyInputIndex 0) leftClassId choiceSection
  rightTerm <-
    representativeTerm (AntiUnifyInputIndex 1) rightClassId choiceSection
  fst
    <$> first
      AntiUnifyClassIdAllocationFailed
      (antiUnifyWithTermStore addTerm graph leftTerm rightTerm)

antiUnifyAll ::
  (Language f, ZipMatch f, Ord cost) =>
  CostAlgebra f cost ->
  NonEmpty ClassId ->
  EGraph f a ->
  Either AntiUnifyObstruction (NaryLGGResult f ClassId)
antiUnifyAll costAlgebra classIds graph = do
  snapshot <-
    maybe
      (Left AntiUnifyGraphNotStable)
      Right
      (stableExtractionSnapshotFromEGraph graph)
  let choiceSection =
        extractChoiceSection (liftCostAlgebra costAlgebra) (stableExtractionSnapshotTable snapshot)
  terms <-
    traverse
      (\(inputIndex, classId) -> representativeTerm inputIndex classId choiceSection)
      (antiUnifyInputs classIds)
  fst
    <$> first
      AntiUnifyClassIdAllocationFailed
      (antiUnifyAllWithTermStore addTerm graph terms)

antiUnifyInputs :: NonEmpty ClassId -> NonEmpty (AntiUnifyInputIndex, ClassId)
antiUnifyInputs =
  NonEmpty.zip (fmap AntiUnifyInputIndex (0 :| [1 ..]))

representativeTerm ::
  (Language f, Ord cost) =>
  AntiUnifyInputIndex ->
  ClassId ->
  ExtractionChoiceSection f a cost ->
  Either AntiUnifyObstruction (Fix f)
representativeTerm inputIndex classId choiceSection =
  maybe
    (Left (AntiUnifyClassNotExtractable inputIndex classId))
    (Right . erTerm)
    (extractFromChoiceSection classId choiceSection)

module Moonlight.EGraph.Pure.AntiUnify
  ( AntiUnifyObstruction (..),
    AntiUnifySide (..),
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
import Moonlight.EGraph.Pure.Extraction
  ( CostAlgebra (..),
    ExtractionResult (..),
    StableExtractionSnapshot,
    extract,
    stableExtractionSnapshotFromEGraph,
  )
import Moonlight.EGraph.Pure.Kernel.HashCons (addTerm)
import Moonlight.Core (Language, UnionFindAllocationError)
import Moonlight.EGraph.Pure.Types (ClassId, EGraph)
import Data.Fix (Fix)
import Moonlight.Core.Pattern.AntiUnify
  ( BinaryLGGResult (..),
    NaryLGGResult (..),
    antiUnifyAllWithTermStore,
    antiUnifyWithTermStore,
  )

type AntiUnifySide :: Type
data AntiUnifySide
  = AntiUnifyLeft
  | AntiUnifyRight
  deriving stock (Eq, Ord, Show)

type AntiUnifyObstruction :: Type
data AntiUnifyObstruction
  = AntiUnifyGraphNotStable
  | AntiUnifyClassNotExtractable !AntiUnifySide !ClassId
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
  leftTerm <-
    representativeTerm costAlgebra AntiUnifyLeft leftClassId snapshot
  rightTerm <-
    representativeTerm costAlgebra AntiUnifyRight rightClassId snapshot
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
  terms <-
    traverse
      (\(side, classId) -> representativeTerm costAlgebra side classId snapshot)
      (antiUnifySides classIds)
  fst
    <$> first
      AntiUnifyClassIdAllocationFailed
      (antiUnifyAllWithTermStore addTerm graph terms)

antiUnifySides :: NonEmpty ClassId -> NonEmpty (AntiUnifySide, ClassId)
antiUnifySides (firstClass :| remainingClasses) =
  (AntiUnifyLeft, firstClass) :| fmap ((,) AntiUnifyRight) remainingClasses

representativeTerm ::
  (Language f, Ord cost) =>
  CostAlgebra f cost ->
  AntiUnifySide ->
  ClassId ->
  StableExtractionSnapshot f a ->
  Either AntiUnifyObstruction (Fix f)
representativeTerm costAlgebra side classId snapshot =
  maybe
    (Left (AntiUnifyClassNotExtractable side classId))
    (Right . erTerm)
    (extract costAlgebra classId snapshot)

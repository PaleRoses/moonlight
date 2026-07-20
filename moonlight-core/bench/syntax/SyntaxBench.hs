{-# LANGUAGE DeriveTraversable #-}

module SyntaxBench
  ( syntaxBenchmarks,
  )
where

import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.Set qualified as Set
import BenchSupport
  ( caseLabel,
    keys,
    syntaxSizes,
  )
import Moonlight.Core (ClassId (..))
import Moonlight.Core qualified as EGraph
import Moonlight.Core
  ( zipSameNodeShape,
  )
import Moonlight.Core
  ( Pattern (..),
    patternVariables,
  )
import Moonlight.Core
  ( Substitution (..),
    emptySubstitution,
    insertSubst,
    mergeSubstitutions,
  )
import Moonlight.Core
  ( StructuralLaw (..),
    TheorySpec (..),
    commutativeBinary,
    expandPatternByTheory,
  )
import Test.Tasty.Bench
  ( Benchmark,
    bench,
    bgroup,
    nf,
  )
import Prelude

data BenchNode a
  = BenchLeaf !Int
  | BenchUnary !a
  | BenchBinary !a !a
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

syntaxBenchmarks :: Benchmark
syntaxBenchmarks =
  bgroup
    "syntax"
    (syntaxSizes >>= syntaxBenchmarksForSize)

syntaxBenchmarksForSize :: Int -> [Benchmark]
syntaxBenchmarksForSize size =
  [ bench (caseLabel "pattern variables" size) (nf patternVariableWeight size),
    bench (caseLabel "commutative expansion" size) (nf theoryExpansionWeight size),
    bench (caseLabel "substitution incremental merge" size) (nf substitutionMergeWeight size),
    bench (caseLabel "substitution pair merge" size) (nf substitutionPairMergeWeight size),
    bench (caseLabel "hackage: containers IntMap checked union" size) (nf hackageContainersCheckedUnionWeight size),
    bench (caseLabel "same-shape zip" size) (nf shapeZipWeight size)
  ]

patternVariableWeight :: Int -> Int
patternVariableWeight =
  Set.size . patternVariables . benchPattern

theoryExpansionWeight :: Int -> Int
theoryExpansionWeight =
  sum . fmap (length . expandPatternByTheory commutativeTheory . binaryPatternForKey) . keys

substitutionMergeWeight :: Int -> Int
substitutionMergeWeight size =
  case foldl' mergeStep (Just emptySubstitution) (keys size) of
    Nothing -> 0
    Just substitution -> substitutionSize substitution

substitutionPairMergeWeight :: Int -> Int
substitutionPairMergeWeight size =
  case mergeSubstitutions (benchSubstitution size) (benchSubstitution size) of
    Nothing -> 0
    Just substitution -> substitutionSize substitution

hackageContainersCheckedUnionWeight :: Int -> Int
hackageContainersCheckedUnionWeight size =
  maybe 0 IntMap.size (checkedIntMapUnion (benchSubstitutionMap size) (benchSubstitutionMap size))

checkedIntMapUnion :: IntMap ClassId -> IntMap ClassId -> Maybe (IntMap ClassId)
checkedIntMapUnion left right =
  if IntMap.null conflicts
    then Just (IntMap.union left right)
    else Nothing
  where
    conflicts =
      IntMap.filter (uncurry (/=)) (IntMap.intersectionWith (,) left right)

shapeZipWeight :: Int -> Int
shapeZipWeight size =
  case zipSameNodeShape (benchNode size) (benchNode (size + 1)) of
    Nothing -> 0
    Just zipped -> length zipped

benchPattern :: Int -> Pattern BenchNode
benchPattern size =
  foldr
    (\key rest -> PatternNode (BenchBinary (PatternVar (EGraph.mkPatternVar key)) rest))
    (PatternNode (BenchLeaf 0))
    (keys size)

binaryPatternForKey :: Int -> Pattern BenchNode
binaryPatternForKey key =
  PatternNode
    ( BenchBinary
        (PatternVar (EGraph.mkPatternVar key))
        (PatternNode (BenchLeaf key))
    )

benchNode :: Int -> BenchNode Int
benchNode size =
  BenchBinary (size + 1) (size + 2)

commutativeTheory :: TheorySpec BenchNode
commutativeTheory =
  TheorySpec {tsClassify = classify}
  where
    classify node =
      case node of
        BenchBinary _left _right -> commutativeBinary BenchBinary
        _ -> Ordinary

    classify :: BenchNode value -> StructuralLaw BenchNode value

mergeStep :: Maybe Substitution -> Int -> Maybe Substitution
mergeStep maybeSubstitution key =
  maybeSubstitution >>= \substitution ->
    mergeSubstitutions substitution (insertSubst (EGraph.mkPatternVar key) (ClassId key) emptySubstitution)

benchSubstitution :: Int -> Substitution
benchSubstitution =
  Substitution . benchSubstitutionMap

benchSubstitutionMap :: Int -> IntMap ClassId
benchSubstitutionMap size =
  IntMap.fromAscList
    [ (key, ClassId key)
    | key <- keys size
    ]

substitutionSize :: Substitution -> Int
substitutionSize (Substitution entries) =
  IntMap.size entries

module Moonlight.Control.ProgramSpec
  ( tests,
  )
where

import Numeric.Natural (Natural)

import Moonlight.Control.Class
  ( Control (..),
    sequenceAll,
  )
import Moonlight.Control.Program
  ( ProgramAlgebra (..),
    foldProgram,
    programContexts,
    programPhases,
    programSize,
  )
import Moonlight.Control.Program.Internal
  ( Program (..),
    normalize,
    seqSpine,
    structuralEq,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))
import Test.Tasty.QuickCheck qualified as QC

tests :: TestTree
tests =
  testGroup
    "Program structure"
    [ testCase "Functor substitution distributes over andThen" testFunctorDistributesOverAndThen,
      testCase "Monad bind substitutes phases structurally" testMonadBindSubstitutes,
      testCase "programPhases enumerates phases left-to-right" testProgramPhasesOrder,
      testCase "programContexts enumerates scoped contexts outside-in" testProgramContextsOrder,
      testCase "programSize counts all constructors" testProgramSizeCountsAll,
      testCase "normalize eliminates skip in seq spine" testNormalizeSeqEliminatesSkip,
      testCase "normalize right-nests seq spine" testNormalizeSeqRightNested,
      testCase "normalize preserves skip branches in or spine" testNormalizeOrPreservesSkip,
      testCase "normalize fuses adjacent scopes" testNormalizeScopeFusion,
      testCase "normalize does not collapse nested attempt" testNormalizeNestedAttemptPreserved,
      testCase "upTo 0 reduces to skip" testUpToZeroIsSkip,
      testCase "upTo n skip reduces to skip" testUpToSkipIsSkip,
      testCase "attempt skip reduces to skip" testAttemptSkipIsSkip,
      testCase "scoped mempty skip reduces to skip" testScopedSkipIsSkip,
      testCase "canonical equality vs structural equality distinction" testCanonicalVsStructuralEq,
      QC.testProperty "normalize is idempotent" prop_normalizeIdempotent,
      QC.testProperty "foldProgram with class algebra round-trips through Program" prop_foldRoundTrip,
      QC.testProperty "programSize is positive" prop_programSizePositive
    ]

newtype AProgram = AProgram
  { aProgram :: Program () Int
  }
  deriving stock (Eq, Show)

instance QC.Arbitrary AProgram where
  arbitrary = QC.sized genProgram
  shrink _ = []

genProgram :: Int -> QC.Gen AProgram
genProgram size
  | size <= 0 =
      AProgram <$> genLeaf
  | otherwise =
      QC.frequency
        [ (3, AProgram <$> genLeaf),
          (2, fmap (AProgram . uncurry andThen) ((,) <$> subtree <*> subtree)),
          (1, fmap (AProgram . uncurry orElse) ((,) <$> subtree <*> subtree)),
          (2, AProgram <$> (upTo <$> genNat <*> subtree)),
          (2, fmap (AProgram . attempt) subtree),
          (1, AProgram <$> (scoped () <$> subtree))
        ]
  where
    subtree = aProgram <$> genProgram (size `div` 2)
    genLeaf =
      QC.frequency
        [ (1, pure skip),
          (5, phase <$> QC.chooseInt (-8, 8))
        ]
    genNat :: QC.Gen Natural
    genNat = fromIntegral <$> QC.chooseInt (0, 4)

testFunctorDistributesOverAndThen :: Assertion
testFunctorDistributesOverAndThen = do
  let p :: Program () Int
      p = andThen (phase 1) (phase 2)
      q = fmap (* 10) p
  programPhases q @?= [10, 20]

testMonadBindSubstitutes :: Assertion
testMonadBindSubstitutes = do
  let p :: Program () Int
      p = andThen (phase 0) (orElse (phase 1) (phase 2))
      q :: Program () String
      q = p >>= \n -> phase (show n)
  programPhases q @?= ["0", "1", "2"]

testProgramPhasesOrder :: Assertion
testProgramPhasesOrder = do
  let p :: Program () String
      p = sequenceAll [phase "a", phase "b", phase "c"]
  programPhases p @?= ["a", "b", "c"]

testProgramContextsOrder :: Assertion
testProgramContextsOrder = do
  let p :: Program String String
      p = andThen (scoped "outer" (scoped "inner" (phase "x"))) (scoped "solo" (phase "y"))
  programContexts p @?= ["outer", "inner", "solo"]

testProgramSizeCountsAll :: Assertion
testProgramSizeCountsAll = do
  let p :: Program () Int
      p = andThen (attempt (phase 1)) (upTo 2 (phase 2))
  programSize p @?= 5

testNormalizeSeqEliminatesSkip :: Assertion
testNormalizeSeqEliminatesSkip = do
  let p :: Program () String
      p = sequenceAll [skip, phase "a", skip, phase "b", skip]
      n = normalize p
  programPhases n @?= ["a", "b"]
  let spine = seqSpine n
  assertBool "normalized seq is not Skip" (case n of { Skip -> False; _ -> True })
  length spine @?= 2

testNormalizeSeqRightNested :: Assertion
testNormalizeSeqRightNested = do
  let p :: Program () Int
      p = andThen (andThen (phase 1) (phase 2)) (phase 3)
      n = normalize p
  structuralEq n (Seq (Phase 1) (Seq (Phase 2) (Phase 3))) @?= True

testNormalizeOrPreservesSkip :: Assertion
testNormalizeOrPreservesSkip = do
  let p :: Program () Int
      p = orElse skip (phase 1)
      n = normalize p
  structuralEq n (Or Skip (Phase 1)) @?= True

testNormalizeScopeFusion :: Assertion
testNormalizeScopeFusion = do
  let p :: Program String Int
      p = scoped "a" (scoped "b" (phase 1))
      n = normalize p
  structuralEq n (Scoped "ab" (Phase 1)) @?= True

testNormalizeNestedAttemptPreserved :: Assertion
testNormalizeNestedAttemptPreserved = do
  let p :: Program () Int
      p = attempt (attempt (phase 1))
      n = normalize p
  structuralEq n (Attempt (Attempt (Phase 1))) @?= True

testUpToZeroIsSkip :: Assertion
testUpToZeroIsSkip = do
  let p :: Program () Int
      p = upTo 0 (phase 42)
  structuralEq p Skip @?= True

testUpToSkipIsSkip :: Assertion
testUpToSkipIsSkip = do
  let p :: Program () Int
      p = upTo 3 skip
  structuralEq p Skip @?= True

testAttemptSkipIsSkip :: Assertion
testAttemptSkipIsSkip = do
  let p :: Program () Int
      p = attempt skip
  structuralEq p Skip @?= True

testScopedSkipIsSkip :: Assertion
testScopedSkipIsSkip = do
  let p :: Program String Int
      p = scoped mempty skip
  structuralEq p Skip @?= True

testCanonicalVsStructuralEq :: Assertion
testCanonicalVsStructuralEq = do
  let left :: Program () Int
      left = andThen (andThen (phase 1) (phase 2)) (phase 3)
      right :: Program () Int
      right = andThen (phase 1) (andThen (phase 2) (phase 3))
  left == right @?= True
  structuralEq left right @?= False

prop_normalizeIdempotent :: AProgram -> QC.Property
prop_normalizeIdempotent (AProgram p) =
  normalize (normalize p) QC.=== normalize p

prop_foldRoundTrip :: AProgram -> QC.Property
prop_foldRoundTrip (AProgram p) =
  foldProgram
    ProgramAlgebra
      { paSkip = skip,
        paPhase = phase,
        paSeq = andThen,
        paOr = orElse,
        paUpTo = upTo,
        paAttempt = attempt,
        paScoped = scoped
      }
    (normalize p)
    QC.=== normalize p

prop_programSizePositive :: AProgram -> QC.Property
prop_programSizePositive (AProgram p) =
  QC.counterexample (show p) (programSize p >= 1)

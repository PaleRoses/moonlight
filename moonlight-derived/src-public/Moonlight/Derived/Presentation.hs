{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | A compilation surface for authoring a 'Derived' object from a flat list of
-- labeled differentials. This module is /syntax and compilation only/: a
-- 'DerivedSpec' is unvalidated input data — the exact analogue of a list of matrix
-- entries — and it is never consumed by anything downstream. 'compileDerived' is the
-- sole exit, and its result is always the sealed, normalized 'Derived' carrier; the
-- specification itself carries no invariants and holds no meaning once compiled.
--
-- The pipeline is: each 'DifferentialSpec' is expanded against its row and column
-- labels into a blocked differential, the differentials are assembled into an
-- injective complex, the complex is minimized over the site, and the minimal complex
-- is normalized into the canonical node order. Every stage's failure is reflected
-- into 'DerivedBuildError'; presentation-level shape faults are detected here, before
-- delegating, so they can name the offending differential by index.
module Moonlight.Derived.Presentation
  ( DerivedSpec (..)
  , DifferentialSpec (..)
  , DerivedBuildError (..)
  , compileDerived
  ) where

import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Moonlight.Algebra (IntegralDomain)
import Moonlight.Core (Field, MoonlightError)
import Moonlight.Derived.Pure.Failure (DerivedFailure (..))
import Moonlight.Derived.Pure.Gluing.Peeling (minimizeComposableComplex)
import Moonlight.Derived.Pure.Site.InjectiveComplex
  ( Derived
  , mkComposableInjectiveComplex
  , mkNormalizedDerivedFromComposableChecked
  )
import Moonlight.Derived.Pure.Site.LabeledMatrix
  ( BlockedMat
  , DenseMat (..)
  , denseMatCols
  , denseMatRows
  , fromExpandedChecked
  )
import Moonlight.Derived.Pure.Site.Poset
  ( DerivedPoset (..)
  , FinObjectId (..)
  , leq
  , memberOfDerivedPoset
  )

-- | One labeled differential of a presentation: a dense coefficient matrix together
-- with the graded row and column labels that name its object axes. The label vectors
-- must match the matrix shape — that agreement is what 'fromExpandedChecked' reads to
-- recover the blocked, axis-grouped form.
data DifferentialSpec a = DifferentialSpec
  { diffRowLabels :: !(Vector FinObjectId)
  , diffColLabels :: !(Vector FinObjectId)
  , diffMatrix :: !(DenseMat a)
  }

-- | A presentation of a derived object: the starting cohomological degree together
-- with the ordered list of differentials, low degree first. Pure input data — see the
-- module header.
data DerivedSpec a = DerivedSpec
  { dsStartDegree :: !Int
  , dsDifferentials :: ![DifferentialSpec a]
  }

-- | The typed faults of compilation. 'DerivedBuildFailure' and
-- 'DerivedBuildKernelError' reflect the two failure vocabularies the kernel pipeline
-- speaks — validated-carrier faults and coefficient-algebra faults respectively.
-- 'DerivedBuildRowLabelMismatch' and 'DerivedBuildColLabelMismatch' are
-- presentation-level shape faults, each naming the zero-based index of the offending
-- differential. 'DerivedBuildOrderViolation' is the compiler's sheaf-lawfulness
-- fault, naming the degree of the offending differential and
-- the target and source labels of a nonzero entry that crosses the order of the
-- site. The constructors from 'DerivedBuildDuplicateDegree' onward are the
-- authoring faults of the name-binding dialect in
-- "Moonlight.Derived.Presentation.Builder", named by degree.
data DerivedBuildError
  = DerivedBuildFailure !DerivedFailure
  | DerivedBuildKernelError !MoonlightError
  | DerivedBuildEmpty
  | DerivedBuildRowLabelMismatch !Int !Int !Int
  | DerivedBuildColLabelMismatch !Int !Int !Int
  | DerivedBuildOrderViolation !Int !FinObjectId !FinObjectId
  | DerivedBuildDuplicateDegree !Int
  | DerivedBuildUnknownDegree !Int
  | DerivedBuildForeignSummand !Int !Int
  | DerivedBuildNonAdjacentDifferential !Int !Int
  | DerivedBuildDuplicateDifferential !Int
  | DerivedBuildComponentDegreeMismatch !Int !Int
  | DerivedBuildDuplicateComponent !Int !Int !Int
  | DerivedBuildDenseShapeMismatch !Int !(Int, Int) !(Int, Int)
  | DerivedBuildDegreeGap !Int
  | DerivedBuildMissingDifferential !Int
  | DerivedBuildPatternFailure !String
  deriving stock (Eq, Show)

-- | Compile a presentation into a sealed derived object over the given site. Returns
-- 'Left' at the first fault, whether a presentation-level shape mismatch, a
-- carrier-validation failure, or a coefficient-algebra failure during minimization.
compileDerived ::
  (Eq a, Field a, IntegralDomain a, Num a) =>
  DerivedPoset -> DerivedSpec a -> Either DerivedBuildError (Derived a)
compileDerived poset DerivedSpec {dsStartDegree, dsDifferentials}
  | null dsDifferentials = Left DerivedBuildEmpty
  | otherwise = do
      sheafLawfulSpec poset (DerivedSpec {dsStartDegree, dsDifferentials})
      blockedDiffs <-
        traverse
          (uncurry compileDifferential)
          (zip [0 ..] dsDifferentials)
      composableComplex <-
        first
          DerivedBuildFailure
          (mkComposableInjectiveComplex dsStartDegree (V.fromList blockedDiffs))
      minimizedComplex <-
        first
          DerivedBuildKernelError
          (minimizeComposableComplex composableComplex)
      first
        DerivedBuildFailure
        (mkNormalizedDerivedFromComposableChecked poset minimizedComplex)

sheafLawfulSpec ::
  (Eq a, Num a) =>
  DerivedPoset -> DerivedSpec a -> Either DerivedBuildError ()
sheafLawfulSpec poset DerivedSpec {dsStartDegree, dsDifferentials} =
  traverse_
    (uncurry (lawfulDifferentialSpec poset))
    (zip [dsStartDegree ..] dsDifferentials)

lawfulDifferentialSpec ::
  (Eq a, Num a) =>
  DerivedPoset -> Int -> DifferentialSpec a -> Either DerivedBuildError ()
lawfulDifferentialSpec poset degree DifferentialSpec
  { diffRowLabels
  , diffColLabels
  , diffMatrix = DenseMat {dmData}
  } =
    traverse_ validateNode diffColLabels
      *> traverse_ validateRow (V.zip diffRowLabels dmData)
  where
    validateNode nodeValue@(FinObjectId nodeKey)
      | memberOfDerivedPoset poset nodeValue = Right ()
      | otherwise = Left (DerivedBuildFailure (DerivedPosetUnknownNode nodeKey))

    validateRow (rowNode, rowValues) =
      validateNode rowNode
        *> traverse_ (validateEntry rowNode) (V.zip diffColLabels rowValues)

    validateEntry rowNode (colNode, entryValue)
      | entryValue == 0 || leq poset rowNode colNode = Right ()
      | otherwise = Left (DerivedBuildOrderViolation degree rowNode colNode)

compileDifferential ::
  (Eq a, Num a) =>
  Int -> DifferentialSpec a -> Either DerivedBuildError (BlockedMat a)
compileDifferential index DifferentialSpec {diffRowLabels, diffColLabels, diffMatrix}
  | V.length diffRowLabels /= denseMatRows diffMatrix =
      Left
        ( DerivedBuildRowLabelMismatch
            index
            (denseMatRows diffMatrix)
            (V.length diffRowLabels)
        )
  | V.length diffColLabels /= denseMatCols diffMatrix =
      Left
        ( DerivedBuildColLabelMismatch
            index
            (denseMatCols diffMatrix)
            (V.length diffColLabels)
        )
  | otherwise =
      first
        DerivedBuildFailure
        (fromExpandedChecked diffRowLabels diffColLabels diffMatrix)

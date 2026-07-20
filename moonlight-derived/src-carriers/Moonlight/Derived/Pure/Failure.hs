{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Derived.Pure.Failure
  ( DerivedFailure (..)
  , derivedFailureMessage
  , derivedFailureToMoonlightError
  ) where

import Data.Kind (Type)
import Moonlight.Core (MoonlightError (..))

type DerivedFailure :: Type
data DerivedFailure
  = DerivedPosetSelfLoop !Int
  | DerivedPosetCycle
  | DerivedPosetUnknownNode !Int
  | DerivedPosetNonThinCategory !String
  | DerivedPosetNonPosetalCategory !String
  | DerivedPosetSiteLoweringFailed !String
  | DerivedMatrixShapeMismatch !String !(Int, Int) !(Int, Int)
  | DerivedMatrixMetadataMismatch !String !(Int, Int) !(Int, Int)
  | DerivedMatrixOutOfBounds !String !Int !Int
  | DerivedComplexEmpty
  | DerivedComplexIncompatibleAxes
  | DerivedComplexNonzeroAdjacentComposition
  | DerivedComplexNonminimal
  | DerivedComplexRestrictionUnstable
  | DerivedMapDegreeMismatch !String
  | DerivedMapAxisMismatch !Int
  | DerivedMapSquareNotCommuting !Int
  | DerivedFunctorInvalidProjection !String
  | DerivedFunctorMissingFiberLift !String
  | DerivedFunctorInvalidSupport !String
  | DerivedFunctorSiteMismatch
  | DerivedFunctorApplicationFailed !String
  | DerivedTensorLayoutMismatch !String
  | DerivedGorensteinHomologyFailure !String
  deriving stock (Eq, Ord, Show, Read)

derivedFailureMessage :: DerivedFailure -> String
derivedFailureMessage failureValue =
  case failureValue of
    DerivedPosetSelfLoop nodeValue ->
      "derived poset self-loop at node " <> show nodeValue
    DerivedPosetCycle ->
      "derived poset cover relation contains a cycle"
    DerivedPosetUnknownNode nodeValue ->
      "derived poset unknown node " <> show nodeValue
    DerivedPosetNonThinCategory reasonValue ->
      "finite category is not thin: " <> reasonValue
    DerivedPosetNonPosetalCategory reasonValue ->
      "finite category is not posetal: " <> reasonValue
    DerivedPosetSiteLoweringFailed reasonValue ->
      "site lowering to finite category failed: " <> reasonValue
    DerivedMatrixShapeMismatch contextValue expectedShape actualShape ->
      contextValue <> ": matrix shape mismatch; expected " <> show expectedShape <> ", got " <> show actualShape
    DerivedMatrixMetadataMismatch contextValue metadataShape payloadShape ->
      contextValue <> ": dense matrix metadata mismatch; metadata=" <> show metadataShape <> ", payload=" <> show payloadShape
    DerivedMatrixOutOfBounds contextValue axisSizeValue axisIndex ->
      contextValue <> ": index " <> show axisIndex <> " out of bounds for axis cardinality " <> show axisSizeValue
    DerivedComplexEmpty ->
      "derived complex is empty"
    DerivedComplexIncompatibleAxes ->
      "derived complex adjacent differentials have incompatible object axes"
    DerivedComplexNonzeroAdjacentComposition ->
      "derived complex consecutive differentials do not compose to zero"
    DerivedComplexNonminimal ->
      "derived complex is not minimal"
    DerivedComplexRestrictionUnstable ->
      "derived complex does not have a uniform poset-order variance"
    DerivedMapDegreeMismatch reasonValue ->
      "derived map degree windows do not align: " <> reasonValue
    DerivedMapAxisMismatch degreeValue ->
      "derived map component axes do not match source/target object axes at degree " <> show degreeValue
    DerivedMapSquareNotCommuting degreeValue ->
      "derived map square does not commute at degree " <> show degreeValue
    DerivedFunctorInvalidProjection reasonValue ->
      "derived functor invalid projection: " <> reasonValue
    DerivedFunctorMissingFiberLift reasonValue ->
      "derived functor missing fiber lift: " <> reasonValue
    DerivedFunctorInvalidSupport reasonValue ->
      "derived functor invalid support: " <> reasonValue
    DerivedFunctorSiteMismatch ->
      "derived functor operands belong to different poset orders"
    DerivedFunctorApplicationFailed reasonValue ->
      "derived finite functor application failed: " <> reasonValue
    DerivedTensorLayoutMismatch reasonValue ->
      "derived tensor layout mismatch: " <> reasonValue
    DerivedGorensteinHomologyFailure reasonValue ->
      "Gorenstein-star homology construction failed: " <> reasonValue

derivedFailureToMoonlightError :: DerivedFailure -> MoonlightError
derivedFailureToMoonlightError =
  InvariantViolation . derivedFailureMessage

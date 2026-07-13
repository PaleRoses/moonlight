{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE RankNTypes #-}

-- | Cost-based extraction from a relational-front host.
-- Owns the signature-indexed cost algebra, extraction budget config,
-- existential extracted terms, and extraction obstructions.
-- Contracts: extraction canonicalizes the requested class, requires a valid
-- finite host table, and checks the reified term against sort witnesses.
module Moonlight.Rewrite.Relational.Front.Extraction
  ( Cost (..),
    ExtractRoundLimit (..),
    ExtractConfig (..),
    defaultExtractConfig,
    Extracted,
    extractedTerm,
    extractedClass,
    extractedCost,
    SomeExtracted (..),
    ExtractError (..),
    extract,
    extractWith,
    extractSome,
    extractSomeWith,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Data.Type.Equality
  ( (:~:) (..),
  )
import Moonlight.Core
  ( ClassId,
  )
import Moonlight.EGraph.Pure.Extraction.Core
  ( CostAlgebra (..),
    ExtractionConvergenceReport,
    ExtractionFixpointBudget (..),
    ExtractionResult (..),
    ExtractionTable,
    extractionClass,
    extractionTable,
    liftCostAlgebra,
  )
import Moonlight.EGraph.Pure.Extraction.Algebra
  ( extractFromTableBounded,
  )
import Moonlight.EGraph.Pure.Types
  ( ENode (..),
    classIdKey,
  )
import Data.Fix
  ( Fix (..),
  )
import Moonlight.Rewrite.DSL
  ( HTraversable (..),
    K (..),
    Node (..),
    NodeTag,
    RewriteSignature (..),
    SortWitness,
    sameSortWitness,
    sortWitnessSortName,
  )
import Moonlight.Rewrite.DSL
  ( SortName,
    Term (..),
  )
import Moonlight.Rewrite.Relational.Front.Host
  ( Host,
    hostCanonicalClass,
    hostClassCount,
    hostNodeClasses,
  )
import Numeric.Natural
  ( Natural,
  )

data Cost sig cost = Cost
  { nodeCost :: forall sort. sig sort (K cost) -> cost
  }

data ExtractRoundLimit
  = ExtractAutoRounds
  | ExtractMaxRounds !Natural
  deriving stock (Eq, Ord, Show, Read)

data ExtractConfig = ExtractConfig
  { extractRoundLimit :: !ExtractRoundLimit
  }
  deriving stock (Eq, Ord, Show, Read)

defaultExtractConfig :: ExtractConfig
defaultExtractConfig =
  ExtractConfig
    { extractRoundLimit = ExtractAutoRounds
    }

data Extracted sig cost sort = Extracted
  { extractedTerm :: !(Term sig sort),
    extractedClass :: !ClassId,
    extractedCost :: !cost
  }

data SomeExtracted sig cost where
  SomeExtracted ::
    !(SortWitness sort) ->
    !(Extracted sig cost sort) ->
    SomeExtracted sig cost

data ExtractError
  = ExtractClassMissing !ClassId
  | ExtractSortMismatch !ClassId !SortName !SortName
  | ExtractReifiedChildSortMismatch !SortName !SortName
  | ExtractHostTableInvalid
  | ExtractNoFiniteRepresentative !ClassId
  | ExtractFixpointExhausted !ExtractionConvergenceReport
  deriving stock (Eq, Ord, Show)

data SomeTerm sig where
  SomeTerm :: !(SortWitness sort) -> !(Term sig sort) -> SomeTerm sig

extract ::
  (RewriteSignature sig, Ord (NodeTag sig), Ord cost) =>
  SortWitness sort ->
  Cost sig cost ->
  ClassId ->
  Host sig ->
  Either ExtractError (Extracted sig cost sort)
extract =
  extractWith defaultExtractConfig

extractWith ::
  (RewriteSignature sig, Ord (NodeTag sig), Ord cost) =>
  ExtractConfig ->
  SortWitness sort ->
  Cost sig cost ->
  ClassId ->
  Host sig ->
  Either ExtractError (Extracted sig cost sort)
extractWith config expectedSort costAlgebra classId host = do
  SomeExtracted observedSort extracted <-
    extractSomeWith config costAlgebra classId host
  case sameSortWitness expectedSort observedSort of
    Just Refl ->
      Right extracted

    Nothing ->
      Left
        ( ExtractSortMismatch
            (extractedClass extracted)
            (sortWitnessSortName expectedSort)
            (sortWitnessSortName observedSort)
        )

extractSome ::
  (RewriteSignature sig, Ord (NodeTag sig), Ord cost) =>
  Cost sig cost ->
  ClassId ->
  Host sig ->
  Either ExtractError (SomeExtracted sig cost)
extractSome =
  extractSomeWith defaultExtractConfig

extractSomeWith ::
  (RewriteSignature sig, Ord (NodeTag sig), Ord cost) =>
  ExtractConfig ->
  Cost sig cost ->
  ClassId ->
  Host sig ->
  Either ExtractError (SomeExtracted sig cost)
extractSomeWith config costAlgebra classId host = do
  rootClass <-
    requireCanonicalClass host classId
  extractionHostTable <-
    requireHostExtractionTable host
  let roundLimit =
        effectiveExtractRoundLimit config host
      extractionBudget =
        ExtractionFixpointBudget roundLimit
  extractionResult <-
    case extractFromTableBounded
      extractionBudget
      (liftCostAlgebra (toCostAlgebra costAlgebra))
      rootClass
      extractionHostTable of
      Left convergenceReport ->
        Left (ExtractFixpointExhausted convergenceReport)

      Right Nothing ->
        Left (ExtractNoFiniteRepresentative rootClass)

      Right (Just resultValue) ->
        Right resultValue
  extractedFromFix
    (erClass extractionResult)
    (erCost extractionResult)
    (erTerm extractionResult)

requireHostExtractionTable ::
  RewriteSignature sig =>
  Host sig ->
  Either ExtractError (ExtractionTable (Node sig) ())
requireHostExtractionTable host =
  maybe
    (Left ExtractHostTableInvalid)
    Right
    (hostExtractionTable host)

hostExtractionTable ::
  RewriteSignature sig =>
  Host sig ->
  Maybe (ExtractionTable (Node sig) ())
hostExtractionTable host =
  extractionTable
    ( IntMap.fromList
        [ ( classIdKey classId,
            extractionClass () (fmap ENode nodes)
          )
        | (classId, nodes) <- hostNodeClasses host
        ]
    )
    (hostCanonicalClass host)

toCostAlgebra :: Cost sig cost -> CostAlgebra (Node sig) cost
toCostAlgebra costAlgebra =
  CostAlgebra $ \case
    Node sigNode ->
      nodeCost costAlgebra sigNode

extractedFromFix ::
  RewriteSignature sig =>
  ClassId ->
  cost ->
  Fix (Node sig) ->
  Either ExtractError (SomeExtracted sig cost)
extractedFromFix classId costValue fixTerm = do
  SomeTerm resultSort term <-
    termFromFix fixTerm
  Right
    ( SomeExtracted
        resultSort
        Extracted
          { extractedTerm = term,
            extractedClass = classId,
            extractedCost = costValue
          }
    )

termFromFix ::
  RewriteSignature sig =>
  Fix (Node sig) ->
  Either ExtractError (SomeTerm sig)
termFromFix (Fix (Node sigNode)) = do
  children <-
    htraverseWithSort typedChildFromFix sigNode
  Right (SomeTerm (nodeResultSort sigNode) (TNode children))

typedChildFromFix ::
  RewriteSignature sig =>
  SortWitness sort ->
  K (Fix (Node sig)) sort ->
  Either ExtractError (Term sig sort)
typedChildFromFix expectedSort (K childFix) = do
  SomeTerm observedSort childTerm <-
    termFromFix childFix
  case sameSortWitness expectedSort observedSort of
    Just Refl ->
      Right childTerm

    Nothing ->
      Left
        ( ExtractReifiedChildSortMismatch
            (sortWitnessSortName expectedSort)
            (sortWitnessSortName observedSort)
        )

requireCanonicalClass ::
  Host sig ->
  ClassId ->
  Either ExtractError ClassId
requireCanonicalClass host classId =
  maybe
    (Left (ExtractClassMissing classId))
    Right
    (hostCanonicalClass host classId)

effectiveExtractRoundLimit :: ExtractConfig -> Host sig -> Natural
effectiveExtractRoundLimit config host =
  case extractRoundLimit config of
    ExtractAutoRounds ->
      -- A finite representative can only improve along a simple class chain;
      -- the host topology, not a decorative magic number, sets the default.
      fromIntegral (hostClassCount host + 1)

    ExtractMaxRounds maxRounds ->
      maxRounds

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Sheaf.Query.Restriction
  ( MatchRestrictionError (..),
    ObstructionVerdict,
    PruningReport (..),
    RowPruningObstruction (..),
    RowPruningFootprint (..),
    RowPruningResult (..),
    Verdict (..),
    acceptIfAnyAccepted,
    pruneRowsWithVerdict,
    rowPruningVerdict,
  )
where

import Data.Bifunctor (first)
import Data.IntSet (IntSet)
import Moonlight.Differential.Row.Block
  ( RowDesc,
    RowBlock,
    RowBlockIdentity,
    RowBuildError,
    RowOperationError,
    RowProgramError,
    RowState (Canonical),
    rowBlockLayout,
    foldRowBlock,
    fromSlotRows,
    rowDescSupport,
    rowSlots,
  )
import Moonlight.Sheaf.Pruning (PruningCertificate (..), PruningReport (..))
import Moonlight.Sheaf.Verdict (ObstructionVerdict, Verdict (..), acceptIfAnyAccepted, rejectedFromList)

-- | Typed obstruction bridge for row restriction math. The plan obstruction is
-- supplied by the saturation-owned matcher; sheaf only classifies the restriction
-- failure.
data MatchRestrictionError obstruction
  = MatchRestrictionRowProgramError !RowProgramError
  | MatchRestrictionRowBuildError !RowBuildError
  | MatchRestrictionRowOperationError !RowOperationError
  | MatchRestrictionAtomKeyMismatch !IntSet !IntSet
  | MatchRestrictionPlanObstruction !obstruction
  deriving stock (Eq, Show)

data RowPruningObstruction
  = LocalRowAbsent
  | ChildRestrictionUnsupported
  | ParentLiftUnsupported
  deriving stock (Eq, Ord, Show)

data RowPruningFootprint = RowPruningFootprint
  { rpfRowSupport :: !IntSet
  }
  deriving stock (Eq, Show)

data RowPruningResult obstruction = RowPruningResult
  { rprRows :: !(RowBlock 'Canonical),
    rprRemovedSupport :: !IntSet,
    rprReport :: !(PruningReport RowDesc RowPruningFootprint () obstruction)
  }

pruneRowsWithVerdict ::
  (RowBuildError -> err) ->
  RowBlockIdentity ->
  (RowBlock 'Canonical -> RowDesc -> Either err (ObstructionVerdict obstruction)) ->
  RowBlock 'Canonical ->
  Either err (RowPruningResult obstruction)
pruneRowsWithVerdict buildError identityValue keep rows = foldRowBlock step (Right ([], mempty)) rows >>= finalize
  where
    step accumulator desc = do
      (keptRows, report) <- accumulator
      verdict <- keep rows desc
      let slots = rowSlots rows desc
          footprint =
            RowPruningFootprint
              { rpfRowSupport = rowDescSupport rows desc
              }
      pure $ case verdict of
        Accepted () -> (slots : keptRows, report <> PruningReport [(desc, footprint)] [])
        Rejected obstructions ->
          ( keptRows,
            report
              <> PruningReport
                []
                [ ( desc,
                    PruningCertificate
                      { pcObstructions = obstructions,
                        pcFootprint = footprint,
                        pcDiagnostic = Nothing
                      }
                  )
                ]
          )

    finalize (keptRows, report) = do
      let removedSupport = foldMap (rpfRowSupport . pcFootprint . snd) (prPruned report)
      prunedRows <-
        first buildError $
          fromSlotRows identityValue (rowBlockLayout rows) (reverse keptRows)
      Right
        RowPruningResult
          { rprRows = prunedRows,
            rprRemovedSupport = removedSupport,
            rprReport = report
          }

rowPruningVerdict :: obstruction -> Bool -> ObstructionVerdict obstruction
rowPruningVerdict obstruction allowed = rejectedFromList [obstruction | not allowed]

module Moonlight.Analysis.Summary
  ( StructuralSummary (..),
    StructuralSummaryModel (..),
    summarizeStructure,
  )
where

import Data.Kind (Type)
import Moonlight.Analysis.Homotopy (NerveHomotopyProfile (..))
import Moonlight.Homology (HomologyFailure)
import Moonlight.Sheaf.Cochain.Coboundary (checkCoboundaryNilpotence)
import Moonlight.Sheaf.Operator.GradedComplex (GradedComplex)
import Moonlight.Pale.Diagnostic.Global.Summary (StructuralSummary (..))

type StructuralSummaryModel :: Type -> Type -> Type -> Type
data StructuralSummaryModel structure cell stalk = StructuralSummaryModel
  { ssmCellCount :: structure -> Int,
    ssmRestrictionCount :: structure -> Int,
    ssmCochainComplex :: structure -> Either HomologyFailure (GradedComplex cell Int),
    ssmHomotopyProfile :: structure -> Either HomologyFailure NerveHomotopyProfile
  }

summarizeStructure ::
  StructuralSummaryModel structure cell stalk ->
  structure ->
  Either HomologyFailure StructuralSummary
summarizeStructure summaryModel structureValue = do
  cochainComplex <- ssmCochainComplex summaryModel structureValue
  homotopyProfile <- ssmHomotopyProfile summaryModel structureValue
  pure
    StructuralSummary
      { ssConnectedComponents = nhpConnectedComponents homotopyProfile,
        ssBettiNumbers = nhpBettiVector homotopyProfile,
        ssCellCount = ssmCellCount summaryModel structureValue,
        ssRestrictionCount = ssmRestrictionCount summaryModel structureValue,
        ssCoboundaryNilpotent = checkCoboundaryNilpotence cochainComplex,
        ssMicrosupportSize = Nothing,
        ssCriticalCellCount = Nothing,
        ssNoncriticalFraction = Nothing
      }

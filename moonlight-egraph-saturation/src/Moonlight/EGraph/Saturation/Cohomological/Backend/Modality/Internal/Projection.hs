{-# LANGUAGE PatternSynonyms #-}

module Moonlight.EGraph.Saturation.Cohomological.Backend.Modality.Internal.Projection
  ( equalityConstraints,
    equalityReification,
    bindOccurrence,
  )
where

import Data.IntSet (IntSet)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Moonlight.Core
import Moonlight.Core
  ( Substitution,
    extendSubst
  )
import Moonlight.EGraph.Pure.Types
  ( ClassId (..),
  )
import Moonlight.EGraph.Saturation.Cohomological.Backend.Instance.Internal.Site
  ( EGraphAnchor,
    EGraphExactConstraint,
  )
import Moonlight.EGraph.Saturation.Cohomological.Types
  ( EqualityModalityEnvironment (..),
    PatternOccurrence (..),
  )
import Moonlight.Sheaf.Obstruction
  ( Anchor (..),
    ConstraintId (..),
    ExactLabelCode (..),
    OccurrenceId,
  )
import Moonlight.Sheaf.Obstruction
  ( SectionCoordinate (StructuralCoordinate),
    SectionCoordinate,
  )
import Moonlight.Sheaf.Obstruction
  ( SectionAssignment,
    SectionReification,
    equalityConstraintsBy,
    sectionBinding,
    sectionReification,
  )

equalityReification ::
  EqualityModalityEnvironment f ->
  SectionReification (SectionCoordinate EGraphAnchor) Substitution
equalityReification environment =
  sectionReification
    (\sectionAssignment seedSubstitution ->
       List.foldl'
         (\maybeSubstitution occurrenceValue ->
            maybeSubstitution
              >>= \substitution ->
                case poBoundVariable occurrenceValue of
                  Nothing -> Just substitution
                  Just patternVar ->
                    bindOccurrence patternVar (poId occurrenceValue) sectionAssignment substitution
         )
         (Just seedSubstitution)
         (emeOccurrences environment)
    )

bindOccurrence ::
  PatternVar ->
  OccurrenceId ->
  SectionAssignment (SectionCoordinate EGraphAnchor) ->
  Substitution ->
  Maybe Substitution
bindOccurrence patternVar occurrenceId sectionAssignment substitution =
  case sectionBinding (StructuralCoordinate (OccurrenceAnchor occurrenceId)) sectionAssignment of
    Just (ClassLabelCode classKey) ->
      extendSubst patternVar (ClassId classKey) substitution
    _ -> Nothing

equalityConstraints ::
  Map OccurrenceId IntSet ->
  [PatternOccurrence f] ->
  ConstraintId ->
  [EGraphExactConstraint]
equalityConstraints occurrenceDomains occurrences startingId =
  fst
    ( equalityConstraintsBy
        poId
        poBoundVariable
        occurrenceDomains
        occurrences
        startingId
    )

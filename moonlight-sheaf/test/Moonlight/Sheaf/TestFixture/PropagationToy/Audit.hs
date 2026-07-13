module Moonlight.Sheaf.TestFixture.PropagationToy.Audit
  ( fullToyCompatibilityAuditAfterPatch,
    fullToyCompatibilityAuditAfterPatchWith,
    scopedToyCompatibilityAuditAfterPatch,
    scopedToyCompatibilityAuditAfterPatchWith,
  )
where

import Data.IntSet qualified as IntSet
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Delta.Scope
  ( Scope,
    Scoped (..),
    cleanScope,
    dirtyScope,
    foldScope,
    fullScope,
  )
import Moonlight.Sheaf.Index.Dense (denseIndexKeyOf)
import Moonlight.Sheaf.TestFixture.PropagationToy
  ( ToyCell,
    ToyPatch (..),
    ToyPropagationObstruction (..),
    ToySection,
    ToySheaf,
    ToyStalk,
    toyAlgebra,
    toyPreparedDescent,
    toySheaf,
    toySheafModel,
  )
import Moonlight.Sheaf.Section.Certified
  ( SectionCertification,
    SectionCertificationError,
    SectionCertificationFailure (..),
    certifyPreparedSectionCompatibility,
    certifyPreparedSectionExtentCompatibility,
  )
import Moonlight.Sheaf.Section.Model
  ( sheafModelFingerprint,
    sheafModelObjects,
    sheafModelVersion,
  )
import Moonlight.Sheaf.Section.ObjectIndex
  ( unObjectKey,
  )
import Moonlight.Sheaf.Section.Stalk.Discrete
  ( DiscreteMismatch,
  )
import Moonlight.Sheaf.Section.Store.State
import Moonlight.Sheaf.Section.Store.Types

fullToyCompatibilityAuditAfterPatch ::
  ToySection ->
  Scoped scope ToyPatch ->
  Either ToyPropagationObstruction (SectionCertification ToyCell (DiscreteMismatch ToyStalk))
fullToyCompatibilityAuditAfterPatch section0 delta = do
  sheaf <- toySheaf
  fullToyCompatibilityAuditAfterPatchWith sheaf section0 delta

fullToyCompatibilityAuditAfterPatchWith ::
  ToySheaf ->
  ToySection ->
  Scoped scope ToyPatch ->
  Either ToyPropagationObstruction (SectionCertification ToyCell (DiscreteMismatch ToyStalk))
fullToyCompatibilityAuditAfterPatchWith sheaf section0 delta = do
  section1 <- fullToyCompatibilityAuditPatch sheaf (maybe mempty unToyPatch (payload delta)) section0
  mapToyCertification (certifyPreparedSectionCompatibility (toyPreparedDescent sheaf) toyAlgebra section1)

scopedToyCompatibilityAuditAfterPatch ::
  ToySection ->
  Scoped (Set ToyCell) ToyPatch ->
  Either ToyPropagationObstruction (SectionCertification ToyCell (DiscreteMismatch ToyStalk))
scopedToyCompatibilityAuditAfterPatch section0 delta = do
  sheaf <- toySheaf
  scopedToyCompatibilityAuditAfterPatchWith sheaf section0 delta

scopedToyCompatibilityAuditAfterPatchWith ::
  ToySheaf ->
  ToySection ->
  Scoped (Set ToyCell) ToyPatch ->
  Either ToyPropagationObstruction (SectionCertification ToyCell (DiscreteMismatch ToyStalk))
scopedToyCompatibilityAuditAfterPatchWith sheaf section0 delta = do
  section1 <- fullToyCompatibilityAuditPatch sheaf (maybe mempty unToyPatch (payload delta)) section0
  objectExtent <- toyObjectExtent sheaf (scope delta)
  mapToyCertification
    (certifyPreparedSectionExtentCompatibility (toyPreparedDescent sheaf) toyAlgebra objectExtent section1)

toyObjectExtent ::
  ToySheaf ->
  Scope (Set ToyCell) ->
  Either ToyPropagationObstruction (Scope IntSet.IntSet)
toyObjectExtent sheaf =
  foldScope
    (Right cleanScope)
    (fmap (dirtyScope . IntSet.fromList) . traverse keyOf . Set.toList)
    (Right fullScope)
  where
    keyOf cell =
      maybe
        (Left (ToyPatchSectionStoreFailed (SectionStoreUnknownCell cell)))
        (Right . unObjectKey)
        (denseIndexKeyOf cell (sheafModelObjects (toySheafModel sheaf)))

mapToyCertification ::
  Either (SectionCertificationError ToyCell) certification ->
  Either ToyPropagationObstruction certification
mapToyCertification =
  either (Left . ToySectionRejected . SectionCertificationInfrastructureFailed) Right

fullToyCompatibilityAuditPatch ::
  ToySheaf ->
  Map ToyCell ToyStalk ->
  ToySection ->
  Either ToyPropagationObstruction ToySection
fullToyCompatibilityAuditPatch sheaf assignments sectionValue =
  mapToyStore
    ( assignLocal
        model
        SectionDelta
          { sdModelFingerprint = sheafModelFingerprint model,
            sdModelVersion = sheafModelVersion model,
            sdAssignments = assignments
          }
        sectionValue
    )
  where
    model =
      toySheafModel sheaf

mapToyStore ::
  Either (SectionStoreError ToyCell) value ->
  Either ToyPropagationObstruction value
mapToyStore =
  either (Left . ToyPatchSectionStoreFailed) Right

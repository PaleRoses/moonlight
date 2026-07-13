{-# LANGUAGE StandaloneKindSignatures #-}

-- | Compiled sheaf models whose fingerprints identify model structure, not cell labels.
module Moonlight.Sheaf.Section.Model
  ( ModelFingerprint,
    SheafModel,
    SheafModelBuildError (..),
    SheafModelVersion (..),
    emptySheafModel,
    prepareSheafModel,
    sheafModelFingerprint,
    sheafModelVersion,
    sheafModelObjects,
    sheafModelBasis,
    sheafModelRestrictions,
    sheafModelPlans,
    modelCells,
    modelRestrictionsTo,
  )
where

import Data.Bifunctor (first)
import Data.IntMap.Strict qualified as IntMap
import Data.Kind (Type)
import Data.Void (Void, absurd)
import Data.Word (Word64)
import Moonlight.Core
  ( StableHashDigest,
    stableHashEncodingChunks,
    stableHashEncodingWord64LE,
  )
import Moonlight.Sheaf.Kernel.Basis
  ( SheafBasis,
    mkSheafBasis,
  )
import Moonlight.Sheaf.Index.Dense
  ( denseIndexCount,
    denseIndexValues,
  )
import Moonlight.Sheaf.Section.Morphism
  ( Restriction,
    RestrictionId (..),
    RestrictionKind (..),
    RestrictionPresentation,
    incidenceCoefficientValue,
  )
import Moonlight.Sheaf.Section.ObjectIndex
  ( ObjectIndex,
    ObjectKey (..),
    SheafModelVersion (..),
  )
import Moonlight.Sheaf.Section.Plan
  ( RestrictionPlan (..),
    SheafPlans (..),
    sheafPlansFromRestrictionIndex,
  )
import Moonlight.Sheaf.Section.Restriction
  ( RestrictionIndex,
    RestrictionIndexError,
    buildRestrictionIndex,
    restrictionsTo,
  )

type ModelFingerprint :: Type
newtype ModelFingerprint = ModelFingerprint StableHashDigest
  deriving stock (Eq, Ord, Show)

type SheafModel :: Type -> Type -> Type
data SheafModel cell witness = SheafModel
  { smFingerprint :: !ModelFingerprint,
    smVersion :: !SheafModelVersion,
    smObjects :: !(ObjectIndex cell),
    smBasis :: !(SheafBasis cell),
    smRestrictions :: !(RestrictionIndex cell witness),
    smPlans :: !SheafPlans
  }
  deriving stock (Eq, Show)

type SheafModelBuildError :: Type -> Type
data SheafModelBuildError cell
  = SheafModelRestrictionBuildError !(RestrictionIndexError cell)
  deriving stock (Eq, Show)

emptySheafModel ::
  Ord cell =>
  SheafModelVersion ->
  ObjectIndex cell ->
  Either (SheafModelBuildError cell) (SheafModel cell witness)
emptySheafModel version objects =
  prepareSheafModel version objects absurd ([] :: [Void])

prepareSheafModel ::
  Ord cell =>
  SheafModelVersion ->
  ObjectIndex cell ->
  RestrictionPresentation morphism cell witness ->
  [morphism] ->
  Either (SheafModelBuildError cell) (SheafModel cell witness)
prepareSheafModel version objects present morphisms = do
  restrictions <-
    first SheafModelRestrictionBuildError
      (buildRestrictionIndex objects present morphisms)
  let plans = sheafPlansFromRestrictionIndex restrictions
  pure
    SheafModel
      { smFingerprint = fingerprintModelStructure version (denseIndexCount objects) plans,
        smVersion = version,
        smObjects = objects,
        smBasis = mkSheafBasis (denseIndexValues objects),
        smRestrictions = restrictions,
        smPlans = plans
      }

fingerprintModelStructure :: SheafModelVersion -> Int -> SheafPlans -> ModelFingerprint
fingerprintModelStructure version objectCount plans =
  ModelFingerprint
    ( stableHashEncodingChunks
        stableHashEncodingWord64LE
        ( fromIntegral (unSheafModelVersion version)
            : fromIntegral objectCount
            : concatMap restrictionPlanFingerprintWords (IntMap.elems (spRestrictionPlansById plans))
        )
    )

restrictionPlanFingerprintWords :: RestrictionPlan -> [Word64]
restrictionPlanFingerprintWords restrictionPlan =
  [ fromIntegral (unRestrictionId (rpRestrictionId restrictionPlan)),
    fromIntegral (unObjectKey (rpSourceKey restrictionPlan)),
    fromIntegral (unObjectKey (rpTargetKey restrictionPlan))
  ]
    <> restrictionKindFingerprintWords (rpKind restrictionPlan)

restrictionKindFingerprintWords :: RestrictionKind -> [Word64]
restrictionKindFingerprintWords restrictionKind =
  case restrictionKind of
    IncidenceRestriction coefficient ->
      [0, fromIntegral (incidenceCoefficientValue coefficient)]
    PortalRestriction ->
      [1, 0]

sheafModelFingerprint :: SheafModel cell witness -> ModelFingerprint
sheafModelFingerprint =
  smFingerprint
{-# INLINE sheafModelFingerprint #-}

sheafModelVersion :: SheafModel cell witness -> SheafModelVersion
sheafModelVersion =
  smVersion

sheafModelObjects :: SheafModel cell witness -> ObjectIndex cell
sheafModelObjects =
  smObjects

sheafModelBasis :: SheafModel cell witness -> SheafBasis cell
sheafModelBasis =
  smBasis

sheafModelRestrictions :: SheafModel cell witness -> RestrictionIndex cell witness
sheafModelRestrictions =
  smRestrictions

sheafModelPlans :: SheafModel cell witness -> SheafPlans
sheafModelPlans =
  smPlans

modelCells :: SheafModel cell witness -> [cell]
modelCells =
  denseIndexValues . smObjects

modelRestrictionsTo ::
  Ord cell =>
  cell ->
  SheafModel cell witness ->
  [Restriction cell witness]
modelRestrictionsTo cell model =
  restrictionsTo (smObjects model) cell (smRestrictions model)

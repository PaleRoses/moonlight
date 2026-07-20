{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Sheaf.Site.Class
  ( CheckedMorphism (..),
    PullbackSquare (..),
    CoveringFamily,
    CoverConstructionError (..),
    SiteConstructionError (..),
    SiteLawFailure (..),
    coveringFamilyFromTargetedWitnesses,
    mkCoveringFamily,
    coverTarget,
    coverArrows,
    coverSources,
    coverSize,
    Site (..),
    isIdentityMorphism,
    siteMorphismUniverse,
    siteRestrictionMorphisms,
    identityCover,
    pullbackCoverAlong,
    transitiveCover,
  )
where

import Data.Bifunctor (first)
import Data.Kind (Constraint, Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Semigroup (sconcat)
import Data.Set qualified as Set

type CheckedMorphism :: Type -> Type -> Type
data CheckedMorphism obj mor = CheckedMorphism
  { cmSource :: !obj,
    cmTarget :: !obj,
    cmWitness :: !mor
  }
  deriving stock (Eq, Ord, Show)

type PullbackSquare :: Type -> Type -> Type
data PullbackSquare obj mor = PullbackSquare
  { psLeftBase :: !(CheckedMorphism obj mor),
    psRightBase :: !(CheckedMorphism obj mor),
    psApex :: !obj,
    psToLeft :: !(CheckedMorphism obj mor),
    psToRight :: !(CheckedMorphism obj mor)
  }
  deriving stock (Eq, Ord, Show)

type CoveringFamily :: Type -> Type -> Type
data CoveringFamily obj mor = CoveringFamily
  { cfTarget :: !obj,
    cfArrows :: !(NonEmpty (CheckedMorphism obj mor))
  }
  deriving stock (Eq, Ord, Show)

type CoverConstructionError :: Type -> Type
data CoverConstructionError obj
  = CoverArrowTargetsMismatch !obj ![obj]
  deriving stock (Eq, Ord, Show)

type SiteConstructionError :: Type -> Type -> Type
data SiteConstructionError obj mor
  = PullbackTargetMismatch !obj !obj
  | PullbackUnavailable !(CheckedMorphism obj mor) !(CheckedMorphism obj mor)
  | PullbackLegSourceMismatch !obj !(CheckedMorphism obj mor)
  | PullbackLegTargetMismatch !(CheckedMorphism obj mor) !(CheckedMorphism obj mor)
  | MissingInnerCover !obj
  | InnerCoverTargetMismatch !obj !obj
  | CompositeUnavailable !(CheckedMorphism obj mor) !(CheckedMorphism obj mor)
  | InvalidCover !(CoverConstructionError obj)
  deriving stock (Eq, Ord, Show)

type SiteLawFailure :: Type -> Type -> Type
data SiteLawFailure obj mor
  = IdentityCoverMalformed !obj
  | CompositionUnavailable
      !(CheckedMorphism obj mor)
      !(CheckedMorphism obj mor)
  | CompositeOutsideSiteMorphisms
      !(CheckedMorphism obj mor)
      !(CheckedMorphism obj mor)
      !(CheckedMorphism obj mor)
  | LeftIdentityLawFailed
      !(CheckedMorphism obj mor)
      !(Maybe (CheckedMorphism obj mor))
  | RightIdentityLawFailed
      !(CheckedMorphism obj mor)
      !(Maybe (CheckedMorphism obj mor))
  | AssociativityLawFailed
      !(CheckedMorphism obj mor)
      !(CheckedMorphism obj mor)
      !(CheckedMorphism obj mor)
      !(Maybe (CheckedMorphism obj mor))
      !(Maybe (CheckedMorphism obj mor))
  | PullbackSquareDoesNotCommute
      !(PullbackSquare obj mor)
      !(Maybe (CheckedMorphism obj mor))
      !(Maybe (CheckedMorphism obj mor))
  | PullbackConstructionFailed
      !(CheckedMorphism obj mor)
      !(CoveringFamily obj mor)
      !(SiteConstructionError obj mor)
  | PullbackCoverWrongTarget
      !(CheckedMorphism obj mor)
      !(CoveringFamily obj mor)
      !obj
  | TransitivityConstructionFailed
      !(CoveringFamily obj mor)
      !(SiteConstructionError obj mor)
  | TransitiveCoverWrongTarget
      !(CoveringFamily obj mor)
      !obj
  deriving stock (Eq, Ord, Show)

mkCoveringFamily ::
  Eq obj =>
  obj ->
  NonEmpty (CheckedMorphism obj mor) ->
  Either (CoverConstructionError obj) (CoveringFamily obj mor)
mkCoveringFamily targetObject arrows =
  let mismatchedTargets =
        filter (/= targetObject) (fmap cmTarget (NE.toList arrows))
   in if null mismatchedTargets
        then
          Right
            CoveringFamily
              { cfTarget = targetObject,
                cfArrows = arrows
              }
        else Left (CoverArrowTargetsMismatch targetObject mismatchedTargets)

coveringFamilyFromTargetedWitnesses ::
  obj ->
  NonEmpty (obj, mor) ->
  CoveringFamily obj mor
coveringFamilyFromTargetedWitnesses targetObject =
  CoveringFamily targetObject
    . fmap
      ( \(sourceObject, witnessValue) ->
          CheckedMorphism sourceObject targetObject witnessValue
      )

coverTarget :: CoveringFamily obj mor -> obj
coverTarget =
  cfTarget

coverArrows :: CoveringFamily obj mor -> NonEmpty (CheckedMorphism obj mor)
coverArrows =
  cfArrows

coverSources :: CoveringFamily obj mor -> [obj]
coverSources =
  fmap cmSource . NE.toList . cfArrows

coverSize :: CoveringFamily obj mor -> Int
coverSize =
  NE.length . cfArrows

type Site :: Type -> Constraint
class Ord (SiteObject site) => Site site where
  type SiteObject site :: Type
  type SiteMorphism site :: Type

  siteObjects ::
    site ->
    [SiteObject site]

  siteMorphisms ::
    site ->
    [CheckedMorphism (SiteObject site) (SiteMorphism site)]

  identityAt ::
    site ->
    SiteObject site ->
    CheckedMorphism (SiteObject site) (SiteMorphism site)

  coversAt ::
    site ->
    SiteObject site ->
    [CoveringFamily (SiteObject site) (SiteMorphism site)]

  composeChecked ::
    site ->
    CheckedMorphism (SiteObject site) (SiteMorphism site) ->
    CheckedMorphism (SiteObject site) (SiteMorphism site) ->
    Maybe (CheckedMorphism (SiteObject site) (SiteMorphism site))

  pullbackPair ::
    site ->
    CheckedMorphism (SiteObject site) (SiteMorphism site) ->
    CheckedMorphism (SiteObject site) (SiteMorphism site) ->
    Maybe (PullbackSquare (SiteObject site) (SiteMorphism site))

isIdentityMorphism ::
  (Site site, Eq (SiteMorphism site)) =>
  site ->
  CheckedMorphism (SiteObject site) (SiteMorphism site) ->
  Bool
isIdentityMorphism site morphismValue =
  morphismValue == identityAt site (cmSource morphismValue)

siteMorphismUniverse ::
  (Site site, Ord (SiteMorphism site)) =>
  site ->
  [CheckedMorphism (SiteObject site) (SiteMorphism site)]
siteMorphismUniverse site =
  Set.toAscList . Set.fromList $
    siteMorphisms site <> fmap (identityAt site) (siteObjects site)
{-# INLINEABLE siteMorphismUniverse #-}

siteRestrictionMorphisms ::
  (Site site, Eq (SiteMorphism site)) =>
  site ->
  [CheckedMorphism (SiteObject site) (SiteMorphism site)]
siteRestrictionMorphisms site =
  filter (not . isIdentityMorphism site) (siteMorphisms site)

identityCover ::
  Site site =>
  site ->
  SiteObject site ->
  CoveringFamily (SiteObject site) (SiteMorphism site)
identityCover site objectValue =
  CoveringFamily
    { cfTarget = objectValue,
      cfArrows = identityAt site objectValue :| []
    }

mkSiteCoveringFamily ::
  Eq obj =>
  obj ->
  NonEmpty (CheckedMorphism obj mor) ->
  Either
    (SiteConstructionError obj mor)
    (CoveringFamily obj mor)
mkSiteCoveringFamily targetObject =
  first InvalidCover . mkCoveringFamily targetObject

pullbackCoverAlong ::
  Site site =>
  site ->
  CheckedMorphism (SiteObject site) (SiteMorphism site) ->
  CoveringFamily (SiteObject site) (SiteMorphism site) ->
  Either
    (SiteConstructionError (SiteObject site) (SiteMorphism site))
    (CoveringFamily (SiteObject site) (SiteMorphism site))
pullbackCoverAlong site alongMorphism coverValue
  | coverTarget coverValue /= cmTarget alongMorphism =
      Left
        ( PullbackTargetMismatch
            (coverTarget coverValue)
            (cmTarget alongMorphism)
        )
  | otherwise = do
      pulledBackArrows <-
        traverse
          (pullbackAlongOne site alongMorphism)
          (coverArrows coverValue)
      mkSiteCoveringFamily (cmSource alongMorphism) pulledBackArrows

transitiveCover ::
  Site site =>
  site ->
  CoveringFamily (SiteObject site) (SiteMorphism site) ->
  Map
    (SiteObject site)
    (CoveringFamily (SiteObject site) (SiteMorphism site)) ->
  Either
    (SiteConstructionError (SiteObject site) (SiteMorphism site))
    (CoveringFamily (SiteObject site) (SiteMorphism site))
transitiveCover site outerCover innerCovers = do
  compositeChunks <-
    traverse composeForOuter (coverArrows outerCover)
  mkSiteCoveringFamily
    (coverTarget outerCover)
    (sconcat compositeChunks)
  where
    composeForOuter outerArrow = do
      innerCover <-
        maybe
          (Left (MissingInnerCover (cmSource outerArrow)))
          Right
          (Map.lookup (cmSource outerArrow) innerCovers)
      if coverTarget innerCover /= cmSource outerArrow
        then
          Left
            ( InnerCoverTargetMismatch
                (cmSource outerArrow)
                (coverTarget innerCover)
            )
        else
          traverse
            (composeOne outerArrow)
            (coverArrows innerCover)

    composeOne outerArrow innerArrow =
      maybe
        (Left (CompositeUnavailable outerArrow innerArrow))
        Right
        (composeChecked site outerArrow innerArrow)

pullbackAlongOne ::
  Site site =>
  site ->
  CheckedMorphism (SiteObject site) (SiteMorphism site) ->
  CheckedMorphism (SiteObject site) (SiteMorphism site) ->
  Either
    (SiteConstructionError (SiteObject site) (SiteMorphism site))
    (CheckedMorphism (SiteObject site) (SiteMorphism site))
pullbackAlongOne site alongMorphism coverArrow =
  case pullbackPair site coverArrow alongMorphism of
    Nothing ->
      Left (PullbackUnavailable coverArrow alongMorphism)
    Just square
      | cmSource (psToLeft square) /= psApex square ->
          Left (PullbackLegSourceMismatch (psApex square) (psToLeft square))
      | cmSource (psToRight square) /= psApex square ->
          Left (PullbackLegSourceMismatch (psApex square) (psToRight square))
      | cmTarget (psToLeft square) == cmSource coverArrow
          && cmTarget (psToRight square) == cmSource alongMorphism ->
          Right (psToRight square)
      | otherwise ->
          Left (PullbackLegTargetMismatch coverArrow alongMorphism)

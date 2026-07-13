{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Sheaf.Site.Stalk.Restriction
  ( SiteRestrictionWitness (..),
    SiteRestrictionBuildError (..),
    siteRestrictionStalkAlgebra,
    buildGrothendieckRestrictions,
    buildGrothendieckRestrictionsWithStalkCache,
    buildNerveRestrictions,
  )
where

import Data.Bifunctor (first)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Sheaf.Kernel.Basis
  ( SheafBasis,
    mkSheafBasis,
  )
import Moonlight.Sheaf.Section.Morphism
  ( RestrictionKind,
    RestrictionParts (..),
    mkIncidenceRestriction,
  )
import Moonlight.Sheaf.Kernel.Basis (basisCells)
import Moonlight.Sheaf.Section.ObjectIndex
  ( mkObjectIndex,
  )
import Moonlight.Sheaf.Section.Restriction
  ( RestrictionIndex,
    RestrictionIndexError (..),
    buildRestrictionIndex,
  )
import Moonlight.Sheaf.Section.Stalk
  ( StalkAlgebra (..),
    StalkRestrictionKernel (..),
  )
import Moonlight.Sheaf.Site.Grothendieck
  ( GrothendieckCell,
    GrothendieckFaceMorphism,
    GrothendieckSite,
    grothendieckFaceMorphismOrientation,
    grothendieckFaceMorphismSource,
    grothendieckFaceMorphismTarget,
    grothendieckSiteCells,
    grothendieckSiteFaceMorphisms,
  )
import Moonlight.Sheaf.Site.Construction.Nerve
  ( FaceMorphism,
    faceMorphismOrientation,
    faceMorphismSource,
    faceMorphismTarget,
    NerveCell,
    NerveMorphism,
    NerveSite,
    NerveSiteAlgebra (..),
    NerveSource,
    nerveSiteBasis,
    nerveSiteCategory,
    siteFaceMorphisms,
  )
import Moonlight.Sheaf.Site.Stalk.Interface
  ( FaceStalkProjectionError,
    InterfaceDomain (..),
    InterfaceMorphism,
    InterfaceObject,
    InterfaceStalk,
    grothendieckStalkFromCell,
    targetStalkForFace,
  )
import Moonlight.Sheaf.Site.System (SystemMor, SystemOb, SystemTag)

data SiteRestrictionWitness face stalk = SiteRestrictionWitness
  { srwFace :: !face,
    srwTargetStalk :: !stalk
  }
  deriving stock (Eq, Show)

data SiteRestrictionBuildError cell projectionError
  = SiteRestrictionIndexBuildError !(RestrictionIndexError cell)
  | SiteRestrictionProjectionBuildError !projectionError
  deriving stock (Eq, Show)

siteRestrictionStalkAlgebra ::
  StalkAlgebra witness stalk mismatch repairObstruction ->
  StalkAlgebra (SiteRestrictionWitness face stalk) stalk mismatch ()
siteRestrictionStalkAlgebra baseAlgebra =
  baseAlgebra
    { saRestrictionKernel = \witness -> StalkRestrictionMap (const (srwTargetStalk witness)),
      saRepair = const (Left ())
    }

buildNerveRestrictions ::
  forall tag.
  ( NerveSiteAlgebra tag,
    InterfaceDomain tag,
    NerveSource tag ~ InterfaceObject tag,
    NerveMorphism tag ~ InterfaceMorphism tag
  ) =>
  NerveSite tag ->
  Either
    (SiteRestrictionBuildError (NerveCell tag) (FaceStalkProjectionError tag))
    (RestrictionIndex (NerveCell tag) (SiteRestrictionWitness (FaceMorphism tag) (InterfaceStalk tag)))
buildNerveRestrictions siteValue = do
  faceWitnesses <- traverse faceWitness (siteFaceMorphisms siteValue)
  first SiteRestrictionIndexBuildError
    ( buildIncidenceRestrictionIndex
        (nerveSiteBasis siteValue)
        faceWitnesses
        (\(kindValue, _) -> kindValue)
        (faceMorphismSource . fst . snd)
        (faceMorphismTarget . fst . snd)
        (uncurry SiteRestrictionWitness . snd)
    )
  where
    faceWitness ::
      FaceMorphism tag ->
      Either
        (SiteRestrictionBuildError (NerveCell tag) (FaceStalkProjectionError tag))
        (RestrictionKind, (FaceMorphism tag, InterfaceStalk tag))
    faceWitness faceValue = do
      restrictionKind <-
        first SiteRestrictionIndexBuildError
          (incidenceRestrictionFor (faceMorphismSource faceValue) (faceMorphismTarget faceValue) (faceMorphismOrientation faceValue))
      targetStalk <-
        first SiteRestrictionProjectionBuildError (targetStalkForFace (nerveSiteCategory siteValue) faceValue)
      pure (restrictionKind, (faceValue, targetStalk))

buildGrothendieckRestrictions ::
  ( InterfaceDomain (SystemTag system),
    SystemOb system ~ InterfaceObject (SystemTag system),
    SystemMor system ~ InterfaceMorphism (SystemTag system)
  ) =>
  GrothendieckSite system ->
  Either
    (RestrictionIndexError (GrothendieckCell system))
    (RestrictionIndex (GrothendieckCell system) (SiteRestrictionWitness (GrothendieckFaceMorphism system) (InterfaceStalk (SystemTag system))))
buildGrothendieckRestrictions =
  buildGrothendieckRestrictionsBy grothendieckTargetStalk

buildGrothendieckRestrictionsWithStalkCache ::
  ( InterfaceDomain (SystemTag system),
    SystemOb system ~ InterfaceObject (SystemTag system),
    SystemMor system ~ InterfaceMorphism (SystemTag system)
  ) =>
  GrothendieckSite system ->
  Map (GrothendieckCell system) (InterfaceStalk (SystemTag system)) ->
  Either
    (RestrictionIndexError (GrothendieckCell system))
    (RestrictionIndex (GrothendieckCell system) (SiteRestrictionWitness (GrothendieckFaceMorphism system) (InterfaceStalk (SystemTag system))))
buildGrothendieckRestrictionsWithStalkCache siteValue stalkCache =
  buildGrothendieckRestrictionsBy
    (cachedGrothendieckTargetStalk stalkCache)
    siteValue

buildGrothendieckRestrictionsBy ::
  (GrothendieckFaceMorphism system -> InterfaceStalk tag) ->
  GrothendieckSite system ->
  Either
    (RestrictionIndexError (GrothendieckCell system))
    (RestrictionIndex (GrothendieckCell system) (SiteRestrictionWitness (GrothendieckFaceMorphism system) (InterfaceStalk tag)))
buildGrothendieckRestrictionsBy targetStalk siteValue = do
  faceWitnesses <- traverse faceWitness (grothendieckSiteFaceMorphisms siteValue)
  buildIncidenceRestrictionIndex
    (mkSheafBasis (grothendieckSiteCells siteValue))
    faceWitnesses
    fst
    (grothendieckFaceMorphismSource . snd)
    (grothendieckFaceMorphismTarget . snd)
    (\(_, face) -> SiteRestrictionWitness face (targetStalk face))
  where
    faceWitness ::
      GrothendieckFaceMorphism system ->
      Either
        (RestrictionIndexError (GrothendieckCell system))
        (RestrictionKind, GrothendieckFaceMorphism system)
    faceWitness faceValue =
      fmap
        (\restrictionKind -> (restrictionKind, faceValue))
        ( incidenceRestrictionFor
            (grothendieckFaceMorphismSource faceValue)
            (grothendieckFaceMorphismTarget faceValue)
            (grothendieckFaceMorphismOrientation faceValue)
        )

buildIncidenceRestrictionIndex ::
  Ord cell =>
  SheafBasis cell ->
  [morphism] ->
  (morphism -> RestrictionKind) ->
  (morphism -> cell) ->
  (morphism -> cell) ->
  (morphism -> SiteRestrictionWitness face stalk) ->
  Either
    (RestrictionIndexError cell)
    (RestrictionIndex cell (SiteRestrictionWitness face stalk))
buildIncidenceRestrictionIndex basis morphisms morphismKind morphismSource morphismTarget morphismWitness =
  buildRestrictionIndex
    (mkObjectIndex (basisCells basis))
    ( \morphism ->
        RestrictionParts
          { partKind = morphismKind morphism,
            partSource = morphismSource morphism,
            partTarget = morphismTarget morphism,
            partWitness = morphismWitness morphism
          }
    )
    morphisms

incidenceRestrictionFor ::
  cell ->
  cell ->
  Int ->
  Either (RestrictionIndexError cell) RestrictionKind
incidenceRestrictionFor sourceCell targetCell coefficient =
  case mkIncidenceRestriction coefficient of
    Just restrictionKind ->
      Right restrictionKind
    Nothing ->
      Left (RestrictionZeroIncidenceCoefficient sourceCell targetCell)

grothendieckTargetStalk ::
  ( InterfaceDomain (SystemTag system),
    SystemOb system ~ InterfaceObject (SystemTag system),
    SystemMor system ~ InterfaceMorphism (SystemTag system)
  ) =>
  GrothendieckFaceMorphism system ->
  InterfaceStalk (SystemTag system)
grothendieckTargetStalk =
  grothendieckStalkFromCell . grothendieckFaceMorphismTarget

cachedGrothendieckTargetStalk ::
  ( InterfaceDomain (SystemTag system),
    SystemOb system ~ InterfaceObject (SystemTag system),
    SystemMor system ~ InterfaceMorphism (SystemTag system)
  ) =>
  Map (GrothendieckCell system) (InterfaceStalk (SystemTag system)) ->
  GrothendieckFaceMorphism system ->
  InterfaceStalk (SystemTag system)
cachedGrothendieckTargetStalk stalkCache faceMorphism =
  Map.findWithDefault
    (grothendieckTargetStalk faceMorphism)
    (grothendieckFaceMorphismTarget faceMorphism)
    stalkCache

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Moonlight.Sheaf.Internal.PublicModel
  ( PreparedSite (..),
    Section (..),
    PartialSection (..),
    PreparedCover (..),
    GlobalSection (..),
    RepairResult (..),
    MatchingFamily (..),
    CompatibleMatchingFamily (..),
    Amalgamation (..),
    CoverStalkUniverse (..),
    SeparatedCover (..),
    UniqueAmalgamation (..),
  )
where

import Data.Kind (Type)
import Moonlight.Sheaf.Section.Certified qualified as Certified
import Moonlight.Sheaf.Section.Model qualified as Model
import Moonlight.Sheaf.Section.Repair qualified as Repair
import Moonlight.Sheaf.Section.Store.Types qualified as Store
import Moonlight.Sheaf.Sheaf.Gluing qualified as Gluing
import Moonlight.Sheaf.Presheaf.Core
  ( CompiledRestriction,
  )
import Moonlight.Sheaf.Site.Class
  ( SiteMorphism,
    SiteObject,
  )
import Moonlight.Sheaf.Site.Plan
  ( CoverPlan,
    SitePlans,
  )

type PreparedSite :: Type -> Type -> Type
data PreparedSite owner site = PreparedSite
  { preparedSiteInternal :: !site,
    preparedSiteModelInternal :: !(Model.SheafModel owner (SiteObject site) (CompiledRestriction site)),
    preparedSitePlansInternal :: !(SitePlans (SiteObject site) (SiteMorphism site))
  }

type role PreparedSite nominal nominal

deriving stock instance
  (Eq site, Eq (SiteObject site), Eq (SiteMorphism site)) =>
  Eq (PreparedSite owner site)

deriving stock instance
  (Show site, Show (SiteObject site), Show (SiteMorphism site)) =>
  Show (PreparedSite owner site)

type Section :: Type -> Type -> Type -> Type
data Section owner site stalk = Section
  { sectionOwnerInternal :: !(PreparedSite owner site),
    sectionStoreInternal :: !(Store.TotalSectionStore owner (SiteObject site) stalk)
  }

type role Section nominal nominal representational

deriving stock instance
  ( Eq site,
    Eq (SiteObject site),
    Eq (SiteMorphism site),
    Eq (Store.TotalSectionStore owner (SiteObject site) stalk)
  ) =>
  Eq (Section owner site stalk)

deriving stock instance
  ( Show site,
    Show (SiteObject site),
    Show (SiteMorphism site),
    Show (Store.TotalSectionStore owner (SiteObject site) stalk)
  ) =>
  Show (Section owner site stalk)

type PartialSection :: Type -> Type -> Type -> Type
data PartialSection owner site stalk = PartialSection
  { partialSectionOwnerInternal :: !(PreparedSite owner site),
    partialSectionStoreInternal :: !(Store.PartialSectionStore owner (SiteObject site) stalk)
  }

type role PartialSection nominal nominal representational

deriving stock instance
  ( Eq site,
    Eq (SiteObject site),
    Eq (SiteMorphism site),
    Eq (Store.PartialSectionStore owner (SiteObject site) stalk)
  ) =>
  Eq (PartialSection owner site stalk)

deriving stock instance
  ( Show site,
    Show (SiteObject site),
    Show (SiteMorphism site),
    Show (Store.PartialSectionStore owner (SiteObject site) stalk)
  ) =>
  Show (PartialSection owner site stalk)

type PreparedCover :: Type -> Type -> Type
data PreparedCover owner site = PreparedCover
  { preparedCoverOwnerInternal :: !(PreparedSite owner site),
    preparedCoverPlanInternal :: !(CoverPlan (SiteObject site) (SiteMorphism site))
  }

type role PreparedCover nominal nominal

deriving stock instance
  (Eq site, Eq (SiteObject site), Eq (SiteMorphism site)) =>
  Eq (PreparedCover owner site)

deriving stock instance
  (Show site, Show (SiteObject site), Show (SiteMorphism site)) =>
  Show (PreparedCover owner site)

type GlobalSection :: Type -> Type -> Type -> Type
data GlobalSection owner site stalk = GlobalSection
  { globalSectionOwnerInternal :: !(PreparedSite owner site),
    globalSectionValueInternal :: !(Certified.GlobalSection owner (SiteObject site) stalk)
  }

type role GlobalSection nominal nominal representational

deriving stock instance
  ( Eq site,
    Eq (SiteObject site),
    Eq (SiteMorphism site),
    Eq (Certified.GlobalSection owner (SiteObject site) stalk)
  ) =>
  Eq (GlobalSection owner site stalk)

deriving stock instance
  ( Show site,
    Show (SiteObject site),
    Show (SiteMorphism site),
    Show (Certified.GlobalSection owner (SiteObject site) stalk)
  ) =>
  Show (GlobalSection owner site stalk)

type RepairResult :: Type -> Type -> Type -> Type -> Type
data RepairResult owner site stalk mismatch = RepairResult
  { repairedAssignment :: !(PartialSection owner site stalk),
    repairDiagnostics :: !(Repair.RepairDiagnostics (SiteObject site) mismatch),
    repairStatus :: !Repair.RepairStatus
  }

deriving stock instance
  ( Eq site,
    Eq (SiteObject site),
    Eq (SiteMorphism site),
    Eq (Store.PartialSectionStore owner (SiteObject site) stalk),
    Eq (Repair.RepairDiagnostics (SiteObject site) mismatch)
  ) =>
  Eq (RepairResult owner site stalk mismatch)

deriving stock instance
  ( Show site,
    Show (SiteObject site),
    Show (SiteMorphism site),
    Show (Store.PartialSectionStore owner (SiteObject site) stalk),
    Show (Repair.RepairDiagnostics (SiteObject site) mismatch)
  ) =>
  Show (RepairResult owner site stalk mismatch)

type MatchingFamily :: Type -> Type -> Type -> Type
data MatchingFamily owner site stalk = MatchingFamily
  { matchingFamilyOwnerInternal :: !(PreparedCover owner site),
    matchingFamilyRawInternal :: !(Gluing.MatchingFamily site stalk)
  }

type CompatibleMatchingFamily :: Type -> Type -> Type -> Type
data CompatibleMatchingFamily owner site stalk = CompatibleMatchingFamily
  { compatibleMatchingFamilyOwnerInternal :: !(PreparedCover owner site),
    compatibleMatchingFamilyRawInternal :: !(Gluing.CompatibleMatchingFamily site stalk)
  }

type Amalgamation :: Type -> Type -> Type -> Type
data Amalgamation owner site stalk = Amalgamation
  { amalgamationOwnerInternal :: !(PreparedCover owner site),
    amalgamationRawInternal :: !(Gluing.Amalgamation site stalk)
  }

type CoverStalkUniverse :: Type -> Type -> Type -> Type
data CoverStalkUniverse owner site stalk = CoverStalkUniverse
  { coverStalkUniverseOwnerInternal :: !(PreparedCover owner site),
    coverStalkUniverseRawInternal :: !(Gluing.CoverStalkUniverse stalk)
  }

type SeparatedCover :: Type -> Type -> Type -> Type
data SeparatedCover owner site stalk = SeparatedCover
  { separatedCoverOwnerInternal :: !(PreparedCover owner site),
    separatedCoverRawInternal :: !(Gluing.SeparatedCover site stalk)
  }

type UniqueAmalgamation :: Type -> Type -> Type -> Type
data UniqueAmalgamation owner site stalk = UniqueAmalgamation
  { uniqueAmalgamationOwnerInternal :: !(PreparedCover owner site),
    uniqueAmalgamationRawInternal :: !(Gluing.UniqueAmalgamation site stalk)
  }

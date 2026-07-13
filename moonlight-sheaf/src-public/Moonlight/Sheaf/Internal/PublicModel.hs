{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
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

type PreparedSite :: Type -> Type
data PreparedSite site = PreparedSite
  { preparedSiteInternal :: !site,
    preparedSiteModelInternal :: !(Model.SheafModel (SiteObject site) (CompiledRestriction site)),
    preparedSitePlansInternal :: !(SitePlans (SiteObject site) (SiteMorphism site))
  }

deriving stock instance
  (Eq site, Eq (SiteObject site), Eq (SiteMorphism site)) =>
  Eq (PreparedSite site)

deriving stock instance
  (Show site, Show (SiteObject site), Show (SiteMorphism site)) =>
  Show (PreparedSite site)

type Section :: Type -> Type -> Type
data Section site stalk = Section
  { sectionOwnerInternal :: !(PreparedSite site),
    sectionStoreInternal :: !(Store.TotalSectionStore (SiteObject site) stalk)
  }

deriving stock instance
  ( Eq site,
    Eq (SiteObject site),
    Eq (SiteMorphism site),
    Eq (Store.TotalSectionStore (SiteObject site) stalk)
  ) =>
  Eq (Section site stalk)

deriving stock instance
  ( Show site,
    Show (SiteObject site),
    Show (SiteMorphism site),
    Show (Store.TotalSectionStore (SiteObject site) stalk)
  ) =>
  Show (Section site stalk)

type PartialSection :: Type -> Type -> Type
data PartialSection site stalk = PartialSection
  { partialSectionOwnerInternal :: !(PreparedSite site),
    partialSectionStoreInternal :: !(Store.PartialSectionStore (SiteObject site) stalk)
  }

deriving stock instance
  ( Eq site,
    Eq (SiteObject site),
    Eq (SiteMorphism site),
    Eq (Store.PartialSectionStore (SiteObject site) stalk)
  ) =>
  Eq (PartialSection site stalk)

deriving stock instance
  ( Show site,
    Show (SiteObject site),
    Show (SiteMorphism site),
    Show (Store.PartialSectionStore (SiteObject site) stalk)
  ) =>
  Show (PartialSection site stalk)

type PreparedCover :: Type -> Type
data PreparedCover site = PreparedCover
  { preparedCoverOwnerInternal :: !(PreparedSite site),
    preparedCoverPlanInternal :: !(CoverPlan (SiteObject site) (SiteMorphism site))
  }

deriving stock instance
  (Eq site, Eq (SiteObject site), Eq (SiteMorphism site)) =>
  Eq (PreparedCover site)

deriving stock instance
  (Show site, Show (SiteObject site), Show (SiteMorphism site)) =>
  Show (PreparedCover site)

type GlobalSection :: Type -> Type -> Type
data GlobalSection site stalk = GlobalSection
  { globalSectionOwnerInternal :: !(PreparedSite site),
    globalSectionValueInternal :: !(Certified.GlobalSection (SiteObject site) stalk)
  }

deriving stock instance
  ( Eq site,
    Eq (SiteObject site),
    Eq (SiteMorphism site),
    Eq (Certified.GlobalSection (SiteObject site) stalk)
  ) =>
  Eq (GlobalSection site stalk)

deriving stock instance
  ( Show site,
    Show (SiteObject site),
    Show (SiteMorphism site),
    Show (Certified.GlobalSection (SiteObject site) stalk)
  ) =>
  Show (GlobalSection site stalk)

type RepairResult :: Type -> Type -> Type -> Type
data RepairResult site stalk mismatch = RepairResult
  { repairedAssignment :: !(PartialSection site stalk),
    repairDiagnostics :: !(Repair.RepairDiagnostics (SiteObject site) mismatch),
    repairStatus :: !Repair.RepairStatus
  }

deriving stock instance
  ( Eq site,
    Eq (SiteObject site),
    Eq (SiteMorphism site),
    Eq (Store.PartialSectionStore (SiteObject site) stalk),
    Eq (Repair.RepairDiagnostics (SiteObject site) mismatch)
  ) =>
  Eq (RepairResult site stalk mismatch)

deriving stock instance
  ( Show site,
    Show (SiteObject site),
    Show (SiteMorphism site),
    Show (Store.PartialSectionStore (SiteObject site) stalk),
    Show (Repair.RepairDiagnostics (SiteObject site) mismatch)
  ) =>
  Show (RepairResult site stalk mismatch)

type MatchingFamily :: Type -> Type -> Type
data MatchingFamily site stalk = MatchingFamily
  { matchingFamilyOwnerInternal :: !(PreparedCover site),
    matchingFamilyRawInternal :: !(Gluing.MatchingFamily site stalk)
  }

type CompatibleMatchingFamily :: Type -> Type -> Type
data CompatibleMatchingFamily site stalk = CompatibleMatchingFamily
  { compatibleMatchingFamilyOwnerInternal :: !(PreparedCover site),
    compatibleMatchingFamilyRawInternal :: !(Gluing.CompatibleMatchingFamily site stalk)
  }

type Amalgamation :: Type -> Type -> Type
data Amalgamation site stalk = Amalgamation
  { amalgamationOwnerInternal :: !(PreparedCover site),
    amalgamationRawInternal :: !(Gluing.Amalgamation site stalk)
  }

type CoverStalkUniverse :: Type -> Type -> Type
data CoverStalkUniverse site stalk = CoverStalkUniverse
  { coverStalkUniverseOwnerInternal :: !(PreparedCover site),
    coverStalkUniverseRawInternal :: !(Gluing.CoverStalkUniverse stalk)
  }

type SeparatedCover :: Type -> Type -> Type
data SeparatedCover site stalk = SeparatedCover
  { separatedCoverOwnerInternal :: !(PreparedCover site),
    separatedCoverRawInternal :: !(Gluing.SeparatedCover site stalk)
  }

type UniqueAmalgamation :: Type -> Type -> Type
data UniqueAmalgamation site stalk = UniqueAmalgamation
  { uniqueAmalgamationOwnerInternal :: !(PreparedCover site),
    uniqueAmalgamationRawInternal :: !(Gluing.UniqueAmalgamation site stalk)
  }

module Moonlight.Cosheaf.Test.Support
  ( prepareFullFiniteCosheafChain,
    fullFiniteCosheafColimit,
    compileFullTropicalCostTable,
  )
where

import Data.Bifunctor (first)
import Moonlight.Cosheaf
  ( CosheafColimit,
    CosheafColimitFailure (..),
    CosheafSupportFailure,
    FiniteCosheaf,
    PreparedFiniteCosheafChain,
    TropicalCostModel,
    TropicalCostTable,
    TropicalCosectionFailure (..),
    ccCosheaf,
    compileTropicalCostTableFromSupportPlan,
    finiteCosheafColimitFromSupportPlan,
    fullFiniteCosheafChainSupportPlan,
    prepareFiniteCosheafChainFromSupportPlan,
  )
import Moonlight.Sheaf.Site.Class
  ( Site (..),
  )
import Numeric.Natural (Natural)

prepareFullFiniteCosheafChain ::
  (Site site, Ord (SiteMorphism site)) =>
  Natural ->
  FiniteCosheaf site value ->
  Either
    (CosheafSupportFailure (SiteObject site) (SiteMorphism site) value)
    (PreparedFiniteCosheafChain site value)
prepareFullFiniteCosheafChain maxDegreeValue cosheaf = do
  supportPlan <- fullFiniteCosheafChainSupportPlan maxDegreeValue cosheaf
  prepareFiniteCosheafChainFromSupportPlan supportPlan cosheaf

fullFiniteCosheafColimit ::
  (Site site, Ord value) =>
  FiniteCosheaf site value ->
  Either
    (CosheafColimitFailure (SiteObject site) (SiteMorphism site) value)
    (CosheafColimit site value)
fullFiniteCosheafColimit cosheaf = do
  supportPlan <- first CosheafColimitSupportInvalid (fullFiniteCosheafChainSupportPlan 1 cosheaf)
  finiteCosheafColimitFromSupportPlan supportPlan cosheaf

compileFullTropicalCostTable ::
  (Site site, Ord value) =>
  CosheafColimit site value ->
  TropicalCostModel site value ->
  Either
    (TropicalCosectionFailure (SiteObject site) (SiteMorphism site) value)
    (TropicalCostTable site value)
compileFullTropicalCostTable colimit costModel = do
  supportPlan <- first TropicalSupportInvalid (fullFiniteCosheafChainSupportPlan 1 (ccCosheaf colimit))
  compileTropicalCostTableFromSupportPlan supportPlan colimit costModel

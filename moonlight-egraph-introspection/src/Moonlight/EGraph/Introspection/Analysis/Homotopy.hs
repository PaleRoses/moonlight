module Moonlight.EGraph.Introspection.Analysis.Homotopy
  ( NerveHomotopyProfile (..),
    bettiProfileOfNerve,
    grothendieckHomotopyProfile,
    nerveHomotopyProfile,
  )
where

import Data.Map.Strict qualified as Map
import Moonlight.Analysis.Homotopy
  ( CellularHomotopyModel (..),
    NerveHomotopyProfile (..),
    bettiProfileOfSite,
    boundaryFromOrientationMap,
    homotopyProfileOfSite,
  )
import Moonlight.Core (ZipMatch (..), HasConstructorTag, Pattern)
import Moonlight.Sheaf.Site
  ( grothendieckCategory,
    mkGrothendieckSite,
  )
import Moonlight.Sheaf.Site (grothendieckChainComplexFromSite)
import Moonlight.EGraph.Introspection.Core.Rewrite (RewriteSystem, RewriteTag, rsCategory)
import Moonlight.Sheaf.Site
  ( CellKey (..),
    NerveCell,
    NerveSite,
    faceMorphismOrientation,
    faceMorphismSource,
    faceMorphismTarget,
    nerveCellKey,
    nerveSiteBasis,
    nerveSiteDepth,
    siteFaceMorphisms,
  )
import Moonlight.Sheaf.Site (ContextPresentationSystem)
import Moonlight.Sheaf.Site (LatticeAnalyzableSystem)
import Moonlight.Homology
  ( HomologyFailure,
    freeBettiVector,
  )
import Moonlight.Sheaf.Kernel.Basis (basisCells)
import Moonlight.Category.Simplicial (pi0Nerve)
import Numeric.Natural (Natural)

nerveHomotopyProfile :: (HasConstructorTag f, ZipMatch f, Ord (Pattern f)) => RewriteSystem f -> NerveSite (RewriteTag f) -> Either HomologyFailure NerveHomotopyProfile
nerveHomotopyProfile rewriteSystem siteValue =
  homotopyProfileOfSite
    (length (pi0Nerve (rsCategory rewriteSystem)))
    nerveCellularModel
    siteValue

bettiProfileOfNerve :: NerveSite f -> Either HomologyFailure [Int]
bettiProfileOfNerve =
  bettiProfileOfSite nerveCellularModel

grothendieckHomotopyProfile :: (ContextPresentationSystem system, LatticeAnalyzableSystem system) => system -> Natural -> Either HomologyFailure NerveHomotopyProfile
grothendieckHomotopyProfile systemValue depthValue =
  let siteValue = mkGrothendieckSite systemValue depthValue
   in fmap
        (\bettiVectorValue ->
            NerveHomotopyProfile
              { nhpConnectedComponents = length (pi0Nerve (grothendieckCategory systemValue)),
                nhpBettiVector = bettiVectorValue
              }
        )
        (freeBettiVector <$> grothendieckChainComplexFromSite siteValue)

nerveCellularModel :: CellularHomotopyModel (NerveSite f) (NerveCell f)
nerveCellularModel =
  CellularHomotopyModel
    { chmMaxDimension = fromIntegral . nerveSiteDepth,
      chmCellsAtDimension = \dimensionValue siteValue ->
        filter ((== dimensionValue) . nerveCellDimensionInt) (basisCells (nerveSiteBasis siteValue)),
      chmBoundaryOf = \dimensionValue siteValue cellValue ->
        boundaryFromOrientationMap (nerveOrientationMapForDimension dimensionValue siteValue) cellValue
    }

nerveOrientationMapForDimension ::
  Int ->
  NerveSite f ->
  Map.Map (NerveCell f, NerveCell f) Int
nerveOrientationMapForDimension dimensionValue =
  Map.filter (/= 0)
    . Map.fromListWith (+)
    . fmap (\faceMorphism -> ((faceMorphismSource faceMorphism, faceMorphismTarget faceMorphism), faceMorphismOrientation faceMorphism))
    . filter ((== dimensionValue) . nerveCellDimensionInt . faceMorphismSource)
    . siteFaceMorphisms

nerveCellDimensionInt :: NerveCell tag -> Int
nerveCellDimensionInt =
  fromIntegral . ckDimension . nerveCellKey

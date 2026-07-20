{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- | Finite site-law validation; pullback universality is not checked.
module Moonlight.Sheaf.Site.Class.Validation
  ( siteLawFailures,
    checkCompositionClosureLaw,
    checkLeftIdentityLaw,
    checkRightIdentityLaw,
    checkAssociativityLaw,
    checkPullbackSquareCommutativityLaw,
    checkIdentityCoverLaw,
    checkPullbackStabilityLaw,
    checkTransitivityLaw,
    allCompositionClosureFailures,
    allLeftIdentityFailures,
    allRightIdentityFailures,
    allAssociativityFailures,
    allPullbackSquareCommutativityFailures,
    allIdentityCoverFailures,
    allPullbackStabilityFailures,
    allTransitivityFailures,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, mapMaybe)
import Data.Set qualified as Set
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism (..),
    CoveringFamily,
    PullbackSquare (..),
    Site (..),
    SiteLawFailure (..),
    coverSources,
    coverTarget,
    identityCover,
    pullbackCoverAlong,
    siteMorphismUniverse,
    transitiveCover,
  )

siteLawFailures ::
  (Site site, Ord (SiteMorphism site)) =>
  site ->
  [SiteLawFailure (SiteObject site) (SiteMorphism site)]
siteLawFailures site =
  allCompositionClosureFailures site
    <> allLeftIdentityFailures site
    <> allRightIdentityFailures site
    <> allAssociativityFailures site
    <> allPullbackSquareCommutativityFailures site
    <> allIdentityCoverFailures site
    <> allPullbackStabilityFailures site
    <> allTransitivityFailures site

allCompositionClosureFailures ::
  (Site site, Eq (SiteMorphism site)) =>
  site ->
  [SiteLawFailure (SiteObject site) (SiteMorphism site)]
allCompositionClosureFailures site =
  catMaybes
    [ checkCompositionClosureLaw site outerMorphism innerMorphism
    | innerMorphism <- siteMorphisms site,
      outerMorphism <- siteMorphisms site,
      cmTarget innerMorphism == cmSource outerMorphism
    ]

allLeftIdentityFailures ::
  (Site site, Ord (SiteMorphism site)) =>
  site ->
  [SiteLawFailure (SiteObject site) (SiteMorphism site)]
allLeftIdentityFailures site =
  mapMaybe (checkLeftIdentityLaw site) (siteMorphismUniverse site)

allRightIdentityFailures ::
  (Site site, Ord (SiteMorphism site)) =>
  site ->
  [SiteLawFailure (SiteObject site) (SiteMorphism site)]
allRightIdentityFailures site =
  mapMaybe (checkRightIdentityLaw site) (siteMorphismUniverse site)

allAssociativityFailures ::
  (Site site, Ord (SiteMorphism site)) =>
  site ->
  [SiteLawFailure (SiteObject site) (SiteMorphism site)]
allAssociativityFailures site =
  let morphisms = siteMorphismUniverse site
   in catMaybes
        [ checkAssociativityLaw site outerMorphism middleMorphism innerMorphism
        | innerMorphism <- morphisms,
          middleMorphism <- morphisms,
          cmTarget innerMorphism == cmSource middleMorphism,
          outerMorphism <- morphisms,
          cmTarget middleMorphism == cmSource outerMorphism
        ]

allPullbackSquareCommutativityFailures ::
  (Site site, Ord (SiteMorphism site)) =>
  site ->
  [SiteLawFailure (SiteObject site) (SiteMorphism site)]
allPullbackSquareCommutativityFailures site =
  let morphisms = siteMorphismUniverse site
   in mapMaybe
        (checkPullbackSquareCommutativityLaw site)
        [ square
        | leftMorphism <- morphisms,
          rightMorphism <- morphisms,
          cmTarget leftMorphism == cmTarget rightMorphism,
          Just square <- [pullbackPair site leftMorphism rightMorphism]
        ]

allIdentityCoverFailures :: Site site => site -> [SiteLawFailure (SiteObject site) (SiteMorphism site)]
allIdentityCoverFailures site =
  mapMaybe (checkIdentityCoverLaw site) (siteObjects site)

allPullbackStabilityFailures :: Site site => site -> [SiteLawFailure (SiteObject site) (SiteMorphism site)]
allPullbackStabilityFailures site =
  catMaybes
    [ checkPullbackStabilityLaw site morphismValue coverValue
    | morphismValue <- siteMorphisms site,
      coverValue <- coverChoicesFor site (cmTarget morphismValue)
    ]

allTransitivityFailures :: Site site => site -> [SiteLawFailure (SiteObject site) (SiteMorphism site)]
allTransitivityFailures site =
  catMaybes
    [ checkTransitivityLaw site outerCover innerCovers
    | targetObject <- siteObjects site,
      outerCover <- coverChoicesFor site targetObject,
      innerCovers <- coverChoiceMaps site (coverSources outerCover)
    ]

coverChoicesFor :: Site site => site -> SiteObject site -> [CoveringFamily (SiteObject site) (SiteMorphism site)]
coverChoicesFor site objectValue =
  identityCover site objectValue : coversAt site objectValue

coverChoiceMaps :: Site site => site -> [SiteObject site] -> [Map (SiteObject site) (CoveringFamily (SiteObject site) (SiteMorphism site))]
coverChoiceMaps site =
  fmap Map.fromList
    . traverse (\sourceObject -> fmap (sourceObject,) (coverChoicesFor site sourceObject))
    . Set.toAscList
    . Set.fromList

checkCompositionClosureLaw ::
  (Site site, Eq (SiteMorphism site)) =>
  site ->
  CheckedMorphism (SiteObject site) (SiteMorphism site) ->
  CheckedMorphism (SiteObject site) (SiteMorphism site) ->
  Maybe (SiteLawFailure (SiteObject site) (SiteMorphism site))
checkCompositionClosureLaw site outerMorphism innerMorphism =
  case composeChecked site outerMorphism innerMorphism of
    Nothing ->
      Just (CompositionUnavailable outerMorphism innerMorphism)
    Just compositeMorphism
      | compositeMorphism `elem` siteMorphisms site ->
          Nothing
      | otherwise ->
          Just
            ( CompositeOutsideSiteMorphisms
                outerMorphism
                innerMorphism
                compositeMorphism
            )

checkLeftIdentityLaw ::
  (Site site, Eq (SiteMorphism site)) =>
  site ->
  CheckedMorphism (SiteObject site) (SiteMorphism site) ->
  Maybe (SiteLawFailure (SiteObject site) (SiteMorphism site))
checkLeftIdentityLaw site morphismValue =
  let actualComposite =
        composeChecked site (identityAt site (cmTarget morphismValue)) morphismValue
   in if actualComposite == Just morphismValue
        then Nothing
        else Just (LeftIdentityLawFailed morphismValue actualComposite)

checkRightIdentityLaw ::
  (Site site, Eq (SiteMorphism site)) =>
  site ->
  CheckedMorphism (SiteObject site) (SiteMorphism site) ->
  Maybe (SiteLawFailure (SiteObject site) (SiteMorphism site))
checkRightIdentityLaw site morphismValue =
  let actualComposite =
        composeChecked site morphismValue (identityAt site (cmSource morphismValue))
   in if actualComposite == Just morphismValue
        then Nothing
        else Just (RightIdentityLawFailed morphismValue actualComposite)

checkAssociativityLaw ::
  (Site site, Eq (SiteMorphism site)) =>
  site ->
  CheckedMorphism (SiteObject site) (SiteMorphism site) ->
  CheckedMorphism (SiteObject site) (SiteMorphism site) ->
  CheckedMorphism (SiteObject site) (SiteMorphism site) ->
  Maybe (SiteLawFailure (SiteObject site) (SiteMorphism site))
checkAssociativityLaw site outerMorphism middleMorphism innerMorphism =
  let leftAssociated =
        composeChecked site middleMorphism innerMorphism
          >>= composeChecked site outerMorphism
      rightAssociated =
        composeChecked site outerMorphism middleMorphism
          >>= \outerMiddle -> composeChecked site outerMiddle innerMorphism
   in if leftAssociated == rightAssociated
        then Nothing
        else
          Just
            ( AssociativityLawFailed
                outerMorphism
                middleMorphism
                innerMorphism
                leftAssociated
                rightAssociated
            )

checkPullbackSquareCommutativityLaw ::
  (Site site, Eq (SiteMorphism site)) =>
  site ->
  PullbackSquare (SiteObject site) (SiteMorphism site) ->
  Maybe (SiteLawFailure (SiteObject site) (SiteMorphism site))
checkPullbackSquareCommutativityLaw site square =
  let leftComposite =
        composeChecked site (psLeftBase square) (psToLeft square)
      rightComposite =
        composeChecked site (psRightBase square) (psToRight square)
   in case (leftComposite, rightComposite) of
        (Just leftMorphism, Just rightMorphism)
          | leftMorphism == rightMorphism ->
              Nothing
        _ ->
          Just
            ( PullbackSquareDoesNotCommute
                square
                leftComposite
                rightComposite
            )

checkIdentityCoverLaw ::
  Site site =>
  site ->
  SiteObject site ->
  Maybe (SiteLawFailure (SiteObject site) (SiteMorphism site))
checkIdentityCoverLaw site objectValue =
  let identityArrow = identityAt site objectValue
   in if cmSource identityArrow == objectValue
        && cmTarget identityArrow == objectValue
        then Nothing
        else Just (IdentityCoverMalformed objectValue)

checkPullbackStabilityLaw ::
  Site site =>
  site ->
  CheckedMorphism (SiteObject site) (SiteMorphism site) ->
  CoveringFamily (SiteObject site) (SiteMorphism site) ->
  Maybe (SiteLawFailure (SiteObject site) (SiteMorphism site))
checkPullbackStabilityLaw site alongMorphism coverValue =
  checkCoverConstruction
    (pullbackCoverAlong site alongMorphism coverValue)
    (PullbackConstructionFailed alongMorphism coverValue)
    (cmSource alongMorphism)
    coverTarget
    (PullbackCoverWrongTarget alongMorphism coverValue)

checkTransitivityLaw ::
  Site site =>
  site ->
  CoveringFamily (SiteObject site) (SiteMorphism site) ->
  Map
    (SiteObject site)
    (CoveringFamily (SiteObject site) (SiteMorphism site)) ->
  Maybe (SiteLawFailure (SiteObject site) (SiteMorphism site))
checkTransitivityLaw site outerCover innerCovers =
  checkCoverConstruction
    (transitiveCover site outerCover innerCovers)
    (TransitivityConstructionFailed outerCover)
    (coverTarget outerCover)
    coverTarget
    (TransitiveCoverWrongTarget outerCover)

checkCoverConstruction ::
  Eq target =>
  Either constructionError cover ->
  (constructionError -> failure) ->
  target ->
  (cover -> target) ->
  (target -> failure) ->
  Maybe failure
checkCoverConstruction construction mkConstructionFailed expectedTarget targetOf mkWrongTarget =
  case construction of
    Left constructionError ->
      Just (mkConstructionFailed constructionError)
    Right coverValue ->
      let actualTarget = targetOf coverValue
       in if actualTarget == expectedTarget
            then Nothing
            else Just (mkWrongTarget actualTarget)

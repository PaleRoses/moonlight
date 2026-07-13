module Moonlight.Optics.Effect.Laws
  ( lensGetPutLaw,
    lensPutGetLaw,
    lensPutPutLaw,
    prismPreviewReviewLaw,
    prismReviewPreviewLaw,
    restrictionFunctorialLaw,
    restrictionCompatibilityLaw,
    traversalIdentityLaw,
    traversalCompositionLaw,
  )
where

import Optics.Core
import Moonlight.Optics.Pure.Restriction (Restriction)
import qualified Moonlight.Optics.Pure.Restriction as Restriction

lensGetPutLaw :: Eq source => Lens' source focus -> source -> Bool
lensGetPutLaw optic source =
  set optic (view optic source) source == source

lensPutGetLaw :: Eq focus => Lens' source focus -> focus -> source -> Bool
lensPutGetLaw optic focus source =
  view optic (set optic focus source) == focus

lensPutPutLaw :: Eq source => Lens' source focus -> focus -> focus -> source -> Bool
lensPutPutLaw optic first second source =
  set optic second (set optic first source) == set optic second source

prismPreviewReviewLaw :: Eq focus => Prism' source focus -> focus -> Bool
prismPreviewReviewLaw optic focus =
  preview optic (review optic focus) == Just focus

prismReviewPreviewLaw :: Eq source => Prism' source focus -> source -> Bool
prismReviewPreviewLaw optic source =
  maybe source (review optic) (preview optic source) == source

restrictionFunctorialLaw ::
  Eq descendant =>
  Restriction source intermediate edgeKey ->
  Restriction intermediate descendant vertexKey ->
  Restriction source descendant composedKey ->
  (edgeKey -> vertexKey -> composedKey) ->
  edgeKey ->
  vertexKey ->
  source ->
  Bool
restrictionFunctorialLaw first second combined incidence edgeKey vertexKey source =
  Restriction.restrictionFunctorialLaw first second combined incidence edgeKey vertexKey source

restrictionCompatibilityLaw ::
  Eq child =>
  Restriction parent child key ->
  (parent -> child) ->
  key ->
  parent ->
  Bool
restrictionCompatibilityLaw restrictionAt direct key parent =
  Restriction.restrictionCompatibilityLaw restrictionAt direct key parent

traversalIdentityLaw :: Eq source => Traversal' source focus -> source -> Bool
traversalIdentityLaw optic source =
  over optic id source == source

traversalCompositionLaw :: Eq source => Traversal' source focus -> (focus -> focus) -> (focus -> focus) -> source -> Bool
traversalCompositionLaw optic first second source =
  over optic first (over optic second source) == over optic (first . second) source

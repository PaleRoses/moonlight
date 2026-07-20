module Moonlight.Optics.Pure.Restriction
  ( Restriction (..),
    restrict,
    composeRestriction,
    restrictionFunctorialLaw,
    restrictionCompatibilityLaw,
  )
where

import Data.Kind (Type)
import Optics.Core

type Restriction :: Type -> Type -> Type -> Type
newtype Restriction parent child key = Restriction
  { runRestriction :: key -> parent -> child
  }

restrict :: key -> Restriction parent child key -> Getter parent child
restrict key (Restriction restrictionAt) =
  to (restrictionAt key)

composeRestriction :: Getter parent child -> Getter child descendant -> Getter parent descendant
composeRestriction = (%)

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
  view (restrict edgeKey first % restrict vertexKey second) source
    == view (restrict (incidence edgeKey vertexKey) combined) source

restrictionCompatibilityLaw ::
  Eq child =>
  Restriction parent child key ->
  (parent -> child) ->
  key ->
  parent ->
  Bool
restrictionCompatibilityLaw restrictionAt direct key parent =
  view (restrict key restrictionAt) parent == direct parent

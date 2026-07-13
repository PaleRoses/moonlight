{-# LANGUAGE TypeFamilies #-}

module SpanModelLaws
  ( SpanOverlapLawFailure (..),
    checkOverlapProjectionLaw,
    checkComposedInterfaceLegLaw,
  )
where

import Data.Kind (Type)
import Data.Proxy (Proxy)
import Data.Set (Set)
import Moonlight.Rewrite.Algebra
  ( ComposedInterface (..),
    SpanModel (..),
    SpanOverlap (..),
  )

type SpanOverlapLawFailure :: Type -> Type
data SpanOverlapLawFailure model
  = LeftProjectionDoesNotReachApex
      !(SpanObject model)
      !(SpanObject model)
  | RightProjectionDoesNotReachApex
      !(SpanObject model)
      !(SpanObject model)
  | ComposedLeftLegInvalid
      !(SpanModelError model)
  | ComposedRightLegInvalid
      !(SpanModelError model)

deriving stock instance
  ( Eq (SpanObject model),
    Eq (SpanModelError model)
  ) =>
  Eq (SpanOverlapLawFailure model)

deriving stock instance
  ( Show (SpanObject model),
    Show (SpanModelError model)
  ) =>
  Show (SpanOverlapLawFailure model)

checkOverlapProjectionLaw ::
  (SpanOverlap model, Eq (SpanObject model)) =>
  Proxy model ->
  Set (SpanRef model) ->
  SpanObject model ->
  SpanObject model ->
  Either (SpanOverlapError model) [SpanOverlapLawFailure model]
checkOverlapProjectionLaw model forbiddenRefs leftObject rightObject = do
  witness <-
    spanOverlapFreshFrom model forbiddenRefs leftObject rightObject

  let apex =
        spanOverlapApex model witness

      leftImage =
        spanProjectObject
          model
          (spanOverlapLeftProjection model witness)
          leftObject

      rightImage =
        spanProjectObject
          model
          (spanOverlapRightProjection model witness)
          rightObject

      leftFailure =
        [ LeftProjectionDoesNotReachApex leftImage apex
          | leftImage /= apex
        ]

      rightFailure =
        [ RightProjectionDoesNotReachApex rightImage apex
          | rightImage /= apex
        ]

  pure (leftFailure <> rightFailure)

checkComposedInterfaceLegLaw ::
  SpanModel model =>
  Proxy model ->
  SpanObject model ->
  SpanObject model ->
  ComposedInterface model ->
  [SpanOverlapLawFailure model]
checkComposedInterfaceLegLaw model leftObject rightObject composedInterface =
  leftFailure <> rightFailure
  where
    leftFailure =
      case
        spanValidateLeg
          model
          (ciInterface composedInterface)
          (ciLeftLeg composedInterface)
          leftObject
      of
        Right () ->
          []
        Left err ->
          [ComposedLeftLegInvalid err]

    rightFailure =
      case
        spanValidateLeg
          model
          (ciInterface composedInterface)
          (ciRightLeg composedInterface)
          rightObject
      of
        Right () ->
          []
        Left err ->
          [ComposedRightLegInvalid err]

module Moonlight.Sheaf.Twist.Config
  ( TwistConfig (..),
    genericJoin,
    guided,
    withProof,
    withBinderScope,
    twistConfig,
    twistConfigUsing,
  )
where

import Data.Kind (Type)

type TwistConfig :: Type -> Type -> Type -> Type
data TwistConfig saturation guidance proof = TwistConfig
  { tcSaturation :: !saturation,
    tcGuidance :: !(Maybe guidance),
    tcProofAnnotationBuilder :: !proof
  }

guided ::
  guidance ->
  TwistConfig saturation guidance proof ->
  TwistConfig saturation guidance proof
guided guidanceValue twistConfigValue =
  twistConfigValue {tcGuidance = Just guidanceValue}

withProof ::
  proof ->
  TwistConfig saturation guidance oldProof ->
  TwistConfig saturation guidance proof
withProof proofValue twistConfigValue =
  TwistConfig
    { tcSaturation = tcSaturation twistConfigValue,
      tcGuidance = tcGuidance twistConfigValue,
      tcProofAnnotationBuilder = proofValue
    }

withBinderScope ::
  (binder -> saturation -> saturation) ->
  binder ->
  TwistConfig saturation guidance proof ->
  TwistConfig saturation guidance proof
withBinderScope applyBinderScope binderScope twistConfigValue =
  twistConfigValue
    { tcSaturation = applyBinderScope binderScope (tcSaturation twistConfigValue)
    }

twistConfig :: proof -> saturation -> TwistConfig saturation guidance proof
twistConfig =
  twistConfigUsing Nothing

twistConfigUsing ::
  Maybe guidance ->
  proof ->
  saturation ->
  TwistConfig saturation guidance proof
twistConfigUsing maybeGuidance proofValue saturationValue =
  TwistConfig
    { tcSaturation = saturationValue,
      tcGuidance = maybeGuidance,
      tcProofAnnotationBuilder = proofValue
    }

genericJoin ::
  proof ->
  (budget -> saturation) ->
  budget ->
  TwistConfig saturation guidance proof
genericJoin proofValue buildSaturation =
  twistConfig proofValue . buildSaturation

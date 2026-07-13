module Moonlight.Sheaf.Twist.Execute
  ( SupportExecution (..),
    ContextExecution (..),
    ContextProofExecution (..),
  )
where

import Data.Kind (Type)
import Moonlight.Sheaf.Twist.SupportedRuleSpec
  ( SupportedFactBook,
    SupportedRuleBook,
  )

type SupportExecution :: Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type
data SupportExecution context rawFact compiledSupport saturation guidance proof termination proofGraph supportResult err =
  SupportExecution
    { seEffectiveFactBook ::
        saturation ->
        proofGraph ->
        SupportedFactBook context rawFact ->
        SupportedFactBook context rawFact,
      seRunSupport ::
        compiledSupport ->
        proof ->
        Maybe guidance ->
        saturation ->
        termination ->
        proofGraph ->
        Either err supportResult
    }

type ContextExecution :: Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type
data ContextExecution ctx rawRule saturation guidance contextGraph contextResult err =
  ContextExecution
    { ceRunContext ::
        Maybe guidance ->
        saturation ->
        ctx ->
        SupportedRuleBook ctx rawRule ->
        contextGraph ->
        Either err contextResult
    }

type ContextProofExecution :: Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type
data ContextProofExecution ctx rawRule saturation guidance proof proofGraph proofResult err =
  ContextProofExecution
    { cpeRunContextProof ::
        proof ->
        Maybe guidance ->
        saturation ->
        ctx ->
        SupportedRuleBook ctx rawRule ->
        proofGraph ->
        Either err proofResult
    }

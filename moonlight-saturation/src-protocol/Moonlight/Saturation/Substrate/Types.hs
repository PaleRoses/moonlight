{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Saturation.Substrate.Types
  ( TrivialContext,
    SatGraph,
    SatBaseGraph,
    SatClassId,
    SatContext,
    SatObstruction,
    SatCapabilityResolver,
    SatFactStore,
    SatFactIndex,
    SatFactSource,
    SatFactRule,
    SatFactCompileError,
    SatFactRound,
    SatQuery,
    SatMatchSnapshot,
    SatMatchSection,
    SatMatchingDelta,
    SatChangeSummary,
    SatRuleSource,
    SatRule,
    SatRuleKey,
    SatRewriteContext,
    SatRuleCompileError,
    SatRawMatch,
    SatRawMatchRejection,
    SatRequestMatch,
    SatMatchWorld,
    SatMatchingRequest,
    SatMatch,
    SatSupportedMatch,
    SatSupportWitness,
    SatMatchState,
    SatMatchStrategy,
    SatRebuild,
    SatApplicationError,
    SatApplicationResult,
    SatProofGraph,
    SatProofBuilder,
  )
where

import Data.Kind (Type)

type TrivialContext :: Type
type TrivialContext = ()

type SatGraph :: Type -> Type
type family SatGraph u

type SatBaseGraph :: Type -> Type
type family SatBaseGraph u

type SatClassId :: Type -> Type
type family SatClassId u

type SatContext :: Type -> Type
type family SatContext u

type SatObstruction :: Type -> Type
type family SatObstruction u

type SatCapabilityResolver :: Type -> Type
type family SatCapabilityResolver u

type SatFactStore :: Type -> Type
type family SatFactStore u

type SatFactIndex :: Type -> Type
type family SatFactIndex u

type SatFactSource :: Type -> Type
type family SatFactSource u

type SatFactRule :: Type -> Type
type family SatFactRule u

type SatFactCompileError :: Type -> Type
type family SatFactCompileError u

type SatFactRound :: Type -> Type
type family SatFactRound u

type SatQuery :: Type -> Type
type family SatQuery u

type SatMatchSnapshot :: Type -> Type
type family SatMatchSnapshot u

type SatMatchSection :: Type -> Type
type family SatMatchSection u

type SatMatchingDelta :: Type -> Type
type family SatMatchingDelta u

type SatChangeSummary :: Type -> Type
type family SatChangeSummary u

type SatRuleSource :: Type -> Type
type family SatRuleSource u

type SatRule :: Type -> Type
type family SatRule u

type SatRuleKey :: Type -> Type
type family SatRuleKey u

type SatRewriteContext :: Type -> Type
type family SatRewriteContext u

type SatRuleCompileError :: Type -> Type
type family SatRuleCompileError u

type SatRawMatch :: Type -> Type
type family SatRawMatch u

type SatRawMatchRejection :: Type -> Type
type family SatRawMatchRejection u

type SatRequestMatch :: Type -> Type
type family SatRequestMatch u

type SatMatchWorld :: Type -> Type
type family SatMatchWorld u

type SatMatchingRequest :: Type -> Type
type family SatMatchingRequest u

type SatMatch :: Type -> Type
type family SatMatch u

type SatSupportedMatch :: Type -> Type
type family SatSupportedMatch u

type SatSupportWitness :: Type -> Type
type family SatSupportWitness u

type SatMatchState :: Type -> Type
type family SatMatchState u

type SatMatchStrategy :: Type -> Type
type family SatMatchStrategy u

type SatRebuild :: Type -> Type
type family SatRebuild u

type SatApplicationError :: Type -> Type
type family SatApplicationError u

type SatApplicationResult :: Type -> Type
type family SatApplicationResult u

type SatProofGraph :: Type -> Type -> Type
type family SatProofGraph u p

type SatProofBuilder :: Type -> Type -> Type
type family SatProofBuilder u p

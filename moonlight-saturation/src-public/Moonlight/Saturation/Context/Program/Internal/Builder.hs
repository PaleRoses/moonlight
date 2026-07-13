{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Saturation.Context.Program.Internal.Builder
  ( ProgramBuildError,
    ProgramDecl (..),
    ProgramFragment (..),
    emptyProgramFragment,
    singletonProgramDecl,
    appendProgramFragments,
    finishProgram,
  )
where

import Data.Foldable qualified as Foldable
import Data.Kind (Type)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Core (RewriteRuleId)
import Moonlight.Core
  ( MatchActivationIndex (..),
    SiteIndex (..),
    SiteProgram (..),
  )
import Moonlight.Saturation.Context.Error
  ( ProgramRelation (..),
    ProgramViolation (..),
    RuleKind (..),
    SaturationProgramSite (..),
    SaturationSupportError (..),
  )
import Moonlight.Saturation.Context.Program.Internal.Validate
  ( validateSourceProgram,
  )
import Moonlight.Saturation.Context.Program.Plan
  ( Program,
    ProgramStage (SourceProgramStage),
  )
import Moonlight.Saturation.Matching
  ( MatchSite (..),
  )
import Moonlight.Saturation.Substrate

type ProgramBuildError :: Type -> Type
type ProgramBuildError u =
  SaturationSupportError u

type ProgramDecl :: Type -> Type
data ProgramDecl u
  = DeclareRewrite !(MatchSite (SatContext u)) !(SatRuleSource u)
  | DeclareFact !(MatchSite (SatContext u)) !(SatFactSource u)
  | ActivateBaseRewrite !(MatchSite (SatContext u)) !RewriteRuleId
  | DeclareBaseRewriteSupport !RewriteRuleId !(SupportBasis (SatContext u))

type ProgramFragment :: Type -> Type
newtype ProgramFragment u = ProgramFragment
  { pfDecls :: Seq (ProgramDecl u)
  }
  deriving newtype (Semigroup, Monoid)

emptyProgramFragment :: ProgramFragment u
emptyProgramFragment =
  mempty
{-# INLINE emptyProgramFragment #-}

singletonProgramDecl ::
  ProgramDecl u ->
  ProgramFragment u
singletonProgramDecl =
  ProgramFragment . Seq.singleton
{-# INLINE singletonProgramDecl #-}

appendProgramFragments ::
  ProgramFragment u ->
  ProgramFragment u ->
  ProgramFragment u
appendProgramFragments =
  (<>)
{-# INLINE appendProgramFragments #-}

type ProgramBuilder :: Type -> Type
data ProgramBuilder u = ProgramBuilder
  { pbBaseRewrites :: !(Seq (SatRuleSource u)),
    pbBaseFacts :: !(Seq (SatFactSource u)),
    pbContextRewrites :: !(Map (SatContext u) (Seq (SatRuleSource u))),
    pbContextFacts :: !(Map (SatContext u) (Seq (SatFactSource u))),
    pbBaseActivation :: !(Set RewriteRuleId),
    pbContextActivation :: !(Map (SatContext u) (Set RewriteRuleId)),
    pbBaseSupport :: !(Map RewriteRuleId (SupportBasis (SatContext u))),
    pbDuplicateSupportIds :: !(Set RewriteRuleId)
  }

emptyProgramBuilder :: ProgramBuilder u
emptyProgramBuilder =
  ProgramBuilder
    { pbBaseRewrites = Seq.empty,
      pbBaseFacts = Seq.empty,
      pbContextRewrites = Map.empty,
      pbContextFacts = Map.empty,
      pbBaseActivation = Set.empty,
      pbContextActivation = Map.empty,
      pbBaseSupport = Map.empty,
      pbDuplicateSupportIds = Set.empty
    }

finishProgram ::
  forall u.
  (RewriteSystem u, FactSystem u, Ord (SatContext u)) =>
  ProgramFragment u ->
  Either
    (ProgramBuildError u)
    (Program 'SourceProgramStage u)
finishProgram (ProgramFragment declarations) =
  let builder =
        Foldable.foldl'
          applyProgramDecl
          emptyProgramBuilder
          declarations
      sourceProgram =
        sourceProgramFromBuilder builder
      violations =
        duplicateSupportViolations (pbDuplicateSupportIds builder)
          <> sourceProgramViolations sourceProgram
   in case NonEmpty.nonEmpty violations of
        Nothing ->
          Right sourceProgram
        Just nonEmptyViolations ->
          Left (SaturationSupportError nonEmptyViolations)
  where
    sourceProgramViolations ::
      Program 'SourceProgramStage u ->
      [ProgramViolation (SatContext u)]
    sourceProgramViolations sourceProgram =
      case validateSourceProgram @u sourceProgram of
        Right () ->
          []
        Left supportError ->
          NonEmpty.toList (unSaturationSupportError supportError)
{-# INLINE finishProgram #-}

applyProgramDecl ::
  Ord (SatContext u) =>
  ProgramBuilder u ->
  ProgramDecl u ->
  ProgramBuilder u
applyProgramDecl builder declaration =
  case declaration of
    DeclareRewrite site source ->
      declareRewrite site source builder
    DeclareFact site source ->
      declareFact site source builder
    ActivateBaseRewrite site ruleIdValue ->
      declareActivation site ruleIdValue builder
    DeclareBaseRewriteSupport ruleIdValue supportBasis ->
      declareSupport ruleIdValue supportBasis builder

declareRewrite ::
  Ord (SatContext u) =>
  MatchSite (SatContext u) ->
  SatRuleSource u ->
  ProgramBuilder u ->
  ProgramBuilder u
declareRewrite site source builder =
  case site of
    BaseSite ->
      builder
        { pbBaseRewrites =
            pbBaseRewrites builder Seq.|> source
        }
    ContextSite contextValue ->
      builder
        { pbContextRewrites =
            Map.insertWith
              (flip (<>))
              contextValue
              (Seq.singleton source)
              (pbContextRewrites builder)
        }

declareFact ::
  Ord (SatContext u) =>
  MatchSite (SatContext u) ->
  SatFactSource u ->
  ProgramBuilder u ->
  ProgramBuilder u
declareFact site source builder =
  case site of
    BaseSite ->
      builder
        { pbBaseFacts =
            pbBaseFacts builder Seq.|> source
        }
    ContextSite contextValue ->
      builder
        { pbContextFacts =
            Map.insertWith
              (flip (<>))
              contextValue
              (Seq.singleton source)
              (pbContextFacts builder)
        }

declareActivation ::
  Ord (SatContext u) =>
  MatchSite (SatContext u) ->
  RewriteRuleId ->
  ProgramBuilder u ->
  ProgramBuilder u
declareActivation site ruleIdValue builder =
  case site of
    BaseSite ->
      builder
        { pbBaseActivation =
            Set.insert ruleIdValue (pbBaseActivation builder)
        }
    ContextSite contextValue ->
      builder
        { pbContextActivation =
            Map.insertWith
              Set.union
              contextValue
              (Set.singleton ruleIdValue)
              (pbContextActivation builder)
        }

declareSupport ::
  RewriteRuleId ->
  SupportBasis (SatContext u) ->
  ProgramBuilder u ->
  ProgramBuilder u
declareSupport ruleIdValue supportBasis builder =
  if Map.member ruleIdValue (pbBaseSupport builder)
    then
      builder
        { pbDuplicateSupportIds =
            Set.insert ruleIdValue (pbDuplicateSupportIds builder)
        }
    else
      builder
        { pbBaseSupport =
            Map.insert ruleIdValue supportBasis (pbBaseSupport builder)
        }

sourceProgramFromBuilder ::
  ProgramBuilder u ->
  Program 'SourceProgramStage u
sourceProgramFromBuilder builder =
  SiteProgram
    { spFactRules =
        SiteIndex
          { siBase = Foldable.toList (pbBaseFacts builder),
            siContexts = fmap Foldable.toList (pbContextFacts builder)
          },
      spRewriteRules =
        SiteIndex
          { siBase = Foldable.toList (pbBaseRewrites builder),
            siContexts = fmap Foldable.toList (pbContextRewrites builder)
          },
      spSupportedFactRules = [],
      spSupportedRewriteRules = Map.empty,
      spRewriteActivation =
        MatchActivationIndex
          { maiBase = pbBaseActivation builder,
            maiContexts =
              Map.filter
                (not . Set.null)
                (pbContextActivation builder)
          },
      spBaseRewriteSupport =
        pbBaseSupport builder
    }

duplicateSupportViolations ::
  Set RewriteRuleId ->
  [ProgramViolation context]
duplicateSupportViolations ruleIds =
  [ ProgramViolation
      { pvSite = BaseProgramSite,
        pvRuleKind = RewriteRuleKind,
        pvRelation = DuplicateSupportDeclaration,
        pvRuleIds = ruleIds
      }
  | not (Set.null ruleIds)
  ]

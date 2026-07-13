{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.EGraph.Introspection.Analysis.Equivariant
  ( GlobalAutomorphismGroup (..)
  , globalAutomorphismGroup
  , equivariantRepresentatives
  , equivariantPruningGate
  ) where

import Data.Function ((&))
import Data.Kind (Type)
import Data.List (sort)
import Data.List.NonEmpty (toList)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Moonlight.Analysis.Equivariant qualified as Generic
import Moonlight.Analysis.Equivariant
  ( GlobalAutomorphismGroup (..)
  , GlobalOrbitModel (..)
  )
import Moonlight.Category (chainMorphisms)
import Moonlight.Core (RegionNodeId (..), RewriteRuleId)
import Moonlight.Sheaf.Site
  ( GrothendieckCell,
    grothendieckCellSimplex,
    gmTargetMorphism,
  )
import Moonlight.Sheaf.Site (SiteComplexScaffold (..))
import Moonlight.Homology (basisCellNodeId)
import Moonlight.EGraph.Introspection.Analysis.Resolution
  ( ResolutionBundle (..),
    ResolutionKernel (..),
    RewriteSiteScaffold,
  )
import Moonlight.EGraph.Introspection.Core.Rewrite
  ( RewriteSystem
  , RuntimeRuleIdentity (..)
  , resolveRuntimeRuleIdentity
  )
import Moonlight.Sheaf.Section.Stalk.Groupoid
  ( InterfaceStalkGroupoid
  , interfaceStalkObjects
  , orbitRepresentatives
  )
import Moonlight.EGraph.Introspection.Core.Rewrite (RewriteMorphism)
import Moonlight.Sheaf.Obstruction (CandidateRegionSeed (..))
import Moonlight.Category.Simplicial (nerveSimplexChain)

globalAutomorphismGroup :: Eq (RewriteMorphism f) => ResolutionBundle f -> GlobalAutomorphismGroup
globalAutomorphismGroup resolutionValue =
  let rewriteSystem = rkRewriteSystem (rbKernel resolutionValue)
      analysisScaffold = rkScaffold (rbKernel resolutionValue)
   in Generic.globalAutomorphismGroup
        (rewriteOrbitModel rewriteSystem)
        analysisScaffold

equivariantRepresentatives :: GlobalAutomorphismGroup -> [CandidateRegionSeed root] -> [CandidateRegionSeed root]
equivariantRepresentatives =
  Generic.equivariantRepresentatives regionNodeValue

equivariantPruningGate ::
  (CandidateRegionSeed root -> InterfaceStalkGroupoid) ->
  GlobalAutomorphismGroup ->
  CandidateRegionSeed root ->
  Bool
equivariantPruningGate projectLocalGroupoid =
  Generic.equivariantPruningGate
    regionNodeValue
    (interfaceStalkObjects . projectLocalGroupoid)
    (orbitRepresentatives . projectLocalGroupoid)

rewriteOrbitModel ::
  Eq (RewriteMorphism f) =>
  RewriteSystem f ->
  GlobalOrbitModel
    (RewriteSiteScaffold f)
    (GrothendieckCell (RewriteSystem f))
    (OrbitFingerprint)
rewriteOrbitModel rewriteSystem =
  GlobalOrbitModel
    { gomCellsWithNodeIds = \scaffold ->
        scsBasisRefs scaffold
          & Map.toList
          & fmap
            (\(cellValue, basisRefValue) ->
                (cellValue, basisCellNodeId (scsChainComplex scaffold) basisRefValue)
            ),
      gomFingerprintOf =
        cellFingerprint rewriteSystem
    }

type OrbitFingerprint :: Type
data OrbitFingerprint
  = SingletonOrbit !Int
  | SymmetricOrbit ![RewriteRuleId]
  deriving stock (Eq, Ord, Show)

cellFingerprint ::
  Eq (RewriteMorphism f) =>
  RewriteSystem f ->
  GrothendieckCell (RewriteSystem f) ->
  Int ->
  OrbitFingerprint
cellFingerprint rewriteSystem cellValue nodeIdValue =
  let ambiguousClassIds =
        grothendieckCellSimplex cellValue
          & nerveSimplexChain
          & chainMorphisms
          & mapMaybe gmTargetMorphism
          & mapMaybe (ambiguousClassRepresentative rewriteSystem)
          & sort
   in case ambiguousClassIds of
        [] -> SingletonOrbit nodeIdValue
        _ -> SymmetricOrbit ambiguousClassIds

ambiguousClassRepresentative :: Eq (RewriteMorphism f) => RewriteSystem f -> RewriteMorphism f -> Maybe RewriteRuleId
ambiguousClassRepresentative rewriteSystem spanValue =
  case resolveRuntimeRuleIdentity rewriteSystem spanValue of
    AmbiguousRuntimeRuleIdentity ruleIds ->
      case sort (toList ruleIds) of
        classRepresentative : _ -> Just classRepresentative
        [] -> Nothing
    UniqueRuntimeRuleIdentity {} -> Nothing
    NoRuntimeRuleIdentity -> Nothing

regionNodeValue :: CandidateRegionSeed root -> Int
regionNodeValue seedValue =
  case crsNodeId seedValue of
    RegionNodeId ordinalValue -> ordinalValue

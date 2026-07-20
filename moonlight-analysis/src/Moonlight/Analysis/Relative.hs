{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Analysis.Relative
  ( AbsoluteDiagnostics (..),
    GroundingKind (..),
    MorphismGrounding (..),
    ChainGrounding (..),
    RelativeDiagnostics (..),
    SupportRuntimeCounts (..),
    RuntimeRelativeDiagnostics (..),
    RelativeGroundingModel (..),
    RuntimeGroundingOverlay (..),
    absoluteDiagnostics,
    relativeDiagnostics,
    runtimeRelativeDiagnostics,
  )
where

import Data.Kind (Type)
import Data.List (nubBy)
import Data.Maybe (mapMaybe)

type AbsoluteDiagnostics :: Type -> Type
newtype AbsoluteDiagnostics summary = AbsoluteDiagnostics
  { adStructuralSummary :: summary
  }
  deriving stock (Eq, Show)

type GroundingKind :: Type
newtype GroundingKind = GroundingKind
  { groundingKindName :: String
  }
  deriving stock (Eq, Ord, Show, Read)

type MorphismGrounding :: Type -> Type -> Type
data MorphismGrounding morphism node = MorphismGrounding
  { mgStaticMorphism :: morphism,
    mgGroundedNode :: Maybe node
  }
  deriving stock (Eq, Show)

type ChainGrounding :: Type -> Type -> Type -> Type
data ChainGrounding morphism node edge = ChainGrounding
  { cgStaticLeft :: morphism,
    cgStaticRight :: morphism,
    cgGroundedLeft :: node,
    cgGroundedRight :: node,
    cgGroundedEdge :: Maybe edge
  }
  deriving stock (Eq, Show)

type RelativeDiagnostics :: Type -> Type -> Type -> Type -> Type
data RelativeDiagnostics summary morphism node edge = RelativeDiagnostics
  { rdAbsolute :: AbsoluteDiagnostics summary,
    rdGroundingKind :: GroundingKind,
    rdGroundedMorphismCount :: Int,
    rdGroundedNodeCoverage :: Int,
    rdGroundableChainCount :: Int,
    rdGroundedChainCount :: Int,
    rdVerticalLoss :: Int,
    rdStructuralCompressionGap :: Int,
    rdMorphismGroundings :: [MorphismGrounding morphism node],
    rdChainGroundings :: [ChainGrounding morphism node edge]
  }
  deriving stock (Eq, Show)

type SupportRuntimeCounts :: Type
data SupportRuntimeCounts = SupportRuntimeCounts
  { srcObservedRuleCount :: Int,
    srcSuppressedRuleCount :: Int,
    srcCooldownRuleCount :: Int
  }
  deriving stock (Eq, Show, Read)

type RuntimeRelativeDiagnostics :: Type -> Type -> Type -> Type -> Type
data RuntimeRelativeDiagnostics summary morphism node edge = RuntimeRelativeDiagnostics
  { rrdBase :: RelativeDiagnostics summary morphism node edge,
    rrdObservedGroundedMorphismCount :: Int,
    rrdObservedGroundedNodeCoverage :: Int,
    rrdObservedGroundedChainCount :: Int,
    rrdUnobservedGroundedChainCount :: Int,
    rrdUnmappedGroundedNodeCount :: Int,
    rrdAmbiguousGroundedNodeCount :: Int,
    rrdSupportRuntimeCounts :: Maybe SupportRuntimeCounts
  }
  deriving stock (Eq, Show)

type RelativeGroundingModel :: Type -> Type -> Type -> Type -> Type -> Type
data RelativeGroundingModel domain summary morphism node edge = RelativeGroundingModel
  { rgmSummaryOf :: domain -> summary,
    rgmVerticalLoss :: summary -> Int,
    rgmMorphismsOf :: domain -> [morphism],
    rgmComposablePairsOf :: domain -> [(morphism, morphism)],
    rgmGroundMorphism :: morphism -> Maybe node,
    rgmFindEdge :: node -> node -> Maybe edge,
    rgmGroundingKind :: GroundingKind
  }

type RuntimeGroundingOverlay :: Type -> Type -> Type
data RuntimeGroundingOverlay node edge = RuntimeGroundingOverlay
  { rgoObservedNodes :: [node],
    rgoObservedEdges :: [edge],
    rgoUnobservedEdges :: [edge],
    rgoUnmappedNodes :: [node],
    rgoAmbiguousNodes :: [node],
    rgoSupportCounts :: Maybe SupportRuntimeCounts
  }

absoluteDiagnostics :: (domain -> summary) -> domain -> AbsoluteDiagnostics summary
absoluteDiagnostics summarize =
  AbsoluteDiagnostics . summarize

relativeDiagnostics ::
  Eq node =>
  RelativeGroundingModel domain summary morphism node edge ->
  domain ->
  RelativeDiagnostics summary morphism node edge
relativeDiagnostics groundingModel domainValue =
  let absoluteValue = absoluteDiagnostics (rgmSummaryOf groundingModel) domainValue
      summaryValue = adStructuralSummary absoluteValue
      morphismGroundings =
        fmap
          (\morphismValue ->
              MorphismGrounding
                { mgStaticMorphism = morphismValue,
                  mgGroundedNode = rgmGroundMorphism groundingModel morphismValue
                }
          )
          (rgmMorphismsOf groundingModel domainValue)
      chainGroundings =
        mapMaybe
          (groundedChain groundingModel)
          (rgmComposablePairsOf groundingModel domainValue)
      groundedNodes = mapMaybe mgGroundedNode morphismGroundings
      groundedMorphismCount = length groundedNodes
      groundedNodeCoverage = length (nubBy (==) groundedNodes)
      groundedChainCount = length (mapMaybe cgGroundedEdge chainGroundings)
   in RelativeDiagnostics
        { rdAbsolute = absoluteValue,
          rdGroundingKind = rgmGroundingKind groundingModel,
          rdGroundedMorphismCount = groundedMorphismCount,
          rdGroundedNodeCoverage = groundedNodeCoverage,
          rdGroundableChainCount = length chainGroundings,
          rdGroundedChainCount = groundedChainCount,
          rdVerticalLoss = rgmVerticalLoss groundingModel summaryValue,
          rdStructuralCompressionGap = groundedMorphismCount - groundedNodeCoverage,
          rdMorphismGroundings = morphismGroundings,
          rdChainGroundings = chainGroundings
        }

runtimeRelativeDiagnostics ::
  (Eq node, Eq edge) =>
  RuntimeGroundingOverlay node edge ->
  RelativeDiagnostics summary morphism node edge ->
  RuntimeRelativeDiagnostics summary morphism node edge
runtimeRelativeDiagnostics runtimeOverlay relativeValue =
  let groundedNodes = mapMaybe mgGroundedNode (rdMorphismGroundings relativeValue)
      uniqueGroundedNodes = nubBy (==) groundedNodes
      observedGroundedNodes =
        filter (`elem` rgoObservedNodes runtimeOverlay) groundedNodes
      uniqueObservedGroundedNodes = nubBy (==) observedGroundedNodes
      groundedEdges = mapMaybe cgGroundedEdge (rdChainGroundings relativeValue)
   in RuntimeRelativeDiagnostics
        { rrdBase = relativeValue,
          rrdObservedGroundedMorphismCount = length observedGroundedNodes,
          rrdObservedGroundedNodeCoverage = length uniqueObservedGroundedNodes,
          rrdObservedGroundedChainCount = length (filter (`elem` rgoObservedEdges runtimeOverlay) groundedEdges),
          rrdUnobservedGroundedChainCount = length (filter (`elem` rgoUnobservedEdges runtimeOverlay) groundedEdges),
          rrdUnmappedGroundedNodeCount = length (filter (`elem` rgoUnmappedNodes runtimeOverlay) uniqueGroundedNodes),
          rrdAmbiguousGroundedNodeCount = length (filter (`elem` rgoAmbiguousNodes runtimeOverlay) uniqueGroundedNodes),
          rrdSupportRuntimeCounts = rgoSupportCounts runtimeOverlay
        }

groundedChain ::
  RelativeGroundingModel domain summary morphism node edge ->
  (morphism, morphism) ->
  Maybe (ChainGrounding morphism node edge)
groundedChain groundingModel (leftMorphism, rightMorphism) = do
  groundedLeft <- rgmGroundMorphism groundingModel leftMorphism
  groundedRight <- rgmGroundMorphism groundingModel rightMorphism
  pure
    ChainGrounding
      { cgStaticLeft = leftMorphism,
        cgStaticRight = rightMorphism,
        cgGroundedLeft = groundedLeft,
        cgGroundedRight = groundedRight,
        cgGroundedEdge = rgmFindEdge groundingModel groundedLeft groundedRight
      }

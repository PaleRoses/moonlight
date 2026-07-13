module Moonlight.EGraph.Test.Scale.Site
  ( ScaleContext,
    scaleContextIndex,
    ScaleSiteShape (..),
    ScaleSiteError (..),
    SupportProbe (..),
    ScaleTopology (..),
    ScaleSite,
    scaleSiteLattice,
    scaleSiteContexts,
    scaleSiteContextCount,
    scaleSiteBottom,
    scaleSiteTop,
    scaleSiteTopology,
    scaleSitePrimaryProbe,
    scaleSiteSecondaryProbe,
    scaleSiteSampledContexts,
    scaledChain,
    scaledTree,
    scaledDiamondStack,
  )
where

import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as Set
import Moonlight.FiniteLattice
  ( ContextLattice,
    ContextLatticeCompileError,
    ContextLatticeLookupError,
    compileContextLattice,
    contextOrderDecl,
    supportReachableLatticeContexts,
    upperCovers,
  )
import Moonlight.Rewrite.ProofContext (principalSupport)

newtype ScaleContext = ScaleContext Int
  deriving stock (Eq, Ord)

instance Show ScaleContext where
  show contextValue =
    "C" <> show (scaleContextIndex contextValue)

scaleContextIndex :: ScaleContext -> Int
scaleContextIndex (ScaleContext contextIndex) =
  contextIndex

data ScaleSiteShape
  = ScaleChain
  | ScaleTree
  | ScaleDiamondStack
  deriving stock (Eq, Ord, Show)

data ScaleSiteError
  = ScaleSiteSizeTooSmall !ScaleSiteShape !Int !Int
  | ScaleSiteCompileFailed !(ContextLatticeCompileError ScaleContext)
  | ScaleSiteLookupFailed !(ContextLatticeLookupError ScaleContext)
  | ScaleSiteEmptyUpset !ScaleContext
  deriving stock (Eq, Show)

data SupportProbe = SupportProbe
  { supportProbeAnchor :: !ScaleContext,
    supportProbeUpset :: !(NonEmpty ScaleContext),
    supportProbeFrontier :: ![ScaleContext]
  }
  deriving stock (Eq, Show)

data ScaleTopology
  = LinearTopology !SupportProbe
  | BranchingTopology !SupportProbe !SupportProbe
  deriving stock (Eq, Show)

data ScaleSite = ScaleSite
  { ssLattice :: !(ContextLattice ScaleContext),
    ssContexts :: !(NonEmpty ScaleContext),
    ssBottom :: !ScaleContext,
    ssTop :: !ScaleContext,
    ssTopology :: !ScaleTopology
  }

scaleSiteLattice :: ScaleSite -> ContextLattice ScaleContext
scaleSiteLattice =
  ssLattice

scaleSiteContexts :: ScaleSite -> NonEmpty ScaleContext
scaleSiteContexts =
  ssContexts

scaleSiteContextCount :: ScaleSite -> Int
scaleSiteContextCount =
  NonEmpty.length . ssContexts

scaleSiteBottom :: ScaleSite -> ScaleContext
scaleSiteBottom =
  ssBottom

scaleSiteTop :: ScaleSite -> ScaleContext
scaleSiteTop =
  ssTop

scaleSiteTopology :: ScaleSite -> ScaleTopology
scaleSiteTopology =
  ssTopology

scaleSitePrimaryProbe :: ScaleSite -> SupportProbe
scaleSitePrimaryProbe site =
  case ssTopology site of
    LinearTopology primaryProbe -> primaryProbe
    BranchingTopology primaryProbe _ -> primaryProbe

scaleSiteSecondaryProbe :: ScaleSite -> Maybe SupportProbe
scaleSiteSecondaryProbe site =
  case ssTopology site of
    LinearTopology _ -> Nothing
    BranchingTopology _ secondaryProbe -> Just secondaryProbe

scaleSiteSampledContexts :: ScaleSite -> NonEmpty ScaleContext
scaleSiteSampledContexts site =
  let primaryProbe = scaleSitePrimaryProbe site
      sampledSet =
        Set.fromList
          [ scaleSiteBottom site,
            scaleSiteTop site,
            supportProbeAnchor primaryProbe
          ]
          <> Set.fromList (supportProbeFrontier primaryProbe)
          <> foldMap
            ( \secondaryProbe ->
                Set.fromList
                  ( supportProbeAnchor secondaryProbe
                      : supportProbeFrontier secondaryProbe
                  )
            )
            (scaleSiteSecondaryProbe site)
      bottomContext = scaleSiteBottom site
   in bottomContext
        :| filter
          (/= bottomContext)
          (Set.toAscList sampledSet)

scaledChain :: Int -> Either ScaleSiteError ScaleSite
scaledChain contextCount = do
  requireMinimum ScaleChain 3 contextCount
  let contexts = contextsForCount contextCount
      bottomContext = ScaleContext 0
      topContext = ScaleContext (contextCount - 1)
      probeContext = ScaleContext (contextCount `div` 2)
      edges =
        fmap
          (\contextIndex -> (ScaleContext contextIndex, ScaleContext (contextIndex + 1)))
          [0 .. contextCount - 2]
  compileScaleSite
    contexts
    bottomContext
    topContext
    edges
    (Left probeContext)

scaledTree :: Int -> Either ScaleSiteError ScaleSite
scaledTree contextCount = do
  requireMinimum ScaleTree 4 contextCount
  let contexts = contextsForCount contextCount
      bottomContext = ScaleContext 0
      topContext = ScaleContext (contextCount - 1)
      treeEdges =
        fmap
          ( \contextIndex ->
              ( ScaleContext ((contextIndex - 1) `div` 2),
                ScaleContext contextIndex
              )
          )
          [1 .. contextCount - 2]
      topEdges =
        fmap
          (\contextIndex -> (ScaleContext contextIndex, topContext))
          [0 .. contextCount - 2]
  compileScaleSite
    contexts
    bottomContext
    topContext
    (treeEdges <> topEdges)
    (Right (ScaleContext 1, ScaleContext 2))

scaledDiamondStack :: Int -> Either ScaleSiteError ScaleSite
scaledDiamondStack diamondCount = do
  requireMinimum ScaleDiamondStack 1 diamondCount
  let contextCount = 3 * diamondCount + 1
      contexts = contextsForCount contextCount
      bottomContext = ScaleContext 0
      topContext = ScaleContext (contextCount - 1)
      layerEdges = foldMap diamondEdges [0 .. diamondCount - 1]
      probeLayer = diamondCount `div` 2
      probeBottomIndex = 3 * probeLayer
  compileScaleSite
    contexts
    bottomContext
    topContext
    layerEdges
    (Right (ScaleContext (probeBottomIndex + 1), ScaleContext (probeBottomIndex + 2)))

requireMinimum :: ScaleSiteShape -> Int -> Int -> Either ScaleSiteError ()
requireMinimum shape minimumSize actualSize =
  if actualSize < minimumSize
    then Left (ScaleSiteSizeTooSmall shape minimumSize actualSize)
    else Right ()

contextsForCount :: Int -> NonEmpty ScaleContext
contextsForCount contextCount =
  ScaleContext 0 :| fmap ScaleContext [1 .. contextCount - 1]

diamondEdges :: Int -> [(ScaleContext, ScaleContext)]
diamondEdges layerIndex =
  let bottomContext = ScaleContext (3 * layerIndex)
      leftContext = ScaleContext (3 * layerIndex + 1)
      rightContext = ScaleContext (3 * layerIndex + 2)
      topContext = ScaleContext (3 * layerIndex + 3)
   in [ (bottomContext, leftContext),
        (bottomContext, rightContext),
        (leftContext, topContext),
        (rightContext, topContext)
      ]

compileScaleSite ::
  NonEmpty ScaleContext ->
  ScaleContext ->
  ScaleContext ->
  [(ScaleContext, ScaleContext)] ->
  Either ScaleContext (ScaleContext, ScaleContext) ->
  Either ScaleSiteError ScaleSite
compileScaleSite contexts bottomContext topContext edges probeAnchors = do
  lattice <-
    first ScaleSiteCompileFailed $
      compileContextLattice
        (Set.fromList (NonEmpty.toList contexts))
        (contextOrderDecl topContext bottomContext edges)
  topology <-
    case probeAnchors of
      Left primaryAnchor ->
        LinearTopology <$> supportProbe lattice contexts primaryAnchor
      Right (primaryAnchor, secondaryAnchor) ->
        BranchingTopology
          <$> supportProbe lattice contexts primaryAnchor
          <*> supportProbe lattice contexts secondaryAnchor
  pure
    ScaleSite
      { ssLattice = lattice,
        ssContexts = contexts,
        ssBottom = bottomContext,
        ssTop = topContext,
        ssTopology = topology
      }

supportProbe ::
  ContextLattice ScaleContext ->
  NonEmpty ScaleContext ->
  ScaleContext ->
  Either ScaleSiteError SupportProbe
supportProbe lattice contexts anchor = do
  upsetValues <-
    first ScaleSiteLookupFailed $
      supportReachableLatticeContexts lattice (principalSupport anchor)
  upset <-
    case NonEmpty.nonEmpty upsetValues of
      Nothing -> Left (ScaleSiteEmptyUpset anchor)
      Just nonEmptyUpset -> Right nonEmptyUpset
  let upsetSet = Set.fromList upsetValues
      outsideValues =
        filter (`Set.notMember` upsetSet) (NonEmpty.toList contexts)
  frontier <-
    first ScaleSiteLookupFailed $
      traverse (frontierCandidate lattice upsetSet) outsideValues
  pure
    SupportProbe
      { supportProbeAnchor = anchor,
        supportProbeUpset = upset,
        supportProbeFrontier =
          fmap fst (filter snd (zip outsideValues frontier))
      }

frontierCandidate ::
  ContextLattice ScaleContext ->
  Set.Set ScaleContext ->
  ScaleContext ->
  Either (ContextLatticeLookupError ScaleContext) Bool
frontierCandidate lattice upsetSet contextValue =
  all (`Set.member` upsetSet) <$> upperCovers lattice contextValue

-- | Scale corpus construction, site descent, and validated fixture gluing.
module Moonlight.EGraph.Bench.Harness.Fixture
  ( SiteSpec (..),
    ScaleProbes (..),
    ScaleCorpus (..),
    prepareScaleCorpus,
    ScaleFixtureSpec (..),
    ScaleFixture (..),
    buildScaleSite,
    prepareScaleFixture,
  ) where

import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.Fix (Fix)
import Data.List.NonEmpty qualified as NonEmpty
import Moonlight.Core (ClassId, Language)
import Moonlight.EGraph.Bench.Harness.Digest (contextGraphDigest, graphDigest)
import Moonlight.EGraph.Bench.Harness.Run (BenchFailure)
import Moonlight.EGraph.Pure.Analysis (AnalysisSpec)
import Moonlight.EGraph.Pure.Change (EGraphMutationResult (..))
import Moonlight.EGraph.Pure.Context qualified as Context
import Moonlight.EGraph.Pure.Context.Core (cegSite)
import Moonlight.EGraph.Saturation.Context.State qualified as SaturationState
import Moonlight.EGraph.Pure.Kernel.HashCons (insertTermsTracked)
import Moonlight.EGraph.Pure.Types (EGraph, emptyEGraph)
import Moonlight.EGraph.Test.Scale.Site (ScaleContext, ScaleSite)
import Moonlight.EGraph.Test.Scale.Site qualified as Scale
import Moonlight.Rewrite.ProofContext (principalSupport)
import Moonlight.Rewrite.ProofContext (ProofRetention (KeepNoProof))
import Moonlight.Rewrite.System (RewriteCondition)
import Moonlight.Rewrite.System (FactRule)
import Moonlight.Rewrite.System (RawRewriteRule)
import Moonlight.Sheaf.Twist.SupportedRuleSpec qualified as SheafTwist

data SiteSpec
  = TreeSite !Int
  | ChainSite !Int
  | DiamondStackSite !Int

data ScaleProbes = ScaleProbes
  { probeBottom :: !ScaleContext,
    probePrimary :: !ScaleContext,
    probeSecondary :: !(Maybe ScaleContext)
  }

data ScaleCorpus f analysis = ScaleCorpus
  { scTermCount :: !Int,
    scBaseGraph :: !(EGraph f analysis),
    scClassIds :: ![ClassId]
  }

prepareScaleCorpus ::
  Language f => AnalysisSpec f analysis -> [Fix f] -> Either BenchFailure (ScaleCorpus f analysis)
prepareScaleCorpus analysis terms =
  first show (insertTermsTracked terms (emptyEGraph analysis)) >>= \mutation ->
    let baseGraph = emrGraph mutation
        classIds = emrResult mutation
     in graphDigest baseGraph
          `seq` length classIds
          `seq` Right (ScaleCorpus (length terms) baseGraph classIds)

type AnchoredSpecs value = [(ScaleContext, value)]
type ScaleRewrite f = RawRewriteRule (RewriteCondition () f) f

data ScaleFixtureSpec f analysis = ScaleFixtureSpec
  { sfsCorpus :: !(Either BenchFailure (ScaleCorpus f analysis)),
    sfsRules ::
      ScaleProbes -> Either BenchFailure (AnchoredSpecs (ScaleRewrite f)),
    sfsFacts ::
      ScaleProbes -> Either BenchFailure (AnchoredSpecs (FactRule () f))
  }

data ScaleFixture f analysis = ScaleFixture
  { sfSite :: !ScaleSite,
    sfProbes :: !ScaleProbes,
    sfTermCount :: !Int,
    sfBaseGraph :: !(EGraph f analysis),
    sfClassIds :: ![ClassId],
    sfContextGraph :: !(Context.ContextEGraph f analysis ScaleContext),
    sfProofGraph :: !(SaturationState.SaturatingProofEGraph () f analysis ScaleContext ()),
    sfRuleBook ::
      !(SheafTwist.SupportedRuleBook ScaleContext (ScaleRewrite f)),
    sfFactBook :: !(SheafTwist.SupportedFactBook ScaleContext (FactRule () f)),
    sfDigest :: !Int
  }

buildScaleSite :: SiteSpec -> Either BenchFailure (ScaleSite, ScaleProbes)
buildScaleSite siteSpec = do
  let (label, siteResult) =
        case siteSpec of
          TreeSite count -> ("scaled tree site (" <> show count <> ")", Scale.scaledTree count)
          ChainSite count -> ("scaled chain site (" <> show count <> ")", Scale.scaledChain count)
          DiamondStackSite count ->
            ("scaled diamond-stack site (" <> show count <> ")", Scale.scaledDiamondStack count)
  site <- first (((label <> ": ") <>) . show) siteResult
  pure
    ( site,
      ScaleProbes
        (Scale.scaleSiteBottom site)
        (Scale.supportProbeAnchor (Scale.scaleSitePrimaryProbe site))
        (Scale.supportProbeAnchor <$> Scale.scaleSiteSecondaryProbe site)
    )

prepareScaleFixture ::
  Language f =>
  (ScaleSite, ScaleProbes) ->
  ScaleFixtureSpec f analysis ->
  Either BenchFailure (ScaleFixture f analysis)
prepareScaleFixture (site, probes) spec = do
  corpus <- sfsCorpus spec
  let contexts = NonEmpty.toList (Scale.scaleSiteContexts site)
  contextGraph <-
    foldM
      (\graphValue contextValue -> first show (Context.activateContext contextValue graphValue))
      (Context.emptyContextEGraph (Scale.scaleSiteLattice site) (scBaseGraph corpus))
      contexts
  let
      siteValue = cegSite contextGraph
      expectedContextCount = Scale.scaleSiteContextCount site
      activeContextCount = length (Context.contextPreparedObjects contextGraph)
  if activeContextCount == expectedContextCount
    then pure ()
    else
      Left
        ( "scale fixture: context activation mismatch: expected "
            <> show expectedContextCount
            <> ", active "
            <> show activeContextCount
        )
  ruleSpecs <- sfsRules spec probes
  factSpecs <- sfsFacts spec probes
  ruleBook <-
    first (("scale fixture rule book: " <>) . show) $
      SheafTwist.supportedRuleBook
        siteValue
        [SheafTwist.SupportedRuleSpec (principalSupport anchor) rule | (anchor, rule) <- ruleSpecs]
  factBook <-
    first (("scale fixture fact book: " <>) . show) $
      SheafTwist.supportedFactBook
        siteValue
        [SheafTwist.SupportedFactSpec (principalSupport anchor) fact | (anchor, fact) <- factSpecs]
  activeRuleDigest <-
    first (("scale fixture active rules: " <>) . show) $
      sum . fmap length
        <$> traverse (\context -> SheafTwist.rulesActiveAt siteValue context ruleBook) contexts
  activeFactDigest <-
    first (("scale fixture active facts: " <>) . show) $
      sum . fmap length
        <$> traverse (\context -> SheafTwist.factRulesActiveAt siteValue context factBook) contexts
  let fixture =
        ScaleFixture
          { sfSite = site,
            sfProbes = probes,
            sfTermCount = scTermCount corpus,
            sfBaseGraph = scBaseGraph corpus,
            sfClassIds = scClassIds corpus,
            sfContextGraph = contextGraph,
            sfProofGraph =
              SaturationState.emptySaturatingProofEGraphWithRetention
                KeepNoProof
                contextGraph,
            sfRuleBook = ruleBook,
            sfFactBook = factBook,
            sfDigest =
              contextGraphDigest contextGraph
                + activeContextCount
                + activeRuleDigest
                + activeFactDigest
          }
  fixture `seq` pure fixture

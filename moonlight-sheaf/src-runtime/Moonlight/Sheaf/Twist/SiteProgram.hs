module Moonlight.Sheaf.Twist.SiteProgram
  ( buildSupportSiteProgram,
    accumulateSupportMap,
    accumulateNestedSupportMap,
  )
where

import Data.Foldable (foldlM)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Core qualified as SiteProgram
import Moonlight.Core
  ( SupportIndexedRule (..),
  )
import Moonlight.Sheaf.Twist.FactClosure
  ( CompiledSupportedFactRule (..),
  )

accumulateSupportMap ::
  Ord key =>
  (support -> support -> support) ->
  support ->
  [key] ->
  Map key support ->
  Map key support
accumulateSupportMap supportUnion supportValue keys acc =
  foldl'
    (\currentAcc key -> Map.insertWith supportUnion key supportValue currentAcc)
    acc
    keys

accumulateNestedSupportMap ::
  Ord outer =>
  (support -> support -> support) ->
  support ->
  [inner] ->
  (inner -> [outer]) ->
  Map outer support ->
  Map outer support
accumulateNestedSupportMap supportUnion supportValue items expandKeys acc =
  foldl'
    (\currentAcc item -> accumulateSupportMap supportUnion supportValue (expandKeys item) currentAcc)
    acc
    items

buildSupportSiteProgram ::
  (Ord ctx, Ord rid) =>
  (support -> support -> Either err support) ->
  [CompiledSupportedFactRule support fact] ->
  [SupportIndexedRule support rewrite] ->
  (rewrite -> rid) ->
  Either err (SiteProgram.SiteProgram ctx rewrite fact rid support)
buildSupportSiteProgram supportUnion compiledFactRules compiledRules rewriteId =
  do
    supportedRewriteRules <-
      foldlM insertSupportedRewrite Map.empty compiledRules
    let baseRewriteSupport =
          fmap sirSupport supportedRewriteRules
        baseRewriteRules =
          fmap sirRule (Map.elems supportedRewriteRules)
    pure
      SiteProgram.SiteProgram
        { SiteProgram.spFactRules =
            SiteProgram.SiteIndex
              { SiteProgram.siBase = [],
                SiteProgram.siContexts = Map.empty
              },
          SiteProgram.spRewriteRules =
            SiteProgram.SiteIndex
              { SiteProgram.siBase = baseRewriteRules,
                SiteProgram.siContexts = Map.empty
              },
          SiteProgram.spSupportedFactRules =
            fmap supportedFactRule compiledFactRules,
          SiteProgram.spSupportedRewriteRules =
            supportedRewriteRules,
          SiteProgram.spRewriteActivation =
            SiteProgram.MatchActivationIndex
              { SiteProgram.maiBase = mempty,
                SiteProgram.maiContexts = Map.empty
              },
          SiteProgram.spBaseRewriteSupport = baseRewriteSupport
        }
  where
    insertSupportedRewrite accumulatedRules indexedRule =
      let ruleId =
            rewriteId (sirRule indexedRule)
       in case Map.lookup ruleId accumulatedRules of
        Nothing ->
          Right (Map.insert ruleId indexedRule accumulatedRules)
        Just existingRule ->
          fmap
            ( \mergedSupport ->
                Map.insert
                  ruleId
                  indexedRule {sirSupport = mergedSupport}
                  accumulatedRules
            )
            (supportUnion (sirSupport existingRule) (sirSupport indexedRule))

supportedFactRule ::
  CompiledSupportedFactRule support fact ->
  SupportIndexedRule support fact
supportedFactRule compiledFactRule =
  SupportIndexedRule
    { sirSupport = csfrSupport compiledFactRule,
      sirRule = csfrRule compiledFactRule
    }

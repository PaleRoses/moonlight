module Moonlight.EGraph.Test.Saturation.Diagnostics
  ( SupportFamilyDiagnostics (..),
    supportFamilyDiagnostics,
    supportFamilyDiagnosticsForProofGraph,
  )
where

import Data.Kind (Type)
import Data.Set qualified as Set
import Moonlight.EGraph.Pure.Context
  ( cegSite,
    contextPreparedObjects,
  )
import Moonlight.EGraph.Pure.Context.Proof (ProofEGraph, ProofGraph (pgGraph))
import Moonlight.Sheaf.Context.Site
  ( PreparedContextSite,
    preparedSupportReachableObjects,
  )
import Moonlight.Sheaf.Twist.SupportedRuleSpec qualified as SheafTwist
type SupportFamilyDiagnostics :: Type -> Type
data SupportFamilyDiagnostics c = SupportFamilyDiagnostics
  { sfdCachedContextCount :: !Int,
    sfdSupportedRuleCount :: !Int,
    sfdCompiledRuleEntryCount :: !Int,
    sfdMaxRuleContextWidth :: !Int,
    sfdGlobalSupportedRuleCount :: !Int
  }
  deriving stock (Eq, Show)

supportFamilyDiagnostics ::
  Ord c =>
  PreparedContextSite owner c ->
  [c] ->
  SheafTwist.SupportedRuleBook owner c rule ->
  SupportFamilyDiagnostics c
supportFamilyDiagnostics site contexts supportFamilyValue =
  let cachedContexts =
        Set.fromList contexts
      cachedContextCount =
        Set.size cachedContexts
      supportWidth supportValue =
        Set.size
          ( Set.intersection
              cachedContexts
              (either (const Set.empty) id (preparedSupportReachableObjects site cachedContexts supportValue))
          )
      supportedRules =
        SheafTwist.supportedRules supportFamilyValue
      ruleWidths =
        fmap (supportWidth . SheafTwist.srsSupport) supportedRules
      compiledRuleEntryCount =
        sum
          ( fmap
              ( \contextValue ->
                  either
                    (const 0)
                    length
                    (SheafTwist.rulesActiveAt site contextValue supportFamilyValue)
              )
              contexts
          )
   in SupportFamilyDiagnostics
        { sfdCachedContextCount = cachedContextCount,
          sfdSupportedRuleCount = length supportedRules,
          sfdCompiledRuleEntryCount = compiledRuleEntryCount,
          sfdMaxRuleContextWidth = maximum (0 : ruleWidths),
          sfdGlobalSupportedRuleCount =
            length
              (filter (== cachedContextCount) ruleWidths)
        }

supportFamilyDiagnosticsForProofGraph ::
  Ord c =>
  ProofEGraph owner f a c p ->
  SheafTwist.SupportedRuleBook owner c rule ->
  SupportFamilyDiagnostics c
supportFamilyDiagnosticsForProofGraph proofGraph supportFamilyValue =
  let contextGraph = pgGraph proofGraph
   in supportFamilyDiagnostics
        (cegSite contextGraph)
        (contextPreparedObjects contextGraph)
        supportFamilyValue

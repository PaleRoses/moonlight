-- | Twist interpreter entry points: support, program, context, and proof
-- runs.
module Moonlight.Sheaf.Twist.Main
  ( runTwistSupport,
    runTwistProgram,
    runTwistContext,
    runTwistContextProof,
  )
where

import Moonlight.Sheaf.Twist.Compile
  ( TwistCompilation (..),
  )
import Moonlight.Sheaf.Twist.Config
  ( TwistConfig (..),
    withProof,
  )
import Moonlight.Sheaf.Twist.Execute
  ( ContextExecution (..),
    ContextProofExecution (..),
    SupportExecution (..),
  )
import Moonlight.Sheaf.Twist.Program
  ( SupportExecutionProgram (..),
  )
import Moonlight.Sheaf.Twist.SupportedRuleSpec
  ( SupportedFactBook,
    SupportedRuleBook,
  )

runTwistSupport ::
  SupportExecution owner ctx rawFact compiledSupport saturation guidance proof termination proofGraph supportResult err ->
  TwistCompilation owner proofGraph ctx rawRule compiledRule rawFact compiledFact compiledSupport err ->
  TwistConfig saturation guidance proof ->
  SupportedRuleBook owner ctx rawRule ->
  SupportedFactBook owner ctx rawFact ->
  termination ->
  proofGraph ->
  Either err supportResult
runTwistSupport supportExecution compilation twistConfigValue ruleBookValue factBookValue terminationGoal proofGraph = do
  compiledSupportProgram <-
    tcCompileSupportProgram
      compilation
      proofGraph
      ruleBookValue
      (seEffectiveFactBook supportExecution (tcSaturation twistConfigValue) proofGraph factBookValue)
  seRunSupport supportExecution
    compiledSupportProgram
    (tcProofAnnotationBuilder twistConfigValue)
    (tcGuidance twistConfigValue)
    (tcSaturation twistConfigValue)
    terminationGoal
    proofGraph

runTwistProgram ::
  SupportExecution owner ctx rawFact compiledSupport saturation guidance proof termination proofGraph supportResult err ->
  TwistCompilation owner proofGraph ctx rawRule compiledRule rawFact compiledFact compiledSupport err ->
  TwistConfig saturation guidance proof ->
  SupportExecutionProgram owner ctx rawRule rawFact proof ->
  termination ->
  proofGraph ->
  Either err supportResult
runTwistProgram supportExecution compilation twistConfigValue programValue =
  runTwistSupport
    supportExecution
    compilation
    (withProof (sepProofCarrier programValue) twistConfigValue)
    (sepRuleBook programValue)
    (sepFacts programValue)

runTwistContext ::
  ContextExecution owner ctx rawRule saturation guidance contextGraph contextResult err ->
  TwistConfig saturation guidance proof ->
  ctx ->
  SupportedRuleBook owner ctx rawRule ->
  contextGraph ->
  Either err contextResult
runTwistContext contextExecution twistConfigValue =
  ceRunContext contextExecution
    (tcGuidance twistConfigValue)
    (tcSaturation twistConfigValue)

runTwistContextProof ::
  ContextProofExecution owner ctx rawRule saturation guidance proof proofGraph proofResult err ->
  TwistConfig saturation guidance proof ->
  ctx ->
  SupportedRuleBook owner ctx rawRule ->
  proofGraph ->
  Either err proofResult
runTwistContextProof contextProofExecution twistConfigValue =
  cpeRunContextProof contextProofExecution
    (tcProofAnnotationBuilder twistConfigValue)
    (tcGuidance twistConfigValue)
    (tcSaturation twistConfigValue)

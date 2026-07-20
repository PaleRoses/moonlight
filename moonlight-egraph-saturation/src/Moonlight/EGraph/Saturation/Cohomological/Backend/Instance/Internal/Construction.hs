module Moonlight.EGraph.Saturation.Cohomological.Backend.Instance.Internal.Construction
  ( mkCohomologicalBackend,
    withRewriteSystemWitness,
    cohomologicalBackendForProfile,
  )
where

import Moonlight.Core (Language)
import Moonlight.EGraph.Introspection.Core.Rewrite (RewriteSystem)
import Moonlight.EGraph.Saturation.Cohomological.Backend.Modality
  ( validateSheafModalityCoverage,
  )
import Moonlight.EGraph.Saturation.Cohomological.Backend.Instance.Internal.Site
  ( CohomologicalBackend (..),
  )
import Moonlight.EGraph.Saturation.Cohomological.Types
  ( EGraphSectionCertification,
  )
import Moonlight.Sheaf.Obstruction
  ( CohomologicalPolicy,
    CohomologicalProfile,
    profilePolicy,
  )

mkCohomologicalBackend ::
  Language f =>
  EGraphSectionCertification owner c f ->
  CohomologicalPolicy ->
  CohomologicalBackend owner c f
mkCohomologicalBackend context policy =
  CohomologicalBackend
    { cbContext = context,
      cbPolicy = policy,
      cbModalityCoverage = validateSheafModalityCoverage context,
      cbRewriteSystem = Nothing,
      cbSeedInterpreter = Nothing
    }

withRewriteSystemWitness ::
  RewriteSystem f ->
  CohomologicalBackend owner c f ->
  CohomologicalBackend owner c f
withRewriteSystemWitness rewriteSystem configuration =
  configuration {cbRewriteSystem = Just rewriteSystem}

cohomologicalBackendForProfile ::
  Language f =>
  EGraphSectionCertification owner c f ->
  CohomologicalProfile ->
  CohomologicalBackend owner c f
cohomologicalBackendForProfile context profile =
  mkCohomologicalBackend context (profilePolicy profile)

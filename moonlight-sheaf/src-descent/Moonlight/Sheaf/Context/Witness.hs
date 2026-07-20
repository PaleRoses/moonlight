module Moonlight.Sheaf.Context.Witness
  ( ContextMorphism,
    CanonicalContextSection,
    mkContextMorphism,
    canonicalContextSectionAt,
    contextRestrictionKernel,
    contextRestrictionIdentity,
    contextRestrictionFunctorialAction,
    contextGlobalSectionInvariant,
    analysisRestrictionIdentity,
    analysisRestrictionComposition,
    analysisGlobalSectionInvariant,
    contextAnalysisRestrictionIdentity,
    contextAnalysisRestrictionComposition,
    contextAnalysisGlobalSectionInvariant,
  )
where

import Data.Kind (Type)
import Moonlight.Sheaf.Context.Algebra
  ( ContextAlgebraSite,
    contextAnalysisFor,
    contextAnalysisJoin,
    classesFor,
    contextCachedContexts,
    contextPreparedSite,
    restrictAnalysisToTarget
  )
import Moonlight.Sheaf.Context.Core
  ( ContextLattice (..),
    contextRefinesTo,
  )
import Moonlight.Sheaf.Context.Section
  ( ContextClassSection (..),
    analysisSectionMismatches,
    restrictSectionToTarget,
  )
import Moonlight.Sheaf.Section.Restriction.Witness
  ( RestrictionKernel (..),
  )
import qualified Moonlight.Sheaf.Section.Restriction.Witness as SheafRestriction
import Moonlight.Sheaf.Context.Site
  ( PreparedContextSupportError,
    preparedContextRestrictsTo,
  )
import Moonlight.FiniteLattice
  ( ContextLatticeLookupError
  )

type ContextMorphism :: Type -> Type
type ContextMorphism = SheafRestriction.ContextMorphism

type CanonicalContextSection :: Type -> Type
type CanonicalContextSection = SheafRestriction.CanonicalContextSection

mkContextMorphism :: Ord c => ContextLattice c -> c -> c -> Either (ContextLatticeLookupError c) (Maybe (ContextMorphism c))
mkContextMorphism latticeValue =
  SheafRestriction.mkContextMorphism (contextRefinesTo latticeValue)

canonicalContextSectionAt :: ContextAlgebraSite store ctx classId analysis => ctx -> store -> Either (PreparedContextSupportError ctx) (CanonicalContextSection (ContextClassSection classId))
canonicalContextSectionAt contextValue store =
  fmap
    (SheafRestriction.CanonicalContextSection . ContextClassSection)
    (classesFor contextValue store)

contextRestrictionKernel ::
  ContextAlgebraSite store ctx classId analysis =>
  store ->
  RestrictionKernel ctx (PreparedContextSupportError ctx) (ContextClassSection classId)
contextRestrictionKernel store =
  RestrictionKernel
    { rkRefinesTo = preparedContextRestrictsTo (contextPreparedSite store),
      rkCanonicalSectionAt = (`canonicalContextSectionAt` store),
      rkRestrictSectionToTarget =
        \sourceContext targetContext sectionValue -> do
          targetClasses <- classesFor targetContext store
          pure
            ( restrictSectionToTarget
                targetClasses
                sourceContext
                targetContext
                sectionValue
            ),
      rkCachedContexts = contextCachedContexts store
    }

contextRestrictionIdentity ::
  ContextAlgebraSite store ctx classId analysis =>
  ctx ->
  store ->
  Either (PreparedContextSupportError ctx) Bool
contextRestrictionIdentity contextValue store =
  SheafRestriction.contextRestrictionIdentity
    contextValue
    (contextRestrictionKernel store)

contextRestrictionFunctorialAction ::
  ContextAlgebraSite store ctx classId analysis =>
  ContextMorphism ctx ->
  ContextMorphism ctx ->
  store ->
  Either (PreparedContextSupportError ctx) Bool
contextRestrictionFunctorialAction firstMorphism secondMorphism store =
  SheafRestriction.contextRestrictionFunctorialAction
    firstMorphism
    secondMorphism
    (contextRestrictionKernel store)

contextGlobalSectionInvariant ::
  ContextAlgebraSite store ctx classId analysis =>
  ContextMorphism ctx ->
  store ->
  Either (PreparedContextSupportError ctx) Bool
contextGlobalSectionInvariant contextMorphism store =
  SheafRestriction.contextGlobalSectionInvariant
    contextMorphism
    (contextRestrictionKernel store)

analysisRestrictionIdentity ::
  Eq analysis =>
  (ctx -> analysis) ->
  (ctx -> ctx -> analysis -> analysis) ->
  ctx ->
  Bool
analysisRestrictionIdentity analysisAt restrictAnalysis contextValue =
  let sectionValue = analysisAt contextValue
   in restrictAnalysis contextValue contextValue sectionValue == sectionValue

analysisRestrictionComposition ::
  Eq analysis =>
  (ctx -> analysis) ->
  (ctx -> ctx -> analysis -> analysis) ->
  ContextMorphism ctx ->
  ContextMorphism ctx ->
  Bool
analysisRestrictionComposition analysisAt restrictAnalysis firstMorphism secondMorphism =
  let sourceContext = SheafRestriction.contextMorphismSource firstMorphism
      middleContext = SheafRestriction.contextMorphismTarget firstMorphism
      targetContext = SheafRestriction.contextMorphismTarget secondMorphism
      targetAnalysis = analysisAt targetContext
   in restrictAnalysis sourceContext middleContext
        (restrictAnalysis middleContext targetContext targetAnalysis)
        == restrictAnalysis sourceContext targetContext targetAnalysis

analysisGlobalSectionInvariant ::
  Eq analysis =>
  (ctx -> analysis) ->
  (ctx -> ctx -> analysis -> analysis) ->
  ContextMorphism ctx ->
  Bool
analysisGlobalSectionInvariant analysisAt restrictAnalysis morphism =
  let sourceContext = SheafRestriction.contextMorphismSource morphism
      targetContext = SheafRestriction.contextMorphismTarget morphism
   in analysisAt sourceContext
        == restrictAnalysis sourceContext targetContext (analysisAt targetContext)

contextAnalysisRestrictionIdentity ::
  (ContextAlgebraSite store ctx classId analysis, Eq analysis) =>
  ctx ->
  store ->
  Either (PreparedContextSupportError ctx) Bool
contextAnalysisRestrictionIdentity contextValue store = do
  analysisSection <- contextAnalysisFor contextValue store
  classSection <- classesFor contextValue store
  let restricted = restrictAnalysisToTarget (contextAnalysisJoin store) classSection analysisSection
  pure (null (analysisSectionMismatches analysisSection restricted))

contextAnalysisRestrictionComposition ::
  (ContextAlgebraSite store ctx classId analysis, Eq analysis) =>
  ContextMorphism ctx ->
  ContextMorphism ctx ->
  store ->
  Either (PreparedContextSupportError ctx) Bool
contextAnalysisRestrictionComposition firstMorphism secondMorphism store = do
  let targetContext = SheafRestriction.contextMorphismTarget secondMorphism
      middleContext = SheafRestriction.contextMorphismTarget firstMorphism
      sourceContext = SheafRestriction.contextMorphismSource firstMorphism
  targetAnalysis <- contextAnalysisFor targetContext store
  middleClasses <- classesFor middleContext store
  sourceClasses <- classesFor sourceContext store
  let
      viaMiddle =
        restrictAnalysisToTarget (contextAnalysisJoin store) sourceClasses
          (restrictAnalysisToTarget (contextAnalysisJoin store) middleClasses targetAnalysis)
      direct = restrictAnalysisToTarget (contextAnalysisJoin store) sourceClasses targetAnalysis
  pure (null (analysisSectionMismatches viaMiddle direct))

contextAnalysisGlobalSectionInvariant ::
  (ContextAlgebraSite store ctx classId analysis, Eq analysis) =>
  ContextMorphism ctx ->
  store ->
  Either (PreparedContextSupportError ctx) Bool
contextAnalysisGlobalSectionInvariant morphism store = do
  let sourceContext = SheafRestriction.contextMorphismSource morphism
      targetContext = SheafRestriction.contextMorphismTarget morphism
  sourceAnalysis <- contextAnalysisFor sourceContext store
  targetAnalysis <- contextAnalysisFor targetContext store
  sourceClasses <- classesFor sourceContext store
  let
      pushforward = restrictAnalysisToTarget (contextAnalysisJoin store) sourceClasses targetAnalysis
  pure (null (analysisSectionMismatches sourceAnalysis pushforward))

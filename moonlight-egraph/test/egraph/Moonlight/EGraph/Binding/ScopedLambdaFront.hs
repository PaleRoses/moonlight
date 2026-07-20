{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}

module Moonlight.EGraph.Binding.ScopedLambdaFront
  ( ScopedLambdaSig,
    ScopedLambdaContext (..),
    ScopedLambdaRelations (..),
    ScopedLambdaExtraTerm (..),
    declareScopedLambdaRelations,
    compileScopedLambdaShapePlan,
    compileScopedLambdaElaboration,
    emptyScopedLambdaGraph,
    scopedLambdaGraph,
  )
where

import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Monoid (First (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (fromString)
import Moonlight.Core (emptyTheorySpec)
import Moonlight.EGraph.Effect.CoveringSurface (SurfaceKind)
import Moonlight.EGraph.Pure.Context (withEmptyContextEGraph)
import Moonlight.EGraph.Pure.Saturation.Front
  ( EGraphFrontM,
    Term,
  )
import Moonlight.EGraph.Pure.Saturation.Front.PackedNode
  ( PackedNode,
    packAnalysisSpec,
  )
import Moonlight.EGraph.Pure.Saturation.Front.Binding
  ( BindingChild,
    BindingIngestError,
    BindingPath,
    BindingPathSegment,
    BindingPlan,
    BindingRootName,
    bindingChild,
    bindingPathChildNamed,
    bindingPathName,
    bindingPathSegmentName,
    bindingPlanContexts,
  )
import Moonlight.EGraph.Pure.Saturation.Front.Binding.Language
  ( BindingElaboration,
    BindingFresheningSyntax (..),
    BindingLanguageError (..),
    BindingLanguageRelations,
    BindingLanguageSyntax (..),
    BindingSubstitutionDecision (..),
    BindingSubstitutionSite (..),
    compileBindingElaboration,
    declareBindingLanguageRelations,
  )
import Moonlight.EGraph.Pure.Saturation.Front.Binding.Scoped
  ( ScopedBindingNode (..),
    ScopedBindingSyntax (..),
    ScopedBindingTree (..),
    compileScopedBindingTerm,
    scopedBindingChildPathNamed,
  )
import Moonlight.EGraph.Spec.LambdaBindingGoal.Lambda
  ( LamF (..),
    LamAnalysis,
    Name,
    appTerm,
    lamAnalysisSpec,
    nameString,
  )
import Moonlight.EGraph.Pure.Types (emptyEGraphWithTheory)
import Moonlight.EGraph.Saturation.Context.State
  ( SaturatingContextEGraph,
    emptySaturatingContextEGraph,
  )
import Moonlight.EGraph.Test.Front.Mono
  ( MonoSig,
    monoAnalysisSpec,
    monoFix,
  )
import Data.Fix (Fix (..))
import Moonlight.FiniteLattice
  ( ContextLattice,
    compileContextLattice,
    contextOrderDecl
  )

-- | Test-side lambda signature. The production binding-language layer remains
-- lambda-blind; this module is the witness algebra.
type ScopedLambdaSig = MonoSig LamF

data ScopedLambdaContext
  = ScopedGlobal
  | ScopedBinder
  | ScopedBody
  deriving stock (Eq, Ord, Show)

newtype ScopedLambdaRelations = ScopedLambdaRelations
  { slrBindingRelations :: BindingLanguageRelations ScopedLambdaSig
  }

data ScopedLambdaExtraTerm = ScopedLambdaExtraTerm
  { sletSegment :: !String,
    sletContext :: !ScopedLambdaContext,
    sletTerm :: !(Fix LamF)
  }

declareScopedLambdaRelations ::
  EGraphFrontM ScopedLambdaSig analysis context ScopedLambdaRelations
declareScopedLambdaRelations =
  ScopedLambdaRelations <$> declareBindingLanguageRelations "lambda"

compileScopedLambdaShapePlan ::
  BindingRootName ->
  Fix LamF ->
  [ScopedLambdaExtraTerm] ->
  Either BindingIngestError (BindingPlan ScopedLambdaSig ScopedLambdaContext)
compileScopedLambdaShapePlan rawRootName rootTerm extraTerms =
  compileScopedBindingTerm (scopedLambdaScopedSyntax rawRootName extraTerms) rawRootName rootTerm

compileScopedLambdaElaboration ::
  ScopedLambdaRelations ->
  BindingRootName ->
  Fix LamF ->
  [ScopedLambdaExtraTerm] ->
  Either
    (BindingLanguageError Name)
    (BindingElaboration ScopedLambdaSig ScopedLambdaContext Name)
compileScopedLambdaElaboration (ScopedLambdaRelations relations) rawRootName rootTerm extraTerms =
  compileBindingElaboration
    relations
    scopedLambdaBindingLanguage
    scopedLambdaFresheningSyntax
    (scopedLambdaScopedSyntax rawRootName extraTerms)
    rawRootName
    rootTerm

scopedLambdaGraph ::
  BindingPlan ScopedLambdaSig ScopedLambdaContext ->
  (forall owner. SaturatingContextEGraph owner SurfaceKind (PackedNode ScopedLambdaSig) LamAnalysis ScopedLambdaContext -> result) ->
  Either String result
scopedLambdaGraph bindingPlan useGraph =
  fmap
    (\contextLattice -> emptyScopedLambdaGraph contextLattice useGraph)
    (scopedLambdaLattice (bindingPlanContexts bindingPlan))

emptyScopedLambdaGraph ::
  Ord context =>
  ContextLattice context ->
  (forall owner. SaturatingContextEGraph owner SurfaceKind (PackedNode ScopedLambdaSig) LamAnalysis context -> result) ->
  result
emptyScopedLambdaGraph lattice useGraph =
  withEmptyContextEGraph
    lattice
    (emptyEGraphWithTheory (packAnalysisSpec (monoAnalysisSpec lamAnalysisSpec)) emptyTheorySpec)
    (useGraph . emptySaturatingContextEGraph)

scopedLambdaLattice :: [ScopedLambdaContext] -> Either String (ContextLattice ScopedLambdaContext)
scopedLambdaLattice derivedContexts =
  first show $
    compileContextLattice
      (Set.fromList (ScopedGlobal : derivedContexts))
      ( contextOrderDecl
          ScopedBody
          ScopedGlobal
          [ (ScopedGlobal, ScopedBinder),
            (ScopedBinder, ScopedBody)
          ]
      )

scopedLambdaScopedSyntax ::
  BindingRootName ->
  [ScopedLambdaExtraTerm] ->
  ScopedBindingSyntax LamF ScopedLambdaSig ScopedLambdaContext ()
scopedLambdaScopedSyntax rawRootName extraTerms =
  ScopedBindingSyntax
    { sbsInitialScope = (),
      sbsRootContext = ScopedBinder,
      sbsChildren = scopedLambdaChildren rawRootName extraTerms,
      sbsFactsAtNode = const (Right []),
      sbsTermAtPath = \_ _ -> monoFix
    }

scopedLambdaChildren ::
  BindingRootName ->
  [ScopedLambdaExtraTerm] ->
  BindingPath ->
  () ->
  Fix LamF ->
  Either BindingIngestError [BindingChild LamF ScopedLambdaContext ()]
scopedLambdaChildren rawRootName extraTerms path _ termValue = do
  syntaxChildren <- scopedLambdaSyntaxChildren termValue
  extraChildren <-
    if bindingPathName path == rawRootName
      then traverse extraTermChild extraTerms
      else Right []
  Right (syntaxChildren <> extraChildren)

scopedLambdaSyntaxChildren ::
  Fix LamF ->
  Either BindingIngestError [BindingChild LamF ScopedLambdaContext ()]
scopedLambdaSyntaxChildren (Fix layer) =
  case layer of
    LVar {} ->
      Right []
    LLit {} ->
      Right []
    LLam _ body ->
      traverse
        (\(name, childTerm) -> bindingChild name ScopedBody () childTerm)
        [("body", body)]
    LApp function argument ->
      traverse
        (\(name, childTerm) -> bindingChild name ScopedBody () childTerm)
        [("function", function), ("argument", argument)]
    LAdd left right ->
      traverse
        (\(name, childTerm) -> bindingChild name ScopedBody () childTerm)
        [("left", left), ("right", right)]

extraTermChild ::
  ScopedLambdaExtraTerm ->
  Either BindingIngestError (BindingChild LamF ScopedLambdaContext ())
extraTermChild extraTerm =
  bindingChild
    (sletSegment extraTerm)
    (sletContext extraTerm)
    ()
    (sletTerm extraTerm)

scopedLambdaBindingLanguage :: BindingLanguageSyntax LamF Name
scopedLambdaBindingLanguage =
  BindingLanguageSyntax
    { blsOccurrencesAt = scopedLambdaOccurrencesAt,
      blsBindersEnteringChild = scopedLambdaBindersEnteringChild,
      blsSubstitutionSitesAt = scopedLambdaSubstitutionSitesAt
    }

scopedLambdaOccurrencesAt :: BindingPath -> Fix LamF -> Set Name
scopedLambdaOccurrencesAt _ (Fix layer) =
  case layer of
    LVar name -> Set.singleton name
    LLam {} -> Set.empty
    LApp {} -> Set.empty
    LLit {} -> Set.empty
    LAdd {} -> Set.empty

scopedLambdaBindersEnteringChild ::
  BindingPath ->
  Fix LamF ->
  BindingPathSegment ->
  Fix LamF ->
  Set Name
scopedLambdaBindersEnteringChild _ (Fix layer) childSegment _ =
  case layer of
    LLam binder _
      | bindingPathSegmentName childSegment == "body" ->
          Set.singleton binder
    _ ->
      Set.empty

scopedLambdaSubstitutionSitesAt ::
  ScopedBindingNode LamF context scope ->
  Either (BindingLanguageError Name) [BindingSubstitutionSite Name]
scopedLambdaSubstitutionSitesAt node =
  case sbnTerm node of
    Fix (LApp (Fix (LLam binder _)) _) -> do
      functionPath <- childPathNamed "function" node
      bodyPath <- childPathUnder functionPath "body"
      argumentPath <- childPathNamed "argument" node
      Right
        [ BindingSubstitutionSite
            { bssBinder = binder,
              bssBodyPath = bodyPath,
              bssArgumentPath = argumentPath
            }
        ]
    _ ->
      Right []

scopedLambdaFresheningSyntax :: BindingFresheningSyntax LamF ScopedLambdaSig Name
scopedLambdaFresheningSyntax =
  BindingFresheningSyntax
    { bfsFreshenBinders = scopedLambdaFreshenBinders,
      bfsFreshenedRedex = scopedLambdaFreshenedRedex,
      bfsContractedResult = scopedLambdaContractedResult
    }

data FreshNameSelection = FreshNameSelection
  { fnsRenames :: !(Map Name Name),
    fnsAvoid :: !(Set Name)
  }

scopedLambdaFreshenBinders ::
  BindingSubstitutionDecision Name ->
  Either (BindingLanguageError Name) (Map Name Name)
scopedLambdaFreshenBinders decision =
  fnsRenames
    <$> foldM
      selectFreshName
      FreshNameSelection
        { fnsRenames = Map.empty,
          fnsAvoid = freshNameAvoidSet decision
        }
      (Set.toAscList (capturedBinders decision))

freshNameAvoidSet :: BindingSubstitutionDecision Name -> Set Name
freshNameAvoidSet decision =
  Set.unions
    [ bsdArgumentFreeBinders decision,
      bsdBodyCapturingBinders decision,
      Set.singleton (bsdBinder decision)
    ]

capturedBinders :: BindingSubstitutionDecision Name -> Set Name
capturedBinders decision =
  Set.intersection
    (bsdArgumentFreeBinders decision)
    (bsdBodyCapturingBinders decision)

selectFreshName ::
  FreshNameSelection ->
  Name ->
  Either (BindingLanguageError Name) FreshNameSelection
selectFreshName selection binderName = do
  freshName <-
    maybe
      (Left (BindingLanguageFreshNameExhausted binderName))
      Right
      (find (`Set.notMember` fnsAvoid selection) (freshNameCandidates binderName))
  pure
    selection
      { fnsRenames = Map.insert binderName freshName (fnsRenames selection),
        fnsAvoid = Set.insert freshName (fnsAvoid selection)
      }

freshNameCandidates :: Name -> [Name]
freshNameCandidates name =
  fmap (freshNameAt name) [0 .. 1024 :: Int]

freshNameAt :: Name -> Int -> Name
freshNameAt name index =
  fromStringName (nameString name <> show index)

fromStringName :: String -> Name
fromStringName =
  fromString

scopedLambdaFreshenedRedex ::
  Map Name Name ->
  ScopedBindingTree LamF context scope ->
  BindingSubstitutionDecision Name ->
  Either (BindingLanguageError Name) (Term ScopedLambdaSig "Expr")
scopedLambdaFreshenedRedex renames tree decision = do
  redex <-
    substitutionRedexAt tree decision
  pure $ monoFix $ appTerm (freshenedFunction renames redex) (sraArgument redex)

scopedLambdaContractedResult ::
  Map Name Name ->
  ScopedBindingTree LamF context scope ->
  BindingSubstitutionDecision Name ->
  Either (BindingLanguageError Name) (Term ScopedLambdaSig "Expr")
scopedLambdaContractedResult renames tree decision = do
  redex <-
    substitutionRedexAt tree decision
  let body =
        renameCapturedBinders renames (sraBody redex)
      result =
        substituteName (sraBinder redex) (sraArgument redex) body
  pure (monoFix result)

data SubstitutionRedex = SubstitutionRedex
  { sraBinder :: !Name,
    sraBody :: !(Fix LamF),
    sraArgument :: !(Fix LamF)
  }

substitutionRedexAt ::
  ScopedBindingTree LamF context scope ->
  BindingSubstitutionDecision Name ->
  Either (BindingLanguageError Name) SubstitutionRedex
substitutionRedexAt tree decision = do
  redexTree <-
    maybe
      (Left (BindingLanguageUnknownPath (bsdRedexPath decision)))
      Right
      (findScopedBindingTree (bsdRedexPath decision) tree)
  case sbnTerm (sbtNode redexTree) of
    Fix (LApp (Fix (LLam binder body)) argument)
      | binder == bsdBinder decision ->
          Right
            SubstitutionRedex
              { sraBinder = binder,
                sraBody = body,
                sraArgument = argument
              }
    _ ->
      Left (BindingLanguageUnexpectedSubstitutionShape (bsdRedexPath decision))

findScopedBindingTree ::
  BindingPath ->
  ScopedBindingTree LamF context scope ->
  Maybe (ScopedBindingTree LamF context scope)
findScopedBindingTree path tree
  | sbnPath (sbtNode tree) == path =
      Just tree
  | otherwise =
      getFirst $
        foldMap
          (First . findScopedBindingTree path . snd)
          (sbtChildren tree)

freshenedFunction :: Map Name Name -> SubstitutionRedex -> Fix LamF
freshenedFunction renames redex =
  Fix (LLam (sraBinder redex) (renameCapturedBinders renames (sraBody redex)))

renameCapturedBinders :: Map Name Name -> Fix LamF -> Fix LamF
renameCapturedBinders renames =
  renameCapturedBindersWith Map.empty
  where
    renameCapturedBindersWith activeRenames (Fix layer) =
      case layer of
        LVar name ->
          Fix (LVar (Map.findWithDefault name name activeRenames))
        LLam binder body ->
          let freshBinder =
                Map.findWithDefault binder binder renames
              nextRenames =
                if Map.member binder renames
                  then Map.insert binder freshBinder activeRenames
                  else Map.delete binder activeRenames
           in Fix (LLam freshBinder (renameCapturedBindersWith nextRenames body))
        LApp function argument ->
          Fix (LApp (renameCapturedBindersWith activeRenames function) (renameCapturedBindersWith activeRenames argument))
        LLit value ->
          Fix (LLit value)
        LAdd left right ->
          Fix (LAdd (renameCapturedBindersWith activeRenames left) (renameCapturedBindersWith activeRenames right))

substituteName :: Name -> Fix LamF -> Fix LamF -> Fix LamF
substituteName binder replacement (Fix layer) =
  case layer of
    LVar name
      | name == binder ->
          replacement
      | otherwise ->
          Fix (LVar name)
    LLam name body
      | name == binder ->
          Fix (LLam name body)
      | otherwise ->
          Fix (LLam name (substituteName binder replacement body))
    LApp function argument ->
      Fix (LApp (substituteName binder replacement function) (substituteName binder replacement argument))
    LLit value ->
      Fix (LLit value)
    LAdd left right ->
      Fix (LAdd (substituteName binder replacement left) (substituteName binder replacement right))

childPathNamed ::
  String ->
  ScopedBindingNode LamF context scope ->
  Either (BindingLanguageError Name) BindingPath
childPathNamed rawSegment =
  first BindingLanguageIngestError . scopedBindingChildPathNamed rawSegment

childPathUnder ::
  BindingPath ->
  String ->
  Either (BindingLanguageError Name) BindingPath
childPathUnder parentPath rawSegment =
  first BindingLanguageIngestError (bindingPathChildNamed parentPath rawSegment)

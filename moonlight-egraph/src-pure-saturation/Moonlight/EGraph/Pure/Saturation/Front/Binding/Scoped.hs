{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.EGraph.Pure.Saturation.Front.Binding.Scoped
  ( ScopedBindingSyntax (..),
    ScopedBindingNode (..),
    ScopedBindingResolvedChild (..),
    ScopedBindingTree (..),
    scopedBindingChildPathNamed,
    compileScopedBindingTree,
    bindingPlanFromScopedBindingTree,
    compileScopedBindingTerm,
  )
where

import Data.Kind (Type)
import GHC.TypeLits (Symbol)
import Moonlight.EGraph.Pure.Saturation.Front
  ( Term,
  )
import Moonlight.EGraph.Pure.Saturation.Front.Binding
  ( BindingChild (..),
    BindingFact,
    BindingIngestError (..),
    BindingPath,
    BindingPlan,
    BindingPlanEntry (..),
    BindingRootName,
    bindingPathChild,
    bindingPathChildNamed,
    bindingPathSingletonNamed,
    bindingPlanFromEntries,
  )
import Data.Fix (Fix)

type ScopedBindingSyntax :: (Type -> Type) -> (Symbol -> (Symbol -> Type) -> Type) -> Type -> Type -> Type
data ScopedBindingSyntax f sig context scope = ScopedBindingSyntax
  { sbsInitialScope :: !scope,
    sbsRootContext :: !context,
    sbsChildren :: !(BindingPath -> scope -> Fix f -> Either BindingIngestError [BindingChild f context scope]),
    sbsFactsAtNode :: !(ScopedBindingNode f context scope -> Either BindingIngestError [BindingFact sig]),
    sbsTermAtPath :: !(BindingPath -> scope -> Fix f -> Term sig "Expr")
  }

type ScopedBindingNode :: (Type -> Type) -> Type -> Type -> Type
data ScopedBindingNode f context scope = ScopedBindingNode
  { sbnPath :: !BindingPath,
    sbnContext :: !context,
    sbnScope :: !scope,
    sbnTerm :: !(Fix f),
    sbnChildren :: ![ScopedBindingResolvedChild f context scope]
  }

type ScopedBindingResolvedChild :: (Type -> Type) -> Type -> Type -> Type
data ScopedBindingResolvedChild f context scope = ScopedBindingResolvedChild
  { sbrcPath :: !BindingPath,
    sbrcChild :: !(BindingChild f context scope)
  }

type ScopedBindingTree :: (Type -> Type) -> Type -> Type -> Type
data ScopedBindingTree f context scope = ScopedBindingTree
  { sbtNode :: !(ScopedBindingNode f context scope),
    sbtChildren :: ![(ScopedBindingResolvedChild f context scope, ScopedBindingTree f context scope)]
  }

scopedBindingChildPathNamed ::
  String ->
  ScopedBindingNode f context scope ->
  Either BindingIngestError BindingPath
scopedBindingChildPathNamed rawSegment node = do
  expectedPath <- bindingPathChildNamed (sbnPath node) rawSegment
  if any ((== expectedPath) . sbrcPath) (sbnChildren node)
    then Right expectedPath
    else Left (BindingUnknownFactPath (sbnPath node) expectedPath)

compileScopedBindingTree ::
  ScopedBindingSyntax f sig context scope ->
  BindingRootName ->
  Fix f ->
  Either BindingIngestError (ScopedBindingTree f context scope)
compileScopedBindingTree syntax rawRootName rootTerm = do
  rootPath <- bindingPathSingletonNamed rawRootName
  scopedBindingTree
    syntax
    rootPath
    (sbsInitialScope syntax)
    (sbsRootContext syntax)
    rootTerm

bindingPlanFromScopedBindingTree ::
  ScopedBindingSyntax f sig context scope ->
  ScopedBindingTree f context scope ->
  Either BindingIngestError (BindingPlan sig context)
bindingPlanFromScopedBindingTree syntax tree = do
  entries <- scopedBindingTreeEntries syntax tree
  bindingPlanFromEntries (sbnPath (sbtNode tree)) entries

compileScopedBindingTerm ::
  ScopedBindingSyntax f sig context scope ->
  BindingRootName ->
  Fix f ->
  Either BindingIngestError (BindingPlan sig context)
compileScopedBindingTerm syntax rawRootName rootTerm =
  compileScopedBindingTree syntax rawRootName rootTerm
    >>= bindingPlanFromScopedBindingTree syntax

scopedBindingTree ::
  ScopedBindingSyntax f sig context scope ->
  BindingPath ->
  scope ->
  context ->
  Fix f ->
  Either BindingIngestError (ScopedBindingTree f context scope)
scopedBindingTree syntax path scope contextValue termValue = do
  children <-
    sbsChildren syntax path scope termValue
  let resolvedChildren =
        fmap (resolveScopedChild path) children
      node =
        ScopedBindingNode
          { sbnPath = path,
            sbnContext = contextValue,
            sbnScope = scope,
            sbnTerm = termValue,
            sbnChildren = resolvedChildren
          }
  childTrees <-
    traverse
      (scopedChildTree syntax)
      resolvedChildren
  pure
    ScopedBindingTree
      { sbtNode = node,
        sbtChildren = zip resolvedChildren childTrees
      }

scopedBindingTreeEntries ::
  ScopedBindingSyntax f sig context scope ->
  ScopedBindingTree f context scope ->
  Either BindingIngestError [BindingPlanEntry sig context]
scopedBindingTreeEntries syntax tree = do
  let node =
        sbtNode tree
  facts <-
    sbsFactsAtNode syntax node
  childEntries <-
    fmap concat $
      traverse
        (scopedBindingTreeEntries syntax . snd)
        (sbtChildren tree)
  pure
    ( BindingPlanEntry
        { bpePath = sbnPath node,
          bpeContext = sbnContext node,
          bpeTerm = sbsTermAtPath syntax (sbnPath node) (sbnScope node) (sbnTerm node),
          bpeFacts = facts
        }
        : childEntries
    )

resolveScopedChild :: BindingPath -> BindingChild f context scope -> ScopedBindingResolvedChild f context scope
resolveScopedChild parentPath child =
  ScopedBindingResolvedChild
    { sbrcPath = bindingPathChild parentPath (bcSegment child),
      sbrcChild = child
    }

scopedChildTree ::
  ScopedBindingSyntax f sig context scope ->
  ScopedBindingResolvedChild f context scope ->
  Either BindingIngestError (ScopedBindingTree f context scope)
scopedChildTree syntax resolvedChild =
  scopedBindingTree
    syntax
    (sbrcPath resolvedChild)
    (bcScope child)
    (bcContext child)
    (bcTerm child)
  where
    child =
      sbrcChild resolvedChild

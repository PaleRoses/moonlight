module Moonlight.Sheaf.Twist.Apply
  ( SupportMergeHost (..),
    applySupportMergeWith,
    SupportedRewriteGraphHost (..),
    applySupportedRewriteToGraphWith,
  )
where

import Control.Monad (foldM)
import Data.Kind (Type)

type SupportMergeHost :: Type -> Type -> Type -> Type -> Type -> Type
data SupportMergeHost support ctx classId graph err = SupportMergeHost
  { smhSupportGenerators :: !(support -> [ctx]),
    smhIsGlobalSupport :: !(support -> Bool),
    smhGlobalMerge :: !(classId -> classId -> graph -> Either err graph),
    smhContextMerge :: !(ctx -> classId -> classId -> graph -> Either err graph)
  }

applySupportMergeWith ::
  SupportMergeHost support ctx classId graph err ->
  support ->
  classId ->
  classId ->
  graph ->
  Either err graph
applySupportMergeWith host supportValue leftClassId rightClassId graphValue =
  if smhIsGlobalSupport host supportValue
    then smhGlobalMerge host leftClassId rightClassId graphValue
    else
      foldM
        (\currentGraph contextValue -> smhContextMerge host contextValue leftClassId rightClassId currentGraph)
        graphValue
        (smhSupportGenerators host supportValue)

type SupportedRewriteGraphHost :: Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type
data SupportedRewriteGraphHost runtime supported support classId graph rhs err =
  SupportedRewriteGraphHost
    { srghSupport :: !(supported -> support),
      srghLeftClassId :: !(supported -> classId),
      srghInstantiateRhs :: !(runtime -> supported -> graph -> Either err rhs),
      srghInsertRhs :: !(support -> rhs -> graph -> Either err (classId, graph)),
      srghMergeClasses :: !(support -> classId -> classId -> graph -> Either err graph)
    }

applySupportedRewriteToGraphWith ::
  SupportedRewriteGraphHost runtime supported support classId graph rhs err ->
  runtime ->
  supported ->
  graph ->
  Either err (classId, classId, graph)
applySupportedRewriteToGraphWith host runtimeValue supportedValue graphValue = do
  let supportValue = srghSupport host supportedValue
      leftClassId = srghLeftClassId host supportedValue
  rhsValue <- srghInstantiateRhs host runtimeValue supportedValue graphValue
  (rightClassId, graphWithRhs) <- srghInsertRhs host supportValue rhsValue graphValue
  mergedGraph <- srghMergeClasses host supportValue leftClassId rightClassId graphWithRhs
  pure (leftClassId, rightClassId, mergedGraph)

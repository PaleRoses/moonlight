module Moonlight.Flow.Runtime.Create
  ( createRuntime,
    createRuntimeWithOptions,
    createRuntimeWithContextLattice,
  )
where

import Moonlight.Flow.Runtime.Backend
  ( defaultBackend,
    sheafBackend,
  )
import Moonlight.Flow.Runtime.Kernel.Create
  ( createRelDiffRuntimeWithBackend,
    createRuntimeWithBackend,
  )
import Moonlight.Flow.Runtime.Spec.Schema
  ( RuntimeSpec,
  )
import Moonlight.Flow.Runtime.Types
  ( Runtime (..),
    RuntimeCreateError,
    RuntimeCreateOptions,
    defaultRuntimeCreateOptions,
  )
import Moonlight.FiniteLattice
  ( ContextLattice
  )

createRuntime ::
  (Ord ctx, Ord prop) =>
  RuntimeSpec ctx prop ->
  Either (RuntimeCreateError ctx prop) (Runtime ctx prop)
createRuntime spec =
  createRuntimeWithBackend defaultBackend spec defaultRuntimeCreateOptions

createRuntimeWithOptions ::
  (Ord ctx, Ord prop) =>
  RuntimeSpec ctx prop ->
  RuntimeCreateOptions ->
  Either (RuntimeCreateError ctx prop) (Runtime ctx prop)
createRuntimeWithOptions spec options =
  createRuntimeWithBackend defaultBackend spec options

createRuntimeWithContextLattice ::
  (Ord ctx, Ord prop) =>
  ContextLattice ctx ->
  RuntimeSpec ctx prop ->
  Either (RuntimeCreateError ctx prop) (Runtime ctx prop)
createRuntimeWithContextLattice latticeValue spec =
  Runtime
    <$> createRelDiffRuntimeWithBackend
      (sheafBackend latticeValue)
      spec
      defaultRuntimeCreateOptions

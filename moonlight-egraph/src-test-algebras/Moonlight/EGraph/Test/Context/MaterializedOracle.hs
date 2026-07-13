-- | Test-only eager interpretation of authored contextual equalities.
--
-- Production owns one regional quotient section.  This module deliberately
-- takes the expensive pointwise route so differential tests retain an
-- independent semantic oracle without smuggling graph materialization back
-- into the runtime.
module Moonlight.EGraph.Test.Context.MaterializedOracle
  ( materializedContextGraphAt,
    materializedContextRepresentativeAt,
  )
where

import Data.Map.Strict qualified as Map
import Moonlight.Core (Language)
import Moonlight.EGraph.Pure.Context.Core
  ( ContextEGraph,
    cegBase,
    cegContextFibers,
    cegSite,
    contextAuthoredUnionPairs,
  )
import Moonlight.EGraph.Pure.Rebuild (merge, rebuild)
import Moonlight.EGraph.Pure.Types
  ( ClassId,
    EGraph,
    canonicalizeClassId,
  )
import Moonlight.Sheaf.Context.Site
  ( PreparedContextSupportError,
    preparedContextRestrictsTo,
  )

materializedContextGraphAt ::
  (Language f, Ord c) =>
  c ->
  ContextEGraph f a c ->
  Either (PreparedContextSupportError c) (EGraph f a)
materializedContextGraphAt contextValue contextGraph =
  rebuild . applyAuthoredPairs (cegBase contextGraph) . concat
    <$> traverse pairsVisibleAtContext (Map.keys (cegContextFibers contextGraph))
  where
    pairsVisibleAtContext authoredContext =
      ( \isVisible ->
          if isVisible
            then contextAuthoredUnionPairs authoredContext contextGraph
            else []
      )
        <$> preparedContextRestrictsTo
          (cegSite contextGraph)
          contextValue
          authoredContext

applyAuthoredPairs :: EGraph f a -> [(ClassId, ClassId)] -> EGraph f a
applyAuthoredPairs =
  foldl' (\graphValue (leftClass, rightClass) -> merge leftClass rightClass graphValue)

materializedContextRepresentativeAt ::
  (Language f, Ord c) =>
  c ->
  ClassId ->
  ContextEGraph f a c ->
  Either (PreparedContextSupportError c) ClassId
materializedContextRepresentativeAt contextValue classId contextGraph =
  flip canonicalizeClassId classId
    <$> materializedContextGraphAt contextValue contextGraph

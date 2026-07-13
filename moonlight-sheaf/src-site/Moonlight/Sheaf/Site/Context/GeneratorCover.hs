{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Sheaf.Site.Context.GeneratorCover
  ( ContextGeneratorCover (..),
    contextClosure,
  )
where

import Data.Function ((&))
import Data.Kind (Constraint, Type)
import Data.Set qualified as Set
import Moonlight.Algebra (JoinSemilattice (..), Lattice, MeetSemilattice (..))
import Moonlight.Sheaf.Site.System (AnalyzableSystem, SystemCtx)

type ContextGeneratorCover :: Type -> Constraint
class (AnalyzableSystem system, Lattice (SystemCtx system)) => ContextGeneratorCover system where
  contextGenerators :: system -> [SystemCtx system]
  contextIsBottom :: system -> SystemCtx system -> Bool

contextClosure :: ContextGeneratorCover system => system -> [SystemCtx system]
contextClosure systemValue =
  contextGenerators systemValue
    & Set.fromList
    & closeMeetJoin
    & Set.toAscList
    & filter (not . contextIsBottom systemValue)

closeMeetJoin ::
  (Ord context, JoinSemilattice context, MeetSemilattice context) =>
  Set.Set context ->
  Set.Set context
closeMeetJoin seeds =
  let initialFrontier = meetJoinFrontier seeds seeds `Set.difference` seeds
   in frontierLoop seeds initialFrontier

frontierLoop ::
  (Ord context, JoinSemilattice context, MeetSemilattice context) =>
  Set.Set context ->
  Set.Set context ->
  Set.Set context
frontierLoop known frontier
  | Set.null frontier =
      known
  | otherwise =
      let expanded = Set.union known frontier
          novel = meetJoinFrontier frontier expanded `Set.difference` expanded
       in frontierLoop expanded novel

meetJoinFrontier ::
  (Ord context, JoinSemilattice context, MeetSemilattice context) =>
  Set.Set context ->
  Set.Set context ->
  Set.Set context
meetJoinFrontier frontier full =
  let frontierValues = Set.toAscList frontier
      fullValues = Set.toAscList full
   in frontierValues
        & foldMap
          ( \leftContext ->
              fullValues
                & foldMap
                  ( \rightContext ->
                      Set.fromList
                        [ join leftContext rightContext,
                          meet leftContext rightContext
                        ]
                  )
          )

{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Saturation.Context.Program.Internal.SiteIndex
  ( compileSiteIndex,
  )
where

import Data.Bifunctor (first)
import Data.Map.Strict qualified as Map
import Moonlight.Core
  ( SiteIndex (..),
  )
import Moonlight.Saturation.Context.Error
  ( SaturationProgramSite (..),
  )

compileSiteIndex ::
  ([source] -> Either err [compiled]) ->
  (SaturationProgramSite context -> err -> compileError) ->
  SiteIndex context source ->
  Either compileError (SiteIndex context compiled)
compileSiteIndex compileRules compileErrorAt ruleSources =
  SiteIndex
    <$> compileAt BaseProgramSite (siBase ruleSources)
    <*> Map.traverseWithKey
      (\contextValue -> compileAt (ContextProgramSite contextValue))
      (siContexts ruleSources)
  where
    compileAt site =
      first (compileErrorAt site) . compileRules
{-# INLINE compileSiteIndex #-}

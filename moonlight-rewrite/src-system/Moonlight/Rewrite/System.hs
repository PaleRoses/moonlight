{-# LANGUAGE GHC2024 #-}

-- | Public facade for checked rewrite systems and their pure logic.
--
-- The defining modules below remain the semantic owners.  This module adds no
-- construction, interpretation, or compatibility behavior.
module Moonlight.Rewrite.System
  ( module Moonlight.Rewrite.System.Check,
    module Moonlight.Rewrite.System.Checked,
    module Moonlight.Rewrite.System.Compose,
    module Moonlight.Rewrite.System.Law,
    module Moonlight.Rewrite.System.Logic.Decoration,
    module Moonlight.Rewrite.System.Logic.Guard,
    module Moonlight.Rewrite.System.Logic.Rule,
    module Moonlight.Rewrite.System.Logic.SemiNaive,
    module Moonlight.Rewrite.System.Logic.SemiNaive.Config,
    module Moonlight.Rewrite.System.Logic.SemiNaive.Engine,
    module Moonlight.Rewrite.System.Logic.SemiNaive.Input,
    module Moonlight.Rewrite.System.Logic.Store,
    module Moonlight.Rewrite.System.Origin,
    module Moonlight.Rewrite.System.Plan,
    module Moonlight.Rewrite.System.Proof.Retention,
    module Moonlight.Rewrite.System.Rule.Check,
    module Moonlight.Rewrite.System.Rule.Elaborate,
    module Moonlight.Rewrite.System.RuleName,
    module Moonlight.Rewrite.System.RuleSpec,
    module Moonlight.Rewrite.System.Support,
  )
where

import Moonlight.Rewrite.System.Check
import Moonlight.Rewrite.System.Checked
import Moonlight.Rewrite.System.Compose
import Moonlight.Rewrite.System.Law
import Moonlight.Rewrite.System.Logic.Decoration
import Moonlight.Rewrite.System.Logic.Guard
import Moonlight.Rewrite.System.Logic.Rule
import Moonlight.Rewrite.System.Logic.SemiNaive
import Moonlight.Rewrite.System.Logic.SemiNaive.Config
import Moonlight.Rewrite.System.Logic.SemiNaive.Engine
import Moonlight.Rewrite.System.Logic.SemiNaive.Input
import Moonlight.Rewrite.System.Logic.Store
import Moonlight.Rewrite.System.Origin
import Moonlight.Rewrite.System.Plan
import Moonlight.Rewrite.System.Proof.Retention
import Moonlight.Rewrite.System.Rule.Check
import Moonlight.Rewrite.System.Rule.Elaborate
import Moonlight.Rewrite.System.RuleName
import Moonlight.Rewrite.System.RuleSpec
import Moonlight.Rewrite.System.Support

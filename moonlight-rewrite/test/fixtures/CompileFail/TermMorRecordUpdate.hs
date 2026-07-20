module TermMorRecordUpdate where

import Moonlight.Rewrite.Algebra (TermMor, termMorTarget)

forgeTermMorphism :: TermMor f -> TermMor f
forgeTermMorphism morphism =
  morphism {termMorTarget = termMorTarget morphism}

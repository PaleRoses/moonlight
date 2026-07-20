module MatchRecordUpdate where

import Moonlight.Rewrite.Relational.Front (Match, matchRevision)

forgeMatch :: Match -> Match
forgeMatch matchValue =
  matchValue {matchRevision = matchRevision matchValue}

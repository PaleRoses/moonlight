module Moonlight.Rewrite.DSL.CallSite
  ( callStackSourceLocation,
  )
where

import Data.Maybe (listToMaybe)
import GHC.Stack (CallStack, SrcLoc, getCallStack)

callStackSourceLocation :: CallStack -> Maybe SrcLoc
callStackSourceLocation =
  fmap snd . listToMaybe . getCallStack

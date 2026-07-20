module Moonlight.Pale.Test.Global.Either
  ( stringifyLeft,
    stringifyLeftWith,
  )
where

import Data.Bifunctor (first)

stringifyLeft :: Show errorValue => Either errorValue value -> Either String value
stringifyLeft = first show

stringifyLeftWith ::
  Show errorValue =>
  (String -> String) ->
  Either errorValue value ->
  Either String value
stringifyLeftWith renderError =
  first (renderError . show)

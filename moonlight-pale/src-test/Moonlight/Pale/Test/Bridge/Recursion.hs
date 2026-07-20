module Moonlight.Pale.Test.Bridge.Recursion
  ( cataAfterAnaIdentity,
    interpreterCoherence,
    hyloCoherence,
  )
where

cataAfterAnaIdentity :: Eq seed => (seed -> recursive) -> (recursive -> seed) -> seed -> Bool
cataAfterAnaIdentity anamorphism catamorphism seed =
  catamorphism (anamorphism seed) == seed

interpreterCoherence :: Eq value => (seed -> recursive) -> (recursive -> value) -> (seed -> value) -> seed -> Bool
interpreterCoherence anamorphism interpretation seedInterpreter seed =
  seedInterpreter seed == interpretation (anamorphism seed)

hyloCoherence :: Eq value => (seed -> value) -> (seed -> recursive) -> (recursive -> value) -> seed -> Bool
hyloCoherence hylomorphism anamorphism interpretation =
  interpreterCoherence anamorphism interpretation hylomorphism
